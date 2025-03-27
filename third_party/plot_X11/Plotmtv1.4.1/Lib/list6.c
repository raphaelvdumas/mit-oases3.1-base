/*
 * list6.c - utilities that use linked-list data-structures 
 *
 * Find the triangle/rectangle elements in different regions that 
 * share common points
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include "CNplot.h"

void   CNgenerate_boundary_from_points();
static void        generate_region_colors();
void   CNgenerate_ptseg_list();
static void        add_point();
static void        add_ptseg();
static int         shared_elements();
static int         shared_elem_is_flagged();
void   CNfind_boundary_ptsegs();
void   CNfind_boundary_polys();
static void        find_boundary_polys_by_regionID();
static void        find_boundary_polys_by_regionname();
static int         same_material();
static void        sort_segms();
static void        add_to_taillist();
static void        add_to_headlist();
static CNsegmptr   matching_segm();
static int         regionID_of_element();
static CNregionptr regionptr_of_element();
static CNtriaptr   *create_tria_array();
static CNrectptr   *create_rect_array();
static void        elem_range_err();
static void        elem_index_err();

void CNmat_boundary();
void CNmat_exp_boundary();

/*********************************************/
/*********  GENERATE A BOUNDARY LINE  ********/
/*********************************************/

/*
 * Generate the boundaries of regions from points
 * This routine replaces CNgenerate_boundary_from_nodes() 
 *
 * This routine works on mesh-datasets as well as quantity datasets.
 * However quantity datasets need a valid parent...
 */
void CNgenerate_boundary_from_points(dptr, verbose)
CNdatasetptr dptr;
int          verbose;
{
   CNdatasetptr dmesh;
   CNregionptr  Rg;
   CNelemptr    E;
#ifdef DEBUG
   CNptsegptr   P;
#endif

   /* Error checking */
   if (dptr == NULL) {
      (void) fprintf(stderr,
      "CNgenerate_boundary_from_points:Error - Null dataset!\n");
      return;
   }
   if (dptr->datatype == CN_PIF_PARENT)
      dmesh = dptr;
   else if ((dmesh=dptr->parent) == NULL) {
      (void) fprintf(stderr,
      "CNgenerate_boundary_from_points:Error - Null parent dataset!\n");
      return;
   }
   if (dptr->elemhead == NULL) {
      (void) fprintf(stderr, "CNgenerate_boundary_from_points:");
      (void) fprintf(stderr,
                     "Error - No triangle or rectangle elements in dataset!\n");
      return;
   }
   if (dmesh->pointhead == NULL) {
      (void) fprintf(stderr,
      "CNgenerate_boundary_from_points:Error - No points in parent dataset!\n");
      return;
   }

   /*
    * Apply the region properties to the triangle/rectangle elements
    * Modify the region properties for Oxide type materials
    * Attach the region pointers to the triangle/rectangle elements
    */
   for (Rg=dptr->regionhead; Rg!=NULL; Rg=Rg->next) {
      if ((strcmp(Rg->matname,"Oxide")==0) || (strcmp(Rg->matname,"SiO2")==0))
         Rg->nocont = CN_TRUE;
      for (E=dptr->elemhead; E!=NULL; E=E->next) {
         if ((E->type == CN_TRIA_STR) && (E->tria != NULL)) {
            if (E->tria->region == Rg->ID) {
               E->tria->nocont = Rg->nocont;
               E->tria->R      = Rg;
            }
         } else if ((E->type == CN_RECT_STR) && (E->rect != NULL)) {
            if (E->rect->region == Rg->ID) {
               E->rect->nocont = Rg->nocont;
               E->rect->R      = Rg;
            }
         }
      }
   }

   /* Generate a point-segment list for the mesh */
   if (dmesh->ptseghead == NULL)
   CNgenerate_ptseg_list(dmesh->elemhead, dmesh->elemtail,
                         dmesh->pointhead, dmesh->pointtail,
                         &(dmesh->ptseghead), &(dmesh->ptsegtail));
   
   /* Set the colors in the different regions */
   generate_region_colors(dptr->regionhead, dptr->regiontail);

   /* Find the point-segments on the boundaries between regions */
   for (Rg=dptr->regionhead; Rg!=NULL; Rg=Rg->next) {
      /* Do only those regions that have not been treated yet */
      if (Rg->polyhead==NULL) {
         if (verbose) CNprint_region(Rg);
         CNfind_boundary_polys(Rg, 
                               dmesh->ptseghead, dmesh->ptsegtail,
                               dptr->elemhead, dptr->elemtail,
                               dptr->triahead, dptr->triatail,
                               dptr->recthead, dptr->recttail,
                               (dptr==dmesh)?CN_FALSE:CN_TRUE);
      }
   }
}


/* 
 * Set the colors in the different regions 
 */
/*ARGSUSED*/
static void generate_region_colors(regionhead, regiontail)
CNregionptr regionhead, regiontail;
{
#define NCOLORS 6
   CNregionptr R, RA;
   int color_index, samecolor;
   int FOUND;

   /* use only certain colors */
   static int color_array[NCOLORS] = { 6, 7, 8, 9, 2, 3 };

   color_index = 0;
   for (R=regionhead; R!=NULL; R=R->next) {
      /* Check to see if the material has already been specified */
      FOUND = CN_FALSE;
      for (RA=regionhead; RA!=R && RA!=NULL && !FOUND; RA=RA->next)
          if (strcmp(RA->matname,R->matname)==0) {
             FOUND=CN_TRUE;
             samecolor = RA->color;
          }
 
      if (!FOUND) {
         /* 
          * Set the region color 
          * Don't change the color if it has already been specified 
          */
         if (R->color < -1) {
            /* The color is selected from the array */
            R->color = color_array[color_index % NCOLORS];

            /* Increment the color array index if no match was found */
            color_index++;
         }
      } else {
         /* Set the color to the matching region's color */
         R->color = samecolor;
      }
   }
}


/*****************************************************/
/*********  FIND SEGMENTS FROM TRIANGLE LIST  ********/
/*****************************************************/

typedef struct _locpoint_sruct {
   CNpointptr p;
   CNelistptr elisthead;
   CNelistptr elisttail;
   int        nelem;
} CNlocpoint;

/*
 * Given a list of triangle/rectangle elements, generate a list of segments.
 * The objective is to generate a hierarchical list of elements, segments
 * and points.
 * Segments must not be repeated in the list.
 */
/*ARGSUSED*/
void CNgenerate_ptseg_list(elemhead,elemtail,
                           pointhead,pointtail,
                           ptseghead,ptsegtail)
CNelemptr  elemhead,   elemtail;
CNpointptr pointhead,  pointtail;
CNptsegptr *ptseghead, *ptsegtail;
{
   CNelemptr  E;
   CNpointptr P;
   CNlocpoint *pointarr;
   int        npoints, ptsegID=0, i;
   unsigned int siz;

   /*
    * Make sure the point-segment list is empty first
    */
   if (*ptseghead != NULL) CNdelete_ptseg_list(ptseghead, ptsegtail);

   /*
    * Reset the element and point flags
    */
   for (E=elemhead;  E!=NULL; E=E->next) E->flag = CN_FALSE;
   for (P=pointhead; P!=NULL; P=P->next) P->flag = CN_FALSE;

   /* 
    * Allocate space for local storage 
    * The list of triangles/rectangle elements belonging to each point 
    * is stored in a temporary array.
    */
   npoints = CNcount_points(pointhead, pointtail);
   siz     = (unsigned int)(npoints*sizeof(CNlocpoint));
   if ((pointarr = (CNlocpoint *)malloc(siz))==NULL) {
      (void) fprintf(stderr,"Error - insufficient memory!\n");
      return;
   }

   /* 
    * Fill the point array and use the point flag to index the point array
    */
   npoints=0;
   for (P=pointhead; P!=NULL; P=P->next) {
      pointarr[npoints].p         = P;
      pointarr[npoints].elisthead = NULL;
      pointarr[npoints].elisttail = NULL;
      pointarr[npoints].nelem     = 0;
      P->flag= npoints;
      npoints++;
   }

   /* 
    * Fill the point array with the point-element interconnections
    */
   for (E=elemhead; E!=NULL; E=E->next) {
      if ((E->type == CN_TRIA_STR) && (E->tria != NULL)) {
         /* The element is a triangle */
         if ((E->tria->n1->coord == E->tria->n2->coord) ||
             (E->tria->n2->coord == E->tria->n3->coord) ||
             (E->tria->n3->coord == E->tria->n1->coord)) {
            (void) fprintf(stderr,"Error - Tria %d shares 1 or more points ",
                           E->tria->ID);
            (void) fprintf(stderr,"(P%d P%d P%d)\n",
                           E->tria->n1->coord->ID, 
                           E->tria->n2->coord->ID, 
                           E->tria->n3->coord->ID);
         } else {
            add_point(E->tria->n1->coord,E,pointarr,npoints);
            add_point(E->tria->n2->coord,E,pointarr,npoints);
            add_point(E->tria->n3->coord,E,pointarr,npoints);
         }
      } else if ((E->type == CN_RECT_STR) && (E->rect != NULL)) {
         /* The element is a rectangle */
         if ((E->rect->n1->coord == E->rect->n2->coord) ||
             (E->rect->n2->coord == E->rect->n3->coord) ||
             (E->rect->n3->coord == E->rect->n4->coord) ||
             (E->rect->n4->coord == E->rect->n1->coord)) {
            (void) fprintf(stderr,"Error - Rect %d shares 1 or more points ",
                           E->rect->ID);
            (void) fprintf(stderr,"(P%d P%d P%d P%d)\n",
                           E->rect->n1->coord->ID, 
                           E->rect->n2->coord->ID, 
                           E->rect->n3->coord->ID, 
                           E->rect->n4->coord->ID);
         } else {
            add_point(E->rect->n1->coord,E,pointarr,npoints);
            add_point(E->rect->n2->coord,E,pointarr,npoints);
            add_point(E->rect->n3->coord,E,pointarr,npoints);
            add_point(E->rect->n4->coord,E,pointarr,npoints);
         }
      }
   }

   /*
    * Now go thru all the elements again and set up a pt-segm list.
    * shared_elements() is called to find the number of elements
    * shared by each pt-segm - this is used as a check for adding pt-segms
    */
   for (E=elemhead; E!=NULL; E=E->next) {
      if ((E->type == CN_TRIA_STR) && (E->tria != NULL)) {
         /* The element is a triangle */
         add_ptseg(ptseghead, ptsegtail, E->tria->n1->coord, E->tria->n2->coord,
                   &ptsegID, pointarr, npoints);
         add_ptseg(ptseghead, ptsegtail, E->tria->n2->coord, E->tria->n3->coord,
                   &ptsegID, pointarr, npoints);
         add_ptseg(ptseghead, ptsegtail, E->tria->n3->coord, E->tria->n1->coord,
                   &ptsegID, pointarr, npoints);
         E->flag = CN_TRUE;            /* Flag the element */
      } else if ((E->type == CN_RECT_STR) && (E->rect != NULL)) {
         /* The element is a rectangle */
         add_ptseg(ptseghead, ptsegtail, E->rect->n1->coord, E->rect->n2->coord,
                   &ptsegID, pointarr, npoints);
         add_ptseg(ptseghead, ptsegtail, E->rect->n2->coord, E->rect->n3->coord,
                   &ptsegID, pointarr, npoints);
         add_ptseg(ptseghead, ptsegtail, E->rect->n3->coord, E->rect->n4->coord,
                   &ptsegID, pointarr, npoints);
         add_ptseg(ptseghead, ptsegtail, E->rect->n4->coord, E->rect->n1->coord,
                   &ptsegID, pointarr, npoints);
         E->flag = CN_TRUE;            /* Flag the element */
      }
   }

#ifdef DEBUG
   /*
    * Print the point-segment list
    */
   CNprint_elem_list(elemhead, elemtail);
   CNprint_ptseg_list(*ptseghead, *ptsegtail);
   for (i=0; i<npoints; i++) {
      (void) fprintf(stdout,"Point ID = %d  ",pointarr[i].p->ID);
      CNprint_elist(pointarr[i].elisthead, pointarr[i].elisttail);
   }
#endif

   /* 
    * Free the elist
    */
   for (i=0; i<npoints; i++) {
      CNdelete_elist_list(&(pointarr[i].elisthead), &(pointarr[i].elisttail));
   }

   /* 
    * Free the local array
    */
   free((char *)pointarr);

   /* 
    * Identify the point-segments on the boundary 
    */
   CNfind_boundary_ptsegs(*ptseghead, *ptsegtail);

}

/* 
 * Add an element to the point's element list.
 */
/*ARGSUSED*/
static void add_point(P,E,pointarr,npoints)
CNpointptr P;
CNelemptr  E;
CNlocpoint *pointarr;
int       npoints;
{
   CNinsert_elist(&(pointarr[P->flag].elisthead),
                  &(pointarr[P->flag].elisttail),
                  E);
}

/*
 * Add a pt-segm to the list, but only if the pt-segm does not already
 * exist.  The check is to see if both of the elements attached to the
 * pt-segm has already been flagged; if that is true, then the pt-segm must
 * already be in the pt-segm list.
 */
/*ARGSUSED*/
static void add_ptseg(ptseghead,ptsegtail,p1,p2,ptsegID,pointarr,npoints)
CNptsegptr *ptseghead, *ptsegtail;
CNpointptr p1, p2;
int        *ptsegID;
CNlocpoint *pointarr;
int        npoints;
{
   CNelemptr elem_arr[2];
   CNptsegptr sg;
   int       i, nelem = 0;

   /*
    * shared_elements() is called to find the number of elements
    * shared by each ptseg - this is used as a check for adding ptsegs
    */
   nelem = shared_elements(elem_arr, p1, p2, pointarr, npoints);
   if (!shared_elem_is_flagged(elem_arr, nelem)) {
      /* Insert the ptsegment to the list */
      sg = CNinsert_ptseg(ptseghead,ptsegtail, p1, p2, (*ptsegID)++);

      /* Add neighboring element info to the segment */
      sg->numelem = nelem;
      for (i=0; i<nelem && i<2; i++)
         sg->nbrelem[i] = elem_arr[i];
   }
}


/*
 * Find the triangle/rectangle elements shared by two points (a segm)
 */
/*ARGSUSED*/
static int shared_elements(elem_arr,p1,p2,pointarr,npoints)
CNelemptr *elem_arr;
CNpointptr p1, p2;
CNlocpoint *pointarr;
int        npoints;
{
   int nelem=0, k;
   int FOUND = CN_FALSE;
   CNelistptr ELA, ELB;
   CNelemptr  EA,  EB;

   for (ELA=pointarr[p1->flag].elisthead; ELA!=NULL; ELA=ELA->next) 
   for (ELB=pointarr[p2->flag].elisthead; ELB!=NULL; ELB=ELB->next) {
      EA = ELA->E;
      EB = ELB->E;
      if (EA == EB) {
         /*
          * This element is shared by both points
          * Check the current array to see if the element is
          * already in the list
          */
         FOUND = CN_FALSE;
         for (k=0; k<nelem && !FOUND; k++)
            if (elem_arr[k] == EA) FOUND = CN_TRUE;
         if (!FOUND && nelem<2) elem_arr[nelem++] = EA;
      }
   }

#ifdef DEBUG
   /* Print out */
   if (nelem > 0) {
      (void) fprintf(stdout,"Points %d and %d share %d tria/rect elements: (",
              p1->ID,p2->ID,nelem);
      for (i=0; i<nelem; i++)
         if ((elem_arr[i]->type == CN_TRIA_STR) && (elem_arr[i]->tria != NULL))
         (void) fprintf(stdout,"T%d ", elem_arr[i]->tria->ID);
         else if ((elem_arr[i]->type == CN_RECT_STR) && 
                 (elem_arr[i]->rect != NULL))
         (void) fprintf(stdout,"R%d ", elem_arr[i]->rect->ID);
      (void) fprintf(stdout,")\n");
   }
#endif

   /* Return the number of shared elements */
   return (nelem);
}


/*
 * Go through the elements shared by a segm and see if one of
 * the elements has been flagged.
 */
static int shared_elem_is_flagged(elem_arr,nelem)
CNelemptr *elem_arr;
int       nelem;
{
   int flag = CN_FALSE;
   int i;

   /* Check the number of shared elements - valid range : 0 - 2 */
   if (nelem > 2) {
      (void) fprintf(stderr,
      "Warning - found more than 2 adjacent triangle/rectangle elements!\n");
      nelem = 2;
   }

   for (i=0; i<nelem && !flag; i++) {
      if (elem_arr[i]->flag == CN_TRUE) flag = CN_TRUE;
   }

   return(flag);
}


/***************************************************/
/*********  FIND SEGMENTS BOUNDING REGIONS  ********/
/***************************************************/

/*
 * Search for the segments on the boundary of adjoining regions
 */
/*ARGSUSED*/
void CNfind_boundary_ptsegs(ptseghead, ptsegtail)
CNptsegptr ptseghead, ptsegtail;
{
   CNptsegptr S;
   int region[2];

   /* Go through a point-segment list and find the elements on the boundary */
   for (S=ptseghead; S!=NULL; S=S->next) {
      S->boundary = CN_FALSE;

      /* Find out if the segment is a boundary segment */
      if (S->numelem == 0) 
         (void) fprintf(stderr,
         "Error - Segment %d has no tria/rect elements!\n",S->ID);

      else if (S->numelem == 1)
         /* This is a boundary segment - a tria/rect element on the edge */
         S->boundary = CN_TRUE;

      else if (S->numelem == 2) {
         region[0] = regionID_of_element(S->nbrelem[0]);
         region[1] = regionID_of_element(S->nbrelem[1]);
         if (region[0] != region[1]) {
            /* This is a boundary segment - two adjoining regions */
            S->boundary = CN_TRUE;
         }
      }
   }
}


/***************************************************/
/*********  FIND POLYGON BOUNDING A REGION  ********/
/***************************************************/

/*
 * Generate a polygon list of all the boundaries of a region
 * The elements contained in the ptseg list belong to the parent.
 * If this is a quantity-derived dataset then we want to use the
 * elements in the quantity, which should have the same element ID's
 */
/*ARGSUSED*/
void CNfind_boundary_polys(R,
                           ptseghead,  ptsegtail, 
                           elemhead,   elemtail,
                           triahead,   triatail,
                           recthead,   recttail,
                           rematch_elems)
CNregionptr R;
CNptsegptr  ptseghead,  ptsegtail;
CNelemptr   elemhead,   elemtail;
CNtriaptr   triahead,   triatail;
CNrectptr   recthead,   recttail;
int         rematch_elems;
{
   /* Find the region's true boundary */
   find_boundary_polys_by_regionID(ptseghead, ptsegtail, 
                                   elemhead, elemtail,
                                   triahead, triatail,
                                   recthead, recttail,
                                   &(R->polyhead), &(R->polytail), 
                                   R->ID,
                                   rematch_elems);

   /* Find the material boundary */
   find_boundary_polys_by_regionname(ptseghead, ptsegtail, 
                                   elemhead, elemtail,
                                   triahead, triatail,
                                   recthead, recttail,
                                   &(R->matpolyhead), &(R->matpolytail), 
                                   R->ID, R->matname,
                                   rematch_elems);
}


/*
 * Generate a polygon list of all the boundaries of a region
 * The elements contained in the ptseg list belong to the parent.
 * If this is a quantity-derived dataset then we want to use the
 * elements in the quantity, which should have the same element ID's
 *
 * Do the sorting by regionID
 */
/*ARGSUSED*/
static void 
find_boundary_polys_by_regionID(ptseghead, ptsegtail, 
                                elemhead, elemtail,
                                triahead, triatail,
                                recthead, recttail,
                                polyhead, polytail, regID, rematch_elems)
CNptsegptr ptseghead, ptsegtail;
CNelemptr  elemhead, elemtail;
CNtriaptr  triahead, triatail;
CNrectptr  recthead, recttail;
CNpolyptr *polyhead, *polytail;
int        regID;
int        rematch_elems;
{
   CNptsegptr P;
   CNsegmptr  segmhead=NULL, segmtail=NULL;
   CNtriaptr  T, T2;
   CNrectptr  R, R2;
   CNelemptr  E;
   CNnodeptr  ndarr[10];
   int        nnodes=0;
   CNtriaptr  *triaarr=NULL;
   int        trIDmax=0, trIDmin=0;
   CNrectptr  *rectarr=NULL;
   int        rcIDmax=0, rcIDmin=0;
   int        ierr=0;

   if (rematch_elems) {
      /*
      (void) printf("rematching elements...\n");
       */

      /* Create a temp array to store the triangles */
      triaarr = create_tria_array(triahead,triatail,&trIDmax,&trIDmin);
      if (triaarr != NULL) {
         /* Copy the triangles to the array */
         for (T=triahead; T!=NULL; T=T->next)
            if ((T->ID >= trIDmin) && (T->ID <= trIDmax))
               triaarr[T->ID - trIDmin] = T;
      }

      /* Create a temp array to store the rectangles */
      rectarr = create_rect_array(recthead,recttail,&rcIDmax,&rcIDmin);
      if (rectarr != NULL) { 
         /* Copy the elements to the array */
         for (R=recthead; R!=NULL; R=R->next)
            if ((R->ID >= rcIDmin) && (R->ID <= rcIDmax))
               rectarr[R->ID - rcIDmin] = R;
      }

      /* If there are neither rectangles or triangles, get out */
      if (rectarr==NULL && triaarr==NULL) return;
   }

   /* 
    * Go thru each point-segment and search for the elements in the
    * region of interest
    */
   for (P=ptseghead; P!=NULL; P=P->next) {
      /* Act only on the boundary segments */
      if (P->boundary == CN_FALSE) continue;

      /* Select the element that matches the region ID */
      E = NULL;
      if ((P->numelem == 1) && (regionID_of_element(P->nbrelem[0]) == regID)) {
         E = P->nbrelem[0];
      } else if ((P->numelem == 2) && 
                 (regionID_of_element(P->nbrelem[0]) == regID)) {
         E = P->nbrelem[0];
      } else if ((P->numelem == 2) && 
                 (regionID_of_element(P->nbrelem[1]) == regID)) {
         E = P->nbrelem[1];
      } 

      /* Should end up with either T or R with a NULL value */
      T = NULL;
      R = NULL;
      if (E != NULL) {
         T = (E->type == CN_TRIA_STR) ? E->tria : NULL;
         if (T!=NULL && triaarr!=NULL && rematch_elems) {
            if ((T->ID < trIDmin) && (T->ID > trIDmax))
               elem_range_err("tria",T->ID,trIDmin,trIDmax,ierr++);
            else if ((T2 = triaarr[T->ID-trIDmin]) == NULL)
               elem_index_err("tria",T->ID,ierr++);
            else
               T = T2;
         }
         R = (E->type == CN_RECT_STR) ? E->rect : NULL;
         if (R!=NULL && rectarr!=NULL && rematch_elems) {
            if ((R->ID < trIDmin) && (R->ID > rcIDmax))
               elem_range_err("rect",R->ID,rcIDmin,rcIDmax,ierr++);
            else if ((R2 = rectarr[R->ID-rcIDmin]) == NULL)
               elem_index_err("rect",R->ID,ierr++);
            else
               R = R2;
         }
      }

      if (T!=NULL || R!=NULL) {
         nnodes=0;
         if (T!=NULL) {
            /* Find nodes on the tria that match the points on the segment */
            if (T->n1->coord == P->p1 || T->n1->coord == P->p2)
               ndarr[nnodes++] = T->n1;
            if (T->n2->coord == P->p1 || T->n2->coord == P->p2)
               ndarr[nnodes++] = T->n2;
            if (T->n3->coord == P->p1 || T->n3->coord == P->p2)
               ndarr[nnodes++] = T->n3;
         } else if (R!=NULL) {
            /* Find nodes on the rect that match the points on the segment */
            if (R->n1->coord == P->p1 || R->n1->coord == P->p2)
               ndarr[nnodes++] = R->n1;
            if (R->n2->coord == P->p1 || R->n2->coord == P->p2)
               ndarr[nnodes++] = R->n2;
            if (R->n3->coord == P->p1 || R->n3->coord == P->p2)
               ndarr[nnodes++] = R->n3;
            if (R->n4->coord == P->p1 || R->n4->coord == P->p2)
               ndarr[nnodes++] = R->n4;
         }
         if (nnodes != 2) {
            (void) fprintf(stderr,
            "Warning - found incorrect no of nodes(%d, need 2)!\n",nnodes);
         } else {
            (void) CNinsert_segm(&segmhead, &segmtail, 
                                 ndarr[0], ndarr[1], regID);
         }
      }   
   }

   /* 
    * Now arrange the segments in a polygon 
    */
   sort_segms(&segmhead, &segmtail, polyhead, polytail, regID);

   /*
    * Free the local array
    */
   if (triaarr) free((char *)triaarr);
   if (rectarr) free((char *)rectarr);

}

/*
 * Generate a polygon list of all the boundaries of a region
 * The elements contained in the ptseg list belong to the parent.
 * If this is a quantity-derived dataset then we want to use the
 * elements in the quantity, which should have the same element ID's
 *
 * Do the sorting by regionname
 */
/*ARGSUSED*/
static void 
find_boundary_polys_by_regionname(ptseghead, ptsegtail, 
                                  elemhead, elemtail,
                                  triahead, triatail,
                                  recthead, recttail,
                                  polyhead, polytail, 
                                  regID, regname, rematch_elems)
CNptsegptr ptseghead, ptsegtail;
CNelemptr  elemhead, elemtail;
CNtriaptr  triahead, triatail;
CNrectptr  recthead, recttail;
CNpolyptr *polyhead, *polytail;
int        regID;
char      *regname;
int        rematch_elems;
{
   CNptsegptr P;
   CNsegmptr  segmhead=NULL, segmtail=NULL;
   CNtriaptr  T, T2;
   CNrectptr  R, R2;
   CNelemptr  E;
   CNnodeptr  ndarr[10];
   int        nnodes=0;
   CNtriaptr  *triaarr=NULL;
   int        trIDmax=0, trIDmin=0;
   CNrectptr  *rectarr=NULL;
   int        rcIDmax=0, rcIDmin=0;
   int        ierr=0;

   if (rematch_elems) {
      /*
      (void) printf("rematching elements...\n");
       */

      /* Create a temp array to store the triangles */
      triaarr = create_tria_array(triahead,triatail,&trIDmax,&trIDmin);
      if (triaarr != NULL) {
         /* Copy the triangles to the array */
         for (T=triahead; T!=NULL; T=T->next)
            if ((T->ID >= trIDmin) && (T->ID <= trIDmax))
               triaarr[T->ID - trIDmin] = T;
      }
 
      /* Create a temp array to store the rectangles */
      rectarr = create_rect_array(recthead,recttail,&rcIDmax,&rcIDmin);
      if (rectarr != NULL) {
         /* Copy the elements to the array */
         for (R=recthead; R!=NULL; R=R->next)
            if ((R->ID >= rcIDmin) && (R->ID <= rcIDmax))
               rectarr[R->ID - rcIDmin] = R;
      }
 
      /* If there are neither rectangles or triangles, get out */
      if (rectarr==NULL && triaarr==NULL) return;
   }

   /* 
    * Go thru each point-segment and search for the elements in the
    * region of interest
    */
   for (P=ptseghead; P!=NULL; P=P->next) {
      /* Act only on the boundary segments */
      if (P->boundary == CN_FALSE) continue;

      /* Filter out segments with elements which share the same matnames */
      if ((P->numelem == 2) && 
          (same_material(regionptr_of_element(P->nbrelem[0]), regname)) &&
          (same_material(regionptr_of_element(P->nbrelem[1]), regname)))
         continue;

      /* Select the element that matches the region ID */
      E = NULL;
      if ((P->numelem == 1) && 
                 (same_material(regionptr_of_element(P->nbrelem[0]), regname))){
         E = P->nbrelem[0];
      } else if ((P->numelem == 2) &&
                 (same_material(regionptr_of_element(P->nbrelem[0]), regname))){
         E = P->nbrelem[0];
      } else if ((P->numelem == 2) &&
                 (same_material(regionptr_of_element(P->nbrelem[1]), regname))){
         E = P->nbrelem[1];
      }

      /* Should end up with either T or R with a NULL value */
      T = NULL;
      R = NULL;
      if (E != NULL) {
         T = (E->type == CN_TRIA_STR) ? E->tria : NULL;
         if (T!=NULL && triaarr!=NULL && rematch_elems) {
            if ((T->ID < trIDmin) && (T->ID > trIDmax))
               elem_range_err("tria",T->ID,trIDmin,trIDmax,ierr++);
            else if ((T2 = triaarr[T->ID-trIDmin]) == NULL)
               elem_index_err("tria",T->ID,ierr++);
            else
               T = T2;
         }
         R = (E->type == CN_RECT_STR) ? E->rect : NULL;
         if (R!=NULL && rectarr!=NULL && rematch_elems) {
            if ((R->ID < trIDmin) && (R->ID > rcIDmax))
               elem_range_err("rect",R->ID,rcIDmin,rcIDmax,ierr++);
            else if ((R2 = rectarr[R->ID-rcIDmin]) == NULL)
               elem_index_err("rect",R->ID,ierr++);
            else
               R = R2;
         }
      }
 
      if (T!=NULL || R!=NULL) {
         nnodes=0;
         if (T!=NULL) {
            /* Find nodes on the tria that match the points on the segment */
            if (T->n1->coord == P->p1 || T->n1->coord == P->p2)
               ndarr[nnodes++] = T->n1;
            if (T->n2->coord == P->p1 || T->n2->coord == P->p2)
               ndarr[nnodes++] = T->n2;
            if (T->n3->coord == P->p1 || T->n3->coord == P->p2)
               ndarr[nnodes++] = T->n3;
         } else if (R!=NULL) {
            /* Find nodes on the rect that match the points on the segment */
            if (R->n1->coord == P->p1 || R->n1->coord == P->p2)
               ndarr[nnodes++] = R->n1;
            if (R->n2->coord == P->p1 || R->n2->coord == P->p2)
               ndarr[nnodes++] = R->n2;
            if (R->n3->coord == P->p1 || R->n3->coord == P->p2)
               ndarr[nnodes++] = R->n3;
            if (R->n4->coord == P->p1 || R->n4->coord == P->p2)
               ndarr[nnodes++] = R->n4;
         }
         if (nnodes != 2) {
            (void) fprintf(stderr,
            "Warning - found incorrect no of nodes(%d, need 2)!\n",nnodes);
         } else {
            (void) CNinsert_segm(&segmhead, &segmtail, 
                                 ndarr[0], ndarr[1], regID);
         }
      }   
   }

   /* 
    * Now arrange the segments in a polygon 
    */
   sort_segms(&segmhead, &segmtail, polyhead, polytail, regID);

   /*
    * Free the local array
    */
   if (triaarr) free((char *)triaarr);
   if (rectarr) free((char *)rectarr);

}


/*
 * Check that the material names are identical
 */
static int same_material(R, matname)
CNregionptr R;
char        *matname;
{
   /* Error check */
   if ((R == NULL) || (R->matname == NULL) || (matname == NULL)) 
      return(CN_FALSE);

   if (strcmp(R->matname, matname) == 0) 
      return(CN_TRUE);
   else
      return(CN_FALSE);
}

      
/*
 * sort out the list of segments to form a joined curve
 */
static void sort_segms(segmhead,segmtail,polyhead,polytail,regID)
CNsegmptr *segmhead, *segmtail;
CNpolyptr  *polyhead, *polytail;
int        regID;
{
 
   CNsegmptr  S;
   CNnodeptr  n1, n2;
   CNnlistptr nlisthead=NULL, nlisttail=NULL;
   int        pID=0;
 
   while ((S = *segmhead)!=NULL) {
      nlisthead = NULL;
      nlisttail = NULL;
      n1 = S->n1;
      n2 = S->n2;
      CNinsert_tailnlist(&nlisthead, &nlisttail, n1);
      CNinsert_tailnlist(&nlisthead, &nlisttail, n2);
      CNdelete_segm(segmhead, segmtail, S);
      add_to_taillist(segmhead, segmtail, n2, &nlisthead, &nlisttail);
      add_to_headlist(segmhead, segmtail, n1, &nlisthead, &nlisttail);
      (void) CNinsert_poly(polyhead, polytail, 
                           nlisthead, nlisttail, regID, pID++);
   }
}
 
/*
 * add to the tail of the data list
 */
static void
add_to_taillist(segm_listhead, segm_listtail, nd, nlisthead, nlisttail)
CNsegmptr *segm_listhead, *segm_listtail;
CNnodeptr  nd;
CNnlistptr *nlisthead, *nlisttail;
{
   CNsegmptr S;
 
   /* add the further node at every loop iteration */
   while ((S=matching_segm(segm_listhead,nd))!=NULL) {
      if (S->n1 == nd) {
         CNinsert_tailnlist(nlisthead, nlisttail, S->n2);
         nd = S->n2;
      } else {
         CNinsert_tailnlist(nlisthead, nlisttail, S->n1);
         nd = S->n1;
      }
      CNdelete_segm(segm_listhead, segm_listtail, S);
   }
   return;
}

/*
 * add to the head of the data list
 */
static void
add_to_headlist(segm_listhead, segm_listtail, nd, nlisthead, nlisttail)
CNsegmptr *segm_listhead, *segm_listtail;
CNnodeptr  nd;
CNnlistptr *nlisthead, *nlisttail;
{
   CNsegmptr S;
 
   /* add the further node at every loop iteration */
   while ((S=matching_segm(segm_listhead,nd))!=NULL) {
      if (S->n1 == nd) {
         CNinsert_headnlist(nlisthead, nlisttail, S->n2);
         nd = S->n2;
      } else {
         CNinsert_headnlist(nlisthead, nlisttail, S->n1);
         nd = S->n1;
      }
      CNdelete_segm(segm_listhead, segm_listtail, S);
   }
   return;
}

/*
 * Return segm containing node that matches a given node
 * We are looking for the segment that is attached to the current
 * segment
 */
static CNsegmptr matching_segm(segm_listhead,nd)
CNsegmptr *segm_listhead;
CNnodeptr  nd;
{
   CNsegmptr  S,SF=NULL;
   int        FOUND = CN_FALSE;
 
   /* check the segments in the list */
   for (S=(*segm_listhead); S!=NULL && !FOUND; S=S->next) {
      /* check the nodes on either end of the segment */
      if ((S->n1 == nd) || (S->n2 == nd)) {
         FOUND = CN_TRUE;
      }
      if (FOUND) SF = S;
   }
 
   /* return the segm-ptr */
   if (!FOUND) SF = NULL;
   return(SF);
}


/*
 * Utility functions for matching elements 
 */

/* 
 * Return the region ID of the element
 */
static int regionID_of_element(elem)
CNelemptr elem;
{
    int region;

    if ((elem->type == CN_TRIA_STR) && (elem->tria != NULL)) 
       region = elem->tria->region;
    else if ((elem->type == CN_RECT_STR) && (elem->rect != NULL)) 
       region = elem->rect->region;
    else
       region = -1;

    /* return */
    return region;
}

/* 
 * Return the region pointer of the element
 */
static CNregionptr regionptr_of_element(elem)
CNelemptr elem;
{
    CNregionptr region;

    if ((elem->type == CN_TRIA_STR) && (elem->tria != NULL)) 
       region = elem->tria->R;
    else if ((elem->type == CN_RECT_STR) && (elem->rect != NULL)) 
       region = elem->rect->R;
    else
       region = NULL;

    /* return */
    return region;
}

/*
 * Create a temp array to store the trias
 */
/*ARGSUSED*/
static CNtriaptr *create_tria_array(triahead,triatail,tmax,tmin)
CNtriaptr triahead, triatail;
int       *tmax, *tmin;
{
   CNtriaptr T, *trarr=NULL;
   int       trIDmin, trIDmax, trrange;
   int       i;
   unsigned  int size;
 
   if (triahead == NULL) return(NULL);
 
   /* Go thru the trialist and check for max ID */
   trIDmin = triahead->ID;
   trIDmax = triahead->ID;
   for (T=triahead; T!=NULL; T=T->next) {
      if (T->ID < trIDmin) trIDmin = T->ID;
      if (T->ID > trIDmax) trIDmax = T->ID;
   }
 
   trrange = trIDmax - trIDmin + 1;
   if (trrange <= 0) {
   (void) fprintf(stderr,
         "Error - The specified range of %s [%d, %d] is too small! (min=%d)!\n",
          "trias", trIDmin, trIDmax, 0);
          return(NULL);
   } else if (trrange > CN_MAXOBJS) {
   (void) fprintf(stderr,
         "Error - The specified range of %s [%d, %d] is too large! (max=%d)!\n",
          "trias", trIDmin, trIDmax, CN_MAXOBJS);
          return(NULL);
   }
 
   /* Calculate the storage requirements */
   size    = (unsigned int)(trrange*sizeof(CNtriaptr));
 
   /* Allocate temp storage space for trias */
   if ((trarr = (CNtriaptr *)malloc(size))==NULL) {
      (void) fprintf(stderr,"Couldn't allocate space for triaptr array\n");
      return(NULL);
   }
 
   /* Fill the array */
   for (i=0; i<trrange; i++) trarr[i] = NULL;
 
   /* Return */
   *tmax = trIDmax;
   *tmin = trIDmin;
   return(trarr);
}
 
/*
 * Create a temp array to store the rects
 */
/*ARGSUSED*/
static CNrectptr *create_rect_array(recthead,recttail,rmax,rmin)
CNrectptr recthead, recttail;
int       *rmax, *rmin;
{
   CNrectptr R, *rcarr=NULL;
   int       rcIDmin, rcIDmax, rcrange;
   int       i;
   unsigned  int size;
 
   if (recthead == NULL) return(NULL);
 
   /* Go thru the rectlist and check for max ID */
   rcIDmin = recthead->ID;
   rcIDmax = recthead->ID;
   for (R=recthead; R!=NULL; R=R->next) {
      if (R->ID < rcIDmin) rcIDmin = R->ID;
      if (R->ID > rcIDmax) rcIDmax = R->ID;
   }
 
   rcrange = rcIDmax - rcIDmin + 1;
   if (rcrange <= 0) {
   (void) fprintf(stderr,
         "Error - The specified range of %s [%d, %d] is too small! (min=%d)!\n",
          "rects", rcIDmin, rcIDmax, 0);
          return(NULL);
   } else if (rcrange > CN_MAXOBJS) {
   (void) fprintf(stderr,
         "Error - The specified range of %s [%d, %d] is too large! (max=%d)!\n",
          "rects", rcIDmin, rcIDmax, CN_MAXOBJS);
          return(NULL);
   }
 
   /* Calculate the storage requirements */
   size    = (unsigned int)(rcrange*sizeof(CNrectptr));
 
   /* Allocate temp storage space for rects */
   if ((rcarr = (CNrectptr *)malloc(size))==NULL) {
      (void) fprintf(stderr,"Couldn't allocate space for rectptr array\n");
      return(NULL);
   }
 
   /* Fill the array */
   for (i=0; i<rcrange; i++) rcarr[i] = NULL;
 
   /* Return */
   *rmax = rcIDmax;
   *rmin = rcIDmin;
   return(rcarr);
}
 
/*
 * The element is out of array range
 */
/*ARGSUSED*/
static void elem_range_err(element, ID, minID, maxID, ierr)
char *element;
int   ID, minID, maxID, ierr;
{
   (void) fprintf(stderr,
           "Error - The %s %d is not in the valid range [%d, %d]\n",
           element, ID, minID, maxID);
}
 
/*
 * The element does not exist
 */
/*ARGSUSED*/
static void elem_index_err(element, ID, ierr)
char *element;
int   ID, ierr;
{
   (void) fprintf(stderr,"Error - The %s %d has not been defined!\n",
           element, ID);
}


/******************************************************/
/*********  FIND POLYGON BOUNDING 2 MATERIALS  ********/
/******************************************************/

/*
 * Find the boundaries of 2 adjoining materials
 * (multiple regions might contain similar materials)
 * This routine works on mesh-datasets as well as quantity datasets.
 * However quantity datasets need a valid parent...
 */
void CNmat_boundary(dptr,mat1,mat2,polyhead,polytail)
CNdatasetptr dptr;
char *mat1, *mat2;
CNpolyptr *polyhead, *polytail;
{
   CNdatasetptr dmesh;
   CNregionptr  Rg;
   CNptsegptr   P;
   CNsegmptr    segmhead=NULL, segmtail=NULL;
   CNelemptr    E;
   CNtriaptr    T, T2;
   CNrectptr    R, R2;
   CNtriaptr    *triaarr=NULL;
   int          trIDmax=0, trIDmin=0;
   CNrectptr    *rectarr=NULL;
   int          rcIDmax=0, rcIDmin=0;
   int          ierr=0;
   int          reg1ID;
   int          reg2ID;
   int          rematch_elems=CN_FALSE;
   int          MAT1_FOUND=CN_TRUE, MAT2_FOUND=CN_TRUE;
   CNnodeptr    ndarr[10];
   int          nnodes=0;

   /* Error checking */
   if (dptr == NULL) {
      (void) fprintf(stderr,
      "CNmat_boundary : Error - Null dataset!\n");
      return;
   }
   if (dptr->datatype == CN_PIF_PARENT)
      dmesh = dptr;
   else if ((dmesh=dptr->parent) == NULL) {
      (void) fprintf(stderr,
      "CNmat_boundary : Error - Null parent dataset!\n");
      return;
   }
   if (dptr->elemhead == NULL) {
      (void) fprintf(stderr,
      "CNmat_boundary : Error - No tria/rect elements in dataset!\n");
      return;
   }
   if (dmesh->pointhead == NULL) {
      (void) fprintf(stderr,
      "CNmat_boundary : Error - No points in parent dataset!\n");
      return;
   }

   /* Generate a point-segment list for the mesh */
   if (dmesh->ptseghead == NULL) {
   CNgenerate_ptseg_list(dmesh->elemhead,  dmesh->elemtail,
                         dmesh->pointhead, dmesh->pointtail,
                         &(dmesh->ptseghead), &(dmesh->ptsegtail));
   }
   
   /*
    * The point-segment list is connected to the elements of the
    * mesh.  These need to be matched to the quantity's elements.
    */
   if (dptr != dmesh) rematch_elems = CN_TRUE;
   if (rematch_elems) {
      /* Create a temp array to store the elements */
      triaarr = create_tria_array(dptr->triahead,dptr->triatail,
                                  &trIDmax,&trIDmin);
      if (triaarr != NULL) {
         /* Copy the triangles to the array */
         for (T=dptr->triahead; T!=NULL; T=T->next)
            if ((T->ID >= trIDmin) && (T->ID <= trIDmax))
               triaarr[T->ID - trIDmin] = T;
      }
 
      /* Create a temp array to store the rectangles */
      rectarr = create_rect_array(dptr->recthead,dptr->recttail,
                                  &rcIDmax,&rcIDmin);
      if (rectarr != NULL) {
         /* Copy the rectangles to the array */
         for (R=dptr->recthead; R!=NULL; R=R->next)
            if ((R->ID >= rcIDmin) && (R->ID <= rcIDmax))
               rectarr[R->ID - rcIDmin] = R;
      }
 
      /* If there are neither rectangles or triangles, get out */
      if (rectarr==NULL && triaarr==NULL) return;
   }

#ifdef DEBUG
   nnodes = 0;
   for (P=dmesh->ptseghead; P!=NULL; P=P->next) 
      if (P->boundary == CN_TRUE) nnodes++;
   (void) printf("Found %d boundary segments\n",nnodes);
   nnodes = 0;
   for (P=dmesh->ptseghead; P!=NULL; P=P->next) 
      if (P->boundary == CN_TRUE && P->numelem==2) nnodes++;
   (void) printf("Found %d double-elem boundary segments\n",nnodes);
#endif

   /* 
    * Go thru the segment list and find the segments that match
    * the two material descriptions
    */
   for (P=dmesh->ptseghead; P!=NULL; P=P->next) {
      /* Act only on the boundary segments */
      if (P->boundary == CN_FALSE) continue;

      /* Need 2 adjoining regions */
      if (P->numelem != 2) continue;

      reg1ID = regionID_of_element(P->nbrelem[0]);
      reg2ID = regionID_of_element(P->nbrelem[1]);

      /* Go thru the regions and find regions that match the regID and mat */
      MAT1_FOUND = CN_FALSE;
      MAT2_FOUND = CN_FALSE;
      for (Rg=dptr->regionhead; Rg!=NULL; Rg=Rg->next) {
         if ((Rg->ID == reg1ID) && (strcmp(Rg->matname,mat1)==0)) {
            /*
            reg1ID = reg1ID;
             */
            MAT1_FOUND = CN_TRUE;
         } else if ((Rg->ID == reg2ID) && (strcmp(Rg->matname,mat1)==0)){
            reg1ID = reg2ID;
            MAT1_FOUND = CN_TRUE;
         } else if ((Rg->ID == reg1ID) && (strcmp(Rg->matname,mat2)==0)){
            /*
            reg2ID = reg1ID;
             */
            MAT2_FOUND = CN_TRUE;
         } else if ((Rg->ID == reg2ID) && (strcmp(Rg->matname,mat2)==0)){
            /*
            reg2ID = reg2ID;
             */
            MAT2_FOUND = CN_TRUE;
         }
      }

      /* Success!  Add the points to the node-segment list */
      if (MAT1_FOUND && MAT2_FOUND) {
         /* E is the element in material 1 */
         if (regionID_of_element(P->nbrelem[0]) == reg1ID) {
            E = P->nbrelem[0];
         } else {
            E = P->nbrelem[1];
         }
         if (E!=NULL && rematch_elems) {

            /* Should end up with either T or R with a NULL value */
            T = NULL;
            R = NULL;
            if (E != NULL) {
               T = (E->type == CN_TRIA_STR) ? E->tria : NULL;
               if (T!=NULL && triaarr!=NULL && rematch_elems) {
                  if ((T->ID < trIDmin) && (T->ID > trIDmax))
                     elem_range_err("tria",T->ID,trIDmin,trIDmax,ierr++);
                  else if ((T2 = triaarr[T->ID-trIDmin]) == NULL)
                     elem_index_err("tria",T->ID,ierr++);
                  else
                     T = T2;
               }
               R = (E->type == CN_RECT_STR) ? E->rect : NULL;
               if (R!=NULL && rectarr!=NULL && rematch_elems) {
                  if ((R->ID < trIDmin) && (R->ID > rcIDmax))
                     elem_range_err("rect",R->ID,rcIDmin,rcIDmax,ierr++);
                  else if ((R2 = rectarr[R->ID-rcIDmin]) == NULL)
                     elem_index_err("rect",R->ID,ierr++);
                  else
                     R = R2;
               }
            }
            if (T!=NULL || R!=NULL) {
               nnodes=0;
               if (T!=NULL) {
                  /* Find nodes on the tria that match points on the segment */
                  if (T->n1->coord == P->p1 || T->n1->coord == P->p2)
                     ndarr[nnodes++] = T->n1;
                  if (T->n2->coord == P->p1 || T->n2->coord == P->p2)
                     ndarr[nnodes++] = T->n2;
                  if (T->n3->coord == P->p1 || T->n3->coord == P->p2)
                     ndarr[nnodes++] = T->n3;
               } else if (R!=NULL) {
                  /* Find nodes on the rect that match points on the segment */
                  if (R->n1->coord == P->p1 || R->n1->coord == P->p2)
                     ndarr[nnodes++] = R->n1;
                  if (R->n2->coord == P->p1 || R->n2->coord == P->p2)
                     ndarr[nnodes++] = R->n2;
                  if (R->n3->coord == P->p1 || R->n3->coord == P->p2)
                     ndarr[nnodes++] = R->n3;
                  if (R->n4->coord == P->p1 || R->n4->coord == P->p2)
                     ndarr[nnodes++] = R->n4;
               }

               if (nnodes != 2) {
                  (void) fprintf(stderr,
                  "Warning - found incorrect no of nodes(%d, need 2)!\n",
                  nnodes);
               } else {
                  (void) CNinsert_segm(&segmhead, &segmtail, 
                                       ndarr[0], ndarr[1], reg1ID);
               }
            }
         }
      }   
   }

   /* 
    * Now arrange the segments in a polygon 
    */
   sort_segms(&segmhead, &segmtail, polyhead, polytail, reg1ID);

#ifdef DEBUG
   /* print out the segment list */
   CNprint_segm_list(segmhead, segmtail);

   /* print out the polygon list */
   CNprint_poly_list(*polyhead, *polytail);
#endif

   /*
    * Free the local array
    */
   if (triaarr) free((char *)triaarr);
   if (rectarr) free((char *)rectarr);
}



/****************************************************/
/*********  FIND POLYGON BOUNDING A MATERIAL ********/
/****************************************************/

/*
 * Find the exposed boundary of a material
 * i.e. the boundary of the material which does NOT touch adjacent materials.
 * (multiple regions might contain similar materials)
 * This routine works on mesh-datasets as well as quantity datasets.
 * However quantity datasets need a valid parent...
 */
void CNmat_exp_boundary(dptr,mat1,polyhead,polytail)
CNdatasetptr dptr;
char *mat1;
CNpolyptr *polyhead, *polytail;
{
   CNdatasetptr dmesh;
   CNregionptr  Rg;
   CNptsegptr   P;
   CNsegmptr    segmhead=NULL, segmtail=NULL;
   CNelemptr    E;
   CNtriaptr    T, T2;
   CNrectptr    R, R2;
   CNtriaptr    *triaarr=NULL;
   int          trIDmax=0, trIDmin=0;
   CNrectptr    *rectarr=NULL;
   int          rcIDmax=0, rcIDmin=0;
   int          ierr=0;
   int          reg1ID;
   int          rematch_elems=CN_FALSE;
   int          MAT1_FOUND=CN_TRUE;
   CNnodeptr    ndarr[10];
   int          nnodes=0;

   /* Error checking */
   if (dptr == NULL) {
      (void) fprintf(stderr,
      "CNmat_exp_boundary : Error - Null dataset!\n");
      return;
   }
   if (dptr->datatype == CN_PIF_PARENT)
      dmesh = dptr;
   else if ((dmesh=dptr->parent) == NULL) {
      (void) fprintf(stderr,
      "CNmat_exp_boundary : Error - Null parent dataset!\n");
      return;
   }
   if (dptr->elemhead == NULL) {
      (void) fprintf(stderr,
      "CNmat_exp_boundary : Error - No tria/rect elements in dataset!\n");
      return;
   }
   if (dmesh->pointhead == NULL) {
      (void) fprintf(stderr,
      "CNmat_exp_boundary : Error - No points in parent dataset!\n");
      return;
   }

   /* Generate a point-segment list for the mesh */
   if (dmesh->ptseghead == NULL) {
   CNgenerate_ptseg_list(dmesh->elemhead,  dmesh->elemtail,
                         dmesh->pointhead, dmesh->pointtail,
                         &(dmesh->ptseghead), &(dmesh->ptsegtail));
   }
   
   /*
    * The point-segment list is connected to the elements of the
    * mesh.  These need to be matched to the quantity's elements.
    */
   if (dptr != dmesh) rematch_elems = CN_TRUE;
   if (rematch_elems) {

      /* Create a temp array to store the elements */
      triaarr = create_tria_array(dptr->triahead,dptr->triatail,
                                  &trIDmax,&trIDmin);
      if (triaarr != NULL) {
         /* Copy the triangles to the array */
         for (T=dptr->triahead; T!=NULL; T=T->next)
            if ((T->ID >= trIDmin) && (T->ID <= trIDmax))
               triaarr[T->ID - trIDmin] = T;
      }
 
      /* Create a temp array to store the rectangles */
      rectarr = create_rect_array(dptr->recthead,dptr->recttail,
                                  &rcIDmax,&rcIDmin);
      if (rectarr != NULL) {
         /* Copy the rectangles to the array */
         for (R=dptr->recthead; R!=NULL; R=R->next)
            if ((R->ID >= rcIDmin) && (R->ID <= rcIDmax))
               rectarr[R->ID - rcIDmin] = R;
      }
 
      /* If there are neither rectangles or triangles, get out */
      if (rectarr==NULL && triaarr==NULL) return;
   }

#ifdef DEBUG
   nnodes = 0;
   for (P=dmesh->ptseghead; P!=NULL; P=P->next) 
      if (P->boundary == CN_TRUE) nnodes++;
   (void) printf("Found %d boundary segments\n",nnodes);
   nnodes = 0;
   for (P=dmesh->ptseghead; P!=NULL; P=P->next) 
      if (P->boundary == CN_TRUE && P->numelem==2) nnodes++;
   (void) printf("Found %d double-elem boundary segments\n",nnodes);
#endif

   /* 
    * Go thru the segment list and find the segments that match
    * the material description
    */
   for (P=dmesh->ptseghead; P!=NULL; P=P->next) {
      /* Act only on the boundary segments */
      if (P->boundary == CN_FALSE) continue;

      /* Need only 1 material (no adjoining elements) */
      if (P->numelem != 1) continue;

      reg1ID = regionID_of_element(P->nbrelem[0]);

      /* Go thru the regions and find regions that match the regID and mat */
      MAT1_FOUND = CN_FALSE;
      for (Rg=dptr->regionhead; Rg!=NULL; Rg=Rg->next) {
         if ((Rg->ID == reg1ID) && (strcmp(Rg->matname,mat1)==0)) {
            /*
            reg1ID = reg1ID;
             */
            MAT1_FOUND = CN_TRUE;
         }
      }

      /* Success!  Add the points to the node-segment list */
      if (MAT1_FOUND) {
         /* E is the element in material 1 */
         E = P->nbrelem[0];

         if (E!=NULL && rematch_elems) {
 
            /* Should end up with either T or R with a NULL value */
            T = NULL;
            R = NULL;
            if (E != NULL) {
               T = (E->type == CN_TRIA_STR) ? E->tria : NULL;
               if (T!=NULL && triaarr!=NULL && rematch_elems) {
                  if ((T->ID < trIDmin) && (T->ID > trIDmax))
                     elem_range_err("tria",T->ID,trIDmin,trIDmax,ierr++);
                  else if ((T2 = triaarr[T->ID-trIDmin]) == NULL)
                     elem_index_err("tria",T->ID,ierr++);
                  else
                     T = T2;
               }
               R = (E->type == CN_RECT_STR) ? E->rect : NULL;
               if (R!=NULL && rectarr!=NULL && rematch_elems) {
                  if ((R->ID < trIDmin) && (R->ID > rcIDmax))
                     elem_range_err("rect",R->ID,rcIDmin,rcIDmax,ierr++);
                  else if ((R2 = rectarr[R->ID-rcIDmin]) == NULL)
                     elem_index_err("rect",R->ID,ierr++);
                  else
                     R = R2;
               }
            }
            if (T!=NULL || R!=NULL) {
               nnodes=0;
               if (T!=NULL) {
                  /* Find nodes on the tria that match points on the segment */
                  if (T->n1->coord == P->p1 || T->n1->coord == P->p2)
                     ndarr[nnodes++] = T->n1;
                  if (T->n2->coord == P->p1 || T->n2->coord == P->p2)
                     ndarr[nnodes++] = T->n2;
                  if (T->n3->coord == P->p1 || T->n3->coord == P->p2)
                     ndarr[nnodes++] = T->n3;
               } else if (R!=NULL) {
                  /* Find nodes on the rect that match points on the segment */
                  if (R->n1->coord == P->p1 || R->n1->coord == P->p2)
                     ndarr[nnodes++] = R->n1;
                  if (R->n2->coord == P->p1 || R->n2->coord == P->p2)
                     ndarr[nnodes++] = R->n2;
                  if (R->n3->coord == P->p1 || R->n3->coord == P->p2)
                     ndarr[nnodes++] = R->n3;
                  if (R->n4->coord == P->p1 || R->n4->coord == P->p2)
                     ndarr[nnodes++] = R->n4;
               }

               if (nnodes != 2) {
                  (void) fprintf(stderr,
                  "Warning - found incorrect no of nodes(%d, need 2)!\n",
                  nnodes);
               } else {
                  (void) CNinsert_segm(&segmhead, &segmtail, 
                                       ndarr[0], ndarr[1], reg1ID);
               }
            }
         }
      }   
   }

   /* 
    * Now arrange the segments in a polygon 
    */
   sort_segms(&segmhead, &segmtail, polyhead, polytail, reg1ID);

#ifdef DEBUG
   /* print out the segment list */
   CNprint_segm_list(segmhead, segmtail);

   /* print out the polygon list */
   CNprint_poly_list(*polyhead, *polytail);
#endif

   /*
    * Free the local array
    */
   if (triaarr) free((char *)triaarr);
   if (rectarr) free((char *)rectarr);
}

