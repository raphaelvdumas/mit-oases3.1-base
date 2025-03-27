/*
 * list5.c - utilities that use linked-list data-structures 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include "CNplot.h"

void   CNgenerate_boundary();
void   CNgenerate_boundary_from_nodes();
void   CNgenerate_segment_list();
static void       add_node();
static void       add_segm();
static int        shared_triangles();
static int        shared_tria_is_flagged();
void   CNfind_boundary_segments();
static void       sort_slist();
static void       add_to_taillist();
static void       add_to_headlist();
static CNslistptr matching_slist(); 

/*********************************************/
/*********  GENERATE A BOUNDARY LINE  ********/
/*********************************************/

void CNgenerate_boundary(dptr, verbose)
CNdatasetptr dptr;
int          verbose;
{
   CNgenerate_boundary_from_points(dptr, verbose);
}

/*
 * Generate the boundaries of regions from nodes
 * This routine is obsolete - CNgenerate_boundary_from_points()
 * should be used instead as that routine has greater flexibility
 *
 * This routine works on mesh-datasets as well as quantity datasets.
 */
void CNgenerate_boundary_from_nodes(dptr, verbose)
CNdatasetptr dptr;
int          verbose;
{
   CNregionptr R;

   /* Error checking */
   if (dptr == NULL) {
      (void) fprintf(stderr,
      "CNgenerate_boundary_from_nodes : Error - Null dataset!\n");
      return;
   }
   if (dptr->triahead == NULL) {
      (void) fprintf(stderr,
      "CNgenerate_boundary_from_nodes : Error - No triangles in dataset!\n");
      return;
   }
   if (dptr->nodehead == NULL) {
      (void) fprintf(stderr,
      "CNgenerate_boundary_from_nodes : Error - No nodes in dataset!\n");
      return;
   }

   /* Generate a new segment list if necessary */
   if (dptr->segmhead == NULL) 
   CNgenerate_segment_list(dptr->triahead, dptr->triatail,
                           dptr->nodehead, dptr->nodetail,
                           &(dptr->segmhead), &(dptr->segmtail));
   
   /* Find the segments corresponding to the various regions */
   for (R=dptr->regionhead; R!=NULL; R=R->next) {
      /* Do only those regions that have not been treated yet */
      if (R->polyhead==NULL) {
         if (verbose) CNprint_region(R);
         CNfind_boundary_segments(dptr->segmhead, dptr->segmtail,
                                  &(R->polyhead), &(R->polytail), R->ID);
      }
   }
}


/*****************************************************/
/*********  FIND SEGMENTS FROM TRIANGLE LIST  ********/
/*****************************************************/

typedef struct _locnode_sruct {
   CNnodeptr  n;
   CNtlistptr tlisthead;
   CNtlistptr tlisttail;
   int        ntria;
} CNlocnode;

/*
 * Given a list of triangles, generate a list of segments.
 * The object is to generate a hierarchical list of triangles, segments
 * and nodes.
 * Segments must not be repeated in the list.
 */
/*ARGSUSED*/
void CNgenerate_segment_list(triahead,triatail,
                             nodehead,nodetail,
                             segmhead,segmtail)
CNtriaptr  triahead,  triatail;
CNsegmptr  *segmhead, *segmtail;
CNnodeptr  nodehead,  nodetail;
{
   CNtriaptr  T;
   CNnodeptr  N;
   CNlocnode *nodearr;
   int        nnodes, segmID=0, i;
   unsigned int siz;

   /*
    * Make sure the segment list is empty first
    */
   if (*segmhead != NULL) CNdelete_segm_list(segmhead, segmtail);

   /*
    * Reset the triangle and node flags
    */
   for (T=triahead; T!=NULL; T=T->next) T->flag = CN_FALSE;
   for (N=nodehead; N!=NULL; N=N->next) N->flag = CN_FALSE;

   /* 
    * Allocate space for local storage 
    * The list of triangles belonging to each node is stored in a
    * temporary array.
    */
   nnodes = CNcount_nodes(nodehead, nodetail);
   siz     = (unsigned int)(nnodes*sizeof(CNlocnode));
   if ((nodearr = (CNlocnode *)malloc(siz))==NULL) {
      (void) fprintf(stderr,"Error - insufficient memory!\n");
      return;
   }

   /* 
    * Fill the node array and use the node flag to index the node array
    */
   nnodes=0;
   for (N=nodehead; N!=NULL; N=N->next) {
      nodearr[nnodes].n         = N;
      nodearr[nnodes].tlisthead = NULL;
      nodearr[nnodes].tlisttail = NULL;
      N->flag= nnodes;
      nnodes++;
   }

   /* 
    * Fill the nodearr array and get the node-triangle interconnection
    */
   for (T=triahead; T!=NULL; T=T->next) {
      add_node(T->n1,T,nodearr,nnodes);
      add_node(T->n2,T,nodearr,nnodes);
      add_node(T->n3,T,nodearr,nnodes);
   }

   /*
    * Now go thru all the triangles again and set up a segm list.
    * shared_triangles() is called to find the number of triangles
    * shared by each segm - this is used as a check for adding segms
    */
   for (T=triahead; T!=NULL; T=T->next) {
      add_segm(segmhead, segmtail, T->n1, T->n2, &segmID, nodearr, nnodes);
      add_segm(segmhead, segmtail, T->n2, T->n3, &segmID, nodearr, nnodes);
      add_segm(segmhead, segmtail, T->n1, T->n3, &segmID, nodearr, nnodes);
      T->flag = CN_TRUE;            /* Flag the triangle */
   }

#ifdef DEBUG
   /*
    * Print the segm list
    */
   CNprint_tria_list(triahead, triatail);
   CNprint_segm_list(*segmhead, *segmtail);
   for (i=0; i<nnodes; i++) {
      (void) fprintf(stdout,"Node ID = %d  ",nodearr[i].n->ID);
      CNprint_tlist(nodearr[i].tlisthead, nodearr[i].tlisttail);
   }
#endif

   /* 
    * Free the tlist
    */
   for (i=0; i<nnodes; i++)
      CNdelete_tlist_list(&(nodearr[i].tlisthead), &(nodearr[i].tlisttail));

   /* 
    * Free the local array
    */
   free((char *)nodearr);
}

/* 
 * Add a triangle to the node's triangle list.
 */
/*ARGSUSED*/
static void add_node(N,T,nodearr,nnodes)
CNnodeptr N;
CNtriaptr T;
CNlocnode *nodearr;
int       nnodes;
{
   CNinsert_tlist(&(nodearr[N->flag].tlisthead),
                  &(nodearr[N->flag].tlisttail),T);
}

/*
 * Add a segm to the list, but only if the segm does not already
 * exist.  The check is to see if both of the triangles attached to the
 * segm has already been flagged; if that is true, then the segm must
 * already be in the segm list.
 */
/*ARGSUSED*/
static void add_segm(segmhead,segmtail,n1,n2,segmID,nodearr,nnodes)
CNsegmptr *segmhead, *segmtail;
CNnodeptr n1, n2;
int       *segmID;
CNlocnode *nodearr;
int        nnodes;
{
   CNtriaptr tria_arr[2];
   CNsegmptr sg;
   int       i, ntria = 0;

   /*
    * shared_triangles() is called to find the number of triangles
    * shared by each segm - this is used as a check for adding segms
    */
   ntria = shared_triangles(tria_arr, n1, n2, nodearr, nnodes);
   if (!shared_tria_is_flagged(tria_arr, ntria)) {
      /* Insert the segment to the list */
      sg = CNinsert_segm(segmhead,segmtail, n1, n2, (*segmID)++);

      /* Add neighboring triangle info to the segment */
      sg->numtri = ntria;
      for (i=0; i<ntria && i<2; i++)
         sg->nbrtri[i] = tria_arr[i];
   }
}


/*
 * Find the triangles shared by two nodes (a segm)
 */
/*ARGSUSED*/
static int shared_triangles(tria_arr,n1,n2,nodearr,nnodes)
CNtriaptr *tria_arr;
CNnodeptr n1, n2;
CNlocnode *nodearr;
int        nnodes;
{
   int ntria=0, k;
   int FOUND = CN_FALSE;
   CNtriaptr  TA, TB;
   CNtlistptr TLA, TLB;


   for (TLA=nodearr[n1->flag].tlisthead; TLA!=NULL; TLA=TLA->next) 
   for (TLB=nodearr[n2->flag].tlisthead; TLB!=NULL; TLB=TLB->next) {
      TA = TLA->T; 
      TB = TLB->T; 
      if (TA == TB) {
         /*
          * This triangle is shared by both nodes
          * Check the current array to see if the triangle is
          * already in the list
          */
         FOUND = CN_FALSE;
         for (k=0; k<ntria && !FOUND; k++)
            if (tria_arr[k] == TA) FOUND = CN_TRUE;
         if (!FOUND && ntria<2) tria_arr[ntria++] = TA;
      }
   }

#ifdef DEBUG
   /* Print out */
   if (ntria > 0) {
      (void) fprintf(stdout,"Nodes %d and %d share %d triangles: (",
              n1->ID,n2->ID,ntria);
      for (i=0; i<ntria; i++)
         (void) fprintf(stdout,"%d ",tria_arr[i]->ID);
      (void) fprintf(stdout,")\n");
   }
#endif

   /* Return the number of shared triangles */
   return (ntria);
}


/*
 * Go through the triangles shared by a segm and see if one of
 * the triangles has been flagged.
 */
static int shared_tria_is_flagged(tria_arr,ntria)
CNtriaptr *tria_arr;
int       ntria;
{
   int flag = CN_FALSE;
   int i;

   /* Check the number of shared triangles - valid range : 0 - 2 */
   if (ntria > 2) {
      (void) fprintf(stderr,"Warning - found more than 2 adjacent triangles!\n");
      ntria = 2;
   }

   for (i=0; i<ntria && !flag; i++) {
      if (tria_arr[i]->flag == CN_TRUE) flag = CN_TRUE;
   }

   return(flag);
}


/*****************************************************/
/*********  FIND SEGMENTS BOUNDING TRIANGLES  ********/
/*****************************************************/

/*
 * Search for the segments belonging to a region 
 */
/*ARGSUSED*/
void CNfind_boundary_segments(segmhead, segmtail, polyhead, polytail, regID)
CNsegmptr segmhead, segmtail;
CNpolyptr *polyhead, *polytail;
int       regID;
{
   CNsegmptr  S;
   CNslistptr slisthead=NULL, slisttail=NULL;
   int        boundary_segm;

   /* Go through a segment list and find the triangles on the boundary */
   for (S=segmhead; S!=NULL; S=S->next) {
      boundary_segm = CN_FALSE;

      /* Find out if the segment is a boundary segment */
      if (S->numtri == 0) 
         (void) fprintf(stderr,"Error - Segment %d has no triangles!\n",S->ID);

      else if ((S->numtri == 1) && (S->nbrtri[0]->region == regID))
         /* This is a boundary segment for the region */
         boundary_segm = CN_TRUE;

      else if ( (S->numtri == 2) &&
                ( (S->nbrtri[0]->region == regID) || 
                  (S->nbrtri[1]->region == regID) ) ) {
         if (S->nbrtri[0]->region != S->nbrtri[1]->region)
            /* This is a boundary segment */
            boundary_segm = CN_TRUE;
      }

      /* Add the boundary segment to a temporary list */
      if (boundary_segm) CNinsert_slist(&slisthead, &slisttail, S);
   }

#ifdef DEBUG
   /* Print the list */
   CNprint_slist(slisthead, slisttail);
#endif

   /* Rearrange the segments into a polygon */
   sort_slist(&slisthead, &slisttail, polyhead, polytail, regID);

#ifdef DEBUG
   /* Print out the node IDs in the polygon */
   CNprint_poly_list(*polyhead, *polytail);
#endif

   /* Delete the list */
   CNdelete_slist_list(&slisthead, &slisttail);
}


/*
 * sort out the list of segments to form a joined curve
 */
static void sort_slist(slisthead,slisttail,polyhead,polytail,regID)
CNslistptr *slisthead, *slisttail;
CNpolyptr  *polyhead, *polytail;
int        regID;
{

   CNslistptr SL;
   CNnodeptr  n1, n2; 
   CNnlistptr nlisthead, nlisttail;
   int        pID=0;

   while ((SL = *slisthead)!=NULL) {
      nlisthead = NULL;
      nlisttail = NULL;
      n1 = SL->S->n1;
      n2 = SL->S->n2;
      CNinsert_tailnlist(&nlisthead, &nlisttail, n1);
      CNinsert_tailnlist(&nlisthead, &nlisttail, n2);
      CNdelete_slist(slisthead, slisttail, SL);
      add_to_taillist(slisthead, slisttail, n2, &nlisthead, &nlisttail);
      add_to_headlist(slisthead, slisttail, n1, &nlisthead, &nlisttail);
      (void) CNinsert_poly(polyhead, polytail, 
                           nlisthead, nlisttail, regID, pID++);
   }
}

/*
 * add to the tail of the data list
 */
static void
add_to_taillist(slist_listhead, slist_listtail, nd, nlisthead, nlisttail)
CNslistptr *slist_listhead, *slist_listtail;
CNnodeptr  nd;
CNnlistptr *nlisthead, *nlisttail;
{
   CNslistptr SL;

   /* add the further node at every loop iteration */
   while ((SL=matching_slist(slist_listhead,nd))!=NULL) {
      if (SL->S->n1 == nd) {
         CNinsert_tailnlist(nlisthead, nlisttail, SL->S->n2);
         nd = SL->S->n2;
      } else {
         CNinsert_tailnlist(nlisthead, nlisttail, SL->S->n1);
         nd = SL->S->n1;
      }
      CNdelete_slist(slist_listhead, slist_listtail, SL);
   }
   return;
}

/*
 * add to the head of the data list
 */
static void
add_to_headlist(slist_listhead, slist_listtail, nd, nlisthead, nlisttail)
CNslistptr *slist_listhead, *slist_listtail;
CNnodeptr  nd;
CNnlistptr *nlisthead, *nlisttail;
{
   CNslistptr SL;

   /* add the further node at every loop iteration */
   while ((SL=matching_slist(slist_listhead,nd))!=NULL) {
      if (SL->S->n1 == nd) {
         CNinsert_headnlist(nlisthead, nlisttail, SL->S->n2);
         nd = SL->S->n2;
      } else {
         CNinsert_headnlist(nlisthead, nlisttail, SL->S->n1);
         nd = SL->S->n1;
      }
      CNdelete_slist(slist_listhead, slist_listtail, SL);
   }
   return;
}


/*
 * Return segm containing node that matches a given node 
 * We are looking for the segment that is attached to the current
 * segment
 */
static CNslistptr matching_slist(slist_listhead,nd)
CNslistptr *slist_listhead;
CNnodeptr  nd;
{
   CNslistptr SL,SF=NULL;
   int        FOUND = CN_FALSE;

   /* check the segments in the list */
   for (SL=(*slist_listhead); SL!=NULL && !FOUND; SL=SL->next) {
      /* check the nodes on either end of the segment */
      if ((SL->S->n1 == nd) || (SL->S->n2 == nd)) {
         FOUND = CN_TRUE;
      }
      if (FOUND) SF = SL;
   }

   /* return the segm-ptr */
   if (!FOUND) SF = NULL;
   return(SF);
}


