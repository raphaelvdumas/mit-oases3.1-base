/*
 * listutils.c - Useful utilities for manipulating nodes, datasets, curves
 *               and other such structures.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "CNplot.h"

static void copy_abs_pointlist();
static void reflect_segment();
static void reflect_x_segments();
static void reflect_y_segments();
static void reflect_z_segments();

/*
 * find a node common to two segments
 */
CNnodeptr CNcommon_node(s1, s2)
CNsegmptr s1, s2;
{
   CNnodeptr cmn_node;

   cmn_node = NULL;
   if ((s1->n1==s2->n1)||(s1->n1==s2->n2))
      cmn_node = s1->n1;
   else if ((s1->n2==s2->n1)||(s1->n2==s2->n2))
      cmn_node = s1->n2;
   return(cmn_node);
}


/*
 * Apply global curve properties to a list of curves.
 * Override individual curve specifications if asked to.
 */
/*ARGSUSED*/
void CNreset_curves(curvehead, curvetail, gbcv_pr, override)
CNcurveptr          curvehead, curvetail;
CNgbcurve_property *gbcv_pr;
int                 override;
{
   CNcurveptr C;
   int        lnmin, lnmax, linetyp;
   int        mkmin, mkmax, marktyp;
   int        flmin, flmax, filltyp;
   int        tmp, count;

   /* Error checking */
   if (curvehead == NULL) {
      (void) fprintf(stderr,
                     "CNreset_curves() : Error - NULL curve-list!\n");
      return;
   }
   if (gbcv_pr == NULL) {
      (void) fprintf(stderr,
                     "CNreset_curves() : Error - NULL property!\n");
      return;
   }

   /*
    * Setting "dlinechange=on" is equivalent to setting the linetypes
    * and linecolors on each individual curve 
    * Setting "dlinetype=1" is equivalent to setting "dlinechange=off"
    */

   /* Get the min and max line types */
   lnmin = gbcv_pr->minlntyp;
   lnmax = gbcv_pr->maxlntyp;
   if (lnmax < lnmin) {
      tmp   = lnmax;
      lnmax = lnmin;
      lnmin = tmp;
   }

   /* Get the min and max marker types */
   mkmin = gbcv_pr->minmktyp;
   mkmax = gbcv_pr->maxmktyp;
   if (mkmax < mkmin) {
      tmp   = mkmax;
      mkmax = mkmin;
      mkmin = tmp;
   }

   /* Get the min and max fill types */
   flmin = gbcv_pr->minfltyp;
   flmax = gbcv_pr->maxfltyp;
   if (flmax < flmin) {
      tmp   = flmax;
      flmax = flmin;
      flmin = tmp;
   }


   /* Set overall properties - override individual properties */
   count=0;
   for (C=curvehead; C!=NULL; C=C->next) {

      linetyp = lnmin;
      if (gbcv_pr->linechg) linetyp += count % (lnmax - lnmin + 1);
      marktyp = mkmin;
      if (gbcv_pr->markchg) marktyp += count % (mkmax - mkmin + 1);
      filltyp = flmin;
      if (gbcv_pr->fillchg) filltyp += count % (flmax - flmin + 1);
      count++;

      /* Line label */
      if ((gbcv_pr->flag & CNlnlabel) != 0) {
         if (override || ((C->curv_pr.flag & CNlinelabel) == 0)) {
            CNdestroy_string(C->curv_pr.linelabel);
            if (C==curvehead)
            C->curv_pr.linelabel = CNcreate_string(gbcv_pr->lnlabel);
            else
            C->curv_pr.linelabel = NULL;
         }
      }

      /* Line types */
      if ((gbcv_pr->flag & CNlnwidth) != 0) {
         if (override || ((C->curv_pr.flag & CNlinewidth) == 0)) 
         C->curv_pr.linewidth = gbcv_pr->lnwidth;
      }

      if ((gbcv_pr->flag & CNlntype ) != 0) {
         if (override || ((C->curv_pr.flag & CNlinetype) == 0)) 
         C->curv_pr.linetype  = gbcv_pr->lntype ;
      }

      if ((gbcv_pr->flag & CNlncolor) != 0) {
         if (override || ((C->curv_pr.flag & CNlinecolor) == 0)) 
         C->curv_pr.linecolor = gbcv_pr->lncolor;
      }

      if ((gbcv_pr->flag & CNlinechg) != 0) {
         if (override || ((C->curv_pr.flag & CNlinetype) == 0)) 
         C->curv_pr.linetype  = linetyp;
         if (override || ((C->curv_pr.flag & CNlinecolor) == 0)) 
         C->curv_pr.linecolor = linetyp;
      }

      /* Marker types */
      if ((gbcv_pr->flag & CNmktype ) != 0) {
         if (override || ((C->curv_pr.flag & CNmarktype) == 0)) 
         C->curv_pr.marktype  = gbcv_pr->mktype ;
      }

      if ((gbcv_pr->flag & CNmksize ) != 0) {
         if (override || ((C->curv_pr.flag & CNmarksize) == 0)) 
         C->curv_pr.marksize  = gbcv_pr->mksize ;
      }

      if ((gbcv_pr->flag & CNmkcolor) != 0) {
         if (override || ((C->curv_pr.flag & CNmarkcolor) == 0)) 
         C->curv_pr.markcolor = gbcv_pr->mkcolor;
      }

      if ((gbcv_pr->flag & CNmarkchg) != 0) {
         if (override || ((C->curv_pr.flag & CNmarktype) == 0)) 
         C->curv_pr.marktype  = marktyp;
         if (override || ((C->curv_pr.flag & CNmarkcolor) == 0)) 
         C->curv_pr.markcolor = marktyp;
      }

      /* Fill types */
      if ((gbcv_pr->flag & CNfltype ) != 0) {
         if (override || ((C->curv_pr.flag & CNfilltype) == 0)) 
         C->curv_pr.filltype  = gbcv_pr->fltype ;
      }

      if ((gbcv_pr->flag & CNflcolor) != 0) {
         if (override || ((C->curv_pr.flag & CNfillcolor) == 0)) 
         C->curv_pr.fillcolor = gbcv_pr->flcolor;
      }

      if ((gbcv_pr->flag & CNfillchg) != 0) {
         if (override || ((C->curv_pr.flag & CNfilltype) == 0)) 
         C->curv_pr.filltype  = filltyp;
         if (override || ((C->curv_pr.flag & CNfillcolor) == 0)) 
         C->curv_pr.fillcolor = filltyp;
      }
   }
}



/*****************************************************/
/*********       FIND MAX AND MIN VALUES      ********/
/*****************************************************/

/*
 * ELEMENTS 
 */

/*
 * Find the min and max of the element list
 */
/*ARGSUSED*/
void CNget_elemlist_maxmin(elemhead,elemtail,
                          xmin,xmax,ymin,ymax,zmin,zmax,tmin,tmax)
CNelemptr elemhead,elemtail;
double    *xmin,*xmax,*ymin,*ymax,*zmin,*zmax,*tmin,*tmax;
{
   CNelemptr E;
   double    txmin, txmax, tymin, tymax, tzmin, tzmax, ttmin, ttmax;

   /* Reinitialize */
   *xmin = *ymin = *zmin = *tmin =  CN_LARGE;
   *xmax = *ymax = *zmax = *tmax = -CN_LARGE;

   /* Search all the nodes of the elements */
   for (E=elemhead; E!=NULL; E=E->next) {
      CNget_elem_maxmin(E,
         &txmin,&txmax,&tymin,&tymax,&tzmin,&tzmax,&ttmin,&ttmax);
      if (txmin < *xmin) *xmin = txmin;
      if (txmax > *xmax) *xmax = txmax;
      if (tymin < *ymin) *ymin = tymin;
      if (tymax > *ymax) *ymax = tymax;
      if (tzmin < *zmin) *zmin = tzmin;
      if (tzmax > *zmax) *zmax = tzmax;
      if (ttmin < *tmin) *tmin = ttmin;
      if (ttmax > *tmax) *tmax = ttmax;
   }

}

/* 
 * Find the min and max of an element
 */
void CNget_elem_maxmin(E,xmin,xmax,ymin,ymax,zmin,zmax,tmin,tmax)
CNelemptr E;
double    *xmin,*xmax,*ymin,*ymax,*zmin,*zmax,*tmin,*tmax;
{
   if (E->type == CN_TRIA_STR && E->tria != NULL)
      CNget_tria_maxmin(E->tria,xmin,xmax,ymin,ymax,zmin,zmax,tmin,tmax);
   else if (E->type == CN_RECT_STR && E->rect != NULL)
      CNget_rect_maxmin(E->rect,xmin,xmax,ymin,ymax,zmin,zmax,tmin,tmax);
}

/*
 * TRIANGLES 
 */

/*
 * Find the min and max of the triangle list
 */
/*ARGSUSED*/
void CNget_trialist_maxmin(triahead,triatail,
                          xmin,xmax,ymin,ymax,zmin,zmax,tmin,tmax)
CNtriaptr triahead,triatail;
double    *xmin,*xmax,*ymin,*ymax,*zmin,*zmax,*tmin,*tmax;
{
   CNtriaptr T;
   double    txmin, txmax, tymin, tymax, tzmin, tzmax, ttmin, ttmax;

   /* Reinitialize */
   *xmin = *ymin = *zmin = *tmin =  CN_LARGE;
   *xmax = *ymax = *zmax = *tmax = -CN_LARGE;

   /* Search all the nodes */
   for (T=triahead; T!=NULL; T=T->next) {
      CNget_tria_maxmin(T,
         &txmin,&txmax,&tymin,&tymax,&tzmin,&tzmax,&ttmin,&ttmax);
      if (txmin < *xmin) *xmin = txmin;
      if (txmax > *xmax) *xmax = txmax;
      if (tymin < *ymin) *ymin = tymin;
      if (tymax > *ymax) *ymax = tymax;
      if (tzmin < *zmin) *zmin = tzmin;
      if (tzmax > *zmax) *zmax = tzmax;
      if (ttmin < *tmin) *tmin = ttmin;
      if (ttmax > *tmax) *tmax = ttmax;
   }
}


/* 
 * Find the min and max of a triangle
 */
void CNget_tria_maxmin(T,xmin,xmax,ymin,ymax,zmin,zmax,tmin,tmax)
CNtriaptr T;
double    *xmin,*xmax,*ymin,*ymax,*zmin,*zmax,*tmin,*tmax;
{
   /* Search by coordinate */
   CNget_tria_xmaxmin(T,xmin,xmax);
   CNget_tria_ymaxmin(T,ymin,ymax);
   CNget_tria_zmaxmin(T,zmin,zmax);
   CNget_tria_tmaxmin(T,tmin,tmax);
}


/* 
 * Find the min and max x-values of a triangle
 */
void CNget_tria_xmaxmin(T,xmin,xmax)
CNtriaptr T;
double    *xmin,*xmax;
{
   /* Reinitialize */
   *xmin =  CN_LARGE;
   *xmax = -CN_LARGE;

   /* Search all the nodes */
   if (T->n1->coord->x < *xmin) *xmin = T->n1->coord->x;
   if (T->n1->coord->x > *xmax) *xmax = T->n1->coord->x;

   if (T->n2->coord->x < *xmin) *xmin = T->n2->coord->x;
   if (T->n2->coord->x > *xmax) *xmax = T->n2->coord->x;

   if (T->n3->coord->x < *xmin) *xmin = T->n3->coord->x;
   if (T->n3->coord->x > *xmax) *xmax = T->n3->coord->x;
}


/* 
 * Find the min and max y-values of a triangle
 */
void CNget_tria_ymaxmin(T,ymin,ymax)
CNtriaptr T;
double    *ymin,*ymax;
{
   /* Reinitialize */
   *ymin =  CN_LARGE;
   *ymax = -CN_LARGE;

   /* Search all the nodes */
   if (T->n1->coord->y < *ymin) *ymin = T->n1->coord->y;
   if (T->n1->coord->y > *ymax) *ymax = T->n1->coord->y;

   if (T->n2->coord->y < *ymin) *ymin = T->n2->coord->y;
   if (T->n2->coord->y > *ymax) *ymax = T->n2->coord->y;

   if (T->n3->coord->y < *ymin) *ymin = T->n3->coord->y;
   if (T->n3->coord->y > *ymax) *ymax = T->n3->coord->y;
}


/* 
 * Find the min and max z-values of a triangle
 */
void CNget_tria_zmaxmin(T,zmin,zmax)
CNtriaptr T;
double    *zmin,*zmax;
{
   /* Reinitialize */
   *zmin =  CN_LARGE;
   *zmax = -CN_LARGE;

   /* Search all the nodes */
   if (T->n1->coord->z < *zmin) *zmin = T->n1->coord->z;
   if (T->n1->coord->z > *zmax) *zmax = T->n1->coord->z;

   if (T->n2->coord->z < *zmin) *zmin = T->n2->coord->z;
   if (T->n2->coord->z > *zmax) *zmax = T->n2->coord->z;

   if (T->n3->coord->z < *zmin) *zmin = T->n3->coord->z;
   if (T->n3->coord->z > *zmax) *zmax = T->n3->coord->z;
}


/* 
 * Find the min and max t-values of a triangle
 */
void CNget_tria_tmaxmin(T,tmin,tmax)
CNtriaptr T;
double    *tmin,*tmax;
{
   /* Reinitialize */
   *tmin =  CN_LARGE;
   *tmax = -CN_LARGE;

   /* Search all the nodes */
   if (T->n1->t        < *tmin) *tmin = T->n1->t       ;
   if (T->n1->t        > *tmax) *tmax = T->n1->t       ;

   if (T->n2->t        < *tmin) *tmin = T->n2->t       ;
   if (T->n2->t        > *tmax) *tmax = T->n2->t       ;

   if (T->n3->t        < *tmin) *tmin = T->n3->t       ;
   if (T->n3->t        > *tmax) *tmax = T->n3->t       ;
}



/*
 * RECTANGLES
 */

/* 
 * Find the min and max of a rectangle
 */
void CNget_rect_maxmin(R,xmin,xmax,ymin,ymax,zmin,zmax,tmin,tmax)
CNrectptr R;
double    *xmin,*xmax,*ymin,*ymax,*zmin,*zmax,*tmin,*tmax;
{
   /* Search by coordinate */
   CNget_rect_xmaxmin(R,xmin,xmax);
   CNget_rect_ymaxmin(R,ymin,ymax);
   CNget_rect_zmaxmin(R,zmin,zmax);
   CNget_rect_tmaxmin(R,tmin,tmax);
}


/* 
 * Find the min and max x-values of a rectangle
 */
void CNget_rect_xmaxmin(R,xmin,xmax)
CNrectptr R;
double    *xmin,*xmax;
{
   /* Reinitialize */
   *xmin =  CN_LARGE;
   *xmax = -CN_LARGE;

   /* Search all the nodes */
   if (R->n1->coord->x < *xmin) *xmin = R->n1->coord->x;
   if (R->n1->coord->x > *xmax) *xmax = R->n1->coord->x;

   if (R->n2->coord->x < *xmin) *xmin = R->n2->coord->x;
   if (R->n2->coord->x > *xmax) *xmax = R->n2->coord->x;

   if (R->n3->coord->x < *xmin) *xmin = R->n3->coord->x;
   if (R->n3->coord->x > *xmax) *xmax = R->n3->coord->x;

   if (R->n4->coord->x < *xmin) *xmin = R->n4->coord->x;
   if (R->n4->coord->x > *xmax) *xmax = R->n4->coord->x;
}


/* 
 * Find the min and max y-values of a rectangle
 */
void CNget_rect_ymaxmin(R,ymin,ymax)
CNrectptr R;
double    *ymin,*ymax;
{
   /* Reinitialize */
   *ymin =  CN_LARGE;
   *ymax = -CN_LARGE;

   /* Search all the nodes */
   if (R->n1->coord->y < *ymin) *ymin = R->n1->coord->y;
   if (R->n1->coord->y > *ymax) *ymax = R->n1->coord->y;

   if (R->n2->coord->y < *ymin) *ymin = R->n2->coord->y;
   if (R->n2->coord->y > *ymax) *ymax = R->n2->coord->y;

   if (R->n3->coord->y < *ymin) *ymin = R->n3->coord->y;
   if (R->n3->coord->y > *ymax) *ymax = R->n3->coord->y;

   if (R->n4->coord->y < *ymin) *ymin = R->n4->coord->y;
   if (R->n4->coord->y > *ymax) *ymax = R->n4->coord->y;
}


/* 
 * Find the min and max z-values of a rectangle
 */
void CNget_rect_zmaxmin(R,zmin,zmax)
CNrectptr R;
double    *zmin,*zmax;
{
   /* Reinitialize */
   *zmin =  CN_LARGE;
   *zmax = -CN_LARGE;

   /* Search all the nodes */
   if (R->n1->coord->z < *zmin) *zmin = R->n1->coord->z;
   if (R->n1->coord->z > *zmax) *zmax = R->n1->coord->z;

   if (R->n2->coord->z < *zmin) *zmin = R->n2->coord->z;
   if (R->n2->coord->z > *zmax) *zmax = R->n2->coord->z;

   if (R->n3->coord->z < *zmin) *zmin = R->n3->coord->z;
   if (R->n3->coord->z > *zmax) *zmax = R->n3->coord->z;

   if (R->n4->coord->z < *zmin) *zmin = R->n4->coord->z;
   if (R->n4->coord->z > *zmax) *zmax = R->n4->coord->z;
}


/* 
 * Find the min and max t-values of a rectangle
 */
void CNget_rect_tmaxmin(R,tmin,tmax)
CNrectptr R;
double    *tmin,*tmax;
{
   /* Reinitialize */
   *tmin =  CN_LARGE;
   *tmax = -CN_LARGE;

   /* Search all the nodes */
   if (R->n1->t        < *tmin) *tmin = R->n1->t       ;
   if (R->n1->t        > *tmax) *tmax = R->n1->t       ;

   if (R->n2->t        < *tmin) *tmin = R->n2->t       ;
   if (R->n2->t        > *tmax) *tmax = R->n2->t       ;

   if (R->n3->t        < *tmin) *tmin = R->n3->t       ;
   if (R->n3->t        > *tmax) *tmax = R->n3->t       ;

   if (R->n4->t        < *tmin) *tmin = R->n4->t       ;
   if (R->n4->t        > *tmax) *tmax = R->n4->t       ;
}



/* 
 * POLYGONS
 */

/* 
 * Find the min and max of a rectangle
 */
void CNget_poly_maxmin(P,xmin,xmax,ymin,ymax,zmin,zmax,tmin,tmax)
CNpolyptr P;
double    *xmin,*xmax,*ymin,*ymax,*zmin,*zmax,*tmin,*tmax;
{
   /* Search by coordinate */
   CNget_poly_xmaxmin(P,xmin,xmax);
   CNget_poly_ymaxmin(P,ymin,ymax);
   CNget_poly_zmaxmin(P,zmin,zmax);
   CNget_poly_tmaxmin(P,tmin,tmax);
}


/* 
 * Find the min and max x-values of a polygon  
 */
void CNget_poly_xmaxmin(P,xmin,xmax)
CNpolyptr P;
double    *xmin,*xmax;
{
   CNnlistptr Nd;

   /* Reinitialize */
   *xmin =  CN_LARGE;
   *xmax = -CN_LARGE;

   /* Search all the nodes */
   for (Nd=P->nlisthead; Nd!=NULL; Nd=Nd->next) {
      if (Nd->N->coord->x < *xmin) *xmin = Nd->N->coord->x;
      if (Nd->N->coord->x > *xmax) *xmax = Nd->N->coord->x;
   }
}


/* 
 * Find the min and max y-values of a polygon
 */
void CNget_poly_ymaxmin(P,ymin,ymax)
CNpolyptr P;
double    *ymin,*ymax;
{
   CNnlistptr Nd;

   /* Reinitialize */
   *ymin =  CN_LARGE;
   *ymax = -CN_LARGE;

   /* Search all the nodes */
   for (Nd=P->nlisthead; Nd!=NULL; Nd=Nd->next) {
      if (Nd->N->coord->y < *ymin) *ymin = Nd->N->coord->y;
      if (Nd->N->coord->y > *ymax) *ymax = Nd->N->coord->y;
   }
}


/* 
 * Find the min and max z-values of a polygon
 */
void CNget_poly_zmaxmin(P,zmin,zmax)
CNpolyptr P;
double    *zmin,*zmax;
{
   CNnlistptr Nd;

   /* Reinitialize */
   *zmin =  CN_LARGE;
   *zmax = -CN_LARGE;

   /* Search all the nodes */
   for (Nd=P->nlisthead; Nd!=NULL; Nd=Nd->next) {
      if (Nd->N->coord->z < *zmin) *zmin = Nd->N->coord->z;
      if (Nd->N->coord->z > *zmax) *zmax = Nd->N->coord->z;
   }
}


/* 
 * Find the min and max t-values of a polygon
 */
void CNget_poly_tmaxmin(P,tmin,tmax)
CNpolyptr P;
double    *tmin,*tmax;
{
   CNnlistptr Nd;

   /* Reinitialize */
   *tmin =  CN_LARGE;
   *tmax = -CN_LARGE;

   /* Search all the nodes */
   for (Nd=P->nlisthead; Nd!=NULL; Nd=Nd->next) {
      if (Nd->N->t < *tmin) *tmin = Nd->N->t;
      if (Nd->N->t > *tmax) *tmax = Nd->N->t;
   }
}


/*
 * NODES
 */

/*
 * Find the min and max of the node list
 */
/*ARGSUSED*/
void CNget_nodelist_tmaxmin(nodehead,nodetail,tmin,tmax)
CNnodeptr nodehead,nodetail;
double    *tmin,*tmax;
{
   CNnodeptr N;

   /* Reinitialize */
   *tmin =  CN_LARGE;
   *tmax = -CN_LARGE;

   /* Search all the nodes */
   for (N=nodehead; N!=NULL; N=N->next) {
      if (N->t < *tmin) *tmin = N->t;
      if (N->t > *tmax) *tmax = N->t;
   }
}



/*
 * CURVES
 */

/*
 * Find the min and max of a curve-list
 */
/*ARGSUSED*/
void CNget_curvelist_maxmin(curvehead,curvetail,xmin,xmax,ymin,ymax,zmin,zmax)
CNcurveptr curvehead,curvetail;
double    *xmin,*xmax,*ymin,*ymax,*zmin,*zmax;
{
   CNcurveptr C;
   double     cxmin, cxmax, cymin, cymax, czmin, czmax;

   /* Reinitialize */
   *xmin = *ymin = *zmin =  CN_LARGE;
   *xmax = *ymax = *zmax = -CN_LARGE;
   
   for (C=curvehead; C!=NULL; C=C->next) {
      CNget_curve_maxmin(C,&cxmin,&cxmax,&cymin,&cymax,&czmin,&czmax);
      if (cxmin < *xmin) *xmin = cxmin;
      if (cxmax > *xmax) *xmax = cxmax;
      if (cymin < *ymin) *ymin = cymin;
      if (cymax > *ymax) *ymax = cymax;
      if (czmin < *zmin) *zmin = czmin;
      if (czmax > *zmax) *zmax = czmax;
   }
}
  

/*
 * Find the min and max of a curve
 * This is just an interface to the pointlist search
 */
void CNget_curve_maxmin(C,xmin,xmax,ymin,ymax,zmin,zmax)
CNcurveptr C;
double    *xmin,*xmax,*ymin,*ymax,*zmin,*zmax;
{
   CNget_pointlist_maxmin(C->pointhead, C->pointtail,
                      xmin,xmax,ymin,ymax,zmin,zmax);
}

 

/*
 * POINTS
 */

/*
 * Find the min and max of a pointlist
 */
/*ARGSUSED*/
void CNget_pointlist_maxmin(pointhead,pointtail,xmin,xmax,ymin,ymax,zmin,zmax)
CNpointptr pointhead, pointtail;
double    *xmin,*xmax,*ymin,*ymax,*zmin,*zmax;
{
   CNpointptr P;
 
   /* Reinitialize */
   *xmin = *ymin = *zmin =  CN_LARGE;
   *xmax = *ymax = *zmax = -CN_LARGE;
 
   /* Search all the points */
   for (P=pointhead; P!=NULL; P=P->next) {
      if (P->x < *xmin) *xmin = P->x;
      if (P->x > *xmax) *xmax = P->x;
      if (P->y < *ymin) *ymin = P->y;
      if (P->y > *ymax) *ymax = P->y;
      if (P->z < *zmin) *zmin = P->z;
      if (P->z > *zmax) *zmax = P->z;
   }
}
 

/* 
 * VECTORS
 */

/*
 * Find the min and max of a vector-list
 */
/*ARGSUSED*/
void CNget_vectorlist_maxmin(vectorhead,vectortail,
                             xmin,xmax,ymin,ymax,zmin,zmax)
CNvecptr vectorhead,vectortail;
double    *xmin,*xmax,*ymin,*ymax,*zmin,*zmax;
{
   CNvecptr V;

   /* Reinitialize */
   *xmin = *ymin = *zmin =  CN_LARGE;
   *xmax = *ymax = *zmax = -CN_LARGE;
   
   for (V=vectorhead; V!=NULL; V=V->next) {
      if (V->x < *xmin) *xmin = V->x;
      if (V->x > *xmax) *xmax = V->x;
      if (V->y < *ymin) *ymin = V->y;
      if (V->y > *ymax) *ymax = V->y;
      if (V->z < *zmin) *zmin = V->z;
      if (V->z > *zmax) *zmax = V->z;
   }
}
  


/*
 * Utilities for reflecting
 */
#define ARRSIZ 100

/*
 * Copy a list of points and take the absolute value
 */
/*ARGSUSED*/
void CNcopy_abslog_pointlist(new_pointhead, new_pointtail,
                          old_pointhead, old_pointtail,
                          xabs, yabs, zabs, xlog, ylog, zlog)
CNpointptr *new_pointhead, *new_pointtail;
CNpointptr old_pointhead, old_pointtail;
short      xabs, yabs, zabs;
short      xlog, ylog, zlog;
{
   CNpointptr P;

   /* Initialize */
   *new_pointhead = NULL;
   *new_pointtail = NULL;
 
   /* Error check */
   if (old_pointhead == NULL) return;

   /* Copy the pointlist */
   if (xabs || yabs || zabs) {
      copy_abs_pointlist(new_pointhead, new_pointtail,
                           old_pointhead, old_pointtail,
                           xabs, yabs, zabs);
   } else {
      CNcopy_pointlist(new_pointhead, new_pointtail,
                       old_pointhead, old_pointtail);
   }
   if (*new_pointhead == NULL) return;
 
   /*
    * Take the log of the curve
    */
   if (xlog || ylog || zlog) {
      for (P=(*new_pointhead); P!=NULL; P=P->next) {
         if (xlog) P->x = CNlog10(P->x);
         if (ylog) P->y = CNlog10(P->y);
         if (zlog) P->z = CNlog10(P->z);
      }
   }
}
   
/*
 * Copy a list of points
 */
/*ARGSUSED*/
void CNcopy_pointlist(new_pointhead, new_pointtail,
                      old_pointhead, old_pointtail)
CNpointptr *new_pointhead, *new_pointtail;
CNpointptr old_pointhead, old_pointtail;
{
   CNpointptr P;

   /* Initialize */
   *new_pointhead = NULL;
   *new_pointtail = NULL;

   for (P=old_pointhead; P!=NULL; P=P->next)
      (void) CNinsert_point(new_pointhead, new_pointtail,
                            P->x, P->y, P->z, P->ID);
}


/*
 * Copy a list of points and take the absolute value
 */
/*ARGSUSED*/
static void copy_abs_pointlist(new_pointhead, new_pointtail,
                          old_pointhead, old_pointtail,
                          xabs, yabs, zabs)
CNpointptr *new_pointhead, *new_pointtail;
CNpointptr old_pointhead, old_pointtail;
short      xabs, yabs, zabs;
{
   CNpointptr P;
   double     xarr[ARRSIZ], yarr[ARRSIZ], zarr[ARRSIZ];
   double     x, y, z;
   int        npts, i, ID;
 
   /* Initialize */
   *new_pointhead = NULL;
   *new_pointtail = NULL;
 
   /* Error check */
   if (old_pointhead == NULL) return;

   /* Copy the pointlist */
   for (P=old_pointhead; P!=NULL; P=P->next) {
 
      if (P->next == NULL) {
         /* End-point - just add the point */
         x = P->x;  if (xabs) x = fabs(x);
         y = P->y;  if (yabs) y = fabs(y);
         z = P->z;  if (zabs) z = fabs(z);
         (void) CNinsert_point(new_pointhead,
                               new_pointtail,
                               x, y, z, P->ID);
      } else {
         /* Check to see if the segment crosses axes */
         xarr[0] = P->x;
         yarr[0] = P->y;
         zarr[0] = P->z;
         xarr[1] = P->next->x;
         yarr[1] = P->next->y;
         zarr[1] = P->next->z;
         npts    = 2;
         reflect_segment(xarr, yarr, zarr, &npts, ARRSIZ, xabs, yabs, zabs);
         /* Add all points but the last */
         for (i=0; i<npts-1; i++) {
            ID = (i==0) ? P->ID : -1;
            (void) CNinsert_point(new_pointhead,
                                  new_pointtail,
                                  xarr[i], yarr[i], zarr[i], ID);
         }
      }
   }
}


/*
 * Reflect a segment 
 * In theory, we start with 2 points, and end up with a max of 6 points.
 * As a result, don't worry about array checking.
 */
static void reflect_segment(xarr, yarr, zarr, npts, arrsiz, xabs, yabs, zabs)
double *xarr, *yarr, *zarr;
int    *npts;
short  xabs, yabs, zabs;
{
   /* Reflect the segment in x */
   if (xabs) reflect_x_segments(xarr, yarr, zarr, npts, arrsiz);

   /* Reflect the segment in y */
   if (yabs) reflect_y_segments(xarr, yarr, zarr, npts, arrsiz);

   /* Reflect the segment in y */
   if (zabs) reflect_z_segments(xarr, yarr, zarr, npts, arrsiz);
}

/*
 * Reflect segments across x-axis
 */
static void reflect_x_segments(xarr, yarr, zarr, npts, arrsiz)
double *xarr, *yarr, *zarr;
int    *npts;
int    arrsiz;
{
   double xtarr[ARRSIZ], ytarr[ARRSIZ], ztarr[ARRSIZ];
   double x1, x2, xm, y1, y2, ym, z1, z2, zm, t;
   int    ntpts=0;
   int    i;

   for (i=0; i<*npts; i++) {
      /* Add the 1st point */
      xtarr[ntpts] = fabs(xarr[i]);
      ytarr[ntpts] = yarr[i];
      ztarr[ntpts] = zarr[i];
      ntpts++;

      if (i == *npts-1) continue;

      /* Add more points if necessary */
      if ((xarr[i] * xarr[i+1]) < 0.0) {
         x1 = xarr[i];
         y1 = yarr[i];
         z1 = zarr[i];
         x2 = xarr[i+1];
         y2 = yarr[i+1];
         z2 = zarr[i+1];
         t  = (0.0 - x1) / (x2 - x1);
         xm = x1 + t*(x2-x1);
         ym = y1 + t*(y2-y1);
         zm = z1 + t*(z2-z1);
         xtarr[ntpts] = fabs(xm);
         ytarr[ntpts] = ym;
         ztarr[ntpts] = zm;
         ntpts++;
      }
   }

   /* Transfer the arrays */
   for (i=0; i<ntpts && i<arrsiz; i++) {
      xarr[i] = xtarr[i];
      yarr[i] = ytarr[i];
      zarr[i] = ztarr[i];
   }
   *npts = ntpts;
}

/*
 * Reflect segments across y-axis
 */
static void reflect_y_segments(xarr, yarr, zarr, npts, arrsiz)
double *xarr, *yarr, *zarr;
int    *npts;
int    arrsiz;
{
   double xtarr[ARRSIZ], ytarr[ARRSIZ], ztarr[ARRSIZ];
   double x1, x2, xm, y1, y2, ym, z1, z2, zm, t;
   int    ntpts=0;
   int    i;
 
   for (i=0; i<*npts; i++) {
      /* Add the 1st point */
      xtarr[ntpts] = xarr[i];
      ytarr[ntpts] = fabs(yarr[i]);
      ztarr[ntpts] = zarr[i];
      ntpts++;
 
      if (i == *npts-1) continue;
 
      /* Add more points if necessary */
      if ((yarr[i] * yarr[i+1]) < 0.0) {
         x1 = xarr[i];
         y1 = yarr[i];
         z1 = zarr[i];
         x2 = xarr[i+1];
         y2 = yarr[i+1];
         z2 = zarr[i+1];
         t  = (0.0 - y1) / (y2 - y1);
         xm = x1 + t*(x2-x1);
         ym = y1 + t*(y2-y1);
         zm = z1 + t*(z2-z1);
         xtarr[ntpts] = xm;
         ytarr[ntpts] = fabs(ym);
         ztarr[ntpts] = zm;
         ntpts++;
      }
   }
 
   /* Transfer the arrays */
   for (i=0; i<ntpts && i<arrsiz; i++) {
      xarr[i] = xtarr[i];
      yarr[i] = ytarr[i];
      zarr[i] = ztarr[i];
   }
   *npts = ntpts;
}

/*
 * Reflect segments across z-axis
 */
static void reflect_z_segments(xarr, yarr, zarr, npts, arrsiz)
double *xarr, *yarr, *zarr;
int    *npts;
int    arrsiz;
{
   double xtarr[ARRSIZ], ytarr[ARRSIZ], ztarr[ARRSIZ];
   double x1, x2, xm, y1, y2, ym, z1, z2, zm, t;
   int    ntpts=0;
   int    i;
 
   for (i=0; i<*npts; i++) {
      /* Add the 1st point */
      xtarr[ntpts] = xarr[i];
      ytarr[ntpts] = yarr[i];
      ztarr[ntpts] = fabs(zarr[i]);
      ntpts++;
 
      if (i == *npts-1) continue;
 
      /* Add more points if necessary */
      if ((zarr[i] * zarr[i+1]) < 0.0) {
         x1 = xarr[i];
         y1 = yarr[i];
         z1 = zarr[i];
         x2 = xarr[i+1];
         y2 = yarr[i+1];
         z2 = zarr[i+1];
         t  = (0.0 - z1) / (z2 - z1);
         xm = x1 + t*(x2-x1);
         ym = y1 + t*(y2-y1);
         zm = z1 + t*(z2-z1);
         xtarr[ntpts] = xm;
         ytarr[ntpts] = ym;
         ztarr[ntpts] = fabs(zm);
         ntpts++;
      }
   }
 
   /* Transfer the arrays */
   for (i=0; i<ntpts && i<arrsiz; i++) {
      xarr[i] = xtarr[i];
      yarr[i] = ytarr[i];
      zarr[i] = ztarr[i];
   }
   *npts = ntpts;
}


/*
 * ELEMENTS
 */
/*ARGSUSED*/
void CNbuild_elemlist(elemhead, elemtail,
                      triahead, triatail, 
                      recthead, recttail)
CNelemptr *elemhead, *elemtail;
CNtriaptr  triahead,  triatail;
CNrectptr  recthead,  recttail;
{
    CNtriaptr T;
    CNrectptr R;

    /* Store the triangles in the element list */
    R = NULL;
    for (T=triahead; T!=NULL; T=T->next) {
       (void) CNinsert_elem(elemhead, elemtail, T, R, CN_TRIA_STR);
    }

    /* Store the rectangles in the element list */
    T = NULL;
    for (R=recthead; R!=NULL; R=R->next) {
       (void) CNinsert_elem(elemhead, elemtail, T, R, CN_RECT_STR);
    }
}

