/*
 * misc.c - miscellaneous useful utilities 
 */

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "CNplot.h"

static void swap();

static int plotset_has_filledcontour();
static int plotset_has_grid();
static int plotset_has_mesh4Dquant();

/*
 * Find out if there is a color contour dataset in the plotset
 */
int CNplotset_has_colored_contours(Pptr)
CNplotsetptr Pptr;
{
   int FOUND = CN_FALSE;

   /* Error Checking */
   if (Pptr == NULL) {
      (void) fprintf(stderr,
             "CNplotset_has_colored_contours() : Error! Null plotset!\n");
      return(0);
   }

   FOUND = plotset_has_filledcontour(Pptr) ||
           plotset_has_grid(Pptr) ||
           plotset_has_mesh4Dquant(Pptr);

   /* Return status */
   return(FOUND);
}

/*
 * Find out if there is a filled contour dataset in the plotset
 */
static int plotset_has_filledcontour(Pptr)
CNplotsetptr Pptr;
{
   CNdslistptr DS;
   int FOUND = CN_FALSE;

   /* Error Checking */
   if (Pptr == NULL) {
      (void) fprintf(stderr,
                     "CNplotset_has_filledcontour() : Error! Null plotset!\n");
      return(0);
   }

   /* Go thru the list */
   for (DS=Pptr->datahead; DS!=NULL && !FOUND; DS=DS->next)
      if ( ((DS->Dptr->datatype == CN_CONTOUR) || 
            (DS->Dptr->datatype == CN_POLYGON)) && 
           (DS->Dptr->data_pr.contstyle == CN_FILLCONT))
         FOUND = CN_TRUE;

   /* Return status */
   return(FOUND);
}

/*
 * Find out if there is a grid dataset in the plotset
 */
static int plotset_has_grid(Pptr)
CNplotsetptr Pptr;
{
   CNdslistptr DS;
   int FOUND = CN_FALSE;

   /* Error Checking */
   if (Pptr == NULL) {
      (void) fprintf(stderr,
                     "CNplotset_has_grid() : Error! Null plotset!\n");
      return(0);
   }

   /* Go thru the list */
   for (DS=Pptr->datahead; DS!=NULL && !FOUND; DS=DS->next)
      if (DS->Dptr->grid && DS->Dptr->datatype==CN_GRID4D) FOUND = CN_TRUE;

   /* Return status */
   return(FOUND);
}

/*
 * Find out if there is a quant-mesh4D dataset in the plotset
 */
static int plotset_has_mesh4Dquant(Pptr)
CNplotsetptr Pptr;
{
   CNdslistptr DS;
   int FOUND = CN_FALSE;

   /* Error Checking */
   if (Pptr == NULL) {
      (void) fprintf(stderr,
                     "CNplotset_has_mesh4Dquant() : Error! Null plotset!\n");
      return(0);
   }

   /* Go thru the list */
   for (DS=Pptr->datahead; DS!=NULL && !FOUND; DS=DS->next)
      if (DS->Dptr->mesh4D && DS->Dptr->datatype==CN_MESH4D_C) FOUND = CN_TRUE;

   /* Return status */
   return(FOUND);
}

/*
 * Find out if there is a line-label dataset in the plotset
 */
int CNplotset_has_linelabels(Pptr)
CNplotsetptr Pptr;
{
   CNdslistptr DS;
   CNcurveptr  C;
   int FOUND = CN_FALSE;

   /* Error Checking */
   if (Pptr == NULL) {
      (void) fprintf(stderr,
                     "CNplotset_has_linelabels() : Error! Null plotset!\n");
      return(0);
   }

   /* Go thru the list */
   for (DS=Pptr->datahead; DS!=NULL && !FOUND; DS=DS->next) {

      /* Plot only the 2D/3D plot labels */
      if ((DS->Dptr->datatype==CN_PLOT2D) ||
          (DS->Dptr->datatype==CN_PROBAB) ||
          (DS->Dptr->datatype==CN_BARCHART) ||
          (DS->Dptr->datatype==CN_PLOT3D)){

        /* Curve set */
        for (C=DS->Dptr->curvehead; C!=NULL && !FOUND; C=C->next) {
          if((C->curv_pr.linelabel!=NULL)&&CNstrlen(C->curv_pr.linelabel)!=0)
          FOUND = CN_TRUE;
        }
      }
   }

   /* Return status */
   return(FOUND);
}


/*
 * Find out if there is a contour in the data list
 */
int CNplotset_has_contour(Pptr)
CNplotsetptr Pptr;
{
   CNdslistptr DS;
   int FOUND = CN_FALSE;

   /* Error Checking */
   if (Pptr == NULL) {
      (void) fprintf(stderr,"CNplotset_has_contour() : Error! Null plotset!\n");
      return(0);
   }

   /* Go thru the list */
   for (DS=Pptr->datahead; DS!=NULL && !FOUND; DS=DS->next)
      if (DS->Dptr->datatype == CN_CONTOUR)
         FOUND = CN_TRUE;

   /* Return status */
   return(FOUND);
}


/*
 * Go through the data sets in the plotset and find the
 * largest boundary that will fit around all the datasets
 */
void
CNset_plotset_boundaries(Pptr, xmin, xmax, ymin, ymax, zmin, zmax)
CNplotsetptr Pptr;
double       *xmin, *xmax, *ymin, *ymax, *zmin, *zmax;
{
   CNdslistptr D;

   /* check to see if there is any data */
   if (Pptr == NULL) {
      (void) fprintf(stderr,"   ***Error! No data in the plotset\n");
      return;  
   }

   /* Initialize the boundary data */
   *xmin =  CN_LARGE; 
   *xmax = -CN_LARGE;
   *ymin =  CN_LARGE;
   *ymax = -CN_LARGE;
   *zmin =  CN_LARGE;
   *zmax = -CN_LARGE;

   /* Go thru the data sets and compare boundaries */
   for (D=Pptr->datahead; D!=NULL; D=D->next) {
      if (D->Dptr->bxmin < *xmin) *xmin = D->Dptr->bxmin;
      if (D->Dptr->bxmax > *xmax) *xmax = D->Dptr->bxmax;
      if (D->Dptr->bymin < *ymin) *ymin = D->Dptr->bymin;
      if (D->Dptr->bymax > *ymax) *ymax = D->Dptr->bymax;
      if (D->Dptr->datatype != CN_PLOT2D) {
         if (D->Dptr->bzmin < *zmin) *zmin = D->Dptr->bzmin;
         if (D->Dptr->bzmax > *zmax) *zmax = D->Dptr->bzmax;
      }
   }
   if (*zmin > *zmax) {
      *zmin = Pptr->datahead->Dptr->bzmin;
      *zmax = Pptr->datahead->Dptr->bzmax;
   }
}


/*
 * Go through the data sets in the plotset and find the
 * largest viewport that will fit around all the datasets
 */
void
CNset_plotset_viewport(Pptr, xmin, xmax, ymin, ymax, zmin, zmax)
CNplotsetptr Pptr;
double       *xmin, *xmax, *ymin, *ymax, *zmin, *zmax;
{
   CNdslistptr D;
   CNdatasetptr DP;

   /* check to see if there is any data */
   if (Pptr == NULL) {
      (void) fprintf(stderr,"   ***Error! No data in the plotset\n");
      return;  
   }

   /* Initialize the viewport data */
   *xmin = Pptr->datahead->Dptr->plot_pr.vxmin;
   *xmax = Pptr->datahead->Dptr->plot_pr.vxmax;
   *ymin = Pptr->datahead->Dptr->plot_pr.vymin;
   *ymax = Pptr->datahead->Dptr->plot_pr.vymax;
   *zmin = Pptr->datahead->Dptr->plot_pr.vzmin;
   *zmax = Pptr->datahead->Dptr->plot_pr.vzmax;

   /* Go thru the data sets and compare boundaries */
   for (D=Pptr->datahead->next; D!=NULL; D=D->next) {
      DP = D->Dptr;
      if (DP->plot_pr.vxmin < *xmin) *xmin = DP->plot_pr.vxmin;
      if (DP->plot_pr.vxmax > *xmax) *xmax = DP->plot_pr.vxmax;
      if (DP->plot_pr.vymin < *ymin) *ymin = DP->plot_pr.vymin;
      if (DP->plot_pr.vymax > *ymax) *ymax = DP->plot_pr.vymax;
      if (DP->plot_pr.vzmin < *zmin) *zmin = DP->plot_pr.vzmin;
      if (DP->plot_pr.vzmax > *zmax) *zmax = DP->plot_pr.vzmax;
   }
}

/*
 * Reset the viewport for the plotset
 */
void CNreset_plotset_viewport(WP)
CNplotsetptr WP;
{
   double xmin, xmax, ymin, ymax, zmin, zmax;

   /* Error Checking */
   if (WP==NULL) {
      (void) fprintf(stderr,
                     "CNreset_plotset_viewport() : Error - Null plotset!\n");
      return;
   }

   /*
    * Reset the plot boundaries
    * The values returned are the bounding-box values of all the
    * datasets contained within the plotset
    */
   CNset_plotset_viewport(WP, &xmin, &xmax, &ymin, &ymax, &zmin, &zmax);
   if ((WP->plot_pr.flag1 & CNvxmin) == 0) WP->plot_pr.vxmin = xmin;
   if ((WP->plot_pr.flag1 & CNvxmax) == 0) WP->plot_pr.vxmax = xmax;
   if ((WP->plot_pr.flag1 & CNvymin) == 0) WP->plot_pr.vymin = ymin;
   if ((WP->plot_pr.flag1 & CNvymax) == 0) WP->plot_pr.vymax = ymax;
   if ((WP->plot_pr.flag1 & CNvzmin) == 0) WP->plot_pr.vzmin = zmin;
   if ((WP->plot_pr.flag1 & CNvzmax) == 0) WP->plot_pr.vzmax = zmax;

   /* Save the current viewport */
   WP->plot_pr.prev_vxmin = WP->plot_pr.vxmin;
   WP->plot_pr.prev_vxmax = WP->plot_pr.vxmax;
   WP->plot_pr.prev_vymin = WP->plot_pr.vymin;
   WP->plot_pr.prev_vymax = WP->plot_pr.vymax;
   WP->plot_pr.prev_vzmin = WP->plot_pr.vzmin;
   WP->plot_pr.prev_vzmax = WP->plot_pr.vzmax;

   /* Reset the 3D window */
   CNreinitialize_view(WP->view_pr,xmin,xmax,ymin,ymax,zmin,zmax);
}

/*
 * Make a copy of a dataset
 */
CNdatasetptr CNcopy_dataset(Dptr,dataID)
CNdatasetptr Dptr;
int          *dataID;
{
   CNdatasetptr Dnew=NULL;
   CNcurveptr   C, Cnew;
   CNpointptr   P;

   /* Error Checking */
   if (Dptr==NULL) {
      (void) fprintf(stderr,
                     "CNcopy_dataset() : Error - Null dataset!\n");
      return(NULL);
   }
   
   /* This only works for curve datasets */
   if (!(Dptr->datatype == CN_PLOT2D || Dptr->datatype == CN_PLOT3D)) {
      (void) fprintf(stdout,
                     "Sorry - can only copy curve datasets for now.\n");
      return(NULL);
   }

   /* Check curve list */
   if (Dptr->curvehead == NULL) {
      (void) fprintf(stderr,
                     "CNcopy_dataset() : Error - No curves in dataset!\n");
      return(NULL);
   }
  
   /* Create a new dataset */
   Dnew = CNmake_dataset(Dptr->filename,
                         Dptr->label,
                         (int) Dptr->datatype,
                         Dptr->bxmin, Dptr->bxmax,
                         Dptr->bymin, Dptr->bymax,
                         Dptr->bzmin, Dptr->bzmax,
                         Dptr->plot_pr.vxmin, Dptr->plot_pr.vxmax,
                         Dptr->plot_pr.vymin, Dptr->plot_pr.vymax,
                         Dptr->plot_pr.vzmin, Dptr->plot_pr.vzmax,
                         *dataID);
   if (Dnew == NULL) return(NULL);

   /* Increment the dataID */
   (*dataID)++;

   /* Copy the curves */
   for (C=Dptr->curvehead; C!=NULL; C=C->next) {
      /* Allocate a curve data structure */
      Cnew = CNinsert_curve(&(Dnew->curvehead), &(Dnew->curvetail), C->ID);

      /* Copy the properties */
      CNset_curve_property(&(Cnew->curv_pr), &(C->curv_pr));

      /* Copy the points */
      for (P=C->pointhead; P!=NULL; P=P->next)
         (void) CNinsert_point(&(Cnew->pointhead),&(Cnew->pointtail),
                               P->x,P->y,P->z,(int)P->ID);
   }

   /* Copy the properties */
   CNset_plotset_property(&(Dnew->plot_pr), &(Dptr->plot_pr));
   CNset_dataset_property(&(Dnew->data_pr), &(Dptr->data_pr));

   /* Return */
   return(Dnew);
}

/*
 * Transform the data in a dataset
 */
int CNtransform_dataset(Dptr, xshift, yshift, zshift,
                            xmult,  ymult,  zmult)
CNdatasetptr Dptr;
double xshift, yshift, zshift, xmult,  ymult,  zmult;
{
   CNcurveptr C;
   CNpointptr P;
   int        status = 0;

   /* 
    * x' = xmult * x + xshift
    */

   /* Error Checking */
   if (Dptr==NULL) {
      (void) fprintf(stderr,
                     "CNtransform_dataset() : Error - Null dataset!\n");
      return(0);
   }
   
   /* Modify each curve in the dataset */
   if (Dptr->datatype == CN_PLOT2D || Dptr->datatype == CN_PLOT3D) {
      status = 1;
      for (C=Dptr->curvehead; C!=NULL; C=C->next) {
         for (P=C->pointhead; P!=NULL; P=P->next) {
            P->x = xmult*P->x + xshift; 
            P->y = ymult*P->y + yshift; 
            P->z = zmult*P->z + zshift; 
         }
      }
   }

   /* Reset the boundary */
   Dptr->bxmin = xmult*Dptr->bxmin + xshift; 
   Dptr->bymin = ymult*Dptr->bymin + yshift; 
   Dptr->bzmin = zmult*Dptr->bzmin + zshift; 
   Dptr->bxmax = xmult*Dptr->bxmax + xshift; 
   Dptr->bymax = ymult*Dptr->bymax + yshift; 
   Dptr->bzmax = zmult*Dptr->bzmax + zshift; 
   swap(&(Dptr->bxmin), &(Dptr->bxmax));
   swap(&(Dptr->bymin), &(Dptr->bymax));
   swap(&(Dptr->bzmin), &(Dptr->bzmax));

   /* Reset the viewport */
   Dptr->plot_pr.vxmin = xmult*Dptr->plot_pr.vxmin + xshift; 
   Dptr->plot_pr.vymin = ymult*Dptr->plot_pr.vymin + yshift; 
   Dptr->plot_pr.vzmin = zmult*Dptr->plot_pr.vzmin + zshift; 
   Dptr->plot_pr.vxmax = xmult*Dptr->plot_pr.vxmax + xshift; 
   Dptr->plot_pr.vymax = ymult*Dptr->plot_pr.vymax + yshift; 
   Dptr->plot_pr.vzmax = zmult*Dptr->plot_pr.vzmax + zshift; 
   swap(&(Dptr->plot_pr.vxmin), &(Dptr->plot_pr.vxmax));
   swap(&(Dptr->plot_pr.vymin), &(Dptr->plot_pr.vymax));
   swap(&(Dptr->plot_pr.vzmin), &(Dptr->plot_pr.vzmax));

   return(status);
}

static void swap(a,b)
double *a, *b;
{
   double tmp;

   if (*a > *b) {
      tmp = *a;
      *a = *b;
      *b = tmp;
   } 
}
