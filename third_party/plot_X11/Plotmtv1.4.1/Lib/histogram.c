/*
 * histogram.c - procedures to build and maintain a histogram
 *               data structure
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include "CNplot.h"

/*
 * HISTOGRAM DATA STRUCTURE
 *    A histogram is used to store data for a histogram plot.
 *    The data consists of points (x only) that are sorted into
 *    bins then plotted.
 */

#define NBINS    10
#define MAXBINS 1000

/*
 * Given a histogram dataset, sort the data into bins and create curves
 */
void CNsort_histogram(dptr, verbose)
CNdatasetptr dptr;
int          verbose;
{
   CNpointptr P;
   CNcurveptr Cptr;
   double     x1, y1, x2, y2;
   double     xmin, xmax, binwidth, binstart;
   double     dpower;
   int        power;
   int        bins[MAXBINS];
   int        bmax;
   int        i;
   int        curveID=0;

   /* Error-check */
   if (dptr == NULL) {
      (void) fprintf(stderr,"CNsort_histogram: Error! NULL dataset!\n");
      return;
   }
   if (dptr->datatype != CN_HISTOGRAM) {
      (void) fprintf(stderr,
                     "CNsort_histogram: Error! Need a HISTOGRAM dataset!\n");
      return;
   }
   if (dptr->histogram == NULL) {
      (void) fprintf(stderr,
                     "CNsort_histogram: Error! NULL HISTOGRAM structure!\n");
      return;
   }

   /* Get rid of the curvelist in the dataset first */
   CNdelete_curve_list(&(dptr->curvehead), &(dptr->curvetail));

   /* Get the binwidth and binstart - special treatment for xmin=xmax */
   xmin = dptr->histogram->xmin;
   xmax = dptr->histogram->xmax;
   if ((xmax - xmin) < CN_SMALL) {
      /* Just put in one bar */

      /* Set binwidth */
      binwidth = 0.0;
      if ((dptr->data_pr.flag2 & CNbinwidth) != 0) {
         binwidth = dptr->data_pr.binwidth;
      } else {
         /* The binwidth is on the order of the mean */
         dpower   = CNlog10(xmin);
         power    = (dpower > -CN_SMALL) ? (int)dpower : (int)dpower - 1;
         binwidth = CNround_down(xmin,power);
      }
      if (binwidth < CN_SMALL) binwidth = 0.1;

      /* Get the binstart */
      if ((dptr->data_pr.flag2 & CNbinstart) != 0) {
         binstart = dptr->data_pr.binstart;
      } else {
         binstart = xmin - 0.5*binwidth;
      }

      /* Set xmin, xmax to the size of 3 bars */
      xmin = xmin - 1.5*binwidth;
      xmax = xmax + 1.5*binwidth;

   } else {
      /* Default of 10 bars */

      /* Set the binwidth */
      if ((dptr->data_pr.flag2 & CNbinwidth) != 0) {
         binwidth = dptr->data_pr.binwidth;
         if ((binwidth <= 0.0) || ((xmax-xmin)/binwidth > MAXBINS)) 
            binwidth = (xmax - xmin)/NBINS;
      } else {
         binwidth = (xmax - xmin)/NBINS;
      }

      /* If the binwidth is too small then adjust it */
      if (binwidth < CN_SMALL) binwidth = 0.1;

      /* Get the binstart */
      if ((dptr->data_pr.flag2 & CNbinstart) != 0) {
         binstart = dptr->data_pr.binstart;
      } else {
         dpower   = CNlog10(xmax-xmin);
         power    = (dpower > -CN_SMALL) ? (int)dpower : (int)dpower - 1;
         binstart = CNround_down(xmin,power);
      }
   }

   /* Put the binwidth, binstart values back */
   dptr->data_pr.binwidth = binwidth;
   dptr->data_pr.binstart = binstart;

   /* Print info */
   if (verbose) {
      (void) fprintf(stdout,"Histogram:\n");
      (void) fprintf(stdout,"xmin      = %g\n",xmin);
      (void) fprintf(stdout,"xmax      = %g\n",xmax);
      (void) fprintf(stdout,"bin width = %g\n",binwidth);
      (void) fprintf(stdout,"bin start = %g\n",binstart);
   }

   /* Initialize the bins */
   for (i=0; i<MAXBINS; i++) bins[i] = 0; 

   /* Fill the bins */
   for (P=dptr->histogram->pointhead; P!=NULL; P=P->next) {
      i = (int)((P->x - binstart)/binwidth);
      if (i>=0 && i<MAXBINS) bins[i]++;
   }

   /* Create the curves */
   bmax = 0;
   for (i=0; i<MAXBINS; i++) {
      if (bins[i] > 0) {

         /* Boundary */
         x1 = binstart + (i  )*binwidth;
         x2 = binstart + (i+1)*binwidth;
         y1 = -1000.0;
         y2 = (double) bins[i];
         /*
         (void) printf("x1=%g x2=%g y1=%g y2=%g\n",x1,x2,y1,y2);
          */

         /* Min/Max */
         if (bins[i] > bmax) bmax = bins[i];
         if (x1 < xmin) xmin = x1;
         if (x2 > xmax) xmax = x2;

         /* Allocate a curve data-structure */
         Cptr = CNinsert_curve(&(dptr->curvehead), &(dptr->curvetail),
                               curveID++);
         if (Cptr!=NULL) {
            (void) CNinsert_point(&(Cptr->pointhead), &(Cptr->pointtail),
                                  x1,y1,0.0,0);
            (void) CNinsert_point(&(Cptr->pointhead), &(Cptr->pointtail),
                                  x1,y2,0.0,0);
            (void) CNinsert_point(&(Cptr->pointhead), &(Cptr->pointtail),
                                  x2,y2,0.0,0);
            (void) CNinsert_point(&(Cptr->pointhead), &(Cptr->pointtail),
                                  x2,y1,0.0,0);
            (void) CNinsert_point(&(Cptr->pointhead), &(Cptr->pointtail),
                                  x1,y1,0.0,0);
            /* Set the properties */
            Cptr->curv_pr.filltype = dptr->histogram->filltype;
            Cptr->curv_pr.flag     = Cptr->curv_pr.flag & CNfilltype;
            Cptr->curv_pr.fillcolor= dptr->histogram->fillcolor;
            Cptr->curv_pr.flag     = Cptr->curv_pr.flag & CNfillcolor;
            Cptr->curv_pr.linecolor= 0;
            Cptr->curv_pr.flag     = Cptr->curv_pr.flag & CNlinecolor;
         }
      }
   }

   /* Reset the dataset boundaries */
   dptr->bymin         = 0.0;
   dptr->bymax         = (double) (bmax+1);
   dptr->plot_pr.vymin = 0.0;
   dptr->plot_pr.vymax = (double) (bmax+1);
   dptr->bxmin         = xmin;
   dptr->bxmax         = xmax;
   dptr->plot_pr.vxmin = xmin;
   dptr->plot_pr.vxmax = xmax;

   /* Print info */
   if (verbose) {
      (void) fprintf(stdout, "Generated %d new curves\n",
                     CNcount_curves(dptr->curvehead, dptr->curvetail));
   }
}
        



/*
 * HISTOGRAM DATA STRUCTURE
 *    A histogram is used to store data for a histogram plot.
 *    The data consists of points (x only) that are sorted into
 *    bins then plotted.
 */
CNhistogramptr CNmake_histogram(xmin, xmax, pointhead, pointtail)
double     xmin, xmax;
CNpointptr pointhead, pointtail;
{
   CNhistogramptr newptr;
   unsigned int size = sizeof(CNhistogram);

   if ((newptr = (CNhistogramptr)malloc(size))!=NULL) {
      newptr->xmin      = xmin;
      newptr->xmax      = xmax;
      newptr->filltype  = CN_FILL_SOLID;
      newptr->fillcolor = 5;
      /* Linked lists */
      newptr->pointhead = pointhead;
      newptr->pointtail = pointtail;
   }
   return(newptr);
}


/*
 * Delete histogram 
 */
void CNdelete_histogram(Sptr)
CNhistogramptr Sptr;
{
   /* delete all the points in Dptr */
   CNdelete_point_list(&(Sptr->pointhead),&(Sptr->pointtail));

   /* Now delete Sptr */
   free ((char*)Sptr);
}


/*
 * Print info on the histogram
 */
void CNprint_histogram(Sptr)
CNhistogramptr Sptr;
{
   int npoints;

   /* Count the contents of the slice */
   npoints = CNcount_points(Sptr->pointhead,Sptr->pointtail);

   /* Print the contents of the slice */
   (void) fprintf(stdout,"Histogram: \n");
   (void) fprintf(stdout,"   No of points = %d\n", npoints);
}

