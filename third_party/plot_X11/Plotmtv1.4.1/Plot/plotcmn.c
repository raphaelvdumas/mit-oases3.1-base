/*
 * plotcmn.c - common utilities for X11 and PS plots
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "PXplot.h"
#include "CNplot.h"

#define EXTREMELY_SMALL 1.0e-99

/*
 * Procedure declarations
 */
void PXcheck_viewport();
void PXconvert_viewport_to_log();
void PXtranslate_range();
void PXget_autorange();
void PXidentify_view_planes();
void PXfind_outer_axes();
static int find_min_of_four();
static void transform_point();

void PXquery_contours();
int  PXquery_labels();

int  PXnamedColorIndex();
int  PXlineColorIndex();
int  PXfillColorIndex();

void PXadd_axislabel();
void PXfind_axis_precision();
static int coincident();
static int find_g_precision();
static int find_e_precision();
static int is_explabel();
void PXmodify_explabel();

/*
 * 2D VIEWPORT routines
 */

/*
 * Check and fix the viewport
 */
/*ARGSUSED*/
void PXcheck_viewport(min,max,bmin,bmax,logscale,absolute,probability,axis)
double *min, *max;              /* Range on axis to be fixed */
double bmin, bmax;              /* Actual plot boundary      */
short  *logscale;               /* Logarithmic scale on axis */
short  absolute;                /* Take the absolute value   */
short  probability;             /* This is a probability plot*/
char   *axis;                   /* Axis label, e.g. "x"      */
{
   double tmp, intv, xmin, xmax;
   int    reset;

   /* 
    * If absolute, then change the min/max
    */
   if (absolute) {
      if ( ((*min) < 0) || ((*max) < 0) ) {
         xmin = *min;
         xmax = *max;
         if (xmin > xmax) {
            xmin = *max;
            xmax = *min;
         }
         *max = LARGER_OF(fabs(xmin), fabs(xmax));
         *min = SMALLER_OF(fabs(xmin), fabs(xmax));
         if (xmax > 0) *min = 0.0;
      }
   }

   /* 
    * Check the viewport range (max > min) 
    */
   if (*max < *min) {
      (void) fprintf(stderr,"*** Warning : The %s viewport ",axis);
      (void) fprintf(stderr,
                     "minimum value is greater than the maximum value!\n");
      tmp  = *max;
      *max = *min;
      *min = tmp;
      (void) fprintf(stderr,"*** Switching the two... ");
      (void) fprintf(stderr,"%smin=%g  %smax=%g\n",axis,*min,axis,*max);
   }

   /* 
    * Check the viewport range (max - min) 
    */
   if ((*max - *min) < EXTREMELY_SMALL) {
      (void) fprintf(stderr,"*** Warning : The %s viewport ",axis);
      (void) fprintf(stderr,
                     "range (currently %g) is too small!\n", (*max - *min));
      intv = bmax-bmin;
      if (intv < EXTREMELY_SMALL) intv = 1.0e-5;
      *max = *max + intv;
      *min = *min - intv;
      (void) fprintf(stderr,"*** Resetting the %s-axis range... ",axis);
      (void) fprintf(stderr,"%smin=%g  %smax=%g\n",axis,*min,axis,*max);
   }

   /*
    * Adjust for probability scale
    *    min=0.000001   max=0.999999
    */
   if (probability) {
      reset = CN_FALSE;
      if (*max > 0.999999) {
         (void) fprintf(stderr,"*** Warning : The %s viewport ",axis);
         (void) fprintf(stderr,"maximum value is larger than 0.999999!\n");
         reset = CN_TRUE;
         *max = 0.999999;
      } else if (*max < 0.000001) {
         (void) fprintf(stderr,"*** Warning : The %s viewport ",axis);
         (void) fprintf(stderr,"maximum value is smaller than 0.000001!\n");
         reset = CN_TRUE;
         *max = 0.999999;
      }
      if (*min < 0.000001) {
         (void) fprintf(stderr,"*** Warning : The %s viewport ",axis);
         (void) fprintf(stderr,"minimum value is smaller than 0.000001!\n");
         reset = CN_TRUE;
         *min = 0.000001;
      } else if (*min > 0.999999) {
         (void) fprintf(stderr,"*** Warning : The %s viewport ",axis);
         (void) fprintf(stderr,"maximum value is larger than 0.999999!\n");
         reset = CN_TRUE;
         *min = 0.000001;
      }
      if (reset) {
      (void) fprintf(stderr,"*** Resetting the %s-axis range... ",axis);
      (void) fprintf(stderr,"%smin=%g  %smax=%g\n",axis,*min,axis,*max);
      }
      *logscale = CN_FALSE;
   }

   /*
    * Adjust for log-scale
    * At this point, max > min
    *    if max < 0
    *        check data boundary - if bmax < 0  => linear plot
    *                              else max = bmax
    *                                   min = 1.0e-5
    */
   if (*logscale) {
      if (*max <= 0.0) {
         (void) fprintf(stderr,"*** Warning : The %s viewport ",axis);
         (void) fprintf(stderr,"minimum and maximum values are both ");
         (void) fprintf(stderr,"either zero or negative!\n");
         if (bmax <= 0.0) {
            (void) fprintf(stderr,
            "*** The %s-axis will be plotted on a linear scale (%slog=OFF)\n",
            axis,axis);
            *logscale = CN_FALSE;
         } else {
            *max = bmax;
            *min = 1.0e-5;
            (void) fprintf(stderr,"*** Resetting the %s-axis range... ",axis);
            (void) fprintf(stderr,"%smin=%g  %smax=%g\n",axis,*min,axis,*max);
         }
      } else if (*min <= 0.0) {
         *min = (*max > 1.0e-5) ? 1.0e-5 : 0.1*(*max);
         (void) fprintf(stderr,"*** Warning : The %s viewport ",axis);
         (void) fprintf(stderr,"minimum value is ");
         (void) fprintf(stderr,"either zero or negative!\n");
         (void) fprintf(stderr,"*** Resetting the %s-axis range... ",axis);
         (void) fprintf(stderr,"%smin=%g  %smax=%g\n",axis,*min,axis,*max);
      }
   }
}


/*
 * Given xmin, xmax, and log/autorange options,
 * calculate the log10 axis range.
 * If autorange is set, then round down the min and round up the max.
 */
void PXconvert_viewport_to_log(lgmin, lgmax, min, max, logscale, autorange)
double *lgmin, *lgmax;       /* The new logarithmic range */
double min, max;             /* The original range        */
short  logscale;             /* Is this in log-scale      */
short  autorange;            /* Automatic range selection */
{
   double lmin,lmax;

   /*
    * If log axes then convert the boundary values.
    */
   if (logscale) {
      lmin = CNlog10(min);
      lmax = CNlog10(max);
      if (autorange) {
         /* Get rounded limits */
         lmin = CNround_down(lmin,0);
         lmax = CNround_up  (lmax,0);
      }
   } else {
      lmin = min;
      lmax = max;
   }

   /* Return the logged values */
   *lgmin = lmin;
   *lgmax = lmax;
}


/*
 * TRANSLATION ROUTINES
 */

/*
 * General translation
 */
void PXtranslate_range(x,xmin,xmax,y,ymin,ymax)
double *x;           /* The value to be obtained        */
double xmin, xmax;   /* The new range                   */
double y;            /* The value to be translated from */
double ymin, ymax;   /* The original range              */
{
   double dx, dy;
   /*
    *        xmin......x......xmax
    *          |       |        |
    *        ymin......y......ymax
    *
    *   (x    - xmin)  =  (y    - ymin)
    *   -------------     -------------
    *   (xmax - xmin)  =  (ymax - ymin)
    */
   dx = xmax - xmin;
   dy = ymax - ymin;
   
   /* Scale */
   if (fabs(dy) > EXTREMELY_SMALL) *x = xmin + (y - ymin)*(dx)/(dy);
   else                            *x = xmin;
}


/*
 * Autorange procedure
 */

/*
 * Get the rounded intervals.
 * Scales are to be drawn from dmin to dmax
 * But if autorange is set, then want rounded limits dmin2, dmax2
 * Example : min  = 0.1  max  = 0.9
 *        => dmin = 0.1  dmax = 0.9
 *           dmin2= 0.0  dmax2= 1.0
 *
 * The behavior is different for log-scale
 * In this case the min and max are assumed to be logged already
 * Example : real_min = 0.1  real_max = 0.9
 *           min  = -1   max  = -0.046
 *        => dmin = -1   dmax = -0.046
 *           dmin2= -1   dmax2= 0.0
 */
void PXget_autorange(min,max,
                     dmin,dmax,dmin2,dmax2,delta,dlog,autorng)
double min,max,*dmin,*dmax,*dmin2,*dmax2,*delta;
short  dlog, autorng;
{
   if (!dlog) {
      /* get rounded limits */
      *dmin = min;
      *dmax = max;
      CNget_autorange(*dmin,*dmax,dmin2,dmax2,delta);
   } else {
      *dmin = *dmin2 = min;
      *dmax = *dmax2 = max;
      if (!autorng) {
         /* Get rounded limits */
         *dmin2 = CNround_down(*dmin,0);
         *dmax2 = CNround_up(*dmax,0);
      }
      *delta = 1.0;
   }
}


/*
 * 3D-related plot routines 
 */

/* 
 * Identify the inner and outer planes in the current view
 */
void PXidentify_view_planes(xmin, xmax, ymin, ymax, zmin, zmax,
                       xminin, xmaxin, yminin, ymaxin, zminin, zmaxin,
                       view_transfo)
double   xmin, ymin, zmin, xmax, ymax, zmax;
int      *xminin, *yminin, *zminin, *xmaxin, *ymaxin, *zmaxin;
CNmatrix view_transfo;
{
   double x1,y1,z1,x2,y2,z2;

   /*
    * Look for the innermost x-y plane first
    */
   transform_point(xmin,ymin,zmin,&x1,&y1,&z1,view_transfo);
   transform_point(xmin,ymin,zmax,&x2,&y2,&z2,view_transfo);
   if (z2 > z1) {
      /* z=zmin is the inner plane */
      *zminin = 1;
      *zmaxin = 0;
   } else {
      *zminin = 0;
      *zmaxin = 1;
   }

   /*
    * Look for the innermost xz-plane 
    */
   transform_point(xmin,ymin,zmin,&x1,&y1,&z1,view_transfo);
   transform_point(xmin,ymax,zmin,&x2,&y2,&z2,view_transfo);
   if (z2 > z1) {
      /* y=ymin is the inner plane */
      *yminin = 1;
      *ymaxin = 0;
   } else {
      *yminin = 0;
      *ymaxin = 1;
   }

   /*
    * Look for the innermost yz-plane 
    */
   transform_point(xmin,ymin,zmin,&x1,&y1,&z1,view_transfo);
   transform_point(xmax,ymin,zmin,&x2,&y2,&z2,view_transfo);
   if (z2 > z1) {
      /* x=xmin is the inner plane */
      *xminin = 1;
      *xmaxin = 0;
   } else {
      *xminin = 0;
      *xmaxin = 1;
   }
}


/*
 * Find the points on the axes that are outer-most in the current view.
 */
void PXfind_outer_axes(xmin, ymin, zmin, xmax, ymax, zmax, view_transfo,
                       pta, ptb, ptc, ptd)
double   xmin, ymin, zmin, xmax, ymax, zmax;
CNmatrix view_transfo;
CNcoord  *pta, *ptb, *ptc, *ptd;
{
   double   x00, y00, x01, y01, x11, y11, x10, y10, ztmp;
   double   minz, maxz;
   int      i, i_ab, j_ab, i_bc, j_bc, i_cd, j_cd;
   double   xval[2][2], yval[2][2];

   /* 
    *      (xa,ya,za)
    *           |
    *           |
    *      (xb,yb,zb)
    *           \
    *            \
    *         (xc,yc,zc)
    *              ---- (xd,yd,zd)
    */

   /* Fill in the 2D array */
   xval[0][0] = xmin;  yval[0][0] = ymin;
   xval[0][1] = xmin;  yval[0][1] = ymax;
   xval[1][0] = xmax;  yval[1][0] = ymin;
   xval[1][1] = xmax;  yval[1][1] = ymax;

   /* 
    * Choose the z-value of the x-y plane first 
    * We are looking for the lower xy-plane
    * Assume the world z-axis is parallel to the plot y-axis.
    *    minz = (zmin ? zmax)
    *    Test (xmin,ymin,zmin).y vs. (xmin,ymin,zmax).y
    */
   transform_point(xmin,ymin,zmin,&x00,&y00,&ztmp,view_transfo);
   transform_point(xmin,ymin,zmax,&x01,&y01,&ztmp,view_transfo);
   minz = (y00 < y01) ? zmin : zmax;
   maxz = (y00 < y01) ? zmax : zmin;
    
   /* 
    * Find which of the (x,y) pairs on that xy-plane are in front
    */
   transform_point(xmin,ymax,zmin,&x01,&y01,&ztmp,view_transfo);
   transform_point(xmax,ymax,zmin,&x11,&y11,&ztmp,view_transfo);
   transform_point(xmax,ymin,zmin,&x10,&y10,&ztmp,view_transfo);

   /* Find the left-most point */
   i = find_min_of_four(x00,x01,x10,x11);

   /* 
    * Once the left-most point has been found, the next
    * point is adjacent to the left-most point.  Find this
    * by comparing y-values 
    * In X11, we want larger Y values.
    */
   switch (i) {
   case 0: i_ab = 0;  j_ab = 0;                     /* (xmin,ymin) */
           if (y01 < y10) { i_bc = 0;  j_bc = 1; }  /* (xmin,ymax) */
           else           { i_bc = 1;  j_bc = 0; }  /* (xmax,ymin) */
           i_cd = 1;  j_cd = 1;                     /* (xmax,ymax) */
           break;
   case 1: i_ab = 0;  j_ab = 1;                     /* (xmin,ymax) */
           if (y00 < y11) { i_bc = 0;  j_bc = 0; }  /* (xmin,ymin) */
           else           { i_bc = 1;  j_bc = 1; }  /* (xmax,ymax) */
           i_cd = 1;  j_cd = 0;                     /* (xmax,ymin) */
           break;
   case 2: i_ab = 1;  j_ab = 0;                     /* (xmax,ymin) */
           if (y00 < y11) { i_bc = 0;  j_bc = 0; }  /* (xmin,ymin) */
           else           { i_bc = 1;  j_bc = 1; }  /* (xmax,ymax) */
           i_cd = 0;  j_cd = 1;                     /* (xmin,ymax) */
           break;
   default:
   case 3: i_ab = 1;  j_ab = 1;                     /* (xmax,ymax) */
           if (y01 < y10) { i_bc = 0;  j_bc = 1; }  /* (xmin,ymax) */
           else           { i_bc = 1;  j_bc = 0; }  /* (xmax,ymin) */
           i_cd = 0;  j_cd = 0;                     /* (xmin,ymin) */
           break;
   }

   /* Now get the points on axes to be drawn */
   pta->x = xval[i_ab][j_ab];  pta->y = yval[i_ab][j_ab];  pta->z = maxz;
   ptb->x = xval[i_ab][j_ab];  ptb->y = yval[i_ab][j_ab];  ptb->z = minz;
   ptc->x = xval[i_bc][j_bc];  ptc->y = yval[i_bc][j_bc];  ptc->z = minz;
   ptd->x = xval[i_cd][j_cd];  ptd->y = yval[i_cd][j_cd];  ptd->z = minz;
}

/* Given 4 numbers, find the min */
static int find_min_of_four(x0, x1, x2, x3)
double x0, x1, x2, x3;
{
   int    mini, i;
   double x[4], minx;
   x[0] = x0;
   x[1] = x1;
   x[2] = x2;
   x[3] = x3;

   /* search for the min */
   minx = x0;
   mini = 0;
   for (i=1; i<4; i++)
      if (x[i] < minx) { minx = x[i]; mini = i; }

   /* Return */
   return(mini);
}


/*
 * Convenience function for transforming a point
 */
static void transform_point(x,y,z,X,Y,Z,view_transfo)
double    x, y, z;       /* Real-world coordinates         */
double   *X,*Y,*Z;       /* After transformation           */
CNmatrix view_transfo;   /* The view transformation matrix */
{
   CNcoord newpt, point;
   point.x = x;
   point.y = y;
   point.z = z;
   newpt = CNtransform_point(&point,view_transfo);
   *X = newpt.x;
   *Y = newpt.y;
   *Z = newpt.z;
}


/*
 * Set contour levels for the contour-type datasets
 */
void PXquery_contours(plotdata,cstephead,csteptail,contour_dptr)
CNplotsetptr plotdata;
CNcontstepptr *cstephead, *csteptail;
CNdatasetptr  *contour_dptr;
{
   CNcontstepptr C;
   CNdslistptr   DS;
   CNdatasetptr  Dptr;
   int           FOUND;

   if (plotdata==NULL) return;

   /* Initialize */
   *cstephead    = NULL;
   *csteptail    = NULL;
   *contour_dptr = NULL;

   /*
    * Go thru each 2D dataset and use the first contour dataset with
    * contfill turned on
    */
   FOUND = CN_FALSE;
   for (DS=plotdata->datahead; DS!=NULL && !FOUND; DS=DS->next) {
      if ( ((DS->Dptr->datatype==CN_CONTOUR) ||
            (DS->Dptr->datatype==CN_POLYGON)) &&
           (DS->Dptr->data_pr.contstyle == CN_FILLCONT) ){
         FOUND = CN_TRUE;
         Dptr = DS->Dptr;
      } else if (DS->Dptr->grid) {
         FOUND = CN_TRUE;
         Dptr = DS->Dptr;
      } else if (DS->Dptr->mesh4D && DS->Dptr->datatype==CN_MESH4D_C) {
         FOUND = CN_TRUE;
         Dptr = DS->Dptr;
      }
   }

   if (FOUND) {
      /*
       * Select the contour step size
       * This is taken from the pre-existing list
       */
      for (C=Dptr->cstephead; C!=NULL; C=C->next)
      (void) CNinsert_contstep(cstephead, csteptail, C->value);

      /* Add an additional step to help colored contours */
      if (Dptr->cstephead != NULL)
      (void) CNinsert_contstep(cstephead, csteptail, CN_LARGE);

      /* Set the contour-dptr */
      *contour_dptr = Dptr;
   }
}


/*
 * Find out if side labels are to be plotted
 */
int PXquery_labels(plotdata)
CNplotsetptr plotdata;
{
   int LABEL_FOUND=CN_FALSE, CONT_FOUND=CN_FALSE, FOUND=CN_FALSE;

   /* Check for line-labels */
   LABEL_FOUND= CNplotset_has_linelabels(plotdata);

   /* Now check for contour scales */
   CONT_FOUND = CNplotset_has_colored_contours(plotdata);

   /* return value */
   if (plotdata->plot_pr.sidelabel == CN_FALSE)
      return(CN_FALSE);
   else {
      FOUND = LABEL_FOUND + CONT_FOUND;
      return(FOUND);
   }
}



/*
 * COLOR indexing routines 
 */


/*
 * Return the index to a named color
 *   background=-1 and foreground=0 are returned unchanged
 *   colors between 0-9 are indexed to a unique number ranging
 *   from 0
 *   to   PX_MAX_NAMED_COLORS (10)
 */
int PXnamedColorIndex(namedcol)
int namedcol;
{
   int index;

   /*
    * -1 is the background color 
    *  0 is the foreground color
    *  1-11 are named colors (1=medblue, 2=yellow...)
    */

   /* Index to the appropriate color */
   if (namedcol <= 0)
      /* Return unchanged */
      index = namedcol;
   else
      /* One of 11 line colors */
      index = (namedcol-1) % (PX_MAX_NAMED_COLORS-2) + 2;

   return(index);
}

/*
 * Return the index to a line color
 *   background=-1 and foreground=0 are returned unchanged
 *   colors between 1-10 are indexed to a unique number ranging
 *   from PX_MAX_NAMED_COLORS+PX_MAX_FILL_COLORS (10+32)
 *   to   PX_MAX_NAMED_COLORS+PX_MAX_FILL_COLORS+PX_MAX_LINE_COLORS (10+32+10)
 */
int PXlineColorIndex(linecol)
int linecol;
{
   int index;

   /*
    * -1 is the background color
    *  0 is the foreground color
    *  1-10 are line colors
    */

   /* Index to the appropriate color */
   if (linecol <= 0)
      /* Return unchanged */
      index = linecol;
   else
      /* One of 10 line colors */
      index = ((linecol-1) % PX_MAX_LINE_COLORS) +
              PX_MAX_FILL_COLORS + PX_MAX_NAMED_COLORS;

   return(index);
}

/*
 * Return the index to a fill color
 *   background=-1 and foreground=0 are returned unchanged
 *   colors between 1-32 are indexed to a unique number ranging
 *   from PX_MAX_NAMED_COLORS (10)
 *   to   PX_MAX_NAMED_COLORS+PX_MAX_FILL_COLORS (10+32)
 */
int PXfillColorIndex(fillcol)
int fillcol;
{
   int index;

   /*
    * -1 is the background color
    *  0 is the foreground color
    *  1-32 are fill colors
    */

   /* Index to the appropriate color */
   if (fillcol <= 0)
      /* Return unchanged */
      index = fillcol;
   else
      /* One of 32 fill colors */
      index = (fillcol-1) % PX_MAX_FILL_COLORS + PX_MAX_NAMED_COLORS;

   return(index);
}



/*
 * LABELING/FORMATING UTILITIES
 */

/*
 * Add a label to the axislabel array
 */
void PXadd_axislabel(axislabels, nlabels, maxlabels, value, xpos, ypos)
PXlabel *axislabels;    /* Array containing label values and positions */
int     *nlabels;       /* Current no of array elements                */
int     maxlabels;      /* Array size                                  */
double  value;          /* value of the label e.g. 3.154               */
double  xpos;           /* x-center                                    */
double  ypos;           /* y-center                                    */
{
   /* Error check */
   if ((axislabels == NULL) || (*nlabels >= maxlabels)) return;
 
   /* Put in the label */
   axislabels[*nlabels].value = value;
   axislabels[*nlabels].x     = xpos; 
   axislabels[*nlabels].y     = ypos; 
   (*nlabels)++;
}

#define IDL_G_PRECISION  4
#define IDL_E_PRECISION  1
#define MAX_PRECISION    8

/*
 * Find the optimal precision for printing out the axis labels
 * The criteria is that adjacent axis labels should be different from
 * each other, i.e. cannot have "1.1   1.1   1.1" if values are 1.101, 1.102
 * etc.
 *
 * This can be potentially slow because of extensive use of string
 * comparisons, so don't test all the elements of the array.
 *
 * There's probably a better way to do this...
 *
 * Be a bit careful since the 0th and 1st value could be identical
 */
void PXfind_axis_precision(axislabels, nlabels, precision, explabel)
PXlabel *axislabels;    /* Array containing label values and positions */
int     nlabels;        /* Current no of array elements                */
int     *precision;     /* Precision used in formatting label %.[p]g   */
int     *explabel;      /* Use %e or %g format (exponential)           */
{
   int p1, p2, p3, i;

   /* Error check */
   if ((axislabels == NULL) || (nlabels <= 0)) return;

   /* Initialize */
   *precision = IDL_G_PRECISION;
   *explabel  = CN_FALSE; 

   /* First find the precision */
   if (nlabels == 2) {
      p1 = find_g_precision(axislabels[0].value, 
                            axislabels[1].value);
      *precision = p1;
   } else if (nlabels > 2 && nlabels < 4) {
      p1 = find_g_precision(axislabels[0].value, 
                            axislabels[1].value);
      p2 = find_g_precision(axislabels[0].value, 
                            axislabels[nlabels-1].value);
      p3 = find_g_precision(axislabels[nlabels-2].value, 
                            axislabels[nlabels-1].value);
      *precision = MAXOF3(p1, p2, p3);
   } else if (nlabels >= 4) {
      if (!coincident(axislabels[0], axislabels[1]))
         p1 = find_g_precision(axislabels[0].value, 
                               axislabels[1].value);
      else 
         p1 = find_g_precision(axislabels[1].value, 
                               axislabels[2].value);
      p2    = find_g_precision(axislabels[0].value, 
                               axislabels[nlabels-1].value);
      if (!coincident(axislabels[nlabels-1], axislabels[nlabels-2]))
         p3 = find_g_precision(axislabels[nlabels-2].value, 
                               axislabels[nlabels-1].value);
      else
         p3 = find_g_precision(axislabels[nlabels-3].value, 
                               axislabels[nlabels-2].value);
      *precision = MAXOF3(p1, p2, p3);
   }

   /* 
    * Now determine if we need to use exponential format,
    * in which case we have to do the precision test again
    * (because the best precision for exp is not necc the best for g format)
    * e.g.  100 1000 1e4 needs only one precision in %e format
    */
   for (i=0; i<nlabels && !(*explabel); i++) {
      if (is_explabel(axislabels[i].value, *precision))
         *explabel = CN_TRUE;
   }

   /*
    * If we are using exponential format, then redo the precision test
    */
   if (*explabel) {
      /* First find the precision */
      if (nlabels == 2) {
         p1 = find_e_precision(axislabels[0].value, 
                               axislabels[1].value);
         *precision = p1;
      } else if (nlabels > 2 && nlabels < 4) {
         p1 = find_e_precision(axislabels[0].value, 
                               axislabels[1].value);
         p2 = find_e_precision(axislabels[0].value, 
                               axislabels[nlabels-1].value);
         p3 = find_e_precision(axislabels[nlabels-2].value, 
                               axislabels[nlabels-1].value);
         *precision = MAXOF3(p1, p2, p3);
      } else if (nlabels >= 4) {
         if (!coincident(axislabels[0], axislabels[1]))
            p1 = find_e_precision(axislabels[0].value, 
                                  axislabels[1].value);
         else 
            p1 = find_e_precision(axislabels[1].value, 
                                  axislabels[2].value);
         p2    = find_e_precision(axislabels[0].value, 
                                  axislabels[nlabels-1].value);
         if (!coincident(axislabels[nlabels-1], axislabels[nlabels-2]))
            p3 = find_e_precision(axislabels[nlabels-2].value, 
                                  axislabels[nlabels-1].value);
         else
            p3 = find_e_precision(axislabels[nlabels-3].value, 
                                  axislabels[nlabels-2].value);
         *precision = MAXOF3(p1, p2, p3);
      }
   }
   /*
   (void) printf("precision=%d  explabel=%d\n",*precision,*explabel);
    */
}


/*
 * Check to see if 2 labels are coincident, ie. occupy the same space in
 * the plot window 
 */
static int coincident(axislabel1, axislabel2)
PXlabel axislabel1, axislabel2;
{
   int    coinc;
   double distsq;

   distsq = (axislabel1.x - axislabel2.x) * (axislabel1.x - axislabel2.x) +
          (axislabel1.y - axislabel2.y) * (axislabel1.y - axislabel2.y);
   if (distsq < 9.05)
      coinc = CN_TRUE;
   else
      coinc = CN_FALSE;
   /*
   printf("axislabel1=(%g %g) axislabel2=(%g %g) distsq=%g coinc=%d\n",
   axislabel1.x, axislabel1.y, axislabel2.x, axislabel2.y, distsq, coinc);
    */

   return(coinc);
}

/*
 * Find the best precision to use in the %g format by comparing strings
 */
static int find_g_precision(x1, x2)
double x1, x2;
{
   char label1[20], label2[20];
   int FOUND=CN_FALSE;
   int p, prec;

   /* If the two numbers are identical, then don't bother */
   if (fabs(x1-x2) < CN_TINY) return(IDL_G_PRECISION);

   /* 
    * Now loop thru the precisions and find the smallest that provides
    * differentiation between the two numbers
    */
   for (p=IDL_G_PRECISION; p<MAX_PRECISION && !FOUND; p++) {
      (void) sprintf(label1, "%.*g", p, x1);
      (void) sprintf(label2, "%.*g", p, x2);
      if (strcmp(label1, label2) != 0) {
         FOUND = CN_TRUE;
         prec  = p;
      }
   }
   if (!FOUND) prec = MAX_PRECISION;
   return(prec);
}

/*
 * Find the best precision to use in the %g format by comparing strings
 */
static int find_e_precision(x1, x2)
double x1, x2;
{
   char label1[20], label2[20];
   int FOUND=CN_FALSE;
   int p, prec;

   /* If the two numbers are identical, then don't bother */
   if (fabs(x1-x2) < CN_TINY) return(IDL_E_PRECISION);

   /* 
    * Now loop thru the precisions and find the smallest that provides
    * differentiation between the two numbers
    */
   for (p=IDL_E_PRECISION; p<MAX_PRECISION && !FOUND; p++) {
      (void) sprintf(label1, "%.*e", p, x1);
      (void) sprintf(label2, "%.*e", p, x2);
      if (strcmp(label1, label2) != 0) {
         FOUND = CN_TRUE;
         prec  = p;
      }
   }
   if (!FOUND) prec = MAX_PRECISION;
   return(prec);
}


/*
 * Check to see if the label is to be plotted with an exponent 
 */
static int is_explabel(vallbl, precision)
double vallbl;
int    precision;
{
   char   text[100];

   (void) sprintf(text,"%.*g",precision,vallbl);
   if (strrchr(text,'e') != NULL)
      return(CN_TRUE);
   else
      return(CN_FALSE);
}

/* 
 * Modify the exponentiated text-string e.g 1.0e+05 => 1.0e5
 */
void PXmodify_explabel(text, exponent, nonexponent, isexp, postscript)
char *text, *exponent, *nonexponent;
int  *isexp;
int  postscript;   /* Flag to put in filler for postscript */
{
   char   *epos;
   int    len, i;
 
   /* Initialize */
   *isexp = CN_FALSE;
 
   if ((epos = strrchr(text,'e')) != NULL) {
      /* Get the string address before the e */
      (void) strcpy(nonexponent,text);
      if ((strlen(text)-strlen(epos)) > 0)
         nonexponent[strlen(text)-strlen(epos)] = '\0';
 
      /* Get the str address after the e */
      epos++;
      (void) strcpy(exponent,epos);

      /* Get rid of the +0 */
      if ( ((len=strlen(exponent)) > 0) && (exponent[0]=='+')) {
         /* Get rid of '+' */
         for (i=0; i<len && exponent[i]!='\0'; i++)
            exponent[i] = exponent[i+1];
         /* Get rid of '0' */
         if ( ((len=strlen(exponent)) > 0) && (exponent[0]=='0')) {
            for (i=0; i<len && exponent[i]!='\0'; i++)
               exponent[i] = exponent[i+1];
         }
      } else if ( ((len=strlen(exponent)) > 1) &&
                   (exponent[0]=='-') && (exponent[1]=='0')) {
         /* Make -05 into -5 => Get rid of '0' */
         for (i=1; i<len && exponent[i]!='\0'; i++)
            exponent[i] = exponent[i+1];
      }
 
      /* Reformulate the text string */
      if (postscript) 
      (void) sprintf(text,"%sxee%s",nonexponent, exponent);
      else
      (void) sprintf(text,"%se%s",nonexponent, exponent);
 
      /* Set the flag */
      *isexp = CN_TRUE;
   }
}

