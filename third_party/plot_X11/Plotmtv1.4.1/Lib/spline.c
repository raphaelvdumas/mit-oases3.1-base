/*
 * Routines for spline interpolation and approximation
 *    - Cubic B-Spline (open, closed)
 *    - Doubled Cubic B-Spline (open, closed)
 *    - Quadratic B-Spline (open, closed)
 *    - Catmull-Rom Spline (open, closed)
 *    - Cubic Bezier spline (open, closed)
 *    - Quadratic Bezier Spline (open, closed)
 */

#include <stdio.h>
#include <stdlib.h>
#include "CNspline.h"

#define SMALL 1.0e-5

static double eval_cubic_spline();
static double eval_quadr_spline();
static double eval_ctrom_spline();
static double eval_quadr_bezier_spline();
static double eval_cubic_bezier_spline();

/*
 * Retrun a string denoting the spline type
 */
char *CNsplinetype(splinetype)
int splinetype;
{
   char *plot;

   switch (splinetype) {
   case CN_SP_NONE   : 
        plot="Linear (Spline Interpolation is NOT used)"; 
        break;
   case CN_SP_CUBICB    : 
        plot="Cubic B-Spline (Approximates control points)";  
        break;
   case CN_SP_DBLCUBICB : 
        plot="Doubled Cubic B-Spline (Closer Approximation of control points)";
        break;
   case CN_SP_QUADRB    : 
        plot="Quadratic B-Spline (Approximates control points)";  
        break;
   case CN_SP_CTROM     : 
        plot="Catmull-Rom Spline (Interpolates through control points)";
        break;
   case CN_SP_QDBEZIER  : 
        plot="Quadratic Bezier Spline (Hits midpoints of control points)"; 
        break;
   case CN_SP_CBBEZIER  :
   default              : 
        plot="Cubic Bezier Spline (Hits midpoints of control points)";
        break;
   }

   return(plot);
}


/*
 * Create a spline curve from a given array
 */
void CNcreate_spline(xarr,npts,xs,nspts,ndiv,splinetype,closed)
double xarr[], xs[];
int    npts, *nspts, ndiv, splinetype, closed;
{
   switch (splinetype) {
   case CN_SP_CUBICB   :
         /* Single cubic B-spline */
         if (!closed) 
            CNmake_cubic_B_spline(xarr,npts,xs,nspts,ndiv);
         else 
            CNmake_closed_cubic_B_spline(xarr,npts,xs,nspts,ndiv);
         break;
   case CN_SP_DBLCUBICB:
         /* Double cubic B-spline */
         if (!closed) 
            CNmake_double_cubic_B_spline(xarr,npts,xs,nspts,ndiv);
         else 
            CNmake_double_closed_cubic_B_spline(xarr,npts,xs,nspts,ndiv);
         break;
   case CN_SP_QUADRB   :
         /* Single quadratic spline */
         if (!closed) 
            CNmake_quadr_B_spline(xarr,npts,xs,nspts,ndiv);
         else 
            CNmake_closed_quadr_B_spline(xarr,npts,xs,nspts,ndiv);
         break;
   case CN_SP_CTROM    :
         /* Single catmull-rom spline */
         if (!closed) 
            CNmake_ctrom_spline(xarr,npts,xs,nspts,ndiv);
         else 
            CNmake_closed_ctrom_spline(xarr,npts,xs,nspts,ndiv);
         break;
   case CN_SP_QDBEZIER :
         /* Single Quadratic Bezier spline */
         if (!closed) 
            CNmake_quadr_bezier_spline(xarr,npts,xs,nspts,ndiv);
         else
            CNmake_closed_quadr_bezier_spline(xarr,npts,xs,nspts,ndiv);
         break;
   case CN_SP_CBBEZIER :
   default             :
         /* Single Cubic Bezier spline */
         if (!closed) 
            CNmake_cubic_bezier_spline(xarr,npts,xs,nspts,ndiv);
         else
            CNmake_closed_cubic_bezier_spline(xarr,npts,xs,nspts,ndiv);
         break;
   }
}


/* 
 * Single open cubic spline 
 *    Spline curve approximates the real curve and terminates at end-points
 *    Control points are doubled only at the end-points
 */
void CNmake_cubic_B_spline(xarr,npts,xs,nspts,ndiv)
double xarr[], xs[];
int    npts, *nspts, ndiv;
{
   double u,du;
   int    i,j;

   /* If too few points, just copy the original array to the new array */
   if (npts <= 2) {
      *nspts = npts;
      for (i=0; i<npts; i++) 
         xs[i] = xarr[i];
      return;
   }  

   /* Parameter subdivision */
   if (ndiv <= 0) ndiv = 20;
   du = 1.0/(double)ndiv;

   j=0;
   xs[j++] = xarr[0];  /* Initial Point */
   for (i=0; i<npts-1; i++) {
      u=0;
      do {
         if (i==0)
            xs[j] = eval_cubic_spline(u,xarr[i],xarr[i],xarr[i+1],xarr[i+2]);
         else if (i==npts-2)
            xs[j] = eval_cubic_spline(u,xarr[i-1],xarr[i],xarr[i+1],xarr[i+1]);
         else
            xs[j] = eval_cubic_spline(u,xarr[i-1],xarr[i],xarr[i+1],xarr[i+2]);
         u += du;
         j++;
      } while (u < 1.0 + SMALL);
   }
   xs[j++] = xarr[npts-1];  /* Final Point */ 
   *nspts = j;
}


/* 
 * Single closed cubic spline 
 *    Spline curve approximates the real curve
 *    Control points are "folded over" at the end-points
 */
void CNmake_closed_cubic_B_spline(xarr,npts,xs,nspts,ndiv)
double xarr[], xs[];
int    npts, *nspts, ndiv;
{
   double u, du;
   int    i,j;

   /* If too few points, just copy the original array to the new array */
   if (npts <= 2) {
      *nspts = npts;
      for (i=0; i<npts; i++) 
         xs[i] = xarr[i];
      return;
   }  

   /* Parameter subdivision */
   if (ndiv <= 0) ndiv = 20;
   du = 1.0/(double)ndiv;

   j=0;
   for (i=0; i<npts; i++) {
      u=0;
      do {
         if (i==0)
          xs[j]=eval_cubic_spline(u,xarr[npts-1],xarr[i],xarr[i+1],xarr[i+2]);
         else if (i==npts-2) 
          xs[j]=eval_cubic_spline(u,xarr[i-1],xarr[i],xarr[i+1],xarr[0]);
         else if (i==npts-1) 
          xs[j]=eval_cubic_spline(u,xarr[i-1],xarr[i],xarr[0],xarr[1]);
         else 
          xs[j]=eval_cubic_spline(u,xarr[i-1],xarr[i],xarr[i+1],xarr[i+2]);
         u += du;
         j++;
      } while (u < 1.0 + SMALL);
   }
   *nspts = j;
}


/* 
 * Doubled cubic B-spline 
 *    All Control points are doubled
 */
void CNmake_double_cubic_B_spline(xarr,npts,xs,nspts,ndiv)
double xarr[], xs[];
int    npts, *nspts, ndiv;
{
   double u, du;
   int    i, j;

   /* If too few points, just copy the original array to the new array */
   if (npts <= 2) {
      *nspts = npts;
      for (i=0; i<npts; i++) 
         xs[i] = xarr[i];
      return;
   }  

   /* Parameter subdivision */
   if (ndiv <= 0) ndiv = 20;
   du = 1.0/(double)ndiv;

   j=0;
   for (i=0; i<npts-1; i++) {
      u=0;
      do {
         if (i==0)
            xs[j] = eval_cubic_spline(u,xarr[i],xarr[i],xarr[i],xarr[i+1]);
         else 
            xs[j] = eval_cubic_spline(u,xarr[i-1],xarr[i],xarr[i],xarr[i+1]);
         u += du;
         j++;
      } while (u < 1.0 + SMALL);
   }
   xs[j++] = xarr[npts-1];  /* Final Point */ 
   *nspts = j;
}


/* 
 * Doubled closed cubic B-spline 
 *    All Control points are doubled
 */
void CNmake_double_closed_cubic_B_spline(xarr,npts,xs,nspts,ndiv)
double xarr[], xs[];
int    npts, *nspts, ndiv;
{
   double u, du;
   int    i, j;

   /* If too few points, just copy the original array to the new array */
   if (npts <= 2) {
      *nspts = npts;
      for (i=0; i<npts; i++) 
         xs[i] = xarr[i];
      return;
   }  

   /* Parameter subdivision */
   if (ndiv <= 0) ndiv = 20;
   du = 1.0/(double)ndiv;

   j=0;
   for (i=0; i<npts; i++) {
      u=0;
      do {
         if (i==0)
            xs[j] = eval_cubic_spline(u,xarr[npts-1],xarr[i],xarr[i],xarr[i+1]);
         else if (i==npts-1)
            xs[j] = eval_cubic_spline(u,xarr[i-1],xarr[i],xarr[i],xarr[0]);
         else 
            xs[j] = eval_cubic_spline(u,xarr[i-1],xarr[i],xarr[i],xarr[i+1]);
         u += du;
         j++;
      } while (u < 1.0 + SMALL);
      /* 
       * The spline above does the curves near the points.
       * For completeness, need to do spline(x[i],x[i],x[i+1],x[i+1])
       * But since that spline is essentially a straight line, just add in
       * a straight line 
       */
      if (i==npts-1) {
         xs[j++] = eval_cubic_spline(0.0,xarr[i],xarr[i],xarr[0],xarr[0]);
         xs[j++] = eval_cubic_spline(1.0,xarr[i],xarr[i],xarr[0],xarr[0]);
      } else {
         xs[j++] = eval_cubic_spline(0.0,xarr[i],xarr[i],xarr[i+1],xarr[i+1]);
         xs[j++] = eval_cubic_spline(1.0,xarr[i],xarr[i],xarr[i+1],xarr[i+1]);
      }
   }
   *nspts = j;
}


/* 
 * Evaluate the cubic B-spline 
 */
static double eval_cubic_spline(u,xa,xb,xc,xd)
double u,xa,xb,xc,xd;
{
   double c;
   
   /* Check the value of u */
   if (u < -SMALL || u > 1.0 + SMALL) {
   (void) fprintf(stderr,"Error - attempt to evaluate u=%f outside [0,1] range\n",u);
   return(0.0);
   }
   c = u*u*u*(-1*xa + 3*xb - 3*xc + xd)
       + u*u*( 3*xa - 6*xb + 3*xc     )
       +   u*(-3*xa        + 3*xc     )
       +     (   xa + 4*xb +   xc     );
   c = c/6.0;
   return(c);
}


/* 
 * Single quadratic B-spline 
 *    Spline curve approximates the real curve and terminates at end-points
 *    Control points are doubled only at the end-points
 */
void CNmake_quadr_B_spline(xarr,npts,xs,nspts,ndiv)
double xarr[], xs[];
int    npts, *nspts, ndiv;
{
   double u, du;
   int    i, j;

   /* If too few points, just copy the original array to the new array */
   if (npts <= 2) {
      *nspts = npts;
      for (i=0; i<npts; i++) 
         xs[i] = xarr[i];
      return;
   }  

   /* Parameter subdivision */
   if (ndiv <= 0) ndiv = 20;
   du = 1.0/(double)ndiv;

   j=0;
   xs[j++] = xarr[0];  /* Initial Point */
   for (i=0; i<npts-1; i++) {
      u=0;
      do {
         if (i==0) 
            xs[j] = eval_quadr_spline(u,xarr[i],xarr[i],xarr[i+1]);
         else 
            xs[j] = eval_quadr_spline(u,xarr[i-1],xarr[i],xarr[i+1]);
         u += du;
         j++;
      } while (u < 1.0 + SMALL);
   }
   xs[j++] = xarr[npts-1];  /* Final Point */ 
   *nspts = j;
}


/* 
 * Single quadratic spline 
 *    Spline curve approximates the real curve
 *    Control points are "folded over" at the end-points
 */
void CNmake_closed_quadr_B_spline(xarr,npts,xs,nspts,ndiv)
double xarr[], xs[];
int    npts, *nspts, ndiv;
{
   double u, du;
   int    i, j;

   /* If too few points, just copy the original array to the new array */
   if (npts <= 2) {
      *nspts = npts;
      for (i=0; i<npts; i++) 
         xs[i] = xarr[i];
      return;
   }  

   /* Parameter subdivision */
   if (ndiv <= 0) ndiv = 20;
   du = 1.0/(double)ndiv;

   j=0;
   for (i=0; i<npts; i++) {
      u=0;
      do {
         if (i==0) 
            xs[j] = eval_quadr_spline(u,xarr[npts-1],xarr[i],xarr[i+1]);
         else if (i==npts-1)
            xs[j] = eval_quadr_spline(u,xarr[i-1],xarr[i],xarr[0]);
         else 
            xs[j] = eval_quadr_spline(u,xarr[i-1],xarr[i],xarr[i+1]);
         u += du;
         j++;
      } while (u < 1.0 + SMALL);
   }
   *nspts = j;
}


/* 
 * Evaluate the quadratic B-spline 
 */
static double eval_quadr_spline(u,xa,xb,xc)
double u,xa,xb,xc;
{
   double c;
   
   /* Check the value of u */
   if (u < -SMALL || u > 1.0 + SMALL) {
   (void) fprintf(stderr,"Error - attempt to evaluate u=%f outside [0,1] range\n",u);
   return(0.0);
   }
   c =   u*u*(   xa - 2*xb +   xc)
       +   u*(-2*xa + 2*xb       )
       +     (   xa +   xb       );
   c = c/2.0;
   return(c);
}


/* 
 * Single (Open) Catmull-Rom spline 
 *    The splines pass through the real datapoints.
 *    Control points are doubled only at the end-points
 */
void CNmake_ctrom_spline(xarr,npts,xs,nspts,ndiv)
double xarr[], xs[];
int    npts, *nspts, ndiv;
{
   double u, du;
   int    i, j;

   /* If too few points, just copy the original array to the new array */
   if (npts <= 2) {
      *nspts = npts;
      for (i=0; i<npts; i++) 
         xs[i] = xarr[i];
      return;
   }  

   /* Parameter subdivision */
   if (ndiv <= 0) ndiv = 20;
   du = 1.0/(double)ndiv;

   j=0;
   for (i=0; i<npts-1; i++) {
      u=0;
      do {
         if (i==0) 
            xs[j] = eval_ctrom_spline(u,xarr[i],xarr[i],xarr[i+1],xarr[i+2]);
         else if (i==npts-2)
            xs[j] = eval_ctrom_spline(u,xarr[i-1],xarr[i],xarr[i+1],xarr[i+1]);
         else 
            xs[j] = eval_ctrom_spline(u,xarr[i-1],xarr[i],xarr[i+1],xarr[i+2]);
         u += du;
         j++;
      } while (u < 1.0 + SMALL);
   }
   *nspts = j;
}


/* 
 * Single (Closed) Catmull-Rom spline 
 *    The splines pass through the real datapoints.
 *    Control points are doubled only at the end-points
 */
void CNmake_closed_ctrom_spline(xarr,npts,xs,nspts,ndiv)
double xarr[], xs[];
int    npts, *nspts, ndiv;
{
   double u, du;
   int    i,j;

   /* If too few points, just copy the original array to the new array */
   if (npts <= 2) {
      *nspts = npts;
      for (i=0; i<npts; i++) 
         xs[i] = xarr[i];
      return;
   }  

   /* Parameter subdivision */
   if (ndiv <= 0) ndiv = 20;
   du = 1.0/(double)ndiv;

   j=0;
   for (i=0; i<npts; i++) {
      u=0;
      do {
         if (i==0) 
            xs[j]=eval_ctrom_spline(u,xarr[npts-1],xarr[i],xarr[i+1],xarr[i+2]);
         else if (i==npts-2)
            xs[j] = eval_ctrom_spline(u,xarr[i-1],xarr[i],xarr[i+1],xarr[0]);
         else if (i==npts-1)
            xs[j] = eval_ctrom_spline(u,xarr[i-1],xarr[i],xarr[0],xarr[1]);
         else 
            xs[j] = eval_ctrom_spline(u,xarr[i-1],xarr[i],xarr[i+1],xarr[i+2]);
         u += du;
         j++;
      } while (u < 1.0 + SMALL);
   }
   *nspts = j;
}


/*
 * Evaluate the Catmull-rom spline 
 */
static double eval_ctrom_spline(u,xa,xb,xc,xd)
double u,xa,xb,xc,xd;
{
   double c, B=0.5;
   
   /* Check the value of u */
   if (u < -SMALL || u > 1.0 + SMALL) {
   (void) fprintf(stderr,"Error - attempt to evaluate u=%f outside [0,1] range\n",u);
   return(0.0);
   }
   c = u*u*u*( -B*xa + (2-B)*xb +   (B-2)*xc + B*xd )
       + u*u*(2*B*xa + (B-3)*xb + (3-2*B)*xc - B*xd )
       +   u*( -B*xa +                  B*xc        )
       +     (               xb                     );
   return(c);
}


/* 
 * Single (Open) Quadratic Bezier spline 
 *    Spline curve approximates the real curve and terminates at end-points
 *    Control points are doubled only at the end-points
 */
void CNmake_quadr_bezier_spline(xarr,npts,xs,nspts,ndiv)
double xarr[], xs[];
int    npts, *nspts, ndiv;
{
   double xm1, xm2, xm3, u, du;
   int    i, j;

   /* If too few points, just copy the original array to the new array */
   if (npts <= 2) {
      *nspts = npts;
      for (i=0; i<npts; i++) 
         xs[i] = xarr[i];
      return;
   }  

   /* Parameter subdivision */
   if (ndiv <= 0) ndiv = 20;
   du = 1.0/(double)ndiv;

   j=0;
   for (i=1; i<npts-1; i++) {
      u=0;
      do {
         if (i==1) 
            xm1 = xarr[i-1];
         else 
            xm1 = 0.5*(xarr[i-1]+ xarr[i]  );

         if (i==npts-2)
            xm3 = xarr[i+1];
         else
            xm3 = 0.5*(xarr[i]  + xarr[i+1]);
         xm2 = xarr[i];

         xs[j] = eval_quadr_bezier_spline(u,xm1,xm2,xm3);
         u += du;
         j++;
      } while (u < 1.0 + SMALL);
   }
   *nspts = j;
}


/* 
 * Single Closed Quadratic Bezier spline 
 *    Spline curve approximates the real curve
 */
void CNmake_closed_quadr_bezier_spline(xarr,npts,xs,nspts,ndiv)
double xarr[], xs[];
int    npts, *nspts, ndiv;
{
   double xm1, xm2, xm3, u, du;
   int    i,j;

   /* If too few points, just copy the original array to the new array */
   if (npts <= 2) {
      *nspts = npts;
      for (i=0; i<npts; i++) 
         xs[i] = xarr[i];
      return;
   }

   /* Parameter subdivision */
   if (ndiv <= 0) ndiv = 20;
   du = 1.0/(double)ndiv;

   j=0;
   for (i=0; i<npts; i++) {
      u=0;
      do {
         if (i==0) {
            xm1 = 0.5*(xarr[npts-1] + xarr[i]);
            xm2 = xarr[i];
            xm3 = 0.5*(xarr[i]      + xarr[i+1]);
         } else if (i==npts-1){
            xm1 = 0.5*(xarr[i-1] + xarr[i]);
            xm2 = xarr[i];
            xm3 = 0.5*(xarr[i]   + xarr[0]);
         } else {
            xm1 = 0.5*(xarr[i-1]+ xarr[i]  );
            xm2 = xarr[i];
            xm3 = 0.5*(xarr[i]  + xarr[i+1]);
         }
         xs[j] = eval_quadr_bezier_spline(u,xm1,xm2,xm3);
         u += du;
         j++;
      } while (u < 1.0 + SMALL);
   }
   *nspts = j;
}


/* 
 * Single (Open) Cubic Bezier spline
 *    Spline curve approximates the real curve and terminates at end-points
 *    Control points are doubled only at the end-points
 */
void CNmake_cubic_bezier_spline(xarr,npts,xs,nspts,ndiv)
double xarr[], xs[];
int    npts, *nspts, ndiv;
{
   double xm1, xm2, xm3, xm4, u, du;
   int    i,j;

   /* If too few points, just copy the original array to the new array */
   if (npts <= 2) {
      *nspts = npts;
      for (i=0; i<npts; i++) 
         xs[i] = xarr[i];
      return;
   }

   /* Parameter subdivision */
   if (ndiv <= 0) ndiv = 20;
   du = 1.0/(double)ndiv;

   j=0;
   for (i=1; i<npts-1; i++) {
      u=0;
      do {
         if (i==1) 
            xm1 = xarr[i-1];
         else 
            xm1 = xarr[i] + 0.50*(xarr[i-1] - xarr[i]);
         if (i==npts-2)
            xm4 = xarr[i+1];
         else
            xm4 = xarr[i] + 0.50*(xarr[i+1] - xarr[i]);

         xm2 = xarr[i] + 0.05*(xarr[i-1] - xarr[i]);
         xm3 = xarr[i] + 0.05*(xarr[i+1] - xarr[i]);

         xs[j] = eval_cubic_bezier_spline(u,xm1,xm2,xm3,xm4);
         u += du;
         j++;
      } while (u < 1.0 + SMALL);
   }
   *nspts = j;
}


/* 
 * Single Closed Cubic Bezier spline 
 *    Spline curve approximates the real curve
 */
void CNmake_closed_cubic_bezier_spline(xarr,npts,xs,nspts,ndiv)
double xarr[], xs[];
int    npts, *nspts, ndiv;
{
   double xm1, xm2, xm3, xm4, u, du;
   int    i,j;

   /* If too few points, just copy the original array to the new array */
   if (npts <= 2) {
      *nspts = npts;
      for (i=0; i<npts; i++) 
         xs[i] = xarr[i];
      return;
   }

   /* Parameter subdivision */
   if (ndiv <= 0) ndiv = 20;
   du = 1.0/(double)ndiv;

   j=0;
   for (i=0; i<npts; i++) {
      u=0;
      do {
         if (i==0) {
            xm1 = xarr[i] + 0.50*(xarr[npts-1] - xarr[i]);
            xm2 = xarr[i] + 0.05*(xarr[npts-1] - xarr[i]);
            xm3 = xarr[i] + 0.05*(xarr[i+1] - xarr[i]);
            xm4 = xarr[i] + 0.50*(xarr[i+1] - xarr[i]);
         } else if (i==npts-1){
            xm1 = xarr[i] + 0.50*(xarr[i-1] - xarr[i]);
            xm2 = xarr[i] + 0.05*(xarr[i-1] - xarr[i]);
            xm3 = xarr[i] + 0.05*(xarr[0  ] - xarr[i]);
            xm4 = xarr[i] + 0.50*(xarr[0  ] - xarr[i]);
         } else {
            xm1 = xarr[i] + 0.50*(xarr[i-1] - xarr[i]);
            xm2 = xarr[i] + 0.05*(xarr[i-1] - xarr[i]);
            xm3 = xarr[i] + 0.05*(xarr[i+1] - xarr[i]);
            xm4 = xarr[i] + 0.50*(xarr[i+1] - xarr[i]);
         }
         xs[j] = eval_cubic_bezier_spline(u,xm1,xm2,xm3,xm4);
         u += du;
         j++;
      } while (u < 1.0 + SMALL);
   }
   *nspts = j;
}


/*
 * Evaluate the quadratic Bezier spline
 */
static double eval_quadr_bezier_spline(u,xa,xb,xc)
double u,xa,xb,xc;
{
   double c;
   
   /* Check the value of u */
   if (u < -SMALL || u > 1.0 + SMALL) {
   (void) fprintf(stderr,"Error - attempt to evaluate u=%f outside [0,1] range\n",u);
   return(0.0);
   }
   c =   u*u*(   xa - 2*xb + xc)
       +   u*(-2*xa + 2*xb     )
       +     (   xa            );
   return(c);
}


/*
 * Evaluate the cubic Bezier spline
 */
static double eval_cubic_bezier_spline(u,xa,xb,xc,xd)
double u,xa,xb,xc,xd;
{
   double c;
   
   /* Check the value of u */
   if (u < -SMALL || u > 1.0 + SMALL) {
   (void) fprintf(stderr,"Error - attempt to evaluate u=%f outside [0,1] range\n",u);
   return(0.0);
   }
   c = u*u*u*(  -xa + 3*xb - 3*xc + xd )
       + u*u*( 3*xa - 6*xb + 3*xc      )
       +   u*(-3*xa + 3*xb             )
       +     (   xa                    );
   return(c);
}

