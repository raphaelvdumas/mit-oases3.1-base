/*
 * intersect.c 
 *    Find intersections of objects based on segment-plane intersections
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "CNdata.h"
#include "CNproperty.h"
#include "CNdatatypes.h"
#include "CNintersect.h"

/*

static void test_rect_intsct();
static void test_tria_intsct();
static void test_line_intsct();

main()
{
   test_rect_intsct();
}

static void test_rect_intsct()
{
   CNpoint  pta, ptb, ptc, ptd;
   CNpoint  points[10];
   int      npts;
   double   a, b, c, d;
   int      intsct, i;

   pta.x = 0.0; pta.y = 0.0; pta.z = 1.0;
   ptb.x = 1.0; ptb.y = 0.0; ptb.z = 0.0;
   ptc.x = 1.0; ptc.y = 1.0; ptc.z = 1.0;
   ptd.x = 0.0; ptd.y = 1.0; ptd.z = 0.0;

   (void) fprintf(stdout,"\nEnter the equation of the plane: (ax+by+cz=d)\n");
   (void) fprintf(stdout,"   a ="); scanf("%lf",&a);
   (void) fprintf(stdout,"   b ="); scanf("%lf",&b);
   (void) fprintf(stdout,"   c ="); scanf("%lf",&c);
   (void) fprintf(stdout,"   d ="); scanf("%lf",&d); d = -1.0*d;
   (void) fprintf(stdout,"The plane equation is : %g*x + %g*y + %g*z = %g\n",a,b,c,-d);

   intsct = CNpoly4_intsct_plane(a, b, c, d, &pta, &ptb, &ptc, &ptd, 
                                 points, &npts, 1);

   (void) fprintf(stdout,"\nNo of intersections = %d  No of segment pairs = %d\n",npts,npts/2);
   for (i=0; i<npts; i++)
      (void) fprintf(stdout,"pt %d = (%10.5f  %10.5f  %10.5f)\n",
          i, points[i].x, points[i].y, points[i].z);
}

static void test_tria_intsct()
{
   CNpoint  pta, ptb, ptc;
   CNpoint  pt1, pt2;
   double   a, b, c, d;
   int      intsct;

   pta.x = 0.0; pta.y = 0.0; pta.z = 1.0;
   ptb.x = 1.0; ptb.y = 0.0; ptb.z = 1.0;
   ptc.x = 1.0; ptc.y = 1.0; ptc.z = 1.0;

   (void) fprintf(stdout,"\nEnter the equation of the plane: (ax+by+cz=d)\n");
   (void) fprintf(stdout,"   a ="); scanf("%lf",&a);
   (void) fprintf(stdout,"   b ="); scanf("%lf",&b);
   (void) fprintf(stdout,"   c ="); scanf("%lf",&c);
   (void) fprintf(stdout,"   d ="); scanf("%lf",&d); d = -1.0*d;
   (void) fprintf(stdout,"The plane equation is : %g*x + %g*y + %g*z = %g\n",a,b,c,-d);

   intsct = CNpoly3_intsct_plane(a, b, c, d, &pta, &ptb, &ptc, &pt1, &pt2, 1);

   if (intsct) {
      (void) fprintf(stdout,"Found intersection\n");
      (void) fprintf(stdout,"pt1 = (%10.5f  %10.5f  %10.5f)\n", pt1.x, pt1.y, pt1.z);
      (void) fprintf(stdout,"pt2 = (%10.5f  %10.5f  %10.5f)\n", pt2.x, pt2.y, pt2.z);
   }
}


static void test_line_intsct()
{
   CNpoint  pta, ptb, ptc;
   double   a, b, c, d;
   int      intsct;

   pta.x = 1.0; pta.y = 1.0; pta.z = 1.0;
   ptb.x = 0.0; ptb.y = 0.0; ptb.z = 0.0;
   ptc.x = 0.0; ptc.y = 0.0; ptc.z = 0.0;

   (void) fprintf(stdout,"\nEnter the equation of the plane: (ax+by+cz=d)\n");
   (void) fprintf(stdout,"   a ="); scanf("%lf",&a);
   (void) fprintf(stdout,"   b ="); scanf("%lf",&b);
   (void) fprintf(stdout,"   c ="); scanf("%lf",&c);
   (void) fprintf(stdout,"   d ="); scanf("%lf",&d);
   (void) fprintf(stdout,"The plane equation is : %g*x + %g*y + %g*z = %g\n",a,b,c,d);

   intsct = CNline_intsct_plane(a, b, c, -d, &pta, &ptb, &ptc);

   (void) fprintf(stdout,"pta = (%10.5f  %10.5f  %10.5f)\n",pta.x, pta.y, pta.z);
   (void) fprintf(stdout,"ptb = (%10.5f  %10.5f  %10.5f)\n",ptb.x, ptb.y, ptb.z);
   (void) fprintf(stdout,"ptc = (%10.5f  %10.5f  %10.5f)\n",ptc.x, ptc.y, ptc.z);
   (void) fprintf(stdout,"%s\n",(intsct) ? "Found Intersection" : "No Intersection");
}
*/


/*
 * Find the intersection between a rectangle and a plane 
 * 4 vertices are specified:                           v1----v2
 *    Assume that 4 vertices are sequential, i.e.       |    |
 *                                                     v4----v3
 */
int CNpoly4_intsct_plane(a,b,c,d,pta,ptb,ptc,ptd,intpt,npts,verbose)
double   a, b, c, d;               /* The equation of the plane */
CNpoint  *pta, *ptb, *ptc, *ptd;   /* Rectangle vertices        */
CNpoint  intpt[];                  /* Intersection coords       */
int      *npts;                    /* Number of intersections   */
int      verbose;                  /* Verbosity flag for debug  */
{
   CNpoint  pt1, pt2, pt3, pt4, point[4];
   double   d1, d2, d3, d4;
   int      in1, in2, in3, in4, nint;
   int      intsct = CN_FALSE;
   int      lg01, lg12, lg20;
   int      i = 0;

   /* Initialize */
   *npts = 0;

   /* Double-check plane equation */
   if ((fabs(a)<CN_SMALLER) && (fabs(b)<CN_SMALLER) && (fabs(c)<CN_SMALLER)) {
      (void) fprintf(stderr,"The plane is undefined! ");
      (void) fprintf(stderr,"Plane equation: %gx + %gy + %gz = %g\n",a,b,c,-d);
      return(intsct);
   }

   /* Normalize the plane equation */
   CNnormalize_plane(&a,&b,&c,&d);

   /* Check the rectangle first using plane equation : ax + by + cz + d = 0 */
   d1 = -1.0 * ( a * pta->x + b * pta->y + c * pta->z);
   d2 = -1.0 * ( a * ptb->x + b * ptb->y + c * ptb->z);
   d3 = -1.0 * ( a * ptc->x + b * ptc->y + c * ptc->z);
   d4 = -1.0 * ( a * ptd->x + b * ptd->y + c * ptd->z);

   /*
    * if d1, d2, d3, d4 all greater or all less than d0, then no intsct
    * If the rectangle is underneath the plane and touches the plane then count
    *    as no intersection.
    * If the rectangle is above the plane and touches the plane then count
    *    as an intersection.
    * This avoids double-intersections if the plane is coincident with an
    * edge of the rectangle.
    */
   if ( (fabs(d-d1)<CN_SMALLER) && (fabs(d-d2)<CN_SMALLER) &&
        (fabs(d-d3)<CN_SMALLER) && (fabs(d-d4)<CN_SMALLER) ){
      /* Rectangle is on the plane */
      if (verbose) (void) fprintf(stdout,"Plane is parallel to rectangle\n");
      intsct = CN_FALSE;
      return(intsct); 

   } else if ( ((d1 >= d ) && (d2 >= d) && (d3 >= d) && (d4 >= d)) ||
               ((d1 <  d ) && (d2 <  d) && (d3 <  d) && (d4 <  d)) ) {
      /* Rectangle has points all on one side of plane */
      intsct = CN_FALSE;
      return(intsct);
   }
      
   /* Find intersections of line-segments */
   in1 = CNline_intsct_plane(a, b, c, d, pta, ptb, &pt1);
   in2 = CNline_intsct_plane(a, b, c, d, ptc, ptb, &pt2);
   in3 = CNline_intsct_plane(a, b, c, d, ptc, ptd, &pt3);
   in4 = CNline_intsct_plane(a, b, c, d, pta, ptd, &pt4);
   if (in1) point[i++]=pt1;
   if (in2) point[i++]=pt2;
   if (in3) point[i++]=pt3;
   if (in4) point[i++]=pt4;
   nint = in1 + in2 + in3 + in4;

   /* Print intersection info */
   if (verbose) {
      (void) fprintf(stdout,"%d intersections\n",nint);
      for (i=0; i<nint; i++)
         (void) fprintf(stdout,"Intersection #%d : (%10.5f %10.5f %10.5f)\n",
                 i,point[i].x,point[i].y,point[i].z);
   }

   /* There can be 0 to 4 intersections */
   if (nint == 1) {
      /* 
       * One intersection - must be floating point error where
       * the plane is at the tip of a vertice   
       */
      intsct = CN_FALSE;
   } else if (nint == 2) {
      /* 
       * Two intersections - just return 2 
       */
      intpt[0] = point[0];   
      intpt[1] = point[1];   
      *npts    = 2;
      intsct   = CN_TRUE;
   } else if (nint == 3) {
      /* 
       * Three intersections - check the distance between intersections
       * to screen out floating-point-error.
       * Possible that 3 points are on the plane...
       */
      lg01 = CNlongline(&(point[0]),&(point[1]),1.0e-5);
      lg12 = CNlongline(&(point[1]),&(point[2]),1.0e-5);
      lg20 = CNlongline(&(point[2]),&(point[0]),1.0e-5);
      /* If there are 3 long-lines add all 3 to the list */
      if (lg01 && lg12 && lg20) {
         if (lg01) {
            intpt[(*npts)++] = point[0];
            intpt[(*npts)++] = point[1];
         }
         if (lg12) {
            intpt[(*npts)++] = point[1];
            intpt[(*npts)++] = point[2];
         }
         if (lg20) {
            intpt[(*npts)++] = point[2];
            intpt[(*npts)++] = point[0];
         }
      } else if (!lg01) {
         /* pt0 = pt1, ln12 = ln20 */
         if (lg12) {
            intpt[(*npts)++] = point[1];
            intpt[(*npts)++] = point[2];
         }
      } else if (!lg12) { 
         /* pt1 = pt2, ln01 = ln20 */
         if (lg01) {
            intpt[(*npts)++] = point[0];
            intpt[(*npts)++] = point[1];
         }
      } else if (!lg20) { 
         /* pt0 = pt2, ln01 = ln21 */
         if (lg01) {
            intpt[(*npts)++] = point[0];
            intpt[(*npts)++] = point[1];
         }
      }
      if ((*npts)>0) intsct = CN_TRUE;
   } else if (nint == 4) {
      /*
       * Four intersections - match up pairs based on whether the
       * vertice in between 2 intersections is high or low.
       *
       * There is a possibility that some points are the same due to
       * floating-point error - handle these cases too       
       */
      
      /*
      if (d1 > d) {
       */
      if (d1 < d) {
         /* Pairs are (p1,p2) (p3,p4) */
         if (CNlongline(&pt1,&pt2,1.0e-5) && CNlongline(&pt3,&pt4,1.0e-5)) {
            intpt[(*npts)++] = point[0];
            intpt[(*npts)++] = point[1];
            intpt[(*npts)++] = point[2];
            intpt[(*npts)++] = point[3];
         } else {
            intpt[(*npts)++] = point[0];
            intpt[(*npts)++] = point[2];
         }
      } else { 
         /* Pairs are (p1,p4) (p2,p3) */
         if (CNlongline(&pt1,&pt4,1.0e-5) && CNlongline(&pt2,&pt3,1.0e-5)) {
            intpt[(*npts)++] = point[0];
            intpt[(*npts)++] = point[3];
            intpt[(*npts)++] = point[1];
            intpt[(*npts)++] = point[2];
         } else {
            intpt[(*npts)++] = point[0];
            intpt[(*npts)++] = point[1];
         }
      }
      intsct = CN_TRUE; 
   } else {
      /*
       * Zero or non-valid no of intersections
       */
      intsct = CN_FALSE;
   } 

   /* Return status */
   return(intsct);
}
   

/*
 * Find the intersection between a triangle and a plane 
 */
int CNpoly3_intsct_plane(a,b,c,d,pta,ptb,ptc,ptmin,ptmax,verbose)
double   a, b, c, d;               /* The equation of the plane */
CNpoint  *pta, *ptb, *ptc;         /* Triangle vertices         */
CNpoint  *ptmin, *ptmax;           /* Intersections             */
int      verbose;                  /* Verbosity flag for debug  */
{
   CNpoint  pt1, pt2, pt3, point[3];
   double   d1, d2, d3;
   int      in1, in2, in3, nint;
   int      intsct = CN_FALSE;
   int      i = 0;

   /* Double-check plane equation */
   if ((fabs(a)<CN_SMALLER) && (fabs(b)<CN_SMALLER) && (fabs(c)<CN_SMALLER)) {
      (void) fprintf(stderr,"The plane is undefined! ");
      (void) fprintf(stderr,"Plane equation: %gx + %gy + %gz = %g\n",a,b,c,-d);
      return(intsct);
   }

   /* Normalize the plane equation */
   CNnormalize_plane(&a,&b,&c,&d);

   /* Check the triangle first using plane equation : ax + by + cz + d = 0 */
   d1 = -1.0 * ( a * pta->x + b * pta->y + c * pta->z);
   d2 = -1.0 * ( a * ptb->x + b * ptb->y + c * ptb->z);
   d3 = -1.0 * ( a * ptc->x + b * ptc->y + c * ptc->z);

   /*
    * if d1, d2, d3 all greater or all less than d0, then no intsct
    * If the triangle is underneath the plane and touches the plane then count
    *    as no intersection.
    * If the triangle is above the plane and touches the plane then count
    *    as an intersection.
    * This avoids double-intersections if the plane is coincident with an
    * edge of the triangle.
    */
   if ( (fabs(d-d1)<CN_SMALLER) && 
        (fabs(d-d2)<CN_SMALLER) &&
        (fabs(d-d3)<CN_SMALLER) ){
      /* Tria is on the plane */
      if (verbose) (void) fprintf(stdout,"Plane is parallel to triangle\n");
      intsct = CN_FALSE;
      return(intsct); 

   } else if ( ((d1 >= d ) && (d2 >= d) && (d3 >= d)) ||
               ((d1 <  d ) && (d2 <  d) && (d3 <  d)) ) {
      /* Tria has points all on one side of plane */
      intsct = CN_FALSE;
      return(intsct);

   }
      
   /* Find intersections of line-segments */
   in1 = CNline_intsct_plane(a, b, c, d, pta, ptb, &pt1);
   in2 = CNline_intsct_plane(a, b, c, d, ptc, ptb, &pt2);
   in3 = CNline_intsct_plane(a, b, c, d, ptc, pta, &pt3);
   if (in1) point[i++]=pt1;
   if (in2) point[i++]=pt2;
   if (in3) point[i++]=pt3;
   nint = in1 + in2 + in3;
   if (verbose) (void) fprintf(stdout,"%d intersections\n",nint);

   /* There can be 0 to 3 intersections */
   if (nint == 1) {
      /* 
       * One intersection - must be floating point error where
       * the plane is at the tip of the triangle
       */
      intsct = CN_FALSE;
   } else if (nint == 2) {
      /* 
       * Two intersections - just return 2 
       */
      *ptmin = point[0];   
      *ptmax = point[1];   
      intsct = CN_TRUE;
   } else if (nint == 3) {
      /* 
       * Three intersections - two of these must be pretty close
       */
      if (verbose) {
         for (i=0; i<nint; i++)
            (void) fprintf(stdout,"Intersection #%d : (%10.5f %10.5f %10.5f)\n",
                    i,point[i].x,point[i].y,point[i].z);
      }
      /* Get the line-segment */
      if ( (fabs(point[0].x - point[1].x) < CN_SMALLER) &&
           (fabs(point[0].y - point[1].y) < CN_SMALLER) &&
           (fabs(point[0].z - point[1].z) < CN_SMALLER) ) {
         *ptmin = point[0];
         *ptmax = point[2];
      } else {
         *ptmin = point[0];
         *ptmax = point[1];
      }
      intsct = CN_TRUE;
   } else {
      /*
       * Zero or non-valid no of intersections
       */
      intsct = CN_FALSE;
   } 

   /* Return status */
   return(intsct);
}
   

/*
 * find the intersection values between a line-segment and a plane
 */
int CNline_intsct_plane(a, b, c, d, pta, ptb, ptc)
double   a, b, c, d;       /* The equation of the plane          */
CNpoint  *pta, *ptb;       /* The two points on the line-segment */
CNpoint  *ptc;             /* The intersection                   */
{
   int    intsct = CN_FALSE;
   double d1, d2, t;
   
   /* Double-check plane equation */
   if ((fabs(a)<CN_SMALLER) && (fabs(b)<CN_SMALLER) && (fabs(c)<CN_SMALLER)) {
      (void) fprintf(stderr,"The plane is undefined! ");
      (void) fprintf(stderr,"Plane equation: %gx + %gy + %gz = %g\n",a,b,c,-d);
      return(intsct);
   }

   /* Get plane equations for 2 points */
   d1 = -1.0 * ( a * pta->x + b * pta->y + c * pta->z);
   d2 = -1.0 * ( a * ptb->x + b * ptb->y + c * ptb->z);

   /*
    * We need to avoid multiple points where a joint of 2 lines
    * falls exactly on a plane.  In such a case, screen the lines first
    * so that one line is "above" and one line intersects the plane.
    * But can't do this here because then the order of the points on the
    * line makes a difference.
    */

   if (fabs(d2-d1) < CN_SMALLER) {
      /* The line lies on or is parallel to the plane */
      intsct = CN_FALSE;
      return(intsct);
   } else if ( ((d1 > d) && (d2 > d)) || ((d1 < d) && (d2 < d)) ) {
      /* Line has points all on one side of the planes */
      intsct = CN_FALSE;
      return(intsct);
   }

   /*
    * Let the parameter t overlap a bit.
    * This means that if one point of the line is exactly on the plane, 
    * it will be counted as an intersection regardless of its position 
    * on the line.
    */

   /* Find the intersection */
   t  = (d - d1) / (d2 - d1);
   if ((t > -CN_SMALLER) && (t <= 1.0+CN_SMALLER)) {
      intsct = CN_TRUE;
      if (t < 0.5) {
         ptc->x = pta->x + t*(ptb->x - pta->x);
         ptc->y = pta->y + t*(ptb->y - pta->y);
         ptc->z = pta->z + t*(ptb->z - pta->z);
      } else {
         /*
          * This is to prevent problems when t is very small,
          * e.g. t=1e-20, so that 1-t=1.  In such cases, the order
          * of points makes a difference.  This if-else statement
          * circumvents the problem by switching the order of points.
          */
         t  = (d - d2) / (d1 - d2);
         ptc->x = ptb->x + t*(pta->x - ptb->x);
         ptc->y = ptb->y + t*(pta->y - ptb->y);
         ptc->z = ptb->z + t*(pta->z - ptb->z);
      }
   }

   /* Return status */
   return(intsct);
}

/*
 * find out if a line-segment is long or short
 *
 * This comparison needs to be done on a relative basis
 */
int CNlongline(pta,ptb,delta)
CNpoint *pta,*ptb;
double  delta;
{
   int    longln = CN_TRUE;
   double dx,dy,dz,dlsq;

   dx = pta->x - ptb->x;  
   dy = pta->y - ptb->y; 
   dz = pta->z - ptb->z;
   dlsq = dx*dx + dy*dy + dz*dz;

   /* 
    * Delta is the boundary which is larger than dx,dy,dz
    * but delta could be zero
    */
   delta = 1e-15*delta*delta;
   if (delta < CN_SMALLER) delta = CN_SMALLER;

   longln = (dlsq > delta) ? CN_TRUE : CN_FALSE;

   return(longln);
}

/*
 * find out if a line-segment is long or short
 *
 * This comparison needs to be done on a relative basis
 */
int CNlongline_old(pta,ptb)
CNpoint *pta,*ptb;
{
   int    longln = CN_TRUE;
   double dx,dy,dz;
   double xm,ym,zm;

   dx = fabs(pta->x - ptb->x);  xm = fabs(pta->x + ptb->x);
   dy = fabs(pta->y - ptb->y);  ym = fabs(pta->y + ptb->y);
   dz = fabs(pta->z - ptb->z);  zm = fabs(pta->z + ptb->z);

   if (xm < CN_SMALLER) 
      longln = (dx    > CN_SMALL) ? CN_TRUE : CN_FALSE;
   else 
      longln = (dx/xm > CN_SMALL) ? CN_TRUE : CN_FALSE;
   if (longln) return(longln);   

   if (ym < CN_SMALLER) 
      longln = (dy    > CN_SMALL) ? CN_TRUE : CN_FALSE;
   else 
      longln = (dy/ym > CN_SMALL) ? CN_TRUE : CN_FALSE;
   if (longln) return(longln);   

   if (zm < CN_SMALLER) 
      longln = (dz    > CN_SMALL) ? CN_TRUE : CN_FALSE;
   else 
      longln = (dz/zm > CN_SMALL) ? CN_TRUE : CN_FALSE;
   if (longln) return(longln);   

   return(longln);
}

