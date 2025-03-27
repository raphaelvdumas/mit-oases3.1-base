/*
 * intersect3D.c - routines to find the intersection of
 *                 a triangle with a plane
 */
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "CNdata.h"
#include "CNproperty.h"
#include "CNdatatypes.h"
#include "CNintersect.h"

#define FALSE    0
#define TRUE     1
#define PARALLEL 2

#define SMALL        1.0e-10
#define LARGE        1.0e10

/* mesh data structure */
typedef struct data_strct {
   double x;
   double y;
   double z;
} data;


static int  tria_intersects_plane();
static int  x_plane_intsct();
static void rotate_y_point();
static void inv_rotate_y_point();
static int  y_plane_intsct();
static void rotate_x_point();
static void inv_rotate_x_point();
static int  z_plane_intsct();
static int  z_intsct_tria();
static int  lines_intersect();

static double min(), max();

static void find_normal_vector();
static data find_line_vector();
static data cross_product();

static int  tria_intsct_plane();
static int  line_intsct_plane();
static int  xtria_intsct_plane();
static int  ytria_intsct_plane();
static int  ztria_intsct_plane();

/*

static void call_pdraw();

main()
{
   int    CNfind_tria_intsct_plane();
   int    pdraw=TRUE;

   double x1 = 0.0;
   double y1 = 0.0;
   double z1 = 0.0;
   double x2 = 0.0;
   double y2 = 1.0;
   double z2 = 0.0;
   double x3 = 0.0;
   double y3 = 0.0;
   double z3 = 1.0;
   double a = 0.0;
   double b = 1.0;
   double c = 0.0;
   double d = -0.5;
   double x4, y4, z4, x5, y5, z5;
   int    intsct;

   (void) fprintf(stdout,"Enter the equation of the plane: (ax+by+cz=d)\n");
   (void) fprintf(stdout,"   a ="); scanf("%lf",&a);
   (void) fprintf(stdout,"   b ="); scanf("%lf",&b);
   (void) fprintf(stdout,"   c ="); scanf("%lf",&c);
   (void) fprintf(stdout,"   d ="); scanf("%lf",&d);
   (void) fprintf(stdout,"The plane equation is : %g*x + %g*y + %g*z = %g\n",a,b,c,d);

   intsct = CNfind_tria_intsct_plane(a,b,c,-d,
                                     x1,y1,z1,
                                     x2,y2,z2,
                                     x3,y3,z3,
                                     &x4,&y4,&z4,
                                     &x5,&y5,&z5,1);
   if (intsct == 1) {
      (void) fprintf(stdout,"Intersection :\n");
      (void) fprintf(stdout,"   x1 = %10.5f  y1 = %10.5f  z1 = %10.5f\n",x4,y4,z4);
      (void) fprintf(stdout,"   x2 = %10.5f  y2 = %10.5f  z2 = %10.5f\n",x5,y5,z5);
      if (pdraw) call_pdraw(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,x5,y5,z5);
   } else if (intsct == 2) {
      (void) fprintf(stdout,"The triangle and the plane are parallel\n");
   } else {
      (void) fprintf(stdout,"No intersection!\n");
   }
}
*/

/* 
 * Print out the triangle and intersection to a file and plot it 
 */
/*
static void call_pdraw(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,x5,y5,z5)
double x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,x5,y5,z5;
{
   FILE   *fp, *fopen();
   double xmin, xmax, ymin, ymax, zmin, zmax;
   char   command[1000];
   char   *outfile = "/tmp/outfile";

   if ((fp = fopen(outfile,"w")) == NULL) {
      (void) fprintf(stderr,"Cat: couldnt open file\n",outfile);
      return;  
   }

   xmin = min(x1,x2,x3);
   xmax = max(x1,x2,x3);
   ymin = min(y1,y2,y3);
   ymax = max(y1,y2,y3);
   zmin = min(z1,z2,z3);
   zmax = max(z1,z2,z3);
   (void) fprintf(fp,"%f %f %f %f %f %f\n2.0\n",xmin,xmax,ymin,ymax,zmin,zmax);

   (void) fprintf(fp,"4.0\n");
   (void) fprintf(fp,"%f %f %f\n",x1,y1,z1);
   (void) fprintf(fp,"%f %f %f\n",x2,y2,z2);
   (void) fprintf(fp,"%f %f %f\n",x3,y3,z3);
   (void) fprintf(fp,"%f %f %f\n",x1,y1,z1);

   (void) fprintf(fp,"2.0\n");
   (void) fprintf(fp,"%f %f %f\n",x4,y4,z4);
   (void) fprintf(fp,"%f %f %f\n",x5,y5,z5);
   
   (void) fclose(fp);

   (void) sprintf(command,"pdraw -a 0 -ps %s",outfile);
   (void) system(command);
}
*/
   

/*
 * Normalize a plane equation so that same results are obtained if a,b,c
 * are negative or positive
 * This is important when comparing d-values of points/planes.
 */
void CNnormalize_plane(a,b,c,d)
double *a,*b,*c,*d;
{
   /* 
    * A triangle intersects a plane only if it is above the plane.
    * This avoids multiple intersections by a triangle below the plane
    *
    *              / Segment1
    *             /
    *      --------------plane
    *           /
    *          /Segment2
    * Need to get the same result if plane is specified with 
    *    (ax + by + d = 0)  vs (-ax - by - d = 0)
    */
   if (fabs(*a) > SMALL) {
      if (*a < 0) {
         *a = -1.0 * (*a); 
         *b = -1.0 * (*b); 
         *c = -1.0 * (*c); 
         *d = -1.0 * (*d);
      }
   } else if (fabs(*b) > SMALL) {
      if (*b < 0) {
         *a = -1.0 * (*a); 
         *b = -1.0 * (*b); 
         *c = -1.0 * (*c); 
         *d = -1.0 * (*d);
      }
   } else if (fabs(*c) > SMALL) {
      if (*c < 0) {
         *a = -1.0 * (*a); 
         *b = -1.0 * (*b); 
         *c = -1.0 * (*c); 
         *d = -1.0 * (*d);
      }
   }
}

/* 
 * find the intersection between a triangle and a plane 
 */
int CNfind_tria_intsct_plane(a,b,c,d,
                             x1,y1,z1,x2,y2,z2,x3,y3,z3,
                             x4,y4,z4,x5,y5,z5,verbose)
double a,b,c,d;     /* Equation of the plane */
double x1,y1,z1;    /* Triangle vertice      */
double x2,y2,z2;    /* Triangle vertice      */
double x3,y3,z3;    /* Triangle vertice      */
double *x4,*y4,*z4; /* Intersection point    */
double *x5,*y5,*z5; /* Intersection point    */
int    verbose;     /* Verbose flag          */
{
   data pa1, pa2, pa3, N1, ptmin, ptmax;
   int  intsct;

   /* assign triangle coordinates to data structure */
   pa1.x = x1;    pa1.y = y1;    pa1.z = z1;
   pa2.x = x2;    pa2.y = y2;    pa2.z = z2;
   pa3.x = x3;    pa3.y = y3;    pa3.z = z3;

   /* Normalize the plane equation */
   CNnormalize_plane(&a,&b,&c,&d);

   /* do a initial check to see if the triangle does intsct the plane */
   intsct = tria_intersects_plane(a,b,c,d,&pa1,&pa2,&pa3);

   if (intsct == FALSE)
      /* no intersection */
      return(intsct);
   else if (intsct == PARALLEL)
      /* parallel triangle - plane : tria is on plane */
      return(intsct);
   else   
      /* keep on going */
      ;

   /* find the normal vector to the triangle */
   find_normal_vector(&pa1,&pa2,&pa3,&N1);

   /* check values */
   if ( (fabs(a) < SMALL) && (fabs(b) < SMALL) && (fabs(c) < SMALL)) {
      (void) fprintf(stderr,"Error! Plane a,b,c=0!\n");
      intsct = FALSE;
      return(intsct);
   }
   if ( (fabs(N1.x) < SMALL) && (fabs(N1.y) < SMALL) && (fabs(N1.z) < SMALL)) {
      (void) fprintf(stderr,"Error! Triangle a,b,c=0!\n");
      intsct = FALSE;
      return(intsct);
   }

   if ( (fabs(N1.z) > SMALL) && (fabs(c) > SMALL) ) {
      /* find the intersection by eliminating z */
      if (verbose) (void) fprintf(stdout,"c1>0  c2>0\n");
      intsct = z_plane_intsct(a,b,c,d,&pa1,&pa2,&pa3,&ptmin,&ptmax);

   } else if ( (fabs(N1.x) > SMALL) && (fabs(a) > SMALL) ) {
      /* find the intersection by eliminating x */
      if (verbose) (void) fprintf(stdout,"a1>0  a2>0\n");
      intsct = x_plane_intsct(a,b,c,d,&pa1,&pa2,&pa3,&ptmin,&ptmax);

   } else if ( (fabs(N1.y) > SMALL) && (fabs(b) > SMALL) ) {
      /* find the intersection by eliminating y */
      if (verbose) (void) fprintf(stdout,"b1>0  b2>0\n");
      intsct = y_plane_intsct(a,b,c,d,&pa1,&pa2,&pa3,&ptmin,&ptmax);

   } else {
      if ( (fabs(N1.z) < SMALL) && (fabs(c) > SMALL) ) {
      /* The triangle is normal to z */

         if ( (fabs(a) < SMALL) && (fabs(b) < SMALL) ) {
            /* The triangle intersects plane cz + d = 0 */
            if (verbose) (void) fprintf(stdout,"c1=0 a2=0 b2=0\n");
            intsct = tria_intsct_plane(&pa1,&pa2,&pa3,'z',-d/c,&ptmin,&ptmax);

         } else if (fabs(N1.x) < SMALL) {
            /* The triangle is on the y-plane */
            if (verbose) (void) fprintf(stdout,"a1=0 c1=0\n");
            intsct = ytria_intsct_plane(a,b,c,d,&pa1,&pa2,&pa3,&ptmin,&ptmax);
       
         } else if (fabs(N1.y) < SMALL) {
            /* The triangle is on the x-plane */
            if (verbose) (void) fprintf(stdout,"b1=0 c1=0\n");
            intsct = xtria_intsct_plane(a,b,c,d,&pa1,&pa2,&pa3,&ptmin,&ptmax);
        
         }

      } else if ( (fabs(N1.z) > SMALL) && (fabs(c) < SMALL) ) {
      /* The clipping plane is normal to z */

         if ( (fabs(N1.x) < SMALL) && (fabs(N1.y) < SMALL) ) {
            /* The triangle is on the z-plane */
            if (verbose) (void) fprintf(stdout,"c2=0 a1=0 b1=0\n");
            intsct = ztria_intsct_plane(a,b,c,d,&pa1,&pa2,&pa3,&ptmin,&ptmax);

         } else if (fabs(a) < SMALL) {
            /* The plane is parallel to y : by + d = 0 */
            if (verbose) (void) fprintf(stdout,"a2=0 c2=0\n");
            intsct = tria_intsct_plane(&pa1,&pa2,&pa3,'y',-d/b,&ptmin,&ptmax);

         } else if (fabs(b) < SMALL) {
            /* The plane is parallel to x : ax + d = 0 */
            if (verbose) (void) fprintf(stdout,"b2=0 c2=0\n");
            intsct = tria_intsct_plane(&pa1,&pa2,&pa3,'x',-d/a,&ptmin,&ptmax);
   
         }

      } else if ( (fabs(N1.z) < SMALL) && (fabs(c) < SMALL) ) {
      /* The clipping plane and the triangle are both normal to z */

         if (fabs(a) < SMALL) {
            /* The plane is parallel to y : by + d = 0 */
            if (verbose) (void) fprintf(stdout,"c1=0 a2=0 c2=0\n");
            intsct = tria_intsct_plane(&pa1,&pa2,&pa3,'y',-d/b,&ptmin,&ptmax);

         } else if (fabs(b) < SMALL) {
            /* The plane is parallel to x : ax + d = 0 */
            if (verbose) (void) fprintf(stdout,"c1=0 b2=0 c2=0\n");
            intsct = tria_intsct_plane(&pa1,&pa2,&pa3,'x',-d/a,&ptmin,&ptmax);
   
         } 
      }
   }

   if (intsct == 1) {
      *x4 = ptmin.x;
      *y4 = ptmin.y;
      *z4 = ptmin.z;
      *x5 = ptmax.x;
      *y5 = ptmax.y;
      *z5 = ptmax.z;
   } else {
      *x4 = 0.0;
      *y4 = 0.0;
      *z4 = 0.0;
      *x5 = 0.0;
      *y5 = 0.0;
      *z5 = 0.0;
   }
   return (intsct);
}


/* 
 * find out if the plane does intersect the triangle 
 */
static int tria_intersects_plane(a,b,c,d,p1,p2,p3)
double a,b,c,d;
data   *p1, *p2, *p3;
{
   double d1, d2, d3;
   int    intsct;

   /* plane equation : ax + by + cz + d = 0 */
   d1 = -1.0*( a * p1->x + b * p1->y + c * p1->z); 
   d2 = -1.0*( a * p2->x + b * p2->y + c * p2->z); 
   d3 = -1.0*( a * p3->x + b * p3->y + c * p3->z); 

   /*
    * if d1, d2, d3 all greater or all less than d0, then no intsct 
    * If the triangle is underneath the plane and touches the plane then count
    *    as no intersection.
    * If the triangle is above the plane and touches the plane then count
    *    as an intersection.
    * This avoids double-intersections if the plane is coincident with an
    * edge of the triangle.
    */
   if ( ((d1 >= d ) && (d2 >= d) && (d3 >= d)) ||
        ((d1 <  d ) && (d2 <  d) && (d3 <  d)) )
      /* Tria has points all on one side of plane */
      intsct = FALSE;
   else 
      intsct = TRUE;

   /* if triangles are on the same plane, d1 = d, d2 = d, d3 = d */
   if ( (fabs(d1 - d) < SMALL) &&
        (fabs(d2 - d) < SMALL) &&
        (fabs(d3 - d) < SMALL) ) 
      intsct = PARALLEL;

   /* return intsct */
   return (intsct);
}

/*******************************/
/********   ROTATIONS   ********/
/*******************************/

/* 
 * find the intersections by eliminating x 
 */
static int x_plane_intsct(a,b,c,d,pa1,pa2,pa3,ptmin,ptmax)
double  a, b, c, d;
data    *pa1, *pa2, *pa3;
data    *ptmin,*ptmax;
{
   int intsct;

   rotate_y_point(pa1);
   rotate_y_point(pa2);
   rotate_y_point(pa3);
   intsct = z_plane_intsct(-c,b,a,d,pa1,pa2,pa3,ptmin,ptmax);
   inv_rotate_y_point(pa1);
   inv_rotate_y_point(pa2);
   inv_rotate_y_point(pa3);
   inv_rotate_y_point(ptmin);
   inv_rotate_y_point(ptmax);
   return(intsct);
}

/*
 * switch x and z values (or rotate 90 deg about the y-axis) 
 */
static void rotate_y_point(pt)
data *pt;
{
   double x,z;
   x = pt->x;
   z = pt->z;
   pt->x = -z;
   pt->z =  x;
}

/* 
 * switch x and z values (or rotate -90 deg about the y-axis) 
 */
static void inv_rotate_y_point(pt)
data *pt;
{
   double x,z;
   x = pt->x;
   z = pt->z;
   pt->x =  z;
   pt->z = -x;
}

/* 
 * find the intersections by eliminating y 
 */
static int y_plane_intsct(a,b,c,d,pa1,pa2,pa3,ptmin,ptmax)
double  a, b, c, d;
data    *pa1, *pa2, *pa3;
data    *ptmin,*ptmax;
{
   int intsct;

   rotate_x_point(pa1);
   rotate_x_point(pa2);
   rotate_x_point(pa3);
   intsct = z_plane_intsct(a,-c,b,d,pa1,pa2,pa3,ptmin,ptmax);
   inv_rotate_x_point(pa1);
   inv_rotate_x_point(pa2);
   inv_rotate_x_point(pa3);
   inv_rotate_x_point(ptmin);
   inv_rotate_x_point(ptmax);
   return(intsct);
}

/* 
 * switch y and z values (or rotate 90 deg about the x-axis) 
 */
static void rotate_x_point(pt)
data *pt;
{
   double y,z;
   y = pt->y;
   z = pt->z;
   pt->y = -z;
   pt->z =  y;
}

/* 
 * switch y and z values (or rotate -90 deg about the x-axis) 
 */
static void inv_rotate_x_point(pt)
data *pt;
{
   double y,z;
   y = pt->y;
   z = pt->z;
   pt->y =  z;
   pt->z = -y;
}

/***************************************/
/********  ACTUAL INTERSECTION  ********/
/***************************************/

/* 
 * find the intersections by eliminating z
 */
static int z_plane_intsct(a0,b0,c0,d0,pa1,pa2,pa3,ptmin,ptmax)
double  a0, b0, c0, d0;
data    *pa1, *pa2, *pa3;
data    *ptmin,*ptmax;
{
   double a1, b1, c1, d1;
   double an, bn, dn;
   data   N1;
   int    intsct=FALSE;

  /* 
   * plane    : a0*x + b0*y + c0*z + d0 = 0
   * triangle : Nodes (pa1, pa2, pa3)
   *            a1*x + b1*y + c1*z + d1 = 0;
   *
   * eliminate z
   *            a0/c0*x + b0/c0*y + d0/c0 = a1/c1*x + b1/c1*y + d1/c1
   *            (a0/c0 - a1/c1)x + (b0/c0 - b1/c1)y + (d0/c0 - d1/c1) = 0;
   *                        an*x +             bn*y +             dn  = 0;
   */

   /* find the normal vector to the triangle - the equation of the plane */
   find_normal_vector(pa1,pa2,pa3,&N1);
   a1 = N1.x;
   b1 = N1.y;
   c1 = N1.z;
   d1 = -(a1*pa1->x + b1*pa1->y + c1*pa1->z);

   an = a0/c0 - a1/c1; 
   bn = b0/c0 - b1/c1; 
   dn = d0/c0 - d1/c1;
   if ((fabs(an) < SMALL) && (fabs(bn) < SMALL)) {
      /* the planes are parallel */
      (void) fprintf(stdout,"Warning!  The triangle is parallel to the plane!\n");
      intsct = FALSE;
   }

   /* Now find the intersection of a line with the triangle */
   intsct = z_intsct_tria(an,bn,dn,pa1,pa2,pa3,ptmin,ptmax);
   return(intsct);
}

/* 
 * find the intersection of a triangle with a plane (ax+by+d=0) 
 * should come out with a minimum of 2 points
 */
static int z_intsct_tria(a,b,d,p1,p2,p3,ptmin,ptmax)
double a,b,d;
data   *p1,*p2,*p3;
data    *ptmin,*ptmax;
{
   int    intsct=TRUE;
   data   point[3];
   double d1,d2,d3;
   double x1,y1,x2,y2,x4,y4,z4,x5,y5,z5,x6,y6,z6;
   double xi,yi,t;
   double xmin, xmax, ymin, ymax;
   int    i,j=0;

   ptmin->x =  LARGE;
   ptmax->x = -LARGE;

   /* first find out if the planes intersect */
   d1 = -(a*p1->x + b*p1->y);
   d2 = -(a*p2->x + b*p2->y);
   d3 = -(a*p3->x + b*p3->y);
   if ( ((d1>d)&&(d2>d)&&(d3>d)) || ((d1<d)&&(d2<d)&&(d3<d)) ) 
      intsct = FALSE;      /* points are all above or below the plane */

   if (intsct) {
      /* determine a bounding box for the triangle */
      xmin = min(p1->x, p2->x, p3->x) - 0.1;
      ymin = min(p1->y, p2->y, p3->y) - 0.1;
      xmax = max(p1->x, p2->x, p3->x) + 0.1;
      ymax = max(p1->y, p2->y, p3->y) + 0.1;

      /* find 2 points on the line ax + by + d = 0 */
      if ((fabs(a) < SMALL) && (fabs(b) < SMALL)) {
         (void) fprintf(stderr,"Error: Plane is not specified!\n");
         (void) fflush(stdout);            /* flush the IO buffer */
         return(FALSE);
      } else if (fabs(a) < SMALL) {           /* by = -d */
         x1 = xmin; y1 = -d/b;
         x2 = xmax; y2 = -d/b;
      } else if (fabs(b) < SMALL) {           /* ax = -d */
         y1 = ymin; x1 = -d/a;
         y2 = ymax; x2 = -d/a;
      } else {                                /* ax + by = -d */
         x1 = xmin; y1 = (-d-a*x1)/b;
         x2 = xmax; y2 = (-d-a*x2)/b;
      } 
      
      /* find the intersection of the line with the triangle */
      x4 = p1->x;  y4 = p1->y;  z4 = p1->z;
      x5 = p2->x;  y5 = p2->y;  z5 = p2->z;
      x6 = p3->x;  y6 = p3->y;  z6 = p3->z;
      if (lines_intersect(x1,y1,x2,y2,x4,y4,x5,y5,&xi,&yi,&t)) {
         point[j].x = xi;
         point[j].y = yi;
         point[j].z = z4 + t*(z5-z4);
         j++;
      }
      if (lines_intersect(x1,y1,x2,y2,x5,y5,x6,y6,&xi,&yi,&t)) {
         point[j].x = xi;
         point[j].y = yi;
         point[j].z = z5 + t*(z6-z5);
         j++;
      }
      if (lines_intersect(x1,y1,x2,y2,x6,y6,x4,y4,&xi,&yi,&t)) {
         point[j].x = xi;
         point[j].y = yi;
         point[j].z = z6 + t*(z4-z6);
         j++;
      }

      /* there can be 0 to 3 intersections */
      for (i=0; i<j; i++) {
         if ( (     point[i].x < ptmin->x-SMALL ) ||
             ((fabs(point[i].x - ptmin->x)<SMALL) && 
              (     point[i].y < ptmin->y-SMALL )) ||
             ((fabs(point[i].x - ptmin->x)<SMALL) && 
              (fabs(point[i].y - ptmin->y)<SMALL) && (point[i].z < ptmin->z)) ){
            ptmin->x = point[i].x;
            ptmin->y = point[i].y;
            ptmin->z = point[i].z;
         }
         if ( (     point[i].x > ptmax->x+SMALL ) ||
             ((fabs(point[i].x - ptmax->x)<SMALL) && 
              (     point[i].y > ptmax->y+SMALL )) ||
             ((fabs(point[i].x - ptmax->x)<SMALL) && 
              (fabs(point[i].y - ptmax->y)<SMALL) && (point[i].z > ptmax->z)) ){
            ptmax->x = point[i].x;
            ptmax->y = point[i].y;
            ptmax->z = point[i].z;
         }
      }
      if (j==0) intsct=FALSE;
   }
   return(intsct);
}

/* 
 * intersection of 2 lines 
 */
static int lines_intersect(x1,y1,x2,y2,x3,y3,x4,y4,xi,yi,t)
double x1,y1,x2,y2,x3,y3,x4,y4,*xi,*yi,*t;
{
   int intst = TRUE;

   double xdm, ydm, xdn, ydn, cm, cn, denom;

   xdm = x2-x1;  ydm = y2-y1;
   xdn = x4-x3;  ydn = y4-y3;

   denom = xdn*ydm - xdm*ydn;
   /* if denom is zero, then lines are parallel */
   if (fabs(denom) < SMALL)  intst = FALSE;

   if (intst) {
      cm = x1*y2 - x2*y1;
      cn = x3*y4 - x4*y3;
      *xi = (xdn*cm - xdm*cn) / denom;
      *yi = (ydn*cm - ydm*cn) / denom;
      /* check to see if intersection is on both lines */
      if ( ( ((x1<=*xi+SMALL)&&(*xi<=x2+SMALL)) ||
             ((x2<=*xi+SMALL)&&(*xi<=x1+SMALL)) ) &&
           ( ((x3<=*xi+SMALL)&&(*xi<=x4+SMALL)) ||
             ((x4<=*xi+SMALL)&&(*xi<=x3+SMALL)) ) )
         intst = TRUE;
      else
         intst = FALSE;
      if (intst) {
         if ( ( ((y1<=*yi+SMALL)&&(*yi<=y2+SMALL)) ||
                ((y2<=*yi+SMALL)&&(*yi<=y1+SMALL)) ) &&
              ( ((y3<=*yi+SMALL)&&(*yi<=y4+SMALL)) ||
                ((y4<=*yi+SMALL)&&(*yi<=y3+SMALL)) ) )
            intst = TRUE;
         else
            intst = FALSE;
      }
      if (intst) {
         if      (fabs(x4-x3) > SMALL) *t = (*xi-x3)/(x4-x3);
         else if (fabs(y4-y3) > SMALL) *t = (*yi-y3)/(y4-y3);
      }
   }
   return(intst);
}

/************************************/
/********   MISC FUNCTIONS   ********/
/************************************/

static double min(a,b,c)
double a, b, c;
{ 
   double minval;
   
   minval = a;
   if (minval > b) minval = b; 
   if (minval > c) minval = c; 

   return(minval);
}

static double max(a,b,c)
double a, b, c;
{ 
   double maxval;
   
   maxval = a;
   if (maxval < b) maxval = b; 
   if (maxval < c) maxval = c; 

   return(maxval);
}

/***************************************/
/********   VECTOR OPERATIONS   ********/
/***************************************/

/* 
 * find the normal vector of a plane 
 */
static void find_normal_vector(pa1,pa2,pa3,normal)
data *pa1,*pa2,*pa3,*normal;
{
   data   lv1, lv2;

   lv1 = find_line_vector(pa1,pa2);
   lv2 = find_line_vector(pa2,pa3);
   *normal = cross_product(&lv1,&lv2);
}

/* 
 * given 2 points, find the line vector 
 */
static data find_line_vector(p1,p2)
data *p1, *p2;
{
   data linevec;

   linevec.x = p2->x - p1->x;
   linevec.y = p2->y - p1->y;
   linevec.z = p2->z - p1->z;
   return(linevec);
}

/* 
 * find the cross product of 2 vectors 
 */
static data cross_product(vec1,vec2)
data *vec1, *vec2;
{
   data cross;

   /* A x B = (AyBz - AzBy)x + (AzBx - AxBz)y + (AxBy - AyBx)z */
   cross.x = vec1->y * vec2->z - vec1->z * vec2->y;
   cross.y = vec1->z * vec2->x - vec1->x * vec2->z;
   cross.z = vec1->x * vec2->y - vec1->y * vec2->x;
   return(cross);
}


/***************************************************************/
/********   SPECIAL CASE : Triangle + Orthogonal Plane  ********/
/***************************************************************/

/* 
 * Triangle intersects an orthogonal plane (x=c, y=c, or z=c)
 */
static int tria_intsct_plane(p1,p2,p3,plane,val,ptmin,ptmax)
data *p1,*p2,*p3,*ptmin,*ptmax;
char plane;
double val;
{
   data   point[3];
   double xi,yi,zi;
   int    i,j=0;
   int    intsct = FALSE;

   ptmin->x =  LARGE;
   ptmax->x = -LARGE;

   if      (plane=='x') xi = val;
   else if (plane=='y') yi = val;
   else if (plane=='z') zi = val;

   if (line_intsct_plane(p1,p2,&xi,&yi,&zi,plane)){
      point[j].x = xi;
      point[j].y = yi;
      point[j].z = zi;
      j++;
   }
   if (line_intsct_plane(p2,p3,&xi,&yi,&zi,plane)){
      point[j].x = xi;
      point[j].y = yi;
      point[j].z = zi;
      j++;
   }
   if (line_intsct_plane(p3,p1,&xi,&yi,&zi,plane)){
      point[j].x = xi;
      point[j].y = yi;
      point[j].z = zi;
      j++;
   }
   for (i=0; i<j; i++) {
      if ( (     point[i].x < ptmin->x-SMALL ) ||
          ((fabs(point[i].x - ptmin->x)<SMALL) && 
           (     point[i].y < ptmin->y-SMALL )) ||
          ((fabs(point[i].x - ptmin->x)<SMALL) && 
           (fabs(point[i].y - ptmin->y)<SMALL) && (point[i].z < ptmin->z)) ){
         ptmin->x = point[i].x;
         ptmin->y = point[i].y;
         ptmin->z = point[i].z;
      }
      if ( (     point[i].x > ptmax->x+SMALL ) ||
          ((fabs(point[i].x - ptmax->x)<SMALL) && 
           (     point[i].y > ptmax->y+SMALL )) ||
          ((fabs(point[i].x - ptmax->x)<SMALL) && 
           (fabs(point[i].y - ptmax->y)<SMALL) && (point[i].z > ptmax->z)) ){
         ptmax->x = point[i].x;
         ptmax->y = point[i].y;
         ptmax->z = point[i].z;
      }
   }
   if (j<=0) intsct = FALSE;
   else      intsct = TRUE;
   return(intsct);
}

/*
 * find the intersection values between a line segment and a plane 
 */
static int line_intsct_plane(pta,ptb,xval,yval,zval,target)
double *xval,*yval,*zval;
data   *pta,*ptb;
char   target;
{
   int intsct = TRUE;
   double dx,dy,dz,t;

   dx = ptb->x - pta->x;
   dy = ptb->y - pta->y;
   dz = ptb->z - pta->z;
   if (target == 'x') 
       if (fabs(dx) < SMALL)  intsct = FALSE;
       else                   t  = (*xval - pta->x)/dx;
   else if (target == 'y') 
       if (fabs(dy) < SMALL)  intsct = FALSE;
       else                   t  = (*yval - pta->y)/dy;
   else if (target == 'z') 
       if (fabs(dz) < SMALL)  intsct = FALSE;
       else                   t  = (*zval - pta->z)/dz;

   if (intsct) {
      *xval = pta->x + t*dx;
      *yval = pta->y + t*dy;
      *zval = pta->z + t*dz;
      if ( (t< -1.0*SMALL) || (t> 1.0+SMALL)) intsct = FALSE;
   }
   return (intsct);
}

/***************************************************************/
/********   SPECIAL CASE : Orthogonal Triangle + Plane  ********/
/***************************************************************/

/* 
 * An orthogonal triangle (x=xc) intersects a plane 
 */
static int xtria_intsct_plane(a,b,c,d,pa1,pa2,pa3,ptmin,ptmax)
double  a, b, c, d;
data    *pa1, *pa2, *pa3;
data    *ptmin,*ptmax;
{
   int intsct;

   rotate_y_point(pa1);
   rotate_y_point(pa2);
   rotate_y_point(pa3);
   intsct = ztria_intsct_plane(-c,b,a,d,pa1,pa2,pa3,ptmin,ptmax);
   inv_rotate_y_point(pa1);
   inv_rotate_y_point(pa2);
   inv_rotate_y_point(pa3);
   inv_rotate_y_point(ptmin);
   inv_rotate_y_point(ptmax);
   return(intsct);
}

/* 
 * An orthogonal triangle (y=yc) intersects a plane 
 */
static int ytria_intsct_plane(a,b,c,d,pa1,pa2,pa3,ptmin,ptmax)
double  a, b, c, d;
data    *pa1, *pa2, *pa3;
data    *ptmin,*ptmax;
{
   int intsct;

   rotate_x_point(pa1);
   rotate_x_point(pa2);
   rotate_x_point(pa3);
   intsct = ztria_intsct_plane(a,-c,b,d,pa1,pa2,pa3,ptmin,ptmax);
   inv_rotate_x_point(pa1);
   inv_rotate_x_point(pa2);
   inv_rotate_x_point(pa3);
   inv_rotate_x_point(ptmin);
   inv_rotate_x_point(ptmax);
   return(intsct);
}

/* 
 * An orthogonal triangle (z=zc) intersects a plane 
 */
static int ztria_intsct_plane(a,b,c,d,pa1,pa2,pa3,ptmin,ptmax)
double  a, b, c, d;
data    *pa1, *pa2, *pa3;
data    *ptmin,*ptmax;
{
   double an, bn, dn;
   int    intsct;

   /* 
    * Triangle is parallel with z=0 plane (z=zc) 
    * Intersection is with line : a*x +  b*y + c*zc +d = 0 
    *                            an*x + bn*y +    dn   = 0;
    */
   an = a;
   bn = b;
   dn = c*pa1->z + d;  
   /* Now find the intersection of a line with the triangle */
   intsct = z_intsct_tria(an,bn,dn,pa1,pa2,pa3,ptmin,ptmax);
   return(intsct);
}
