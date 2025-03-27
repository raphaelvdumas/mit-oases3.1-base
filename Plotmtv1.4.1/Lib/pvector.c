/*
 * pvector.c - useful vector operations - these actually are on coords
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "CNdata.h"
#include "CNpvector.h"

/*
 * Normalize a vector
 * return 0 if the vector has zero-length
 */
int CNnormalize_vector(va)
CNcoord  *va;
{
   double dx,dy,dz,ds=1.0;

   /* Get the vector length */
   dx = va->x;
   dy = va->y;
   dz = va->z;
   ds = sqrt(dx*dx + dy*dy + dz*dz);
   if (ds < CN_SMALLER) return(0);

   /* Now normalize */
   va->x = va->x/ds;
   va->y = va->y/ds;
   va->z = va->z/ds;

   /* Return status */
   return(1);
}

/*
 * Return the length-squared of a vector
 */
double CNvector_lengthsq(va)
CNcoord  *va;
{
   double ds;

   /* Get the vector length */
   ds = va->x*va->x + va->y*va->y + va->z*va->z;
 
   /* Return */
   return(ds);
}

/* 
 * Get the angle between 2 vectors 
 */
double CNvector_angle(A,B)
CNcoord  *A, *B;
{
   double dotprod, angle;
   CNcoord va, vb;
   
   /* Make copies of the vectors */
   va.x = A->x;
   va.y = A->y;
   va.z = A->z;
   vb.x = B->x;
   vb.y = B->y;
   vb.z = B->z;

   /* Normalize the vectors first */
   (void) CNnormalize_vector(&va);
   (void) CNnormalize_vector(&vb);

   dotprod = CNvector_dotproduct(&va,&vb);
   angle   = acos(dotprod);

   return(angle);
}

/*
 * Get the dot product of 2 vectors 
 */
double CNvector_dotproduct(A,B)
CNcoord  *A, *B;
{
   double res;
   res = A->x * B->x + A->y * B->y + A->z * B->z;
   return(res);
}

/*
 * find the normal vector of a plane
 * This gets 3 points and returns a,b,c,d of the plane
 */
void CNconv_tria_to_plane(x1,y1,z1,x2,y2,z2,x3,y3,z3,a,b,c,d)
double x1,y1,z1,x2,y2,z2,x3,y3,z3;
double *a,*b,*c,*d;
{
   CNcoord  p1, p2, p3;
   CNcoord  normal;
   double   a0,b0,c0,d0;

   /* Put the points into point data-structure */
   p1.x = x1;   p1.y = y1;   p1.z = z1;
   p2.x = x2;   p2.y = y2;   p2.z = z2;
   p3.x = x3;   p3.y = y3;   p3.z = z3;

   /* Get the plane-normal */
   CNfind_normal_vector(&p1,&p2,&p3,&normal);

   /* The plane equation is ax+by+cz+d = 0 */
   a0 = normal.x;
   b0 = normal.y;
   c0 = normal.z;
   d0 = -1.0*(a0*x1 + b0*y1 + c0*z1);

   /* Normalize the plane */
   if (fabs(a0) > CN_SMALLER) {
      *a = a0 / a0;
      *b = b0 / a0;
      *c = c0 / a0;
      *d = d0 / a0;
   } else if (fabs(b0) > CN_SMALLER) {
      *a = a0 / b0;
      *b = b0 / b0;
      *c = c0 / b0;
      *d = d0 / b0;
   } else if (fabs(c0) > CN_SMALLER) {
      *a = a0 / c0;
      *b = b0 / c0;
      *c = c0 / c0;
      *d = d0 / c0;
   } else {
      *a = a0;
      *b = b0;
      *c = c0;
      *d = d0;
   }
}

/*
 * find the normal vector of a plane
 */
void CNfind_normal_vector(pa1,pa2,pa3,normal)
CNcoord *pa1,*pa2,*pa3,*normal;
{
   CNcoord  lv1, lv2;

   lv1 = CNfind_line_vector(pa1,pa2);
   lv2 = CNfind_line_vector(pa2,pa3);
   *normal = CNcross_product(&lv1,&lv2);
}

/*
 * given 2 points, find the line vector
 */
CNcoord  CNfind_line_vector(p1,p2)
CNcoord *p1, *p2;
{
   CNcoord  linevec;

   linevec.x = p2->x - p1->x;
   linevec.y = p2->y - p1->y;
   linevec.z = p2->z - p1->z;
   return(linevec);
}

/*
 * find the cross product of 2 vectors
 */
CNcoord  CNcross_product(vec1,vec2)
CNcoord  *vec1, *vec2;
{
   CNcoord  cross;

   /* A x B = (AyBz - AzBy)x + (AzBx - AxBz)y + (AxBy - AyBx)z */
   cross.x = vec1->y * vec2->z - vec1->z * vec2->y;
   cross.y = vec1->z * vec2->x - vec1->x * vec2->z;
   cross.z = vec1->x * vec2->y - vec1->y * vec2->x;
   return(cross);
}

