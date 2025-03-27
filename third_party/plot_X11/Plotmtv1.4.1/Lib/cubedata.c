/*
 * cubedata.c - procedures for manipulating cubes and blocks
 */
 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include <math.h>
#include "CNplot.h"

static void solid_cube_faces();
static void break_cube_in_x();
static void break_cube_in_y();
static void break_cube_in_z();

static void create_tria();
static void create_rect();

/*
 * Find out which faces of the cube (or prisms) are exposed
 * This routine just finds the 3 exposed faces of a cube
 * (the 3 faces closest to the viewer) and subdivides these
 * faces if the cube is composed of 2 prisms.
 */
void CNfind_exposed_faces_of_cube(B, ggrid,
                                  polyhead,  polytail,
                                  nodehead,  nodetail,
                                  pointhead, pointtail,
                                  xminin, xmaxin, 
                                  yminin, ymaxin,
                                  zminin, zmaxin,
                                  dosort)
CNblockptr  B;                       /* Block containing cube      */
CNmesh4Dptr ggrid;                   /* Grid with material info    */
CNpolyptr   *polyhead, *polytail;    /* List of polygons returned  */
CNnodeptr   *nodehead, *nodetail;    /* List of nodes returned     */
CNpointptr  *pointhead, *pointtail;  /* List of points returned    */
int         xminin, xmaxin;          /* x-face view flags          */
int         yminin, ymaxin;          /* y-face view flags          */
int         zminin, zmaxin;          /* z-face view flags          */
int         *dosort;                 /* Sort before plotting       */
{
   if (B == NULL || B->cube == NULL) return;
 
   /* Determine the composition of the cube */
   if (!CNcube_is_divided(B->cube)) {
      /* We have a single cube */
      solid_cube_faces(B, 
                       polyhead,  polytail,
                       nodehead,  nodetail,
                       pointhead, pointtail,
                       xminin, xmaxin, 
                       yminin, ymaxin,
                       zminin, zmaxin);
      *dosort = CN_FALSE;
   } else {
      /* The cube is divided into 2 prisms */
      *dosort = CN_FALSE;

      /* break it in x */
      if ((B->cube->pcode & CN_XPRISM) != 0) {
         break_cube_in_x(B, ggrid,
                         polyhead,  polytail,
                         nodehead,  nodetail,
                         pointhead, pointtail,
                         xminin, xmaxin, 
                         yminin, ymaxin,
                         zminin, zmaxin, dosort);
      } else if ((B->cube->pcode & CN_YPRISM) != 0) {
         break_cube_in_y(B, ggrid,
                         polyhead,  polytail,
                         nodehead,  nodetail,
                         pointhead, pointtail,
                         xminin, xmaxin, 
                         yminin, ymaxin,
                         zminin, zmaxin, dosort);
      } else if ((B->cube->pcode & CN_ZPRISM) != 0) {
         break_cube_in_z(B, ggrid,
                         polyhead,  polytail,
                         nodehead,  nodetail,
                         pointhead, pointtail,
                         xminin, xmaxin, 
                         yminin, ymaxin,
                         zminin, zmaxin, dosort);
      }
   }
}


/*
 * The cube has just one material - get the 3 faces closest
 * to the viewer.
 */
static void solid_cube_faces(B,
                             polyhead,  polytail,
                             nodehead,  nodetail,
                             pointhead, pointtail,
                             xminin, xmaxin, 
                             yminin, ymaxin,
                             zminin, zmaxin)
CNblockptr  B;                       /* Block containing cube      */
CNpolyptr   *polyhead, *polytail;    /* List of polygons returned  */
CNnodeptr   *nodehead, *nodetail;    /* List of nodes returned     */
CNpointptr  *pointhead, *pointtail;  /* List of points returned    */
int         xminin, xmaxin;          /* x-face view flags          */
int         yminin, ymaxin;          /* y-face view flags          */
int         zminin, zmaxin;          /* z-face view flags          */
{
   CNcubeptr   C;
   CNpointptr  pt000, pt001, pt010, pt011, pt100, pt101, pt110, pt111;
   CNnodeptr   nd000, nd001, nd010, nd011, nd100, nd101, nd110, nd111;
   int         bmat;

   if (B == NULL || B->cube == NULL) return;
 
   /* Material is in mat1 */
   bmat  = B->cube->mat1;

   /* Create points and nodes */
   C     = B->cube;
   pt000 = CNinsert_point(pointhead, pointtail, C->x0, C->y0, C->z0, 0);
   pt001 = CNinsert_point(pointhead, pointtail, C->x0, C->y0, C->z1, 0);
   pt010 = CNinsert_point(pointhead, pointtail, C->x0, C->y1, C->z0, 0);
   pt011 = CNinsert_point(pointhead, pointtail, C->x0, C->y1, C->z1, 0);
   pt100 = CNinsert_point(pointhead, pointtail, C->x1, C->y0, C->z0, 0);
   pt101 = CNinsert_point(pointhead, pointtail, C->x1, C->y0, C->z1, 0);
   pt110 = CNinsert_point(pointhead, pointtail, C->x1, C->y1, C->z0, 0);
   pt111 = CNinsert_point(pointhead, pointtail, C->x1, C->y1, C->z1, 0);

   /* Create nodes */
   nd000 = CNinsert_tailnode(nodehead, nodetail, pt000, B->t000, 0);
   nd001 = CNinsert_tailnode(nodehead, nodetail, pt001, B->t001, 0);
   nd010 = CNinsert_tailnode(nodehead, nodetail, pt010, B->t010, 0);
   nd011 = CNinsert_tailnode(nodehead, nodetail, pt011, B->t011, 0);
   nd100 = CNinsert_tailnode(nodehead, nodetail, pt100, B->t100, 0);
   nd101 = CNinsert_tailnode(nodehead, nodetail, pt101, B->t101, 0);
   nd110 = CNinsert_tailnode(nodehead, nodetail, pt110, B->t110, 0);
   nd111 = CNinsert_tailnode(nodehead, nodetail, pt111, B->t111, 0);

   /*
    * Create polygons (rectangles/triangles)
    * The basic unit is a rectangle, which is divided into 2 triangles
    * if the cube has 2 different prisms/materials.  
    */

   /*
    * XPLANE 
    */

   /* Rectangles on the x-plane */
   if (xminin) {

      /* Draw the surface at x=xmax */
      if (pt100->x > pt000->x) { /* x1 is greater than x0 */
         create_rect(polyhead, polytail, 
                     nd100, nd110, nd111, nd101, bmat,
                     CNcube_xmax_is_electrode(C));
      } else { /* x0 is greater than x1 */
         create_rect(polyhead, polytail, 
                     nd000, nd010, nd011, nd001, bmat,
                     CNcube_xmax_is_electrode(C));
      } /* if x1 > x0 */

   } else if (xmaxin) { /* The xmin plane is closer */

      /* Draw the surface at x=xmin */
      if (pt100->x > pt000->x) {
         create_rect(polyhead, polytail, 
                     nd000, nd010, nd011, nd001, bmat,
                     CNcube_xmin_is_electrode(C));
      } else { /* x0 is greater than x1 */
         create_rect(polyhead, polytail, 
                     nd100, nd110, nd111, nd101, bmat,
                     CNcube_xmin_is_electrode(C));
      } /* if x1 > x0 */

   } 

   /*
    * YPLANE 
    */

   /* Rectangles on the y-plane */
   if (yminin) {

      /* Draw the surface at y=ymax */
      if (pt010->y > pt000->y) { /* y1 > y0 */
         create_rect(polyhead, polytail, 
                     nd010, nd011, nd111, nd110, bmat,
                     CNcube_ymax_is_electrode(C));
      } else { /* y0 > y1 */
         create_rect(polyhead, polytail, 
                     nd000, nd001, nd101, nd100, bmat,
                     CNcube_ymax_is_electrode(C));
      } /* if y1 > y0 */

   } else if (ymaxin) {

      /* Draw the surface at y=ymin */
      if (pt010->y > pt000->y) { /* y1 > y0 */
         create_rect(polyhead, polytail, 
                     nd000, nd001, nd101, nd100, bmat,
                     CNcube_ymin_is_electrode(C));
      } else { /* y0 > y1 */
         create_rect(polyhead, polytail, 
                     nd010, nd011, nd111, nd110, bmat,
                     CNcube_ymin_is_electrode(C));
      } /* if y1 > y0 */

   }

   /*
    * ZPLANE 
    */

   /* Rectangles on the z-plane */
   if (zminin) {

      /* Draw the surface at z=zmax */
      if (pt001->z > pt000->z) { /* z1 > z0 */
         create_rect(polyhead, polytail, 
                     nd001, nd101, nd111, nd011, bmat,
                     CNcube_zmax_is_electrode(C));
      } else { /* z0 > z1 */
         create_rect(polyhead, polytail, 
                     nd000, nd100, nd110, nd010, bmat,
                     CNcube_zmax_is_electrode(C));
      } /* if z1 > z0 */

   } else if (zmaxin) {

      /* Draw the surface at z=zmin */
      if (pt001->z > pt000->z) { /* z1 > z0 */
         create_rect(polyhead, polytail, 
                     nd000, nd100, nd110, nd010, bmat,
                     CNcube_zmin_is_electrode(C));
      } else { /* z0 > z1 */
         create_rect(polyhead, polytail, 
                     nd001, nd101, nd111, nd011, bmat,
                     CNcube_zmin_is_electrode(C));
      } /* if z1 > z0 */

   }
}


/*
 * Break up a cube into its prisms (x-prisms only)
 */
static void break_cube_in_x(B, ggrid, 
                            polyhead,  polytail,
                            nodehead,  nodetail,
                            pointhead, pointtail,
                            xminin, xmaxin, 
                            yminin, ymaxin,
                            zminin, zmaxin, dosort)
CNblockptr  B;                       /* Block containing cube      */
CNmesh4Dptr ggrid;                   /* Grid with material info    */
CNpolyptr   *polyhead, *polytail;    /* List of polygons returned  */
CNnodeptr   *nodehead, *nodetail;    /* List of nodes returned     */
CNpointptr  *pointhead, *pointtail;  /* List of points returned    */
int         xminin, xmaxin;          /* x-face view flags          */
int         yminin, ymaxin;          /* y-face view flags          */
int         zminin, zmaxin;          /* z-face view flags          */
int         *dosort;                 /* Sort before plotting       */
{
   CNcubeptr  C;
   CNpointptr pt000, pt001, pt010, pt011, pt100, pt101, pt110, pt111;
   CNnodeptr  nd000, nd001, nd010, nd011, nd100, nd101, nd110, nd111;
   int        bmat, bmat_is_air;
   int        tmat, tmat_is_air;
   int        tmp;
   
   if ((B==NULL) || (B->cube==NULL)) return;
   if ((B->cube->pcode & CN_XPRISM) == 0) return;

   /* Create points and nodes */
   C     = B->cube;
   pt000 = CNinsert_point(pointhead, pointtail, C->x0, C->y0, C->z0, 0);
   pt001 = CNinsert_point(pointhead, pointtail, C->x0, C->y0, C->z1, 0);
   pt010 = CNinsert_point(pointhead, pointtail, C->x0, C->y1, C->z0, 0);
   pt011 = CNinsert_point(pointhead, pointtail, C->x0, C->y1, C->z1, 0);
   pt100 = CNinsert_point(pointhead, pointtail, C->x1, C->y0, C->z0, 0);
   pt101 = CNinsert_point(pointhead, pointtail, C->x1, C->y0, C->z1, 0);
   pt110 = CNinsert_point(pointhead, pointtail, C->x1, C->y1, C->z0, 0);
   pt111 = CNinsert_point(pointhead, pointtail, C->x1, C->y1, C->z1, 0);
 
   /* Create nodes */
   nd000 = CNinsert_tailnode(nodehead, nodetail, pt000, B->t000, 0);
   nd001 = CNinsert_tailnode(nodehead, nodetail, pt001, B->t001, 0);
   nd010 = CNinsert_tailnode(nodehead, nodetail, pt010, B->t010, 0);
   nd011 = CNinsert_tailnode(nodehead, nodetail, pt011, B->t011, 0);
   nd100 = CNinsert_tailnode(nodehead, nodetail, pt100, B->t100, 0);
   nd101 = CNinsert_tailnode(nodehead, nodetail, pt101, B->t101, 0);
   nd110 = CNinsert_tailnode(nodehead, nodetail, pt110, B->t110, 0);
   nd111 = CNinsert_tailnode(nodehead, nodetail, pt111, B->t111, 0);

   /* Mat for bottom and top prism */
   if ((C->pcode & CN_MAT1ONMIN) != 0) {
      /* Bottom prism is mat1, top prism is mat2 */
      bmat = C->mat1;
      tmat = C->mat2;
   } else {
      /* Bottom prism is mat2, top prism is mat1 */
      bmat = C->mat2;
      tmat = C->mat1;
   }
   bmat_is_air = CNmesh4D_mat_is_air(bmat, ggrid);
   tmat_is_air = CNmesh4D_mat_is_air(tmat, ggrid);

   /* Modify xminin, xmaxin flags */
   if (C->x0 > C->x1) {
      tmp    = xminin;
      xminin = xmaxin;
      xmaxin = tmp;
   }
   if (C->y0 > C->y1) {
      tmp    = yminin;
      yminin = ymaxin;
      ymaxin = tmp;
   }
   if (C->z0 > C->z1) {
      tmp    = zminin;
      zminin = zmaxin;
      zmaxin = tmp;
   }

   /*
    * Start with a full cube with the prism side along the diagonal
    * and then slice that up against the boundaries
    *
    * Only the faces exposed to the viewer, indicated by xminin, xmaxin
    * flags are created.  xminin=T means that the xmin plane is farther
    * away from the viewer and is thus hidden from view.
    */

   /* 
    * The prisms are parallel to the x-axis 
    */
   if ((C->pcode & CN_CUTONMIN) != 0) {
      /*
       * BOTTOM PRISM
       *  z----
       *   | /|
       *   |/X|
       *   x---y
       * cutline goes thru the x=xmin axis
       */

      /* Create the prism only if it is not air */
      if (!bmat_is_air) {
       
         /* Triangle on the x=xmin plane */
         if (!xminin)
         create_tria(polyhead, polytail, nd000, nd010, nd011, bmat,
                     CNcube_xmin_is_electrode(C));

         /* Triangle on the x=xmax plane */
         if (!xmaxin)
         create_tria(polyhead, polytail, nd100, nd110, nd111, bmat,
                     CNcube_xmax_is_electrode(C));

         /* Rectangle on the y=ymax plane */
         if (!ymaxin)
         create_rect(polyhead, polytail, nd010, nd110, nd111, nd011, bmat,
                     CNcube_ymax_is_electrode(C));

         /* Rectangle on the z=zmin plane */
         if (!zminin)
         create_rect(polyhead, polytail, nd000, nd010, nd110, nd100, bmat,
                     CNcube_zmin_is_electrode(C));

         /* Slanted Rectangle on the yz diagonal */
         /* Don't draw the diagonal if both prisms are visible */
         if (tmat_is_air) {
         create_rect(polyhead, polytail, nd000, nd100, nd111, nd011, bmat,
                     CNcube_diag_is_electrode(C));
         *dosort = CN_TRUE;
         }

      }

      /*
       * TOP PRISM
       *  z----
       *   |X/|
       *   |/ |
       *   x---y
       * cutline goes thru the x=xmin axis
       */

      /* Create the prism only if it is not air */
      if (!tmat_is_air) {
       
         /* Triangle on the x=xmin plane */
         if (!xminin)
         create_tria(polyhead, polytail, nd000, nd001, nd011, tmat,
                     CNcube_xmin_is_electrode(C));

         /* Triangle on the x=xmax plane */
         if (!xmaxin)
         create_tria(polyhead, polytail, nd100, nd101, nd111, tmat,
                     CNcube_xmax_is_electrode(C));

         /* Rectangle on the y=ymin plane */
         if (!yminin)
         create_rect(polyhead, polytail, nd000, nd100, nd101, nd001, tmat,
                     CNcube_ymin_is_electrode(C));

         /* Rectangle on the z=zmax plane */
         if (!zmaxin)
         create_rect(polyhead, polytail, nd001, nd011, nd111, nd101, tmat,
                     CNcube_zmax_is_electrode(C));

         /* Slanted Rectangle on the yz diagonal */
         /* Don't draw the diagonal if both prisms are visible */
         if (bmat_is_air) {
         create_rect(polyhead, polytail, nd000, nd100, nd111, nd011, tmat,
                     CNcube_diag_is_electrode(C));
         *dosort = CN_TRUE;
         }

      }

   } else {
      /*
       * BOTTOM PRISM
       *  z----
       *   |\ |
       *   |X\|
       *   x---y
       * cutline does not go thru the x=xmin axis
       */

      /* Create the prism only if it is not air */
      if (!bmat_is_air) {
       
         /* Triangle on the x=xmin plane */
         if (!xminin)
         create_tria(polyhead, polytail, nd000, nd010, nd001, bmat,
                     CNcube_xmin_is_electrode(C));

         /* Triangle on the x=xmax plane */
         if (!xmaxin)
         create_tria(polyhead, polytail, nd100, nd110, nd101, bmat,
                     CNcube_xmax_is_electrode(C));

         /* Rectangle on the y=ymin plane */
         if (!yminin)
         create_rect(polyhead, polytail, nd000, nd100, nd101, nd001, bmat,
                     CNcube_ymin_is_electrode(C));

         /* Rectangle on the z=zmin plane */
         if (!zminin)
         create_rect(polyhead, polytail, nd000, nd010, nd110, nd100, bmat,
                     CNcube_zmin_is_electrode(C));

         /* Slanted Rectangle on the yz diagonal */
         /* Don't draw the diagonal if both prisms are visible */
         if (tmat_is_air) {
         create_rect(polyhead, polytail, nd010, nd110, nd101, nd001, bmat,
                     CNcube_diag_is_electrode(C));
         *dosort = CN_TRUE;
         }

      }

      /*
       * TOP PRISM
       *  z----
       *   |\X|
       *   | \|
       *   x---y
       * cutline does not go thru the x=xmin axis
       */

      /* Create the prism only if it is not air */
      if (!tmat_is_air) {
       
         /* Triangle on the x=xmin plane */
         if (!xminin)
         create_tria(polyhead, polytail, nd011, nd010, nd001, tmat,
                     CNcube_xmin_is_electrode(C));

         /* Triangle on the x=xmax plane */
         if (!xmaxin)
         create_tria(polyhead, polytail, nd111, nd110, nd101, tmat,
                     CNcube_xmax_is_electrode(C));

         /* Rectangle on the y=ymax plane */
         if (!ymaxin)
         create_rect(polyhead, polytail, nd010, nd110, nd111, nd011, tmat,
                     CNcube_ymax_is_electrode(C));

         /* Rectangle on the z=zmax plane */
         if (!zmaxin)
         create_rect(polyhead, polytail, nd001, nd011, nd111, nd101, tmat,
                     CNcube_zmax_is_electrode(C));

         /* Slanted Rectangle on the yz diagonal */
         /* Don't draw the diagonal if both prisms are visible */
         if (bmat_is_air) {
         create_rect(polyhead, polytail, nd010, nd110, nd101, nd001, tmat,
                     CNcube_diag_is_electrode(C));
         *dosort = CN_TRUE;
         }

      }
   }
}


/*
 * Break up a cube into its prisms (y-prisms only)
 */
static void break_cube_in_y(B, ggrid, 
                            polyhead,  polytail,
                            nodehead,  nodetail,
                            pointhead, pointtail,
                            xminin, xmaxin, 
                            yminin, ymaxin,
                            zminin, zmaxin, dosort)
CNblockptr  B;                       /* Block containing cube      */
CNmesh4Dptr ggrid;                   /* Grid with material info    */
CNpolyptr   *polyhead, *polytail;    /* List of polygons returned  */
CNnodeptr   *nodehead, *nodetail;    /* List of nodes returned     */
CNpointptr  *pointhead, *pointtail;  /* List of points returned    */
int         xminin, xmaxin;          /* x-face view flags          */
int         yminin, ymaxin;          /* y-face view flags          */
int         zminin, zmaxin;          /* z-face view flags          */
int         *dosort;                 /* Sort before plotting       */
{
   CNcubeptr  C;
   CNpointptr pt000, pt001, pt010, pt011, pt100, pt101, pt110, pt111;
   CNnodeptr  nd000, nd001, nd010, nd011, nd100, nd101, nd110, nd111;
   int        bmat, bmat_is_air;
   int        tmat, tmat_is_air;
   int        tmp;
   
   if ((B==NULL) || (B->cube==NULL)) return;
   if ((B->cube->pcode & CN_YPRISM) == 0) return;

   /* Create points and nodes */
   C     = B->cube;
   pt000 = CNinsert_point(pointhead, pointtail, C->x0, C->y0, C->z0, 0);
   pt001 = CNinsert_point(pointhead, pointtail, C->x0, C->y0, C->z1, 0);
   pt010 = CNinsert_point(pointhead, pointtail, C->x0, C->y1, C->z0, 0);
   pt011 = CNinsert_point(pointhead, pointtail, C->x0, C->y1, C->z1, 0);
   pt100 = CNinsert_point(pointhead, pointtail, C->x1, C->y0, C->z0, 0);
   pt101 = CNinsert_point(pointhead, pointtail, C->x1, C->y0, C->z1, 0);
   pt110 = CNinsert_point(pointhead, pointtail, C->x1, C->y1, C->z0, 0);
   pt111 = CNinsert_point(pointhead, pointtail, C->x1, C->y1, C->z1, 0);
 
   /* Create nodes */
   nd000 = CNinsert_tailnode(nodehead, nodetail, pt000, B->t000, 0);
   nd001 = CNinsert_tailnode(nodehead, nodetail, pt001, B->t001, 0);
   nd010 = CNinsert_tailnode(nodehead, nodetail, pt010, B->t010, 0);
   nd011 = CNinsert_tailnode(nodehead, nodetail, pt011, B->t011, 0);
   nd100 = CNinsert_tailnode(nodehead, nodetail, pt100, B->t100, 0);
   nd101 = CNinsert_tailnode(nodehead, nodetail, pt101, B->t101, 0);
   nd110 = CNinsert_tailnode(nodehead, nodetail, pt110, B->t110, 0);
   nd111 = CNinsert_tailnode(nodehead, nodetail, pt111, B->t111, 0);

   /* Mat for bottom and top prism */
   if ((C->pcode & CN_MAT1ONMIN) != 0) {
      /* Bottom prism is mat1, top prism is mat2 */
      bmat = C->mat1;
      tmat = C->mat2;
   } else {
      /* Bottom prism is mat2, top prism is mat1 */
      bmat = C->mat2;
      tmat = C->mat1;
   }
   bmat_is_air = CNmesh4D_mat_is_air(bmat, ggrid);
   tmat_is_air = CNmesh4D_mat_is_air(tmat, ggrid);

   /* Modify xminin, xmaxin flags */
   if (C->x0 > C->x1) {
      tmp    = xminin;
      xminin = xmaxin;
      xmaxin = tmp;
   }
   if (C->y0 > C->y1) {
      tmp    = yminin;
      yminin = ymaxin;
      ymaxin = tmp;
   }
   if (C->z0 > C->z1) {
      tmp    = zminin;
      zminin = zmaxin;
      zmaxin = tmp;
   }

   /*
    * Start with a full cube with the prism side along the diagonal
    * and then slice that up against the boundaries
    *
    * Only the faces exposed to the viewer, indicated by xminin, xmaxin
    * flags are created.  xminin=T means that the xmin plane is farther
    * away from the viewer and is thus hidden from view.
    */

   /* 
    * The prisms are parallel to the y-axis 
    */
   if ((C->pcode & CN_CUTONMIN) != 0) {
      /*
       * BOTTOM PRISM
       *  x----
       *   | /|
       *   |/X|
       *   y---z
       * cutline goes thru the y=ymin axis
       */

      /* Create the prism only if it is not air */
      if (!bmat_is_air) {
       
         /* Triangle on the y=ymin plane */
         if (!yminin)
         create_tria(polyhead, polytail, nd000, nd101, nd001, bmat,
                     CNcube_ymin_is_electrode(C));

         /* Triangle on the y=ymax plane */
         if (!ymaxin)
         create_tria(polyhead, polytail, nd010, nd111, nd011, bmat,
                     CNcube_ymax_is_electrode(C));

         /* Rectangle on the x=xmin plane */
         if (!xminin)
         create_rect(polyhead, polytail, nd000, nd010, nd011, nd001, bmat,
                     CNcube_xmin_is_electrode(C));

         /* Rectangle on the z=zmax plane */
         if (!zmaxin)
         create_rect(polyhead, polytail, nd001, nd011, nd111, nd101, bmat,
                     CNcube_zmax_is_electrode(C));

         /* Slanted Rectangle on the xz diagonal */
         if (tmat_is_air) {
         create_rect(polyhead, polytail, nd000, nd101, nd111, nd010, bmat,
                     CNcube_diag_is_electrode(C));
         *dosort = CN_TRUE;
         }
      }

      /*
       * TOP PRISM
       *  x----
       *   |X/|
       *   |/ |
       *   y---z
       * cutline goes thru the y=ymin axis
       */

      /* Create the prism only if it is not air */
      if (!tmat_is_air) {
       
         /* Triangle on the y=ymin plane */
         if (!yminin)
         create_tria(polyhead, polytail, nd000, nd100, nd101, tmat,
                     CNcube_ymin_is_electrode(C));

         /* Triangle on the y=ymax plane */
         if (!ymaxin)
         create_tria(polyhead, polytail, nd010, nd110, nd111, tmat,
                     CNcube_ymax_is_electrode(C));

         /* Rectangle on the x=xmax plane */
         if (!xmaxin)
         create_rect(polyhead, polytail, nd100, nd110, nd111, nd101, tmat,
                     CNcube_xmax_is_electrode(C));

         /* Rectangle on the z=zmin plane */
         if (!zminin)
         create_rect(polyhead, polytail, nd000, nd010, nd110, nd100, tmat,
                     CNcube_zmin_is_electrode(C));

         /* Slanted Rectangle on the xz diagonal */
         if (bmat_is_air) {
         create_rect(polyhead, polytail, nd000, nd101, nd111, nd010, tmat,
                     CNcube_diag_is_electrode(C));
         *dosort = CN_TRUE;
         }
      }

   } else {
      /*
       * BOTTOM PRISM
       *  x----
       *   |\ |
       *   |X\|
       *   y---z
       * cutline does not go thru the y=ymin axis
       */

      /* Create the prism only if it is not air */
      if (!bmat_is_air) {
       
         /* Triangle on the y=ymin plane */
         if (!yminin)
         create_tria(polyhead, polytail, nd000, nd100, nd001, bmat,
                     CNcube_ymin_is_electrode(C));

         /* Triangle on the y=ymax plane */
         if (!ymaxin)
         create_tria(polyhead, polytail, nd010, nd110, nd011, bmat,
                     CNcube_ymax_is_electrode(C));

         /* Rectangle on the x=xmin plane */
         if (!xminin)
         create_rect(polyhead, polytail, nd000, nd010, nd011, nd001, bmat,
                     CNcube_xmin_is_electrode(C));

         /* Rectangle on the z=zmin plane */
         if (!zminin)
         create_rect(polyhead, polytail, nd000, nd010, nd110, nd100, bmat,
                     CNcube_zmin_is_electrode(C));

         /* Slanted Rectangle on the xz diagonal */
         if (tmat_is_air) {
         create_rect(polyhead, polytail, nd100, nd110, nd011, nd001, bmat,
                     CNcube_diag_is_electrode(C));
         *dosort = CN_TRUE;
         }

      }

      /*
       * TOP PRISM
       *  x----
       *   |\X|
       *   | \|
       *   y---z
       * cutline does not go thru the y=ymin axis
       */

      /* Create the prism only if it is not air */
      if (!tmat_is_air) {
       
         /* Triangle on the y=ymin plane */
         if (!yminin)
         create_tria(polyhead, polytail, nd101, nd100, nd001, tmat,
                     CNcube_ymin_is_electrode(C));

         /* Triangle on the y=ymax plane */
         if (!ymaxin)
         create_tria(polyhead, polytail, nd111, nd110, nd011, tmat,
                     CNcube_ymax_is_electrode(C));

         /* Rectangle on the x=xmax plane */
         if (!xmaxin)
         create_rect(polyhead, polytail, nd100, nd110, nd111, nd101, tmat,
                     CNcube_xmax_is_electrode(C));

         /* Rectangle on the z=zmax plane */
         if (!zmaxin)
         create_rect(polyhead, polytail, nd001, nd011, nd111, nd101, tmat,
                     CNcube_zmax_is_electrode(C));

         /* Slanted Rectangle on the xz diagonal */
         if (bmat_is_air) {
         create_rect(polyhead, polytail, nd100, nd110, nd011, nd001, tmat,
                     CNcube_diag_is_electrode(C));
         *dosort = CN_TRUE;
         }

      }
   }
}


/*
 * Break up a cube into its prisms (z-prisms only)
 */
static void break_cube_in_z(B, ggrid, 
                            polyhead,  polytail,
                            nodehead,  nodetail,
                            pointhead, pointtail,
                            xminin, xmaxin, 
                            yminin, ymaxin,
                            zminin, zmaxin, dosort)
CNblockptr  B;                       /* Block containing cube      */
CNmesh4Dptr ggrid;                   /* Grid with material info    */
CNpolyptr   *polyhead, *polytail;    /* List of polygons returned  */
CNnodeptr   *nodehead, *nodetail;    /* List of nodes returned     */
CNpointptr  *pointhead, *pointtail;  /* List of points returned    */
int         xminin, xmaxin;          /* x-face view flags          */
int         yminin, ymaxin;          /* y-face view flags          */
int         zminin, zmaxin;          /* z-face view flags          */
int         *dosort;                 /* Sort before plotting       */
{
   CNcubeptr  C;
   CNpointptr pt000, pt001, pt010, pt011, pt100, pt101, pt110, pt111;
   CNnodeptr  nd000, nd001, nd010, nd011, nd100, nd101, nd110, nd111;
   int        bmat, bmat_is_air;
   int        tmat, tmat_is_air;
   int        tmp;
   
   if ((B==NULL) || (B->cube==NULL)) return;
   if ((B->cube->pcode & CN_ZPRISM) == 0) return;

   /* Create points and nodes */
   C     = B->cube;
   pt000 = CNinsert_point(pointhead, pointtail, C->x0, C->y0, C->z0, 0);
   pt001 = CNinsert_point(pointhead, pointtail, C->x0, C->y0, C->z1, 0);
   pt010 = CNinsert_point(pointhead, pointtail, C->x0, C->y1, C->z0, 0);
   pt011 = CNinsert_point(pointhead, pointtail, C->x0, C->y1, C->z1, 0);
   pt100 = CNinsert_point(pointhead, pointtail, C->x1, C->y0, C->z0, 0);
   pt101 = CNinsert_point(pointhead, pointtail, C->x1, C->y0, C->z1, 0);
   pt110 = CNinsert_point(pointhead, pointtail, C->x1, C->y1, C->z0, 0);
   pt111 = CNinsert_point(pointhead, pointtail, C->x1, C->y1, C->z1, 0);
 
   /* Create nodes */
   nd000 = CNinsert_tailnode(nodehead, nodetail, pt000, B->t000, 0);
   nd001 = CNinsert_tailnode(nodehead, nodetail, pt001, B->t001, 0);
   nd010 = CNinsert_tailnode(nodehead, nodetail, pt010, B->t010, 0);
   nd011 = CNinsert_tailnode(nodehead, nodetail, pt011, B->t011, 0);
   nd100 = CNinsert_tailnode(nodehead, nodetail, pt100, B->t100, 0);
   nd101 = CNinsert_tailnode(nodehead, nodetail, pt101, B->t101, 0);
   nd110 = CNinsert_tailnode(nodehead, nodetail, pt110, B->t110, 0);
   nd111 = CNinsert_tailnode(nodehead, nodetail, pt111, B->t111, 0);

   /* Mat for bottom and top prism */
   if ((C->pcode & CN_MAT1ONMIN) != 0) {
      /* Bottom prism is mat1, top prism is mat2 */
      bmat = C->mat1;
      tmat = C->mat2;
   } else {
      /* Bottom prism is mat2, top prism is mat1 */
      bmat = C->mat2;
      tmat = C->mat1;
   }
   bmat_is_air = CNmesh4D_mat_is_air(bmat, ggrid);
   tmat_is_air = CNmesh4D_mat_is_air(tmat, ggrid);

   /* Modify xminin, xmaxin flags */
   if (C->x0 > C->x1) {
      tmp    = xminin;
      xminin = xmaxin;
      xmaxin = tmp;
   }
   if (C->y0 > C->y1) {
      tmp    = yminin;
      yminin = ymaxin;
      ymaxin = tmp;
   }
   if (C->z0 > C->z1) {
      tmp    = zminin;
      zminin = zmaxin;
      zmaxin = tmp;
   }

   /*
    * Start with a full cube with the prism side along the diagonal
    * and then slice that up against the boundaries
    *
    * Only the faces exposed to the viewer, indicated by xminin, xmaxin
    * flags are created.  xminin=T means that the xmin plane is farther
    * away from the viewer and is thus hidden from view.
    */

   /* 
    * The prisms are parallel to the z-axis 
    */
   if ((C->pcode & CN_CUTONMIN) != 0) {
      /*
       * BOTTOM PRISM
       *  y----
       *   | /|
       *   |/X|
       *   z---x
       * cutline goes thru the z=zmin axis
       */

      /* Create the prism only if it is not air */
      if (!bmat_is_air) {
       
         /* Triangle on the z=zmin plane */
         if (!zminin)
         create_tria(polyhead, polytail, nd000, nd100, nd110, bmat,
                     CNcube_zmin_is_electrode(C));

         /* Triangle on the z=zmax plane */
         if (!zmaxin)
         create_tria(polyhead, polytail, nd001, nd101, nd111, bmat,
                     CNcube_zmax_is_electrode(C));

         /* Rectangle on the y=ymin plane */
         if (!yminin)
         create_rect(polyhead, polytail, nd000, nd100, nd101, nd001, bmat,
                     CNcube_ymin_is_electrode(C));

         /* Rectangle on the x=xmax plane */
         if (!xmaxin)
         create_rect(polyhead, polytail, nd100, nd110, nd111, nd101, bmat,
                     CNcube_xmax_is_electrode(C));

         /* Slanted Rectangle on the xy diagonal */
         if (tmat_is_air) {
         create_rect(polyhead, polytail, nd000, nd110, nd111, nd001, bmat,
                     CNcube_diag_is_electrode(C));
         *dosort = CN_TRUE;
         }
      }

      /*
       * TOP PRISM
       *  y----
       *   |X/|
       *   |/ |
       *   z---x
       * cutline goes thru the z=zmin axis
       */

      /* Create the prism only if it is not air */
      if (!tmat_is_air) {
       
         /* Triangle on the z=zmin plane */
         if (!zminin)
         create_tria(polyhead, polytail, nd000, nd010, nd110, tmat,
                     CNcube_zmin_is_electrode(C));

         /* Triangle on the z=zmax plane */
         if (!zmaxin)
         create_tria(polyhead, polytail, nd001, nd011, nd111, tmat,
                     CNcube_zmax_is_electrode(C));

         /* Rectangle on the y=ymax plane */
         if (!ymaxin)
         create_rect(polyhead, polytail, nd010, nd110, nd111, nd011, tmat,
                     CNcube_ymax_is_electrode(C));

         /* Rectangle on the x=xmin plane */
         if (!xminin)
         create_rect(polyhead, polytail, nd000, nd010, nd011, nd001, tmat,
                     CNcube_xmin_is_electrode(C));

         /* Slanted Rectangle on the xy diagonal */
         if (bmat_is_air) {
         create_rect(polyhead, polytail, nd000, nd110, nd111, nd001, tmat,
                     CNcube_diag_is_electrode(C));
         *dosort = CN_TRUE;
         }

      }

   } else {
      /*
       * BOTTOM PRISM
       *  y----
       *   |\ |
       *   |X\|
       *   z---x
       * cutline does not go thru the z=zmin axis
       */

      /* Create the prism only if it is not air */
      if (!bmat_is_air) {
       
         /* Triangle on the z=zmin plane */
         if (!zminin)
         create_tria(polyhead, polytail, nd000, nd100, nd010, bmat,
                     CNcube_zmin_is_electrode(C));

         /* Triangle on the z=zmax plane */
         if (!zmaxin)
         create_tria(polyhead, polytail, nd001, nd101, nd011, bmat,
                     CNcube_zmax_is_electrode(C));

         /* Rectangle on the y=ymin plane */
         if (!yminin)
         create_rect(polyhead, polytail, nd000, nd100, nd101, nd001, bmat,
                     CNcube_ymin_is_electrode(C));

         /* Rectangle on the x=xmin plane */
         if (!xminin)
         create_rect(polyhead, polytail, nd000, nd010, nd011, nd001, bmat,
                     CNcube_xmin_is_electrode(C));

         /* Slanted Rectangle on the xz diagonal */
         if (tmat_is_air) {
         create_rect(polyhead, polytail, nd010, nd100, nd101, nd011, bmat,
                     CNcube_diag_is_electrode(C));
         *dosort = CN_TRUE;
         }

      }

      /*
       * TOP PRISM
       *  y----
       *   |\X|
       *   | \|
       *   z---x
       * cutline does not go thru the z=zmin axis
       */

      /* Create the prism only if it is not air */
      if (!tmat_is_air) {
       
         /* Triangle on the z=zmin plane */
         if (!zminin)
         create_tria(polyhead, polytail, nd100, nd010, nd110, tmat,
                     CNcube_zmin_is_electrode(C));

         /* Triangle on the z=zmax plane */
         if (!zmaxin)
         create_tria(polyhead, polytail, nd101, nd011, nd111, tmat,
                     CNcube_zmax_is_electrode(C));

         /* Rectangle on the y=ymax plane */
         if (!ymaxin)
         create_rect(polyhead, polytail, nd010, nd110, nd111, nd011, tmat,
                     CNcube_ymax_is_electrode(C));

         /* Rectangle on the x=xmax plane */
         if (!xmaxin)
         create_rect(polyhead, polytail, nd100, nd110, nd111, nd101, tmat,
                     CNcube_xmax_is_electrode(C));

         /* Slanted Rectangle on the xy diagonal */
         if (bmat_is_air) {
         create_rect(polyhead, polytail, nd010, nd100, nd101, nd011, tmat,
                     CNcube_diag_is_electrode(C));
         *dosort = CN_TRUE;
         }

      }
   }
}



/*
 * CREATION UTILITIES
 */

/*
 * Create a triangle and place it in the poly list
 */
static void create_tria(polyhead, polytail, n1, n2, n3, region, is_electrode)
CNpolyptr  *polyhead, *polytail;
CNnodeptr  n1, n2, n3;
int        region;
int        is_electrode;
{
   CNnlistptr nlhead=NULL, nltail=NULL;

   if (is_electrode) region = CN_ELECTRODE;
   (void) CNinsert_tailnlist(&nlhead, &nltail, n1);
   (void) CNinsert_tailnlist(&nlhead, &nltail, n2);
   (void) CNinsert_tailnlist(&nlhead, &nltail, n3);
   (void) CNinsert_tailnlist(&nlhead, &nltail, n1);
   (void) CNinsert_poly(polyhead, polytail, nlhead, nltail, region, 0);
}

/*
 * Create a rectangle and place it in the poly list
 */
static void create_rect(polyhead, polytail, n1, n2, n3, n4, region,is_electrode)
CNpolyptr  *polyhead, *polytail;
CNnodeptr  n1, n2, n3, n4;
int        region;
int        is_electrode;
{
   CNnlistptr nlhead=NULL, nltail=NULL;

   if (is_electrode) region = CN_ELECTRODE;
   (void) CNinsert_tailnlist(&nlhead, &nltail, n1);
   (void) CNinsert_tailnlist(&nlhead, &nltail, n2);
   (void) CNinsert_tailnlist(&nlhead, &nltail, n3);
   (void) CNinsert_tailnlist(&nlhead, &nltail, n4);
   (void) CNinsert_tailnlist(&nlhead, &nltail, n1);
   (void) CNinsert_poly(polyhead, polytail, nlhead, nltail, region, 0);
}
