/*
 * matrix.c - matrix and view transformations 
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <malloc.h>
#include "CNdata.h"
#include "CNplot3D.h"

#define X_AXIS 0                       /* X-Axis                 */
#define Y_AXIS 1                       /* Y-Axis                 */
#define Z_AXIS 2                       /* Z-Axis                 */
#define PI     3.14159265358979323846  /* PI                     */
#define DEG2RAD PI/180.0               /* Degrees to Radians     */
#define RAD2DEG 180.0/PI               /* Radians to Degrees     */
#define SMALL   1.0e-99                /* A small number         */

/*
 * The C matrix is organized in row-major order, which
 * means that the matrix appears as :
 *    A[0][0]  A[0][1] ..   A[0][n]
 *    A[1][0]  ...
 *    ...
 *    A[m][0]  ...     ..   A[m][n]
 *
 * Note that this is the same notation/convention used 
 * for mathematical representation of matrices, i.e. Aij = A[i][j]
 *
 * Note also that the cartesian coordinate system is different
 * form that used in matrix representation, i.e. element (1,0) would
 * refer to the matrix element A[0][1]
 */

/*
 * Matrix procedures 
 */
CNviewptr   CNcreate_view();
void        CNinitialize_view();
void        CNreinitialize_view();

void        CNcalculate_view_transfo();
void        CNcalculate_inv_view_transfo();
static int  diag_matrix();
static void rotate_view();
static void inv_rotate_view();
static void scale_view();
static void inv_scale_view();
void        CNscale_window_to_viewport();

void        CNcalculate_eyepos();
void        CNrotate_view();
void        CNget_view_angles();
static void get_view_angles();

static void init_matrix();
static void create_identity_matrix();
static void create_zero_matrix();
static void create_scale_matrix();
static void create_mirror_matrix();
static void create_translate_matrix();
static void create_perspective_matrix();
static void mult_matrix();
void        CNcopy_matrix();
static void print_matrix();

CNcoord     CNtransform_point();


/**********************************/
/*** INITIALIZE VIEW PARAMETERS ***/
/**********************************/

/*
 * Create and initialize a view structure
 */
CNviewptr CNcreate_view()
{
   CNviewptr view_params;
   unsigned int size = sizeof(CNview);

   /* Allocate space for the view data-struct */
   if ((view_params = (CNviewptr)malloc(size))==NULL) {
      (void) fprintf(stderr,"Error! Insufficient memory!\n");
      (void) fprintf(stderr,"Cannot allocate CNview in CNcreate_view()!\n");
      exit(-1);
   }

   /* Now initialize the view */
   CNinitialize_view(view_params,0.0,1.0,0.0,1.0,0.0,1.0);

   /* return */
   return(view_params);
}


/* 
 * Delete the view 
 */
void CNdelete_view(view_params)
CNviewptr view_params;
{
   /* Delete the structure at the given address */
   free ((char *)view_params);
}


/*
 * Initialize the 3D viewing parameters
 */
void CNinitialize_view(view_params,xmin,xmax,ymin,ymax,zmin,zmax)
CNviewptr view_params;
double    xmin, xmax;
double    ymin, ymax;
double    zmin, zmax;
{
   double ds;

   /* flag */
   view_params->flag = 0;

   /* view center */
   view_params->viewcenter.x  =  0.5*(xmax+xmin);
   view_params->viewcenter.y  =  0.5*(ymax+ymin);
   view_params->viewcenter.z  =  0.5*(zmax+zmin);

   /* eye position relative to the view center */
   view_params->eyepos.x  = 1.0;
   view_params->eyepos.y  = 1.5;
   view_params->eyepos.z  = 0.5;

   /* view-up vector */
   view_params->viewup.x = 0.0;
   view_params->viewup.y = 0.0;
   view_params->viewup.z = 1.0;

   /* Min and max boundaries */
   view_params->xmin = xmin;
   view_params->xmax = xmax;
   view_params->ymin = ymin;
   view_params->ymax = ymax;
   view_params->zmin = zmin;
   view_params->zmax = zmax;

   /* Window scaling factors */
   view_params->windscl_xl = - CN_WINDOWSCALE;
   view_params->windscl_xr =   CN_WINDOWSCALE;
   view_params->windscl_yb = - CN_WINDOWSCALE;
   view_params->windscl_yt =   CN_WINDOWSCALE;

   view_params->prev_windscl_xl = view_params->windscl_xl;
   view_params->prev_windscl_xr = view_params->windscl_xr;
   view_params->prev_windscl_yb = view_params->windscl_yb;
   view_params->prev_windscl_yt = view_params->windscl_yt;

   /* True window - Want the window to be x:[-ds,ds]  y:[-ds,ds] */
   ds = MAXOF3(xmax-xmin, ymax-ymin, zmax-zmin);
   view_params->window_xl = view_params->windscl_xl*ds;
   view_params->window_xr = view_params->windscl_xr*ds;
   view_params->window_yb = view_params->windscl_yb*ds;
   view_params->window_yt = view_params->windscl_yt*ds;

   /* viewport - in unit coordinates */
   view_params->viewport_xl = 0.0;
   view_params->viewport_xr = 1.0;
   view_params->viewport_yb = 0.0;
   view_params->viewport_yt = 1.0;

   /* left_handed_world (right-handed coords by default) */
   view_params->left_handed_world = 0;

   /* Equal scaling on all axes */
   view_params->axis_scale  = 1;
   view_params->xaxis_scale = 1.0;
   view_params->yaxis_scale = 1.0;
   view_params->zaxis_scale = 1.0;

   /* Label the axes */
   view_params->axis_label    = 1;

   /* Rotation of axis with view (1=static; axis rotates) */
   view_params->axis_movement = 0;

   /* 3D axis guides */
   view_params->axis_guides   = CN_TRUE;

   /* Hiddenline */
   view_params->hiddenline    = 0;

   /* view options */
   view_params->paint_cube    = 0;
   view_params->projection    = CN_PARALLEL;

   /* Initialize the view-transfo matrix */
   CNcalculate_view_transfo(view_params);
}

/* 
 * Reinitialize the view
 */
void CNreinitialize_view(view_params,xmin,xmax,ymin,ymax,zmin,zmax)
CNviewptr view_params;
double    xmin, xmax;
double    ymin, ymax;
double    zmin, zmax;
{
   double ds;

   /* 
    * The eye position is specified relative to the view center,
    * so it does not have to be modified
    */

   /* view center */
   view_params->viewcenter.x  =  0.5*(xmax+xmin);
   view_params->viewcenter.y  =  0.5*(ymax+ymin);
   view_params->viewcenter.z  =  0.5*(zmax+zmin);

   /* Min and max boundaries */
   view_params->xmin = xmin;
   view_params->xmax = xmax;
   view_params->ymin = ymin;
   view_params->ymax = ymax;
   view_params->zmin = zmin;
   view_params->zmax = zmax;

   /* Window - Adjust only if asked to */
   ds = MAXOF3(xmax-xmin, ymax-ymin, zmax-zmin);
   view_params->window_xl = view_params->windscl_xl*ds;
   view_params->window_xr = view_params->windscl_xr*ds;
   view_params->window_yb = view_params->windscl_yb*ds;
   view_params->window_yt = view_params->windscl_yt*ds;

   /* Initialize the view-transfo matrix */
   CNcalculate_view_transfo(view_params);
}



/*******************************/
/*** TRANSFORMATION MATRICES ***/
/*******************************/

/*
 * Calculate the view-transformation matrix given the eye-position,
 * the view-center, and the view-up vector.
 */
void CNcalculate_view_transfo(view_params)
CNviewptr view_params;
{
   CNmatrix R, Rt, Rp, Rmx, Rpr, Rs, Rvp;

   /* Create an identity matrix */
   create_identity_matrix(R);

   /* Translate to the viewcenter */
   create_translate_matrix(Rt,-view_params->viewcenter.x,
                              -view_params->viewcenter.y,
                              -view_params->viewcenter.z);
   mult_matrix(R, R, Rt);

#ifdef DEBUG
   (void) fprintf(stdout,"Translation matrix:\n");
   print_matrix(Rt);
#endif

   /* Apply scaling */
   scale_view(Rs, view_params->axis_scale, 
                  view_params->xaxis_scale,
                  view_params->yaxis_scale,
                  view_params->zaxis_scale,
                  view_params->xmin, view_params->xmax, 
                  view_params->ymin, view_params->ymax, 
                  view_params->zmin, view_params->zmax);
   mult_matrix(R, R, Rs);

#ifdef DEBUG
   (void) fprintf(stdout,"Scaling matrix:\n");
   print_matrix(Rs);
#endif

   /* Get the rotation matrix */
   rotate_view(Rp, &(view_params->eyepos), 
                   &(view_params->viewup));
   mult_matrix(R,R,Rp);

#ifdef DEBUG
   (void) fprintf(stdout,"Rotation matrix:\n");
   print_matrix(Rp);
#endif

   /* Mirror to switch from right-hand coords to left-hand coords */
   if (view_params->left_handed_world) {
      create_mirror_matrix(Rmx, X_AXIS);
      mult_matrix(R, R, Rmx);
   }

   /* Project on to view plane */
   if (view_params->projection == CN_PERSPECTIVE) {
      create_perspective_matrix(Rpr, -view_params->viewcenter.z);
      mult_matrix(R, R, Rpr);
#ifdef DEBUG
      (void) fprintf(stdout,"Perspective matrix:\n");
      print_matrix(Rpr);
#endif
   }

   /* Window-viewport scaling */
   CNscale_window_to_viewport(Rvp, 
                     view_params->window_xl, view_params->window_xr,
                     view_params->window_yb, view_params->window_yt,
                     view_params->viewport_xl, view_params->viewport_xr,
                     view_params->viewport_yb, view_params->viewport_yt);

#ifdef DEBUG
   (void) fprintf(stdout,"Viewport Scaling matrix:\n");
   print_matrix(Rvp);
#endif

   /* The final matrix */
   mult_matrix(view_params->view_transfo, R, Rvp);

#ifdef DEBUG
   (void) fprintf(stdout,"Transform matrix:\n");
   print_matrix(view_params->view_transfo);
#endif
}


/*
 * Calculate the inverse view-transformation matrix given the eye-position,
 * the view-center, and the view-up vector.
 */
void CNcalculate_inv_view_transfo(view_params,T,debug)
CNviewptr view_params;
CNmatrix  T;
int       debug;
{
   CNmatrix R;
   CNmatrix Rt,     Rp,     Rmx,     Rs,     Rvp;
   CNmatrix Rt_inv, Rp_inv, Rmx_inv, Rs_inv, Rvp_inv;
   int      err;

   /* Create an identity matrix */
   create_identity_matrix(R);

   /* window -> viewport */
   CNscale_window_to_viewport(Rvp, 
                     view_params->window_xl, view_params->window_xr,
                     view_params->window_yb, view_params->window_yt,
                     view_params->viewport_xl, view_params->viewport_xr,
                     view_params->viewport_yb, view_params->viewport_yt);

   /* viewport->window scaling */
   CNscale_viewport_to_window(Rvp_inv, 
                     view_params->window_xl, view_params->window_xr,
                     view_params->window_yb, view_params->window_yt,
                     view_params->viewport_xl, view_params->viewport_xr,
                     view_params->viewport_yb, view_params->viewport_yt);
   mult_matrix(R, R, Rvp_inv);

   /* No perspective viewing yet */

   /* Mirror to switch from right-hand coords to left-hand coords */
   create_identity_matrix(Rmx);
   create_identity_matrix(Rmx_inv);
   if (view_params->left_handed_world) {
      create_mirror_matrix(Rmx, X_AXIS);
      create_mirror_matrix(Rmx_inv, X_AXIS);
      mult_matrix(R, R, Rmx_inv);
   }

   /* Get the rotation matrix */
   rotate_view(Rp, &(view_params->eyepos),
                   &(view_params->viewup));

   /* Get the inverse rotation matrix */
   inv_rotate_view(Rp_inv, 
                   &(view_params->eyepos),
                   &(view_params->viewup));
   mult_matrix(R,R,Rp_inv);

   /* Apply scaling */
   scale_view(Rs, view_params->axis_scale,
                  view_params->xaxis_scale,
                  view_params->yaxis_scale,
                  view_params->zaxis_scale,
                  view_params->xmin, view_params->xmax,
                  view_params->ymin, view_params->ymax,
                  view_params->zmin, view_params->zmax);

   /* Apply inverse scaling */
   inv_scale_view(Rs_inv, 
                  view_params->axis_scale,
                  view_params->xaxis_scale,
                  view_params->yaxis_scale,
                  view_params->zaxis_scale,
                  view_params->xmin, view_params->xmax,
                  view_params->ymin, view_params->ymax,
                  view_params->zmin, view_params->zmax);
   mult_matrix(R, R, Rs_inv);

   /* Translate to the viewcenter */
   create_translate_matrix(Rt,-view_params->viewcenter.x,
                              -view_params->viewcenter.y,
                              -view_params->viewcenter.z);

   /* Translate from the viewcenter */
   create_translate_matrix(Rt_inv, 
                               view_params->viewcenter.x,
                               view_params->viewcenter.y,
                               view_params->viewcenter.z);
   mult_matrix(R, R, Rt_inv);

   /* The final result */
   CNcopy_matrix(T,R);

   /* Check the results */
   err = diag_matrix("transform",view_params->view_transfo, T, 0);
   if (debug) {
      if (err==0) {
      (void) diag_matrix("transform",view_params->view_transfo, T, 1);
      (void) fprintf(stdout,"The inverse matrix was calculated correctly!\n");
      } else {
      (void) diag_matrix("viewport-window",Rvp, Rvp_inv, 1);
      (void) diag_matrix("mirror",Rmx, Rmx_inv, 1);
      (void) diag_matrix("rotate",Rp, Rp_inv, 1);
      (void) diag_matrix("scale",Rs, Rs_inv, 1);
      (void) diag_matrix("translate",Rt, Rt_inv, 1);
      (void) diag_matrix("transform",view_params->view_transfo, T, 1);
      }
   }
#ifdef DEBUG
#endif
}

/*
 * Diagnose - print the product of 2 matrices
 */
static int diag_matrix(label,R1,R2,verbose)
char     *label;
CNmatrix R1,R2;
int      verbose;
{
#define SMALL_NUM 1e-5    /* Precision is not too important... */
   CNmatrix R,Rx1,Rx2;
   int      i,j,ERR=0;

   mult_matrix(R, R1, R2);

   /* Print out the results */
   if (verbose) {
      (void) fprintf(stdout,"Matrix operation - %s\n",label);
      print_matrix(R1);
      print_matrix(R2);
      print_matrix(R);
   }

   /* 
    * Check to see if this is an identity matrix,
    * do this by multiplying an arbitrary matrix by R 
    */
   init_matrix(Rx1,
               1.0, 2.0, 3.0, 4.0,
               5.0, 6.0, 7.0, 8.0,
               9.0,10.0,11.0,12.0,
              13.0,14.0,15.0,16.0);
   mult_matrix(Rx2, R, Rx1);
   for (i=0; i<CN_NDIM && !ERR; i++)
   for (j=0; j<CN_NDIM && !ERR; j++)
      if (fabs(Rx1[i][j] - Rx2[i][j]) > SMALL_NUM) ERR = 1;
   
   if (ERR) 
   (void) fprintf(stderr,"Warning! The product matrix is not an identity matrix!\n");
   
   return(ERR); 
}

/*
 * This just rotates about the axis given by eyepos-viewcenter.
 * The eyeposition is assumed to be relative to the viewcenter,
 * i.e. the true eyepos is eyepos+viewcenter
 * No translation is applied!
 */
static void rotate_view(T,eyepos,viewup)
CNmatrix T;
CNcoord  *eyepos, *viewup;
{
   CNmatrix Rx, Ry, Rp, Rz;
   CNcoord  up;
   double   x, y, z, l, v;
   double   ux, uy, w;

   /* eye/center */
   x = eyepos->x;
   y = eyepos->y;
   z = eyepos->z;
   l = sqrt(x*x + y*y + z*z);
   v = sqrt(      y*y + z*z);

   /* 
    * Rotate around the x-axis to put the view-plane normal
    * onto the (x,z) plane.  Rotation of theta,
    * where cos(theta) = z/v, and sin(theta) = y/v
    *
    * If the viewplane normal is parallel to the x-axis then
    * don't rotate about x
    */
   if (v > SMALL) 
      init_matrix(Rx,
                  1.0, 0.0, 0.0, 0.0,
                  0.0, z/v, y/v, 0.0,
                  0.0,-y/v, z/v, 0.0,
                  0.0, 0.0, 0.0, 1.0);
   else 
      create_identity_matrix(Rx);

   /* 
    * Rotate around the y-axis to put the view-plane normal
    * onto the z-axis.  Rotation of (-phi)
    * where cos( phi) = v/l, and sin( phi) =  x/l
    * where cos(-phi) = v/l, and sin(-phi) = -x/l
    *
    * If the eye-center to view-center distance is zero then 
    * don't rotate about y (in this case Rx=I, Ry=I, I=Ident matrix)
    */
   if (l > SMALL) 
      init_matrix(Ry,
                  v/l, 0.0, x/l, 0.0,
                  0.0, 1.0, 0.0, 0.0,
                 -x/l, 0.0, v/l, 0.0,
                  0.0, 0.0, 0.0, 1.0);
   else 
      create_identity_matrix(Ry);
  
   /*
    * Multiply the two matrices to get the current rotation matrix
    */
   mult_matrix(Rp,Rx,Ry);

   /*
    * Rotate about the z-axis to align the view-plane "Up" vector with
    * the original y-axis.  Transform the "Up" vector first, and then
    * rotate using the transformed vector coordinates.
    * Rotate by theta
    * where cos(theta) = uy/w, sin(theta) = ux/w
    */
   up = CNtransform_point(viewup,Rp);
   ux = up.x;
   uy = up.y;
   w  = sqrt(ux*ux + uy*uy);
   if (w > SMALL)
      init_matrix(Rz,
                  uy/w, ux/w, 0.0, 0.0,
                 -ux/w, uy/w, 0.0, 0.0,
                  0.0,  0.0,  1.0, 0.0,
                  0.0,  0.0,  0.0, 1.0);
   else 
      create_identity_matrix(Rz);

   /* 
    * Multiply the previous rotation matrix with Rz
    */
   mult_matrix(Rp,Rp,Rz);

   /* 
    * Send back the final matrix 
    */
   CNcopy_matrix(T, Rp);
}

/*
 * This just rotates about the axis given by eyepos-viewcenter.
 * The eyeposition is assumed to be relative to the viewcenter,
 * i.e. the true eyepos is eyepos+viewcenter
 * No translation is applied!
 * Get the inverse-transformation matrix
 */
static void inv_rotate_view(T,eyepos,viewup)
CNmatrix T;
CNcoord  *eyepos, *viewup;
{
   CNmatrix Rx,     Ry,     Rp,     Rz;
   CNmatrix Rx_inv, Ry_inv, Rp_inv, Rz_inv;
   CNcoord  up;
   double   x, y, z, l, v;
   double   ux, uy, w;

   /* eye/center */
   x = eyepos->x;
   y = eyepos->y;
   z = eyepos->z;
   l = sqrt(x*x + y*y + z*z);
   v = sqrt(      y*y + z*z);

   /* 
    * Rotate around the x-axis to put the view-plane normal
    * onto the (x,z) plane.  Rotation of theta,
    * where cos(theta) = z/v, and sin(theta) = y/v
    *
    * If the viewplane normal is parallel to the x-axis then
    * don't rotate about x
    */
   if (v > SMALL) {
      init_matrix(Rx,
                  1.0, 0.0, 0.0, 0.0,
                  0.0, z/v, y/v, 0.0,
                  0.0,-y/v, z/v, 0.0,
                  0.0, 0.0, 0.0, 1.0);
      init_matrix(Rx_inv,
                  1.0, 0.0, 0.0, 0.0,
                  0.0, z/v,-y/v, 0.0,
                  0.0, y/v, z/v, 0.0,
                  0.0, 0.0, 0.0, 1.0);
   } else {
      create_identity_matrix(Rx);
      create_identity_matrix(Rx_inv);
   }

   /* 
    * Rotate around the y-axis to put the view-plane normal
    * onto the z-axis.  Rotation of (-phi)
    * where cos( phi) = v/l, and sin( phi) =  x/l
    * where cos(-phi) = v/l, and sin(-phi) = -x/l
    *
    * If the eye-center to view-center distance is zero then 
    * don't rotate about y (in this case Rx=I, Ry=I, I=Ident matrix)
    */
   if (l > SMALL) { 
      init_matrix(Ry,
                  v/l, 0.0, x/l, 0.0,
                  0.0, 1.0, 0.0, 0.0,
                 -x/l, 0.0, v/l, 0.0,
                  0.0, 0.0, 0.0, 1.0);
      init_matrix(Ry_inv,
                  v/l, 0.0,-x/l, 0.0,
                  0.0, 1.0, 0.0, 0.0,
                  x/l, 0.0, v/l, 0.0,
                  0.0, 0.0, 0.0, 1.0);
   } else {
      create_identity_matrix(Ry);
      create_identity_matrix(Ry_inv);
   }
  
   /*
    * Multiply the two matrices to get the current rotation matrix
    */
   mult_matrix(Rp,Rx,Ry);
   mult_matrix(Rp_inv,Ry_inv,Rx_inv);

   /*
    * Rotate about the z-axis to align the view-plane "Up" vector with
    * the original y-axis.  Transform the "Up" vector first, and then
    * rotate using the transformed vector coordinates.
    * Rotate by theta
    * where cos(theta) = uy/w, sin(theta) = ux/w
    */
   up = CNtransform_point(viewup,Rp);
   ux = up.x;
   uy = up.y;
   w  = sqrt(ux*ux + uy*uy);
   if (w > SMALL) {
      init_matrix(Rz,
                  uy/w, ux/w, 0.0, 0.0,
                 -ux/w, uy/w, 0.0, 0.0,
                  0.0,  0.0,  1.0, 0.0,
                  0.0,  0.0,  0.0, 1.0);
      init_matrix(Rz_inv,
                  uy/w,-ux/w, 0.0, 0.0,
                  ux/w, uy/w, 0.0, 0.0,
                  0.0,  0.0,  1.0, 0.0,
                  0.0,  0.0,  0.0, 1.0);
   } else {
      create_identity_matrix(Rz);
      create_identity_matrix(Rz_inv);
   }

   /* 
    * The final   matrix T     = Rx     * Ry     * Rz 
    *                          = Rp              * Rz
    *
    * The inverse matrix T_inv = Rz_inv * Ry_inv * Rx_inv 
    *                          = Rz_inv * Rp_inv
    */
   mult_matrix(Rp,Rp,Rz);
   mult_matrix(Rp,Rz_inv,Rp_inv);

   /* 
    * Send back the final matrix 
    */
   CNcopy_matrix(T, Rp);
}

/* 
 * Apply scaling 
 */
static void scale_view(T, axis_scale, 
                       xaxis_scale, yaxis_scale, zaxis_scale,
                       xmin, xmax, ymin, ymax, zmin, zmax)
CNmatrix T;
short    axis_scale;
double   xaxis_scale, yaxis_scale, zaxis_scale;
double   xmin, xmax, ymin, ymax, zmin, zmax;
{
   double scalemax;
   double sx, sy, sz;
   double ds;

   sx = 1.0;
   sy = 1.0;
   sz = 1.0;

   /* Scale each axis so that all axis lengths are the same */
   if (axis_scale) {
      /* Scale the xyz axis scale factors so that the max scale is 1.0 */
      scalemax = MAXOF3(xaxis_scale, yaxis_scale, zaxis_scale);
      if (xaxis_scale > SMALL) xaxis_scale = xaxis_scale/scalemax;
      else                     xaxis_scale = 1.0;
      if (yaxis_scale > SMALL) yaxis_scale = yaxis_scale/scalemax;
      else                     yaxis_scale = 1.0;
      if (zaxis_scale > SMALL) zaxis_scale = zaxis_scale/scalemax;
      else                     zaxis_scale = 1.0;

      ds = MAXOF3(xmax-xmin, ymax-ymin, zmax-zmin);
      if ((xmax - xmin) > SMALL) sx = xaxis_scale*ds/(xmax - xmin);
      if ((ymax - ymin) > SMALL) sy = yaxis_scale*ds/(ymax - ymin);
      if ((zmax - zmin) > SMALL) sz = zaxis_scale*ds/(zmax - zmin);
   }

   /* Create the scale matrix */
   create_scale_matrix(T,sx,sy,sz);
}

/*
 * Apply scaling
 * Return the inverse transfo matrix
 */
static void inv_scale_view(T, axis_scale, 
                           xaxis_scale, yaxis_scale, zaxis_scale,
                           xmin, xmax, ymin, ymax, zmin, zmax)
CNmatrix T;
short    axis_scale;
double   xaxis_scale, yaxis_scale, zaxis_scale;
double   xmin, xmax, ymin, ymax, zmin, zmax;
{
   double scalemax;
   double sx, sy, sz;
   double ds;

   sx = 1.0;
   sy = 1.0;
   sz = 1.0;

   /* Scale each axis so that all axis lengths are the same */
   if (axis_scale) {
      /* Scale the xyz axis scale factors so that the max scale is 1.0 */
      scalemax = MAXOF3(xaxis_scale, yaxis_scale, zaxis_scale);
      if (xaxis_scale > SMALL) xaxis_scale = xaxis_scale/scalemax;
      else                     xaxis_scale = 1.0;
      if (yaxis_scale > SMALL) yaxis_scale = yaxis_scale/scalemax;
      else                     yaxis_scale = 1.0;
      if (zaxis_scale > SMALL) zaxis_scale = zaxis_scale/scalemax;
      else                     zaxis_scale = 1.0;

      ds = MAXOF3(xmax-xmin, ymax-ymin, zmax-zmin);
      if (fabs(ds) < SMALL) ds = SMALL;
      if ((xmax - xmin) > SMALL) sx = 1.0*(xmax - xmin)/(ds*xaxis_scale);
      if ((ymax - ymin) > SMALL) sy = 1.0*(ymax - ymin)/(ds*yaxis_scale);
      if ((zmax - zmin) > SMALL) sz = 1.0*(zmax - zmin)/(ds*zaxis_scale);
   }

   /* Create the scale matrix */
   create_scale_matrix(T,sx,sy,sz);
}

/*
 * Given a transformation matrix, find a new matrix that will enable the
 * entire plot to fit inside the viewport.
 */
void CNscale_window_to_viewport(T, 
                              window_xl, window_xr,
                              window_yb, window_yt,
                              viewport_xl, viewport_xr,
                              viewport_yb, viewport_yt)
CNmatrix T;
double   window_xl, window_xr;
double   window_yb, window_yt;
double   viewport_xl, viewport_xr;
double   viewport_yb, viewport_yt;
{
   double sx,sy,tx,ty;

   /* create a matrix to scale the window to the viewport */
   sx = (viewport_xr - viewport_xl)/(window_xr - window_xl);
   sy = (viewport_yt - viewport_yb)/(window_yt - window_yb);
   tx = viewport_xl - sx*window_xl;
   ty = viewport_yb - sy*window_yb;
   create_identity_matrix(T);
   T[0][0] = sx;
   T[1][1] = sy;
   T[3][0] = tx;
   T[3][1] = ty;
}


/*
 * Given a transformation matrix, find a new matrix that will enable the
 * entire plot to fit inside the viewport.
 * Return the inverse transformation matrix.
 */
void CNscale_viewport_to_window(T,
                              window_xl, window_xr,
                              window_yb, window_yt,
                              viewport_xl, viewport_xr,
                              viewport_yb, viewport_yt)
CNmatrix T;
double   window_xl, window_xr;
double   window_yb, window_yt;
double   viewport_xl, viewport_xr;
double   viewport_yb, viewport_yt;
{
   CNscale_window_to_viewport(T,
                              viewport_xl, viewport_xr,
                              viewport_yb, viewport_yt,
                              window_xl, window_xr,
                              window_yb, window_yt);
}



/****************************************/
/*** USEFUL TRANSFORMATION PROCEDURES ***/
/****************************************/

/*
 * Given absolute angle rotations, find the new eye-position 
 * Theta and phi are specified in degrees.
 */
void CNcalculate_eyepos(view_params,theta,phi,xeye,yeye,zeye)
CNviewptr view_params;
double    theta, phi;
double    *xeye,*yeye,*zeye;
{
   double   x, y, z, r;

   /* eye/center */
   x = view_params->eyepos.x;
   y = view_params->eyepos.y;
   z = view_params->eyepos.z;
   r = sqrt(x*x + y*y + z*z);

   /* The new eye pos */
   *xeye = r*sin(phi*DEG2RAD)*cos(theta*DEG2RAD);
   *yeye = r*sin(phi*DEG2RAD)*sin(theta*DEG2RAD);
   *zeye = r*cos(phi*DEG2RAD);
}


/*
 * Rotate the eyepos-viewcenter by the given angles
 * d_theta and d_phi are angle rotations from the current eyepos-viewcenter.
 */
void CNrotate_view(view_params,d_theta,d_phi)
CNviewptr view_params;
double    d_theta, d_phi;
{
   double   theta0, phi0;
   double   theta, phi;
   double   x,y,z;

   /* theta, phi rotation angles based on current eye position */
   CNget_view_angles(view_params, &theta0, &phi0);

   /* The new rotation angles */
   theta = theta0 + d_theta;
   phi   = phi0   + d_phi;  

   /* Constrict Phi to between 0 and 180 degrees */
   if (phi < 0.0 || phi > 180.0) phi = phi0;

   /* Get the new eye-position */
   CNcalculate_eyepos(view_params,
                      theta,phi,&x,&y,&z);
   view_params->eyepos.x = x; 
   view_params->eyepos.y = y; 
   view_params->eyepos.z = z; 

#ifdef DEBUG
   (void) fprintf(stdout,"Theta = %7.3f  Phi = %7.3f",theta,phi);
   print_point(&neweye);
#endif
}


/*
 * Calculate view angles based on a cartesian view-axis
 */
void CNget_view_angles(view_params, theta, phi)
CNviewptr view_params;
double    *theta;
double    *phi;
{
   /* theta, phi rotation angles based on current eye position */
   get_view_angles(&(view_params->eyepos), theta, phi);

}

/* 
 * Calculate view angles based on a cartesian view-axis
 */
static void get_view_angles(eyepos, theta, phi)
CNcoord  *eyepos;
double   *theta;
double   *phi;
{
   double dx, dy, dz, l, r;

   /* view vector */
   dx = eyepos->x;
   dy = eyepos->y;
   dz = eyepos->z;
   l  = sqrt(dx*dx + dy*dy + dz*dz);
   r  = sqrt(dx*dx + dy*dy);

   if (l > SMALL)
      *phi = acos(dz/l)*RAD2DEG;
   else
      *phi = 0.0;

   if (r > SMALL) {
      *theta = acos(dx/r)*RAD2DEG;
      if (dy < 0) *theta = 360.0 - *theta;
   } else
      *theta = 0.0;
}


/*************************************/
/*** BASIC TRANSFORMATION MATRICES ***/
/*************************************/

/* 
 * Initialize the identity matrix
 */
static void init_matrix(A,
                        A00, A01, A02, A03,
                        A10, A11, A12, A13,
                        A20, A21, A22, A23,
                        A30, A31, A32, A33)
CNmatrix A;
double   A00, A01, A02, A03;
double   A10, A11, A12, A13;
double   A20, A21, A22, A23;
double   A30, A31, A32, A33;
{
   A[0][0] = A00;
   A[0][1] = A01;
   A[0][2] = A02;
   A[0][3] = A03;

   A[1][0] = A10;
   A[1][1] = A11;
   A[1][2] = A12;
   A[1][3] = A13;

   A[2][0] = A20;
   A[2][1] = A21;
   A[2][2] = A22;
   A[2][3] = A23;

   A[3][0] = A30;
   A[3][1] = A31;
   A[3][2] = A32;
   A[3][3] = A33;
}

/* 
 * Initialize the identity matrix
 */
static void create_identity_matrix(A)
CNmatrix A;
{
   int i,j;

   for (i=0; i<CN_NDIM; i++) 
      for (j=0; j<CN_NDIM; j++) 
         if (i==j) A[i][j] = 1.0;
         else      A[i][j] = 0.0;
}

/* 
 * Initialize the zero matrix
 */
static void create_zero_matrix(A)
CNmatrix A;
{
   int i,j;

   for (i=0; i<CN_NDIM; i++) 
      for (j=0; j<CN_NDIM; j++) 
         A[i][j] = 0.0;
}

/* 
 * Create a matrix to apply scale transformations
 */
static void create_scale_matrix(A,sx,sy,sz)
CNmatrix A;
double   sx, sy, sz;
{
   /* Initialize the identity matrix */
   create_identity_matrix(A);

   /* Fill in the appropriate matrix elements */
   A[0][0] = sx;
   A[1][1] = sy;
   A[2][2] = sz;
}

/* 
 * Create a matrix to apply mirror transformations
 */
static void create_mirror_matrix(A,axis)
CNmatrix A;
int      axis; 
{
   double sx, sy, sz;

   sx = 1.0;
   sy = 1.0;
   sz = 1.0;
   if      (axis == X_AXIS) sx = -1.0;
   else if (axis == Y_AXIS) sy = -1.0;
   else if (axis == Z_AXIS) sz = -1.0;
   else {
      (void) fprintf(stderr,"Error: Unrecognized axis-type!\n");
      (void) fprintf(stderr,"No mirroring will be applied\n");
   }

   /* Create a scale matrix */
   create_scale_matrix(A,sx,sy,sz);
}

#ifdef DEBUG
/*
 * Create a matrix to apply rotation transformations
 */
static void create_rotate_matrix(A,axis,angle)
CNmatrix A;
int      axis;
double   angle;
{
   /* Initialize the identity matrix */
   create_identity_matrix(A);

   /* Fill in the appropriate matrix elements */
   if (axis == Z_AXIS) {
      A[0][0] =  cos(angle);
      A[0][1] =  sin(angle);
      A[1][0] = -sin(angle);
      A[1][1] =  cos(angle);
   } else if (axis == Y_AXIS) {
      A[0][0] =  cos(angle);
      A[0][2] = -sin(angle);
      A[2][0] =  sin(angle);
      A[2][2] =  cos(angle);
   } else if (axis == X_AXIS) {
      A[1][1] =  cos(angle);
      A[1][2] =  sin(angle);
      A[2][1] = -sin(angle);
      A[2][2] =  cos(angle);
   } else {
      (void) fprintf(stderr,"Error: Unrecognized axis-type!\n");
      (void) fprintf(stderr,"No rotation will be applied\n");
   }
}
#endif

/*
 * Create a matrix to apply translate transformations
 */
static void create_translate_matrix(A,tx,ty,tz)
CNmatrix A;
double   tx, ty, tz;
{
   /* Initialize the identity matrix */
   create_identity_matrix(A);

   /* Fill in the appropriate matrix elements */
   A[3][0] = tx;
   A[3][1] = ty;
   A[3][2] = tz;
}

/*
 * Create a matrix to apply perspective transformations
 */
static void create_perspective_matrix(A,d)
CNmatrix A;
double   d;
{
   /*
    * d is the distance from the origin 
    * if it is zero then make it a small number 
    */
   if (fabs(d) < SMALL) d = SMALL;

   /* Initialize the identity matrix */
   create_identity_matrix(A);

   /* Fill in the appropriate matrix elements */
   A[2][2] = 1/d;
   A[2][3] = 1/d;
}


/*************************/
/*** MATRIX OPERATIONS ***/
/*************************/

/*
 * Multiply 2 matrices : A = B * C
 */
static void mult_matrix(A,B,C)
CNmatrix A,B,C;
{
   CNmatrix D;
   int      i,j,k;

   /*
    *  There is a possibility that A could be the same address as
    * B or C.  Thus multiply first into a temp matrix and then copy
    * the result into A.
    */
   /* Initialize resultant matrix */
   create_zero_matrix(D);

   /* Multiply the matrices */
   for (i=0; i<CN_NDIM; i++) 
      for (j=0; j<CN_NDIM; j++) 
         for (k=0; k<CN_NDIM; k++) 
            D[i][j] += B[i][k]*C[k][j];

   /* Copy D into A */
   CNcopy_matrix(A,D);
}

/*
 * Copy Matrix B into A 
 */
void CNcopy_matrix(A,B)
CNmatrix A,B;
{
   int i,j;

   for (i=0; i<CN_NDIM; i++) 
      for (j=0; j<CN_NDIM; j++) 
         A[i][j] = B[i][j];
}

/* 
 * Print out the matrix elements 
 */
static void print_matrix(A)
CNmatrix A;
{
   int i,j;

   for (i=0; i<CN_NDIM; i++) {
      (void) fprintf(stdout,"\n");
      for (j=0; j<CN_NDIM; j++) 
         (void) fprintf(stdout,"%10.5f ",A[i][j]);
   }
   (void) fprintf(stdout,"\n");
}


/************************/
/*** POINT OPERATIONS ***/
/************************/

/*
 * Transform a point using a 4x4 transformation matrix
 */
CNcoord  CNtransform_point(P,T)
CNcoord   *P;
CNmatrix T;
{
   CNcoord  newpoint;
   double  a[CN_NDIM];
   int     j;

   for (j=0; j<CN_NDIM; j++)
      a[j] = P->x * T[0][j] +
             P->y * T[1][j] +
             P->z * T[2][j] + T[3][j];
   newpoint.x = a[0]/a[3];
   newpoint.y = a[1]/a[3];
   newpoint.z = a[2]/a[3];
   return(newpoint);
}
