/*
 * mesh4Ddata.c - manipulate the mesh4D grid data 
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <malloc.h>
#include "CNplot.h"

static double kx_index();
static double ky_index();
static double kz_index();
static int cube_is_air();
static int cube_has_air();
static int cube_touches_air();

static int slice_mesh4D_x();
static int slice_mesh4D_y();
static int slice_mesh4D_z();

static void create_tria();
static void create_rect();

/*
 * The volumetric grid data is originally in a 1D array
 *   [ [0,0,0]...[i,0,0]...[nx,0,0], [0,j,0]...[i,j,0]...[nx,ny,0] ]
 *   [ [0,0,1]...[i,0,1]...[nx,0,1], [0,j,1]...[i,j,1]...[nx,ny,1] ]
 *
 *   [ [0,0,nz]...[i,0,nz]...[nx,0,nz], [0,j,nz]...[i,j,nz]...[nx,ny,nz] ]
 * Save the arrays in a mesh4D grid data-structure 
 */
CNdatasetptr
CNcreate_mesh4D_parent(filename, dataname,
                       xgrid_arr, ygrid_arr, zgrid_arr,
                       mat1_arr, mat2_arr, prism_arr,
                       nx, ny, nz, 
                       ID)
char      *filename;          /* Name of original file */
char      *dataname;          /* Descriptive label     */
double    *xgrid_arr;         /* 1D data array x[i]    */
double    *ygrid_arr;         /* 1D data array y[j]    */
double    *zgrid_arr;         /* 1D data array z[k]    */
double    *mat1_arr;          /* Mat1 array (optional) */
double    *mat2_arr;          /* Mat2 array (optional) */
double    *prism_arr;         /* Prism array (optional)*/
int       nx, ny, nz;         /* #x, y and z gridpoints*/
int       ID;                 /* Data ID               */
{
   CNmesh4Dptr   mesh4D;
   CNdatasetptr  dptr;
   double        xmin, xmax, ymin, ymax, zmin, zmax;
   int           i;

   /* Check the arrays */
   if (xgrid_arr == NULL) {
      (void) fprintf(stderr,"Error! NULL x-array!\n");
      return(NULL);
   }
   if (ygrid_arr == NULL) {
      (void) fprintf(stderr,"Error! NULL y-array!\n");
      return(NULL);
   }
   if (zgrid_arr == NULL) {
      (void) fprintf(stderr,"Error! NULL z-array!\n");
      return(NULL);
   }

   /* Search for the max and min */
   xmin =  CN_LARGE;
   xmax = -CN_LARGE;
   for (i=0; i<nx; i++) {
      if (xgrid_arr[i] < xmin) xmin = xgrid_arr[i];
      if (xgrid_arr[i] > xmax) xmax = xgrid_arr[i];
   }
   ymin =  CN_LARGE;
   ymax = -CN_LARGE;
   for (i=0; i<ny; i++) {
      if (ygrid_arr[i] < ymin) ymin = ygrid_arr[i];
      if (ygrid_arr[i] > ymax) ymax = ygrid_arr[i];
   }
   zmin =  CN_LARGE;
   zmax = -CN_LARGE;
   for (i=0; i<nz; i++) {
      if (zgrid_arr[i] < zmin) zmin = zgrid_arr[i];
      if (zgrid_arr[i] > zmax) zmax = zgrid_arr[i];
   }

   /*
    * Create and initialize a data structure to hold this contour dataset.
    * The boundary is the same as the viewport.
    */
   if ((dptr = CNmake_dataset(filename,dataname,CN_MESH4D_P,
                              xmin,xmax,ymin,ymax,zmin,zmax,
                              xmin,xmax,ymin,ymax,zmin,zmax,ID))==NULL)
      return(dptr);

   /* 
    * Create a mesh4D grid structure 
    */
   mesh4D = CNmake_mesh4D(0);
   if (mesh4D == NULL) {
      /* Free the dataset */

      /* Free the char strings */
      CNdestroy_string(dptr->filename);
      CNdestroy_string(dptr->label);

      /* Delete the view */
      CNdelete_view(dptr->view_pr);

      /* Now delete dptr */
      free ((char*)dptr);

   } else {
      /* Fill in the dataset */
      dptr->mesh4D        = mesh4D;
      mesh4D->xarray      = xgrid_arr;
      mesh4D->yarray      = ygrid_arr;
      mesh4D->zarray      = zgrid_arr;
      mesh4D->mat1_array  = mat1_arr;
      mesh4D->mat2_array  = mat2_arr;
      mesh4D->prism_array = prism_arr;
      mesh4D->nx          = nx;
      mesh4D->ny          = ny;
      mesh4D->nz          = nz;
      mesh4D->xmin        = xmin;
      mesh4D->xmax        = xmax;
      mesh4D->ymin        = ymin;
      mesh4D->ymax        = ymax;
      mesh4D->zmin        = zmin;
      mesh4D->zmax        = zmax;
   }

   /*
    * Return the pointer
    */
   return(dptr);
}

/*
 * The volumetric grid data is originally in a 1D array
 *   [ [0,0,0]...[i,0,0]...[nx,0,0], [0,j,0]...[i,j,0]...[nx,ny,0] ]
 *   [ [0,0,1]...[i,0,1]...[nx,0,1], [0,j,1]...[i,j,1]...[nx,ny,1] ]
 *
 *   [ [0,0,nz]...[i,0,nz]...[nx,0,nz], [0,j,nz]...[i,j,nz]...[nx,ny,nz] ]
 * Save the arrays in a mesh4D grid data-structure 
 */
CNdatasetptr
CNcreate_mesh4D_child(filename, dataname,
                      Gparent, qgrid_arr, nq,
                      ID)
char      *filename;          /* Name of original file */
char      *dataname;          /* Descriptive label     */
CNmesh4Dptr Gparent;          /* Coordinate data       */
double    *qgrid_arr;         /* 1D data array q(i,j,k)*/
int       nq;                 /* #t gridpoints         */
int       ID;                 /* Data ID               */
{
   CNmesh4Dptr   mesh4D;
   CNdatasetptr  dptr;
   double        xmin, xmax, ymin, ymax, zmin, zmax, qmin, qmax;
   int           i;

   /* Check the arrays */
   if (Gparent == NULL) {
      (void) fprintf(stderr,"Error! NULL mesh4D parent!\n");
      return(NULL);
   }
   if (qgrid_arr == NULL) {
      (void) fprintf(stderr,"Error! NULL q-array!\n");
      return(NULL);
   }

   /* Check the dimensions */
   if (nq !=  (Gparent->nx * Gparent->ny * Gparent->nz) ) {
      (void) fprintf(stderr,"Error! Invalid q-array dimensions!\n");
      return(NULL);
   }

   /* Search for the max and min */
   qmin =  CN_LARGE;
   qmax = -CN_LARGE;
   for (i=0; i<nq; i++) {
      if (qgrid_arr[i] < qmin) qmin = qgrid_arr[i];
      if (qgrid_arr[i] > qmax) qmax = qgrid_arr[i];
   }

   /*
    * Create and initialize a data structure to hold this contour dataset.
    * The boundary is the same as the viewport.
    */
   xmin = Gparent->xmin;
   xmax = Gparent->xmax;
   ymin = Gparent->ymin;
   ymax = Gparent->ymax;
   zmin = Gparent->zmin;
   zmax = Gparent->zmax;
   if ((dptr = CNmake_dataset(filename,dataname,CN_MESH4D_C,
                              xmin,xmax,ymin,ymax,zmin,zmax,
                              xmin,xmax,ymin,ymax,zmin,zmax,ID))==NULL)
      return(dptr);

 
   /* 
    * Create a mesh4D grid structure 
    */
   mesh4D = CNmake_mesh4D(0);
   if (mesh4D == NULL) {
      /* Free the dataset */
 
      /* Free the char strings */
      CNdestroy_string(dptr->filename);
      CNdestroy_string(dptr->label);
 
      /* Delete the view */
      CNdelete_view(dptr->view_pr);
 
      /* Now delete dptr */
      free ((char*)dptr);
 
   } else {
      /* Fill in the dataset */
      dptr->mesh4D   = mesh4D;
      mesh4D->qarray = qgrid_arr;
      mesh4D->nx     = Gparent->nx;
      mesh4D->ny     = Gparent->ny;
      mesh4D->nz     = Gparent->nz;
      mesh4D->nq     = nq;
      mesh4D->xmin   = xmin;
      mesh4D->xmax   = xmax;
      mesh4D->ymin   = ymin;
      mesh4D->ymax   = ymax;
      mesh4D->zmin   = zmin;
      mesh4D->zmax   = zmax;
      mesh4D->qmin   = qmin;
      mesh4D->qmax   = qmax;
 
      /* Set contour properties */
      dptr->data_pr.cmin = qmin;
      dptr->data_pr.cmax = qmax;
      dptr->data_pr.cstep= CNround_to_decimal((qmax-qmin)/(double)CN_IDLSTEPS);
   }
 
   /*
    * Return the pointer
    */
   return(dptr);
}


/*
 * SLICE Routines for MESH4D grids (prisms)
 */

/*
 * Find the exposed blocks/cubes 
 */
void CNslice_mesh4D(ggrid, gquant, 
                    blockhead, blocktail, cubehead, cubetail,
                    vxmin, vxmax, vymin, vymax, vzmin, vzmax,
                    xminin, xmaxin, yminin, ymaxin, zminin, zmaxin,
                    verbose)
CNmesh4Dptr ggrid;                    /* Grid structure        */
CNmesh4Dptr gquant;                   /* Attached quantity     */
CNblockptr  *blockhead, *blocktail;
CNcubeptr   *cubehead, *cubetail;
double      vxmin, vxmax;             /* View boundary         */
double      vymin, vymax;             /* View boundary         */
double      vzmin, vzmax;             /* View boundary         */
int         xminin, xmaxin;           /* x-face view flags     */
int         yminin, ymaxin;           /* y-face view flags     */
int         zminin, zmaxin;           /* z-face view flags     */
int         verbose;
{
   int         i, j, k;
   int         cbID=0, blID=0;
   int         imin, imax, jmin, jmax, kmin, kmax;
   short       mat1, mat2;
   int         pcode;
   int         exposed, isair, tmp, boundary;
   double      t;
   double      kxmin, kxmax, kymin, kymax, kzmin, kzmax;
   double      x0, x1, y0, y1, z0, z1;
   double      t000, t001, t010, t011, t100, t101, t110, t111;
   CNcubeptr   cube;
   
   /* 
    * Check the grid 
    */
   if (ggrid == NULL) {
      (void) fprintf(stderr,"Error! Cannot slice NULL grid!\n");
      return;
   }

   /*
    * Go thru the prisms and find the exposed faces 
    */
   
   /* Find the bracketing indexes, kmin corresponding to vzmin, etc */
   kxmin = kx_index(ggrid, vxmin); 
   kxmax = kx_index(ggrid, vxmax); 
   kymin = ky_index(ggrid, vymin); 
   kymax = ky_index(ggrid, vymax); 
   kzmin = kz_index(ggrid, vzmin); 
   kzmax = kz_index(ggrid, vzmax); 
   imin  = floor ( SMALLER_OF(kxmin,kxmax) );
   imax  = ceil  ( LARGER_OF (kxmin,kxmax) );
   jmin  = floor ( SMALLER_OF(kymin,kymax) );
   jmax  = ceil  ( LARGER_OF (kymin,kymax) );
   kmin  = floor ( SMALLER_OF(kzmin,kzmax) );
   kmax  = ceil  ( LARGER_OF (kzmin,kzmax) );

   /* Modify xminin, xmaxin flags */
   if (CNmesh4D_x(ggrid,imin) > CNmesh4D_x(ggrid,imin+1)) {
      tmp    = xminin;
      xminin = xmaxin;
      xmaxin = tmp;
   }
   if (CNmesh4D_y(ggrid,jmin) > CNmesh4D_y(ggrid,jmin+1)) {
      tmp    = yminin;
      yminin = ymaxin;
      ymaxin = tmp;
   }
   if (CNmesh4D_z(ggrid,kmin) > CNmesh4D_z(ggrid,kmin+1)) {
      tmp    = zminin;
      zminin = zmaxin;
      zmaxin = tmp;
   }

   /* Print information */
   if (verbose) {
      (void) fprintf(stdout,"Searching the grid at :\n");
      (void) fprintf(stdout,"   xmin=%g (kx=%g)...\n",vxmin,kxmin);
      (void) fprintf(stdout,"   xmax=%g (kx=%g)...\n",vxmax,kxmax);
      (void) fprintf(stdout,"   ymin=%g (ky=%g)...\n",vymin,kymin);
      (void) fprintf(stdout,"   ymax=%g (ky=%g)...\n",vymax,kymax);
      (void) fprintf(stdout,"   zmin=%g (kz=%g)...\n",vzmin,kzmin);
      (void) fprintf(stdout,"   zmax=%g (kz=%g)...\n",vzmax,kzmax);
   }

   /* Go thru the prisms/cubes and search for exposed faces */
   t000 = 0.0;
   t001 = 0.0;
   t010 = 0.0;
   t011 = 0.0;
   t100 = 0.0;
   t101 = 0.0;
   t110 = 0.0;
   t111 = 0.0;
   for (i=imin; i<imax; i++) 
   for (j=jmin; j<jmax; j++) 
   for (k=kmin; k<kmax; k++) {

      /* Don't draw the cube if it is air */
      isair   = cube_is_air(ggrid, i, j, k);
      if (isair) continue;

      /* Check to see if the cube touches air */
      exposed = cube_touches_air(ggrid, i, j, k);

      /* Find out if the cube is on the boundary and is visible */
      boundary = CN_FALSE;
      if ((i==imin && !xminin) || (i==imax-1 && !xmaxin) ||
          (j==jmin && !yminin) || (j==jmax-1 && !ymaxin) ||
          (k==kmin && !zminin) || (k==kmax-1 && !zmaxin) )
         boundary = CN_TRUE;

      /* The surface is exposed */
      if (!isair && (boundary || exposed)) {

         /* The bounds of the cube */
         x0   = CNmesh4D_x(ggrid,i);
         x1   = CNmesh4D_x(ggrid,i+1);
         y0   = CNmesh4D_y(ggrid,j);
         y1   = CNmesh4D_y(ggrid,j+1);
         z0   = CNmesh4D_z(ggrid,k);
         z1   = CNmesh4D_z(ggrid,k+1);
         if (gquant != NULL) {
            t000 = CNmesh4D_q(gquant,i  , j  , k  );
            t001 = CNmesh4D_q(gquant,i  , j  , k+1);
            t010 = CNmesh4D_q(gquant,i  , j+1, k  );
            t011 = CNmesh4D_q(gquant,i  , j+1, k+1);
            t100 = CNmesh4D_q(gquant,i+1, j  , k  );
            t101 = CNmesh4D_q(gquant,i+1, j  , k+1);
            t110 = CNmesh4D_q(gquant,i+1, j+1, k  );
            t111 = CNmesh4D_q(gquant,i+1, j+1, k+1);
         }

         /* 
          * Check bounds and clip as necessary 
          */

         /* Clip against vxmin */
         if (x0 < vxmin && vxmin < x1) {
            t    = (vxmin - x0)/(x1 - x0);
            t000 = t000 + t*(t100 - t000);
            t010 = t010 + t*(t110 - t010);
            t011 = t011 + t*(t111 - t011);
            t001 = t001 + t*(t101 - t001);
            x0   = vxmin;
         } else if (x0 > vxmin && vxmin > x1) {
            t    = (vxmin - x0)/(x1 - x0);
            t100 = t000 + t*(t100 - t000);
            t110 = t010 + t*(t110 - t010);
            t111 = t011 + t*(t111 - t011);
            t101 = t001 + t*(t101 - t001);
            x1   = vxmin;
         }

         /* Clip against vxmax */
         if (x0 < vxmax && vxmax < x1) {
            t    = (vxmax - x0)/(x1 - x0);
            t100 = t000 + t*(t100 - t000);
            t110 = t010 + t*(t110 - t010);
            t111 = t011 + t*(t111 - t011);
            t101 = t001 + t*(t101 - t001);
            x1   = vxmax;
         } else if (x0 > vxmax && vxmax > x1) {
            t    = (vxmax - x0)/(x1 - x0);
            t000 = t000 + t*(t100 - t000);
            t010 = t010 + t*(t110 - t010);
            t011 = t011 + t*(t111 - t011);
            t001 = t001 + t*(t101 - t001);
            x0   = vxmax;
         }

         /* Clip against vymin */
         if (y0 < vymin && vymin < y1) {
            t    = (vymin - y0)/(y1 - y0);
            t000 = t000 + t*(t010 - t000);
            t100 = t100 + t*(t110 - t100);
            t101 = t101 + t*(t111 - t101);
            t001 = t001 + t*(t011 - t001);
            y0   = vymin;
         } else if (y0 > vymin && vymin > y1) {
            t    = (vymin - y0)/(y1 - y0);
            t010 = t000 + t*(t010 - t000);
            t110 = t100 + t*(t110 - t100);
            t111 = t101 + t*(t111 - t101);
            t011 = t001 + t*(t011 - t001);
            y1   = vymin;
         }

         /* Clip against vymax */
         if (y0 < vymax && vymax < y1) {
            t    = (vymax - y0)/(y1 - y0);
            t010 = t000 + t*(t010 - t000);
            t110 = t100 + t*(t110 - t100);
            t111 = t101 + t*(t111 - t101);
            t011 = t001 + t*(t011 - t001);
            y1   = vymax;
         } else if (y0 > vymax && vymax > y1) {
            t    = (vymax - y0)/(y1 - y0);
            t000 = t000 + t*(t010 - t000);
            t100 = t100 + t*(t110 - t100);
            t101 = t101 + t*(t111 - t101);
            t001 = t001 + t*(t011 - t001);
            y0   = vymax;
         }

         /* Clip against vzmin */
         if (z0 < vzmin && vzmin < z1) {
            t    = (vzmin - z0)/(z1 - z0);
            t000 = t000 + t*(t001 - t000);
            t100 = t100 + t*(t101 - t100);
            t110 = t110 + t*(t111 - t110);
            t010 = t010 + t*(t011 - t010);
            z0   = vzmin;
         } else if (z0 > vzmin && vzmin > z1) {
            t    = (vzmin - z0)/(z1 - z0);
            t001 = t000 + t*(t001 - t000);
            t101 = t100 + t*(t101 - t100);
            t111 = t110 + t*(t111 - t110);
            t011 = t010 + t*(t011 - t010);
            z1   = vzmin;
         }

         /* Clip against vzmax */
         if (z0 < vzmax && vzmax < z1) {
            t    = (vzmax - z0)/(z1 - z0);
            t001 = t000 + t*(t001 - t000);
            t101 = t100 + t*(t101 - t100);
            t111 = t110 + t*(t111 - t110);
            t011 = t010 + t*(t011 - t010);
            z1   = vzmax;
         } else if (z0 > vzmax && vzmax > z1) {
            t    = (vzmax - z0)/(z1 - z0);
            t000 = t000 + t*(t001 - t000);
            t100 = t100 + t*(t101 - t100);
            t110 = t110 + t*(t111 - t110);
            t010 = t010 + t*(t011 - t010);
            z0   = vzmax;
         }

         /* 
          * Create a list of cubes and faces
          */
         mat1  = (short) CNmesh4D_m1(ggrid,i,j,k);
         mat2  = (short) CNmesh4D_m2(ggrid,i,j,k);
         pcode = (int)   CNmesh4D_mp(ggrid,i,j,k);
         cube  = CNinsert_cube(cubehead, cubetail, 
                               x0, x1, y0, y1, z0, z1, 
                               mat1, mat2, pcode, cbID++);
         (void)  CNinsert_block(blockhead, blocktail,
                                cube,
                                t000, t001, t010, t011, t100, t101, t110, t111,
                                blID++);

      }
   }
   
   if (verbose) {
      (void) fprintf(stdout,"Grid was successfully partitioned\n");
      (void) fprintf(stdout,
                     "Found %d blocks %d cubes\n",
                     CNcount_blocks(*blockhead, *blocktail), 
                     CNcount_cubes(*cubehead, *cubetail)); 
   }
}


/*
 * Search for the bracketing index kx, which corresponds to
 * the x real-world coordinate.
 *     [0 .. nx-1]  .... [grid_xmin .. grid_xmax]
 *          ^                         ^
 *         kx                         x
 * This assumes that the grid array is sequentially ordered
 */
static double kx_index(Gptr, x)
CNmesh4Dptr Gptr;                     /* Grid structure        */
double      x;
{
   int    FOUND;
   int    k;
   double kx, x0, x1;

   /* Find the bounds of the grid array first */
   x0 = CNmesh4D_x(Gptr,0);
   x1 = CNmesh4D_x(Gptr,Gptr->nx-1);

   /* Check to see if the requested x-val is outside the bounds */
   if (x < x0 && x < x1) {
      if (x0 < x1) kx = (double) 0;
      else         kx = (double) Gptr->nx-1;
      return(kx);
   }
   if (x > x0 && x > x1) {
      if (x0 < x1) kx = (double) Gptr->nx-1;
      else         kx = (double) 0;
      return(kx);
   }
      
   /* OK, the x value must be inside the bounds */
   FOUND = CN_FALSE;
   for (k=0; k<Gptr->nx-1 && !FOUND; k++) {
      x0 = CNmesh4D_x(Gptr,k);
      x1 = CNmesh4D_x(Gptr,k+1);
      if ((x >= x0 && x <= x1) || (x >= x1 && x <= x0)) {
         FOUND = CN_TRUE;
         kx    = (double)k + (x - x0)/(x1-x0);
      }
   }

   /* If there was no match, print an error */
   if (!FOUND) {
      (void) fprintf(stderr,"Error! Couldn't find x=%g in the grid!\n",x);
      (void) fprintf(stderr,"The grid x-array is not properly ordered!\n");
      kx = 0.0;
   }

   /* return */
   return(kx);
}


/*
 * Search for the bracketing index ky, which corresponds to
 * the y real-world coordinate.
 *     [0 .. ny-1]  .... [grid_ymin .. grid_ymax]
 *          ^                         ^
 *         ky                         y
 * This assumes that the grid array is sequentially ordered
 */
static double ky_index(Gptr, y)
CNmesh4Dptr Gptr;                     /* Grid structure        */
double      y;
{
   int    FOUND;
   int    k;
   double ky, y0, y1;

   /* Find the bounds of the grid array first */
   y0 = CNmesh4D_y(Gptr,0);
   y1 = CNmesh4D_y(Gptr,Gptr->ny-1);

   /* Check to see if the requested y-val is outside the bounds */
   if (y < y0 && y < y1) {
      if (y0 < y1) ky = (double) 0;
      else         ky = (double) Gptr->ny-1;
      return(ky);
   }
   if (y > y0 && y > y1) {
      if (y0 < y1) ky = (double) Gptr->ny-1;
      else         ky = (double) 0;
      return(ky);
   }
      
   /* OK, the y value must be inside the bounds */
   FOUND = CN_FALSE;
   for (k=0; k<Gptr->ny-1 && !FOUND; k++) {
      y0 = CNmesh4D_y(Gptr,k);
      y1 = CNmesh4D_y(Gptr,k+1);
      if ((y >= y0 && y <= y1) || (y >= y1 && y <= y0)) {
         FOUND = CN_TRUE;
         ky    = (double)k + (y - y0)/(y1-y0);
      }
   }

   /* If there was no match, print an error */
   if (!FOUND) {
      (void) fprintf(stderr,"Error! Couldn't find y=%g in the grid!\n",y);
      (void) fprintf(stderr,"The grid y-array is not properly ordered!\n");
      ky = 0.0;
   }

   /* return */
   return(ky);
}


/*
 * Search for the bracketing index kz, which corresponds to
 * the z real-world coordinate.
 *     [0 .. nz-1]  .... [grid_zmin .. grid_zmax]
 *          ^                         ^
 *         kz                         z
 * This assumes that the grid array is sequentially ordered
 */
static double kz_index(Gptr, z)
CNmesh4Dptr Gptr;                     /* Grid structure        */
double      z;
{
   int    FOUND;
   int    k;
   double kz, z0, z1;

   /* Find the bounds of the grid array first */
   z0 = CNmesh4D_z(Gptr,0);
   z1 = CNmesh4D_z(Gptr,Gptr->nz-1);

   /* Check to see if the requested z-val is outside the bounds */
   if (z < z0 && z < z1) {
      if (z0 < z1) kz = (double) 0;
      else         kz = (double) Gptr->nz-1;
      return(kz);
   }
   if (z > z0 && z > z1) {
      if (z0 < z1) kz = (double) Gptr->nz-1;
      else         kz = (double) 0;
      return(kz);
   }
      
   /* OK, the z value must be inside the bounds */
   FOUND = CN_FALSE;
   for (k=0; k<Gptr->nz-1 && !FOUND; k++) {
      z0 = CNmesh4D_z(Gptr,k);
      z1 = CNmesh4D_z(Gptr,k+1);
      if ((z >= z0 && z <= z1) || (z >= z1 && z <= z0)) {
         FOUND = CN_TRUE;
         kz    = (double)k + (z - z0)/(z1-z0);
      }
   }

   /* If there was no match, print an error */
   if (!FOUND) {
      (void) fprintf(stderr,"Error! Couldn't find z=%g in the grid!\n",z);
      (void) fprintf(stderr,"The grid z-array is not properly ordered!\n");
      kz = 0.0;
   }

   /* return */
   return(kz);
}

/* 
 * Check to see if the cube is completely air 
 * If both materials in the cube are air, then the cube is air
 */
static int cube_is_air(ggrid, i, j, k)
CNmesh4Dptr ggrid;
int         i,j,k;
{
   int isair   = CN_FALSE;
   int divided = CN_FALSE;
   short mat1, mat2;
   int   pcode;

   /* Check the 1st 3 bits first to see if the cube is divided */
   pcode = (int) CNmesh4D_mp(ggrid,i,j,k);
   if ( ((pcode & CN_XPRISM) != 0) ||
        ((pcode & CN_YPRISM) != 0) ||
        ((pcode & CN_ZPRISM) != 0) )
      divided = CN_TRUE;

   mat1 = (short) CNmesh4D_m1(ggrid,i,j,k);
   mat2 = (short) CNmesh4D_m2(ggrid,i,j,k);
   if (!divided || (mat1 == mat2))
      isair = CNmesh4D_mat_is_air(mat1, ggrid);
   else
      isair = CNmesh4D_mat_is_air(mat1, ggrid) && 
              CNmesh4D_mat_is_air(mat2, ggrid);

   /* return */
   return(isair);
}

/* 
 * Check to see if the cube has an air component
 * If either material in the prism is air, then the cube has air
 * This check is used to determine exposure.
 */
static int cube_has_air(ggrid, i, j, k)
CNmesh4Dptr ggrid;
int         i,j,k;
{
   int isair   = CN_FALSE;
   int divided = CN_FALSE;
   short mat1, mat2;
   int   pcode;

   /* Check the 1st 3 bits first to see if the cube is divided */
   pcode = (int) CNmesh4D_mp(ggrid,i,j,k);
   if ( ((pcode & CN_XPRISM) != 0) ||
        ((pcode & CN_YPRISM) != 0) ||
        ((pcode & CN_ZPRISM) != 0) )
      divided = CN_TRUE;

   mat1 = (short) CNmesh4D_m1(ggrid,i,j,k);
   mat2 = (short) CNmesh4D_m2(ggrid,i,j,k);
   if (!divided || (mat1 == mat2))
      isair = CNmesh4D_mat_is_air(mat1, ggrid);
   else
      isair = CNmesh4D_mat_is_air(mat1, ggrid) || 
              CNmesh4D_mat_is_air(mat2, ggrid);

   /* return */
   return(isair);
}

/* 
 * Check to see if the cube touches air 
 */
static int cube_touches_air(ggrid, i, j, k)
CNmesh4Dptr ggrid;
int         i,j,k;
{
   int exposed = CN_FALSE;

   /* Check neighboring cube */
   if (!exposed && i-1>=0) {
      exposed = cube_has_air(ggrid,i-1,j,k);
   }

   /* Check neighboring cube */
   if (!exposed && i+1<ggrid->nx-1) {
      exposed = cube_has_air(ggrid,i+1,j,k);
   }

   /* Check neighboring cube */
   if (!exposed && j-1>=0) {
      exposed = cube_has_air(ggrid,i,j-1,k);
   }

   /* Check neighboring cube */
   if (!exposed && j+1<ggrid->ny-1) {
      exposed = cube_has_air(ggrid,i,j+1,k);
   }

   /* Check neighboring cube */
   if (!exposed && k-1>=0) {
      exposed = cube_has_air(ggrid,i,j,k-1);
   }

   /* Check neighboring cube */
   if (!exposed && k+1<ggrid->nz-1) {
      exposed = cube_has_air(ggrid,i,j,k+1);
   }

   /* return */
   return(exposed);
}


/*
 * Given a material and the list of regions, determine if
 * the material corresponds to "air"
 */
int CNmesh4D_mat_is_air(mat, ggrid)
int         mat;
CNmesh4Dptr ggrid;
{
   CNregionptr R;
   int isair = CN_FALSE;
   int FOUND = CN_FALSE;

   /* Search thru the regions and find the matching ID */
   for (R=ggrid->regionhead; R!=NULL && !FOUND; R=R->next) {
      if (R->ID == mat) {
         FOUND = CN_TRUE;
         isair = R->isair;
      }
   }

   /* Return result */
   return(isair);
}

/*
 * Given a material and a list of regions, determine
 * return the various plot flags (fillcolor, nocont, noplot)
 */
/*ARGSUSED*/
void CNmesh4D_mat_options(mat, regionhead, regiontail, nocont, noplot, color)
int mat;
CNregionptr regionhead, regiontail;
int *nocont;
int *noplot;
int *color;
{
   CNregionptr R;
   int FOUND = CN_FALSE;

   /* Initialize defaults */
   *noplot = CN_FALSE;
   *nocont = CN_FALSE;
   *color  = 0;

   /* Search thru the regions and find the matching ID */
   for (R=regionhead; R!=NULL && !FOUND; R=R->next) {
      if (R->ID == mat) {
         FOUND   = CN_TRUE;
         *nocont = R->nocont;
         *noplot = R->isair;
         *color  = R->color;
      }
   }
}



/*
 * 2D SLICE
 */

/*
 * Given a 4D mesh dataset, slice in x, y or z and create a new dataset
 * noflip is used primarily for debugging
 */
CNdatasetptr CNslice_mesh4D_dataset(dptr, slice_plane, slice_value,
                                    xlabel, ylabel, zlabel,
                                    noflip, dataID, verbose)
CNdatasetptr dptr;
int          slice_plane;
double       slice_value;
char         *xlabel, *ylabel, *zlabel;   /* X-Y-Z labels */
int          noflip;                      /* Don't flip to xy plane */
int          *dataID;
int          verbose;
{
   CNdatasetptr Dslice=NULL, dparent=NULL;
   CNmesh4Dptr  mesh4D_grid, mesh4D_quant;
   CNpointptr   P;
   CNregionptr  R, Rnew;
   CNpolyptr    polyhead=NULL,  polytail=NULL;    /* List of polygons   */
   CNnodeptr    nodehead=NULL,  nodetail=NULL;    /* List of nodes      */
   CNpointptr   pointhead=NULL, pointtail=NULL;   /* List of points     */
   double       xmin, xmax, ymin, ymax, zmin, zmax, tmin, tmax;
   char         label[CN_MAXCHAR];
   char         x_label[CN_MAXCHAR];
   char         y_label[CN_MAXCHAR];
   char         z_label[CN_MAXCHAR];
   int          status;

   /* Error check */
   if (dptr == NULL) {
      (void) fprintf(stderr,"Error! NULL dataset!\n");
      return(NULL);
   }
   if (dptr->mesh4D == NULL) {
      (void) fprintf(stderr,"Error! NULL mesh4D structure!\n");
      return(NULL);
   }
   if (dptr->datatype != CN_MESH4D_P && dptr->datatype != CN_MESH4D_C) {
      (void) fprintf(stderr,"Error! Cannot slice %s dataset\n",
                     CNdatatype(dptr->datatype));
      return(NULL);
   }
   if (dptr->datatype == CN_MESH4D_C) {
      if (dptr->parent == NULL || dptr->parent->mesh4D == NULL) {
         (void) fprintf(stderr,"Error! Couldn't find valid mesh4D grid\n");
         return(NULL);
      }
      dparent      = dptr->parent;
      mesh4D_grid  = dptr->parent->mesh4D;
      mesh4D_quant = dptr->mesh4D;
   } else if (dptr->datatype == CN_MESH4D_P) {
      dparent      = dptr;
      mesh4D_grid  = dptr->mesh4D;
      mesh4D_quant = NULL;
   } else {
      return(NULL);
   }

   /* Initialize the axis labels */
   if (xlabel == NULL) (void) strcpy(x_label, "X-Axis");
   else                (void) strcpy(x_label, xlabel);
   if (ylabel == NULL) (void) strcpy(y_label, "Y-Axis");
   else                (void) strcpy(y_label, ylabel);
   if (zlabel == NULL) (void) strcpy(z_label, "Z-Axis");
   else                (void) strcpy(z_label, zlabel);

   /* Do the slice */
   status = 0;
   if (slice_plane == CN_XSLICE) {
      /* Slice */
      status = slice_mesh4D_x(mesh4D_grid, mesh4D_quant, slice_value,
                              &polyhead, &polytail,
                              &nodehead, &nodetail,
                              &pointhead,&pointtail,
                              verbose);

      /* reflip to the correct x-plane if necessary */
      if (noflip) {
         for (P=pointhead; P!=NULL; P=P->next) {
            P->z = P->y;
            P->y = P->x;
            P->x = slice_value;
         }
      }

      /* Slice parameters */
      (void) sprintf(label,"'%s' Slice : val=%g", x_label, slice_value);
      if (noflip) {
         xmin = mesh4D_grid->xmin;
         xmax = mesh4D_grid->xmax;
         ymin = mesh4D_grid->ymin;
         ymax = mesh4D_grid->ymax;
         zmin = mesh4D_grid->zmin;
         zmax = mesh4D_grid->zmax;
      } else {
         xmin = mesh4D_grid->ymin;
         xmax = mesh4D_grid->ymax;
         ymin = mesh4D_grid->zmin;
         ymax = mesh4D_grid->zmax;
         if (mesh4D_quant != NULL) {
            zmin = mesh4D_quant->qmin;
            zmax = mesh4D_quant->qmax;
         } else {
            zmin = -0.5*(mesh4D_grid->zmax - mesh4D_grid->zmin);
            zmax =  0.5*(mesh4D_grid->zmax - mesh4D_grid->zmin);
         }
      }
   } else if (slice_plane == CN_YSLICE) {
      /* Slice in y */
      status = slice_mesh4D_y(mesh4D_grid, mesh4D_quant, slice_value,
                              &polyhead, &polytail,
                              &nodehead, &nodetail,
                              &pointhead,&pointtail,
                              verbose);

      /* reflip to the correct y-plane if necessary */
      if (noflip) {
         for (P=pointhead; P!=NULL; P=P->next) {
            P->z = P->y;
            P->y = slice_value;
         }
      }

      /* Slice parameters */
      (void) sprintf(label,"'%s' Slice : val=%g", y_label, slice_value);
      if (noflip) {
         xmin = mesh4D_grid->xmin;
         xmax = mesh4D_grid->xmax;
         ymin = mesh4D_grid->ymin;
         ymax = mesh4D_grid->ymax;
         zmin = mesh4D_grid->zmin;
         zmax = mesh4D_grid->zmax;
      } else {
         xmin = mesh4D_grid->xmin;
         xmax = mesh4D_grid->xmax;
         ymin = mesh4D_grid->zmin;
         ymax = mesh4D_grid->zmax;
         if (mesh4D_quant != NULL) {
            zmin = mesh4D_quant->qmin;
            zmax = mesh4D_quant->qmax;
         } else {
            zmin = -0.5*(mesh4D_grid->zmax - mesh4D_grid->zmin);
            zmax =  0.5*(mesh4D_grid->zmax - mesh4D_grid->zmin);
         }
      }
   } else {
      /* Slice in z */
      status = slice_mesh4D_z(mesh4D_grid, mesh4D_quant, slice_value,
                              &polyhead, &polytail,
                              &nodehead, &nodetail,
                              &pointhead,&pointtail,
                              verbose);

      /* reflip to the correct z-plane if necessary */
      if (noflip) {
         for (P=pointhead; P!=NULL; P=P->next) {
            P->z = slice_value;
         }
      }

      /* Slice parameters */
      (void) sprintf(label,"'%s' Slice : val=%g", z_label, slice_value);
      if (noflip) {
         xmin = mesh4D_grid->xmin;
         xmax = mesh4D_grid->xmax;
         ymin = mesh4D_grid->ymin;
         ymax = mesh4D_grid->ymax;
         zmin = mesh4D_grid->zmin;
         zmax = mesh4D_grid->zmax;
      } else {
         xmin = mesh4D_grid->xmin;
         xmax = mesh4D_grid->xmax;
         ymin = mesh4D_grid->ymin;
         ymax = mesh4D_grid->ymax;
         if (mesh4D_quant != NULL) {
            zmin = mesh4D_quant->qmin;
            zmax = mesh4D_quant->qmax;
         } else {
            zmin = -0.5*(mesh4D_grid->zmax - mesh4D_grid->zmin);
            zmax =  0.5*(mesh4D_grid->zmax - mesh4D_grid->zmin);
         }
      }
   }

   if (status == 0) {
      /* Error message */
      (void) fprintf(stderr,"Error! Couldn't slice dataset\n");
      /* Free the lists */
      CNdelete_point_list(&pointhead,&pointtail);
      CNdelete_node_list(&nodehead,&nodetail);
      CNdelete_poly_list(&polyhead,&polytail);
      return(NULL);
   }

   /* Now make a dataset */
   if ((Dslice = CNmake_dataset(label,label, CN_POLYGON,
                                xmin,xmax, ymin,ymax, zmin,zmax,
                                xmin,xmax, ymin,ymax, zmin,zmax,
                                *dataID))==NULL) {
      /* Free the lists */
      CNdelete_point_list(&pointhead,&pointtail);
      CNdelete_node_list(&nodehead,&nodetail);
      CNdelete_poly_list(&polyhead,&polytail);
      return(NULL);
   }

   /* Increment the data ID */
   (*dataID)++;

   /* Attach the points/nodes/trias/rects */
   Dslice->pointhead = pointhead;
   Dslice->pointtail = pointtail;
   Dslice->nodehead  = nodehead;
   Dslice->nodetail  = nodetail;
   Dslice->polyhead  = polyhead;
   Dslice->polytail  = polytail;

   /* Attach the parent */
   Dslice->parent    = dparent;

   /* Copy the regions */
   for (R=mesh4D_grid->regionhead; R!=NULL; R=R->next) {
      Rnew = CNinsert_region(&(Dslice->regionhead), &(Dslice->regiontail),
                             R->matname, R->matID, R->ID);
      if (Rnew != NULL) {
         Rnew->color  = R->color;
         Rnew->isair  = R->isair;
         Rnew->nocont = R->nocont;
      }
   }

   /* Copy some of the properties from the original dataset */
   CNset_dataset_property(&(Dslice->data_pr),&(dptr->data_pr));
   if (mesh4D_quant == NULL)
   (void) CNparse_dataset_property(&(Dslice->data_pr),
                                   "contfill","False",verbose);

   /* Copy the contour-step list from the grid to the slice */
   if (dptr->cstephead && (dptr->data_pr.stepmethod == CN_USERDEFN)){
      CNcopy_contstep_list(&(Dslice->cstephead), &(Dslice->csteptail),
                           dptr->cstephead,   dptr->csteptail);
   }

   /* Set the labels */
   (void) CNparse_plotset_property(&(Dslice->plot_pr),
                                   "toplabel",label,verbose);
   (void) CNparse_plotset_property(&(Dslice->plot_pr),
                                   "zlabel","T-Axis",verbose);
   if (slice_plane == CN_XSLICE) {
        (void) CNparse_plotset_property(&(Dslice->plot_pr),
                                        "xlabel",y_label,verbose);
        (void) CNparse_plotset_property(&(Dslice->plot_pr),
                                        "ylabel",z_label,verbose);
   } else if (slice_plane == CN_YSLICE) {
        (void) CNparse_plotset_property(&(Dslice->plot_pr),
                                        "xlabel",x_label,verbose);
        (void) CNparse_plotset_property(&(Dslice->plot_pr),
                                        "ylabel",z_label,verbose);
   } else {
        (void) CNparse_plotset_property(&(Dslice->plot_pr),
                                        "xlabel",x_label,verbose);
        (void) CNparse_plotset_property(&(Dslice->plot_pr),
                                        "ylabel",y_label,verbose);
   }

   /* Figure out the contour min/max */
   CNget_nodelist_tmaxmin(nodehead,nodetail,&tmin,&tmax);
   if (EQUAL(tmin,tmax)) {
      tmin = -0.5;
      tmax =  0.5;
   }

   /* Set the contour steps and limits */
   Dslice->data_pr.cmin  = tmin;
   Dslice->data_pr.cmax  = tmax;
   Dslice->data_pr.cstep = CNround_to_decimal((tmax-tmin)/(double)CN_IDLSTEPS);

   /* Slice the contours */
   CNslice_contours(Dslice,verbose);

   /* Return */
   return(Dslice);
}


/*
 * Slice a 4D mesh
 * return 0 upon failure, 1 if successful
 */
static int slice_mesh4D_x(ggrid, gquant, x, 
                          polyhead, polytail, 
                          nodehead, nodetail, 
                          pointhead,pointtail, 
                          verbose)
CNmesh4Dptr ggrid;                    /* Grid structure     */
CNmesh4Dptr gquant;                   /* Attached quantity  */
double      x;                        /* Slice value        */
CNpolyptr   *polyhead,  *polytail;    /* List of polygons   */
CNnodeptr   *nodehead,  *nodetail;    /* List of nodes      */
CNpointptr  *pointhead, *pointtail;   /* List of points     */
int         verbose;
{
   CNnodeptr  nd00, nd10, nd11, nd01, ndt0, ndt1, nd0t, nd1t;
   CNpointptr pt00, pt10, pt11, pt01, ptt0, ptt1, pt0t, pt1t;
   double     y00, z00, v00, y10, z10, v10;
   double     y11, z11, v11, y01, z01, v01;
   double     yt0, yt1;
   double     z0t, z1t;
   double     vt0, vt1, v0t, v1t;
   double     x0, x1, v0, v1, t;
   int        ptID=0, ndID=0, polyID=0;
   int        i, j, k, ix;
   int        mat1, mat2, pcode, bmat, tmat;
   int        divided;
   int        FOUND;

   /*
    * Check the grid
    */
   if (ggrid == NULL) {
      (void) fprintf(stderr,
                     "slice_mesh4D_x(): Error! Cannot slice NULL grid!\n");
      return(0);
   }

   /*
    * Now check the bounds
    */
   if ((x < ggrid->xmin) || (x > ggrid->xmax)) {
      if (verbose) {
      (void) fprintf(stdout,"x=%g is outside the x-bounds [%g, %g]\n",
                     x, ggrid->xmin, ggrid->xmax);
      }
      return(0);
   }

   /*
    * Find the bracketing index
    */
   FOUND = CN_FALSE;
   ix    = 0;
   for (i=0; i<ggrid->nx-1 && !FOUND; i++) {
      x0 = CNmesh4D_x(ggrid,i);
      x1 = CNmesh4D_x(ggrid,i+1);
      if ((x >= x0 && x <= x1) || (x >= x1 && x <= x0)) {
         FOUND = CN_TRUE;
         ix    = i;
         t     = (x - x0)/(x1-x0);
      }
   }

   /*
    * Return if not found
    */
   if (!FOUND) {
      (void) fprintf(stderr,"Error! Couldn't find x=%g in the grid!\n",x);
      (void) fprintf(stderr,"The grid x-array is not properly ordered!\n");
      return(0);
   }

   /*
    * Print information
    */
   if (verbose)
      (void) fprintf(stdout,"Slicing the grid at x=%g (ix=%d)...\n",x,ix);

   /*
    * Go thru the cubes and search for intersections
    */
   for (k=0; k<ggrid->nz-1; k++)
   for (j=0; j<ggrid->ny-1; j++) {
      /* Point 1 - Interpolate at (ix,j,k) and (ix+1,j,k) */
      y00= CNmesh4D_y(ggrid,j);
      z00= CNmesh4D_z(ggrid,k);
      if (gquant != NULL) {
         v0 = CNmesh4D_q(gquant,ix  ,j,k);
         v1 = CNmesh4D_q(gquant,ix+1,j,k);
         v00= v0 + t*(v1-v0);
      } else {
         v00= 0.0;
      }
 
      /* Point 2 - Interpolate at (ix,j+1,k) and (ix+1,j+1,k) */
      y10= CNmesh4D_y(ggrid,j+1);
      z10= CNmesh4D_z(ggrid,k);
      if (gquant != NULL) {
         v0 = CNmesh4D_q(gquant,ix  ,j+1,k);
         v1 = CNmesh4D_q(gquant,ix+1,j+1,k);
         v10= v0 + t*(v1-v0);
      } else {
         v10= 0.0;
      }
 
      /* Point 3 - Interpolate at (ix,j+1,k+1) and (ix+1,j+1,k+1) */
      y11= CNmesh4D_y(ggrid,j+1);
      z11= CNmesh4D_z(ggrid,k+1);
      if (gquant != NULL) {
         v0 = CNmesh4D_q(gquant,ix  ,j+1,k+1);
         v1 = CNmesh4D_q(gquant,ix+1,j+1,k+1);
         v11= v0 + t*(v1-v0);
      } else {
         v11= 0.0;
      }
 
      /* Point 4 - Interpolate at (ix,j,k+1) and (ix+1,j,k+1) */
      y01= CNmesh4D_y(ggrid,j);
      z01= CNmesh4D_z(ggrid,k+1);
      if (gquant != NULL) {
         v0 = CNmesh4D_q(gquant,ix  ,j,k+1);
         v1 = CNmesh4D_q(gquant,ix+1,j,k+1);
         v01= v0 + t*(v1-v0);
      } else {
         v01= 0.0;
      }
 
      /* Check to see if the grid is divided here */
      mat1  = CNmesh4D_m1(ggrid, ix, j, k);
      mat2  = CNmesh4D_m2(ggrid, ix, j, k);
      pcode = CNmesh4D_mp(ggrid, ix, j, k);
      /* Check 1st 3 bits */
      divided = CN_FALSE;
      if ( ((pcode & CN_XPRISM) != 0) ||
           ((pcode & CN_YPRISM) != 0) ||
           ((pcode & CN_ZPRISM) != 0) )
         divided = CN_TRUE;
      /* Check the materials just to make sure */
      if (divided)
         if (mat1 == mat2) divided = CN_FALSE;

      if (!divided) {

         /* If not divided, just save the rectangle */
         pt00 = CNinsert_point(pointhead,pointtail,y00,z00,v00,ptID++);
         pt10 = CNinsert_point(pointhead,pointtail,y10,z10,v10,ptID++);
         pt11 = CNinsert_point(pointhead,pointtail,y11,z11,v11,ptID++);
         pt01 = CNinsert_point(pointhead,pointtail,y01,z01,v01,ptID++);
         nd00 = CNinsert_tailnode(nodehead,nodetail,pt00,v00,ndID++);
         nd10 = CNinsert_tailnode(nodehead,nodetail,pt10,v10,ndID++);
         nd11 = CNinsert_tailnode(nodehead,nodetail,pt11,v11,ndID++);
         nd01 = CNinsert_tailnode(nodehead,nodetail,pt01,v01,ndID++);
         create_rect(polyhead,polytail,
                     nd00,nd10,nd11,nd01,mat1,polyID++);

      } else {

         /* Mat for bottom and top prism */
         if ((pcode & CN_MAT1ONMIN) != 0) {
            /* Bottom prism is mat1, top prism is mat2 */
            bmat = mat1;
            tmat = mat2;
         } else {
            /* Bottom prism is mat2, top prism is mat1 */
            bmat = mat2;
            tmat = mat1;
         }

         /* The slice will always contain these 4 points/nodes */
         pt00 = CNinsert_point(pointhead,pointtail,y00,z00,v00,ptID++);
         pt10 = CNinsert_point(pointhead,pointtail,y10,z10,v10,ptID++);
         pt11 = CNinsert_point(pointhead,pointtail,y11,z11,v11,ptID++);
         pt01 = CNinsert_point(pointhead,pointtail,y01,z01,v01,ptID++);
         nd00 = CNinsert_tailnode(nodehead,nodetail,pt00,v00,ndID++);
         nd10 = CNinsert_tailnode(nodehead,nodetail,pt10,v10,ndID++);
         nd11 = CNinsert_tailnode(nodehead,nodetail,pt11,v11,ndID++);
         nd01 = CNinsert_tailnode(nodehead,nodetail,pt01,v01,ndID++);

         if ((pcode & CN_XPRISM) != 0) {
            /* This is a slice parallel to the x-axis, so get 2 triangles */
            if ((pcode & CN_CUTONMIN) != 0) {
               /*
                *  z----
                *   |T/|
                *   |/B|
                *   x---y
                * cutline goes thru the x=xmin axis
                */
               create_tria(polyhead,polytail,
                           nd00,nd10,nd11,bmat,polyID++);
               create_tria(polyhead,polytail,
                           nd00,nd01,nd11,tmat,polyID++);
            } else {
               /*
                *  z----
                *   |\T|
                *   |B\|
                *   x---y
                * cutline does not go thru the x=xmin axis
                */
               create_tria(polyhead,polytail,
                           nd00,nd10,nd01,bmat,polyID++);
               create_tria(polyhead,polytail,
                           nd11,nd10,nd01,tmat,polyID++);
            }
         } else if ((pcode & CN_YPRISM) != 0) {
            /* The slice is parallel to y, so get 2 rectangles */
            if ((pcode & CN_CUTONMIN) != 0) {
               /*
                *  x----
                * ..|T/|.....
                *   |/B|
                *   y---z
                * cutline goes thru the y=ymin axis
                */
               z0t = z00 + t*(z01-z00);
               z1t = z10 + t*(z11-z10);
               v0t = v00 + t*(v01-v00);
               v1t = v10 + t*(v11-v10);
               pt0t = CNinsert_point(pointhead,pointtail,y00,z0t,v0t,ptID++);
               pt1t = CNinsert_point(pointhead,pointtail,y10,z1t,v1t,ptID++);
               nd0t = CNinsert_tailnode(nodehead,nodetail,pt0t,v0t,ndID++);
               nd1t = CNinsert_tailnode(nodehead,nodetail,pt1t,v1t,ndID++);
               create_rect(polyhead,polytail,
                           nd00,nd10,nd1t,nd0t,tmat,polyID++);
               create_rect(polyhead,polytail,
                           nd0t,nd1t,nd11,nd01,bmat,polyID++);
            } else {
               /*
                *  x----
                * ..|\T|.....
                *   |B\|
                *   y---z
                * cutline does not go thru the y=ymin axis
                */
               z0t = z01 + t*(z00-z01);
               z1t = z11 + t*(z10-z11);
               v0t = v01 + t*(v00-v01);
               v1t = v11 + t*(v10-v11);
               pt0t = CNinsert_point(pointhead,pointtail,y00,z0t,v0t,ptID++);
               pt1t = CNinsert_point(pointhead,pointtail,y10,z1t,v1t,ptID++);
               nd0t = CNinsert_tailnode(nodehead,nodetail,pt0t,v0t,ndID++);
               nd1t = CNinsert_tailnode(nodehead,nodetail,pt1t,v1t,ndID++);
               create_rect(polyhead,polytail,
                           nd00,nd10,nd1t,nd0t,bmat,polyID++);
               create_rect(polyhead,polytail,
                           nd0t,nd1t,nd11,nd01,tmat,polyID++);
            }
         } else if ((pcode & CN_ZPRISM) != 0) {
            /* The slice is parallel to z, so get 2 rectangles */
            if ((pcode & CN_CUTONMIN) != 0) {
               /*     .
                *  y----
                *   |T/|     
                *   |/B|
                *   z---x
                *     .
                * cutline goes thru the z=zmin axis
                */
               yt0 = y00 + t*(y10-y00);
               yt1 = y01 + t*(y11-y01);
               vt0 = v00 + t*(v10-v00);
               vt1 = v01 + t*(v11-v01);
               ptt0 = CNinsert_point(pointhead,pointtail,yt0,z00,vt0,ptID++);
               ptt1 = CNinsert_point(pointhead,pointtail,yt1,z01,vt1,ptID++);
               ndt0 = CNinsert_tailnode(nodehead,nodetail,ptt0,vt0,ndID++);
               ndt1 = CNinsert_tailnode(nodehead,nodetail,ptt1,vt1,ndID++);
               create_rect(polyhead,polytail,
                           nd00,ndt0,ndt1,nd01,bmat,polyID++);
               create_rect(polyhead,polytail,
                           ndt0,nd10,nd11,ndt1,tmat,polyID++);
            } else {
               /*     .
                *  y----
                *   |\T|     
                *   |B\|
                *   z---x
                *     .
                * cutline does not go thru the y=ymin axis
                */
               yt0 = y10 + t*(y00-y10);
               yt1 = y11 + t*(y01-y11);
               vt0 = v10 + t*(v00-v10);
               vt1 = v11 + t*(v01-v11);
               ptt0 = CNinsert_point(pointhead,pointtail,yt0,z00,vt0,ptID++);
               ptt1 = CNinsert_point(pointhead,pointtail,yt1,z01,vt1,ptID++);
               ndt0 = CNinsert_tailnode(nodehead,nodetail,ptt0,vt0,ndID++);
               ndt1 = CNinsert_tailnode(nodehead,nodetail,ptt1,vt1,ndID++);
               create_rect(polyhead,polytail,
                           nd00,ndt0,ndt1,nd01,bmat,polyID++);
               create_rect(polyhead,polytail,
                           ndt0,nd10,nd11,ndt1,tmat,polyID++);
            }
         }
      } /* if divided */
   }

   /* Error checking */
   if (ptID == 0) {
      (void) fprintf(stderr,"Error! 0 points!\n");
      return(0);
   }
   if (ndID == 0) {
      (void) fprintf(stderr,"Error! 0 nodes!\n");
      return(0);
   }
   if (polyID == 0) {
      (void) fprintf(stderr,"Error! 0 rects, 0 trias!\n");
      return(0);
   }

   if (verbose) {
      (void) fprintf(stdout,"Grid was successfully sliced at x=%g...",x);
      (void) fprintf(stdout,
                     "Found %d polygons %d nodes %d points\n",
                     CNcount_polys(*polyhead, *polytail),
                     CNcount_nodes(*nodehead, *nodetail),
                     CNcount_points(*pointhead, *pointtail));
   }

   /* return */
   return(1);
}


/*
 * Slice a 4D mesh
 * return 0 upon failure, 1 if successful
 */
static int slice_mesh4D_y(ggrid, gquant, y, 
                          polyhead, polytail, 
                          nodehead, nodetail, 
                          pointhead,pointtail, 
                          verbose)
CNmesh4Dptr ggrid;                    /* Grid structure     */
CNmesh4Dptr gquant;                   /* Attached quantity  */
double      y;                        /* Slice value        */
CNpolyptr   *polyhead,  *polytail;    /* List of polygons   */
CNnodeptr   *nodehead,  *nodetail;    /* List of nodes      */
CNpointptr  *pointhead, *pointtail;   /* List of points     */
int         verbose;
{
   CNnodeptr  nd00, nd10, nd11, nd01, ndt0, ndt1, nd0t, nd1t;
   CNpointptr pt00, pt10, pt11, pt01, ptt0, ptt1, pt0t, pt1t;
   double     x00, z00, v00, x10, z10, v10;
   double     x11, z11, v11, x01, z01, v01;
   double     xt0, xt1;
   double     z0t, z1t;
   double     vt0, vt1, v0t, v1t;
   double     y0, y1, v0, v1, t;
   int        ptID=0, ndID=0, polyID=0;
   int        i, j, k, jy;
   int        mat1, mat2, pcode, bmat, tmat;
   int        divided;
   int        FOUND;

   /*
    * Check the grid
    */
   if (ggrid == NULL) {
      (void) fprintf(stderr,
                     "slice_mesh4D_y(): Error! Cannot slice NULL grid!\n");
      return(0);
   }

   /*
    * Now check the bounds
    */
   if ((y < ggrid->ymin) || (y > ggrid->ymax)) {
      if (verbose) {
      (void) fprintf(stdout,"y=%g is outside the y-bounds [%g, %g]\n",
                     y, ggrid->ymin, ggrid->ymax);
      }
      return(0);
   }

   /*
    * Find the bracketing index
    */
   FOUND = CN_FALSE;
   jy    = 0;
   for (j=0; j<ggrid->ny-1 && !FOUND; j++) {
      y0 = CNmesh4D_y(ggrid,j);
      y1 = CNmesh4D_y(ggrid,j+1);
      if ((y >= y0 && y <= y1) || (y >= y1 && y <= y0)) {
         FOUND = CN_TRUE;
         jy    = j;
         t     = (y - y0)/(y1-y0);
      }
   }

   /*
    * Return if not found
    */
   if (!FOUND) {
      (void) fprintf(stderr,"Error! Couldn't find y=%g in the grid!\n",y);
      (void) fprintf(stderr,"The grid y-array is not properly ordered!\n");
      return(0);
   }

   /*
    * Print information
    */
   if (verbose)
      (void) fprintf(stdout,"Slicing the grid at y=%g (jy=%d)...\n",y,jy);

   /*
    * Go thru the cubes and search for intersections
    */
   for (k=0; k<ggrid->nz-1; k++)
   for (i=0; i<ggrid->nx-1; i++) {
      /* Point 1 - Interpolate at (i,jy,k) and (i,jy+1,k) */
      x00= CNmesh4D_x(ggrid,i);
      z00= CNmesh4D_z(ggrid,k);
      if (gquant != NULL) {
         v0 = CNmesh4D_q(gquant,i,jy  ,k);
         v1 = CNmesh4D_q(gquant,i,jy+1,k);
         v00= v0 + t*(v1-v0);
      } else {
         v00= 0.0;
      }
 
      /* Point 2 - Interpolate at (i+1,jy,k) and (i+1,jy+1,k) */
      x10= CNmesh4D_x(ggrid,i+1);
      z10= CNmesh4D_z(ggrid,k);
      if (gquant != NULL) {
         v0 = CNmesh4D_q(gquant,i+1,jy  ,k);
         v1 = CNmesh4D_q(gquant,i+1,jy+1,k);
         v10= v0 + t*(v1-v0);
      } else {
         v10= 0.0;
      }
 
      /* Point 3 - Interpolate at (i+1,jy,k+1) and (i+1,jy+1,k+1) */
      x11= CNmesh4D_x(ggrid,i+1);
      z11= CNmesh4D_z(ggrid,k+1);
      if (gquant != NULL) {
         v0 = CNmesh4D_q(gquant,i+1,jy  ,k+1);
         v1 = CNmesh4D_q(gquant,i+1,jy+1,k+1);
         v11= v0 + t*(v1-v0);
      } else {
         v11= 0.0;
      }
 
      /* Point 4 - Interpolate at (i,jy,k+1) and (i,jy+1,k+1) */
      x01= CNmesh4D_x(ggrid,i);
      z01= CNmesh4D_z(ggrid,k+1);
      if (gquant != NULL) {
         v0 = CNmesh4D_q(gquant,i,jy  ,k+1);
         v1 = CNmesh4D_q(gquant,i,jy+1,k+1);
         v01= v0 + t*(v1-v0);
      } else {
         v01= 0.0;
      }
 
      /* Check to see if the grid is divided here */
      mat1  = CNmesh4D_m1(ggrid, i, jy, k);
      mat2  = CNmesh4D_m2(ggrid, i, jy, k);
      pcode = CNmesh4D_mp(ggrid, i, jy, k);
      /* Check 1st 3 bits */
      divided = CN_FALSE;
      if ( ((pcode & CN_XPRISM) != 0) ||
           ((pcode & CN_YPRISM) != 0) ||
           ((pcode & CN_ZPRISM) != 0) )
         divided = CN_TRUE;
      /* Check the materials just to make sure */
      if (divided)
         if (mat1 == mat2) divided = CN_FALSE;

      if (!divided) {

         /* If not divided, just save the rectangle */
         pt00 = CNinsert_point(pointhead,pointtail,x00,z00,v00,ptID++);
         pt10 = CNinsert_point(pointhead,pointtail,x10,z10,v10,ptID++);
         pt11 = CNinsert_point(pointhead,pointtail,x11,z11,v11,ptID++);
         pt01 = CNinsert_point(pointhead,pointtail,x01,z01,v01,ptID++);
         nd00 = CNinsert_tailnode(nodehead,nodetail,pt00,v00,ndID++);
         nd10 = CNinsert_tailnode(nodehead,nodetail,pt10,v10,ndID++);
         nd11 = CNinsert_tailnode(nodehead,nodetail,pt11,v11,ndID++);
         nd01 = CNinsert_tailnode(nodehead,nodetail,pt01,v01,ndID++);
         create_rect(polyhead,polytail,
                     nd00,nd10,nd11,nd01,mat1,polyID++);

      } else {

         /* Mat for bottom and top prism */
         if ((pcode & CN_MAT1ONMIN) != 0) {
            /* Bottom prism is mat1, top prism is mat2 */
            bmat = mat1;
            tmat = mat2;
         } else {
            /* Bottom prism is mat2, top prism is mat1 */
            bmat = mat2;
            tmat = mat1;
         }

         /* The slice will always contain these 4 points/nodes */
         pt00 = CNinsert_point(pointhead,pointtail,x00,z00,v00,ptID++);
         pt10 = CNinsert_point(pointhead,pointtail,x10,z10,v10,ptID++);
         pt11 = CNinsert_point(pointhead,pointtail,x11,z11,v11,ptID++);
         pt01 = CNinsert_point(pointhead,pointtail,x01,z01,v01,ptID++);
         nd00 = CNinsert_tailnode(nodehead,nodetail,pt00,v00,ndID++);
         nd10 = CNinsert_tailnode(nodehead,nodetail,pt10,v10,ndID++);
         nd11 = CNinsert_tailnode(nodehead,nodetail,pt11,v11,ndID++);
         nd01 = CNinsert_tailnode(nodehead,nodetail,pt01,v01,ndID++);

         if ((pcode & CN_XPRISM) != 0) {
            /* This is a slice parallel to the x-axis, so get 2 rectangles */
            if ((pcode & CN_CUTONMIN) != 0) {
               /*     .
                *  z----
                *   |T/|
                *   |/B|
                *   x---y
                *     .
                * cutline goes thru the x=xmin axis
                */
               z0t = z00 + t*(z01-z00);
               z1t = z10 + t*(z11-z10);
               v0t = v00 + t*(v01-v00);
               v1t = v10 + t*(v11-v10);
               pt0t = CNinsert_point(pointhead,pointtail,x00,z0t,v0t,ptID++);
               pt1t = CNinsert_point(pointhead,pointtail,x10,z1t,v1t,ptID++);
               nd0t = CNinsert_tailnode(nodehead,nodetail,pt0t,v0t,ndID++);
               nd1t = CNinsert_tailnode(nodehead,nodetail,pt1t,v1t,ndID++);
               create_rect(polyhead,polytail,
                           nd00,nd10,nd1t,nd0t,bmat,polyID++);
               create_rect(polyhead,polytail,
                           nd0t,nd1t,nd11,nd01,tmat,polyID++);
            } else {
               /*     .
                *  z----
                *   |\T|
                *   |B\|
                *   x---y
                *     .
                * cutline does not go thru the x=xmin axis
                */
               z0t = z01 + t*(z00-z01);
               z1t = z11 + t*(z10-z11);
               v0t = v01 + t*(v00-v01);
               v1t = v11 + t*(v10-v11);
               pt0t = CNinsert_point(pointhead,pointtail,x00,z0t,v0t,ptID++);
               pt1t = CNinsert_point(pointhead,pointtail,x10,z1t,v1t,ptID++);
               nd0t = CNinsert_tailnode(nodehead,nodetail,pt0t,v0t,ndID++);
               nd1t = CNinsert_tailnode(nodehead,nodetail,pt1t,v1t,ndID++);
               create_rect(polyhead,polytail,
                           nd00,nd10,nd1t,nd0t,bmat,polyID++);
               create_rect(polyhead,polytail,
                           nd0t,nd1t,nd11,nd01,tmat,polyID++);
            }
         } else if ((pcode & CN_YPRISM) != 0) {
            /* The slice is parallel to y, so get 2 triangles */
            if ((pcode & CN_CUTONMIN) != 0) {
               /*
                *  x----
                *   |T/|
                *   |/B|
                *   y---z
                * cutline goes thru the y=ymin axis
                */
               create_tria(polyhead,polytail,
                           nd00,nd10,nd11,tmat,polyID++);
               create_tria(polyhead,polytail,
                           nd00,nd01,nd11,bmat,polyID++);
            } else {
               /*
                *  x----
                *   |\T|
                *   |B\|
                *   y---z
                * cutline does not go thru the y=ymin axis
                */
               create_tria(polyhead,polytail,
                           nd00,nd10,nd01,bmat,polyID++);
               create_tria(polyhead,polytail,
                           nd11,nd10,nd01,tmat,polyID++);
            }
         } else if ((pcode & CN_ZPRISM) != 0) {
            /* The slice is parallel to z, so get 2 rectangles */
            if ((pcode & CN_CUTONMIN) != 0) {
               /*     
                *  y----
                * ..|T/|.... 
                *   |/B|
                *   z---x
                * cutline goes thru the z=zmin axis
                */
               xt0 = x00 + t*(x10-x00);
               xt1 = x01 + t*(x11-x01);
               vt0 = v00 + t*(v10-v00);
               vt1 = v01 + t*(v11-v01);
               ptt0 = CNinsert_point(pointhead,pointtail,xt0,z00,vt0,ptID++);
               ptt1 = CNinsert_point(pointhead,pointtail,xt1,z01,vt1,ptID++);
               ndt0 = CNinsert_tailnode(nodehead,nodetail,ptt0,vt0,ndID++);
               ndt1 = CNinsert_tailnode(nodehead,nodetail,ptt1,vt1,ndID++);
               create_rect(polyhead,polytail,
                           nd00,ndt0,ndt1,nd01,tmat,polyID++);
               create_rect(polyhead,polytail,
                           ndt0,nd10,nd11,ndt1,bmat,polyID++);
            } else {
               /*      
                *  y----
                * ..|\T|.... 
                *   |B\|
                *   z---x
                * cutline does not go thru the y=ymin axis
                */
               xt0 = x10 + t*(x00-x10);
               xt1 = x11 + t*(x01-x11);
               vt0 = v10 + t*(v00-v10);
               vt1 = v11 + t*(v01-v11);
               ptt0 = CNinsert_point(pointhead,pointtail,xt0,z00,vt0,ptID++);
               ptt1 = CNinsert_point(pointhead,pointtail,xt1,z01,vt1,ptID++);
               ndt0 = CNinsert_tailnode(nodehead,nodetail,ptt0,vt0,ndID++);
               ndt1 = CNinsert_tailnode(nodehead,nodetail,ptt1,vt1,ndID++);
               create_rect(polyhead,polytail,
                           nd00,ndt0,ndt1,nd01,bmat,polyID++);
               create_rect(polyhead,polytail,
                           ndt0,nd10,nd11,ndt1,tmat,polyID++);
            }
         }
      } /* if divided */
   }

   /* Error checking */
   if (ptID == 0) {
      (void) fprintf(stderr,"Error! 0 points!\n");
      return(0);
   }
   if (ndID == 0) {
      (void) fprintf(stderr,"Error! 0 nodes!\n");
      return(0);
   }
   if (polyID == 0) {
      (void) fprintf(stderr,"Error! 0 rects, 0 trias!\n");
      return(0);
   }

   if (verbose) {
      (void) fprintf(stdout,"Grid was successfully sliced at y=%g...",y);
      (void) fprintf(stdout,
                     "Found %d polygons %d nodes %d points\n",
                     CNcount_polys(*polyhead, *polytail),
                     CNcount_nodes(*nodehead, *nodetail),
                     CNcount_points(*pointhead, *pointtail));
   }

   /* return */
   return(1);
}


/*
 * Slice a 4D mesh
 * return 0 upon failure, 1 if successful
 */
static int slice_mesh4D_z(ggrid, gquant, z, 
                          polyhead, polytail, 
                          nodehead, nodetail, 
                          pointhead,pointtail, 
                          verbose)
CNmesh4Dptr ggrid;                    /* Grid structure     */
CNmesh4Dptr gquant;                   /* Attached quantity  */
double      z;                        /* Slice value        */
CNpolyptr   *polyhead,  *polytail;    /* List of polygons   */
CNnodeptr   *nodehead,  *nodetail;    /* List of nodes      */
CNpointptr  *pointhead, *pointtail;   /* List of points     */
int         verbose;
{
   CNnodeptr  nd00, nd10, nd11, nd01, ndt0, ndt1, nd0t, nd1t;
   CNpointptr pt00, pt10, pt11, pt01, ptt0, ptt1, pt0t, pt1t;
   double     x00, y00, v00, x10, y10, v10;
   double     x11, y11, v11, x01, y01, v01;
   double     y0t, y1t;
   double     xt0, xt1;
   double     vt0, vt1, v0t, v1t;
   double     z0, z1, v0, v1, t;
   int        ptID=0, ndID=0, polyID=0;
   int        i, j, k, kz;
   int        mat1, mat2, pcode, bmat, tmat;
   int        divided;
   int        FOUND;

   /*
    * Check the grid
    */
   if (ggrid == NULL) {
      (void) fprintf(stderr,
                     "slice_mesh4D_z(): Error! Cannot slice NULL grid!\n");
      return(0);
   }

   /*
    * Now check the bounds
    */
   if ((z < ggrid->zmin) || (z > ggrid->zmax)) {
      if (verbose) {
      (void) fprintf(stdout,"z=%g is outside the z-bounds [%g, %g]\n",
                     z, ggrid->zmin, ggrid->zmax);
      }
      return(0);
   }

   /*
    * Find the bracketing index
    */
   FOUND = CN_FALSE;
   kz    = 0;
   for (k=0; k<ggrid->nz-1 && !FOUND; k++) {
      z0 = CNmesh4D_z(ggrid,k);
      z1 = CNmesh4D_z(ggrid,k+1);
      if ((z >= z0 && z <= z1) || (z >= z1 && z <= z0)) {
         FOUND = CN_TRUE;
         kz    = k;
         t     = (z - z0)/(z1-z0);
      }
   }

   /*
    * Return if not found
    */
   if (!FOUND) {
      (void) fprintf(stderr,"Error! Couldn't find z=%g in the grid!\n",z);
      (void) fprintf(stderr,"The grid z-array is not properly ordered!\n");
      return(0);
   }

   /*
    * Print information
    */
   if (verbose)
      (void) fprintf(stdout,"Slicing the grid at z=%g (kz=%d)...\n",z,kz);

   /*
    * Go thru the cubes and search for intersections
    */
   for (i=0; i<ggrid->nx-1; i++)
   for (j=0; j<ggrid->ny-1; j++) {
      /* Point 1 - Interpolate at (i,j,kz) and (i,j,kz+1) */
      x00= CNmesh4D_x(ggrid,i);
      y00= CNmesh4D_y(ggrid,j);
      if (gquant != NULL) {
         v0 = CNmesh4D_q(gquant,i,j,kz);
         v1 = CNmesh4D_q(gquant,i,j,kz+1);
         v00= v0 + t*(v1-v0);
      } else {
         v00= 0.0;
      }
 
      /* Point 2 - Interpolate at (i+1,j,kz) and (i+1,j,kz+1) */
      x10= CNmesh4D_x(ggrid,i+1);
      y10= CNmesh4D_y(ggrid,j);
      if (gquant != NULL) {
         v0 = CNmesh4D_q(gquant,i+1,j,kz);
         v1 = CNmesh4D_q(gquant,i+1,j,kz+1);
         v10= v0 + t*(v1-v0);
      } else {
         v10= 0.0;
      }
 
      /* Point 3 - Interpolate at (i+1,j+1,kz) and (i+1,j+1,kz+1) */
      x11= CNmesh4D_x(ggrid,i+1);
      y11= CNmesh4D_y(ggrid,j+1);
      if (gquant != NULL) {
         v0 = CNmesh4D_q(gquant,i+1,j+1,kz);
         v1 = CNmesh4D_q(gquant,i+1,j+1,kz+1);
         v11= v0 + t*(v1-v0);
      } else {
         v11= 0.0;
      }
 
      /* Point 4 - Interpolate at (i,j+1,kz) and (i,j+1,kz+1) */
      x01= CNmesh4D_x(ggrid,i);
      y01= CNmesh4D_y(ggrid,j+1);
      if (gquant != NULL) {
         v0 = CNmesh4D_q(gquant,i,j+1,kz);
         v1 = CNmesh4D_q(gquant,i,j+1,kz+1);
         v01= v0 + t*(v1-v0);
      } else {
         v01= 0.0;
      }
 
      /* Check to see if the grid is divided here */
      mat1  = CNmesh4D_m1(ggrid, i, j, kz);
      mat2  = CNmesh4D_m2(ggrid, i, j, kz);
      pcode = CNmesh4D_mp(ggrid, i, j, kz);
      /* Check 1st 3 bits */
      divided = CN_FALSE;
      if ( ((pcode & CN_XPRISM) != 0) ||
           ((pcode & CN_YPRISM) != 0) ||
           ((pcode & CN_ZPRISM) != 0) )
         divided = CN_TRUE;
      /* Check the materials just to make sure */
      if (divided)
         if (mat1 == mat2) divided = CN_FALSE;

      if (!divided) {

         /* If not divided, just save the rectangle */
         pt00 = CNinsert_point(pointhead,pointtail,x00,y00,v00,ptID++);
         pt10 = CNinsert_point(pointhead,pointtail,x10,y10,v10,ptID++);
         pt11 = CNinsert_point(pointhead,pointtail,x11,y11,v11,ptID++);
         pt01 = CNinsert_point(pointhead,pointtail,x01,y01,v01,ptID++);
         nd00 = CNinsert_tailnode(nodehead,nodetail,pt00,v00,ndID++);
         nd10 = CNinsert_tailnode(nodehead,nodetail,pt10,v10,ndID++);
         nd11 = CNinsert_tailnode(nodehead,nodetail,pt11,v11,ndID++);
         nd01 = CNinsert_tailnode(nodehead,nodetail,pt01,v01,ndID++);
         create_rect(polyhead,polytail,
                     nd00,nd10,nd11,nd01,mat1,polyID++);

      } else {

         /* Mat for bottom and top prism */
         if ((pcode & CN_MAT1ONMIN) != 0) {
            /* Bottom prism is mat1, top prism is mat2 */
            bmat = mat1;
            tmat = mat2;
         } else {
            /* Bottom prism is mat2, top prism is mat1 */
            bmat = mat2;
            tmat = mat1;
         }

         /* The slice will always contain these 4 points/nodes */
         pt00 = CNinsert_point(pointhead,pointtail,x00,y00,v00,ptID++);
         pt10 = CNinsert_point(pointhead,pointtail,x10,y10,v10,ptID++);
         pt11 = CNinsert_point(pointhead,pointtail,x11,y11,v11,ptID++);
         pt01 = CNinsert_point(pointhead,pointtail,x01,y01,v01,ptID++);
         nd00 = CNinsert_tailnode(nodehead,nodetail,pt00,v00,ndID++);
         nd10 = CNinsert_tailnode(nodehead,nodetail,pt10,v10,ndID++);
         nd11 = CNinsert_tailnode(nodehead,nodetail,pt11,v11,ndID++);
         nd01 = CNinsert_tailnode(nodehead,nodetail,pt01,v01,ndID++);

         if ((pcode & CN_XPRISM) != 0) {
            /* This is a slice parallel to the x-axis, so get 2 rectangles */
            if ((pcode & CN_CUTONMIN) != 0) {
               /*
                *  z----
                * ..|T/|....
                *   |/B|
                *   x---y
                * cutline goes thru the x=xmin axis
                */
               y0t = y00 + t*(y01-y00);
               y1t = y10 + t*(y11-y10);
               v0t = v00 + t*(v01-v00);
               v1t = v10 + t*(v11-v10);
               pt0t = CNinsert_point(pointhead,pointtail,x00,y0t,v0t,ptID++);
               pt1t = CNinsert_point(pointhead,pointtail,x10,y1t,v1t,ptID++);
               nd0t = CNinsert_tailnode(nodehead,nodetail,pt0t,v0t,ndID++);
               nd1t = CNinsert_tailnode(nodehead,nodetail,pt1t,v1t,ndID++);
               create_rect(polyhead,polytail,
                           nd00,nd10,nd1t,nd0t,tmat,polyID++);
               create_rect(polyhead,polytail,
                           nd0t,nd1t,nd11,nd01,bmat,polyID++);
            } else {
               /*
                *  z----
                * ..|\T|....
                *   |B\|
                *   x---y
                * cutline does not go thru the x=xmin axis
                */
               y0t = y01 + t*(y00-y01);
               y1t = y11 + t*(y10-y11);
               v0t = v01 + t*(v00-v01);
               v1t = v11 + t*(v10-v11);
               pt0t = CNinsert_point(pointhead,pointtail,x00,y0t,v0t,ptID++);
               pt1t = CNinsert_point(pointhead,pointtail,x10,y1t,v1t,ptID++);
               nd0t = CNinsert_tailnode(nodehead,nodetail,pt0t,v0t,ndID++);
               nd1t = CNinsert_tailnode(nodehead,nodetail,pt1t,v1t,ndID++);
               create_rect(polyhead,polytail,
                           nd00,nd10,nd1t,nd0t,bmat,polyID++);
               create_rect(polyhead,polytail,
                           nd0t,nd1t,nd11,nd01,tmat,polyID++);
            }
         } else if ((pcode & CN_YPRISM) != 0) {
            /* The slice is parallel to y, so get 2 rectangles */
            if ((pcode & CN_CUTONMIN) != 0) {
               /*     .
                *  x----
                *   |T/|
                *   |/B|
                *   y---z
                *     .
                * cutline goes thru the y=ymin axis
                */
               xt0 = x00 + t*(x10-x00);
               xt1 = x01 + t*(x11-x01);
               vt0 = v00 + t*(v10-v00);
               vt1 = v01 + t*(v11-v01);
               ptt0 = CNinsert_point(pointhead,pointtail,xt0,y00,vt0,ptID++);
               ptt1 = CNinsert_point(pointhead,pointtail,xt1,y01,vt1,ptID++);
               ndt0 = CNinsert_tailnode(nodehead,nodetail,ptt0,vt0,ndID++);
               ndt1 = CNinsert_tailnode(nodehead,nodetail,ptt1,vt1,ndID++);
               create_rect(polyhead,polytail,
                           nd00,ndt0,ndt1,nd01,bmat,polyID++);
               create_rect(polyhead,polytail,
                           ndt0,ndt1,nd11,nd10,tmat,polyID++);
            } else {
               /*     .
                *  x----
                *   |\T|
                *   |B\|
                *   y---z
                *     .
                * cutline does not go thru the y=ymin axis
                */
               xt0 = x10 + t*(x00-x10);
               xt1 = x11 + t*(x01-x11);
               vt0 = v10 + t*(v00-v10);
               vt1 = v11 + t*(v01-v11);
               ptt0 = CNinsert_point(pointhead,pointtail,xt0,y00,vt0,ptID++);
               ptt1 = CNinsert_point(pointhead,pointtail,xt1,y01,vt1,ptID++);
               ndt0 = CNinsert_tailnode(nodehead,nodetail,ptt0,vt0,ndID++);
               ndt1 = CNinsert_tailnode(nodehead,nodetail,ptt1,vt1,ndID++);
               create_rect(polyhead,polytail,
                           nd00,ndt0,ndt1,nd01,bmat,polyID++);
               create_rect(polyhead,polytail,
                           ndt0,ndt1,nd11,nd10,tmat,polyID++);
            }
         } else if ((pcode & CN_ZPRISM) != 0) {
            /* The slice is parallel to z, so get 2 triangles */
            if ((pcode & CN_CUTONMIN) != 0) {
               /*     
                *  y----
                *   |T/|     
                *   |/B|
                *   z---x
                * cutline goes thru the z=zmin axis
                */
               create_tria(polyhead,polytail,
                           nd00,nd10,nd11,bmat,polyID++);
               create_tria(polyhead,polytail,
                           nd00,nd01,nd11,tmat,polyID++);
            } else {
               /*     .
                *  y----
                *   |\T|     
                *   |B\|
                *   z---x
                *     .
                * cutline does not go thru the y=ymin axis
                */
               create_tria(polyhead,polytail,
                           nd00,nd10,nd01,bmat,polyID++);
               create_tria(polyhead,polytail,
                           nd11,nd10,nd01,tmat,polyID++);
            }
         }
      } /* if divided */
   }

   /* Error checking */
   if (ptID == 0) {
      (void) fprintf(stderr,"Error! 0 points!\n");
      return(0);
   }
   if (ndID == 0) {
      (void) fprintf(stderr,"Error! 0 nodes!\n");
      return(0);
   }
   if (polyID == 0) {
      (void) fprintf(stderr,"Error! 0 rects, 0 trias!\n");
      return(0);
   }

   if (verbose) {
      (void) fprintf(stdout,"Grid was successfully sliced at z=%g...",z);
      (void) fprintf(stdout,
                     "Found %d polygons %d nodes %d points\n",
                     CNcount_polys(*polyhead, *polytail),
                     CNcount_nodes(*nodehead, *nodetail),
                     CNcount_points(*pointhead, *pointtail));
   }

   /* return */
   return(1);
}



/*
 * CREATION UTILITIES
 */

/*
 * Create a triangle and place it in the poly list
 */
static void create_tria(polyhead, polytail, n1, n2, n3, region, ID)
CNpolyptr  *polyhead, *polytail;
CNnodeptr  n1, n2, n3;
int        region;
int        ID;
{
   CNnlistptr nlhead=NULL, nltail=NULL;

   (void) CNinsert_tailnlist(&nlhead, &nltail, n1);
   (void) CNinsert_tailnlist(&nlhead, &nltail, n2);
   (void) CNinsert_tailnlist(&nlhead, &nltail, n3);
   (void) CNinsert_tailnlist(&nlhead, &nltail, n1);
   (void) CNinsert_poly(polyhead, polytail, nlhead, nltail, region, ID);
}

/*
 * Create a rectangle and place it in the poly list
 */
static void create_rect(polyhead, polytail, n1, n2, n3, n4, region, ID)
CNpolyptr  *polyhead, *polytail;
CNnodeptr  n1, n2, n3, n4;
int        region;
int        ID;
{
   CNnlistptr nlhead=NULL, nltail=NULL;

   (void) CNinsert_tailnlist(&nlhead, &nltail, n1);
   (void) CNinsert_tailnlist(&nlhead, &nltail, n2);
   (void) CNinsert_tailnlist(&nlhead, &nltail, n3);
   (void) CNinsert_tailnlist(&nlhead, &nltail, n4);
   (void) CNinsert_tailnlist(&nlhead, &nltail, n1);
   (void) CNinsert_poly(polyhead, polytail, nlhead, nltail, region, ID);
}
