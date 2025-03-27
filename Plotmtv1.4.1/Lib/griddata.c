/*
 * griddata.c - manipulate the grid data 
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <malloc.h>
#include "CNplot.h"

/*
 * The volumetric grid data is originally in a 1D array
 *   [ [0,0,0]...[i,0,0]...[nx,0,0], [0,j,0]...[i,j,0]...[nx,ny,0] ]
 *   [ [0,0,1]...[i,0,1]...[nx,0,1], [0,j,1]...[i,j,1]...[nx,ny,1] ]
 *
 *   [ [0,0,nz]...[i,0,nz]...[nx,0,nz], [0,j,nz]...[i,j,nz]...[nx,ny,nz] ]
 * Save the arrays in a grid data-structure 
 */
CNdatasetptr
CNget_volumetric_grid_data(filename, dataname,
                           xgrid_arr, ygrid_arr, zgrid_arr, tgrid_arr,
                           nx, ny, nz, 
                           ID)
char      *filename;          /* Name of original file */
char      *dataname;          /* Descriptive label     */
double    *xgrid_arr;         /* 1D data array x[i]    */
double    *ygrid_arr;         /* 1D data array y[j]    */
double    *zgrid_arr;         /* 1D data array z[k]    */
double    *tgrid_arr;         /* 1D data array z(i,j,k)*/
int       nx, ny, nz;         /* #x, y and z gridpoints*/
int       ID;                 /* Data ID               */
{
   CNgrid4Dptr   grid;
   CNdatasetptr  dptr;
   double        xmin, xmax, ymin, ymax, zmin, zmax, tmin, tmax;
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
   if (tgrid_arr == NULL) {
      (void) fprintf(stderr,"Error! NULL t-array!\n");
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
   tmin =  CN_LARGE;
   tmax = -CN_LARGE;
   for (i=0; i<nx*ny*nz; i++) {
      if (tgrid_arr[i] < tmin) tmin = tgrid_arr[i];
      if (tgrid_arr[i] > tmax) tmax = tgrid_arr[i];
   }

   /*
    * Create and initialize a data structure to hold this contour dataset.
    * The boundary is the same as the viewport.
    */
   if ((dptr = CNmake_dataset(filename,dataname,CN_GRID4D,
                              xmin,xmax,ymin,ymax,zmin,zmax,
                              xmin,xmax,ymin,ymax,zmin,zmax,ID))==NULL)
      return(dptr);

   /* 
    * Create a grid structure 
    */
   grid = CNmake_grid4D(0);
   if (grid == NULL) {
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
      dptr->grid   = grid;
      grid->xarray = xgrid_arr;
      grid->yarray = ygrid_arr;
      grid->zarray = zgrid_arr;
      grid->tarray = tgrid_arr;
      grid->nx     = nx;
      grid->ny     = ny;
      grid->nz     = nz;
      grid->nt     = nx*ny*nz;
      grid->xmin   = xmin;
      grid->xmax   = xmax;
      grid->ymin   = ymin;
      grid->ymax   = ymax;
      grid->zmin   = zmin;
      grid->zmax   = zmax;
      grid->tmin   = tmin;
      grid->tmax   = tmax;

      /* Set contour properties */
      dptr->data_pr.cmin = tmin;
      dptr->data_pr.cmax = tmax;
      dptr->data_pr.cstep= CNround_to_decimal((tmax-tmin)/(double)CN_IDLSTEPS);

   }

   /*
    * Return the pointer
    */
   return(dptr);
}


/*
 * Slice a grid along the x-axis
 */
CNsliceptr CNslice_grid4D_x(Gptr, x, contintrp, verbose)
CNgrid4Dptr Gptr;                     /* Grid structure        */
double      x;                        /* Slice value           */
int         contintrp;                /* Contour interpolation */
int         verbose;
{
   int        i, j, k, ix, ny, nz;
   int        FOUND;
   double     x0, x1, y, z, t;
   double     v0, v1, v;
   CNsliceptr slice;
   CNpointptr P;
   double     *ygrid_arr=NULL;
   double     *zgrid_arr=NULL;
   double     *tgrid_arr=NULL;
   int        joincurve=CN_FALSE;
   CNrectptr  recthead=NULL, recttail=NULL;   /* List of rectangles */
   CNtriaptr  triahead=NULL, triatail=NULL;   /* List of triangles  */
   CNnodeptr  nodehead=NULL, nodetail=NULL;   /* List of nodes      */
   CNpointptr pointhead=NULL,pointtail=NULL;  /* List of points     */
   
   /* 
    * Check the grid 
    */
   if (Gptr == NULL) {
      (void) fprintf(stderr,"Error! Cannot slice NULL grid!\n");
      return(NULL);
   }

   /*
    * Now check the bounds 
    */
   if ((x < Gptr->xmin) || (x > Gptr->xmax)) {
      if (verbose) {
      (void) fprintf(stdout,"x=%g is outside the x-bounds [%g, %g]\n",
                     x, Gptr->xmin, Gptr->xmax);
      }
      return(NULL);
   }

   /*
    * Find the bracketing index
    */
   FOUND = CN_FALSE;
   ix    = 0;
   for (i=0; i<Gptr->nx-1 && !FOUND; i++) {
      x0 = CNgrid4D_x(Gptr,i);
      x1 = CNgrid4D_x(Gptr,i+1);
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
      return(NULL);
   }

   /*
    * Print information 
    */
   if (verbose)
      (void) fprintf(stdout,"Slicing the grid at x=%g (ix=%d)...\n",x,ix);

   /*
    * Allocate room for 1D arrays
    */
   ny = Gptr->ny;
   nz = Gptr->nz;
   ygrid_arr = CNcreate_1D_double_array(ny);
   zgrid_arr = CNcreate_1D_double_array(nz);
   tgrid_arr = CNcreate_1D_double_array(ny*nz);
   if (ygrid_arr==NULL || zgrid_arr==NULL || tgrid_arr==NULL) {
      (void) fprintf(stderr,"Unable to allocate temp array space!\n");
      if (ygrid_arr) CNfree_1D_double_array(ygrid_arr);
      if (zgrid_arr) CNfree_1D_double_array(zgrid_arr);
      if (tgrid_arr) CNfree_1D_double_array(tgrid_arr);
      return(NULL);
   }

   /* 
    * Get the intersections and store the 1D arrays
    */
   for (j=0; j<ny; j++) {
      y = CNgrid4D_y(Gptr,j);
      CNset_1D_double_array_value(ygrid_arr,j,ny,&y);
   }
   for (k=0; k<nz; k++) {
      z = CNgrid4D_z(Gptr,k);
      CNset_1D_double_array_value(zgrid_arr,k,nz,&z);
   }
   for (k=0; k<nz; k++) 
   for (j=0; j<ny; j++) {
      y = CNgrid4D_y(Gptr,j);
      z = CNgrid4D_z(Gptr,k);

      /* Get the values at (ix,j,k) and (ix+1,j,k) */
      v0 = CNgrid4D_t(Gptr,ix  ,j,k);
      v1 = CNgrid4D_t(Gptr,ix+1,j,k);
      v  = v0 + t*(v1-v0);
      CNset_1D_double_array_value(tgrid_arr,j+k*ny,ny*nz,&v);
   }

   /*
    * Convert the arrays into points, nodes, triangles, rectangles 
    */
   CNpartition_rectilinear_data(&pointhead, &pointtail,
                                &nodehead,  &nodetail,
                                &triahead,  &triatail,
                                &recthead,  &recttail,
                                ygrid_arr, ny, Gptr->ymin, Gptr->ymax,
                                zgrid_arr, nz, Gptr->zmin, Gptr->zmax,
                                tgrid_arr, ny*nz, Gptr->tmin, Gptr->tmax,
                                contintrp, joincurve);

   /* Rearrange the point data */
   for (P=pointhead; P!=NULL; P=P->next) {
      y = P->x;
      z = P->y;
      P->x = x;
      P->y = y;
      P->z = z;
   }

   if (verbose) {
      (void) fprintf(stdout,"Grid was successfully sliced at x=%g...",x);
      (void) fprintf(stdout,
                     "Found %d rectangles %d triangles %d nodes %d points\n",
                     CNcount_rects(recthead, recttail), 
                     CNcount_trias(triahead, triatail), 
                     CNcount_nodes(nodehead, nodetail), 
                     CNcount_points(pointhead, pointtail));
   }

   /* Create a slice datastructure */
   if ((slice = CNmake_slice()) != NULL) {
      slice->slice_plane = CN_XSLICE;
      slice->slice_value = x;
      slice->xmin        = Gptr->xmin;
      slice->xmax        = Gptr->xmax;
      slice->ymin        = Gptr->ymin;
      slice->ymax        = Gptr->ymax;
      slice->zmin        = Gptr->zmin;
      slice->zmax        = Gptr->zmax;
      slice->tmin        = Gptr->tmin;
      slice->tmax        = Gptr->tmax;
      slice->xarray      = ygrid_arr;
      slice->yarray      = zgrid_arr;
      slice->zarray      = tgrid_arr;
      slice->nx          = ny;
      slice->ny          = nz;
      slice->nz          = ny*nz;
      slice->pointhead   = pointhead;
      slice->pointtail   = pointtail;
      slice->nodehead    = nodehead;
      slice->nodetail    = nodetail;
      slice->triahead    = triahead;
      slice->triatail    = triatail;
      slice->recthead    = recthead;
      slice->recttail    = recttail;
   }

   /* Delete the temp arrays (these are deleted during CNdelete_slice() */
   /*
   CNfree_1D_double_array(ygrid_arr);
   CNfree_1D_double_array(zgrid_arr);
   CNfree_1D_double_array(tgrid_arr);
    */

   /* Return the slice */
   return(slice);
}


/*
 * Slice a grid along the y-axis
 */
CNsliceptr CNslice_grid4D_y(Gptr, y, contintrp, verbose)
CNgrid4Dptr Gptr;                     /* Grid structure        */
double      y;                        /* Slice value           */
int         contintrp;                /* Contour interpolation */
int         verbose;
{
   int        i, j, k, jy, nx, nz;
   int        FOUND;
   double     y0, y1, x, z, t;
   double     v0, v1, v;
   CNsliceptr slice;
   CNpointptr P;
   double     *xgrid_arr=NULL;
   double     *zgrid_arr=NULL;
   double     *tgrid_arr=NULL;
   int        joincurve=CN_FALSE;
   CNrectptr  recthead=NULL, recttail=NULL;   /* List of rectangles */
   CNtriaptr  triahead=NULL, triatail=NULL;   /* List of triangles  */
   CNnodeptr  nodehead=NULL, nodetail=NULL;   /* List of nodes      */
   CNpointptr pointhead=NULL,pointtail=NULL;  /* List of points     */
   
   /* 
    * Check the grid 
    */
   if (Gptr == NULL) {
      (void) fprintf(stderr,"Error! Cannot slice NULL grid!\n");
      return(NULL);
   }

   /*
    * Now check the bounds 
    */
   if ((y < Gptr->ymin) || (y > Gptr->ymax)) {
      if (verbose) {
      (void) fprintf(stdout,"y=%g is outside the y-bounds [%g, %g]\n",
                     y, Gptr->ymin, Gptr->ymax);
      }
      return(NULL);
   }

   /*
    * Find the bracketing index
    */
   FOUND = CN_FALSE;
   jy    = 0;
   for (j=0; j<Gptr->ny-1 && !FOUND; j++) {
      y0 = CNgrid4D_y(Gptr,j);
      y1 = CNgrid4D_y(Gptr,j+1);
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
      return(NULL);
   }

   /*
    * Print information 
    */
   if (verbose)
      (void) fprintf(stdout,"Slicing the grid at y=%g (jy=%d)...\n",y,jy);

   /*
    * Allocate room for 1D arrays
    */
   nx = Gptr->nx;
   nz = Gptr->nz;
   xgrid_arr = CNcreate_1D_double_array(nx);
   zgrid_arr = CNcreate_1D_double_array(nz);
   tgrid_arr = CNcreate_1D_double_array(nx*nz);
   if (xgrid_arr==NULL || zgrid_arr==NULL || tgrid_arr==NULL) {
      (void) fprintf(stderr,"Unable to allocate temp array space!\n");
      if (xgrid_arr) CNfree_1D_double_array(xgrid_arr);
      if (zgrid_arr) CNfree_1D_double_array(zgrid_arr);
      if (tgrid_arr) CNfree_1D_double_array(tgrid_arr);
      return(NULL);
   }

   /* 
    * Get the intersections and store the 1D arrays
    */
   for (i=0; i<nx; i++) {
      x = CNgrid4D_x(Gptr,i);
      CNset_1D_double_array_value(xgrid_arr,i,nx,&x);
   }
   for (k=0; k<nz; k++) {
      z = CNgrid4D_z(Gptr,k);
      CNset_1D_double_array_value(zgrid_arr,k,nz,&z);
   }
   for (k=0; k<nz; k++) 
   for (i=0; i<nx; i++) {
      x = CNgrid4D_x(Gptr,i);
      z = CNgrid4D_z(Gptr,k);

      /* Get the values at (i,jy,k) and (i,jy+1,k) */
      v0 = CNgrid4D_t(Gptr,i,jy  ,k);
      v1 = CNgrid4D_t(Gptr,i,jy+1,k);
      v  = v0 + t*(v1-v0);
      CNset_1D_double_array_value(tgrid_arr,i+k*nx,nx*nz,&v);
   }

   /*
    * Convert the arrays into points, nodes, triangles, rectangles 
    */
   CNpartition_rectilinear_data(&pointhead, &pointtail,
                                &nodehead,  &nodetail,
                                &triahead,  &triatail,
                                &recthead,  &recttail,
                                xgrid_arr, nx, Gptr->xmin, Gptr->xmax,
                                zgrid_arr, nz, Gptr->zmin, Gptr->zmax,
                                tgrid_arr, nx*nz, Gptr->tmin, Gptr->tmax,
                                contintrp, joincurve);

   /* Rearrange the point data */
   for (P=pointhead; P!=NULL; P=P->next) {
      x = P->x;
      z = P->y;
      P->x = x;
      P->y = y;
      P->z = z;
   }

   if (verbose) {
      (void) fprintf(stdout,"Grid was successfully sliced at y=%g...",y);
      (void) fprintf(stdout,
                     "Found %d rectangles %d triangles %d nodes %d points\n",
                     CNcount_rects(recthead, recttail), 
                     CNcount_trias(triahead, triatail), 
                     CNcount_nodes(nodehead, nodetail), 
                     CNcount_points(pointhead, pointtail));
   }

   /* Create a slice datastructure */
   if ((slice = CNmake_slice()) != NULL) {
      slice->slice_plane = CN_YSLICE;
      slice->slice_value = y;
      slice->xmin        = Gptr->xmin;
      slice->xmax        = Gptr->xmax;
      slice->ymin        = Gptr->ymin;
      slice->ymax        = Gptr->ymax;
      slice->zmin        = Gptr->zmin;
      slice->zmax        = Gptr->zmax;
      slice->tmin        = Gptr->tmin;
      slice->tmax        = Gptr->tmax;
      slice->xarray      = xgrid_arr;
      slice->yarray      = zgrid_arr;
      slice->zarray      = tgrid_arr;
      slice->nx          = nx;
      slice->ny          = nz;
      slice->nz          = nx*nz;
      slice->pointhead   = pointhead;
      slice->pointtail   = pointtail;
      slice->nodehead    = nodehead;
      slice->nodetail    = nodetail;
      slice->triahead    = triahead;
      slice->triatail    = triatail;
      slice->recthead    = recthead;
      slice->recttail    = recttail;
   }

   /* Delete the temp arrays (these are deleted during CNdelete_slice() */
   /*
   CNfree_1D_double_array(xgrid_arr);
   CNfree_1D_double_array(zgrid_arr);
   CNfree_1D_double_array(tgrid_arr);
    */

   /* Return the slice */
   return(slice);
}


/*
 * Slice a grid along the z-axis
 */
CNsliceptr CNslice_grid4D_z(Gptr, z, contintrp, verbose)
CNgrid4Dptr Gptr;                     /* Grid structure        */
double      z;                        /* Slice value           */
int         contintrp;                /* Contour interpolation */
int         verbose;
{
   int        i, j, k, kz, nx, ny;
   int        FOUND;
   double     z0, z1, x, y, t;
   double     v0, v1, v;
   CNsliceptr slice;
   CNpointptr P;
   double     *xgrid_arr=NULL;
   double     *ygrid_arr=NULL;
   double     *tgrid_arr=NULL;
   int        joincurve=CN_FALSE;
   CNrectptr  recthead=NULL, recttail=NULL;   /* List of rectangles */
   CNtriaptr  triahead=NULL, triatail=NULL;   /* List of triangles  */
   CNnodeptr  nodehead=NULL, nodetail=NULL;   /* List of nodes      */
   CNpointptr pointhead=NULL,pointtail=NULL;  /* List of points     */
   
   /* 
    * Check the grid 
    */
   if (Gptr == NULL) {
      (void) fprintf(stderr,"Error! Cannot slice NULL grid!\n");
      return(NULL);
   }

   /*
    * Now check the bounds 
    */
   if ((z < Gptr->zmin) || (z > Gptr->zmax)) {
      if (verbose) {
      (void) fprintf(stdout,"z=%g is outside the z-bounds [%g, %g]\n",
                     z, Gptr->zmin, Gptr->zmax);
      }
      return(NULL);
   }

   /*
    * Find the bracketing index
    */
   FOUND = CN_FALSE;
   kz    = 0;
   for (k=0; k<Gptr->nz-1 && !FOUND; k++) {
      z0 = CNgrid4D_z(Gptr,k);
      z1 = CNgrid4D_z(Gptr,k+1);
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
      return(NULL);
   }

   /*
    * Print information 
    */
   if (verbose)
      (void) fprintf(stdout,"Slicing the grid at z=%g (kz=%d)...\n",z,kz);

   /*
    * Allocate room for 1D arrays
    */
   nx = Gptr->nx;
   ny = Gptr->ny;
   xgrid_arr = CNcreate_1D_double_array(nx);
   ygrid_arr = CNcreate_1D_double_array(ny);
   tgrid_arr = CNcreate_1D_double_array(nx*ny);
   if (xgrid_arr==NULL || ygrid_arr==NULL || tgrid_arr==NULL) {
      (void) fprintf(stderr,"Unable to allocate temp array space!\n");
      if (xgrid_arr) CNfree_1D_double_array(xgrid_arr);
      if (ygrid_arr) CNfree_1D_double_array(ygrid_arr);
      if (tgrid_arr) CNfree_1D_double_array(tgrid_arr);
      return(NULL);
   }

   /* 
    * Get the intersections and store the 1D arrays
    */
   for (i=0; i<nx; i++) {
      x = CNgrid4D_x(Gptr,i);
      CNset_1D_double_array_value(xgrid_arr,i,nx,&x);
   }
   for (j=0; j<ny; j++) {
      y = CNgrid4D_y(Gptr,j);
      CNset_1D_double_array_value(ygrid_arr,j,ny,&y);
   }
   for (j=0; j<ny; j++) 
   for (i=0; i<nx; i++) {
      x = CNgrid4D_x(Gptr,i);
      y = CNgrid4D_y(Gptr,j);

      /* Get the values at (i,j,kz) and (i,j,kz+1) */
      v0 = CNgrid4D_t(Gptr,i,j,kz  );
      v1 = CNgrid4D_t(Gptr,i,j,kz+1);
      v  = v0 + t*(v1-v0);
      CNset_1D_double_array_value(tgrid_arr,i+j*nx,nx*ny,&v);
   }

   /*
    * Convert the arrays into points, nodes, triangles, rectangles 
    */
   CNpartition_rectilinear_data(&pointhead, &pointtail,
                                &nodehead,  &nodetail,
                                &triahead,  &triatail,
                                &recthead,  &recttail,
                                xgrid_arr, nx, Gptr->xmin, Gptr->xmax,
                                ygrid_arr, ny, Gptr->zmin, Gptr->zmax,
                                tgrid_arr, nx*ny, Gptr->tmin, Gptr->tmax,
                                contintrp, joincurve);

   /* Rearrange the point data */
   for (P=pointhead; P!=NULL; P=P->next) {
      P->z = z;
   }

   if (verbose) {
      (void) fprintf(stdout,"Grid was successfully sliced at z=%g...",z);
      (void) fprintf(stdout,
                     "Found %d rectangles %d triangles %d nodes %d points\n",
                     CNcount_rects(recthead, recttail), 
                     CNcount_trias(triahead, triatail), 
                     CNcount_nodes(nodehead, nodetail), 
                     CNcount_points(pointhead, pointtail));
   }

   /* Create a slice datastructure */
   if ((slice = CNmake_slice()) != NULL) {
      slice->slice_plane = CN_ZSLICE;
      slice->slice_value = z;
      slice->xmin        = Gptr->xmin;
      slice->xmax        = Gptr->xmax;
      slice->ymin        = Gptr->ymin;
      slice->ymax        = Gptr->ymax;
      slice->zmin        = Gptr->zmin;
      slice->zmax        = Gptr->zmax;
      slice->tmin        = Gptr->tmin;
      slice->tmax        = Gptr->tmax;
      slice->xarray      = xgrid_arr;
      slice->yarray      = ygrid_arr;
      slice->zarray      = tgrid_arr;
      slice->nx          = nx;
      slice->ny          = ny;
      slice->nz          = nx*ny;
      slice->pointhead   = pointhead;
      slice->pointtail   = pointtail;
      slice->nodehead    = nodehead;
      slice->nodetail    = nodetail;
      slice->triahead    = triahead;
      slice->triatail    = triatail;
      slice->recthead    = recthead;
      slice->recttail    = recttail;
   }

   /* Delete the temp arrays (these are deleted during CNdelete_slice() */
   /*
   CNfree_1D_double_array(xgrid_arr);
   CNfree_1D_double_array(ygrid_arr);
   CNfree_1D_double_array(tgrid_arr);
    */

   /* Return the slice */
   return(slice);
}


/*
 * Create a dataset from a slice
 */
CNdatasetptr CNcreate_dataset_from_slice(slice,xlabel,ylabel,zlabel,ID,verbose)
CNsliceptr slice;
char       *xlabel, *ylabel, *zlabel;
int        ID,verbose;
{
   CNdatasetptr dptr;
   CNrectptr    recthead=NULL, recttail=NULL;   /* List of rectangles */
   CNtriaptr    triahead=NULL, triatail=NULL;   /* List of triangles  */
   CNnodeptr    nodehead=NULL, nodetail=NULL;   /* List of nodes      */
   CNpointptr   pointhead=NULL,pointtail=NULL;  /* List of points     */
   CNtriaptr    T;
   CNrectptr    R;
   CNnodeptr    *ndarr, N;
   CNpointptr   *ptarr, P;
   CNgrid4Dptr  grid;
   unsigned     int arr_size;
   int          npoints, nnodes, ntrias, nrects, i,j,k;
   int          nx, ny, nz;
   double       x,y,z,xmin,xmax,ymin,ymax,zmin,zmax;
   double       *xarray=NULL, *yarray=NULL, *zarray=NULL;
   char         label[CN_MAXCHAR];
   char         x_label[CN_MAXCHAR];
   char         y_label[CN_MAXCHAR];
   char         z_label[CN_MAXCHAR];

   /* Check the slice */
   if (slice == NULL) {
      (void) fprintf(stderr,"Error! NULL slice!\n");
      return(NULL);
   }

   /* Initialize the axis labels */
   if (xlabel == NULL) (void) strcpy(x_label, "X-Axis");
   else                (void) strcpy(x_label, xlabel);
   if (ylabel == NULL) (void) strcpy(y_label, "Y-Axis");
   else                (void) strcpy(y_label, ylabel);
   if (zlabel == NULL) (void) strcpy(z_label, "Z-Axis");
   else                (void) strcpy(z_label, zlabel);

   /* Count the number of nodes and points and rectangles */
   npoints = CNcount_points(slice->pointhead, slice->pointtail);
   nnodes  = CNcount_nodes (slice->nodehead,  slice->nodetail);
   ntrias  = CNcount_trias (slice->triahead,  slice->triatail);
   nrects  = CNcount_rects (slice->recthead,  slice->recttail);

   /* Error checking */
   if (npoints == 0) {
      (void) fprintf(stderr,"Error! 0 points!\n");
      return(NULL);
   }
   if (nnodes == 0) {
      (void) fprintf(stderr,"Error! 0 nodes!\n");
      return(NULL);
   }
   if (nrects == 0 && ntrias == 0) {
      (void) fprintf(stderr,"Error! 0 rects, 0 trias!\n");
      return(NULL);
   }

   /* allocate space for points using malloc(), and return a pointer */
   arr_size = (unsigned)(nnodes)*sizeof(CNpointptr);
   if ( (ptarr = (CNpointptr *)malloc(arr_size)) == NULL) {
      (void) fprintf(stderr,"Out of memory\n");
      exit(-1);
   }
   /* allocate space for nodes using malloc(), and return a pointer */
   arr_size = (unsigned)(nnodes)*sizeof(CNnodeptr);
   if ( (ndarr = (CNnodeptr *)malloc(arr_size)) == NULL) {
      (void) fprintf(stderr,"Out of memory\n");
      exit(-1);
   }

   /* Renumber the points and nodes just in case */
   i = 0;
   for (P=slice->pointhead; P!=NULL; P=P->next) P->ID = i++;
   i = 0;
   for (N=slice->nodehead; N!=NULL; N=N->next)  N->ID = i++;

   /* Copy the points */
   for (P=slice->pointhead; P!=NULL; P=P->next) {
      /* Adjust the x-y-z coordinates */
      switch (slice->slice_plane) {
      case CN_XSLICE : x=P->y; y=P->z; z=0.0; break;
      case CN_YSLICE : x=P->x; y=P->z; z=0.0; break;
      case CN_ZSLICE : 
      default        : x=P->x; y=P->y; z=0.0; break;
      }
      ptarr[P->ID] = CNinsert_point(&pointhead,&pointtail,x,y,z,P->ID);
   }

   /* Copy the nodes */
   for (N=slice->nodehead; N!=NULL; N=N->next) 
      ndarr[N->ID] = CNinsert_tailnode(&nodehead,&nodetail,
                                       ptarr[N->coord->ID],N->t,N->ID);

   /* Copy the rectangles */
   for (R=slice->recthead; R!=NULL; R=R->next)
      (void) CNinsert_rect(&recthead,&recttail,
                           ndarr[R->n1->ID],
                           ndarr[R->n2->ID],
                           ndarr[R->n3->ID],
                           ndarr[R->n4->ID],R->ID);

   /* Copy the triangles */
   for (T=slice->triahead; T!=NULL; T=T->next)
      (void) CNinsert_tria(&triahead,&triatail,
                           ndarr[T->n1->ID],
                           ndarr[T->n2->ID],
                           ndarr[T->n3->ID],0,T->ID);

   /* Free the arrays */
   free ((char *) ptarr);
   free ((char *) ndarr);

   /* Create the label */
   switch (slice->slice_plane) {
   case CN_XSLICE : (void) sprintf(label," \"%s\" Slice : val=%g",
                    x_label, slice->slice_value);
                    break;
   case CN_YSLICE : (void) sprintf(label," \"%s\" Slice : val=%g",
                    y_label, slice->slice_value);
                    break;
   case CN_ZSLICE : 
   default        : (void) sprintf(label," \"%s\" Slice : val=%g",
                    z_label, slice->slice_value);
                    break;
   }

   /* Set the xmin xmax etc */
   switch (slice->slice_plane) {
   case CN_XSLICE : xmin = slice->ymin;  xmax = slice->ymax; 
                    ymin = slice->zmin;  ymax = slice->zmax; 
                    zmin = slice->tmin;  zmax = slice->tmax;
                    break;
   case CN_YSLICE : xmin = slice->xmin;  xmax = slice->xmax; 
                    ymin = slice->zmin;  ymax = slice->zmax; 
                    zmin = slice->tmin;  zmax = slice->tmax;
                    break;
   case CN_ZSLICE :
   default        : xmin = slice->xmin;  xmax = slice->xmax; 
                    ymin = slice->ymin;  ymax = slice->ymax; 
                    zmin = slice->tmin;  zmax = slice->tmax;
                    break;
   }


   /* Now make a dataset */
   if ((dptr = CNmake_dataset(label,label,CN_CONTOUR,
                              xmin,xmax, ymin,ymax, zmin,zmax,
                              xmin,xmax, ymin,ymax, zmin,zmax,
                              ID))==NULL) {
      /* Free the lists */
      CNdelete_point_list(&pointhead,&pointtail);
      CNdelete_node_list(&nodehead,&nodetail);
      CNdelete_tria_list(&triahead,&triatail);
      CNdelete_rect_list(&recthead,&recttail);
      return(dptr);
   }

   /* Attach the points/nodes/trias/rects */
   dptr->pointhead = pointhead;
   dptr->pointtail = pointtail;
   dptr->nodehead  = nodehead;
   dptr->nodetail  = nodetail;
   dptr->triahead  = triahead;
   dptr->triatail  = triatail;
   dptr->recthead  = recthead;
   dptr->recttail  = recttail;

   /* Set the labels */
   (void) CNparse_plotset_property(&dptr->plot_pr,
                                   "toplabel",label,verbose);
   (void) CNparse_plotset_property(&dptr->plot_pr,
                                   "zlabel","T-Axis",verbose);
   switch (slice->slice_plane) {
   case CN_XSLICE : 
        (void) CNparse_plotset_property(&dptr->plot_pr,
                                        "xlabel",y_label,verbose);
        (void) CNparse_plotset_property(&dptr->plot_pr,
                                        "ylabel",z_label,verbose);
        break;
   case CN_YSLICE : 
        (void) CNparse_plotset_property(&dptr->plot_pr,
                                        "xlabel",x_label,verbose);
        (void) CNparse_plotset_property(&dptr->plot_pr,
                                        "ylabel",z_label,verbose);
        break;
   case CN_ZSLICE : 
   default        :
        (void) CNparse_plotset_property(&dptr->plot_pr,
                                        "xlabel",x_label,verbose);
        (void) CNparse_plotset_property(&dptr->plot_pr,
                                        "ylabel",y_label,verbose);
        break;
   }

   /* Slice the contours */
   dptr->data_pr.cmin  = slice->tmin;
   dptr->data_pr.cmax  = slice->tmax;
   dptr->data_pr.cstep = CNround_to_decimal(
                          (slice->tmax-slice->tmin)/(double)CN_IDLSTEPS);
   CNslice_contours(dptr,verbose);

   /* Put in a grid and the array info */
   if (slice->xarray && slice->yarray && slice->zarray) {
      /* Allocate room for 1D arrays */
      nx     = slice->nx;
      ny     = slice->ny;
      nz     = slice->nz;
      xarray = CNcreate_1D_double_array(nx);
      yarray = CNcreate_1D_double_array(ny);
      zarray = CNcreate_1D_double_array(nz);
      if (xarray==NULL || yarray==NULL || zarray==NULL) {
         (void) fprintf(stderr,"Unable to allocate temp array space!\n");
         if (xarray) CNfree_1D_double_array(xarray);
         if (yarray) CNfree_1D_double_array(yarray);
         if (zarray) CNfree_1D_double_array(zarray);
      } else {
         /* Fill the arrays */
         for (i=0; i<nx; i++) {
            x = CNget_1D_double_array_value(slice->xarray,i,nx);
            CNset_1D_double_array_value(xarray,i,nx,&x);
         }
         for (j=0; j<ny; j++) {
            y = CNget_1D_double_array_value(slice->yarray,j,ny);
            CNset_1D_double_array_value(yarray,j,ny,&y);
         }
         for (k=0; k<nz; k++) {
            z = CNget_1D_double_array_value(slice->zarray,k,nz);
            CNset_1D_double_array_value(zarray,k,nz,&z);
         }
         /* Create a grid-structure */
         if ((grid = CNmake_grid4D(0)) == NULL) {
            (void) fprintf(stderr,"Unable to allocate grid space!\n");
            if (xarray) CNfree_1D_double_array(xarray);
            if (yarray) CNfree_1D_double_array(yarray);
            if (zarray) CNfree_1D_double_array(zarray);
         } else {
            /* Fill in the grid */
            dptr->grid   = grid;
            grid->xarray = xarray;
            grid->yarray = yarray;
            grid->zarray = zarray;
            grid->nx     = nx;
            grid->ny     = ny;
            grid->nz     = nz;
            grid->xmin   = xmin;
            grid->xmax   = xmax;
            grid->ymin   = ymin;
            grid->ymax   = ymax;
            grid->zmin   = zmin;
            grid->zmax   = zmax;
         } 
      }
   }

   /* Return */
   return(dptr);
}

/*
 * Slice a grid4d dataset
 */
CNdatasetptr CNslice_grid4D_dataset(dptr, slice_plane, slice_value,
                                    xlabel, ylabel, zlabel,
                                    dataID, verbose)
CNdatasetptr dptr;
int          slice_plane;
double       slice_value;
char         *xlabel, *ylabel, *zlabel;   /* X-Y-Z labels */
int          *dataID;
int          verbose;
{
   CNdatasetptr Dslice=NULL;
   CNsliceptr   slice=NULL;
   CNgrid4Dptr  grid;
   double       cmin,cmax,cstep;
   int          contintrp;

   /* Error check */
   if (dptr == NULL) {
      (void) fprintf(stderr,"Error! NULL dataset!\n");
      return(NULL);
   }
   if ((grid=dptr->grid) == NULL) {
      (void) fprintf(stderr,"Error! NULL mesh4D structure!\n");
      return(NULL);
   }

   /* Contour interpolation */
   contintrp = dptr->data_pr.contintrp;

   switch (slice_plane) {
   case CN_XSLICE : /* Slice in x */
                    slice = CNslice_grid4D_x(grid, slice_value,
                                             contintrp, verbose);
                    break;
   case CN_YSLICE : /* Slice in y */
                    slice = CNslice_grid4D_y(grid, slice_value,
                                             contintrp, verbose);
                    break;
   case CN_ZSLICE :
   default        : /* Slice in z */
                    slice = CNslice_grid4D_z(grid, slice_value,
                                             contintrp, verbose);
                    break;
   }

   if (slice == NULL) {
      (void) fprintf(stderr,"ERROR: Could't slice grid at %s = %g\n",
                     CNsliceplane(slice_plane), slice_value);
      return(NULL);
   }

   /* Create a dataset */
   Dslice = CNcreate_dataset_from_slice(slice,
                                        xlabel, ylabel, zlabel,
                                        *dataID,verbose);
   if (Dslice == NULL) {
      (void) fprintf(stderr,"Error! Couldn't create dataset!\n");
      CNdelete_slice(slice);
      return(NULL);
   } 

   /* Replace the slice in the grid */
   if (grid->slice) CNdelete_slice(grid->slice);
   grid->slice = slice;
 
   /* Increment the data ID */
   (*dataID)++;

   /* Copy some properties from the grid data-properties */
   cmin = Dslice->data_pr.cmin;
   cmax = Dslice->data_pr.cmax;
   cstep= Dslice->data_pr.cstep;
   CNset_dataset_property(&(Dslice->data_pr),&(dptr->data_pr));
   Dslice->data_pr.cmin  = cmin;
   Dslice->data_pr.cmax  = cmax;
   Dslice->data_pr.cstep = cstep;
 
   /* Copy the contour-step list from the grid to the slice */
   if (dptr->cstephead && (dptr->data_pr.stepmethod == CN_USERDEFN)){
      CNcopy_contstep_list(&(Dslice->cstephead), &(Dslice->csteptail),
                           dptr->cstephead,   dptr->csteptail);
   }
 
   /* The slice is initially drawn as filled contours */
   (void) CNparse_dataset_property(&(Dslice->data_pr),
                                   "contstyle","2",verbose);
   (void) CNparse_plotset_property(&(Dslice->plot_pr),
                                   "equalscale","on",verbose);

 
   /* Return */
   return(Dslice);
}

