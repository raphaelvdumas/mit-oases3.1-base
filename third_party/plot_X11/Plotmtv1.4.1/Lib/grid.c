/*
 * grid.c - procedures to build and maintain a volumetric grid
 *           data structure
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include "CNplot.h"

static double get_1D_double_array_value();

/*
 * SLICE DATA STRUCTURE
 *    A slice is used to store data derived from a slice of a volumetric
 *    grid.  The slice information, on an orthogonal plane, is stored
 *    in rectangles, points and nodes.
 */
CNsliceptr CNmake_slice()
{
   CNsliceptr newptr;
   unsigned int size = sizeof(CNslice);

   if ((newptr = (CNsliceptr)malloc(size))!=NULL) {
      /* Slice Information */
      newptr->slice_plane  = CN_NSLICE;
      newptr->slice_value  = 0.0;
 
      /* Values */
      newptr->xmin   = 0.0;
      newptr->xmax   = 0.0;
      newptr->ymin   = 0.0;
      newptr->ymax   = 0.0;
      newptr->zmin   = 0.0;
      newptr->zmax   = 0.0;
      newptr->tmin   = 0.0;
      newptr->tmax   = 0.0;

      /* For making contour datasets */
      newptr->xarray = NULL;
      newptr->yarray = NULL;
      newptr->zarray = NULL;
      newptr->nx     = 0;
      newptr->ny     = 0;
      newptr->nz     = 0;

      /* Linked lists */
      newptr->pointhead = NULL;
      newptr->pointtail = NULL;
      newptr->nodehead  = NULL;
      newptr->nodetail  = NULL;
      newptr->triahead  = NULL;
      newptr->triatail  = NULL;
      newptr->recthead  = NULL;
      newptr->recttail  = NULL;
   }
   return(newptr);
}


/*
 * Delete slice 
 */
void CNdelete_slice(Sptr)
CNsliceptr Sptr;
{
   /* Free the arrays */
   if (Sptr->xarray) free((char *)Sptr->xarray);
   if (Sptr->yarray) free((char *)Sptr->yarray);
   if (Sptr->zarray) free((char *)Sptr->zarray);
 
   /* Reset the array counts */
   Sptr->nx = 0;
   Sptr->ny = 0;
   Sptr->nz = 0;
 
   /* delete all the rectangles in Sptr */
   CNdelete_rect_list(&(Sptr->recthead),&(Sptr->recttail));

   /* delete all the triangles in Sptr */
   CNdelete_tria_list(&(Sptr->triahead),&(Sptr->triatail));

   /* delete all the nodes in Dptr */
   CNdelete_node_list(&(Sptr->nodehead),&(Sptr->nodetail));

   /* delete all the points in Dptr */
   CNdelete_point_list(&(Sptr->pointhead),&(Sptr->pointtail));

   /* Now delete Sptr */
   free ((char*)Sptr);
}


/*
 * Print info on the slice
 */
void CNprint_slice(Sptr)
CNsliceptr Sptr;
{
   int npoints, nnodes, ntrias, nrects;

   (void) fprintf(stdout,"Slice Plane: %s\n",CNsliceplane(Sptr->slice_plane));
   (void) fprintf(stdout,"Slice Value: %g\n",Sptr->slice_value);

   /* Count the contents of the slice */
   npoints = CNcount_points(Sptr->pointhead,Sptr->pointtail);
   nnodes  = CNcount_nodes (Sptr->nodehead, Sptr->nodetail );
   ntrias  = CNcount_trias (Sptr->triahead, Sptr->triatail );
   nrects  = CNcount_rects (Sptr->recthead, Sptr->recttail );

   /* Print the contents of the slice */
   (void) fprintf(stdout,"   No of points = %d\n", npoints);
   (void) fprintf(stdout,"   No of nodes  = %d\n", nnodes);
   (void) fprintf(stdout,"   No of trias  = %d\n", ntrias);
   (void) fprintf(stdout,"   No of rects  = %d\n", nrects);
}

/*
 * Return a string denoting the plot-type
 */
char *CNsliceplane(sliceplane)
int sliceplane;
{
   char *data;

   switch (sliceplane) {
   case CN_NSLICE     : data = "No Slice";  break;
   case CN_XSLICE     : data = "X Slice";  break;
   case CN_YSLICE     : data = "Y Slice";  break;
   case CN_ZSLICE     : data = "Z Slice";  break;
   default            : data = "No Slice";  break;
   }

   return(data);
}


/*
 * GRID DATA STRUCTURE
 *    A grid is used to store single-valued data on a volumetric grid.
 *    The data is represented in the form of arrays.
 */

/*
 * Allocate room for a grid4D data structure
 */
CNgrid4Dptr CNmake_grid4D(ID)
int    ID;
{
   CNgrid4Dptr newptr;
   unsigned int size = sizeof(CNgrid4D);

   if ((newptr = (CNgrid4Dptr)malloc(size))!=NULL) {
      newptr->ID     = ID;
      newptr->flag   = 0;
      newptr->xarray = NULL;
      newptr->yarray = NULL;
      newptr->zarray = NULL;
      newptr->tarray = NULL;
      newptr->nx     = 0;
      newptr->ny     = 0;
      newptr->nz     = 0;
      newptr->nt     = 0;
      newptr->xmin   = 0.0;
      newptr->xmax   = 0.0;
      newptr->ymin   = 0.0;
      newptr->ymax   = 0.0;
      newptr->zmin   = 0.0;
      newptr->zmax   = 0.0;
      newptr->tmin   = 0.0;
      newptr->tmax   = 0.0;
      newptr->slice  = NULL;
   }
   return(newptr);
}


/*
 * Delete grid4D
 */
void CNdelete_grid4D(Gptr)
CNgrid4Dptr Gptr;
{
   /* Free the slice */
   if (Gptr->slice != NULL) CNdelete_slice(Gptr->slice);

   /* Free the arrays */
   if (Gptr->xarray) free((char *)Gptr->xarray); 
   if (Gptr->yarray) free((char *)Gptr->yarray); 
   if (Gptr->zarray) free((char *)Gptr->zarray); 
   if (Gptr->tarray) free((char *)Gptr->tarray); 
   
   /* Reset the array counts */
   Gptr->nx = 0;
   Gptr->ny = 0;
   Gptr->nz = 0;
   Gptr->nt = 0;

   /* Now delete Gptr */
   free ((char*)Gptr);
}


/*
 * Retrieve data from the grid
 */

/* x-value */
double CNgrid4D_x(Gptr,i)
CNgrid4Dptr Gptr;
int         i;
{
   double val;
   val = get_1D_double_array_value(Gptr->xarray,i,Gptr->nx);
   return(val);
}

/* y-value */
double CNgrid4D_y(Gptr,i)
CNgrid4Dptr Gptr;
int         i;
{
   double val;
   val = get_1D_double_array_value(Gptr->yarray,i,Gptr->ny);
   return(val);
}

/* z-value */
double CNgrid4D_z(Gptr,i)
CNgrid4Dptr Gptr;
int         i;
{
   double val;
   val = get_1D_double_array_value(Gptr->zarray,i,Gptr->nz);
   return(val);
}

/* t-value */
double CNgrid4D_t(Gptr,i,j,k)
CNgrid4Dptr Gptr;
int         i,j,k;
{
   double val;
   int    t;

   t   = i + j*Gptr->nx + k*Gptr->nx*Gptr->ny;
   val = get_1D_double_array_value(Gptr->tarray,t,Gptr->nt);
   return(val);
}

/*
 * Print info on the grid4D
 */
void CNprint_grid4D(Gptr)
CNgrid4Dptr Gptr;
{
   int i,j,k,i0,j0,k0;

   (void) fprintf(stdout,"Grid %d:\n",Gptr->ID);
   (void) fprintf(stdout,"   x[0] = %8.3g  x[%5d] = %8.3g\n",
                  CNgrid4D_x(Gptr,0),
                  Gptr->nx-1,
                  CNgrid4D_x(Gptr,Gptr->nx-1));
   (void) fprintf(stdout,"   y[0] = %8.3g  y[%5d] = %8.3g\n",
                  CNgrid4D_y(Gptr,0),
                  Gptr->ny-1, 
                  CNgrid4D_y(Gptr,Gptr->ny-1));
   (void) fprintf(stdout,"   z[0] = %8.3g  z[%5d] = %8.3g\n",
                  CNgrid4D_z(Gptr,0),
                  Gptr->nz-1, 
                  CNgrid4D_z(Gptr,Gptr->nz-1));
   for (k=0; k<2; k++)
   for (i=0; i<2; i++)
   for (j=0; j<2; j++) {
       i0 = i*(Gptr->nx-1);
       j0 = j*(Gptr->ny-1);
       k0 = k*(Gptr->nz-1);
       (void) fprintf(stdout,"   t[%2d][%2d][%2d] = %8.3g\n",
                      i0, j0, k0, CNgrid4D_t(Gptr,i0, j0, k0));
   }

   /* Print slice info */
   if (Gptr->slice) CNprint_slice(Gptr->slice);
}



/*
 * Array routines 
 */

/*
 * get the value of an array element
 */
static double get_1D_double_array_value(arrptr,i,isize)
double *arrptr;
int i,isize;
{
   double val;
   if (i<0 || i>=isize) {
      (void) fprintf(stderr,"Element [%d] is out of bounds!\n",i);
      return(0.0);
   }
   val = *(arrptr + i);
   return(val);
}
