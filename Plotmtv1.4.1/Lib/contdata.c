/*
 *  contdata.c  -  manipulate the contour data 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "CNplot.h"

static void convert_rect_to_rect();
static void convert_rect_to_rect2();
static void convert_rect_to_tria2();
static void convert_rect_to_tria4();
static void convert_rect_to_mat();
static void create_boundary_nodes();

/* 
 * The contour data is originally in a 3D list of triangles
 *   [ T0 (npts, pt0, pt1, pt2) ... TN (npts, pt0, pt1, pt2) ]
 * Go thru the contour data and rearrange it into a set of triangles
 *
 * This data is stored in a linked list, since there is there could be
 * multiple contour data sets (e.g. 2 sets of data from 2 files)
 *
 * xmin, xmax, ymin, ymax, zmin, zmax - bounds
 * triahead,triatail - pointers to linked list of triangles 
 * cstep  - the contour step size
 */
CNdatasetptr 
CNget_triangular_contour_data(filename, dataname,
                              xmin,xmax,ymin,ymax,zmin,zmax,
                              triahead,triatail,ID)
char      *filename;          /* Name of original file */
char      *dataname;          /* Data name             */
double    xmin,xmax;          /* Data boundaries (x)   */
double    ymin,ymax;          /* Data boundaries (y)   */
double    zmin,zmax;          /* Data boundaries (z)   */
CNtriaptr triahead,triatail;  /* List of triangles     */
int       ID;                 /* Data ID               */
{
   CNdatasetptr  CNmake_dataset();
   CNdatasetptr  dptr;

   /* 
    * Create and initialize a data structure to hold this contour data set
    * The boundary is the same as the viewport.
    */
   if ((dptr = CNmake_dataset(filename,dataname,CN_CONTOUR,
                              xmin,xmax,ymin,ymax,zmin,zmax,           
                              xmin,xmax,ymin,ymax,zmin,zmax,ID))==NULL)
      return(dptr);

   /*
    * Make sure the grid field is empty 
    */
   dptr->grid = NULL;

   /* 
    * put in zmin and zmax into the cmin/cmax fields
    */
   dptr->data_pr.cmin = zmin;
   dptr->data_pr.cmax = zmax;

   /*
    * Calculate the optimum stepsize (default method is CN_STEPSIZE)
    */
   dptr->data_pr.cstep = CNround_to_decimal((zmax-zmin)/(double)CN_IDLSTEPS);

   /*
    * Link the triangles to the data-structure 
    */
   dptr->triahead = triahead;
   dptr->triatail = triatail;

   /* 
    * Return the pointer 
    */
   return(dptr);
}


/* 
 * The contour data is originally in a 1D array
 *   [ [0,0]...[i,0]...[nx,0], [0,j]...[i,j]...[nx,ny] ]
 * Go thru the contour data and rearrange it into a set of rectangles/triangles
 * The contour data is in a non-uniform grid
 *
 * This data is stored in a dataset; the original 1D array (2D grid) is
 * discarded.
 */
CNdatasetptr 
CNget_rectilinear_contour_data(filename, dataname,
                               xgrid_arr, ygrid_arr, zgrid_arr, nx, ny,
                               joincurve,contintrp,ID)
char      *filename;          /* Name of original file */
char      *dataname;          /* Data name             */
double    *xgrid_arr;         /* 1D data array x[i]    */
double    *ygrid_arr;         /* 1D data array y[j]    */
double    *zgrid_arr;         /* 1D data array z(i,j)  */
int       nx, ny;             /* #x and y gridpoints   */
int       joincurve;          /* Contour interpolation */
int       contintrp;          /* Interpolate rectangle */
int       ID;                 /* Data ID               */
{
   CNdatasetptr  CNmake_dataset();
   CNdatasetptr  dptr;
   CNgrid4Dptr   grid;
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
   for (i=0; i<nx*ny; i++) {
      if (zgrid_arr[i] < zmin) zmin = zgrid_arr[i];
      if (zgrid_arr[i] > zmax) zmax = zgrid_arr[i];
   }

   /* 
    * Create and initialize a data structure to hold this contour dataset.
    * The boundary is the same as the viewport.
    */
   if ((dptr = CNmake_dataset(filename,dataname,CN_CONTOUR,
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
 
      return(NULL);

   } else {
      /* Fill in the grid */
      dptr->grid   = grid;
      grid->xarray = xgrid_arr;
      grid->yarray = ygrid_arr;
      grid->zarray = zgrid_arr;
      grid->nx     = nx;
      grid->ny     = ny;
      grid->nz     = nx*ny;
      grid->xmin   = xmin;
      grid->xmax   = xmax;
      grid->ymin   = ymin;
      grid->ymax   = ymax;
      grid->zmin   = zmin;
      grid->zmax   = zmax;
   }

   /* 
    * put in zmin and zmax into the cmin/cmax fields
    */
   dptr->data_pr.cmin = zmin;
   dptr->data_pr.cmax = zmax;

   /*
    * Calculate the optimum stepsize (default method is CN_STEPSIZE)
    */
   dptr->data_pr.cstep = CNround_to_decimal((zmax-zmin)/(double)CN_IDLSTEPS);

   /*
    * convert the rectangular arrays into a linked list of triangles/rects
    */
   CNpartition_rectilinear_data(
       &(dptr->pointhead), &(dptr->pointtail),
       &(dptr->nodehead),  &(dptr->nodetail),
       &(dptr->triahead),  &(dptr->triatail),
       &(dptr->recthead),  &(dptr->recttail),
       dptr->grid->xarray, dptr->grid->nx, dptr->grid->xmin, dptr->grid->xmax,
       dptr->grid->yarray, dptr->grid->ny, dptr->grid->ymin, dptr->grid->ymax,
       dptr->grid->zarray, dptr->grid->nz, dptr->grid->zmin, dptr->grid->zmax,
       contintrp, joincurve);

   /* 
    * Return the pointer 
    */
   return(dptr);
}


/*
 * Change the contour interpolation style 
 */
void CNreconstruct_contour_mesh(dptr)
CNdatasetptr dptr;
{
   int joincurve=CN_FALSE;

   /* Check for a valid dptr */
   if (dptr == NULL) {
      (void) fprintf(stderr, "***Error! Cannot contour NULL dataset\n");
      return;
   }
   if (dptr->datatype != CN_CONTOUR) {
      (void) fprintf(stderr,
                     "***Error! Cannot reinterpolate NON-CONTOUR dataset\n");
      return;
   }
   if (dptr->grid == NULL) {
      (void) fprintf(stderr, "***Error! No grid attached to dataset\n");
      return;
   }

   /* Delete the current triangles/points */
   CNdelete_rect_list (&(dptr->recthead) ,&(dptr->recttail));
   CNdelete_tria_list (&(dptr->triahead) ,&(dptr->triatail));
   CNdelete_node_list (&(dptr->nodehead) ,&(dptr->nodetail));
   CNdelete_point_list(&(dptr->pointhead),&(dptr->pointtail));

   /*
    * convert the rectangular arrays into a linked list of triangles/rects
    */
   CNpartition_rectilinear_data(
       &(dptr->pointhead), &(dptr->pointtail),
       &(dptr->nodehead),  &(dptr->nodetail),
       &(dptr->triahead),  &(dptr->triatail),
       &(dptr->recthead),  &(dptr->recttail),
       dptr->grid->xarray, dptr->grid->nx, dptr->grid->xmin, dptr->grid->xmax,
       dptr->grid->yarray, dptr->grid->ny, dptr->grid->ymin, dptr->grid->ymax,
       dptr->grid->zarray, dptr->grid->nz, dptr->grid->zmin, dptr->grid->zmax,
       (int) dptr->data_pr.contintrp, joincurve);


}

/*
 * Convert the arrays into points, nodes, triangles, rectangles
 */
void CNpartition_rectilinear_data(pointhead, pointtail,
                                  nodehead,  nodetail,
                                  triahead,  triatail,
                                  recthead,  recttail,
                                  xgrid_arr, nx, xmin, xmax,
                                  ygrid_arr, ny, ymin, ymax,
                                  zgrid_arr, nz, zmin, zmax,
                                  contintrp, joincurve)
CNpointptr *pointhead, *pointtail;
CNnodeptr  *nodehead,  *nodetail;
CNtriaptr  *triahead,  *triatail;
CNrectptr  *recthead,  *recttail;
double     *xgrid_arr, xmin, xmax;
double     *ygrid_arr, ymin, ymax;
double     *zgrid_arr, zmin, zmax;
int        nx, ny, nz;
int        contintrp, joincurve;
{
   /*
    * Check the array size - if there are more than 500x500 elements
    * then don't rectangularize or triangularize
    * 
    * Given nx*ny elements, get approx nx*ny rects, nx*ny nodes, nx*ny pts
    *   Point size ~= 40 bytes
    *   Node  size ~= 32 bytes
    *   Rect  size ~= 40 bytes
    * Therefore nx*ny*112 bytes = total size required
    */
   if (nx*ny*(sizeof(CNpoint)+sizeof(CNnode)+sizeof(CNrect)) > 64000000) {
      (void) fprintf(stdout,"Warning! Too many points in the array!\n");
      (void) fprintf(stdout,"Rectangularization failed...!\n");
      return;
   }

   /*
    * convert the rectangular array into a linked list of triangles/rects
    */
   if (contintrp == CN_RECT2TRIA)
      /* Two triangles to a rectangle */
      convert_rect_to_tria2(pointhead, pointtail,
                            nodehead,  nodetail,
                            triahead,  triatail,
                            recthead,  recttail,
                            xgrid_arr, nx, xmin, xmax,
                            ygrid_arr, ny, ymin, ymax,
                            zgrid_arr, nz, zmin, zmax,
                            joincurve);

   else if (contintrp == CN_RECT4TRIA)
      /* Four triangles to a rectangle */
      convert_rect_to_tria4(pointhead, pointtail,
                            nodehead,  nodetail,
                            triahead,  triatail,
                            recthead,  recttail,
                            xgrid_arr, nx, xmin, xmax,
                            ygrid_arr, ny, ymin, ymax,
                            zgrid_arr, nz, zmin, zmax,
                            joincurve);

   else if (contintrp == CN_RECTFLAT)
      /* Put the nodes in flat rectangles */
      convert_rect_to_rect2(pointhead, pointtail,
                            nodehead,  nodetail,
                            triahead,  triatail,
                            recthead,  recttail,
                            xgrid_arr, nx, xmin, xmax,
                            ygrid_arr, ny, ymin, ymax,
                            zgrid_arr, nz, zmin, zmax,
                            joincurve);

   else if (contintrp == CN_RECT2MAT)
      /* Put the nodes in material-dependent rectangles */
      convert_rect_to_mat(pointhead, pointtail,
                            nodehead,  nodetail,
                            triahead,  triatail,
                            recthead,  recttail,
                            xgrid_arr, nx, xmin, xmax,
                            ygrid_arr, ny, ymin, ymax,
                            zgrid_arr, nz, zmin, zmax,
                            joincurve);

   else 
      /* Put the nodes in a rectangle */
      convert_rect_to_rect (pointhead, pointtail,
                            nodehead,  nodetail,
                            triahead,  triatail,
                            recthead,  recttail,
                            xgrid_arr, nx, xmin, xmax,
                            ygrid_arr, ny, ymin, ymax,
                            zgrid_arr, nz, zmin, zmax,
                            joincurve);
}


/*
 * convert the rectangular array into a linked list of rectangles
 * A rectangle contains 4 nodes.
 * Each node contains 1 point.
 * So the dataset contains a list of points, a list of nodes and a
 * list of rectangles. In this case, there is a one-to-one mapping
 * between nodes, but in the more general case, several nodes could
 * map to a single point.
 */
/*ARGSUSED*/
static void convert_rect_to_rect(pointhead, pointtail,
                                 nodehead,  nodetail,
                                 triahead,  triatail,
                                 recthead,  recttail,
                                 xgrid_arr, nx, xmin, xmax,
                                 ygrid_arr, ny, ymin, ymax,
                                 zgrid_arr, nz, zmin, zmax,
                                 joincurve)
CNpointptr *pointhead, *pointtail;
CNnodeptr  *nodehead,  *nodetail;
CNtriaptr  *triahead,  *triatail;
CNrectptr  *recthead,  *recttail;
double     *xgrid_arr, xmin, xmax;    /* 1D data array x[i]     */
double     *ygrid_arr, ymin, ymax;    /* 1D data array y[j]     */
double     *zgrid_arr, zmin, zmax;    /* 1D data array z[i,j]   */
int        nx, ny, nz;                /* #x and y gridpoints    */
int        joincurve;                 /* Boundary interpolation */
{
   CNpointptr pt;
   CNnodeptr *ndarr;
   CNnodeptr  nd, nd00, nd01, nd10, nd11;

   double     x, y, z, t;
   int        i, j, istart, iend, jstart, jend;
   int        ptID=0, ndID=0, rtID=0;

   /* 
    * If the contours are to be joined at the boundary, then
    * there must be a boundary layer around the contour box.
    * Create a bunch of rectangles filling up this boundary layer.
    * 
    * The array normally goes from x[1]..x[nx]
    * Boundary values are          x[0], x[nx+1]
    */
   if (joincurve != CN_NONE) {
      istart = 0;
      jstart = 0;
      iend   = nx+2;
      jend   = ny+2;
   } else {
      istart = 1;
      jstart = 1;
      iend   = nx+1;
      jend   = ny+1;
   }

   /* create node array */
   ndarr = CNcreate_nodeptr_array(nx+2,ny+2);

   /* create a list of nodes and points */
   for (i=1; i<nx+1; i++)
   for (j=1; j<ny+1; j++) {
      x  = CNget_1D_double_array_value(xgrid_arr,(i-1),nx);
      y  = CNget_1D_double_array_value(ygrid_arr,(j-1),ny);
      z  = 0.0;
      t  = CNget_1D_double_array_value(zgrid_arr,((i-1)+(j-1)*nx),nx*ny);
      pt = CNinsert_point(pointhead,pointtail,x,y,z,ptID++);
      nd = CNinsert_tailnode(nodehead,nodetail,pt,t,ndID++);
      CNset_nodeptr_array_value(ndarr,i,j,nx+2,ny+2,&nd);
   }

   /* create a list of nodes at the boundary */
   if (joincurve != CN_NONE)
      create_boundary_nodes(ndarr,
                            nodehead,nodetail,&ndID,
                            pointhead,pointtail,&ptID,
                            xmin,xmax,ymin,ymax,zmin,zmax,
                            xgrid_arr,ygrid_arr,nx,ny,joincurve);

   /* create a list of rectangles */
   for (i=istart; i<iend-1; i++)
   for (j=jstart; j<jend-1; j++) {
      nd00 = CNget_nodeptr_array_value(ndarr,i  ,j  ,nx+2,ny+2);
      nd01 = CNget_nodeptr_array_value(ndarr,i  ,j+1,nx+2,ny+2);
      nd10 = CNget_nodeptr_array_value(ndarr,i+1,j  ,nx+2,ny+2);
      nd11 = CNget_nodeptr_array_value(ndarr,i+1,j+1,nx+2,ny+2);
      (void) CNinsert_rect(recthead,recttail,
                           nd00,nd01,nd11,nd10,rtID++);
   }

   /* free the temp arrays */
   CNfree_nodeptr_array(ndarr);
}


/*
 * convert the rectangular array into a linked list of flat rectangles
 * The rectangle is actually a z=constant plane surrounding the x-y point
 * A rectangle contains 4 nodes - each with the same t-value.
 * Each node contains 1 point.
 * So the dataset contains a list of points, a list of nodes and a
 * list of rectangles. In this case, there is a one-to-one mapping
 * between nodes, but in the more general case, several nodes could
 * map to a single point.
 */
/*ARGSUSED*/
static void convert_rect_to_rect2(pointhead, pointtail,
                                  nodehead,  nodetail,
                                  triahead,  triatail,
                                  recthead,  recttail,
                                  xgrid_arr, nx, xmin, xmax,
                                  ygrid_arr, ny, ymin, ymax,
                                  zgrid_arr, nz, zmin, zmax,
                                  joincurve)
CNpointptr *pointhead, *pointtail;
CNnodeptr  *nodehead,  *nodetail;
CNtriaptr  *triahead,  *triatail;
CNrectptr  *recthead,  *recttail;
double     *xgrid_arr, xmin, xmax;    /* 1D data array x[i]     */
double     *ygrid_arr, ymin, ymax;    /* 1D data array y[j]     */
double     *zgrid_arr, zmin, zmax;    /* 1D data array z[i,j]   */
int        nx, ny, nz;                /* #x and y gridpoints    */
int        joincurve;                 /* Boundary interpolation */
{
   CNpointptr pt;
   CNpointptr *ptarr;
   CNnodeptr  nd00, nd01, nd10, nd11;

   double     xc, yc, xl, yl, x, y, z, t;
   int        i, j;
   int        ptID=0, ndID=0, rtID=0;

   /* 
    * No boundary - joincurve is ignored
    */

   /* create point array */
   ptarr = CNcreate_pointptr_array(nx+1,ny+1);

   /* create a list of nodes and points */
   for (i=0; i<=nx; i++)
   for (j=0; j<=ny; j++) {
      /* Center of the rect */
      if (i==nx)
      xc = CNget_1D_double_array_value(xgrid_arr,i-1,nx);
      else
      xc = CNget_1D_double_array_value(xgrid_arr,i,nx);
      if (j==ny)
      yc = CNget_1D_double_array_value(ygrid_arr,j-1,ny);
      else
      yc = CNget_1D_double_array_value(ygrid_arr,j,ny);

      /* Lower left corner */
      xl = (i<=0) ? xc : CNget_1D_double_array_value(xgrid_arr,(i-1),nx);
      yl = (j<=0) ? yc : CNget_1D_double_array_value(ygrid_arr,(j-1),ny);

      x  = xc - 0.5*(xc-xl);
      y  = yc - 0.5*(yc-yl);
      z  = 0.0;
      pt = CNinsert_point(pointhead,pointtail,x,y,z,ptID++);
      CNset_pointptr_array_value(ptarr,i,j,nx+1,ny+1,&pt);
   }

   /* create a list of nodes and rectangles */
   for (i=0; i<nx; i++)
   for (j=0; j<ny; j++) {
      /* All 4 nodes share same t-value */
      t    = CNget_1D_double_array_value(zgrid_arr,(i+j*nx),nx*ny);

      /* 4 nodes */
      pt   = CNget_pointptr_array_value(ptarr,i  ,j  ,nx+1,ny+1);
      nd00 = CNinsert_tailnode(nodehead,nodetail,pt,t,ndID++);
      pt   = CNget_pointptr_array_value(ptarr,i  ,j+1,nx+1,ny+1);
      nd01 = CNinsert_tailnode(nodehead,nodetail,pt,t,ndID++);
      pt   = CNget_pointptr_array_value(ptarr,i+1,j  ,nx+1,ny+1);
      nd10 = CNinsert_tailnode(nodehead,nodetail,pt,t,ndID++);
      pt   = CNget_pointptr_array_value(ptarr,i+1,j+1,nx+1,ny+1);
      nd11 = CNinsert_tailnode(nodehead,nodetail,pt,t,ndID++);

      /* Create the rectangle */
      (void) CNinsert_rect(recthead,recttail,
                           nd00,nd01,nd11,nd10,rtID++);
   }

   /* free the temp arrays */
   CNfree_pointptr_array(ptarr);
}


/*
 * convert the rectangular array into a linked list of triangles
 * Each rectangle is split into 2 triangles
 */
/*ARGSUSED*/
static void convert_rect_to_tria2(pointhead, pointtail,
                                  nodehead,  nodetail,
                                  triahead,  triatail,
                                  recthead,  recttail,
                                  xgrid_arr, nx, xmin, xmax,
                                  ygrid_arr, ny, ymin, ymax,
                                  zgrid_arr, nz, zmin, zmax,
                                  joincurve)
CNpointptr *pointhead, *pointtail;
CNnodeptr  *nodehead,  *nodetail;
CNtriaptr  *triahead,  *triatail;
CNrectptr  *recthead,  *recttail;
double     *xgrid_arr, xmin, xmax;    /* 1D data array x[i]     */
double     *ygrid_arr, ymin, ymax;    /* 1D data array y[j]     */
double     *zgrid_arr, zmin, zmax;    /* 1D data array z[i,j]   */
int        nx, ny, nz;                /* #x and y gridpoints    */
int        joincurve;                 /* Boundary interpolation */
{
   CNpointptr pt;
   CNnodeptr *ndarr;
   CNnodeptr  nd, nd00, nd01, nd10, nd11;

   double     x,y,z,t;
   int        i, j, istart, iend, jstart, jend;
   int        ptID=0, ndID=0, trID=0;

   /* 
    * If the contours are to be joined at the boundary, then
    * there must be a boundary layer around the contour box.
    * Create a bunch of triangles filling up this boundary layer.
    * 
    * The array normally goes from x[1]..x[nx]
    * Boundary values are          x[0], x[nx+1]
    */
   if (joincurve != CN_NONE) {
      istart = 0;
      jstart = 0;
      iend   = nx+2;
      jend   = ny+2;
   } else {
      istart = 1;
      jstart = 1;
      iend   = nx+1;
      jend   = ny+1;
   }

   /* create node array */
   ndarr = CNcreate_nodeptr_array(nx+2,ny+2);

   /* create a list of nodes */
   for (i=1; i<nx+1; i++)
   for (j=1; j<ny+1; j++) {
      x  = CNget_1D_double_array_value(xgrid_arr,(i-1),nx);
      y  = CNget_1D_double_array_value(ygrid_arr,(j-1),ny);
      z  = 0.0;
      t  = CNget_1D_double_array_value(zgrid_arr,((i-1)+(j-1)*nx),nx*ny);
      pt = CNinsert_point(pointhead,pointtail,x,y,z,ptID++);
      nd = CNinsert_tailnode(nodehead,nodetail,pt,t,ndID++);
      CNset_nodeptr_array_value(ndarr,i,j,nx+2,ny+2,&nd);
   }

   /* create a list of nodes at the boundary */
   if (joincurve != CN_NONE)
      create_boundary_nodes(ndarr,
                            nodehead,nodetail,&ndID,
                            pointhead,pointtail,&ptID,
                            xmin,xmax,ymin,ymax,zmin,zmax,
                            xgrid_arr,ygrid_arr,nx,ny,joincurve);

   /* create a list of triangles */
   for (i=istart; i<iend-1; i++)
   for (j=jstart; j<jend-1; j++) {
      nd00 = CNget_nodeptr_array_value(ndarr,i  ,j  ,nx+2,ny+2);
      nd01 = CNget_nodeptr_array_value(ndarr,i  ,j+1,nx+2,ny+2);
      nd10 = CNget_nodeptr_array_value(ndarr,i+1,j  ,nx+2,ny+2);
      nd11 = CNget_nodeptr_array_value(ndarr,i+1,j+1,nx+2,ny+2);
      (void) CNinsert_tria(triahead,triatail,
                           nd00,nd01,nd10,0,trID++);
      (void) CNinsert_tria(triahead,triatail,
                           nd11,nd01,nd10,0,trID++);
   }

   /* free the temp arrays */
   CNfree_nodeptr_array(ndarr);
}


/*
 * convert the rectangular array into a linked list of triangles
 * Each rectangle is split into 4 triangles
 */
/*ARGSUSED*/
static void convert_rect_to_tria4(pointhead, pointtail,
                                  nodehead,  nodetail,
                                  triahead,  triatail,
                                  recthead,  recttail,
                                  xgrid_arr, nx, xmin, xmax,
                                  ygrid_arr, ny, ymin, ymax,
                                  zgrid_arr, nz, zmin, zmax,
                                  joincurve)
CNpointptr *pointhead, *pointtail;
CNnodeptr  *nodehead,  *nodetail;
CNtriaptr  *triahead,  *triatail;
CNrectptr  *recthead,  *recttail;
double     *xgrid_arr, xmin, xmax;    /* 1D data array x[i]     */
double     *ygrid_arr, ymin, ymax;    /* 1D data array y[j]     */
double     *zgrid_arr, zmin, zmax;    /* 1D data array z[i,j]   */
int        nx, ny, nz;                /* #x and y gridpoints    */
int        joincurve;                 /* Boundary interpolation */
{
   CNpointptr pt;
   CNnodeptr *ndarr, *ndmarr;
   CNnodeptr  nd00, nd01, nd10, nd11, nd, ndmp;

   double     x, y, z, t;
   int        i, j, istart, iend, jstart, jend;
   int        ptID=0, ndID=0, trID=0;

   /* 
    * If the contours are to be joined at the boundary, then
    * there must be a boundary layer around the contour box.
    * Create a bunch of triangles filling up this boundary layer.
    * 
    * The array normally goes from x[1]..x[nx]
    * Boundary values are          x[0], x[nx+1]
    */
   if (joincurve != CN_NONE) {
      istart = 0;
      jstart = 0;
      iend   = nx+2;
      jend   = ny+2;
   } else {
      istart = 1;
      jstart = 1;
      iend   = nx+1;
      jend   = ny+1;
   }

   /* create node array */
   ndarr   = CNcreate_nodeptr_array(nx+2,ny+2);
   ndmarr  = CNcreate_nodeptr_array(nx+2,ny+2);

   /* create a list of nodes at the grid intersections */
   for (i=1; i<nx+1; i++)
   for (j=1; j<ny+1; j++) {
      x  = CNget_1D_double_array_value(xgrid_arr,(i-1),nx);
      y  = CNget_1D_double_array_value(ygrid_arr,(j-1),ny);
      z  = 0.0;
      t  = CNget_1D_double_array_value(zgrid_arr,((i-1)+(j-1)*nx),nx*ny);
      pt = CNinsert_point(pointhead,pointtail,x,y,z,ptID++);
      nd = CNinsert_tailnode(nodehead,nodetail,pt,t,ndID++);
      CNset_nodeptr_array_value(ndarr,i,j,nx+2,ny+2,&nd);
   }

   /* create a list of nodes at the boundary */
   if (joincurve != CN_NONE)
      create_boundary_nodes(ndarr,
                            nodehead,nodetail,&ndID,
                            pointhead,pointtail,&ptID,
                            xmin,xmax,ymin,ymax,zmin,zmax,
                            xgrid_arr,ygrid_arr,nx,ny,joincurve);

   /* create a list of nodes at the grid midpoints */
   for (i=istart; i<iend-1; i++)
   for (j=jstart; j<jend-1; j++) {
      nd00 = CNget_nodeptr_array_value(ndarr,i  ,j  ,nx+2,ny+2);
      nd01 = CNget_nodeptr_array_value(ndarr,i  ,j+1,nx+2,ny+2);
      nd10 = CNget_nodeptr_array_value(ndarr,i+1,j  ,nx+2,ny+2);
      nd11 = CNget_nodeptr_array_value(ndarr,i+1,j+1,nx+2,ny+2);
      x  = 0.50*(nd00->coord->x + nd10->coord->x);
      y  = 0.50*(nd00->coord->y + nd01->coord->y);
      z  = 0.25*(nd00->coord->z + nd01->coord->z + 
                 nd10->coord->z + nd11->coord->z);
      t  = 0.25*(nd00->t        + nd01->t        + 
                 nd10->t        + nd11->t       );
      pt = CNinsert_point(pointhead,pointtail,x,y,z,ptID++);
      nd = CNinsert_tailnode(nodehead,nodetail,pt,t,ndID++);
      CNset_nodeptr_array_value(ndmarr,i,j,nx+2,ny+2,&nd);
   }

   /* create a list of triangles */
   for (i=istart; i<iend-1; i++)
   for (j=jstart; j<jend-1; j++) {
      ndmp = CNget_nodeptr_array_value(ndmarr,i  ,j  ,nx+2,ny+2);
      nd00 = CNget_nodeptr_array_value(ndarr, i  ,j  ,nx+2,ny+2);
      nd01 = CNget_nodeptr_array_value(ndarr, i  ,j+1,nx+2,ny+2);
      nd10 = CNget_nodeptr_array_value(ndarr, i+1,j  ,nx+2,ny+2);
      nd11 = CNget_nodeptr_array_value(ndarr, i+1,j+1,nx+2,ny+2);
      (void) CNinsert_tria(triahead,triatail,
                           nd00,nd10,ndmp,0,trID++);
      (void) CNinsert_tria(triahead,triatail,
                           nd10,nd11,ndmp,0,trID++);
      (void) CNinsert_tria(triahead,triatail,
                           nd11,nd01,ndmp,0,trID++);
      (void) CNinsert_tria(triahead,triatail,
                           nd01,nd00,ndmp,0,trID++);
   }

   /* free the temp arrays */
   CNfree_nodeptr_array(ndarr);
   CNfree_nodeptr_array(ndmarr);
}


/* create a list of nodes and points at the boundary */
static void create_boundary_nodes(ndarr,
                                  nodehead,nodetail,ndID,
                                  pointhead,pointtail,ptID,
                                  xmin,xmax,ymin,ymax,zmin,zmax,
                                  xgrid_arr, ygrid_arr,
                                  nx, ny, joincurve)
CNnodeptr  *ndarr, *nodehead, *nodetail;
CNpointptr *pointhead, *pointtail;
int        *ndID, *ptID;
double     xmin, xmax, ymin, ymax, zmax, zmin;
double     *xgrid_arr;         /* 1D data array x[i]    */
double     *ygrid_arr;         /* 1D data array y[j]    */
int        nx, ny, joincurve;
{
   CNnodeptr  nd;
   CNpointptr pt; 
   double     dx, dy, deltax, deltay, ttarget;
   double     x, y, z, t;
   int        i, j;

   /* Unit grid size */
   dx   = (xmax-xmin)/(double)(nx-1);
   dy   = (ymax-ymin)/(double)(ny-1);

   /* determine if the boundary layer is a min or a maxima */
   if (joincurve == CN_HIGH)
      ttarget = zmax;
   else
      ttarget = zmin;

   /* The width of the boundary */
   deltax = deltay = 1.0e-3*((dx > dy) ? dy : dx);
   if (xgrid_arr[0] > xgrid_arr[nx-1]) deltax = -1*deltax;
   if (ygrid_arr[0] > ygrid_arr[ny-1]) deltay = -1*deltay;

   /* fill a boundary layer for i=0/nx+1, variable j */
   for (j=0; j<ny+2; j++) {
      if      (j==0   ) y = ygrid_arr[0] - deltay;
      else if (j==ny+1) y = ygrid_arr[ny-1] + deltay;
      else              y = ygrid_arr[j-1];

      i  = 0;
      x  = xgrid_arr[0] - deltax;
      z  = 0.0;
      t  = ttarget;
      pt = CNinsert_point(pointhead,pointtail,x,y,z,(*ptID)++);
      nd = CNinsert_tailnode(nodehead,nodetail,pt,t,(*ndID)++);
      CNset_nodeptr_array_value(ndarr,i,j,nx+2,ny+2,&nd);

      i  = nx+1;
      x  = xgrid_arr[nx-1] + deltax;
      z  = 0.0;
      t  = ttarget;
      pt = CNinsert_point(pointhead,pointtail,x,y,z,(*ptID)++);
      nd = CNinsert_tailnode(nodehead,nodetail,pt,t,(*ndID)++);
      CNset_nodeptr_array_value(ndarr,i,j,nx+2,ny+2,&nd);
   }

   /* fill a boundary layer for j=0/ny+1, variable i */
   for (i=1; i<nx+1; i++) {
      j  = 0;
      x  = xgrid_arr[i-1];
      y  = ygrid_arr[0] - deltay; 
      z  = 0.0;
      t  = ttarget;
      pt = CNinsert_point(pointhead,pointtail,x,y,z,(*ptID)++);
      nd = CNinsert_tailnode(nodehead,nodetail,pt,t,(*ndID)++);
      CNset_nodeptr_array_value(ndarr,i,j,nx+2,ny+2,&nd);

      j  = ny+1;
      x  = xgrid_arr[i-1];
      y  = ygrid_arr[ny-1] + deltay;
      z  = 0.0;
      t  = ttarget;
      pt = CNinsert_point(pointhead,pointtail,x,y,z,(*ptID)++);
      nd = CNinsert_tailnode(nodehead,nodetail,pt,t,(*ndID)++);
      CNset_nodeptr_array_value(ndarr,i,j,nx+2,ny+2,&nd);
   }
}


/*
 * convert the rectangular array into a linked list of flat rectangles
 * or flat triangles.  Each rectangle is divided up into a rectangle
 * or 2 triangles depending on the integer values of the node t-values.
 */
/*ARGSUSED*/
static void convert_rect_to_mat(pointhead, pointtail,
                                  nodehead,  nodetail,
                                  triahead,  triatail,
                                  recthead,  recttail,
                                  xgrid_arr, nx, xmin, xmax,
                                  ygrid_arr, ny, ymin, ymax,
                                  zgrid_arr, nz, zmin, zmax,
                                  joincurve)
CNpointptr *pointhead, *pointtail;
CNnodeptr  *nodehead,  *nodetail;
CNtriaptr  *triahead,  *triatail;
CNrectptr  *recthead,  *recttail;
double     *xgrid_arr, xmin, xmax;    /* 1D data array x[i]     */
double     *ygrid_arr, ymin, ymax;    /* 1D data array y[j]     */
double     *zgrid_arr, zmin, zmax;    /* 1D data array z[i,j]   */
int        nx, ny, nz;                /* #x and y gridpoints    */
int        joincurve;                 /* Boundary interpolation */
{
   CNpointptr pt;
   CNpointptr *ptarr;
   CNnodeptr  nd00, nd01, nd10, nd11;

   double     x, y, z, t;
   double     t00, t01, t10, t11;
   int        i00, i01, i10, i11;
   int        ierr=0;
   int        i, j;
   int        ptID=0, ndID=0, trID=0, rtID=0;
   int        MATCH_FOUND;

   /* 
    * No boundary - joincurve is ignored
    */

   /* create point array */
   ptarr = CNcreate_pointptr_array(nx,ny);

   /* create a list of points */
   for (i=0; i<nx; i++)
   for (j=0; j<ny; j++) {
      x  = CNget_1D_double_array_value(xgrid_arr,i,nx);
      y  = CNget_1D_double_array_value(ygrid_arr,j,ny);
      z  = 0.0;
      t  = CNget_1D_double_array_value(zgrid_arr,i+j*nx,nx*ny);
      pt = CNinsert_point(pointhead,pointtail,x,y,z,ptID++);
      CNset_pointptr_array_value(ptarr,i,j,nx,ny,&pt);
   }

   /* create a list of nodes and rectangles */
   for (i=0; i<nx-1; i++)
   for (j=0; j<ny-1; j++) {
      /* Get the t-values for the 4 nodes */
      t00  = CNget_1D_double_array_value(zgrid_arr,((i  )+(j  )*nx),nx*ny);
      t01  = CNget_1D_double_array_value(zgrid_arr,((i  )+(j+1)*nx),nx*ny);
      t10  = CNget_1D_double_array_value(zgrid_arr,((i+1)+(j  )*nx),nx*ny);
      t11  = CNget_1D_double_array_value(zgrid_arr,((i+1)+(j+1)*nx),nx*ny);

      /* Convert to integers */
      i00  = (int)fabs(t00);
      i01  = (int)fabs(t01);
      i10  = (int)fabs(t10);
      i11  = (int)fabs(t11);

      /* Reset match flag */
      MATCH_FOUND = CN_FALSE;

      /* If all 4 nodes have a common bit, then there is a single material */
      if ((i00 & i01 & i10 & i11) > 0) {

         /* Create a single (flat) rectangles with one value */
         t    = (double)(i00 & i01 & i10 & i11);

         /* Create 4 nodes */
         pt   = CNget_pointptr_array_value(ptarr,i  ,j  ,nx,ny);
         nd00 = CNinsert_tailnode(nodehead,nodetail,pt,t,ndID++);
         pt   = CNget_pointptr_array_value(ptarr,i  ,j+1,nx,ny);
         nd01 = CNinsert_tailnode(nodehead,nodetail,pt,t,ndID++);
         pt   = CNget_pointptr_array_value(ptarr,i+1,j  ,nx,ny);
         nd10 = CNinsert_tailnode(nodehead,nodetail,pt,t,ndID++);
         pt   = CNget_pointptr_array_value(ptarr,i+1,j+1,nx,ny);
         nd11 = CNinsert_tailnode(nodehead,nodetail,pt,t,ndID++);

         /* Create the rectangle */
         (void) CNinsert_rect(recthead,recttail,
                              nd00,nd01,nd11,nd10,rtID++);
         MATCH_FOUND = CN_TRUE;

      } else if ( (i00==0 && i01==0 && i10==0 && i11==0) ||
                  (i00==0 && i01==0) ||
                  (i01==0 && i11==0) ||
                  (i11==0 && i10==0) ||
                  (i10==0 && i00==0) ||
                  (i00==0 && (i01 & i10 & i11)==0) || 
                  (i01==0 && (i00 & i10 & i11)==0) || 
                  (i10==0 && (i00 & i01 & i11)==0) || 
                  (i11==0 && (i00 & i01 & i10)==0) ) {

         /* 
          * Empty material - fill it with -1 
          * This occurs if 
          *  (a) all four nodes have 0 values or 
          *  (b) 2 adjacent node have 0 values 
          *  (c) one node has 0 and the other 3 don't have a matching mat
          */

         /* Create a single (flat) rectangles with one value */
         t    = -1.0;
 
         /* Create 4 nodes */
         pt   = CNget_pointptr_array_value(ptarr,i  ,j  ,nx,ny);
         nd00 = CNinsert_tailnode(nodehead,nodetail,pt,t,ndID++);
         pt   = CNget_pointptr_array_value(ptarr,i  ,j+1,nx,ny);
         nd01 = CNinsert_tailnode(nodehead,nodetail,pt,t,ndID++);
         pt   = CNget_pointptr_array_value(ptarr,i+1,j  ,nx,ny);
         nd10 = CNinsert_tailnode(nodehead,nodetail,pt,t,ndID++);
         pt   = CNget_pointptr_array_value(ptarr,i+1,j+1,nx,ny);
         nd11 = CNinsert_tailnode(nodehead,nodetail,pt,t,ndID++);
 
         /* Create the rectangle */
         (void) CNinsert_rect(recthead,recttail,
                              nd00,nd01,nd11,nd10,rtID++);
         MATCH_FOUND = CN_TRUE;

      } else {

         /* OK, there are probably 2 or more materials in this rectangle */
         /* Try out 2 different triangle orientations      
          *
          *  01     11        01    11
          *   -------         -------
          *   |\    |         |    /|
          *   | \   |         |   / |
          *   |  \  |         |  /  |
          *   |   \ |         | /   |
          *   |    \|         |/    |
          *   -------         -------
          *  00     10        00    10
          */


         /* Try the first orientation */
         if ( ((i00 & i10 & i11) > 0) || ((i00 & i01 & i11) > 0) ) {

            /* First triangle */
            if ( ((i00 & i10 & i11) > 0) || (i00==0 || i10==0 || i11==0)) {
               t    = (double) (i00 & i10 & i11);
               if (i00==0 || i10==0 || i11==0) t = -1.0;

               /* Create 3 nodes */
               pt   = CNget_pointptr_array_value(ptarr,i  ,j  ,nx,ny);
               nd00 = CNinsert_tailnode(nodehead,nodetail,pt,t,ndID++);
               pt   = CNget_pointptr_array_value(ptarr,i+1,j  ,nx,ny);
               nd10 = CNinsert_tailnode(nodehead,nodetail,pt,t,ndID++);
               pt   = CNget_pointptr_array_value(ptarr,i+1,j+1,nx,ny);
               nd11 = CNinsert_tailnode(nodehead,nodetail,pt,t,ndID++);

               /* Create a triangle */
               (void) CNinsert_tria(triahead,triatail,
                                    nd00,nd10,nd11,0,trID++);
               MATCH_FOUND = CN_TRUE;
            }

            /* Second triangle */
            if ( ((i00 & i01 & i11) > 0) || (i00==0 || i01==0 || i11==0)) {
               t    = (double) (i00 & i01 & i11);
               if (i00==0 || i01==0 || i11==0) t = -1.0;

               /* Create 3 nodes */
               pt   = CNget_pointptr_array_value(ptarr,i  ,j  ,nx,ny);
               nd00 = CNinsert_tailnode(nodehead,nodetail,pt,t,ndID++);
               pt   = CNget_pointptr_array_value(ptarr,i  ,j+1,nx,ny);
               nd01 = CNinsert_tailnode(nodehead,nodetail,pt,t,ndID++);
               pt   = CNget_pointptr_array_value(ptarr,i+1,j+1,nx,ny);
               nd11 = CNinsert_tailnode(nodehead,nodetail,pt,t,ndID++);

               /* Create a triangle */
               (void) CNinsert_tria(triahead,triatail,
                                    nd00,nd01,nd11,0,trID++);
               MATCH_FOUND = CN_TRUE;
            }

         /* Try another orientation */
         } else if ( ((i01 & i00 & i10) > 0) || ((i01 & i11 & i10) > 0) ) {

            /* First triangle */
            if ( ((i01 & i00 & i10) > 0) || (i01==0 || i00==0 || i10==0)) {
               t    = (double) (i01 & i00 & i10);
               if (i01==0 || i00==0 || i10==0) t = -1.0;

               /* Create 3 nodes */
               pt   = CNget_pointptr_array_value(ptarr,i  ,j  ,nx,ny);
               nd00 = CNinsert_tailnode(nodehead,nodetail,pt,t,ndID++);
               pt   = CNget_pointptr_array_value(ptarr,i  ,j+1,nx,ny);
               nd01 = CNinsert_tailnode(nodehead,nodetail,pt,t,ndID++);
               pt   = CNget_pointptr_array_value(ptarr,i+1,j  ,nx,ny);
               nd10 = CNinsert_tailnode(nodehead,nodetail,pt,t,ndID++);

               /* Create a triangle */
               (void) CNinsert_tria(triahead,triatail,
                                    nd01,nd00,nd10,0,trID++);
               MATCH_FOUND = CN_TRUE;
            }

            /* Second triangle */
            if ( ((i01 & i11 & i10) > 0) || (i01==0 || i11==0 || i10==0)) {
               t    = (double) (i01 & i11 & i10);
               if (i01==0 || i11==0 || i10==0) t = -1.0;

               /* Create 3 nodes */
               pt   = CNget_pointptr_array_value(ptarr,i  ,j+1,nx,ny);
               nd01 = CNinsert_tailnode(nodehead,nodetail,pt,t,ndID++);
               pt   = CNget_pointptr_array_value(ptarr,i+1,j  ,nx,ny);
               nd10 = CNinsert_tailnode(nodehead,nodetail,pt,t,ndID++);
               pt   = CNget_pointptr_array_value(ptarr,i+1,j+1,nx,ny);
               nd11 = CNinsert_tailnode(nodehead,nodetail,pt,t,ndID++);

               /* Create a triangle */
               (void) CNinsert_tria(triahead,triatail,
                                    nd01,nd11,nd10,0,trID++);
               MATCH_FOUND = CN_TRUE;
            }
         }
      }

      if (!MATCH_FOUND) {
         ierr++;
         (void) fprintf(stderr,"Unable to convert to tria/rect!\n");
         (void) fprintf(stderr,"  p00.t = %7.4f (%4d)\n",t00,i00);
         (void) fprintf(stderr,"  p01.t = %7.4f (%4d)\n",t01,i01);
         (void) fprintf(stderr,"  p10.t = %7.4f (%4d)\n",t10,i10);
         (void) fprintf(stderr,"  p11.t = %7.4f (%4d)\n",t11,i11);
      }
   }
 
   /* free the temp arrays */
   CNfree_pointptr_array(ptarr);

   if (ierr > 0) {
      (void) fprintf(stderr,"rect_bit_conv - %d errors encountered!\n",ierr);
      (void) fprintf(stderr,
          "Only managed to convert %d triangles %d rectangles\n",trID,rtID);
   }
}


