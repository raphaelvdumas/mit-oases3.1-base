/*
 * CNgrid.h - definitions for a grid
 *
 * This file requires CNdatatypes.h
 */

#ifndef CNgrid_defined
#define CNgrid_defined

/*
 *    A slice is a collection of rectangles, points and nodes 
 */
/* Slice */
typedef struct CNslice_strct {
   short  slice_plane;
   double slice_value;
   double xmin, xmax;
   double ymin, ymax;
   double zmin, zmax;
   double tmin, tmax;
   double *xarray;
   double *yarray;
   double *zarray;
   int    nx;
   int    ny;
   int    nz;
   struct CNpoint_strct   *pointhead;   /* The points of tria/rectangles */
   struct CNpoint_strct   *pointtail;
   struct CNnode_strct    *nodehead;    /* The nodes of tria/rectangles */
   struct CNnode_strct    *nodetail;
   struct CNtria_strct    *triahead;    /* The triangles in the mesh    */
   struct CNtria_strct    *triatail;
   struct CNrect_strct    *recthead;    /* The rectangular mesh         */
   struct CNrect_strct    *recttail;
} CNslice;
typedef struct CNslice_strct *CNsliceptr;

/*
 *    A grid is a 4D volumetric grid consisting of x,y,z and t arrays. 
 *    A slice is associated with a grid
 */

/* 4D grid */
typedef struct CNgrid4D_strct {
   short  ID;
   short  flag;
   double *xarray;
   double *yarray;
   double *zarray;
   double *tarray;
   int    nx;
   int    ny;
   int    nz;
   int    nt;
   double xmin, xmax;
   double ymin, ymax;
   double zmin, zmax;
   double tmin, tmax;
   struct CNslice_strct *slice;   /* Pointer to a slice */
} CNgrid4D;
typedef struct CNgrid4D_strct *CNgrid4Dptr;

extern CNsliceptr   CNmake_slice();
extern void         CNdelete_slice();
extern void         CNprint_slice();
extern char        *CNsliceplane();

extern CNgrid4Dptr  CNmake_grid4D();
extern void         CNdelete_grid4D();
extern double       CNgrid4D_x();
extern double       CNgrid4D_y();
extern double       CNgrid4D_z();
extern double       CNgrid4D_t();
extern void         CNprint_grid4D();

#endif /* CNgrid_defined */

