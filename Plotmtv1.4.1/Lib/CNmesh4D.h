/*
 * CNmesh4D.h - definitions for a 4D mesh (similar to grid)
 *
 * This file requires CNdatatypes.h and CNquant.h
 */

#ifndef CNmesh4D_defined
#define CNmesh4D_defined

/*
 *    A mesh4D is a 3D volumetric grid consisting of x,y,z 
 *    cartesian-coordinate arrays. 
 *
 *    The mat1, mat2 and prism arrays are used to associate materials/regions
 *    with the grid. These arrays indicate that the cube is to broken up into
 *    2 prisms.
 *
 *    The regions are used to match a material name with its ID and a noplot
 *    flag
 */

/* 4D grid */
typedef struct CNmesh4D_strct {
   short  ID;
   short  flag;
   double *xarray;
   double *yarray;
   double *zarray;
   double *qarray;
   double *mat1_array;
   double *mat2_array;
   double *prism_array;
   int    nx;
   int    ny;
   int    nz;
   int    nq;
   double xmin, xmax;
   double ymin, ymax;
   double zmin, zmax;
   double qmin, qmax;
   struct CNregion_strct *regionhead;
   struct CNregion_strct *regiontail;
   struct CNquant4D_strct *quant4Dhead;
   struct CNquant4D_strct *quant4Dtail;
} CNmesh4D;
typedef struct CNmesh4D_strct *CNmesh4Dptr;


/* 4D quantities - this is used only on a temporary basis */
typedef struct CNquant4D_strct {
   int    ID;
   char   *name;     /* Quantity name    */
   double *qarray;   /* Pointer to array */
   int    npts;      /* Array size       */
   struct CNquant4D_strct *next;
   struct CNquant4D_strct *prev;
} CNquant4D;
typedef struct CNquant4D_strct *CNquant4Dptr;


/*
 * External procedure declarations
 */
extern CNmesh4Dptr  CNmake_mesh4D();
extern void         CNdelete_mesh4D();
extern double       CNmesh4D_x();
extern double       CNmesh4D_y();
extern double       CNmesh4D_z();
extern double       CNmesh4D_q();
extern double       CNmesh4D_m1();
extern double       CNmesh4D_m2();
extern double       CNmesh4D_mp();
extern void         CNprint_mesh4D();

extern CNquant4Dptr CNmake_quant4D();
extern CNquant4Dptr CNinsert_quant4D();
extern void         CNadd_quant4D();
extern void         CNdelete_quant4D();
extern void         CNdelete_quant4D_list();
extern void         CNprint_quant4D_list();
extern void         CNprint_quant4D();
extern int          CNcount_quant4Ds();

extern void         CNslice_mesh4D();
extern int          CNmesh4D_mat_is_air();
extern void         CNmesh4D_mat_options();

#endif /* CNmesh4D_defined */

