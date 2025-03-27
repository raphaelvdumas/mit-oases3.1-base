/*
 * CNcube.c - definitions for a cube
 */

#ifndef CNcube_defined 
#define CNcube_defined

/*
 * The cube's makeup is reflected in these bits (cube.pcode)
 */
#define CN_XPRISM     (1L << 0)      /* prism parallel to x   */
#define CN_YPRISM     (1L << 1)      /* prism parallel to y   */
#define CN_ZPRISM     (1L << 2)      /* prism parallel to z   */
#define CN_CUTONMIN   (1L << 3)      /* cutline touches min   */

#define CN_XMINELECT  (1L << 4)      /* electrode on xmin     */
#define CN_YMINELECT  (1L << 5)      /* electrode on ymin     */
#define CN_ZMINELECT  (1L << 6)      /* electrode on zmin     */

#define CN_XMAXELECT  (1L << 8)      /* electrode on xmax     */
#define CN_YMAXELECT  (1L << 9)      /* electrode on ymax     */
#define CN_ZMAXELECT  (1L << 10)     /* electrode on zmax     */

#define CN_DIAGELECT  (1L << 12)     /* electrode on diagonal */
#define CN_MAT1ONMIN  (1L << 13)     /* mat1    touches min   */

/* 
 * A cube is used to represent a geometrical block structure.
 * Internally, the cube could be divided into 2 prisms where 
 * the cut is parallel to the x, y, or z axis.
 */
/* Cube */
typedef struct CNcube_strct {
   short  ID;
   short  mat1;        /* Material of 1st prism         */
   short  mat2;        /* Material of 2nd prism         */
   int    pcode;       /* Prism encoding                */
   double x0, x1;      /* x-boundaries                  */
   double y0, y1;      /* y-boundaries                  */
   double z0, z1;      /* z-boundaries                  */
   double x0_o, x1_o;  /* original x-boundaries         */
   double y0_o, y1_o;  /* original y-boundaries         */
   double z0_o, z1_o;  /* original z-boundaries         */
   double zave;        /* Used for sorting              */
   struct CNcube_strct *next;
   struct CNcube_strct *prev;
} CNcube;
typedef struct CNcube_strct *CNcubeptr;


/* 
 * A block is used to represent 4D data, that is, values in 3D space
 * on each of the vertices of the cube.  Internally, the block could
 * be divided into 2 prisms where the cut is parallel to the x, y, or z axis.
 * The block refers to the cube for its geometry.
 * Multiple blocks could point to the same cube.
 * Thus the block-cube relation is analogous to the node-point relationship.
 */
/* Block */
typedef struct CNblock_strct {
   short  ID;
   double t000;        /* t-val at x=xmin y=ymin z=zmin */
   double t001;        /* t-val at x=xmin y=ymin z=zmax */
   double t010;        /* t-val at x=xmin y=ymax z=zmin */
   double t011;        /* t-val at x=xmin y=ymax z=zmax */
   double t100;        /* t-val at x=xmax y=ymin z=zmin */
   double t101;        /* t-val at x=xmax y=ymin z=zmax */
   double t110;        /* t-val at x=xmax y=ymax z=zmin */
   double t111;        /* t-val at x=xmax y=ymax z=zmax */
   double zave;        /* Used for sorting */
   struct CNcube_strct  *cube;  /* The actual geometry  */
   struct CNblock_strct *next;
   struct CNblock_strct *prev;
} CNblock;
typedef struct CNblock_strct *CNblockptr;


/*
 * Extern declarations
 */
extern void CNfind_exposed_faces_of_cube();
extern int  CNcube_is_divided();
extern int  CNcube_xmin_is_electrode();
extern int  CNcube_xmax_is_electrode();
extern int  CNcube_ymin_is_electrode();
extern int  CNcube_ymax_is_electrode();
extern int  CNcube_zmin_is_electrode();
extern int  CNcube_zmax_is_electrode();
extern int  CNcube_diag_is_electrode();

extern CNcubeptr  CNmake_cube();
extern CNcubeptr  CNinsert_cube();
extern void       CNdelete_cube();
extern void       CNdelete_cube_list();
extern void       CNprint_cube_list();
extern int        CNcount_cubes();

extern CNblockptr CNmake_block();
extern CNblockptr CNinsert_block();
extern void       CNdelete_block();
extern void       CNdelete_block_list();
extern void       CNprint_block_list();
extern int        CNcount_blocks();

#endif /* CNcube_defined */

