/*
 * CNvector.h - definition for vectors
 *
 * This file requires the inclusion of "CNdata.h"
 */

#ifndef CNvector_defined
#define CNvector_defined

#define CN_ALL_FIELDS -1         /* To denote vectors           */
#define CN_MAG_FIELDS -2         /* Vector magnitude            */

/*
 * A vector consists of magnitude and direction (vx,vy,vz)
 * and a point (x,y,z).  For simplicity, each vector is self-contained,
 * i.e. contains both magnitude and point, unlike nodes which point to
 * points.
 */

/* A vector in a linked list */
typedef struct CNvector_strct {
   short  ID;
   short  flag;
   short  noplot;
   double x;
   double y;
   double z;
   double vx;
   double vy;
   double vz;
   struct CNnode_strct *nd;
   struct CNvector_strct *next;
   struct CNvector_strct *prev;
} CNvector;
typedef struct CNvector_strct *CNvecptr;

/*
 * The vector_bag contains the vectors and various bits of info
 * related to the vectors
 */
 
/* A vector-box */
typedef struct CNvecbox_strct {
   short  ID;
   short  linetype;
   short  linecolor;
   short  linewidth;
   short  marktype;
   short  markcolor;
   double vlen_max;
   double vlen_min;
   double xmin, xmax;
   double ymin, ymax;
   double zmin, zmax;
   struct CNvector_strct *vectorhead;
   struct CNvector_strct *vectortail;
} CNvecbox;
typedef struct CNvecbox_strct *CNvecboxptr;


/*
 * Extern declarations
 */
extern double      CNdefault_vector_scale();
extern double      CNmax_vector();
extern double      CNmin_vector();
extern double      CNveclog10();

extern CNvecboxptr CNmake_vectorbox();
extern void        CNdelete_vectorbox();
extern void        CNprint_vectorbox();

extern CNvecptr    CNmake_vector();
extern CNvecptr    CNinsert_vector();
extern void        CNdelete_vector();
extern void        CNdelete_vector_list();
extern void        CNprint_vector_list();
extern int         CNcount_vectors();

#endif /* CNvector_defined */
