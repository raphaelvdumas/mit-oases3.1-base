/*
 * CNquant.h - quantites and node-values
 *
 * requires CNdatatypes.h
 */

#ifndef CNquant_defined
#define CNquant_defined

/*
 * The nodelist and quantity so that a quantity can be associated with
 * a physical point in space
 */

/* Node values */
#define CN_MAXFIELDS 10
typedef struct CNndval_strct {
   int    nodeID;                /* Node ID               */
   short  nfields;               /* number of fields      */
   double field[CN_MAXFIELDS];   /* Field value at a node */
   struct CNndval_strct *next;
   struct CNndval_strct *prev;
} CNndval;
typedef struct CNndval_strct *CNndvalptr;

/* A quantity associated with nodes */
typedef struct CNquant_strct {
   short  ID;
   short  nfields;               /* number of fields      */
   char   *label;                /* Unique identifier     */
   struct CNndval_strct *ndvalhead;
   struct CNndval_strct *ndvaltail;
   struct CNquant_strct *next;
   struct CNquant_strct *prev;
} CNquant;
typedef struct CNquant_strct *CNquantptr;

/* A region is used to store boundary information */
typedef struct CNregion_strct {
   short ID;
   short color;
   short matID;
   short isair;
   short nocont;                        /* Don't draw in this region    */
   char  *matname;
   struct CNpoly_strct   *polyhead;     /* The polygonal boundary       */
   struct CNpoly_strct   *polytail;
   struct CNpoly_strct   *matpolyhead;  /* The material boundary        */
   struct CNpoly_strct   *matpolytail;
   struct CNregion_strct *next;
   struct CNregion_strct *prev;
} CNregion;
typedef struct CNregion_strct *CNregionptr;

/* Use this to define electrode-regions */
#define CN_ELECTRODE -100

extern CNquantptr   CNmake_quant();
extern CNquantptr   CNinsert_quant();
extern void         CNadd_quant();
extern void         CNdelete_quant();
extern void         CNdelete_quant_list();
extern void         CNprint_quant_list();
extern void         CNprint_quant();
extern int          CNcount_quants();
extern int          CNquant_list_size();
extern int          CNquant_size();

extern CNndvalptr   CNinsert_ndval();
extern void         CNdelete_ndval();
extern void         CNdelete_ndval_list();
extern void         CNprint_ndval_list();
extern void         CNprint_ndval();
extern int          CNcount_ndvals();

extern CNregionptr  CNinsert_region();
extern void         CNdelete_region();
extern void         CNdelete_region_list();
extern void         CNprint_region_list();
extern void         CNprint_region();
extern int          CNcount_regions();
extern int          CNregion_list_size();
extern int          CNregion_size();

#endif /* CNquant_defined */

