/*
 * CNdatatypes.h - structure definitions
 *
 * This file requires the inclusion of "CNdata.h" and "CNproperty.h"
 */

#ifndef CNdatatypes_defined
#define CNdatatypes_defined

/*
 * General plot data structure - the basic mesh element is a triangle
 *    A triangle consists of 3 segments and 3 points
 *    A segment contains 2 nodes  
 *    A line is similar to a segment but contains 2 points.
 *    A node is a point but with linked list info attached.
 *
 *    A curve is made up of a linked list of points
 *    A polygon is made up of a linked list of nodes 
 *    A dataset consists of a linked list of curves, triangles, etc. 
 */

/* A point in a linked list */
typedef struct CNpoint_strct {
   short   ID;
   short   flag;
   double  x;
   double  y;
   double  z;
   struct  CNpoint_strct *next;
   struct  CNpoint_strct *prev;
} CNpoint;
typedef struct CNpoint_strct *CNpointptr;

/* nodes */
typedef struct CNnode_strct {
   short      ID;
   short      flag;
   short      ptID;
   CNpointptr coord;
   double     t;
   struct     CNnode_strct  *next;
   struct     CNnode_strct  *prev;
} CNnode;
typedef struct CNnode_strct *CNnodeptr;


/*
 * Simple geometrical objects 
 *    A segment contains 2 nodes, and points to 2 neighboring triangles
 *    A point-segment contains 2 points, and points to 2 neighboring triangles
 *    A line is a self-contained segment and have coordinate data.
 *       - used primarily for constructing joined curves.
 *    A triangle consists of 3 nodes
 *    A rectangle consists of 4 nodes arranged sequentially
 *    A polygon contains a linked list of sequential nodes.
 *    A curve is similar to a polygon except that it contains information
 *       on how to draw the curve, i.e. linewidth, filltype.
 */

/* segments */
typedef struct CNsegm_strct {
   short  ID;
   short  flag;
   struct CNnode_strct *n1;
   struct CNnode_strct *n2;
   struct CNsegm_strct *next;
   struct CNsegm_strct *prev;
   struct CNtria_strct *nbrtri[2];
   int    numtri;
} CNsegm;
typedef struct CNsegm_strct *CNsegmptr;

/* point-segments - segments that connect tria/rect elements to points */
typedef struct CNptseg_strct {
   short  ID;
   short  flag;
   short  boundary;
   struct CNpoint_strct *p1;
   struct CNpoint_strct *p2;
   struct CNptseg_strct *next;
   struct CNptseg_strct *prev;
   struct CNelem_strct  *nbrelem[2];
   int    numelem;
} CNptseg;
typedef struct CNptseg_strct *CNptsegptr;

/* lines */
typedef struct CNline_strct {
   CNpoint  pt1;
   CNpoint  pt2;
   struct CNline_strct *next;
   struct CNline_strct *prev;
} CNline;
typedef struct CNline_strct *CNlineptr;

/* triangles */
typedef struct CNtria_strct {
   short  ID;
   short  flag;
   short  region;
   short  nocont;            /* No contours here */
   short  n1ID, n2ID, n3ID;
   double zave;
   struct CNnode_strct *n1;
   struct CNnode_strct *n2;
   struct CNnode_strct *n3;
   struct CNregion_strct *R;
   struct CNtria_strct *next;
   struct CNtria_strct *prev;
} CNtria;
typedef struct CNtria_strct *CNtriaptr;

/* rectangles */
typedef struct CNrect_strct {
   short  ID;
   short  flag;
   short  region;
   short  nocont;           /* No contours here */
   short  n1ID, n2ID, n3ID, n4ID;
   double zave;
   struct CNnode_strct *n1;
   struct CNnode_strct *n2;
   struct CNnode_strct *n3;
   struct CNnode_strct *n4;
   struct CNregion_strct *R;
   struct CNrect_strct *next;
   struct CNrect_strct *prev;
} CNrect;
typedef struct CNrect_strct *CNrectptr;

/* element - could contain either a rectangle or a triangle */
typedef struct CNelem_strct {
   short  type;
   short  flag;
   double zave;
   struct CNtria_strct *tria;
   struct CNrect_strct *rect;
   struct CNelem_strct *next;
   struct CNelem_strct *prev;
} CNelem;
typedef struct CNelem_strct *CNelemptr;

/* polygon */
typedef struct CNpoly_strct {
   short  ID;
   short  flag;
   short  region;
   short  fill;
   double zave;
   struct CNnlist_strct *nlisthead;
   struct CNnlist_strct *nlisttail;
   struct CNpoly_strct *next;
   struct CNpoly_strct *prev;
} CNpoly;
typedef struct CNpoly_strct *CNpolyptr;

/* curves */
typedef struct CNcurve_strct {
   short  ID;
   short  focus;                     /* For editing */
   double zave;                      /* For sorting */
   CNcurve_property      curv_pr;
   struct CNpoint_strct *pointhead;
   struct CNpoint_strct *pointtail;
   struct CNcurve_strct *next;
   struct CNcurve_strct *prev;
} CNcurve;
typedef struct CNcurve_strct *CNcurveptr;


/*
 * A typical linked-list data-structure, e.g. a list of triangles,
 * has pointers to the head and tail of the list, e.g triahead, triatail.
 * But we sometimes need to build another list of triangles, without
 * duplicating the current list.  Therefore the new list contains
 * POINTERS to the triangles; this preserve the original order of the list.
 *
 * Example :
 *    list of pointers to the triangle data-structure.
 *           triahead->T->T->triatail
 *
 *    list of pointers to the triangle-pointers
 *           tlisthead->TL->TL->tlisttail
 *              |        |   |     |
 *              T        T   T     T
 */

/* list of triangle pointers - contains list of triaptrs */
typedef struct CNtlist_strct {
   struct CNtria_strct  *T;
   struct CNtlist_strct *next;
   struct CNtlist_strct *prev;
} CNtlist;
typedef struct CNtlist_strct *CNtlistptr;

/* list of element pointers - contains list of elemptrs */
typedef struct CNelist_strct {
   struct CNelem_strct  *E;
   struct CNelist_strct *next;
   struct CNelist_strct *prev;
} CNelist;
typedef struct CNelist_strct *CNelistptr;

/* list of segment pointers - contains list of segmptrs */
typedef struct CNslist_strct {
   struct CNsegm_strct  *S;
   struct CNslist_strct *next;
   struct CNslist_strct *prev;
} CNslist;
typedef struct CNslist_strct *CNslistptr;

/* list of node pointers - contains list of nodeptrs */
typedef struct CNnlist_strct {
   struct CNnode_strct  *N;
   struct CNnlist_strct *next;
   struct CNnlist_strct *prev;
} CNnlist;
typedef struct CNnlist_strct *CNnlistptr;

/* list of curve pointers - contains list of curveptrs */
typedef struct CNcvlist_strct {
   struct CNcurve_strct  *C;
   struct CNcvlist_strct *next;
   struct CNcvlist_strct *prev;
} CNcvlist;
typedef struct CNcvlist_strct *CNcvlistptr;



/*
 * PROCEDURE DECLARATIONS - LINKED LISTS
 */

#define CNinsert_point CNinsert_tailpoint
extern CNpointptr CNmake_point();
extern CNpointptr CNinsert_tailpoint();
extern CNpointptr CNinsert_headpoint();
extern void       CNdelete_point();
extern void       CNdelete_point_list();
extern void       CNprint_point_list();
extern int        CNcount_points();

#define CNinsert_node CNinsert_tailnode
extern CNnodeptr  CNmake_node();
extern CNnodeptr  CNinsert_tailnode();
extern CNnodeptr  CNinsert_headnode();
extern void       CNdelete_node();
extern void       CNdelete_node_list();
extern CNnodeptr  CNcreate_node();
extern CNnodeptr  CNcreate_tailnode();
extern CNnodeptr  CNcreate_headnode();
extern void       CNremove_node();
extern void       CNremove_node_list();
extern void       CNprint_node_list();
extern void       CNprint_node();
extern int        CNcount_nodes();

extern CNsegmptr  CNinsert_segm();
extern void       CNdelete_segm();
extern void       CNdelete_segm_list();
extern void       CNprint_segm_list();
extern void       CNprint_segm();
extern int        CNcount_segms();

extern CNptsegptr CNinsert_ptseg();
extern void       CNdelete_ptseg();
extern void       CNdelete_ptseg_list();
extern void       CNprint_ptseg_list();
extern void       CNprint_ptseg();
extern int        CNcount_ptsegs();

extern void       CNinsert_line();
extern void       CNdelete_line();
extern void       CNdelete_line_list();
extern void       CNprint_line_list();

extern CNtriaptr  CNmake_tria();
extern CNtriaptr  CNinsert_tria();
extern void       CNdelete_tria();
extern void       CNdelete_tria_list();
extern void       CNprint_tria_list();
extern void       CNprint_tria();
extern int        CNcount_trias();

extern CNrectptr  CNmake_rect();
extern CNrectptr  CNinsert_rect();
extern void       CNdelete_rect();
extern void       CNdelete_rect_list();
extern void       CNprint_rect_list();
extern void       CNprint_rect();
extern int        CNcount_rects();

extern CNelemptr  CNmake_elem();
extern CNelemptr  CNinsert_elem();
extern void       CNdelete_elem();
extern void       CNdelete_elem_list();
extern void       CNprint_elem_list();
extern void       CNprint_elem();
extern int        CNcount_elems();

extern CNpolyptr  CNmake_poly();
extern CNpolyptr  CNinsert_poly();
extern void       CNdelete_poly();
extern void       CNdelete_poly_list();
extern void       CNprint_poly_list();
extern int        CNcount_polys();
extern void       CNstore_poly();

extern CNcurveptr CNmake_curve();
extern CNcurveptr CNinsert_curve();
extern void       CNdelete_curve();
extern void       CNdelete_curve_list();
extern void       CNprint_curve_list();
extern void       CNprint_curve();
extern int        CNcount_curves();
extern int        CNcurve_list_size();
extern int        CNcurve_size();
extern void       CNstore_curve();
extern void       CNappend_curves();

extern void       CNinsert_tlist();
extern void       CNdelete_tlist();
extern void       CNdelete_tlist_list();
extern void       CNprint_tlist();
extern int        CNcount_tlists();

extern void       CNinsert_elist();
extern void       CNdelete_elist();
extern void       CNdelete_elist_list();
extern void       CNprint_elist();
extern int        CNcount_elists();

extern void       CNinsert_slist();
extern void       CNdelete_slist();
extern void       CNdelete_slist_list();
extern void       CNprint_slist();
extern int        CNcount_slists();

extern void       CNinsert_tailnlist();
extern void       CNinsert_headnlist();
extern void       CNdelete_nlist();
extern void       CNdelete_nlist_list();
extern void       CNprint_nlist_list();
extern void       CNprint_nlist();
extern int        CNcount_nlists();

extern void       CNinsert_cvlist();
extern void       CNdelete_cvlist();
extern void       CNdelete_cvlist_list();
extern void       CNprint_cvlist();
extern int        CNcount_cvlists();

#endif /* CNdatatypes_defined */

