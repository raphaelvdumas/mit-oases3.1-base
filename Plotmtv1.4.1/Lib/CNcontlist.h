/*
 * CNcontlist.h - definitions for contour steps
 *
 * This file requires "CNplot.h"
 */

#ifndef CNcontlist_defined
#define CNcontlist_defined

/*
 * Use a linked list of contours, where each contour contains
 *    level          (z=value)
 *    linecolor
 *    linewidth
 *    linetype
 */
typedef struct CNcontstep_strct {
   double                  value;
   CNcurve_property        curv_pr;
   struct CNcontstep_strct *next;
   struct CNcontstep_strct *prev;
} CNcontstep;
typedef struct CNcontstep_strct *CNcontstepptr;

/*
 * PROCEDURE DECLARATIONS - LINKED LISTS
 */
 
extern void          CNparse_contstep_string();
extern void          CNset_contstep_levels();
extern char         *CNcontstep_string();
extern void          CNapply_contstep_gbcurv_pr();
extern void          CNwrite_contsteps();

extern CNcontstepptr CNmake_contstep();
extern CNcontstepptr CNinsert_contstep();
extern CNcontstepptr CNstore_contstep();
extern void          CNdelete_contstep();
extern void          CNdelete_contstep_list();
extern void          CNprint_contstep_list();
extern int           CNcount_contsteps();
extern void          CNcopy_contstep_list();

#endif /* CNcontlist_defined */
