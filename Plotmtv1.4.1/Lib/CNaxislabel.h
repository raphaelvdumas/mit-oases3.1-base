/*
 * CNaxislabel.h - definitions for arbitrary axis labels
 *                 This allows users to specify the axis labels on the plot
 *
 * This file requires "CNplot.h"
 */   

#ifndef CNaxislabel_defined
#define CNaxislabel_defined

/*
 * Use a linked list of axis labels where each label consists of a character
 * string and a value (axis position in real coordinates)
 */
typedef struct CNaxislabel_strct {
   double pos;
   char   *name;
   struct CNaxislabel_strct *next;
   struct CNaxislabel_strct *prev;
} CNaxislabel;
typedef struct CNaxislabel_strct *CNaxislabelptr;


/*
 * PROCEDURE DECLARATIONS
 */

extern void CNparse_axislabel_string();
extern void CNwrite_xaxislabels();
extern void CNwrite_yaxislabels();
extern void CNwrite_zaxislabels();

extern CNaxislabelptr CNmake_axislabel();
extern CNaxislabelptr CNinsert_axislabel();
extern CNaxislabelptr CNstore_axislabel();
extern void           CNdelete_axislabel();
extern void           CNdelete_axislabel_list();
extern void           CNprint_axislabel_list();
extern int            CNcount_axislabels();
extern void           CNcopy_axislabel_list();

#endif /* CNaxislabel_defined */
