/*
 * CNprobe.h - definition for probe
 *
 * This file requires "CNplot.h"
 */

#ifndef CNprobe_defined
#define CNprobe_defined

/*
 * A probe essentially is the intersection of a mesh element with
 * a line.  The probe returns a list of probe coordinates, and the element
 * IDs from which the data was derived.
 */

/* A probe in a linked list */
typedef struct CNprobe_strct {
   short   ID;
   double  x;
   double  y;
   double  z;
   CNtriaptr   T; 
   CNrectptr   R; 
   CNpolyptr   P;
   CNregionptr reg;
   struct  CNprobe_strct *next;
   struct  CNprobe_strct *prev;
} CNprobe;
typedef struct CNprobe_strct *CNprobeptr;


/*
 * Extern declarations
 */
extern void CNprobe_dataset();
extern int  CNpoint_in_region();

extern CNprobeptr CNinsert_probe();
extern void       CNdelete_probe();
extern void       CNdelete_probe_list();
extern void       CNprint_probe_list();
extern int        CNcount_probes();

extern int CNprobe_dataset_zave();

#endif /* CNprobe_defined */

