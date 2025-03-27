/*
 * CNhistogram.h - definitions for a histogram
 *
 * This file requires CNdatatypes.h
 */

#ifndef CNhistogram_defined
#define CNhistogram_defined

/*
 *    A histogram contains a list of points to be sorted into bins
 */
/* Histogram */
typedef struct CNhistogram_strct {
   double xmin;                         /* Min value     */
   double xmax;                         /* Max value     */
   short  filltype;                     /* Fill type     */
   short  fillcolor;                    /* Fill color    */
   struct CNpoint_strct   *pointhead;   /* The original points */
   struct CNpoint_strct   *pointtail;
} CNhistogram;
typedef struct CNhistogram_strct *CNhistogramptr;

extern void CNsort_histogram();

extern CNhistogramptr  CNmake_histogram();
extern void            CNdelete_histogram();
extern void            CNprint_histogram();

#endif /* CNhistogram_defined */

