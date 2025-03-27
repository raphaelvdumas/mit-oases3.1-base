/*
 * CNbarchart.h - procedures to build and maintain a barchart
 *              data structure
 */

#ifndef CNbarchart_defined
#define CNbarchart_defined

/*
 * BARCHART DATA STRUCTURE
 *    A barchart is used to store data for a bar chart.
 *    The data consists of points (x only) collected in bins and bars.
 *    bins then plotted.
 */

/*
 * Bar Chart data has the following format:
 *             "Bar_A"   "Bar_B"   "Bar_C"
 *   "value1"    xa1       xb1       xc1
 *   "value2"    xa2       xb2       xc2
 *   "value3" ...
 *
 * The resulting chart is obtained:
 *
 *         |      C
 *         |  A  |=|                  C
 *         | |=|B| |    A B C       B|=|
 *         | | |=| |   |=|=|=|    A|=| |
 *         | | | | |   | | | |   |=| | |
 *         | | | | |   | | | |   | | | |
 *         ----------|---------|---------|
 *           value1    value2    value3
 *
 * Each of the intervals "value1", "value2" etc are defined as bins,
 * and each bin contains 1 or more bars.
 *
 * Thus the data-structure contains a linked list of bins
 * Each bin has a name (e.g. "value1" and a linked list of bars
 * Each bar in turn contains a point and a pointer to a name (e.g. "A")
 */

/* Bar */
typedef struct CNbar_strct {
   char   *name;
   double value;
   short  filltype;
   short  fillcolor;
   struct CNbar_strct *next;
   struct CNbar_strct *prev;
} CNbar;
typedef struct CNbar_strct *CNbarptr;

/* Bin */
typedef struct CNbin_strct {
   char   *name;
   double xmin;
   double xmax;
   struct CNbar_strct *barhead;
   struct CNbar_strct *bartail;
   struct CNbin_strct *next;
   struct CNbin_strct *prev;
} CNbin;
typedef struct CNbin_strct *CNbinptr;

/* Barchart */
typedef struct CNbarchart_strct {
   struct CNbin_strct *binhead;
   struct CNbin_strct *bintail;
} CNbarchart;
typedef struct CNbarchart_strct *CNbarchartptr;

CNbarptr      CNmake_bar();
CNbarptr      CNinsert_bar();
void          CNstore_bar();
void          CNdelete_bar();
void          CNdelete_bar_list();

CNbinptr      CNmake_bin();
CNbinptr      CNinsert_bin();
void          CNstore_bin();
void          CNdelete_bin();
void          CNdelete_bin_list();

CNbarchartptr CNmake_barchart();
void          CNdelete_barchart();

void          CNsort_barchart();

#endif /* CNbarchart_defined */
