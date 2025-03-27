/*
 * barchart.c - procedures to build and maintain a barchart
 *              data structure
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include "CNplot.h"

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

static int      CNcount_bars();
static int      CNcount_bins();

/*
 * Given a barchart dataset, sort the data and create curves
 */
void CNsort_barchart(dptr, verbose)
CNdatasetptr dptr;
int          verbose;
{
   CNbarptr Barptr;
   CNbinptr Binptr;
   CNcurveptr Cptr;
   int    nbars, bar_count;
   int    nbins, bin_count;
   double bar_width, bar_offset;
   double bin_width;
   double xmin, xmax;
   double x1, y1, x2, y2;
   int    curveID=0;
   
   /* Error-check */
   if (dptr == NULL) {
      (void) fprintf(stderr,"CNsort_barchart: Error! NULL dataset!\n");
      return;
   }
   if (dptr->datatype != CN_BARCHART) {
      (void) fprintf(stderr,
                     "CNsort_barchart: Error! Need a BARCHART dataset!\n");
      return;
   }
   if (dptr->barchart == NULL) {
      (void) fprintf(stderr,
                     "CNsort_barchart: Error! NULL BARCHART structure!\n");
      return;
   }
   if (dptr->barchart->binhead == NULL) {
      (void) fprintf(stderr,
                     "CNsort_barchart: Error! No bars in BARCHART!\n");
      return;
   }
 
   /* Get rid of the curvelist in the dataset first */
   CNdelete_curve_list(&(dptr->curvehead), &(dptr->curvetail));
 
   /* Initialize */ 
   nbars      = CNcount_bars(dptr->barchart->binhead->barhead,
                             dptr->barchart->binhead->bartail);
   nbins      = CNcount_bins(dptr->barchart->binhead,
                             dptr->barchart->bintail);
   bar_offset = 0.2;
   bin_width  = 1.0;
   bar_width  = (bin_width - 2.0*bar_offset)/(double)nbars;
   /*
   bar_width  = 0.6;
   bin_width  = nbars*bar_width + 2.0*bar_offset;
    */
   xmin       = 0.0;
   xmax       = nbins*bin_width;

   /* Create the various bars */
   bin_count  = 0;
   for (Binptr=dptr->barchart->binhead; Binptr!=NULL; Binptr=Binptr->next) {
      /* Set the min/max */
      Binptr->xmin = xmin + (bin_count  )*bin_width;
      Binptr->xmax = xmin + (bin_count+1)*bin_width;
      bin_count++;

      bar_count = 0;
      for (Barptr=Binptr->barhead; Barptr!=NULL; Barptr=Barptr->next) {
         x1 = Binptr->xmin + (bar_count  )*bar_width + bar_offset;
         x2 = Binptr->xmin + (bar_count+1)*bar_width + bar_offset;
         bar_count++;
         y1 = dptr->data_pr.barmin;   /* Defaults to 0.0 */
         y2 = Barptr->value;

         /* Allocate a curve data-structure */
         Cptr = CNinsert_curve(&(dptr->curvehead), &(dptr->curvetail), 
                               curveID++);
         if (Cptr!=NULL) {
            (void) CNinsert_point(&(Cptr->pointhead), &(Cptr->pointtail),
                                  x1,y1,0.0,0);
            (void) CNinsert_point(&(Cptr->pointhead), &(Cptr->pointtail),
                                  x1,y2,0.0,0);
            (void) CNinsert_point(&(Cptr->pointhead), &(Cptr->pointtail),
                                  x2,y2,0.0,0);
            (void) CNinsert_point(&(Cptr->pointhead), &(Cptr->pointtail),
                                  x2,y1,0.0,0);
            (void) CNinsert_point(&(Cptr->pointhead), &(Cptr->pointtail),
                                  x1,y1,0.0,0);
            /* Set the properties */
            Cptr->curv_pr.filltype = Barptr->filltype;
            Cptr->curv_pr.flag     = Cptr->curv_pr.flag & CNfilltype;
            Cptr->curv_pr.fillcolor= Barptr->fillcolor;
            Cptr->curv_pr.flag     = Cptr->curv_pr.flag & CNfillcolor;
            if (Binptr==dptr->barchart->binhead && Barptr->name) { 
               (void) CNparse_curve_property(&(Cptr->curv_pr),
                                             "linelabel",Barptr->name,0);
            }
         }
      }
   }

   /* Reset the dataset boundaries */
   dptr->bxmin         = xmin;
   dptr->bxmax         = xmax;
   dptr->plot_pr.vxmin = xmin;
   dptr->plot_pr.vxmax = xmax;

   /* Print info */
   if (verbose) {
      (void) fprintf(stdout, "Generated %d new curves\n",
                     CNcount_curves(dptr->curvehead, dptr->curvetail));
   }
}


/*
 * BAR DATA_STRUCT ALLOCATION
 */
 
/*
 * make a bar
 */
CNbarptr CNmake_bar(name, value)
char    *name;
double  value;
{
   CNbarptr newptr;
   unsigned int size = sizeof(CNbar);
 
   if ((newptr = (CNbarptr)malloc(size))!=NULL) {
      newptr->name      = CNcreate_string(name);
      newptr->value     = value;
      newptr->filltype  = CN_FILL_SOLID;
      newptr->fillcolor = 2;
      newptr->next      = NULL;
      newptr->prev      = NULL;
   }
 
   return(newptr);
}
 

/*
 * Insert a bar at the tail of the current bar list
 */
CNbarptr CNinsert_bar(bar_listhead,bar_listtail,name,value)
CNbarptr *bar_listhead, *bar_listtail;
char     *name;
double    value;
{
   CNbarptr next,A,B;
 
   A = *bar_listtail;
   if ((B=CNmake_bar(name,value))!=NULL) {
      if (A==NULL) {
         *bar_listhead = B;
         *bar_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next    != NULL) next->prev = B;
         if (B->next == NULL) *bar_listtail = B;
      }
   }
   return(B);
}
 

/*
 * Store the linked list
 */
void CNstore_bar(bar_listhead, bar_listtail, Cptr)
CNbarptr *bar_listhead, *bar_listtail, Cptr;
{
   CNbarptr A, B, next;
 
   A = *bar_listtail;
   if ((B=Cptr)!=NULL) {
      if (A==NULL) {
         *bar_listhead = B;
         *bar_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next != NULL) next->prev = B;
         if (B->next == NULL) *bar_listtail = B;
      }
   }
}


/*
 * Delete bar
 */
void CNdelete_bar(bar_listhead, bar_listtail, B)
CNbarptr *bar_listhead, *bar_listtail;
CNbarptr B;
{
   CNbarptr prev,next;
 
   /* Destroy the name string */
   if (B->name) CNdestroy_string(B->name);

   prev = B->prev;
   next = B->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (B==*bar_listhead) *bar_listhead = next;
   if (B==*bar_listtail) *bar_listtail = prev;
 
   /* Now delete B */
   free ((char*)B);
}

 
/*
 * Delete all the bars in the list
 */
void CNdelete_bar_list(bar_listhead, bar_listtail)
CNbarptr *bar_listhead, *bar_listtail;
{
   CNbarptr B;
 
   while ((B = *bar_listhead) != NULL)
      CNdelete_bar(bar_listhead, bar_listtail, B);
}


/*
 * Count the number of bars in the list
 */
/*ARGSUSED*/
static int CNcount_bars(bar_listhead, bar_listtail)
CNbarptr bar_listhead, bar_listtail;
{
   CNbarptr B;
   int      count = 0;
 
   for (B=bar_listhead; B!=NULL; B=B->next) count++;
 
   return(count);
}


/*
 * BIN DATA_STRUCT ALLOCATION
 */
 
/*
 * make a bin
 */
CNbinptr CNmake_bin(name)
char *name;
{
   CNbinptr newptr;
   unsigned int size = sizeof(CNbin);
 
   if ((newptr = (CNbinptr)malloc(size))!=NULL) {
      newptr->name      = CNcreate_string(name);
      newptr->xmin      = 0.0;
      newptr->xmax      = 0.0;
      newptr->barhead   = NULL;
      newptr->bartail   = NULL;
      newptr->next      = NULL;
      newptr->prev      = NULL;
   }
 
   return(newptr);
}
 

/*
 * Insert a bin at the tail of the current bin list
 */
CNbinptr CNinsert_bin(bin_listhead,bin_listtail,name)
CNbinptr *bin_listhead, *bin_listtail;
char     *name;
{
   CNbinptr next,A,B;
 
   A = *bin_listtail;
   if ((B=CNmake_bin(name))!=NULL) {
      if (A==NULL) {
         *bin_listhead = B;
         *bin_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next    != NULL) next->prev = B;
         if (B->next == NULL) *bin_listtail = B;
      }
   }
   return(B);
}
 

/*
 * Store the linked list
 */
void CNstore_bin(bin_listhead, bin_listtail, Cptr)
CNbinptr *bin_listhead, *bin_listtail, Cptr;
{
   CNbinptr A, B, next;
 
   A = *bin_listtail;
   if ((B=Cptr)!=NULL) {
      if (A==NULL) {
         *bin_listhead = B;
         *bin_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next != NULL) next->prev = B;
         if (B->next == NULL) *bin_listtail = B;
      }
   }
}


/*
 * Delete bin
 */
void CNdelete_bin(bin_listhead, bin_listtail, B)
CNbinptr *bin_listhead, *bin_listtail;
CNbinptr B;
{
   CNbinptr prev,next;
 
   /* Destroy the name string */
   if (B->name) CNdestroy_string(B->name);

   /* Delete all the bars in the bin */
   CNdelete_bar_list(&(B->barhead), &(B->bartail));

   prev = B->prev;
   next = B->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (B==*bin_listhead) *bin_listhead = next;
   if (B==*bin_listtail) *bin_listtail = prev;
 
   /* Now delete B */
   free ((char*)B);
}

 
/*
 * Delete all the bins in the list
 */
void CNdelete_bin_list(bin_listhead, bin_listtail)
CNbinptr *bin_listhead, *bin_listtail;
{
   CNbinptr B;
 
   while ((B = *bin_listhead) != NULL)
      CNdelete_bin(bin_listhead, bin_listtail, B);
}


/*
 * Count the number of bins in the list
 */
/*ARGSUSED*/
static int CNcount_bins(bin_listhead, bin_listtail)
CNbinptr bin_listhead, bin_listtail;
{
   CNbinptr B;
   int      count = 0;
 
   for (B=bin_listhead; B!=NULL; B=B->next) count++;
 
   return(count);
}


/*
 * BAR-CHART DATA_STRUCT ALLOCATION
 */
 
/*
 * make a barchart
 */
CNbarchartptr CNmake_barchart(binhead, bintail)
CNbinptr binhead, bintail;
{
   CNbarchartptr newptr;
   unsigned int size = sizeof(CNbarchart);
 
   if ((newptr = (CNbarchartptr)malloc(size))!=NULL) {
      newptr->binhead = binhead;
      newptr->bintail = bintail;
   }
 
   return(newptr);
}
 
 
/*
 * Delete barchart
 */
void CNdelete_barchart(B)
CNbarchartptr B;
{
   /* Delete all the bins in the barchart */
   CNdelete_bin_list(&(B->binhead), &(B->bintail));

   /* Now delete B */
   free ((char*)B);
}

