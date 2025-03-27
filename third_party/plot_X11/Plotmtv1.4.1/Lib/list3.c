/*
 * list3.c - linked list procedures involving quantities and node-values
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include "CNdata.h"
#include "CNproperty.h"
#include "CNdatatypes.h"
#include "CNquant.h"
#include "CNstring.h"

static CNndvalptr make_ndval();
static CNregionptr make_region();

/*
 * QUANTITIES
 */

/*
 * quantities are used to store field information at nodes
 */
CNquantptr CNmake_quant(label, nfields, ID)
char       *label;    
int        nfields;
int        ID;
{
   CNquantptr newptr;
   unsigned int size = sizeof(CNquant);

   if ((newptr = (CNquantptr)malloc(size))!=NULL) {
      newptr->label     = CNcreate_string(label);
      newptr->ID        = ID;
      newptr->nfields   = nfields;
      newptr->ndvalhead = NULL;
      newptr->ndvaltail = NULL;
      newptr->next      = NULL;
      newptr->prev      = NULL;
   }
   return(newptr);
}

/*
 * Insert a quantity at the tail of the current quantity list
 */
CNquantptr CNinsert_quant(quant_listhead,quant_listtail,
                          label, nfields, ID)
CNquantptr *quant_listhead, *quant_listtail;
char       *label;    
int        nfields;
int        ID;
{
   CNquantptr next,A,B;

   A = *quant_listtail;
   if ((B=CNmake_quant(label, nfields, ID))!=NULL) {
      if (A==NULL) {
         *quant_listhead = B;
         *quant_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next    != NULL) next->prev = B;
         if (B->next == NULL) *quant_listtail = B;
      }
   }
   return(B);
}

/*
 * Insert a quantity at the tail of the current quantity list
 */
void CNadd_quant(quant_listhead,quant_listtail,quant)
CNquantptr *quant_listhead, *quant_listtail, quant;
{
   CNquantptr next,A,B;

   A = *quant_listtail;
   if ((B=quant)!=NULL) {
      if (A==NULL) {
         *quant_listhead = B;
         *quant_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next    != NULL) next->prev = B;
         if (B->next == NULL) *quant_listtail = B;
      }
   }
}


/*
 * Delete quant
 */
void CNdelete_quant(quant_listhead, quant_listtail, Q)
CNquantptr *quant_listhead, *quant_listtail;
CNquantptr Q;
{
   CNquantptr prev,next;

   /* Delete the label */
   CNdestroy_string(Q->label);

   /* Delete all the node-values associated with the quantity */
   CNdelete_ndval_list(&(Q->ndvalhead), &(Q->ndvaltail));

   prev = Q->prev;
   next = Q->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (Q==*quant_listhead) *quant_listhead = next;
   if (Q==*quant_listtail) *quant_listtail = prev;

   /* Now delete Q */
   free ((char*)Q);
}


/*
 * Delete all the quantities in the list
 */
void CNdelete_quant_list(quant_listhead, quant_listtail)
CNquantptr *quant_listhead, *quant_listtail;
{
   CNquantptr Q;

   while ((Q = *quant_listhead) != NULL)
      CNdelete_quant(quant_listhead, quant_listtail, Q);
}


/*
 * print out the list of quantities
 */
/*ARGSUSED*/
void CNprint_quant_list(quant_listhead, quant_listtail)
CNquantptr quant_listhead, quant_listtail;
{
   CNquantptr Q;

   for (Q=quant_listhead; Q!=NULL; Q=Q->next)
      CNprint_quant(Q);
}


/*
 * print a quantity 
 */
void CNprint_quant(Q)
CNquantptr Q;
{
   (void) fprintf(stdout,
                  "   QuantID# %3d   Label=\"%s\"  Fields=%d\n",
                  Q->ID, Q->label, Q->nfields);
   CNprint_ndval_list(Q->ndvalhead, Q->ndvaltail);
   (void) fflush(stdout);
}


/*
 * Count the number of quants in the list
 */
/*ARGSUSED*/
int CNcount_quants(quant_listhead, quant_listtail)
CNquantptr quant_listhead, quant_listtail;
{
   CNquantptr P;
   int        count = 0;

   for (P=quant_listhead; P!=NULL; P=P->next) count++;

   return(count);
}


/*
 * Return the size of all quants in the list
 */
/*ARGSUSED*/
int CNquant_list_size(quant_listhead, quant_listtail)
CNquantptr quant_listhead, quant_listtail;
{
   CNquantptr C;
   int        size = 0;

   for (C=quant_listhead; C!=NULL; C=C->next)
      size += CNquant_size(C);

   return(size);
}


/*
 * Return the (approximate) size of the quant
 */
int CNquant_size(Qptr)
CNquantptr Qptr;
{
   int    size, nndvals;

   if (Qptr == NULL) return(0);

   /* The basic size */
   size = sizeof(CNquant);

   /* Count the contents of the curve */
   nndvals = CNcount_ndvals(Qptr->ndvalhead,Qptr->ndvaltail);

   /* Count the size of the components */
   size += nndvals*sizeof(CNndval);

   /* return */
   return(size);
}


/*
 * NODE-VALUES
 */

/*
 * node-values are used to store field information at nodes
 */
static
CNndvalptr make_ndval(nfields, nodeID)
int        nfields, nodeID;
{
   CNndvalptr newptr;
   unsigned int size = sizeof(CNndval);
   int        i;

   if ((newptr = (CNndvalptr)malloc(size))!=NULL) {
      newptr->nodeID    = nodeID;
      newptr->nfields   = nfields;
      for (i=0; i<CN_MAXFIELDS; i++) newptr->field[i] = 0.0;
      newptr->next      = NULL;
      newptr->prev      = NULL;
   }
   return(newptr);
}

/*
 * Insert a node-value at the tail of the current node-value list
 */
CNndvalptr CNinsert_ndval(ndval_listhead,ndval_listtail, nfields, ID)
CNndvalptr *ndval_listhead, *ndval_listtail;
int        nfields, ID;
{
   CNndvalptr next,A,B;

   A = *ndval_listtail;
   if ((B=make_ndval(nfields, ID))!=NULL) {
      if (A==NULL) {
         *ndval_listhead = B;
         *ndval_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next    != NULL) next->prev = B;
         if (B->next == NULL) *ndval_listtail = B;
      }
   }
   return(B);
}

/*
 * Delete ndval
 */
void CNdelete_ndval(ndval_listhead, ndval_listtail, N)
CNndvalptr *ndval_listhead, *ndval_listtail;
CNndvalptr N;
{
   CNndvalptr prev,next;

   prev = N->prev;
   next = N->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (N==*ndval_listhead) *ndval_listhead = next;
   if (N==*ndval_listtail) *ndval_listtail = prev;

   /* Now delete N */
   free ((char*)N);
}


/*
 * Delete all the node-values in the list
 */
void CNdelete_ndval_list(ndval_listhead, ndval_listtail)
CNndvalptr *ndval_listhead, *ndval_listtail;
{
   CNndvalptr N;

   while ((N = *ndval_listhead) != NULL)
      CNdelete_ndval(ndval_listhead, ndval_listtail, N);
}


/*
 * print out the list of node-values
 */
/*ARGSUSED*/
void CNprint_ndval_list(ndval_listhead, ndval_listtail)
CNndvalptr ndval_listhead, ndval_listtail;
{
   void CNprint_ndval();
   CNndvalptr N;

   for (N=ndval_listhead; N!=NULL; N=N->next)
      CNprint_ndval(N);
}


/*
 * print a node-value 
 */
void CNprint_ndval(N)
CNndvalptr N;
{
   int i;

   (void) fprintf(stdout,"   NdvalID# %3d   Fields (%3d):", N->nodeID, N->nfields);
   for (i=0; i<N->nfields; i++)
      (void) fprintf(stdout," [%d] %8.5e ", i, N->field[i]);
   (void) fprintf(stdout,"\n");
   (void) fflush(stdout);
}


/*
 * Count the number of ndvals in the list
 */
/*ARGSUSED*/
int CNcount_ndvals(ndval_listhead, ndval_listtail)
CNndvalptr ndval_listhead, ndval_listtail;
{
   CNndvalptr P;
   int        count = 0;

   for (P=ndval_listhead; P!=NULL; P=P->next) count++;

   return(count);
}



/*
 * REGIONS
 */

/*
 * regions are used to store boundary information.
 * A region contains a label identifier and a collection of nodes.
 */
static
CNregionptr make_region(matname, matID, ID)
char *matname;
int   matID;
int   ID;
{
   CNregionptr newptr;
   unsigned int size = sizeof(CNregion);

   if ((newptr = (CNregionptr)malloc(size))!=NULL) {
      newptr->matname     = CNcreate_string(matname);
      newptr->matID       = matID;
      newptr->ID          = ID;
      newptr->color       = -1;
      newptr->isair       = CN_FALSE;
      newptr->nocont      = CN_FALSE;
      newptr->polyhead    = NULL;
      newptr->polytail    = NULL;
      newptr->matpolyhead = NULL;
      newptr->matpolytail = NULL;
      newptr->next        = NULL;
      newptr->prev        = NULL;
   }
   return(newptr);
}

/*
 * Insert a region at the tail of the current region list
 */
CNregionptr CNinsert_region(region_listhead,region_listtail, matname, matID, ID)
CNregionptr *region_listhead, *region_listtail;
char        *matname;
int         matID, ID;
{
   CNregionptr next,A,B;

   A = *region_listtail;
   if ((B=make_region(matname, matID, ID))!=NULL) {
      if (A==NULL) {
         *region_listhead = B;
         *region_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next    != NULL) next->prev = B;
         if (B->next == NULL) *region_listtail = B;
      }
   }
   return(B);
}

/*
 * Delete region
 */
void CNdelete_region(region_listhead, region_listtail, R)
CNregionptr *region_listhead, *region_listtail;
CNregionptr R;
{
   CNregionptr prev,next;

   /* Delete the label */
   CNdestroy_string(R->matname);

   /* Delete the polygons associated with the quantity */
   CNdelete_poly_list(&(R->polyhead), &(R->polytail));
   CNdelete_poly_list(&(R->matpolyhead), &(R->matpolytail));

   prev = R->prev;
   next = R->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (R==*region_listhead) *region_listhead = next;
   if (R==*region_listtail) *region_listtail = prev;

   /* Now delete R */
   free ((char*)R);
}


/*
 * Delete all the regions in the list
 */
void CNdelete_region_list(region_listhead, region_listtail)
CNregionptr *region_listhead, *region_listtail;
{
   CNregionptr N;

   while ((N = *region_listhead) != NULL)
      CNdelete_region(region_listhead, region_listtail, N);
}


/*
 * print out the list of regions 
 */
/*ARGSUSED*/
void CNprint_region_list(region_listhead, region_listtail)
CNregionptr region_listhead, region_listtail;
{
   void CNprint_region();
   CNregionptr N;

   for (N=region_listhead; N!=NULL; N=N->next)
      CNprint_region(N);
}


/*
 * print a region 
 */
void CNprint_region(R)
CNregionptr R;
{
   (void) fprintf(stdout,"   RegionID# %3d   Label=\"%s\" \n", 
                  R->ID, R->matname);
   CNprint_poly_list(R->polyhead, R->polytail);
   (void) fflush(stdout);
}


/*
 * Count the number of regions in the list
 */
/*ARGSUSED*/
int CNcount_regions(region_listhead, region_listtail)
CNregionptr region_listhead, region_listtail;
{
   CNregionptr P;
   int        count = 0;

   for (P=region_listhead; P!=NULL; P=P->next) count++;

   return(count);
}


/*
 * Return the size of all regions in the list
 */
/*ARGSUSED*/
int CNregion_list_size(region_listhead, region_listtail)
CNregionptr region_listhead, region_listtail;
{
   CNregionptr C;
   int        size = 0;

   for (C=region_listhead; C!=NULL; C=C->next)
      size += CNregion_size(C);

   return(size);
}


/*
 * Return the (approximate) size of the region
 */
int CNregion_size(Rptr)
CNregionptr Rptr;
{
   int    size;

   if (Rptr == NULL) return(0);

   /* The basic size */
   size = sizeof(CNregion);

   /* Count the contents of the region */
   size += CNpoly_list_size(Rptr->polyhead,Rptr->polytail);

   /* return */
   return(size);
}


