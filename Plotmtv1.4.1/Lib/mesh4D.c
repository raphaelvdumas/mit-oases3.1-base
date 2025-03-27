/*
 * mesh4D.c - procedures to build and maintain a volumetric grid
 *            data structure used by the mesh4D-type dataset
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include "CNplot.h"

static double get_1D_double_array_value();

/*
 * MESH4D DATA STRUCTURE
 *    A mesh4D is used to store a volumetric grid and single-valued
 *    data (quantities) stored on that grid.
 *    The data is represented in the form of arrays.
 *    The mesh4D_parent has the volumetric grid, while the mesh4D_child
 *    only contains the qarray.
 */

/*
 * Allocate room for a mesh4D data structure
 */
CNmesh4Dptr CNmake_mesh4D(ID)
int    ID;
{
   CNmesh4Dptr newptr;
   unsigned int size = sizeof(CNmesh4D);

   if ((newptr = (CNmesh4Dptr)malloc(size))!=NULL) {
      newptr->ID         = ID;
      newptr->flag       = 0;
      newptr->xarray     = NULL;
      newptr->yarray     = NULL;
      newptr->zarray     = NULL;
      newptr->qarray     = NULL;
      newptr->mat1_array = NULL;
      newptr->mat2_array = NULL;
      newptr->prism_array= NULL;
      newptr->nx         = 0;
      newptr->ny         = 0;
      newptr->nz         = 0;
      newptr->nq         = 0;
      newptr->xmin       = 0.0;
      newptr->xmax       = 0.0;
      newptr->ymin       = 0.0;
      newptr->ymax       = 0.0;
      newptr->zmin       = 0.0;
      newptr->zmax       = 0.0;
      newptr->qmin       = 0.0;
      newptr->qmax       = 0.0;
      newptr->regionhead = NULL;
      newptr->regiontail = NULL;
      newptr->quant4Dhead= NULL;
      newptr->quant4Dtail= NULL;
   }
   return(newptr);
}


/*
 * Delete mesh4D
 */
void CNdelete_mesh4D(Gptr)
CNmesh4Dptr Gptr;
{
   /* Free the arrays */
   if (Gptr->xarray) free((char *)Gptr->xarray); 
   if (Gptr->yarray) free((char *)Gptr->yarray); 
   if (Gptr->zarray) free((char *)Gptr->zarray); 
   if (Gptr->qarray) free((char *)Gptr->qarray); 
   if (Gptr->mat1_array ) free((char *)Gptr->mat1_array); 
   if (Gptr->mat2_array ) free((char *)Gptr->mat2_array); 
   if (Gptr->prism_array) free((char *)Gptr->prism_array); 
   
   /* Free the regions */
   CNdelete_region_list(&(Gptr->regionhead), &(Gptr->regiontail));

   /* Free the quantities */
   CNdelete_quant4D_list(&(Gptr->quant4Dhead), &(Gptr->quant4Dtail));

   /* Reset the array counts */
   Gptr->nx = 0;
   Gptr->ny = 0;
   Gptr->nz = 0;
   Gptr->nq = 0;

   /* Now delete Gptr */
   free ((char*)Gptr);
}


/*
 * Retrieve data from the grid
 */

/* x-value */
double CNmesh4D_x(Gptr,i)
CNmesh4Dptr Gptr;
int         i;
{
   double val;
   val = get_1D_double_array_value(Gptr->xarray,i,Gptr->nx);
   return(val);
}

/* y-value */
double CNmesh4D_y(Gptr,i)
CNmesh4Dptr Gptr;
int         i;
{
   double val;
   val = get_1D_double_array_value(Gptr->yarray,i,Gptr->ny);
   return(val);
}

/* z-value */
double CNmesh4D_z(Gptr,i)
CNmesh4Dptr Gptr;
int         i;
{
   double val;
   val = get_1D_double_array_value(Gptr->zarray,i,Gptr->nz);
   return(val);
}

/* q-value */
double CNmesh4D_q(Gptr,i,j,k)
CNmesh4Dptr Gptr;
int         i,j,k;
{
   double val;
   int    t;

   t   = i + j*Gptr->nx + k*Gptr->nx*Gptr->ny;
   val = get_1D_double_array_value(Gptr->qarray,t,Gptr->nq);
   return(val);
}

/* mat1-value */
double CNmesh4D_m1(Gptr,i,j,k)
CNmesh4Dptr Gptr;
int         i,j,k;
{
   double val;
   int    t, nc;

   nc  = (Gptr->nx - 1)*(Gptr->ny - 1)*(Gptr->nz - 1);
   t   = i + j*(Gptr->nx -1) + k*(Gptr->nx - 1)*(Gptr->ny - 1);
   val = get_1D_double_array_value(Gptr->mat1_array,t,nc);
   return(val);
}

/* mat2-value */
double CNmesh4D_m2(Gptr,i,j,k)
CNmesh4Dptr Gptr;
int         i,j,k;
{
   double val;
   int    t, nc;

   nc  = (Gptr->nx - 1)*(Gptr->ny - 1)*(Gptr->nz - 1);
   t   = i + j*(Gptr->nx -1) + k*(Gptr->nx - 1)*(Gptr->ny - 1);
   val = get_1D_double_array_value(Gptr->mat2_array,t,nc);
   return(val);
}

/* mat1-value */
double CNmesh4D_mp(Gptr,i,j,k)
CNmesh4Dptr Gptr;
int         i,j,k;
{
   double val;
   int    t, nc;

   nc  = (Gptr->nx - 1)*(Gptr->ny - 1)*(Gptr->nz - 1);
   t   = i + j*(Gptr->nx -1) + k*(Gptr->nx - 1)*(Gptr->ny - 1);
   val = get_1D_double_array_value(Gptr->prism_array,t,nc);
   return(val);
}

/*
 * Print info on the mesh4D
 */
void CNprint_mesh4D(Gptr)
CNmesh4Dptr Gptr;
{
   int i,j,k,i0,j0,k0;

   (void) fprintf(stdout,"Mesh4D Grid %d:\n",Gptr->ID);
   (void) fprintf(stdout,"   x[0] = %8.3g  x[%5d] = %8.3g\n",
                  CNmesh4D_x(Gptr,0),
                  Gptr->nx-1,
                  CNmesh4D_x(Gptr,Gptr->nx-1));
   (void) fprintf(stdout,"   y[0] = %8.3g  y[%5d] = %8.3g\n",
                  CNmesh4D_y(Gptr,0),
                  Gptr->ny-1, 
                  CNmesh4D_y(Gptr,Gptr->ny-1));
   (void) fprintf(stdout,"   z[0] = %8.3g  z[%5d] = %8.3g\n",
                  CNmesh4D_z(Gptr,0),
                  Gptr->nz-1, 
                  CNmesh4D_z(Gptr,Gptr->nz-1));
   for (k=0; k<2; k++)
   for (i=0; i<2; i++)
   for (j=0; j<2; j++) {
       i0 = i*(Gptr->nx-1);
       j0 = j*(Gptr->ny-1);
       k0 = k*(Gptr->nz-1);
       (void) fprintf(stdout,"   t[%2d][%2d][%2d] = %8.3g\n",
                      i0, j0, k0, CNmesh4D_q(Gptr,i0, j0, k0));
   }
}


/*
 * QUANT 4D - this is used only on a temporary basis to store 
 *            quantities based on mesh4D structures
 */

/*
 * quantities are used to store field information at nodes
 */
CNquant4Dptr CNmake_quant4D(label, qarray, npts, ID)
char       *label;
double     *qarray;
int        npts;
int        ID;
{
   CNquant4Dptr newptr;
   unsigned int size = sizeof(CNquant4D);
 
   if ((newptr = (CNquant4Dptr)malloc(size))!=NULL) {
      newptr->ID        = ID;
      newptr->name      = CNcreate_string(label);
      newptr->qarray    = qarray;
      newptr->npts      = npts;
      newptr->next      = NULL;
      newptr->prev      = NULL;
   }
   return(newptr);
}


/*
 * Insert a quantity at the tail of the current quantity list
 */
CNquant4Dptr CNinsert_quant4D(quant4D_listhead,quant4D_listtail,
                              label, qarray, npts, ID)
CNquant4Dptr *quant4D_listhead, *quant4D_listtail;
char         *label;
double       *qarray;
int          npts;
int          ID;
{
   CNquant4Dptr next,A,B;
 
   A = *quant4D_listtail;
   if ((B=CNmake_quant4D(label, qarray, npts, ID))!=NULL) {
      if (A==NULL) {
         *quant4D_listhead = B;
         *quant4D_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next    != NULL) next->prev = B;
         if (B->next == NULL) *quant4D_listtail = B;
      }
   }
   return(B);
}


/*
 * Insert a quantity at the tail of the current quantity list
 */
void CNadd_quant4D(quant4D_listhead,quant4D_listtail,quant4D)
CNquant4Dptr *quant4D_listhead, *quant4D_listtail, quant4D;
{
   CNquant4Dptr next,A,B;
 
   A = *quant4D_listtail;
   if ((B=quant4D)!=NULL) {
      if (A==NULL) {
         *quant4D_listhead = B;
         *quant4D_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next    != NULL) next->prev = B;
         if (B->next == NULL) *quant4D_listtail = B;
      }
   }
}


/*
 * Delete quant
 */
void CNdelete_quant4D(quant4D_listhead, quant4D_listtail, Q)
CNquant4Dptr *quant4D_listhead, *quant4D_listtail;
CNquant4Dptr Q;
{
   CNquant4Dptr prev,next;
 
   /* Delete the label */
   CNdestroy_string(Q->name);
 
   /* Delete the array associated with the quantity */
   if (Q->qarray) free((char *)(Q->qarray));
   Q->qarray = NULL;
 
   prev = Q->prev;
   next = Q->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (Q==*quant4D_listhead) *quant4D_listhead = next;
   if (Q==*quant4D_listtail) *quant4D_listtail = prev;
 
   /* Now delete Q */
   free ((char*)Q);
}


/*
 * Delete all the quantities in the list
 */
void CNdelete_quant4D_list(quant4D_listhead, quant4D_listtail)
CNquant4Dptr *quant4D_listhead, *quant4D_listtail;
{
   CNquant4Dptr Q;
 
   while ((Q = *quant4D_listhead) != NULL)
      CNdelete_quant4D(quant4D_listhead, quant4D_listtail, Q);
}


/*
 * print out the list of quantities
 */
/*ARGSUSED*/
void CNprint_quant4D_list(quant4D_listhead, quant4D_listtail)
CNquant4Dptr quant4D_listhead, quant4D_listtail;
{
   CNquant4Dptr Q;
 
   for (Q=quant4D_listhead; Q!=NULL; Q=Q->next)
      CNprint_quant4D(Q);
}


/*
 * print a quantity
 */
void CNprint_quant4D(Q)
CNquant4Dptr Q;
{
   (void) fprintf(stdout,
                  "   QuantID# %3d   Label=\"%s\"  Array=%d\n",
                  Q->ID, Q->name, Q->qarray);
   (void) fflush(stdout);
}
 
 
/*
 * Count the number of quants in the list
 */
/*ARGSUSED*/
int CNcount_quant4Ds(quant4D_listhead, quant4D_listtail)
CNquant4Dptr quant4D_listhead, quant4D_listtail;
{
   CNquant4Dptr P;
   int        count = 0;
 
   for (P=quant4D_listhead; P!=NULL; P=P->next) count++;
 
   return(count);
}


/*
 * Array routines 
 */

/*
 * get the value of an array element
 */
static double get_1D_double_array_value(arrptr,i,isize)
double *arrptr;
int i,isize;
{
   double val;
   if (i<0 || i>=isize) {
      (void) fprintf(stderr,"Element [%d] is out of bounds!\n",i);
      return(0.0);
   }
   val = *(arrptr + i);
   return(val);
}
