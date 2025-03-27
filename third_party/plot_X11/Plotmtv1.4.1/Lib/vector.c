/*
 * vector.c - linked list procedures building and maintaining lists
 *           of vectors 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include <math.h>
#include "CNdata.h"
#include "CNround.h"
#include "CNvector.h"

static void print_vector();

/*
 * VECTOR MANIPULATION UTILITITES
 */

/*
 * Calculate the default scaling factor for vectors
 */
double CNdefault_vector_scale(vectorhead, vectortail,
                              xmin, xmax, ymin, ymax, zmin, zmax, vlen_max)
CNvecptr vectorhead, vectortail;               /* Vector list         */
double   xmin, xmax, ymin, ymax, zmin, zmax;   /* Vector bounding box */
double   vlen_max;                             /* Max vector length   */
{
   double vscale=1.0;
   double max_len;
   double delx, dely, delz, delt;
   int    smallx, smally, smallz;
   int    nvectors;

   /* Find the number of vectors */
   nvectors = CNcount_vectors(vectorhead, vectortail);
   if (nvectors == 0) return(1.0);

   /* Get the maximum magnitude */
   max_len = vlen_max;

   /* Figure out the best scale factor */
   delx = fabs(xmax-xmin);
   dely = fabs(ymax-ymin);
   delz = fabs(zmax-zmin);
   smallx = delx < CN_SMALLER;
   smally = dely < CN_SMALLER;
   smallz = delz < CN_SMALLER;
   if (smallx && smally && smallz)
      delt  = max_len;
   else if (smallx && smally)
      delt  = 0.2*delz/(double)nvectors;
   else if (smally && smallz)
      delt  = 0.2*delx/(double)nvectors;
   else if (smallx && smallz)
      delt  = 0.2*dely/(double)nvectors;
   else if (smallx)
      delt  = 0.3*SMALLER_OF(dely,delz)/sqrt((double)nvectors);
   else if (smally)
      delt  = 0.3*SMALLER_OF(delx,delz)/sqrt((double)nvectors);
   else if (smallz)
      delt  = 0.3*SMALLER_OF(delx,dely)/sqrt((double)nvectors);
   else
      delt  = 0.3*MINOF3(delx,dely,delz)/exp(0.33*log((double)nvectors));

   vscale    = (max_len <= 0.0) ? 0.0 : delt/max_len;

   /* Return */
   return(vscale);
}

/*
 * Calculate the maximum magnitude of a collection of vectors
 */
/*ARGSUSED*/
double CNmax_vector(vectorhead, vectortail)
CNvecptr vectorhead, vectortail;
{
   CNvecptr V;
   double   lsqmax = 0.0;
   double   lsq, max_len;

   /* Go thru the vector list */
   for (V=vectorhead; V!=NULL; V=V->next)
      if ((lsq = V->vx*V->vx + V->vy*V->vy + V->vz*V->vz) > lsqmax) 
         lsqmax = lsq;   

   /* Take the square-root */
   max_len = sqrt(lsqmax);

   /* Return */
   return(max_len);
}

/*
 * Calculate the minimum magnitude of a collection of vectors
 * This actually finds the smallest non-zero-length vector
 */
/*ARGSUSED*/
double CNmin_vector(vectorhead, vectortail)
CNvecptr vectorhead, vectortail;
{
   CNvecptr V;
   double   lsqmin = CN_LARGE;
   double   lsq, min_len;
   int      MIN_TOUCHED = CN_FALSE;

   /* Go thru the vector list */
   for (V=vectorhead; V!=NULL; V=V->next) {
      lsq = V->vx*V->vx + V->vy*V->vy + V->vz*V->vz;
      if (lsq < lsqmin && lsq > CN_TINY) {
         lsqmin = lsq; 
         MIN_TOUCHED = CN_TRUE;
      }
   }

   /* Take the square-root */
   min_len = sqrt(lsqmin);

   /* If no non-zero min was found... */
   if (!MIN_TOUCHED) min_len = CN_SMALL;

   /* Return */
   return(min_len);
}

/*
 * Log Utility for vectors
 * If the value is less than 1, return 0.
 * Otherwise return log of the absolute value multiplied by the sign
 */
double CNveclog10(x)
double x;
{ 
   double val;
   int    sign;

   if (fabs(x) < 1.0)
      val  = 0.0;
   else {
      sign = (x > 0.0) ? 1 : -1;
      val  = sign * log10(fabs(x));
   }
   return(val);
}



/*
 * VECTOR BOX
 *    a vector-box is used to store the list of vectors
 */

/* 
 * Allocate room for a vector-box
 */
CNvecboxptr CNmake_vectorbox(vectorhead, vectortail, 
                             vlen_min, vlen_max, ID)
CNvecptr    vectorhead, vectortail;
double      vlen_min, vlen_max;
int         ID;
{
   CNvecboxptr newptr;
   unsigned int size = sizeof(CNvecbox);

   if ((newptr = (CNvecboxptr)malloc(size))!=NULL) {
      newptr->ID         = ID;
      newptr->linetype   = CN_LN_SOLID;
      newptr->linecolor  = 0;
      newptr->linewidth  = 1;
      newptr->marktype   = CN_MK_SQUARE1;
      newptr->markcolor  = 0;
      newptr->vlen_min   = vlen_min;
      newptr->vlen_max   = vlen_max;
      newptr->vectorhead = vectorhead;
      newptr->vectortail = vectortail;
   }
   return(newptr);
}


/*
 * Delete vector-box
 */
void CNdelete_vectorbox(Vptr)
CNvecboxptr Vptr;
{
   /* Free the vectors */
   if (Vptr->vectorhead != NULL)
   CNdelete_vector_list(&(Vptr->vectorhead),&(Vptr->vectortail));

   /* Now delete Vptr */
   free ((char*)Vptr);
}


/*
 * Print information on the vector box 
 */
void CNprint_vectorbox(Vptr, verbose)
CNvecboxptr Vptr;
int         verbose;
{
   int nvectors=CNcount_vectors(Vptr->vectorhead, Vptr->vectortail);
    
   (void) fprintf(stdout,"Vector-box ID = %s\n",Vptr->ID);
   (void) fprintf(stdout,"   Max Length = %g\n",Vptr->vlen_max);
   (void) fprintf(stdout,"   Min Length = %g\n",Vptr->vlen_min);
   (void) fprintf(stdout,"   No. vectors= %d\n",nvectors);
   if (verbose) CNprint_vector_list(Vptr->vectorhead, Vptr->vectortail);
}


/*
 * VECTOR DATA STRUCTURE
 *    vectors are used to store the physical coordinates (x,y,z)
 */

/*
 * Allocate room for a vector
 */
CNvecptr CNmake_vector(x,y,z,vx,vy,vz,ID)
double x,y,z,vx,vy,vz; 
int    ID;
{
   CNvecptr newptr;
   unsigned int size = sizeof(CNvector);

   if ((newptr = (CNvecptr)malloc(size))!=NULL) {
      newptr->ID     = ID;
      newptr->flag   = 0;
      newptr->noplot = CN_FALSE;
      newptr->x      = x;
      newptr->y      = y;
      newptr->z      = z;
      newptr->vx     = vx;
      newptr->vy     = vy;
      newptr->vz     = vz;
      newptr->nd     = NULL;
      newptr->next   = NULL;
      newptr->prev   = NULL;
   }
   return(newptr);
}

/*  
 * Insert a vector at the tail of the current vector list
 */
CNvecptr CNinsert_vector(vector_listhead, vector_listtail, 
                         x, y, z, vx, vy, vz, ID)
CNvecptr   *vector_listhead, *vector_listtail;
double     x, y, z, vx, vy, vz;
int        ID;
{
   CNvecptr CNmake_vector();
   CNvecptr next,A,B;

   A = *vector_listtail;
   if ((B=CNmake_vector(x,y,z,vx,vy,vz,ID))!=NULL) {
      if (A==NULL) {
         *vector_listhead = B;
         *vector_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next != NULL) next->prev = B;
         if (B->next == NULL) *vector_listtail = B;
      }
   }
   return(B);
}


/*
 * Delete vector at address L 
 */
void CNdelete_vector(vector_listhead, vector_listtail, L)
CNvecptr *vector_listhead, *vector_listtail;
CNvecptr L;
{
   CNvecptr prev,next;

   prev = L->prev;
   next = L->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (L== *vector_listhead) *vector_listhead = next;
   if (L== *vector_listtail) *vector_listtail = prev;
   free ((char*)L);
   L = NULL;
}


/* 
 * Delete all the vectors in the list
 */
void CNdelete_vector_list(vector_listhead, vector_listtail)
CNvecptr *vector_listhead, *vector_listtail;
{
   CNvecptr P;

   while ((P = *vector_listhead) != NULL)
      CNdelete_vector(vector_listhead, vector_listtail, P);
}


/* 
 * Print out the list of vectors 
 */
/*ARGSUSED*/
void CNprint_vector_list(vector_listhead, vector_listtail)
CNvecptr vector_listhead, vector_listtail;
{
   CNvecptr P;

   for (P=vector_listhead; P!=NULL; P=P->next) 
      print_vector(P);
}


/* 
 * print the coordinates of a vector
 */
static void print_vector(pt)
CNvecptr pt;
{
   (void) fprintf(stdout,"x =%8.5g y =%8.5g z =%8.5g\n",pt->x, pt->y, pt->z);
   (void) fprintf(stdout,"vx=%8.5g vy=%8.5g vz=%8.5g\n",pt->vx,pt->vy,pt->vz);
}


/*
 * Count the number of vectors in the list 
 */
/*ARGSUSED*/
int CNcount_vectors(vector_listhead, vector_listtail)
CNvecptr vector_listhead, vector_listtail;
{
   CNvecptr P;
   int        count = 0;

   for (P=vector_listhead; P!=NULL; P=P->next) count++;

   return(count);
}

