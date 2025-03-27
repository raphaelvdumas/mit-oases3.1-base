/***   sort.c - for sorting   ***/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <malloc.h>
#include "CNplot.h"

/*****************************************************
*          Implementation of Quicksort algorithm     *
*          By Edward W. Scheckler   August 3, 1990   *
*	            EECS Dept.  UC-Berkeley	     *
*	   					     *
*	   For complicated plots, this algorithm     *
* 	   runs in approx. 2NlnN time, which is      *
*	   orders of magnitude faster then bubble    *
*	   sort methods running as N**2              *
*	   This algorithm is not very efficient for  *
*	   lists that are already mostly in order.   *
*	   In those cases, bubble sort is best       *
*	   so do_quick_sort first checks to see      *
*	   if the data is almost ordered.            *
*****************************************************/

void   CNdo_quick_sort_xpoints(); 
static CNpointptr *allocate_quicksort_point_array();
static void       quicksort_xpoints();

void   CNdo_quick_sort_curves(); 
static CNcurveptr *allocate_quicksort_curve_array();
static void       quicksort_curves();
void   CNbubble_sort_curves();
static double calc_curve_zcenter();

void   CNdo_quick_sort_trias(); 
static CNtriaptr *allocate_quicksort_tria_array();
static void       quicksort_trias();
void   CNbubble_sort_trias();
static double calc_tria_zcenter();

void   CNdo_quick_sort_rects(); 
static CNrectptr *allocate_quicksort_rect_array();
static void       quicksort_rects();
void   CNbubble_sort_rects();
static double calc_rect_zcenter();

void   CNdo_quick_sort_elems(); 
static CNelemptr *allocate_quicksort_elem_array();
static void       quicksort_elems();
void   CNbubble_sort_elems();
static double calc_elem_zcenter();

void   CNdo_quick_sort_cubes(); 
static CNcubeptr *allocate_quicksort_cube_array();
static void       quicksort_cubes();
void   CNbubble_sort_cubes();
static double calc_cube_zcenter();

void   CNdo_quick_sort_blocks(); 
static CNblockptr *allocate_quicksort_block_array();
static void       quicksort_blocks();
void   CNbubble_sort_blocks();
static double calc_block_zcenter();

void   CNdo_quick_sort_polys(); 
static CNpolyptr  *allocate_quicksort_poly_array();
static void       quicksort_polys();
void   CNbubble_sort_polys();
static double calc_poly_zcenter();


/*
 * SORT POINTS by x ordinate 
 */
void CNdo_quick_sort_xpoints(pointhead, pointtail) 
CNpointptr *pointhead, *pointtail;
{
   CNpointptr S,A,B,C;
   CNpointptr *a;
   int        narray,in,sort_times;
   int        sorted;

   /* allocate an array for holding pointers to the points */
   narray = CNcount_points(*pointhead, *pointtail);
   a = allocate_quicksort_point_array(narray);

   /* set the elements of the pointptr array a                 */
   in = 0;
   for (S=(*pointhead); S!=NULL; S=S->next) {
      *(a + in) = S;
      in ++;
   }

   /*first check if already sorted*/
   sort_times = 0;
   do {
      sorted = CN_TRUE;
      for (S=(*pointhead); S->next!=NULL; S=S->next) {
         /* compare each adjacent pair of elements */
         B = S->next;
         if (S->x > B->x) {
            A = S->prev;
            C = B->next;
            if (S== *pointhead) *pointhead = B;
            if (B== *pointtail) *pointtail = S;
            if (A!=NULL) A->next = B;
            B->next = S;
            B->prev = A;
            S->next = C;
            S->prev = B;
            if (C!=NULL) C->prev = S;
            S = S->prev;
            sorted = CN_FALSE;
         }
      }
      sort_times++;
   } while (!sorted && sort_times < 5);

   /* If it's sorted, return */
   if (sorted){
      free((char *)a);
      return;
   }

   /*do the quicksort*/
   quicksort_xpoints(a,0,narray-1,narray-1);

   /*put the sorted array elements back into the point list*/
   *pointhead = a[0];
   *pointtail = a[narray-1];
   for(in = 0; in < narray; in++){
	if(in+1 < narray){
	a[in]->next = a[in+1];
	} else {
	a[in]->next = NULL;
	}
	if(in-1 >= 0 ){
	a[in]->prev = a[in-1];
	}else {
	a[in]->prev= NULL;
	}
   }

   free((char *)a);
   
}

/* Allocate room for a bunch of points */
static CNpointptr *allocate_quicksort_point_array(narr)
int narr;
{
   CNpointptr *newptr;
   unsigned int arr_size;
   arr_size = (unsigned)(narr)*sizeof(CNpointptr);
   if ((newptr = (CNpointptr *)malloc(arr_size))==NULL) {
      (void) fprintf(stderr,"Out of memory\n");
      exit(-1);
   }
   return(newptr);
}

/* Sort out points */
static void quicksort_xpoints(a,l,r,narr)
CNpointptr *a;
int l,r,narr;
{
	int i,j;
	int p;
	int sortflag;
	CNpointptr v,t,ta;
	/*if the list is small, do an insertion sort for speed*/
	if(r-l > 5){
		v = a[r];
		i = l - 1;
		j = r;
		do{
			do{
				i++;
			} while(i< narr &&!(a[i]->x >= v->x));
			do{
				j--;
			}while( j > 0 && !(a[j]->x <= v->x));
			t = a[i];
			a[i] = a[j];
			a[j] = t;
		}while(!(j <= i));
			a[j] = a[i];
			a[i] = a[r];
			a[r] = t;
			quicksort_xpoints(a,l,i-1,narr);
			quicksort_xpoints(a,i+1,r,narr);
	} else {
		do{
		   sortflag = 0;
		   for(p=l;p<r;p++){
		      if(a[p]->x > a[p+1]->x){
			  ta = a[p];
			  a[p] = a[p+1];
			  a[p+1] = ta;
			  sortflag = 1;
		      }
		   }
	        }while(sortflag != 0);
	}

}	



/*
 * SORT CURVES
 */
void CNdo_quick_sort_curves(curvehead, curvetail, view_transfo, xlog,ylog,zlog) 
CNcurveptr *curvehead, *curvetail;
CNmatrix   view_transfo;
short      xlog, ylog, zlog;
{
   CNcurveptr S,A,B,C;
   CNcurveptr *a;
   int        narray,in,sort_times;
   int        sorted;

   /* allocate an array for holding pointers to the curves */
   narray = CNcount_curves(*curvehead, *curvetail);
   a = allocate_quicksort_curve_array(narray);

   /* first calculate the z-ave value of all the points in the list */
   /* also set the elements of the curveptr array a                 */
   in = 0;
   for (S=(*curvehead); S!=NULL; S=S->next) {
      *(a + in) = S;
      in ++;

      /* Calculate the z-value of the curve center */
      S->zave = calc_curve_zcenter(S, view_transfo, xlog, ylog, zlog);
   }

   /*first check if already sorted*/
   sort_times = 0;
   do {
      sorted = CN_TRUE;
      for (S=(*curvehead); S->next!=NULL; S=S->next) {
         /* compare each adjacent pair of elements */
         B = S->next;
         if (S->zave > B->zave) {
            A = S->prev;
            C = B->next;
            if (S== *curvehead) *curvehead = B;
            if (B== *curvetail) *curvetail = S;
            if (A!=NULL) A->next = B;
            B->next = S;
            B->prev = A;
            S->next = C;
            S->prev = B;
            if (C!=NULL) C->prev = S;
            S = S->prev;
            sorted = CN_FALSE;
         }
      }
      sort_times++;
   } while (!sorted && sort_times < 5);

   /* If it's sorted, return */
   if (sorted){
      free((char *)a);
      return;
   }

   /*do the quicksort*/
   quicksort_curves(a,0,narray-1,narray-1);

   /*put the sorted array elements back into the curve list*/
   
   *curvehead = a[0];
   *curvetail = a[narray-1];
   for(in = 0; in < narray; in++){
	if(in+1 < narray){
	a[in]->next = a[in+1];
	} else {
	a[in]->next = NULL;
	}
	if(in-1 >= 0 ){
	a[in]->prev = a[in-1];
	}else {
	a[in]->prev= NULL;
	}
   }

   free((char *)a);
   
}

/* Allocate room for a bunch of curves */
static CNcurveptr *allocate_quicksort_curve_array(narr)
int narr;
{
   CNcurveptr *newptr;
   unsigned int arr_size;
   arr_size = (unsigned)(narr)*sizeof(CNcurveptr);
   if ((newptr = (CNcurveptr *)malloc(arr_size))==NULL) {
      (void) fprintf(stderr,"Out of memory\n");
      exit(-1);
   }

   return(newptr);
}

/* Sort out curves */
static void quicksort_curves(a,l,r,narr)
CNcurveptr *a;
int l,r,narr;
{
	int i,j;
	int p;
	int sortflag;
	CNcurveptr v,t,ta;
	/*if the list is small, do an insertion sort for speed*/
	if(r-l > 5){
		v = a[r];
		i = l - 1;
		j = r;
		do{
			do{
				i++;
			} while(i< narr &&!(a[i]->zave >= v->zave));
			do{
				j--;
			}while( j > 0 && !(a[j]->zave <= v->zave));
			t = a[i];
			a[i] = a[j];
			a[j] = t;
		}while(!(j <= i));
			a[j] = a[i];
			a[i] = a[r];
			a[r] = t;
			quicksort_curves(a,l,i-1,narr);
			quicksort_curves(a,i+1,r,narr);
	} else {
		do{
		   sortflag = 0;
		   for(p=l;p<r;p++){
		      if(a[p]->zave > a[p+1]->zave){
			  ta = a[p];
			  a[p] = a[p+1];
			  a[p+1] = ta;
			  sortflag = 1;
		      }
		   }
	        }while(sortflag != 0);
	}

}	


/*
 * Use a bubble sort to sort out curves
 */
void CNbubble_sort_curves(curvehead, curvetail, view_transfo, xlog,ylog,zlog) 
CNcurveptr *curvehead, *curvetail;
CNmatrix   view_transfo;
short      xlog, ylog, zlog;
{
   CNcurveptr S,A,B,C;
   int        sorted;

   /* first calculate the z-ave value of all the points in the list */
   for (S=(*curvehead); S!=NULL; S=S->next) {

      /* Calculate the z-value of the curve center */
      S->zave = calc_curve_zcenter(S, view_transfo, xlog, ylog, zlog);
   }

   /* 
    * now sort according to average z value 
    * the first structure should have the smallest z-value 
    */
   do {
      sorted = CN_TRUE;
      for (S=(*curvehead); S->next!=NULL; S=S->next) {
         /* compare each adjacent pair of elements */
         B = S->next;
         if (S->zave > B->zave) {
            /* A -> S -> B -> C */
            /* A -> B -> S -> C */
            A = S->prev;
            C = B->next;
            if (S== *curvehead) *curvehead = B;
            if (B== *curvetail) *curvetail = S;
            if (A!=NULL) A->next = B;
            B->next = S;
            B->prev = A;
            S->next = C;
            S->prev = B;
            if (C!=NULL) C->prev = S;
            S = S->prev;
            sorted = CN_FALSE;
         }
      }
   } while (!sorted);
}


/*
 * Calculate the z-value of the curve center 
 */
static double calc_curve_zcenter(C, view_transfo, xlog, ylog, zlog)
CNcurveptr C;
CNmatrix   view_transfo;
short      xlog, ylog, zlog;
{
   CNcoord point,newpt;
   double  ztot, zave;
   int     npts;
   CNpointptr P;
   int     longline;

   /* Initialize */
   zave = 0.0;
   ztot = 0.0;
   npts = 0;

   /* Calculate the z-value of the curve center */

   if (C->pointhead == NULL) return 0.0;
 
   /* Check to see if the curve is joined */
   longline = CNlongline(C->pointhead, C->pointtail, 1.0e-5);
 
   /* Calculate the z-value */
   for (P=C->pointhead; P!=NULL; P=P->next) {
 
      /* If the last point is same as the first point, don't count it */
      if ((P->next == NULL) && !longline) continue;
 
      /* Transform the point, taking into account logarithms */
      point.x = (xlog) ? CNlog10(P->x) : P->x;
      point.y = (ylog) ? CNlog10(P->y) : P->y;
      point.z = (zlog) ? CNlog10(P->z) : P->z;
      newpt = CNtransform_point(&point,view_transfo);
      ztot = ztot + newpt.z;
      npts++;
   }
   if (npts>0) zave = ztot/(double)npts;

   /* Return */
   return zave;
}


/*
 * SORT TRIANGLES
 */
void CNdo_quick_sort_trias(triahead, triatail, view_transfo, xlog,ylog,zlog) 
CNtriaptr *triahead, *triatail;
CNmatrix   view_transfo;
short      xlog, ylog, zlog;
{
   CNtriaptr  S,A,B,C;
   CNtriaptr *a;
   int        narray,in,sort_times;
   int        sorted;

   /* Don't sort if there is nothing in the list */
   if (*triahead == NULL) return;

   /* allocate an array for holding pointers to the trias */
   narray = CNcount_trias(*triahead, *triatail);
   a = allocate_quicksort_tria_array(narray);

   /* first calculate the z-ave value of all the points in the list */
   /* also set the elements of the triaptr array a                 */
   in = 0;
   for (S=(*triahead); S!=NULL; S=S->next) {
      *(a + in) = S;
      in ++;

      /* Calculate the z-value of the triangle center */
      S->zave = calc_tria_zcenter(S, view_transfo, xlog, ylog, zlog);
   }

   /*first check if already sorted*/
   sort_times = 0;
   do {
      sorted = CN_TRUE;
      for (S=(*triahead); S->next!=NULL; S=S->next) {
         /* compare each adjacent pair of elements */
         B = S->next;
         if (S->zave > B->zave) {
            A = S->prev;
            C = B->next;
            if (S== *triahead) *triahead = B;
            if (B== *triatail) *triatail = S;
            if (A!=NULL) A->next = B;
            B->next = S;
            B->prev = A;
            S->next = C;
            S->prev = B;
            if (C!=NULL) C->prev = S;
            S = S->prev;
            sorted = CN_FALSE;
         }
      }
      sort_times++;
   } while (!sorted && sort_times < 5);

   /* If it's sorted, return */
   if (sorted){
      free((char *)a);
      return;
   }

   /*do the quicksort*/
   quicksort_trias(a,0,narray-1,narray-1);

   /*put the sorted array elements back into the triangle list*/
   
   *triahead = a[0];
   *triatail = a[narray-1];
   for(in = 0; in < narray; in++){
	if(in+1 < narray){
	a[in]->next = a[in+1];
	} else {
	a[in]->next = NULL;
	}
	if(in-1 >= 0 ){
	a[in]->prev = a[in-1];
	}else {
	a[in]->prev= NULL;
	}
   }

   free((char *)a);
   
}

/* Allocate room for a bunch of triangles */
static CNtriaptr *allocate_quicksort_tria_array(narr)
int narr;
{
   CNtriaptr *newptr;
   unsigned int arr_size;
   arr_size = (unsigned)(narr)*sizeof(CNtriaptr);
   if ((newptr = (CNtriaptr *)malloc(arr_size))==NULL) {
      (void) fprintf(stderr,"Out of memory\n");
      exit(-1);
   }
   return(newptr);
}

/* Sort out trias */
static void quicksort_trias(a,l,r,narr)
CNtriaptr *a;
int l,r,narr;
{
	int i,j;
	int p;
	int sortflag;
	CNtriaptr v,t,ta;
	/*if the list is small, do an insertion sort for speed*/
	if(r-l > 5){
		v = a[r];
		i = l - 1;
		j = r;
		do{
			do{
				i++;
			} while(i< narr &&!(a[i]->zave >= v->zave));
			do{
				j--;
			}while( j > 0 && !(a[j]->zave <= v->zave));
			t = a[i];
			a[i] = a[j];
			a[j] = t;
		}while(!(j <= i));
			a[j] = a[i];
			a[i] = a[r];
			a[r] = t;
			quicksort_trias(a,l,i-1,narr);
			quicksort_trias(a,i+1,r,narr);
	} else {
		do{
		   sortflag = 0;
		   for(p=l;p<r;p++){
		      if(a[p]->zave > a[p+1]->zave){
			  ta = a[p];
			  a[p] = a[p+1];
			  a[p+1] = ta;
			  sortflag = 1;
		      }
		   }
	        }while(sortflag != 0);
	}

}	


/*
 * Use a bubble sort to sort out trias
 */
void CNbubble_sort_trias(triahead, triatail, view_transfo, xlog,ylog,zlog) 
CNtriaptr *triahead, *triatail;
CNmatrix   view_transfo;
short      xlog, ylog, zlog;
{
   CNtriaptr  S,A,B,C;
   int        sorted;

   /* Don't sort if there is nothing in the list */
   if (*triahead == NULL) return;

   /* first calculate the z-ave value of all the points in the list */
   for (S=(*triahead); S!=NULL; S=S->next) {

      /* Calculate the z-value of the triangle center */
      S->zave = calc_tria_zcenter(S, view_transfo, xlog, ylog, zlog);

   }

   /* 
    * now sort according to average z value 
    * the first structure should have the smallest z-value 
    */
   do {
      sorted = CN_TRUE;
      for (S=(*triahead); S->next!=NULL; S=S->next) {
         /* compare each adjacent pair of elements */
         B = S->next;
         if (S->zave > B->zave) {
            /* A -> S -> B -> C */
            /* A -> B -> S -> C */
            A = S->prev;
            C = B->next;
            if (S== *triahead) *triahead = B;
            if (B== *triatail) *triatail = S;
            if (A!=NULL) A->next = B;
            B->next = S;
            B->prev = A;
            S->next = C;
            S->prev = B;
            if (C!=NULL) C->prev = S;
            S = S->prev;
            sorted = CN_FALSE;
         }
      }
   } while (!sorted);
}


/*
 * Calculate the z-value of the triangle center 
 */
static double calc_tria_zcenter(T, view_transfo, xlog, ylog, zlog)
CNtriaptr T;
CNmatrix   view_transfo;
short      xlog, ylog, zlog;
{
   CNcoord point,newpt;
   double  ztot, zave;
   int     npts;

   /* Initialize */
   zave = 0.0;
   ztot = 0.0;
   npts = 0;

   /* Calculate the z-value of the triangle center */

   /* Transform each point, taking into account logarithms */
   point.x = (xlog) ? CNlog10(T->n1->coord->x) : T->n1->coord->x;
   point.y = (ylog) ? CNlog10(T->n1->coord->y) : T->n1->coord->y;
   point.z = (zlog) ? CNlog10(T->n1->t       ) : T->n1->t;
   newpt = CNtransform_point(&point,view_transfo);
   ztot = ztot + newpt.z;
   npts++;

   point.x = (xlog) ? CNlog10(T->n2->coord->x) : T->n2->coord->x;
   point.y = (ylog) ? CNlog10(T->n2->coord->y) : T->n2->coord->y;
   point.z = (zlog) ? CNlog10(T->n2->t       ) : T->n2->t;
   newpt = CNtransform_point(&point,view_transfo);
   ztot = ztot + newpt.z;
   npts++;

   point.x = (xlog) ? CNlog10(T->n3->coord->x) : T->n3->coord->x;
   point.y = (ylog) ? CNlog10(T->n3->coord->y) : T->n3->coord->y;
   point.z = (zlog) ? CNlog10(T->n3->t       ) : T->n3->t;
   newpt = CNtransform_point(&point,view_transfo);
   ztot = ztot + newpt.z;
   npts++;

   if (npts>0) zave = ztot/(double)npts;

   /* Return */
   return zave;
}


/*
 * SORT RECTANGLES
 */
void CNdo_quick_sort_rects(recthead, recttail, view_transfo, xlog,ylog,zlog) 
CNrectptr *recthead, *recttail;
CNmatrix   view_transfo;
short      xlog, ylog, zlog;
{
   CNrectptr  S,A,B,C;
   CNrectptr *a;
   int        narray,in,sort_times;
   int        sorted;

   /* Don't sort if there is nothing in the list */
   if (*recthead == NULL) return;

   /* allocate an array for holding pointers to the rects */
   narray = CNcount_rects(*recthead, *recttail);
   a = allocate_quicksort_rect_array(narray);

   /* first calculate the z-ave value of all the points in the list */
   /* also set the elements of the rectptr array a                 */
   in = 0;
   for (S=(*recthead); S!=NULL; S=S->next) {
      *(a + in) = S;
      in ++;

      /* Calculate the z-value of the rectangle center */
      S->zave = calc_rect_zcenter(S, view_transfo, xlog, ylog, zlog);
   }

   /*first check if already sorted*/
   sort_times = 0;
   do {
      sorted = CN_TRUE;
      for (S=(*recthead); S->next!=NULL; S=S->next) {
         /* compare each adjacent pair of elements */
         B = S->next;
         if (S->zave > B->zave) {
            A = S->prev;
            C = B->next;
            if (S== *recthead) *recthead = B;
            if (B== *recttail) *recttail = S;
            if (A!=NULL) A->next = B;
            B->next = S;
            B->prev = A;
            S->next = C;
            S->prev = B;
            if (C!=NULL) C->prev = S;
            S = S->prev;
            sorted = CN_FALSE;
         }
      }
      sort_times++;
   } while (!sorted && sort_times < 5);

   /* If it's sorted, return */
   if (sorted){
      free((char *)a);
      return;
   }

   /*do the quicksort*/
   quicksort_rects(a,0,narray-1,narray-1);

   /*put the sorted array elements back into the rectangle list*/
   
   *recthead = a[0];
   *recttail = a[narray-1];
   for(in = 0; in < narray; in++){
	if(in+1 < narray){
	a[in]->next = a[in+1];
	} else {
	a[in]->next = NULL;
	}
	if(in-1 >= 0 ){
	a[in]->prev = a[in-1];
	}else {
	a[in]->prev= NULL;
	}
   }

   free((char *)a);
   
}

/* Allocate room for a bunch of rectangles */
static CNrectptr *allocate_quicksort_rect_array(narr)
int narr;
{
   CNrectptr *newptr;
   unsigned int arr_size;
   arr_size = (unsigned)(narr)*sizeof(CNrectptr);
   if ((newptr = (CNrectptr *)malloc(arr_size))==NULL) {
      (void) fprintf(stderr,"Out of memory\n");
      exit(-1);
   }
   return(newptr);
}

/* Sort out rects */
static void quicksort_rects(a,l,r,narr)
CNrectptr *a;
int l,r,narr;
{
	int i,j;
	int p;
	int sortflag;
	CNrectptr v,t,ta;
	/*if the list is small, do an insertion sort for speed*/
	if(r-l > 5){
		v = a[r];
		i = l - 1;
		j = r;
		do{
			do{
				i++;
			} while(i< narr &&!(a[i]->zave >= v->zave));
			do{
				j--;
			}while( j > 0 && !(a[j]->zave <= v->zave));
			t = a[i];
			a[i] = a[j];
			a[j] = t;
		}while(!(j <= i));
			a[j] = a[i];
			a[i] = a[r];
			a[r] = t;
			quicksort_rects(a,l,i-1,narr);
			quicksort_rects(a,i+1,r,narr);
	} else {
		do{
		   sortflag = 0;
		   for(p=l;p<r;p++){
		      if(a[p]->zave > a[p+1]->zave){
			  ta = a[p];
			  a[p] = a[p+1];
			  a[p+1] = ta;
			  sortflag = 1;
		      }
		   }
	        }while(sortflag != 0);
	}

}	


/*
 * Use a bubble sort to sort out rects
 */
void CNbubble_sort_rects(recthead, recttail, view_transfo, xlog,ylog,zlog) 
CNrectptr *recthead, *recttail;
CNmatrix   view_transfo;
short      xlog, ylog, zlog;
{
   CNrectptr  S,A,B,C;
   int        sorted;

   /* Don't sort if there is nothing in the list */
   if (*recthead == NULL) return;

   /* first calculate the z-ave value of all the points in the list */
   for (S=(*recthead); S!=NULL; S=S->next) {

      /* Calculate the z-value of the rectangle center */
      S->zave = calc_rect_zcenter(S, view_transfo, xlog, ylog, zlog);

   }

   /* 
    * now sort according to average z value 
    * the first structure should have the smallest z-value 
    */
   do {
      sorted = CN_TRUE;
      for (S=(*recthead); S->next!=NULL; S=S->next) {
         /* compare each adjacent pair of elements */
         B = S->next;
         if (S->zave > B->zave) {
            /* A -> S -> B -> C */
            /* A -> B -> S -> C */
            A = S->prev;
            C = B->next;
            if (S== *recthead) *recthead = B;
            if (B== *recttail) *recttail = S;
            if (A!=NULL) A->next = B;
            B->next = S;
            B->prev = A;
            S->next = C;
            S->prev = B;
            if (C!=NULL) C->prev = S;
            S = S->prev;
            sorted = CN_FALSE;
         }
      }
   } while (!sorted);
}


/*
 * Calculate the z-value of the rectangle center 
 */
static double calc_rect_zcenter(R, view_transfo, xlog, ylog, zlog)
CNrectptr R;
CNmatrix   view_transfo;
short      xlog, ylog, zlog;
{
   CNcoord point,newpt;
   double  ztot, zave;
   int     npts;

   /* Initialize */
   zave = 0.0;
   ztot = 0.0;
   npts = 0;

   /* Calculate the z-value of the rectangle center */

   /* Transform each point, taking into account logarithms */
   point.x = (xlog) ? CNlog10(R->n1->coord->x) : R->n1->coord->x;
   point.y = (ylog) ? CNlog10(R->n1->coord->y) : R->n1->coord->y;
   point.z = (zlog) ? CNlog10(R->n1->t       ) : R->n1->t;
   newpt = CNtransform_point(&point,view_transfo);
   ztot = ztot + newpt.z;
   npts++;

   point.x = (xlog) ? CNlog10(R->n2->coord->x) : R->n2->coord->x;
   point.y = (ylog) ? CNlog10(R->n2->coord->y) : R->n2->coord->y;
   point.z = (zlog) ? CNlog10(R->n2->t       ) : R->n2->t;
   newpt = CNtransform_point(&point,view_transfo);
   ztot = ztot + newpt.z;
   npts++;

   point.x = (xlog) ? CNlog10(R->n3->coord->x) : R->n3->coord->x;
   point.y = (ylog) ? CNlog10(R->n3->coord->y) : R->n3->coord->y;
   point.z = (zlog) ? CNlog10(R->n3->t       ) : R->n3->t;
   newpt = CNtransform_point(&point,view_transfo);
   ztot = ztot + newpt.z;
   npts++;

   point.x = (xlog) ? CNlog10(R->n4->coord->x) : R->n4->coord->x;
   point.y = (ylog) ? CNlog10(R->n4->coord->y) : R->n4->coord->y;
   point.z = (zlog) ? CNlog10(R->n4->t       ) : R->n4->t;
   newpt = CNtransform_point(&point,view_transfo);
   ztot = ztot + newpt.z;
   npts++;

   if (npts>0) zave = ztot/(double)npts;

   /* Return */
   return zave;
}


/*
 * SORT ELEMENTS  
 * An element contains either a rectangle or a triangle
 */
void CNdo_quick_sort_elems(elemhead, elemtail, view_transfo, xlog,ylog,zlog) 
CNelemptr *elemhead, *elemtail;
CNmatrix   view_transfo;
short      xlog, ylog, zlog;
{
   CNelemptr  S,A,B,C;
   CNelemptr *a;
   int        narray,in,sort_times;
   int        sorted;

   /* Don't sort if there is nothing in the list */
   if (*elemhead == NULL) return;

   /* allocate an array for holding pointers to the elems */
   narray = CNcount_elems(*elemhead, *elemtail);
   a = allocate_quicksort_elem_array(narray);

   /* first calculate the z-ave value of all the points in the list */
   /* also set the elements of the elemptr array a                 */
   in = 0;
   for (S=(*elemhead); S!=NULL; S=S->next) {
      *(a + in) = S;
      in ++;

      /* Calculate the z-value of the element center */
      S->zave = calc_elem_zcenter(S, view_transfo, xlog, ylog, zlog);

   }

   /*first check if already sorted*/
   sort_times = 0;
   do {
      sorted = CN_TRUE;
      for (S=(*elemhead); S->next!=NULL; S=S->next) {
         /* compare each adjacent pair of elements */
         B = S->next;
         if (S->zave > B->zave) {
            A = S->prev;
            C = B->next;
            if (S== *elemhead) *elemhead = B;
            if (B== *elemtail) *elemtail = S;
            if (A!=NULL) A->next = B;
            B->next = S;
            B->prev = A;
            S->next = C;
            S->prev = B;
            if (C!=NULL) C->prev = S;
            S = S->prev;
            sorted = CN_FALSE;
         }
      }
      sort_times++;
   } while (!sorted && sort_times < 5);

   /* If it's sorted, return */
   if (sorted){
      free((char *)a);
      return;
   }

   /*do the quicksort*/
   quicksort_elems(a,0,narray-1,narray-1);

   /*put the sorted array elements back into the element list*/
   
   *elemhead = a[0];
   *elemtail = a[narray-1];
   for(in = 0; in < narray; in++){
	if(in+1 < narray){
	a[in]->next = a[in+1];
	} else {
	a[in]->next = NULL;
	}
	if(in-1 >= 0 ){
	a[in]->prev = a[in-1];
	}else {
	a[in]->prev= NULL;
	}
   }

   free((char *)a);
   
}

/* Allocate room for a bunch of elements */
static CNelemptr *allocate_quicksort_elem_array(narr)
int narr;
{
   CNelemptr *newptr;
   unsigned int arr_size;
   arr_size = (unsigned)(narr)*sizeof(CNelemptr);
   if ((newptr = (CNelemptr *)malloc(arr_size))==NULL) {
      (void) fprintf(stderr,"Out of memory\n");
      exit(-1);
   }
   return(newptr);
}

/* Sort out elems */
static void quicksort_elems(a,l,r,narr)
CNelemptr *a;
int l,r,narr;
{
	int i,j;
	int p;
	int sortflag;
	CNelemptr v,t,ta;
	/*if the list is small, do an insertion sort for speed*/
	if(r-l > 5){
		v = a[r];
		i = l - 1;
		j = r;
		do{
			do{
				i++;
			} while(i< narr &&!(a[i]->zave >= v->zave));
			do{
				j--;
			}while( j > 0 && !(a[j]->zave <= v->zave));
			t = a[i];
			a[i] = a[j];
			a[j] = t;
		}while(!(j <= i));
			a[j] = a[i];
			a[i] = a[r];
			a[r] = t;
			quicksort_elems(a,l,i-1,narr);
			quicksort_elems(a,i+1,r,narr);
	} else {
		do{
		   sortflag = 0;
		   for(p=l;p<r;p++){
		      if(a[p]->zave > a[p+1]->zave){
			  ta = a[p];
			  a[p] = a[p+1];
			  a[p+1] = ta;
			  sortflag = 1;
		      }
		   }
	        }while(sortflag != 0);
	}

}	


/*
 * Use a bubble sort to sort out elems
 */
void CNbubble_sort_elems(elemhead, elemtail, view_transfo, xlog,ylog,zlog) 
CNelemptr *elemhead, *elemtail;
CNmatrix   view_transfo;
short      xlog, ylog, zlog;
{
   CNelemptr  S,A,B,C;
   int        sorted;

   /* Don't sort if there is nothing in the list */
   if (*elemhead == NULL) return;

   /* first calculate the z-ave value of all the points in the list */
   for (S=(*elemhead); S!=NULL; S=S->next) {

      /* Calculate the z-value of the element center */
      S->zave = calc_elem_zcenter(S, view_transfo, xlog, ylog, zlog);

   }

   /* 
    * now sort according to average z value 
    * the first structure should have the smallest z-value 
    */
   do {
      sorted = CN_TRUE;
      for (S=(*elemhead); S->next!=NULL; S=S->next) {
         /* compare each adjacent pair of elements */
         B = S->next;
         if (S->zave > B->zave) {
            /* A -> S -> B -> C */
            /* A -> B -> S -> C */
            A = S->prev;
            C = B->next;
            if (S== *elemhead) *elemhead = B;
            if (B== *elemtail) *elemtail = S;
            if (A!=NULL) A->next = B;
            B->next = S;
            B->prev = A;
            S->next = C;
            S->prev = B;
            if (C!=NULL) C->prev = S;
            S = S->prev;
            sorted = CN_FALSE;
         }
      }
   } while (!sorted);
}


/*
 * Calculate the z-value of the element center 
 */
static double calc_elem_zcenter(E, view_transfo, xlog, ylog, zlog)
CNelemptr E;
CNmatrix   view_transfo;
short      xlog, ylog, zlog;
{
   double zave=0.0;

   if ((E->type == CN_TRIA_STR) && (E->tria != NULL)) {
      zave = calc_tria_zcenter(E->tria, view_transfo, xlog, ylog, zlog);
   } else if ((E->type == CN_RECT_STR) && (E->rect != NULL)) {
      zave = calc_rect_zcenter(E->rect, view_transfo, xlog, ylog, zlog);
   }  

   /* Return */
   return zave;
}



/*
 * SORT CUBES
 */
void CNdo_quick_sort_cubes(cubehead, cubetail, view_transfo, xlog,ylog,zlog) 
CNcubeptr *cubehead, *cubetail;
CNmatrix   view_transfo;
short      xlog, ylog, zlog;
{
   CNcubeptr  S,A,B,C;
   CNcubeptr *a;
   int        narray,in,sort_times;
   int        sorted;

   /* Don't sort if there is nothing in the list */
   if (*cubehead == NULL) return;

   /* allocate an array for holding pointers to the cubes */
   narray = CNcount_cubes(*cubehead, *cubetail);
   a = allocate_quicksort_cube_array(narray);

   /* first calculate the z-ave value of all the points in the list */
   /* also set the elements of the cubeptr array a                 */
   in = 0;
   for (S=(*cubehead); S!=NULL; S=S->next) {
      *(a + in) = S;
      in ++;

      /* Calculate the z-value of the cube center */
      S->zave = calc_cube_zcenter(S, view_transfo, xlog, ylog, zlog);
   }

   /*first check if already sorted*/
   sort_times = 0;
   do {
      sorted = CN_TRUE;
      for (S=(*cubehead); S->next!=NULL; S=S->next) {
         /* compare each adjacent pair of elements */
         B = S->next;
         if (S->zave > B->zave) {
            A = S->prev;
            C = B->next;
            if (S== *cubehead) *cubehead = B;
            if (B== *cubetail) *cubetail = S;
            if (A!=NULL) A->next = B;
            B->next = S;
            B->prev = A;
            S->next = C;
            S->prev = B;
            if (C!=NULL) C->prev = S;
            S = S->prev;
            sorted = CN_FALSE;
         }
      }
      sort_times++;
   } while (!sorted && sort_times < 5);

   /* If it's sorted, return */
   if (sorted){
      free((char *)a);
      return;
   }

   /*do the quicksort*/
   quicksort_cubes(a,0,narray-1,narray-1);

   /*put the sorted array elements back into the cube list*/
   
   *cubehead = a[0];
   *cubetail = a[narray-1];
   for(in = 0; in < narray; in++){
	if(in+1 < narray){
	a[in]->next = a[in+1];
	} else {
	a[in]->next = NULL;
	}
	if(in-1 >= 0 ){
	a[in]->prev = a[in-1];
	}else {
	a[in]->prev= NULL;
	}
   }

   free((char *)a);
   
}

/* Allocate room for a bunch of cubes */
static CNcubeptr *allocate_quicksort_cube_array(narr)
int narr;
{
   CNcubeptr *newptr;
   unsigned int arr_size;
   arr_size = (unsigned)(narr)*sizeof(CNcubeptr);
   if ((newptr = (CNcubeptr *)malloc(arr_size))==NULL) {
      (void) fprintf(stderr,"Out of memory\n");
      exit(-1);
   }
   return(newptr);
}

/* Sort out cubes */
static void quicksort_cubes(a,l,r,narr)
CNcubeptr *a;
int l,r,narr;
{
	int i,j;
	int p;
	int sortflag;
	CNcubeptr v,t,ta;
	/*if the list is small, do an insertion sort for speed*/
	if(r-l > 5){
		v = a[r];
		i = l - 1;
		j = r;
		do{
			do{
				i++;
			} while(i< narr &&!(a[i]->zave >= v->zave));
			do{
				j--;
			}while( j > 0 && !(a[j]->zave <= v->zave));
			t = a[i];
			a[i] = a[j];
			a[j] = t;
		}while(!(j <= i));
			a[j] = a[i];
			a[i] = a[r];
			a[r] = t;
			quicksort_cubes(a,l,i-1,narr);
			quicksort_cubes(a,i+1,r,narr);
	} else {
		do{
		   sortflag = 0;
		   for(p=l;p<r;p++){
		      if(a[p]->zave > a[p+1]->zave){
			  ta = a[p];
			  a[p] = a[p+1];
			  a[p+1] = ta;
			  sortflag = 1;
		      }
		   }
	        }while(sortflag != 0);
	}

}	


/*
 * Use a bubble sort to sort out cubes
 */
void CNbubble_sort_cubes(cubehead, cubetail, view_transfo, xlog,ylog,zlog) 
CNcubeptr *cubehead, *cubetail;
CNmatrix   view_transfo;
short      xlog, ylog, zlog;
{
   CNcubeptr  S,A,B,C;
   int        sorted;

   /* Don't sort if there is nothing in the list */
   if (*cubehead == NULL) return;

   /* first calculate the z-ave value of all the points in the list */
   for (S=(*cubehead); S!=NULL; S=S->next) {

      /* Calculate the z-value of the cube center */
      S->zave = calc_cube_zcenter(S, view_transfo, xlog, ylog, zlog);
   }

   /* 
    * now sort according to average z value 
    * the first structure should have the smallest z-value 
    */
   do {
      sorted = CN_TRUE;
      for (S=(*cubehead); S->next!=NULL; S=S->next) {
         /* compare each adjacent pair of elements */
         B = S->next;
         if (S->zave > B->zave) {
            /* A -> S -> B -> C */
            /* A -> B -> S -> C */
            A = S->prev;
            C = B->next;
            if (S== *cubehead) *cubehead = B;
            if (B== *cubetail) *cubetail = S;
            if (A!=NULL) A->next = B;
            B->next = S;
            B->prev = A;
            S->next = C;
            S->prev = B;
            if (C!=NULL) C->prev = S;
            S = S->prev;
            sorted = CN_FALSE;
         }
      }
   } while (!sorted);
}

/*
 * Calculate the z-value of the cube center 
 */
static double calc_cube_zcenter(C, view_transfo, xlog, ylog, zlog)
CNcubeptr C;
CNmatrix   view_transfo;
short      xlog, ylog, zlog;
{
   CNcoord point,newpt;
   double  zave;

   /* Initialize */
   zave = 0.0;

   /* Calculate the z-value of the polygon center */

   /* Transform each point, taking into account logarithms */
   point.x = 0.5*(C->x0 + C->x1); if (xlog) point.x = CNlog10(point.x);
   point.y = 0.5*(C->y0 + C->y1); if (ylog) point.y = CNlog10(point.y);
   point.z = 0.5*(C->z0 + C->z1); if (zlog) point.z = CNlog10(point.z);
   newpt = CNtransform_point(&point,view_transfo);
   zave = newpt.z;

   /* Return */
   return zave;
}



/*
 * SORT BLOCKS
 */
void CNdo_quick_sort_blocks(blockhead, blocktail, view_transfo, xlog,ylog,zlog) 
CNblockptr *blockhead, *blocktail;
CNmatrix   view_transfo;
short      xlog, ylog, zlog;
{
   CNblockptr  S,A,B,C;
   CNblockptr *a;
   int        narray,in,sort_times;
   int        sorted;

   /* Don't sort if there is nothing in the list */
   if (*blockhead == NULL) return;

   /* allocate an array for holding pointers to the blocks */
   narray = CNcount_blocks(*blockhead, *blocktail);
   a = allocate_quicksort_block_array(narray);

   /* first calculate the z-ave value of all the points in the list */
   /* also set the elements of the blockptr array a                 */
   in = 0;
   for (S=(*blockhead); S!=NULL; S=S->next) {
      *(a + in) = S;
      in ++;

      /* Calculate the z-value of the block center */
      S->zave = calc_block_zcenter(S, view_transfo, xlog, ylog, zlog);
   }

   /*first check if already sorted*/
   sort_times = 0;
   do {
      sorted = CN_TRUE;
      for (S=(*blockhead); S->next!=NULL; S=S->next) {
         /* compare each adjacent pair of elements */
         B = S->next;
         if (S->zave > B->zave) {
            A = S->prev;
            C = B->next;
            if (S== *blockhead) *blockhead = B;
            if (B== *blocktail) *blocktail = S;
            if (A!=NULL) A->next = B;
            B->next = S;
            B->prev = A;
            S->next = C;
            S->prev = B;
            if (C!=NULL) C->prev = S;
            S = S->prev;
            sorted = CN_FALSE;
         }
      }
      sort_times++;
   } while (!sorted && sort_times < 5);

   /* If it's sorted, return */
   if (sorted){
      free((char *)a);
      return;
   }

   /*do the quicksort*/
   quicksort_blocks(a,0,narray-1,narray-1);

   /*put the sorted array elements back into the block list*/
   
   *blockhead = a[0];
   *blocktail = a[narray-1];
   for(in = 0; in < narray; in++){
	if(in+1 < narray){
	a[in]->next = a[in+1];
	} else {
	a[in]->next = NULL;
	}
	if(in-1 >= 0 ){
	a[in]->prev = a[in-1];
	}else {
	a[in]->prev= NULL;
	}
   }

   free((char *)a);
   
}

/* Allocate room for a bunch of blocks */
static CNblockptr *allocate_quicksort_block_array(narr)
int narr;
{
   CNblockptr *newptr;
   unsigned int arr_size;
   arr_size = (unsigned)(narr)*sizeof(CNblockptr);
   if ((newptr = (CNblockptr *)malloc(arr_size))==NULL) {
      (void) fprintf(stderr,"Out of memory\n");
      exit(-1);
   }
   return(newptr);
}

/* Sort out blocks */
static void quicksort_blocks(a,l,r,narr)
CNblockptr *a;
int l,r,narr;
{
	int i,j;
	int p;
	int sortflag;
	CNblockptr v,t,ta;
	/*if the list is small, do an insertion sort for speed*/
	if(r-l > 5){
		v = a[r];
		i = l - 1;
		j = r;
		do{
			do{
				i++;
			} while(i< narr &&!(a[i]->zave >= v->zave));
			do{
				j--;
			}while( j > 0 && !(a[j]->zave <= v->zave));
			t = a[i];
			a[i] = a[j];
			a[j] = t;
		}while(!(j <= i));
			a[j] = a[i];
			a[i] = a[r];
			a[r] = t;
			quicksort_blocks(a,l,i-1,narr);
			quicksort_blocks(a,i+1,r,narr);
	} else {
		do{
		   sortflag = 0;
		   for(p=l;p<r;p++){
		      if(a[p]->zave > a[p+1]->zave){
			  ta = a[p];
			  a[p] = a[p+1];
			  a[p+1] = ta;
			  sortflag = 1;
		      }
		   }
	        }while(sortflag != 0);
	}

}	


/*
 * Use a bubble sort to sort out blocks
 */
void CNbubble_sort_blocks(blockhead, blocktail, view_transfo, xlog,ylog,zlog) 
CNblockptr *blockhead, *blocktail;
CNmatrix   view_transfo;
short      xlog, ylog, zlog;
{
   CNblockptr  S,A,B,C;
   int        sorted;

   /* Don't sort if there is nothing in the list */
   if (*blockhead == NULL) return;

   /* first calculate the z-ave value of all the points in the list */
   for (S=(*blockhead); S!=NULL; S=S->next) {

      /* Calculate the z-value of the block center */
      S->zave = calc_block_zcenter(S, view_transfo, xlog, ylog, zlog);
   }

   /* 
    * now sort according to average z value 
    * the first structure should have the smallest z-value 
    */
   do {
      sorted = CN_TRUE;
      for (S=(*blockhead); S->next!=NULL; S=S->next) {
         /* compare each adjacent pair of elements */
         B = S->next;
         if (S->zave > B->zave) {
            /* A -> S -> B -> C */
            /* A -> B -> S -> C */
            A = S->prev;
            C = B->next;
            if (S== *blockhead) *blockhead = B;
            if (B== *blocktail) *blocktail = S;
            if (A!=NULL) A->next = B;
            B->next = S;
            B->prev = A;
            S->next = C;
            S->prev = B;
            if (C!=NULL) C->prev = S;
            S = S->prev;
            sorted = CN_FALSE;
         }
      }
   } while (!sorted);
}


/*
 * Calculate the z-value of the block center 
 */
static double calc_block_zcenter(B, view_transfo, xlog, ylog, zlog)
CNblockptr B;
CNmatrix   view_transfo;
short      xlog, ylog, zlog;
{
   CNcoord point,newpt;
   double  zave;

   /* Initialize */
   zave = 0.0;

   /* Calculate the z-value of the polygon center */

   /* Transform each point, taking into account logarithms */
   point.x = 0.5*(B->cube->x0 + B->cube->x1);
   point.y = 0.5*(B->cube->y0 + B->cube->y1);
   point.z = 0.5*(B->cube->z0 + B->cube->z1);
   if (xlog) point.x = CNlog10(point.x);
   if (ylog) point.y = CNlog10(point.y);
   if (zlog) point.z = CNlog10(point.z);
   newpt = CNtransform_point(&point,view_transfo);
   zave = newpt.z;

   /* Return */
   return zave;
}


/*
 * SORT POLYGONS
 */
void CNdo_quick_sort_polys(polyhead, polytail, view_transfo, xlog,ylog,zlog) 
CNpolyptr *polyhead, *polytail;
CNmatrix   view_transfo;
short      xlog, ylog, zlog;
{
   CNpolyptr  S,A,B,C;
   CNpolyptr *a;
   int        narray,in,sort_times;
   int        sorted;

   /* Don't sort if there is nothing in the list */
   if (*polyhead == NULL) return;

   /* allocate an array for holding pointers to the polys */
   narray = CNcount_polys(*polyhead, *polytail);
   a = allocate_quicksort_poly_array(narray);

   /* first calculate the z-ave value of all the points in the list */
   /* also set the elements of the polyptr array a                 */
   in = 0;
   for (S=(*polyhead); S!=NULL; S=S->next) {
      *(a + in) = S;
      in ++;

      /* Calculate the z-value of the polygon center */
      S->zave = calc_poly_zcenter(S, view_transfo, xlog, ylog, zlog);
   }

   /*first check if already sorted*/
   sort_times = 0;
   do {
      sorted = CN_TRUE;
      for (S=(*polyhead); S->next!=NULL; S=S->next) {
         /* compare each adjacent pair of elements */
         B = S->next;
         if (S->zave > B->zave) {
            A = S->prev;
            C = B->next;
            if (S== *polyhead) *polyhead = B;
            if (B== *polytail) *polytail = S;
            if (A!=NULL) A->next = B;
            B->next = S;
            B->prev = A;
            S->next = C;
            S->prev = B;
            if (C!=NULL) C->prev = S;
            S = S->prev;
            sorted = CN_FALSE;
         }
      }
      sort_times++;
   } while (!sorted && sort_times < 5);

   /* If it's sorted, return */
   if (sorted){
      free((char *)a);
      return;
   }

   /*do the quicksort*/
   quicksort_polys(a,0,narray-1,narray-1);

   /*put the sorted array elements back into the poly list*/
   
   *polyhead = a[0];
   *polytail = a[narray-1];
   for(in = 0; in < narray; in++){
	if(in+1 < narray){
	a[in]->next = a[in+1];
	} else {
	a[in]->next = NULL;
	}
	if(in-1 >= 0 ){
	a[in]->prev = a[in-1];
	}else {
	a[in]->prev= NULL;
	}
   }

   free((char *)a);
   
}

/* Allocate room for a bunch of polys */
static CNpolyptr *allocate_quicksort_poly_array(narr)
int narr;
{
   CNpolyptr *newptr;
   unsigned int arr_size;
   arr_size = (unsigned)(narr)*sizeof(CNpolyptr);
   if ((newptr = (CNpolyptr *)malloc(arr_size))==NULL) {
      (void) fprintf(stderr,"Out of memory\n");
      exit(-1);
   }
   return(newptr);
}

/* Sort out polys */
static void quicksort_polys(a,l,r,narr)
CNpolyptr *a;
int l,r,narr;
{
	int i,j;
	int p;
	int sortflag;
	CNpolyptr v,t,ta;
	/*if the list is small, do an insertion sort for speed*/
	if(r-l > 5){
		v = a[r];
		i = l - 1;
		j = r;
		do{
			do{
				i++;
			} while(i< narr &&!(a[i]->zave >= v->zave));
			do{
				j--;
			}while( j > 0 && !(a[j]->zave <= v->zave));
			t = a[i];
			a[i] = a[j];
			a[j] = t;
		}while(!(j <= i));
			a[j] = a[i];
			a[i] = a[r];
			a[r] = t;
			quicksort_polys(a,l,i-1,narr);
			quicksort_polys(a,i+1,r,narr);
	} else {
		do{
		   sortflag = 0;
		   for(p=l;p<r;p++){
		      if(a[p]->zave > a[p+1]->zave){
			  ta = a[p];
			  a[p] = a[p+1];
			  a[p+1] = ta;
			  sortflag = 1;
		      }
		   }
	        }while(sortflag != 0);
	}

}	


/*
 * Use a bubble sort to sort out polys
 */
void CNbubble_sort_polys(polyhead, polytail, view_transfo, xlog,ylog,zlog) 
CNpolyptr *polyhead, *polytail;
CNmatrix   view_transfo;
short      xlog, ylog, zlog;
{
   CNpolyptr  S,A,B,C;
   int        sorted;

   /* Don't sort if there is nothing in the list */
   if (*polyhead == NULL) return;

   /* first calculate the z-ave value of all the points in the list */
   for (S=(*polyhead); S!=NULL; S=S->next) {

      /* Calculate the z-value of the polygon center */
      S->zave = calc_poly_zcenter(S, view_transfo, xlog, ylog, zlog);
   }

   /* 
    * now sort according to average z value 
    * the first structure should have the smallest z-value 
    */
   do {
      sorted = CN_TRUE;
      for (S=(*polyhead); S->next!=NULL; S=S->next) {
         /* compare each adjacent pair of elements */
         B = S->next;
         if (S->zave > B->zave) {
            /* A -> S -> B -> C */
            /* A -> B -> S -> C */
            A = S->prev;
            C = B->next;
            if (S== *polyhead) *polyhead = B;
            if (B== *polytail) *polytail = S;
            if (A!=NULL) A->next = B;
            B->next = S;
            B->prev = A;
            S->next = C;
            S->prev = B;
            if (C!=NULL) C->prev = S;
            S = S->prev;
            sorted = CN_FALSE;
         }
      }
   } while (!sorted);
}


/*
 * Calculate the z-value of the polygon center 
 */
static double calc_poly_zcenter(P, view_transfo, xlog, ylog, zlog)
CNpolyptr P;
CNmatrix   view_transfo;
short      xlog, ylog, zlog;
{
   CNcoord point,newpt;
   double  ztot, zave;
   int     npts;
   CNnlistptr Nd;
   int     longline;

   /* Initialize */
   zave = 0.0;
   ztot = 0.0;
   npts = 0;

   /* Calculate the z-value of the polygon center */

   if (P->nlisthead == NULL) return 0.0;

   /* Check to see if the curve is joined */
   longline = CNlongline(P->nlisthead->N->coord,
                         P->nlisttail->N->coord, 1.0e-5);
 
   /* Calculate the z-value of the poly center */
   for (Nd=P->nlisthead; Nd!=NULL; Nd=Nd->next) {
 
      /* If the last point is same as the first point, don't count it */
      if ((Nd->next == NULL) && !longline) continue;
 
      /* Transform the point, taking into account logarithms */
      point.x = (xlog) ? CNlog10(Nd->N->coord->x) : Nd->N->coord->x;
      point.y = (ylog) ? CNlog10(Nd->N->coord->y) : Nd->N->coord->y;
      point.z = (zlog) ? CNlog10(Nd->N->coord->z) : Nd->N->coord->z;
      newpt = CNtransform_point(&point,view_transfo);
      ztot = ztot + newpt.z;
      npts++;
   }
   if (npts>0) zave = ztot/(double)npts;

   /* Return */
   return zave;
}

