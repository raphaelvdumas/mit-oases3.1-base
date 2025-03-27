/*
 * list4.c - linked list procedures building and maintaining lists
 *           of pointers to already-existing lists
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include "CNdata.h"
#include "CNproperty.h"
#include "CNdatatypes.h"

static CNtlistptr make_tlist();
static CNslistptr make_slist();
static CNnlistptr make_nlist();
static CNnlistptr make_nlist();
static CNcvlistptr make_cvlist();

/*
 * TRIANGLE LIST
 *    The triangle-list contains pointers to a set of triaptrs 
 */


/*
 * triangle-lists are used to store pointers to triangles
 */
static
CNtlistptr make_tlist(T)
CNtriaptr T; 
{
   CNtlistptr newptr;
   unsigned int size;

   size = sizeof(CNtlist);
   if ((newptr = (CNtlistptr)malloc(size))!=NULL) {
      newptr->T      = T;
      newptr->next   = NULL;
      newptr->prev   = NULL;
   }
   return(newptr);
}


/* 
 * Insert a tlist at the tail of the current triangle list 
 */
void CNinsert_tlist(tlist_listhead,tlist_listtail,T)
CNtlistptr *tlist_listhead, *tlist_listtail;
CNtriaptr  T; 
{
   CNtlistptr next,A,B;

   A = *tlist_listtail;
   if ((B=make_tlist(T))!=NULL) {
      if (A==NULL) {
         *tlist_listhead = B;
         *tlist_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next    != NULL) next->prev = B;
         if (B->next == NULL) *tlist_listtail = B;
      }
   }
}


/* 
 * Delete tlist 
 */
void CNdelete_tlist(tlist_listhead, tlist_listtail, T)
CNtlistptr *tlist_listhead, *tlist_listtail;
CNtlistptr T;
{
   CNtlistptr prev,next;

   prev = T->prev;
   next = T->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (T==*tlist_listhead) *tlist_listhead = next;
   if (T==*tlist_listtail) *tlist_listtail = prev;

   /* Now delete T */
   free ((char*)T);
}


/* 
 * Delete all the triangles in the list
 */
void CNdelete_tlist_list(tlist_listhead, tlist_listtail)
CNtlistptr *tlist_listhead, *tlist_listtail;
{
   CNtlistptr T;

   while ((T = *tlist_listhead) != NULL)
      CNdelete_tlist(tlist_listhead, tlist_listtail, T);
}


/* 
 * print out the list of triangles 
 */
/*ARGSUSED*/
void CNprint_tlist(tlist_listhead, tlist_listtail)
CNtlistptr tlist_listhead, tlist_listtail;
{
   CNtlistptr TL;

   (void) fprintf(stdout,"Triangle list:");
   for (TL=tlist_listhead; TL!=NULL; TL=TL->next) 
      (void) fprintf(stdout," %d", (TL->T==NULL) ? -1 : TL->T->ID);
   (void) fprintf(stdout,"\n");
   (void) fflush(stdout);
}


/*
 * Count the number of tlists in the list
 */
/*ARGSUSED*/
int CNcount_tlists(tlist_listhead, tlist_listtail)
CNtlistptr tlist_listhead, tlist_listtail;
{
   CNtlistptr  N;
   int        count = 0;

   for (N=tlist_listhead; N!=NULL; N=N->next) count++;

   return(count);
}


/*
 * ELEMENT LIST
 *    The element-list contains pointers to a set of elemptrs 
 */


/*
 * element-lists are used to store pointers to elements
 */
static
CNelistptr make_elist(E)
CNelemptr E; 
{
   CNelistptr newptr;
   unsigned int size;

   size = sizeof(CNelist);
   if ((newptr = (CNelistptr)malloc(size))!=NULL) {
      newptr->E      = E;
      newptr->next   = NULL;
      newptr->prev   = NULL;
   }
   return(newptr);
}


/* 
 * Insert a elist at the tail of the current element list 
 */
void CNinsert_elist(elist_listhead,elist_listtail,E)
CNelistptr *elist_listhead, *elist_listtail;
CNelemptr  E; 
{
   CNelistptr next,A,B;

   A = *elist_listtail;
   if ((B=make_elist(E))!=NULL) {
      if (A==NULL) {
         *elist_listhead = B;
         *elist_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next    != NULL) next->prev = B;
         if (B->next == NULL) *elist_listtail = B;
      }
   }
}


/* 
 * Delete elist 
 */
void CNdelete_elist(elist_listhead, elist_listtail, E)
CNelistptr *elist_listhead, *elist_listtail;
CNelistptr E;
{
   CNelistptr prev,next;

   prev = E->prev;
   next = E->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (E==*elist_listhead) *elist_listhead = next;
   if (E==*elist_listtail) *elist_listtail = prev;

   /* Now delete E */
   free ((char*)E);
}


/* 
 * Delete all the elements in the list
 */
void CNdelete_elist_list(elist_listhead, elist_listtail)
CNelistptr *elist_listhead, *elist_listtail;
{
   CNelistptr E;

   while ((E = *elist_listhead) != NULL)
      CNdelete_elist(elist_listhead, elist_listtail, E);
}


/* 
 * print out the list of elements 
 */
/*ARGSUSED*/
void CNprint_elist(elist_listhead, elist_listtail)
CNelistptr elist_listhead, elist_listtail;
{
   CNelistptr EL;

   (void) fprintf(stdout,"Element list:");
   for (EL=elist_listhead; EL!=NULL; EL=EL->next) {
      if (EL->E == NULL)
      (void) fprintf(stdout," %d", -1);
      else {
         if ((EL->E->type == CN_TRIA_STR) && (EL->E->tria != NULL)) 
         (void) fprintf(stdout," T%d", EL->E->tria->ID);
         else if ((EL->E->type == CN_RECT_STR) && (EL->E->rect != NULL)) 
         (void) fprintf(stdout," R%d", EL->E->rect->ID);
      }
   }
   (void) fprintf(stdout,"\n");
   (void) fflush(stdout);
}


/*
 * Count the number of elists in the list
 */
/*ARGSUSED*/
int CNcount_elists(elist_listhead, elist_listtail)
CNelistptr elist_listhead, elist_listtail;
{
   CNelistptr  E;
   int        count = 0;

   for (E=elist_listhead; E!=NULL; E=E->next) count++;

   return(count);
}


/*
 * SEGMENT LIST
 *    The segment-list contains pointers to a set of segmptrs 
 */


/*
 * segment-lists are used to store pointers to segments
 */
static
CNslistptr make_slist(S)
CNsegmptr S; 
{
   CNslistptr newptr;
   unsigned int size;

   size = sizeof(CNslist);
   if ((newptr = (CNslistptr)malloc(size))!=NULL) {
      newptr->S      = S;
      newptr->next   = NULL;
      newptr->prev   = NULL;
   }
   return(newptr);
}


/*  
 * Insert a slist at the tail of the current slist list
 */
void CNinsert_slist(slist_listhead, slist_listtail, S)
CNslistptr *slist_listhead, *slist_listtail;
CNsegmptr S;
{
   CNslistptr next,A,B;

   A = *slist_listtail;
   if ((B=make_slist(S))!=NULL) {
      if (A==NULL) {
         *slist_listhead = B;
         *slist_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next != NULL) next->prev = B;
         if (B->next == NULL) *slist_listtail = B;
      }
   }
}


/*
 * Delete slist at address L 
 */
void CNdelete_slist(slist_listhead, slist_listtail, L)
CNslistptr *slist_listhead, *slist_listtail;
CNslistptr L;
{
   CNslistptr prev,next;

   prev = L->prev;
   next = L->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (L== *slist_listhead) *slist_listhead = next;
   if (L== *slist_listtail) *slist_listtail = prev;
   free ((char*)L);
   L = NULL;
}


/* 
 * Delete all the segmptrs in the list
 */
void CNdelete_slist_list(slist_listhead, slist_listtail)
CNslistptr *slist_listhead, *slist_listtail;
{
   CNslistptr S;

   while ((S = *slist_listhead) != NULL)
      CNdelete_slist(slist_listhead, slist_listtail, S);
}


/*
 * print out the list of segments 
 */
/*ARGSUSED*/
void CNprint_slist(slist_listhead, slist_listtail)
CNslistptr slist_listhead, slist_listtail;
{
   CNslistptr SL;

   (void) fprintf(stdout,"Segment list:");
   for (SL=slist_listhead; SL!=NULL; SL=SL->next)
      (void) fprintf(stdout," %d", (SL->S==NULL) ? -1 : SL->S->ID);
   (void) fprintf(stdout,"\n");
   (void) fflush(stdout);
}


/*
 * Count the number of slists in the list
 */
/*ARGSUSED*/
int CNcount_slists(slist_listhead, slist_listtail)
CNslistptr slist_listhead, slist_listtail;
{
   CNslistptr  N;
   int        count = 0;

   for (N=slist_listhead; N!=NULL; N=N->next) count++;

   return(count);
}



/*
 * NODEPTR LIST
 *    The nodeptr-list contains pointers to a set of nodeptrs 
 */


/*
 * nodeptr-lists are used to store pointers to nodeptrs
 */
static
CNnlistptr make_nlist(N)
CNnodeptr N; 
{
   CNnlistptr newptr;
   unsigned int size;

   size = sizeof(CNnlist);
   if ((newptr = (CNnlistptr)malloc(size))!=NULL) {
      newptr->N      = N;
      newptr->next   = NULL;
      newptr->prev   = NULL;
   }
   return(newptr);
}


/*  
 * Insert a node at the tail of the current nlist list
 */
void CNinsert_tailnlist(nlist_listhead, nlist_listtail, N)
CNnlistptr *nlist_listhead, *nlist_listtail;
CNnodeptr N;
{
   CNnlistptr next,A,B;

   A = *nlist_listtail;
   if ((B=make_nlist(N))!=NULL) {
      if (A==NULL) {
         *nlist_listhead = B;
         *nlist_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next != NULL) next->prev = B;
         if (B->next == NULL) *nlist_listtail = B;
      }
   }
}


/*  
 * Insert a node at the head of the current nlist list
 */
void CNinsert_headnlist(nlist_listhead, nlist_listtail, N)
CNnlistptr *nlist_listhead, *nlist_listtail;
CNnodeptr N;
{
   CNnlistptr A,B;

   A = *nlist_listhead;
   if ((B=make_nlist(N))!=NULL) {
      if (A==NULL) {
         *nlist_listhead = B;
         *nlist_listtail = B;
      } else {
         B->next = A;
         A->prev = B;
         *nlist_listhead = B;
      }
   }
}


/*
 * Delete nlist at address L 
 */
void CNdelete_nlist(nlist_listhead, nlist_listtail, L)
CNnlistptr *nlist_listhead, *nlist_listtail;
CNnlistptr L;
{
   CNnlistptr prev,next;

   prev = L->prev;
   next = L->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (L== *nlist_listhead) *nlist_listhead = next;
   if (L== *nlist_listtail) *nlist_listtail = prev;
   free ((char*)L);
   L = NULL;
}


/* 
 * Delete all the nodeptrs in the list
 */
void CNdelete_nlist_list(nlist_listhead, nlist_listtail)
CNnlistptr *nlist_listhead, *nlist_listtail;
{
   CNnlistptr N;

   while ((N = *nlist_listhead) != NULL)
      CNdelete_nlist(nlist_listhead, nlist_listtail, N);
}


/*
 * print out the list of nodeptrs 
 */
/*ARGSUSED*/
void CNprint_nlist(nlist_listhead, nlist_listtail)
CNnlistptr nlist_listhead, nlist_listtail;
{
   CNnlistptr NL;

   (void) fprintf(stdout,"Node list:");
   for (NL=nlist_listhead; NL!=NULL; NL=NL->next)
      (void) fprintf(stdout," %d", (NL->N==NULL) ? -1 : NL->N->ID);
   (void) fprintf(stdout,"\n");
   (void) fflush(stdout);
}

/*
 * Count the number of nlists in the list
 */
/*ARGSUSED*/
int CNcount_nlists(nlist_listhead, nlist_listtail)
CNnlistptr nlist_listhead, nlist_listtail;
{
   CNnlistptr  N;
   int        count = 0;

   for (N=nlist_listhead; N!=NULL; N=N->next) count++;

   return(count);
}


/*
 * CURVE LIST
 *    The curve-list contains pointers to a set of curveptrs 
 */


/*
 * curve-lists are used to store pointers to curves
 */
static
CNcvlistptr make_cvlist(C)
CNcurveptr C; 
{
   CNcvlistptr newptr;
   unsigned int size;

   size = sizeof(CNcvlist);
   if ((newptr = (CNcvlistptr)malloc(size))!=NULL) {
      newptr->C      = C;
      newptr->next   = NULL;
      newptr->prev   = NULL;
   }
   return(newptr);
}


/* 
 * Insert a cvlist at the tail of the current curve list 
 */
void CNinsert_cvlist(cvlist_listhead,cvlist_listtail,C)
CNcvlistptr *cvlist_listhead, *cvlist_listtail;
CNcurveptr  C; 
{
   CNcvlistptr next,A,B;

   A = *cvlist_listtail;
   if ((B=make_cvlist(C))!=NULL) {
      if (A==NULL) {
         *cvlist_listhead = B;
         *cvlist_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next    != NULL) next->prev = B;
         if (B->next == NULL) *cvlist_listtail = B;
      }
   }
}


/* 
 * Delete cvlist 
 */
void CNdelete_cvlist(cvlist_listhead, cvlist_listtail, C)
CNcvlistptr *cvlist_listhead, *cvlist_listtail;
CNcvlistptr C;
{
   CNcvlistptr prev,next;

   prev = C->prev;
   next = C->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (C==*cvlist_listhead) *cvlist_listhead = next;
   if (C==*cvlist_listtail) *cvlist_listtail = prev;

   /* Now delete C */
   free ((char*)C);
}


/* 
 * Delete all the curves in the list
 */
void CNdelete_cvlist_list(cvlist_listhead, cvlist_listtail)
CNcvlistptr *cvlist_listhead, *cvlist_listtail;
{
   CNcvlistptr C;

   while ((C = *cvlist_listhead) != NULL)
      CNdelete_cvlist(cvlist_listhead, cvlist_listtail, C);
}


/* 
 * print out the list of curves 
 */
/*ARGSUSED*/
void CNprint_cvlist(cvlist_listhead, cvlist_listtail)
CNcvlistptr cvlist_listhead, cvlist_listtail;
{
   CNcvlistptr CS;

   (void) fprintf(stdout,"Curve list:");
   for (CS=cvlist_listhead; CS!=NULL; CS=CS->next) 
      (void) fprintf(stdout," %d",  (CS->C==NULL) ? -1 : CS->C->ID);
   (void) fprintf(stdout,"\n");
   (void) fflush(stdout);
}


/*
 * Count the number of cvlists in the list
 */
/*ARGSUSED*/
int CNcount_cvlists(cvlist_listhead, cvlist_listtail)
CNcvlistptr cvlist_listhead, cvlist_listtail;
{
   CNcvlistptr  N;
   int        count = 0;

   for (N=cvlist_listhead; N!=NULL; N=N->next) count++;

   return(count);
}



