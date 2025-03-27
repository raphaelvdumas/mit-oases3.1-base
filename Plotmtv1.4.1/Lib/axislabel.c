/*
 * axislabel.c - linked list procedures building and maintaining lists
 *               of axis labels and positions
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <malloc.h>
#include "CNplot.h"

static void print_axislabel();

/*
 * Given a string of axis label/value, parse it and fill the axislabel list
 */
void CNparse_axislabel_string(labelhead, labeltail, label)
CNaxislabelptr     *labelhead, *labeltail;
char               *label;
{
   char *word[CN_MAXWORDS];
   char  pos[CN_MAXCHAR];
   char  name[CN_MAXCHAR];
   int   argfound = CN_FALSE;
   int   nw=0;
   int   i;

   if (label == NULL || strlen(label) < 0) return;

   /* Strip "(" or ")" */
   for (i=0; label[i]!='\0'; i++) {
      if (label[i] == '(') label[i] = ' ';
      if (label[i] == ')') label[i] = ' ';
   }

   /* Parse line into lots of words */
   if ((nw = CNgetwords(label,word,CN_MAXWORDS)) >= 2) {
 
      /* Copy the first 2 words */
      CNassign_string_keyword(pos ,word[0],"argument",0);
      CNassign_string_keyword(name,word[1],"value   ",0);
      argfound = CN_TRUE;

   } else {

      (void) fprintf(stderr,"   Warning! axislabel needs 2 arguments: ");
      (void) fprintf(stderr,"axislabel = (pos name)\n");

   }

   /* If the 2 arguments were found, then add the label to the list */
   if (argfound) {
      (void) CNinsert_axislabel(labelhead, labeltail, atof(pos), name);
   }

   /* Free the words */
   if (nw > 0) CNfreewords(&nw,word);
}


/*
 * Print the list of axis labels
 */
/*ARGSUSED*/
void CNwrite_xaxislabels(fp, labelhead, labeltail)
FILE *fp;
CNaxislabelptr labelhead, labeltail;
{
   CNaxislabelptr A;

   if (fp == NULL) return;
   if (labelhead == NULL) return;

   for (A=labelhead; A!=NULL; A=A->next) {
      (void) fprintf(fp,"%% xaxislabel = (%g \"%s\")\n",A->pos, A->name);
   }
}


/*
 * Print the list of axis labels
 */
/*ARGSUSED*/
void CNwrite_yaxislabels(fp, labelhead, labeltail)
FILE *fp;
CNaxislabelptr labelhead, labeltail;
{
   CNaxislabelptr A;

   if (fp == NULL) return;
   if (labelhead == NULL) return;

   for (A=labelhead; A!=NULL; A=A->next) {
      (void) fprintf(fp,"%% yaxislabel = (%g \"%s\")\n",A->pos, A->name);
   }
}



/*
 * Print the list of axis labels
 */
/*ARGSUSED*/
void CNwrite_zaxislabels(fp, labelhead, labeltail)
FILE *fp;
CNaxislabelptr labelhead, labeltail;
{
   CNaxislabelptr A;

   if (fp == NULL) return;
   if (labelhead == NULL) return;

   for (A=labelhead; A!=NULL; A=A->next) {
      (void) fprintf(fp,"%% zaxislabel = (%g \"%s\")\n",A->pos, A->name);
   }
}


/*
 * AXISLABEL DATA STRUCTURE
 *    A axislabel-structure is used to store the position of the axis label
 *    and the axis label itself.
 *    Axis-labels are stored in an ordered list (sorted by axis position)
 *    so the list-insertion routines used here differ from the norm.
 */

/* 
 * Allocate room for an axislabel-structure
 */
CNaxislabelptr CNmake_axislabel(pos, name)
double pos; 
char   *name;
{
   CNaxislabelptr newptr;
   unsigned int size = sizeof(CNaxislabel);

   if ((newptr = (CNaxislabelptr)malloc(size))!=NULL) {
      /* position and name */
      newptr->pos       = pos;
      newptr->name      = CNcreate_string(name);

      /* Set the linked lists */
      newptr->next      = NULL;
      newptr->prev      = NULL;
   }
   return(newptr);
}


/*  
 * Insert a axislabel at the tail of the current axislabel list
 */
static CNaxislabelptr store_axislabel(axislabel_listhead, axislabel_listtail, 
                                      L, B)
CNaxislabelptr *axislabel_listhead, *axislabel_listtail, L, B;
{
   CNaxislabelptr next,A;

   A = L;
   if (B!=NULL) {
      if (A==NULL) {
         *axislabel_listhead = B;
         *axislabel_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next != NULL) next->prev = B;
         if (B->next == NULL) *axislabel_listtail = B;
      }
   }
   return(B);
}


/*  
 * Insert a axislabel at the head of the current axislabel list
 */
static CNaxislabelptr store_headaxislabel(axislabel_listhead, 
                                          axislabel_listtail, 
                                          B)
CNaxislabelptr *axislabel_listhead, *axislabel_listtail, B;
{
   CNaxislabelptr A;
 
   A = *axislabel_listhead;
   if (B!=NULL) {
      if (A==NULL) {
         *axislabel_listhead = B;
         *axislabel_listtail = B;
      } else {
         B->next = A;
         A->prev = B;
         *axislabel_listhead = B;
      }
   }
   return(B);
}


/*  
 * Insert a axislabel at the tail of the current axislabel list
 */
static CNaxislabelptr insert_axislabel(axislabel_listhead, axislabel_listtail, 
                                       L, pos, name)
CNaxislabelptr *axislabel_listhead, *axislabel_listtail, L;
double         pos;
char           *name;
{
   return(store_axislabel(axislabel_listhead, axislabel_listtail,
                          L, CNmake_axislabel(pos, name)));
}


/*  
 * Insert a axislabel at the tail of the current axislabel list
 */
static CNaxislabelptr insert_headaxislabel(axislabel_listhead, 
                                           axislabel_listtail, 
                                           pos, name)
CNaxislabelptr *axislabel_listhead, *axislabel_listtail;
double         pos;
char           *name;
{
   return(store_headaxislabel(axislabel_listhead, axislabel_listtail,
                              CNmake_axislabel(pos, name)));
}


/*  
 * Insert a axislabel at the tail of the current axislabel list
 */
CNaxislabelptr CNinsert_axislabel(axislabel_listhead, axislabel_listtail, 
                                  pos, name)
CNaxislabelptr *axislabel_listhead, *axislabel_listtail;
double         pos;
char           *name;
{
   CNaxislabelptr C, CL, Cptr=NULL;
   int          FOUND;

   if ((*axislabel_listhead == NULL) || (pos < (*axislabel_listhead)->pos)) {
      /* Insert at the head */
      Cptr = insert_headaxislabel(axislabel_listhead, axislabel_listtail, 
                                  pos, name);
   } else {
      /* 
       * Search for the insertion point
       * Identical axislabels are NOT added to the list 
       */
      FOUND    = CN_FALSE;
      for (C=(*axislabel_listhead); C!=NULL && !FOUND; C=C->next) {
         if (C->next == NULL) {
            CL = C;
            FOUND    = CN_TRUE;
         } else if (C->pos < pos && C->next->pos > pos) {
            CL = C;
            FOUND    = CN_TRUE;
         } else if (C->pos == pos) {
            CL = NULL;
            FOUND    = CN_TRUE;
         }
      }
      if (CL!=NULL)
         Cptr = insert_axislabel(axislabel_listhead, axislabel_listtail, CL, 
                                 pos, name);
   }
   return(Cptr);
}


/*  
 * Insert a axislabel in the current axislabel list
 */
CNaxislabelptr CNstore_axislabel(axislabel_listhead, axislabel_listtail, L)
CNaxislabelptr *axislabel_listhead, *axislabel_listtail, L;
{
   CNaxislabelptr C, CL, Cptr=NULL;
   int          FOUND;

   if (L==NULL) return(NULL);

   if ((*axislabel_listhead == NULL) || (L->pos < (*axislabel_listhead)->pos)){
      /* Insert at the head */
      Cptr = store_headaxislabel(axislabel_listhead, axislabel_listtail, L);
   } else {
      /* 
       * Search for the insertion point
       * Identical axislabels are NOT added to the list 
       */
      FOUND    = CN_FALSE;
      for (C=(*axislabel_listhead); C!=NULL && !FOUND; C=C->next) {
         if (C->next == NULL) {
            CL = C;
            FOUND    = CN_TRUE;
         } else if (C->pos < L->pos && C->next->pos > L->pos) {
            CL = C;
            FOUND    = CN_TRUE;
         } else if (C->pos == L->pos) {
            CL = NULL;
            FOUND    = CN_TRUE;
         }
      }
      if (CL!=NULL)
         Cptr = store_axislabel(axislabel_listhead, axislabel_listtail, CL, L);
   }
   return(Cptr);
}


/*
 * Delete axislabel at address L 
 */
void CNdelete_axislabel(axislabel_listhead, axislabel_listtail, L)
CNaxislabelptr *axislabel_listhead, *axislabel_listtail;
CNaxislabelptr L;
{
   CNaxislabelptr prev,next;

   /* Delete the name string */
   if (L->name) free((char *)L->name);

   prev = L->prev;
   next = L->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (L== *axislabel_listhead) *axislabel_listhead = next;
   if (L== *axislabel_listtail) *axislabel_listtail = prev;
   free ((char*)L);
   L = NULL;
}


/* 
 * Delete all the axislabels in the list
 */
void CNdelete_axislabel_list(axislabel_listhead, axislabel_listtail)
CNaxislabelptr *axislabel_listhead, *axislabel_listtail;
{
   CNaxislabelptr P;

   while ((P = *axislabel_listhead) != NULL)
      CNdelete_axislabel(axislabel_listhead, axislabel_listtail, P);
}


/* 
 * Print out the list of axislabels 
 */
/*ARGSUSED*/
void CNprint_axislabel_list(axislabel_listhead, axislabel_listtail)
CNaxislabelptr axislabel_listhead, axislabel_listtail;
{
   CNaxislabelptr C;

   (void) printf("%d axislabels\n",
                 CNcount_axislabels(axislabel_listhead, axislabel_listtail));

   for (C=axislabel_listhead; C!=NULL; C=C->next) 
      print_axislabel(C);
   (void) printf("\n");
}


/* 
 * print the values of a axislabel
 */
static void print_axislabel(C)
CNaxislabelptr C;
{
   (void) fprintf(stdout,"pos= %10.5f  label=\"%s\"\n",C->pos, C->name);
}


/*
 * Count the number of axislabels in the list 
 */
/*ARGSUSED*/
int CNcount_axislabels(axislabel_listhead, axislabel_listtail)
CNaxislabelptr axislabel_listhead, axislabel_listtail;
{
   CNaxislabelptr P;
   int        count = 0;

   for (P=axislabel_listhead; P!=NULL; P=P->next) count++;

   return(count);
}


/*
 * Copy one axislabel list to another
 */
/*ARGSUSED*/
void CNcopy_axislabel_list(Cto_head, Cto_tail, Cfr_head, Cfr_tail)
CNaxislabelptr *Cto_head, *Cto_tail;
CNaxislabelptr  Cfr_head,  Cfr_tail;
{
   CNaxislabelptr C;
 
   /* Delete the old list */
   CNdelete_axislabel_list(Cto_head, Cto_tail);
 
   /* Create a new list */
   for (C=Cfr_head; C!=NULL; C=C->next) {
      (void) CNinsert_axislabel(Cto_head, Cto_tail, C->pos, C->name);
   }
}

