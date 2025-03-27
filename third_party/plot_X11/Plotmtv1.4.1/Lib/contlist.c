/*
 * contlist.c - linked list procedures building and maintaining lists
 *              of contour steps/levels 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <malloc.h>
#include "CNplot.h"

static void apply_contstep_prop();
static void print_contstep();

/*
 * Given a string of contour steps - presumably in list form (...)
 * parse it and fill the contour-step list
 */
void CNparse_contstep_string(cstephead, csteptail, clevel, gbcv_pr)
CNcontstepptr      *cstephead, *csteptail;
char               *clevel;
CNcurve_property   *gbcv_pr;
{
   char  *argtbl[CN_MAXWORDS];   /* array of pointers to strings */
   char  *valtbl[CN_MAXWORDS];   /* array of pointers to strings */
   char  newclevel[CN_MAXCHAR];
   int   nargs=0, nvals=0, len, i;
   int   argfound = CN_FALSE;
   short sval;
   CNcontstepptr C;
   CNcurve_property   curv_pr;

   if (clevel == NULL || strlen(clevel) < 0) return;

   /* Initialize the curve_property */
   CNset_default_curve_property(&curv_pr);

   /* There could be keywords in the line so lowercase all the chars */
   CNstring_to_lower(clevel);

   /* Strip "(" or ")" */
   for (i=0; clevel[i]!='\0'; i++) {
      if (clevel[i] == '(') clevel[i] = ' ';
      if (clevel[i] == ')') clevel[i] = ' ';
   }

   /* Make up a new string to satisfy the word-matching routine */
   (void) sprintf(newclevel,"contours %s",clevel);

   /* Get the argument-value pairs from the line */
   if (CNparse_line(newclevel, CN_MAXCHAR,
                    &nargs, argtbl, CN_MAXWORDS,
                    &nvals, valtbl, CN_MAXWORDS)) {

      /* Go thru the arguments and find specialized matches */
      i = 0;
      while (i < nargs) {
 
         /* Go thru the arguments and find a match */
         argfound = CNparse_curve_property(&curv_pr,argtbl[i],valtbl[i],0);

         /* Reset the table if a match was found; otherwise increment index */
         CNdownshift(argfound,&i,argtbl,valtbl,&nargs,&nvals);
      }

      /* Go thru the words again and apply properties to the contsteps */
      for (i=0; i<nargs; i++) {
         if ( ((len=strlen(argtbl[i])) <= 0) ||
              (len==1 && argtbl[i][0]=='\n')) continue;

         if ((strcmp(argtbl[i], "clear")==0)||(strcmp(argtbl[i], "new")==0)) {

            /* Clear the list */
            CNassign_boolean_keyword(&sval  ,valtbl[i],"clear",0);
            if (sval)
            CNdelete_contstep_list(cstephead, csteptail);

         } else if (isalpha(argtbl[i][0])) {
            /* Ignore this word */
            (void) fprintf(stderr,
            "Warning : Unrecognized contour-step keyword \"%s\"\n",argtbl[i]);

         } else {
            /* Add to list */
            C = CNinsert_contstep(cstephead, csteptail, atof(argtbl[i]));
            if (C!= NULL) {
               if ((gbcv_pr != NULL) && (gbcv_pr->flag))
                  apply_contstep_prop(C,gbcv_pr); 
               if (curv_pr.flag) 
                  apply_contstep_prop(C,&curv_pr); 
            }
         }
      }
   }

   /* Clean up the curve-property */
   CNdelete_curve_property_fields(&curv_pr);

   /* Free the word table */
   if (nargs  > 0) CNfreewords(&nargs ,argtbl);
   if (nvals  > 0) CNfreewords(&nvals ,valtbl);
}


/*
 * Set the contours-levels if the contour property field is set
 */
void CNset_contstep_levels(cstephead, csteptail, data_pr)
CNcontstepptr *cstephead, *csteptail;
CNdataset_property *data_pr;
{
   /* Screen the data */
   if ((data_pr->flag1 & CNctrlevel) == 0) return;
   if (data_pr->contours == NULL) return;
   
   /* Now work on the contours */
   CNparse_contstep_string(cstephead, csteptail, data_pr->contours,
                           (CNcurve_property *)NULL);

   /* Reset the data_property field */
   CNdestroy_string(data_pr->contours);
   data_pr->contours   = (char *)NULL;
   data_pr->stepmethod = CN_USERDEFN;
   data_pr->flag1      = data_pr->flag1 | CNstepmethod;
}


/*
 * Make up a new string containing the contour steps
 */
/*ARGSUSED*/
char *CNcontstep_string(cstephead, csteptail)
CNcontstepptr cstephead, csteptail;
{
   CNcontstepptr C;
   char          *clevel=NULL;
   unsigned int  size=CN_MAXCHAR*sizeof(char);

   if (cstephead==NULL) return(clevel);

   if ((clevel = (char *)malloc(size))!=NULL) {
      /* Open */
      (void) strcpy(clevel,"(");

      /* Set the string */
      for (C=cstephead; C!=NULL; C=C->next)
         (void) sprintf(clevel,"%s%g ",clevel,C->value);

      /* Close */
      (void) strcat(clevel,")");
   }  
   return(clevel);
}


/*
 * Apply curve properties to the individual contours
 */
/*ARGSUSED*/
void CNapply_contstep_gbcurv_pr(cstephead,csteptail,gbcv_pr)
CNcontstepptr    cstephead, csteptail;
CNgbcurve_property *gbcv_pr;
{
   CNcurve_property curv_pr;
   CNcontstepptr    C;

   /* Initialize the curve property */
   CNset_default_curve_property(&curv_pr);

   /* Copy the gbcurve property into a curve property */
   if ((gbcv_pr->flag & CNlnwidth) != 0) {
      curv_pr.linewidth = gbcv_pr->lnwidth;
      curv_pr.flag      = curv_pr.flag | CNlinewidth;
   }
   if ((gbcv_pr->flag & CNlntype) != 0) {
      curv_pr.linetype  = gbcv_pr->lntype;
      curv_pr.flag      = curv_pr.flag | CNlinetype;
   }
   if ((gbcv_pr->flag & CNlncolor) != 0) {
      curv_pr.linecolor = gbcv_pr->lncolor;
      curv_pr.flag      = curv_pr.flag | CNlinecolor;
   }
   if ((gbcv_pr->flag & CNmktype) != 0) {
      curv_pr.marktype  = gbcv_pr->mktype;
      curv_pr.flag      = curv_pr.flag | CNmarktype;
   }
   if ((gbcv_pr->flag & CNmkcolor) != 0) {
      curv_pr.markcolor = gbcv_pr->mkcolor;
      curv_pr.flag      = curv_pr.flag | CNmarkcolor;
   }

   /* Apply the curve properties to the individual contsteps */
   if (curv_pr.flag != 0) {
      for (C=cstephead; C!=NULL; C=C->next)
         apply_contstep_prop(C, &curv_pr);
   }
}


/*
 * Apply curve properties to the individual contours
 */
static void apply_contstep_prop(C,curv_pr)
CNcontstepptr    C;
CNcurve_property *curv_pr;
{
   if ((curv_pr->flag & CNlinewidth) != 0) {
      C->curv_pr.linewidth = curv_pr->linewidth;
      C->curv_pr.flag      = C->curv_pr.flag | CNlinewidth;
   }
   if ((curv_pr->flag & CNlinetype) != 0) {
      C->curv_pr.linetype  = curv_pr->linetype;
      C->curv_pr.flag      = C->curv_pr.flag | CNlinetype;
   }
   if ((curv_pr->flag & CNlinecolor) != 0) {
      C->curv_pr.linecolor = curv_pr->linecolor;
      C->curv_pr.flag      = C->curv_pr.flag | CNlinecolor;
   }
   if ((curv_pr->flag & CNmarktype) != 0) {
      C->curv_pr.marktype  = curv_pr->marktype;
      C->curv_pr.flag      = C->curv_pr.flag | CNmarktype;
   }
   if ((curv_pr->flag & CNmarkcolor) != 0) {
      C->curv_pr.markcolor = curv_pr->markcolor;
      C->curv_pr.flag      = C->curv_pr.flag | CNmarkcolor;
   }
}


/*
 * Print the list of contour steps
 */
/*ARGSUSED*/
void CNwrite_contsteps(fp, cstephead, csteptail)
FILE *fp;
CNcontstepptr cstephead, csteptail;
{
   CNcontstepptr C;
   int           new_list = CN_TRUE;

   if (fp == NULL) return;
   if (cstephead == NULL) return;

   for (C=cstephead; C!=NULL; C=C->next) {
      if (new_list) 
      (void) fprintf(fp,"%% contours  = (%g ",C->value);
      else
      (void) fprintf(fp,"%g ",C->value);

      /* Check the next C */
      if ((C->next != NULL) &&
          (C->curv_pr.flag      == C->next->curv_pr.flag     ) &&
          (C->curv_pr.linewidth == C->next->curv_pr.linewidth) &&
          (C->curv_pr.linetype  == C->next->curv_pr.linetype ) &&
          (C->curv_pr.linecolor == C->next->curv_pr.linecolor) &&
          (C->curv_pr.marktype  == C->next->curv_pr.marktype ) &&
          (C->curv_pr.markcolor == C->next->curv_pr.markcolor)) 
         new_list = CN_FALSE;
      else 
         new_list = CN_TRUE;

      if (new_list) {
         if (C->curv_pr.flag) {
            if ((C->curv_pr.flag & CNlinewidth) != 0)
            (void) fprintf(fp,"lw=%d ",C->curv_pr.linewidth);
            if ((C->curv_pr.flag & CNlinetype ) != 0)
            (void) fprintf(fp,"lt=%d ",C->curv_pr.linetype );
            if ((C->curv_pr.flag & CNlinecolor) != 0)
            (void) fprintf(fp,"lc=%d ",C->curv_pr.linecolor);
            if ((C->curv_pr.flag & CNmarktype ) != 0)
            (void) fprintf(fp,"mt=%d ",C->curv_pr.marktype );
            if ((C->curv_pr.flag & CNmarkcolor) != 0)
            (void) fprintf(fp,"mc=%d ",C->curv_pr.markcolor);
         }
         (void) fprintf(fp,")\n");
      }
   }
}


/*
 * CONTOUR-STEP DATA STRUCTURE
 *    a contour-step-structure is used to store the slice-level and
 *    curve-plotting information.
 *    Contour-steps are stored in an ordered list, so the list-insertion 
 *    routines used here differ from the norm.
 */

/* 
 * Allocate room for a contour-step-structure
 */
CNcontstepptr CNmake_contstep(val)
double val; 
{
   CNcontstepptr newptr;
   unsigned int size = sizeof(CNcontstep);

   if ((newptr = (CNcontstepptr)malloc(size))!=NULL) {
      /* Value */
      newptr->value     = val;

      /* Set the default property */
      CNset_default_curve_property(&(newptr->curv_pr));

      /* Set the linked lists */
      newptr->next      = NULL;
      newptr->prev      = NULL;
   }
   return(newptr);
}


/*  
 * Insert a contstep at the tail of the current contstep list
 */
static CNcontstepptr store_contstep(contstep_listhead, contstep_listtail, 
                                     L, B)
CNcontstepptr *contstep_listhead, *contstep_listtail, L, B;
{
   CNcontstepptr next,A;

   A = L;
   if (B!=NULL) {
      if (A==NULL) {
         *contstep_listhead = B;
         *contstep_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next != NULL) next->prev = B;
         if (B->next == NULL) *contstep_listtail = B;
      }
   }
   return(B);
}


/*  
 * Insert a contstep at the head of the current contstep list
 */
static CNcontstepptr store_headcontstep(contstep_listhead, contstep_listtail, 
                                        B)
CNcontstepptr *contstep_listhead, *contstep_listtail, B;
{
   CNcontstepptr A;
 
   A = *contstep_listhead;
   if (B!=NULL) {
      if (A==NULL) {
         *contstep_listhead = B;
         *contstep_listtail = B;
      } else {
         B->next = A;
         A->prev = B;
         *contstep_listhead = B;
      }
   }
   return(B);
}


/*  
 * Insert a contstep at the tail of the current contstep list
 */
static CNcontstepptr insert_contstep(contstep_listhead, contstep_listtail, 
                                     L, val)
CNcontstepptr *contstep_listhead, *contstep_listtail, L;
double       val;
{
   return(store_contstep(contstep_listhead, contstep_listtail,
                         L, CNmake_contstep(val)));
}


/*  
 * Insert a contstep at the tail of the current contstep list
 */
static CNcontstepptr insert_headcontstep(contstep_listhead, contstep_listtail, 
                                         val)
CNcontstepptr *contstep_listhead, *contstep_listtail;
double       val;
{
   return(store_headcontstep(contstep_listhead, contstep_listtail,
                             CNmake_contstep(val)));
}


/*  
 * Insert a contstep at the tail of the current contstep list
 */
CNcontstepptr CNinsert_contstep(contstep_listhead, contstep_listtail, val)
CNcontstepptr *contstep_listhead, *contstep_listtail;
double       val;
{
   CNcontstepptr C, CL, Cptr=NULL;
   int          FOUND;

   if ((*contstep_listhead == NULL) || (val < (*contstep_listhead)->value)) {
      /* Insert at the head */
      Cptr = insert_headcontstep(contstep_listhead, contstep_listtail, val);
   } else {
      /* 
       * Search for the insertion point
       * Identical contsteps are NOT added to the list 
       */
      FOUND    = CN_FALSE;
      for (C=(*contstep_listhead); C!=NULL && !FOUND; C=C->next) {
         if (C->next == NULL) {
            CL = C;
            FOUND    = CN_TRUE;
         } else if (C->value < val && C->next->value > val) {
            CL = C;
            FOUND    = CN_TRUE;
         } else if (C->value == val) {
            CL = NULL;
            FOUND    = CN_TRUE;
         }
      }
      if (CL!=NULL)
         Cptr = insert_contstep(contstep_listhead, contstep_listtail, CL, val);
   }
   return(Cptr);
}


/*  
 * Insert a contstep in the current contstep list
 */
CNcontstepptr CNstore_contstep(contstep_listhead, contstep_listtail, L)
CNcontstepptr *contstep_listhead, *contstep_listtail, L;
{
   CNcontstepptr C, CL, Cptr=NULL;
   int          FOUND;

   if (L==NULL) return(NULL);

   if ((*contstep_listhead == NULL) || (L->value<(*contstep_listhead)->value)){
      /* Insert at the head */
      Cptr = store_headcontstep(contstep_listhead, contstep_listtail, L);
   } else {
      /* 
       * Search for the insertion point
       * Identical contsteps are NOT added to the list 
       */
      FOUND    = CN_FALSE;
      for (C=(*contstep_listhead); C!=NULL && !FOUND; C=C->next) {
         if (C->next == NULL) {
            CL = C;
            FOUND    = CN_TRUE;
         } else if (C->value < L->value && C->next->value > L->value) {
            CL = C;
            FOUND    = CN_TRUE;
         } else if (C->value == L->value) {
            CL = NULL;
            FOUND    = CN_TRUE;
         }
      }
      if (CL!=NULL)
         Cptr = store_contstep(contstep_listhead, contstep_listtail, CL, L);
   }
   return(Cptr);
}


/*
 * Delete contstep at address L 
 */
void CNdelete_contstep(contstep_listhead, contstep_listtail, L)
CNcontstepptr *contstep_listhead, *contstep_listtail;
CNcontstepptr L;
{
   CNcontstepptr prev,next;

   /* Delete the curve property */
   CNdelete_curve_property_fields(&(L->curv_pr));

   prev = L->prev;
   next = L->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (L== *contstep_listhead) *contstep_listhead = next;
   if (L== *contstep_listtail) *contstep_listtail = prev;
   free ((char*)L);
   L = NULL;
}


/* 
 * Delete all the contsteps in the list
 */
void CNdelete_contstep_list(contstep_listhead, contstep_listtail)
CNcontstepptr *contstep_listhead, *contstep_listtail;
{
   CNcontstepptr P;

   while ((P = *contstep_listhead) != NULL)
      CNdelete_contstep(contstep_listhead, contstep_listtail, P);
}


/* 
 * Print out the list of contsteps 
 */
/*ARGSUSED*/
void CNprint_contstep_list(contstep_listhead, contstep_listtail)
CNcontstepptr contstep_listhead, contstep_listtail;
{
   CNcontstepptr C;

   (void) printf("%d contour-steps\n",
                 CNcount_contsteps(contstep_listhead, contstep_listtail));

   for (C=contstep_listhead; C!=NULL; C=C->next) 
      print_contstep(C);
   (void) printf("\n");
}


/* 
 * print the values of a contstep
 */
static void print_contstep(C)
CNcontstepptr C;
{
   (void) fprintf(stdout,"z= %10.5g\n",C->value);
}


/*
 * Count the number of contsteps in the list 
 */
/*ARGSUSED*/
int CNcount_contsteps(contstep_listhead, contstep_listtail)
CNcontstepptr contstep_listhead, contstep_listtail;
{
   CNcontstepptr P;
   int        count = 0;

   for (P=contstep_listhead; P!=NULL; P=P->next) count++;

   return(count);
}


/*
 * Copy one contstep list to another
 */
/*ARGSUSED*/
void CNcopy_contstep_list(Cto_head, Cto_tail, Cfr_head, Cfr_tail)
CNcontstepptr *Cto_head, *Cto_tail;
CNcontstepptr  Cfr_head,  Cfr_tail;
{
   CNcontstepptr C, Cnew;
 
   /* Delete the old list */
   CNdelete_contstep_list(Cto_head, Cto_tail);
 
   /* Create a new list */
   for (C=Cfr_head; C!=NULL; C=C->next) {
      Cnew = CNinsert_contstep(Cto_head, Cto_tail, C->value);
      CNset_curve_property(&(Cnew->curv_pr), &(C->curv_pr));
   }
}

