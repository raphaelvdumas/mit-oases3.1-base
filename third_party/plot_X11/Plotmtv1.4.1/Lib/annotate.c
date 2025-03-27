/*
 * annotate.c - procedures to build and maintain an annotation list
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include <math.h>
#include "CNdata.h"
#include "CNproperty.h"
#include "CNstring.h"
#include "CNannotate.h"

static int parse_line();

/*
 * Utility functions to build annotations
 */

/* Create a rectangle */
CNannotptr CNcreate_rect_annotation(annot_listhead, annot_listtail,  
                                    x1,y1,z1,x2,y2,z2,ID)
CNannotptr *annot_listhead, *annot_listtail;
double     x1,y1,z1,x2,y2,z2;
int        ID;
{
   CNannotptr aptr;

   /* Create an annotation */
   if ((aptr=CNinsert_annotation(annot_listhead,annot_listtail,ID))!=NULL){
      aptr->type  = CN_AN_RECT;
      aptr->pt1.x = x1;
      aptr->pt1.y = y1;
      aptr->pt1.z = z1;
      aptr->pt2.x = x2;
      aptr->pt2.y = y2;
      aptr->pt2.z = z2;
   }
   return(aptr);
}

/* Create a line */
CNannotptr CNcreate_line_annotation(annot_listhead, annot_listtail,  
                                    x1,y1,z1,x2,y2,z2,ID)
CNannotptr *annot_listhead, *annot_listtail;
double     x1,y1,z1,x2,y2,z2;
int        ID;
{
   CNannotptr aptr;

   /* Create an annotation */
   if ((aptr=CNinsert_annotation(annot_listhead,annot_listtail,ID))!=NULL){
      aptr->type  = CN_AN_LINE;
      aptr->pt1.x = x1;
      aptr->pt1.y = y1;
      aptr->pt1.z = z1;
      aptr->pt2.x = x2;
      aptr->pt2.y = y2;
      aptr->pt2.z = z2;
   }
   return(aptr);
}

/* Create an arrow */
CNannotptr CNcreate_arrow_annotation(annot_listhead, annot_listtail,  
                                    x1,y1,z1,x2,y2,z2,ID)
CNannotptr *annot_listhead, *annot_listtail;
double     x1,y1,z1,x2,y2,z2;
int        ID;
{
   CNannotptr aptr;

   /* Create an annotation */
   if ((aptr=CNinsert_annotation(annot_listhead,annot_listtail,ID))!=NULL){
      aptr->type  = CN_AN_ARROW;
      aptr->pt1.x = x1;
      aptr->pt1.y = y1;
      aptr->pt1.z = z1;
      aptr->pt2.x = x2;
      aptr->pt2.y = y2;
      aptr->pt2.z = z2;
   }
   return(aptr);
}

/* Create a point */
CNannotptr CNcreate_point_annotation(annot_listhead, annot_listtail,  
                                    x1,y1,z1,ID)
CNannotptr *annot_listhead, *annot_listtail;
double     x1,y1,z1;
int        ID;
{
   CNannotptr aptr;

   /* Create an annotation */
   if ((aptr=CNinsert_annotation(annot_listhead,annot_listtail,ID))!=NULL){
      aptr->type  = CN_AN_POINT;
      aptr->pt1.x = x1;
      aptr->pt1.y = y1;
      aptr->pt1.z = z1;
      aptr->property.marktype   = CN_MK_SQUARE1;
      aptr->property.markcolor  = 0;
   }
   return(aptr);
}

/* Create a text-field */
CNannotptr CNcreate_text_annotation(annot_listhead, annot_listtail,
                                    x1,y1,z1,ID)
CNannotptr *annot_listhead, *annot_listtail;
double     x1,y1,z1;
int        ID;
{
   CNannotptr aptr;

   /* Create an annotation */
   if ((aptr=CNinsert_annotation(annot_listhead,annot_listtail,ID))!=NULL){
      aptr->type  = CN_AN_TEXT;
      aptr->pt1.x = x1;
      aptr->pt1.y = y1;
      aptr->pt1.z = z1;
   }
   return(aptr);
}




/*
 * ANNOTATATION DATA STRUCTURE
 *    A annotation is a simple geometric object that is placed in a plot
 */

/*
 * Allocate room for an annotation data-structure
 */
CNannotptr CNmake_annotation(ID)
int ID;
{
   CNannotptr newptr;
   unsigned int size = sizeof(CNannotation);

   if ((newptr = (CNannotptr)malloc(size))!=NULL) {
      /* annotation Information */
      newptr->ID    = ID;
      newptr->type  = CN_AN_NONE;
 
      /* Values */
      newptr->pt1.x  = 0.0;
      newptr->pt1.y  = 0.0;
      newptr->pt1.z  = 0.0;
      newptr->pt2.x  = 0.0;
      newptr->pt2.y  = 0.0;
      newptr->pt2.z  = 0.0;

      /* Set the default property */
      CNset_default_annotation_property(&(newptr->property));

      /* Linked lists */
      newptr->next   = NULL;
      newptr->prev   = NULL;
   }
   return(newptr);
}

/*
 * Insert an annotation at the tail of the current annotation list
 */
CNannotptr CNinsert_annotation(annot_listhead, annot_listtail, ID)
CNannotptr *annot_listhead, *annot_listtail;
int        ID;
{
   CNannotptr next,A,B;

   A = *annot_listtail;
   if ((B=CNmake_annotation(ID))!=NULL) {
      if (A==NULL) {
         *annot_listhead = B;
         *annot_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next != NULL) next->prev = B;
         if (B->next == NULL) *annot_listtail = B;
      }
   }
   return(B);
}


/*
 * Delete all the annotations in the list
 */
void CNdelete_annotation_list(annot_listhead, annot_listtail)
CNannotptr *annot_listhead, *annot_listtail;
{
   CNannotptr L;

   while ((L = *annot_listhead) != NULL)
      CNdelete_annotation(annot_listhead, annot_listtail, L);
}


/*
 * Delete annotation at address L
 */
void CNdelete_annotation(annot_listhead, annot_listtail, L)
CNannotptr *annot_listhead, *annot_listtail;
CNannotptr L;
{
   CNannotptr prev,next;

   if (L->property.linelabel) free((char *)L->property.linelabel);

   prev = L->prev;
   next = L->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (L== *annot_listhead) *annot_listhead = next;
   if (L== *annot_listtail) *annot_listtail = prev;
   free ((char*)L);
   L = NULL;
}


/*
 * Print out the list of annotations
 */
/*ARGSUSED*/
void CNprint_annotation_list(annot_listhead, annot_listtail)
CNannotptr annot_listhead, annot_listtail;
{
   CNannotptr L;

   for (L=annot_listhead; L!=NULL; L=L->next) {
      CNprint_annotation(L);
      (void) fflush(stdout);
   }
}

/*
 * Print info on the annotation
 */
void CNprint_annotation(L)
CNannotptr L;
{
   (void) fprintf(stdout,"Annotation ID  : %d\n",L->ID);
   (void) fprintf(stdout,"Annotation Type: %s\n",CNannotationtype(L->type));

   (void) fprintf(stdout,"Point 1:\n");
   (void) fprintf(stdout,"   x= %10.5g\n",L->pt1.x);
   (void) fprintf(stdout,"   y= %10.5g\n",L->pt1.y);
   (void) fprintf(stdout,"   z= %10.5g\n",L->pt1.z);

   if ((L->type != CN_AN_POINT) && 
       (L->type != CN_AN_TEXT)) {
   (void) fprintf(stdout,"Point 2:\n");
   (void) fprintf(stdout,"   x= %10.5g\n",L->pt2.x);
   (void) fprintf(stdout,"   y= %10.5g\n",L->pt2.y);
   (void) fprintf(stdout,"   z= %10.5g\n",L->pt2.z);
   }

   CNprint_annotation_property(&(L->property));
}


/*
 * Count the number of annotations in the list
 */
/*ARGSUSED*/
int CNcount_annotations(annot_listhead, annot_listtail)
CNannotptr annot_listhead, annot_listtail;
{
   CNannotptr L;
   int        count = 0;

   for (L=annot_listhead; L!=NULL; L=L->next) count++;

   return(count);
}


/*
 * Store an annotation at the tail of the current annotation list
 */
void CNstore_annotation(annot_listhead, annot_listtail, Aptr)
CNannotptr *annot_listhead, *annot_listtail, Aptr;
{
   CNannotptr next,A,B;

   A = *annot_listtail;
   if ((B=Aptr)!=NULL) {
      if (A==NULL) {
         *annot_listhead = B;
         *annot_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next != NULL) next->prev = B;
         if (B->next == NULL) *annot_listtail = B;
      }
   }
}


/*
 * Copy one annotation list to another
 */
/*ARGSUSED*/
void CNcopy_annotation_list(Cto_head, Cto_tail, Cfr_head, Cfr_tail)
CNannotptr *Cto_head, *Cto_tail;
CNannotptr  Cfr_head,  Cfr_tail;
{
   CNannotptr C, Cnew;
 
   /* Delete the old list */
   CNdelete_annotation_list(Cto_head, Cto_tail);
 
   /* Create a new list */
   for (C=Cfr_head; C!=NULL; C=C->next) {
      Cnew = CNinsert_annotation(Cto_head, Cto_tail, C->ID);
      CNset_annotation_property(&(Cnew->property), &(C->property));
   }
}


/*
 * Return a string denoting the annotation-type
 */
char *CNannotationtype(type)
int type;
{
   char *data;

   switch (type) {
   case CN_AN_NONE    : data = "No Annotation";  break;
   case CN_AN_RECT    : data = "Rectangle";      break;
   case CN_AN_LINE    : data = "Line";           break;
   case CN_AN_ARROW   : data = "Arrow";          break;
   case CN_AN_POINT   : data = "Point";          break;
   case CN_AN_TEXT    : data = "Text";           break;
   default            : data = "No Annotation";  break;
   }

   return(data);
}


/*
 * ANNOTATION PROPERTIES
 *
 * This is just a convenient way to group the descriptions of the annotation
 */
 
/*
 * Allocate room for a annotation property structure
 */
CNannotation_property *CNmake_annotation_property()
{
   CNannotation_property *newptr;
   unsigned int size = sizeof(CNannotation_property);
 
   if ((newptr = (CNannotation_property *)malloc(size))!=NULL) {
      CNset_default_annotation_property(newptr);
   }
   return(newptr);
}

/*
 * Delete a annotation property structure
 */
void CNdelete_annotation_property(prop)
CNannotation_property *prop;
{
   if (prop!=NULL) {
      if (prop->linelabel) free((char *)prop->linelabel);
      free((char *)prop);
   }
}

/*
 * Set the default property of an annotation
 */
void CNset_default_annotation_property(prop)
CNannotation_property *prop;
{
   /* Flag */
   prop->flag      = 0;

   /* Lines */
   prop->linelabel  = NULL;
   prop->linewidth  = 1;
   prop->linetype   = CN_LN_SOLID;
   prop->linecolor  = 0;

   /* Markers */
   prop->marksize   = 1;
   prop->marktype   = CN_MK_NONE;
   prop->markcolor  = 0;

   /* Fill */
   prop->filltype   = CN_FILL_NONE;
   prop->fillcolor  = 0;

   /* Position */
   prop->absolute   = CN_FALSE;
   prop->doclip     = CN_TRUE;

   /* Font size */
   prop->fontsize   = 10;

   /* Scale fonts with plot size */
   prop->doscale    = CN_FALSE;
}

/*
 * Print the values of a annotation property, primarily for debugging purposes
 */
void CNprint_annotation_property(prop)
CNannotation_property *prop;
{
   (void) fprintf(stdout,"Label      = \"%s\"\n",prop->linelabel);
   (void) fprintf(stdout,"Linewidth  = %d\n",prop->linewidth);
   (void) fprintf(stdout,"Linetype   = %d\n",prop->linetype);
   (void) fprintf(stdout,"Linecolor  = %d\n",prop->linecolor);
   (void) fprintf(stdout,"Markertype = %d\n",prop->marktype);
   (void) fprintf(stdout,"Markercolor= %d\n",prop->markcolor);
   (void) fprintf(stdout,"Filltype   = %d\n",prop->filltype);
   (void) fprintf(stdout,"Fillcolor  = %d\n",prop->fillcolor);
   (void) fprintf(stdout,"Absolute   = %s\n",BOOLEAN_VALUE(prop->absolute));
   (void) fprintf(stdout,"Clip       = %s\n",BOOLEAN_VALUE(prop->doclip));
   (void) fprintf(stdout,"Fontsize   = %d\n",prop->fontsize);
   (void) fprintf(stdout,"Scale      = %s\n",BOOLEAN_VALUE(prop->doscale));
}

/*
 * Set the property of a annotation
 *    This is done by copying the contents of one annotation to another
 *    based on one annotation's flag.
 */
void CNset_annotation_property(C1,C2)
CNannotation_property *C1, *C2;
{
  /* Error checking */
  if (C1==NULL || C2==NULL) {
     (void) fprintf(stderr,
     "Error in CNset_annotation_property() : Null annotation property!\n");
     return;
  }

  C1->flag = C1->flag | C2->flag;
  if ((C2->flag & CNlinelabel) != 0) {
      CNdestroy_string(C1->linelabel);
      C1->linelabel  = CNcreate_string(C2->linelabel);
  }
  if ((C2->flag & CNlinewidth) != 0) C1->linewidth= C2->linewidth;
  if ((C2->flag & CNlinetype ) != 0) C1->linetype = C2->linetype;
  if ((C2->flag & CNlinecolor) != 0) C1->linecolor= C2->linecolor;
  if ((C2->flag & CNmarksize ) != 0) C1->marksize = C2->marksize;
  if ((C2->flag & CNmarktype ) != 0) C1->marktype = C2->marktype;
  if ((C2->flag & CNmarkcolor) != 0) C1->markcolor= C2->markcolor;
  if ((C2->flag & CNfilltype ) != 0) C1->filltype = C2->filltype ;
  if ((C2->flag & CNfillcolor) != 0) C1->fillcolor= C2->fillcolor;
  if ((C2->flag & CNabsolute ) != 0) C1->absolute = C2->absolute ;
  if ((C2->flag & CNdoclip   ) != 0) C1->doclip   = C2->doclip   ;
  if ((C2->flag & CNfontsize ) != 0) C1->fontsize = C2->fontsize ;
  if ((C2->flag & CNdoscale  ) != 0) C1->doscale  = C2->doscale  ;
}

static enum annotation_keyvals {
   ANlnlabel,
   ANlnwidth,
   ANlntype,
   ANlncolor,
   ANmksize,
   ANmktype,
   ANmkcolor,
   ANfltype,
   ANflcolor,
   ANabslute,
   ANdoclip,
   ANfontsz,
   ANdoscale
};
#define ANNOTATION_MAXKEY 30
static CNkeyword annotation_keywords[] = {
   {"linelabel",  ANlnlabel, CN_STRING , 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"label",      ANlnlabel, CN_STRING , 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"ll",         ANlnlabel, CN_STRING , 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"linewidth",  ANlnwidth, CN_INTEGER, 1, 1, 1, 0,32, 0.0, 0.0, 0.0},
   {"lw",         ANlnwidth, CN_INTEGER, 1, 1, 1, 0,32, 0.0, 0.0, 0.0},
   {"linetype",   ANlntype,  CN_INTEGER, 1, 0, 1, 0, 0, 0.0,0.0,0.0},
   {"lt",         ANlntype,  CN_INTEGER, 1, 0, 1, 0, 0, 0.0,0.0,0.0},
   {"linestyle",  ANlntype,  CN_INTEGER, 1, 0, 1, 0, 0, 0.0,0.0,0.0},
   {"linecolor",  ANlncolor, CN_INTEGER, 1, 0, 1,-1, 0, 0.0,0.0,0.0},
   {"lc",         ANlncolor, CN_INTEGER, 1, 0, 1,-1, 0, 0.0,0.0,0.0},
   {"markersize", ANmksize,  CN_INTEGER, 1, 1, 1, 1,10, 0.0,0.0,0.0},
   {"ms",         ANmksize,  CN_INTEGER, 1, 1, 1, 1,10, 0.0,0.0,0.0},
   {"markertype", ANmktype,  CN_INTEGER, 1, 0, 1, 0, 0, 0.0,0.0,0.0},
   {"mt",         ANmktype,  CN_INTEGER, 1, 0, 1, 0, 0, 0.0,0.0,0.0},
   {"markerstyle",ANmktype,  CN_INTEGER, 1, 0, 1, 0, 0, 0.0,0.0,0.0},
   {"markercolor",ANmkcolor, CN_INTEGER, 1, 0, 1,-1, 0, 0.0,0.0,0.0},
   {"mc",         ANmkcolor, CN_INTEGER, 1, 0, 1,-1, 0, 0.0,0.0,0.0},
   {"filltype",   ANfltype,  CN_INTEGER, 1, 0, 0, 0, 0, 0.0,0.0,0.0},
   {"ft",         ANfltype,  CN_INTEGER, 1, 0, 0, 0, 0, 0.0,0.0,0.0},
   {"fillstyle",  ANfltype,  CN_INTEGER, 1, 0, 0, 0, 0, 0.0,0.0,0.0},
   {"fillcolor",  ANflcolor, CN_INTEGER, 1, 0, 1,-1, 0, 0.0,0.0,0.0},
   {"fc",         ANflcolor, CN_INTEGER, 1, 0, 1,-1, 0, 0.0,0.0,0.0},
   {"absolute",   ANabslute, CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0,0.0,0.0},
   {"abs",        ANabslute, CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0,0.0,0.0},
   {"doclip",     ANdoclip , CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0,0.0,0.0},
   {"clip",       ANdoclip , CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0,0.0,0.0},
   {"fn",         ANfontsz , CN_INTEGER, 1, 1,10, 1,30, 0.0,0.0,0.0},
   {"fontsize",   ANfontsz , CN_INTEGER, 1, 1,10, 1,30, 0.0,0.0,0.0},
   {"doscale",    ANdoscale, CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0,0.0,0.0},
   {"scale",      ANdoscale, CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0,0.0,0.0},
};

/*
 * Act on a annotation-keyword (argument=value) and change the annotation 
 * property
 */
int CNparse_annotation_property(property,argument,value,verbose)
CNannotation_property  *property;           /* annotation property */
char              *argument,*value;
int               verbose;
{
   CNkeyword *key;
   int       match = 0, keyval = -1, i;
   char      cval[CN_MAXCHAR];
   int       ival=0;
   short     sval=0;

   /* Search for a matching argument keyword */
   for (i=0; i<ANNOTATION_MAXKEY && !match; i++) {
      if (strcmp(annotation_keywords[i].keyword,argument) == 0) {
         match = 1;
         keyval = annotation_keywords[i].keyval;
         key    = &(annotation_keywords[i]);
      }
   }

   /* Return if not found */
   if (match == 0) return(match);

   /* Now get the corresponding value and change the structure */
   switch (keyval) {
   case ANlnlabel :
      CNassign_string_keyword(cval,value,argument,verbose);
      property->flag = property->flag | CNlinelabel;
      CNdestroy_string(property->linelabel);
      property->linelabel  = CNcreate_string(cval);
      break;
   case ANlnwidth :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      property->flag = property->flag | CNlinewidth;
      property->linewidth = ival;
      break;
   case ANlntype :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      property->flag = property->flag | CNlinetype;
      property->linetype = ival;
      break;
   case ANlncolor :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      property->flag = property->flag | CNlinecolor;
      property->linecolor = ival;
      break;
   case ANmktype :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      property->flag = property->flag | CNmarktype;
      property->marktype = ival;
      break;
   case ANmksize :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      property->flag = property->flag | CNmarksize;
      property->marksize = ival;
      break;
   case ANmkcolor :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      property->flag = property->flag | CNmarkcolor;
      property->markcolor = ival;
      break;
   case ANfltype :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      property->flag = property->flag | CNfilltype;
      property->filltype = ival;
      break;
   case ANflcolor :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      property->flag = property->flag | CNfillcolor;
      property->fillcolor = ival;
      break;
   case ANabslute :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag = property->flag | CNabsolute;
      property->absolute = sval;
      break;
   case ANdoclip  :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag = property->flag | CNdoclip;
      property->doclip = sval;
      break;
   case ANfontsz  :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      property->flag = property->flag | CNfontsize;
      property->fontsize = ival;
      break;
   case ANdoscale :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag = property->flag | CNdoscale;
      property->doscale = sval;
      break;
   default :
      match = 0;
      break;
   }

   /* Return status */
   return(match);
}

/*
 * Print the list of allowable keywords
 */
void CNprint_annotation_keywords(verbose)
int verbose;
{
   CNprint_keywords(annotation_keywords,ANNOTATION_MAXKEY,"Annotation",verbose);
}


/*
 * Print the list of keywords that have been set
 */
/*ARGSUSED*/
void CNwrite_annotations(fp,annothead,annottail)
FILE *fp;
CNannotptr annothead,annottail;
{
   CNannotation_property def_prop;
   CNannotptr            aptr;

   /* Check annotations first */
   if (annothead==NULL) return;

   /* Go thru each annotation */
   for (aptr=annothead; aptr!=NULL; aptr=aptr->next) {

      /* Set default properties */
      CNset_default_annotation_property(&def_prop);

      /* Print the keyword and arguments based on the annotation type */
      switch(aptr->type) {
      case CN_AN_RECT :
      case CN_AN_LINE :
      case CN_AN_ARROW:
           /* Rectangle, line, arrow requires P1, P2 */
           (void) fprintf(fp,"@ %s ", CNannotationtype(aptr->type));
           (void) fprintf(fp,"x1=%g y1=%g z1=%g ",
                          aptr->pt1.x, aptr->pt1.y, aptr->pt1.z);
           (void) fprintf(fp,"x2=%g y2=%g z2=%g ",
                          aptr->pt2.x, aptr->pt2.y, aptr->pt2.z);
           break;
      case CN_AN_POINT:
      case CN_AN_TEXT :
           /* Point/text requires only one point */
           (void) fprintf(fp,"@ %s ", CNannotationtype(aptr->type));
           (void) fprintf(fp,"x1=%g y1=%g z1=%g ",
                          aptr->pt1.x, aptr->pt1.y, aptr->pt1.z);
           
           break;
      default:
           break;
      }

      /* Now print the properties of the annotation */
      if (aptr->property.linelabel)
      (void) fprintf(fp,"linelabel=\"%s\" ",aptr->property.linelabel   );

      if (aptr->property.linewidth != def_prop.linewidth)
      (void) fprintf(fp,"linewidth=%d ",aptr->property.linewidth);
 
      if (aptr->property.linetype  != def_prop.linetype )
      (void) fprintf(fp,"linetype=%d ",aptr->property.linetype );
 
      if (aptr->property.linecolor != def_prop.linecolor)
      (void) fprintf(fp,"linecolor=%d ",aptr->property.linecolor);
 
      if (aptr->property.marksize  != def_prop.marksize )
      (void) fprintf(fp,"markersize=%d ",aptr->property.marksize );
 
      if (aptr->property.marktype  != def_prop.marktype )
      (void) fprintf(fp,"markertype=%d ",aptr->property.marktype );
 
      if (aptr->property.markcolor != def_prop.markcolor)
      (void) fprintf(fp,"markercolor=%d ",aptr->property.markcolor);
 
      if (aptr->property.filltype  != def_prop.filltype )
      (void) fprintf(fp,"filltype=%d ",aptr->property.filltype );
 
      if (aptr->property.fillcolor != def_prop.fillcolor)
      (void) fprintf(fp,"fillcolor=%d ",aptr->property.fillcolor);

      if (aptr->property.absolute != def_prop.absolute)
      (void) fprintf(fp,"absolute=%s ",BOOLEAN_VALUE(aptr->property.absolute));

      if (aptr->property.doclip != def_prop.doclip)
      (void) fprintf(fp,"doclip=%s ",BOOLEAN_VALUE(aptr->property.doclip));

      if (aptr->property.fontsize != def_prop.fontsize)
      (void) fprintf(fp,"fontsize=%d ",aptr->property.fontsize);

      if (aptr->property.doscale != def_prop.doscale)
      (void) fprintf(fp,"doscale=%s ",BOOLEAN_VALUE(aptr->property.doscale));

      /* Add on end-of-line */
      (void) fprintf(fp,"\n");
   }
}


/*
 * Annotation keywords 
 */
typedef struct _annot_keywd {
   char *keyword; 
   int  keyval;
} annot_keywd;

static annot_keywd an_keywords[] = {
   {"rectangle", CN_AN_RECT},
   {"rect",      CN_AN_RECT},
   {"line",      CN_AN_LINE},
   {"arrow",     CN_AN_ARROW},
   {"point",     CN_AN_POINT},
   {"text",      CN_AN_TEXT},
};


/*
 * Given an annotation line, parse it and fill an annotation structure
 *
 * This is of the form "keyword arg=val arg=val..."
 */
void CNparse_annotation_line(line, lim, annot_listhead, annot_listtail, vbs)
char       *line;
int        lim;
CNannotptr *annot_listhead, *annot_listtail;
int        vbs;
{
   CNannotptr  aptr = NULL;
   CNannotation_property an_prop;
   char   *argtbl[CN_MAXWORDS], *valtbl[CN_MAXWORDS];
   char   header[CN_MAXCHAR];
   int    keyval = -1, match = 0, nkeywds;
   int    nargs = 0, nvals = 0;
   int    argfound, i;
   double x1=0.0, y1=0.0, z1=0.0, x2=0.0, y2=0.0, z2=0.0;

   /* Get the argument-value pairs from the line */
   if (parse_line(line, lim, 
                  &nargs, argtbl, CN_MAXWORDS, 
                  &nvals, valtbl, CN_MAXWORDS, header)) {

      /* Convert the header to lowercase */
      CNstring_to_lower(header);

      /* Search for a matching argument keyword */
      nkeywds = sizeof(an_keywords)/sizeof(annot_keywd);
      for (i=0; i<nkeywds && !match; i++) {
         if (strcmp(an_keywords[i].keyword,header) == 0) {
            match = 1;
            keyval = an_keywords[i].keyval;
         }
      }

      /* Error */
      if (!match) {
         (void) fprintf(stderr,"Error! Unrecognized annotation \"%s\"\n",
                        header);
         CNfreewords(&nargs, argtbl);
         CNfreewords(&nvals, valtbl);
         if (an_prop.linelabel) free((char *)an_prop.linelabel);
         return;
      } 

      /* Go thru the arguments and find specialized matches */
      i = 0;
      while (i < nargs) {

         /* Go thru the arguments and find a match */
         argfound = CN_TRUE;

         if (strcmp(argtbl[i],"x1")==0) {
            x1    = atof(valtbl[i]);
            if (vbs)
            (void)fprintf(stdout,"   ANNOTN  : %-14s= %g\n","x1",x1);

         } else if (strcmp(argtbl[i],"y1")==0) {
            y1    = atof(valtbl[i]);
            if (vbs)
            (void)fprintf(stdout,"   ANNOTN  : %-14s= %g\n","y1",y1);

         } else if (strcmp(argtbl[i],"z1")==0) {
            z1    = atof(valtbl[i]);
            if (vbs)
            (void)fprintf(stdout,"   ANNOTN  : %-14s= %g\n","z1",z1);

         } else if (strcmp(argtbl[i],"x2")==0) {
            x2    = atof(valtbl[i]);
            if (vbs)
            (void)fprintf(stdout,"   ANNOTN  : %-14s= %g\n","x2",x2);

         } else if (strcmp(argtbl[i],"y2")==0) {
            y2    = atof(valtbl[i]);
            if (vbs)
            (void)fprintf(stdout,"   ANNOTN  : %-14s= %g\n","y2",y2);

         } else if (strcmp(argtbl[i],"z2")==0) {
            z2    = atof(valtbl[i]);
            if (vbs)
            (void)fprintf(stdout,"   ANNOTN  : %-14s= %g\n","z2",z2);

         } else {
            argfound = CN_FALSE;
         }

         /* Reset the table if a match was found; otherwise increment index */
         CNdownshift(argfound,&i,argtbl,valtbl,&nargs,&nvals);
      }

      /* Initialize the annotation property */
      CNset_default_annotation_property(&an_prop);

      /* Look for annotation arguments */
      for (i=0; i<nargs; i++) {
         if (!CNparse_annotation_property(&an_prop,argtbl[i],valtbl[i],vbs))
            (void) fprintf(stderr,
                    "warning : Invalid annotation option \"%s=%s\"\n",
                    argtbl[i],valtbl[i]);
      }

      /* Create an annotation */
      switch (keyval) {
      case CN_AN_RECT : 
           aptr = CNcreate_rect_annotation(annot_listhead, annot_listtail,  
                                           x1,y1,z1,x2,y2,z2,0);
           break;
      case CN_AN_LINE : 
           aptr = CNcreate_line_annotation(annot_listhead, annot_listtail,  
                                           x1,y1,z1,x2,y2,z2,0);
           break;
      case CN_AN_ARROW: 
           aptr = CNcreate_arrow_annotation(annot_listhead, annot_listtail,  
                                           x1,y1,z1,x2,y2,z2,0);
           break;
      case CN_AN_POINT: 
           aptr = CNcreate_point_annotation(annot_listhead, annot_listtail,  
                                           x1,y1,z1,0);
           break;
      case CN_AN_TEXT: 
           aptr = CNcreate_text_annotation(annot_listhead, annot_listtail,  
                                           x1,y1,z1,0);
           break;
      default :
           aptr = NULL;
           break;
      }

      if (aptr != NULL) {
         /* Apply the annotation options to the annotation */
        CNset_annotation_property(&(aptr->property),&an_prop);

        /* Print */
        if (vbs) CNprint_annotation(aptr);
      }
      /* Clear the tables */
      CNfreewords(&nargs, argtbl);
      CNfreewords(&nvals, valtbl);
   }
   if (an_prop.linelabel) free((char *)an_prop.linelabel);
}

/*
 * Parse a line and break it up to argument=value pairs
 * The line is of the format "command [arg=val] [arg=val]..."
 * Return (1) if successful; (0) otherwise.
 */
static int parse_line(line, lim, 
                      nargs, argtbl, maxargs, nvals, valtbl, maxvals, header)
char *line;
int  lim;                    /* Max line array size           */
int  *nargs;                 /* Number of arguments           */
char **argtbl;               /* Array of string arguments     */
int  maxargs;                /* Max no of arguments           */
int  *nvals;                 /* Number of values              */
char **valtbl;               /* Array of string values        */
int  maxvals;                /* Max no of values              */
char *header;                /* First argument                */
{
   char *words[CN_MAXWORDS];   /* array of pointers to strings */
   int  nwords=0, len, err;

   /* Reinitialize */
   *nargs = 0;
   *nvals = 0;

   /* Go thru the line and get rid of the comments */
   len = strlen(line);
   if (len > 0) len = CNuncomment_line(line, lim);

   if (len <= 0) return (0);

   /* Get the words in the line */
   nwords = CNgetwords2(line,words, CN_MAXWORDS);
   if (nwords > 0) (void) strcpy(header, words[0]);

   /* Break up the string into argument=value pairs ; return if error */
   err = CNfill_argval_tables(&nwords,words,
                              nargs,argtbl,maxargs,nvals,valtbl,maxvals);

   /* Free the word table */
   if (nwords > 0) CNfreewords(&nwords,words);

   /* Return */
   return(err);
}

