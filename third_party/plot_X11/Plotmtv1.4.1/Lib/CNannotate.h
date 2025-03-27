/*
 * CNannotate.h - definitions for annotations on a plot
 *
 * This file requires CNdata.h and CNproperty.h
 */

#ifndef CNannotate_defined
#define CNannotate_defined

#define CN_AN_NONE  0
#define CN_AN_RECT  1
#define CN_AN_LINE  2
#define CN_AN_ARROW 3
#define CN_AN_POINT 4
#define CN_AN_TEXT  5

/* Flags argument for changing curve_property */
#define CNabsolute  (1L <<  9)    /* User-specified absolute position  */
#define CNdoclip    (1L << 10)    /* User-specified relative clipping  */
#define CNdoscale   (1L << 11)    /* Scale font with plot size         */
#define CNfontsize  (1L << 12)    /* Text font size                    */

/*
 * The annotation property structure describes properties of the annotation
 * For the most part, the annotation property resembles a curve property.
 */
/* Annotation property */
typedef struct CNannotation_property_strct {
   long   flag;
   char   *linelabel;                  /* Line label        */
   short  linewidth;                   /* Line width        */
   short  linetype;                    /* Line type         */
   short  linecolor;                   /* Line color        */
   short  marksize;                    /* Marker Size       */
   short  marktype;                    /* Marker Type       */
   short  markcolor;                   /* Marker Color      */
   short  filltype;                    /* Fill type         */
   short  fillcolor;                   /* Fill color        */
   short  absolute;                    /* Absolute/Relative */
   short  doclip;                      /* Relative Clipping */
   short  doscale;                     /* Scale annotations */
   int    fontsize;                    /* Font size (scaled)*/
} CNannotation_property;

/* 
 * A annotation is a simple geometric object that is placed
 * on a plot
 */
/* Annotate */
typedef struct CNannotation_strct {
   short   ID;
   short   type;
   CNcoord pt1;
   CNcoord pt2;
   CNannotation_property property;
   struct CNannotation_strct *next;
   struct CNannotation_strct *prev;
} CNannotation;
typedef struct CNannotation_strct *CNannotptr;

extern CNannotptr CNmake_annotation();
extern CNannotptr CNinsert_annotation();
extern void       CNdelete_annotation();
extern void       CNdelete_annotation_list();
extern void       CNprint_annotation_list();
extern void       CNprint_annotation();
extern int        CNcount_annotations();
extern void       CNstore_annotation();
extern void       CNcopy_annotation_list();
extern char      *CNannotationtype();

extern CNannotation_property *CNmake_annotation_property();
extern void       CNdelete_annotation_property();
extern void       CNset_default_annotation_property();
extern void       CNprint_annotation_property();
extern void       CNset_annotation_property();
extern int        CNparse_annotation_property();
extern void       CNprint_annotation_keywords();
extern void       CNwrite_annotations();
extern void       CNparse_annotation_line();

extern CNannotptr CNcreate_rect_annotation();
extern CNannotptr CNcreate_line_annotation();
extern CNannotptr CNcreate_arrow_annotation();
extern CNannotptr CNcreate_point_annotation();
extern CNannotptr CNcreate_text_annotation();

#endif /* CNannotate_defined */
