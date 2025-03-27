/*
 * CNstring.h - external procedure declarations for string utilities 
 */

#ifndef CNstring_defined
#define CNstring_defined

#define CN_BOOLEAN 1
#define CN_SHORT   2
#define CN_INTEGER 3
#define CN_FLOAT   4
#define CN_DOUBLE  5
#define CN_STRING  6

typedef struct CNkeyword_strct {
   char   *keyword;
   int    keyval;
   int    keytype;
   int    chkmin, chkmax;
   int    ival, imin, imax;
   double dval, dmin, dmax;
} CNkeyword;

/* Read and manipulate words */
extern int   CNgetline();
extern int   CNget_uncommented_line();
#define      CNgetucline CNget_uncommented_line
extern int   CNuncomment_line();
extern int   CNskip_blanks();
extern int   CNget2words();
extern int   CNgetwords();
extern int   CNgetwords2();
extern void  CNsaveword();
extern void  CNprint_words();
extern void  CNfreewords();
extern int   CNparse_line();
extern int   CNfill_argval_tables();
extern void  CNdownshift();
extern int   CNparse_pairs();

/* Assign variables */
extern void  CNprint_keywords();
extern void  CNassign_boolean_keyword();
extern void  CNassign_short_keyword();
extern void  CNassign_int_keyword();
extern void  CNassign_float_keyword();
extern void  CNassign_double_keyword();
extern void  CNassign_string_keyword();

/* Work on strings */
extern int   CNstrlen();
extern char *CNcreate_string();
extern void  CNdestroy_string();
extern char *CNstring_concat();
extern void  CNstring_to_upper();
extern void  CNstring_to_lower();
extern int   CNstring_to_boolean();

/* Read strings and files */
extern int          CNrd_dbl();
extern int          CNread_option();
extern void         CNerr_message();
extern int          CNopen_file();
extern void         CNclose_file();

#endif /* CNstring_defined */

