/*
 * readdata.c - utility routines to read data from a file 
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "CNdata.h"
#include "CNstring.h"

#ifndef S_ISDIR
#define S_ISDIR(m) ((m & S_IFMT)==S_IFDIR)
#endif

static int  skip_blanks();

/* 
 * Read a double; skip the whole line if failed 
 * return 0 upon failure, return 1 if successful
 */
int CNrd_dbl(fp,x,lineno)
FILE   *fp;              /* File descriptor */
double *x;               /* Value returned  */
int    *lineno;          /* Line count      */
{
   int   n;
   int   c;
   double y=0.0;

   /* Get rid of spaces and blanks first */
   if (skip_blanks(fp,lineno)==EOF) return(EOF);

   /*
    * try to read a double-precision variable 
    * if fail, then skip the whole line 
    */
   while ( (n=fscanf(fp,"%lf",&y)) != EOF && n!=1) {
      if (n!=1)
         while ( (c=getc(fp)) != EOF && c != '\n') 
            /*EMPTY*/
            ;
         if (c=='\n') (*lineno)++;
         if (c==EOF) {CNerr_message(0,*lineno); return(EOF);}
      }

   if (n==EOF) {CNerr_message(0,*lineno); return(EOF);}
   *x = y;
   return(n);
}


/*
 * Test the first character - if it is a "$" then read the line
 * Rely on EOF=-1 to stop outer loop.
 */
int CNread_header(fp,lineno,line,lim)
FILE *fp;                        /* File descriptor   */
int  *lineno;                    /* Line count        */
char *line;                      /* Character array   */
int  lim;                        /* Maximum line size */
{
   int  c;
   int  optfound = CN_FALSE;

   /* Get rid of spaces and blanks first */
   if (skip_blanks(fp,lineno)==EOF) return(EOF);

   /* 
    * Now we should be positioned on a new line...   
    * If the next character is a '$' act accordingly 
    */
   if ((c=getc(fp)) == '$') {
      if (CNget_uncommented_line(fp,line,lineno,lim)==EOF) return(EOF);
      optfound = CN_TRUE;
   } else {
      /* Reposition the stream */
      (void) ungetc(c, fp);
   }
   return(optfound);
}


/*
 * Test the first character - if it is a "%" then read the line
 * Rely on EOF=-1 to stop outer loop.
 */
int CNread_option(fp,lineno,line,lim)
FILE *fp;                        /* File descriptor   */
int  *lineno;                    /* Line count        */
char *line;                      /* Character array   */
int  lim;                        /* Maximum line size */
{
   int  c;
   int  optfound = CN_FALSE;

   /* Get rid of spaces and blanks first */
   if (skip_blanks(fp,lineno)==EOF) return(EOF);

   /* 
    * Now we should be positioned on a new line...   
    * If the next character is a '%' act accordingly 
    */
   if ((c=getc(fp)) == '%') {
      if (CNget_uncommented_line(fp,line,lineno,lim)==EOF) return(EOF);
      optfound = CN_TRUE;
   } else {
      /* Reposition the stream */
      (void) ungetc(c, fp);
   }
   return(optfound);
}


/*
 * Skip over blanks, tabs, EOLNS and comments
 * Return 1 or EOF.
 */
static int skip_blanks(fp,lineno)
FILE *fp;
int  *lineno;
{
   int c;
   int CONTINUE = CN_TRUE;

   /* Get rid of spaces and blanks first */
   while ( CONTINUE &&
           (c=getc(fp)) != EOF &&
           (c == ' ' || c == '\t' || c == '\n' || c == '#') ) {
      /* If EOLN then increment the linecount */
      if (c == '\n') (*lineno)++;

      /* Get rid of comment character and anything after the comment */
      if (c == '#') {
         while ( (c=getc(fp)) != EOF && c != '\n')
            /*EMPTY*/
            ;

         /* If EOLN then increment the linecount */
         if (c == '\n') (*lineno)++;
      }

      /* Deal with EOF */
      if (c == EOF) CONTINUE = CN_FALSE;
   }

   /* put back the last read character, 'cos it may be part of a number/word.*/
   if (c != EOF) (void) ungetc(c, fp);

   /* return status */
   if (c == EOF) return(EOF);
   else          return(1);
}


/*
 * Print out an error message due to bad data
 */
void CNerr_message(mesg,lineno)
int mesg;
int lineno;
{
   switch (mesg) {
   case 0 : (void) fprintf(stderr,"Error: End-of-File encountered");
            break;
   case 1 : (void) fprintf(stderr,"Error: Plot Curves <= 0");
            break;
   case 2 : (void) fprintf(stderr,"Error: Plot Points <= 0");
            break;
   case 3 : (void) fprintf(stderr,"Error: Incorrect Format! Stopped");
            break;
   case 4 : (void) fprintf(stderr,"Error: Expecting xmin, xmax, etc. Stopped");
            break;
   case 5 : (void) fprintf(stderr,"Error: Expecting nx, ny. Stopped");
            break;
   default: (void) fprintf(stderr,"Error: Incorrect Format! Stopped");
            break;
   }
   (void) fprintf(stderr," at line #%d\n",lineno);
}


/* 
 * try to open a file for reading - this could be a compressed file 
 * return 0 upon failure, return 1 if successful
 */
int CNopen_file(fname,fp,ispipe)
char *fname;
FILE **fp;
int  *ispipe;
{
   FILE *fopen();
   char cmd[CN_MAXCHAR];
   struct stat buf;

   /* Initialize */
   *ispipe = CN_FALSE;

   /* try to open a file for reading */
   if ((strlen(fname) > 2) && (!strcmp(fname + strlen(fname) - 2,".Z"))) {
      (void) sprintf(cmd,"uncompress -c %s",fname);
      *fp = popen(cmd,"r");
      *ispipe = CN_TRUE;
   } else if (
      ((strlen(fname) > 2) && (!strcmp(fname + strlen(fname) - 2,".z"))) ||
      ((strlen(fname) > 3) && (!strcmp(fname + strlen(fname) - 3,".gz"))) ) {
      (void) sprintf(cmd,"gunzip -c %s",fname);
      *fp = popen(cmd,"r");
      *ispipe = CN_TRUE;
   } else {
      *fp = fopen(fname,"r");
   }

   if (*fp == NULL) {
      (void) fprintf(stderr,"cat : couldn't open file %s\n",fname);
      return(0);
   } 

   /*
    * Make sure this is a file and not a directory 
    */
   if ((fstat(fileno(*fp),&buf)==0) && S_ISDIR(buf.st_mode)) {
      (void) fprintf(stderr,"cat : \"%s\" is a directory!\n",fname);
      CNclose_file(*fp, *ispipe);
      return(0);
   }

   return(1);
}

/* 
 * close a file/pipe 
 */
void CNclose_file(fp,fp_ispipe)
FILE *fp;
int  fp_ispipe;
{
   /* A pipe must be closed with pclose */
   if (fp_ispipe) 
      (void) pclose(fp);
   else 
      (void) fclose(fp);
}
