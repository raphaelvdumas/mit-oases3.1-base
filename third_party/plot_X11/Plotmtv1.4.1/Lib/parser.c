/*
 * parser.c - parses the line 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>
#include <malloc.h>
#include "CNdata.h"
#include "CNstring.h"

#define YES CN_TRUE
#define NO  CN_FALSE

static int   getwords();
char         *CNstrip_keyword();
static int   break_string();

/*
 * get line into s 
 */
int CNgetline(op,s,lineno,lim)
FILE *op;                     /* File descriptor   */
char s[];                     /* Character array   */
int  *lineno;                 /* Line count        */
int  lim;                     /* Maximum line size */
{
    int c,cprev=' ',i=0;

    /* read the line until termination marks EOLN */
    while ((c=getc(op))!=EOF && !(c=='\n' && cprev!='\\') && i<lim-2) {
       if (cprev=='\\' && c=='\n') {
          /* The string is continued on another line... */
          i--;
          (*lineno)++;
       } else {
          /* Collect the character */
          s[i++] = c;
       }
       cprev = c;
    }

    /* End-of-line was encountered */
    if (c=='\n') (*lineno)++;

    /* End the string */
    if (i==lim-2 || c=='\n') s[i++] = '\n';
    s[i] = '\0';

    if (c==EOF)
       return(EOF);
    return(i);
}


/*
 * Get an uncommented line 
 * This uses CNgetline() and then strips out the comments
 * Also strip out leading blanks
 */
int CNget_uncommented_line(op,s,lineno,lim)
FILE *op;                     /* File descriptor   */
char s[];                     /* Character array   */
int  *lineno;                 /* Line count        */
int  lim;                     /* Maximum line size */
{
   int n;

   if ((n=CNgetline(op,s,lineno,lim))!=EOF) {
      n = CNskip_blanks(s,lim);
      n = CNuncomment_line(s,lim);
   }
   return(n);
}


/*
 * Go thru the line and screen out comments
 */
int CNuncomment_line(line,lim)
char *line;                    /* Character array   */
int  lim;                      /* Maximum line size */
{
   int  i, DONE=CN_FALSE;
   int  bracket_count=0, collectword=CN_FALSE;
   char cprev, c;

   cprev = -1;
   for (i=0; i<lim && line[i]!='\0' && !DONE; i++) {
      c = line[i];
 
      if (c == '\'' || c == '"') {
         /* Collect characters inside quotes */
         if (!collectword) {
            /* Begin collecting */
            collectword = CN_TRUE;
            cprev = c;
         } else if (cprev == c) {
            /* Stop collecting */
            collectword = CN_FALSE;
         }
      } else if (c=='(' || c==')') {
         /* Collect characters inside brackets until closing bracket is found */ 
         if (!collectword && c=='(') {
            /* Begin collecting */
            collectword = CN_TRUE;
            cprev = c;
            bracket_count = 0;
         } else if (collectword && cprev=='(' && c==')' && bracket_count==0) {
            /* Stop collecting special characters */
            collectword = CN_FALSE;
         } else {
            /* Keep track of pairs of brackets */
            if (c=='(') bracket_count++;
            if (c==')') bracket_count--;
         } 
      }

      if (!collectword) {
         /* Check for # only if the # is not embedded in braces or quotes */
         if (line[i] == '#') {
            if (i>0 && line[i-1]=='\\') {
               /*EMPTY*/
               /* Ignore "#" if preceded by "\", i.e. "\#" */
               ;
            } else {
               line[i] = '\0';
               DONE = CN_TRUE;
            }
         }
      }
   }
   return(strlen(line));
}

/*
 * Strip out leading blanks
 */
int CNskip_blanks(line,lim)
char *line;                    /* Character array   */
int  lim;                      /* Maximum line size */
{
   int i,istart=0;

   for (i=0; i<lim && line[i]!='\0' && (line[i]==' ' || line[i]=='\t'); i++)
      istart=i+1;

   if (istart!=0) {
      /* Shift the characters in the line */
      for (i=0; i<lim && line[i]!='\0' && i<strlen(line)-istart; i++)
         line[i] = line[i+istart];
      line[i++] = '\0';
   }

   return(strlen(line));    
}

/* 
 * Get the first 2 words in a line 
 */
int CNget2words(line,argument,value)
char *line;        /* The original line */
char *argument;    /* The first word    */
char *value;       /* The second word   */
{
   char *word[CN_MAXWORDS];
   int  nw=0, FOUND=CN_FALSE;

   /* Parse line into lots of words */
   if ((nw = CNgetwords(line,word,CN_MAXWORDS)) >= 2) {

      /* Copy the first 2 words */
      CNassign_string_keyword(argument,word[0],"argument",0);
      CNassign_string_keyword(value   ,word[1],"value   ",0);
      FOUND=CN_TRUE;

      /* Convert only the argument to lowercase */
      CNstring_to_lower(argument);
   }

   /* Free the words */
   CNfreewords(&nw,word);

   /* Return status */
   return(FOUND);
}


/* 
 * Get the words in a null-terminated line 
 *    char *word[MAXWORD];
 *    nw = CNgetwords(line,word,MAXWORD);
 *    CNfreewords(&nw,word);
 */
static int getwords(s,wdtbl,maxwds,equal_delimits) 
char s[];
char **wdtbl;
int  maxwds;
int  equal_delimits;   /* Equal-sign is used as a word delimiter */
{
   int  i, nc, nw, inword;
   int  collectword, bracket_count=0;
   char c, cprev;
   char line[CN_MAXCHAR];

   nc          = nw = 0;
   inword      = CN_FALSE;
   collectword = CN_FALSE;

   /* Return if zero-length line */
   if (strlen(s) == 0) return(nw);

   /* Now go thru each character in the line */
   cprev= -1;
   for (i=0; s[i] != '\0'; i++) {
      c = s[i];

      /* The backslash means that the next character is treated normally */
      if (c=='\\') {
         if (s[i+1] != '\0')
            c = s[++i];
         else 
            continue;
      }

      if (c==' ' || c=='\n' || c=='\t' || ((c=='=') && equal_delimits) ||
          c=='[' || c==']'  || c==':'  || c==',') {
         /* 
          * ignore all these characters unless they are inside quotes.
          * space and the above characters act as delimiters
          */
         if (inword && (!collectword || c=='\n') ) {

            /* Terminate the line      */
            line[nc++] = '\0'; 

            /* copy line to word array */
            CNsaveword(wdtbl,maxwds,line,&nw,nc+2);

            /* Reset word collection   */
            nc = 0; inword = CN_FALSE; 

         } else if (collectword) {

            /* Collect character */
            line[nc++] = c;

         }

      } else if (c == '\'' || c == '"') {

         /*
          * Collect characters inside quotes 
          */

         /* Continue collecting special characters */
         line[nc++] = c;
         if (!inword) inword = CN_TRUE;

         /* Set collect flags */
         if (!collectword) {

            /* Start collecting special characters */
            collectword = CN_TRUE;   
            cprev       = c; 

         } else if (cprev ==  c) {

            /* Stop collecting special characters */
            collectword = CN_FALSE;  

         }

      } else if (c=='(' || c==')') {

         /*
          * Collect characters inside brackets until closing bracket is found
          */

         /* Continue collecting special characters */
         line[nc++] = c;
         if (!inword) inword = CN_TRUE;

         /* Set collect flags */
         if (!collectword && c=='(') {

            /* Start collecting */
            collectword = CN_TRUE;   
            cprev       = c; 

            /* Reset the bracket count */
            bracket_count=0;

         } else if (collectword && cprev=='(' && c==')' && bracket_count==0) {

            /* Stop collecting special characters */
            collectword = CN_FALSE;  

         } else {

           /* Keep track of pairs of brackets */
           if (c=='(') bracket_count++;
           if (c==')') bracket_count--;

         }

      } else {

         /*
          * Normal characters 
          */

         /* Continue collecting     */
         line[nc++] = c; 
         if (!inword) inword = CN_TRUE;
      }
   }

   /* Append a null to the last word */
   if (inword) {
      /* NULL-terminate the line and copy word to word array */
      line[nc++] = '\0';
      CNsaveword(wdtbl,maxwds,line,&nw,nc+2);      
   }

#ifdef DEBUG
   CNprint_words(nw,wdtbl);
#endif

   return(nw);
}


/* 
 * Get the words in a null-terminated line 
 *    char *word[MAXWORD];
 *    nw = CNgetwords(line,word,MAXWORD);
 *    CNfreewords(&nw,word);
 *
 * This routine uses the equal sign '=' as a word delimiter.
 */
int CNgetwords(s,wdtbl,maxwds) 
char s[];
char **wdtbl;
int  maxwds;
{
   int nwords;
   int equal_as_delimiter=CN_TRUE;

   /* Use getwords */
   nwords = getwords(s,wdtbl,maxwds,equal_as_delimiter);

   return(nwords);
}


/* 
 * Get the words in a null-terminated line 
 *    char *word[MAXWORD];
 *    nw = CNgetwords2(line,word,MAXWORD);
 *    CNfreewords(&nw,word);
 *
 * This routine is almost identical to CNgetwords except that '=' is not
 * used as a word delimiter.
 */
int CNgetwords2(s,wdtbl,maxwds) 
char s[];
char **wdtbl;
int  maxwds;
{
   int nwords;
   int equal_as_delimiter=CN_FALSE;

   /* Use getwords */
   nwords = getwords(s,wdtbl,maxwds,equal_as_delimiter);

   return(nwords);
}


/*
 * Save the word in an array of pointers
 */
void CNsaveword(wordtbl, maxwords, line, nwords, nchars) 
char **wordtbl;     /* word-table (array of word pointers) */
int  maxwords;      /* max no of words in the word-table   */
char *line;         /* word to be saved                    */
int  *nwords;       /* Number of words so far              */
int  nchars;        /* Number of characters in the word    */
{
   char *sptr;
   unsigned int wsize;

   /* Don't save an empty string */
   if (nchars == 0) return;          

   /* Check to see that there is enough space in the word-table */
   if ((*nwords) >= maxwords-1) {
      (void) fprintf(stderr,"The word-table is full!");
      (void) fprintf(stderr,"The word is not being saved!\n");
      return;
   }

   /* Make sure that the \0 terminating character is saved too */
   if (strlen(line) >= nchars) nchars++;

   wsize = nchars*sizeof(char);
   if ( (sptr = (char *)malloc(wsize)) == NULL) {
      (void) fprintf(stderr,"\nOut of internal storage.  Exiting...\n");
      exit(-1);
   } else {
      (void) strcpy(sptr,line);
      wordtbl[(*nwords)++] = sptr;
   }
}


/*
 * Print the words in the word array 
 */
void CNprint_words(argc,argv)
int argc;
char **argv;
{
   int i;

   /* print out the words */
   (void) fprintf(stdout," %d words =",argc);
   for (i=0; i<argc; i++)
      (void) fprintf(stdout," [%s]",argv[i]);
   (void) fprintf(stdout,"\n");
}


/* 
 * Free the strings allocated in the pointer array 
 */
void CNfreewords(argc,argv)
int  *argc;
char **argv;
{
   int i;
   for (i=0; i<(*argc); i++) 
         free ((char *) argv[i]);
   *argc = 0;
}



/*
 * UTILITY PARSING ROUTINES
 */

/*
 * Parse a line and break it up to argument=value pairs 
 * The line is of the format "command [arg=val] [arg=val]..."
 * Return (1) if successful; (0) otherwise.
 */
int CNparse_line(line, lim, nargs, argtbl, maxargs, nvals, valtbl, maxvals)
char *line;
int  lim;                    /* Max line size                 */
int  *nargs;                 /* Number of arguments           */
char **argtbl;               /* Array of string arguments     */
int  maxargs;                /* Max no of arguments           */
int  *nvals;                 /* Number of values              */
char **valtbl;               /* Array of string values        */
int  maxvals;                /* Max no of values              */
{
   char *words[CN_MAXWORDS];   /* array of pointers to strings */
   int  nwords=0, len, err;

   /* Reinitialize */
   *nargs = 0;
   *nvals = 0;

   /* Go thru the line and get rid of the comments */
   len = strlen(line);
   if (len > 0) len = CNuncomment_line(line,lim);

   if (len <= 0) return (0);

   /* Get the words in the line */
   nwords = CNgetwords2(line,words,CN_MAXWORDS);

   /* Break up the string into argument=value pairs ; return if error */
   err = CNfill_argval_tables(&nwords,words,
                              nargs,argtbl,maxargs,nvals,valtbl,maxvals); 

   /* Free the word table */
   if (nwords > 0) CNfreewords(&nwords,words);

   /* Return */
   return(err);
}


/*
 * Break up an array of word pairs in the x=y format into argument=value pairs
 */
int CNfill_argval_tables(nwords, words, 
                         nargs, argtbl, maxargs, nvals, valtbl, maxvals)
int  *nwords;                /* Number of words               */
char **words;                /* Array of words                */
int  *nargs;                 /* Number of arguments           */
char **argtbl;               /* Array of string arguments     */
int  maxargs;                /* max no of arguments           */
int  *nvals;                 /* Number of values              */
char **valtbl;               /* Array of string values        */
int  maxvals;                /* max no of values              */
{
   int  err;
   char argument[CN_MAXCHAR], value[CN_MAXCHAR], newvalue[CN_MAXCHAR];

   /* Initialize */
   *nargs = *nvals = 0;

   /*
    * Break up the string into argument=value pairs
    */

   /* Go thru the arguments - parse_pairs fails quietly if only 1 arg */
   while ((err=CNparse_pairs(nwords,words,argument,value)) > 0) {

      /* Save the argument and value in an array */
      (void) strcpy(newvalue, CNstrip_keyword(value));
      CNsaveword(argtbl,maxargs,argument,nargs,strlen(argument)+2);
      CNsaveword(valtbl,maxvals,newvalue,nvals,strlen(newvalue)+2);

   }

   /* Clear the tables if error occured */
   if (err==0) {
      CNfreewords(nargs, argtbl);
      CNfreewords(nvals, valtbl);
      return(0);
   } else {
      return(1);
   } 
}


/*
 * Eliminate one entry in the table of strings
 */
void CNdownshift(argfound,index,argtbl,valtbl,nargs,nvals)
int         argfound;               /* Flag to remove array element  */
int         *index;                 /* Array element to remove       */
char        **argtbl;               /* Array of string arguments     */
char        **valtbl;               /* Array of string values        */
int         *nargs;                 /* Number of arguments           */
int         *nvals;                 /* Number of values              */
{
   char *arg, *val;
   int  i,j;

   /* Error check */
   if (*index > *nargs) {
      (void) fprintf(stderr,"CNdownshift() : Warning - invalid array index!\n");
      return;
   }
   i = *index;

   if (argfound) {
      /* Save the original elements */
      arg = argtbl[i];
      val = valtbl[i];

      /* Down shift the array elements */
      for (j=i; j<(*nargs)-1; j++) {
         argtbl[j] = argtbl[j+1];
         valtbl[j] = valtbl[j+1];
      }

      /* Remove the original entries */
      free ((char *)arg);
      free ((char *)val);

      /* Reset the no of entries */
      (*nargs)--;
      (*nvals)--;

   } else {
      (*index)++;
   }
}


/*
 * Given a 2D array of words, parse it and find a twin set
 * of the format argument=value
 *
 * Return status :
 *    1 : Found a pair
 *    0 : Error
 *   -1 : Done - no more pairs
 */
int CNparse_pairs(argc, argv, argument, value)
int  *argc;
char **argv;
char *argument;
char *value;
{
   char tmparr[CN_MAXCHAR];
   int  i, j, arglen, vallen;

#ifdef DEBUG
   CNprint_words(*argc,argv);
#endif

   /* check the number of arguments */
   if (*argc <= 1) {
      /* Fail quietly */
      return(-1);
   }

   /*
    * start with the second word in the array
    */
   i = 1;

   /*
    * check to see if the equal sign is embedded in the argument
    */
   if ((arglen = break_string(argv[i],argument,value)) == -1) {
      /*
       * no equal sign - the equal sign must be in the next argument
       */
      if (++i >= *argc) {
         /* No more arguments - the argument must be a boolean */
         i--; (void) strcpy(value,"true");
      } else if ((vallen = break_string(argv[i],value,tmparr)) == -1) {
         /*
          * there is still no equal sign!
          * This must be a boolean argument
          */
         i--; (void) strcpy(value,"true");
      } else if ((vallen == 0) && (strlen(tmparr) == 0)) {
         /*
          * There is a lone equal-sign "=", the value is the next string
          */
         if (++i >= *argc) {
            (void) fprintf(stderr,
            "bad # arg: %.50s [arg=value] [arg=value]...\n", argv[0]);
            return(0);
         }
         (void) strcpy(value,argv[i]);
      } else if (vallen == 0) {
         /*
          * There is an equal sign followed by characters, i.e. "=xxx"
          * the value is the tmparr string
          */
         (void) strcpy(value,tmparr);
      } else {
         /*
          * value has the form "xxx=yyy" - do nothing in this case
          */
         i--; (void) strcpy(value,"true");
      }

   } else if (arglen == 0) {
      /*
       * the argument length is zero - the string is of the form "=xxx"
       */
      (void) fprintf(stderr,
      "bad # arg: %.50s [arg=value] [arg=value]...\n", argv[0]);
      return(0);

   } else if (strlen(value) == 0) {
      /*
       * the string was of the form "xxx= yyy"
       * the value is the next argument
       */
      if (++i >= *argc) {
         (void) fprintf(stderr,
         "bad # arg: %.50s [arg=value] [arg=value]...\n", argv[0]);
         return(0);
      }
      (void) strcpy(value,argv[i]);

   /*EMPTY*/
   } else {
      /*
       * This is perfect "xxx=yyy"
       */
   }

#ifdef DEBUG
   (void) fprintf(stdout,"   string   = \"%s\"\n",argv[0]);
   (void) fprintf(stdout,"   argument = \"%s\"\n",argument);
   (void) fprintf(stdout,"   value    = \"%s\"\n",value);
#endif

   /* Convert only the argument to lowercase */
   CNstring_to_lower(argument);

   /* Change the values returned */
   for (j=1; j<=i; j++)
     free((char *)argv[j]);
   for (j=1; j<*argc-i; j++)
      argv[j] = argv[j+i];
   *argc = *argc - i;

   return(1);
}

/* break the string at an equal sign */
static int break_string(s,argument,value)
char *s;
char *argument;
char *value;
{
   char *val;
   int  arglen;

   /* initialize argument and value */
   (void) strcpy(argument,"");
   (void) strcpy(value   ,"");

   /* search the string for the first occurence of the equal sign */
   if ((val = strchr(s,'=')) != NULL) {
      /*
       * argument is the string before val
       */
      arglen = strlen(s) - strlen(val);
      (void) strncat(argument,s,arglen);

      /*
       * val is the pointer to the string including the = sign
       */
      val++;
      (void) strcpy(value,val);

      /*
       * return the length of the argument
       */
      return(strlen(argument));
   } else {
      /*
       * There is no equal-sign in the string
       */
      (void) strcpy(argument,s);
      (void) strcpy(value   ,"");
      return(-1);
   }
}


/*
 * KEYWORD OPERATIONS
 */

/*
 * Print the list of allowable keywords
 */
void CNprint_keywords(keyarr,nkeys,label,verbose)
CNkeyword keyarr[];
int       nkeys;
char      *label;
int       verbose;
{
   CNkeyword *key;
   char      header[CN_MAXCHAR], min[CN_MAXCHAR], max[CN_MAXCHAR];
   int       i;

   if (verbose == 0) {
      (void) fprintf(stdout,"   <%s options>\n",label);
   } else if (verbose == 1) {
      (void) fprintf(stdout,"   <%s options>\n",label);
      (void) sprintf(header,"      (%s keywords : ",label);
      for (i=0; i<nkeys; i++) {
         key = &(keyarr[i]);
         if (strlen(header)+strlen(key->keyword) > 80) {
            (void) fprintf(stdout,"%s\n",header);
            (void) sprintf(header,"          %s ",key->keyword);
         } else {
            (void) strcat(header,key->keyword);
            (void) strcat(header," ");
         }
      }
      (void) strcat(header,")\n\n");
      (void) fprintf(stdout,"%s",header);
   } else {
      (void) fprintf(stdout,"   <%s options>\n",label);
      (void) fprintf(stdout,"      (%s keywords :)\n",label);
      for (i=0; i<nkeys; i++) {
         key = &(keyarr[i]);
         (void) fprintf(stdout,"         [%s",key->keyword);
         switch (key->keytype) {
         case CN_BOOLEAN : (void) fprintf(stdout," = ON/OFF]\n"); break;
         case CN_SHORT   :
         case CN_INTEGER : (void) strcpy(min,"-INF");
                           if (key->chkmin) (void) sprintf(min,"%d",key->imin);
                           (void) strcpy(max,"INF");
                           if (key->chkmax) (void) sprintf(max,"%d",key->imax);
                           (void) fprintf(stdout," = %s..%s]\n",min,max);
                           break;
         case CN_FLOAT   :
         case CN_DOUBLE  : (void) strcpy(min,"-INF");
                           if (key->chkmin) (void) sprintf(min,"%g",key->dmin);
                           (void) strcpy(max,"INF");
                           if (key->chkmax) (void) sprintf(max,"%g",key->dmax);
                           (void) fprintf(stdout," = %s..%s]\n",min,max);
                           break;
         case CN_STRING  : (void) fprintf(stdout," = \"label\"]\n"); break;
         default         : (void) fprintf(stdout,"]\n"); break;
         }
      }
      (void) fprintf(stdout,"\n");
   }
   (void) fflush(stdout);
}



/*
 * Assign a boolean value to a keyword
 */
void CNassign_boolean_keyword(keyval,value,label,verbose)
short  *keyval;             /* The value to be translated to   */
char   *value;              /* The value to be translated from */
char   *label;              /* The keyword label to be printed */
int    verbose;             /* Verbosity flag                  */
{
   int    intval;

   if ((intval = CNstring_to_boolean(value))==CN_ERROR) {
      (void) fprintf(stdout,
                     "   Options : *** %-9s - Unknown keyvalue = \"%s\"\n",
                     label,value);
      (void) fprintf(stdout,"             *** Allowed values are :");
      (void) fprintf(stdout,"\"TRUE\" \"FALSE\" \"ON\" \"OFF\"\n");
   } else
      *keyval = intval;
   if (verbose) 
   (void) fprintf(stdout,
                  "   Options : %-14s= %s\n",label,BOOLEAN_VALUE(*keyval));
}


/*
 * Assign a short integer value to a keyword
 */
void CNassign_short_keyword(keyval,value,label,
                          default_val,minval,maxval,minrange,maxrange,verbose)
short  *keyval;             /* The value to be translated to   */
char   *value;              /* The value to be translated from */
char   *label;              /* The keyword label to be printed */
short  default_val;         /* Default value                   */
short  minval, maxval;      /* Max and min range               */
short  minrange, maxrange;  /* Flags for applying ranges       */
int    verbose;             /* Verbosity flag                  */
{
   int val;

   val = atoi(value);

   /* Check max range */
   if (maxrange) {
      if (val > maxval) {
         (void) fprintf(stderr,"   Options : *** %-9s = %d",label, val);
         (void) fprintf(stderr," is greater than allowed value (%d)!\n",maxval);
         (void) fprintf(stderr,"             *** %-9s reset to %d\n",
                 label, default_val);
         val = default_val;
      }
   }

   /* Check min range */
   if (minrange) {
      if (val < minval) {
         (void) fprintf(stderr,"   Options : *** %-9s = %d",label, val);
         (void) fprintf(stderr," is less than allowed value (%d)!\n",minval);
         (void) fprintf(stderr,"             *** %-9s reset to %d\n",
                 label, default_val);
         val = default_val;
      }
   }

   if (verbose) (void) fprintf(stdout,"   Options : %-14s= %d\n", label, val);

   /* Return the value of the keyword */
   *keyval = val;
}


/*
 * Assign a integer value to a keyword
 */
void CNassign_int_keyword(keyval,value,label,
                          default_val,minval,maxval,minrange,maxrange,verbose)
int    *keyval;             /* The value to be translated to   */
char   *value;              /* The value to be translated from */
char   *label;              /* The keyword label to be printed */
int    default_val;         /* Default value                   */
int    maxval, minval;      /* Max and min range               */
int    maxrange, minrange;  /* Flags for applying ranges       */
int    verbose;             /* Verbosity flag                  */
{
   int val; 

   val = atoi(value);

   /* Check max range */
   if (maxrange) {
      if (val > maxval) {
         (void) fprintf(stderr,"   Options : *** %-9s = %d",label, val);
         (void) fprintf(stderr," is greater than allowed value (%d)!\n",maxval);
         (void) fprintf(stderr,"             *** %-9s reset to %d\n",
                 label, default_val);
         val = default_val;
      }
   }

   /* Check min range */
   if (minrange) {
      if (val < minval) {
         (void) fprintf(stderr,"   Options : *** %-9s = %d",label, val);
         (void) fprintf(stderr," is less than allowed value! (%d)\n",minval);
         (void) fprintf(stderr,"             *** %-9s reset to %d\n",
                 label, default_val);
         val = default_val;
      }
   }

   if (verbose) (void) fprintf(stdout,"   Options : %-14s= %d\n", label, val);

   /* Return the value of the keyword */
   *keyval = val;
}


/*
 * Assign a floating-precision value to a keyword
 */
void CNassign_float_keyword(keyval,value,label,
                          default_val,minval,maxval,minrange,maxrange,verbose)
float  *keyval;             /* The value to be translated to   */
char   *value;              /* The value to be translated from */
char   *label;              /* The keyword label to be printed */
float  default_val;         /* Default value                   */
float  minval, maxval;      /* Max and min range               */
short  minrange, maxrange;  /* Flags for applying ranges       */
int    verbose;             /* Verbosity flag                  */
{
   float val;

   val = atof(value);

   /* Check max range */
   if (maxrange) {
      if (val > maxval) {
         (void) fprintf(stderr,"   Options : *** %-9s = %f",label, val);
         (void) fprintf(stderr," is greater than allowed value (%f)!\n",maxval);
         (void) fprintf(stderr,"             *** %-9s reset to %f\n",
                 label, default_val);
         val = default_val;
      }
   }

   /* Check min range */
   if (minrange) {
      if (val < minval) {
         (void) fprintf(stderr,"   Options : *** %-9s = %f",label, val);
         (void) fprintf(stderr," is less than allowed value (%f)!\n",minval);
         (void) fprintf(stderr,"             *** %-9s reset to %f\n",
                 label, default_val);
         val = default_val;
      }
   }

   if (verbose) (void) fprintf(stdout,"   Options : %-14s= %f\n", label, val);

   /* Return the value of the keyword */
   *keyval = val;
}


/*
 * Assign a double-precision value to a keyword
 */
void CNassign_double_keyword(keyval,value,label,
                     default_val,minval,maxval,minrange,maxrange,verbose)
double *keyval;             /* The value to be translated to   */
char   *value;              /* The value to be translated from */
char   *label;              /* The keyword label to be printed */
double default_val;         /* Default value                   */
double maxval, minval;      /* Max and min range               */
int    maxrange, minrange;  /* Flags for applying ranges       */
int    verbose;             /* Verbosity flag                  */
{
   double val; 

   val = atof(value);

   /* Check max range */
   if (maxrange) {
      if (val > maxval) {
         (void) fprintf(stderr,"   Options : *** %-9s = %g",label, val);
         (void) fprintf(stderr," is greater than allowed value (%f)!\n",maxval);
         (void) fprintf(stderr,"             *** %-9s reset to %g\n",
                 label, default_val);
         val = default_val;
      }
   }

   /* Check min range */
   if (minrange) {
      if (val < minval) {
         (void) fprintf(stderr,"   Options : *** %-9s = %g",label, val);
         (void) fprintf(stderr," is less than allowed value (%f)!\n",minval);
         (void) fprintf(stderr,"             *** %-9s reset to %g\n",
                 label, default_val);
         val = default_val;
      }
   }

   if (verbose) (void) fprintf(stdout,"   Options : %-14s= %g\n", label, val);

   /* Return the value of the keyword */
   *keyval = val;
}


/*
 * Assign a string value to a keyword
 */
void CNassign_string_keyword(keyval,value,label,verbose)
char   *keyval;             /* The value to be translated to   */
char   *value;              /* The value to be translated from */
char   *label;              /* The keyword label to be printed */
int    verbose;             /* Verbosity flag                  */
{
   /* Check the value */
   if (value == NULL) {
      (void) fprintf(stderr,
                     "   Options : *** Cannot copy null value to keyword!\n");
      return;
   }

   /* Copy the string */
   (void) strcpy(keyval,CNstrip_keyword(value));

   if (verbose) 
      (void) fprintf(stdout,"   Options : %-14s= \"%s\"\n", label, keyval);
}


/*
 * Strip off leading characters, including quotes
 */
char *CNstrip_keyword(value)
char *value;
{
   static char keyword[CN_MAXCHAR];
   int    len;

   /* Copy the string */
   len = strlen(value);
   if (len > 0) {
      if (value[0] == '"') {

         /* Strip off leading and ending quote '"' */
         (void) strcpy(keyword,value+1);
         len--;
         if ((len>=1) && (keyword[len-1]=='"')) keyword[len-1]='\0';

      } else if (value[0] == '\'') {

         /* Strip off leading and ending quote '\'' */
         (void) strcpy(keyword,value+1);
         len--;
         if ((len>=1) && (keyword[len-1]=='\'')) keyword[len-1]='\0';

      } else if (value[0] == '(') {

         /* Strip off leading and ending brackets '(',')' */
         (void) strcpy(keyword,value+1);
         len--;
         if ((len>=1) && (keyword[len-1]==')')) keyword[len-1]='\0';

      } else {

         /* Copy the string as is */
         (void) strcpy(keyword,value);

      }

   } else {

      /* Copy the string as is */
      (void) strcpy(keyword,value);

   }

   /* Return */
   return(keyword);
}


/*
 * STRING MANIPULATION 
 */

/*
 * Check the length of a string 
 */
int CNstrlen(label)
char *label;
{
   /* 
    * If the string is NULL, then don't use strlen 
    */
   if (label == NULL)  
      return(0);
   else
      return(strlen(label));
}

/*
 * Allocate room for a string
 */
char *CNcreate_string(label)
char *label;
{
   char *newstr = NULL;
   int  len;
   unsigned int wsize;

   if (label == NULL) return(newstr);

   /* Make sure that the \0 terminating character is saved too */
   len=strlen(label);
   len++;

   wsize = (unsigned int)(len*sizeof(char));
   if ((newstr = (char *)malloc(wsize)) != NULL)
      (void) strcpy(newstr,label);
   return(newstr);
}


/*
 * Allocate room for a string
 */
void CNdestroy_string(label)
char *label;
{
   if (label!=NULL)
      free(label);
}

/*
 * Get the string address after the last "/"
 * This is to convert a string such as "/usr/etc" to "etc"
 */
char *CNstring_concat(name)
char *name;
{
   char *newstr;

   if ((newstr = strrchr(name,'/')) == NULL || strlen(newstr) <= 0)
      newstr = name;
   else
      ++newstr;

   return(newstr);
}


/*
 * convert a string from lowercase to uppercase
 */
void CNstring_to_upper(string)
char *string;
{
   int i;

   /* scan the string; convert lowercase to uppercase */
   for (i=0; i<CN_MAXCHAR && string[i] != '\0'; i++) {
      if (islower(string[i]))
         string[i] = toupper(string[i]);
   }
}


/*
 * convert a string from uppercase to lowercase
 */
void CNstring_to_lower(string)
char *string;
{
   int i;

   /* scan the string; convert uppercase to lowercase */
   for (i=0; i<CN_MAXCHAR && string[i] != '\0'; i++) {
      if (isupper(string[i]))
         string[i] = tolower(string[i]);
   }
}


/*
 * Convert a character string into boolean
 */
int CNstring_to_boolean(string)
char *string;
{
   int val;
   char word[CN_MAXCHAR];

   /* convert the word to uppercase first, for easier comparisons */
   (void) strcpy(word,string);
   CNstring_to_upper(word);

   /* "T", "TRUE", "F", "FALSE", "ON", "OFF" are all accepted */
   if ((strncmp(word,"T",1)==0) || (strcmp (word,"ON")==0))
      val = CN_TRUE;
   else if ((strncmp(word,"F",1)==0) || (strcmp (word,"OFF")==0))
      val = CN_FALSE;
   else
      val = CN_ERROR;
   return(val);
}

