/*
 * pdraw2mtv.c - program to convert from pdraw-format data to plotmtv
 */
 
#include <stdio.h>
#include <strings.h>
#include "CNplot.h"

static CNdslistptr dslisthead=NULL;
static CNdslistptr dslisttail=NULL;

static void usage();
static void read_pdrawdata();

main(argc,argv)
int argc;
char **argv;
{
   char progname[CN_MAXCHAR];
   char outfile [CN_MAXCHAR];
   char datafile[CN_MAXCHAR];
   int  xdrbinary=CN_FALSE;
   int  binary   =CN_FALSE;
   int  FILEFOUND=CN_FALSE;
   int  i;
   
   /* Initialize */
   (void) strcpy(outfile ,"plot.mtv");
   (void) strcpy(progname,argv[0]);
   
   /* Parse the line first */
   for (i=1; i<argc; i++) {
      if (strncmp(argv[i],"-b",2) == 0) {           /* binary data */
         binary = CN_TRUE;
         continue;
      }
      if (strcmp(argv[i],"-o") == 0) {              /* output file */
         if (++i >= argc) usage(progname);
         (void) strcpy(outfile,argv[i]);
         continue;
      }
      if (argv[i][0] != '-' && argv[i][0] != '=') { /* input file */
         FILEFOUND = CN_TRUE;
         (void) strcpy(datafile,argv[i]);
         (void) fprintf(stdout,
                "\nReading pdraw data from \"%s\"...\n",datafile);
         read_pdrawdata(datafile);

      }
   }

   /* Serious syntax error */
   if (!FILEFOUND) {
      usage(progname);
      (void) fprintf(stderr,"%s: Datafile has not been specified!\n",progname);
      exit(-1);
   }

   /* If no datasets found */
   if (dslisthead == NULL) {
      (void) fprintf(stderr,"%s: Error! No data available!\n",progname);
      exit(-1);
   }

   /* Message */
   (void) fprintf(stdout,
                  "Writing %d datasets to \"%s\" in %s MTVDAT format...\n",
                  CNcount_dslists(dslisthead, dslisttail), 
                  outfile,  ((binary) ? "BINARY" : "ASCII"));

   /*
    * Print the data
    */
   CNwrite_plotmtv(outfile,(FILE *)NULL,CN_FILE,
                   (CNdatasetptr) NULL, (CNdatasetptr) NULL,
                   &dslisthead,&dslisttail,binary,xdrbinary,0);

   /* Stop */
   (void) fprintf(stdout,"\n%s : Done!\n",progname);
   exit(0);
}

/*
 * Print out valid command line options
 */
static void usage(progname)
char *progname;
{
   (void) fprintf(stderr,"%s [-b] [-o outfile] contourfile1 contourfile2...\n",progname);
   exit(-1);
}

/* 
 * Read the datafile in pdraw format 
 */
static void read_pdrawdata(filename)
char *filename;
{   
   CNdatasetptr dptr;


   /* Read the data from a file */
   dptr = CNread_plot3D(filename,(FILE *)NULL,CN_FILE,
                         CN_FALSE, 1, 0);

   /* Add the dataset to the list */
   if (dptr) 
   (void) CNinsert_dslist(&dslisthead, &dslisttail, dptr);
}
