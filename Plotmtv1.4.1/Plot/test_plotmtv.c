/*
 * test_plotmtv.c - program to test the mtv read and plotting routines
 */

/*
 * #ifdef INTEL_ONLY are used to denote sections of code that have been
 * disabled.  These are primarily the hierarchical mesh formats which
 * are specialized for process/device modeling
 * Also the XDR machine-independent format has been disabled.
 *
 * The ifdef statements are left here to allow easier comparison with
 * intel specific code which is not available for public distribution
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "PXplot.h"
#include "CNplot.h"
#include "patchlevel.h"

/* 
 * Definitions
 */
#define PSFILE     "dataplot.ps"

/* 
 * Globals
 */
static char   progname[CN_MAXCHAR];
static char   printer [CN_MAXCHAR];
static char   printCmd[CN_MAXCHAR];
static char   psfile  [CN_MAXCHAR];
static double scale;
static int    landscape;
static int    postscript;
static int    printplot;
static int    noxplot;
static int    verbose;
static int    debug;
static int    color_postscript;
static int    nodate;
static int    plot3d;
static int    vertplot;
static int    drawframe;
static int    nopixmap;
static int    plot_all;   /* All plots in a single window */
static int    plot_mult;  /* All plots in a single multi-paned window */

/*
 * Data storage
 */
static CNplotsetptr plot_listhead;   /* Head to list of plots    */
static CNplotsetptr plot_listtail;   /* Tail to list of plots    */
static int          plotID;          /* Plot ID                  */

static CNdatasetptr data_listhead;   /* Head to list of datasets */
static CNdatasetptr data_listtail;   /* Tail to list of datasets */
static int          dataID;          /* Data ID                  */

static char *datafileArray[CN_MAXWORDS];
static int  ndatafiles=0;
static int  clobber = CN_FALSE;

/*
 * X11 commands
 */
static char  *geometry;
static int    reverse;

static char *plot_foreground=NULL;
static char *plot_background=NULL;

/*
 * Procedure Declarations
 */
static void         initialize_data();
static void         print_init_message();
static void         read_commandline_options();
static void         usage();
static void         read_mtvdata();
static void         do_plot();
static CNplotsetptr create_plotset();
static CNplotsetptr create_single_plotset();
static void         draw_view();
static void         draw_view_mult();
static void         do_cleanup();

/* 
 * Main program 
 */
main(argc,argv)
int argc;
char **argv;
{
   /* Initialize data first */
   initialize_data(argv[0]);

   /* Read the options */
   read_commandline_options(argc, argv);

   /* Plot */
   do_plot();

   /* Clean up */
   do_cleanup();

   /* Quit */
   (void) fprintf(stdout,"Congratulations!!!  You have been TERMINATED...\n");
   exit(0);
}


/*
 * Initialize data 
 */
static void initialize_data(pgname)
char *pgname;
{
   char   *getenv();
   char   *ptr;

#ifndef PRINTER_NAME
   static char *printer_name = "hpps";
# else
   static char *printer_name = PRINTER_NAME;
#endif

#ifndef PRINTER_CMD
   static char *printer_cmd  = "lpr -h";
#else
   static char *printer_cmd  = PRINTER_CMD;
#endif

   /* Program name */
   (void) strcpy(progname,pgname);

   /* Print the initial message */
   print_init_message();

   /* printer type */
   if ((ptr = getenv("PRINTER")) == NULL) {
      (void) strcpy(printer,printer_name);
   } else {
      (void) strcpy(printer,ptr);
   }
   (void) fprintf(stdout,"   Default Printer           = %s\n",printer);

   /* printer command */
   if ((ptr = getenv("MTV_PRINTER_CMD")) == NULL) {
      (void) strcpy(printCmd,printer_cmd);
   } else {
      (void) strcpy(printCmd,ptr);
   }
   (void) fprintf(stdout,"   Default Print Command     = %s\n",printCmd);

   /* Global variables */
   verbose           = CN_FALSE;
   debug             = CN_FALSE;

   /* Plot variables */
   postscript        = CN_TRUE;
   printplot         = CN_FALSE;
   noxplot           = CN_FALSE;
   landscape         = CN_FALSE;
   scale             = 1.00;
   plot3d            = CN_FALSE;
   nodate            = CN_FALSE;
   drawframe         = CN_TRUE;  

   /* Plot-window variables */
   plot_all          = CN_FALSE;
   plot_mult         = CN_FALSE;
   vertplot          = CN_FALSE;
   nopixmap          = CN_FALSE;

   /* X11 variables */
   geometry          = NULL;
   reverse           = CN_FALSE;

   /* Datafile array */
   ndatafiles        = 0;
   clobber           = CN_FALSE;

   /* Postscript file */
   (void) strcpy(psfile,PSFILE);

   /* check the POSTSCRIPT environment variable */
   postscript = CN_TRUE;
   if ((ptr = getenv("MTV_POSTSCRIPT")) != NULL) {
      CNstring_to_lower(ptr);
      if ((strcmp(ptr,"off")==0) || (strcmp(ptr,"false")==0)) {
         postscript = CN_FALSE;
         (void) fprintf(stdout,"   Postscript                = OFF\n");
      }
   }

   /* check the PSCOLOR    environment variable */
   color_postscript = CN_FALSE;
   if ((ptr = getenv("MTV_PSCOLOR")) != NULL) {
      CNstring_to_lower(ptr);
      if ((strcmp(ptr,"on")==0) || (strcmp(ptr,"true")==0)) {
         color_postscript = CN_TRUE;
         (void) fprintf(stdout,"   Color Postscript          = ON\n");
      }
   }

   /* check MTV_COLORMAP environment variable */
   if ((ptr = getenv("MTV_COLORMAP")) != NULL) {
     CNstring_to_lower(ptr);
     (void) fprintf(stdout,"   Default Colormap          = %s\n",ptr);
   }

   /* check MTV_PSFONT environment variable */
   if ((ptr = getenv("MTV_PSFONT")) != NULL) {
     CNstring_to_lower(ptr);
     (void) fprintf(stdout,"   Default PostScript Font   = %s\n",ptr);
   }

   /* check MTV_PSITALIC environment variable */
   if ((ptr = getenv("MTV_PSITALIC")) != NULL) {
     CNstring_to_lower(ptr);
     if ((strcmp(ptr,"on")==0) || (strcmp(ptr,"true")==0))
       (void) fprintf(stdout,"   MTV_PSITALIC              = ON\n");
   }
    
   /* Linked lists */
   plotID            = 1;
   plot_listhead     = NULL;
   plot_listtail     = NULL;
   dataID            = 1;
   data_listhead     = NULL;
   data_listtail     = NULL;
}


/*
 * Print the inital prompt
 */
static void print_init_message()
{
  /*
    (void) fprintf(stdout,"\nWelcome to %s V%d.%d.%d %s\n",
    progname,VERSION,PATCHLEVEL,SUBPATCHLEVEL,DATE);
    */
  (void) fprintf(stdout,
		 "\nWelcome to Jai's Special Version of Plotmtv1.4.1");
  (void) fprintf(stdout,
		 "\nAustin J. Lee 03/23/97\n");
}


/* 
 * Read the command-line options as well as data.
 * The results are all saved in static variables
 */
static void read_commandline_options(argc, argv)
int argc;
char** argv;
{
   int  i, FILEFOUND = CN_FALSE;
   char datafile[CN_MAXCHAR];
   int  err = CN_FALSE;

   /* Parse the line first */
   for (i=1; i<argc; i++) {
      if (strncmp(argv[i],"-verbose",2) == 0) {    /* verbose     */
         verbose = CN_TRUE;
         (void) fprintf(stdout,"   Commands: verbose         = ON\n");
         continue;
      }

      if (strncmp(argv[i],"-debug",2) == 0) {      /* debug       */
         verbose = CN_TRUE;
         debug   = CN_TRUE;
         (void) fprintf(stdout,"   Commands: debug           = ON\n");
         continue;
      }

      if (strcmp(argv[i],"-quick") == 0) {         /* program identifier */
         continue;
      }

      if ( (strcmp(argv[i],"-nops") == 0) ||
           (strcmp(argv[i],"-ps"  ) == 0) ) {      /* postscript */
         (void) fprintf(stdout,
                "***Sorry, the \"%s\" option has been disabled\n",argv[i]);
         continue;
      }

      if ( (strcmp(argv[i],"-print") == 0) ||
           (strcmp(argv[i],"-prt")   == 0) ) {     /* print */
         printplot = CN_TRUE;
         (void) fprintf(stdout,"   Commands: printplot       = ON\n");
         continue;
      }

      if ( (strncmp(argv[i],"-noxplot",5) == 0) ||
           (strncmp(argv[i],"-noplot" ,5) == 0) ) {    /* noxplot */
         noxplot = CN_TRUE;
         (void) fprintf(stdout,"   Commands: noxplot         = ON\n");
         continue;
      }

      if (strncmp(argv[i],"-landscape",2) == 0) {  /* landscape */
         landscape = CN_TRUE;
         (void) fprintf(stdout,"   Commands: landscape       = ON\n");
         continue;
      }

      if (strncmp(argv[i],"-scale",2) == 0) {      /* scale  */
         if (++i >= argc) { usage(progname); exit(-1); }
         scale = atof(argv[i]);
         if (scale < 0.1 || scale > 1.0) {
            scale = 1.0;
            (void) fprintf(stdout,"   Warning : Invalid scale -");
            (void) fprintf(stdout," scale reset to %g\n",scale);
         }
         (void) fprintf(stdout,"   Commands: scale           = %g\n",scale);
         continue;
         }

      if ( (strcmp(argv[i],"-colorps") == 0) || 
           (strcmp(argv[i],"-color"  ) == 0) ||                    
           (strcmp(argv[i],"-pscolor") == 0) ) {   /* color ps */
         color_postscript = CN_TRUE;
         (void) fprintf(stdout,"   Commands: color postscript= ON\n");
         continue;
      }

      if (strcmp(argv[i],"-noframe") == 0) {        /* no frame on PS plot */
         drawframe = CN_FALSE;
         (void) fprintf(stdout,"   Commands: noframe         = ON\n");
         continue;
      }

      if (strcmp(argv[i],"-nodate") == 0) {        /* no postscript date */
         nodate = CN_TRUE;
         (void) fprintf(stdout,"   Commands: nodate          = ON\n");
         continue;
      }

      if ((strncmp(argv[i],"-3d",3) == 0) ||   
          (strncmp(argv[i],"-3D",3) == 0)) {       /* 3D plot  */
         plot3d           = CN_TRUE;
         (void) fprintf(stdout,"   Commands: plot3D          = ON\n");
         continue;
      }

      if (strncmp(argv[i],"-nopixmap",5) == 0) {   /* don't use pixmap */
         nopixmap = CN_TRUE;
         (void) fprintf(stdout,"   Commands: nopixmap        = ON\n");
         continue;
         }

      if (strncmp(argv[i],"-plotall",4) == 0) {    /* Plot all */
         plot_all         = CN_TRUE;
         (void) fprintf(stdout,"   Commands: plotall         = ON\n");
         continue;
         }

      if ((strncmp(argv[i],"-combo",4) == 0) ||
          (strncmp(argv[i],"-multiple",4) == 0)) { /* mult plots, same page */
         plot_mult        = CN_TRUE;
         (void) fprintf(stdout,"   Commands: combination plot= ON\n");
         continue;
         }

      if (strcmp(argv[i],"-forcevert") == 0) {     /* vertical combo plot */
         vertplot         = CN_TRUE;
         (void) fprintf(stdout,"   Commands: vertical combo  = ON\n");
         continue;
         }

      if ((strncmp(argv[i],"-pfg",3) == 0) ||      /* foreground color */
          (strcmp (argv[i],"-fg") == 0)) {
         if (++i >= argc) { usage(progname); exit(-1); }
         plot_foreground  = argv[i];
         (void) fprintf(stdout,"   Commands: foreground color= %s\n",
                        plot_foreground);
         continue;
      }

      if ((strncmp(argv[i],"-pbg",3) == 0) ||      /* background color */
          (strcmp (argv[i],"-bg") == 0)) {
         if (++i >= argc) { usage(progname); exit(-1); }
         plot_background  = argv[i];
         (void) fprintf(stdout,"   Commands: background color= %s\n",
                        plot_background);
         continue;
      }

      if (strncmp(argv[i],"-P",2) == 0) {          /* Printer */
         (void) strcpy(printer,argv[i]+2);
         (void) fprintf(stdout, "   Commands: Printer         = %s\n",printer);
         continue;
      }

      if (strcmp(argv[i],"-printcmd") == 0) {      /* print command */
         if (++i >= argc) { usage(progname); exit(-1); }
         (void) strcpy(printCmd, argv[i]);
         (void) fprintf(stdout,"   Commands: Print command   = \"%s\"\n",
                          printCmd);
         continue;
      }

      if (strncmp(argv[i],"-geom",2) == 0) {       /* X11 geometry */
         if (++i >= argc) { usage(progname); exit(-1); }
         geometry = argv[i];
         (void) fprintf(stdout,"   Commands: X11 Geometry    = \"%s\"\n",
                        geometry);
         continue;
      }

      if ((strcmp(argv[i],"-rv") == 0) ||
          (strcmp(argv[i],"-reverse") == 0)) {     /* X11 reverse color */
         reverse          = CN_TRUE;
         (void) fprintf(stdout,"   Commands: reverse         = ON\n");
         continue;
      }

      if (strcmp(argv[i],"-o") == 0) {             /* postscript file */
         if (++i >= argc) { usage(progname); exit(-1); }
         (void) strcpy(psfile, argv[i]);
         (void) fprintf(stdout,"   Commands: Postscript file = \"%s\"\n",
                          psfile);
         continue;
         }

      if (strncmp(argv[i],"-clobber",4) == 0) {    /* Delete datafile */
         clobber          = CN_TRUE;
         (void) fprintf(stdout,"   Commands: clobber         = ON\n");
         continue;
         }

      if (argv[i][0] == '-') err = CN_TRUE;

      if (argv[i][0] != '-' && argv[i][0] != '=') {
         FILEFOUND = CN_TRUE;
         (void) strcpy(datafile,argv[i]);
         /* Read the data and store in the global dataset list */
         read_mtvdata(datafile);
         (void) fprintf(stdout,
                "   Commands: Data File       = %s\n",datafile);

         /* Save the datafile in the array */
         CNsaveword(datafileArray, CN_MAXWORDS, datafile, &ndatafiles,
                    strlen(datafile)+2);
      }
   }

   /* Serious syntax error */
   if (!FILEFOUND) {
      usage(progname);
      (void) fprintf(stderr,"%s: Datafile has not been specified!\n",progname);
      exit(-1);
   }

   /* Incorrect specifications (non-critical syntax error) */
   if (err) usage(progname);

   /* Go thru the datasets */
   if (data_listhead == NULL) {
      (void) fprintf(stderr,
            "%s: Unable to find valid data in the file \"%s\"!\n",
            progname,datafile);
      do_cleanup();
      exit(-1);
   }
}


/*
 * Print out valid command line options
 */
static void usage(progname)
char *progname;
{
   (void) fprintf(stderr,"%s [-Pprinter] \
[-printcmd command] \
[-colorps] \
[-3d] \
[-v] \
[-d] \
[-l] \
[-scale scale] \
[-noframe] \
[-nodate] \
[-noxplot] \
[-plotall] \
[-combo] \
[-print] \
[-nopixmap] \
[-geom <geometry>] \
[-rv] \
[-pfg foreground_color] \
[-pbg background_color] \
[-fg foreground_color] \
[-bg background_color] \
[-o postscript-file] \
[-clobber] \
datafile1 datafile2...\n", progname);
}


/*
 * Read data in "mtvplot" format
 */
static void read_mtvdata(filename)
char        *filename;                /* Data file to read             */
{

   CNdslistptr dshead=NULL, dstail=NULL, ds;
   CNdatasetptr dptr=NULL;

   /*
    * Read the plotmtv data from a file and store in a data-structure
    */
   CNread_plotmtv(filename,(FILE *)NULL,CN_FILE,
                  data_listhead, data_listtail,
                  &dshead,&dstail,&dataID,verbose);

   /* Copy the data to the local list */
   for (ds=dshead; ds!=NULL; ds=ds->next)
      if (ds->Dptr) CNstore_dataset(&data_listhead, &data_listtail, 
                                    ds->Dptr, 0);

#ifdef INTEL_ONLY
   /* Fill out quantity datasets */
   for (dptr=data_listhead; dptr!=NULL; dptr=dptr->next)
      if (dptr->datatype == CN_PIF_CHILD)
         (void) CNfill_pif_quantity_dataset(dptr,verbose);
#endif

   /* Delete the dslist */ 
   CNdelete_dslist_list(&dshead, &dstail);
}


/*
 * Draw the plot
 */
static void do_plot()
{
   CNdatasetptr dptr;
   CNplotsetptr pptr;
   CNpslistptr  pshead=NULL, pstail=NULL;
   int          plottype;

   /* 
    * The way this works is as follows:
    *  (1) Create a bunch of plotsets, one for each dataset.
    *      (if plot_all=True, then create only 1 plotset containing all
    *       the datasets.)
    *  (2) Send the list of plotsets to the appropriate drawing routine.
    *      draw_view() draws each plotset one at a time, while
    *      draw_view_mult() draws draws all the plotsets in the same window.
    */
   if (plot_all) {

      /* Create a single plotset containing all the datasets */
      plottype = plot3d ? CN_PLOT3D : CN_PLOT2D;
      if ((pptr = create_single_plotset(plottype, verbose)) != NULL) {
         (void) CNinsert_pslist(&pshead, &pstail, pptr);
      }

      /* Draw the plot */
      draw_view(pshead, pstail);

      /* Delete */
      CNdelete_pslist_list(&pshead, &pstail);
      CNdelete_plotset_list(&plot_listhead, &plot_listtail);

   } else {

      /* Create a list of plotsets to be plotted */
      for (dptr=data_listhead; dptr!=NULL; dptr=dptr->next) {
          plottype = plot3d ? CN_PLOT3D : CN_PLOT2D;
          if ((pptr = create_plotset(dptr, plottype, verbose)) != NULL) {
             (void) CNinsert_pslist(&pshead, &pstail, pptr);
          }
      } 

      /* Now plot this */
      if (pshead !=NULL) {
         if (plot_mult) {
            /* Draw all the plotsets in the same window */
            draw_view_mult(pshead, pstail);
         } else {
            /* Draw each plotset one at a time */
            draw_view(pshead, pstail);
         }
      }

      /* Delete */
      CNdelete_pslist_list(&pshead, &pstail);
      CNdelete_plotset_list(&plot_listhead, &plot_listtail);
   }
}



/*
 * Create a plotset
 * Take a dataset and encapsulate it in a plotset
 * This puts a single dataset into a single plotset.
 *
 * The plotset is stored in the global plotset list AND also returned
 * to the calling routine.
 */
static CNplotsetptr create_plotset(dptr, plottype, debug)
CNdatasetptr dptr;
int          plottype;
int          debug;
{
   CNplotsetptr WP;
   CNdslistptr  dshead=NULL, dstail=NULL, DS;
   int          contour=CN_FALSE, eqscal=CN_FALSE;

   if (debug) (void) fprintf(stdout,"Creating a new plotset...\n");

   /* Error checking */
   if (dptr == NULL) {
      (void) fprintf(stderr,"create_plotset() : Error! NULL dataset!\n");
      return(NULL);
   }

   /* Put the dataset in a list */
   (void) CNinsert_dslist(&dshead, &dstail, dptr);

   /* Create the plotset */
   WP = CNinsert_plotset(&plot_listhead, &plot_listtail, CN_NONAME, plotID++);

   /*
    * Set the options/properties of the plotset
    */

   /* plot-type */
   WP->plottype = plottype;

   /* Plot format */
   if (dshead->Dptr->datatype == CN_PROBAB) 
      WP->plotformat = CN_PROBABILITY_PLOT;
   else if (dshead->Dptr->datatype == CN_HISTOGRAM)
      WP->plotformat = CN_HISTOGRAM_PLOT;
   else if (dshead->Dptr->datatype == CN_BARCHART)
      WP->plotformat = CN_BARCHART_PLOT;

   /* Insert the dataset into the plotset */
   WP->datahead = dshead;
   WP->datatail = dstail;

   /* Check the list for contour type datasets */
   for (DS=dshead; DS!=NULL && !contour; DS=DS->next) { 
      if (DS->Dptr->datatype == CN_CONTOUR) contour = CN_TRUE;
   }

   /* Check the list for equalscale datasets */
   for (DS=dshead; DS!=NULL && !eqscal; DS=DS->next)
      if (DS->Dptr->plot_pr.equalscale) eqscal = CN_TRUE;

   /* Set the plot shape */
   if (contour || eqscal) {
      WP->plot_pr.equalscale = CN_TRUE;
      WP->plot_pr.fitpage    = CN_FALSE;
      WP->plot_pr.flag1 = WP->plot_pr.flag1 | CNequalscale;
      WP->plot_pr.flag1 = WP->plot_pr.flag1 | CNfitpage;
   }

   /* Apply the properties based on the first dataset */
   CNset_plotset_property(&(WP->plot_pr),&(dshead->Dptr->plot_pr));

   /* Apply the view properties based on the first dataset */
   CNset_view_property(WP->view_pr,dshead->Dptr->view_pr);

   /* Set the viewport of the plotset */
   CNreset_plotset_viewport(WP);

   /* Return the plotset */
   return(WP);
}

/*
 * Create a plotset
 * Take the dataset list and encapsulate it in a plotset
 * This puts all the datasets in a single plotset.
 *
 * The plotset is stored in the global plotset list AND also returned
 * to the calling routine.
 */
static CNplotsetptr create_single_plotset(plottype, debug)
int          plottype;
int          debug;
{
   CNplotsetptr WP;
   CNdatasetptr dptr;
   CNdslistptr  dshead=NULL, dstail=NULL, DS;
   int          contour=CN_FALSE, eqscal=CN_FALSE;

   if (debug) (void) fprintf(stdout,"Creating a new plotset...\n");

   /* Error checking */
   if (data_listhead == NULL) {
      (void) fprintf(stderr,"create_plotset() : Error! NULL dataset!\n");
      return(NULL);
   }

   /* Put all the datasets in a list */
   for (dptr=data_listhead; dptr!=NULL; dptr=dptr->next)
   (void) CNinsert_dslist(&dshead, &dstail, dptr);

   /* Create the plotset */
   WP = CNinsert_plotset(&plot_listhead, &plot_listtail, CN_NONAME, plotID++);

   /*
    * Set the options/properties of the plotset
    */

   /* plot-type */
   WP->plottype = plottype;

   /* Plot format */
   if (dshead->Dptr->datatype == CN_PROBAB) 
      WP->plotformat = CN_PROBABILITY_PLOT;
   else if (dshead->Dptr->datatype == CN_HISTOGRAM)
      WP->plotformat = CN_HISTOGRAM_PLOT;
   else if (dshead->Dptr->datatype == CN_BARCHART)
      WP->plotformat = CN_BARCHART_PLOT;
   else
      WP->plotformat = CN_SCIENTIFIC_PLOT;

   /* Insert the dataset into the plotset */
   WP->datahead = dshead;
   WP->datatail = dstail;

   /* Check the list for contour type datasets */
   for (DS=dshead; DS!=NULL && !contour; DS=DS->next) {
      if (DS->Dptr->datatype == CN_CONTOUR) contour = CN_TRUE;
   }

   /* Check the list for equalscale datasets */
   for (DS=dshead; DS!=NULL && !eqscal; DS=DS->next)
      if (DS->Dptr->plot_pr.equalscale) eqscal = CN_TRUE;

   /* Set the plot shape */
   if (contour || eqscal) {
      WP->plot_pr.equalscale = CN_TRUE;
      WP->plot_pr.fitpage    = CN_FALSE;
      WP->plot_pr.flag1 = WP->plot_pr.flag1 | CNequalscale;
      WP->plot_pr.flag1 = WP->plot_pr.flag1 | CNfitpage;
   }

   /* Don't modify overlapping datasets */
   (void) CNparse_plotset_property(&(WP->plot_pr),"overlay","off",0);

   /* Apply the properties based on the first dataset */
   CNset_plotset_property(&(WP->plot_pr),&(dshead->Dptr->plot_pr));

   /* Apply the view properties based on the first dataset */
   CNset_view_property(WP->view_pr,dshead->Dptr->view_pr);

   /* Set the viewport of the plotset */
   CNreset_plotset_viewport(WP);

   /* Return the plotset */
   return(WP);
}

/*
 * Plot the data 
 */
static void draw_view(pshead, pstail)
CNpslistptr pshead, pstail;
{
   int          PXcreateXWindow();
   int          fileprt=CN_FALSE;
   int          prtdate=!nodate;
   char         error_message[CN_MAXCHAR];
   char         toplabel[CN_MAXCHAR];
   CNplotsetptr pptr;
   CNpslistptr  ps;
   int          c;
   int          status;

   /* Check the pptr list */
   if (pshead == NULL || pshead->Pptr==NULL) {
      (void) fprintf(stderr,"Error! NULL data in draw_view_mult()!\n");
      return;
   }

   if (!noxplot) {
      /*
       * X11 Plot 
       * Printing is done via a button on the plot window
       */
      status = PXcreateXWindow(pshead,pstail,progname,"",
                             plot_foreground,plot_background,nopixmap,
                             psfile, printCmd, printer,
                             scale, drawframe,
                             landscape, prtdate, color_postscript,
                             geometry, reverse, 
                             debug);
      if (status == 0) noxplot = CN_TRUE;

   } else {

      /*
       * Postscript Plot
       * noxplot=TRUE; do a batch-type postscript plot
       */

      /* Cycle through the pslist */
      fileprt = printplot;
      for (ps=pshead; ps!=NULL; ps=ps->next) {
         pptr = ps->Pptr;
         if (pptr == NULL) continue;
 
         if (!printplot) {

            /* Toplabel */
            if (pptr->plot_pr.toplabel == NULL)
               (void) strcpy(toplabel , CN_DEF_TLABEL);
            else
               (void) strcpy(toplabel , pptr->plot_pr.toplabel);

            /* Print some info about the plotset */
            (void) fprintf(stdout,"   Plotset %d\n", pptr->ID);
            (void) fprintf(stdout,"   Title = \"%s\"\n", toplabel);
            
            /* prompt for a print out */
            (void) fprintf(stdout,"   Type return (or n/q) to ");
            (void) fprintf(stdout,"send the plot to the %s printer :",printer);
            if ( (c=getc(stdin)) == '\n' || c == 'y' || c == 'Y' ) {
               fileprt = CN_TRUE;
            } else if (c == 'q')
               exit(1);
            if (c != '\n') {
               while ( (c=getc(stdin)) != '\n')
                  /*EMPTY*/
                  ;
            }
         } 

         /* print directly */
         PXplotps(psfile, printCmd, printer, 
                  scale, drawframe, 
                  landscape, prtdate, PX_EPSI, color_postscript,
                  1, fileprt, pptr, error_message, verbose);
      }
   }
}


/*
 * Plot the data 
 */
static void draw_view_mult(pshead, pstail)
CNpslistptr  pshead, pstail;
{
   int PXcreateXMultWindow();
   int fileprt=CN_FALSE;
   int prtdate=!nodate;
   int c;
   char error_message[CN_MAXCHAR];
   int  status;

   /* Check the pptr list */
   if (pshead == NULL || pshead->Pptr==NULL) {
      (void) fprintf(stderr,"Error! NULL data in draw_view_mult()!\n");
      return;
   }

   if (!noxplot) {
      /*
       * X11 Plot 
       */
      status = PXcreateXMultWindow(pshead,pstail,progname,"",
                             plot_foreground,plot_background,nopixmap,
                             psfile, printCmd, printer,
                             scale, drawframe,
                             landscape, prtdate, color_postscript,
                             geometry, reverse, 
                             vertplot, debug);
      if (status == 0) noxplot = CN_TRUE;

   } else {

      /*
       * Postscript Plot
       * noxplot=TRUE; do a batch-type postscript plot
       */
      fileprt = printplot;
      if (!printplot) {
         /* prompt for a print out */
         (void) fprintf(stdout,"   Type return (or n/q) to ");
         (void) fprintf(stdout,"send the plot to the %s printer :",printer);
         if ( (c=getc(stdin)) == '\n' || c == 'y' || c == 'Y' ) {
            fileprt = CN_TRUE;
         } else if (c == 'q')
            exit(1);
         if (c != '\n') {
            while ( (c=getc(stdin)) != '\n')
               /*EMPTY*/
               ;
         }
      } 

      /* print directly */
      PXplotps_mult(psfile, printCmd, printer, 
                    scale, drawframe, 
                    landscape, prtdate, PX_EPSI, color_postscript,
                    1, fileprt, vertplot,
                    pshead, pstail, error_message, verbose);
   }
}


/*
 * Clean up
 */
static void do_cleanup()
{
   int i=0;

   /* Delete the datasets */
   CNdelete_dataset_list(&data_listhead, &data_listtail);

   /* Delete the datafiles */
   if (clobber) {
      (void) fprintf(stdout, "\n");
      for (i=0; i<ndatafiles; i++) {
         if (unlink(datafileArray[i]) == 0) {
            (void) fprintf(stdout, "Deleted file \"%s\"\n",
                           datafileArray[i]);
         } else {
            (void) fprintf(stdout, "Error! Couldn't delete file \"%s\"!\n",
                           datafileArray[i]);
         }
      }
   }

   /* Delete the datafile array */
   CNfreewords(&ndatafiles, datafileArray);
}

