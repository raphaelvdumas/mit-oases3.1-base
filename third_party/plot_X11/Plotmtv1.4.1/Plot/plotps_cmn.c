/*
 * plotps_cmn.c - PostScript subroutines for drawing plots
 *                These are common to both 2D and 3D plotting.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "plotps.h"
#include "CNplot.h"
#include "Bitmaps/eps.inc"

#define PSFILE "dataplot.ps"

/*
 * Global variables 
 */
/* Plot scale */
double scale;
double fscale;
 
/* Landscape */
short landscape;
 
/* Print the date */
short printdate;
 
/* Color */
short pscolor;

/* Draw the frame */
static int drawframe;

/* PS page dimensions */
double  Pgxmin, Pgxmax, Pgymin, Pgymax;
 
/* FILE */
FILE *ips;

/* Annotation fontsize */
static double annot_fontsize;
static int    useISOLatinFont;

/* Global Font Selection 
 * usePSfontType = 0 ; Times-Roman        & Times-Bold        ; default
 * usePSfontType = 1 ; Helvetica          & Helvetica-Bold
 * usePSfontType = 2 ; Courier            & Courier-Bold
 * usePSfontType = 3 ; Times-Italic       & Times-BoldItalic
 * usePSfontType = 4 ; Helvetica-Oblique  & Helvetica-BoldOblique
 * usePSfontType = 6 ; Courier-Oblique    & Courier-BoldOblique
 */

static int usePSfontType;

/*
 * FORWARD DECLARATIONS
 */
void         PXplotps();
void         PXplotps_mult();
static void  calc_plot_orientation();
static void  draw_singleplot();
static void  print_plot();
static void  initEPSI();
static void  initPS();
static void  readPS_options();
static void  initPS_boundingbox();
static void  drawPS_boundingbox();
static void  initPS_linetypes();
static void  initPS_colors();
static void  load_cmap();
#ifdef ANALYTIC_COLORMAP
static void  colorscale1();
static void  colorscale2();
#endif
static void  colorscale_wrb();
static void  colorscale_rgb();

#include "cmap.h"

static void  endgrPS();
static void  endgrEPSI();
 
int          PXlinetypPS();
void         PXmarkerPS();

void         PXnamedColorPS();
void         PXlineColorPS();
void         PXfillColorPS();
void         PXsetColorPS();
int          PXpolyColorIndexPS();

void         PXfillPS_polygon();
static void  PXfilltypPS();
void         PXdrawPS_line();

void         PXplotPS_linelabels();
void         PXplotPS_contscale();

void         PXtranslate_world_to_PS();
static void _PXtranslate_world_to_PS();
void         PXtranslate_PS_to_world();
static void _PXtranslate_PS_to_world();

char         *PXmodifyPS_string();

/*
 * Draw the plot in PostScript
 */
void PXplotps(filename,printCmd,prtr,
              scl,drframe,ldscap,prtdate,psformat,ps_color,copies,prplot,
              plotdata,error_message,debug)
char         *filename;              /* File to write to          */
char         *printCmd;              /* Printer command           */
char         *prtr;                  /* Printer to send file to   */
double       scl;                    /* Plot scale                */
int          drframe;                /* Draw frame on scaled plot */
int          ldscap;                 /* landscape mode            */
int          prtdate;                /* plot the date             */
int          psformat;               /* EPSI format               */
int          ps_color;               /* color postscript          */
int          copies;                 /* No of copies              */
int          prplot;                 /* Print the plot?           */
CNplotsetptr plotdata;               /* The plot data is here     */
char         *error_message;         /* Error message             */
int          debug;
{
   FILE   *fopen();
   char   printer[CN_MAXCHAR], psfile[CN_MAXCHAR];
   int    printplot;
   struct stat buf;
   off_t  filesize=0;
   double dx, dy, xc, yc;

   /* Error checking */
   if (plotdata==NULL) {
      (void) fprintf(stderr,"PXplotps(): Error! Cannot plot NULL plotset!\n");
      (void) strcpy(error_message,"Error! Cannot plot NULL plotset!\n");
      return;
   }

   /* Copy the variables to the static variables first */
   scale     = scl;
   landscape = ldscap;
   printdate = prtdate;
   printplot = prplot;
   pscolor   = ps_color;
   drawframe = drframe;
   (void) strcpy(printer,prtr);
   (void) strcpy(psfile, filename);

   /* Error-check the variables */
   if (scale < 0.01 || scale > 10.0) scale = 1.0;
   if (copies <= 0)                  copies   = 1;
   if (psformat < PX_RAW || psformat > PX_EPSF) psformat = PX_RAW;
   if (strlen(psfile )==0) (void) strcpy(psfile, PSFILE);

   /* 
    * Set the font/marker scale - this increases the size of fonts
    * when the plot is plotted at less than page sizes
    */
   fscale = scale;
   if (scale < 0.8) fscale = 0.8*scale + 0.16;

   /* Print the variables */
   if (debug) {
      (void) fprintf(stdout,"Copies     = %d\n",copies);
      (void) fprintf(stdout,"Landscape  = %d\n",landscape);
      (void) fprintf(stdout,"Print Date = %d\n",printdate);
      (void) fprintf(stdout,"Printplot  = %d\n",printplot);
      (void) fprintf(stdout,"Printer    = %s\n",printer);
      (void) fprintf(stdout,"PS Format  = %d\n",psformat);
      (void) fprintf(stdout,"PS File    = %s\n",psfile);
      (void) fprintf(stdout,"Scale      = %.3g\n",scale);
      (void) fprintf(stdout,"Font Scale = %.3g\n",fscale);
      (void) fprintf(stdout,"Plotset ID = %d\n",plotdata->ID);
   }

   /* 
    * Now do the PostScript plot 
    */

   /* PostScript plot */
   if ((ips = fopen(psfile,"w")) == NULL) {
      (void) fprintf(stderr,"cat: can't open %s\n",psfile);
      (void) sprintf(error_message,"Error! Can't open %s\n",psfile);
      return;
   }
   (void) fprintf(stdout,"\n   Proceeding with PostScript Plot...");

   /* Set the page dimensions - include scaling factor         */
   /* The plot is always plotted around the center of the page */
   Pgxmin = 0.0;
   Pgxmax = (!landscape) ? PX_DIM : PXL_DIM;
   Pgymin = 0.0;
   Pgymax = (!landscape) ? PY_DIM : PYL_DIM;
   if (scale != 1.0) {
      dx     = 0.5*(Pgxmax - Pgxmin);
      xc     = 0.5*(Pgxmax + Pgxmin);
      Pgxmin = xc - scale*dx;
      Pgxmax = xc + scale*dx;
      dy     = 0.5*(Pgymax - Pgymin);
      yc     = 0.5*(Pgymax + Pgymin);
      Pgymin = yc - scale*dy;
      Pgymax = yc + scale*dy;
   }
#ifdef DEBUG
   (void) printf("Page dimensions:\n");
   (void) printf("  xmin=%7.2f  xmax=%7.2f\n",Pgxmin,Pgxmax);
   (void) printf("  ymin=%7.2f  ymax=%7.2f\n",Pgymin,Pgymax);
#endif

 
   /* EPS format */
   if (psformat == PX_EPSI || psformat == PX_EPSF) initEPSI();

   /* Initialize PostScript variables */
   initPS();

   /* Initialize the bounding box */
   initPS_boundingbox();

   /* If scale < 1.0 draw the bounding box around the plot */
   if (scale < 1.0 && drawframe) drawPS_boundingbox();

   /* Draw the main part of the plot */
   if (plotdata->plottype == CN_PLOT3D) {
      PXplotps3D(plotdata);
   } else {
      PXplotps2D(plotdata);
   }

   /* End the PostScript plot */
   endgrPS();

   /* Close the EPS */
   if (psformat == PX_EPSI || psformat == PX_EPSF) endgrEPSI();

   /* Get the file size */
   (void) fflush(ips);
   if (fstat(fileno(ips),&buf)==0) filesize = buf.st_size; 

   /* Close the file */
   (void) fclose(ips);

   (void) fprintf(stdout,"done\n");
   (void) fprintf(stdout,"   The file size is %d KBytes.\n",filesize/1000);

   /* Send the plot to the printer */
   print_plot(printCmd, copies, psfile, printer, printplot, filesize, debug);
}


/*
 * Draw the plot in PostScript
 * This is used for putting multiple plots on the same page.
 *
 * This is a bit ugly in that it relies on setting global variables
 * to set the size of each plot.  But hey, it works...
 */
void PXplotps_mult(filename,printCmd,prtr,
                   scl,drawframe,ldscap,prtdate,psformat,ps_color,copies,prplot,
                   force_vertical,
                   pshead,pstail,error_message,debug)
char         *filename;              /* File to write to          */
char         *printCmd;              /* Printer command           */
char         *prtr;                  /* Printer to send file to   */
double       scl;                    /* Plot scale                */
int          drawframe;              /* Draw frame on scaled plot */
int          ldscap;                 /* landscape mode            */
int          prtdate;                /* plot the date             */
int          psformat;               /* EPSI format               */
int          ps_color;               /* color postscript          */
int          copies;                 /* No of copies              */
int          prplot;                 /* Print the plot?           */
int          force_vertical;         /* Force vertical plots      */
CNpslistptr  pshead, pstail;         /* The plot data is here     */
char         *error_message;         /* Error message             */
int          debug;
{
   FILE   *fopen();
   char   printer[CN_MAXCHAR], psfile[CN_MAXCHAR];
   int    printplot;
   struct stat buf;
   off_t  filesize=0;
   double dx, dy, xc, yc;
   CNpslistptr  PS;
   int          nplots, nx, ny, i, j;
   double       Master_Pgxmin, Master_Pgxmax, Master_Pgymin, Master_Pgymax;

   /* Error checking */
   if (pshead==NULL || pshead->Pptr==NULL) {
      (void) fprintf(stderr,"PXplotps(): Error! Cannot plot NULL plotset!\n");
      (void) strcpy(error_message,"Error! Cannot plot NULL plotset!\n");
      return;
   }

   /* Copy the variables to the static variables first */
   scale     = scl;
   landscape = ldscap;
   printdate = prtdate;
   printplot = prplot;
   pscolor   = ps_color;
   (void) strcpy(printer,prtr);
   (void) strcpy(psfile, filename);

   /* Don't print the date in this case */
   printdate = CN_FALSE;

   /* Error-check the variables */
   if (scale < 0.01 || scale > 10.0) scale = 1.0;
   if (copies <= 0)                  copies   = 1;
   if (psformat < PX_RAW || psformat > PX_EPSF) psformat = PX_RAW;
   if (strlen(psfile )==0) (void) strcpy(psfile, PSFILE);

   /* Count the number of plots */
   nplots = CNcount_pslists(pshead, pstail);

   /* 
    * Set the font/marker scale - this increases the size of fonts
    * when the plot is plotted at less than page sizes
    */
   fscale = scale;
   if (scale < 0.8) fscale = 0.8*scale + 0.16;

   /* Print the variables */
   if (debug) {
      (void) fprintf(stdout,"Copies     = %d\n",copies);
      (void) fprintf(stdout,"Landscape  = %d\n",landscape);
      (void) fprintf(stdout,"Print Date = %d\n",printdate);
      (void) fprintf(stdout,"Printplot  = %d\n",printplot);
      (void) fprintf(stdout,"Printer    = %s\n",printer);
      (void) fprintf(stdout,"PS Format  = %d\n",psformat);
      (void) fprintf(stdout,"PS File    = %s\n",psfile);
      (void) fprintf(stdout,"Scale      = %.3g\n",scale);
      (void) fprintf(stdout,"Font Scale = %.3g\n",fscale);
   }

   /* 
    * Now do the PostScript plot 
    */

   /* PostScript plot */
   if ((ips = fopen(psfile,"w")) == NULL) {
      (void) fprintf(stderr,"cat: can't open %s\n",psfile);
      (void) sprintf(error_message,"Error! Can't open %s\n",psfile);
      return;
   }
   (void) fprintf(stdout,"\n   Proceeding with PostScript Plot...");

   /* Set the page dimensions - include scaling factor         */
   /* The plot is always plotted around the center of the page */
   Pgxmin = 20.0;
   Pgxmax = ((!landscape) ? PX_DIM : PXL_DIM) - 20.0;
   Pgymin = 20.0;
   Pgymax = ((!landscape) ? PY_DIM : PYL_DIM) - 20.0;
   if (scale != 1.0) {
      dx     = 0.5*(Pgxmax - Pgxmin);
      xc     = 0.5*(Pgxmax + Pgxmin);
      Pgxmin = xc - scale*dx;
      Pgxmax = xc + scale*dx;
      dy     = 0.5*(Pgymax - Pgymin);
      yc     = 0.5*(Pgymax + Pgymin);
      Pgymin = yc - scale*dy;
      Pgymax = yc + scale*dy;
   }
   Master_Pgxmin = Pgxmin;
   Master_Pgxmax = Pgxmax;
   Master_Pgymin = Pgymin;
   Master_Pgymax = Pgymax;
#ifdef DEBUG
   (void) printf("Page dimensions:\n");
   (void) printf("  xmin=%7.2f  xmax=%7.2f\n",Pgxmin,Pgxmax);
   (void) printf("  ymin=%7.2f  ymax=%7.2f\n",Pgymin,Pgymax);
#endif

   /* EPS format */
   if (psformat == PX_EPSI || psformat == PX_EPSF) initEPSI();

   /* Calculate the orientation of the plots */
   calc_plot_orientation(nplots, &nx, &ny, force_vertical, debug);

   /* Draw the RAW Postscript plot */
   initPS();

   /* Calculate the size of each plot */
   dx = (Master_Pgxmax - Master_Pgxmin)/(double)(nx);
   dy = (Master_Pgymax - Master_Pgymin)/(double)(ny);

   /* 
    * Draw the plots one at a time 
    */
   if (force_vertical) {
      /* 
       * Start from the top and zig-zag downwards (top->down, left->right)
       *  | +-+
       *  | | |
       *  +-+ +
       */
      PS = pshead;
      for (i=0; i<nx && PS!=NULL; i++) {
         for (j=ny-1; j>=0 && PS!=NULL; j--) {

            /* Reset the plot dimensions */
            Pgxmin = Master_Pgxmin + (i  )*dx;
            Pgxmax = Master_Pgxmin + (i+1)*dx;
            Pgymin = Master_Pgymin + (j  )*dy;
            Pgymax = Master_Pgymin + (j+1)*dy;

            /* Draw one plot at a time */
            draw_singleplot(PS->Pptr, drawframe, debug);

            /* Next plotset...*/
            PS = PS->next;
         }
      }
   } else {
      /* 
       * Start from the top and zig-zag downwards (left->right, top->bottom)
       *  ----+
       *      |
       *  +---+
       *  |
       *  +----
       */
      PS = pshead;
      for (j=ny-1; j>=0 && PS!=NULL; j--) {
         for (i=0; i<nx && PS!=NULL; i++) {

            /* Reset the plot dimensions */
            Pgxmin = Master_Pgxmin + (i  )*dx;
            Pgxmax = Master_Pgxmin + (i+1)*dx;
            Pgymin = Master_Pgymin + (j  )*dy;
            Pgymax = Master_Pgymin + (j+1)*dy;

            /* Draw one plot at a time */
            draw_singleplot(PS->Pptr, drawframe, debug);

            /* Next plotset...*/
            PS = PS->next;
         }
      }
   }

   /* End the PostScript plot */
   endgrPS();

   /* Close the EPS */
   if (psformat == PX_EPSI || psformat == PX_EPSF) endgrEPSI();

   /* Get the file size */
   (void) fflush(ips);
   if (fstat(fileno(ips),&buf)==0) filesize = buf.st_size; 

   /* Close the file */
   (void) fclose(ips);

   (void) fprintf(stdout,"done\n");
   (void) fprintf(stdout,"   The file size is %d KBytes.\n",filesize/1000);

   /* Send the plot to the printer */
   print_plot(printCmd, copies, psfile, printer, printplot, filesize, debug);
}

/* 
 * Calculate the orientation of the plots 
 */
static void calc_plot_orientation(nplots, nx, ny, force_vertical, debug)
int nplots;
int *nx;
int *ny;
int force_vertical;
{
   /* 
    * Try to be intelligent about plotting... 
    *    nx = number of plots in x
    *    ny = number of plots in y
    *    total number of plot areas = nx*ny
    * Try to fill in these plot areas as best as possible
    * Also reset the scale factors
    * Set min scale factor to 3X                            
    */
   if (force_vertical) {
      /* Force the plots to be laid out horizontally (only 1 column) */
      if (nplots <= 3) {
         *nx    = 1;
         *ny    = nplots;
         scale  = scale/2.0;
         fscale = fscale/(double)(nplots);
      } else if (nplots <= 6) {
         *nx    = 1;
         *ny    = nplots;
         scale  = scale/3.0;
         fscale = fscale/3.0;
      } else if (nplots <= 12) {
         /* Subdivide into 2 columns */
         *nx    = 2;
         *ny    = 6;
         scale  = scale/3.0;
         fscale = fscale/3.0;
      } else {
         /* Subdivide into 3 columns */
         if (nplots > 18) {
            (void) fprintf(stderr,
                   "\nWarning! Can only put max of 18 plots on same page!\n");
         }
         *nx    = 3;
         *ny    = 6;
         scale  = scale/3.0;
         fscale = fscale/3.0;
      } 
   } else {
      /* Put the plots in a rectangular grid */
      if (nplots <= 3) {
         if (landscape) {
            *nx = nplots;
            *ny = 1;
         } else {
            *nx = 1;
            *ny = nplots;
         }
         scale  = scale/2.0;
         fscale = fscale/2.0;
      } else if (nplots <= 4) {
         *nx    = 2;
         *ny    = 2;
         scale  = scale/2.0;
         fscale = fscale/2.0;
      } else if (nplots <= 6) {
         if (landscape) {
            *nx  = 3;
            *ny  = 2;
         } else {
            *nx  = 2;
            *ny  = 3;
         }
         scale  = scale/3.0;
         fscale = fscale/3.0;
      } else if (nplots <= 9) {
         *nx    = 3;
         *ny    = 3;
         scale  = scale/3.0;
         fscale = fscale/3.0;
      } else if (nplots <= 12) {
         if (landscape) {
            *nx  = 4;
            *ny  = 3;
         } else {
            *nx  = 3;
            *ny  = 4;
         }
         scale  = scale/3.0;
         fscale = fscale/3.0;
      } else {
         if (nplots > 16) {
            (void) fprintf(stderr,
                   "\nWarning! Can only put max of 16 plots on same page!\n");
         }
         *nx    = 4;
         *ny    = 4;
         scale  = scale/3.0;
         fscale = fscale/3.0;
      } 
   }
   if (debug)
   (void) fprintf(stdout,"nplots=%d nx=%d ny=%d\n",nplots, *nx, *ny);
}

   
/*
 * Draw a single plot
 */
static void draw_singleplot(pptr, drawframe, debug)
CNplotsetptr pptr;
int drawframe;
int debug;
{
   /* Initialize the bounding box */
   initPS_boundingbox();

   /* draw the bounding box around the plot */
   if (drawframe) drawPS_boundingbox();

   /* Set the plotset */
   if (pptr==NULL) {
      (void) fprintf(stderr,
                     "PXplotps(): Error! Cannot plot NULL plotset!\n");
   } else {

      /* Print out the plotset ID */
      if (debug) {
      (void) fprintf(stdout,"Plotset ID = %d\n",pptr->ID);
      (void) fprintf(stdout,"Scale      = %.3g\n",scale);
      (void) fprintf(stdout,"Font Scale = %.3g\n",fscale);
      (void) fprintf(stdout,"Page dimensions:\n");
      (void) fprintf(stdout,"  xmin=%7.2f  xmax=%7.2f\n",Pgxmin,Pgxmax);
      (void) fprintf(stdout,"  ymin=%7.2f  ymax=%7.2f\n",Pgymin,Pgymax);
      }

      /* Draw the main part of the plot */
      if (pptr->plottype == CN_PLOT3D) {
         PXplotps3D(pptr);
      } else {
         PXplotps2D(pptr);
      }
   }
}

/* 
 * Send the plot to the printer 
 */
static void print_plot(printCmd, copies, psfile, printer, printplot, 
                       filesize, debug)
char *printCmd;
int   copies;
char *psfile;
char *printer;
int   printplot;
off_t filesize;
int   debug;
{
   char   command[CN_MAXCHAR];
   char   message[CN_MAXCHAR];
   int    status;

   if (strncmp(printCmd,"pm",2)==0) {
      /* "pm" printing utility used in Intel Portland */
      (void) sprintf(command,"%s %s %s 91",printCmd,psfile,printer);
   } else if (strcmp(printCmd,"lp")==0) {
      /* Must be the HP "lp" command */
      (void) sprintf(command,"%s -n%d -d%s %s",printCmd,copies,printer,psfile);
   } else if (strncmp(printCmd,"lpr",3)==0) {
      /* "The normal "lpr" command */
      (void) sprintf(command,"%s -#%d -P%s %s",printCmd,copies,printer,psfile);
   } else {
      /* Unknown command */
      (void) sprintf(command,"%s %s",printCmd,psfile);
   }
   if (printplot) {
      if (filesize > 1000000) {
         (void) sprintf(message,
         "Error! The file size (%5.2f MBytes) is greater than 1MByte!\n\
          Cannot send this file to the printer due to 1MB lpr file-size\n\
          limit!  Check with your system administrator for information\n\
          on printing large POSTSCRIPT files.\n\
          \n\
          The PostScript plot has been saved in \"%s\"\n",
          1.0e-6*filesize,psfile);
          (void) fprintf(stderr,message);
          /*
          PX_X11PrintError(message);
           */
       } else {
          status = system(command);
          if (status != 0) {
             (void) fprintf(stdout,
                    "   Error! Couldn't print the PS file \"%s\"!\n", psfile);
             (void) fprintf(stdout,
                    "system status = %d\n",status);
          } else {
             (void) fprintf(stdout,
                    "   The PostScript file \"%s\" has been ",psfile);
             (void) fprintf(stdout,
                    "sent to the \"%s\" printer\n",printer);
          }
          if (debug) 
          (void)fprintf(stdout,"   The print command is: \"%s\"\n",command); 
      }
   } else {
      (void)fprintf(stdout,"   The PostScript plot has been saved in \"%s\"\n",
      psfile);
   }
}



/* Output the EPSI header */
static void initEPSI()
{
#define PSBEGIN "%!"
#define PSMID   "%%"
   int i;

   (void) fprintf(stdout,
          "\n   Writing out PostScript data in EPSI 2.0 format...");

   /* EPSI Header */
   (void) fprintf(ips,"%sPS-Adobe-2.0 EPSF-2.0\n",PSBEGIN);
   (void) fprintf(ips,"%sBoundingBox: %g %g %g %g\n",PSMID,
                  Pgxmin,Pgymin,Pgxmax,Pgymax);
   (void) fprintf(ips,"%sPages: 0\n",PSMID);
   (void) fprintf(ips,"%sCreator: Kenny Toh\n",PSMID);
   (void) fprintf(ips,"%sCreationDate: October 1, 1991\n",PSMID);
   (void) fprintf(ips,"%sEndComments\n",PSMID);
   (void) fprintf(ips,"%sBeginPreview: %d %d 1 %d\n",PSMID,
          4*(strlen(eps_words[0])-1),eps_lines,eps_lines);
   for (i=0; i<eps_lines; i++)
      (void) fprintf(ips,"%s\n",eps_words[i]);
   (void) fprintf(ips,"%sEndPreview\n",PSMID);
   (void) fprintf(ips,"%sEndProlog\n",PSMID);
   (void) fprintf(ips,"%sPage: \"one\" 1\n",PSMID);
}


/* Output PostScript Subroutines */
static void initPS()
{
   extern double scale;
   char   time[CN_MAXCHAR];
   static double ps_topfont   = (double) PS_TOPFONT;
   static double ps_sidefont  = (double) PS_SIDEFONT;
   static double ps_axisfont  = (double) PS_AXISFONT;
   static double ps_mkrsfont  = (double) PS_MKRSFONT;
   static double ps_annotfont = (double) PS_ANNOTFONT;
   static double ps_datefont  = (double) PS_DATEFONT;
   static double ps_ctrfont   = (double) PS_CTRFONT;
   
   char   *getenv();
   char   *ptr;

   /* Default is to use the isolatin font */
   useISOLatinFont=CN_TRUE;

   /* Global Font Selection */
   if ((ptr = getenv("MTV_PSFONT")) !=NULL) {
     CNstring_to_lower(ptr);
     if(strcmp(ptr,"helvetica")==0)
       usePSfontType=1;
     else if(strcmp(ptr,"courier")==0)
       usePSfontType=2;
     else
       usePSfontType=0;
   }
   else
     usePSfontType=0;
   
   if ((ptr = getenv("MTV_PSITALIC")) !=NULL) {
     CNstring_to_lower(ptr);
     if ((strcmp(ptr,"on")==0) || (strcmp(ptr,"true")==0))
       usePSfontType += 3;
   }
   
   (void) fprintf(stdout,"\n   PostScript output fonts : ");
   if (usePSfontType==0) 
     (void) fprintf(stdout,"Time-Roman, Times-Bold");
   else if (usePSfontType==1)
     (void) fprintf(stdout,"Helvetica, Helvetica-Bold");
   else if (usePSfontType==2)
     (void) fprintf(stdout,"Courier , Courier-Bold");
   else if (usePSfontType==3)
     (void) fprintf(stdout,"Time-Italic, Times-BoldItalic");
   else if (usePSfontType==4)
     (void) fprintf(stdout,"Helvetica-Oblique, Helvetica-BoldOblique");
   else if (usePSfontType==5)
     (void) fprintf(stdout,"Courier-Oblique, Courier-BoldOblique");

   (void) fprintf(stdout,"..."); 
     
   /* Read Options */
   readPS_options(&useISOLatinFont,
                  &ps_topfont,
                  &ps_sidefont,
                  &ps_axisfont,
                  &ps_mkrsfont,
                  &ps_annotfont,
                  &ps_datefont,
                  &ps_ctrfont);

   /* Save the annotation fontsize */
   annot_fontsize = ps_annotfont;

   /* begin PostScript output */
   (void) fprintf(ips,"%c!\n",'%');

   if (landscape) (void) fprintf(ips,"-90 rotate -790 0 translate\n");

/* Font definitions */
(void) fprintf(ips,"%%------------------------------------------------\n");
(void) fprintf(ips,"%%--------------Font Definitions------------------\n");
(void) fprintf(ips,"%%------------------------------------------------\n");
(void) fprintf(ips,"\n");

/* Procedure for finding font height */
(void) fprintf(ips,"/findheight                 %% Find the font height\n");
(void) fprintf(ips,"{ gsave\n");
(void) fprintf(ips,"  newpath\n");
(void) fprintf(ips,"  0 0 moveto\n");
(void) fprintf(ips,"  (X) true charpath\n");
(void) fprintf(ips,"  flattenpath\n");
(void) fprintf(ips,"  pathbbox /capheight exch def pop pop pop\n");
(void) fprintf(ips,"  grestore\n");
(void) fprintf(ips,"} def\n");
(void) fprintf(ips,"\n");

/* Don't use ISOLatin font for PostScript Level 1 machines */
if (useISOLatinFont) {

  (void) fprintf(ips,"%% Load ISOLatin font for ");
  if (usePSfontType==0) 
    (void) fprintf(ips,"Times-Roman\n/Times-Roman");
  else if (usePSfontType==1)
    (void) fprintf(ips,"Helvetica\n/Helvetica");
  else if (usePSfontType==2)
    (void) fprintf(ips,"Courier\n/Courier");
  else if (usePSfontType==3) 
    (void) fprintf(ips,"Times-Italic\n/Times-Italic");
  else if (usePSfontType==4)
    (void) fprintf(ips,"Helvetica-Oblique\n/Helvetica-Oblique");
  else if (usePSfontType==5) 
    (void) fprintf(ips,"Courier-Oblique\n/Courier-Oblique");
  (void) fprintf(ips," findfont\n");
  (void) fprintf(ips,"dup length dict begin\n");
  (void) fprintf(ips,"  {1 index /FID ne {def} {pop pop} ifelse} forall\n");
  (void) fprintf(ips,"  /Encoding ISOLatin1Encoding def\n");
  (void) fprintf(ips,"  currentdict\n");
  (void) fprintf(ips,"end\n");  
  if (usePSfontType==0) 
    (void) fprintf(ips,"/Times-Roman");
  else if (usePSfontType==1)
    (void) fprintf(ips,"/Helvetica");
  else if (usePSfontType==2)
    (void) fprintf(ips,"/Courier");
  else if (usePSfontType==3) 
    (void) fprintf(ips,"/Times-Italic");
  else if (usePSfontType==4)
    (void) fprintf(ips,"/Helvetica-Oblique");
  else if (usePSfontType==5)
    (void) fprintf(ips,"/Courier-Oblique");
  (void) fprintf(ips,"-ISOLatin1 exch definefont pop\n");
  (void) fprintf(ips,"\n");

  (void) fprintf(ips,"%% Load ISOLatin font for ");
  if (usePSfontType==0) 
    (void) fprintf(ips,"Times-Bold\n/Times-Bold");
  else if (usePSfontType==1)
    (void) fprintf(ips,"Helvetica-Bold\n/Helvetica-Bold");
  else if (usePSfontType==2)
    (void) fprintf(ips,"Courier-Bold\n/Courier-Bold");
  else if (usePSfontType==3) 
    (void) fprintf(ips,"Times-BoldItalic\n/Times-BoldItalic");
  else if (usePSfontType==4)
    (void) fprintf(ips,"Helvetica-BoldOblique\n/Helvetica-BoldOblique");
  else if (usePSfontType==5) 
    (void) fprintf(ips,"Courier-BoldOblique\n/Courier-BoldOblique");
  (void) fprintf(ips," findfont\n");
  (void) fprintf(ips,"dup length dict begin\n");
  (void) fprintf(ips,"  {1 index /FID ne {def} {pop pop} ifelse} forall\n");
  (void) fprintf(ips,"  /Encoding ISOLatin1Encoding def\n");
  (void) fprintf(ips,"  currentdict\n");
  (void) fprintf(ips,"end\n");
  if (usePSfontType==0) 
    (void) fprintf(ips,"/Times-Bold");
  else if (usePSfontType==1)
    (void) fprintf(ips,"/Helvetica-Bold");
  else if (usePSfontType==2)
    (void) fprintf(ips,"/Courier-Bold");
  else if (usePSfontType==3) 
    (void) fprintf(ips,"/Times-BoldItalic");
  else if (usePSfontType==4)
    (void) fprintf(ips,"/Helvetica-BoldOblique");
  else if (usePSfontType==5)
    (void) fprintf(ips,"/Courier-BoldOblique");
  (void) fprintf(ips,"-ISOLatin1 exch definefont pop\n");
  (void) fprintf(ips,"\n");
} 

(void) fprintf(ips,"%% Routine for doing exponents\n");
(void) fprintf(ips,"/exponentdict 4 dict def\n");
(void) fprintf(ips,"/exponentshow\n");
(void) fprintf(ips,"{ exponentdict begin\n");
(void) fprintf(ips,"  %% Get the argument - a string to be exponentiated\n");
(void) fprintf(ips,"  /exponent exch def\n");
(void) fprintf(ips,"  /regularfont currentfont def\n");
(void) fprintf(ips,"  /exponentfont currentfont ");
(void) fprintf(ips,"[0.8 0 0 0.7 0 0] makefont def\n");
(void) fprintf(ips,"\n");
(void) fprintf(ips,"  %% Find the height of the numeral 1\n");
(void) fprintf(ips,"  gsave\n");
(void) fprintf(ips,"     newpath\n");
(void) fprintf(ips,"     0 0 moveto\n");
(void) fprintf(ips,"     (X) true charpath\n");
(void) fprintf(ips,"     flattenpath\n");
(void) fprintf(ips,"     pathbbox /height exch def pop pop pop\n");
(void) fprintf(ips,"  grestore\n");
(void) fprintf(ips,"\n");
(void) fprintf(ips,"  %% Print a multiply (x) followed by the exponent\n");

/* Don't use ISOLatin font for PostScript Level 1 machines */
if (useISOLatinFont) {
(void) fprintf(ips,"  (\32710) show\n");
} else {
(void) fprintf(ips,"  (x10) show\n");
}

(void) fprintf(ips,"  0 0.6 height mul rmoveto     %% Move up by 40-percent\n");
(void) fprintf(ips,"  exponentfont setfont exponent show  %% Print string\n");
(void) fprintf(ips,"  0 0.6 height mul neg rmoveto %% Move back to baseline\n");
(void) fprintf(ips,"  regularfont setfont        %% return to original font\n");
(void) fprintf(ips,"\n");
(void) fprintf(ips,"  end\n");
(void) fprintf(ips,"} def\n");
(void) fprintf(ips,"\n");

(void) fprintf(ips,"%% Define font sizes\n");
(void) fprintf(ips,"/defFontSizeTopLbl   %.2f def\n",ps_topfont*fscale);
(void) fprintf(ips,"/defFontSizeSideLbl  %.2f def\n",ps_sidefont*fscale);
(void) fprintf(ips,"/defFontSizeAxisLbl  %.2f def\n",ps_axisfont*fscale);
(void) fprintf(ips,"/defFontSizeMkrsLbl  %.2f def\n",ps_mkrsfont*fscale);
(void) fprintf(ips,"/defFontSizeAnnotLbl %.2f def\n",ps_annotfont*fscale);
(void) fprintf(ips,"/defFontSizeDateLbl  %.2f def\n",ps_datefont*fscale);
(void) fprintf(ips,"/defFontSizeCtrLbl   %.2f def\n",ps_ctrfont*fscale);
(void) fprintf(ips,"\n");

(void) fprintf(ips,"%% Define font locations/offsets\n");
(void) fprintf(ips,"/UPJ   {} def\n");
(void) fprintf(ips,"/MIJT  {0 defFontSizeTopLbl   -2 div rmoveto} def\n");
(void) fprintf(ips,"/BOJT  {0 defFontSizeTopLbl   neg rmoveto} def\n");
(void) fprintf(ips,"/MIJS  {0 defFontSizeSideLbl  -2 div rmoveto} def\n");
(void) fprintf(ips,"/BOJS  {0 defFontSizeSideLbl  neg rmoveto} def\n");
(void) fprintf(ips,"/MIJA  {0 defFontSizeAxisLbl  -2 div rmoveto} def\n");
(void) fprintf(ips,"/BOJA  {0 defFontSizeAxisLbl  neg rmoveto} def\n");
(void) fprintf(ips,"/MIJM  {0 defFontSizeMkrsLbl  -2 div rmoveto} def\n");
(void) fprintf(ips,"/BOJM  {0 defFontSizeMkrsLbl  neg rmoveto} def\n");
(void) fprintf(ips,"/MIJN  {0 defFontSizeAnnotLbl -2 div rmoveto} def\n");
(void) fprintf(ips,"/BOJN  {0 defFontSizeAnnotLbl neg rmoveto} def\n");
(void) fprintf(ips,"/MIJD  {0 defFontSizeDateLbl  -2 div rmoveto} def\n");
(void) fprintf(ips,"/BOJD  {0 defFontSizeDateLbl  neg rmoveto} def\n");
(void) fprintf(ips,"/MIJC  {0 defFontSizeCtrLbl   -2 div rmoveto} def\n");
(void) fprintf(ips,"/BOJC  {0 defFontSizeCtrLbl   neg rmoveto} def\n");
(void) fprintf(ips,"/LEJ   {} def\n");
(void) fprintf(ips,"/CEJ   {dup stringwidth pop -2 div 0 rmoveto} def\n");
(void) fprintf(ips,"/RIJ   {dup stringwidth pop neg 0 rmoveto} def\n");
(void) fprintf(ips,"\n");

/* Don't use ISOLatin font for PostScript Level 1 machines */
if (useISOLatinFont) {
  /* Use ISOLatin fonts */
  (void) fprintf(ips,"%% Define fonts\n");
  if (usePSfontType==0) {
    (void) fprintf(ips,"/TopLblFont   /Times-Bold-ISOLatin1  ");
    (void) fprintf(ips,"findfont defFontSizeTopLbl   scalefont def\n");
    (void) fprintf(ips,"/SideLblFont  /Times-Bold-ISOLatin1  ");
    (void) fprintf(ips,"findfont defFontSizeSideLbl  scalefont def\n");
    (void) fprintf(ips,"/AxisLblFont  /Times-Roman-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeAxisLbl  scalefont def\n");
    (void) fprintf(ips,"/MkrsLblFont  /Times-Roman-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeMkrsLbl  scalefont def\n");
    (void) fprintf(ips,"/AnnotLblFont /Times-Roman-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeAnnotLbl scalefont def\n");
    (void) fprintf(ips,"/DateLblFont  /Times-Bold-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeDateLbl  scalefont def\n");
    (void) fprintf(ips,"/CtrLblFont   /Times-Roman-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeCtrLbl   scalefont def\n");}

  else if(usePSfontType==1) {
    (void) fprintf(ips,"/TopLblFont   /Helvetica-Bold-ISOLatin1  ");
    (void) fprintf(ips,"findfont defFontSizeTopLbl   scalefont def\n");
    (void) fprintf(ips,"/SideLblFont  /Helvetica-Bold-ISOLatin1  ");
    (void) fprintf(ips,"findfont defFontSizeSideLbl  scalefont def\n");
    (void) fprintf(ips,"/AxisLblFont  /Helvetica-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeAxisLbl  scalefont def\n");
    (void) fprintf(ips,"/MkrsLblFont  /Helvetica-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeMkrsLbl  scalefont def\n");
    (void) fprintf(ips,"/AnnotLblFont /Helvetica-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeAnnotLbl scalefont def\n");
    (void) fprintf(ips,"/DateLblFont  /Helvetica-Bold-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeDateLbl  scalefont def\n");
    (void) fprintf(ips,"/CtrLblFont   /Helvetica-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeCtrLbl   scalefont def\n");}

  else if(usePSfontType==2) {
    (void) fprintf(ips,"/TopLblFont   /Courier-Bold-ISOLatin1  ");
    (void) fprintf(ips,"findfont defFontSizeTopLbl   scalefont def\n");
    (void) fprintf(ips,"/SideLblFont  /Courier-Bold-ISOLatin1  ");
    (void) fprintf(ips,"findfont defFontSizeSideLbl  scalefont def\n");
    (void) fprintf(ips,"/AxisLblFont  /Courier-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeAxisLbl  scalefont def\n");
    (void) fprintf(ips,"/MkrsLblFont  /Courier-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeMkrsLbl  scalefont def\n");
    (void) fprintf(ips,"/AnnotLblFont /Courier-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeAnnotLbl scalefont def\n");
    (void) fprintf(ips,"/DateLblFont  /Courier-Bold-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeDateLbl  scalefont def\n");
    (void) fprintf(ips,"/CtrLblFont   /Courier-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeCtrLbl   scalefont def\n");}
  
  else if (usePSfontType==3) {
    (void) fprintf(ips,"/TopLblFont   /Times-BoldItalic-ISOLatin1  ");
    (void) fprintf(ips,"findfont defFontSizeTopLbl   scalefont def\n");
    (void) fprintf(ips,"/SideLblFont  /Times-BoldItalic-ISOLatin1  ");
    (void) fprintf(ips,"findfont defFontSizeSideLbl  scalefont def\n");
    (void) fprintf(ips,"/AxisLblFont  /Times-Italic-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeAxisLbl  scalefont def\n");
    (void) fprintf(ips,"/MkrsLblFont  /Times-Italic-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeMkrsLbl  scalefont def\n");
    (void) fprintf(ips,"/AnnotLblFont /Times-Italic-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeAnnotLbl scalefont def\n");
    (void) fprintf(ips,"/DateLblFont  /Times-BoldItalic-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeDateLbl  scalefont def\n");
    (void) fprintf(ips,"/CtrLblFont   /Times-Italic-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeCtrLbl   scalefont def\n");}

  else if(usePSfontType==4) {
    (void) fprintf(ips,"/TopLblFont   /Helvetica-BoldOblique-ISOLatin1  ");
    (void) fprintf(ips,"findfont defFontSizeTopLbl   scalefont def\n");
    (void) fprintf(ips,"/SideLblFont  /Helvetica-BoldOblique-ISOLatin1  ");
    (void) fprintf(ips,"findfont defFontSizeSideLbl  scalefont def\n");
    (void) fprintf(ips,"/AxisLblFont  /Helvetica-Oblique-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeAxisLbl  scalefont def\n");
    (void) fprintf(ips,"/MkrsLblFont  /Helvetica-Oblique-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeMkrsLbl  scalefont def\n");
    (void) fprintf(ips,"/AnnotLblFont /Helvetica-Oblique-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeAnnotLbl scalefont def\n");
    (void) fprintf(ips,"/DateLblFont  /Helvetica-BoldOblique-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeDateLbl  scalefont def\n");
    (void) fprintf(ips,"/CtrLblFont   /Helvetica-Oblique-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeCtrLbl   scalefont def\n");}

  else if(usePSfontType==5) {
    (void) fprintf(ips,"/TopLblFont   /Courier-BoldOblique-ISOLatin1  ");
    (void) fprintf(ips,"findfont defFontSizeTopLbl   scalefont def\n");
    (void) fprintf(ips,"/SideLblFont  /Courier-BoldOblique-ISOLatin1  ");
    (void) fprintf(ips,"findfont defFontSizeSideLbl  scalefont def\n");
    (void) fprintf(ips,"/AxisLblFont  /Courier-Oblique-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeAxisLbl  scalefont def\n");
    (void) fprintf(ips,"/MkrsLblFont  /Courier-Oblique-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeMkrsLbl  scalefont def\n");
    (void) fprintf(ips,"/AnnotLblFont /Courier-Oblique-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeAnnotLbl scalefont def\n");
    (void) fprintf(ips,"/DateLblFont  /Courier-BoldOblique-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeDateLbl  scalefont def\n");
    (void) fprintf(ips,"/CtrLblFont   /Courier-Oblique-ISOLatin1 ");
    (void) fprintf(ips,"findfont defFontSizeCtrLbl   scalefont def\n");}
  (void) fprintf(ips,"\n"); }
else {
  /* Just use the normal fonts */
  (void) fprintf(ips,"%% Define fonts\n");
  if (usePSfontType==0) {
    (void) fprintf(ips,"/TopLblFont   /Times-Bold  ");
    (void) fprintf(ips,"findfont defFontSizeTopLbl   scalefont def\n");
    (void) fprintf(ips,"/SideLblFont  /Times-Bold  ");
    (void) fprintf(ips,"findfont defFontSizeSideLbl  scalefont def\n");
    (void) fprintf(ips,"/AxisLblFont  /Times-Roman ");
    (void) fprintf(ips,"findfont defFontSizeAxisLbl  scalefont def\n");
    (void) fprintf(ips,"/MkrsLblFont  /Times-Roman ");
    (void) fprintf(ips,"findfont defFontSizeMkrsLbl  scalefont def\n");
    (void) fprintf(ips,"/AnnotLblFont /Times-Roman ");
    (void) fprintf(ips,"findfont defFontSizeAnnotLbl scalefont def\n");
    (void) fprintf(ips,"/DateLblFont  /Times-Bold  ");
    (void) fprintf(ips,"findfont defFontSizeDateLbl  scalefont def\n");
    (void) fprintf(ips,"/CtrLblFont   /Times-Roman ");
    (void) fprintf(ips,"findfont defFontSizeCtrLbl   scalefont def\n");}
  
  else if (usePSfontType==1) {
    (void) fprintf(ips,"/TopLblFont   /Helvetica-Bold  ");
    (void) fprintf(ips,"findfont defFontSizeTopLbl   scalefont def\n");
    (void) fprintf(ips,"/SideLblFont  /Helvetica-Bold  ");
    (void) fprintf(ips,"findfont defFontSizeSideLbl  scalefont def\n");
    (void) fprintf(ips,"/AxisLblFont  /Helvetica ");
    (void) fprintf(ips,"findfont defFontSizeAxisLbl  scalefont def\n");
    (void) fprintf(ips,"/MkrsLblFont  /Helvetica ");
    (void) fprintf(ips,"findfont defFontSizeMkrsLbl  scalefont def\n");
    (void) fprintf(ips,"/AnnotLblFont /Helvetica ");
    (void) fprintf(ips,"findfont defFontSizeAnnotLbl scalefont def\n");
    (void) fprintf(ips,"/DateLblFont  /Helvetica-Bold  ");
    (void) fprintf(ips,"findfont defFontSizeDateLbl  scalefont def\n");
    (void) fprintf(ips,"/CtrLblFont   /Helvetica ");
    (void) fprintf(ips,"findfont defFontSizeCtrLbl   scalefont def\n");}

  else if (usePSfontType==2) {
    (void) fprintf(ips,"/TopLblFont   /Courier-Bold  ");
    (void) fprintf(ips,"findfont defFontSizeTopLbl   scalefont def\n");
    (void) fprintf(ips,"/SideLblFont  /Courier-Bold  ");
    (void) fprintf(ips,"findfont defFontSizeSideLbl  scalefont def\n");
    (void) fprintf(ips,"/AxisLblFont  /Courier ");
    (void) fprintf(ips,"findfont defFontSizeAxisLbl  scalefont def\n");
    (void) fprintf(ips,"/MkrsLblFont  /Courier ");
    (void) fprintf(ips,"findfont defFontSizeMkrsLbl  scalefont def\n");
    (void) fprintf(ips,"/AnnotLblFont /Courier ");
    (void) fprintf(ips,"findfont defFontSizeAnnotLbl scalefont def\n");
    (void) fprintf(ips,"/DateLblFont  /Courier-Bold  ");
    (void) fprintf(ips,"findfont defFontSizeDateLbl  scalefont def\n");
    (void) fprintf(ips,"/CtrLblFont   /Courier ");
    (void) fprintf(ips,"findfont defFontSizeCtrLbl   scalefont def\n");}

  if (usePSfontType==3) {
    (void) fprintf(ips,"/TopLblFont   /Times-BoldItalic  ");
    (void) fprintf(ips,"findfont defFontSizeTopLbl   scalefont def\n");
    (void) fprintf(ips,"/SideLblFont  /Times-BoldItalic  ");
    (void) fprintf(ips,"findfont defFontSizeSideLbl  scalefont def\n");
    (void) fprintf(ips,"/AxisLblFont  /Times-Italic ");
    (void) fprintf(ips,"findfont defFontSizeAxisLbl  scalefont def\n");
    (void) fprintf(ips,"/MkrsLblFont  /Times-Italic ");
    (void) fprintf(ips,"findfont defFontSizeMkrsLbl  scalefont def\n");
    (void) fprintf(ips,"/AnnotLblFont /Times-Italic ");
    (void) fprintf(ips,"findfont defFontSizeAnnotLbl scalefont def\n");
    (void) fprintf(ips,"/DateLblFont  /Times-BoldItalic  ");
    (void) fprintf(ips,"findfont defFontSizeDateLbl  scalefont def\n");
    (void) fprintf(ips,"/CtrLblFont   /Times-Italic ");
    (void) fprintf(ips,"findfont defFontSizeCtrLbl   scalefont def\n");}
  
  else if (usePSfontType==4) {
    (void) fprintf(ips,"/TopLblFont   /Helvetica-BoldOblique  ");
    (void) fprintf(ips,"findfont defFontSizeTopLbl   scalefont def\n");
    (void) fprintf(ips,"/SideLblFont  /Helvetica-BoldOblique  ");
    (void) fprintf(ips,"findfont defFontSizeSideLbl  scalefont def\n");
    (void) fprintf(ips,"/AxisLblFont  /Helvetica-Oblique ");
    (void) fprintf(ips,"findfont defFontSizeAxisLbl  scalefont def\n");
    (void) fprintf(ips,"/MkrsLblFont  /Helvetica-Oblique ");
    (void) fprintf(ips,"findfont defFontSizeMkrsLbl  scalefont def\n");
    (void) fprintf(ips,"/AnnotLblFont /Helvetica-Oblique ");
    (void) fprintf(ips,"findfont defFontSizeAnnotLbl scalefont def\n");
    (void) fprintf(ips,"/DateLblFont  /Helvetica-BoldOblique  ");
    (void) fprintf(ips,"findfont defFontSizeDateLbl  scalefont def\n");
    (void) fprintf(ips,"/CtrLblFont   /Helvetica-Oblique ");
    (void) fprintf(ips,"findfont defFontSizeCtrLbl   scalefont def\n");}

  else if (usePSfontType==5) {
    (void) fprintf(ips,"/TopLblFont   /Courier-BoldOblique  ");
    (void) fprintf(ips,"findfont defFontSizeTopLbl   scalefont def\n");
    (void) fprintf(ips,"/SideLblFont  /Courier-BoldOblique  ");
    (void) fprintf(ips,"findfont defFontSizeSideLbl  scalefont def\n");
    (void) fprintf(ips,"/AxisLblFont  /Courier-Oblique ");
    (void) fprintf(ips,"findfont defFontSizeAxisLbl  scalefont def\n");
    (void) fprintf(ips,"/MkrsLblFont  /Courier-Oblique ");
    (void) fprintf(ips,"findfont defFontSizeMkrsLbl  scalefont def\n");
    (void) fprintf(ips,"/AnnotLblFont /Courier-Oblique ");
    (void) fprintf(ips,"findfont defFontSizeAnnotLbl scalefont def\n");
    (void) fprintf(ips,"/DateLblFont  /Courier-BoldOblique  ");
    (void) fprintf(ips,"findfont defFontSizeDateLbl  scalefont def\n");
    (void) fprintf(ips,"/CtrLblFont   /Courier-Oblique ");
    (void) fprintf(ips,"findfont defFontSizeCtrLbl   scalefont def\n");}

  (void) fprintf(ips,"\n");
}

(void) fprintf(ips,"%% Define font definition macros\n");
(void) fprintf(ips,"/setTopLblFont  { TopLblFont  setfont findheight } def\n");
(void) fprintf(ips,"/setSideLblFont { SideLblFont setfont findheight } def\n");
(void) fprintf(ips,"/setAxisLblFont { AxisLblFont setfont findheight } def\n");
(void) fprintf(ips,"/setMkrsLblFont { MkrsLblFont setfont findheight } def\n");
(void) fprintf(ips,"/setAnnotLblFont{ AnnotLblFont setfont findheight } def\n");
(void) fprintf(ips,"/setDateLblFont { DateLblFont setfont findheight } def\n");
(void) fprintf(ips,"/setCtrLblFont  { CtrLblFont  setfont findheight } def\n");
(void) fprintf(ips,"\n");

/* Useful procedures */
(void) fprintf(ips,"%%------------------------------------------------\n");
(void) fprintf(ips,"%%--------------Label Procedures------------------\n");
(void) fprintf(ips,"%%------------------------------------------------\n");
(void) fprintf(ips,"\n");

(void) fprintf(ips,"/drawtria                  %% stack : x0,y0,x1,y1,x2,y2\n");
(void) fprintf(ips,"{/p3y exch def\n");
(void) fprintf(ips," /p3x exch def\n");
(void) fprintf(ips," /p2y exch def\n");
(void) fprintf(ips," /p2x exch def\n");
(void) fprintf(ips," newpath moveto\n");
(void) fprintf(ips," p2x p2y lineto\n");
(void) fprintf(ips," p3x p3y lineto\n");
(void) fprintf(ips," closepath\n");
(void) fprintf(ips,"} def\n");
(void) fprintf(ips,"\n");

(void) fprintf(ips,"/filltria                  %% stack : x0,y0,x1,y1,x2,y2\n");
(void) fprintf(ips,"{drawtria fill} def\n");
(void) fprintf(ips,"\n");

(void) fprintf(ips,"/drawrect                  %% stack : x0,y0,xl,yl\n");
(void) fprintf(ips,"{/ywidth exch def\n");
(void) fprintf(ips," /xwidth exch def\n");
(void) fprintf(ips," newpath moveto\n");
(void) fprintf(ips," 0 ywidth rlineto\n");
(void) fprintf(ips," xwidth 0 rlineto\n");
(void) fprintf(ips," 0 ywidth neg rlineto\n");
(void) fprintf(ips," closepath\n");
(void) fprintf(ips,"} def\n");
(void) fprintf(ips,"\n");

(void) fprintf(ips,"/fillrect                  %% stack : x0,y0,xl,yl\n");
(void) fprintf(ips,"{drawrect fill} def\n");
(void) fprintf(ips,"\n");

(void) fprintf(ips,"/drawbox                    %% stack : xl, yl\n");
(void) fprintf(ips,"{dup 0 exch rlineto        %% vertical line, up\n");
(void) fprintf(ips," exch dup 0 rlineto        %% horizontal line, right\n");
(void) fprintf(ips," exch neg 0 exch rlineto   %% vertical line, down\n");
(void) fprintf(ips," neg 0 rlineto             %% horizontal line, left\n");
(void) fprintf(ips," 1.0 setgray fill 0.0 setgray\n");
(void) fprintf(ips,"} def\n");
(void) fprintf(ips,"\n");

(void) fprintf(ips,"/drawline                  %% stack : x0,y0,xl,yl\n");
(void) fprintf(ips,"{/yl exch def\n");
(void) fprintf(ips," /xl exch def\n");
(void) fprintf(ips," newpath moveto\n");
(void) fprintf(ips," xl yl rlineto stroke\n");
(void) fprintf(ips,"} def\n");
(void) fprintf(ips,"\n");

(void) fprintf(ips,"/leftshow                   %% left-justfd boxed string\n");
(void) fprintf(ips,"{gsave\n");
(void) fprintf(ips," dup stringwidth pop capheight  %% Stack font dimensn\n");
(void) fprintf(ips," drawbox                   %% Draw the box\n");
(void) fprintf(ips," grestore show\n");
(void) fprintf(ips,"} def\n");
(void) fprintf(ips,"\n");

(void) fprintf(ips,"/centershow                 %% Show ctred boxed string\n");
(void) fprintf(ips,"{dup stringwidth pop capheight  %% Stack font dimensn\n");
(void) fprintf(ips," -2 div 0 exch rmoveto     %% Move to the left\n");
(void) fprintf(ips," -2 div 0 rmoveto          %% Move down\n");
(void) fprintf(ips," leftshow\n");
(void) fprintf(ips,"} def\n");
(void) fprintf(ips,"\n");

(void) fprintf(ips,"/plotlabel                  %% Draw a centered label\n");
(void) fprintf(ips,"{newpath 0 0 moveto centershow } def\n");
(void) fprintf(ips,"\n");

(void) fprintf(ips,"%%------------------------------------------------\n");
(void) fprintf(ips,"%%--------------Marker Definitions----------------\n");
(void) fprintf(ips,"%%------------------------------------------------\n");
(void) fprintf(ips,"\n");

(void) fprintf(ips,"/wfill          %% white fill\n");
(void) fprintf(ips,"{gsave 1.0 setgray fill grestore stroke} def\n");
(void) fprintf(ips,"\n");

(void) fprintf(ips,"/markdot        %%dot\n");
(void) fprintf(ips,"{%.2f %.2f rmoveto\n",-0.5,0.0);
(void) fprintf(ips," %.2f %.2f rlineto\n", 1.0, 0.0);
(void) fprintf(ips," %.2f %.2f rmoveto\n",-0.5,-0.5);
(void) fprintf(ips," %.2f %.2f rlineto\n", 0.0, 1.0);
(void) fprintf(ips,"stroke } def\n");
(void) fprintf(ips,"\n");

(void) fprintf(ips,"%%------------------------------------------------\n");
(void) fprintf(ips,"%%--------------Pattern Definitions---------------\n");
(void) fprintf(ips,"%%------------------------------------------------\n");
(void) fprintf(ips,"\n");

(void) fprintf(ips,"/verticals1         %% stack : width, height\n");
(void) fprintf(ips,"{/height exch def\n");
(void) fprintf(ips," /width  exch def\n");
(void) fprintf(ips," newpath\n");
(void) fprintf(ips," 0 10 width\n");
(void) fprintf(ips," { 0 moveto 0 height rlineto } for\n");
(void) fprintf(ips," stroke\n");
(void) fprintf(ips,"} def\n");
(void) fprintf(ips,"\n");

(void) fprintf(ips,"/verticals2         %% stack : width, height\n");
(void) fprintf(ips,"{/height exch def\n");
(void) fprintf(ips," /width  exch def\n");
(void) fprintf(ips," newpath\n");
(void) fprintf(ips," 0 3 width\n");
(void) fprintf(ips," { 0 moveto 0 height rlineto } for\n");
(void) fprintf(ips," stroke\n");
(void) fprintf(ips,"} def\n");
(void) fprintf(ips,"\n");

(void) fprintf(ips,"/horizontals1       %% stack : width, height\n");
(void) fprintf(ips,"{/height exch def\n");
(void) fprintf(ips," /width  exch def\n");
(void) fprintf(ips," newpath\n");
(void) fprintf(ips," 0 10 height\n");
(void) fprintf(ips," { 0 exch moveto width 0 rlineto } for\n");
(void) fprintf(ips," stroke\n");
(void) fprintf(ips,"} def\n");
(void) fprintf(ips,"\n");

(void) fprintf(ips,"/horizontals2       %% stack : width, height\n");
(void) fprintf(ips,"{/height exch def\n");
(void) fprintf(ips," /width  exch def\n");
(void) fprintf(ips," newpath\n");
(void) fprintf(ips," 0 3  height\n");
(void) fprintf(ips," { 0 exch moveto width 0 rlineto } for\n");
(void) fprintf(ips," stroke\n");
(void) fprintf(ips,"} def\n");
(void) fprintf(ips,"\n");

(void) fprintf(ips,"/diagonals1         %% stack : width, height\n");
(void) fprintf(ips,"{/height exch def\n");
(void) fprintf(ips," /width  exch def\n");
(void) fprintf(ips," newpath\n");
(void) fprintf(ips," height neg 10 width\n");
(void) fprintf(ips," { 0 moveto height height rlineto } for\n");
(void) fprintf(ips," stroke\n");
(void) fprintf(ips,"} def\n");
(void) fprintf(ips,"\n");

(void) fprintf(ips,"/diagonals2         %% stack : width, height\n");
(void) fprintf(ips,"{/height exch def\n");
(void) fprintf(ips," /width  exch def\n");
(void) fprintf(ips," newpath\n");
(void) fprintf(ips," 0 10 width height add\n");
(void) fprintf(ips," { 0 moveto height neg height rlineto } for\n");
(void) fprintf(ips," stroke\n");
(void) fprintf(ips,"} def\n");
(void) fprintf(ips,"\n");

   /* Initialize the linetypes */
   initPS_linetypes();

   /* Initialize the colors */
   initPS_colors();

   /* Print the date */ 
   if (printdate) {
   (void) fprintf(ips,"%%------------------------------------------------\n");
   (void) fprintf(ips,"%%--------------Print the Date--------------------\n");
   (void) fprintf(ips,"%%------------------------------------------------\n");
      /* print the time */
      CNget_localtime(time,CN_MAXCHAR);
      (void) fprintf(ips,"setDateLblFont\n");
      (void) fprintf(ips,"%.2f %.2f moveto ",
                     Pgxmin+0.3*scale*PBDR_DIM,Pgymax-0.4*scale*PBDR_DIM);
      (void) fprintf(ips,"(%s) show\n",time);
   }
}


/*
 * Read various Postscript options. 
 */
static void readPS_options(useISOLatinFont,
                           ps_topfont,
                           ps_sidefont,
                           ps_axisfont,
                           ps_mkrsfont,
                           ps_annotfont,
                           ps_datefont,
                           ps_ctrfont)
int    *useISOLatinFont;   /* Use ISOLatin Postscript Font */
double *ps_topfont;        /* Title Font size              */
double *ps_sidefont;       /* Side Label Font size         */
double *ps_axisfont;       /* Axis Font size               */
double *ps_mkrsfont;       /* Sidebar Marker Font size     */
double *ps_annotfont;      /* Annotation Font size         */
double *ps_datefont;       /* Date/Time Stamp Font size    */
double *ps_ctrfont;        /* Contour Label Font size      */
{
   char   *getenv();
   char   *ptr;
   int    verbose = CN_FALSE;
   double dval;
   static int options_done = CN_FALSE;

   /* We only need to read options once, so set and check a static variable */
   if (options_done) return;
   if (!options_done) options_done = CN_TRUE;

   /* Check the ISOLatin Font environment variable */
   if ((ptr = getenv("MTV_USE_ISOLATIN_FONT")) != NULL) {
      CNstring_to_lower(ptr);
      if ((strcmp(ptr,"off")==0) || (strcmp(ptr,"false")==0))
         *useISOLatinFont = CN_FALSE;
      else
         *useISOLatinFont = CN_TRUE;
   }

   /* Check the topfont size - this is for the title */
   if ((ptr = getenv("MTV_PSFONT1")) != NULL) {
         CNassign_double_keyword(&dval, ptr, "MTV_PSFONT1",
                                 (double)PS_TOPFONT,0.1,50.0,1,1,verbose);
         *ps_topfont = dval;
   }

   /* Check the sidefont size - this is for side labels */
   if ((ptr = getenv("MTV_PSFONT2")) != NULL) {
         CNassign_double_keyword(&dval, ptr, "MTV_PSFONT2",
                                 (double)PS_SIDEFONT,0.1,50.0,1,1,verbose);
         *ps_sidefont = dval;
   }

   /* Check the axisfont size - this is for axis labels */
   if ((ptr = getenv("MTV_PSFONT3")) != NULL) {
         CNassign_double_keyword(&dval, ptr, "MTV_PSFONT3",
                                 (double)PS_AXISFONT,0.1,50.0,1,1,verbose);
         *ps_axisfont = dval;
   }

   /* Check the mkrsfont size - this is for side-bar annotations */
   if ((ptr = getenv("MTV_PSFONT4")) != NULL) {
         CNassign_double_keyword(&dval, ptr, "MTV_PSFONT4",
                                 (double)PS_MKRSFONT,0.1,50.0,1,1,verbose);
         *ps_mkrsfont = dval;
   }

   /* Check the annotfont size - this is for in-plot annotations */
   if ((ptr = getenv("MTV_PSFONT5")) != NULL) {
         CNassign_double_keyword(&dval, ptr, "MTV_PSFONT5",
                                 (double)PS_ANNOTFONT,0.1,50.0,1,1,verbose);
         *ps_annotfont = dval;
   }

   /* Check the datefont size - this is for date/time stamp */
   if ((ptr = getenv("MTV_PSFONT6")) != NULL) {
         CNassign_double_keyword(&dval, ptr, "MTV_PSFONT6",
                                 (double)PS_DATEFONT,0.1,50.0,1,1,verbose);
         *ps_datefont = dval;
   }

   /* Check the ctrfont size - this is for contour line labels */
   if ((ptr = getenv("MTV_PSFONT7")) != NULL) {
         CNassign_double_keyword(&dval, ptr, "MTV_PSFONT7",
                                 (double)PS_CTRFONT,0.1,50.0,1,1,verbose);
         *ps_ctrfont = dval;
   }
}


/*  
 * Initialize a bounding box for the page 
 * The bounding box is used to:
 *   (a) clip a 3D plot so that it fits in 2D
 *   (b) draw a frame when the scale factor is less than 1
 */
static void initPS_boundingbox()
{
   (void) fprintf(ips,"%%------------------------------------------------\n");
   (void) fprintf(ips,"%%--------------Define the Bounding Box-----------\n");
   (void) fprintf(ips,"%%------------------------------------------------\n");
   (void) fprintf(ips,"/boundingboxpath {\n");
   (void) fprintf(ips,"newpath\n");
   (void) fprintf(ips,"%.2f %.2f moveto\n",Pgxmin,Pgymin);
   (void) fprintf(ips,"0.0 %.2f rlineto\n",Pgymax-Pgymin);
   (void) fprintf(ips,"%.2f 0.0 rlineto\n",Pgxmax-Pgxmin);
   (void) fprintf(ips,"0.0 %.2f rlineto\n",Pgymin-Pgymax);
   (void) fprintf(ips,"closepath } def\n");
}

/*
 * Draw the bounding box for the page
 * This usually happens when the scale factor is less than 1
 */
static void drawPS_boundingbox()
{
   (void) fprintf(ips,"%g setlinewidth boundingboxpath stroke\n",1*scale);
}


/*
 * Set up linetype definitions
 */
static void initPS_linetypes()
{
   int i;

(void) fprintf(ips,"%%------------------------------------------------\n");
(void) fprintf(ips,"%%--------------Linetype (dash) Definitions-------\n");
(void) fprintf(ips,"%%------------------------------------------------\n");
(void) fprintf(ips,"\n");

   /* Load a linetype palette */
   for (i= -1; i<=CN_LN_TYPES; i++) {
      switch(i) {
      case CN_LN_NONE :
            /* No Line */
            (void) fprintf(ips,"/MLT%-2d {",i);
            (void) fprintf(ips,"[0 1] 0 setdash         ");
            (void) fprintf(ips,"} def   %% No Line\n");
            break;
      case CN_LN_SOLID :
            /* Solid */
            (void) fprintf(ips,"/MLT%-2d {",i);
            (void) fprintf(ips,"[] 0 setdash            ");
            (void) fprintf(ips,"} def   %% Solid Line\n");
            break;
      case CN_LN_DASHED :
            /* Dashed */
            (void) fprintf(ips,"/MLT%-2d {",i);
            (void) fprintf(ips,"[6 6] 0 setdash         ");
            (void) fprintf(ips,"} def   %% Dashed Line\n");
            break;
      case CN_LN_DOTTED :
            /* Dotted */
            (void) fprintf(ips,"/MLT%-2d {",i);
            (void) fprintf(ips,"[1 2] 1 setdash         ");
            (void) fprintf(ips,"} def   %% Dotted Line\n");
            break;
      case CN_LN_DOTDASH :
            /* Dot Dash */
            (void) fprintf(ips,"/MLT%-2d {",i);
            (void) fprintf(ips,"[6 2 2 2] 0 setdash     ");
            (void) fprintf(ips,"} def   %% Dot-Dash Line\n");
            break;
      case CN_LN_LONGDOT :
            /* Long Dots */
            (void) fprintf(ips,"/MLT%-2d {",i);
            (void) fprintf(ips,"[2 2] 0 setdash         ");
            (void) fprintf(ips,"} def   %% Long-dotted Line\n");
            break;
      case CN_LN_DOUBLEDOT :
            /* Double Dot */
            (void) fprintf(ips,"/MLT%-2d {",i);
            (void) fprintf(ips,"[2 2 2 6] 0 setdash     ");
            (void) fprintf(ips,"} def   %% Double-dotted Line\n");
            break;
      case CN_LN_LONGDASH :
            /* Long Dash */
            (void) fprintf(ips,"/MLT%-2d {",i);
            (void) fprintf(ips,"[8 2] 0 setdash         ");
            (void) fprintf(ips,"} def   %% Long-dashed Line\n");
            break;
      case CN_LN_DOTDASH2 :
            /* Dot Dash */
            (void) fprintf(ips,"/MLT%-2d {",i);
            (void) fprintf(ips,"[9 6 2 6] 0 setdash     ");
            (void) fprintf(ips,"} def   %% Dot-Dashed Line\n");
           break;
      case CN_LN_TRIPLEDOT :
            /* Triple Dot */
            (void) fprintf(ips,"/MLT%-2d {",i);
            (void) fprintf(ips,"[1 1 1 1 1 3] 0 setdash ");
            (void) fprintf(ips,"} def   %% Triple-dot Line\n");
            break;
      case CN_LN_DOTDOTDASH:
            /* DotDot Dash */
            (void) fprintf(ips,"/MLT%-2d {",i);
            (void) fprintf(ips,"[3 3 3 3 6 3] 0 setdash ");
            (void) fprintf(ips,"} def   %% DotDotDash Line\n");
            break;
      case CN_GD_DOTTED   :
            /* Grid Dots */
            (void) fprintf(ips,"/MLT_GD {");
            (void) fprintf(ips,"[0.5 2] 0 setdash       ");
            (void) fprintf(ips,"} def   %% GridDotted Line\n");
            break;
      default: /* Solid */
            (void) fprintf(ips,"/MLT%-2d {",i);
            (void) fprintf(ips,"[] 0 setdash            ");
            (void) fprintf(ips,"} def   %% Solid Line\n");
            break;
      }
   }
(void) fprintf(ips,"\n");
}



/*
 * Set up color definitions
 * Load in different definitions for BW postscript
 */
static void initPS_colors()
{
   int i, depth=8;
   int r,g,b;
   double dr,dg,db;
   double max, min, val;

(void) fprintf(ips,"%%------------------------------------------------\n");
(void) fprintf(ips,"%%--------------Color   Definitions---------------\n");
(void) fprintf(ips,"%%------------------------------------------------\n");
(void) fprintf(ips,"\n");

   /* Load a color palette */
   for (i=0; i<PX_MAX_FILL_COLORS; i++) {

      /* Get the rgb values and allocate a color */
      load_cmap(i, &r, &g, &b, depth);

      /* Rescale the colors */
      dr = r/256.0;
      dg = g/256.0;
      db = b/256.0;

      /*
       * Gray-scale fill
       * 0.9 - white  - col=MAX
       * 0.3 - dark   - col=0
       */
      max     = 0.99;
      min     = 0.20;
      val     = max + (i)*(min-max)/(double)(PX_MAX_FILL_COLORS-1);

      /* write out this color */
      if (pscolor) 
      (void) fprintf(ips,"/MFC%-2d { %g %g %g setrgbcolor} def \n",i,dr,dg,db);
      else
      (void) fprintf(ips,"/MFC%-2d { %g setgray} def \n",i,val);
   }
   (void) fprintf(ips,"\n");

   /* Load line colors */
   for (i=0; i<PX_MAX_LINE_COLORS; i++) {
      switch (i) {
      case 0 : r=255; g=255; b=0  ;                       /* yellow     */
               break;
      case 1 : r=0  ; g=255; b=255;                       /* cyan       */
               break;
      case 2 : r=0  ; g=255; b=0  ;                       /* Green      */
               break;
      case 3 : r=255; g=0  ; b=0  ;                       /* red        */
               break;
      case 4 : r=30 ; g=144; b=255;                       /* dodgerBlue */
               break;
      case 5 : r=255; g=165; b=0  ;                       /* orange     */
               break;
      case 6 : r=255; g=0  ; b=255;                       /* magenta    */
               break;
      case 7 : r=255; g=192; b=203;                       /* Pink       */
               break;
      case 8 : r=64 ; g=224; b=208;                       /* Turquoise  */
               break;
      case 9 : r=210; g=105; b=30 ;                       /* chocolate  */
               break;
      default: r=255; g=255; b=0  ;                       /* yellow     */
               break;
      }

      /* Rescale the colors */
      dr = r/256.0;
      dg = g/256.0;
      db = b/256.0;

      /*
       * 0.9 - white  - col=0
       * 0.0 - dark   - col=MAX
       */
      max     = 0.95;
      min     = 0.00;
      val     = min + (i)*(max-min)/(double)(PX_MAX_LINE_COLORS-1);

      /* write out this color */
      if (pscolor) 
      (void) fprintf(ips,"/MLC%-2d { %g %g %g setrgbcolor} def\n",i,dr,dg,db);
      else
      (void) fprintf(ips,"/MLC%-2d { %g setgray} def\n",i,val);
   }
   (void) fprintf(ips,"\n");

   /* Load named colors */
   for (i=0; i<PX_MAX_NAMED_COLORS; i++) {
      switch (i) {
      case 0 : r=  0; g=  0; b=  0;                       /* black      */
               break;
      case 1 : r=255; g=255; b=255;                       /* white      */
               break;
      case 2 : r=  0; g=  0; b=255;                       /* mediumblue */
               break;
      case 3 : r=255; g=255; b=  0;                       /* yellow     */
               break;
      case 4 : r=  0; g=255; b=255;                       /* cyan       */
               break;
      case 5 : r=  0; g=255; b=  0;                       /* green      */
               break;
      case 6 : r=255; g=  0; b=  0;                       /* red        */
               break;
      case 7 : r= 30; g=144; b=255;                       /* dodgerblue */
               break;
      case 8 : r=255; g=165; b=  0;                       /* orange     */
               break;
      case 9 : r=255; g=  0; b=255;                       /* magenta    */
               break;
      case 10: r=255; g=182; b=193;                       /* LightPink  */
               break;
      case 11: r=175; g=238; b=238;                       /* PTurquoise */
               break;
      case 12: r=210; g=105; b= 30;                       /* chocolate  */
               break;
      default: r=255; g=255; b=  0;                       /* yellow     */
               break;
      }

      /* Rescale the colors */
      dr = r/256.0;
      dg = g/256.0;
      db = b/256.0;

      /* write out this color */
      if (pscolor) 
      (void) fprintf(ips,"/MNC%-2d { %g %g %g setrgbcolor} def \n",i,dr,dg,db);
      else
      (void) fprintf(ips,"/MNC%-2d { 0.0 setgray} def\n",i);
   }
   (void) fprintf(ips,"\n");
}

/*
 * Return the rgb values
 * x is a number between 0 and 31
 * return rgb values ranging from 0 to 255
 */
static void load_cmap(x,r,g,b,depth)
int x;
int *r, *g, *b, depth;
{
   char *getenv();
   char *ptr;
   int  use_wrb_colormap = CN_FALSE;

   /* Reset the index */
   if (x < 0)   x = 0;
   if (x > 31)  x = 31;
 
   /* Reverse the index */
   if ((ptr = getenv("MTV_REVERSE")) != NULL)
     {
       CNstring_to_lower(ptr);
       if ((strcmp(ptr,"on")==0) || (strcmp(ptr,"true")==0))
         x=31-x;
     }

   /* Gray-scale */
   if (depth == 1) {
      *r = *g = *b = x*8;
      return;
   }

   /* Select color map */
   if ((ptr = getenv("MTV_COLORMAP")) !=NULL) {
     CNstring_to_lower(ptr);
     if(strcmp(ptr,"rgb")==0) 
       colorscale_rgb(x,r,g,b);

     else if(strcmp(ptr,"wrb")==0)
       colorscale_wrb(x,r,g,b);

     else if(strcmp(ptr,"hsv")==0)
       colorscale_hsv(x,r,g,b);

     else if(strcmp(ptr,"gray")==0)
       colorscale_gray(x,r,g,b);

     else if(strcmp(ptr,"hot")==0)
       colorscale_hot(x,r,g,b);

     else if(strcmp(ptr,"cool")==0)
       colorscale_cool(x,r,g,b);

     else if(strcmp(ptr,"bone")==0)
       colorscale_bone(x,r,g,b);

     else if(strcmp(ptr,"copper")==0)
       colorscale_copper(x,r,g,b);

     else if(strcmp(ptr,"pink")==0)
       colorscale_pink(x,r,g,b);

     else if(strcmp(ptr,"jet")==0)
       colorscale_jet(x,r,g,b);

     else if(strcmp(ptr,"astro")==0)
       colorscale_astro(x,r,g,b);

     else if(strcmp(ptr,"heat")==0)
       colorscale_heat(x,r,g,b);

     else if(strcmp(ptr,"srb")==0)
       colorscale_srb(x,r,g,b);

     else if(strcmp(ptr,"lrb")==0)
       colorscale_lrb(x,r,g,b);

#ifdef ANALYTIC_COLORMAP
     else if(strcmp(ptr,"scale1")==0)
       colorscale1(x,r,g,b);

     else if(strcmp(ptr,"scale2")==0)
       colorscale2(x,r,g,b);
#endif

     else
       colorscale_rgb(x,r,g,b);
   }
   else
     if ((ptr = getenv("MTV_WRB_COLORMAP")) != NULL)
       {
         CNstring_to_lower(ptr);
         if ((strcmp(ptr,"off")==0) || (strcmp(ptr,"false")==0))
           colorscale_rgb(x,r,g,b);
         else
           colorscale_wrb(x,r,g,b);
       }
     else
       colorscale_rgb(x,r,g,b);

   /* Check bounds */
   if (*r < 0) *r = 0;
   if (*g < 0) *g = 0;
   if (*b < 0) *b = 0;
   if (*r > 255) *r = 255;
   if (*g > 255) *g = 255;
   if (*b > 255) *b = 255;
}
 
 
#ifdef ANALYTIC_COLORMAP
/*
 * This is the original color scale
 */
static void colorscale1(c,r,g,b)
int c;
int *r, *g, *b;
{
   int x;
 
   /* Scale x to a value between 0 and 256 */
   x = 10 + (int)(c*(256-20)/(double)31);
 
   /* Get the rgb values */
   if (x < 34) {
      *r = 0;
      *g = 0;
      *b = 4*x + 120;
   } else if (x < 98) {
      *r = 0;
      *g = 4*(x-34);
      *b = 255;
   } else if (x < 162) {
      *r = 4*(x-98);
      *g = 255;
      *b = 255 - 4*(x-98);
   } else if (x < 226) {
      *r = 255;
      *g = 255 - 4*(x-162);
      *b = 0;
   } else if (x < 256) {
      *r = 255 - 4*(x-226);
      *g = 0;
      *b = 0;
   }
}
 

/*
 * This color scale includes purple
 */
static void colorscale2(c,r,g,b)
int c;
int *r, *g, *b;
{
   int x;
 
   /* Scale x to a value between 0 and 256 */
   x = 10 + (int)(c*(256-20)/(double)31);
 
   /* Get the rgb values */
   if      (x <  54) *r = 216 - 4*x;
   else if (x < 101) *r = 0;
   else if (x < 152) *r = 5*(x-101);
   else if (x < 216) *r = 255;
   else if (x < 256) *r = 255 - 4*(x-216);
 
   if      (x <  34) *g = 0;
   else if (x <  98) *g = 4*(x-34);
   else if (x < 152) *g = 255;
   else if (x < 216) *g = 255 - 4*(x-152);
   else if (x < 256) *g = 0;
 
   if      (x <  54) *b = 147 + 2*x;
   else if (x <  98) *b = 255;
   else if (x < 149) *b = 255 - 5*(x-98);
   else if (x < 256) *b = 0;
   /*
   (void) printf("x=%d  r=%d  g=%d  b=%d\n",x,*r, *g, *b);
    */
}
#endif 

/*
 * This handpicked color scale ranges from
 * dark blue to red to yellow to white.
 * The colors on the local color postscript printer are different
 * so a little tuning is required to lighten the blue and to ease the
 * transitions between the colors
 */
static void colorscale_wrb(c,r,g,b)
int c;
int *r, *g, *b;
{
   switch(c) {
   case  0: *r=  0; *g=255; *b=255; break; /* Cyan */
   case  1: *r=  0; *g=225; *b=255; break;
   case  2: *r=  0; *g=190; *b=255; break; /* Light Blue */
   case  3: *r=  0; *g=160; *b=255; break; 
   case  4: *r=  0; *g=130; *b=255; break;
   case  5: *r=  0; *g=100; *b=255; break; /* Blue */

   case  6: *r= 35; *g=  0; *b=255; break; /* Purple */
   case  7: *r= 70; *g=  0; *b=255; break; 
   case  8: *r=105; *g=  0; *b=255; break;
   case  9: *r=140; *g=  0; *b=255; break;
   case 10: *r=175; *g=  0; *b=255; break;
   case 11: *r=210; *g=  0; *b=255; break; 
   case 12: *r=255; *g=  0; *b=255; break; /* Magenta */
   case 13: *r=205; *g=  0; *b=205; break;
   case 14: *r=165; *g=  0; *b=165; break;

   case 15: *r=180; *g=  0; *b=  0; break; /* Dark red */
   case 16: *r=205; *g=  0; *b=  0; break;
   case 17: *r=230; *g=  0; *b=  0; break;
   case 18: *r=255; *g=  0; *b=  0; break; /* Red */
   case 19: *r=255; *g= 20; *b=  0; break; /* Orange red */
   case 20: *r=255; *g= 45; *b=  0; break;
   case 21: *r=255; *g= 75; *b=  0; break; /* Dark orange */
   case 22: *r=255; *g=105; *b=  0; break; /* Orange */
   case 23: *r=255; *g=135; *b=  0; break;
   case 24: *r=255; *g=165; *b=  0; break;
   case 25: *r=255; *g=195; *b=  0; break;
   case 26: *r=255; *g=225; *b=  0; break;
   case 27: *r=255; *g=255; *b=  0; break; /* Yellow */
   case 28: *r=255; *g=255; *b=128; break;
   case 29: *r=255; *g=255; *b=200; break;
   case 30: *r=255; *g=255; *b=250; break; /* Snow */
   case 31: *r=255; *g=250; *b=255; break; /* White */
   default: *r=255; *g=255; *b=255; break; /* White */
   }
}


/*
 * This handpicked color scale ranges from red to yellow to green to blue
 * The colors on the local color postscript printer are different
 * so a little tuning is required to lighten the blue and to ease the
 * transitions between the colors
 */
static void colorscale_rgb(c,r,g,b)
int c;
int *r, *g, *b;
{
   switch(c) {
   case  0: *r=  0; *g=  0; *b=150; break; /* Dark Blue */
   case  1: *r=  0; *g=  0; *b=220; break; /* Medium Blue */
   case  2: *r=  0; *g=  0; *b=255; break; /* Blue */
   case  3: *r=  0; *g=100; *b=255; break; /* Blue */
   case  4: *r=  0; *g=130; *b=255; break;
   case  5: *r=  0; *g=160; *b=255; break;
   case  6: *r=  0; *g=190; *b=255; break; /* Light Blue */
   case  7: *r=  0; *g=225; *b=255; break;
   case  8: *r=  0; *g=255; *b=255; break; /* Cyan */
   case  9: *r=  0; *g=255; *b=192; break;
   case 10: *r=  0; *g=255; *b=128; break;
   case 11: *r=  0; *g=255; *b= 64; break;
   case 12: *r=  0; *g=255; *b=  0; break; /* Green */
   case 13: *r= 48; *g=255; *b= 10; break; 
   case 14: *r= 92; *g=255; *b= 20; break; /* chartreuse */
   case 15: *r=140; *g=255; *b= 47; break; /* Green Yellow */
   case 16: *r=192; *g=255; *b= 47; break;
   case 17: *r=220; *g=255; *b= 20; break;
   case 18: *r=255; *g=255; *b=  0; break; /* Yellow */
   case 19: *r=255; *g=230; *b=  0; break;
   case 20: *r=255; *g=205; *b=  0; break;
   case 21: *r=255; *g=170; *b=  0; break;
   case 22: *r=255; *g=140; *b=  0; break;
   case 23: *r=255; *g=110; *b=  0; break; /* Orange */
   case 24: *r=255; *g= 80; *b=  0; break; /* Dark orange */
   case 25: *r=255; *g= 50; *b=  0; break; /* Orange red */
   case 26: *r=255; *g=  0; *b=  0; break; /* Red */
   case 27: *r=220; *g=  0; *b=  0; break;
   case 28: *r=190; *g=  0; *b=  0; break;
   case 29: *r=160; *g=  0; *b=  0; break;
   case 30: *r=130; *g=  0; *b=  0; break; /* Dark red */
   case 31: *r=100; *g=  0; *b=  0; break; /* Dark red */
   default: *r=255; *g=255; *b=255; break; /* White */
   }
}


/* 
 * End procedure 
 */
static void endgrPS()
{
   (void) fprintf(ips,"showpage\n");
}


/* 
 * END EPSI 
 */
static void endgrEPSI()
{
   (void) fprintf(ips,"%sEOF\n","%%");
}



/*
 * SET COLORS/PATTERNS
 */

/*
 * set the linetype 
 */
int PXlinetypPS(linetyp, ithk)
int  linetyp;
int  ithk;
{
   int linepattern = CN_TRUE;

   /*
    * Reset the linetype if necessary
    *   Linetype = -1 is dotted grid
    *   Linetype =  0 is dots (no line)
    *   Linetype = 1-10 is various linetypes
    *   Linetype 11 => linetype 1 (linetypes cycle through 1-10)
    */
   if ((linetyp != CN_GD_DOTTED) && (linetyp != CN_LN_NONE))
      linetyp = (linetyp - 1) % CN_LN_TYPES + 1;

   switch (linetyp) {
   case CN_LN_NONE :
            (void) fprintf(ips,"MLT%-2d ",linetyp);           /* No Line */
            linepattern = CN_FALSE;
            break;
   case CN_LN_SOLID :
            (void) fprintf(ips,"MLT%-2d ",linetyp);           /* Solid */
            break;
   case CN_LN_DASHED :
            (void) fprintf(ips,"MLT%-2d ",linetyp);           /* Dashed */
            break;
   case CN_LN_DOTTED :
            (void) fprintf(ips,"MLT%-2d ",linetyp);           /* Dotted */
            break;
   case CN_LN_DOTDASH :
            (void) fprintf(ips,"MLT%-2d ",linetyp);           /* Dot Dash */
            break;
   case CN_LN_LONGDOT :
            (void) fprintf(ips,"MLT%-2d ",linetyp);           /* Long Dots */
            break;
   case CN_LN_DOUBLEDOT :
            (void) fprintf(ips,"MLT%-2d ",linetyp);           /* Double Dot */
            break;
   case CN_LN_LONGDASH :
            (void) fprintf(ips,"MLT%-2d ",linetyp);           /* Long Dash */
            break;
   case CN_LN_DOTDASH2 :
            (void) fprintf(ips,"MLT%-2d ",linetyp);           /* Dot Dash */
            break;
   case CN_LN_TRIPLEDOT :
            (void) fprintf(ips,"MLT%-2d ",linetyp);           /* Triple Dot */
            break;
   case CN_LN_DOTDOTDASH:
            (void) fprintf(ips,"MLT%-2d ",linetyp);           /* DotDot Dash */
            break;
   case CN_GD_DOTTED   :
            (void) fprintf(ips,"MLT_GD ");                    /* Grid Dots */
            break;
   default: (void) fprintf(ips,"MLT%-2d ",linetyp);    
            break;
   }
   (void) fprintf(ips,"%.2f setlinewidth\n",0.5*ithk*scale);

   /* Return */
   return(linepattern);
}


/* 
 * Draw a marker 
 */
void PXmarkerPS(markertype,markersize,x,y)
int    markertype, markersize;
double x,y;
{
   double scl;
   double d=4*fscale;

   /*
    * Reset the markertype if necessary
    *   marktype =  0 is dots (no line)
    *   marktype = 1-13 is various marktypes
    *   marktype 14 => marktype 1 (marktypes cycle through 1-13)
    */
   if (markertype != CN_MK_NONE)
      markertype = (markertype - 1) % CN_MK_TYPES + 1;

   /*
    * Scale factor for markers
    *   marksize = 1 is default size
    *   marksize = 2 is slightly larger
    */
   if (markersize < 1) markersize = 1;
   scl = 0.4*markersize+0.6;

   switch (markertype) {
   case CN_MK_NONE      : 
            /* don't draw */
            break;
   case CN_MK_DOT       : 
            /* A single point */
            (void) fprintf(ips,"newpath %.2f %.2f moveto markdot\n",x,y);
            break;
   case CN_MK_CROSS     : 
            /* A cross */
            (void) fprintf(ips,"%.2f %.2f %.2f %.2f drawline\n",
                           x-0.5*d*scl, y          , d*scl, 0.0);
            (void) fprintf(ips,"%.2f %.2f %.2f %.2f drawline\n",
                           x          , y-0.5*d*scl, 0.0  , d*scl);
            break;
   case CN_MK_X         : 
            /* X marks the spot */
            (void) fprintf(ips,"%.2f %.2f %.2f %.2f drawline\n",
                           x-0.5*d*scl, y-0.5*d*scl, 1.0*d*scl, 1.0*d*scl);
            (void) fprintf(ips,"%.2f %.2f %.2f %.2f drawline\n",
                           x+0.5*d*scl, y-0.5*d*scl,-1.0*d*scl, 1.0*d*scl);
            break;
   case CN_MK_SQUARE1   : 
            /* A white-filled rectangle */
            (void) fprintf(ips,"%.2f %.2f %.2f %.2f drawrect wfill\n",
                           x-0.4*d*scl, y-0.4*d*scl,
                             0.8*d*scl,   0.8*d*scl);
            break;
   case CN_MK_SQUARE2   : 
            /* A filled rectangle */
            (void) fprintf(ips,"%.2f %.2f %.2f %.2f drawrect fill\n",
                           x-0.4*d*scl, y-0.4*d*scl,
                             0.8*d*scl,   0.8*d*scl);
            break;
   case CN_MK_DIAMOND1  : 
            /* A white-filled diamond */
            (void) fprintf(ips,"newpath %.2f %.2f moveto\n", x , y-0.5*d*scl);
            (void) fprintf(ips,"%.2f %.2f rlineto\n", 0.5*d*scl , 0.5*d*scl);
            (void) fprintf(ips,"%.2f %.2f rlineto\n",-0.5*d*scl , 0.5*d*scl);
            (void) fprintf(ips,"%.2f %.2f rlineto\n",-0.5*d*scl ,-0.5*d*scl);
            (void) fprintf(ips,"%.2f %.2f rlineto\n", 0.5*d*scl ,-0.5*d*scl);
            (void) fprintf(ips,"wfill\n");
            break;
   case CN_MK_DIAMOND2  :
            /* A filled diamond */
            (void) fprintf(ips,"newpath %.2f %.2f moveto\n", x , y-0.5*d*scl);
            (void) fprintf(ips,"%.2f %.2f rlineto\n", 0.5*d*scl , 0.5*d*scl);
            (void) fprintf(ips,"%.2f %.2f rlineto\n",-0.5*d*scl , 0.5*d*scl);
            (void) fprintf(ips,"%.2f %.2f rlineto\n",-0.5*d*scl ,-0.5*d*scl);
            (void) fprintf(ips,"%.2f %.2f rlineto\n", 0.5*d*scl ,-0.5*d*scl);
            (void) fprintf(ips,"fill\n");
            break;
   case CN_MK_TRIANGLE1 : 
            /* A white-filled upright triangle */
            (void) fprintf(ips,"%.2f %.2f %.2f %.2f %.2f %.2f drawtria wfill\n",
                           x-0.5*d*scl, y-0.22*d*scl,
                           x+0.5*d*scl, y-0.22*d*scl, 
                           x          , y+0.66*d*scl);
            break;
   case CN_MK_TRIANGLE2 : 
            /* A filled upright triangle */
            (void) fprintf(ips,"%.2f %.2f %.2f %.2f %.2f %.2f drawtria fill\n",
                           x-0.5*d*scl, y-0.22*d*scl,
                           x+0.5*d*scl, y-0.22*d*scl, 
                           x          , y+0.66*d*scl);
            break;
   case CN_MK_ITRIANGLE1: 
            /* A white-filled upside-down triangle */
            (void) fprintf(ips,"%.2f %.2f %.2f %.2f %.2f %.2f drawtria wfill\n",
                           x-0.5*d*scl, y+0.22*d*scl,
                           x+0.5*d*scl, y+0.22*d*scl, 
                           x          , y-0.66*d*scl);
            break;
   case CN_MK_ITRIANGLE2: 
            /* A filled upside-down triangle */
            (void) fprintf(ips,"%.2f %.2f %.2f %.2f %.2f %.2f drawtria fill\n",
                           x-0.5*d*scl, y+0.22*d*scl,
                           x+0.5*d*scl, y+0.22*d*scl, 
                           x          , y-0.66*d*scl);
            break;
   case CN_MK_CIRCLE1   : 
            /* A filled circle */
            (void) fprintf(ips,"newpath %.2f %.2f moveto\n", x ,y-0.5*d*scl);
            (void) fprintf(ips,"%.2f %.2f rlineto\n", 0.0       ,     d*scl);
            (void) fprintf(ips,"%.2f %.2f rmoveto\n",-0.5*d*scl ,-0.5*d*scl);
            (void) fprintf(ips,"%.2f %.2f rlineto\n",     d*scl , 0.0      );
            (void) fprintf(ips,"%.2f %.2f rlineto\n",-0.5*d*scl , 0.0      );
            (void) fprintf(ips,"currentpoint %.2f 0 360 arc wfill\n",0.5*d*scl);
            break;
   case CN_MK_CIRCLE2   : 
            /* A filled circle */
            (void) fprintf(ips,"newpath %.2f %.2f moveto\n", x ,y-0.5*d*scl);
            (void) fprintf(ips,"%.2f %.2f rlineto\n", 0.0       ,     d*scl);
            (void) fprintf(ips,"%.2f %.2f rmoveto\n",-0.5*d*scl ,-0.5*d*scl);
            (void) fprintf(ips,"%.2f %.2f rlineto\n",     d*scl , 0.0      );
            (void) fprintf(ips,"%.2f %.2f rlineto\n",-0.5*d*scl , 0.0      );
            (void) fprintf(ips,"currentpoint %.2f 0 360 arc fill\n",0.5*d*scl);
            break;
   default: break;
   }
}


/*
 * Set a namedcolor
 */
/*ARGSUSED*/
void PXnamedColorPS(namedcol)
int namedcol;
{
   /*
    * -1 is the background color (PS white)
    *  0 is the foreground color
    *  1-11 are named colors (1=medblue, 2=yellow...)
    */

   if (namedcol <= -1) {
      /* Background color */
      (void) fprintf(ips,"1.0 setgray\n");

   } else if (namedcol == 0) {
      /* Foreground color */
      (void) fprintf(ips,"0.0 setgray\n");

   } else {
      /* Color */
      namedcol = PXnamedColorIndex(namedcol)+2;
      (void) fprintf(ips,"MNC%d\n",namedcol);
   }
}

/*
 * Set a linecolor
 *   linecolor=-1 gives the background color (PS white)
 *   linecolor= 0 gives the foreground color (PS black)
 *   linecolor= 1-10 (MAX_LINE_COLORS=10) gives a line color 
 *   linecolor > 10 cycles through the 10 specified colors
 *   color11=color1, etc.
 */
/*ARGSUSED*/
void PXlineColorPS(linecol)
int linecol;
{
   /*
    * -1 is the background color (PS white)
    *  0 is the foreground color (PS black)
    *  1 is the foreground color (PS black)
    *  2-10 are line colors
    */

   if (linecol <= -1) {
      /* Background color (PS white) */
      (void) fprintf(ips,"1.0 setgray\n");

   } else if (linecol == 0) {
      /* Foreground color (PS black) */
      (void) fprintf(ips,"0.0 setgray\n");

   } else {
      /* Adjust the index */
      linecol = (linecol-1) % PX_MAX_LINE_COLORS;

      /* Color */
      (void) fprintf(ips,"MLC%d\n",linecol);
   }
}

/* 
 * choose a fillcolor for a gradual ramp (in grayscale)
 */
void PXfillColorPS(fillcol)
int fillcol;
{
   /*
    * -1 is the background color (PS white)
    *  0 is the foreground color (PS black)
    *  1-32      are fill colors
    *
    *  fill #1   white (gray=0.9)
    *  fill #32  dark  (gray=0.2)
    */

   if (fillcol <= -1) {
      /* Background color (PS white) */
      (void) fprintf(ips,"1.0 setgray\n");

   } else if (fillcol == 0) {
      /* Foreground color (PS black) */
      (void) fprintf(ips,"0.0 setgray\n");

   } else {
      /* Adjust the index */
      fillcol = (fillcol-1) % PX_MAX_FILL_COLORS;

      /* Use predefined colors */
      (void) fprintf(ips,"MFC%d\n",fillcol);
   }
}


/*
 * Set a color
 *   0 = white (foreground color)
 *   1 = black (background color
 *   2-12  are named colors
 *   13-22 are fill colors
 *   23-54 are line colors
 */
void PXsetColorPS(col)
int col;
{
   int index = col % (PX_MAX_COLORS);

   if (index < PX_MAX_NAMED_COLORS)
      PXnamedColorPS(index);
   else if (index < PX_MAX_FILL_COLORS+PX_MAX_NAMED_COLORS)
      PXfillColorPS(index-PX_MAX_NAMED_COLORS+1);
   else
      PXlineColorPS(index-PX_MAX_FILL_COLORS-PX_MAX_NAMED_COLORS+1);

   return;
}

/*
 * Set the index to a fill color - used primarily for drawing polygons
 *
 * The real reason for this routine is that the color-postscript and
 * grayscale mapping are different.  Linecolor(1) should be black on
 * a bw postscript plot and yellow on a color postscript plot.
 *
 * Note:
 *   PXlinecolorPS(1)                    => yellow (C),  black (BW)
 *   PXsetcolorPS(PXpolyColorIndexPS(1)) => yellow (C), light gray (BW)
 */
int PXpolyColorIndexPS(polycolor)
int polycolor;
{
   int index;
 
   /* If colorps is set, this should return the lineColorIndex */
   if (pscolor) return(PXlineColorIndex(polycolor));
 
   /*
    * Return gray-values ranging from 0.8 to 0.3
    */
 
   if (polycolor <= 0)
      /* Return unchanged */
      index = polycolor;
   else {
      index = (polycolor - 1) % PX_MAX_LINE_COLORS;
      switch(index) {
      case 0 : index = 10; break;
      case 1 : index =  8; break;
      case 2 : index =  6; break;
      case 3 : index =  4; break;
      case 4 : index =  2; break;
      case 5 : index =  9; break;
      case 6 : index =  7; break;
      case 7 : index =  5; break;
      case 8 : index =  3; break;
      case 9 : index =  1; break;
      default: index = 10; break;
      }
      index = PXlineColorIndex(index);
   }    
   /* Return the color index */
   return(index);
}


/*
 * Set the annotation font size.
 * PXsetAnnotFontX(0) resets the font to the normal font.
 */
void PXsetAnnotFontPS(fontsize)
double fontsize;
{

   /* Redefine the postscript variable */ 
   if (fontsize <= 0.0) {
   (void) fprintf(ips,"/defFontSizeAnnotLbl %.2f def\n",annot_fontsize*fscale);
   } else {
   (void) fprintf(ips,"/defFontSizeAnnotLbl %.2f def\n",fontsize*fscale);
   }

   /* Reset the font */
   (void) fprintf(ips,"/AnnotLblFont ");

   if (usePSfontType==0) 
     (void) fprintf(ips,"/Times-Roman");
   else if (usePSfontType==1)
     (void) fprintf(ips,"/Helvetica");
   else if (usePSfontType==2)
     (void) fprintf(ips,"/Courier");
   else if (usePSfontType==3) 
     (void) fprintf(ips,"/Times-Italic");
   else if (usePSfontType==4)
     (void) fprintf(ips,"/Helvetica-Oblique");
   else if (usePSfontType==5)
     (void) fprintf(ips,"/Courier-Oblique");

   /* Use ISOLatin fonts */
   if (useISOLatinFont) 
     (void) fprintf(ips,"-ISOLatin1 ");
   else
     (void) fprintf(ips," ");

   (void) fprintf(ips,"findfont defFontSizeAnnotLbl scalefont def\n");

   /* Set the font */
   (void) fprintf(ips,"setAnnotLblFont\n");
}



/* 
 * USEFUL DRAWING PRIMITIVES
 */

/*
 * Convenience function to fill a polygon
 */
void PXfillPS_polygon(points,npoints,
                      filltype, fillcolor, linestyle, linecolor, linethick)
PSPoint points[];
int     npoints;
int     filltype, fillcolor, linestyle, linecolor, linethick;
{
   double bxmin, bxmax, bymin, bymax;
   int    i;

   /* Need more than 2 points */
   if (npoints <= 2) return;

   /* if filltype and linestyle are both NONE (0), return now */
   if ((filltype==CN_FILL_NONE) && (linestyle==CN_LN_NONE)) return;

   /* Find the polygon's bounding box */
   bxmin = bxmax = points[0].x;
   bymin = bymax = points[0].y;
   for (i=0; i<npoints; i++) {
      if (bxmin > points[i].x) bxmin = points[i].x;
      if (bxmax < points[i].x) bxmax = points[i].x;
      if (bymin > points[i].y) bymin = points[i].y;
      if (bymax < points[i].y) bymax = points[i].y;
   }

   /* Define the polygon */
   (void) fprintf(ips,"newpath\n%.2f %.2f moveto\n", 
                  points[0].x,points[0].y);
   for (i=1; i<npoints; i++)
      (void) fprintf(ips,"%.2f %.2f lineto\n",points[i].x,points[i].y);
   (void) fprintf(ips,"closepath\n");

   /*
    * Color the polygon
    */
   if (filltype != CN_FILL_NONE) {

      /* Set the filltype and fillcolor (and fill the polygon) */
      PXfilltypPS(filltype,fillcolor,bxmin,bxmax,bymin,bymax);

   }

   /*
    * Draw the outline of the polygon
    */
   if (linestyle != CN_LN_NONE) {
      /* Set the color */
      PXsetColorPS(linecolor);

      /* Set the linetype */
      (void) PXlinetypPS(linestyle,linethick);

      /* Draw the polygon */
      (void) fprintf(ips,"stroke\n");

      /* Reset the linetype */
      (void) PXlinetypPS(CN_LN_SOLID,1);

      /* Reset the color */
      PXsetColorPS(0);
   }
}


/*
 * Set the fill-style
 * This needs to be preceded by a polygon definition, e.g.
 *    newpath { 0 0 moveto 1 1 rlineto } closepath
 *    fillColorPS(X)   ==  X setgrey
 *    filltypPS(1)   ==  gsave  fill  grestore
 *    fillColorPS(0)   ==  0 setgrey
 *    stroke
 */
static void PXfilltypPS(filltype,fillcolor,bxmin,bxmax,bymin,bymax)
int    filltype, fillcolor;
double bxmin,bxmax,bymin,bymax;
{
   /*
    * Now set up the fills
    * Reset the filltype if necessary
    *   Filltype =  0 is no Fill
    *   Filltype = 1-8 is various Filltypes
    *   Filltype 9 => Filltype 1 (Filltypes cycle through 1-8)
    */
   if (filltype != CN_FILL_NONE)
      filltype = (filltype - 1) % CN_FILL_TYPES + 1;

   /* Set the color */
   PXsetColorPS(fillcolor);

   /* Set the patterns */
   switch (filltype) {
   case CN_FILL_NONE:
            /* Do nothing */
            break;
   case CN_FILL_SOLID :
            /* Solid Fill */
            (void) fprintf(ips,"gsave\n");
            (void) fprintf(ips,"fill\n");
            (void) fprintf(ips,"grestore\n");
            break;
   case CN_FILL_SQUARE :
            (void) fprintf(ips,"gsave\n");
            (void) fprintf(ips,"fill\n");
            (void) fprintf(ips,"grestore\n");
            (void) fprintf(ips,"gsave\n");
            if (fillcolor == 0) PXsetColorPS(-1);
            else                PXsetColorPS(0);
            (void) fprintf(ips,"clip\n");
            (void) fprintf(ips,"[] 0 setdash 0.5 setlinewidth\n");
            (void) fprintf(ips,"%.2f %.2f translate\n", bxmin, bymin);
            (void) fprintf(ips,"%.2f %.2f verticals1  \n",
                           bxmax-bxmin,bymax-bymin);
            (void) fprintf(ips,"%.2f %.2f horizontals1\n",
                           bxmax-bxmin,bymax-bymin);
            (void) fprintf(ips,"%.2f %.2f translate\n",-bxmin,-bymin);
            (void) fprintf(ips,"grestore\n");
            break;
   case CN_FILL_DIAG1  :
            /* Diagonal lines */
            (void) fprintf(ips,"gsave\n");
            (void) fprintf(ips,"fill\n");
            (void) fprintf(ips,"grestore\n");
            (void) fprintf(ips,"gsave\n");
            if (fillcolor == 0) PXsetColorPS(-1);
            else                PXsetColorPS(0);
            (void) fprintf(ips,"clip\n");
            (void) fprintf(ips,"[] 0 setdash 0.5 setlinewidth\n");
            (void) fprintf(ips,"%.2f %.2f translate\n", bxmin, bymin);
            (void) fprintf(ips,"%.2f %.2f diagonals1  \n",
                           bxmax-bxmin,bymax-bymin);
            (void) fprintf(ips,"%.2f %.2f translate\n",-bxmin,-bymin);
            (void) fprintf(ips,"grestore\n");
            break;
   case CN_FILL_DIAG2  :
            /* Diagonal lines */
            (void) fprintf(ips,"gsave\n");
            (void) fprintf(ips,"fill\n");
            (void) fprintf(ips,"grestore\n");
            (void) fprintf(ips,"gsave\n");
            if (fillcolor == 0) PXsetColorPS(-1);
            else                PXsetColorPS(0);
            (void) fprintf(ips,"clip\n");
            (void) fprintf(ips,"[] 0 setdash 0.5 setlinewidth\n");
            (void) fprintf(ips,"%.2f %.2f translate\n", bxmin, bymin);
            (void) fprintf(ips,"%.2f %.2f diagonals2  \n",
                           bxmax-bxmin,bymax-bymin);
            (void) fprintf(ips,"%.2f %.2f translate\n",-bxmin,-bymin);
            (void) fprintf(ips,"grestore\n");
            break;
   case CN_FILL_CHKRBD :
            /* Diagonal lines */
            (void) fprintf(ips,"gsave\n");
            (void) fprintf(ips,"fill\n");
            (void) fprintf(ips,"grestore\n");
            (void) fprintf(ips,"gsave\n");
            if (fillcolor == 0) PXsetColorPS(-1);
            else                PXsetColorPS(0);
            (void) fprintf(ips,"clip\n");
            (void) fprintf(ips,"[] 0 setdash 0.5 setlinewidth\n");
            (void) fprintf(ips,"%.2f %.2f translate\n", bxmin, bymin);
            (void) fprintf(ips,"%.2f %.2f diagonals1  \n",
                           bxmax-bxmin,bymax-bymin);
            (void) fprintf(ips,"%.2f %.2f diagonals2  \n",
                           bxmax-bxmin,bymax-bymin);
            (void) fprintf(ips,"%.2f %.2f translate\n",-bxmin,-bymin);
            (void) fprintf(ips,"grestore\n");
            break;
   case CN_FILL_VERT   :
            /* Vertical lines */
            (void) fprintf(ips,"gsave\n");
            (void) fprintf(ips,"fill\n");
            (void) fprintf(ips,"grestore\n");
            (void) fprintf(ips,"gsave\n");
            if (fillcolor == 0) PXsetColorPS(-1);
            else                PXsetColorPS(0);
            (void) fprintf(ips,"clip\n");
            (void) fprintf(ips,"[] 0 setdash 0.5 setlinewidth\n");
            (void) fprintf(ips,"%.2f %.2f translate\n", bxmin, bymin);
            (void) fprintf(ips,"%.2f %.2f verticals2  \n",
                           bxmax-bxmin,bymax-bymin);
            (void) fprintf(ips,"%.2f %.2f translate\n",-bxmin,-bymin);
            (void) fprintf(ips,"grestore\n");
            break;
   case CN_FILL_HORZ   :
            /* Horizontal lines */
            (void) fprintf(ips,"gsave\n");
            (void) fprintf(ips,"fill\n");
            (void) fprintf(ips,"grestore\n");
            (void) fprintf(ips,"gsave\n");
            if (fillcolor == 0) PXsetColorPS(-1);
            else                PXsetColorPS(0);
            (void) fprintf(ips,"clip\n");
            (void) fprintf(ips,"[] 0 setdash 0.5 setlinewidth\n");
            (void) fprintf(ips,"%.2f %.2f translate\n", bxmin, bymin);
            (void) fprintf(ips,"%.2f %.2f horizontals2\n",
                           bxmax-bxmin,bymax-bymin);
            (void) fprintf(ips,"%.2f %.2f translate\n",-bxmin,-bymin);
            (void) fprintf(ips,"grestore\n");
            break;

   case CN_FILL_DOTTED :
            /* Dots */
            (void) fprintf(ips,"gsave\n");
            (void) fprintf(ips,"fill\n");
            (void) fprintf(ips,"grestore\n");
            (void) fprintf(ips,"gsave\n");
            if (fillcolor == 0) PXsetColorPS(-1);
            else                PXsetColorPS(0);
            (void) fprintf(ips,"clip\n");
            (void) fprintf(ips,"[2 4] 0 setdash 0.5 setlinewidth\n");
            (void) fprintf(ips,"%.2f %.2f translate\n", bxmin, bymin);
            (void) fprintf(ips,"%.2f %.2f horizontals2\n",
                           bxmax-bxmin,bymax-bymin);
            (void) fprintf(ips,"[] 0 setdash\n");
            (void) fprintf(ips,"%.2f %.2f translate\n",-bxmin,-bymin);
            (void) fprintf(ips,"grestore\n");
            break;
   default: break;
   }

   /* ReSet the color */
   PXsetColorPS(0);
}


/*
 * Draw a line 
 */
void PXdrawPS_line(xa,ya,xb,yb,linetyp,ithk)
double xa,ya,xb,yb;
int  linetyp;
int  ithk;
{
   int linepat;

   linepat = PXlinetypPS(linetyp, ithk);
   if (linepat) {
      (void) fprintf(ips,"%.2f %.2f ",xa,ya);
      (void) fprintf(ips,"%.2f %.2f drawline\n",xb-xa,yb-ya);
   }
   (void) PXlinetypPS(CN_LN_SOLID, 1);
}



/*
 * Shared 2D/3D functions
 */

/*
 * plot linelabels in POSTSCRIPT
 */
/*ARGSUSED*/
void PXplotPS_linelabels(plotdata, hiddenline,
                         Pxmin,Pxmax,Pymin,Pymax,offset)
CNplotsetptr plotdata;
short        hiddenline;
double  Pxmin,Pxmax,Pymin,Pymax;
double  offset;
{  
   double  x1, y1, width, height, y1a, y1b, x1a;
   double  text_xpos, text_ypos;
   PSPoint points[10];
   int     npoints;
   int     linepat, colrinc, lineinc;
   int     linetype, linecolor, marktype, markcolor, fillcolor, filltype;
   int     marksize;
   int     applyfill;
   int     doclip=CN_TRUE;
   int     right_justify_longlabels=CN_FALSE;

   CNdslistptr  DS;
   CNcurveptr   C;


   /*
    * The label must fit inside PBDR_DIM=100.
    * Out of this amount, take up 30 for the lines and 60 for the labels.
    */
   /* Initial Points */
   width  = 25*scale;
   height = 22*scale;        /* MkrsLbl */
   x1     = Pxmax + offset;
   y1     = Pymax - height;

   /* Set the font */
   (void) fprintf(ips,"%%------------------------------------------------\n");
   (void) fprintf(ips,"%%--------------Draw Side Labels------------------\n");
   (void) fprintf(ips,"%%------------------------------------------------\n");
   (void) fprintf(ips,"setMkrsLblFont\n");

   /* Save the state and set a clipping path */
   if (!drawframe) doclip = CN_FALSE;
   if (doclip) {
      (void) fprintf(ips,"gsave\n");
      (void) fprintf(ips,"%g %g %g %g drawrect clip\n",
                     Pxmax, Pymin, 
                     Pgxmax-1*scale - (Pxmax), Pymax-Pymin); 
   }

   /* Go thru each 2D dataset */
   colrinc = 0;
   lineinc = 0;
   for (DS=plotdata->datahead; DS!=NULL && y1>Pgymin; DS=DS->next) {

      /* Plot only the 2D/3D plot labels */
      if ((DS->Dptr->datatype==CN_PLOT2D) || 
          (DS->Dptr->datatype==CN_PROBAB) || 
          (DS->Dptr->datatype==CN_BARCHART) || 
          (DS->Dptr->datatype==CN_PLOT3D)){

         /* Curve properties inherited from the dataset */
         applyfill = DS->Dptr->data_pr.applyfill;

         /* Curve set */
         for (C=DS->Dptr->curvehead; C!=NULL && y1>Pgymin; C=C->next) {

            x1a = x1 + 2*scale;
            y1a = y1 + 9*scale;
            y1b = y1 + height - 1*scale;

            if (CNstrlen(C->curv_pr.linelabel)==0) continue;

            /* 
             * If the curve property is identical to the prev curve
             * don't do anything
             */
            if ((C->prev != NULL) &&
                (CNstrlen(C->prev->curv_pr.linelabel)>0) &&
                (strcmp(C->prev->curv_pr.linelabel, C->curv_pr.linelabel)==0) &&
                (C->prev->curv_pr.linewidth == C->curv_pr.linewidth) &&
                (C->prev->curv_pr.linetype  == C->curv_pr.linetype ) &&
                (C->prev->curv_pr.linecolor == C->curv_pr.linecolor) &&
                (C->prev->curv_pr.marksize  == C->curv_pr.marksize ) &&
                (C->prev->curv_pr.marktype  == C->curv_pr.marktype ) &&
                (C->prev->curv_pr.markcolor == C->curv_pr.markcolor) &&
                (C->prev->curv_pr.filltype  == C->curv_pr.filltype ) &&
                (C->prev->curv_pr.fillcolor == C->curv_pr.fillcolor))  
               continue;

            /* Set the line type */
            linetype = C->curv_pr.linetype;
            if (linetype != 0) linetype += lineinc;
 
            /* Set the line color */
            linecolor = C->curv_pr.linecolor;
            if (linecolor > 0) linecolor += colrinc;
 
            /* Set the line type */
            marktype = C->curv_pr.marktype;
            if (marktype != 0) marktype += lineinc;
 
            /* Set the marker size */
            marksize = C->curv_pr.marksize;

            /* Set the marker color */
            markcolor = C->curv_pr.markcolor;
            if (markcolor > 0) markcolor += colrinc;

            /* Set the filltype - special treatment for hiddenline */
            filltype = C->curv_pr.filltype;
            if (hiddenline && C->curv_pr.filltype==CN_FILL_NONE)
               filltype = CN_FILL_SOLID;
            if (!applyfill) filltype = CN_FILL_NONE;
 
            /* Set the fill color */
            fillcolor = C->curv_pr.fillcolor;

            if (filltype == CN_FILL_NONE) {

               /* Draw the line */
               PXlineColorPS(linecolor);
               PXdrawPS_line(x1a,y1a,x1+width,y1a,
                             linetype,C->curv_pr.linewidth);

               /* Draw the markers */
               (void) PXlinetypPS(CN_LN_SOLID,1);
               PXlineColorPS(markcolor);
               PXmarkerPS(marktype,
                          marksize, x1a      ,y1a);
               PXmarkerPS(marktype,
                          marksize, x1a+width,y1a);

            } else {

               /* Draw the rectangle */
               linepat = PXlinetypPS(linetype,C->curv_pr.linewidth);
               PXsetColorPS(linecolor);
               npoints=5;
               points[0].x = x1a      ;  points[0].y = y1a;
               points[1].x = x1a      ;  points[1].y = y1b;
               points[2].x = x1a+width;  points[2].y = y1b;
               points[3].x = x1a+width;  points[3].y = y1a;
               points[4].x = x1a      ;  points[4].y = y1a;
               PXfillPS_polygon(points,npoints,
                                filltype, PXpolyColorIndexPS(fillcolor),
                                CN_LN_NONE, 0, 1);

               /* Set the line color */
               PXlineColorPS(linecolor);

               /* Draw the outline */
               if (linepat)
                  (void) fprintf(ips,
                                 "%.2f %.2f %.2f %.2f drawrect stroke\n",
                                 x1a,y1a,width,y1b-y1a);

               /* Draw the markers */
               (void) PXlinetypPS(CN_LN_SOLID,1);
               PXlineColorPS(markcolor);
               PXmarkerPS(marktype,
                          marksize,x1a      ,y1a);
               PXmarkerPS(marktype,
                          marksize,x1a+width,y1a);
               PXmarkerPS(marktype,
                          marksize,x1a      ,y1b);
               PXmarkerPS(marktype,
                          marksize,x1a+width,y1b);

            }

            /* Reset the foreground */
            PXsetColorPS(0);

            /* Draw the text */
            if (right_justify_longlabels &&
                (strlen(C->curv_pr.linelabel)*PS_MKRSFONT*fscale*0.45 > 
                 ((Pgxmax-6*scale) - (Pxmax+offset)))) {
               /* 
                * Work within the area defined by 
                * x1=Pxmax+offset
                * x2=Pgxmax-6*scale
                */
               /* Right-justify the string */
               text_xpos  = Pgxmax - 6*scale;
               text_ypos  = y1;
               (void) fprintf(ips,"%.2f %.2f moveto ",text_xpos,text_ypos);
               (void) fprintf(ips,"(%s) MIJM RIJ show\n",
                              PXmodifyPS_string(C->curv_pr.linelabel));
            } else {
               text_xpos  = x1;
               text_ypos  = y1;
               (void) fprintf(ips,"%.2f %.2f moveto ",text_xpos,text_ypos);
               (void) fprintf(ips,"(%s) MIJM LEJ show\n",
                              PXmodifyPS_string(C->curv_pr.linelabel));
            }

            /* Reset the position */
            y1 -= 1.5*height;
         }

         if (DS->Dptr->curvehead != NULL) {
            if (!plotdata->plot_pr.overlay) {
               colrinc = 0;
               lineinc = 0;
            } else {
               colrinc++;
               lineinc++;
            }
         }
      }
   }

   /* Reset the foreground */
   PXsetColorPS(0);

   /* Unclip */
   if (doclip) {
      (void) fprintf(ips,"grestore\n");
   }
}

/*
 * Plot a color-scale in POSTSCRIPT
 */
/*ARGSUSED*/
void PXplotPS_contscale(cstephead, csteptail,
                        Pxmin, Pxmax, Pymin, Pymax, offset, contclip)
CNcontstepptr cstephead, csteptail;
double  Pxmin,Pxmax,Pymin,Pymax;
double  offset;
short   contclip;
{
   CNcontstepptr C;
   double x0, y0, xwid, ywid;
   double text_xpos, text_ypos;
   char   text[10];
   int    i, colr, inc, nctrs;

   /* Error check */
   if (cstephead== 0) return;

   /* Count no of steps */
   nctrs = CNcount_contsteps(cstephead, csteptail) - 1;
 
   /*
    * The label must fit inside PBDR_DIM=100.
    * Out of this amount, take up 30 for the lines and 60 for the labels.
    * There are NCTRS+1 bands.
    */

   /* Initial Points */
   xwid   = 15*scale;
   ywid   = (Pymax-Pymin)/(double)(nctrs+1);
   x0     = Pxmax + offset;
   y0     = Pymin;

   /* Set the font */
   (void) fprintf(ips,"%%------------------------------------------------\n");
   (void) fprintf(ips,"%%--------------Draw Contour Side Labels----------\n");
   (void) fprintf(ips,"%%------------------------------------------------\n");
   (void) fprintf(ips,"setMkrsLblFont\n");

   /* y-divisions
    *
    *  ----  z1
    *  |  |
    *  ----  z0
    *
    */
   i = 0;
   for (C=cstephead; C!=NULL; C=C->next) {
      /* Set the color - Scale the colors from 1 to 32 */
      colr      = (int)((double)(i*(PX_MAX_FILL_COLORS-1))/(double)nctrs) + 1;

      /* Draw a filled rectangle */
      y0 = Pymin + i*ywid;
      if (y0 > Pymax) y0 = Pymax;

      PXfillColorPS(colr);
      if (contclip && (C==cstephead || C==csteptail)) PXfillColorPS(-1);
      (void) fprintf(ips, "%.2f %.2f %.2f %.2f drawrect fill\n",
                     x0,y0,xwid,ywid);
      PXfillColorPS(0);

      inc = (nctrs > 15) ? 3 : 1;
      if ((i%inc == 0 || i == 0) && (i != nctrs)) {
         /* Label the rectangle */
         (void) sprintf(text,"%.3g",C->value);
         text_xpos  = x0 + xwid + 3*scale;
         text_ypos  = y0 + ywid;
         (void) fprintf(ips,"%.2f %.2f moveto ",text_xpos,text_ypos);
         (void) fprintf(ips,"(%s) MIJM LEJ show\n",text);
         PXdrawPS_line(x0+xwid,y0+ywid,x0+xwid-2*scale,y0+ywid, 1,1);
         PXdrawPS_line(x0     ,y0+ywid,x0     -2*scale,y0+ywid, 1,1);
      }

      /* Increment the step number */
      i++;
   }

   /* Draw a big rectangle to outline the colors */
   y0   = Pymin;
   ywid = Pymax - Pymin;
   (void) fprintf(ips, "%.2f %.2f %.2f %.2f drawrect stroke\n",
                  x0,y0,xwid,ywid);
}

/*
 * Plot an annotation text-label.
 * The font-size of this text-label is scalable.
 * Default is to center-justify the text.
 */
void PXplotPS_scalable_font(xc, yc, label,
                           fontsize,
                           leftJustify, rightJustify,
                           topJustify, bottomJustify)
double xc;             /* x-coordinate     */
double yc;             /* y-coordinate     */
char*  label;          /* text label       */
double fontsize;       /* font size        */
int    leftJustify;    /* left-justified   */
int    rightJustify;   /* right-justified  */
int    topJustify;     /* top-justified    */
int    bottomJustify;  /* bottom-justified */
{
   double text_xpos, text_ypos;
   char   horzJustify[10];
   char   vertJustify[10];

   /* Set the font */
   PXsetAnnotFontPS(fontsize);

   /* Adjust the left-right location */
   if (leftJustify) {
      /* Left-justified */
      text_xpos  = xc + 6.0*scale;
      (void) strcpy(horzJustify, "LEJ");
   } else if (rightJustify) {
      /* Right-justified */
      text_xpos  = xc - 6.0*scale;
      (void) strcpy(horzJustify, "RIJ");
   } else {
      /* Center-justified */
      text_xpos  = xc;
      (void) strcpy(horzJustify, "CEJ");
   }

   /* Adjust the top-bottom location */
   if (topJustify) {
      /* Print the text on top of the point */
      text_ypos = yc + 6*scale;
      (void) strcpy(vertJustify, "UPJ");
   } else if (bottomJustify) {
      /* Print the text on bottom of the point */
      text_ypos = yc - 6*scale;
      (void) strcpy(vertJustify, "BOJN");
   } else {
      /* Center-justified */
      text_ypos = yc;
      (void) strcpy(vertJustify, "MIJN");
   }

   /* Draw the text */
   (void) fprintf(ips,"%.2f %.2f moveto ", text_xpos, text_ypos);
         (void) fprintf(ips,"(%s) %s %s show\n",
                        PXmodifyPS_string(label), vertJustify, horzJustify);

   /* Reset the font */
   PXsetAnnotFontPS(0.0);
}


/*
 * TRANSLATION ROUTINES
 */


/* 
 * Translate world to PS coordinates and apply log options
 */
void PXtranslate_world_to_PS(xw, yw, xp, yp,
                        Pxmin, Pxmax, Pymin, Pymax,
                        xmin, xmax, ymin, ymax,
                        xlog, ylog, xflip, yflip)
double  xw,  yw;                   /* World coordinates            */
double *xp, *yp;                   /* PS plot coordinates          */
double  Pxmin, Pxmax, Pymin, Pymax;/* Postscript viewport          */
double  xmin,  xmax,  ymin,  ymax; /* world (linear/log) viewport  */
short   xlog,  ylog;               /* Logarithmic scales           */
short   xflip, yflip;              /* xmin is mapped to Xmax       */
{
   double x_min, x_max, y_min, y_max, x1, y1, tmp;

   /* 
    * Linear scale :
    *        xmin......x......xmax
    *          |       |        |
    *       Pxmin......P.....Pxmax
    *
    * Log scale :
    *        xmin..... x..... xmax
    *          |       |        |
    *       Pxmin......P.....Pxmax
    *    where xmin is the rounded log(real_xmin)
    */

   /* Get the actual world viewport to be translated */
   if (xlog) { x1 = CNlog10(xw); x_min =  xmin; x_max =  xmax; }
   else      { x1 = xw         ; x_min =  xmin; x_max =  xmax; }

   if (ylog) { y1 = CNlog10(yw); y_min =  ymin; y_max =  ymax; }
   else      { y1 = yw         ; y_min =  ymin; y_max =  ymax; }

   /* Interchange min and max limits if necessary */
   if (xflip){ tmp= x_max;       x_max = x_min; x_min = tmp;   }
   if (yflip){ tmp= y_max;       y_max = y_min; y_min = tmp;   }

   /* Now translate */
   _PXtranslate_world_to_PS(x1, y1, xp, yp,
                      Pxmin, Pxmax, Pymin, Pymax,
                      x_min, x_max, y_min, y_max);
}

/*
 * Translate from real coordinates to PS coordinates
 */
static void _PXtranslate_world_to_PS(xw, yw, xp, yp,
                        Pxmin, Pxmax, Pymin, Pymax,
                        xmin, xmax, ymin, ymax)
double  xw,  yw;                   /* World coordinates            */
double *xp, *yp;                   /* PS plot coordinates          */
double  Pxmin, Pxmax, Pymin, Pymax;
double  xmin, xmax, ymin, ymax;
{
   double xval, yval;

   /*
    * Translate from real world coordinates to PS plot coordinates.
    */
   PXtranslate_range(&xval,(double)Pxmin,(double)Pxmax,xw,xmin,xmax);
   PXtranslate_range(&yval,(double)Pymin,(double)Pymax,yw,ymin,ymax);
   *xp = xval;
   *yp = yval;
} 

/* 
 * Translate PS to world coordinates and apply log options
 */
void PXtranslate_PS_to_world(xp, yp, xw, yw,
                        Pxmin, Pxmax, Pymin, Pymax,
                        xmin, xmax, ymin, ymax,
                        xlog, ylog)
double  xp,  yp;                   /* PS plot coordinates          */
double *xw, *yw;                   /* World coordinates            */
double  Pxmin, Pxmax, Pymin, Pymax;/* PS viewport                  */
double  xmin,  xmax,  ymin,  ymax; /* world (linear) viewport      */
short   xlog,  ylog;               /* Logarithmic scales           */
{
   double x_min, x_max, y_min, y_max, x1, y1;

   /* 
    * Linear scale :
    *        xmin......x......xmax
    *          |       |        |
    *       Pxmin......P.....Pxmax
    *
    * Log scale :
    *        xmin..... x..... xmax
    *          |       |        |
    *       Pxmin......P.....Pxmax
    *    where xmin is the rounded log(real_xmin)
    */

   /* Get the actual world viewport to be translated */
   x_min =  xmin; x_max =  xmax;
   y_min =  ymin; y_max =  ymax; 

   /* Now translate from PS coords to world coordinates */
   _PXtranslate_PS_to_world(xp, yp, &x1, &y1,
                    Pxmin, Pxmax, Pymin, Pymax,
                    x_min, x_max, y_min, y_max);

   /* Now get the linear world coordinates (inverse log if necessary) */
   if (xlog) *xw = pow(10.0,x1);
   else      *xw = x1;
   if (ylog) *yw = pow(10.0,y1);
   else      *yw = y1;
}

/*
 * Translate from PS coordinates to real coordinates
 * This does the actual translation.
 */
static void _PXtranslate_PS_to_world(xp, yp, xw, yw,
                             Pxmin, Pxmax, Pymin, Pymax,
                             xmin, xmax, ymin, ymax)
double  xp,  yp;                   /* PS plot coordinates          */
double *xw, *yw;                   /* World coordinates            */
double Pxmin, Pxmax, Pymin, Pymax;
double xmin, xmax, ymin, ymax;
{
   double xval, yval; 

   /* 
    * Translate from PS coordinate to real coordinates.
    */
   PXtranslate_range(&xval,xmin,xmax,xp,Pxmin,Pxmax);
   PXtranslate_range(&yval,ymin,ymax,yp,Pymin,Pymax);
   *xw = xval;
   *yw = yval; 
}



/*
 * MISC UTILITIES
 */

/*
 * Modify a character set to take care of "(" and ")" problems
 */
char *PXmodifyPS_string(label)
char *label;
{
   static char newlabel[CN_MAXCHAR];
   char   c;
   int    i, count=0;

   /* Initialize */
   (void) strcpy(newlabel,"");

   if (label == NULL) return(newlabel);

   for (i=0; i<CN_MAXCHAR && count<CN_MAXCHAR && (c=label[i])!='\0'; i++) {
      if (c == '(') {
         newlabel[count++] = '\\';
         newlabel[count++] = '0';
         newlabel[count++] = '5';
         newlabel[count++] = '0';
      } else if (c== ')') {
         newlabel[count++] = '\\';
         newlabel[count++] = '0';
         newlabel[count++] = '5';
         newlabel[count++] = '1';
      } else {
         newlabel[count++] = c;
      }
   }
   if (count < CN_MAXCHAR)
      newlabel[count++] = '\0';
   else
      newlabel[count] = '\0';

   /* Return the new label */
   return(newlabel);
}

