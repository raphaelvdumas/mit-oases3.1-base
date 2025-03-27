/*
 * plotps_2D.c - PostScript subroutines for drawing 2D plots
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "plotps.h"
#include "CNplot.h"

#define EXTREMELY_SMALL 1.0e-99

/* Maximum no of points in an array */
#define MAX_ARR_SIZE  1000

/* Structure for buffering points */
#define MAX_POINTS    100
typedef struct poly_strct {
   PSPoint points[MAX_POINTS];
   int     npoints;
} ps_point_container;
static ps_point_container point_buffer[PX_MAX_FILL_COLORS];
static void init_point_buffer();
static void buffer_point();
static void flush_point_buffer();

/*
 * FORWARD DECLARATIONS
 */
void   PXplotps2D();
static void drawPSplot2D();
static void axesPS2D();
static void boundaryPS2D();
static void plotPS2D_xticks();
static void plotPS2D_yticks();
static void plotPS2D_Auto_xticks();
static void plotPS2D_Auto_yticks();
static void plotPS2D_User_xticks();
static void plotPS2D_User_yticks();
static void plotPS2D_Prob_yticks();
static void plotPS2D_Bar_xticks();
static void plotPS2D_xticklabel();
static void plotPS2D_yticklabel();
static int  xticklabel_overlap();

static void plotPS2D();
static void plotPS2D_grid();
static void plotPS2D_mesh4D();
static void plotPS2D_mesh4D_cube();
static void plotPS2D_vectors();
static void plotPS2D_boundary();
static void plotPS2D_trias();
static void plotPS2D_rects();
static void plotPS2D_polys();
static void plotPS2D_single_solid_tria();
static void plotPS2D_single_solid_rect();
static void plotPS2D_single_solid_poly();
static void plotPS2D_single_fill_tria();
static void plotPS2D_single_fill_rect();
static void plotPS2D_single_fill_poly();
static void plotPS2D_nodes();
static void plotPS2D_dataset_curves();
static void plotPS2D_curve();
static void fillPS2D_curve();
static void plotPS2D_spline_curve();
static void plotPS2D_markers();

static void plotPS2D_curveID();
static void plotPS2D_pointIDs();
static void plotPS2D_nodeIDs();
static void plotPS2D_triaIDs();
static void plotPS2D_rectIDs();

static void annotatePS2D();
static void plotPS2D_single_annotation();
static void plotPS2D_annot_rect();
static void plotPS2D_annot_line();
static void plotPS2D_annot_arrow();
static void plotPS2D_arrow();
static void plotPS2D_annot_point();
static void plotPS2D_annot_text();
static void clipPS2D_in_xmin();
static void clipPS2D_in_ymin();
static void clipPS2D_in_xmax();
static void clipPS2D_in_ymax();

static void plotPS2D_contlabel();
static void drawPS2D_label();
static void plotPS2D_sidelabels();

static void trn_world_to_PS();
static void trn_world_to_PS_nolog();
static double y_probability();

static int  do_fillpoly5();
static int  do_fillpoly4();
static int  do_fillpoly3();
static int  do_outlinepoly5();
static int  do_outlinepoly4();
static int  rightangle();

static int plotPS2D_bitmap_rects();
static int find_contour_color();
 
/* Plot variables */
static short  grid;
static short  clip;
static short  xflip, yflip;
static short  xabs, yabs, zabs;
static short  xlog, ylog, zlog;
static short  xticks, yticks;
static short  xautorange, yautorange;
static double xyratio;
static double xscale, yscale;
static short  overlay;
static short  innerticks;
static int    plotlabels;
static char   xlabel[MAXCHR];
static char   ylabel[MAXCHR];
static char   toplabel[MAXCHR];
static char   *comment;
static char   *subtitle;

/* Plot type */
static short  probability_plot;
static short  histogram_plot;
static short  barchart_plot;

/* Hiddenline - used to mirrow 3D hiddenline plot */
static short  hiddenline;

/* The plotset */
static CNplotsetptr plotdata=NULL;

/* contour steps */
static CNcontstepptr cstephead=NULL;
static CNcontstepptr csteptail=NULL;

/* Contour dptr */
static CNdatasetptr contour_dptr=NULL;

/* The real-world dimensions of the plot */
static double xmin, xmax, ymin, ymax, zmin, zmax;

/* PSplot dimensions */
static double  Pxmin, Pymin, Pxmax, Pymax;

/* Scaling factors */
static double idx, jdy;


/*
 * Draw the plot in PostScript
 */
void PXplotps2D(pdata)
CNplotsetptr pdata;                  /* The plot data is here   */
{
   void PXquery_contours();
   int  PXquery_labels();
   void PXcheck_viewport();
   void PXconvert_viewport_to_log();

   double lxmin, lxmax, lymin, lymax;
   double bxmin, bxmax, bymin, bymax, bzmin, bzmax;
   int    equalscale, fitpage;
   double Pxwid,Pywid,ratio,dx,dy;
   char   label[CN_MAXCHAR];

   /* Error checking */
   if (pdata==NULL) {
      (void) fprintf(stderr,"PXplotps2D(): Error! Cannot plot NULL plotset!\n");
      return;
   }

   /* Copy the variables to the static variables first */
   plotdata  = pdata; 

   /* Initialize some other plot variables */
   grid       = plotdata->plot_pr.grid;
   xflip      = plotdata->plot_pr.xflip;
   yflip      = plotdata->plot_pr.yflip;
   xabs       = plotdata->plot_pr.xabs;
   yabs       = plotdata->plot_pr.yabs;
   zabs       = CN_FALSE;
   xlog       = plotdata->plot_pr.xlog;
   ylog       = plotdata->plot_pr.ylog;
   zlog       = CN_FALSE;
   xticks     = plotdata->plot_pr.xticks;
   yticks     = plotdata->plot_pr.yticks;
   xautorange = plotdata->plot_pr.xautorange;
   yautorange = plotdata->plot_pr.yautorange;
   xyratio    = plotdata->plot_pr.xyratio;
   equalscale = plotdata->plot_pr.equalscale;
   fitpage    = plotdata->plot_pr.fitpage;
   xscale     = plotdata->plot_pr.xscale; if (xscale <= 0) xscale = 1.0;
   yscale     = plotdata->plot_pr.yscale; if (yscale <= 0) yscale = 1.0;
   overlay    = plotdata->plot_pr.overlay;
   innerticks = plotdata->plot_pr.innerticks;
   clip       = CN_TRUE;
   hiddenline = plotdata->view_pr->hiddenline;
   plotlabels = PXquery_labels(plotdata);

   /* Probability plot ? */
   probability_plot = CN_FALSE;
   if (plotdata->plotformat == CN_PROBABILITY_PLOT) {
      probability_plot = CN_TRUE;
      equalscale       = CN_FALSE;
      ylog             = CN_FALSE;
   }
 
   /* Histogram plot ? */
   histogram_plot = CN_FALSE;
   if (plotdata->plotformat == CN_HISTOGRAM_PLOT) {
      histogram_plot   = CN_TRUE;
      equalscale       = CN_FALSE;
      xlog             = CN_FALSE;
      xabs             = CN_FALSE;
   }
 
   /* Barchart plot ? */
   barchart_plot = CN_FALSE;
   if (plotdata->plotformat == CN_BARCHART_PLOT) {
      barchart_plot    = CN_TRUE;
      equalscale       = CN_FALSE;
      xlog             = CN_FALSE;
      xabs             = CN_FALSE;
   }

   /* User-specified axis labels? */
   if (plotdata->plot_pr.xlabelhead != NULL) {
      xlog             = CN_FALSE;
      xabs             = CN_FALSE;
   }
   if (plotdata->plot_pr.ylabelhead != NULL) {
      ylog             = CN_FALSE;
      yabs             = CN_FALSE;
   }

   /* xlabel */
   if (plotdata->plot_pr.xlabel == NULL)
      (void) strcpy(xlabel   , CN_DEF_XLABEL);
   else
      (void) strcpy(xlabel   , plotdata->plot_pr.xlabel);
   if (xscale != 1.0) {
      (void) sprintf(label," (x %g)",xscale);
      (void) strcat(xlabel, label);
   }
 
   /* ylabel */
   if (plotdata->plot_pr.ylabel == NULL)
      (void) strcpy(ylabel   , CN_DEF_YLABEL);
   else
      (void) strcpy(ylabel   , plotdata->plot_pr.ylabel);
   if (yscale != 1.0) {
      (void) sprintf(label," (x %g)",yscale);
      (void) strcat(ylabel, label);
   }
 
   /* Toplabel */
   if (plotdata->plot_pr.toplabel == NULL)
      (void) strcpy(toplabel , CN_DEF_TLABEL);
   else
      (void) strcpy(toplabel , plotdata->plot_pr.toplabel);

    
   /* Comment & subtitle */
   comment = plotdata->plot_pr.comment;
   subtitle= plotdata->plot_pr.subtitle;

   /*
    * The plot boundaries of the plot are given by P->plot_pr->vxmin, etc,
    * which we define as the "viewport" in world coordinates.
    * This is what the user should see, i.e. a plot bounded by vxmin,vxmax.
    *
    * The plotset however has another set of boundaries given by the
    * overlap of all the datasets contained within the plotset.
    * This boundary is used only if the viewport is in error.
    * For example if vxmin=vxmax, then vxmin<=bxmin, vxmax<=bxmax.
    */

   /*
    * Find the plotset boundary from its component datasets
    */
   /* Reset the plot boundaries */
   CNset_plotset_boundaries(plotdata,
                            &bxmin, &bxmax, &bymin, &bymax, &bzmin, &bzmax);

   /* The viewport */
   xmin = plotdata->plot_pr.vxmin;
   xmax = plotdata->plot_pr.vxmax;
   ymin = plotdata->plot_pr.vymin;
   ymax = plotdata->plot_pr.vymax;
   zmin = plotdata->plot_pr.vzmin;
   zmax = plotdata->plot_pr.vzmax;

   zmin = bzmin;
   zmax = bzmax;

   /*
    * Check and fix the viewport
    */
   PXcheck_viewport(&xmin,&xmax,bxmin,bxmax,&xlog,xabs,CN_FALSE,"x");
   plotdata->plot_pr.vxmax = xmax;
   plotdata->plot_pr.vxmin = xmin;
   plotdata->plot_pr.xlog  = xlog;

   PXcheck_viewport(&ymin,&ymax,bymin,bymax,&ylog,yabs,probability_plot,"y");
   plotdata->plot_pr.vymax = ymax;
   plotdata->plot_pr.vymin = ymin;
   plotdata->plot_pr.ylog  = ylog;

   /*
    * If log axes then convert the boundary values.
    * The min and max boundaries are converted to log-values.
    */
   PXconvert_viewport_to_log(&lxmin, &lxmax, xmin, xmax, xlog, xautorange);
   PXconvert_viewport_to_log(&lymin, &lymax, ymin, ymax, ylog, yautorange);
   xmin = lxmin;
   xmax = lxmax;
   ymin = lymin;
   ymax = lymax;

   /* Save the plot viewport */
   plotdata->plot_pr.pxmin = xmin;
   plotdata->plot_pr.pxmax = xmax;
   plotdata->plot_pr.pymin = ymin;
   plotdata->plot_pr.pymax = ymax;

   /* Equal-scale doen't make sense for a non-linear plot */
   if (((xlog && !ylog) || (!xlog && ylog)) && equalscale) {
      equalscale = CN_FALSE;
      fitpage    = CN_TRUE;
   }

   /* Initialize - Pgxmin, Pgxmax, Pgymin, Pgymax are page dimensions */
   Pxmin = Pgxmin + 1.1*scale*PBDR_DIM;
   Pxmax = Pgxmax - 1.0*scale*PBDR_DIM;
   Pymin = Pgymin + 1.0*scale*PBDR_DIM;
   Pymax = Pgymax - 1.2*scale*PBDR_DIM; /* Leave a bit of room for labels */
   if (plotlabels) {
      /* plot_pr.slabellen defines the additional space needed for the labels */
      Pxmax = Pxmax - 1.0*scale*plotdata->plot_pr.slabellen;
   }
   Pxwid = Pxmax - Pxmin;
   Pywid = Pymax - Pymin;
   if (Pxwid < 0) {                     /* Very unlikely to happen... */
      Pxmin = Pgxmin;
      Pxmax = Pgxmax;
      Pxwid = Pxmax - Pxmin;
   }
   if (Pywid < 0) {                     /* Very unlikely to happen... */
      Pymin = Pgymin;
      Pymax = Pgymax;
      Pywid = Pymax - Pymin;
   }

   if (equalscale) {
      /* Fit the plot inside the window */
      dx = xmax - xmin;
      dy = ymax - ymin;
      ratio = dy/dx;
      if (ratio <= (Pywid/Pxwid)) {
         /* decrease Pywid : Pywid  = Pxwid*ratio */
         Pymax = Pymax - 0.5*(Pywid - Pxwid*ratio);
         Pymin = Pymin + 0.5*(Pywid - Pxwid*ratio);
      } else {
         /* decrease xwid : Pxwid  = Pywid/ratio */
         Pxmax = Pxmax - 0.5*(Pxwid - Pywid/ratio);
         Pxmin = Pxmin + 0.5*(Pxwid - Pywid/ratio);
         Pxmax = Pxmin + Pywid/ratio;
      }

   /*EMPTY*/
   } else if (fitpage) {
      /* The plot fills the whole page */
      ;

   } else {
      /* The plot must fit inside the window */
      if (Pxwid*xyratio < Pywid) {
         /* decrease Pywid : Pywid  = Pxwid*xyratio */
         Pymax = Pymax - 0.5*(Pywid - Pxwid*xyratio);
         Pymin = Pymin + 0.5*(Pywid - Pxwid*xyratio);
      } else {
         /* decrease Pxwid : Pxwid  = Pywid/ratio */
         Pxmax = Pxmax - 0.5*(Pxwid - Pywid/xyratio);
         Pxmin = Pxmin + 0.5*(Pxwid - Pywid/xyratio);
         Pxmax = Pxmin + Pywid/xyratio;
      }
   }

   /* initialize the scaling factors */
   dx = xmax - xmin;
   dy = ymax - ymin;
   idx = (Pxmax-Pxmin)/dx;
   jdy = (Pymax-Pymin)/dy; 

   /* 
    * Now do the PostScript plot 
    */
   drawPSplot2D();

}

/*
 * Draw the plot in POSTSCRIPT
 */
static void drawPSplot2D()
{
   int    ltmp=6;

   /* Draw the RAW Postscript plot */

   /* Set contour levels for the contour-type datasets */
   cstephead = NULL;
   csteptail = NULL;
   PXquery_contours(plotdata,&cstephead,&csteptail,&contour_dptr);

   /* Draw the axes */
   axesPS2D();

   /* clip if necessary */
   if (clip) (void) fprintf(ips,"gsave\n");
   if (clip) (void) fprintf(ips,"rectanglepath clip\n");

   /* Draw the plot */
   plotPS2D();

   /* Redraw the boundary */
   boundaryPS2D(&ltmp);

   /* Unclip */
   if (clip) (void) fprintf(ips,"grestore\n");

   /* Draw the annotations */
   annotatePS2D();

   /* Delete contour step-levels */
   CNdelete_contstep_list(&cstephead, &csteptail);
}

/*
 * draw the boundary, axes and labels of the plot and initialize its size
 */
static void axesPS2D()
{
   int    dist, label_len;

   (void) fprintf(ips,"%%------------------------------------------------\n");
   (void) fprintf(ips,"%%--------------Draw the Axes---------------------\n");
   (void) fprintf(ips,"%%------------------------------------------------\n");

   /* define the axes */
   (void) fprintf(ips,"/rectanglepath {\n");
   (void) fprintf(ips,"newpath\n");
   (void) fprintf(ips,"%.2f %.2f moveto\n",Pxmin,Pymin);
   (void) fprintf(ips,"0.0 %.2f rlineto\n",Pymax-Pymin);
   (void) fprintf(ips,"%.2f 0.0 rlineto\n",Pxmax-Pxmin);
   (void) fprintf(ips,"0.0 %.2f rlineto\n",Pymin-Pymax);
   (void) fprintf(ips,"closepath } def\n");

   /* Draw the boundary and tick marks */
   boundaryPS2D(&label_len);

   /* Don't redraw the grid */
   if (grid) grid = CN_FALSE;

   (void) fprintf(ips,"%%------------------------------------------------\n");
   (void) fprintf(ips,"%%--------------Draw the Labels-------------------\n");
   (void) fprintf(ips,"%%------------------------------------------------\n");

   /* Top Label */
   (void) fprintf(ips,"setTopLblFont\n");
   (void) fprintf(ips,"%.2f %.2f moveto ",0.5*(Pxmax+Pxmin),Pymax+30*scale);
   (void) fprintf(ips,"(%s) UPJ CEJ show\n",PXmodifyPS_string(toplabel));

   /* X-axis Label */
   (void) fprintf(ips,"setSideLblFont\n");
   (void) fprintf(ips,"%.2f %.2f moveto ",0.5*(Pxmax+Pxmin),Pymin-25*scale);
   (void) fprintf(ips,"(%s) BOJS CEJ show\n",PXmodifyPS_string(xlabel));

   /* Y-axis Label */
   (void) fprintf(ips,"setSideLblFont\n");
   (void) fprintf(ips,"/Ylabel {");
   dist = 10*label_len + 5;
   if (dist > 70) dist = 70;
   (void) fprintf(ips,"%.2f %.2f moveto ",0.5*(Pymax+Pymin),
                  dist*scale-Pxmin);
   (void) fprintf(ips,"(%s) UPJ CEJ show ",PXmodifyPS_string(ylabel));
   (void) fprintf(ips,"} def\n90 rotate Ylabel -90 rotate\n"); 

   /* Comment */
   if (comment != NULL) {
   (void) fprintf(ips,"setDateLblFont\n");
   (void) fprintf(ips,"%.2f %.2f moveto ",
                  Pgxmax-0.3*scale*PBDR_DIM,Pgymax-0.4*scale*PBDR_DIM);
   (void) fprintf(ips,"(%s) UPJ RIJ show\n",PXmodifyPS_string(comment));
   }

   /* Subtitle */
   if (subtitle != NULL) {
   (void) fprintf(ips,"setAxisLblFont\n");
   (void) fprintf(ips,"%.2f %.2f moveto ",0.5*(Pxmax+Pxmin),Pymax+9*scale);
   (void) fprintf(ips,"(%s) UPJ CEJ show\n",PXmodifyPS_string(subtitle));
   }

   /* Draw the plot labels */
   if (plotlabels) plotPS2D_sidelabels();

   /* Reset the linewidth */
   (void) fprintf(ips,"%.2f setlinewidth\n",0.5*scale);
}

/*
 * Draw the boundary and tick marks of the plot.
 * This is done last 'cos the axes might have been
 * obscured during the course of the plot.
 * The label-length controls the placement of the y-axis label.
 * This routine is called twice - once by axes() and another after plot().
 */
static void boundaryPS2D(label_len)
int  *label_len;
{
   /* 
    * The label-length controls the placement of the y-axis labels 
    */
   (void) fprintf(ips,"%%------------------------------------------------\n");
   (void) fprintf(ips,"%%--------------Draw the TickMarks----------------\n");
   (void) fprintf(ips,"%%------------------------------------------------\n");

   /* Reset color */
   PXsetColorPS(0);

   /* define the axes */
   (void) fprintf(ips,"%g setlinewidth rectanglepath stroke\n",2*scale);

   /* Draw the tick marks and labels on the x-axis */
   (void) fprintf(ips,"%g setlinewidth\n",1*scale);
   if (barchart_plot) {

      /* Tickmarks based on 1st barchart dataset */
      plotPS2D_Bar_xticks();

   } else if (plotdata->plot_pr.xlabelhead != NULL) {
 
      /* Tickmarks based on user-specification */
      plotPS2D_User_xticks();

   } else if (!xautorange && !xlog) {

      /* Linear, fixed tickmarks */
      plotPS2D_xticks();

   } else {
      /* This is the default method */

      /* If xlog then do lots of tiny tickmarks */
      plotPS2D_Auto_xticks();

   }

   /* Draw the tick marks and labels on the y-axis */
   *label_len = 6;
   if (probability_plot) {

      /* Tickmarks in an erfc distribution */
      plotPS2D_Prob_yticks(label_len);

   } else if (plotdata->plot_pr.ylabelhead != NULL) {
 
      /* Tickmarks based on user-specification */
      plotPS2D_User_yticks(label_len);

   } else if (!yautorange && !ylog) {

      /* Linear, fixed tickmarks */
      plotPS2D_yticks(label_len);

   } else {
      /* This is the default method */

      /* If ylog then do lots of tiny tickmarks */
      plotPS2D_Auto_yticks(label_len);
   }
}

/*
 * Draw the x-axis tick marks and labels
 * This is NEVER used with log-scale
 */
static void plotPS2D_xticks()
{
   double  xdx,xintv;
   double  Pxtmp,Pytmp;
   double  vallbl;
   int     i;
   int     explabel, precision;
   PXlabel axislabels[PX_MAX_LABELS];
   int     nlabels=0, lastlabel, doplot2;
 
   /* Useful data */
   xintv = (Pxmax-Pxmin)/(double)xticks;
   xdx   = (xmax-xmin)/(double)xticks;

   /* Tick Marks on the X-axis */
   (void) fprintf(ips,"%% Draw the X-Axis\n");
   (void) fprintf(ips,"setAxisLblFont\n");
   for (i=0; i<=xticks; i++) {
      Pxtmp = Pxmin + xintv*i;

      /* Lower Ticks */
      Pytmp = Pymin;
      (void) fprintf(ips,"%.2f %.2f ",Pxtmp,Pytmp);
      if (innerticks)
      (void) fprintf(ips,"0 %.2f drawline\n", 7*scale);
      else
      (void) fprintf(ips,"0 %.2f drawline\n",-7*scale);

      /* Upper Ticks */
      Pytmp = Pymax;
      (void) fprintf(ips,"%.2f %.2f ",Pxtmp,Pytmp);
      (void) fprintf(ips,"0 %.2f drawline\n",-7*scale);

      /* Put on a label */
      vallbl = xmin  + i*xdx;
      if (xflip) vallbl = xmax - i*xdx;
      if (xlog) vallbl = pow(10.0,vallbl);
      vallbl = vallbl/xscale;
      PXadd_axislabel(axislabels, &nlabels, PX_MAX_LABELS,
                      vallbl, (double)Pxtmp, (double)Pymin);
   }

   /* draw the grid */
   if (grid == CN_TRUE) {
      for (i=1; i<xticks; i++) {
         Pxtmp = Pxmin + xintv*i;
         Pytmp = Pymax;
         (void) fprintf(ips,"[] 1 setdash  ");
         (void) fprintf(ips,"%.2f setlinewidth 0.9 setgray\n",0.5);
         (void) fprintf(ips,"%.2f %.2f ",Pxtmp,Pytmp);
         (void) fprintf(ips,"0 %.2f drawline\n",Pymin-Pymax);
         (void) fprintf(ips,"%.2f setlinewidth 0.0 setgray\n",1.0*scale);
         (void) PXlinetypPS(CN_LN_SOLID,2);
      }
   }

   /* Now work on the labels */
   if (nlabels > 0) {
      PXfind_axis_precision(axislabels, nlabels, &precision, &explabel);
      lastlabel = -1;
      for (i=0; i<nlabels; i++) {
         /*
          * Now plot this ONLY if there is space between the last plotted
          * label
          */
         if (lastlabel < 0) {
            doplot2   = CN_TRUE;
         } else if (!xticklabel_overlap(axislabels[lastlabel],axislabels[i],
                   precision, explabel)) {
            doplot2   = CN_TRUE;
         } else {
            doplot2   = CN_FALSE;
         }
         if (doplot2) {
            plotPS2D_xticklabel(axislabels[i].value,
                                axislabels[i].x,
                                axislabels[i].y,
                                (char*)NULL, precision, explabel);
            lastlabel = i;
         }
      }
   }
}

/*
 * Draw the y-axis tick marks and labels
 * This is NEVER used with log-scale
 */
static void plotPS2D_yticks(label_len)
int  *label_len;
{
   double ydy,yintv;
   double Pxtmp,Pytmp;
   double vallbl;
   int    i, llen = 0;
   int     explabel, precision;
   PXlabel axislabels[PX_MAX_LABELS];
   int     nlabels=0;

   /* Useful data */
   yintv = (Pymax-Pymin)/(double)yticks;
   ydy   = (ymax-ymin)/(double)yticks;

   /* Tick Marks on the Y-axis */
   (void) fprintf(ips,"%% Draw the Y-Axis\n");
   (void) fprintf(ips,"setAxisLblFont\n");
   for (i=0; i<=yticks; i++) {
      Pytmp = Pymin + yintv*i;

      /* Left-side Ticks */
      Pxtmp = Pxmin;
      (void) fprintf(ips,"%.2f %.2f ",Pxtmp,Pytmp);
      if (innerticks)
      (void) fprintf(ips,"%.2f 0 drawline\n", 7*scale);
      else
      (void) fprintf(ips,"%.2f 0 drawline\n",-7*scale);

      /* Right-side Ticks */
      Pxtmp = Pxmax;
      (void) fprintf(ips,"%.2f %.2f ",Pxtmp,Pytmp);
      (void) fprintf(ips,"%.2f 0 drawline\n",-7*scale);

      /* Put on a label */
      vallbl = ymin  + i*ydy;
      if (yflip) vallbl = ymax - i*ydy;
      if (ylog) vallbl = pow(10.0,vallbl);
      vallbl = vallbl/yscale;
      PXadd_axislabel(axislabels, &nlabels, PX_MAX_LABELS,
                      vallbl, (double)Pxmin, (double)Pytmp);
   }

   /* draw the grid */
   if (grid == CN_TRUE) {
      for (i=1; i<yticks; i++) {
         Pytmp = Pymin + yintv*i;
         Pxtmp = Pxmax;
         (void) fprintf(ips,"[] 1 setdash  ");
         (void) fprintf(ips,"%.2f setlinewidth 0.9 setgray\n",0.5);
         (void) fprintf(ips,"%.2f %.2f ",Pxtmp,Pytmp);
         (void) fprintf(ips,"%.2f 0 drawline\n",Pxmin-Pxmax);
         (void) fprintf(ips,"%.2f setlinewidth 0.0 setgray\n",1.0*scale);
         (void) PXlinetypPS(CN_LN_SOLID,1);
      }
   }

   /* Now work on the labels */
   if (nlabels > 0) {
      PXfind_axis_precision(axislabels, nlabels, &precision, &explabel);
      for (i=0; i<nlabels; i++) {
         plotPS2D_yticklabel(axislabels[i].value,
                             axislabels[i].x,
                             axislabels[i].y,
                             (char*)NULL, precision, explabel, &llen);
      }
   }

   /* return the label length */
   if (llen > *label_len) *label_len = llen;
}


/* 
 * Draw the x-axis tick marks and labels with automatic ranging 
 */
static void plotPS2D_Auto_xticks()
{
   void    PXget_autorange();

   double  dmin, dmax, dmin2, dmax2;
   double  dtmp, dt, delta;
   double  Pxtmp;
   int     i, itick, nticks, tck;
   int     explabel, precision;
   PXlabel axislabels[PX_MAX_LABELS];
   int     nlabels=0, doplot, doplot2, lastlabel;
   double  vallbl;

   /*
    * LINEAR SCALE
    * Start with a linear range (e.g. xmin = 0.05  xmax = 50.0)
    * Autoranging              =>     dmin = 0.05  dmax = 50.0
    *                                 dmin2= 0.0   dmax2= 50.0
    *                                 delta= 10.0
    *               => get scales at  x=0,10,20,30,40,50
    *
    * LOG SCALES
    * Start with a linear range (e.g. xmin = 0.05  xmax = 50.0)
    * For log scale, these numbers are converted first to logarithmic values
    * during initialization     (e.g. xmin = -1.3  xmax = 1.7  )
    * The autoranging forces =>       dmin = -1.3  dmax = 1.7
    *                                 dmin2= -2.0  dmax2= 2.0
    *                                 delta=  1.0
    *               => get scales at  x=-2,-1,0,1,2
    * So all I have to do is plot the scales in log10 increments
    */

   /*
    * This does not work too well if (xmax-xmin) << xmin
    * because of floating point errors.
    */

   /* 
    * useful data 
    * X = Pxmin + (x-xmin)*(Pxmax-Pxmin)/(xmax-xmin)
    * Y = Pymin + (y-ymin)*(Pymax-Pymin)/(ymax-ymin)
    */
   
   /* Tick Marks on the X-axis */
   (void) fprintf(ips,"%% Draw the X-Axis\n");
   (void) fprintf(ips,"setAxisLblFont\n");

   /* Get the rounded intervals */
   PXget_autorange(xmin,xmax,
                  &dmin,&dmax,&dmin2,&dmax2,&delta,xlog,xautorange);

   /* If dmin2=dmin then subtract delta from dmin2 so that dmin2 < dmin */
   /* This is used to get the leftmost tick label on the plot           */
   if (dmin2==dmin) dmin2 -= delta;

   /* Put on the first tick */
   vallbl = dmin;
   if (xlog) vallbl = pow(10.0,vallbl);
   vallbl = vallbl/xscale;
   if (!xflip) {
      PXadd_axislabel(axislabels, &nlabels, PX_MAX_LABELS,
                      vallbl, (double)Pxmin, (double)Pymin);
   } else {
      PXadd_axislabel(axislabels, &nlabels, PX_MAX_LABELS,
                      vallbl, (double)Pxmax, (double)Pymin);
   }

   /* Draw the tickmarks from dmin2 to dmax2 - dtmp is distance from dmin2 */
   if (delta <= 0.0) nticks = 0;
   else              nticks = (int)((dmax2 - dmin2)/delta) + 1;
   for (itick=0; itick<nticks; itick++) {
      dtmp = itick*delta;
      for (i=1; i<=10; i++) {
         /* distance from dtmp - actual phy loc is dt+dtmp+dmin2 */
         dt = i*0.1*delta;
         if (xlog) dt = log10((double)i); 
         if (xlog && i==1) continue;  /* Don't do this case */

         /* Translate to plot-coordinates */
         Pxtmp = Pxmin + ((dtmp+(dmin2-dmin))*idx) + (dt*idx);
         if (xflip)
         Pxtmp = Pxmax - ((dtmp+(dmin2-dmin))*idx) - (dt*idx);

         /* Don't draw if this is outside the plot boundaries */
         if (Pxtmp < Pxmin-0.01*scale || Pxtmp > Pxmax+0.01*scale) continue;

         /* Draw the grid */
         if (grid && (Pxtmp > Pxmin+0.01*scale && Pxtmp < Pxmax-0.01*scale)) {
            /*
             * PS major grid is the same as the PS minor grid 
             */
            if (i==10) {
               /* Major grid */
               (void) fprintf(ips,"%.2f setlinewidth 0.9 setgray\n",0.5);
               (void) fprintf(ips,"%.2f %.2f ",Pxtmp,Pymin);
               (void) fprintf(ips,"0 %.2f drawline\n",Pymax-Pymin);
               (void) fprintf(ips,"%.2f setlinewidth 0.0 setgray\n",1.0*scale);
            } else if (scale > 0.4) {
               /* Minor grid - don't draw the minor grid for small plots */
               (void) fprintf(ips,"%.2f setlinewidth 0.9 setgray\n",0.5);
               (void) fprintf(ips,"%.2f %.2f ",Pxtmp,Pymin);
               (void) fprintf(ips,"0 %.2f drawline\n",Pymax-Pymin);
               (void) fprintf(ips,"%.2f setlinewidth 0.0 setgray\n",1.0*scale);
            }
         }

         /* The length of the tick depends on i */
         if      (i==10) tck = 7;  /* major tick */
         else if (i==5 ) tck = 5;  /* major sub-tick */
         else            tck = 3;  /* minor tick */

         /* Draw the ticks */

         /* Lower Ticks */
         (void) fprintf(ips,"%.2f %.2f ",Pxtmp,Pymin);
         if (innerticks) 
         (void) fprintf(ips,"0 %.2f drawline\n", tck*scale);
         else
         (void) fprintf(ips,"0 %.2f drawline\n",-tck*scale);

         /* Upper Ticks */
         (void) fprintf(ips,"%.2f %.2f ",Pxtmp,Pymax);
         (void) fprintf(ips,"0 %.2f drawline\n",-tck*scale);

         /* Draw labels */
         if (i==10) {
            vallbl = dt+dtmp+dmin2;
            if (fabs(vallbl/delta) < 1e-10) vallbl=0.0; /* If x~=0 print 0 */
            if (xlog) vallbl = pow(10.0,vallbl);
            vallbl = vallbl/xscale;
            PXadd_axislabel(axislabels, &nlabels, PX_MAX_LABELS,
                            vallbl, (double)Pxtmp, (double)Pymin);
         }
      }
   }

   /* Put on the last tick */
   vallbl = dmax;
   if (xlog) vallbl = pow(10.0,vallbl);
   vallbl = vallbl/xscale;
   if (!xflip) {
      PXadd_axislabel(axislabels, &nlabels, PX_MAX_LABELS,
                      vallbl, (double)Pxmax, (double)Pymin);
   } else {
      PXadd_axislabel(axislabels, &nlabels, PX_MAX_LABELS,
                      vallbl, (double)Pxmin, (double)Pymin);
   }

   /* Now work on the labels */
   if (nlabels > 0) {
      PXfind_axis_precision(axislabels, nlabels, &precision, &explabel);
      lastlabel = -1;
      for (i=0; i<nlabels; i++) {
         doplot = CN_TRUE;
         if (i==0) {
            /* Don't plot the 1st label if it is too close to the second */
            if (nlabels >= 2) {
               if (EQUAL(axislabels[i].value, axislabels[i+1].value))
                  doplot = CN_FALSE;
               else if (xticklabel_overlap(axislabels[i],axislabels[i+1],
                        precision, explabel))
                  doplot = CN_FALSE;
            }
         } else if (i==nlabels-1) {
            /* Don't plot the 1st label if it is too close to the second */
            if (nlabels >= 2) {
               if (EQUAL(axislabels[i].value, axislabels[i-1].value))
                  doplot = CN_FALSE;
               else if (xticklabel_overlap(axislabels[i],axislabels[i-1],
                        precision, explabel))
                  doplot = CN_FALSE;
            }
         }
         if (doplot) {
            /*
             * Now plot this ONLY if there is space between the last plotted
            * label
             */
            if (lastlabel < 0) {
               doplot2   = CN_TRUE;
            } else if (!xticklabel_overlap(axislabels[lastlabel],axislabels[i],
                       precision, explabel)) {
               doplot2   = CN_TRUE;
            } else {
               doplot2   = CN_FALSE;
            }
            if (doplot2) {
               plotPS2D_xticklabel(axislabels[i].value,
                                   axislabels[i].x,
                                   axislabels[i].y,
                                   (char*)NULL, precision, explabel);
               lastlabel = i;
            }
         }
      }
   }
}
 

/* 
 * Draw the y-axis tick marks and labels with automatic ranging 
 */
static void plotPS2D_Auto_yticks(label_len)
int  *label_len;
{
   void   PXget_autorange();

   double  dmin, dmax, dmin2, dmax2;
   double  dtmp, dt, delta;
   double  Pytmp;
   int     i, itick, nticks, tck;
   int     llen=0;
   int     explabel, precision;
   PXlabel axislabels[PX_MAX_LABELS];
   int     nlabels=0, doplot;
   double  vallbl;

   /*
    * LINEAR SCALE
    * Start with a linear range (e.g. xmin = 0.05  xmax = 50.0)
    * Autoranging              =>     dmin = 0.05  dmax = 50.0
    *                                 dmin2= 0.0   dmax2= 50.0
    *                                 delta= 10.0
    *               => get scales at  x=0,10,20,30,40,50
    *
    * LOG SCALES
    * Start with a linear range (e.g. xmin = 0.05  xmax = 50.0)
    * For log scale, these numbers are converted first to logarithmic values
    * during initialization     (e.g. xmin = -1.3  xmax = 1.7  )
    * The autoranging forces =>       dmin = -1.3  dmax = 1.7
    *                                 dmin2= -2.0  dmax2= 2.0
    *                                 delta=  1.0
    *               => get scales at  x=-2,-1,0,1,2
    * So all I have to do is plot the scales in log10 increments
    */

   /*
    * This does not work too well if (xmax-xmin) << xmin
    * because of floating point errors.
    */

   /* 
    * useful data 
    * X = Pxmin + (x-xmin)*(Pxmax-Pxmin)/(xmax-xmin)
    * Y = Pymin + (y-ymin)*(Pymax-Pymin)/(ymax-ymin)
    */
   
   /* Tick Marks on the Y-axis */
   (void) fprintf(ips,"%% Draw the Y-Axis\n");
   (void) fprintf(ips,"setAxisLblFont\n");

   /* Get the rounded intervals */
   PXget_autorange(ymin,ymax,
                  &dmin,&dmax,&dmin2,&dmax2,&delta,ylog,yautorange);

   /* If dmin2=dmin then subtract delta from dmin2 so that dmin2 < dmin */
   /* This is used to get the leftmost tick label on the plot           */
   if (dmin2==dmin) dmin2 -= delta;

   /* Put on the first tick */
   vallbl = dmin;
   if (ylog) vallbl = pow(10.0,vallbl);
   vallbl = vallbl/yscale;
   if (!yflip) {
      PXadd_axislabel(axislabels, &nlabels, PX_MAX_LABELS,
                      vallbl, (double)Pxmin, (double)Pymin);
   } else {
      PXadd_axislabel(axislabels, &nlabels, PX_MAX_LABELS,
                      vallbl, (double)Pxmin, (double)Pymax);
   }

   /* Draw the tickmarks from dmin2 to dmax2 - dtmp is distance from dmin2 */
   if (delta <= 0.0) nticks = 0;
   else              nticks = (int)((dmax2 - dmin2)/delta) + 1;
   for (itick=0; itick<nticks; itick++) {
      dtmp = itick*delta;
      for (i=1; i<=10; i++) {
         /* distance from dtmp - actual phy loc is dt+dtmp+dmin2 */
         dt = i*0.1*delta;
         if (ylog) dt = log10((double)i); 
         if (ylog && i==1) continue;  /* Don't do this case */

         /* Translate to plot-coordinates */
         Pytmp = Pymin + (int)((dtmp+(dmin2-dmin))*jdy) + (int)(dt*jdy);
         if (yflip)
         Pytmp = Pymax - (int)((dtmp+(dmin2-dmin))*jdy) - (int)(dt*jdy);

         /* Don't draw if this is outside the plot boundaries */
         if (Pytmp < Pymin-0.01*scale || Pytmp > Pymax+0.01*scale) continue;

         /* Draw the grid */
         if (grid && (Pytmp > Pymin+0.01*scale && Pytmp < Pymax+0.01*scale)) {
            /*
             * PS major grid is the same as the PS minor grid 
             */
            if (i==10) {
               /* Major grid */
               (void) fprintf(ips,"%.2f setlinewidth 0.9 setgray\n",0.5);
               (void) fprintf(ips,"%.2f %.2f ",Pxmin,Pytmp);
               (void) fprintf(ips,"%.2f 0 drawline\n",Pxmax-Pxmin);
               (void) fprintf(ips,"%.2f setlinewidth 0.0 setgray\n",1.0*scale);
            } else if (scale > 0.4) {
               /* Minor grid - don't draw the minor grid for small plots */
               (void) fprintf(ips,"%.2f setlinewidth 0.9 setgray\n",0.5);
               (void) fprintf(ips,"%.2f %.2f ",Pxmin,Pytmp);
               (void) fprintf(ips,"%.2f 0 drawline\n",Pxmax-Pxmin);
               (void) fprintf(ips,"%.2f setlinewidth 0.0 setgray\n",1.0*scale);
            }
         }

         /* The length of the tick depends on i */
         if      (i==10) tck = 7;  /* major tick */
         else if (i==5 ) tck = 5;  /* major sub-tick */
         else            tck = 3;  /* minor tick */

         /* Draw the ticks */

         /* Left-side Ticks */
         (void) fprintf(ips,"%.2f %.2f ",Pxmin,Pytmp);
         if (innerticks)
         (void) fprintf(ips,"%.2f 0 drawline\n", tck*scale);
         else
         (void) fprintf(ips,"%.2f 0 drawline\n",-tck*scale);

         /* Right-side Ticks */
         (void) fprintf(ips,"%.2f %.2f ",Pxmax,Pytmp);
         (void) fprintf(ips,"%.2f 0 drawline\n",-tck*scale);

         /* Draw labels */
         if (i==10) {
            vallbl = dt+dtmp+dmin2;
            if (fabs(vallbl/delta) < 1e-10) vallbl=0.0; /* If y~=0 print 0 */
            if (ylog) vallbl = pow(10.0,vallbl);
            vallbl = vallbl/yscale;
            PXadd_axislabel(axislabels, &nlabels, PX_MAX_LABELS,
                            vallbl, (double)Pxmin, (double)Pytmp);
         }
      }
   }

   /* Put on the last tick */
   vallbl = dmax;
   if (ylog) vallbl = pow(10.0,vallbl);
   vallbl = vallbl/yscale;
   if (!yflip) {
      PXadd_axislabel(axislabels, &nlabels, PX_MAX_LABELS,
                      vallbl, (double)Pxmin, (double)Pymax);
   } else {
      PXadd_axislabel(axislabels, &nlabels, PX_MAX_LABELS,
                      vallbl, (double)Pxmin, (double)Pymin);
   }
 
   /* Now work on the labels */
   if (nlabels > 0) {
      PXfind_axis_precision(axislabels, nlabels, &precision, &explabel);
      for (i=0; i<nlabels; i++) {
         doplot = CN_TRUE;
         if (i==0) {
            /* Don't plot the 1st label if it is too close to the second */
            if (nlabels >= 2) {
               if (EQUAL(axislabels[i].value, axislabels[i+1].value))
                  doplot = CN_FALSE;
               else if (fabs(axislabels[i].y-axislabels[i+1].y) < 20*scale)
                  doplot = CN_FALSE;
            }
         } else if (i==nlabels-1) {
            /* Don't plot the 1st label if it is too close to the second */
            if (nlabels >= 2) {
               if (EQUAL(axislabels[i].value, axislabels[i-1].value))
                  doplot = CN_FALSE;
               else if (fabs(axislabels[i].y-axislabels[i-1].y) < 20*scale)
                  doplot = CN_FALSE;
            }
         }
         if (doplot)
         plotPS2D_yticklabel(axislabels[i].value,
                             axislabels[i].x,
                             axislabels[i].y,
                             (char*)NULL, precision, explabel, &llen);
      }
   }
 
   /* return the label length */
   if (llen > *label_len) *label_len = llen;
}


/*
 * Draw the x-axis tick marks and labels using user-specified axis labels
 * This is NEVER used with log-scale
 */
static void plotPS2D_User_xticks()
{
   CNaxislabelptr Aptr;
   double         xval;
   double         Pxtmp;
   char           label[CN_MAXCHAR];
 
   /*
    * useful data
    * X = Pxmin + (x-xmin)*(Pxmax-Pxmin)/(xmax-xmin)
    * Y = Pymin + (y-ymin)*(Pymax-Pymin)/(ymax-ymin)
    */
 
   /* Tick Marks on the X-axis */
   (void) fprintf(ips,"%% Draw the X-Axis\n");
   (void) fprintf(ips,"setAxisLblFont\n");
 
   /* Go thru the axis positions and plot labels */
   for (Aptr=plotdata->plot_pr.xlabelhead; Aptr!=NULL; Aptr=Aptr->next) {
      xval = Aptr->pos;
 
      /* Translate to plot-coordinates */
      Pxtmp = Pxmin + (xval - xmin)*idx;
      if (xflip)
      Pxtmp = Pxmax - (xval - xmin)*idx;
 
      /* Don't draw if this is outside the plot boundaries */
      if (Pxtmp < Pxmin-0.01*scale || Pxtmp > Pxmax+0.01*scale) continue;
 
      /* Put on labels */
      (void) strcpy(label, Aptr->name);
      plotPS2D_xticklabel(xval, (double)Pxtmp, (double)Pymin,
                         label, 4, CN_FALSE);
 
      /* Put on tick marks */

      /* Lower Ticks */
      (void) fprintf(ips,"%.2f %.2f ",Pxtmp,Pymin);
      if (innerticks)
      (void) fprintf(ips,"0 %.2f drawline\n", 7*scale);
      else
      (void) fprintf(ips,"0 %.2f drawline\n",-7*scale);

      /* Upper Ticks */
      (void) fprintf(ips,"%.2f %.2f ",Pxtmp,Pymax);
      (void) fprintf(ips,"0 %.2f drawline\n",-7*scale);

      /* Draw the grid */
      if (grid) {
         (void) fprintf(ips,"%.2f setlinewidth 0.9 setgray\n",0.5);
         (void) fprintf(ips,"%.2f %.2f ",Pxtmp,Pymin);
         (void) fprintf(ips,"0 %.2f drawline\n",Pymax-Pymin);
         (void) fprintf(ips,"%.2f setlinewidth 0.0 setgray\n",1.0*scale);
      }
   }
}


/*
 * Draw the y-axis tick marks and labels using user-specified axis labels
 * This is NEVER used with log-scale
 */
static void plotPS2D_User_yticks(label_len)
int *label_len;
{
   CNaxislabelptr Aptr;
   double         yval;
   double         Pytmp;
   char           label[CN_MAXCHAR];
   int            llen = 0;
 
   /*
    * useful data
    * X = Pxmin + (x-xmin)*(Pxmax-Pxmin)/(xmax-xmin)
    * Y = Pymin + (y-ymin)*(Pymax-Pymin)/(ymax-ymin)
    */
 
   /* Tick Marks on the X-axis */
   (void) fprintf(ips,"%% Draw the Y-Axis\n");
   (void) fprintf(ips,"setAxisLblFont\n");
 
   /* Go thru the axis positions and plot labels */
   for (Aptr=plotdata->plot_pr.ylabelhead; Aptr!=NULL; Aptr=Aptr->next) {
      yval = Aptr->pos;
 
      /* Translate to plot-coordinates */
      Pytmp = Pymin + (yval - ymin)*jdy;
      if (yflip)
      Pytmp = Pymax - (yval - ymin)*jdy;
 
      /* Don't draw if this is outside the plot boundaries */
      if (Pytmp < Pymin-0.01*scale || Pytmp > Pymax+0.01*scale) continue;
 
      /* Put on labels */
      (void) strcpy(label, Aptr->name);
      plotPS2D_yticklabel(yval, (double)Pxmin, (double)Pytmp,
                         label, 4, CN_FALSE, &llen);
 
      /* Put on tick marks */

      /* Left-side Ticks */
      (void) fprintf(ips,"%.2f %.2f ",Pxmin,Pytmp);
      if (innerticks)
      (void) fprintf(ips,"%.2f 0 drawline\n", 7*scale);
      else
      (void) fprintf(ips,"%.2f 0 drawline\n",-7*scale);

      /* Right-side Ticks */
      (void) fprintf(ips,"%.2f %.2f ",Pxmax,Pytmp);
      (void) fprintf(ips,"%.2f 0 drawline\n",-7*scale);

      /* Draw the grid */
      if (grid) {
         (void) fprintf(ips,"%.2f setlinewidth 0.9 setgray\n",0.5);
         (void) fprintf(ips,"%.2f %.2f ",Pxmin,Pytmp);
         (void) fprintf(ips,"%.2f 0 drawline\n",Pxmax-Pxmin);
         (void) fprintf(ips,"%.2f setlinewidth 0.0 setgray\n",1.0*scale);
      }
   }

   /* return the label length */
   if (llen > *label_len) *label_len = llen;
}


/*
 * Draw the y-axis tick marks and labels on a probability plot scale
 * i.e. an error-function distribution
 */
static void plotPS2D_Prob_yticks(label_len)
int  *label_len;
{
   double upper_lim, lower_lim, x, yprob, delta;
   double Pytmp;
   int    i, j, error;
   int    tck;
   int    llen=0;

   /*
    * Figure out the axis position (0-1) for
    * x=0.0001, 0.001, 0.01, 0.1,
    *   0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8,
    *   0.9, 0.99, 0.999, 0.9999
    *
    * x=0.0001 => 0
    * x=0.9999 => 1
    *
    * lower_lim => Pxmin    upper_lim => Pxmax
    *   (X-Pxmin)/(Pxmax-Pxmin) = (xprob-lower_lim)/(upper_lim-lower_lim)
    *
    * lower_lim => Pymin    upper_lim => Pymax
    *   (Y-Pymin)/(Pymax-Pymin) = (yprob-lower_lim)/(upper_lim-lower_lim)
    */

   /* Tick Marks on the Y-axis */
   (void) fprintf(ips,"%% Draw the Y-Axis\n");
   (void) fprintf(ips,"setAxisLblFont\n");

   /* The lower and upper limits of the probability scale */
   lower_lim = CNnorm_vert_distance(ymin, &error);
   upper_lim = CNnorm_vert_distance(ymax, &error);

   /* Calculate axes from 0.000001 (0.0001%) to 0.10 (10%) */
   delta = 0.000001;
   for (j=0; j<5; j++) {
      x = delta;
      for (i=0; i<9; i++) {
         /* Get the value on the probability scale */
         yprob = CNnorm_vert_distance(x, &error);
 
         /* Translate to plot-coordinates */
         Pytmp = Pymin + (Pymax-Pymin)*(yprob-lower_lim)/(upper_lim-lower_lim);
         if (yflip)
         Pytmp = Pymax + (Pymin-Pymax)*(yprob-lower_lim)/(upper_lim-lower_lim);

         /* Don't draw if this is outside the plot boundaries */
         if (Pytmp > Pymin-0.01*scale && Pytmp < Pymax+0.01*scale) {

            /* Draw the grid */
            if (grid && (Pytmp>Pymin+0.01*scale && Pytmp<Pymax+0.01*scale)) {
               /*
                * PS major grid is the same as the PS minor grid 
                */
               if (i==0) {
                  /* Major grid */
                  (void) fprintf(ips,"%.2f setlinewidth 0.9 setgray\n",0.5);
                  (void) fprintf(ips,"%.2f %.2f ",Pxmin,Pytmp);
                  (void) fprintf(ips,"%.2f 0 drawline\n",Pxmax-Pxmin);
                  (void) fprintf(ips,"%.2f setlinewidth 0.0 setgray\n",
                                 1.0*scale);
               } else if (scale > 0.4) {
                  /* Minor grid - don't draw the minor grid for small plots */
                  (void) fprintf(ips,"%.2f setlinewidth 0.9 setgray\n",0.5);
                  (void) fprintf(ips,"%.2f %.2f ",Pxmin,Pytmp);
                  (void) fprintf(ips,"%.2f 0 drawline\n",Pxmax-Pxmin);
                  (void) fprintf(ips,"%.2f setlinewidth 0.0 setgray\n",
                                 1.0*scale);
               }
            }

            /* The length of the tick depends on i */
            if      (i==0) tck = 7;  /* major tick */
            else if (i==5) tck = 5;  /* major sub-tick */
            else           tck = 3;  /* minor tick */

            /* Draw the ticks */

            /* Left-side Ticks */
            (void) fprintf(ips,"%.2f %.2f ",Pxmin,Pytmp);
            if (innerticks)
            (void) fprintf(ips,"%.2f 0 drawline\n", tck*scale);
            else
            (void) fprintf(ips,"%.2f 0 drawline\n",-tck*scale);

            /* Right-side Ticks */
            (void) fprintf(ips,"%.2f %.2f ",Pxmax,Pytmp);
            (void) fprintf(ips,"%.2f 0 drawline\n",-tck*scale);

            /* Draw labels */
            if (i==0)
            plotPS2D_yticklabel(x*100.0,(double)Pxmin,(double)Pytmp,
                               (char*)NULL, 6, CN_FALSE, &llen);
         }
         /* Increment */
         x += delta;
      }
      /* Increment */
      delta = delta*10.0;
   }

   /* Calculate axes from 0.1 (10%) to 0.90 (90%) */
   delta = 0.01;
   for (j=1; j<9; j++) {
      x = 0.1*j;
      for (i=0; i<10; i++) {
         /* Get the value on the probability scale */
         yprob = CNnorm_vert_distance(x, &error);
 
         /* Translate to plot-coordinates */
         Pytmp = Pymin + (Pymax-Pymin)*(yprob-lower_lim)/(upper_lim-lower_lim);
         if (yflip)
         Pytmp = Pymax + (Pymin-Pymax)*(yprob-lower_lim)/(upper_lim-lower_lim);

         /* Don't draw if this is outside the plot boundaries */
         if (Pytmp > Pymin-0.01*scale && Pytmp < Pymax+0.01*scale) {

            /* Draw the grid */
            if (grid && (Pytmp>Pymin+0.01*scale && Pytmp<Pymax+0.01*scale)) {
               /*
                * PS major grid is the same as the PS minor grid 
                */
               if (i==0) {
                  /* Major grid */
                  (void) fprintf(ips,"%.2f setlinewidth 0.9 setgray\n",0.5);
                  (void) fprintf(ips,"%.2f %.2f ",Pxmin,Pytmp);
                  (void) fprintf(ips,"%.2f 0 drawline\n",Pxmax-Pxmin);
                  (void) fprintf(ips,"%.2f setlinewidth 0.0 setgray\n",
                                 1.0*scale);
               } else if (scale > 0.4) {
                  if ((ymin < 0.01 && ymax > 0.99 && i%2==0) || 
                      (ymin > 0.01 || ymax < 0.99)) {
                  /* Minor grid - don't draw the minor grid for small plots */
                  (void) fprintf(ips,"%.2f setlinewidth 0.9 setgray\n",0.5);
                  (void) fprintf(ips,"%.2f %.2f ",Pxmin,Pytmp);
                  (void) fprintf(ips,"%.2f 0 drawline\n",Pxmax-Pxmin);
                  (void) fprintf(ips,"%.2f setlinewidth 0.0 setgray\n",
                                 1.0*scale);
                  }
               }
            }

            /* The length of the tick depends on i */
            if      (i==0) tck = 7;  /* major tick */
            else if (i==5) tck = 5;  /* major sub-tick */
            else           tck = 3;  /* minor tick */

            /* Draw the ticks */

            /* Left-side Ticks */
            (void) fprintf(ips,"%.2f %.2f ",Pxmin,Pytmp);
            if (innerticks)
            (void) fprintf(ips,"%.2f 0 drawline\n", tck*scale);
            else
            (void) fprintf(ips,"%.2f 0 drawline\n",-tck*scale);

            /* Right-side Ticks */
            (void) fprintf(ips,"%.2f %.2f ",Pxmax,Pytmp);
            (void) fprintf(ips,"%.2f 0 drawline\n",-tck*scale);

            /* Draw labels */
            if (i==0)
            plotPS2D_yticklabel(x*100.0,(double)Pxmin,(double)Pytmp,
                               (char*)NULL, 4, CN_FALSE, &llen);
         }
         /* Increment */
         x += delta;
      }
   }

   /* Calculate axes from 0.9 (90%) to 0.999999 (99.9999%) */
   delta = 0.01;
   for (j=0; j<6; j++) {
      x = 1.0 - 10.0*delta;
      for (i=0; i<9; i++) {
         /* The loop also goes from 0.99990 to 0.99999 */
         if (x > 0.999999 + CN_SMALL) continue;

         /* Get the value on the probability scale */
         yprob = CNnorm_vert_distance(x, &error);
 
         /* Translate to plot-coordinates */
         Pytmp = Pymin + (Pymax-Pymin)*(yprob-lower_lim)/(upper_lim-lower_lim);
         if (yflip)
         Pytmp = Pymax + (Pymin-Pymax)*(yprob-lower_lim)/(upper_lim-lower_lim);

         /* Don't draw if this is outside the plot boundaries */
         if (Pytmp > Pymin-0.01*scale && Pytmp < Pymax+0.01*scale) {

            /* Draw the grid */
            if (grid && (Pytmp>Pymin+0.01*scale && Pytmp<Pymax+0.01*scale)) {
               /*
                * PS major grid is the same as the PS minor grid 
                */
               if (i==0) {
                  /* Major grid */
                  (void) fprintf(ips,"%.2f setlinewidth 0.9 setgray\n",0.5);
                  (void) fprintf(ips,"%.2f %.2f ",Pxmin,Pytmp);
                  (void) fprintf(ips,"%.2f 0 drawline\n",Pxmax-Pxmin);
                  (void) fprintf(ips,"%.2f setlinewidth 0.0 setgray\n",
                                 1.0*scale);
               } else if (scale > 0.4) {
                  /* Minor grid - don't draw the minor grid for small plots */
                  (void) fprintf(ips,"%.2f setlinewidth 0.9 setgray\n",0.5);
                  (void) fprintf(ips,"%.2f %.2f ",Pxmin,Pytmp);
                  (void) fprintf(ips,"%.2f 0 drawline\n",Pxmax-Pxmin);
                  (void) fprintf(ips,"%.2f setlinewidth 0.0 setgray\n",
                                 1.0*scale);
               }
            }

            /* The length of the tick depends on i */
            if      (i==0) tck = 7;  /* major tick */
            else if (i==5) tck = 5;  /* major sub-tick */
            else           tck = 3;  /* minor tick */

            /* Draw the ticks */

            /* Left-side Ticks */
            (void) fprintf(ips,"%.2f %.2f ",Pxmin,Pytmp);
            if (innerticks)
            (void) fprintf(ips,"%.2f 0 drawline\n", tck*scale);
            else
            (void) fprintf(ips,"%.2f 0 drawline\n",-tck*scale);

            /* Right-side Ticks */
            (void) fprintf(ips,"%.2f %.2f ",Pxmax,Pytmp);
            (void) fprintf(ips,"%.2f 0 drawline\n",-tck*scale);

            /* Draw labels */
            if (i==0)
            plotPS2D_yticklabel(x*100.0,(double)Pxmin,(double)Pytmp,
                               (char*)NULL, 6, CN_FALSE, &llen);
         }
         /* Increment */
         x += delta;
      }
      /* Increment */
      delta = delta*0.10;
   }


   /* return the label length */
   if (llen > *label_len) *label_len = llen;
}


/*
 * Draw the x-axis tick marks and labels on a Bar-chart
 * This is NEVER used with log-scale
 */
static void plotPS2D_Bar_xticks()
{
   CNdatasetptr Dptr=NULL;
   CNdslistptr  DS;
   CNbinptr     Binptr;
   int          FOUND;
   double       xval;
   double       Pxtmp;
   char         label[CN_MAXCHAR];
   double       bin_width, text_width;
   int          max_length, label_length, i;
 
   /*
    * useful data
    * X = Pxmin + (x-xmin)*(Pxmax-Pxmin)/(xmax-xmin)
    * Y = Pymin + (y-ymin)*(Pymax-Pymin)/(ymax-ymin)
    */
 
   /* Find the barchart dataset */
   FOUND = CN_FALSE;
   for (DS=plotdata->datahead; DS!=NULL && !FOUND; DS=DS->next) {
      if (DS->Dptr && DS->Dptr->datatype==CN_BARCHART && DS->Dptr->barchart) {
         FOUND = CN_TRUE;
         Dptr  = DS->Dptr;
      }
   }
 
   /* If there isn't a barchart, do the normal ticks */
   if (!FOUND) {
      (void) fprintf(stdout,"Warning! No barchart datasets found!\n");
      if (!xautorange && !xlog) {
         /* Linear, fixed tickmarks */
         plotPS2D_xticks();
      } else {
         /* If xlog then do lots of tiny tickmarks */
         plotPS2D_Auto_xticks();
      }
      return;
   }
 
   /* Tick Marks on the X-axis */
   (void) fprintf(ips,"%% Draw the X-Axis\n");
   (void) fprintf(ips,"setAxisLblFont\n");

   /*
    * Go thru all the bins and check label length
    */
   max_length = 0;
   for (Binptr=Dptr->barchart->binhead; Binptr!=NULL; Binptr=Binptr->next) {
     if (Binptr->name != NULL) {
         label_length = strlen(Binptr->name);
         if (label_length > max_length) {
            max_length = label_length;
            (void) strcpy(label, Binptr->name);
         }
      }
   }

   /*
    * Get the number of characters in the label of the bin.
    * Assume that all the bins are of the same size
    */
   if ((Binptr=Dptr->barchart->binhead) != NULL) {
      /* Bin width in plot dimensions */
      bin_width  = fabs(Binptr->xmax - Binptr->xmin) * idx;
 
      /* Check the max length to see if the longest label will fit in the bin */
      text_width = PS_AXISFONT*fscale*0.66*strlen(label);
 
      if (text_width > bin_width) {
 
         /* Now check to see if using a smaller font will do the trick */
         (void) fprintf(ips,"setDateLblFont\n");
         text_width = PS_DATEFONT*fscale*0.66*strlen(label);
 
         if (text_width > bin_width) {
 
            /* Need to truncate the string */
            (void) strcpy(label,"W");
            FOUND = CN_FALSE;

            for (i=1; i<max_length && !FOUND; i++) {
               text_width = PS_DATEFONT*fscale*0.66*strlen(label);
               if (text_width > bin_width-5.0) {
                  FOUND = CN_TRUE;
                  max_length = i;
               }
               (void) strcat(label,"W");
            }
         }
      }
   }

   /* Go thru the Bins and plot labels */
   for (Binptr=Dptr->barchart->binhead; Binptr!=NULL; Binptr=Binptr->next) {
     xval = 0.5*(Binptr->xmin + Binptr->xmax);
 
      /* Translate to plot-coordinates */
      Pxtmp = Pxmin + (xval - xmin)*idx;
      if (xflip)
      Pxtmp = Pxmax - (xval - xmin)*idx;
 
      /* Don't draw if this is outside the plot boundaries */
      if (Pxtmp < Pxmin-0.01*scale || Pxtmp > Pxmax+0.01*scale) continue;
 
      /* Put on labels */
      (void) strncpy(label, Binptr->name, max_length);
      label[max_length] = '\0';
      plotPS2D_xticklabel(xval, (double)Pxtmp, (double)Pymin,
                          label, 4, CN_FALSE);
   }
 
   /* Go thru the Bins and plot tickmarks */
   for (Binptr=Dptr->barchart->binhead; Binptr!=NULL; Binptr=Binptr->next) {
     /* Draw the tick at the beginning of the bin */
      xval = Binptr->xmin;
 
      /* Translate to plot-coordinates */
      Pxtmp = Pxmin + (xval - xmin)*idx;
      if (xflip)
      Pxtmp = Pxmax - (xval - xmin)*idx;

      /* Don't draw if this is outside the plot boundaries */
      if (Pxtmp < Pxmin-0.01*scale || Pxtmp > Pxmax+0.01*scale) continue;
 
      /* Draw the ticks */

      /* Lower Ticks */
      (void) fprintf(ips,"%.2f %.2f ",Pxtmp,Pymin);
      if (innerticks)
      (void) fprintf(ips,"0 %.2f drawline\n", 7*scale);
      else
      (void) fprintf(ips,"0 %.2f drawline\n",-7*scale);

      /* Upper Ticks */
      (void) fprintf(ips,"%.2f %.2f ",Pxtmp,Pymax);
      (void) fprintf(ips,"0 %.2f drawline\n",-7*scale);

      if (Binptr->next != NULL) continue;
 
      /* Draw the tick at the end of the bin */
      xval = Binptr->xmax;
 
      /* Translate to plot-coordinates */
      Pxtmp = Pxmin + (xval - xmin)*idx;
      if (xflip)
      Pxtmp = Pxmax - (xval - xmin)*idx;
 
      /* Don't draw if this is outside the plot boundaries */
      if (Pxtmp < Pxmin-0.01*scale || Pxtmp > Pxmax+0.01*scale) continue;
 
      /* Draw the ticks */

      /* Lower Ticks */
      (void) fprintf(ips,"%.2f %.2f ",Pxtmp,Pymin);
      if (innerticks)
      (void) fprintf(ips,"0 %.2f drawline\n", 7*scale);
      else
      (void) fprintf(ips,"0 %.2f drawline\n",-7*scale);

      /* Upper Ticks */
      (void) fprintf(ips,"%.2f %.2f ",Pxtmp,Pymax);
      (void) fprintf(ips,"0 %.2f drawline\n",-7*scale);
   }
}


/* plot the tick label on the x-axis */
static void plotPS2D_xticklabel(vallbl,xpos,ypos,label,precision,explabel)
double vallbl;
double xpos, ypos;
char   *label;
int    precision, explabel;
{
   char   text[MAXCHR];
   char   exponent[MAXCHR];
   char   nonexponent[MAXCHR];
   int    isexp=CN_FALSE;

   if (explabel)
      (void) sprintf(text,"%.*e",precision,vallbl);
   else
      (void) sprintf(text,"%.*g",precision,vallbl);
   if (label != NULL) (void)strcpy(text,label);
   (void) fprintf(ips,"%.2f %.2f moveto ",xpos,ypos-8*scale);

   /* Check for exponents */
   if (label == NULL) 
      PXmodify_explabel(text, exponent, nonexponent, &isexp, CN_TRUE);

   if (isexp) {
      (void) fprintf(ips,"(%s) BOJA CEJ pop (%s) show (%s) exponentshow\n",
                     text,nonexponent,exponent);
   } else {
      (void) fprintf(ips,"(%s) BOJA CEJ show\n",text);
   }
}


/* plot the tick label on the y-axis */
static void plotPS2D_yticklabel(vallbl,xpos,ypos,label,precision,explabel,llen)
double vallbl;
double xpos, ypos;
char   *label;
int    precision, explabel;
int    *llen;
{
   int    text_len;
   char   text[MAXCHR];
   char   exponent[MAXCHR];
   char   nonexponent[MAXCHR];
   int    isexp = CN_FALSE;

   if (explabel)
      (void) sprintf(text,"%.*e",precision,vallbl);
   else
      (void) sprintf(text,"%.*g",precision,vallbl);
   if (label != NULL) (void)strcpy(text,label);
   text_len = strlen(text);
   if (text_len > *llen) *llen = text_len;
   (void) fprintf(ips,"%.2f %.2f moveto ",xpos-8*scale,ypos+3*scale);

   /* Check for exponents */
   if (label == NULL) 
      PXmodify_explabel(text, exponent, nonexponent, &isexp, CN_TRUE);

   if (isexp) {
      /* Recalculate the text length */
      text_len = strlen(text);
      if (text_len > *llen) *llen = text_len;

      (void) fprintf(ips,"(%s) MIJA RIJ pop (%s) show (%s) exponentshow\n",
                     text,nonexponent,exponent);
   } else {
      (void) fprintf(ips,"(%s) MIJA RIJ show\n",text);
   }
}


/* check overlap of 2 labels */
static int xticklabel_overlap(label1, label2, precision, explabel)
PXlabel label1, label2;
int     precision, explabel;
{
   char   text1[MAXCHR];
   char   text2[MAXCHR];
   char   exponent[MAXCHR];
   char   nonexponent[MAXCHR];
   int    isexp = CN_FALSE;
   int    overlap = CN_FALSE;
   double text1_width, text2_width, text_width, text_spacing, tick_distance;

   /* Assume that the two labels are not of equal value */
   if (explabel) {
      (void) sprintf(text1,"%.*e",precision,label1.value);
      (void) sprintf(text2,"%.*e",precision,label2.value);
   } else {
      (void) sprintf(text1,"%.*g",precision,label1.value);
      (void) sprintf(text2,"%.*g",precision,label2.value);
   }
   /* Modify the labels - strip characters from exponents */
   PXmodify_explabel(text1, exponent, nonexponent, &isexp, CN_TRUE);
   PXmodify_explabel(text2, exponent, nonexponent, &isexp, CN_TRUE);
 
   /* Calculate text widths */
   text1_width  = PS_AXISFONT*fscale*0.66*strlen(text1);
   text2_width  = PS_AXISFONT*fscale*0.66*strlen(text2);
   text_spacing = 5.0*fscale;
   text_width   = 0.5*(text1_width + text2_width) + text_spacing;

   tick_distance = fabs(label1.x - label2.x);
   if (text_width < tick_distance)
      overlap = CN_FALSE;
   else
      overlap = CN_TRUE;
 
   /* Return */
   return(overlap);
}


/* draw the plot */
static void plotPS2D()
{
   CNdslistptr  DS, ds;
   CNdatasetptr Dptr;
   int          colrinc=0, lineinc=0;
   int          contfill, meshplot;
   int          PARENT_FOUND;
   int          sc_plot;

   (void) fprintf(ips,"%%------------------------------------------------\n");
   (void) fprintf(ips,"%%--------------Draw the Plot---------------------\n");
   (void) fprintf(ips,"%%------------------------------------------------\n");

   /* Grids and contours are only drawn if the plot is a scientific plot */
   sc_plot = !probability_plot && !histogram_plot && !barchart_plot;

   /*
    * Plot set - draw the grid if that exists
    */
   for (DS=plotdata->datahead; DS!=NULL && sc_plot; DS=DS->next) {
      if (DS->Dptr->grid && DS->Dptr->datatype == CN_GRID4D)
         plotPS2D_grid(DS->Dptr->grid, 
                       DS->Dptr->data_pr.contintrp, 
                       DS->Dptr->data_pr.contclip, 
                       DS->Dptr->data_pr.meshplot);
   }

   /*
    * Plot set - draw the mesh4D grid and related quants if that exists
    */
   for (DS=plotdata->datahead; DS!=NULL && sc_plot; DS=DS->next) {
      if (DS->Dptr->mesh4D)
         plotPS2D_mesh4D(DS->Dptr);
   }

   /*
    * Plot set - covers multiple plots
    * Draw the colored fills first, because if drawn later
    * the fills will obscure the drawn curves.
    */

   /*
    * Draw the boundary if that is available
    * If this is a PIF-type mesh use the parent's boundary
    */
   for (DS=plotdata->datahead; DS!=NULL && sc_plot; DS=DS->next) {

      if (!( (DS->Dptr->datatype == CN_PIF_PARENT) ||
             (DS->Dptr->datatype == CN_PIF_CHILD && DS->Dptr->parent) ||
             (DS->Dptr->datatype == CN_CONTOUR   && DS->Dptr->parent) ||
             (DS->Dptr->datatype == CN_VECTOR    && DS->Dptr->parent) ) )
         continue;
 
      if (DS->Dptr->parent != NULL) {
         /* Check to see if the parent is in the list */
         PARENT_FOUND = CN_FALSE;
         for (ds=plotdata->datahead; ds!=NULL && !PARENT_FOUND; ds=ds->next)
           if (ds->Dptr == DS->Dptr->parent) PARENT_FOUND = CN_TRUE;
 
         /* Select the dptr from which to get plot instructions */
         Dptr = PARENT_FOUND ? DS->Dptr->parent : DS->Dptr;
 
         /* Now draw the boundary */
         plotPS2D_boundary(DS->Dptr->parent,
                          (int)Dptr->data_pr.boundary,
                          (int)Dptr->data_pr.regbound,
                          (int)Dptr->data_pr.fillbnd,
                          (int)Dptr->data_pr.pr_rgID,
                          1,0);
      } else {
         plotPS2D_boundary(DS->Dptr,
                          (int)DS->Dptr->data_pr.boundary,
                          (int)DS->Dptr->data_pr.regbound,
                          (int)DS->Dptr->data_pr.fillbnd,
                          (int)DS->Dptr->data_pr.pr_rgID,
                          1,0);
      }
   }
 
   /*
    * Draw colored contour fills or hiddenline triangles/rectangles
    */
   for (DS=plotdata->datahead; DS!=NULL && sc_plot; DS=DS->next) {

      /* Draw colored triangles but not the curves */
      contfill = ( (DS->Dptr->data_pr.contstyle == CN_FILLCONT) ||
                   (DS->Dptr->data_pr.contstyle == CN_LNFILLCONT) );
      meshplot = DS->Dptr->data_pr.contstyle == CN_PLOTMESH;
      meshplot = meshplot | DS->Dptr->data_pr.meshplot;
      if (contfill || meshplot) {
         plotPS2D_trias(DS->Dptr, contfill, meshplot);
         plotPS2D_rects(DS->Dptr, contfill, meshplot);
         plotPS2D_polys(DS->Dptr, contfill, meshplot);
      }
   }

   /*
    * Now draw the contour and mesh datasets
    */
   colrinc = 0;
   lineinc = 0;
   for (DS=plotdata->datahead; DS!=NULL && sc_plot; DS=DS->next) {

      /* Now draw the contour curves */
      if (DS->Dptr->datatype == CN_CONTOUR) {
         contfill = DS->Dptr->data_pr.contstyle == CN_FILLCONT;
         meshplot = DS->Dptr->data_pr.contstyle == CN_PLOTMESH;

         /*EMPTY*/
         if (contfill || meshplot) {
            /*
             * Don't draw curves for this dataset
             *
             * Note however that if data_pr.contstyle == CN_LINECONT,
             * and data_pr.meshplot = ON, then both the mesh and the
             * contours will be drawn.
             */
            ;
         } else {
            /* Draw the curve set */
            plotPS2D_dataset_curves(DS->Dptr,colrinc,lineinc);
            if (!overlay) {
               colrinc = 0;
               lineinc = 0;
            } else if (DS->Dptr->data_pr.linetypes == 1) {
               colrinc = colrinc ++;
               lineinc = lineinc ++;
            } else {
               colrinc = colrinc + 3;
               lineinc = lineinc + 3;
            }
         }
      }
   }

   /*
    * Now draw the rest of the datasets
    */
   colrinc = 0;
   lineinc = 0;
   for (DS=plotdata->datahead; DS!=NULL; DS=DS->next) {

      /* Don't draw contours */
      if (DS->Dptr->datatype == CN_CONTOUR) continue;

      /* Draw the curve set */
      plotPS2D_dataset_curves(DS->Dptr, colrinc, lineinc);

      if (DS->Dptr->curvehead != NULL) {
         if (!overlay) {
            colrinc = 0;
            lineinc = 0;
         } else {
            colrinc ++;
            lineinc ++;
         }
      }
   }

   /*
    * Draw the vectors
    */
   for (DS=plotdata->datahead; DS!=NULL && sc_plot; DS=DS->next) {
      if (DS->Dptr->vecbox) 
         plotPS2D_vectors(DS->Dptr->vecbox,
                         (int)    DS->Dptr->data_pr.vlog,
                         (double) DS->Dptr->data_pr.vscale,
                         (double) DS->Dptr->data_pr.vlogscale,
                         (int)    DS->Dptr->data_pr.vhead,
                         (int)    DS->Dptr->data_pr.vtail);
   }

   /*
    * Draw the element ID's if necessary
    * curve-point ID's are plotted together with curves
    */
   for (DS=plotdata->datahead; DS!=NULL; DS=DS->next) {
      /* Set the Dptr */
      Dptr = DS->Dptr;
 
      /* Mesh-based Vectors are treated differently */
      if (DS->Dptr->datatype == CN_VECTOR && DS->Dptr->parent != NULL)
         Dptr = DS->Dptr->parent;

      /* Draw ID's for hierarchical meshes */
      if (DS->Dptr->data_pr.pr_ptID) {
         if (DS->Dptr->parent != NULL) {
            if (sc_plot) 
            plotPS2D_pointIDs(DS->Dptr->parent->pointhead, 
                              DS->Dptr->parent->pointtail, CN_FALSE);
         } else {
            plotPS2D_pointIDs(Dptr->pointhead, Dptr->pointtail, CN_FALSE);
         }
      }
      if (DS->Dptr->data_pr.pr_ndID && sc_plot)
         plotPS2D_nodeIDs (Dptr->nodehead,  Dptr->nodetail);
      if (DS->Dptr->data_pr.pr_trID && sc_plot)
         plotPS2D_triaIDs (Dptr->triahead,  Dptr->triatail);
      if (DS->Dptr->data_pr.pr_rtID && sc_plot)
         plotPS2D_rectIDs (Dptr->recthead,  Dptr->recttail);
   }

   /*
    * Draw the boundary again without fills
    * If this is a PIF-type mesh use the parent's boundary
    */
   for (DS=plotdata->datahead; DS!=NULL && sc_plot; DS=DS->next) {

      if (!( (DS->Dptr->datatype == CN_PIF_PARENT) ||
             (DS->Dptr->datatype == CN_PIF_CHILD && DS->Dptr->parent) ||
             (DS->Dptr->datatype == CN_CONTOUR   && DS->Dptr->parent) ||
             (DS->Dptr->datatype == CN_VECTOR    && DS->Dptr->parent) ) )
         continue;
 
      if (DS->Dptr->parent != NULL) {
         /* Check to see if the parent is in the list */
         PARENT_FOUND = CN_FALSE;
         for (ds=plotdata->datahead; ds!=NULL && !PARENT_FOUND; ds=ds->next)
            if (ds->Dptr == DS->Dptr->parent) PARENT_FOUND = CN_TRUE;
 
         /* Select the dptr from which to get plot instructions */
         Dptr = PARENT_FOUND ? DS->Dptr->parent : DS->Dptr;
 
         /* Now draw the boundary */
         plotPS2D_boundary(DS->Dptr->parent,
                          (int)Dptr->data_pr.boundary,
                          (int)Dptr->data_pr.regbound,
                          (int)Dptr->data_pr.fillbnd,
                          (int)Dptr->data_pr.pr_rgID,
                          0,1);
      } else {
         plotPS2D_boundary(DS->Dptr,
                          (int)DS->Dptr->data_pr.boundary,
                          (int)DS->Dptr->data_pr.regbound,
                          (int)DS->Dptr->data_pr.fillbnd,
                          (int)DS->Dptr->data_pr.pr_rgID,
                          0,1);
      }
   }
}


/*
 * Plot the grid in POSTSCRIPT
 */
static void plotPS2D_grid(grid, contintrp, contclip, meshplot)
CNgrid4Dptr grid;
short       contintrp;
short       contclip;
short       meshplot;
{
   CNsliceptr slice=NULL;
   CNrectptr  R;
   CNtriaptr  T;
   int        verbose=0;
   double     cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax;
   double     tmp;
   short      logx=0, logy=0, logz=0, logt=0;
   int        linetype, linecolor, filltype, fillcolor;
 
   /* Initialize colors and line/fill types */
   filltype  = CN_FILL_NONE;
   linetype  = CN_LN_SOLID;
   fillcolor = PXpolyColorIndexPS(1);   /* yellow */
   linecolor = PXpolyColorIndexPS(4);   /* red    */
 
   /* clipping boundaries */
   cxmin = ((xlog) ? pow(10.0,xmin) : xmin);
   cxmax = ((xlog) ? pow(10.0,xmax) : xmax);
   cymin = ((ylog) ? pow(10.0,ymin) : ymin);
   cymax = ((ylog) ? pow(10.0,ymax) : ymax);
   czmin = - CN_LARGE;
   czmax =   CN_LARGE;
   ctmin = czmin;
   ctmax = czmax;
 
   /*
    * Just plot at z=zmax
    */
   tmp = (grid->zmax < zmax) ? grid->zmax : zmax;
   if ((slice = CNslice_grid4D_z(grid,tmp,(int)contintrp,verbose)) != NULL) {
 
      /* Loop through the rectangles */
      for (R=slice->recthead; R!=NULL; R=R->next) {
         /* Quick check to see if the rectangle is in-bounds */
         if (!CNrect_in_bounds(R,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax)) continue;
 
         /* Plot the color fills */
         plotPS2D_single_fill_rect(R,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax,
              logx, logy, logz, logt, contclip);
 
         /* Plot the mesh */
         if (meshplot)
         plotPS2D_single_solid_rect(R,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax,
              logx, logy, logz, logt, 
              filltype, fillcolor, linetype, linecolor);
      }
 
      /* Loop through the triangles */
      for (T=slice->triahead; T!=NULL; T=T->next) {
         /* Quick check to see if the triangle is in-bounds */
         if (!CNtria_in_bounds(T,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax)) continue;
 
         /* Plot the color fills */
         plotPS2D_single_fill_tria(T,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax,
              logx, logy, logz, logt, contclip);
 
         /* Plot the mesh */
         if (meshplot)
         plotPS2D_single_solid_tria(T,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax,
              logx, logy, logz, logt, 
              filltype, fillcolor, linetype, linecolor);
      }

      /* Delete the slice */
      CNdelete_slice(slice);
   }

   /* Reset color */
   PXsetColorPS(0);
}


/*
 * Plot the mesh4D in PostScript
 */
static void plotPS2D_mesh4D(dptr)
CNdatasetptr dptr;
{
   void PXidentify_view_planes();
   CNmesh4Dptr mesh4D_grid=NULL, mesh4D_quant=NULL;
   CNcubeptr   cubehead=NULL, cubetail=NULL;
   CNblockptr  blockhead=NULL,blocktail=NULL, B;
   int         verbose=0;
   int         xminin, xmaxin, yminin, ymaxin, zminin, zmaxin;
   double      cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax;
   short       contclip, meshplot;
 
   /* Error check */
   if (dptr->mesh4D == NULL) return;
   contclip = dptr->data_pr.contclip;
   meshplot = dptr->data_pr.meshplot;
   if (dptr->datatype != CN_MESH4D_P && dptr->datatype != CN_MESH4D_C) return;
   if (dptr->datatype == CN_MESH4D_C) {
      if (dptr->parent == NULL || dptr->parent->mesh4D == NULL) return;
      mesh4D_grid  = dptr->parent->mesh4D;
      mesh4D_quant = dptr->mesh4D;
   } else if (dptr->datatype == CN_MESH4D_P) {
      mesh4D_grid  = dptr->mesh4D;
      mesh4D_quant = NULL;
      meshplot     = CN_TRUE;
   } else {
      return;
   }
 
   /* clipping boundaries */
   cxmin = ((xlog) ? pow(10.0,xmin) : xmin);
   cxmax = ((xlog) ? pow(10.0,xmax) : xmax);
   cymin = ((ylog) ? pow(10.0,ymin) : ymin);
   cymax = ((ylog) ? pow(10.0,ymax) : ymax);
   czmin = ((zlog) ? pow(10.0,zmin) : zmin);
   czmax = ((zlog) ? pow(10.0,zmax) : zmax);
   ctmin = -CN_LARGE;
   ctmax =  CN_LARGE;
 
   /*
    * Identify the inner and outer planes in the current view
    */
   xminin=0; xmaxin=0;
   yminin=0; ymaxin=0;
   zminin=1; zmaxin=0;
 
   /* Find the rectangles/triangles that are exposed */
   CNslice_mesh4D(mesh4D_grid, mesh4D_quant,
                  &blockhead, &blocktail, &cubehead,  &cubetail,
                  cxmin,cxmax,cymin,cymax,czmin,czmax,
                  xminin,xmaxin,yminin,ymaxin,zminin,zmaxin,verbose);
   if (cubehead == NULL) return;
 
   /* Loop through the blocks */
   for (B=blocktail; B!=NULL; B=B->prev) {
      if (B->cube == NULL) continue;
 
      /* Plot a cube */
      plotPS2D_mesh4D_cube(B,
                          cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax,
                          xminin, xmaxin, yminin, ymaxin, zminin, zmaxin,
                          mesh4D_grid, mesh4D_quant, contclip, meshplot);
   }
 
   /* Delete the blocks, cubes and rectangles */
   CNdelete_block_list(&blockhead, &blocktail);
   CNdelete_cube_list (&cubehead , &cubetail);
}
 
 
/*
 * Plot a single mesh-cube
 */
static void plotPS2D_mesh4D_cube(B,
                     cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax,
                     xminin, xmaxin, yminin, ymaxin, zminin, zmaxin,
                     mesh4D_grid, mesh4D_quant, contclip, meshplot)
CNblockptr  B;
double      cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax;
int         xminin, xmaxin, yminin, ymaxin, zminin, zmaxin;
CNmesh4Dptr mesh4D_grid, mesh4D_quant;
short       contclip, meshplot;
{
   CNpolyptr   polyhead=NULL, polytail=NULL, P;   /* List of polygons   */
   CNnodeptr   nodehead=NULL, nodetail=NULL;      /* List of nodes      */
   CNpointptr  pointhead=NULL,pointtail=NULL;     /* List of points     */
   int         dosort=0;
   int         nocont, noplot, color;
   int         filltype, fillcolor, linetype, linecolor;
 
   if (B == NULL || B->cube == NULL) return;
 
   /*
    * Get the list of polygons on the cube
    */
   CNfind_exposed_faces_of_cube(B, mesh4D_grid,
                                &polyhead,  &polytail,
                                &nodehead,  &nodetail,
                                &pointhead, &pointtail,
                                xminin, xmaxin,
                                yminin, ymaxin,
                                zminin, zmaxin, &dosort);
 
   /* Draw each polygon in the prism */
   for (P=polyhead; P!=NULL; P=P->next) {
 
      /*
       * Get the plotting parameters for this material
       *   nocont - don't draw contours (just draw surface)
       *   noplot - don't draw the polygon at all
       *   color  - fill color (if the surface is to be drawn)
       */
      CNmesh4D_mat_options(P->region,
                           mesh4D_grid->regionhead, mesh4D_grid->regiontail,
                           &nocont, &noplot, &color);
      if (noplot) continue;
 
      /* Plot the color fills (only for quant-type meshes) */
      if (mesh4D_quant && !nocont)
         plotPS2D_single_fill_poly(P,
                      cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax,
                      contclip);
 
      /* Plot the mesh (must do this for grid) */
      if ((meshplot) || (mesh4D_quant && nocont)) {
         filltype  = CN_FILL_SOLID;
         if (mesh4D_quant && !nocont) filltype = CN_FILL_NONE;
         fillcolor = PXpolyColorIndexPS(color);
         linetype  = (meshplot) ? CN_LN_SOLID : CN_LN_NONE;
         linecolor = PXpolyColorIndexPS(4);   /* red    */
         plotPS2D_single_solid_poly(P,
                      cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax,
                      filltype, fillcolor, linetype, linecolor);
      }
   }
 
   /* Delete the polygons, nodes, points */
   CNdelete_poly_list(&polyhead, &polytail);
   CNdelete_node_list(&nodehead, &nodetail);
   CNdelete_point_list(&pointhead, &pointtail);
}


/*
 * Plot the vectors in POSTSCRIPT
 */
static void plotPS2D_vectors(Vbox, vlog, vscale, vlogscale, vhead, vtail)
CNvecboxptr  Vbox;
int          vlog;
double       vscale, vlogscale;
int          vhead, vtail;
{
   CNvecptr    Vptr;
   double      vx, vy;
   double      x1, y1, x2, y2;
   double      vmin;
   int         p1_clipped=CN_FALSE, p2_clipped=CN_FALSE;
 
   if (Vbox == NULL) return;
 
   if (vlog) {
      vmin = 0.1*Vbox->vlen_min;
      if (vmin == 0.0) vmin = 1.0;
   }

   /* Go thru the vectors */
   for (Vptr=Vbox->vectorhead; Vptr!=NULL; Vptr=Vptr->next) {
 
      /* Check plot flag */
      if (Vptr->noplot) continue;

      /* Scale the vector starting point */
      trn_world_to_PS(Vptr->x,Vptr->y,&x1,&y1);
 
      /* Scale to the dimensions of the plot specified in the Vbox */
      if (vlog) {
         vx   = CNveclog10(Vptr->vx/vmin) * vlogscale;
         vy   = CNveclog10(Vptr->vy/vmin) * vlogscale;
      } else {
         vx   = Vptr->vx * vscale;
         vy   = Vptr->vy * vscale;
      }
 
      /* Find the vector ending point in device coordinates */
      trn_world_to_PS(Vptr->x+vx,Vptr->y+vy,&x2,&y2);
 
      /* Draw the arrow */
      plotPS2D_arrow(x1,y1,x2,y2,
                     p1_clipped, p2_clipped,
                     (char *)NULL,
                     (int)Vbox->linetype,
                     (int)Vbox->linecolor,
                     (int)Vbox->linewidth,
                     (int)( vtail ? Vbox->marktype : CN_MK_NONE ),
                     1,
                     (int)Vbox->markcolor,vhead, 0.0);
   }
}

/*
 * Plot the boundary in POSTSCRIPT
 */
/*ARGSUSED*/
static void plotPS2D_boundary(Dptr, boundary, reg_boundary,
                              fillbnd, pr_rgID, fill, drawlabel)
CNdatasetptr Dptr;
int          boundary, reg_boundary, fillbnd, pr_rgID;
int          fill, drawlabel;
{
   CNregionptr R;
   CNpolyptr   P;
   CNnodeptr   node_head=NULL, node_tail=NULL, N;
   double      cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax;
   double      midx, midy, x, y;
   char        label[CN_MAXCHAR];
   int         cnt=0;

   if (Dptr->regionhead == NULL) return;

   if (boundary==CN_FALSE && reg_boundary==CN_FALSE && fillbnd==CN_FALSE) 
      return;

   /* Reset the fill - if the boundary is False then don't fill */
   if (fillbnd == CN_FALSE) fill=CN_FALSE;

   /*
    * Clipping Boundary
    */
   cxmin = ((xlog) ? pow(10.0,xmin) : xmin);
   cxmax = ((xlog) ? pow(10.0,xmax) : xmax);
   cymin = ((ylog) ? pow(10.0,ymin) : ymin);
   cymax = ((ylog) ? pow(10.0,ymax) : ymax);
   czmin = - CN_LARGE;
   czmax =   CN_LARGE;
   ctmin = czmin;
   ctmax = czmax;

   /* Set the font */
   (void) fprintf(ips,"setMkrsLblFont\n");

   /* now print out the boundary segments in each region */
   for (R=Dptr->regionhead; R!=NULL; R=R->next) {

      /* Draw the material boundary only (do this only if "boundary" is set */
      for (P=R->matpolyhead; P!=NULL && boundary; P=P->next) {
         /* Clip the polygon */
         CNclip_poly(P, &node_head, &node_tail,
               cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax,
               1, 1, 0, 1, 0);

         /* Plot the nodes */
         plotPS2D_nodes(node_head, node_tail,
                       CN_FILL_NONE, 
                       PXpolyColorIndexPS(R->color),
                       boundary ? CN_LN_SOLID : CN_LN_NONE, 0, 4);

         /* Delete the node-list */
         CNremove_node_list(&node_head, &node_tail);
 
      }

      /* Now draw the region boundary */
      for (P=R->polyhead; P!=NULL; P=P->next) {

         /* Clip the polygon */
         CNclip_poly(P, &node_head, &node_tail,
               cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax,
               1, 1, 0, 1, 0);

         /* Plot the nodes */
         plotPS2D_nodes(node_head, node_tail,
                       fill ? CN_FILL_SOLID : CN_FILL_NONE, 
                       PXpolyColorIndexPS(R->color),
                       reg_boundary ? CN_LN_SOLID : CN_LN_NONE, 0, 4);

         /* Print ID/label information */
         if (pr_rgID && drawlabel && node_head!=NULL) {
            /* Get the midpoint - don't count the last point */
            midx = midy = 0.0;
            cnt = 0;
            for (N=node_head; N!=NULL; N=N->next) {
               if (N==node_tail) continue;
               midx += N->coord->x;
               midy += N->coord->y;
               cnt++;
            }
            midx = midx/(double)cnt;
            midy = midy/(double)cnt;
            trn_world_to_PS(midx,midy,&x,&y);

            /* Check to see if this midpoint is actually inside the region */
            if (CNpoint_in_region(Dptr,midx,midy,(int)R->ID,0)) {
               /* Print the region ID */
               (void) sprintf(label,"Region %d",R->ID);
               (void) fprintf(ips,"%.2f %.2f moveto ",x,y-2*scale);
               (void) fprintf(ips,"(%s) BOJM CEJ show\n",
                              PXmodifyPS_string(label));

               /* Print the material type */
               (void) sprintf(label,"Mat \"%s\"",R->matname);
               (void) fprintf(ips,"%.2f %.2f moveto ",x,y+2*scale);
               (void) fprintf(ips,"(%s) UPJ  CEJ show\n",
                              PXmodifyPS_string(label));
            }
         }

         /* Delete the node-list */
         CNremove_node_list(&node_head, &node_tail);

      }
   }
}


/*
 * Plot the triangular mesh in POSTSCRIPT
 */
static void plotPS2D_trias(Dptr,contfill,meshplot)
CNdatasetptr Dptr;
int          contfill;   /* Draw multi-colored triangles */
int          meshplot;   /* Draw only the mesh           */
{
   CNtriaptr   T;
   double      cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax;
   int         linetype, linecolor, filltype, fillcolor;

   if (Dptr->triahead == NULL) return;

   /* Initialize colors and line/fill types */
   filltype  = CN_FILL_NONE;
   linetype  = CN_LN_SOLID;
   fillcolor = PXpolyColorIndexPS(1);   /* yellow */
   linecolor = PXpolyColorIndexPS(4);   /* red    */
 
   /*
    * Clipping Boundary
    */
   cxmin = ((xlog) ? pow(10.0,xmin) : xmin);
   cxmax = ((xlog) ? pow(10.0,xmax) : xmax);
   cymin = ((ylog) ? pow(10.0,ymin) : ymin);
   cymax = ((ylog) ? pow(10.0,ymax) : ymax);
   czmin = - CN_LARGE;
   czmax =   CN_LARGE;
   ctmin = czmin;
   ctmax = czmax;

   /* Loop through the triangles */
   for (T=Dptr->triahead; T!=NULL; T=T->next) {

      /* Quick check to see if the triangle is in-bounds */
      if (!CNtria_in_bounds(T,
           cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax)) continue;

      /* Draw a fill triangle with gradated colors */
      if (contfill) {
          plotPS2D_single_fill_tria(T,
                                   cxmin, cxmax, cymin, cymax,
                                   czmin, czmax, ctmin, ctmax,
                                   Dptr->data_pr.logx,
                                   Dptr->data_pr.logy,
                                   0,
                                   Dptr->data_pr.logz,
                                   Dptr->data_pr.contclip);
      }

      /* Draw a mesh triangle */
      if (meshplot) {
          plotPS2D_single_solid_tria(T,
                                   cxmin, cxmax, cymin, cymax,
                                   czmin, czmax, ctmin, ctmax,
                                   Dptr->data_pr.logx,
                                   Dptr->data_pr.logy,
                                   0,
                                   Dptr->data_pr.logz, 
                                   filltype, fillcolor, linetype, linecolor);
      }
   }

   /* Reset color */
   PXsetColorPS(0);
}


/*
 * Plot the rectangular mesh in POSTSCRIPT
 */
static void plotPS2D_rects(Dptr,contfill,meshplot)
CNdatasetptr Dptr;
int          contfill;   /* Draw multi-colored rectangles*/
int          meshplot;   /* Draw only the mesh           */
{
   CNrectptr   R;
   double      cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax;
   int         linetype, linecolor, filltype, fillcolor;
   int         status_ok;
 
   /* Try the bitmap method */
   if (Dptr->data_pr.contbitmap &&
       Dptr->data_pr.contintrp==CN_RECTFLAT && contfill && !meshplot) {
      status_ok = plotPS2D_bitmap_rects(Dptr, contfill);
      if (status_ok) return;
   }

   if (Dptr->recthead == NULL) return;
 
   /* Initialize colors and line/fill types */
   filltype  = CN_FILL_NONE;
   linetype  = CN_LN_SOLID;
   fillcolor = PXpolyColorIndexPS(1);   /* yellow */
   linecolor = PXpolyColorIndexPS(4);   /* red    */

   /*
    * Clipping Boundary
    */
   cxmin = ((xlog) ? pow(10.0,xmin) : xmin);
   cxmax = ((xlog) ? pow(10.0,xmax) : xmax);
   cymin = ((ylog) ? pow(10.0,ymin) : ymin);
   cymax = ((ylog) ? pow(10.0,ymax) : ymax);
   czmin = - CN_LARGE;
   czmax =   CN_LARGE;
   ctmin = czmin;
   ctmax = czmax;

   /* Loop through the rectangles */
   for (R=Dptr->recthead; R!=NULL; R=R->next) {

      /* Quick check to see if the rectangle is in-bounds */
      if (!CNrect_in_bounds(R,
           cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax)) continue;

      /* Draw a fill rectangle with gradated colors */
      if (contfill) {
          plotPS2D_single_fill_rect(R,
                                   cxmin, cxmax, cymin, cymax,
                                   czmin, czmax, ctmin, ctmax,
                                   Dptr->data_pr.logx,
                                   Dptr->data_pr.logy,
                                   0,
                                   Dptr->data_pr.logz,
                                   Dptr->data_pr.contclip);
      }

      /* Draw a mesh rectangle */
      if (meshplot) {
          plotPS2D_single_solid_rect(R,
                                   cxmin, cxmax, cymin, cymax,
                                   czmin, czmax, ctmin, ctmax,
                                   Dptr->data_pr.logx,
                                   Dptr->data_pr.logy,
                                   0,
                                   Dptr->data_pr.logz,
                                   filltype, fillcolor, linetype, linecolor);
      }
   }

   /* Reset color */
   PXsetColorPS(0);
}


/*
 * Plot the polygonal mesh in PostScript
 */
static void plotPS2D_polys(Dptr,contfill,meshplot)
CNdatasetptr Dptr;
int          contfill;   /* Draw multi-colored polygons  */
int          meshplot;   /* Draw only the mesh           */
{
   CNpolyptr   P;
   double      cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax;
   int         nocont, noplot, color;
   int         linetype, linecolor, filltype, fillcolor;
 
   if (Dptr->polyhead == NULL) return;
 
   /*
    * Clipping Boundary
    */
   cxmin = ((xlog) ? pow(10.0,xmin) : xmin);
   cxmax = ((xlog) ? pow(10.0,xmax) : xmax);
   cymin = ((ylog) ? pow(10.0,ymin) : ymin);
   cymax = ((ylog) ? pow(10.0,ymax) : ymax);
   czmin = - CN_LARGE;
   czmax =   CN_LARGE;
   ctmin = - CN_LARGE;
   ctmax =   CN_LARGE;
 
   /* Loop through the polygons */
   for (P=Dptr->polyhead; P!=NULL; P=P->next) {
 
      /*
       * Get the plotting parameters for this material
       *   nocont - don't draw contours (just draw surface)
       *   noplot - don't draw the polygon at all
       *   color  - fill color (if the surface is to be drawn)
       */
      CNmesh4D_mat_options(P->region,
                           Dptr->regionhead, Dptr->regiontail,
                           &nocont, &noplot, &color);
      if (noplot) continue;
 
      /* Plot parameters */
      filltype  = CN_FILL_SOLID;
      fillcolor = PXpolyColorIndexPS(color);
      linetype  = (meshplot) ? CN_LN_SOLID : CN_LN_NONE;
      linecolor = PXpolyColorIndexPS(4);   /* red    */
 
      /* Draw a fill rectangle with gradated colors */
      if (contfill && !nocont) {
         plotPS2D_single_fill_poly(P,
                                  cxmin, cxmax, cymin, cymax,
                                  czmin, czmax, ctmin, ctmax,
                                  Dptr->data_pr.contclip);
         filltype = CN_FILL_NONE;
      }
 
      /* Draw a mesh rectangle */
      plotPS2D_single_solid_poly(P,
                                cxmin, cxmax, cymin, cymax,
                                czmin, czmax, ctmin, ctmax,
                                filltype, fillcolor, linetype, linecolor);
   }
 
   /* Reset color */
   PXsetColorPS(0);
}


/*
 * Plot a single solid triangle
 */
static void plotPS2D_single_solid_tria(T,
                                     cxmin, cxmax, cymin, cymax,
                                     czmin, czmax, ctmin, ctmax,
                                     logx, logy, logz, logt,
                                     filltype, fillcolor,
                                     linetype, linecolor)
CNtriaptr T;
double    cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax;
short     logx, logy, logz, logt;
int       filltype, fillcolor;
int       linetype, linecolor;
{
   CNnodeptr   node_head=NULL, node_tail=NULL;

   /* Check the triangle */
   if (T == NULL) return;

   /* Clip the triangle */
   CNclip_tria(T, &node_head, &node_tail,
               cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax,
               1, 1, 0, 1, 
               logx, logy, logz, logt, 0);
   if (node_head == NULL) return;

   /* Plot the nodes */
   plotPS2D_nodes(node_head, node_tail,
                 filltype, fillcolor, linetype, linecolor, 1);

   /* Delete the node-list */
   CNremove_node_list(&node_head, &node_tail);
}


/*
 * Plot a single mesh rectangle
 */
static void plotPS2D_single_solid_rect(R,
                                     cxmin, cxmax, cymin, cymax,
                                     czmin, czmax, ctmin, ctmax,
                                     logx, logy, logz, logt,
                                     filltype, fillcolor,
                                     linetype, linecolor)
CNrectptr R;
double    cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax;
short     logx, logy, logz, logt;
int       filltype, fillcolor;
int       linetype, linecolor;
{
   CNnodeptr   node_head=NULL, node_tail=NULL;

   /* Check the rectangle */
   if (R == NULL) return;

   /* Clip the rectangle */
   CNclip_rect(R, &node_head, &node_tail,
               cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax,
               1, 1, 0, 1,
               logx, logy, logz, logt, 0);
   if (node_head == NULL) return;

   /* Plot the nodes */
   plotPS2D_nodes(node_head, node_tail,
                 filltype, fillcolor, linetype, linecolor, 1);

   /* Delete the node-list */
   CNremove_node_list(&node_head, &node_tail);
}


/*
 * Plot a single solid polygon
 */
static void plotPS2D_single_solid_poly(P,
                                      cxmin, cxmax, cymin, cymax,
                                      czmin, czmax, ctmin, ctmax,
                                      filltype, fillcolor,
                                      linetype, linecolor)
CNpolyptr P;
double    cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax;
int       filltype, fillcolor;
int       linetype, linecolor;
{
   CNnodeptr   node_head=NULL, node_tail=NULL;
 
   /* Check the polygon */
   if (P == NULL) return;
 
   /* Clip the polygon */
   CNclip_poly(P, &node_head, &node_tail,
               cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax,
               1, 1, 0, 1, 0);
   if (node_head == NULL) return;
 
   /* Plot the nodes */
   plotPS2D_nodes(node_head, node_tail,
                 filltype, fillcolor, linetype, linecolor, 1);
 
   /* Delete the node-list */
   CNremove_node_list(&node_head, &node_tail);
}


/*
 * Draw fill colors in a triangle
 */
/*ARGSUSED*/
static void plotPS2D_single_fill_tria(T,
                                     cxmin, cxmax, cymin, cymax,
                                     czmin, czmax, ctmin, ctmax,
                                     logx, logy, logz, logt,
                                     contclip)
CNtriaptr T;
double    cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax;
short     logx, logy, logz, logt;
short     contclip;
{
   CNcontstepptr C;
   CNnodeptr     node_head=NULL, node_tail=NULL;
   double        tmin, tmax, min, max;
   int           i, colr, nctrs;

   /* Check the triangle */
   if (T == NULL) return;

   /* If the nocont flag is set, skip */
   if (T->nocont) return;

   /* Get the min and max of the triangle */
   CNget_tria_tmaxmin(T,&tmin,&tmax);

   /*
    * The contour steps are calculated outside so that there is
    * a common steps for all the contour datasets
    */
   nctrs = CNcount_contsteps(cstephead, csteptail) - 1;

   /* Loop through the contours - min of 2 contours  */
   i = 0;
   for (C=cstephead; C!=NULL; C=C->next) {
      /*
       * There will be nctrs+1 bands; make sure each of these
       * has a distinct color.
       * Scale the colors from 1 to 32
       */
      colr      = (int)((double)(i*(PX_MAX_FILL_COLORS-1))/(double)nctrs) + 1;

      /* Increment the step number */
      i++;

      /* Set the clipping levels */

      /*
       * The contour steps selected in CNselect_contour_step()
       * do not necessarily cover the whole range [tmin..tmax],
       * i.e. cstephead->value approx cmin, and csteptail->value approx cmax
       * However in PXquery_contours() an additional step corresponding
       * to the the max step size CN_LARGE is automatically added.
       */
      if (C->prev == NULL) {
         max = C->value;
         min = -CN_LARGE;
         if (contclip) continue;
      } else {
         max = C->value;
         min = C->prev->value;
         if (contclip && max==CN_LARGE) continue;
      }

      /* Clip the triangle */
      /*EMPTY*/
      if (tmax < min || tmin > max) {

         /* Don't do anything */
         ;

      } else {

         /* Clip the triangle */
         CNclip_tria(T, &node_head, &node_tail,
                     cxmin, cxmax, cymin, cymax, czmin, czmax, min, max,
                     1, 1, 1, 1,
                     logx, logy, logz, logt, 0);

         /* Plot the nodes */
         plotPS2D_nodes(node_head, node_tail,
                       CN_FILL_SOLID, PXfillColorIndex(colr), CN_LN_NONE,0,1);

         /* Delete the node-list */
         CNremove_node_list(&node_head, &node_tail);
      }
   }
}


/*
 * Draw fill colors in a rectangle
 */
/*ARGSUSED*/
static void plotPS2D_single_fill_rect(R,
                                     cxmin, cxmax, cymin, cymax,
                                     czmin, czmax, ctmin, ctmax,
                                     logx, logy, logz, logt,
                                     contclip)
CNrectptr R;
double    cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax;
short     logx, logy, logz, logt;
short     contclip;
{
   CNcontstepptr C;
   CNnodeptr     node_head=NULL, node_tail=NULL;
   double        tmin, tmax, min, max;
   int           i, colr, nctrs;

   /* Check the rectangle */
   if (R == NULL) return;

   /* If the nocont flag is set, skip */
   if (R->nocont) return;

   /* Get the min and max of the rectangle */
   CNget_rect_tmaxmin(R,&tmin,&tmax);

   /*
    * The contour steps are calculated outside so that there is
    * a common steps for all the contour datasets
    */
   nctrs = CNcount_contsteps(cstephead, csteptail) - 1;

   /* Loop through the contours - min of 2 contours */
   i = 0;
   for (C=cstephead; C!=NULL; C=C->next) {
      /*
       * There will be nctrs+1 bands; make sure each of these
       * has a distinct color.
       * Scale the colors from 1 to 32
       */
      colr      = (int)((double)(i*(PX_MAX_FILL_COLORS-1))/(double)nctrs) + 1;

      /* Increment the step number */
      i++;

      /* Set the clipping levels */

      /*
       * The contour steps selected in CNselect_contour_step()
       * do not necessarily cover the whole range [tmin..tmax],
       * i.e. cstephead->value approx cmin, and csteptail->value approx cmax
       * However in PXquery_contours() an additional step corresponding
       * to the the max step size CN_LARGE is automatically added.
       */
      if (C->prev == NULL) {
         max = C->value;
         min = -CN_LARGE;
         if (contclip) continue;
      } else {
         max = C->value;
         min = C->prev->value;
         if (contclip && max==CN_LARGE) continue;
      }

      /* Clip the rectangle */
      /*EMPTY*/
      if (tmax < min || tmin > max) {

         /* Don't do anything */
         ;
      } else {

         /* Clip the rectangle */
         CNclip_rect(R, &node_head, &node_tail,
                     cxmin, cxmax, cymin, cymax, czmin, czmax, min, max,
                     1, 1, 1, 1, 
                     logx, logy, logz, logt, 0);

         /* Plot the nodes */
         plotPS2D_nodes(node_head, node_tail,
                       CN_FILL_SOLID, PXfillColorIndex(colr), CN_LN_NONE,0,1);

         /* Delete the node-list */
         CNremove_node_list(&node_head, &node_tail);
      }
   }
}


/*
 * Draw fill colors in a polygon
 */
/*ARGSUSED*/
static void plotPS2D_single_fill_poly(P,
                                     cxmin, cxmax, cymin, cymax,
                                     czmin, czmax, ctmin, ctmax,
                                     contclip)
CNpolyptr P;
double    cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax;
short     contclip;
{
   CNcontstepptr C;
   CNnodeptr     node_head=NULL, node_tail=NULL;
   double        tmin, tmax, min, max;
   int           i, colr, nctrs;
 
   /* Check the polygon */
   if (P == NULL) return;
 
   /* Get the min and max of the polygon */
   CNget_poly_tmaxmin(P,&tmin,&tmax);
 
   /*
    * The contour steps are calculated outside so that there is
    * a common steps for all the contour datasets
    */
   nctrs = CNcount_contsteps(cstephead, csteptail) - 1;
 
   /* Loop through the contours - min of 2 ctrs */
   i = 0;
   for (C=cstephead; C!=NULL; C=C->next) {
      /*
       * There will be nctrs+1 bands; make sure each of these
       * has a distinct color.
       * Scale the colors from 1 to 32
       */
      colr = (int)((double)(i*(PX_MAX_FILL_COLORS-1))/(double)nctrs) + 1;
 
      /* Increment the step number */
      i++;
 
      /* Set the clipping levels */
 
      /*
       * The contour steps selected in CNselect_contour_step()
       * do not necessarily cover the whole range [tmin..tmax],
       * i.e. cstephead->value approx cmin, and csteptail->value approx cmax
       * However in PXquery_contours() an additional step corresponding
       * to the the max step size CN_LARGE is automatically added.
       */
      if (C->prev == NULL) {
         max = C->value;
         min = -CN_LARGE;
         if (contclip) continue;
      } else {
         max = C->value;
         min = C->prev->value;
         if (contclip && max==CN_LARGE) continue;
      }
 
      /* Clip the polygon */
      /*EMPTY*/
      if (tmax < min || tmin > max) {
 
         /* Don't do anything */
         ;
 
      } else {
 
         if (min < ctmin)  min = ctmin;
         if (max > ctmax)  max = ctmax;
 
         /* Clip the polygon */
         CNclip_poly(P, &node_head, &node_tail,
                     cxmin, cxmax, cymin, cymax, czmin, czmax, min, max,
                     1, 1, 1, 1, 0);
 
         /* Plot the nodes */
         plotPS2D_nodes(node_head, node_tail,
                       CN_FILL_SOLID, PXfillColorIndex(colr),
                       CN_LN_NONE,0,1);
 
         /* Delete the node-list */
         CNremove_node_list(&node_head, &node_tail);
      }
   }
}
 

/*
 * Plot a list of nodes
 */
/*ARGSUSED*/
static void plotPS2D_nodes(node_head, node_tail,
                           filltype, fillcolor, linestyle, linecolor, linethick)
CNnodeptr node_head, node_tail;
int       filltype, fillcolor, linestyle, linecolor, linethick;
{
   CNnodeptr   N;
   PSPoint     points[MAX_ARR_SIZE];
   double      x, y;
   int         match = CN_FALSE;
   int         count;

   /* return now if there is nothing to plot */
   if (node_head == NULL) return;

   /* if filltype and linestyle are both NONE (0), return now */
   if ((filltype==CN_FILL_NONE) && (linestyle==CN_LN_NONE)) return;

   /* rescale points, save in array */
   count=0;
   for (N=node_head; N!=NULL && count<MAX_ARR_SIZE; N=N->next) {
      trn_world_to_PS(N->coord->x,N->coord->y,&x,&y);
      points[count].x = x;
      points[count].y = y;
      count++;
   }

   /*
    * The size of the postscript file can be reduced significantly by
    * using triangles/rectangles as necessary, without going to the
    * general n-point polygon drawing routine.
    */
   if ((linestyle==CN_LN_NONE) && (filltype==CN_FILL_SOLID) && (count<=5)) {
      /* Filled triangle/rectangle */
      if (count==5) {
         match = do_fillpoly5(points, count, fillcolor);
      } else if (count==4) {
         match = do_fillpoly4(points, count, fillcolor);
      } else if (count==3) {
         match = do_fillpoly3(points, count, fillcolor);
      }
   } else if ((linestyle!=CN_LN_NONE) && (filltype==CN_FILL_NONE) &&(count<=5)){
      /* Triangle/rectangle outline */
      if (count==5) {
         match = do_outlinepoly5(points, count, 
                                 linestyle, linecolor, linethick);
      } else if (count==4) {
         match = do_outlinepoly4(points, count, 
                                 linestyle, linecolor, linethick);
      }
   } 

   /* Don't draw if a match was found */
   if (match) return;

   /* Fill the polygon */
   PXfillPS_polygon(points, count, 
                    filltype, fillcolor, linestyle, linecolor, linethick);
}


/*
 * Fill a 5-point polygon
 */
static int do_fillpoly5(points, count, fillcolor)
PSPoint *points;
int     count;
int     fillcolor;
{
   int        angle012, angle123, angle234, angle103;
   int        match = CN_FALSE;

   /* Error check */  
   if (points==NULL || count!=5) return(CN_FALSE);

   /*
    * The only time that this returns FALSE is if point[0]!=point[4]
    */

   /* This could be a rectangle with joined ends */
   if ((points[0].x==points[4].x) && (points[0].y==points[4].y)) {
      /* OK, the ends are joined - check for squareness (dot rule) */
      angle103 = rightangle(points[1].x, points[1].y,
                            points[0].x, points[0].y,
                            points[3].x, points[3].y);
      angle012 = rightangle(points[0].x, points[0].y,
                            points[1].x, points[1].y,
                            points[2].x, points[2].y);
      angle123 = rightangle(points[1].x, points[1].y,
                            points[2].x, points[2].y,
                            points[3].x, points[3].y);
      angle234 = rightangle(points[2].x, points[2].y,
                            points[3].x, points[3].y,
                            points[4].x, points[4].y);
      if (angle103 && angle012 && angle123 && angle234) {

         /* Set the color */
         PXsetColorPS(fillcolor);

         /* Print out the rectangle */
         (void) fprintf(ips,"%.2f %.2f %.2f %.2f fillrect\n",
                        points[0].x, points[0].y,
                        points[2].x-points[0].x,
                        points[2].y-points[0].y);

         match = CN_TRUE;
      } else {
         /* 
          * Hmmm - OK, so this is a 4 sided polygon 
          * Assume that the polygon is well behaved, and divide it
          * into 2 triangles
          */

         /* Set the color */
         PXsetColorPS(fillcolor);

         /* Print out the triangles */
         (void) fprintf(ips,"%.2f %.2f %.2f %.2f %.2f %.2f filltria\n",
                        points[0].x, points[0].y,
                        points[1].x, points[1].y,
                        points[2].x, points[2].y);
         (void) fprintf(ips,"%.2f %.2f %.2f %.2f %.2f %.2f filltria\n",
                        points[2].x, points[2].y,
                        points[3].x, points[3].y,
                        points[0].x, points[0].y);

         match = CN_TRUE;
      }
   }
   return(match);
}


/*
 * Fill a 4-point polygon
 */
static int do_fillpoly4(points, count, fillcolor)
PSPoint *points;
int     count;
int     fillcolor;
{
   PSPoint    newpoints[5];
   int        i;
   int        match = CN_FALSE;

   /* Error check */  
   if (points==NULL || count!=4) return(CN_FALSE);

   /*
    * This routine always returns TRUE 
    */

   /* This could be a triangle with joined ends */
   if ((points[0].x==points[3].x) && (points[0].y==points[3].y)) {
      /* OK, the ends are joined - print triangle */

      /* Set the color */
      PXsetColorPS(fillcolor);
 
      /* Print out the triangle */
      (void) fprintf(ips,"%.2f %.2f %.2f %.2f %.2f %.2f filltria\n",
                     points[0].x, points[0].y,
                     points[1].x, points[1].y,
                     points[2].x, points[2].y);
 
      match = CN_TRUE;

   } else {
      /* 4-sided polygon - check for squareness */

      /* Copy the point list into the new array */
      for (i=0; i<count; i++) {
         newpoints[i].x = points[i].x;
         newpoints[i].y = points[i].y;
      }
      newpoints[count].x = points[0].x;
      newpoints[count].y = points[0].y;

      /* Check 5 sided polygon (count==4) */
      match = do_fillpoly5(newpoints, count+1, fillcolor);

   }
   return(match);
}


/*
 * Fill a 3-point polygon
 */
static int do_fillpoly3(points, count, fillcolor)
PSPoint *points;
int     count;
int     fillcolor;
{
   int        match = CN_FALSE;

   /* Error check */  
   if (points==NULL || count!=3) return(CN_FALSE);

   /*
    * This routine always returns TRUE 
    */

   /* This is could be a line with shared points */
   if ((points[0].x==points[2].x) && (points[0].y==points[2].y)) {
      /* OK, the ends are joined - this is a line, so don;t do a thing */
      match = CN_TRUE;

   } else {
 
      /* Set the color */
      PXsetColorPS(fillcolor);
 
      /* Print out the triangle */
      (void) fprintf(ips,"%.2f %.2f %.2f %.2f %.2f %.2f filltria\n",
                     points[0].x, points[0].y,
                     points[1].x, points[1].y,
                     points[2].x, points[2].y);
 
      match = CN_TRUE;

   } 
   return(match);
}


/*
 * Draw outline of a 5-point polygon
 */
static int do_outlinepoly5(points, count, linestyle, linecolor, linethick)
PSPoint *points;
int     count;
int     linestyle, linecolor, linethick;
{
   int        angle012, angle123, angle234, angle103;
   int        match = CN_FALSE;

   /* Error check */  
   if (points==NULL || count!=5) return(CN_FALSE);

   /*
    * This returns TRUE only if the poly makes up a square 
    */

   /* This could be a rectangle with joined ends */
   if ((points[0].x==points[4].x) && (points[0].y==points[4].y)) {
      /* OK, the ends are joined - check for squareness (dot rule) */
      angle103 = rightangle(points[1].x, points[1].y,
                            points[0].x, points[0].y,
                            points[3].x, points[3].y);
      angle012 = rightangle(points[0].x, points[0].y,
                            points[1].x, points[1].y,
                            points[2].x, points[2].y);
      angle123 = rightangle(points[1].x, points[1].y,
                            points[2].x, points[2].y,
                            points[3].x, points[3].y);
      angle234 = rightangle(points[2].x, points[2].y,
                            points[3].x, points[3].y,
                            points[4].x, points[4].y);
      if (angle103 && angle012 && angle123 && angle234) {

         /* Set the color */
         PXsetColorPS(linecolor);

         /* Set the linetype */
         (void) PXlinetypPS(linestyle,linethick);

         /* Print out the rectangle */
         (void) fprintf(ips,"%.2f %.2f %.2f %.2f drawrect stroke\n",
                        points[0].x, points[0].y,
                        points[2].x-points[0].x,
                        points[2].y-points[0].y);

         match = CN_TRUE;
      } 
   }
   return(match);
}


/*
 * Draw outline of a 4-point polygon
 */
static int do_outlinepoly4(points, count, linestyle, linecolor, linethick)
PSPoint *points;
int     count;
int     linestyle, linecolor, linethick;
{
   int        match = CN_FALSE;

   /* Error check */  
   if (points==NULL || count!=4) return(CN_FALSE);

   /*
    * This routine returns TRUE only if the polygon is closed
    */

   /* This could be a triangle with joined ends */
   if ((points[0].x==points[3].x) && (points[0].y==points[3].y)) {
      /* OK, the ends are joined - print triangle */

      /* Set the color */
      PXsetColorPS(linecolor);
 
      /* Set the linetype */
      (void) PXlinetypPS(linestyle,linethick);
 
      /* Print out the triangle */
      (void) fprintf(ips,"%.2f %.2f %.2f %.2f %.2f %.2f drawtria stroke\n",
                     points[0].x, points[0].y,
                     points[1].x, points[1].y,
                     points[2].x, points[2].y);
 
      match = CN_TRUE;

   } 
   return(match);
}


/*
 * Given 3 points, A, B, C, check the angle ABC using the dot product
 * of the vectors AB and BC
 */
static int rightangle(xa,ya,xb,yb,xc,yc)
double xa,ya,xb,yb,xc,yc;
{
   double dx1, dx2, dy1, dy2;
   double dprod;

   dx1 = xa-xb;
   dy1 = ya-yb;
   dx2 = xc-xb;
   dy2 = yc-yb;
   dprod = dx1*dx2 - dy1*dy2;
   
   /* If 2 of the points are coincident, then return FALSE */
   if ((dx1==0.0 && dy1==0.0) || (dx2==0.0 && dy2==0.0)) 
      return (CN_FALSE);
   else
      return( (fabs(dprod) < CN_SMALL) );
}


/*
 * Draw a set of curves in a dataset
 */
static void plotPS2D_dataset_curves(dptr, colrinc, lineinc)
CNdatasetptr dptr;
int          colrinc, lineinc;
{
   CNcurveptr C;
   int        contour, contlbl, lbloffset=0;
   int        spline, hdnline, applyfill, pr_ptID, pr_cvID;

   /* Check the dataset first */
   if (dptr == NULL || dptr->curvehead == NULL) return;

   /* Initialize */
   contour   = dptr->datatype==CN_CONTOUR;
   contlbl   = dptr->data_pr.contlabel;
   spline    = dptr->data_pr.splinetyp;
   hdnline   = hiddenline || dptr->view_pr->hiddenline;
   applyfill = dptr->data_pr.applyfill;
   pr_ptID   = dptr->data_pr.pr_ptID;
   pr_cvID   = dptr->data_pr.pr_cvID;

   /* Go thru each set of curves */
   for (C=dptr->curvehead; C!=NULL; C=C->next) {

      /*
       * Plot the curve
       */
 
      if (dptr->data_pr.splinetyp == CN_SP_NONE) {
         /* Plot the curve along the given (real) data-points */
         plotPS2D_curve(C,
                        colrinc,lineinc,
                        contour,contlbl,&lbloffset,
                        hdnline,applyfill,pr_ptID, pr_cvID);
      } else {
         /* Plot the curve using spline-approximated data-points */
         plotPS2D_spline_curve(C,
                               spline,colrinc,lineinc,
                               contour,contlbl,&lbloffset,
                               hdnline,applyfill,pr_ptID, pr_cvID);
      }
   }
}


/*
 * Plot the curve in PS
 */
static void plotPS2D_curve(C, 
                           colrinc, lineinc,
                           contour, contlbl, lbloffset,
                           hiddenline, applyfill, pr_ptID, pr_cvID)
CNcurveptr C;
int        colrinc, lineinc;
int        contour, contlbl, *lbloffset;
int        hiddenline, applyfill, pr_ptID, pr_cvID;
{
   CNpointptr  pt_head=NULL, pt_tail=NULL, P;
   PSPoint     points[MAX_ARR_SIZE];
   double      x, y;
   int         count, i, linepat; 
   int         linetype, linecolor, marktype, markcolor, fillcolor, filltype;
   int         marksize;
   double      cxmin,cxmax,cymin,cymax,czmin,czmax;

   if (C==NULL || C->pointhead==NULL) return;

   /*
    * Make a copy of the points in the curve
    * and a copy is placed in the new pointlist.  In this way,
    * linear-interpolation clipping on the points yields the correct results.
    *
    * Thus, the steps are:
    *   (1) Take real-world coordinates and apply log, abs and probability
    *   (2) Clip real-world coordinates against real-world boundaries
    *   (3) Translate real-world coordinates into plot coordinates
    *   (4) Clip plot coordinates against plot-boundaries
    *
    * Only need one clipping, so skip (2) and just clip against plot-boundaries
    */
   CNcopy_abslog_pointlist(&pt_head, &pt_tail,
                           C->pointhead, C->pointtail,
                           xabs, yabs, zabs, xlog, ylog, zlog);
   if (pt_head == NULL) return;
 
   /*
    * Add a new point if the filltype is non-zero
    */
   if (C->curv_pr.filltype != CN_FILL_NONE || hiddenline) {
      (void) CNinsert_tailpoint(&pt_head, &pt_tail,
                                pt_head->x,
                                pt_head->y,
                                pt_head->z,
                                pt_head->ID);
   }
 
   /* Translate the points to probability scale if necessary */
   if (probability_plot) {
      for (P=pt_head; P!=NULL; P=P->next)
         P->y = y_probability(P->y);
   }
 
   /*
    * Rescale to plot-window and clip against the plot-boundaries.
    * Note that at least one clipping must be done.
    * If this is NOT done, the Xpoint array will be filled with
    * negative/large numbers corresponding to points outside the
    * drawing area, which slows down the drawing significantly!
    */
 
   /* rescale points */
   for (P=pt_head; P!=NULL; P=P->next) {
      trn_world_to_PS_nolog(P->x,P->y,&x,&y);
      P->x = x;
      P->y = y;
      P->z = 0.0;
   }
 
   /* Domain boundaries */
   cxmin =   Pxmin;
   cxmax =   Pxmax;
   cymin =   Pymin;
   cymax =   Pymax;
   czmin = - CN_LARGE;
   czmax =   CN_LARGE;

   /* Clip - this modifies the temporary point list */
   CNclip_pointlist(&pt_head,&pt_tail,
                    cxmin,cxmax,cymin,cymax,czmin,czmax,1,1,0,0);
 
   if (pt_head == NULL) return;
 
   /*
    * Set the properties of the curve.
    */

   /* Set the line type */
   linetype = C->curv_pr.linetype;
   if (linetype != 0) linetype += lineinc;

   /* Set the line color */
   linecolor = C->curv_pr.linecolor;
   if (linecolor > 0) linecolor += colrinc;

   /* Set the marker type */
   marktype = C->curv_pr.marktype;
   if (marktype != 0) marktype += lineinc;

   /* Set the marker type */
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

   if (filltype != CN_FILL_NONE) {

      /* Fill the curve first */
      fillPS2D_curve(pt_head,pt_tail,filltype,fillcolor);

      /* Use the specified linecolor */
      PXlineColorPS(linecolor);

      /* Reset the outline color if necessary */
      if (hiddenline) PXlineColorPS(4);

   } else {

      /* Use the specified linecolor */
      PXlineColorPS(linecolor);

   }


   /*
    * Can only plot some 1000-or-so points at a time, so
    * go thru loop until we run out of points.
    * count  = points plotted during each major loop iteration
    */

   /* Set the linepattern and linewidth */
   linepat = PXlinetypPS(linetype,C->curv_pr.linewidth);

   /* First point */
   P = pt_head;

   while (P != NULL && linepat) {

      /* set count to 0 */
      count = 0;

      /* save points in array */
      for ( ; P!=NULL && count<MAX_ARR_SIZE; P=P->next) {
         points[count].x = P->x;
         points[count].y = P->y;
         count++;
      }

      if (count==1) {
         /* Draw the point */
         (void) fprintf(ips,"newpath\n%.2f %.2f moveto\n",
                        points[0].x,points[0].y);
         (void) fprintf(ips,"%.2f %.2f lineto\n",
                        points[0].x,points[0].y);
         (void) fprintf(ips,"stroke\n");
      } else if (count>1) {
         /* Draw the curve */
         (void) fprintf(ips,"newpath\n%.2f %.2f moveto\n",
                        points[0].x,points[0].y);
         for (i=1; i<count; i++) 
            (void) fprintf(ips,"%.2f %.2f lineto\n", points[i].x,points[i].y);
         (void) fprintf(ips,"stroke\n");
      }

      /* if there are more points to come, repeat the last point */
      if (P != NULL) P = P->prev;
   }

   /* Reset the linetype */
   (void) PXlinetypPS(CN_LN_SOLID,1);

   /* Draw the markers - send the original list */
   plotPS2D_markers(C->pointhead, C->pointtail,
                   marktype, marksize, markcolor, pr_ptID, contour);

   /* Put on the contour labels if necessary */
   if (contour && contlbl) 
      plotPS2D_contlabel(pt_head, pt_tail,
                         C->curv_pr.linetype, C->curv_pr.linelabel,
                         (*lbloffset)++);

   /* Draw curve ID */
   if (pr_cvID && !contour)
      plotPS2D_curveID(pt_head, pt_tail, (int)C->ID, filltype);

   /* Delete the point-list */
   CNdelete_point_list(&pt_head, &pt_tail);

   /* Reset foreground color */
   PXsetColorPS(0);
}


/*
 * Fill the curve in PS
 * The pointlist has already been scaled and clipped against
 * the domain and plot boundaries
 */
/*ARGSUSED*/
static void fillPS2D_curve(pt_head, pt_tail, filltype, fillcolor)
CNpointptr pt_head, pt_tail;
int        filltype, fillcolor;
{
   CNpointptr  P;
   PSPoint     points[MAX_ARR_SIZE];
   int         count;

   if (pt_head == NULL) return;

   /* If the curve is not to be filled then get out now */
   if (filltype == 0) return;

   /* rescale points, save in array */
   count = 0;
   for (P=pt_head; P!=NULL && count<MAX_ARR_SIZE; P=P->next) {
      points[count].x = P->x;
      points[count].y = P->y;
      count++;
   }
   
   /* Fill the polygon */
   PXfillPS_polygon(points,count,
                    filltype, PXpolyColorIndexPS(fillcolor), CN_LN_NONE, 0, 1);
}


/*
 * Plot the interpolated curve in PS
 */
static void plotPS2D_spline_curve(C,
                                 splinetype,
                                 colrinc, lineinc,
                                 contour, contlbl, lbloffset,
                                 hiddenline, applyfill, pr_ptID, pr_cvID)
CNcurveptr C;
int        splinetype;
int        colrinc, lineinc;
int        contour, contlbl, *lbloffset;
int        hiddenline, applyfill, pr_ptID, pr_cvID;
{
   CNpointptr  pt_head=NULL, pt_tail=NULL, P;
   PSPoint     points[MAX_ARR_SIZE];
   double      *xarr, *yarr, *zarr, *xs, *ys, *zs;
   double      x, y;
   double      dist;
   int         npts, nspts, ndiv = 20, closed = 0;
   int         count, i, linepat; 
   int         linetype, linecolor, marktype, markcolor, fillcolor, filltype;
   int         marksize;
   double      cxmin,cxmax,cymin,cymax,czmin,czmax;

   if (C==NULL || C->pointhead==NULL) return;

   /*
    * Make a copy of the points in the curve
    * and a copy is placed in the new pointlist.  In this way,
    * linear-interpolation clipping on the points yields the correct results.
    *
    * Thus, the steps are:
    *   (1) Take real-world coordinates and apply log, abs and probability
    *   (1a)Apply spline interpolation
    *   (2) Clip real-world coordinates against real-world boundaries
    *   (3) Translate real-world coordinates into plot coordinates
    *   (4) Clip plot coordinates against plot-boundaries
    *
    * Only need one clipping, so skip (2) and just clip against plot-boundaries
    */
   CNcopy_abslog_pointlist(&pt_head, &pt_tail,
                           C->pointhead, C->pointtail,
                           xabs, yabs, zabs, xlog, ylog, zlog);

   /* Count the number of points in the curve */
   npts = CNcount_points(pt_head, pt_tail);

   /* Translate the points to probability scale if necessary */
   if (probability_plot) {
      for (P=pt_head; P!=NULL; P=P->next)
         P->y = y_probability(P->y);
   }

   /*
    * Allocate double-precision arrays to hold x and y values
    * for the original and interpolated data
    */
   xarr = CNcreate_1D_double_array(npts);
   yarr = CNcreate_1D_double_array(npts);
   zarr = CNcreate_1D_double_array(npts);
   xs   = CNcreate_1D_double_array(npts*(ndiv+5));
   ys   = CNcreate_1D_double_array(npts*(ndiv+5));
   zs   = CNcreate_1D_double_array(npts*(ndiv+5));

   /* Copy the data from the curve to the arrays */
   i=0;
   for (P=pt_head; P!=NULL; P=P->next) {
      xarr[i] = P->x;
      yarr[i] = P->y;
      zarr[i] = P->z;
      i++;
   }

   /* Distance between 1st and last point */
   closed = 0;
   dist = (xarr[0] - xarr[npts-1])*(xarr[0] - xarr[npts-1]) +
          (yarr[0] - yarr[npts-1])*(yarr[0] - yarr[npts-1]) +
          (zarr[0] - zarr[npts-1])*(zarr[0] - zarr[npts-1]);
   if (dist < CN_SMALL) closed = 1;
   if (C->curv_pr.filltype != CN_FILL_NONE || hiddenline) closed=1;
   if (closed && dist<CN_SMALL && npts>1) npts--;

   /* Interpolate using splines */
   CNcreate_spline(xarr,npts,xs,&nspts,ndiv,splinetype,closed);
   CNcreate_spline(yarr,npts,ys,&nspts,ndiv,splinetype,closed);
   CNcreate_spline(zarr,npts,zs,&nspts,ndiv,splinetype,closed);

   /* Transfer the arrays back to a pointlist */
   CNdelete_point_list(&pt_head,&pt_tail);
   for (i=0; i<nspts; i++)
      (void) CNinsert_tailpoint(&pt_head,&pt_tail,xs[i],ys[i],zs[i],i);
 
   /* Make sure the closed curve is really closed - this comes from */
   /* a problem with the spline interpolation                       */
   if (closed)
      (void) CNinsert_tailpoint(&pt_head,&pt_tail,xs[0],ys[0],zs[0],i);

   /*
    * Rescale to plot-window and clip against the plot-boundaries.
    * Note that at least one clipping must be done.
    * If this is NOT done, the Xpoint array will be filled with
    * negative/large numbers corresponding to points outside the
    * drawing area, which slows down the drawing significantly!
    */
 
   /* rescale points, save in array */
   for (P=pt_head; P!=NULL; P=P->next) {
      trn_world_to_PS_nolog(P->x,P->y,&x,&y);
      P->x = x;
      P->y = y;
      P->z = 0.0;
   }

   /* Domain boundaries */
   cxmin =   Pxmin;
   cxmax =   Pxmax;
   cymin =   Pymin;
   cymax =   Pymax;
   czmin = - CN_LARGE;
   czmax =   CN_LARGE;

   /* Clip - this modifies on the temporary point list */
   CNclip_pointlist(&pt_head,&pt_tail,
                    cxmin,cxmax,cymin,cymax,czmin,czmax,1,1,0,0);
   if (pt_head == NULL) return;

   /*
    * Set the properties of the curve.
    */

   /* Set the line type */
   linetype = C->curv_pr.linetype;
   if (linetype != 0) linetype += lineinc;

   /* Set the line color */
   linecolor = C->curv_pr.linecolor;
   if (linecolor > 0) linecolor += colrinc;

   /* Set the marker type */
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

   if (filltype != CN_FILL_NONE) {

      /* Fill the curve first */
      fillPS2D_curve(pt_head,pt_tail,filltype,fillcolor);

      /* Use the specified linecolor */
      PXlineColorPS(linecolor);

      /* Reset the outline color if necessary */
      if (hiddenline) PXlineColorPS(4);

   } else {

      /* Use the specified linecolor */
      PXlineColorPS(linecolor);

   }


   /*
    * Can only plot some 1000-or-so points at a time, so
    * go thru loop until we run out of points.
    * P      = running count of points
    * count  = points plotted during each major loop iteration
    */

   /* Set the linepattern and linewidth */
   linepat = PXlinetypPS(linetype,C->curv_pr.linewidth);

   /* First point */
   P = pt_head;

   /* go thru loop until we run out of points */
   while ((P!=NULL) && linepat) {
      /* rescale points, save in array */
      count = 0;
      for ( ; (P!=NULL) && count<MAX_ARR_SIZE; P=P->next) {
         points[count].x = P->x;
         points[count].y = P->y;
         count++;
      }

      if (count==1) {
         /* Draw the point */
         (void) fprintf(ips,"newpath\n%.2f %.2f moveto\n",
                        points[0].x,points[0].y);
         (void) fprintf(ips,"%.2f %.2f lineto\n",
                        points[0].x,points[0].y);
         (void) fprintf(ips,"stroke\n");
      } else if (count>1) {
         /* Draw the curve */
         (void) fprintf(ips,"newpath\n%.2f %.2f moveto\n",
                        points[0].x,points[0].y);
         for (i=1; i<count; i++)
            (void) fprintf(ips,"%.2f %.2f lineto\n", points[i].x,points[i].y);
         (void) fprintf(ips,"stroke\n");
      }

      /* if there are more points to come, repeat the last point */
      if (P != NULL) P=P->prev;
   }

   /* Reset the linetype */
   (void) PXlinetypPS(CN_LN_SOLID,1);

   /* Draw the markers */
   plotPS2D_markers(C->pointhead, C->pointtail,
                    marktype, marksize, markcolor, pr_ptID, contour);

   /* Put on the contour labels if necessary */
   if (contour && contlbl) 
      plotPS2D_contlabel(pt_head, pt_tail,
                         C->curv_pr.linetype, C->curv_pr.linelabel,
                         (*lbloffset)++);

   /* Free the arrays */
   CNfree_1D_double_array(xarr);
   CNfree_1D_double_array(yarr);
   CNfree_1D_double_array(zarr);
   CNfree_1D_double_array(xs);
   CNfree_1D_double_array(ys);
   CNfree_1D_double_array(zs);

   /* Draw curve ID */
   if (pr_cvID && !contour)
      plotPS2D_curveID(pt_head, pt_tail, (int)C->ID, filltype);

   /* Delete the point-list */
   CNdelete_point_list(&pt_head, &pt_tail);

   /* Reset */
   PXsetColorPS(0);
}


/*
 * Plot the curve markers in PS
 */
/*ARGSUSED*/
static void plotPS2D_markers(pointhead,pointtail,
                             marktype,marksize,markcolor,pr_ptID,contour)
CNpointptr pointhead, pointtail;
int        marktype, marksize, markcolor;
int        pr_ptID, contour;
{
   CNpointptr  P, pt_head=NULL, pt_tail=NULL;
   double      x,y;
   double      pxmin, pxmax, pymin, pymax;

   if (pointhead == NULL) return;

   /*
    * Make a copy of the points in the curve
    * The points are changed into absolute-values or log-values
    * and a copy is placed in the new pointlist.  In this way,
    * linear-interpolation clipping on the points yields the correct results.
    *
    * Thus, the steps are:
    *   (1) Take real-world coordinates and apply log, abs and probability
    *   (2) Clip real-world coordinates against real-world boundaries
    *   (3) Translate real-world coordinates into plot coordinates
    *   (4) Clip plot coordinates against plot-boundaries
    *
    * Only need one clipping, so skip (2) and just clip against plot-boundaries
    */
   CNcopy_abslog_pointlist(&pt_head, &pt_tail,
                           pointhead, pointtail,
                           xabs, yabs, zabs, xlog, ylog, zlog);

   /* Translate the points to probability scale if necessary */
   if (probability_plot) {
      for (P=pt_head; P!=NULL; P=P->next)
         P->y = y_probability(P->y);
   }

   /* Set the linetype and linewidth */
   (void) PXlinetypPS(CN_LN_SOLID,1);
   PXlineColorPS(markcolor);

   /* Plot boundaries */
   pxmin = Pxmin;
   pxmax = Pxmax;
   pymin = Pymin;
   pymax = Pymax;

   /* go thru loop until we run out of points */
   for (P=pt_head; P!=NULL; P=P->next) {

      /* rescale points */
      trn_world_to_PS_nolog(P->x,P->y,&x,&y);

      /* plot the points only when the point is inside the window */
      if (x < pxmin || x > pxmax) continue;
      if (y < pymin || y > pymax) continue;

      /* plot the marker */
      PXmarkerPS(marktype,marksize,x,y);
   }

   /* Draw curve-point ID's */
   if (pr_ptID && !contour)
      plotPS2D_pointIDs(pt_head, pt_tail, CN_TRUE);
 
   /* Delete the point-list */
   CNdelete_point_list(&pt_head, &pt_tail);

   /* Reset */
   PXsetColorPS(0);
}


/*
 * LABELS
 */
 
/*
 * Plot the curve ID label in POSTSCRIPT
 * The pointlist is in plot coordinates
 */
/*ARGSUSED*/
static void plotPS2D_curveID(pointhead, pointtail, curveID, filltype)
CNpointptr pointhead, pointtail;
int        curveID, filltype;
{
   double     dist=0.0;
   double     x, y;
   char       label[CN_MAXCHAR];
   double     rxmin, rxmax, rymin, rymax;
   int        npts;
   CNpointptr P;
 
   if (pointhead == NULL) return;

   /* Boundary */
   rxmin = Pxmin - scale*10;
   rxmax = Pxmax + scale*10;
   rymin = Pymin - scale*10;
   rymax = Pymax + scale*10;
 
   /* Set the font */
   (void) fprintf(ips,"setMkrsLblFont\n");

   /* Set the linetype, width and color */
   (void) PXlinetypPS(CN_LN_SOLID,1);
   PXlineColorPS(0);
 
   /* Count the number of points first */
   npts = CNcount_points(pointhead, pointtail);

   /* If there is only a single point, then put the label close to the point */
   if (npts == 1) {
 
      /* Put the label at the point */
      x = pointhead->x + 2;
      y = pointhead->y + 2;
 
   } else if (filltype == CN_FILL_NONE) {
 
      /* Put the label at the midpoint of the 1st segment */
      x = 0.5 * (pointhead->x + pointhead->next->x) + 2;
      y = 0.5 * (pointhead->y + pointhead->next->y) + 2;
 
   } else {
 
      /* Check the distance between the head and tail */
      x = pointhead->x - pointtail->x;
      y = pointhead->y - pointtail->y;
      dist = x*x + y*y;

      /* Find the midpoint */
      npts=0;
      x = 0.0;
      y = 0.0;
      P = pointhead;
      if (dist < 1.0) P=P->next;
      for ( ; P!=NULL; P=P->next) {
         x += P->x;
         y += P->y;
         npts++;
      }
      x = x/npts;
      y = y/npts;
   }

   /* Print the label only if the point is in bounds */
   if (x > rxmin && x < rxmax && y > rymin && y < rymax) {
 
      (void) sprintf(label,"C%d",curveID);
      (void) fprintf(ips,"%.2f %.2f moveto ", x+2.0*scale,y);
      (void) fprintf(ips,"(%s) MIJM LEJ show\n",PXmodifyPS_string(label));
   }
}
 

/*
 * Plot the point ID labels in POSTSCRIPT
 */
/*ARGSUSED*/
static void plotPS2D_pointIDs(pt_head,pt_tail,nolog)
CNpointptr pt_head, pt_tail;
int        nolog;
{
   CNpointptr  P;
   double      x,y;
   char        label[CN_MAXCHAR];
   double      rxmin, rxmax, rymin, rymax;
 
   if (pt_head == NULL) return;
 
   /* Boundary */
   rxmin = Pxmin - scale*10;
   rxmax = Pxmax + scale*10;
   rymin = Pymin - scale*10;
   rymax = Pymax + scale*10;
 
   /* Set the font */
   (void) fprintf(ips,"setMkrsLblFont\n");

   /* Set the linetype, width and color */
   (void) PXlinetypPS(CN_LN_SOLID,1);
   PXlineColorPS(0);
 
   /* Go thru each point */
   for (P=pt_head; P!=NULL; P=P->next) {
      if (nolog)
      trn_world_to_PS_nolog(P->x,P->y,&x,&y);
      else
      trn_world_to_PS(P->x,P->y,&x,&y);
 
      /* Print the label only if the point is in bounds */
      if (x > rxmin && x < rxmax && y > rymin && y < rymax) {
 
         (void) sprintf(label,"P%d",P->ID);
         (void) fprintf(ips,"%.2f %.2f moveto ", x+2.0*scale,y);
         (void) fprintf(ips,"(%s) MIJM LEJ show\n",PXmodifyPS_string(label));
      }
   }
}
 
/*
 * Plot the node ID labels in POSTSCRIPT
 */
/*ARGSUSED*/
static void plotPS2D_nodeIDs(nd_head,nd_tail)
CNnodeptr nd_head, nd_tail;
{
   CNnodeptr   N;
   double      x,y;
   char        label[CN_MAXCHAR];
   double      rxmin, rxmax, rymin, rymax;
 
   if (nd_head == NULL) return;
 
   /* Boundary */
   rxmin = Pxmin - scale*10;
   rxmax = Pxmax + scale*10;
   rymin = Pymin - scale*10;
   rymax = Pymax + scale*10;
 
   /* Set the font */
   (void) fprintf(ips,"setMkrsLblFont\n");

   /* Set the linetype, width and color */
   (void) PXlinetypPS(CN_LN_SOLID,1);
   PXlineColorPS(0);
 
   /*
    * Multiple nodes could share a single point so be smart about this
    */
   /* Reset all the point-flags */
   for (N=nd_head; N!=NULL; N=N->next) N->coord->flag = 0;
 
   /* Go thru each node */
   for (N=nd_head; N!=NULL; N=N->next) {
      trn_world_to_PS(N->coord->x,N->coord->y,&x,&y);
 
      /* Print the label only if the point is in bounds */
      if (x > rxmin && x < rxmax && y > rymin && y < rymax) {
 
         (void) sprintf(label,"N%d",N->ID);
         (void) fprintf(ips,"%.2f %.2f moveto ",
                        x-2.0*scale,y-N->coord->flag*10.0*scale);
         (void) fprintf(ips,"(%s) MIJM RIJ show\n",
                        PXmodifyPS_string(label));

         /* Increment the point-flag */
         (N->coord->flag)++;
      }
   }
 
   /* Reset all the point-flags */
   for (N=nd_head; N!=NULL; N=N->next) N->coord->flag = 0;
}
 
/*
 * Plot the triangle ID labels in POSTSCRIPT
 */
/*ARGSUSED*/
static void plotPS2D_triaIDs(tr_head,tr_tail)
CNtriaptr tr_head, tr_tail;
{
   CNtriaptr   T;
   double      x,y;
   char        label[CN_MAXCHAR];
   double      rxmin, rxmax, rymin, rymax;
   double      midx, midy;
 
   if (tr_head == NULL) return;
 
   /* Boundary */
   rxmin = Pxmin - scale*10;
   rxmax = Pxmax + scale*10;
   rymin = Pymin - scale*10;
   rymax = Pymax + scale*10;
 
   /* Set the font */
   (void) fprintf(ips,"setMkrsLblFont\n");

   /* Set the linetype, width and color */
   (void) PXlinetypPS(CN_LN_SOLID,1);
   PXlineColorPS(0);
 
   /* Go thru each triangle */
   for (T=tr_head; T!=NULL; T=T->next) {
      /* Get the x-y value of the triangle-midpoint */
      midx = T->n1->coord->x + T->n2->coord->x + T->n3->coord->x;
      midy = T->n1->coord->y + T->n2->coord->y + T->n3->coord->y;
      midx = midx/3.0;
      midy = midy/3.0;
      trn_world_to_PS(midx,midy,&x,&y);
 
      /* Print the label only if the point is in bounds */
      if (x > rxmin && x < rxmax && y > rymin && y < rymax) {
 
         (void) sprintf(label,"T%d",T->ID);
         (void) fprintf(ips,"%.2f %.2f moveto ",x,y);
         (void) fprintf(ips,"(%s) MIJM CEJ show\n",
                        PXmodifyPS_string(label));

      }
   }
}
 
/*
 * Plot the rectangle ID labels in POSTSCRIPT
 */
/*ARGSUSED*/
static void plotPS2D_rectIDs(rt_head,rt_tail)
CNrectptr rt_head, rt_tail;
{
   CNrectptr   R;
   double      x,y;
   char        label[CN_MAXCHAR];
   double      rxmin, rxmax, rymin, rymax;
   double      midx, midy;
 
   if (rt_head == NULL) return;
 
   /* Boundary */
   rxmin = Pxmin - scale*10;
   rxmax = Pxmax + scale*10;
   rymin = Pymin - scale*10;
   rymax = Pymax + scale*10;
 
   /* Set the font */
   (void) fprintf(ips,"setMkrsLblFont\n");

   /* Set the linetype, width and color */
   (void) PXlinetypPS(CN_LN_SOLID,1);
   PXlineColorPS(0);
 
   /* Go thru each rectangle */
   for (R=rt_head; R!=NULL; R=R->next) {
      /* Get the x-y value of the rectangle-midpoint */
      midx = R->n1->coord->x + R->n2->coord->x +
             R->n3->coord->x + R->n4->coord->x;
      midy = R->n1->coord->y + R->n2->coord->y +
             R->n3->coord->y + R->n4->coord->y;
      midx = midx/4.0;
      midy = midy/4.0;
      trn_world_to_PS(midx,midy,&x,&y);
 
      /* Print the label only if the point is in bounds */
      if (x > rxmin && x < rxmax && y > rymin && y < rymax) {
 
         (void) sprintf(label,"R%d",R->ID);
         (void) fprintf(ips,"%.2f %.2f moveto ",x,y);
         (void) fprintf(ips,"(%s) MIJM CEJ show\n",
                        PXmodifyPS_string(label));

      }
   }
}


/*
 * ANNOTATIONS
 */

/*
 * Plot annotations
 */
static void annotatePS2D()
{
   CNannotptr  AP;
   CNdslistptr DS;
   int         FOUND=CN_FALSE;
   double      annotScale=1.0;

   /* Scan thru and see if there is anything to do here */
   if (plotdata->annothead) FOUND = CN_TRUE; 
   for (DS=plotdata->datahead; DS!=NULL && !FOUND; DS=DS->next)
      if (DS->Dptr->data_pr.plotannot && DS->Dptr->annothead)
         FOUND = CN_TRUE;
   if (!FOUND) return;

   (void) fprintf(ips,"%%------------------------------------------------\n");
   (void) fprintf(ips,"%%--------------Draw the Annotations--------------\n");
   (void) fprintf(ips,"%%------------------------------------------------\n");

   /* Set the font */
   (void) fprintf(ips,"setAnnotLblFont\n");

   /* Plot the annotations in each dataset */
   for (DS=plotdata->datahead; DS!=NULL; DS=DS->next) {
      if (DS->Dptr->data_pr.plotannot) {
         /* Figure out the annotation scale */
         annotScale = (DS->Dptr->bxmax - DS->Dptr->bxmin)/(xmax - xmin);
         if (annotScale < 1.0e-10) annotScale = 0.01;
 
         /* Plot each annotation */
         for (AP=DS->Dptr->annothead; AP!=NULL; AP=AP->next)
            plotPS2D_single_annotation(AP, annotScale);
      }
   }

   /* Plot the annotations in the plotset */
   annotScale = 1.0;
   for (AP=plotdata->annothead; AP!=NULL; AP=AP->next)
      plotPS2D_single_annotation(AP, annotScale);
}

/*
 * Plot a single annotation
 */
static void plotPS2D_single_annotation(AP, annotScale)
CNannotptr AP;
double annotScale;
{
   if (AP == NULL) return;

   switch (AP->type) {
   case CN_AN_RECT : /* Draw a rectangle */
                     plotPS2D_annot_rect(AP, annotScale);
                     break;
   case CN_AN_LINE : /* Draw a line      */
                     plotPS2D_annot_line(AP, annotScale);
                     break;
   case CN_AN_ARROW: /* Draw a arrow     */
                     plotPS2D_annot_arrow(AP, annotScale);
                     break;
   case CN_AN_POINT: /* Draw a point     */
                     plotPS2D_annot_point(AP, annotScale);
                     break;
   case CN_AN_TEXT : /* Draw a text label */
                     plotPS2D_annot_text(AP, annotScale);
                     break;
   default         : break;
   }
}


/*
 * Plot an annotation rectangle
 */
static void plotPS2D_annot_rect(AP, annotScale)
CNannotptr AP;
double annotScale;
{
   PSPoint  points[MAX_ARR_SIZE];
   double   x,y,x1,y1,x2,y2;
   int      i=0;
   double   rxmin, rxmax, rymin, rymax;
   double   bxmin, bxmax, bymin, bymax;
   double   fontsize;

   /*
    * The annotated object can be specified either
    *  (a) position relative to data coordinates
    *  (b) absolute position in percentage of width/height
    */

   /* Clipping boundary */
   rxmin = Pgxmin;
   rxmax = Pgxmax;
   rymin = Pgymin;
   rymax = Pgymax;
 
   if (AP->property.absolute) {
 
      /* Rescale the points - the values are in percentages */
      x1 = rxmin + AP->pt1.x * (rxmax-rxmin);
      y1 = rymin + AP->pt1.y * (rymax-rymin);
      x2 = rxmin + AP->pt2.x * (rxmax-rxmin);
      y2 = rymin + AP->pt2.y * (rymax-rymin);

   } else {

      /* Clip against the real-world boundary */
      if (AP->property.doclip) {
         rxmin = Pxmin;
         rxmax = Pxmax;
         rymin = Pymin;
         rymax = Pymax;
      }

      /* Get the dimensions of the line */
      x1 = AP->pt1.x;
      y1 = AP->pt1.y;
      x2 = AP->pt2.x;
      y2 = AP->pt2.y;
 
      /* Convert into log */
      if (xlog) x1 = CNlog10(x1);
      if (xlog) x2 = CNlog10(x2);
      if (ylog) y1 = CNlog10(y1);
      if (ylog) y2 = CNlog10(y2);
 
      /* Convert into probability */
      if (probability_plot) y1 = y_probability(y1);
      if (probability_plot) y2 = y_probability(y2);
 
      /* Rescale the points */
      x=x1;  y=y1;
      trn_world_to_PS_nolog(x,y,&x1,&y1);
      x=x2;  y=y2;
      trn_world_to_PS_nolog(x,y,&x2,&y2);
   }

   /* Check to see if the box is inside the plot area */
   if ((y1 < rymin && y2 < rymin) || (y1 > rymax && y2 > rymax)) return;
   if ((x1 < rxmin && x2 < rxmin) || (x1 > rxmax && x2 > rxmax)) return;

   /* Get the dimensions of the box */
   bxmin = (x1 < x2) ? x1 : x2;
   bxmax = (x1 < x2) ? x2 : x1;
   bymin = (y1 < y2) ? y1 : y2;
   bymax = (y1 < y2) ? y2 : y1;

   /* Clipping boundaries */
   if (bxmin < rxmin) bxmin = rxmin;
   if (bxmax > rxmax) bxmax = rxmax;
   if (bymin < rymin) bymin = rymin;
   if (bymax > rymax) bymax = rymax;

   /* Save points in an array */
   points[i].x = bxmin;   points[i].y = bymin;   i++;
   points[i].x = bxmax;   points[i].y = bymin;   i++;
   points[i].x = bxmax;   points[i].y = bymax;   i++;
   points[i].x = bxmin;   points[i].y = bymax;   i++;
   points[i].x = bxmin;   points[i].y = bymin;   i++;

   /* Fill the polygon */
   PXfillPS_polygon(points, i,
                   (int) AP->property.filltype,
                   PXpolyColorIndexPS( (int) AP->property.fillcolor ),
                   (int) AP->property.linetype,
                   (int) AP->property.linecolor,
                   (int) AP->property.linewidth);

   /* If there is text attached, draw it */
   if (AP->property.linelabel) {
      /* Set the text color */
      if ((AP->property.filltype!=CN_FILL_NONE) && (AP->property.fillcolor==0))
         PXlineColorPS(-1);
      else
         PXlineColorPS(0);

      /* Set the font */
      fontsize = AP->property.fontsize;
      if (AP->property.doscale) {
          fontsize = fontsize * annotScale;
          if (fontsize < 1.0e-10) fontsize = 1.0;
      }

      /* Draw the label centered, in the middle of the box */
      PXplotPS_scalable_font(0.5*(bxmin+bxmax), 0.5*(bymin+bymax),
                            AP->property.linelabel, fontsize,
                            CN_FALSE, CN_FALSE,
                            CN_FALSE, CN_FALSE);

      /* Reset the line color */
      PXlineColorPS(0);
   }
}

/*
 * Plot an annotation line
 */
static void plotPS2D_annot_line(AP, annotScale)
CNannotptr AP;
double annotScale;
{
   double rxmin,rxmax,rymin,rymax;
   double cxmin,cxmax,cymin,cymax;
   double x1,y1,x2,y2,x,y;
   int    p1_clipped=CN_FALSE, p2_clipped=CN_FALSE;
   double fontsize;
   int    leftJustify, rightJustify, topJustify, bottomJustify;

   /*
    * The annotated object can be specified either
    *  (a) position relative to data coordinates
    *  (b) absolute position in percentage of width/height
    */

   /* Clipping boundary */
   rxmin = Pgxmin;
   rxmax = Pgxmax;
   rymin = Pgymin;
   rymax = Pgymax;
 
   if (AP->property.absolute) {
 
      /* Rescale the points - the values are in percentages */
      x1 = rxmin + AP->pt1.x * (rxmax-rxmin);
      y1 = rymin + AP->pt1.y * (rymax-rymin);
      x2 = rxmin + AP->pt2.x * (rxmax-rxmin);
      y2 = rymin + AP->pt2.y * (rymax-rymin);

   } else {
      /* Get the dimensions of the line */
      x1 = AP->pt1.x;
      y1 = AP->pt1.y;
      x2 = AP->pt2.x;
      y2 = AP->pt2.y;

      /* Convert into log */
      if (xlog) x1 = CNlog10(x1);
      if (xlog) x2 = CNlog10(x2);
      if (ylog) y1 = CNlog10(y1);
      if (ylog) y2 = CNlog10(y2);
 
      /* Convert into probability */
      if (probability_plot) y1 = y_probability(y1);
      if (probability_plot) y2 = y_probability(y2);
 
      /* Rescale the points */
      x=x1;  y=y1;
      trn_world_to_PS_nolog(x,y,&x1,&y1);
      x=x2;  y=y2;
      trn_world_to_PS_nolog(x,y,&x2,&y2);

      /* Do pre-clipping aaginst the real-world boundaries */
      if (AP->property.doclip) {
         /* Clipping boundary */
         cxmin = Pxmin-0.1;
         cxmax = Pxmax+0.1;
         cymin = Pymin-0.1;
         cymax = Pymax+0.1;

         if ((x1 <=cxmin && x2 <=cxmin) || (x1 >=cxmax && x2 >=cxmax)) return;
         if ((y1 <=cymin && y2 <=cymin) || (y1 >=cymax && y2 >=cymax)) return;
         clipPS2D_in_xmin(&x1,&y1,&x2,&y2,cxmin,&p1_clipped,&p2_clipped);
         clipPS2D_in_ymin(&x1,&y1,&x2,&y2,cymin,&p1_clipped,&p2_clipped);
         clipPS2D_in_xmax(&x1,&y1,&x2,&y2,cxmax,&p1_clipped,&p2_clipped);
         clipPS2D_in_ymax(&x1,&y1,&x2,&y2,cymax,&p1_clipped,&p2_clipped);
         if ((x1 <=cxmin && x2 <=cxmin) || (x1 >=cxmax && x2 >=cxmax)) return;
         if ((y1 <=cymin && y2 <=cymin) || (y1 >=cymax && y2 >=cymax)) return;
      }
   }

   /* Check to see if the line is inside the plot area */
   if ((x1 < rxmin && x2 < rxmin) || (x1 > rxmax && x2 > rxmax)) return;
   if ((y1 < rymin && y2 < rymin) || (y1 > rymax && y2 > rymax)) return;

   /* Do another clipping */
   clipPS2D_in_xmin(&x1,&y1,&x2,&y2,rxmin,&p1_clipped,&p2_clipped);
   clipPS2D_in_ymin(&x1,&y1,&x2,&y2,rymin,&p1_clipped,&p2_clipped);
   clipPS2D_in_xmax(&x1,&y1,&x2,&y2,rxmax,&p1_clipped,&p2_clipped);
   clipPS2D_in_ymax(&x1,&y1,&x2,&y2,rymax,&p1_clipped,&p2_clipped);

   /* Draw the line */
   PXlineColorPS((int)AP->property.linecolor);
   PXdrawPS_line(x1,y1,x2,y2,
                 (int)AP->property.linetype, (int)AP->property.linewidth);

   /* Draw the markers */
   if ((AP->property.marktype != CN_MK_NONE) && (!p1_clipped || !p2_clipped)) {
      (void) PXlinetypPS(CN_LN_SOLID,1);
      PXlineColorPS(AP->property.markcolor);
      if (!p1_clipped) PXmarkerPS((int)AP->property.marktype,
                                  (int)AP->property.marksize,x1,y1);
      if (!p2_clipped) PXmarkerPS((int)AP->property.marktype,
                                  (int)AP->property.marksize,x2,y2);
   }

   /* If there is text attached, draw it */
   if (!p1_clipped && AP->property.linelabel) {
      /* Set the label color */
      PXlineColorPS(0);

      /* Set the font */
      fontsize = AP->property.fontsize;
      if (AP->property.doscale) {
          fontsize = fontsize * annotScale;
          if (fontsize < 1.0e-10) fontsize = 1.0;
      }
 
      /* Text justification is based on comparison of x1, x2 */
      if (x1 > x2) {
         /* Left-justified */
         leftJustify   = CN_TRUE;
         rightJustify  = CN_FALSE;
         topJustify    = CN_FALSE;
         bottomJustify = CN_FALSE;
      } else if (x1 == x2) {
         /* Center-justified */
         leftJustify   = CN_FALSE;
         rightJustify  = CN_FALSE;
         if (y1 > y2) {
            topJustify    = CN_TRUE;
            bottomJustify = CN_FALSE;
         } else {
            topJustify    = CN_FALSE;
            bottomJustify = CN_TRUE;
         }
      } else {
         /* Right-justified */
         leftJustify   = CN_FALSE;
         rightJustify  = CN_TRUE;
         topJustify    = CN_FALSE;
         bottomJustify = CN_FALSE;
      }

      /* Draw the label centered, in the middle of the box */
      PXplotPS_scalable_font(x1, y1,
                            AP->property.linelabel, fontsize,
                            leftJustify, rightJustify,
                            topJustify, bottomJustify);
   }

   /* Reset */
   PXlineColorPS(0);
}

/*
 * Plot an annotation arrow
 */
static void plotPS2D_annot_arrow(AP, annotScale)
CNannotptr AP;
double annotScale;
{
   double cxmin,cxmax,cymin,cymax;
   double rxmin,rxmax,rymin,rymax;
   double x1,y1,x2,y2,x,y;
   int    p1_clipped=CN_FALSE, p2_clipped=CN_FALSE;
   double fontsize;

   /*
    * The annotated object can be specified either
    *  (a) position relative to data coordinates
    *  (b) absolute position in percentage of width/height
    */
 
   /* Clipping boundary */
   rxmin = Pgxmin;
   rxmax = Pgxmax;
   rymin = Pgymin;
   rymax = Pgymax;
 
   if (AP->property.absolute) {
 
      /* Rescale the points - the values are in percentages */
      x1 = rxmin + AP->pt1.x * (rxmax-rxmin);
      y1 = rymin + AP->pt1.y * (rymax-rymin);
      x2 = rxmin + AP->pt2.x * (rxmax-rxmin);
      y2 = rymin + AP->pt2.y * (rymax-rymin);

   } else {

      /* Get the dimensions of the line */
      x1 = AP->pt1.x;
      y1 = AP->pt1.y;
      x2 = AP->pt2.x;
      y2 = AP->pt2.y;

      /* Convert into log */
      if (xlog) x1 = CNlog10(x1);
      if (xlog) x2 = CNlog10(x2);
      if (ylog) y1 = CNlog10(y1);
      if (ylog) y2 = CNlog10(y2);
 
      /* Convert into probability */
      if (probability_plot) y1 = y_probability(y1);
      if (probability_plot) y2 = y_probability(y2);
 
      /* Rescale the points */
      x=x1;  y=y1;
      trn_world_to_PS_nolog(x,y,&x1,&y1);
      x=x2;  y=y2;
      trn_world_to_PS_nolog(x,y,&x2,&y2);

      /* Do pre-clipping aaginst the real-world boundaries */
      if (AP->property.doclip) {
         /* Clipping boundary */
         cxmin = Pxmin-0.1;
         cxmax = Pxmax+0.1;
         cymin = Pymin-0.1;
         cymax = Pymax+0.1;

         if ((x1 <=cxmin && x2 <=cxmin) || (x1 >=cxmax && x2 >=cxmax)) return;
         if ((y1 <=cymin && y2 <=cymin) || (y1 >=cymax && y2 >=cymax)) return;
         clipPS2D_in_xmin(&x1,&y1,&x2,&y2,cxmin,&p1_clipped,&p2_clipped);
         clipPS2D_in_ymin(&x1,&y1,&x2,&y2,cymin,&p1_clipped,&p2_clipped);
         clipPS2D_in_xmax(&x1,&y1,&x2,&y2,cxmax,&p1_clipped,&p2_clipped);
         clipPS2D_in_ymax(&x1,&y1,&x2,&y2,cymax,&p1_clipped,&p2_clipped);
         if ((x1 <=cxmin && x2 <=cxmin) || (x1 >=cxmax && x2 >=cxmax)) return;
         if ((y1 <=cymin && y2 <=cymin) || (y1 >=cymax && y2 >=cymax)) return;
      }
   }

   /* Set the font */
   fontsize = AP->property.fontsize;
   if (AP->property.doscale) {
      fontsize = fontsize * annotScale;
      if (fontsize < 1.0e-10) fontsize = 1.0;
   }

   /* Draw the arrow */
   plotPS2D_arrow(x1,y1,x2,y2,
                  p1_clipped, p2_clipped,
                  AP->property.linelabel,
                  (int)AP->property.linetype,
                  (int)AP->property.linecolor,
                  (int)AP->property.linewidth,
                  (int)AP->property.marktype,
                  (int)AP->property.marksize,
                  (int)AP->property.markcolor, 1, 
                  fontsize);
}


/*
 * Draw an arrow
 */
static void plotPS2D_arrow(x1,y1,x2,y2,
                           p1_clipped, p2_clipped,
                           linelabel,
                           linetype,linecolor,linewidth,
                           marktype,marksize,markcolor,
                           arrowhead, 
                           fontsize)
double x1,y1,x2,y2;
int    p1_clipped, p2_clipped;
char   *linelabel;
int    linetype,linecolor,linewidth;
int    marktype,marksize,markcolor;
int    arrowhead;
double fontsize;
{
   double rxmin, rxmax, rymin, rymax;
   double vx, vy, vd;
   double xa, ya, xb, yb, xc, yc;
   int    leftJustify, rightJustify, topJustify, bottomJustify;
 
   /* Clipping boundary */
   rxmin = Pgxmin;
   rxmax = Pgxmax;
   rymin = Pgymin;
   rymax = Pgymax;

   /* Check to see if the line is inside the plot area */
   if ((x1 < rxmin && x2 < rxmin) || (x1 > rxmax && x2 > rxmax)) return;
   if ((y1 < rymin && y2 < rymin) || (y1 > rymax && y2 > rymax)) return;
 
   /* Clip the arrow against the plot boundaries */
   clipPS2D_in_xmin(&x1,&y1,&x2,&y2,rxmin,&p1_clipped,&p2_clipped);
   clipPS2D_in_ymin(&x1,&y1,&x2,&y2,rymin,&p1_clipped,&p2_clipped);
   clipPS2D_in_xmax(&x1,&y1,&x2,&y2,rxmax,&p1_clipped,&p2_clipped);
   clipPS2D_in_ymax(&x1,&y1,&x2,&y2,rymax,&p1_clipped,&p2_clipped);
 
   /* Draw the arrow origin */
   (void) PXlinetypPS(CN_LN_SOLID,1);
   PXlineColorPS(markcolor);
   if (!p1_clipped) PXmarkerPS(marktype,marksize,x1,y1);
 
   /* Draw the arrow line */
   PXlineColorPS(linecolor);
   PXdrawPS_line(x1,y1,x2,y2, linetype, linewidth);
 
   /* Draw the arrow head */
   if (!p2_clipped && arrowhead) {
      /* Draw the arrow head */
      (void) PXlinetypPS(CN_LN_SOLID,linewidth);
      vx = x2-x1;
      vy = y2-y1;
      vd = sqrt(vx*vx + vy*vy);
      if (vd > 5.0) {
      xa = x2 - 5.0*vx/vd;
      ya = y2 - 5.0*vy/vd;
      xb = xa - 2.0*vy/vd;
      yb = ya + 2.0*vx/vd;
      xc = xa + 2.0*vy/vd;
      yc = ya - 2.0*vx/vd;
      PXdrawPS_line(x2,y2,xb,yb, CN_LN_SOLID, linewidth);
      PXdrawPS_line(x2,y2,xc,yc, CN_LN_SOLID, linewidth);
      }
   }

   /* If there is text attached, draw it */
   if (!p1_clipped && linelabel) {
      /* Set the label color */
      PXlineColorPS(0);

      /* Text justification is based on comparison of x1, x2 */
      if (x1 > x2) {
         /* Left-justified */
         leftJustify   = CN_TRUE;
         rightJustify  = CN_FALSE;
         topJustify    = CN_FALSE;
         bottomJustify = CN_FALSE;
      } else if (x1 == x2) {
         /* Center-justified */
         leftJustify   = CN_FALSE;
         rightJustify  = CN_FALSE;
         if (y1 > y2) {
            topJustify    = CN_TRUE;
            bottomJustify = CN_FALSE;
         } else {
            topJustify    = CN_FALSE;
            bottomJustify = CN_TRUE;
         }
      } else {
         /* Right-justified */
         leftJustify   = CN_FALSE;
         rightJustify  = CN_TRUE;
         topJustify    = CN_FALSE;
         bottomJustify = CN_FALSE;
      }
 
      /* Draw the label centered, in the middle of the box */
      PXplotPS_scalable_font(x1, y1,
                            linelabel, fontsize,
                            leftJustify, rightJustify,
                            topJustify, bottomJustify);
   }
 
   /* Reset */
   PXlineColorPS(0);
   (void) PXlinetypPS(CN_LN_SOLID,1);
}


/*
 * Plot an annotation point
 */
static void plotPS2D_annot_point(AP, annotScale)
CNannotptr AP;
double annotScale;
{
   double  x,y,x1,y1;
   double  rxmin, rxmax, rymin, rymax;
   double  cxmin, cxmax, cymin, cymax;
   double  fontsize;

   /*
    * The annotated object can be specified either
    *  (a) position relative to data coordinates
    *  (b) absolute position in percentage of width/height
    */
 
   /* Clipping boundary */
   rxmin = Pgxmin;
   rxmax = Pgxmax;
   rymin = Pgymin;
   rymax = Pgymax;
 
   if (AP->property.absolute) {
 
      /* Rescale the points - the values are in percentages */
      x1 = rxmin + AP->pt1.x * (rxmax-rxmin);
      y1 = rymin + AP->pt1.y * (rymax-rymin);

   } else {

      /* Get the point coordinates */
      x1 = AP->pt1.x;
      y1 = AP->pt1.y;
 
      /* Convert into log */
      if (xlog) x1 = CNlog10(x1);
      if (ylog) y1 = CNlog10(y1);
 
      /* Convert into probability */
      if (probability_plot) y1 = y_probability(y1);
 
      /* Rescale the points */
      x=x1;  y=y1;
      trn_world_to_PS_nolog(x,y,&x1,&y1);

      /* Do pre-clipping against the inner plot boundaries */
      if (AP->property.doclip) {
         /* Clipping boundary */
         cxmin = Pxmin;
         cxmax = Pxmax;
         cymin = Pymin;
         cymax = Pymax;

         if ((x1 < cxmin || x1 > cxmax) || (y1 < cymin || y1 > cymax))
            return;
      }
   }

   /* Draw the markers */
   if ((rxmin < x1 && x1 < rxmax) && (rymin < y1 && y1 < rymax)) {
      (void) PXlinetypPS(CN_LN_SOLID,1);
      PXlineColorPS(AP->property.markcolor);
      PXmarkerPS((int)AP->property.marktype,
                 (int)AP->property.marksize,x1,y1);

      /* If there is text attached, draw it */
      if (AP->property.linelabel) {

         /* Reset the label color */
         PXlineColorPS(0);

         /* Set the font */
         fontsize = AP->property.fontsize;
         if (AP->property.doscale) {
             fontsize = fontsize * annotScale;
             if (fontsize < 1.0e-10) fontsize = 1.0;
         }
 
         /* Left-justified, left of the point */
         PXplotPS_scalable_font(x1, y1, AP->property.linelabel, fontsize,
                               CN_TRUE, CN_FALSE,
                               CN_FALSE, CN_FALSE);
      }
   }

   /* Reset */
   PXlineColorPS(0);
}

/*
 * Plot an annotation label
 */
static void plotPS2D_annot_text(AP, annotScale)
CNannotptr AP;
double annotScale;
{
   double  x,y,x1,y1;
   double  rxmin, rxmax, rymin, rymax;
   double  cxmin, cxmax, cymin, cymax;
   double  fontsize;

   /*
    * The annotated object can be specified either
    *  (a) position relative to data coordinates
    *  (b) absolute position in percentage of width/height
    */
 
   /* Clipping boundary */
   rxmin = Pgxmin;
   rxmax = Pgxmax;
   rymin = Pgymin;
   rymax = Pgymax;
 
   if (AP->property.absolute) {
 
      /* Rescale the points - the values are in percentages */
      x1 = rxmin + AP->pt1.x * (rxmax-rxmin);
      y1 = rymin + AP->pt1.y * (rymax-rymin);

   } else {

      /* Get the point coordinates */
      x1 = AP->pt1.x;
      y1 = AP->pt1.y;
 
      /* Convert into log */
      if (xlog) x1 = CNlog10(x1);
      if (ylog) y1 = CNlog10(y1);
 
      /* Convert into probability */
      if (probability_plot) y1 = y_probability(y1);
 
      /* Rescale the points */
      x=x1;  y=y1;
      trn_world_to_PS_nolog(x,y,&x1,&y1);

      /* Do a pre-clipping */
      if (AP->property.doclip) {
         /* Clipping boundary */
         cxmin = Pxmin;
         cxmax = Pxmax;
         cymin = Pymin;
         cymax = Pymax;

         if ((x1 < cxmin || x1 > cxmax) || (y1 < cymin || y1 > cymax))
            return;
      }
   }

   /* Draw the text */
   if ((rxmin < x1 && x1 < rxmax) && (rymin < y1 && y1 < rymax)) {
      if (AP->property.linelabel) {
         /* Set the linetype and colors */
         (void) PXlinetypPS(CN_LN_SOLID,1);
         PXlineColorPS(0);

         /* Set the font */
         fontsize = (double) AP->property.fontsize;
         if (AP->property.doscale) {
             fontsize = fontsize * annotScale;
             if (fontsize < 1.0e-10) fontsize = 1.0;
         }

         /* Center-justified */
         PXplotPS_scalable_font(x1, y1, AP->property.linelabel, fontsize,
                               CN_FALSE, CN_FALSE,
                               CN_FALSE, CN_FALSE);
      }
   }
}

/*
 * Clip a line against xmin
 */
static void clipPS2D_in_xmin(x1,y1,x2,y2,rxmin,p1_clipped,p2_clipped)
double *x1, *y1, *x2, *y2, rxmin;
int    *p1_clipped, *p2_clipped;
{
   double t;

   /* Clip the line against rxmin */
   if (*x1 < rxmin && *x2 > rxmin) {
      t = (rxmin - *x1)/(*x2 - *x1);
      *x1 = *x1 + t*(*x2 - *x1);
      *y1 = *y1 + t*(*y2 - *y1);
      if (*x1 < rxmin) *x1 = rxmin;
      *p1_clipped = CN_TRUE;
   } else if (*x2 < rxmin && *x1 > rxmin) {
      t = (rxmin - *x2)/(*x1 - *x2);
      *x2 = *x2 + t*(*x1 - *x2);
      *y2 = *y2 + t*(*y1 - *y2);
      if (*x2 < rxmin) *x2 = rxmin;
      *p2_clipped = CN_TRUE;
   }
}

/*
 * Clip a line against ymin
 */
static void clipPS2D_in_ymin(x1,y1,x2,y2,rymin,p1_clipped,p2_clipped)
double *x1, *y1, *x2, *y2, rymin;
int    *p1_clipped, *p2_clipped;
{
   clipPS2D_in_xmin(y1,x1,y2,x2,rymin,p1_clipped,p2_clipped);
}

/*
 * Clip a line against xmax
 */
static void clipPS2D_in_xmax(x1,y1,x2,y2,rxmax,p1_clipped,p2_clipped)
double *x1, *y1, *x2, *y2, rxmax;
int    *p1_clipped, *p2_clipped;
{
   double t;

   /* Clip the line against rxmax */
   if (*x1 < rxmax && *x2 > rxmax) {
      t = (rxmax - *x2)/(*x1 - *x2);
      *x2 = *x2 + t*(*x1 - *x2);
      *y2 = *y2 + t*(*y1 - *y2);
      if (*x2 > rxmax) *x2 = rxmax;
      *p2_clipped = CN_TRUE;
   } else if (*x2 < rxmax && *x1 > rxmax) {
      t = (rxmax - *x1)/(*x2 - *x1);
      *x1 = *x1 + t*(*x2 - *x1);
      *y1 = *y1 + t*(*y2 - *y1);
      if (*x1 > rxmax) *x1 = rxmax;
      *p1_clipped = CN_TRUE;
   }
}

/*
 * Clip a line against ymax
 */
static void clipPS2D_in_ymax(x1,y1,x2,y2,rymax,p1_clipped,p2_clipped)
double *x1, *y1, *x2, *y2, rymax;
int    *p1_clipped, *p2_clipped;
{
   clipPS2D_in_xmax(y1,x1,y2,x2,rymax,p1_clipped,p2_clipped);
}



/*
 * LABELS
 */

/*
 * Plot the contour labels in PS
 * The pointlist is in plot coordinates
 */
static void plotPS2D_contlabel(pointhead, pointtail, 
                               linetype, linelabel, lbloffset)
CNpointptr pointhead, pointtail;
int        linetype;
char       *linelabel;
int        lbloffset;
{
   CNpointptr   P;
   double       x, y;
   double       dist, last_label_dist;
   double       idl_dist = 150.0*scale;
   int          npts;

   if ((pointhead == NULL) || (linelabel == NULL)) return;

   /* Reset the color */
   PXsetColorPS(0);

   /* label the contours at given points intervals */
   if (linetype == CN_LN_SOLID || linetype == CN_LN_DASHED) {

      /* Count the number of points first */
      npts = CNcount_points(pointhead, pointtail);

      /* 2 or fewer points are treated differently */
      if (npts == 1) {
 
         /* Put the label at the point */
         x = pointhead->x;
         y = pointhead->y;
 
         /* Draw the text */
         (void) drawPS2D_label(x,y,x+1.0,y,linelabel);

      } else if (npts == 2) {
 
         /* Put the label at the midpoint */
         x = 0.5*(pointhead->x + pointtail->x);
         y = 0.5*(pointhead->y + pointtail->y);
 
         /* Draw the text */
         (void) drawPS2D_label(x,y,pointtail->x,pointtail->y,linelabel);
 
      } else {

         /* Place the label at equally-spaced locations on the curve */
         last_label_dist = 0.3*idl_dist + (lbloffset % 5)*0.1*idl_dist;
         for (P=pointhead->next; P!=NULL; P=P->next) {
 
            /* Distance from previous point to current point */
            dist = sqrt( (P->prev->x - P->x)*(P->prev->x - P->x) +
                         (P->prev->y - P->y)*(P->prev->y - P->y));
 
            /* Distance from the last label */
            last_label_dist += dist;
 
            /*
             * If the distance from the last label is greater then
             * the ideal distance, then draw the label
             */
            if (last_label_dist > idl_dist) {
               /* Draw the label at the midpoint of the current segment */
               x = 0.5*(P->prev->x + P->x);
               y = 0.5*(P->prev->y + P->y);
 
               /* Draw the text */
               (void) drawPS2D_label(x,y,P->x,P->y,linelabel);
 
               /* Reset the last label distance */
               last_label_dist = 0.0;
            }
         }
      }
   }
}


/*
 * Put in a label inside the plot
 * Draw the text , but only if the point is inbounds
 * This is used only to mark Contour lines
 */
static void drawPS2D_label( xm, ym, xn, yn, label)
double xm, ym, xn, yn;
char   *label;
{
   double angle;

   /* Calculate the angle of the label */
   if (fabs(xm-xn)< 1.0e-5)
      angle = -90.0;
   else
      angle = 360.0/(2*3.14159)*atan((yn-ym)/(xn-xm));

   /* Put on the label */
   (void) fprintf(ips,"setCtrLblFont\n");
   (void) fprintf(ips,"%.3g %.3g translate ",xm,ym);
   (void) fprintf(ips,"%.3g rotate (%s) plotlabel  %.3g rotate  ",
                  angle,label,-angle);
   (void) fprintf(ips,"%.3g %.3g translate\n",-xm,-ym);
}


/*
 * Plot line labels on the side if necessary
 */
static void plotPS2D_sidelabels()
{
   short  contclip;
   double xoffset;
   int    LABEL_FOUND = CNplotset_has_linelabels(plotdata);
   int    FCONT_FOUND = CNplotset_has_colored_contours(plotdata);

   /* Plot linelabels if necessary */
   if (LABEL_FOUND) {
      xoffset = 10*scale;
      if (FCONT_FOUND) xoffset += 45*scale;
      PXplotPS_linelabels(plotdata, hiddenline,
                         Pxmin, Pxmax, Pymin, Pymax, xoffset);
   }

   /* Plot colored scales if necessary */
   if (FCONT_FOUND) {
      xoffset  = 10*scale;
      contclip = CN_FALSE;
      if (contour_dptr) contclip = contour_dptr->data_pr.contclip;
      PXplotPS_contscale(cstephead, csteptail,
                         Pxmin, Pxmax, Pymin, Pymax, xoffset, contclip);
   }
}



/*
 * TRANSLATION ROUTINES
 */

/* 
 * Translate world to PS coordinates and apply log options
 * This is for local use only because it uses local static variables.
 */
static void trn_world_to_PS(xw, yw, xp, yp)
double  xw,  yw;                   /* World coordinates            */
double *xp, *yp;                   /* PS plot coordinates          */
{
   double pymin, pymax;
   int    error;
 
   /* If probability plot, apply a different scale to y-coords */
   if (probability_plot) {
      pymin = CNnorm_vert_distance(ymin, &error);
      pymax = CNnorm_vert_distance(ymax, &error);
   } else {
      pymin = ymin;
      pymax = ymax;
   }

   PXtranslate_world_to_PS(xw, yw, xp, yp,
                            Pxmin, Pxmax, Pymin, Pymax,
                            xmin, xmax, pymin, pymax,
                            xlog, ylog, xflip, yflip);
}


/* 
 * Translate world to PS coordinates.
 * Don't apply log options - the data has been previously converted to log
 * This is for local use only because it uses local static variables.
 */
static void trn_world_to_PS_nolog(xw, yw, xp, yp)
double  xw,  yw;                   /* World coordinates            */
double *xp, *yp;                   /* PS plot coordinates          */
{
   double pymin, pymax;
   int    error;
 
   /* If probability plot, apply a different scale to y-coords */
   if (probability_plot) {
      pymin = CNnorm_vert_distance(ymin, &error);
      pymax = CNnorm_vert_distance(ymax, &error);
   } else {
      pymin = ymin;
      pymax = ymax;
   }

   PXtranslate_world_to_PS(xw, yw, xp, yp,
                            Pxmin, Pxmax, Pymin, Pymax,
                            xmin, xmax, pymin, pymax,
                            CN_FALSE, CN_FALSE, xflip, yflip);
}

/*
 * Translate the y-coordinates to probability scale
 */
static double y_probability(y)
double y;
{
   double yp, pymin, pymax;
   int    error=CN_FALSE;
 
   /* This is done only for probability plots */
   if (!probability_plot) return(y);
 
   /* If y is outside [ymin,ymax] then return either ymin/ymax */
   pymin = CNnorm_vert_distance(0.000001, &error);
   pymax = CNnorm_vert_distance(0.999999, &error);
   if (y < 0.000001) return(pymin);
   if (y > 0.999999) return(pymax);
 
   /* Get the value on the probability scale */
   yp = CNnorm_vert_distance(y, &error);
   return(yp);
}



/*
 * OPTIMIZATION FOR DENSELY PACKED GRIDS
 */

/*
 * Plot the rectangular mesh in PostScript
 * Use the bitmap format which should be faster for large datasets
 * Return 0 if failed.
 * This routine uses either rectangles or the original array
 * If both exist, use the original array
 */
static int plotPS2D_bitmap_rects(Dptr,contfill)
CNdatasetptr Dptr;
int          contfill;   /* Draw multi-colored rectangles*/
{
   CNrectptr R;
   double    x1, y1, x2, y2, z;
   double    dx, dy, tmp;
   double    xa, xb, xc, ya, yb, yc;
   int       nx, ny, gnx, gny, ix, iy;
   int       i,j,m,n,colr;
   int       *narr=NULL;
   double    *zarr=NULL;

   /* Error checking - this only works for certain types of data */
   if (Dptr->data_pr.contbitmap == CN_FALSE) return(0);
   if (Dptr->data_pr.contintrp!=CN_RECTFLAT) return(0);
   if (!contfill || xlog || ylog) return(0);
   if (Dptr->recthead == NULL && Dptr->grid==NULL) return(0);

   /*
   (void) fprintf(stdout,"Trying bitmap format...\n");
    */

   /* Initialize bitmap size and scaling factors */
   nx = (int)(0.5*(Pxmax - Pxmin) + 1);
   ny = (int)(0.5*(Pymax - Pymin) + 1);
   dx = (xmax - xmin)/(double)nx;
   dy = (ymax - ymin)/(double)ny;

   /* Allocate arrays */
   narr = CNcreate_1D_int_array(nx*ny);
   zarr = CNcreate_1D_double_array(nx*ny);
   if (narr==NULL || zarr==NULL) {
      (void) fprintf(stderr,"Error! Couldn't create arrays!\n");
      if (narr) (void) free((char *)narr);
      if (zarr) (void) free((char *)zarr);
      return(0);
   }
 
   /* Initialize arrays */
   for (i=0; i<nx*ny; i++) {
      narr[i] = 0;
      zarr[i] = 0.0;
   }

   /* Go thru each rectangle and fill the arrays */
   if (Dptr->grid != NULL) {
      gnx = Dptr->grid->nx;
      gny = Dptr->grid->ny;

      /* Go thru the grid arrays */
      for (m=0; m<gnx; m++) {
 
         /* x-coordinates of rectangle */
         xb    = CNget_1D_double_array_value(Dptr->grid->xarray,m  ,gnx);
         if (m>0)
            xa = CNget_1D_double_array_value(Dptr->grid->xarray,m-1,gnx);
         else
            xa = xb;
         if (m<gnx-1)
            xc = CNget_1D_double_array_value(Dptr->grid->xarray,m+1,gnx);
         else
            xc = xb;
 
         /* Corners */
         x1 = (0.5*(xa + xb) - xmin)/dx;
         x2 = (0.5*(xb + xc) - xmin)/dx;
 
         /* make sure x1 < x2, y1 < y2 */
         if (x1 > x2) {
            tmp = x1;
            x1  = x2;
            x2  = tmp;
         }
 
         /* Check bounds */
         if (x1 > nx || x2 < 0) continue;
 
         /* Go thru the ygrid */
         for (n=0; n<gny; n++) {
            /* Find the corners of the rectangle (centered about [m,n]) */
 
            /* y-coordinates of rectangle */
            yb    = CNget_1D_double_array_value(Dptr->grid->yarray,n  ,gny);
           if (n>0)
               ya = CNget_1D_double_array_value(Dptr->grid->yarray,n-1,gny);
           else
               ya = yb;
            if (n<gny-1)
               yc = CNget_1D_double_array_value(Dptr->grid->yarray,n+1,gny);
           else
               yc = yb;
 
            /* corners */
            y1 = (0.5*(ya + yb) - ymin)/dy;
            y2 = (0.5*(yb + yc) - ymin)/dy;
 
            /* make sure y1 < y2 */
            if (y1 > y2) {
               tmp = y1;
               y1  = y2;
               y2  = tmp;
            }
 
            /* Check bounds */
            if (y1 > ny || y2 < 0) continue;
 
            /* z-value */
            z = CNget_1D_double_array_value(Dptr->grid->zarray,
                                            (m+n*gnx),gnx*gny);
 
            /* Figure out starting points */
            if (x1 <= 0.0) {
               ix = 0;
            } else {
               ix = floor(x1);
               if ((x1 - ix) > 0.5) ix++;
            }
            if (y1 <= 0.0) {
               iy = 0;
            } else {
               iy = floor(y1);
               if ((y1 - iy) > 0.5) iy++;
            }
 
            /* Now fill the arrays */
            for (i=ix; i<nx && i<=x2; i++)
            for (j=iy; j<ny && j<=y2; j++) {
               narr[i+j*nx] += 1;
               zarr[i+j*nx] += z;
            }
         }
      }
   } else {
      /* Go thru the rectangles */
      for (R=Dptr->recthead; R!=NULL; R=R->next) {
         /* Rectangle bounds */
         x1 = (R->n1->coord->x - xmin)/dx;
         x2 = (R->n3->coord->x - xmin)/dx;
         y1 = (R->n1->coord->y - ymin)/dy;
         y2 = (R->n3->coord->y - ymin)/dy;
         z  = R->n1->t;

         /* make sure x1 < x2, y1 < y2 */
         if (x1 > x2) {
            tmp = x1;
            x1  = x2;
            x2  = tmp;
         }
         if (y1 > y2) {
            tmp = y1;
            y1  = y2;
            y2  = tmp;
         }
   
         /* Check bounds */
         if (x1 > nx || x2 < 0 || y1 > ny || y2 < 0) continue;

         /* Figure out starting points */
         if (x1 <= 0.0) {
            ix = 0;
         } else {
            ix = floor(x1);
            if ((x1 - ix) > 0.5) ix++;
         }
         if (y1 <= 0.0) {
            iy = 0;
         } else {
            iy = floor(y1);
            if ((y1 - iy) > 0.5) iy++;
         }
      
         /*
         printf("x1=%g  ix=%d\n",x1,ix);
         printf("y1=%g  iy=%d\n",y1,iy);
          */

         /* Now fill the arrays */
         for (i=ix; i<nx && i<=x2; i++) 
         for (j=iy; j<ny && j<=y2; j++) {
            narr[i+j*nx] += 1;
            zarr[i+j*nx] += z;
         }
      }
   }

   /* Initialize the point buffer */
   init_point_buffer();

   /*
    * Draw the points
    */
   for (j=0; j<ny; j++) {
      for (i=0; i<nx; i++) {
         if (narr[i+j*nx] > 0) {
            z = zarr[i+j*nx]/(double)narr[i+j*nx];
            colr = find_contour_color(z, Dptr->data_pr.contclip);
            if (colr >=0 && colr < PX_MAX_FILL_COLORS) {
               x1 = Pxmin + (double)(2*i-1);
               y1 = Pymin + (double)(2*j-1);
               if (xflip) x1 = Pxmax - (double)(2*i-1);
               if (yflip) y1 = Pymax - (double)(2*j-1);
               buffer_point(x1, y1, colr);
            }
         }
      }
   }

   /* Flush the point buffer */
   flush_point_buffer();

   /* Reset color */
   PXsetColorPS(0);

   /* Free the arrays */
   if (narr) (void) free((char *)narr);
   if (zarr) (void) free((char *)zarr);

   /* Return */
   return(1);
}

/*
 * Find the contour color corresponding to the contour level
 */
static int find_contour_color(z, contclip)
double z;
int    contclip;
{
   CNcontstepptr C;
   double        min,max;
   int           nctrs, i, colr, zcolr;
   int           FOUND=CN_FALSE;

   /*
    * The contour steps are calculated outside so that there is
    * a common steps for all the contour datasets
    */
   nctrs = CNcount_contsteps(cstephead, csteptail) - 1;
 
   /* Loop through the contours - there will be a minimum of 2 steps */
   i = 0;
   for (C=cstephead; C!=NULL && !FOUND; C=C->next) {
      /*
       * There will be nctrs+1 bands; make sure each of these
       * has a distinct color.
       * Scale the colors from 1 to 32
       */
      colr      = (int)((double)(i*(PX_MAX_FILL_COLORS-1))/(double)nctrs) + 1;

      /* Increment the step number */
      i++;

      /* Set the clipping levels */
 
      /*
       * The contour steps selected in CNselect_contour_step()
       * do not necessarily cover the whole range [tmin..tmax],
       * i.e. cstephead->value approx cmin, and csteptail->value approx cmax
       * However in PXquery_contours() an additional step corresponding
       * to the the max step size CN_LARGE is automatically added.
       */
      if (C->prev == NULL) {
         max = C->value;
         min = -CN_LARGE;
         if (contclip) continue;
      } else {
         max = C->value;
         min = C->prev->value;
         if (contclip && max==CN_LARGE) continue;
      }

      /* Find the correct limit */
      if (min <= z && z < max) {
         /* Return the fill color index (0-32) */
         FOUND = CN_TRUE;
         zcolr = PXfillColorIndex(colr) - PX_MAX_NAMED_COLORS;
      }
   } 
 
   if (FOUND)
      return(zcolr);
   else
      return(-1);
}


/*
 * POINT BUFFERING
 * used to speed up drawing of colored points
 */

/*
 * Initialize the point buffer 
 */
static void init_point_buffer()
{
   int i;
   for (i=0; i<PX_MAX_FILL_COLORS; i++) {
      point_buffer[i].npoints = 0;
   }
   (void) fprintf(ips,"/DP {2 2 fillrect} def\n");
}

/*
 * Buffer a point
 */
/*ARGSUSED*/
static void buffer_point(x, y, color)
double x, y;
int color;
{
   int i;

   if (color < 0 || color >= PX_MAX_FILL_COLORS) {
      (void) fprintf(stderr,"Error! Color=%d!\n",color);
      return;
   }

   /*
    * If the polygon buffer is full, empty it 
    */
   if (point_buffer[color].npoints == MAX_POINTS - 1) {
      /*
       * IF the buffer is full, set the foreground color of the gc and
       * and draw the points 
       */
      PXsetColorPS(color + PX_MAX_NAMED_COLORS);
      for (i=0; i<point_buffer[color].npoints; i++) 
         (void) fprintf(ips,"%d %d DP\n",
                        (int)point_buffer[color].points[i].x,
                        (int)point_buffer[color].points[i].y);
      /* Reset the buffer */
      point_buffer[color].npoints = 0;
   }

   /* 
    * Save the point in the point buffer 
    */
   point_buffer[color].points[point_buffer[color].npoints].x = x;
   point_buffer[color].points[point_buffer[color].npoints].y = y;
   point_buffer[color].npoints++;
}

/*
 * flush the point buffer 
 */
static void flush_point_buffer()
{
   int color, i;

   /*
    * Go thru each buffer 
    */
   for (color=0; color<PX_MAX_FILL_COLORS; color++) {
      /*
       * IF there are any polygons in this buffer, draw it
       */      
      if (point_buffer[color].npoints > 0) {
         PXsetColorPS(color + PX_MAX_NAMED_COLORS);
         for (i=0; i<point_buffer[color].npoints; i++) 
            (void) fprintf(ips,"%d %d DP\n",
                           (int)point_buffer[color].points[i].x,
                           (int)point_buffer[color].points[i].y);
         /* Reset the buffer */
         point_buffer[color].npoints = 0;
      }
   }
}

