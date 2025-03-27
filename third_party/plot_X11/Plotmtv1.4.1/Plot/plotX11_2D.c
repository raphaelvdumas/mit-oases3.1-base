/*
 * plotX11_2D.c - all the X11 routines
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "plotX11.h"
#include "CNplot.h"

#define EXTREMELY_SMALL 1.0e-99

/* Max array size */
#define MAX_ARR_SIZE    1000

/* Structure for buffering polygons */
#define MAX_POINTS    100
#define MAX_POLYGONS  200
typedef struct poly_strct {
   XPoint points[MAX_POINTS];
   int    npoints;
} point_container;
struct {
   point_container polygons[PX_MAX_FILL_COLORS][MAX_POLYGONS];
   int             npolygons[PX_MAX_FILL_COLORS];
} poly_buffer;
static void init_polygon_buffer();
static void buffer_polygon();
static void flush_polygon_buffer();

/* Structure for buffering points */
static point_container point_buffer[PX_MAX_FILL_COLORS];
static void init_point_buffer();
static void buffer_point();
static void flush_point_buffer();

/*
 * FORWARD DECLARATIONS
 */
void   PXdrawplotX2D();
static void initXplot2D();
static void drawXplot2D();

static void axesX2D();
static void boundaryX2D();
static void plotX2D_xticks();
static void plotX2D_yticks();
static void plotX2D_Auto_xticks();
static void plotX2D_Auto_yticks();
static void plotX2D_User_xticks();
static void plotX2D_User_yticks();
static void plotX2D_Prob_yticks();
static void plotX2D_Bar_xticks();
#ifdef PLOT_XMINORGRID
static void plotX2D_xminorgrid();
#endif
static void plotX2D_yminorgrid();
static void plotX2D_xticklabel();
static void plotX2D_yticklabel();
static int  xticklabel_overlap();

static void plotX2D();
static void plotX2D_grid();
static void plotX2D_mesh4D();
static void plotX2D_mesh4D_cube();
static void plotX2D_vectors();
static void plotX2D_boundary();
static void plotX2D_trias();
static void plotX2D_rects();
static void plotX2D_polys();
static void plotX2D_single_solid_tria();
static void plotX2D_single_solid_rect();
static void plotX2D_single_solid_poly();
static void plotX2D_single_fill_tria();
static void plotX2D_single_fill_rect();
static void plotX2D_single_fill_poly();
static void plotX2D_nodes();
static void plotX2D_dataset_curves();
static void plotX2D_curve();
static void fillX2D_curve();
static void plotX2D_spline_curve();
static void plotX2D_markers();

static void plotX2D_curveID();
static void plotX2D_pointIDs();
static void plotX2D_nodeIDs();
static void plotX2D_triaIDs();
static void plotX2D_rectIDs();

static void annotateX2D();
static void plotX2D_single_annotation();
static void plotX2D_annot_rect();
static void plotX2D_annot_line();
static void plotX2D_annot_arrow();
static void plotX2D_arrow();
static void plotX2D_annot_point();
static void plotX2D_annot_text();
static void clipX2D_in_xmin();
static void clipX2D_in_ymin();
static void clipX2D_in_xmax();
static void clipX2D_in_ymax();

static void plotX2D_contlabel();
static int  drawX2D_label();
static void plotX2D_sidelabels();

static void   trn_world_to_X11();
static void   trn_world_to_X11_nolog();
static double y_probability();

static int plotX2D_bitmap_rects();
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
static short  innerticks;
static short  overlay;
static int    plotlabels;
static char   xlabel[MAXCHR];
static char   ylabel[MAXCHR];
static char   toplabel[MAXCHR];
static char   *comment;
static char   *subtitle;
static short  printdate;

/* Plot type */
static short  probability_plot;
static short  histogram_plot;
static short  barchart_plot;

/* Hiddenline - used to mirror 3D hiddenline plot */
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

/* Scaling factors */
static double idx, jdy;


/*
 * draw the plot
 */
void PXdrawplotX2D(pdata,
                   prtdate,
                   disp, wind, pix, gc0, gc1, font0, font1, 
                   width, height, xxmin, xxmax, xymin, xymax)
CNplotsetptr pdata;
int          prtdate;
Display      *disp;
Window       wind;
Pixmap       pix;
GC           gc0, gc1;
XFontStruct  *font0, *font1;
int          width, height;
int          *xxmin, *xxmax, *xymin, *xymax;
{
   /* Initialize static global variables */
   initXplot2D(pdata, prtdate, disp, wind, pix, gc0, gc1, font0, font1, 
               width, height);

   /* Draw the plot */
   drawXplot2D();

   /* return xxmin, xxmax etc */
   *xxmin = Xxmin;
   *xxmax = Xxmax;
   *xymin = Xymin;
   *xymax = Xymax;
}


/* 
 * initialize the plot window
 */
static void initXplot2D(pdata,
                        prtdate,
                        disp, wind, pix, gc0, gc1, font0, font1, 
                        width, height)
Display      *disp;
int          prtdate;
Window       wind;
Pixmap       pix;
GC           gc0, gc1;
XFontStruct  *font0, *font1;
int          width, height;
CNplotsetptr pdata;
{
   double lxmin, lxmax, lymin, lymax;
   double bxmin, bxmax, bymin, bymax, bzmin, bzmax;
   double ratio, dx, dy;
   int    Xxwid, Xywid;
   int    equalscale, fitpage;
   char   label[CN_MAXCHAR];

   /* Copy the variables to the static variables first */
   plotdata = pdata;

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
   printdate  = prtdate;

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

   /* Copy the X11-data to static variables */
   display      = disp;
   window       = wind;
   pixmap       = pix;
   font_info    = font0;
   lblfont_info = font1;
   gc           = gc0;
   gcl          = gc1;
   font_height  = font_info->max_bounds.ascent +
                  font_info->max_bounds.descent;
   font_width   = font_info->max_bounds.rbearing -
                  font_info->max_bounds.lbearing;
   lblfont_height=lblfont_info->max_bounds.ascent +
                  lblfont_info->max_bounds.descent;
   lblfont_width =lblfont_info->max_bounds.rbearing -
                  lblfont_info->max_bounds.lbearing;

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
   CNset_plotset_boundaries(pdata, 
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

   /* Initialize dimensions */
   Width = width;
   Height= height;
   Xxmin = DEFAULT_BDR_DIM;
   Xymin = DEFAULT_BDR_DIM;
   Xxwid = Width  - 2*DEFAULT_BDR_DIM;
   if (plotlabels) {
      /* plot_pr.slabellen defines the additional space needed for the labels */
      Xxwid = Xxwid - (plotlabels*LABEL_WIDTH + plotdata->plot_pr.slabellen);
   }
   Xywid = Height - 2*DEFAULT_BDR_DIM;
   if (Xxwid < 0) Xxwid = DEFAULT_BDR_DIM;
   if (Xywid < 0) Xywid = DEFAULT_BDR_DIM;
   Xxmax = Xxmin + Xxwid;
   Xymax = Xymin + Xywid;

   /* 
    * The size of the plot is Xxwid x Xywid;
    * try to fit the plot in the center of the window defined by
    * (0,0) - (Width, Height)
    */

   if (equalscale) {
      /* Fit the plot inside the window */
      dx = xmax - xmin;
      dy = ymax - ymin;
      ratio = dy/dx;
      if (ratio < ((double)Xywid)/((double)Xxwid)) {
         /* decrease ywid */
         Xywid  = (int)(Xxwid*ratio);
         Xymin  = (int)(0.5*(Xymax + Xymin) - 0.5*Xywid);
         Xymax  = Xymin + Xywid;
      } else {
         Xxwid  = (int)(Xywid/ratio);
         Xxmin  = (int)(0.5*(Xxmax + Xxmin) - 0.5*Xxwid);
         Xxmax  = Xxmin + Xxwid;
      }

   /*EMPTY*/
   } else if (fitpage) {
      /* The plot fills the whole page */
      ;

   } else {
      if (Xxwid*xyratio < Xywid) {
         Xywid  = (int)(Xxwid*xyratio);
         Xymin  = (int)(0.5*(Xymax + Xymin) - 0.5*Xywid);
         Xymax  = Xymin + Xywid;
      } else {
         Xxwid  = (int)(Xywid/xyratio);
         Xxmin  = (int)(0.5*(Xxmax + Xxmin) - 0.5*Xxwid);
         Xxmax  = Xxmin + Xxwid;
      }
   }

   /* initialize the scaling factors */
   dx = xmax - xmin;
   dy = ymax - ymin;
   idx = (Xxmax-Xxmin)/dx;
   jdy = (Xymax-Xymin)/(-dy);        /* because of strange X11 format */

}

/* 
 * Draw the plot in X11 
 */
/*ARGSUSED*/
static void drawXplot2D()
{
   XRectangle  rectangles[1];
   int itmp;

   /* Set contour levels for the contour-type datasets */
   cstephead = NULL;
   csteptail = NULL;
   PXquery_contours(plotdata,&cstephead,&csteptail,&contour_dptr);

   /* Draw the axes */
   axesX2D();

   /* clip if necessary */
   if (clip) {
      rectangles[0].x      = Xxmin;
      rectangles[0].y      = Xymin;
      rectangles[0].width  = Xxmax - Xxmin;
      rectangles[0].height = Xymax - Xymin;
      XSetClipRectangles(display,gc ,1,0,rectangles,1,Unsorted);
      XSetClipRectangles(display,gcl,1,0,rectangles,1,Unsorted);
   }

   /* Draw the plot */
   plotX2D();

   /* Redraw the boundary */
   boundaryX2D(&itmp);

   /* Unclip */
   if (clip) {
      XSetClipMask(display,gc ,None);
      XSetClipMask(display,gcl,None);
   }

   /* Draw the annotations */
   annotateX2D();

   /* Delete contour step-levels */
   CNdelete_contstep_list(&cstephead, &csteptail);
}

/*
 * draw the boundary, axes and labels of the plot and initialize its size
 */
static void axesX2D()
{
   char   text[100], c, time[MAXCHR];
   int    text_len, text_width, text_xpos, text_ypos;
   int    label_len;
   int    i;

   /* Initialize */
   XSetLineAttributes(display,gc,1,LineSolid,CapButt,JoinBevel);

   /* Draw the boundary and tick marks */
   boundaryX2D(&label_len);

   /* Don't redraw the grid */
   if (grid) grid = CN_FALSE;

   /* Top Label */
   text_len   = strlen(toplabel);
   text_width = XTextWidth(font_info,toplabel,text_len);
   text_xpos  = (Xxmin + Xxmax - text_width)/2;
   text_ypos  = Xymin - (font_height+lblfont_height);
   if (window)
   XDrawString(display,window,gc,text_xpos,text_ypos,toplabel,text_len);
   if (pixmap)
   XDrawString(display,pixmap,gc,text_xpos,text_ypos,toplabel,text_len);

   /* X-Axis Label */
   text_len   = strlen(xlabel);
   text_width = XTextWidth(font_info,xlabel,text_len);
   text_xpos  = (Xxmin + Xxmax - text_width)/2;
   text_ypos  = Xymax + (int)(3.5*font_height);
   if (text_ypos > Height) text_ypos = Height - 5;
   if (window)
   XDrawString(display,window,gc,text_xpos,text_ypos,xlabel,text_len);
   if (pixmap)
   XDrawString(display,pixmap,gc,text_xpos,text_ypos,xlabel,text_len);

   /* Y-Axis Label */
   /* Don't know how to rotate, so print character by character */
   text_len   = strlen(ylabel);
   text_xpos  = Xxmin - (label_len+1)*font_width - 12;
   if (text_xpos < 5) text_xpos = 5;
   text_ypos  = (Xymin + Xymax)/2 + (int)(0.5*font_height)
                - 0.50*(text_len-1)*(int)(1.2*font_height);
   for (i=0; (c=ylabel[i]) != '\0'; i++) {
      text[0] = c;
      if (window)
      XDrawString(display,window,gc,text_xpos,text_ypos,text,1);
      if (pixmap)
      XDrawString(display,pixmap,gc,text_xpos,text_ypos,text,1);
      text_ypos += (int)(1.2*font_height);
   }

   /* Comment */
   if (comment != NULL) {
      text_len   = strlen(comment);
      text_width = XTextWidth(font_info,comment,text_len);
      text_xpos  = Width - 10 - text_width;
      text_ypos  = 1.2*font_height;
      if (window)
      XDrawString(display,window,gc,text_xpos,text_ypos,comment,text_len);
      if (pixmap)
      XDrawString(display,pixmap,gc,text_xpos,text_ypos,comment,text_len);
   }

   /* Subtitle */
   if (subtitle != NULL) {
      text_len   = strlen(subtitle);
      text_width = XTextWidth(lblfont_info,subtitle,text_len);
      text_xpos  = (Xxmin + Xxmax - text_width)/2;
      text_ypos  = Xymin - lblfont_height;
      if (window)
      XDrawString(display,window,gcl,text_xpos,text_ypos,subtitle,text_len);
      if (pixmap)
      XDrawString(display,pixmap,gcl,text_xpos,text_ypos,subtitle,text_len);
   }

   /* Draw the plot labels */
   if (plotlabels) plotX2D_sidelabels();

   /* put on a time label */
   if (printdate) {
      CNget_localtime(time,MAXCHR);
      text_len   = strlen(time);
      text_width = XTextWidth(font_info,time,text_len);
      text_xpos  = 10 ;
      text_ypos  = 1.2*font_height;
      XSetForeground(display,gc,background_pixel);
      if (window)
      XFillRectangle(display,window,gc,text_xpos, 0, text_width, text_ypos);
      if (pixmap)
      XFillRectangle(display,pixmap,gc,text_xpos, 0, text_width, text_ypos);
      XSetForeground(display,gc,foreground_pixel);
      if (window)
      XDrawString(display,window,gc,text_xpos,text_ypos,time,text_len);
      if (pixmap)
      XDrawString(display,pixmap,gc,text_xpos,text_ypos,time,text_len);
   }
}

/*
 * Draw the boundary and tick marks of the plot.
 * This is done last 'cos the axes might have been 
 * obscured during the course of the plot.
 * The label-length controls the placement of the y-axis label.
 * This routine is called twice - once by axes() and another after plot().
 */
static void boundaryX2D(label_len)
int *label_len;
{
   /*
    * The label-length controls the placement of the y-axis labels
    */

   /* Draw the main axes */
   XSetLineAttributes(display,gc,1,LineSolid,CapButt,JoinBevel);
   if (window)
   PXDrawRectangle(display,window,gc,Xxmin,Xymin,Xxmax-Xxmin,Xymax-Xymin);
   if (pixmap)
   PXDrawRectangle(display,pixmap,gc,Xxmin,Xymin,Xxmax-Xxmin,Xymax-Xymin);

   /* Draw the tick marks and labels on the x-axis */
   if (barchart_plot) {

      /* Tickmarks based on 1st barchart dataset */
      plotX2D_Bar_xticks();

   } else if (plotdata->plot_pr.xlabelhead != NULL) {

      /* Tickmarks based on user-specification */
      plotX2D_User_xticks();

   } else if (!xautorange && !xlog) {

      /* Linear, fixed tickmarks */
      plotX2D_xticks();

   } else {
      /* This is the default method */

      /* If xlog then do lots of tiny tickmarks */
      plotX2D_Auto_xticks();

   }

   /* Draw the tick marks and labels on the y-axis */
   *label_len = 6;
   if (probability_plot) {

      /* Tickmarks in an erfc distribution */
      plotX2D_Prob_yticks(label_len);

   } else if (plotdata->plot_pr.ylabelhead != NULL) {

      /* Tickmarks based on user-specification */
      plotX2D_User_yticks(label_len);

   } else if (!yautorange && !ylog) {

      /* Linear, fixed tickmarks */
      plotX2D_yticks(label_len);

   } else {
      /* This is the default method */

      /* If ylog then do lots of tiny tickmarks */
      plotX2D_Auto_yticks(label_len);

   }
}


/* 
 * Draw the x-axis tick marks and labels 
 * This is NEVER used with log-scale
 */
static void plotX2D_xticks()
{
   double  xdx, xintv;
   double  vallbl;
   int     Xxtmp;
   int     i;
   int     explabel, precision;
   PXlabel axislabels[PX_MAX_LABELS];
   int     nlabels=0, lastlabel, doplot2;

   /* useful data */
   xintv = (Xxmax-Xxmin)/(double)xticks;
   xdx   = (xmax-xmin)/(double)xticks;

   /* xticks */
   for (i=0; i<=xticks; i++) {
      Xxtmp = Xxmin + (int)(i*xintv);
      /* Upper Ticks */
      if (window) XDrawLine(display,window,gc,Xxtmp,Xymin  ,Xxtmp,Xymin+8);
      if (pixmap) XDrawLine(display,pixmap,gc,Xxtmp,Xymin  ,Xxtmp,Xymin+8);

      /* Lower Ticks */
      if (innerticks) {
      if (window) XDrawLine(display,window,gc,Xxtmp,Xymax  ,Xxtmp,Xymax-8);
      if (pixmap) XDrawLine(display,pixmap,gc,Xxtmp,Xymax  ,Xxtmp,Xymax-8);
      } else {
      if (window) XDrawLine(display,window,gc,Xxtmp,Xymax  ,Xxtmp,Xymax+8);
      if (pixmap) XDrawLine(display,pixmap,gc,Xxtmp,Xymax  ,Xxtmp,Xymax+8);
      }

      /* Put on a label */
      vallbl = xmin  + i*xdx;
      if (xflip) vallbl = xmax - i*xdx;
      if (xlog) vallbl = pow(10.0,vallbl);
      vallbl = vallbl/xscale;
      PXadd_axislabel(axislabels, &nlabels, PX_MAX_LABELS,
                      vallbl, (double)Xxtmp, (double)Xymax);
   }

   /* Work on the grid */
   if (grid) {
      (void) PXlinetypX(CN_GD_DOTTED, 1);
      for (i=0; i<xticks; i++) {
#ifdef PLOT_XMINORGRID
         /* Minor grid */
         /* 
          * Don't need this routine, 'cos plotX2D_yminorgrid 
          * plots the minor grid on both x and y             
          */
         int j;
         for (j=1; j<5; j++)
            plotX2D_xminorgrid((int)(Xxmin + (i + j*0.2)*xintv));
#endif
         if (i==0) continue;

         /* Major grid */
         Xxtmp = Xxmin + (int)(i*xintv);
         if (window)
         XDrawLine(display,window,gc,Xxtmp,Xymin,Xxtmp,Xymax);
         if (pixmap)
         XDrawLine(display,pixmap,gc,Xxtmp,Xymin,Xxtmp,Xymax);
      }
      XSetLineAttributes(display,gc,1,LineSolid,CapButt,JoinBevel);
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
            plotX2D_xticklabel(axislabels[i].value,
                               axislabels[i].x,
                               axislabels[i].y,
                               (char*)NULL, precision, explabel, CN_FALSE);
            lastlabel = i;
         }
      }
   }
}

/* 
 * Draw the y-axis tick marks and labels 
 * This is NEVER used with log-scale
 */
static void plotX2D_yticks(label_len)
int  *label_len;
{
   double  ydy, yintv;
   double  vallbl;
   int     Xytmp;
   int     i, j, llen=0;
   int     explabel, precision;
   PXlabel axislabels[PX_MAX_LABELS];
   int     nlabels=0;

   /* useful data */
   yintv = (Xymax-Xymin)/(double)yticks;
   ydy   = (ymax-ymin)/(double)yticks;

   /* yticks */
   for (i=0; i<=yticks; i++) {
      Xytmp = Xymin + (int)(i*yintv);

      /* Left-side Ticks */
      if (innerticks) {
      if (window) XDrawLine(display,window,gc,Xxmin  ,Xytmp,Xxmin+8,Xytmp);
      if (pixmap) XDrawLine(display,pixmap,gc,Xxmin  ,Xytmp,Xxmin+8,Xytmp);
      } else {
      if (window) XDrawLine(display,window,gc,Xxmin  ,Xytmp,Xxmin-8,Xytmp);
      if (pixmap) XDrawLine(display,pixmap,gc,Xxmin  ,Xytmp,Xxmin-8,Xytmp);
      }

      /* Right-side Ticks */
      if (window) XDrawLine(display,window,gc,Xxmax  ,Xytmp,Xxmax-8,Xytmp);
      if (pixmap) XDrawLine(display,pixmap,gc,Xxmax  ,Xytmp,Xxmax-8,Xytmp);

      /* Put on a label */
      vallbl = ymax  - i*ydy;
      if (yflip) vallbl = ymin + i*ydy;
      if (ylog) vallbl = pow(10.0,vallbl);
      vallbl = vallbl/yscale;
      PXadd_axislabel(axislabels, &nlabels, PX_MAX_LABELS,
                      vallbl, (double)Xxmin, (double)Xytmp);
   }

   /* Work on the grid */
   if (grid) {
      (void) PXlinetypX(CN_GD_DOTTED,1);
      for (i=0; i<yticks; i++) {
         /* Minor grid */
         for (j=1; j<5; j++)
            plotX2D_yminorgrid((int)(Xymin + i*yintv + j*0.2*yintv));
         if (i==0) continue;

         /* Major grid */
         Xytmp = Xymin + (int)(i*yintv);
         if (window)
         XDrawLine(display,window,gc,Xxmin,Xytmp,Xxmax,Xytmp);
         if (pixmap)
         XDrawLine(display,pixmap,gc,Xxmin,Xytmp,Xxmax,Xytmp);
      }
      XSetLineAttributes(display,gc,1,LineSolid,CapButt,JoinBevel);
   }

   /* Now work on the labels */
   if (nlabels > 0) {
      PXfind_axis_precision(axislabels, nlabels, &precision, &explabel);
      for (i=0; i<nlabels; i++) {
         plotX2D_yticklabel(axislabels[i].value,
                            axislabels[i].x,
                            axislabels[i].y,
                            (char*)NULL, precision, explabel, CN_FALSE, &llen);
      }
   }

   /* return the label length */
   if (llen > *label_len) *label_len = llen;
}


/* 
 * Draw the x-axis tick marks and labels with automatic ranging 
 */
static void plotX2D_Auto_xticks()
{
   void    PXget_autorange();

   double  dmin, dmax, dmin2, dmax2;
   double  dtmp, dt, delta;
   int     Xxtmp;
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
    * X = Xxmin + (x-xmin)*(Xxmax-Xxmin)/(xmax-xmin) 
    * Y = Xymax + (y-ymin)*(Xymin-Xymax)/(ymax-ymin) 
    */
   
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
                      vallbl, (double)Xxmin, (double)Xymax);
   } else {
      PXadd_axislabel(axislabels, &nlabels, PX_MAX_LABELS,
                      vallbl, (double)Xxmax, (double)Xymax);
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
         Xxtmp = Xxmin + (int)((dtmp+(dmin2-dmin))*idx) + (int)(dt*idx);
         if (xflip)
         Xxtmp = Xxmax - (int)((dtmp+(dmin2-dmin))*idx) - (int)(dt*idx);

         /* Don't draw if this is outside the plot boundaries */
         if (Xxtmp < Xxmin || Xxtmp > Xxmax) continue;

         /* Draw the grid */
         if (grid) {
            if (i==10) {
               /* Major grid */
               (void) PXlinetypX(CN_GD_DOTTED,1);
               if (window)
                  XDrawLine(display,window,gc,Xxtmp,Xymin,Xxtmp,Xymax);
               if (pixmap)
                  XDrawLine(display,pixmap,gc,Xxtmp,Xymin,Xxtmp,Xymax);
               XSetLineAttributes(display,gc,1,LineSolid,CapButt,JoinBevel);

#ifdef PLOT_XMINORGRID
            } else {
               /* Minor grid */
               /* 
                * Don't need this routine, 'cos plotX2D_yminorgrid
                * plots the minor grid on both x and y             
                */
               plotX2D_xminorgrid(Xxtmp);
#endif
            }
         }

         /* The length of the tick depends on i */
         if      (i==10) tck = 8;  /* major tick */
         else if (i==5 ) tck = 6;  /* major sub-tick */
         else            tck = 4;  /* minor tick */

         /* Draw the ticks */

         /* Upper Ticks */
         if (window) XDrawLine(display,window,gc,Xxtmp,Xymin  ,Xxtmp,Xymin+tck);
         if (pixmap) XDrawLine(display,pixmap,gc,Xxtmp,Xymin  ,Xxtmp,Xymin+tck);

         /* Lower Ticks */
         if (innerticks) {
         if (window) XDrawLine(display,window,gc,Xxtmp,Xymax  ,Xxtmp,Xymax-tck);
         if (pixmap) XDrawLine(display,pixmap,gc,Xxtmp,Xymax  ,Xxtmp,Xymax-tck);
         } else {
         if (window) XDrawLine(display,window,gc,Xxtmp,Xymax  ,Xxtmp,Xymax+tck);
         if (pixmap) XDrawLine(display,pixmap,gc,Xxtmp,Xymax  ,Xxtmp,Xymax+tck);
         }

         /* Draw labels */
         if (i==10) {
            vallbl = dt+dtmp+dmin2;
            if (fabs(vallbl/delta) < 1e-10) vallbl=0.0; /* If x~=0 print 0 */
            if (xlog) vallbl = pow(10.0,vallbl);
            vallbl = vallbl/xscale;
            PXadd_axislabel(axislabels, &nlabels, PX_MAX_LABELS,
                            vallbl, (double)Xxtmp, (double)Xymax);
         }
      }
   }

   /* Put on the last tick */
   vallbl = dmax;
   if (xlog) vallbl = pow(10.0,vallbl);
   vallbl = vallbl/xscale;
   if (!xflip) {
      PXadd_axislabel(axislabels, &nlabels, PX_MAX_LABELS,
                      vallbl, (double)Xxmax, (double)Xymax);
   } else {
      PXadd_axislabel(axislabels, &nlabels, PX_MAX_LABELS,
                      vallbl, (double)Xxmin, (double)Xymax);
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
               plotX2D_xticklabel(axislabels[i].value,
                                  axislabels[i].x,
                                  axislabels[i].y,
                                  (char*)NULL, precision, explabel, CN_FALSE);
               lastlabel = i;
            }
         }
      }
   }
}


/* 
 * Draw the y-axis tick marks and labels with automatic ranging 
 */
static void plotX2D_Auto_yticks(label_len)
int  *label_len;
{
   void    PXget_autorange();

   double  dmin, dmax, dmin2, dmax2;
   double  dtmp, dt, delta;
   int     Xytmp;
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
    * X = Xxmin + (x-xmin)*(Xxmax-Xxmin)/(xmax-xmin) 
    * Y = Xymax + (y-ymin)*(Xymin-Xymax)/(ymax-ymin) 
    */
   
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
                      vallbl, (double)Xxmin, (double)Xymax);
   } else {
      PXadd_axislabel(axislabels, &nlabels, PX_MAX_LABELS,
                      vallbl, (double)Xxmin, (double)Xymin);
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
         Xytmp = Xymax + (int)((dtmp+(dmin2-dmin))*jdy) + (int)(dt*jdy);
         if (yflip)
         Xytmp = Xymin - (int)((dtmp+(dmin2-dmin))*jdy) - (int)(dt*jdy);

         /* Don't draw if this is outside the plot boundaries */
         if (Xytmp < Xymin || Xytmp > Xymax) continue;

         /* Draw the grid */
         if (grid) {
            if (i==10) {
               /* Major grid */
               (void) PXlinetypX(CN_GD_DOTTED,1);
               if (window)
                  XDrawLine(display,window,gc,Xxmin,Xytmp,Xxmax,Xytmp);
               if (pixmap)
                  XDrawLine(display,pixmap,gc,Xxmin,Xytmp,Xxmax,Xytmp);
               XSetLineAttributes(display,gc,1,LineSolid,CapButt,JoinBevel);
            } else {
               /* Minor grid */
               plotX2D_yminorgrid(Xytmp);
            }
         }

         /* The length of the tick depends on i */
         if      (i==10) tck = 8;  /* major tick */
         else if (i==5 ) tck = 6;  /* major sub-tick */
         else            tck = 4;  /* minor tick */

         /* Draw the ticks */

         /* Left-side Ticks */
         if (innerticks) {
         if (window) XDrawLine(display,window,gc,Xxmin  ,Xytmp,Xxmin+tck,Xytmp);
         if (pixmap) XDrawLine(display,pixmap,gc,Xxmin  ,Xytmp,Xxmin+tck,Xytmp);
         } else {
         if (window) XDrawLine(display,window,gc,Xxmin  ,Xytmp,Xxmin-tck,Xytmp);
         if (pixmap) XDrawLine(display,pixmap,gc,Xxmin  ,Xytmp,Xxmin-tck,Xytmp);
         }

         /* Right-side Ticks */
         if (window) XDrawLine(display,window,gc,Xxmax  ,Xytmp,Xxmax-tck,Xytmp);
         if (pixmap) XDrawLine(display,pixmap,gc,Xxmax  ,Xytmp,Xxmax-tck,Xytmp);

         /* Draw labels */
         if (i==10) {
            vallbl = dt+dtmp+dmin2;
            if (fabs(vallbl/delta) < 1e-10) vallbl=0.0; /* If y~=0 print 0 */
            if (ylog) vallbl = pow(10.0,vallbl);
            vallbl = vallbl/yscale;
            PXadd_axislabel(axislabels, &nlabels, PX_MAX_LABELS,
                            vallbl, (double)Xxmin, (double)Xytmp);
         }
      }
   }

   /* Put on the last tick */
   vallbl = dmax;
   if (ylog) vallbl = pow(10.0,vallbl);
   vallbl = vallbl/yscale;
   if (!yflip) {
      PXadd_axislabel(axislabels, &nlabels, PX_MAX_LABELS,
                      vallbl, (double)Xxmin, (double)Xymin);
   } else {
      PXadd_axislabel(axislabels, &nlabels, PX_MAX_LABELS,
                      vallbl, (double)Xxmin, (double)Xymax);
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
               else if (fabs(axislabels[i].y-axislabels[i+1].y) < 4*font_width)
                  doplot = CN_FALSE;
            }
         } else if (i==nlabels-1) {
            /* Don't plot the 1st label if it is too close to the second */
            if (nlabels >= 2) { 
               if (EQUAL(axislabels[i].value, axislabels[i-1].value))
                  doplot = CN_FALSE;
               else if (fabs(axislabels[i].y-axislabels[i-1].y) < 4*font_width)
                  doplot = CN_FALSE;
            }
         }
         if (doplot)
         plotX2D_yticklabel(axislabels[i].value,
                            axislabels[i].x,
                            axislabels[i].y,
                            (char*)NULL, precision, explabel, CN_FALSE, &llen);
      }
   }

   /* return the label length */
   if (llen > *label_len) *label_len = llen;
}


/* 
 * Draw the x-axis tick marks and labels using user-specified axis labels
 * This is NEVER used with log-scale
 */
static void plotX2D_User_xticks()
{
   CNaxislabelptr Aptr;
   double         xval;
   int            Xxtmp;
   char           label[CN_MAXCHAR];

   /*
    * useful data
    * X = Xxmin + (x-xmin)*(Xxmax-Xxmin)/(xmax-xmin)
    * Y = Xymax + (y-ymin)*(Xymin-Xymax)/(ymax-ymin)
    */

   /* Go thru the axis positions and plot labels */
   for (Aptr=plotdata->plot_pr.xlabelhead; Aptr!=NULL; Aptr=Aptr->next) {
      xval = Aptr->pos;

      /* Translate to plot-coordinates */
      Xxtmp = Xxmin + (xval - xmin)*idx;
      if (xflip)
      Xxtmp = Xxmax - (xval - xmin)*idx;

      /* Don't draw if this is outside the plot boundaries */
      if (Xxtmp < Xxmin || Xxtmp > Xxmax) continue;

      /* Put on labels */
      (void) strcpy(label, Aptr->name);
      plotX2D_xticklabel(xval, (double)Xxtmp, (double)Xymax,
                         label, 4, CN_FALSE, CN_FALSE);

      /* Put on tick marks */

      /* Upper Ticks */
      if (window) XDrawLine(display,window,gc,Xxtmp,Xymin  ,Xxtmp,Xymin+8);
      if (pixmap) XDrawLine(display,pixmap,gc,Xxtmp,Xymin  ,Xxtmp,Xymin+8);

      /* Lower Ticks */
      if (innerticks) {
      if (window) XDrawLine(display,window,gc,Xxtmp,Xymax  ,Xxtmp,Xymax-8);
      if (pixmap) XDrawLine(display,pixmap,gc,Xxtmp,Xymax  ,Xxtmp,Xymax-8);
      } else {
      if (window) XDrawLine(display,window,gc,Xxtmp,Xymax  ,Xxtmp,Xymax+8);
      if (pixmap) XDrawLine(display,pixmap,gc,Xxtmp,Xymax  ,Xxtmp,Xymax+8);
      }

      /* Draw the grid */
      if (grid) {
         /* Major grid */
         (void) PXlinetypX(CN_GD_DOTTED,1);
         if (window)
            XDrawLine(display,window,gc,Xxtmp,Xymin,Xxtmp,Xymax);
         if (pixmap)
            XDrawLine(display,pixmap,gc,Xxtmp,Xymin,Xxtmp,Xymax);
         XSetLineAttributes(display,gc,1,LineSolid,CapButt,JoinBevel);
      }
   }
}


/* 
 * Draw the y-axis tick marks and labels using user-specified axis labels
 * This is NEVER used with log-scale
 */
static void plotX2D_User_yticks(label_len)
int  *label_len;
{
   CNaxislabelptr Aptr;
   double         yval;
   int            Xytmp;
   char           label[CN_MAXCHAR];
   int            llen=0;

   /*
    * useful data
    * X = Xxmin + (x-xmin)*(Xxmax-Xxmin)/(xmax-xmin)
    * Y = Xymax + (y-ymin)*(Xymin-Xymax)/(ymax-ymin)
    */

   /* Go thru the axis positions and plot labels */
   for (Aptr=plotdata->plot_pr.ylabelhead; Aptr!=NULL; Aptr=Aptr->next) {
      yval = Aptr->pos;

      /* Translate to plot-coordinates */
      Xytmp = Xymax + (yval - ymin)*jdy;
      if (yflip)
      Xytmp = Xymin - (yval - ymin)*jdy;

      /* Don't draw if this is outside the plot boundaries */
      if (Xytmp < Xymin || Xytmp > Xymax) continue;

      /* Put on labels */
      (void) strcpy(label, Aptr->name);
      plotX2D_yticklabel(yval, (double)Xxmin, (double)Xytmp,
                         label, 4, CN_FALSE, CN_FALSE, &llen);

      /* Put on tick marks */

      /* Left-side Ticks */
      if (innerticks) {
      if (window) XDrawLine(display,window,gc,Xxmin,Xytmp,Xxmin+8,Xytmp);
      if (pixmap) XDrawLine(display,window,gc,Xxmin,Xytmp,Xxmin+8,Xytmp);
      } else {
      if (window) XDrawLine(display,window,gc,Xxmin,Xytmp,Xxmin-8,Xytmp);
      if (pixmap) XDrawLine(display,window,gc,Xxmin,Xytmp,Xxmin-8,Xytmp);
      }

      /* Right-side Ticks */
      if (window) XDrawLine(display,window,gc,Xxmax,Xytmp,Xxmax-8,Xytmp);
      if (pixmap) XDrawLine(display,window,gc,Xxmax,Xytmp,Xxmax-8,Xytmp);

      /* Draw the grid */
      if (grid) {
         /* Major grid */
         (void) PXlinetypX(CN_GD_DOTTED,1);
         if (window)
            XDrawLine(display,window,gc,Xxmin,Xytmp,Xxmax,Xytmp);
         if (pixmap)
            XDrawLine(display,pixmap,gc,Xxmin,Xytmp,Xxmax,Xytmp);
         XSetLineAttributes(display,gc,1,LineSolid,CapButt,JoinBevel);
      }
   }

   /* return the label length */
   if (llen > *label_len) *label_len = llen;
}


/* 
 * Draw the y-axis tick marks and labels on a probability plot scale
 * i.e. an error-function distribution
 */
static void plotX2D_Prob_yticks(label_len)
int  *label_len;
{
   double  upper_lim, lower_lim, x, yprob, delta;
   int     Xytmp;
   int     i, j, error;
   int     tck;
   int     llen=0;

   /*
    * Figure out the axis position (0-1) for 
    * x=0.0001, 0.001, 0.01, 0.1,
    *   0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 
    *   0.9, 0.99, 0.999, 0.9999
    *
    * x=0.0001 => 0
    * x=0.9999 => 1
    *
    * lower_lim => Xxmin    upper_lim => Xxmax
    *   (X-Xxmin)/(Xxmax-Xxmin) = (xprob-lower_lim)/(upper_lim-lower_lim)
    *
    * lower_lim => Xymax    upper_lim => Xymin
    *   (Y-Xymax)/(Xymin-Xymax) = (yprob-lower_lim)/(upper_lim-lower_lim)
    */

   
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
         Xytmp = Xymax + (Xymin-Xymax)*(yprob-lower_lim)/(upper_lim-lower_lim);
         if (yflip)
         Xytmp = Xymin + (Xymax-Xymin)*(yprob-lower_lim)/(upper_lim-lower_lim);

         /* Don't draw if this is outside the plot boundaries */
         if (Xytmp >= Xymin-1 && Xytmp <= Xymax+1) {

            /* Draw the grid */
            if (grid) {
               if (i==0) {
                  /* Major grid */
                  (void) PXlinetypX(CN_GD_DOTTED,1);
                  if (window)
                     XDrawLine(display,window,gc,Xxmin,Xytmp,Xxmax,Xytmp);
                  if (pixmap)
                     XDrawLine(display,pixmap,gc,Xxmin,Xytmp,Xxmax,Xytmp);
                  XSetLineAttributes(display,gc,1,LineSolid,CapButt,JoinBevel);
               } else {
                  /* Minor grid */
                  plotX2D_yminorgrid(Xytmp);
               }
            }
   
            /* The length of the tick depends on i */
            if      (i==0) tck = 8;  /* major tick */
            else if (i==5) tck = 6;  /* major sub-tick */
            else           tck = 4;  /* minor tick */
 
            /* Draw the ticks */

            /* Left-side Ticks */
            if (innerticks) {
            if (window)XDrawLine(display,window,gc,Xxmin,Xytmp,Xxmin+tck,Xytmp);
            if (pixmap)XDrawLine(display,pixmap,gc,Xxmin,Xytmp,Xxmin+tck,Xytmp);
            } else {
            if (window)XDrawLine(display,window,gc,Xxmin,Xytmp,Xxmin-tck,Xytmp);
            if (pixmap)XDrawLine(display,pixmap,gc,Xxmin,Xytmp,Xxmin-tck,Xytmp);
            }

            /* Right-side Ticks */
            if (window)XDrawLine(display,window,gc,Xxmax,Xytmp,Xxmax-tck,Xytmp);
            if (pixmap)XDrawLine(display,pixmap,gc,Xxmax,Xytmp,Xxmax-tck,Xytmp);

            /* Draw labels */
            if (i==0) 
            plotX2D_yticklabel(x*100.0,(double)Xxmin,(double)Xytmp,
                               (char*)NULL, 6, CN_FALSE, CN_FALSE, &llen);
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
         Xytmp = Xymax + (Xymin-Xymax)*(yprob-lower_lim)/(upper_lim-lower_lim);
         if (yflip)
         Xytmp = Xymin + (Xymax-Xymin)*(yprob-lower_lim)/(upper_lim-lower_lim);

         /* Don't draw if this is outside the plot boundaries */
         if (Xytmp >= Xymin-1 && Xytmp <= Xymax+1) {

            /* Draw the grid */
            if (grid) {
               if (i==0) {
                  /* Major grid */
                  (void) PXlinetypX(CN_GD_DOTTED,1);
                  if (window)
                     XDrawLine(display,window,gc,Xxmin,Xytmp,Xxmax,Xytmp);
                  if (pixmap)
                     XDrawLine(display,pixmap,gc,Xxmin,Xytmp,Xxmax,Xytmp);
                  XSetLineAttributes(display,gc,1,LineSolid,CapButt,JoinBevel);
               } else {
                  /* Minor grid */
                  plotX2D_yminorgrid(Xytmp);
               }
            }
   
            /* The length of the tick depends on i */
            if      (i==0) tck = 8;  /* major tick */
            else if (i==5) tck = 6;  /* major sub-tick */
            else           tck = 4;  /* minor tick */
 
            /* Draw the ticks */

            /* Left-side Ticks */
            if (innerticks) {
            if (window)XDrawLine(display,window,gc,Xxmin,Xytmp,Xxmin+tck,Xytmp);
            if (pixmap)XDrawLine(display,pixmap,gc,Xxmin,Xytmp,Xxmin+tck,Xytmp);
            } else {
            if (window)XDrawLine(display,window,gc,Xxmin,Xytmp,Xxmin-tck,Xytmp);
            if (pixmap)XDrawLine(display,pixmap,gc,Xxmin,Xytmp,Xxmin-tck,Xytmp);
            }

            /* Right-side Ticks */
            if (window)XDrawLine(display,window,gc,Xxmax,Xytmp,Xxmax-tck,Xytmp);
            if (pixmap)XDrawLine(display,pixmap,gc,Xxmax,Xytmp,Xxmax-tck,Xytmp);

            /* Draw labels */
            if (i==0) 
            plotX2D_yticklabel(x*100.0,(double)Xxmin,(double)Xytmp,
                               (char*)NULL, 4, CN_FALSE, CN_FALSE, &llen);
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
         /* The loop also goes from 0.9999990 to 0.9999999 */
         if (x > 0.999999 + CN_SMALL) continue;

         /* Get the value on the probability scale */
         yprob = CNnorm_vert_distance(x, &error);
        
         /* Translate to plot-coordinates */
         Xytmp = Xymax + (Xymin-Xymax)*(yprob-lower_lim)/(upper_lim-lower_lim);
         if (yflip)
         Xytmp = Xymin + (Xymax-Xymin)*(yprob-lower_lim)/(upper_lim-lower_lim);

         /* Don't draw if this is outside the plot boundaries */
         if (Xytmp >= Xymin-1 && Xytmp <= Xymax+1) { 

            /* Draw the grid */
            if (grid) {
               if (i==0) {
                  /* Major grid */
                  (void) PXlinetypX(CN_GD_DOTTED,1);
                  if (window)
                     XDrawLine(display,window,gc,Xxmin,Xytmp,Xxmax,Xytmp);
                  if (pixmap)
                     XDrawLine(display,pixmap,gc,Xxmin,Xytmp,Xxmax,Xytmp);
                  XSetLineAttributes(display,gc,1,LineSolid,CapButt,JoinBevel);
               } else {
                  /* Minor grid */
                  plotX2D_yminorgrid(Xytmp);
               }
            }
   
            /* The length of the tick depends on i */
            if      (i==0) tck = 8;  /* major tick */
            else if (i==5) tck = 6;  /* major sub-tick */
            else           tck = 4;  /* minor tick */
 
            /* Draw the ticks */

            /* Left-side Ticks */
            if (innerticks) {
            if (window)XDrawLine(display,window,gc,Xxmin,Xytmp,Xxmin+tck,Xytmp);
            if (pixmap)XDrawLine(display,pixmap,gc,Xxmin,Xytmp,Xxmin+tck,Xytmp);
            } else {
            if (window)XDrawLine(display,window,gc,Xxmin,Xytmp,Xxmin-tck,Xytmp);
            if (pixmap)XDrawLine(display,pixmap,gc,Xxmin,Xytmp,Xxmin-tck,Xytmp);
            }

            /* Right-side Ticks */
            if (window)XDrawLine(display,window,gc,Xxmax,Xytmp,Xxmax-tck,Xytmp);
            if (pixmap)XDrawLine(display,pixmap,gc,Xxmax,Xytmp,Xxmax-tck,Xytmp);

            /* Draw labels */
            if (i==0) 
            plotX2D_yticklabel(x*100.0,(double)Xxmin,(double)Xytmp,
                               (char*)NULL, 6, CN_FALSE, CN_FALSE, &llen);
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
static void plotX2D_Bar_xticks()
{
   CNdatasetptr Dptr=NULL;
   CNdslistptr  DS;
   CNbinptr     Binptr;
   int          FOUND;
   double       xval;
   int          Xxtmp;
   char         label[CN_MAXCHAR];
   double       bin_width, text_width;
   int          max_length, label_length, i, use_lblfont;

   /*
    * useful data
    * X = Xxmin + (x-xmin)*(Xxmax-Xxmin)/(xmax-xmin)
    * Y = Xymax + (y-ymin)*(Xymin-Xymax)/(ymax-ymin)
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
         plotX2D_xticks();
      } else {
         /* If xlog then do lots of tiny tickmarks */
         plotX2D_Auto_xticks();
      }
      return;
   }

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
   use_lblfont = CN_FALSE;
   if ((Binptr=Dptr->barchart->binhead) != NULL) {
      /* Bin width in plot dimensions */
      bin_width  = fabs(Binptr->xmax - Binptr->xmin) * idx;

      /* Check the max length to see if the longest label will fit in the bin */
      text_width = XTextWidth(font_info, label, strlen(label));

      if (text_width > bin_width) {

         /* Now check to see if using a smaller font will do the trick */
         text_width = XTextWidth(lblfont_info, label, strlen(label));
         use_lblfont = CN_TRUE;

         if (text_width > bin_width) {

            /* Need to truncate the string */
            (void) strcpy(label,"W");
            FOUND = CN_FALSE;
            for (i=1; i<max_length && !FOUND; i++) {
               text_width = XTextWidth(font_info, label, strlen(label));
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
      Xxtmp = Xxmin + (xval - xmin)*idx;
      if (xflip)
      Xxtmp = Xxmax - (xval - xmin)*idx;

      /* Don't draw if this is outside the plot boundaries */
      if (Xxtmp < Xxmin || Xxtmp > Xxmax) continue;

      /* Put on labels */
      (void) strncpy(label, Binptr->name, max_length);
      label[max_length] = '\0';
      plotX2D_xticklabel(xval, (double)Xxtmp, (double)Xymax,
                         label, 4, CN_FALSE, use_lblfont);
   }

   /* Go thru the Bins and plot tickmarks */
   for (Binptr=Dptr->barchart->binhead; Binptr!=NULL; Binptr=Binptr->next) {
      /* Draw the tick at the beginning of the bin */
      xval = Binptr->xmin;

      /* Translate to plot-coordinates */
      Xxtmp = Xxmin + (xval - xmin)*idx;
      if (xflip)
      Xxtmp = Xxmax - (xval - xmin)*idx;

      /* Don't draw if this is outside the plot boundaries */
      if (Xxtmp < Xxmin || Xxtmp > Xxmax) continue;

      /* Upper Ticks */
      if (window) XDrawLine(display,window,gc,Xxtmp,Xymin  ,Xxtmp,Xymin+8);
      if (pixmap) XDrawLine(display,pixmap,gc,Xxtmp,Xymin  ,Xxtmp,Xymin+8);

      /* Lower Ticks */
      if (innerticks) {
      if (window) XDrawLine(display,window,gc,Xxtmp,Xymax  ,Xxtmp,Xymax-8);
      if (pixmap) XDrawLine(display,pixmap,gc,Xxtmp,Xymax  ,Xxtmp,Xymax-8);
      } else {
      if (window) XDrawLine(display,window,gc,Xxtmp,Xymax  ,Xxtmp,Xymax+8);
      if (pixmap) XDrawLine(display,pixmap,gc,Xxtmp,Xymax  ,Xxtmp,Xymax+8);
      }

      if (Binptr->next != NULL) continue;

      /* Draw the tick at the end of the bin */
      xval = Binptr->xmax;

      /* Translate to plot-coordinates */
      Xxtmp = Xxmin + (xval - xmin)*idx;
      if (xflip)
      Xxtmp = Xxmax - (xval - xmin)*idx;

      /* Don't draw if this is outside the plot boundaries */
      if (Xxtmp < Xxmin || Xxtmp > Xxmax) continue;

      /* Upper Ticks */
      if (window) XDrawLine(display,window,gc,Xxtmp,Xymin  ,Xxtmp,Xymin+8);
      if (pixmap) XDrawLine(display,pixmap,gc,Xxtmp,Xymin  ,Xxtmp,Xymin+8);

      /* Lower Ticks */
      if (innerticks) {
      if (window) XDrawLine(display,window,gc,Xxtmp,Xymax  ,Xxtmp,Xymax-8);
      if (pixmap) XDrawLine(display,pixmap,gc,Xxtmp,Xymax  ,Xxtmp,Xymax-8);
      } else {
      if (window) XDrawLine(display,window,gc,Xxtmp,Xymax  ,Xxtmp,Xymax+8);
      if (pixmap) XDrawLine(display,pixmap,gc,Xxtmp,Xymax  ,Xxtmp,Xymax+8);
      }
   }
}


#ifdef PLOT_XMINORGRID
/* plot the minor grid parallel to the y-axis (x=constant) */
static void plotX2D_xminorgrid(Xxval)
int Xxval;
{
   void   PXget_autorange();
   int    i, j, yval;   
   int    itick, nticks;
   int    Xytmp;
   double yintv, dmin, dmax, dmin2, dmax2, delta, dt, dtmp;

   /* Branch according to whether autoranging/log */
   if (!yautorange && !ylog) {

      /* Calculate the intervals */
      yintv = (Xymax-Xymin)/(double)yticks;

      /* Draw dots */
      for (i=0; i<yticks; i++) {
         for (j=1; j<5; j++) {
            Xyval = Xymin + (i + j*0.2)*yintv; 
            if (window)
            XDrawPoint(display,window,gc,Xxval,Xyval);
            if (pixmap)
            XDrawPoint(display,pixmap,gc,Xxval,Xyval);
         }
      }

   } else {

      /* This duplicates autorange plotting on the y-axis */

      /* Get the rounded intervals */
      PXget_autorange(ymin,ymax,
                     &dmin,&dmax,&dmin2,&dmax2,&delta,ylog,yautorange);

      /* Draw the tickmarks from dmin2 to dmax2 - dtmp is distance from dmin2 */
      if (delta <= 0.0) nticks = 0;
      else              nticks = (int)((dmax2 - dmin2)/delta) + 1;
      for (itick=0; itick<nticks; itick++) {
         dtmp = itick*delta;
         for (i=1; i<10; i++) {
            /* distance from dtmp - actual phy loc is dt+dtmp+dmin2 */
            dt = i*0.1*delta;
            if (ylog) dt = log10((double)i);
            if (ylog && i==1) continue;  /* Don't do this case */
 
            /* Translate to plot-coordinates */
            Xytmp = Xymax + (int)((dtmp+(dmin2-dmin))*jdy) + (int)(dt*jdy);
            if (yflip)
            Xytmp = Xymin - (int)((dtmp+(dmin2-dmin))*jdy) - (int)(dt*jdy);
 
            /* Don't draw if this is outside the plot boundaries */
            if (Xytmp < Xymin || Xytmp > Xymax) continue;

            /* Draw dot */
            Xyval  = Xytmp;
            if (window)
            XDrawPoint(display,window,gc,Xxval,Xyval);
            if (pixmap)
            XDrawPoint(display,pixmap,gc,Xxval,Xyval);
         }
      }
   }
}
#endif

/* plot the minor grid parallel to the x-axis (y=constant) */
static void plotX2D_yminorgrid(Xyval)
int Xyval;
{
   void           PXget_autorange();
   int            i, j, Xxval;   
   int            itick, nticks;
   int            Xxtmp;
   double         xintv, dmin, dmax, dmin2, dmax2, delta, dt, dtmp;
   CNdatasetptr   Dptr=NULL;
   CNdslistptr    DS;
   CNbinptr       Binptr;
   int            FOUND;
   double         xval;

   /* Branch according to whether xautoranging/xlog */
   if (barchart_plot) {

      /* Barchart gridding is treated specially in X */
      /* This code duplicates that of plotX2D_Bar_xticks() */

      /* Find the barchart dataset */
      FOUND = CN_FALSE;
      for (DS=plotdata->datahead; DS!=NULL && !FOUND; DS=DS->next) {
         if (DS->Dptr && DS->Dptr->datatype==CN_BARCHART && DS->Dptr->barchart){
            FOUND = CN_TRUE;
            Dptr  = DS->Dptr;
         }
      }

      /* If there isn't a barchart, do the normal ticks */
      if (!FOUND) {
         /* Calculate the intervals */
         xintv = (Xxmax-Xxmin)/(double)xticks;

         /* Draw dots */
         for (i=0; i<xticks; i++) {
            for (j=0; j<5; j++) {
               Xxval = Xxmin + (i + j*0.2)*xintv; 
               if (window)
               XDrawPoint(display,window,gc,Xxval,Xyval);
               if (pixmap)
               XDrawPoint(display,pixmap,gc,Xxval,Xyval);
            }
         }
      } else {
         /* Draw dots in between the bins */
         for (Binptr=Dptr->barchart->binhead;Binptr!=NULL;Binptr=Binptr->next){
            for (i=0; i<=5; i++) {
               /* Translate to plot-coordinates */
               xval = Binptr->xmin + i*0.2*(Binptr->xmax - Binptr->xmin);
               Xxtmp = Xxmin + (xval - xmin)*idx;
               if (xflip)
               Xxtmp = Xxmax - (xval - xmin)*idx;

               /* Don't draw if this is outside the plot boundaries */
               if (Xxtmp < Xxmin || Xxtmp > Xxmax) continue;

               Xxval = Xxtmp;
               if (window)
               XDrawPoint(display,window,gc,Xxval,Xyval);
               if (pixmap)
               XDrawPoint(display,pixmap,gc,Xxval,Xyval);
            }
         }
      }

   } else if (plotdata->plot_pr.xlabelhead != NULL) {

      /* Don't need to do anything here */

   } else if (!xautorange && !xlog) {

      /* Calculate the intervals */
      xintv = (Xxmax-Xxmin)/(double)xticks;

      /* Draw dots */
      for (i=0; i<xticks; i++) {
         for (j=1; j<5; j++) {
            Xxval = Xxmin + (i + j*0.2)*xintv; 
            if (window)
            XDrawPoint(display,window,gc,Xxval,Xyval);
            if (pixmap)
            XDrawPoint(display,pixmap,gc,Xxval,Xyval);
         }
      }

   } else {

      /* This duplicates autorange plotting on the x-axis */

      /* Get the rounded intervals */
      PXget_autorange(xmin,xmax,
                     &dmin,&dmax,&dmin2,&dmax2,&delta,xlog,xautorange);

      /* Draw the tickmarks from dmin2 to dmax2 - dtmp is distance from dmin2 */
      if (delta <= 0.0) nticks = 0;
      else              nticks = (int)((dmax2 - dmin2)/delta) + 1;
      for (itick=0; itick<nticks; itick++) {
         dtmp = itick*delta;
         for (i=1; i<10; i++) {
            /* distance from dtmp - actual phy loc is dt+dtmp+dmin2 */
            dt = i*0.1*delta;
            if (xlog) dt = log10((double)i);
            if (xlog && i==1) continue;  /* Don't do this case */

            /* Translate to plot-coordinates */
            Xxtmp = Xxmin + (int)((dtmp+(dmin2-dmin))*idx) + (int)(dt*idx);
            if (xflip)
            Xxtmp = Xxmax - (int)((dtmp+(dmin2-dmin))*idx) - (int)(dt*idx);
 
            /* Don't draw if this is outside the plot boundaries */
            if (Xxtmp < Xxmin || Xxtmp > Xxmax) continue;

            /* Draw dot */
            Xxval = Xxtmp;
            if (window)
            XDrawPoint(display,window,gc,Xxval,Xyval);
            if (pixmap)
            XDrawPoint(display,pixmap,gc,Xxval,Xyval);
         }
      }
   }
}


/* plot the tick label on the x-axis */
/*ARGSUSED*/
static void plotX2D_xticklabel(vallbl,xpos,ypos,label,
                               precision,explabel,use_lblfont)
double vallbl;
double xpos, ypos;
char   *label;
int    precision, explabel;
int    use_lblfont;
{
   int    text_len, text_width, text_xpos, text_ypos;
   char   text[MAXCHR];
   char   exponent[MAXCHR];
   char   nonexponent[MAXCHR];
   int    isexp=CN_FALSE;

   if (explabel)
      (void) sprintf(text,"%.*e",precision,vallbl);
   else
      (void) sprintf(text,"%.*g",precision,vallbl);
   if (label != NULL) (void)strcpy(text,label);

   /* Check for exponents */
   if (label == NULL) 
      PXmodify_explabel(text, exponent, nonexponent, &isexp, CN_FALSE);

   if (!use_lblfont) {
      text_len   = strlen(text);
      text_width = XTextWidth(font_info,text,text_len);
      text_xpos  = (int)xpos  - text_width/2;
      text_ypos  = (int)ypos + 5 + (int)(1.5*font_height);
      if (window)
      XDrawString(display,window,gc,text_xpos,text_ypos,text,text_len);
      if (pixmap)
      XDrawString(display,pixmap,gc,text_xpos,text_ypos,text,text_len);
   } else {
      text_len   = strlen(text);
      text_width = XTextWidth(lblfont_info,text,text_len);
      text_xpos  = (int)xpos  - text_width/2;
      text_ypos  = (int)ypos + 5 + (int)(1.5*lblfont_height);
      if (window)
      XDrawString(display,window,gcl,text_xpos,text_ypos,text,text_len);
      if (pixmap)
      XDrawString(display,pixmap,gcl,text_xpos,text_ypos,text,text_len);
   }
}


/* plot the tick label on the y-axis */
/*ARGSUSED*/
static void plotX2D_yticklabel(vallbl,xpos,ypos,label,
                               precision,explabel,use_lblfont,llen)
double vallbl;
double xpos, ypos;
char   *label;
int    precision, explabel;
int    use_lblfont;
int    *llen;
{
   int    text_len, text_width, text_xpos, text_ypos;
   char   text[MAXCHR];
   char   exponent[MAXCHR];
   char   nonexponent[MAXCHR];
   int    isexp=CN_FALSE;

   if (explabel)
      (void) sprintf(text,"%.*e",precision,vallbl);
   else
      (void) sprintf(text,"%.*g",precision,vallbl);
   if (label != NULL) (void)strcpy(text,label);

   /* Check for exponents */
   if (label == NULL) 
      PXmodify_explabel(text, exponent, nonexponent, &isexp, CN_FALSE);

   if (!use_lblfont) {
      text_len   = strlen(text);
      if (text_len > *llen) *llen = text_len;
      text_width = XTextWidth(font_info,text,text_len);
      text_xpos  = (int)xpos - 5 - text_width - (int)(0.5*font_width);
      text_ypos  = (int)ypos + (int)(0.5*font_height);
      if (window)
      XDrawString(display,window,gc,text_xpos,text_ypos,text,text_len);
      if (pixmap)
      XDrawString(display,pixmap,gc,text_xpos,text_ypos,text,text_len);
   } else {
      text_len   = strlen(text);
      if (text_len > *llen) *llen = text_len;
      text_width = XTextWidth(lblfont_info,text,text_len);
      text_xpos  = (int)xpos - 5 - text_width - (int)(0.5*lblfont_width);
      text_ypos  = (int)ypos + (int)(0.5*lblfont_height);
      if (window)
      XDrawString(display,window,gcl,text_xpos,text_ypos,text,text_len);
      if (pixmap)
      XDrawString(display,pixmap,gcl,text_xpos,text_ypos,text,text_len);
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
   int    text1_width, text2_width, text_width, text_spacing, tick_distance;

   /* Assume that the two labels are not of equal value */
   if (explabel) {
      (void) sprintf(text1,"%.*e",precision,label1.value);
      (void) sprintf(text2,"%.*e",precision,label2.value);
   } else {
      (void) sprintf(text1,"%.*g",precision,label1.value);
      (void) sprintf(text2,"%.*g",precision,label2.value);
   }
   /* Modify the labels - strip characters from exponents */
   PXmodify_explabel(text1, exponent, nonexponent, &isexp, CN_FALSE);
   PXmodify_explabel(text2, exponent, nonexponent, &isexp, CN_FALSE);

   /* Calculate text widths */
   text1_width  = XTextWidth(font_info,text1,strlen(text1));
   text2_width  = XTextWidth(font_info,text2,strlen(text2));
   text_spacing = 5;
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
static void plotX2D()
{
   CNdslistptr  DS, ds;
   CNdatasetptr Dptr;
   int          colrinc=0, lineinc=0;
   int          contfill, meshplot;
   int          PARENT_FOUND = CN_FALSE;
   int          sc_plot;

   /* Grids and contours are only drawn if the plot is a scientific plot */
   sc_plot = !probability_plot && !histogram_plot && !barchart_plot;

   /*
    * Plot set - draw the grid if that exists
    */
   for (DS=plotdata->datahead; DS!=NULL && sc_plot; DS=DS->next) {
      if (DS->Dptr->grid && DS->Dptr->datatype == CN_GRID4D) 
         plotX2D_grid(DS->Dptr->grid, 
                      DS->Dptr->data_pr.contintrp,
                      DS->Dptr->data_pr.contclip, 
                      DS->Dptr->data_pr.meshplot);
   }

   /*
    * Plot set - draw the mesh4D grid and related quants if that exists
    */
   for (DS=plotdata->datahead; DS!=NULL && sc_plot; DS=DS->next) {
      if (DS->Dptr->mesh4D)
         plotX2D_mesh4D(DS->Dptr);
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
         plotX2D_boundary(DS->Dptr->parent,
                          (int)Dptr->data_pr.boundary,
                          (int)Dptr->data_pr.regbound,
                          (int)Dptr->data_pr.fillbnd,
                          (int)Dptr->data_pr.pr_rgID,
                          1,0);
      } else {
         plotX2D_boundary(DS->Dptr,
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

      /* Draw colored triangles/rectangles but not the curves */
      contfill = ( (DS->Dptr->data_pr.contstyle == CN_FILLCONT) || 
                   (DS->Dptr->data_pr.contstyle == CN_LNFILLCONT) );
      meshplot = DS->Dptr->data_pr.contstyle == CN_PLOTMESH;
      meshplot = meshplot | DS->Dptr->data_pr.meshplot;
      if (contfill || meshplot) {
         plotX2D_trias(DS->Dptr, contfill, meshplot);
         plotX2D_rects(DS->Dptr, contfill, meshplot);
         plotX2D_polys(DS->Dptr, contfill, meshplot);
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
            plotX2D_dataset_curves(DS->Dptr,colrinc,lineinc);
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
      plotX2D_dataset_curves(DS->Dptr,colrinc,lineinc);

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
         plotX2D_vectors(DS->Dptr->vecbox,
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
            plotX2D_pointIDs(DS->Dptr->parent->pointhead, 
                             DS->Dptr->parent->pointtail, CN_FALSE);
         } else {
            plotX2D_pointIDs(Dptr->pointhead, Dptr->pointtail, CN_FALSE);
         }
      }
      if (DS->Dptr->data_pr.pr_ndID && sc_plot) 
         plotX2D_nodeIDs (Dptr->nodehead,  Dptr->nodetail);
      if (DS->Dptr->data_pr.pr_trID && sc_plot) 
         plotX2D_triaIDs (Dptr->triahead,  Dptr->triatail);
      if (DS->Dptr->data_pr.pr_rtID && sc_plot) 
         plotX2D_rectIDs (Dptr->recthead,  Dptr->recttail);
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
         plotX2D_boundary(DS->Dptr->parent,
                          (int)Dptr->data_pr.boundary,
                          (int)Dptr->data_pr.regbound,
                          (int)Dptr->data_pr.fillbnd,
                          (int)Dptr->data_pr.pr_rgID,
                          0,1);
      } else {
         plotX2D_boundary(DS->Dptr,
                          (int)DS->Dptr->data_pr.boundary,
                          (int)DS->Dptr->data_pr.regbound,
                          (int)DS->Dptr->data_pr.fillbnd,
                          (int)DS->Dptr->data_pr.pr_rgID,
                          0,1);
      }
   }
}


/*
 * Plot the grid in X11
 */
static void plotX2D_grid(grid, contintrp, contclip, meshplot)
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
   fillcolor = PXpolyColorIndexX(1);   /* yellow */
   linecolor = PXpolyColorIndexX(4);   /* red    */

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
 
      /* Initialize the polygon buffer */
      init_polygon_buffer();

      /* Solid Fill */
      XSetFillStyle(display, gc, FillSolid);

      /* Loop through the rectangles */
      for (R=slice->recthead; R!=NULL; R=R->next) {
         /* Quick check to see if the rectangle is in-bounds */
         if (!CNrect_in_bounds(R,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax)) continue;
 
         /* Plot the color fills */
         plotX2D_single_fill_rect(R,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax,
              logx, logy, logz, logt, contclip);

      }

      /* Loop through the triangles */
      for (T=slice->triahead; T!=NULL; T=T->next) {
         /* Quick check to see if the triangle is in-bounds */
         if (!CNtria_in_bounds(T,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax)) continue;
 
         /* Plot the color fills */
         plotX2D_single_fill_tria(T,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax,
              logx, logy, logz, logt, contclip);
      }

      /* Flush the polygon buffer */
      flush_polygon_buffer();

      /* Loop through the rectangles */
      for (R=slice->recthead; R!=NULL; R=R->next) {
         /* Quick check to see if the rectangle is in-bounds */
         if (!CNrect_in_bounds(R,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax)) continue;

         /* Plot the mesh */
         if (meshplot)
         plotX2D_single_solid_rect(R,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax,
              logx, logy, logz, logt,
              filltype, fillcolor, linetype, linecolor);
      } 

      /* Loop through the triangles */
      for (T=slice->triahead; T!=NULL; T=T->next) {
         /* Quick check to see if the triangle is in-bounds */
         if (!CNtria_in_bounds(T,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax)) continue;

         /* Plot the mesh */
         if (meshplot)
         plotX2D_single_solid_tria(T,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax,
              logx, logy, logz, logt, 
              filltype, fillcolor, linetype, linecolor);

      }
 
      /* Delete the slice */
      CNdelete_slice(slice);

      /* Flush the polygon buffer */
      flush_polygon_buffer();

      /* Reset color */
      PXsetColorX(0);
   }
}


/*
 * Plot the mesh4D in X11
 */
static void plotX2D_mesh4D(dptr)
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
      plotX2D_mesh4D_cube(B,
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
static void plotX2D_mesh4D_cube(B,
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
         plotX2D_single_fill_poly(P,
                      cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax,
                      contclip);
 
      /* Plot the mesh (must do this for grid) */
      if ((meshplot) || (mesh4D_quant && nocont)) {
         filltype  = CN_FILL_SOLID;
         if (mesh4D_quant && !nocont) filltype = CN_FILL_NONE;
         fillcolor = PXpolyColorIndexX(color);
         linetype  = (meshplot) ? CN_LN_SOLID : CN_LN_NONE;
         linecolor = PXpolyColorIndexX(4);   /* red    */
         plotX2D_single_solid_poly(P,
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
 * Plot the vectors in X11
 */
static void plotX2D_vectors(Vbox, vlog, vscale, vlogscale, vhead, vtail)
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
      trn_world_to_X11(Vptr->x,Vptr->y,&x1,&y1);
 
      /* Scale to the dimensions of the plot specified in the Vbox */
      if (vlog) {
         vx   = CNveclog10(Vptr->vx/vmin) * vlogscale;
         vy   = CNveclog10(Vptr->vy/vmin) * vlogscale;
      } else {
         vx   = Vptr->vx * vscale;
         vy   = Vptr->vy * vscale;
      }

      /* Find the vector ending point in device coordinates */
      trn_world_to_X11(Vptr->x+vx,Vptr->y+vy,&x2,&y2);
      /*
      (void) printf("coords=(%g %g) vx=%g  vy=%g\n",Vptr->x, Vptr->y,vx,vy);
      (void) printf("start = (%g %g)  end=(%g %g)\n",x1,y1,x2,y2);
       */
 
      /* Draw the arrow */
      plotX2D_arrow(x1,y1,x2,y2,
                    p1_clipped, p2_clipped,
                    (char *)NULL,
                    (int)Vbox->linetype,
                    (int)Vbox->linecolor,
                    (int)Vbox->linewidth,
                    (int)( vtail ? Vbox->marktype : CN_MK_NONE ),
                    1,
                    (int)Vbox->markcolor, vhead, 0);
   }
}
 

/* 
 * Plot the boundary in X11 
 */
static void plotX2D_boundary(Dptr, boundary, reg_boundary,
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
   int         text_xpos, text_ypos, text_len, text_width;
   int         cnt=0;

   if (Dptr->regionhead == NULL) return;

   if (boundary==CN_FALSE && reg_boundary==CN_FALSE && fillbnd==CN_FALSE) 
      return;

   /* The linetype is solid */
   XSetLineAttributes(display,gc,1,LineSolid,CapButt,JoinBevel);
   PXsetColorX(0);

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

   /* now print out the boundary segments in each region */
   for (R=Dptr->regionhead; R!=NULL; R=R->next) {

      /* Draw the material boundary only (do this only if "boundary" is set */
      for (P=R->matpolyhead; P!=NULL && boundary; P=P->next) {
         /* Clip the polygon */
         CNclip_poly(P, &node_head, &node_tail,
               cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax,
               1, 1, 0, 1, 0);

         /* Plot the nodes */
         plotX2D_nodes(node_head, node_tail,
                       CN_FILL_NONE, 
                       PXpolyColorIndexX(R->color), 
                       boundary ? CN_LN_SOLID : CN_LN_NONE, 0, 1);

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
         plotX2D_nodes(node_head, node_tail,
                       fill ? CN_FILL_SOLID : CN_FILL_NONE, 
                       PXpolyColorIndexX(R->color), 
                       reg_boundary ? CN_LN_SOLID : CN_LN_NONE, 0, 1);

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
            trn_world_to_X11(midx,midy,&x,&y);

            /* Check to see if this midpoint is actually inside the region */
            if (CNpoint_in_region(Dptr,midx,midy,(int)R->ID,0)) { 
               /* Print the region ID */
               (void) sprintf(label,"Region #%d",R->ID);
               text_len   = strlen(label);
               text_width = XTextWidth(lblfont_info,label,text_len);
               text_xpos  = x - 0.5*text_width;
               text_ypos  = y + 0.5*lblfont_info->max_bounds.ascent;
               if (window)
               XDrawString(display,window,gcl,text_xpos,text_ypos,
                           label,text_len);
               if (pixmap)
               XDrawString(display,pixmap,gcl,text_xpos,text_ypos,
                           label,text_len);

               /* Print the material type */
               (void) sprintf(label,"Mat \"%s\"",R->matname);
               text_len   = strlen(label);
               text_width = XTextWidth(lblfont_info,label,text_len);
               text_xpos  = x - 0.5*text_width;
               text_ypos  = y - 0.5*lblfont_info->max_bounds.ascent;
               if (window)
               XDrawString(display,window,gcl,text_xpos,text_ypos,
                           label,text_len);
               if (pixmap)
               XDrawString(display,pixmap,gcl,text_xpos,text_ypos,
                           label,text_len);
            }
         }

         /* Delete the node-list */
         CNremove_node_list(&node_head, &node_tail);

      }
   }
}


/*
 * Plot the triangular mesh in X11
 */
static void plotX2D_trias(Dptr,contfill,meshplot)
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
   fillcolor = PXpolyColorIndexX(1);   /* yellow */
   linecolor = PXpolyColorIndexX(4);   /* red    */

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

   /* Initialize the polygon buffer */
   init_polygon_buffer();

   /* Solid Fill */
   XSetFillStyle(display, gc, FillSolid);

   /* Loop through the triangles */
   for (T=Dptr->triahead; T!=NULL && contfill; T=T->next) {

      /* Quick check to see if the triangle is in-bounds */
      if (!CNtria_in_bounds(T,
           cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax)) continue;

      /* Draw a fill triangle with gradated colors */
      plotX2D_single_fill_tria(T,
                               cxmin, cxmax, cymin, cymax,
                               czmin, czmax, ctmin, ctmax,
                               Dptr->data_pr.logx,
                               Dptr->data_pr.logy,
                               0,
                               Dptr->data_pr.logz,
                               Dptr->data_pr.contclip);
   }

   /* Flush the polygon buffer */
   flush_polygon_buffer();

   /* Reset color */
   PXsetColorX(0);

   /* Loop through the triangles */
   for (T=Dptr->triahead; T!=NULL && meshplot; T=T->next) {

      /* Quick check to see if the triangle is in-bounds */
      if (!CNtria_in_bounds(T,
           cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax)) continue;

      /* Draw a mesh triangle */
      plotX2D_single_solid_tria(T,
                               cxmin, cxmax, cymin, cymax,
                               czmin, czmax, ctmin, ctmax,
                               Dptr->data_pr.logx,
                               Dptr->data_pr.logy,
                               0,
                               Dptr->data_pr.logz,
                               filltype, fillcolor, linetype, linecolor);
   }

   /* Reset color */
   PXsetColorX(0);
}


/*
 * Plot the rectangular mesh in X11
 */
static void plotX2D_rects(Dptr,contfill,meshplot)
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
      status_ok = plotX2D_bitmap_rects(Dptr, contfill);
      if (status_ok) return;
   }

   if (Dptr->recthead == NULL) return;

   /* Initialize colors and line/fill types */
   filltype  = CN_FILL_NONE;
   linetype  = CN_LN_SOLID;
   fillcolor = PXpolyColorIndexX(1);   /* yellow */
   linecolor = PXpolyColorIndexX(4);   /* red    */

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

   /* Initialize the polygon buffer */
   init_polygon_buffer();

   /* Solid Fill */
   XSetFillStyle(display, gc, FillSolid);

   /* Loop through the rectangles */
   for (R=Dptr->recthead; R!=NULL && contfill; R=R->next) {

      /* Quick check to see if the rectangle is in-bounds */
      if (!CNrect_in_bounds(R,
           cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax)) continue;

      /* Draw a fill rectangle with gradated colors */
      plotX2D_single_fill_rect(R,
                               cxmin, cxmax, cymin, cymax,
                               czmin, czmax, ctmin, ctmax,
                               Dptr->data_pr.logx,
                               Dptr->data_pr.logy,
                               0,
                               Dptr->data_pr.logz,
                               Dptr->data_pr.contclip);
   }

   /* Flush the polygon buffer */
   flush_polygon_buffer();

   /* Reset color */
   PXsetColorX(0);

   /* Loop through the rectangles */
   for (R=Dptr->recthead; R!=NULL && meshplot; R=R->next) {

      /* Quick check to see if the rectangle is in-bounds */
      if (!CNrect_in_bounds(R,
           cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax)) continue;

      /* Draw a mesh rectangle */
      plotX2D_single_solid_rect(R,
                               cxmin, cxmax, cymin, cymax,
                               czmin, czmax, ctmin, ctmax, 
                               Dptr->data_pr.logx,
                               Dptr->data_pr.logy,
                               0,
                               Dptr->data_pr.logz,
                               filltype, fillcolor, linetype, linecolor);
   }

   /* Reset color */
   PXsetColorX(0);
}


/*
 * Plot the polygonal mesh in X11
 */
static void plotX2D_polys(Dptr,contfill,meshplot)
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
      fillcolor = PXpolyColorIndexX(color);
      linetype  = (meshplot) ? CN_LN_SOLID : CN_LN_NONE;
      linecolor = PXpolyColorIndexX(4);   /* red    */
 
      /* Draw a fill rectangle with gradated colors */
      if (contfill && !nocont) {
         plotX2D_single_fill_poly(P,
                                  cxmin, cxmax, cymin, cymax,
                                  czmin, czmax, ctmin, ctmax,
                                  Dptr->data_pr.contclip);
         filltype = CN_FILL_NONE;
      }
 
      /* Draw a mesh rectangle */
      plotX2D_single_solid_poly(P,
                                cxmin, cxmax, cymin, cymax,
                                czmin, czmax, ctmin, ctmax,
                                filltype, fillcolor, linetype, linecolor);
   }
 
   /* Reset color */
   PXsetColorX(0);
}
 
/*
 * Plot a single solid triangle
 */
static void plotX2D_single_solid_tria(T,
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
   plotX2D_nodes(node_head, node_tail,
                 filltype, fillcolor, linetype, linecolor, 1);

   /* Delete the node-list */
   CNremove_node_list(&node_head, &node_tail);
}


/*
 * Plot a single mesh rectangle
 */
static void plotX2D_single_solid_rect(R,
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
   plotX2D_nodes(node_head, node_tail,
                 filltype, fillcolor, linetype, linecolor, 1);

   /* Delete the node-list */
   CNremove_node_list(&node_head, &node_tail);
}


/*
 * Plot a single solid polygon
 */
static void plotX2D_single_solid_poly(P,
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
   plotX2D_nodes(node_head, node_tail,
                 filltype, fillcolor, linetype, linecolor, 1);
 
   /* Delete the node-list */
   CNremove_node_list(&node_head, &node_tail);
}


/*
 * Draw fill colors in a triangle
 */
/*ARGSUSED*/
static void plotX2D_single_fill_tria(T,
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

   /* Loop through the contours - min of 2 steps */
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

         /* Save the polygon in the buffer */
         if (node_head)
         buffer_polygon(node_head, node_tail,
                        PXfillColorIndex(colr) - PX_MAX_NAMED_COLORS);

         /* Delete the node-list */
         CNremove_node_list(&node_head, &node_tail);
      }
   }
}


/*
 * Draw fill colors in a rectangle
 * Solid fill-type has been set outside of this routine
 */
/*ARGSUSED*/
static void plotX2D_single_fill_rect(R,
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

   /* Loop through the contours - there will be a minimum of 2 steps */
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

         /* Save the polygon in the buffer */
         if (node_head) 
         buffer_polygon(node_head, node_tail, 
                        PXfillColorIndex(colr) - PX_MAX_NAMED_COLORS);

         /* Delete the node-list */
         CNremove_node_list(&node_head, &node_tail);
      }
   }
   /* Reset */
   PXsetColorX(0);
}


/*
 * Draw fill colors in a polygon
 */
/*ARGSUSED*/
static void plotX2D_single_fill_poly(P,
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
         plotX2D_nodes(node_head, node_tail,
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
static void plotX2D_nodes(node_head, node_tail,
                          filltype, fillcolor, linestyle, linecolor, linewidth)
CNnodeptr node_head, node_tail;
int       filltype, fillcolor, linestyle, linecolor, linewidth;
{
   CNnodeptr   N;
   XPoint      points[MAX_ARR_SIZE];
   double      x, y;
   int         count;

   /* return now if there is nothing to plot */
   if (node_head == NULL) return;

   /* if filltype and linestyle are both NONE (0), return now */
   if ((filltype==CN_FILL_NONE) && (linestyle==CN_LN_NONE)) return;

   /* rescale points, save in array */
   count=0;
   for (N=node_head; N!=NULL && count<MAX_ARR_SIZE; N=N->next) {
      trn_world_to_X11(N->coord->x,N->coord->y,&x,&y);
      points[count].x = (int)x;
      points[count].y = (int)y;
      count++;
   }

   /* Fill the polygon */
   PXfillX_polygon(points, count, 
                   filltype, fillcolor, linestyle, linecolor, linewidth);
}


/*
 * Draw a set of curves in a dataset
 */
static void plotX2D_dataset_curves(dptr, colrinc, lineinc)
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
         plotX2D_curve(C, 
                       colrinc,lineinc,
                       contour,contlbl,&lbloffset,
                       hdnline,applyfill,pr_ptID, pr_cvID);
      } else {
         /* Plot the curve using spline-approximated data-points */
         plotX2D_spline_curve(C,
                              spline,colrinc,lineinc,
                              contour,contlbl,&lbloffset,
                              hdnline,applyfill,pr_ptID, pr_cvID);
      }
   }
}


/* 
 * Plot the curve in X11 
 */
static void plotX2D_curve(C, 
                          colrinc, lineinc, 
                          contour, contlbl, lbloffset,
                          hiddenline, applyfill, pr_ptID, pr_cvID)
CNcurveptr C;
int        colrinc, lineinc;
int        contour, contlbl, *lbloffset;
int        hiddenline, applyfill, pr_ptID, pr_cvID;
{
   CNpointptr  pt_head=NULL, pt_tail=NULL, P;
   XPoint      points[MAX_ARR_SIZE];
   double      x,y;
   int         count, maxpts, linepat; 
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
      trn_world_to_X11_nolog(P->x,P->y,&x,&y);
      P->x = x;
      P->y = y;
      P->z = 0.0;
   }

   /* Plot boundaries */
   cxmin =   (double) Xxmin;
   cxmax =   (double) Xxmax;
   cymin =   (double) Xymin;
   cymax =   (double) Xymax;
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
      fillX2D_curve(pt_head,pt_tail,filltype,fillcolor);

      /* Set the linecolor now */
      PXlineColorX(linecolor);
 
      /* Reset the outline color if necessary */
      if (hiddenline) PXlineColorX(4);

   } else {
 
      /* Set the linecolor now */
      PXlineColorX(linecolor);
 
   } 


   /* 
    * The X server has a practical limit on the number of points that
    * can be plotted with one call.  Find this limit and workaround if
    * the limit is less than the no of elements in the permanent array.
    */
   maxpts  = XMaxRequestSize(display) - 3;
   if (maxpts > MAX_ARR_SIZE) maxpts = MAX_ARR_SIZE;

   /* Set the linetype and width */
   linepat = PXlinetypX(linetype,C->curv_pr.linewidth);

   /* First point */
   P = pt_head;

   /* go thru loop until we run out of points */
   while (P != NULL && linepat) {

      /* set count to 0 */
      count = 0;

      /* save points in array */
      for ( ; P!=NULL && count<maxpts; P=P->next) {
         points[count].x = (int)(P->x);
         points[count].y = (int)(P->y);
         count++;
      }

      if (count == 1) {
         /*
          * Some machines (IBM6000) don't like having an array with
          * only one point
          */
         if (window)
         XDrawPoint(display,window,gc,points[0].x,points[0].y);
         if (pixmap)
         XDrawPoint(display,pixmap,gc,points[0].x,points[0].y);
      } else if (count > 1) {
         /* plot the points */
         if (window)
         XDrawLines(display,window,gc,points,count,CoordModeOrigin);
         if (pixmap)
         XDrawLines(display,pixmap,gc,points,count,CoordModeOrigin);
      }

      /* if there are more points to come, repeat the last point */
      if (P != NULL) P = P->prev;
   }

   /* Reset the linetype */
   (void) PXlinetypX(CN_LN_SOLID,1);

   /* Draw the markers - send the original list */
   plotX2D_markers(C->pointhead, C->pointtail, 
                   marktype, marksize, markcolor, pr_ptID, contour);

   /* Put on the contour labels if necessary */
   if (contour && contlbl)  
      plotX2D_contlabel(pt_head, pt_tail, 
                        C->curv_pr.linetype, C->curv_pr.linelabel, 
                        (*lbloffset)++);

   /* Draw curve ID */
   if (pr_cvID && !contour)
      plotX2D_curveID(pt_head, pt_tail, (int)C->ID, filltype);

   /* Delete the point-list */
   CNdelete_point_list(&pt_head, &pt_tail);

   /* Reset color */
   PXsetColorX(0);
}


/* 
 * Fill the curve in X11 
 * The pointlist has already been scaled and clipped against
 * the domain and plot boundaries
 */
/*ARGSUSED*/
static void fillX2D_curve(pt_head, pt_tail, filltype, fillcolor)
CNpointptr pt_head, pt_tail;
int        filltype, fillcolor;
{
   CNpointptr  P;
   XPoint      points[MAX_ARR_SIZE];
   int         count;

   if (pt_head == NULL) return;

   /* If the curve is not to be filled then get out now */
   if (filltype == CN_FILL_NONE) return;

   /* rescale points, save in array */
   count = 0;
   for (P=pt_head; P!=NULL && count<MAX_ARR_SIZE; P=P->next) {
      points[count].x = (int)(P->x);
      points[count].y = (int)(P->y);
      count++;
   }

   /* Fill the polygon */
   PXfillX_polygon(points,count,
                   filltype, PXpolyColorIndexX(fillcolor),CN_LN_NONE,0,1);
}
   

/* 
 * Plot the curve in X11 
 */
static void plotX2D_spline_curve(C,
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
   XPoint      points[MAX_ARR_SIZE];
   double      *xarr, *yarr, *zarr, *xs, *ys, *zs;
   double      dist;
   double      x,y;
   int         npts, nspts, ndiv = 20, closed = 0;
   int         count, i, maxpts, linepat;
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

   /* rescale points */
   for (P=pt_head; P!=NULL; P=P->next) {
      trn_world_to_X11_nolog(P->x,P->y,&x,&y);
      P->x = x;
      P->y = y;
      P->z = 0.0;
   }

   /* Domain boundaries */
   cxmin =   (double) Xxmin;
   cxmax =   (double) Xxmax;
   cymin =   (double) Xymin;
   cymax =   (double) Xymax;
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
      fillX2D_curve(pt_head,pt_tail,filltype,fillcolor);

      /* Set the linecolor now */
      PXlineColorX(linecolor);
 
      /* Reset the outline color if necessary */
      if (hiddenline) PXlineColorX(4);

   } else {
 
      /* Set the linecolor now */
      PXlineColorX(linecolor);
 
   }


   /*
    * The X server has a practical limit on the number of points that
    * can be plotted with one call.  Find this limit and workaround if
    * the limit is less than the no of elements in the permanent array.
    */
   maxpts  = XMaxRequestSize(display) - 3;
   if (maxpts > MAX_ARR_SIZE) maxpts = MAX_ARR_SIZE;

   /* Set the linepattern and linewidth */
   linepat = PXlinetypX(linetype,C->curv_pr.linewidth);

   /* 
    * Can only plot some 1000-or-so points at a time, so
    * go thru loop until we run out of points.
    * P      = running count of points
    * count  = points plotted during each major loop iteration
    */

   /* First point */
   P = pt_head;

   /* go thru loop until we run out of points */
   while ((P!=NULL) && linepat) {

      /* set count to 0 */
      count = 0;
 
      /* save points in array */
      for ( ; P!=NULL && count<maxpts; P=P->next) {
         points[count].x = (int)(P->x);
         points[count].y = (int)(P->y);
         count++;
      }

      if (count == 1) {
         /*
          * Some machines (IBM6000) don't like having an array with
          * only one point
          */
         if (window)
         XDrawPoint(display,window,gc,points[0].x,points[0].y);
         if (pixmap) 
         XDrawPoint(display,pixmap,gc,points[0].x,points[0].y);
      } else if (count > 1) {
         /* plot the points */
         if (window)
         XDrawLines(display,window,gc,points,count,CoordModeOrigin);
         if (pixmap) 
         XDrawLines(display,pixmap,gc,points,count,CoordModeOrigin);
      }

      /* if there are more points to come, repeat the last point */
      if (P != NULL) P=P->prev;
   }

   /* Reset the linetype */
   (void) PXlinetypX(CN_LN_SOLID,1);

   /* Draw the markers */
   plotX2D_markers(C->pointhead, C->pointtail, 
                   marktype, marksize, markcolor, pr_ptID, contour);

   /* Put on the contour labels if necessary */
   if (contour && contlbl) 
      plotX2D_contlabel(pt_head, pt_tail,
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
      plotX2D_curveID(pt_head, pt_tail, (int)C->ID, filltype);

   /* Delete the point-list */
   CNdelete_point_list(&pt_head, &pt_tail);

   /* Reset color */
   PXsetColorX(0);
}


/* 
 * Plot the curve markers in X11 
 */
/*ARGSUSED*/
static void plotX2D_markers(pointhead,pointtail,
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

   /* Set the linetype, width and marker color */
   (void) PXlinetypX(CN_LN_SOLID,1);
   PXlineColorX(markcolor);

   /* Plot boundaries */
   pxmin = (double) Xxmin;
   pxmax = (double) Xxmax;
   pymin = (double) Xymin;
   pymax = (double) Xymax;

   for (P=pt_head; P!=NULL && marktype!=CN_MK_NONE; P=P->next) {

      /* rescale points */
      trn_world_to_X11_nolog(P->x,P->y,&x,&y);

      /* plot the points only when the point is inside the window */
      if (x < pxmin || x > pxmax) continue;
      if (y < pymin || y > pymax) continue;

      /* plot the marker */
      PXmarkerX(marktype,marksize,(int)x,(int)y);
   }

   /* Draw curve-point ID's */
   if (pr_ptID && !contour)
      plotX2D_pointIDs(pt_head, pt_tail, CN_TRUE);
 
   /* Delete the point-list */
   CNdelete_point_list(&pt_head, &pt_tail);

   /* Reset */
   PXsetColorX(0); 
}


/*
 * LABELS
 */

/*
 * Plot the curve ID label in X11
 * The pointlist is in plot coordinates
 */
/*ARGSUSED*/
static void plotX2D_curveID(pointhead, pointtail, curveID, filltype)
CNpointptr pointhead, pointtail;
int        curveID, filltype;
{
   double     dist=0.0;
   double     x, y;
   char       label[CN_MAXCHAR];
   int        text_xpos, text_ypos, text_len, text_width;
   double     rxmin, rxmax, rymin, rymax;
   int        npts;
   CNpointptr P;

   if (pointhead == NULL) return;

   /* Set the text color */
   XSetForeground(display,gcl,colors[0]);

   /* Boundary */
   rxmin = Xxmin - 10;
   rxmax = Xxmax + 10;
   rymin = Xymin - 10;
   rymax = Xymax + 10;
 
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

      /* Use the background color to make this text visible */
      XSetForeground(display,gcl,colors[1]);

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
      text_len   = strlen(label);
      text_width = XTextWidth(lblfont_info,label,text_len);
      text_xpos  = x - 0.5*text_width;
      text_ypos  = y + 0.5*lblfont_info->max_bounds.ascent;
      if (window)
      XDrawString(display,window,gcl,text_xpos,text_ypos,label,text_len);
      if (pixmap)
      XDrawString(display,pixmap,gcl,text_xpos,text_ypos,label,text_len);
   }

   /* Reset the text color */
   XSetForeground(display,gcl,colors[0]);
}


/*
 * Plot the point ID labels in X11
 */
/*ARGSUSED*/
static void plotX2D_pointIDs(pt_head,pt_tail,nolog)
CNpointptr pt_head, pt_tail;
int        nolog;
{
   CNpointptr  P;
   double      x,y;
   char        label[CN_MAXCHAR];
   int         text_xpos, text_ypos, text_len;
   double      rxmin, rxmax, rymin, rymax;
 
   if (pt_head == NULL) return;
 
   /* Boundary */
   rxmin = Xxmin - 10;
   rxmax = Xxmax + 10;
   rymin = Xymin - 10;
   rymax = Xymax + 10;
   
   /* Set the linetype, width and color */
   (void) PXlinetypX(CN_LN_SOLID,1);
   PXlineColorX(0);

   /* Go thru each point */
   for (P=pt_head; P!=NULL; P=P->next) {
      if (nolog) 
      trn_world_to_X11_nolog(P->x,P->y,&x,&y);
      else
      trn_world_to_X11(P->x,P->y,&x,&y);

      /* Print the label only if the point is in bounds */
      if (x > rxmin && x < rxmax && y > rymin && y < rymax) {

         (void) sprintf(label,"P%d",P->ID);
         text_len   = strlen(label);
         text_xpos  = x;
         text_ypos  = y;
         if (window)
         XDrawString(display,window,gcl,text_xpos,text_ypos,label,text_len);
         if (pixmap)
         XDrawString(display,pixmap,gcl,text_xpos,text_ypos,label,text_len);
      }
   }
}

/*
 * Plot the node ID labels in X11
 */
/*ARGSUSED*/
static void plotX2D_nodeIDs(nd_head,nd_tail)
CNnodeptr nd_head, nd_tail;
{
   CNnodeptr   N;
   double      x,y;
   char        label[CN_MAXCHAR];
   int         text_xpos, text_ypos, text_len, text_width;
   double      rxmin, rxmax, rymin, rymax;
 
   if (nd_head == NULL) return;
 
   /* Boundary */
   rxmin = Xxmin - 10;
   rxmax = Xxmax + 10;
   rymin = Xymin - 10;
   rymax = Xymax + 10;
   
   /* Set the linetype, width and color */
   (void) PXlinetypX(CN_LN_SOLID,1);
   PXlineColorX(0);

   /*
    * Multiple nodes could share a single point so be smart about this
    */
   /* Reset all the point-flags */
   for (N=nd_head; N!=NULL; N=N->next) N->coord->flag = 0;

   /* Go thru each node */
   for (N=nd_head; N!=NULL; N=N->next) {
      trn_world_to_X11(N->coord->x,N->coord->y,&x,&y);

      /* Print the label only if the node is in bounds */
      if (x > rxmin && x < rxmax && y > rymin && y < rymax) {

         (void) sprintf(label,"N%d",N->ID);
         text_len   = strlen(label);
         text_width = XTextWidth(lblfont_info,label,text_len);
         text_xpos  = x - text_width - 1;
         text_ypos  = y + N->coord->flag*lblfont_height;
         if (window)
         XDrawString(display,window,gcl,text_xpos,text_ypos,label,text_len);
         if (pixmap)
         XDrawString(display,pixmap,gcl,text_xpos,text_ypos,label,text_len);

         /* Increment the point-flag */
         (N->coord->flag)++;
      }
   }

   /* Reset all the point-flags */
   for (N=nd_head; N!=NULL; N=N->next) N->coord->flag = 0;
}

/*
 * Plot the triangle ID labels in X11
 */
/*ARGSUSED*/
static void plotX2D_triaIDs(tr_head,tr_tail)
CNtriaptr tr_head, tr_tail;
{
   CNtriaptr   T;
   double      x,y;
   char        label[CN_MAXCHAR];
   int         text_xpos, text_ypos, text_len, text_width;
   double      rxmin, rxmax, rymin, rymax;
   double      midx, midy;
 
   if (tr_head == NULL) return;
 
   /* Boundary */
   rxmin = Xxmin - 10;
   rxmax = Xxmax + 10;
   rymin = Xymin - 10;
   rymax = Xymax + 10;
   
   /* Set the linetype, width and color */
   (void) PXlinetypX(CN_LN_SOLID,1);
   PXlineColorX(0);

   /* Go thru each triangle */
   for (T=tr_head; T!=NULL; T=T->next) {
      /* Get the x-y value of the triangle-midpoint */
      midx = T->n1->coord->x + T->n2->coord->x + T->n3->coord->x;
      midy = T->n1->coord->y + T->n2->coord->y + T->n3->coord->y;
      midx = midx/3.0;
      midy = midy/3.0;
      trn_world_to_X11(midx,midy,&x,&y);

      /* Print the label only if the point is in bounds */
      if (x > rxmin && x < rxmax && y > rymin && y < rymax) {

         (void) sprintf(label,"T%d",T->ID);
         text_len   = strlen(label);
         text_width = XTextWidth(lblfont_info,label,text_len);
         text_xpos  = x - 0.5*text_width;
         text_ypos  = y + 0.5*lblfont_info->max_bounds.ascent;
         if (window)
         XDrawString(display,window,gcl,text_xpos,text_ypos,label,text_len);
         if (pixmap)
         XDrawString(display,pixmap,gcl,text_xpos,text_ypos,label,text_len);
      }
   }
}

/*
 * Plot the rectangle ID labels in X11
 */
/*ARGSUSED*/
static void plotX2D_rectIDs(rt_head,rt_tail)
CNrectptr rt_head, rt_tail;
{
   CNrectptr   R;
   double      x,y;
   char        label[CN_MAXCHAR];
   int         text_xpos, text_ypos, text_len, text_width;
   double      rxmin, rxmax, rymin, rymax;
   double      midx, midy;
 
   if (rt_head == NULL) return;
 
   /* Boundary */
   rxmin = Xxmin - 10;
   rxmax = Xxmax + 10;
   rymin = Xymin - 10;
   rymax = Xymax + 10;
   
   /* Set the linetype, width and color */
   (void) PXlinetypX(CN_LN_SOLID,1);
   PXlineColorX(0);

   /* Go thru each rectangle */
   for (R=rt_head; R!=NULL; R=R->next) {
      /* Get the x-y value of the rectangle-midpoint */
      midx = R->n1->coord->x + R->n2->coord->x + 
             R->n3->coord->x + R->n4->coord->x;
      midy = R->n1->coord->y + R->n2->coord->y + 
             R->n3->coord->y + R->n4->coord->y;
      midx = midx/4.0;
      midy = midy/4.0;
      trn_world_to_X11(midx,midy,&x,&y);

      /* Print the label only if the point is in bounds */
      if (x > rxmin && x < rxmax && y > rymin && y < rymax) {

         (void) sprintf(label,"R%d",R->ID);
         text_len   = strlen(label);
         text_width = XTextWidth(lblfont_info,label,text_len);
         text_xpos  = x - 0.5*text_width;
         text_ypos  = y + 0.5*lblfont_info->max_bounds.ascent;
         if (window)
         XDrawString(display,window,gcl,text_xpos,text_ypos,label,text_len);
         if (pixmap)
         XDrawString(display,pixmap,gcl,text_xpos,text_ypos,label,text_len);
      }
   }
}


/*
 * ANNOTATIONS
 */

/*
 * Plot annotations
 */
static void annotateX2D()
{
   CNannotptr  AP;
   CNdslistptr DS;
   double annotScale=1.0;

   /* Plot the annotations in each dataset */
   for (DS=plotdata->datahead; DS!=NULL; DS=DS->next) {
      if (DS->Dptr->data_pr.plotannot) {
         /* Figure out the annotation scale */
         annotScale = (DS->Dptr->bxmax - DS->Dptr->bxmin)/(xmax - xmin);
         if (annotScale < 1.0e-10) annotScale = 0.01;

         /* Plot each annotation */
         for (AP=DS->Dptr->annothead; AP!=NULL; AP=AP->next)
            plotX2D_single_annotation(AP, annotScale);
      }
   }

   /* Plot the annotations in the plotset */
   annotScale = 1.0;
   for (AP=plotdata->annothead; AP!=NULL; AP=AP->next)
      plotX2D_single_annotation(AP, annotScale);
}

/*
 * Plot a single annotation
 */
static void plotX2D_single_annotation(AP, annotScale)
CNannotptr AP;
double annotScale;
{
   if (AP == NULL) return;

   switch (AP->type) {
   case CN_AN_RECT : /* Draw a rectangle */
                     plotX2D_annot_rect(AP, annotScale); 
                     break;
   case CN_AN_LINE : /* Draw a line      */
                     plotX2D_annot_line(AP, annotScale); 
                     break;
   case CN_AN_ARROW: /* Draw a arrow     */
                     plotX2D_annot_arrow(AP, annotScale); 
                     break;
   case CN_AN_POINT: /* Draw a point     */
                     plotX2D_annot_point(AP, annotScale); 
                     break;
   case CN_AN_TEXT : /* Draw a text label */
                     plotX2D_annot_text(AP, annotScale); 
                     break;
   default         : break;
   }
}


/* 
 * Plot an annotation rectangle 
 */
static void plotX2D_annot_rect(AP, annotScale)
CNannotptr AP;
double annotScale;
{
   XPoint points[MAX_ARR_SIZE];
   double x,y,x1,y1,x2,y2;
   int    i=0;
   double rxmin, rxmax, rymin, rymax;
   double bxmin, bxmax, bymin, bymax;
   int    fontsize;

   /* 
    * The annotated object can be specified either
    *  (a) position relative to data coordinates
    *  (b) absolute position in percentage of width/height
    */

   /* Clipping boundary */
   rxmin = (double) 0;
   rxmax = (double) Width;
   rymin = (double) 0;
   rymax = (double) Height;

   if (AP->property.absolute) {
      /* Rescale the points - the values are in percentages */
      x1 = AP->pt1.x * Width;
      y1 = Height - (AP->pt1.y * Height);
      x2 = AP->pt2.x * Width;
      y2 = Height - (AP->pt2.y * Height);

   } else {

      /* Clip against the real-world boundary */
      if (AP->property.doclip) {
         rxmin = Xxmin;
         rxmax = Xxmax;
         rymin = Xymin;
         rymax = Xymax;
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
      trn_world_to_X11_nolog(x,y,&x1,&y1);
      x=x2;  y=y2;
      trn_world_to_X11_nolog(x,y,&x2,&y2);
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
   PXfillX_polygon(points, i,
                   (int) AP->property.filltype, 
                   PXpolyColorIndexX( (int) AP->property.fillcolor ), 
                   (int) AP->property.linetype, 
                   (int) AP->property.linecolor, 
                   (int) AP->property.linewidth);

   /* If there is text attached, draw it */
   if (AP->property.linelabel) {
      /* Set the text color */
      if ((AP->property.filltype!=CN_FILL_NONE) && (AP->property.fillcolor==0))
         XSetForeground(display,gcl,colors[1]);
      else
         XSetForeground(display,gcl,colors[0]);

      /* Set the font */
      fontsize = AP->property.fontsize;
      if (AP->property.doscale) {
          fontsize = fontsize * annotScale;
          if (fontsize == 0) fontsize = 1;
      }

      /* Draw the label centered, in the middle of the box */
      PXplotX_scalable_font(0.5*(bxmin+bxmax), 0.5*(bymin+bymax), 
                            AP->property.linelabel, fontsize,
                            CN_FALSE, CN_FALSE,
                            CN_FALSE, CN_FALSE);

      /* Reset the text color */
      XSetForeground(display,gcl,colors[0]);
   }
}

/* 
 * Plot an annotation line  
 */
static void plotX2D_annot_line(AP, annotScale)
CNannotptr AP;
double annotScale;
{
   double rxmin,rxmax,rymin,rymax;
   double cxmin,cxmax,cymin,cymax;
   double x1,y1,x2,y2,x,y; 
   int    pat;
   int    p1_clipped=CN_FALSE, p2_clipped=CN_FALSE;
   int    fontsize;
   int    leftJustify, rightJustify, topJustify, bottomJustify;

   /*
    * The annotated object can be specified either
    *  (a) position relative to data coordinates
    *  (b) absolute position in percentage of width/height
    */

   /* Clipping boundary */
   rxmin = (double) 0;
   rxmax = (double) Width;
   rymin = (double) 0;
   rymax = (double) Height;

   if (AP->property.absolute) {

      /* Rescale the points - the values are in percentages */
      x1 = AP->pt1.x * Width;
      y1 = Height - (AP->pt1.y * Height);
      x2 = AP->pt2.x * Width;
      y2 = Height - (AP->pt2.y * Height);

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
      trn_world_to_X11_nolog(x,y,&x1,&y1);
      x=x2;  y=y2;
      trn_world_to_X11_nolog(x,y,&x2,&y2);

      /* Do pre-clipping aaginst the inner plot boundaries */
      if (AP->property.doclip) {
         /* Clipping boundary */
         cxmin = (double) Xxmin-1;
         cxmax = (double) Xxmax+1;
         cymin = (double) Xymin-1;
         cymax = (double) Xymax+1;

         if ((x1 <=cxmin && x2 <=cxmin) || (x1 >=cxmax && x2 >=cxmax)) return;
         if ((y1 <=cymin && y2 <=cymin) || (y1 >=cymax && y2 >=cymax)) return;
         clipX2D_in_xmin(&x1,&y1,&x2,&y2,cxmin,&p1_clipped,&p2_clipped);
         clipX2D_in_ymin(&x1,&y1,&x2,&y2,cymin,&p1_clipped,&p2_clipped);
         clipX2D_in_xmax(&x1,&y1,&x2,&y2,cxmax,&p1_clipped,&p2_clipped);
         clipX2D_in_ymax(&x1,&y1,&x2,&y2,cymax,&p1_clipped,&p2_clipped);
         if ((x1 <=cxmin && x2 <=cxmin) || (x1 >=cxmax && x2 >=cxmax)) return;
         if ((y1 <=cymin && y2 <=cymin) || (y1 >=cymax && y2 >=cymax)) return;
      }
   }

   /* Check to see if the line is inside the plot area */
   if ((x1 < rxmin && x2 < rxmin) || (x1 > rxmax && x2 > rxmax)) return;
   if ((y1 < rymin && y2 < rymin) || (y1 > rymax && y2 > rymax)) return;

   /* Do another clipping */
   clipX2D_in_xmin(&x1,&y1,&x2,&y2,rxmin,&p1_clipped,&p2_clipped);
   clipX2D_in_ymin(&x1,&y1,&x2,&y2,rymin,&p1_clipped,&p2_clipped);
   clipX2D_in_xmax(&x1,&y1,&x2,&y2,rxmax,&p1_clipped,&p2_clipped);
   clipX2D_in_ymax(&x1,&y1,&x2,&y2,rymax,&p1_clipped,&p2_clipped);

   /* Set the line characteristics */
   pat = PXlinetypX((int)AP->property.linetype, (int)AP->property.linewidth);
   PXlineColorX((int)AP->property.linecolor);
   if (pat) {
      if (window)
      XDrawLine(display,window,gc, (int)x1,(int)y1,(int)x2,(int)y2);
      if (pixmap)
      XDrawLine(display,pixmap,gc, (int)x1,(int)y1,(int)x2,(int)y2);
   }

   /* Draw the markers */
   if ((AP->property.marktype != CN_MK_NONE) && (!p1_clipped || !p2_clipped)) {
      (void) PXlinetypX(CN_LN_SOLID,1);
      PXlineColorX(AP->property.markcolor);
      if (!p1_clipped) PXmarkerX((int)AP->property.marktype,
                                 (int)AP->property.marksize,(int)x1,(int)y1);
      if (!p2_clipped) PXmarkerX((int)AP->property.marktype,
                                 (int)AP->property.marksize,(int)x2,(int)y2);
   }

   /* If there is text attached, draw it */
   if (!p1_clipped && AP->property.linelabel) {
      
      /* Set the font */
      fontsize = AP->property.fontsize;
      if (AP->property.doscale) {
          fontsize = fontsize * annotScale;
          if (fontsize == 0) fontsize = 1;
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
      PXplotX_scalable_font(x1, y1,
                            AP->property.linelabel, fontsize,
                            leftJustify, rightJustify,
                            topJustify, bottomJustify);
   }

   /* Reset */
   PXlineColorX(0);
}

/* 
 * Plot an annotation arrow 
 */
static void plotX2D_annot_arrow(AP, annotScale)
CNannotptr AP;
double     annotScale;
{
   double cxmin,cxmax,cymin,cymax;
   double x1,y1,x2,y2,x,y;      
   int    p1_clipped=CN_FALSE, p2_clipped=CN_FALSE;
   int    fontsize;

   /*
    * The annotated object can be specified either
    *  (a) position relative to data coordinates
    *  (b) absolute position in percentage of width/height
    */
 
   if (AP->property.absolute) {
 
      /* Rescale the points - the values are in percentages */
      x1 = AP->pt1.x * Width;
      y1 = Height - (AP->pt1.y * Height);
      x2 = AP->pt2.x * Width;
      y2 = Height - (AP->pt2.y * Height);
 
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
      trn_world_to_X11_nolog(x,y,&x1,&y1);
      x=x2;  y=y2;
      trn_world_to_X11_nolog(x,y,&x2,&y2);

      /* Do pre-clipping aaginst the inner plot boundaries */
      if (AP->property.doclip) {
         /* Clipping boundary */
         cxmin = (double) Xxmin-1;
         cxmax = (double) Xxmax+1;
         cymin = (double) Xymin-1;
         cymax = (double) Xymax+1;

         if ((x1 <=cxmin && x2 <=cxmin) || (x1 >=cxmax && x2 >=cxmax)) return;
         if ((y1 <=cymin && y2 <=cymin) || (y1 >=cymax && y2 >=cymax)) return;
         clipX2D_in_xmin(&x1,&y1,&x2,&y2,cxmin,&p1_clipped,&p2_clipped);
         clipX2D_in_ymin(&x1,&y1,&x2,&y2,cymin,&p1_clipped,&p2_clipped);
         clipX2D_in_xmax(&x1,&y1,&x2,&y2,cxmax,&p1_clipped,&p2_clipped);
         clipX2D_in_ymax(&x1,&y1,&x2,&y2,cymax,&p1_clipped,&p2_clipped);
         if ((x1 <=cxmin && x2 <=cxmin) || (x1 >=cxmax && x2 >=cxmax)) return;
         if ((y1 <=cymin && y2 <=cymin) || (y1 >=cymax && y2 >=cymax)) return;
      }
   }

   /* Set the font */
   fontsize = AP->property.fontsize;
   if (AP->property.doscale) {
      fontsize = fontsize * annotScale;
      if (fontsize == 0) fontsize = 1;
   }

   /* Draw the arrow */
   plotX2D_arrow(x1,y1,x2,y2,
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
static void plotX2D_arrow(x1,y1,x2,y2,
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
int    fontsize;
{
   double rxmin, rxmax, rymin, rymax;
   int    pat;
   double vx, vy, vd;
   double xa, ya, xb, yb, xc, yc;
   int    leftJustify, rightJustify, topJustify, bottomJustify;
 
   /* Clipping boundary */
   rxmin = (double) 0;
   rxmax = (double) Width;
   rymin = (double) 0;
   rymax = (double) Height;

   /* Check to see if the line is inside the plot area */
   if ((x1 < rxmin && x2 < rxmin) || (x1 > rxmax && x2 > rxmax)) return;
   if ((y1 < rymin && y2 < rymin) || (y1 > rymax && y2 > rymax)) return;

   /* Clip the arrow against the plot boundaries */
   clipX2D_in_xmin(&x1,&y1,&x2,&y2,rxmin,&p1_clipped,&p2_clipped);
   clipX2D_in_ymin(&x1,&y1,&x2,&y2,rymin,&p1_clipped,&p2_clipped);
   clipX2D_in_xmax(&x1,&y1,&x2,&y2,rxmax,&p1_clipped,&p2_clipped);
   clipX2D_in_ymax(&x1,&y1,&x2,&y2,rymax,&p1_clipped,&p2_clipped);

   /* Draw the arrow origin */
   (void) PXlinetypX(CN_LN_SOLID,1);
   PXlineColorX(markcolor);
   if (!p1_clipped) PXmarkerX(marktype,marksize,(int)x1,(int)y1);

   /* Draw the arrow line */
   pat = PXlinetypX(linetype, linewidth);
   PXlineColorX(linecolor);
   if (pat) {
      if (window)
      XDrawLine(display,window,gc, (int)x1,(int)y1,(int)x2,(int)y2);
      if (pixmap)
      XDrawLine(display,pixmap,gc, (int)x1,(int)y1,(int)x2,(int)y2);
   }

   /* Draw the arrow head */
   if (!p2_clipped && arrowhead) {
      /* Draw the arrow head */
      (void) PXlinetypX(CN_LN_SOLID,linewidth);
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
         if (window) {
         XDrawLine(display,window,gc, (int)x2, (int)y2, (int)xb, (int)yb);
         XDrawLine(display,window,gc, (int)x2, (int)y2, (int)xc, (int)yc);
         }
         if (pixmap) {
         XDrawLine(display,pixmap,gc, (int)x2, (int)y2, (int)xb, (int)yb);
         XDrawLine(display,pixmap,gc, (int)x2, (int)y2, (int)xc, (int)yc);
         }
      }
   }

   /* If there is text attached, draw it */
   if (!p1_clipped && linelabel) {
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
      PXplotX_scalable_font(x1, y1,
                            linelabel, fontsize,
                            leftJustify, rightJustify,
                            topJustify, bottomJustify);
   }

   /* Reset */
   PXlineColorX(0);
   (void) PXlinetypX(CN_LN_SOLID,1);
}

/*
 * Plot an annotation point
 */
static void plotX2D_annot_point(AP, annotScale)
CNannotptr AP;
double annotScale;
{
   double  x,y,x1,y1;
   double  rxmin, rxmax, rymin, rymax;
   double  cxmin, cxmax, cymin, cymax;
   int     fontsize;

   /*
    * The annotated object can be specified either
    *  (a) position relative to data coordinates
    *  (b) absolute position in percentage of width/height
    */
 
   /* Clipping boundary */
   rxmin = (double) 0;
   rxmax = (double) Width;
   rymin = (double) 0;
   rymax = (double) Height;
 
   if (AP->property.absolute) {
 
      /* Rescale the points - the values are in percentages */
      x1 = AP->pt1.x * Width;
      y1 = Height - (AP->pt1.y * Height);

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
      trn_world_to_X11_nolog(x,y,&x1,&y1);

      /* Do pre-clipping against the inner plot boundaries */
      if (AP->property.doclip) {
         /* Clipping boundary */
         cxmin = (double) Xxmin;
         cxmax = (double) Xxmax;
         cymin = (double) Xymin;
         cymax = (double) Xymax;

         if ((x1 < cxmin || x1 > cxmax) || (y1 < cymin || y1 > cymax))
            return;
      }
   }
     
   /* Draw the markers */
   if ((rxmin < x1 && x1 < rxmax) && (rymin < y1 && y1 < rymax)) {
      (void) PXlinetypX(CN_LN_SOLID,1);
      PXlineColorX(AP->property.markcolor);
      PXmarkerX((int)AP->property.marktype,
                (int)AP->property.marksize,(int)x1,(int)y1);

      /* If there is text attached, draw it */
      if (AP->property.linelabel) {
         /* Set the font */
         fontsize = AP->property.fontsize;
         if (AP->property.doscale) {
             fontsize = fontsize * annotScale;
             if (fontsize == 0) fontsize = 1;
         }
 
         /* Left-justified, left of the point */
         PXplotX_scalable_font(x1, y1, AP->property.linelabel, fontsize,
                               CN_TRUE, CN_FALSE,
                               CN_FALSE, CN_FALSE);
      }
   }

   /* Reset */
   PXlineColorX(0);
}

/*
 * Plot an annotation label
 */
static void plotX2D_annot_text(AP, annotScale)
CNannotptr AP;
double annotScale;
{
   double  x,y,x1,y1;
   double  rxmin, rxmax, rymin, rymax;
   double  cxmin, cxmax, cymin, cymax;
   int     fontsize;

   /*
    * The annotated object can be specified either
    *  (a) position relative to data coordinates
    *  (b) absolute position in percentage of width/height
    */
 
   /* Clipping boundary */
   rxmin = (double) 0;
   rxmax = (double) Width;
   rymin = (double) 0;
   rymax = (double) Height;
 
   if (AP->property.absolute) {
 
      /* Rescale the points - the values are in percentages */
      x1 = AP->pt1.x * Width;
      y1 = Height - (AP->pt1.y * Height);

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
      trn_world_to_X11_nolog(x,y,&x1,&y1);

      /* Do a pre-clipping */
      if (AP->property.doclip) {
         /* Clipping boundary */
         cxmin = (double) Xxmin;
         cxmax = (double) Xxmax;
         cymin = (double) Xymin;
         cymax = (double) Xymax;

         if ((x1 < cxmin || x1 > cxmax) || (y1 < cymin || y1 > cymax))
            return;
      }
   }
     
   /* Draw the text */
   if ((rxmin < x1 && x1 < rxmax) && (rymin < y1 && y1 < rymax)) {
      if (AP->property.linelabel) {
         /* Set the linetype and colors */
         (void) PXlinetypX(CN_LN_SOLID,1);
         PXlineColorX(0);

         /* Set the font */
         fontsize = AP->property.fontsize;
         if (AP->property.doscale) {
             fontsize = fontsize * annotScale;
             if (fontsize == 0) fontsize = 1;
         }

         /* Center-justified */
         PXplotX_scalable_font(x1, y1, AP->property.linelabel, fontsize,
                               CN_FALSE, CN_FALSE,
                               CN_FALSE, CN_FALSE);
      }
   }
}


/*
 * Clip a line against xmin
 */
static void clipX2D_in_xmin(x1,y1,x2,y2,rxmin,p1_clipped,p2_clipped)
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
static void clipX2D_in_ymin(x1,y1,x2,y2,rymin,p1_clipped,p2_clipped)
double *x1, *y1, *x2, *y2, rymin;
int    *p1_clipped, *p2_clipped;
{
   clipX2D_in_xmin(y1,x1,y2,x2,rymin,p1_clipped,p2_clipped);
}

/*
 * Clip a line against xmax
 */
static void clipX2D_in_xmax(x1,y1,x2,y2,rxmax,p1_clipped,p2_clipped)
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
static void clipX2D_in_ymax(x1,y1,x2,y2,rymax,p1_clipped,p2_clipped)
double *x1, *y1, *x2, *y2, rymax;
int    *p1_clipped, *p2_clipped;
{
   clipX2D_in_xmax(y1,x1,y2,x2,rymax,p1_clipped,p2_clipped);
}


/*
 * LABELS
 */

/* 
 * Plot the contour labels in X11 
 * The pointlist is in plot coordinates
 */
/*ARGSUSED*/
static void plotX2D_contlabel(pointhead, pointtail, 
                              linetype, linelabel, lbloffset)
CNpointptr pointhead, pointtail;
int        linetype;
char       *linelabel;
int        lbloffset;
{
   CNpointptr  P;
   double      x,y;
   double      dist, last_label_dist;
   double      idl_dist = 150.0;
   int         npts;

   if ((pointhead == NULL) || (linelabel == NULL)) return;

   /* label the contours at given point intervals */
   if (linetype == CN_LN_SOLID || linetype == CN_LN_DASHED) {

      /* Count the number of points first */
      npts = CNcount_points(pointhead, pointtail);

      /* 2 or fewer points are treated differently */
      if (npts == 1) {

         /* Put the label at the point */
         x = pointhead->x;
         y = pointhead->y;

         /* Draw the text */
         (void) drawX2D_label((int)x,(int)y,linelabel);

      } else if (npts == 2) {

         /* Put the label at the midpoint */
         x = 0.5*(pointhead->x + pointtail->x);
         y = 0.5*(pointhead->y + pointtail->y);

         /* Draw the text */
         (void) drawX2D_label((int)x,(int)y,linelabel);
        
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
               (void) drawX2D_label((int)x,(int)y,linelabel);

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
 */
static int drawX2D_label(x,y,label)
int x, y;
char *label;
{
   int         text_len, text_width, text_xpos, text_ypos;
   int         status=0;

   text_len   = strlen(label);
   text_width = XTextWidth(lblfont_info,label,text_len);
   text_xpos  = x - text_width/2;
   text_ypos  = y + 0.5*lblfont_info->max_bounds.ascent; 
   if (text_xpos > Xxmin-5 && text_xpos < Xxmax+5 &&
       text_ypos > Xymin-5 && text_ypos < Xymax+5) {
      if (window)
      XDrawString(display,window,gcl,
                  text_xpos,text_ypos,label,text_len);
      if (pixmap)
      XDrawString(display,pixmap,gcl,
                  text_xpos,text_ypos,label,text_len);
      status = 1;
   }
   return(status);
}


/*
 * Plot line labels on the side if necessary
 */
static void plotX2D_sidelabels()
{
   short contclip;
   int   xoffset;
   int   LABEL_FOUND = CNplotset_has_linelabels(plotdata);
   int   FCONT_FOUND = CNplotset_has_colored_contours(plotdata);

   /* Plot linelabels if necessary */
   if (LABEL_FOUND) {
      xoffset = 10;
      if (FCONT_FOUND) xoffset += LABEL_WIDTH + 20;
      PXplotX_linelabels(plotdata, hiddenline, 
                         Xxmin, Xxmax, Xymin, Xymax, xoffset);
   }

   /* Plot colored scales if necessary */
   if (FCONT_FOUND) {
      xoffset  = 10;
      contclip = CN_FALSE;
      if (contour_dptr) contclip = contour_dptr->data_pr.contclip;
      PXplotX_contscale(cstephead,csteptail,
                        Xxmin, Xxmax, Xymin, Xymax, xoffset, contclip);
   }
}



/*
 * TRANSLATION ROUTINES
 */

/*
 * Translate world to X11 coordinates and apply log options
 * This is for local use only because it uses local static variables.
 */
static void trn_world_to_X11(xw, yw, xp, yp)
double  xw,  yw;                   /* World coordinates            */
double *xp, *yp;                   /* X11 plot coordinates         */
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

   PXtranslate_world_to_X11(xw, yw, xp, yp,
                            Xxmin, Xxmax, Xymin, Xymax,
                            xmin, xmax, pymin, pymax,
                            xlog, ylog, xflip, yflip);
}


/*
 * Translate world to X11 coordinates.
 * Don't apply log options - the data has been previously converted to log
 * This is for local use only because it uses local static variables.
 */
static void trn_world_to_X11_nolog(xw, yw, xp, yp)
double  xw,  yw;                   /* World coordinates            */
double *xp, *yp;                   /* X11 plot coordinates         */
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

   PXtranslate_world_to_X11(xw, yw, xp, yp,
                            Xxmin, Xxmax, Xymin, Xymax,
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
 * POLYGON BUFFERING
 * used to speed up drawing of polygons with solid fills
 */

/*
 * Initialize the polygon buffer 
 */
static void init_polygon_buffer()
{
   int i, j;
   for (i=0; i<PX_MAX_FILL_COLORS; i++) {
      poly_buffer.npolygons[i] = 0;
      for (j=0; j<MAX_POLYGONS; j++)
         poly_buffer.polygons[i][j].npoints = 0;
   }
}

/*
 * Buffer a polygon
 */
/*ARGSUSED*/
static void buffer_polygon(nodehead, nodetail, color)
CNnodeptr nodehead, nodetail;
int       color;
{
   CNnodeptr N;
   double    x, y;
   int       npoints, npolygons, i;

   if (nodehead == NULL) return;
   if (color < 0 || color >= PX_MAX_FILL_COLORS) {
      (void) fprintf(stderr,"Error! Color=%d!\n",color);
      return;
   }

   /*
    * If the polygon buffer is full, empty it 
    */
   if (poly_buffer.npolygons[color] == MAX_POLYGONS - 1) {
      /*
       * IF the buffer is full, set the foreground color of the gc and
       * and draw the polygons 
       */
      XSetForeground(display, gc, colors[color + PX_MAX_NAMED_COLORS]);
      for (i=0; i<poly_buffer.npolygons[color]; i++) {
         if (poly_buffer.polygons[color][i].npoints) {
            if (window)
            XFillPolygon(display,window,gc,
                         poly_buffer.polygons[color][i].points,
                         poly_buffer.polygons[color][i].npoints,
                         Convex,CoordModeOrigin);
            if (pixmap)
            XFillPolygon(display,pixmap,gc,
                         poly_buffer.polygons[color][i].points,
                         poly_buffer.polygons[color][i].npoints,
                         Convex,CoordModeOrigin);
         }
         poly_buffer.polygons[color][i].npoints = 0;
      }
      /* Reset the polygon buffer */
      poly_buffer.npolygons[color] = 0;
   }

   /* 
    * Rescale the points and save in the polygon buffer 
    */
   npoints   = 0;
   npolygons = poly_buffer.npolygons[color];
   for (N=nodehead; N!=NULL && npoints < MAX_POINTS; N=N->next) {
      trn_world_to_X11(N->coord->x,N->coord->y,&x,&y);
      poly_buffer.polygons[color][npolygons].points[npoints].x = (int)x;
      poly_buffer.polygons[color][npolygons].points[npoints].y = (int)y;
      npoints++;
   }
   poly_buffer.polygons[color][npolygons].npoints = npoints;
   poly_buffer.npolygons[color] += 1;
}

/*
 * flush the polygon buffer 
 */
static void flush_polygon_buffer()
{
   int color, i;

   /*
    * Go thru each buffer 
    */
   for (color=0; color<PX_MAX_FILL_COLORS; color++) {
      /*
       * IF there are any polygons in this buffer, draw it
       */      
      if (poly_buffer.npolygons[color]) {
         XSetForeground(display, gc, colors[color + PX_MAX_NAMED_COLORS]);
         for (i=0; i<poly_buffer.npolygons[color]; i++) {
            if (poly_buffer.polygons[color][i].npoints) {
               if (window)
               XFillPolygon(display,window,gc,
                            poly_buffer.polygons[color][i].points,
                            poly_buffer.polygons[color][i].npoints,
                            Convex,CoordModeOrigin);
               if (pixmap)
               XFillPolygon(display,pixmap,gc,
                            poly_buffer.polygons[color][i].points,
                            poly_buffer.polygons[color][i].npoints,
                            Convex,CoordModeOrigin);
            }
            poly_buffer.polygons[color][i].npoints = 0;
         }
      }
      poly_buffer.npolygons[color] = 0;
   }
}


/*
 * OPTIMIZATION FOR DENSELY PACKED GRIDS
 */

/*
 * Plot the rectangular mesh in X11
 * Use the bitmap format which should be faster for large datasets
 * Return 0 if failed.
 * This routine uses either rectangles or the original array
 * If both exist, use the original array
 */
static int plotX2D_bitmap_rects(Dptr,contfill)
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
   nx = Xxmax - Xxmin;
   ny = Xymax - Xymin;
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
   for (i=0; i<nx; i++) 
   for (j=0; j<ny; j++) {
      if (narr[i+j*nx] > 0) {
         z = zarr[i+j*nx]/(double)narr[i+j*nx];
         colr = find_contour_color(z, Dptr->data_pr.contclip);
         if (colr >=0 && colr < PX_MAX_FILL_COLORS) {
            ix = Xxmin + i;
            iy = Xymax - j;
            if (xflip) ix = Xxmax - i;
            if (yflip) iy = Xymin + j;
            buffer_point(ix, iy, colr);
         }
      }
   }

   /* Flush the point buffer */
   flush_point_buffer();

   /* Reset color */
   PXsetColorX(0);

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
}

/*
 * Buffer a point
 */
/*ARGSUSED*/
static void buffer_point(x, y, color)
int x, y;
int color;
{

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
      XSetForeground(display, gc, colors[color + PX_MAX_NAMED_COLORS]);
      if (window)
      XDrawPoints(display,window,gc,
                  point_buffer[color].points,
                  point_buffer[color].npoints,
                  CoordModeOrigin);
      if (pixmap)
      XDrawPoints(display,pixmap,gc,
                  point_buffer[color].points,
                  point_buffer[color].npoints,
                  CoordModeOrigin);
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
   int color;

   /*
    * Go thru each buffer 
    */
   for (color=0; color<PX_MAX_FILL_COLORS; color++) {
      /*
       * IF there are any polygons in this buffer, draw it
       */      
      if (point_buffer[color].npoints > 0) {
         XSetForeground(display, gc, colors[color + PX_MAX_NAMED_COLORS]);
         if (window)
         XDrawPoints(display,window,gc,
                     point_buffer[color].points,
                     point_buffer[color].npoints,
                     CoordModeOrigin);
         if (pixmap)
         XDrawPoints(display,pixmap,gc,
                     point_buffer[color].points,
                     point_buffer[color].npoints,
                     CoordModeOrigin);
         /* Reset the buffer */
         point_buffer[color].npoints = 0;
      }
   }
}

