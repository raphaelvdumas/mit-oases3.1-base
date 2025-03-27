/*
 * plotps_3D.c - PostScript subroutines for drawing 3D plots
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "plotps.h"
#include "CNplot.h"

#define EXTREMELY_SMALL 1.0e-99

#define MAX_ARR_SIZE    1000   /* Maximum no of points in an array */
#define X_AXIS          1
#define Y_AXIS          2
#define Z_AXIS          3


/*
 * FORWARD DECLARATIONS
 */
void PXplotps3D();
static void drawPSplot3D();
static void plotPS3D_labels();
static void plotPS3D_guides();
static void plotPS3D_poly_guides();
static void drawPS3D_frame();
static void paintPS3D_inner_planes();

static void drawPS3D_static_axes();
static void drawPS3D_dynamic_axes();
static void drawPS3D_axis();
static void plotPS3D_ticks();
static void plotPS3D_Auto_ticks();
static void plotPS3D_User_ticks();

static void drawPS3D_line();
static void paintPS3D_poly4();
static void plotPS3D_ticklabel();
static int  ticklabel_overlap();
static void plotPS3D_label();

static void plotPS3D();
static void plotPS3D_grid();
static void plotPS3D_mesh4D();
static void plotPS3D_mesh4D_cube();
static void plotPS3D_vectors();
static void plotPS3D_boundary();
static void plotPS3D_elems();
static void plotPS3D_trias();
static void plotPS3D_rects();
static void plotPS3D_polys();
static void plotPS3D_single_solid_tria();
static void plotPS3D_single_solid_rect();
static void plotPS3D_single_solid_poly();
static void plotPS3D_single_fill_tria();
static void plotPS3D_single_fill_rect();
static void plotPS3D_single_fill_poly();
static void plotPS3D_nodes();
static void plotPS3D_dataset_curves();
static void plotPS3D_curve();
static void fillPS3D_curve();
static void plotPS3D_spline_curve();
static void plotPS3D_markers();

static void plotPS3D_curveID();
static void plotPS3D_pointIDs();
static void plotPS3D_nodeIDs();
static void plotPS3D_triaIDs();
static void plotPS3D_rectIDs();

static void annotatePS3D();
static void plotPS3D_single_annotation();
static void plotPS3D_annot_rect();
static void plotPS3D_annot_line();
static void plotPS3D_annot_arrow();
static void plotPS3D_arrow();
static void plotPS3D_annot_point();
static void plotPS3D_annot_text();
static void clipPS3D_in_xmin();
static void clipPS3D_in_ymin();
static void clipPS3D_in_zmin();
static void clipPS3D_in_xmax();
static void clipPS3D_in_ymax();
static void clipPS3D_in_zmax();

static void plotPS3D_sidelabels();

static void trn_world_to_PS();


/* Plot variables */
static short  grid;
static short  clip;
static short  xabs, yabs, zabs;
static short  xlog, ylog, zlog;
static short  xticks, yticks, zticks;
static short  xautorange, yautorange, zautorange;
static double xscale, yscale, zscale;
static short  overlay;
static int    plotlabels;
static char   xlabel[MAXCHR];
static char   ylabel[MAXCHR];
static char   zlabel[MAXCHR];
static char   toplabel[MAXCHR];
static char   *comment;
static char   *subtitle;

static short  axis_label;    /* Label the axes                  */
static short  axis_movement; /* Axis response to changing views */
static short  axis_guides;   /* Plot axis guides                */
static short  paint_cube;    /* Paint the background            */
static short  hiddenline;    /* Do hiddenline plots             */
static short  quick_sort;    /* Use quicksort algorithm         */
static short  nosort;        /* Don't sort                      */

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

/* The view transformation matrix */
static CNmatrix view_transfo;


/*
 * Draw the plot in PostScript
 */
void PXplotps3D(pdata)
CNplotsetptr pdata;                  /* The plot data is here   */
{
   void PXquery_contours();
   int  PXquery_labels();
   void PXcheck_viewport();
   void PXconvert_viewport_to_log();
   double bxmin, bxmax, bymin, bymax, bzmin, bzmax;
   double lxmin, lxmax, lymin, lymax, lzmin, lzmax;
   double Pxwid, Pywid;
   char   label[CN_MAXCHAR];

   /* Error checking */
   if (pdata==NULL) {
      (void) fprintf(stderr,"PXplotps3D(): Error! Cannot plot NULL plotset!\n");
      return;
   }

   /* Copy the variables to the static variables first */
   plotdata  = pdata; 

   /* Initialize some other plot variables */
   grid          = plotdata->plot_pr.grid;
   xabs          = plotdata->plot_pr.xabs;
   yabs          = plotdata->plot_pr.yabs;
   zabs          = plotdata->plot_pr.zabs;
   xlog          = plotdata->plot_pr.xlog;
   ylog          = plotdata->plot_pr.ylog;
   zlog          = plotdata->plot_pr.zlog;
   xticks        = plotdata->plot_pr.xticks;
   yticks        = plotdata->plot_pr.yticks;
   zticks        = plotdata->plot_pr.zticks;
   xautorange    = plotdata->plot_pr.xautorange;
   yautorange    = plotdata->plot_pr.yautorange;
   zautorange    = plotdata->plot_pr.zautorange;
   xscale        = plotdata->plot_pr.xscale; if (xscale <= 0) xscale = 1.0;
   yscale        = plotdata->plot_pr.yscale; if (yscale <= 0) yscale = 1.0;
   zscale        = plotdata->plot_pr.zscale; if (zscale <= 0) zscale = 1.0;
   overlay       = plotdata->plot_pr.overlay;
   clip          = CN_TRUE;
   axis_label    = plotdata->view_pr->axis_label;
   axis_movement = plotdata->view_pr->axis_movement;
   axis_guides   = plotdata->view_pr->axis_guides;
   paint_cube    = plotdata->view_pr->paint_cube;
   hiddenline    = plotdata->view_pr->hiddenline;
   quick_sort    = CN_TRUE;
   nosort        = CN_FALSE;
   plotlabels    = PXquery_labels(plotdata);

   /* User-specified axis labels? */
   if (plotdata->plot_pr.xlabelhead != NULL) {
      xlog             = CN_FALSE;
      xabs             = CN_FALSE;
   }
   if (plotdata->plot_pr.ylabelhead != NULL) {
      ylog             = CN_FALSE;
      yabs             = CN_FALSE;
   }
   if (plotdata->plot_pr.zlabelhead != NULL) {
      zlog             = CN_FALSE;
      zabs             = CN_FALSE;
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

   /* zlabel */
   if (plotdata->plot_pr.zlabel == NULL)
      (void) strcpy(zlabel   , CN_DEF_ZLABEL);
   else
      (void) strcpy(zlabel   , plotdata->plot_pr.zlabel);
   if (zscale != 1.0) {
      (void) sprintf(label," (x %g)",zscale);
      (void) strcat(zlabel, label);
   }

   /* Toplabel */
   if (plotdata->plot_pr.toplabel == NULL)
      (void) strcpy(toplabel , CN_DEF_TLABEL);
   else
      (void) strcpy(toplabel , plotdata->plot_pr.toplabel);
 
   /* Comment & subtitle */
   comment = plotdata->plot_pr.comment;
   subtitle= plotdata->plot_pr.subtitle;

   /* Reset the plotset boundaries */
   CNset_plotset_boundaries(pdata,
                            &bxmin, &bxmax, &bymin, &bymax, &bzmin, &bzmax);

   /* The viewport */
   xmin = plotdata->plot_pr.vxmin;
   xmax = plotdata->plot_pr.vxmax;
   ymin = plotdata->plot_pr.vymin;
   ymax = plotdata->plot_pr.vymax;
   zmin = plotdata->plot_pr.vzmin;
   zmax = plotdata->plot_pr.vzmax;

   /*
    * Check and fix the viewport
    */
   PXcheck_viewport(&xmin,&xmax,bxmin,bxmax,&xlog,xabs,CN_FALSE,"x");
   plotdata->plot_pr.vxmax = xmax;
   plotdata->plot_pr.vxmin = xmin;
   plotdata->plot_pr.xlog  = xlog;

   PXcheck_viewport(&ymin,&ymax,bymin,bymax,&ylog,yabs,CN_FALSE,"y");
   plotdata->plot_pr.vymax = ymax;
   plotdata->plot_pr.vymin = ymin;
   plotdata->plot_pr.ylog  = ylog;

   PXcheck_viewport(&zmin,&zmax,bzmin,bzmax,&zlog,zabs,CN_FALSE,"z");
   plotdata->plot_pr.vzmax = zmax;
   plotdata->plot_pr.vzmin = zmin;
   plotdata->plot_pr.zlog  = zlog;

   /*
    * If log axes then convert the boundary values.
    * The axes will be drawn from lxmin-lxmax,lymin-lymax,lzmin-lzmax;
    * this is as if a linear set of axes was specified with those
    * boundaries.  However, the curves to be plotted in the plot() routine
    * still have to be adjusted for log-scale
    */
   PXconvert_viewport_to_log(&lxmin, &lxmax, xmin, xmax, xlog, xautorange);
   PXconvert_viewport_to_log(&lymin, &lymax, ymin, ymax, ylog, yautorange);
   PXconvert_viewport_to_log(&lzmin, &lzmax, zmin, zmax, zlog, zautorange);
   xmin = lxmin;
   xmax = lxmax;
   ymin = lymin;
   ymax = lymax;
   zmin = lzmin;
   zmax = lzmax;

   /* Save the plot viewport */
   plotdata->plot_pr.pxmin = xmin;
   plotdata->plot_pr.pxmax = xmax;
   plotdata->plot_pr.pymin = ymin;
   plotdata->plot_pr.pymax = ymax;
   plotdata->plot_pr.pzmin = zmin;
   plotdata->plot_pr.pzmax = zmax;

   /* Recalculate the view_transfo - but don't adjust the window */
   CNreinitialize_view(pdata->view_pr,xmin,xmax,ymin,ymax,zmin,zmax);

   /* The view transformation */
   CNcopy_matrix(view_transfo, pdata->view_pr->view_transfo);

   /*
    * Use a square viewport
    *    the square width is the smaller of (width,height)
    */

   /* Initialize - Pgxmin, Pgxmax, Pgymin, Pgymax are page dimensions */
   Pxmin = Pgxmin + 1.0*scale*PBDR_DIM;
   Pxmax = Pgxmax - 1.0*scale*PBDR_DIM;
   Pymin = Pgymin + 1.0*scale*PBDR_DIM;
   Pymax = Pgymax - 1.2*scale*PBDR_DIM; /* Leave a bit of room for labels */
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

   /*
    * The size of the plot is Pxwid x Pywid (Pxwid = Pywid);
    * try to fit the plot in the center of the window defined by
    * (0,0) - (Width, Height)
    */
   if (Pywid > Pxwid) {
      Pywid = Pxwid;
      Pymin = 0.5*(Pymin + Pymax) - 0.5*Pywid;
      Pymax = Pymin + Pywid;
   }
   if (Pywid < Pxwid) {
      Pxwid = Pywid;
      Pxmin = 0.5*(Pxmin + Pxmax) - 0.5*Pxwid;
      Pxmax = Pxmin + Pxwid;
   }

   /* 
    * Now do the PostScript plot 
    */
   drawPSplot3D();
}

/*
 * Draw the plot in POSTSCRIPT
 * The axes could be dynamic, that is, they adjust to a changing view
 * or static meaning they are fixed to certain coordinates.
 */
static void drawPSplot3D()
{
   void PXfind_outer_axes();
   CNcoord pta, ptb, ptc, ptd;

   /* Set contour levels for the contour-type datasets */
   cstephead = NULL;
   csteptail = NULL;
   PXquery_contours(plotdata,&cstephead,&csteptail, &contour_dptr);

   /*
    * Find the outer-most points on the axes according to view
    */
   PXfind_outer_axes(xmin, ymin, zmin, xmax, ymax, zmax, view_transfo,
                     &pta, &ptb, &ptc, &ptd);

   /* clip if necessary */
   if (clip) (void) fprintf(ips,"gsave\n");
   if (clip) (void) fprintf(ips,"boundingboxpath clip\n");

   /* Put on labels */
   plotPS3D_labels();

   /*
    * Now draw the axes
    */

   /* Paint the background of the cube if necessary */
   if (paint_cube) paintPS3D_inner_planes(&pta, &ptb, &ptc, &ptd);

   /* Draw the frame with dotted lines for back-side axes */
   drawPS3D_frame(&pta, &ptb, &ptc, &ptd, 0);

   /*
    * Draw the plot
    */
   plotPS3D();

   /*
    * Draw the annotations
    */
   annotatePS3D();

   /*
    * Redraw the axes
    */

   /* Redraw the frame to account for axes obscured during data plotting */
   drawPS3D_frame(&pta, &ptb, &ptc, &ptd, 1);

   /* Put on the labels */
   if (axis_label) {
      if (axis_movement == 1)
         drawPS3D_static_axes();
      else
         drawPS3D_dynamic_axes(&pta, &ptb, &ptc, &ptd);
   }

   /* Unclip */
   if (clip) (void) fprintf(ips,"grestore\n");

   /* Delete contour step-levels */
   CNdelete_contstep_list(&cstephead, &csteptail);
}


/*
 * DRAW LABELS
 */
static void plotPS3D_labels()
{
   (void) fprintf(ips,"%%------------------------------------------------\n");
   (void) fprintf(ips,"%%--------------Draw the Labels-------------------\n");
   (void) fprintf(ips,"%%------------------------------------------------\n");

   /* Top Label */
   (void) fprintf(ips,"setTopLblFont\n");
   (void) fprintf(ips,"%.2f %.2f moveto ",0.5*(Pxmax+Pxmin),Pymax+30*fscale);
   (void) fprintf(ips,"(%s) UPJ CEJ show\n",PXmodifyPS_string(toplabel));

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
   (void) fprintf(ips,"%.2f %.2f moveto ",0.5*(Pxmax+Pxmin),Pymax+9*fscale);
   (void) fprintf(ips,"(%s) UPJ CEJ show\n",PXmodifyPS_string(subtitle));
   }

   /* Draw the plot labels */
   if (plotlabels) plotPS3D_sidelabels();

   /* Draw a set of guides */
   if (axis_guides) plotPS3D_guides();
}


/*
 * Draw a set of small axes to use as a guide
 */
static void plotPS3D_guides()
{
   char    labelx[10];
   char    labely[10];
   char    labelz[10];
   double text_xpos, text_ypos;
   double x0,y0,vx_x,vx_y,vy_x,vy_y,vz_x,vz_y;
   double dx,dy,dz,delt;
   double min_x, min_y, wid_x, wid_y, MAXLEN;

   (void) fprintf(ips,"%%------------------------------------------------\n");
   (void) fprintf(ips,"%%--------------Draw the 3D Mini-Axes-------------\n");
   (void) fprintf(ips,"%%------------------------------------------------\n");

   /* Initialize the labels */
   (void) strcpy(labelx, "X");
   (void) strcpy(labely, "Y");
   (void) strcpy(labelz, "Z");
   /*
   if (strlen(xlabel) > 0) labelx[0] = xlabel[0];
   if (strlen(ylabel) > 0) labely[0] = ylabel[0];
   if (strlen(zlabel) > 0) labelz[0] = zlabel[0];
    */

   /*
    * The midpoint of the guide is placed at xmin,ymin
    */
   wid_x = 100.0*scale;
   wid_y = 100.0*scale;
   min_x = Pxmin - 0.5*wid_x;
   min_y = Pymin - 1.0*wid_y;
   MAXLEN = 40.0*scale;
   (void) PXlinetypPS(CN_LN_SOLID, 2);

   /* Figure out the 3D vectors, i.e. X,Y,Z, axis directions */
   delt = MAXOF3(xmax-xmin, ymax-ymin, zmax-zmin);
   trn_world_to_PS(0.0      ,0.0      ,0.0,      &x0,&y0);
   trn_world_to_PS(delt     ,0.0      ,0.0,      &vx_x,&vx_y);
   trn_world_to_PS(0.0      ,delt     ,0.0,      &vy_x,&vy_y);
   trn_world_to_PS(0.0      ,0.0      ,delt     ,&vz_x,&vz_y);

   /* Get the direction vectors */
   vx_x -= x0;   vx_y -= y0;   dx = sqrt(vx_x*vx_x + vx_y*vx_y);
   vy_x -= x0;   vy_y -= y0;   dy = sqrt(vy_x*vy_x + vy_y*vy_y);
   vz_x -= x0;   vz_y -= y0;   dz = sqrt(vz_x*vz_x + vz_y*vz_y);

   /* Scale the vectors */
   if (dx > 1.0e-2) { vx_x = MAXLEN*vx_x/dx;  vx_y = MAXLEN*vx_y/dx;}
   if (dy > 1.0e-2) { vy_x = MAXLEN*vy_x/dy;  vy_y = MAXLEN*vy_y/dy;}
   if (dz > 1.0e-2) { vz_x = MAXLEN*vz_x/dz;  vz_y = MAXLEN*vz_y/dz;}

   /* Now draw the lines */
   x0 = min_x + 0.5*wid_x;
   y0 = min_y + 0.5*wid_y;
   PXdrawPS_line(x0,y0,x0+vx_x,y0+vx_y,CN_LN_SOLID,2);
   PXdrawPS_line(x0,y0,x0+vy_x,y0+vy_y,CN_LN_SOLID,2);
   PXdrawPS_line(x0,y0,x0+vz_x,y0+vz_y,CN_LN_SOLID,2);

   /* Draw polygons representing the planes */
   plotPS3D_poly_guides(x0,y0,vx_x,vx_y,vy_x,vy_y,vz_x,vz_y);

   /* Set the font */
   (void) fprintf(ips,"setMkrsLblFont\n");

   /* Put on axis labels */
   text_xpos  = x0 + 1.2*vx_x;
   text_ypos  = y0 + 1.2*vx_y;
   (void) fprintf(ips,"%.2f %.2f moveto ",text_xpos,text_ypos);
   (void) fprintf(ips,"(%s) MIJM CEJ show\n",PXmodifyPS_string(labelx));
   text_xpos  = x0 + 1.2*vy_x;
   text_ypos  = y0 + 1.2*vy_y;
   (void) fprintf(ips,"%.2f %.2f moveto ",text_xpos,text_ypos);
   (void) fprintf(ips,"(%s) MIJM CEJ show\n",PXmodifyPS_string(labely));
   text_xpos  = x0 + 1.2*vz_x;
   text_ypos  = y0 + 1.2*vz_y;
   (void) fprintf(ips,"%.2f %.2f moveto ",text_xpos,text_ypos);
   (void) fprintf(ips,"(%s) MIJM CEJ show\n",PXmodifyPS_string(labelz));

   (void) PXlinetypPS(CN_LN_SOLID, 1);
}

/*
 * Draw a set of small axes to use as a guide
 */
static void plotPS3D_poly_guides(x0,y0,vx_x,vx_y,vy_x,vy_y,vz_x,vz_y)
double x0,y0,vx_x,vx_y,vy_x,vy_y,vz_x,vz_y;
{
#define XY 1
#define XZ 2
#define YZ 3

   PSPoint xz_plane[5], yz_plane[5], xy_plane[5];
   CNcoord xz_point, yz_point, xy_point;
   int     arr[3], i;
   int     fillcolor, linecolor;
   double  LEN=0.5;

   /* Build polygons to represent the planes */
   xz_plane[0].x = x0;                   xz_plane[0].y = y0;
   xz_plane[1].x = x0+LEN*vx_x;          xz_plane[1].y = y0+LEN*vx_y;
   xz_plane[2].x = x0+LEN*vx_x+LEN*vz_x; xz_plane[2].y = y0+LEN*vx_y+LEN*vz_y;
   xz_plane[3].x = x0+LEN*vz_x;          xz_plane[3].y = y0+LEN*vz_y;
   xz_plane[4].x = x0;                   xz_plane[4].y = y0;

   yz_plane[0].x = x0;                   yz_plane[0].y = y0;
   yz_plane[1].x = x0+LEN*vy_x;          yz_plane[1].y = y0+LEN*vy_y;
   yz_plane[2].x = x0+LEN*vy_x+LEN*vz_x; yz_plane[2].y = y0+LEN*vy_y+LEN*vz_y;
   yz_plane[3].x = x0+LEN*vz_x;          yz_plane[3].y = y0+LEN*vz_y;
   yz_plane[4].x = x0;                   yz_plane[4].y = y0;

   xy_plane[0].x = x0;                   xy_plane[0].y = y0;
   xy_plane[1].x = x0+LEN*vx_x;          xy_plane[1].y = y0+LEN*vx_y;
   xy_plane[2].x = x0+LEN*vx_x+LEN*vy_x; xy_plane[2].y = y0+LEN*vx_y+LEN*vy_y;
   xy_plane[3].x = x0+LEN*vy_x;          xy_plane[3].y = y0+LEN*vy_y;
   xy_plane[4].x = x0;                   xy_plane[4].y = y0;

   /* Figure out the order of the planes by comparing z-values of points */
   xz_point.x = 1.0;    xz_point.y = 0.0;   xz_point.z = 1.0;
   yz_point.x = 0.0;    yz_point.y = 1.0;   yz_point.z = 1.0;
   xy_point.x = 1.0;    xy_point.y = 1.0;   xy_point.z = 0.0;
   xz_point = CNtransform_point(&xz_point,view_transfo);
   yz_point = CNtransform_point(&yz_point,view_transfo);
   xy_point = CNtransform_point(&xy_point,view_transfo);

   if ((xy_point.z > xz_point.z) && (xy_point.z > yz_point.z)) {
      arr[0] = XY;
      arr[1] = (xz_point.z > yz_point.z) ? XZ : YZ;
      arr[2] = (xz_point.z > yz_point.z) ? YZ : XZ;
   } else if ((xz_point.z > xy_point.z) && (xz_point.z > yz_point.z)) {
      arr[0] = XZ;
      arr[1] = (xy_point.z > yz_point.z) ? XY : YZ;
      arr[2] = (xy_point.z > yz_point.z) ? YZ : XY;
   } else {
      arr[0] = YZ;
      arr[1] = (xy_point.z > xz_point.z) ? XY : XZ;
      arr[2] = (xy_point.z > xz_point.z) ? XZ : XY;
   }

   /* Now draw small filled polygons */
   fillcolor = PXpolyColorIndexPS(1);   /* yellow */
   linecolor = PXpolyColorIndexPS(4);   /* red    */
   for (i=2; i>=0; i--) {
      switch (arr[i]) {
      case XY :
         PXfillPS_polygon(xy_plane,5,
                          CN_FILL_SOLID,fillcolor,CN_LN_SOLID,linecolor,1);
         break;
      case XZ :
         PXfillPS_polygon(xz_plane,5,
                          CN_FILL_SOLID,fillcolor,CN_LN_SOLID,linecolor,1);
         break;
      case YZ :
      default :
         PXfillPS_polygon(yz_plane,5,
                          CN_FILL_SOLID,fillcolor,CN_LN_SOLID,linecolor,1);
         break;
      }
   }
   /* Reset */
   PXsetColorPS(0);
}


/*
 * DRAW AXIS FRAME AND PANELS
 */

/*
 * Draw the frame of the cube using given coordinates.
 * The axes on the back side of the cube are dotted
 */
static void drawPS3D_frame(pta, ptb, ptc, ptd, outside)
CNcoord *pta, *ptb, *ptc, *ptd;
int     outside;
{
   CNcoord pte, ptf, ptg, pth;
   CNcoord ptft, ptht;

   /*
    * pts a-d are 4 points on the outer frame
    * e and g are points on the outside of the frame.
    * f and h are points on the inside of the frame.
    * either h or f could be obscured (i.e. in the inner corner)
    *
    *           ______              /|\e
    *        a |\    .\ e          / | \
    *          | \   . \          /  |  \
    *          | h\_____\ g     a|.  |f .|g
    *        b |..|...  |        | ./ \. |
    *           \ |  f. |        | /.h.\ |
    *            \|    .|       b|/  .  \|
    *              ------         \  .  /d
    *             c     d          \ . /
    *                              c\./
    *
    * identify e and f from a-d
    * e is diagonally opposite to c
    * f is diagonally opposite to c, but with the same z-value
    * g has a different z-value compared to d
    * h has a different z-value compared to c
    */

   (void) fprintf(ips,"%%------------------------------------------------\n");
   (void) fprintf(ips,"%%--------------Draw the Frame--------------------\n");
   (void) fprintf(ips,"%%------------------------------------------------\n");

   pte.x = (ptc->x == xmin) ? xmax : xmin;
   pte.y = (ptc->y == ymin) ? ymax : ymin;
   pte.z = (ptc->z == zmin) ? zmax : zmin;

   ptf.x = pte.x;
   ptf.y = pte.y;
   ptf.z = ptc->z;

   ptg.x = ptd->x;
   ptg.y = ptd->y;
   ptg.z = (ptd->z == zmin) ? zmax : zmin;

   pth.x = ptc->x;
   pth.y = ptc->y;
   pth.z = (ptc->z == zmin) ? zmax : zmin;

   /* Check to see which of (f,h) is on the inside corner of the box */
   ptft  = CNtransform_point(&ptf,view_transfo);
   ptht  = CNtransform_point(&pth,view_transfo);

   if (ptft.z > ptht.z) {
      /* f is on the outside, and h is on the inside */

      /* Draw the inner-three axes : h-a, h-g, h-c */
      if (!outside) {
      drawPS3D_line(pth.x, pth.y, pth.z, pta->x,pta->y,pta->z, CN_GD_DOTTED, 1);
      drawPS3D_line(pth.x, pth.y, pth.z, ptc->x,ptc->y,ptc->z, CN_GD_DOTTED, 1);
      drawPS3D_line(pth.x, pth.y, pth.z, ptg.x, ptg.y, ptg.z,  CN_GD_DOTTED, 1);
      }

      /* Draw the outer-three axes : f-e, f-d, f-b */
      drawPS3D_line(ptf.x, ptf.y, ptf.z, ptb->x,ptb->y,ptb->z, CN_LN_SOLID, 1);
      drawPS3D_line(ptf.x, ptf.y, ptf.z, ptd->x,ptd->y,ptd->z, CN_LN_SOLID, 1);
      drawPS3D_line(ptf.x, ptf.y, ptf.z, pte.x, pte.y, pte.z,  CN_LN_SOLID, 1);

   } else {
      /* h is on the outside, and f is on the inside */

      /* Draw the inner-three axes : f-e, f-d, f-b */
      if (!outside) {
      drawPS3D_line(ptf.x, ptf.y, ptf.z, ptb->x,ptb->y,ptb->z, CN_GD_DOTTED, 1);
      drawPS3D_line(ptf.x, ptf.y, ptf.z, ptd->x,ptd->y,ptd->z, CN_GD_DOTTED, 1);
      drawPS3D_line(ptf.x, ptf.y, ptf.z, pte.x, pte.y, pte.z,  CN_GD_DOTTED, 1);
      }

      /* Draw the outer-three axes : h-a, h-g, h-c */
      drawPS3D_line(pth.x, pth.y, pth.z, pta->x,pta->y,pta->z, CN_LN_SOLID, 1);
      drawPS3D_line(pth.x, pth.y, pth.z, ptc->x,ptc->y,ptc->z, CN_LN_SOLID, 1);
      drawPS3D_line(pth.x, pth.y, pth.z, ptg.x, ptg.y, ptg.z,  CN_LN_SOLID, 1);

   }

   /* Draw the upper-three axes : a-e, e-g, g-d */
   if (!outside) {
   drawPS3D_line(pta->x,pta->y,pta->z,pte.x, pte.y, pte.z,  CN_LN_SOLID, 1);
   drawPS3D_line(pte.x, pte.y, pte.z, ptg.x, ptg.y, ptg.z,  CN_LN_SOLID, 1);
   drawPS3D_line(ptg.x, ptg.y, ptg.z, ptd->x,ptd->y,ptd->z, CN_LN_SOLID, 1);
   }

   /* Draw the outer-three frames */
   drawPS3D_line(pta->x,pta->y,pta->z,ptb->x,ptb->y,ptb->z, CN_LN_SOLID, 1);
   drawPS3D_line(ptb->x,ptb->y,ptb->z,ptc->x,ptc->y,ptc->z, CN_LN_SOLID, 1);
   drawPS3D_line(ptc->x,ptc->y,ptc->z,ptd->x,ptd->y,ptd->z, CN_LN_SOLID, 1);
}

/* Paint the back-planes of the cube */
/*ARGSUSED*/
static void paintPS3D_inner_planes(pta, ptb, ptc, ptd)
CNcoord *pta, *ptb, *ptc, *ptd;
{
   double xtmp, ytmp, ztmp;

   (void) fprintf(ips,"%%------------------------------------------------\n");
   (void) fprintf(ips,"%%--------------Draw the Planes-------------------\n");
   (void) fprintf(ips,"%%------------------------------------------------\n");

   /*
    * Draw the xy-plane first
    */
   ztmp = (ptb->z == zmin) ? zmin : zmax;
   paintPS3D_poly4(xmin,ymin,ztmp,
                   xmax,ymin,ztmp,
                   xmax,ymax,ztmp,
                   xmin,ymax,ztmp);

   /*
    * Now draw the xz-plane
    */
   ytmp = (ptc->y == ymin) ? ymax : ymin;
   paintPS3D_poly4(xmin,ytmp,zmin,
                   xmax,ytmp,zmin,
                   xmax,ytmp,zmax,
                   xmin,ytmp,zmax);

   /*
    * Now draw the yz-plane
    */
   xtmp = (ptc->x == xmin) ? xmax : xmin;
   paintPS3D_poly4(xtmp,ymin,zmin,
                   xtmp,ymax,zmin,
                   xtmp,ymax,zmax,
                   xtmp,ymin,zmax);
}


/*
 * DRAW LABELLED AXES
 */

/*
 * Draw the axes of the cube using given coordinates
 * These axes are fixed to certain coordinates and do not
 * change with the view
 */
static void drawPS3D_static_axes()
{
   (void) fprintf(ips,"%%------------------------------------------------\n");
   (void) fprintf(ips,"%%--------------Draw the Axes---------------------\n");
   (void) fprintf(ips,"%%------------------------------------------------\n");

   /* Draw the x-axis */
   drawPS3D_axis(X_AXIS,
                 xmin,ymax,zmin,xmax,ymax,zmin,
                 0.0      ,ymax-ymin,0.0      ,
                 0.0      ,0.0      ,zmin-zmax,
                 xticks, xautorange, xlog, xmin, xmax, xlabel, xscale,
                 plotdata->plot_pr.xlabelhead, plotdata->plot_pr.xlabeltail);

   /* Draw the y-axis */
   drawPS3D_axis(Y_AXIS,
                 xmax,ymin,zmin,xmax,ymax,zmin,
                 xmax-xmin,0.0      ,0.0      ,
                 0.0      ,0.0      ,zmin-zmax,
                 yticks, yautorange, ylog, ymin, ymax, ylabel, yscale,
                 plotdata->plot_pr.ylabelhead, plotdata->plot_pr.ylabeltail);

   /* Draw the z-axis */
   drawPS3D_axis(Z_AXIS,
                 xmax,ymin,zmin,xmax,ymin,zmax,
                 0.0      ,ymin-ymax,0.0      ,
                 xmax-xmin,0.0      ,0.0      ,
                 zticks, zautorange, zlog, zmin, zmax, zlabel, zscale,
                 plotdata->plot_pr.zlabelhead, plotdata->plot_pr.zlabeltail);
}

/*
 * Draw the outer axes of the cube using given coordinates
 * These axes are dynamic, that is, they depend on the view orientation
 */
static void drawPS3D_dynamic_axes(pta, ptb, ptc, ptd)
CNcoord *pta, *ptb, *ptc, *ptd;
{

   (void) fprintf(ips,"%%------------------------------------------------\n");
   (void) fprintf(ips,"%%--------------Draw the Axes---------------------\n");
   (void) fprintf(ips,"%%------------------------------------------------\n");

   /* Draw the z-axis */
   drawPS3D_axis(Z_AXIS,
                 pta->x,pta->y,pta->z,ptb->x,ptb->y,ptb->z,
                 ptb->x-ptc->x,ptb->y-ptc->y,ptb->z-ptc->z,
                 ptc->x-ptd->x,ptc->y-ptd->y,ptc->z-ptd->z,
                 zticks, zautorange, zlog, pta->z, ptb->z, zlabel, zscale,
                 plotdata->plot_pr.zlabelhead, plotdata->plot_pr.zlabeltail);

   /* Draw the x and y axes */
   if (ptb->x == ptc->x) {
      /* b-c is y-axis, c-d is x-axis */

      /* Draw the y-axis */
      drawPS3D_axis(Y_AXIS,
                    ptb->x,ptb->y,ptb->z,ptc->x,ptc->y,ptc->z,
                    ptc->x-ptd->x,ptc->y-ptd->y,ptc->z-ptd->z,
                    ptb->x-pta->x,ptb->y-pta->y,ptb->z-pta->z,
                    yticks, yautorange, ylog, ptb->y, ptc->y, ylabel, yscale,
                    plotdata->plot_pr.ylabelhead, plotdata->plot_pr.ylabeltail);

      /* Draw the x-axis */
      drawPS3D_axis(X_AXIS,
                    ptc->x,ptc->y,ptc->z,ptd->x,ptd->y,ptd->z,
                    ptc->x-ptb->x,ptc->y-ptb->y,ptc->z-ptb->z,
                    ptb->x-pta->x,ptb->y-pta->y,ptb->z-pta->z,
                    xticks, xautorange, xlog, ptc->x, ptd->x, xlabel, xscale,
                    plotdata->plot_pr.xlabelhead, plotdata->plot_pr.xlabeltail);

   } else {
      /* b-c is x-axis, c-d is y-axis */

      /* Draw the x-axis */
      drawPS3D_axis(X_AXIS,
                    ptb->x,ptb->y,ptb->z,ptc->x,ptc->y,ptc->z,
                    ptc->x-ptd->x,ptc->y-ptd->y,ptc->z-ptd->z,
                    ptb->x-pta->x,ptb->y-pta->y,ptb->z-pta->z,
                    xticks, xautorange, xlog, ptb->x, ptc->x, xlabel, xscale,
                    plotdata->plot_pr.xlabelhead, plotdata->plot_pr.xlabeltail);

      /* Draw the y-axis */
      drawPS3D_axis(Y_AXIS,
                    ptc->x,ptc->y,ptc->z,ptd->x,ptd->y,ptd->z,
                    ptc->x-ptb->x,ptc->y-ptb->y,ptc->z-ptb->z,
                    ptb->x-pta->x,ptb->y-pta->y,ptb->z-pta->z,
                    yticks, yautorange, ylog, ptc->y, ptd->y, ylabel, yscale,
                    plotdata->plot_pr.ylabelhead, plotdata->plot_pr.ylabeltail);
   }
}

/* Draw the axis - parallel to a given axis */
static void drawPS3D_axis(axis,
                          xa,ya,za,xb,yb,zb,
                          vx, vy, vz,
                          vbx, vby, vbz,
                          nticks, autorange, plog, min, max, label, scale,
                          labelhead, labeltail)
int    axis;                /* X-axis, Y-axis, Z-axis        */
double xa,ya,za,xb,yb,zb;   /* Start, end points             */
double vx,vy,vz;            /* Vector for drawing tick-marks */
double vbx,vby,vbz;         /* Direction of remaining axis   */
int    nticks;
short  autorange;
short  plog;
double min, max;
char   *label;
double scale;
CNaxislabelptr labelhead, labeltail;
{
   double   xm0, ym0, zm0, xm1, ym1, zm1, x0, y0, x1, y1;
   double   xm2, ym2, zm2, x2, y2;
   double   tmp;
   CNcoord  vtick, vaxis;
   int      parallel=0;

   /*
    * Rearrange min and max
    */
   if (max < min) {
      tmp=xb ; xb =xa ; xa =tmp;
      tmp=yb ; yb =ya ; ya =tmp;
      tmp=zb ; zb =za ; za =tmp;
      tmp=max; max=min; min=tmp;
   }

   /*
    * There are 2 vectors of importance.
    * The first is the vector giving the direction of the axis = vaxis
    * The second is the vector giving the direction of the tick-marks = vtick
    * vaxis can be bidirectional, but the direction of vtick is important.
    *
    *   /\ va
    *     \
    *     /\
    *    vt \
    *        \
    *        /
    */

   /*
    * Find the direction of the tickmarks in PS-coordinates.
    * This means that the vector (vx,vy,vz) must be converted.
    * Don't normalize the vector yet
    */
   xm0 = xa;     ym0 = ya;     zm0 = za;
   xm1 = xa+vx;  ym1 = ya+vy;  zm1 = za+vz;
   trn_world_to_PS(xm0,ym0,zm0,&x0,&y0);
   trn_world_to_PS(xm1,ym1,zm1,&x1,&y1);
   vtick.x = x1-x0;
   vtick.y = y1-y0;
   vtick.z = 0.0;

   /*
    * Find the direction of the axis in PS-coordinates.
    * Again this requires a vector transformation.
    * Don't normalize the vector yet
    */
   xm1 = 0.5*(xa+xb);
   ym1 = 0.5*(ya+yb);
   zm1 = 0.5*(za+zb);
   trn_world_to_PS(xm1,ym1,zm1,&x1,&y1);
   vaxis.x = x1-x0;
   vaxis.y = y1-y0;
   vaxis.z = 0.0;

#ifdef DEBUG
   (void) printf("vtick=(%g %g %g)\n",vtick.x,vtick.y,vtick.z);
   (void) printf("vaxis=(%g %g %g)\n",vaxis.x,vaxis.y,vaxis.z);
#endif
 
   /* Don't draw the axis if the axis points out of the 2D page */
   if (CNvector_lengthsq(&vaxis) < 1.0e-5) return;

   /*
    * Flag if the axis-vector is the same as the tick vector
    * This should only happen when theta=0 or 180, where theta is the view
    * angle from the x-y plane
    */
#ifdef DEBUG
   (void) printf("theta=%g\n",sin(CNvector_angle(&vaxis,&vtick)));
#endif
   parallel = fabs(sin(CNvector_angle(&vaxis,&vtick))) < 1.0e-5;
   if (parallel || (CNvector_lengthsq(&vtick) < 1.0e-5)) {
      xm2 = xa+vbx;  ym2 = ya+vby;  zm2 = za+vbz;
      trn_world_to_PS(xm2,ym2,zm2,&x2,&y2);
      vtick.x = x2-x0;
      vtick.y = y2-y0;
      vtick.z = 0.0;
   }

   /* Normalize the vectors */
   (void) CNnormalize_vector(&vtick);
   (void) CNnormalize_vector(&vaxis);

   /* Put on the label */
   plotPS3D_label(label,x1,y1,&vaxis,&vtick);

   /* Draw tickmarks and tick labels */
   if (labelhead != NULL) {
 
      /* Tickmarks based on user-specification */
      plotPS3D_User_ticks(axis,xa,ya,za,xb,yb,zb,&vaxis,&vtick,
                          labelhead,labeltail);

   } else if (!autorange && !plog) {

      /* Linear, fixed tickmarks */
      plotPS3D_ticks(xa,ya,za,xb,yb,zb,min,max,&vaxis,&vtick,
                     nticks,plog,scale);

   } else {

      /* If xlog then do lots of tiny tickmarks */
      plotPS3D_Auto_ticks(axis,xa,ya,za,xb,yb,zb,min,max,&vaxis,&vtick,
                          autorange,plog,scale);
   }

   /* Draw the main line */
   drawPS3D_line(xa,ya,za,xb,yb,zb,CN_LN_SOLID,2);
}

/*
 * Draw the tickmarks using simple linear interpolation
 * i.e. there are NTICK number of tickmarks.
 * This is NEVER used with log-scale
 */
static void
plotPS3D_ticks(xa,ya,za,xb,yb,zb,min,max,vaxis,vtick,nticks,plog,scale)
double   xa,ya,za,xb,yb,zb;
double   min,max;
CNcoord  *vtick, *vaxis;
int      nticks;
short    plog;
double   scale;
{
   double  xm0, ym0, zm0, x0, y0, x1, y1, vallbl;
   int     i;
   int     explabel, precision;
   PXlabel axislabels[PX_MAX_LABELS];
   int     nlabels=0;

   /* Draw tickmarks */
   (void) PXlinetypPS(CN_LN_SOLID, 1);
   for (i=0; i<=nticks; i++) {
      xm0 = xa + i*(xb-xa)/(double)nticks;
      ym0 = ya + i*(yb-ya)/(double)nticks;
      zm0 = za + i*(zb-za)/(double)nticks;
      trn_world_to_PS(xm0,ym0,zm0,&x0,&y0);
      x1 = x0 + vtick->x*scale*7.0;
      y1 = y0 + vtick->y*scale*7.0;
      PXdrawPS_line(x0,y0,x1,y1,CN_LN_SOLID,1);
      x1 = x0 + vtick->x*scale*12.0;
      y1 = y0 + vtick->y*scale*12.0;
      vallbl = min + i*(max-min)/(double)nticks;

      /* Put on a label */
      if (plog) vallbl = pow(10.0,vallbl);
      vallbl = vallbl/scale;
      PXadd_axislabel(axislabels, &nlabels, PX_MAX_LABELS,
                      vallbl, (double)x1, (double)y1);
   }
 
   /* Now work on the labels */
   if (nlabels > 0) {
      PXfind_axis_precision(axislabels, nlabels, &precision, &explabel);
      for (i=0; i<nlabels; i++) {
         plotPS3D_ticklabel(axislabels[i].value,
                            axislabels[i].x,
                            axislabels[i].y,
                            (char *)NULL,
                            vaxis, vtick, precision, explabel);
      }
   }
}


/*
 * Draw the tickmarks with automatic ranging
 * i.e. there are major and minor tickmarks.
 */
/*ARGSUSED*/
static void plotPS3D_Auto_ticks(axis,xa,ya,za,xb,yb,zb,min,max,vaxis,vtick,
                                autorange,plog,scale)
int      axis;
double   xa,ya,za,xb,yb,zb;
double   min,max;
CNcoord  *vtick, *vaxis;
short    autorange;
short    plog;
double   scale;
{
   void    PXget_autorange();
   double  dmin, dmax, dmin2, dmax2;
   double  dtmp, dt, delta;
   double  xm0, ym0, zm0, x0, y0, x1, y1;
   int     i, itick, nticks, tck;
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
 
   /* Get the rounded intervals */
   PXget_autorange(min,max,
                   &dmin,&dmax,&dmin2,&dmax2,&delta,
                   plog,autorange);

   /* If dmin2=dmin then subtract delta from dmin2 so that dmin2 < dmin */
   /* This is used to get the leftmost tick label on the plot           */
   if (dmin2==dmin) dmin2 -= delta;

   /*
    * Draw in label at beginning of plot boundary
    */

   /* Reset tick length */
   tck = 8;
 
   /* The position of the dmin tickmark on the axis */
   switch (axis) {
   case X_AXIS : xm0=dmin; ym0=ya; zm0=za; break;
   case Y_AXIS : xm0=xa; ym0=dmin; zm0=za; break;
   case Z_AXIS :
   default     : xm0=xa; ym0=ya; zm0=dmin; break;
   }
   trn_world_to_PS(xm0,ym0,zm0,&x0,&y0);
   x1  = x0 + vtick->x*(tck+2);
   y1  = y0 + vtick->y*(tck+2);
 
   /* Put on the first tick */
   vallbl = dmin;
   if (plog) vallbl = pow(10.0,vallbl);
   vallbl = vallbl/scale;
   PXadd_axislabel(axislabels, &nlabels, PX_MAX_LABELS,
                   vallbl, (double)x1, (double)y1);

   /* Draw the tickmarks from dmin2 to dmax2 - dtmp is distance from dmin2 */
   (void) PXlinetypPS(CN_LN_SOLID, 1);
   if (delta <= 0.0) nticks = 0;
   else              nticks = (int)((dmax2 - dmin2)/delta) + 1;
   for (itick=0; itick<nticks; itick++) {
      dtmp = itick*delta;
      for (i=1; i<=10; i++) {
         /* distance from dtmp - actual phy loc is dt+dtmp+dmin2 */
         dt = i*0.1*delta;
         if (plog) dt = log10((double)i);
         if (plog && i==1) continue;  /* Don't do this case */
 
         /* Don't draw if this is outside the plot boundaries */
         if (dt+dtmp+dmin2 < dmin || dt+dtmp+dmin2 > dmax) continue;
 
         /* The length of the tick depends on i */
         if      (i==10) tck = 8;  /* major tick */
         else if (i==5 ) tck = 6;  /* major sub-tick */
         else            tck = 4;  /* minor tick */
 
         /* The position of the tickmark on the axis */
         switch (axis) {
         case X_AXIS : xm0=dt+dtmp+dmin2; ym0=ya; zm0=za; break;
         case Y_AXIS : xm0=xa; ym0=dt+dtmp+dmin2; zm0=za; break;
         case Z_AXIS :
         default     : xm0=xa; ym0=ya; zm0=dt+dtmp+dmin2; break;
         }
         trn_world_to_PS(xm0,ym0,zm0,&x0,&y0);
         x1 = x0 + vtick->x*tck;
         y1 = y0 + vtick->y*tck;
 
         /* Draw the ticks */
         PXdrawPS_line(x0,y0,x1,y1,CN_LN_SOLID,1);
 
         /* Draw labels */
         if (i==10) {
            x1 = x0 + vtick->x*(tck+2);
            y1 = y0 + vtick->y*(tck+2);
 
            vallbl = dt+dtmp+dmin2;
            if (fabs(vallbl/delta) < 1e-10) vallbl=0.0; /* If x~=0 print 0 */
            if (plog) vallbl = pow(10.0,vallbl);
            vallbl = vallbl/scale;
            PXadd_axislabel(axislabels, &nlabels, PX_MAX_LABELS,
                            vallbl, (double)x1, (double)y1);
         }
      }
   }
 
   /*
    * Draw in labels at end of plot boundary
    */

   /* Reset tick length */
   tck = 8;
 
   /* The position of the dmin tickmark on the axis */
   switch (axis) {
   case X_AXIS : xm0=dmax; ym0=ya; zm0=za; break;
   case Y_AXIS : xm0=xa; ym0=dmax; zm0=za; break;
   case Z_AXIS :
   default     : xm0=xa; ym0=ya; zm0=dmax; break;
   }
   trn_world_to_PS(xm0,ym0,zm0,&x0,&y0);
   x1  = x0 + vtick->x*(tck+2);
   y1  = y0 + vtick->y*(tck+2);

   /* Put on the last tick */
   vallbl = dmax;
   if (plog) vallbl = pow(10.0,vallbl);
   vallbl = vallbl/scale;
   PXadd_axislabel(axislabels, &nlabels, PX_MAX_LABELS,
                   vallbl, (double)x1, (double)y1);
 
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
               else if (ticklabel_overlap(axislabels[i],axislabels[i+1],
                        precision, explabel))
                  doplot = CN_FALSE;
            }
         } else if (i==nlabels-1) {
            /* Don't plot the 1st label if it is too close to the second */
            if (nlabels >= 2) {
               if (EQUAL(axislabels[i].value, axislabels[i-1].value))
                  doplot = CN_FALSE;
               else if (ticklabel_overlap(axislabels[i],axislabels[i-1],
                        precision, explabel))
                  doplot = CN_FALSE;
            }
         }
         if (doplot)
         plotPS3D_ticklabel(axislabels[i].value,
                            axislabels[i].x,
                            axislabels[i].y,
                            (char *)NULL,
                            vaxis, vtick, precision, explabel);
      }
   }
}


/*
 * Draw the tickmarks and labels using user-specified labels
 * This is NEVER used with log-scale
 */
/*ARGSUSED*/
static void plotPS3D_User_ticks(axis,
                                xa,ya,za,xb,yb,zb,
                                vaxis,vtick,
                                labelhead, labeltail)
int      axis;
double   xa,ya,za,xb,yb,zb;
CNcoord  *vtick, *vaxis;
CNaxislabelptr labelhead, labeltail;
{
   CNaxislabelptr Aptr;
   double  xm0, ym0, zm0, x0, y0, x1, y1;
   char    label[CN_MAXCHAR];
   int     outside;
 
   /* Draw tickmarks */
   (void) PXlinetypPS(CN_LN_SOLID, 1);
   for (Aptr=labelhead; Aptr!=NULL; Aptr=Aptr->next) {
 
      /* Get the axis coordinates and check if it is out of bounds */
      xm0     = xa;
      ym0     = ya;
      zm0     = za;
      outside = CN_FALSE;
      switch (axis) {
      case X_AXIS : xm0 = Aptr->pos;
                    if ((xm0 > xa && xm0 > xb) || (xm0 < xa && xm0 < xb))
                       outside = CN_TRUE;
                    break;
      case Y_AXIS : ym0 = Aptr->pos;
                    if ((ym0 > ya && ym0 > yb) || (ym0 < ya && ym0 < yb))
                       outside = CN_TRUE;
                    break;
      case Z_AXIS :
      default     : zm0 = Aptr->pos;
                    if ((zm0 > za && zm0 > zb) || (zm0 < za && zm0 < zb))
                       outside = CN_TRUE;
                    break;
      }
 
      /* The tick is out of bounds */
      if (outside) continue;

      /* Translate to PS coordinates */
      trn_world_to_PS(xm0,ym0,zm0,&x0,&y0);
      x1 = x0 + vtick->x*7.0;
      y1 = y0 + vtick->y*7.0;
 
      /* Draw the tick */
      PXdrawPS_line(x0,y0,x1,y1,CN_LN_SOLID,1);
 
      /* Put on a label */
      x1 = x0 + vtick->x*10.0;
      y1 = y0 + vtick->y*10.0;
      (void) strcpy(label, Aptr->name);
      plotPS3D_ticklabel(0.0, x1, y1, label,
                         vaxis, vtick, 4, CN_FALSE);
   }
}


/*
 * Convenient PS drawing functions
 */

/* Draw a line of given thickness in 3D */
static void drawPS3D_line(x1, y1, z1, x2, y2, z2, linetyp, ithk)
double x1, y1, z1, x2, y2, z2;
int    linetyp, ithk;
{
   double  xa, ya, xb, yb;

   trn_world_to_PS(x1,y1,z1,&xa,&ya);
   trn_world_to_PS(x2,y2,z2,&xb,&yb);

   /* draw the line */
   PXdrawPS_line(xa,ya,xb,yb,linetyp,ithk);
}

/* Draw a filled polygon */
static void paintPS3D_poly4(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4)
double x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4;
{
   PSPoint points[5];
   double x,y;

   /* Translate the points into X-coordinates */
   trn_world_to_PS(x1,y1,z1,&x,&y);
   points[0].x = x;
   points[0].y = y;
   points[4].x = x;
   points[4].y = y;

   trn_world_to_PS(x2,y2,z2,&x,&y);
   points[1].x = x;
   points[1].y = y;

   trn_world_to_PS(x3,y3,z3,&x,&y);
   points[2].x = x;
   points[2].y = y;

   trn_world_to_PS(x4,y4,z4,&x,&y);
   points[3].x = x;
   points[3].y = y;

   /* Now draw the polygon */
   PXfillPS_polygon(points,5,1,0,CN_LN_NONE,0,1);
}


/* plot the tick label on an axis at the given x-y coordinates */
/*ARGSUSED*/
static void plotPS3D_ticklabel(vallbl,xpos,ypos,label,
                               vaxis,vtick,precision,explabel)
double   vallbl;
double   xpos,ypos;
char     *label;
CNcoord  *vaxis;    /* Axis vector (normalized)     */
CNcoord  *vtick;    /* Tickmark vector (normalized) */
int      precision, explabel;
{
#define LH 1
#define RH 2
#define CH 3
#define UV 4
#define BV 5
#define MV 6
   char   text[MAXCHR];
   int    vert=MV, horz=CH;
   char   exponent[MAXCHR];
   char   nonexponent[MAXCHR];
   int    isexp=CN_FALSE;

   /*
    * |vtick->x| > |vtick->y| , vtick->x>0  left justified, vertical_centered
    * |vtick->x| > |vtick->y| , vtick->x<0  right justified, vertical_centered
    * |vtick->x| < |vtick->y| , vtick->y>0  center justified, top_justified
    * |vtick->x| < |vtick->y| , vtick->y<0  center justified, bottom_justified
    */

   /* Set the font */
   (void) fprintf(ips,"setMkrsLblFont\n");

   if (explabel)
      (void) sprintf(text,"%.*e",precision,vallbl);
   else
      (void) sprintf(text,"%.*g",precision,vallbl);
   if (label != NULL) (void)strcpy(text,label);
 
   /* Check for exponents */
   if (label == NULL)
   PXmodify_explabel(text, exponent, nonexponent, &isexp, CN_TRUE);

   /* Figure out where to place the label */
   if ((fabs(vtick->x)>fabs(vtick->y)) || (fabs(vaxis->y)>10*fabs(vaxis->x))){
      /* center-vertical justified */
      if (vtick->x > 0.0) horz = LH;     /* Left   horizontal justified */
      else                horz = RH;     /* Right  horizontal justified */
      vert = MV;                         /* Center vertical   justified */
   } else {
      horz = CH;                         /* Center horizontal justified */
      if (vtick->y > 0.0) vert = UV;     /* Top    vertical   justified */
      else                vert = BV;     /* Bottom vertical   justified */
   }

   /* Put down the label */
   if (isexp) {
      (void) fprintf(ips,"%.2f %.2f moveto (%s) ",xpos,ypos,text);
      switch (vert) {
      case UV : (void) fprintf(ips,"UPJ  "); break;
      case BV : (void) fprintf(ips,"BOJA "); break;
      case MV : (void) fprintf(ips,"MIJA "); break;
      default : (void) fprintf(ips,"MIJA "); break;
      }
      switch (horz) {
      case LH : (void) fprintf(ips,"LEJ "); break;
      case RH : (void) fprintf(ips,"RIJ "); break;
      case CH : (void) fprintf(ips,"CEJ "); break;
      default : (void) fprintf(ips,"CEJ "); break;
      }
      (void) fprintf(ips,"pop (%s) show (%s) exponentshow\n",
                     nonexponent,exponent);
   } else {
 
      (void) fprintf(ips,"%.2f %.2f moveto (%s) ",xpos,ypos,text);
      switch (vert) {
      case UV : (void) fprintf(ips,"UPJ  "); break;
      case BV : (void) fprintf(ips,"BOJA "); break;
      case MV : (void) fprintf(ips,"MIJA "); break;
      default : (void) fprintf(ips,"MIJA "); break;
      }
      switch (horz) {
      case LH : (void) fprintf(ips,"LEJ "); break;
      case RH : (void) fprintf(ips,"RIJ "); break;
      case CH : (void) fprintf(ips,"CEJ "); break;
      default : (void) fprintf(ips,"CEJ "); break;
      }
      (void) fprintf(ips,"show\n");

   }
}


/* check overlap of 2 labels */
static int ticklabel_overlap(label1, label2, precision, explabel)
PXlabel label1, label2;
int     precision, explabel;
{
   char   text1[MAXCHR];
   char   text2[MAXCHR];
   char   exponent[MAXCHR];
   char   nonexponent[MAXCHR];
   int    isexp = CN_FALSE;
   int    overlap = CN_FALSE;
   int    text1_width, text2_width, text_width, text_spacing;
   int    tick_xdist, tick_ydist;
 
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
   text1_width  = PS_AXISFONT*fscale*0.66*strlen(text1);
   text2_width  = PS_AXISFONT*fscale*0.66*strlen(text2);
   text_spacing = 1.0*fscale;
   text_width   = 0.5*(text1_width + text2_width) + text_spacing;
 
   /* Calculate spacing between ticks */
   tick_xdist = fabs(label1.x - label2.x);
   tick_ydist = fabs(label1.y - label2.y);

   if ((tick_ydist < 15.0*scale) && (tick_xdist < text_width))
      overlap = CN_TRUE;
   else
      overlap = CN_FALSE;
 
   /* Return */
   return(overlap);
}

/* plot a label on an axis at the given x-y coordinates */
/*ARGSUSED*/
static void plotPS3D_label(text,x,y,vaxis,vtick)
char   *text;
double x,y;
CNcoord  *vaxis;    /* Axis vector (normalized)     */
CNcoord  *vtick;    /* Tickmark vector (normalized) */
{
   double angle, vs, inc;

   /*
    * |vtick->x| > |vtick->y| , vtick->x>0  left justified, vertical_centered
    * |vtick->x| > |vtick->y| , vtick->x<0  right justified, vertical_centered
    * |vtick->x| < |vtick->y| , vtick->y>0  center justified, top_justified
    * |vtick->x| < |vtick->y| , vtick->y<0  center justified, bottom_justified
    */

   /* Get the rotation angle of the text label */
   if (fabs(vaxis->x) < CN_SMALL) {
      angle = 90.0;
      if (fabs(vaxis->y) < CN_SMALL) angle = 0.0;
   } else {
      vs = sqrt (vaxis->x*vaxis->x + vaxis->y*vaxis->y);
      if (vs < CN_SMALL) {
         angle = 0.0;
      } else {
         angle = acos(vaxis->x / vs );
         if (vaxis->y < 0.0) angle = -1.0*angle;
         angle = angle * 180.0 / 3.14158;
      } 
   }

   /* Only allow rotations between -90 to 90 degrees */
   while (angle < -90.0) angle += 180.0;
   while (angle >  90.0) angle -= 180.0;

   /* Now figure out where the label is to be put */
   if (fabs(vaxis->x) < CN_SMALL && fabs(vaxis->y) > CN_SMALL) {
      /* Vertical axis */
      inc = (vtick->x > 0.0) ? 1.0 : -1.0;
      x += inc*scale*70.0;
      y += 0.0;                 
   } else if (fabs(vaxis->y) > 10*fabs(vaxis->x)) {
      x += vtick->x*scale*70.0;
      y += vtick->y*scale*70.0;
   } else if (fabs(vaxis->y) > 5*fabs(vaxis->x)) {
      x += vtick->x*scale*67.0;
      y += vtick->y*scale*67.0;
   } else if (fabs(vaxis->y) > 2*fabs(vaxis->x)) {
      x += vtick->x*scale*64.0;
      y += vtick->y*scale*64.0;
   } else if (fabs(vaxis->y) > 0.5*fabs(vaxis->x)) {
      x += vtick->x*scale*61.0;
      y += vtick->y*scale*61.0;
   } else if (fabs(vaxis->y) > 0.2*fabs(vaxis->x)) {
      x += vtick->x*scale*58.0;
      y += vtick->y*scale*58.0;
   } else if (fabs(vaxis->y) > 0.1*fabs(vaxis->x)) {
      x += vtick->x*scale*55.0;
      y += vtick->y*scale*55.0;
   } else {
      x += vtick->x*scale*50.0;
      y += vtick->y*scale*50.0;
   }

   (void) fprintf(ips,"setSideLblFont\n");
   (void) fprintf(ips,"/Ylabel {");
   (void) fprintf(ips,"0 0 moveto ");
   (void) fprintf(ips,"(%s) MIJS CEJ show } def\n",text);
   (void) fprintf(ips,"%.2f %.2f translate\n",x,y);
   (void) fprintf(ips,"%.2f rotate\n  Ylabel\n%.2f rotate\n",angle,-angle);
   (void) fprintf(ips,"%.2f %.2f translate\n",-x,-y);
}


/*
 * PLOT DATA
 */

/* draw the plot */
static void plotPS3D()
{
   CNdslistptr  DS, ds;
   CNdatasetptr Dptr;
   int          colrinc=0, lineinc=0;
   int          contfill, meshplot;
   int          PARENT_FOUND;

   (void) fprintf(ips,"%%------------------------------------------------\n");
   (void) fprintf(ips,"%%--------------Draw the Plot---------------------\n");
   (void) fprintf(ips,"%%------------------------------------------------\n");

   /*
    * Plot set - draw the grid if that exists
    */
   for (DS=plotdata->datahead; DS!=NULL; DS=DS->next) {
      if (DS->Dptr->grid && DS->Dptr->datatype == CN_GRID4D)
         plotPS3D_grid(DS->Dptr->grid, 
                       DS->Dptr->data_pr.contintrp, 
                       DS->Dptr->data_pr.contclip,
                       DS->Dptr->data_pr.meshplot);
   }

   /*
    * Plot set - draw the mesh4D grid and related quants if that exists
    */
   for (DS=plotdata->datahead; DS!=NULL; DS=DS->next) {
      if (DS->Dptr->mesh4D)
         plotPS3D_mesh4D(DS->Dptr);
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
   for (DS=plotdata->datahead; DS!=NULL; DS=DS->next) {
 
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
         plotPS3D_boundary(DS->Dptr->parent,
                          (int)Dptr->data_pr.boundary,
                          (int)Dptr->data_pr.regbound,
                          (int)Dptr->data_pr.fillbnd,
                          (int)Dptr->data_pr.pr_rgID,
                          1,0);
      } else {
         plotPS3D_boundary(DS->Dptr,
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
   for (DS=plotdata->datahead; DS!=NULL; DS=DS->next) {

      /* Draw colored triangles but not the curves */
      contfill = ( (DS->Dptr->data_pr.contstyle == CN_FILLCONT) ||
                   (DS->Dptr->data_pr.contstyle == CN_LNFILLCONT) );
      meshplot = DS->Dptr->data_pr.contstyle == CN_PLOTMESH;
      meshplot = meshplot | DS->Dptr->data_pr.meshplot;
      if (contfill || meshplot) {
         if (DS->Dptr->elemhead != NULL) {
            plotPS3D_elems(DS->Dptr, contfill, meshplot);
         } else {
            plotPS3D_trias(DS->Dptr, contfill, meshplot);
            plotPS3D_rects(DS->Dptr, contfill, meshplot);
         }
         plotPS3D_polys(DS->Dptr, contfill, meshplot);
      }
   }

   /*
    * Now draw the contour and mesh datasets
    */
   colrinc = 0;
   lineinc = 0;
   for (DS=plotdata->datahead; DS!=NULL; DS=DS->next) {

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
            plotPS3D_dataset_curves(DS->Dptr,colrinc,lineinc);
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
   for (DS=plotdata->datahead; DS!=NULL; DS=DS->next) {

      /* Don't draw contours */
      if (DS->Dptr->datatype == CN_CONTOUR) continue;

      /* Draw the curve set */
      plotPS3D_dataset_curves(DS->Dptr,colrinc,lineinc);

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
   for (DS=plotdata->datahead; DS!=NULL; DS=DS->next) {
      if (DS->Dptr->vecbox) 
         plotPS3D_vectors(DS->Dptr->vecbox,
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
         if (DS->Dptr->parent != NULL)
         plotPS3D_pointIDs(DS->Dptr->parent->pointhead, 
                           DS->Dptr->parent->pointtail, CN_FALSE);
         else
         plotPS3D_pointIDs(Dptr->pointhead, Dptr->pointtail, CN_FALSE);
      }
      if (DS->Dptr->data_pr.pr_ndID)
         plotPS3D_nodeIDs (Dptr->nodehead,  Dptr->nodetail);
      if (DS->Dptr->data_pr.pr_trID)
         plotPS3D_triaIDs (Dptr->triahead,  Dptr->triatail);
      if (DS->Dptr->data_pr.pr_rtID)
         plotPS3D_rectIDs (Dptr->recthead,  Dptr->recttail);
   }
}


/*
 * Plot the grid in PS 
 */
static void plotPS3D_grid(grid, contintrp, contclip, meshplot)
CNgrid4Dptr grid;
short       contintrp;
short       contclip;
short       meshplot;
{
   void PXidentify_view_planes();
   CNsliceptr slice=NULL;
   CNrectptr  R;
   CNtriaptr  T;
   int        verbose=0;
   double     cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax;
   int        xminin, xmaxin, yminin, ymaxin, zminin, zmaxin;
   short      logx=0, logy=0, logz=0, logt=0;
   int        clipz=1;
   double     tmp;
   int        linetype, linecolor, filltype, fillcolor;
 
   /* Initialize colors and line/fill types */
   filltype  = CN_FILL_NONE;
   linetype  = CN_LN_SOLID;
   fillcolor = PXpolyColorIndexPS(1);   /* yellow */
   linecolor = PXpolyColorIndexPS(4);   /* red    */

   /*
    * Identify the inner and outer planes in the current view
    */
   PXidentify_view_planes(grid->xmin, grid->xmax,
                          grid->ymin, grid->ymax,
                          grid->zmin, grid->zmax,
                          &xminin, &xmaxin, &yminin, &ymaxin, &zminin, &zmaxin,
                          view_transfo);

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
    * X slice
    */
   if (xminin) {
      /* Draw the surface at x=xmax */
      tmp = grid->xmax;
      if (tmp > cxmax) tmp = cxmax;
   } else {
      /* Draw the surface at x=xmin */
      tmp = grid->xmin;
      if (tmp < cxmin) tmp = cxmin;
   }
   if ((slice = CNslice_grid4D_x(grid,tmp,(int)contintrp,verbose)) != NULL) {

      /* Loop through the rectangles */
      for (R=slice->recthead; R!=NULL; R=R->next) {
         if (!CNrect_in_bounds(R,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax)) continue;
 
         /* Plot the color fills */
         plotPS3D_single_fill_rect(R,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax,
              logx, logy, logz, logt, contclip, 1);

         /* Plot the mesh */
         if (meshplot)
         plotPS3D_single_solid_rect(R,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax,
              logx, logy, logz, logt, clipz,
              filltype, fillcolor, linetype, linecolor, 1);
      }

      /* Loop through the triangles */
      for (T=slice->triahead; T!=NULL; T=T->next) {
         /* Quick check to see if the triangle is in-bounds */
         if (!CNtria_in_bounds(T,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax)) continue;
 
         /* Plot the color fills */
         plotPS3D_single_fill_tria(T,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax,
              logx, logy, logz, logt, contclip, 1);

         /* Plot the mesh */
         if (meshplot)
         plotPS3D_single_solid_tria(T,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax,
              logx, logy, logz, logt, clipz,
              filltype, fillcolor, linetype, linecolor, 1);
      }

      /* Delete the slice */
      CNdelete_slice(slice);
   }

   /*
    * Y slice
    */
   if (yminin) {
      /* Draw the surface at y=ymax */
      tmp = grid->ymax;
      if (tmp > cymax) tmp = cymax;
   } else {
      /* Draw the surface at y=ymin */
      tmp = grid->ymin;
      if (tmp < cymin) tmp = cymin;
   }
   if ((slice = CNslice_grid4D_y(grid,tmp,(int)contintrp,verbose)) != NULL) {

      /* Loop through the rectangles */
      for (R=slice->recthead; R!=NULL; R=R->next) {
         if (!CNrect_in_bounds(R,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax)) continue;
 
         /* Plot the color fills */
         plotPS3D_single_fill_rect(R,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax,
              logx, logy, logz, logt, contclip, 1);

         /* Plot the mesh */
         if (meshplot)
         plotPS3D_single_solid_rect(R,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax,
              logx, logy, logz, logt, clipz,
              filltype, fillcolor, linetype, linecolor, 1);
      }

      /* Loop through the triangles */
      for (T=slice->triahead; T!=NULL; T=T->next) {
         /* Quick check to see if the triangle is in-bounds */
         if (!CNtria_in_bounds(T,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax)) continue;
 
         /* Plot the color fills */
         plotPS3D_single_fill_tria(T,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax,
              logx, logy, logz, logt, contclip, 1);

         /* Plot the mesh */
         if (meshplot)
         plotPS3D_single_solid_tria(T,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax,
              logx, logy, logz, logt, clipz,
              filltype, fillcolor, linetype, linecolor, 1);
      }

      /* Delete the slice */
      CNdelete_slice(slice);
   }

   /*
    * Z slice
    */
   if (zminin) {
      /* Draw the surface at z=zmax */
      tmp = grid->zmax;
      if (tmp > czmax) tmp = czmax;
   } else {
      /* Draw the surface at z=zmin */
      tmp = grid->zmin;
      if (tmp < czmin) tmp = czmin;
   }
   if ((slice = CNslice_grid4D_z(grid,tmp,(int)contintrp,verbose)) != NULL) {

      /* Loop through the rectangles */
      for (R=slice->recthead; R!=NULL; R=R->next) {
         /* Quick check to see if the rectangle is in-bounds */
         if (!CNrect_in_bounds(R,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax)) continue;
 
         /* Plot the color fills */
         plotPS3D_single_fill_rect(R,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax,
              logx, logy, logz, logt, contclip, 1);

         /* Plot the mesh */
         if (meshplot)
         plotPS3D_single_solid_rect(R,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax,
              logx, logy, logz, logt, clipz,
              filltype, fillcolor, linetype, linecolor, 1);
      }

      /* Loop through the triangles */
      for (T=slice->triahead; T!=NULL; T=T->next) {
         /* Quick check to see if the triangle is in-bounds */
         if (!CNtria_in_bounds(T,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax)) continue;
 
         /* Plot the color fills */
         plotPS3D_single_fill_tria(T,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax,
              logx, logy, logz, logt, contclip, 1);

         /* Plot the mesh */
         if (meshplot)
         plotPS3D_single_solid_tria(T,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax,
              logx, logy, logz, logt, clipz,
              filltype, fillcolor, linetype, linecolor, 1);
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
static void plotPS3D_mesh4D(dptr)
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
   int         dosort=1;
 
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
 
   /*
    * Identify the inner and outer planes in the current view
    */
   PXidentify_view_planes(mesh4D_grid->xmin, mesh4D_grid->xmax,
                          mesh4D_grid->ymin, mesh4D_grid->ymax,
                          mesh4D_grid->zmin, mesh4D_grid->zmax,
                          &xminin, &xmaxin, &yminin, &ymaxin, &zminin, &zmaxin,
                          view_transfo);

   /* clipping boundaries */
   cxmin = ((xlog) ? pow(10.0,xmin) : xmin);
   cxmax = ((xlog) ? pow(10.0,xmax) : xmax);
   cymin = ((ylog) ? pow(10.0,ymin) : ymin);
   cymax = ((ylog) ? pow(10.0,ymax) : ymax);
   czmin = ((zlog) ? pow(10.0,zmin) : zmin);
   czmax = ((zlog) ? pow(10.0,zmax) : zmax);
   ctmin = -CN_LARGE;
   ctmax =  CN_LARGE;
 
   /* Find the rectangles/triangles that are exposed */
   CNslice_mesh4D(mesh4D_grid, mesh4D_quant,
                  &blockhead, &blocktail, &cubehead,  &cubetail,
                  cxmin,cxmax,cymin,cymax,czmin,czmax,
                  xminin,xmaxin,yminin,ymaxin,zminin,zmaxin,verbose);
   if (cubehead == NULL) return;
 
   /* Sort the blocks */
   if (dosort) {
      if (quick_sort)
         CNdo_quick_sort_blocks(&blockhead, &blocktail,
                               view_transfo, xlog, ylog, zlog);
      else
         CNbubble_sort_blocks(  &blockhead, &blocktail,
                               view_transfo, xlog, ylog, zlog);
   }
 
   /* Loop through the blocks */
   for (B=blockhead; B!=NULL; B=B->next) {
      if (B->cube == NULL) continue;
 
      /* Plot a cube */
      plotPS3D_mesh4D_cube(B,
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
static void plotPS3D_mesh4D_cube(B,
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
   int         clipz=1;
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
 
   /* Sort the polygons */
   if (dosort) {
      if (quick_sort)
         CNdo_quick_sort_polys(&polyhead, &polytail,
                               view_transfo, xlog, ylog, zlog);
      else
         CNbubble_sort_polys(  &polyhead, &polytail,
                               view_transfo, xlog, ylog, zlog);
   }
 
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
         plotPS3D_single_fill_poly(P,
                      cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax,
                      contclip, 1);
 
      /* Plot the mesh (must do this for grid) */
      if ((meshplot) || (mesh4D_quant && nocont)) {
         filltype  = CN_FILL_SOLID;
         if (mesh4D_quant && !nocont) filltype = CN_FILL_NONE;
         fillcolor = PXpolyColorIndexPS(color);
         linetype  = (meshplot) ? CN_LN_SOLID : CN_LN_NONE;
         linecolor = PXpolyColorIndexPS(4);   /* red    */
         plotPS3D_single_solid_poly(P,
                      cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax,
                      clipz,
                      filltype, fillcolor, linetype, linecolor, 1);
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
static void plotPS3D_vectors(Vbox, vlog, vscale, vlogscale, vhead, vtail)
CNvecboxptr  Vbox;
int          vlog;
double       vscale, vlogscale;
int          vhead, vtail;
{
   CNvecptr    Vptr;
   double      px, py, pz;
   double      vx, vy, vz;
   double      x1, y1, x2, y2;
   double      vmin;
   double      cxmin, cxmax, cymin, cymax, czmin, czmax;
   int         p1_clipped=CN_FALSE, p2_clipped=CN_FALSE;
 
   if (Vbox == NULL) return;
 
   if (vlog) {
      vmin = 0.1*Vbox->vlen_min;
      if (vmin == 0.0) vmin = 1.0;
   }

   /* Clipping boundary */
   cxmin = ((xlog) ? pow(10.0,xmin) : xmin);
   cxmax = ((xlog) ? pow(10.0,xmax) : xmax);
   cymin = ((ylog) ? pow(10.0,ymin) : ymin);
   cymax = ((ylog) ? pow(10.0,ymax) : ymax);
   czmin = ((zlog) ? pow(10.0,zmin) : zmin);
   czmax = ((zlog) ? pow(10.0,zmax) : zmax);

   /* Go thru the vectors */
   for (Vptr=Vbox->vectorhead; Vptr!=NULL; Vptr=Vptr->next) {
 
      /* Check plot flag */
      if (Vptr->noplot) continue;

      if ( (Vptr->x < cxmin || Vptr->x > cxmax) ||
           (Vptr->y < cymin || Vptr->y > cymax) ||
           (Vptr->z < czmin || Vptr->z > czmax) ) continue;

      /* Scale the vector starting point */
      px = (xlog) ? CNlog10(Vptr->x) : Vptr->x;
      py = (ylog) ? CNlog10(Vptr->y) : Vptr->y;
      pz = (zlog) ? CNlog10(Vptr->z) : Vptr->z;
      trn_world_to_PS(px,py,pz,&x1,&y1);

      /* Scale to the dimensions of the plot specified in the Vbox */
      if (vlog) {
         vx   = CNveclog10(Vptr->vx/vmin) * vlogscale;
         vy   = CNveclog10(Vptr->vy/vmin) * vlogscale;
         vz   = CNveclog10(Vptr->vz/vmin) * vlogscale;
      } else {
         vx   = Vptr->vx * vscale;
         vy   = Vptr->vy * vscale;
         vz   = Vptr->vz * vscale;
      }

      /* Scale the vector ending point */
      px = (xlog) ? CNlog10(Vptr->x + vx) : Vptr->x + vx;
      py = (ylog) ? CNlog10(Vptr->y + vy) : Vptr->y + vy;
      pz = (zlog) ? CNlog10(Vptr->z + vz) : Vptr->z + vz;
      trn_world_to_PS(px,py,pz,&x2,&y2);
 
      /* Draw the arrow */
      plotPS3D_arrow(x1,y1,x2,y2,
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
 * Plot the boundary in Postscript
 */
/*ARGSUSED*/
static void plotPS3D_boundary(Dptr, boundary, reg_boundary,
                              fillbnd, pr_rgID, fill, drawlabel)
CNdatasetptr Dptr;
int          boundary, reg_boundary, fillbnd, pr_rgID;
int          fill, drawlabel;
{
   CNregionptr R;
   CNpolyptr   P;
   CNnodeptr   node_head=NULL, node_tail=NULL, N;
   double      cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax;

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
   ctmin = - CN_LARGE;
   ctmax =   CN_LARGE;

   /* now print out the boundary segments in each region */
   for (R=Dptr->regionhead; R!=NULL; R=R->next) {

      /* Draw the material boundary only (do this only if "boundary" is set */
      for (P=R->matpolyhead; P!=NULL && boundary; P=P->next) {

         /* Clip the polygon */
         CNclip_poly(P, &node_head, &node_tail,
               cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax,
               1, 1, 0, 0, 0);
 
         /* Adjust the z and t values of the nodes */
         for (N=node_head; N!=NULL; N=N->next) {
            N->coord->z = ((zlog) ? pow(10.0,zmin) : zmin);
            N->t        = ((zlog) ? pow(10.0,zmin) : zmin);
         }
 
         /* Plot the nodes */
         plotPS3D_nodes(node_head, node_tail,
                        CN_FILL_NONE, 
                        PXpolyColorIndexPS(R->color),
                        boundary ? CN_LN_SOLID : CN_LN_NONE, 0, 4, 0);
 
         /* Delete the node-list */
         CNremove_node_list(&node_head, &node_tail);
      }

      /* Now draw the region boundary */
      for (P=R->polyhead; P!=NULL; P=P->next) {

         /* Clip the polygon */
         CNclip_poly(P, &node_head, &node_tail,
               cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax,
               1, 1, 0, 0, 0);

         /* Adjust the z and t values of the nodes */
         for (N=node_head; N!=NULL; N=N->next) {
            N->coord->z = ((zlog) ? pow(10.0,zmin) : zmin);
            N->t        = ((zlog) ? pow(10.0,zmin) : zmin);
         }

         plotPS3D_nodes(node_head, node_tail,
                        fill ? CN_FILL_SOLID : CN_FILL_NONE, 
                        PXpolyColorIndexPS(R->color),
                        reg_boundary ? CN_LN_SOLID : CN_LN_NONE, 0, 4, 0);

         /* Delete the node-list */
         CNremove_node_list(&node_head, &node_tail);

      }
   }
}


/*
 * Plot the element mesh in POSTSCRIPT
 * An element contains either a triangle or a rectangle.
 * Do this because need to sort both triangles and rectangles in a big list
 */
static void plotPS3D_elems(Dptr,contfill,meshplot)
CNdatasetptr Dptr;
int          contfill;   /* Draw multi-colored triangles */
int          meshplot;   /* Draw only the mesh           */
{
   CNelemptr   E;
   CNtriaptr   T;
   CNrectptr   R;
   double      cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax;
   int         hdnline;
   int         clipz=1;
   int         linetype, linecolor, filltype, fillcolor;

   if (Dptr->elemhead == NULL) return;

   /* Initialize colors and line/fill types */
   hdnline = hiddenline || Dptr->view_pr->hiddenline;
   if (contfill) hdnline = CN_FALSE;
   filltype  = (hdnline) ? CN_FILL_SOLID : CN_FILL_NONE;
   linetype  = CN_LN_SOLID;
   fillcolor = PXpolyColorIndexPS(1);   /* yellow */
   linecolor = PXpolyColorIndexPS(4);   /* red    */

   /* Sort these elements */
   if (contfill || hiddenline || Dptr->view_pr->hiddenline) {
      /* sort the mesh */
      if (quick_sort)
         CNdo_quick_sort_elems(&(Dptr->elemhead),
                               &(Dptr->elemtail),
                               view_transfo, xlog, ylog, zlog);
      else
         CNbubble_sort_elems(  &(Dptr->elemhead),
                               &(Dptr->elemtail),
                               view_transfo, xlog, ylog, zlog);
   }

   /*
    * Clipping Boundary
    */
   cxmin = ((xlog) ? pow(10.0,xmin) : xmin);
   cxmax = ((xlog) ? pow(10.0,xmax) : xmax);
   cymin = ((ylog) ? pow(10.0,ymin) : ymin);
   cymax = ((ylog) ? pow(10.0,ymax) : ymax);
   czmin = - CN_LARGE;
   czmax =   CN_LARGE;
   ctmin = ((zlog) ? pow(10.0,zmin) : zmin);
   ctmax = ((zlog) ? pow(10.0,zmax) : zmax);

   /* Loop through the elements */
   for (E=Dptr->elemhead; E!=NULL; E=E->next) {

      /* Branch depending on if the element is a triangle or a rectangle */
      if ((E->type == CN_TRIA_STR) && (E->tria != NULL)) {
 
         /* It's a triangle! */
         T = E->tria;
 
         /* Quick check to see if the triangle is in-bounds */
         if (!CNtria_in_bounds(T,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax)) continue;
 
         /* Draw a fill triangle with gradated colors */
         if (contfill) {
             plotPS3D_single_fill_tria(T,
                                      cxmin, cxmax, cymin, cymax,
                                      czmin, czmax, ctmin, ctmax,
                                      Dptr->data_pr.logx,
                                      Dptr->data_pr.logy,
                                      0,
                                      Dptr->data_pr.logz,
                                      Dptr->data_pr.contclip, 0);
         }
 
         /* Draw a mesh triangle */
         if (meshplot) {
             plotPS3D_single_solid_tria(T,
                                      cxmin, cxmax, cymin, cymax,
                                      czmin, czmax, ctmin, ctmax,
                                      Dptr->data_pr.logx,
                                      Dptr->data_pr.logy,
                                      0,
                                      Dptr->data_pr.logz,
                                      clipz,
                                      filltype, fillcolor, linetype, linecolor,
                                      0);
         }
      } else if ((E->type == CN_RECT_STR) && (E->rect != NULL)) {
 
         /* It's a rectangle! */
         R = E->rect;
 
         /* Quick check to see if the rectangle is in-bounds */
         if (!CNrect_in_bounds(R,
              cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax)) continue;
 
         /* Draw a fill rectangle with gradated colors */
         if (contfill) {
             plotPS3D_single_fill_rect(R,
                                      cxmin, cxmax, cymin, cymax,
                                      czmin, czmax, ctmin, ctmax,
                                      Dptr->data_pr.logx,
                                      Dptr->data_pr.logy,
                                      0,
                                      Dptr->data_pr.logz,
                                      Dptr->data_pr.contclip, 0);
         }
 
         /* Draw a mesh rectangle */
         if (meshplot) {
             plotPS3D_single_solid_rect(R,
                                      cxmin, cxmax, cymin, cymax,
                                      czmin, czmax, ctmin, ctmax,
                                      Dptr->data_pr.logx,
                                      Dptr->data_pr.logy,
                                      0,
                                      Dptr->data_pr.logz,
                                      clipz,
                                      filltype, fillcolor, linetype, linecolor,
                                      0);
         }
      }
   }
 
   /* Reset color */
   PXsetColorPS(0);
}


/*
 * Plot the triangular mesh in POSTSCRIPT
 */
static void plotPS3D_trias(Dptr,contfill,meshplot)
CNdatasetptr Dptr;
int          contfill;   /* Draw multi-colored triangles */
int          meshplot;   /* Draw only the mesh           */
{
   CNtriaptr   T;
   double      cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax;
   int         hdnline;
   int         clipz=1;
   int         linetype, linecolor, filltype, fillcolor;

   if (Dptr->triahead == NULL) return;

   /* Initialize colors and line/fill types */
   hdnline = hiddenline || Dptr->view_pr->hiddenline;
   if (contfill) hdnline = CN_FALSE;
   filltype  = (hdnline) ? CN_FILL_SOLID : CN_FILL_NONE;
   linetype  = CN_LN_SOLID;
   fillcolor = PXpolyColorIndexPS(1);   /* yellow */
   linecolor = PXpolyColorIndexPS(4);   /* red    */

   /* Sort these triangles */
   if (contfill || hiddenline || Dptr->view_pr->hiddenline) {
      /* sort the mesh */
      if (quick_sort)
         CNdo_quick_sort_trias(&(Dptr->triahead),
                               &(Dptr->triatail),
                               view_transfo, xlog, ylog, zlog);
      else
         CNbubble_sort_trias(  &(Dptr->triahead),
                               &(Dptr->triatail),
                               view_transfo, xlog, ylog, zlog);
   }

   /*
    * Clipping Boundary
    */
   cxmin = ((xlog) ? pow(10.0,xmin) : xmin);
   cxmax = ((xlog) ? pow(10.0,xmax) : xmax);
   cymin = ((ylog) ? pow(10.0,ymin) : ymin);
   cymax = ((ylog) ? pow(10.0,ymax) : ymax);
   czmin = - CN_LARGE;
   czmax =   CN_LARGE;
   ctmin = ((zlog) ? pow(10.0,zmin) : zmin);
   ctmax = ((zlog) ? pow(10.0,zmax) : zmax);

   /* Loop through the triangles */
   for (T=Dptr->triahead; T!=NULL; T=T->next) {

      /* Quick check to see if the triangle is in-bounds */
      if (!CNtria_in_bounds(T,
           cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax)) continue;

      /* Draw a fill triangle with gradated colors */
      if (contfill) {
          plotPS3D_single_fill_tria(T,
                                   cxmin, cxmax, cymin, cymax,
                                   czmin, czmax, ctmin, ctmax, 
                                   Dptr->data_pr.logx,
                                   Dptr->data_pr.logy,
                                   0,
                                   Dptr->data_pr.logz,
                                   Dptr->data_pr.contclip, 0);
      }

      /* Draw a mesh triangle */
      if (meshplot) {
          plotPS3D_single_solid_tria(T,
                                   cxmin, cxmax, cymin, cymax,
                                   czmin, czmax, ctmin, ctmax,
                                   Dptr->data_pr.logx,
                                   Dptr->data_pr.logy,
                                   0,
                                   Dptr->data_pr.logz,
                                   clipz,
                                   filltype, fillcolor, linetype, linecolor, 0);
      }
   }

   /* Reset color */
   PXsetColorPS(0);
}


/*
 * Plot the rectangular mesh in POSTSCRIPT
 */
static void plotPS3D_rects(Dptr,contfill,meshplot)
CNdatasetptr Dptr;
int          contfill;   /* Draw multi-colored rectangles*/
int          meshplot;   /* Draw only the mesh           */
{
   CNrectptr   R;
   double      cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax;
   int         hdnline;
   int         clipz=1;
   int         linetype, linecolor, filltype, fillcolor;

   if (Dptr->recthead == NULL) return;

   /* Initialize colors and line/fill types */
   hdnline = hiddenline || Dptr->view_pr->hiddenline;
   if (contfill) hdnline = CN_FALSE;
   filltype  = (hdnline) ? CN_FILL_SOLID : CN_FILL_NONE;
   linetype  = CN_LN_SOLID;
   fillcolor = PXpolyColorIndexPS(1);   /* yellow */
   linecolor = PXpolyColorIndexPS(4);   /* red    */

   /* Sort these rectangles */
   if (contfill || hiddenline || Dptr->view_pr->hiddenline) {
      /* sort the mesh */
      if (quick_sort)
         CNdo_quick_sort_rects(&(Dptr->recthead),
                               &(Dptr->recttail),
                               view_transfo, xlog, ylog, zlog);
      else
         CNbubble_sort_rects(  &(Dptr->recthead),
                               &(Dptr->recttail),
                               view_transfo, xlog, ylog, zlog);
   }

   /*
    * Clipping Boundary
    */
   cxmin = ((xlog) ? pow(10.0,xmin) : xmin);
   cxmax = ((xlog) ? pow(10.0,xmax) : xmax);
   cymin = ((ylog) ? pow(10.0,ymin) : ymin);
   cymax = ((ylog) ? pow(10.0,ymax) : ymax);
   czmin = - CN_LARGE;
   czmax =   CN_LARGE;
   ctmin = ((zlog) ? pow(10.0,zmin) : zmin);
   ctmax = ((zlog) ? pow(10.0,zmax) : zmax);

   /* Loop through the rectangles */
   for (R=Dptr->recthead; R!=NULL; R=R->next) {

      /* Quick check to see if the rectangle is in-bounds */
      if (!CNrect_in_bounds(R,
           cxmin,cxmax,cymin,cymax,czmin,czmax,ctmin,ctmax)) continue;

      /* Draw a fill rectangle with gradated colors */
      if (contfill) {
          plotPS3D_single_fill_rect(R,
                                   cxmin, cxmax, cymin, cymax,
                                   czmin, czmax, ctmin, ctmax, 
                                   Dptr->data_pr.logx,
                                   Dptr->data_pr.logy,
                                   0,
                                   Dptr->data_pr.logz,
                                   Dptr->data_pr.contclip, 0);
      }

      /* Draw a mesh rectangle */
      if (meshplot) {
          plotPS3D_single_solid_rect(R,
                                   cxmin, cxmax, cymin, cymax,
                                   czmin, czmax, ctmin, ctmax,
                                   Dptr->data_pr.logx,
                                   Dptr->data_pr.logy,
                                   0,
                                   Dptr->data_pr.logz, 
                                   clipz,
                                   filltype, fillcolor, linetype, linecolor, 0);
      }
   }

   /* Reset color */
   PXsetColorPS(0);
}


/*
 * Plot the polygonal mesh in PostScript
 */
static void plotPS3D_polys(Dptr,contfill,meshplot)
CNdatasetptr Dptr;
int          contfill;   /* Draw multi-colored polygons  */
int          meshplot;   /* Draw only the mesh           */
{
   CNpolyptr   P;
   double      cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax;
   int         clipz=1;
   int         dosort=1;
   int         nocont, noplot, color;
   int         linetype, linecolor, filltype, fillcolor;
 
   if (Dptr->polyhead == NULL) return;
 
   /* Sort these polygons */
   if (dosort) {
      if (quick_sort)
         CNdo_quick_sort_polys(&(Dptr->polyhead),
                               &(Dptr->polytail),
                               view_transfo, xlog, ylog, zlog);
      else
         CNbubble_sort_polys(  &(Dptr->polyhead),
                               &(Dptr->polytail),
                               view_transfo, xlog, ylog, zlog);
   }
 
   /*
    * Clipping Boundary
    */
   cxmin = ((xlog) ? pow(10.0,xmin) : xmin);
   cxmax = ((xlog) ? pow(10.0,xmax) : xmax);
   cymin = ((ylog) ? pow(10.0,ymin) : ymin);
   cymax = ((ylog) ? pow(10.0,ymax) : ymax);
   czmin = ((zlog) ? pow(10.0,zmin) : zmin);
   czmax = ((zlog) ? pow(10.0,zmax) : zmax);
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
         plotPS3D_single_fill_poly(P,
                                  cxmin, cxmax, cymin, cymax,
                                  czmin, czmax, ctmin, ctmax,
                                  Dptr->data_pr.contclip, 1);
         filltype = CN_FILL_NONE;
      }
 
      /* Draw a mesh rectangle */
      plotPS3D_single_solid_poly(P,
                                cxmin, cxmax, cymin, cymax,
                                czmin, czmax, ctmin, ctmax,
                                clipz,
                                filltype, fillcolor, linetype, linecolor, 1);
   }
 
   /* Reset color */
   PXsetColorPS(0);
}


/*
 * Plot a single solid triangle
 */
static void plotPS3D_single_solid_tria(T,
                                     cxmin, cxmax, cymin, cymax,
                                     czmin, czmax, ctmin, ctmax,
                                     logx, logy, logz, logt,
                                     clipz,
                                     filltype, fillcolor,
                                     linetype, linecolor,
                                     draw_points)
CNtriaptr T;
double    cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax;
short     logx, logy, logz, logt;
int       clipz;
int       filltype, fillcolor;
int       linetype, linecolor;
int       draw_points;
{
   CNnodeptr   node_head=NULL, node_tail=NULL;

   /* Check the triangle */
   if (T == NULL) return;

   /* Clip the triangle */
   CNclip_tria(T, &node_head, &node_tail,
               cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax,
               1, 1, clipz, 1,
               logx, logy, logz, logt, 0);
   if (node_head == NULL) return;

   /* Plot the nodes */
   plotPS3D_nodes(node_head, node_tail,
                 filltype, fillcolor, linetype, linecolor, 1, draw_points);

   /* Delete the node-list */
   CNremove_node_list(&node_head, &node_tail);
}


/*
 * Plot a single solid rectangle
 */
static void plotPS3D_single_solid_rect(R,
                                     cxmin, cxmax, cymin, cymax,
                                     czmin, czmax, ctmin, ctmax,
                                     logx, logy, logz, logt,
                                     clipz,
                                     filltype, fillcolor,
                                     linetype, linecolor,
                                     draw_points)
CNrectptr R;
double    cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax;
short     logx, logy, logz, logt;
int       clipz;
int       filltype, fillcolor;
int       linetype, linecolor;
int       draw_points;
{
   CNnodeptr   node_head=NULL, node_tail=NULL;

   /* Check the rectangle */
   if (R == NULL) return;

   /* Clip the rectangle */
   CNclip_rect(R, &node_head, &node_tail,
               cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax,
               1, 1, clipz, 1,
               logx, logy, logz, logt, 0);
   if (node_head == NULL) return;
 
   /* Plot the nodes */
   plotPS3D_nodes(node_head, node_tail,
                 filltype, fillcolor, linetype, linecolor, 1, draw_points);

   /* Delete the node-list */
   CNremove_node_list(&node_head, &node_tail);
}


/*
 * Plot a single solid polygon
 */
static void plotPS3D_single_solid_poly(P,
                                      cxmin, cxmax, cymin, cymax,
                                      czmin, czmax, ctmin, ctmax,
                                      clipz,
                                      filltype, fillcolor,
                                      linetype, linecolor,
                                      draw_points)
CNpolyptr P;
double    cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax;
int       clipz;
int       filltype, fillcolor;
int       linetype, linecolor;
int       draw_points;
{
   CNnodeptr   node_head=NULL, node_tail=NULL;
 
   /* Check the polygon */
   if (P == NULL) return;

   /* Clip the polygon */
   CNclip_poly(P, &node_head, &node_tail,
               cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax,
               1, 1, clipz, 1, 0);
   if (node_head == NULL) return;
 
   /* Plot the nodes */
   plotPS3D_nodes(node_head, node_tail,
                 filltype, fillcolor, linetype, linecolor, 1, draw_points);
 
   /* Delete the node-list */
   CNremove_node_list(&node_head, &node_tail);
}


/*
 * Draw fill colors in a triangle
 */
/*ARGSUSED*/
static void plotPS3D_single_fill_tria(T,
                                     cxmin, cxmax, cymin, cymax,
                                     czmin, czmax, ctmin, ctmax,
                                     logx, logy, logz, logt,
                                     contclip, draw_points)
CNtriaptr T;
double    cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax;
short     logx, logy, logz, logt;
short     contclip;
int       draw_points;
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

      /* Clip the triangle */
      /*EMPTY*/
      if (tmax < min || tmin > max) {

         /* Don't do anything */
         ;

      } else {

         if (min < ctmin)  min = ctmin;
         if (max > ctmax)  max = ctmax;

         /* Clip the triangle */
         CNclip_tria(T, &node_head, &node_tail,
                     cxmin, cxmax, cymin, cymax, czmin, czmax, min, max,
                     1, 1, 1, 1, 
                     logx, logy, logz, logt, 0);

         /* Plot the nodes */
         plotPS3D_nodes(node_head, node_tail,
                       CN_FILL_SOLID, PXfillColorIndex(colr), 
                       CN_LN_NONE, 0, 1, draw_points);

         /* Delete the node-list */
         CNremove_node_list(&node_head, &node_tail);
      }
   }
}


/*
 * Draw fill colors in a rectangle
 */
/*ARGSUSED*/
static void plotPS3D_single_fill_rect(R,
                                     cxmin, cxmax, cymin, cymax,
                                     czmin, czmax, ctmin, ctmax,
                                     logx, logy, logz, logt,
                                     contclip, draw_points)
CNrectptr R;
double    cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax;
short     logx, logy, logz, logt;
short     contclip;
int       draw_points;
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
 
   /* Loop through the contours */
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

         if (min < ctmin)  min = ctmin;
         if (max > ctmax)  max = ctmax;

         /* Clip the rectangle */
         CNclip_rect(R, &node_head, &node_tail,
                     cxmin, cxmax, cymin, cymax, czmin, czmax, min, max,
                     1, 1, 1, 1, 
                     logx, logy, logz, logt, 0);

         /* Plot the nodes */
         plotPS3D_nodes(node_head, node_tail,
                       CN_FILL_SOLID, PXfillColorIndex(colr), 
                       CN_LN_NONE, 0, 1, draw_points);

         /* Delete the node-list */
         CNremove_node_list(&node_head, &node_tail);
      }
   }
}


/*
 * Draw fill colors in a polygon
 */
/*ARGSUSED*/
static void plotPS3D_single_fill_poly(P,
                                     cxmin, cxmax, cymin, cymax,
                                     czmin, czmax, ctmin, ctmax,
                                     contclip, draw_points)
CNpolyptr P;
double    cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax;
short     contclip;
int       draw_points;
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
         plotPS3D_nodes(node_head, node_tail,
                       CN_FILL_SOLID, PXfillColorIndex(colr),
                       CN_LN_NONE,0,1,draw_points);
 
         /* Delete the node-list */
         CNremove_node_list(&node_head, &node_tail);
      }
   }
}

/*
 * Plot a list of nodes
 */
/*ARGSUSED*/
static void plotPS3D_nodes(node_head, node_tail,
                          filltype, fillcolor, linestyle, linecolor, linethick,
                          draw_points)
CNnodeptr node_head, node_tail;
int       filltype, fillcolor, linestyle, linecolor, linethick;
int       draw_points;
{
   CNnodeptr   N;
   CNpointptr  pt_head=NULL, pt_tail=NULL, P;
   PSPoint     points[MAX_ARR_SIZE];
   double      cxmin,cxmax,cymin,cymax,czmin,czmax;
   double      px, py, pz;
   double      x, y;
   int         count;

   /* return now if there is nothing to plot */
   if (node_head == NULL) return;

   /* if filltype and linestyle are both NONE (0), return now */
   if ((filltype==CN_FILL_NONE) && (linestyle==CN_LN_NONE)) return;

   /* rescale points, save in pointlist */
   for (N=node_head; N!=NULL; N=N->next) {
      /* Scale the point to the plot window */
      px = (xlog) ? CNlog10(N->coord->x) : N->coord->x;
      py = (ylog) ? CNlog10(N->coord->y) : N->coord->y;
      pz = (zlog) ? CNlog10(N->t       ) : N->t       ;
      if (draw_points)
      pz = (zlog) ? CNlog10(N->coord->z) : N->coord->z;
      trn_world_to_PS(px,py,pz,&x,&y);
 
      /* Put the point in the list */
      (void) CNinsert_tailpoint(&pt_head, &pt_tail, x, y, 0.0, 0);
   }

   /* Plot window boundaries */
   cxmin = Pgxmin;
   cxmax = Pgxmax;
   cymin = Pgymin;
   cymax = Pgymax;
   czmin = -0.1;     /* Not used */
   czmax =  0.1;     /* Not used */
 
   /* Clip the pointlist against the plot boundary */
   CNclip_pointlist(&pt_head,&pt_tail,
                    cxmin,cxmax,cymin,cymax,czmin,czmax,1,1,0,0);
 
   if (pt_head == NULL) return;
 
   /* Now transfer the list to an array */
   count=0;
   for (P=pt_head; P!=NULL && count<MAX_ARR_SIZE; P=P->next) {
      points[count].x = (int)(P->x);
      points[count].y = (int)(P->y);
      count++;
   }

   /* Fill the polygon */
   PXfillPS_polygon(points, count, 
                    filltype, fillcolor, linestyle, linecolor, linethick);

   /* Delete the point-list */
   CNdelete_point_list(&pt_head, &pt_tail);
}


/*
 * Draw a set of curves in a dataset
 */
static void plotPS3D_dataset_curves(dptr, colrinc, lineinc)
CNdatasetptr dptr;
int          colrinc, lineinc;
{
   CNcurveptr C;
   int        contour, contlbl;
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

   /* Sort these curves - don't sort contours */
   if ((hiddenline || dptr->view_pr->hiddenline) && !nosort && !contour) {
      /* sort the mesh */
      if (quick_sort)
         CNdo_quick_sort_curves(&(dptr->curvehead),
                                &(dptr->curvetail),
                                view_transfo, xlog, ylog, zlog);
      else
         CNbubble_sort_curves(  &(dptr->curvehead),
                                &(dptr->curvetail),
                                view_transfo, xlog, ylog, zlog);
   }

   /* Go thru each set of curves */
   for (C=dptr->curvehead; C!=NULL; C=C->next) {

      /*
       * Plot the curve
       */

      if (dptr->data_pr.splinetyp == CN_SP_NONE) {
         /* Plot the curve along the given (real) data-points */
         plotPS3D_curve(C,
                        colrinc,lineinc,
                        contour,contlbl,
                        hdnline,applyfill,pr_ptID, pr_cvID);
      } else {
         /* Plot the curve using spline-approximated data-points */
         plotPS3D_spline_curve(C,
                               spline,colrinc,lineinc,
                               contour,contlbl,
                               hdnline,applyfill,pr_ptID, pr_cvID);
      }
   }
}


/*
 * Plot the curve in POSTSCRIPT
 */
/*ARGSUSED*/
static void plotPS3D_curve(C, colrinc, lineinc, 
                           contour, contlbl, 
                           hiddenline, applyfill, pr_ptID, pr_cvID)
CNcurveptr C;
int        colrinc, lineinc;
int        contour, contlbl;
int        hiddenline, applyfill, pr_ptID, pr_cvID;
{
   CNpointptr  pt_head=NULL, pt_tail=NULL, P;
   PSPoint     points[MAX_ARR_SIZE];
   double      x,y;
   int         count, i, linepat;
   int         linetype, linecolor, marktype, markcolor, fillcolor, filltype;
   int         marksize;
   double      cxmin,cxmax,cymin,cymax,czmin,czmax;

   if (C==NULL || C->pointhead==NULL) return;

   /*
    * Make a copy of the points in the curve
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
 
   /*
    * Clip the curve against the domain boundaries first
    */
 
   /* Domain boundaries */
   cxmin = xmin;
   cxmax = xmax;
   cymin = ymin;
   cymax = ymax;
   czmin = zmin;
   czmax = zmax;
 
   /* Clip - this modifies the temporary point list */
   CNclip_pointlist(&pt_head,&pt_tail,
                    cxmin,cxmax,cymin,cymax,czmin,czmax,1,1,1,0);
 
   if (pt_head == NULL) return;

   /*
    * Another clipping - this time, clip against the plot window.
    * If this is NOT done, the Xpoint array will be filled with
    * negative/large numbers corresponding to points outside the
    * drawing area, which slows down the drawing significantly!
    */
 
   /* rescale points, save in array */
   for (P=pt_head; P!=NULL; P=P->next) {
      trn_world_to_PS(P->x,P->y,P->z,&x,&y);
      P->x = x;
      P->y = y;
      P->z = 0.0;
   }
 
   /* Plot window boundaries */
   cxmin = Pgxmin;
   cxmax = Pgxmax;
   cymin = Pgymin;
   cymax = Pgymax;
   czmin = -0.1;     /* Not used */
   czmax =  0.1;     /* Not used */
 
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
      fillPS3D_curve(pt_head,pt_tail,filltype,fillcolor);

      /* Set the linecolor now */
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

   /* Set the linepattern and width */
   linepat = PXlinetypPS(linetype,C->curv_pr.linewidth);

   /* First point */
   P = pt_head;

   /* go thru loop until we run out of points */
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

   /* Draw the markers */
   plotPS3D_markers(C->pointhead, C->pointtail,
                    marktype, marksize, markcolor, pr_ptID, contour);

   /* Draw curve ID */
   if (pr_cvID && !contour)
      plotPS3D_curveID(pt_head, pt_tail, (int)C->ID, filltype);

   /* Delete the point-list */
   CNdelete_point_list(&pt_head, &pt_tail);

   /* Reset color */
   PXsetColorPS(0);
}


/*
 * fill the curve in POSTSCRIPT
 * The pointlist has already been scaled and clipped against
 * the domain and plot boundaries
 */
/*ARGSUSED*/
static void fillPS3D_curve(pt_head, pt_tail, filltype, fillcolor)
CNpointptr  pt_head, pt_tail;
int         filltype, fillcolor;
{
   CNpointptr  P;
   PSPoint     points[MAX_ARR_SIZE];
   int         count;

   if (pt_head == NULL) return;

   /* If the curve is not to be filled then get out now */
   if (filltype == CN_FILL_NONE) return;

   /* rescale points, save in array */
   count = 0;
   for (P=pt_head; (P!=NULL) && (count<MAX_ARR_SIZE); P=P->next) {
      points[count].x = P->x;
      points[count].y = P->y;
      count++;
   }

   /* Fill the polygon */
   PXfillPS_polygon(points,count,
                   filltype, PXpolyColorIndexPS(fillcolor),CN_LN_NONE,0,1);
}


/*
 * Plot the curve in POSTSCRIPT
 */
/*ARGSUSED*/
static void plotPS3D_spline_curve(C, splinetype, colrinc, lineinc,
                                  contour, contlbl,
                                  hiddenline, applyfill, pr_ptID, pr_cvID)
CNcurveptr C;
int        splinetype;
int        colrinc, lineinc;
int        contour, contlbl;
int        hiddenline, applyfill, pr_ptID, pr_cvID;
{
   CNpointptr  pt_head=NULL, pt_tail=NULL, P;
   PSPoint     points[MAX_ARR_SIZE];
   double      *xarr, *yarr, *zarr, *xs, *ys, *zs;
   double      dist;
   double      x,y;
   int         npts, nspts, ndiv = 20, closed = 0;
   int         count, i, linepat;
   int         linetype, linecolor, marktype, markcolor, fillcolor, filltype;
   int         marksize;
   double      cxmin,cxmax,cymin,cymax,czmin,czmax;

   if (C==NULL || C->pointhead==NULL) return;

   /*
    * Make a copy of the points in the curve
    */
   CNcopy_abslog_pointlist(&pt_head, &pt_tail,
                           C->pointhead, C->pointtail,
                           xabs, yabs, zabs, xlog, ylog, zlog);
 
   /* Count the number of points in the curve */
   npts = CNcount_points(pt_head, pt_tail);

   /*
    * Allocate double-precision arrays to hold x, y, z values
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
    * Clip the curve against the domain boundaries first
    */

   /* Domain boundaries */
   cxmin = xmin;
   cxmax = xmax;
   cymin = ymin;
   cymax = ymax;
   czmin = zmin;
   czmax = zmax;
 
   /* Clip - this modifies the temporary point list */
   CNclip_pointlist(&pt_head,&pt_tail,
                    cxmin,cxmax,cymin,cymax,czmin,czmax,1,1,1,0);
 
   if (pt_head == NULL) return;

   /*
    * Another clipping - this time, clip against the plot window.
    * If this is NOT done, the Xpoint array will be filled with
    * negative/large numbers corresponding to points outside the
    * drawing area, which slows down the drawing significantly!
    */
 
   /* rescale points, save in array */
   for (P=pt_head; P!=NULL; P=P->next) {
      trn_world_to_PS(P->x,P->y,P->z,&x,&y);
      P->x = x;
      P->y = y;
      P->z = 0.0;
   }
 
   /* Plot window boundaries */
   cxmin = Pgxmin;
   cxmax = Pgxmax;
   cymin = Pgymin;
   cymax = Pgymax;
   czmin = -0.1;     /* Not used */
   czmax =  0.1;     /* Not used */
 
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
      fillPS3D_curve(pt_head,pt_tail,filltype,fillcolor);
 
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

   P = pt_head;

   /* go thru loop until we run out of points */
   while ((P!=NULL) && linepat) {

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
      if (P != NULL) P=P->prev;
   }

   /* Reset the linetype */
   (void) PXlinetypPS(CN_LN_SOLID,1);

   /* Draw the markers */
   plotPS3D_markers(C->pointhead, C->pointtail,
                    marktype, marksize, markcolor, pr_ptID, contour);

   /* Free the arrays */
   CNfree_1D_double_array(xarr);
   CNfree_1D_double_array(yarr);
   CNfree_1D_double_array(zarr);
   CNfree_1D_double_array(xs);
   CNfree_1D_double_array(ys);
   CNfree_1D_double_array(zs);

   /* Draw curve ID */
   if (pr_cvID && !contour)
      plotPS3D_curveID(pt_head, pt_tail, (int)C->ID, filltype);

   /* Delete the point-list */
   CNdelete_point_list(&pt_head, &pt_tail);

   /* Reset color */
   PXsetColorPS(0);
}


/*
 * Plot the curve markers in POSTSCRIPT
 */
/*ARGSUSED*/
static void plotPS3D_markers(pointhead,pointtail,
                            marktype,marksize,markcolor,
                            pr_ptID,contour)
CNpointptr pointhead, pointtail;
int        marktype, marksize, markcolor;
int        pr_ptID, contour;
{
   CNpointptr  P, pt_head=NULL, pt_tail=NULL;
   double      px, py, pz;
   double      x,y;
   double      cxmin, cxmax, cymin, cymax, czmin, czmax;
   double      pxmin, pxmax, pymin, pymax;

   if (pointhead == NULL) return;

   /*
    * Make a copy of the points in the curve
    */
   CNcopy_abslog_pointlist(&pt_head, &pt_tail,
                           pointhead, pointtail,
                           xabs, yabs, zabs, xlog, ylog, zlog);

   /* Set the linetype, width and marker color */
   (void) PXlinetypPS(CN_LN_SOLID,1);
   PXlineColorPS(markcolor);

   /* Domain (Clip) boundaries */
   cxmin = xmin;
   cxmax = xmax;
   cymin = ymin;
   cymax = ymax;
   czmin = zmin;
   czmax = zmax;
 
   /* Plot boundaries */
   pxmin = Pgxmin;
   pxmax = Pgxmax;
   pymin = Pgymin;
   pymax = Pgymax;

   for (P=pt_head; P!=NULL && marktype!=CN_MK_NONE; P=P->next) {

      if (P->x < cxmin || P->x > cxmax) continue;
      if (P->y < cymin || P->y > cymax) continue;
      if (P->z < czmin || P->z > czmax) continue;

      /* rescale points */
      px = P->x;
      py = P->y;
      pz = P->z;
      trn_world_to_PS(px,py,pz,&x,&y);

      /* plot the points only when the point is inside the window */
      if (x < pxmin || x > pxmax) continue;
      if (y < pymin || y > pymax) continue;

      /* Plot the marker */
      PXmarkerPS(marktype,marksize,x,y);
   }

   /* Draw curve-point ID's */
   if (pr_ptID && !contour)
      plotPS3D_pointIDs(pt_head, pt_tail, CN_TRUE);
 
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
static void plotPS3D_curveID(pointhead, pointtail, curveID, filltype)
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
static void plotPS3D_pointIDs(pt_head,pt_tail,nolog)
CNpointptr pt_head, pt_tail;
int        nolog;
{
   CNpointptr  P;
   double      x,y;
   char        label[CN_MAXCHAR];
   double      rxmin, rxmax, rymin, rymax;
   double      cxmin, cxmax, cymin, cymax, czmin, czmax;
   double      px, py, pz;
 
   if (pt_head == NULL) return;
 
   /* Boundary */
   rxmin = Pgxmin;
   rxmax = Pgxmax;
   rymin = Pgymin;
   rymax = Pgymax;

   /* Clip boundaries */
   if (nolog) {
   cxmin = xmin;
   cxmax = xmax;
   cymin = ymin;
   cymax = ymax;
   czmin = zmin;
   czmax = zmax;
   } else {
   cxmin = ((xlog) ? pow(10.0,xmin) : xmin);
   cxmax = ((xlog) ? pow(10.0,xmax) : xmax);
   cymin = ((ylog) ? pow(10.0,ymin) : ymin);
   cymax = ((ylog) ? pow(10.0,ymax) : ymax);
   czmin = ((zlog) ? pow(10.0,zmin) : zmin);
   czmax = ((zlog) ? pow(10.0,zmax) : zmax);
   }

   /* Set the font */
   (void) fprintf(ips,"setMkrsLblFont\n");
 
   /* Set the linetype, width and color */
   (void) PXlinetypPS(CN_LN_SOLID,1);
   PXlineColorPS(0);
 
   /* Go thru each point */
   for (P=pt_head; P!=NULL; P=P->next) {
      /* Check to see if the point is in bounds */
      if ( !( (P->x < cxmin) || (P->x > cxmax) ||
              (P->y < cymin) || (P->y > cymax) ||
              (P->z < czmin) || (P->z > czmax) ) ) {
         if (nolog) {
         px = P->x;
         py = P->y;
         pz = P->z;
         } else {
         px = (xlog) ? CNlog10(P->x) : P->x;
         py = (ylog) ? CNlog10(P->y) : P->y;
         pz = (zlog) ? CNlog10(P->z) : P->z;
         }
         trn_world_to_PS(px,py,pz,&x,&y);

         /* Print the label only if the point is in bounds */
         if (x > rxmin && x < rxmax && y > rymin && y < rymax) {
 
            (void) sprintf(label,"P%d",P->ID);
            (void) fprintf(ips,"%.2f %.2f moveto ",
                           x+2.0*scale,y);
            (void) fprintf(ips,"(%s) MIJM LEJ show\n",
                           PXmodifyPS_string(label));
         }
      }
   }
}


/*
 * Plot the node ID labels in POSTSCRIPT
 */
/*ARGSUSED*/
static void plotPS3D_nodeIDs(nd_head,nd_tail)
CNnodeptr nd_head, nd_tail;
{
   CNnodeptr   N;
   double      x,y;
   char        label[CN_MAXCHAR];
   double      rxmin, rxmax, rymin, rymax;
   double      cxmin, cxmax, cymin, cymax, czmin, czmax;
   double      px, py, pz;
 
   if (nd_head == NULL) return;
 
   /* Boundary */
   rxmin = Pgxmin;
   rxmax = Pgxmax;
   rymin = Pgymin;
   rymax = Pgymax;

   /* Clip boundaries */
   cxmin = ((xlog) ? pow(10.0,xmin) : xmin);
   cxmax = ((xlog) ? pow(10.0,xmax) : xmax);
   cymin = ((ylog) ? pow(10.0,ymin) : ymin);
   cymax = ((ylog) ? pow(10.0,ymax) : ymax);
   czmin = ((zlog) ? pow(10.0,zmin) : zmin);
   czmax = ((zlog) ? pow(10.0,zmax) : zmax);

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
      /* Check to see if the node is in bounds */
      if ( !( (N->coord->x < cxmin) || (N->coord->x > cxmax) ||
              (N->coord->y < cymin) || (N->coord->y > cymax) ||
              (N->t        < czmin) || (N->t        > czmax) ) ) {
         px = (xlog) ? CNlog10(N->coord->x) : N->coord->x;
         py = (ylog) ? CNlog10(N->coord->y) : N->coord->y;
         pz = (zlog) ? CNlog10(N->t       ) : N->t;
         trn_world_to_PS(px,py,pz,&x,&y);
 
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
   }

   /* Reset all the point-flags */
   for (N=nd_head; N!=NULL; N=N->next) N->coord->flag = 0;
}


/*
 * Plot the triangle ID labels in POSTSCRIPT
 */
/*ARGSUSED*/
static void plotPS3D_triaIDs(tr_head,tr_tail)
CNtriaptr tr_head, tr_tail;
{
   CNtriaptr   T;
   double      x,y,midx,midy,midz;
   char        label[CN_MAXCHAR];
   double      rxmin, rxmax, rymin, rymax;
   double      cxmin, cxmax, cymin, cymax, czmin, czmax;
   double      px, py, pz;
 
   if (tr_head == NULL) return;
 
   /* Boundary */
   rxmin = Pgxmin;
   rxmax = Pgxmax;
   rymin = Pgymin;
   rymax = Pgymax;

   /* Clip boundaries */
   cxmin = ((xlog) ? pow(10.0,xmin) : xmin);
   cxmax = ((xlog) ? pow(10.0,xmax) : xmax);
   cymin = ((ylog) ? pow(10.0,ymin) : ymin);
   cymax = ((ylog) ? pow(10.0,ymax) : ymax);
   czmin = ((zlog) ? pow(10.0,zmin) : zmin);
   czmax = ((zlog) ? pow(10.0,zmax) : zmax);

   /* Set the font */
   (void) fprintf(ips,"setMkrsLblFont\n");
 
   /* Set the linetype, width and color */
   (void) PXlinetypPS(CN_LN_SOLID,1);
   PXlineColorPS(0);
 
   /* Go thru each triangle */
   for (T=tr_head; T!=NULL; T=T->next) {
      /* Get the x-y-t value of the triangle-midpoint */
      midx = T->n1->coord->x + T->n2->coord->x + T->n3->coord->x;
      midy = T->n1->coord->y + T->n2->coord->y + T->n3->coord->y;
      midz = T->n1->t        + T->n2->t        + T->n3->t       ;
      midx = midx/3.0;
      midy = midy/3.0;
      midz = midz/3.0;

      /* Check to see if the point is in bounds */
      if ( !( (midx < cxmin) || (midx > cxmax) ||
              (midy < cymin) || (midy > cymax) ||
              (midz < czmin) || (midz > czmax) ) ) {
         px = (xlog) ? CNlog10(midx) : midx;
         py = (ylog) ? CNlog10(midy) : midy;
         pz = (zlog) ? CNlog10(midz) : midz;
         trn_world_to_PS(px,py,pz,&x,&y);

         /* Print the label only if the point is in bounds */
         if (x > rxmin && x < rxmax && y > rymin && y < rymax) {
 
            (void) sprintf(label,"T%d",T->ID);
            (void) fprintf(ips,"%.2f %.2f moveto ",x,y);
            (void) fprintf(ips,"(%s) MIJM CEJ show\n",
                           PXmodifyPS_string(label));
         }
      }
   }
}


/*
 * Plot the rectangle ID labels in POSTSCRIPT
 */
/*ARGSUSED*/
static void plotPS3D_rectIDs(rt_head,rt_tail)
CNrectptr rt_head, rt_tail;
{
   CNrectptr   R;
   double      x,y,midx,midy,midz;
   char        label[CN_MAXCHAR];
   double      rxmin, rxmax, rymin, rymax;
   double      cxmin, cxmax, cymin, cymax, czmin, czmax;
   double      px, py, pz;
 
   if (rt_head == NULL) return;
 
   /* Boundary */
   rxmin = Pgxmin;
   rxmax = Pgxmax;
   rymin = Pgymin;
   rymax = Pgymax;

   /* Clip boundaries */
   cxmin = ((xlog) ? pow(10.0,xmin) : xmin);
   cxmax = ((xlog) ? pow(10.0,xmax) : xmax);
   cymin = ((ylog) ? pow(10.0,ymin) : ymin);
   cymax = ((ylog) ? pow(10.0,ymax) : ymax);
   czmin = ((zlog) ? pow(10.0,zmin) : zmin);
   czmax = ((zlog) ? pow(10.0,zmax) : zmax);

   /* Set the font */
   (void) fprintf(ips,"setMkrsLblFont\n");
 
   /* Set the linetype, width and color */
   (void) PXlinetypPS(CN_LN_SOLID,1);
   PXlineColorPS(0);
 
   /* Go thru each rectangle */
   for (R=rt_head; R!=NULL; R=R->next) {
      /* Get the x-y-t value of the rectangle-midpoint */
      midx = R->n1->coord->x + R->n2->coord->x +
             R->n3->coord->x + R->n4->coord->y;
      midy = R->n1->coord->y + R->n2->coord->y +
             R->n3->coord->y + R->n4->coord->y;
      midz = R->n1->t + R->n2->t + R->n3->t + R->n4->t;
      midx = midx/4.0;
      midy = midy/4.0;
      midz = midz/4.0;

      /* Check to see if the point is in bounds */
      if ( !( (midx < cxmin) || (midx > cxmax) ||
              (midy < cymin) || (midy > cymax) ||
              (midz < czmin) || (midz > czmax) ) ) {
         px = (xlog) ? CNlog10(midx) : midx;
         py = (ylog) ? CNlog10(midy) : midy;
         pz = (zlog) ? CNlog10(midz) : midz;
         trn_world_to_PS(px,py,pz,&x,&y);

         /* Print the label only if the point is in bounds */
         if (x > rxmin && x < rxmax && y > rymin && y < rymax) {
 
            (void) sprintf(label,"R%d",R->ID);
            (void) fprintf(ips,"%.2f %.2f moveto ",x,y);
            (void) fprintf(ips,"(%s) MIJM CEJ show\n",
                           PXmodifyPS_string(label));
         }
      }
   }
}


/*
 * ANNOTATIONS
 */

/*
 * Plot annotations
 */
static void annotatePS3D()
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
         annotScale = (2*CN_WINDOWSCALE)/
                       (plotdata->view_pr->windscl_xr -
                        plotdata->view_pr->windscl_xl);
         if (annotScale < 1.0e-10) annotScale = 0.01;
 
         /* Plot each annotation */
         for (AP=DS->Dptr->annothead; AP!=NULL; AP=AP->next)
            plotPS3D_single_annotation(AP, annotScale);
      }
   }

   /* Plot the annotations in the plotset */
   annotScale = 1.0;
   for (AP=plotdata->annothead; AP!=NULL; AP=AP->next)
      plotPS3D_single_annotation(AP, annotScale);
}

/*
 * Plot a single annotation
 */
static void plotPS3D_single_annotation(AP, annotScale)
CNannotptr AP;
double annotScale;
{
   if (AP == NULL) return;

   switch (AP->type) {
   case CN_AN_RECT : /* Draw a rectangle */
                     plotPS3D_annot_rect(AP, annotScale);
                     break;
   case CN_AN_LINE : /* Draw a line      */
                     plotPS3D_annot_line(AP, annotScale);
                     break;
   case CN_AN_ARROW: /* Draw a arrow     */
                     plotPS3D_annot_arrow(AP, annotScale);
                     break;
   case CN_AN_POINT: /* Draw a point     */
                     plotPS3D_annot_point(AP, annotScale);
                     break;
   case CN_AN_TEXT : /* Draw a text label */
                     plotPS3D_annot_text(AP, annotScale);
                     break;
   default         : break;
   }
}

/*
 * Plot an annotation rectangle
 */
static void plotPS3D_annot_rect(AP, annotScale)
CNannotptr AP;
double annotScale;
{
   PSPoint points[MAX_ARR_SIZE];
   double  px,py,pz,x1,y1,x2,y2,x,y;
   int     i, npts=0;
   double  rxmin, rxmax, rymin, rymax;
   double  bxmin, bxmax, bymin, bymax;
   double  xc, yc;
   CNpointptr  pt_head=NULL, pt_tail=NULL, P;
   double      cxmin,cxmax,cymin,cymax,czmin,czmax;
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
      x2 = rxmin + AP->pt2.x * (rxmax-rxmin);
      y2 = rymin + AP->pt2.y * (rymax-rymin);

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
      i=0;
      points[i].x = bxmin;   points[i].y = bymin;   i++;
      points[i].x = bxmax;   points[i].y = bymin;   i++;
      points[i].x = bxmax;   points[i].y = bymax;   i++;
      points[i].x = bxmin;   points[i].y = bymax;   i++;
      points[i].x = bxmin;   points[i].y = bymin;   i++;
      npts = i;

   } else {

      /* Insert points */
      (void) CNinsert_point(&pt_head, &pt_tail,
                           AP->pt1.x,AP->pt1.y,AP->pt1.z,0);
      (void) CNinsert_point(&pt_head, &pt_tail,
                           AP->pt1.x,AP->pt2.y,AP->pt1.z,0);
      (void) CNinsert_point(&pt_head, &pt_tail,
                           AP->pt2.x,AP->pt2.y,AP->pt2.z,0);
      (void) CNinsert_point(&pt_head, &pt_tail,
                           AP->pt2.x,AP->pt1.y,AP->pt2.z,0);
      (void) CNinsert_point(&pt_head, &pt_tail,
                           AP->pt1.x,AP->pt1.y,AP->pt1.z,0);

      /* Clip boundaries */
      cxmin = ((xlog) ? pow(10.0,xmin) : xmin);
      cxmax = ((xlog) ? pow(10.0,xmax) : xmax);
      cymin = ((ylog) ? pow(10.0,ymin) : ymin);
      cymax = ((ylog) ? pow(10.0,ymax) : ymax);
      czmin = ((zlog) ? pow(10.0,zmin) : zmin);
      czmax = ((zlog) ? pow(10.0,zmax) : zmax);
      if (AP->property.doclip) {
         /* Clip - this modifies on the temporary point list */
         CNclip_pointlist(&pt_head,&pt_tail,
                          cxmin,cxmax,cymin,cymax,czmin,czmax,1,1,1,0);
      }

      if (pt_head == NULL) return;
 
      /*
       * Another clipping - this time, clip against the plot window.
       * If this is NOT done, the Xpoint array will be filled with
       * negative/large numbers corresponding to points outside the
       * drawing area, which slows down the drawing significantly!
       */
 
      /* rescale points, save in array */
      for (P=pt_head; P!=NULL; P=P->next) {
         px = (xlog) ? CNlog10(P->x) : P->x;
         py = (ylog) ? CNlog10(P->y) : P->y;
         pz = (zlog) ? CNlog10(P->z) : P->z;
         trn_world_to_PS(px,py,pz,&x,&y);
         P->x = x;
         P->y = y;
         P->z = 0.0;
      }

      /* Plot window boundaries */
      cxmin = 0.0;
      cxmax = (double) (landscape ? PXL_DIM : PX_DIM);
      cymin = 0.0;
      cymax = (double) (landscape ? PYL_DIM : PY_DIM);
      czmin = -0.1;     /* Not used */
      czmax =  0.1;     /* Not used */
 
      /* Clip - this modifies the temporary point list */
      CNclip_pointlist(&pt_head,&pt_tail,
                       cxmin,cxmax,cymin,cymax,czmin,czmax,1,1,0,0);
 
      if (pt_head == NULL) return;
 
      /* save points in array */
      npts = 0;
      for (P=pt_head; P!=NULL && npts<MAX_ARR_SIZE; P=P->next) {
         points[npts].x = (int)(P->x);
         points[npts].y = (int)(P->y);
         npts++;
      }
   }

   /* Fill the polygon */
   PXfillPS_polygon(points, npts,
                   (int) AP->property.filltype,
                   PXpolyColorIndexPS( (int) AP->property.fillcolor ),
                   (int) AP->property.linetype,
                   (int) AP->property.linecolor,
                   (int) AP->property.linewidth);

   if (npts<=1) return;
 
   /* Get the polygon center */
   xc = yc = 0.0;
   for (i=0; i<npts; i++) {
      xc += points[i].x;
      yc += points[i].y;
   }
   if ((points[npts-1].x==points[0].x) && (points[npts-1].y==points[0].y)) {
      xc -= points[0].x;
      yc -= points[0].y;
      xc = xc/(double)(npts-1);
      yc = yc/(double)(npts-1);
   } else {
      xc = xc/(double)(npts);
      yc = yc/(double)(npts);
   }

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
      PXplotPS_scalable_font(xc, yc,
                            AP->property.linelabel, fontsize,
                            CN_FALSE, CN_FALSE,
                            CN_FALSE, CN_FALSE);

      /* Reset the line color */
      PXlineColorPS(0);
   }

   /* Delete the point-list */
   CNdelete_point_list(&pt_head, &pt_tail);
}

/*
 * Plot an annotation line
 */
static void plotPS3D_annot_line(AP, annotScale)
CNannotptr AP;
double annotScale;
{
   double rxmin,rxmax,rymin,rymax;
   double cxmin,cxmax,cymin,cymax,czmin,czmax;
   double px,py,pz,x1,y1,z1,x2,y2,z2;
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
      z1 = AP->pt1.z;
      x2 = AP->pt2.x;
      y2 = AP->pt2.y;
      z2 = AP->pt2.z;

      /* Do pre-clipping aaginst the real-world boundaries */
      if (AP->property.doclip) {
        /* Clipping boundary */
        cxmin = ((xlog) ? pow(10.0,xmin) : xmin);
        cxmax = ((xlog) ? pow(10.0,xmax) : xmax);
        cymin = ((ylog) ? pow(10.0,ymin) : ymin);
        cymax = ((ylog) ? pow(10.0,ymax) : ymax);
        czmin = ((zlog) ? pow(10.0,zmin) : zmin);
        czmax = ((zlog) ? pow(10.0,zmax) : zmax);

        if ((x1 <=cxmin && x2 <=cxmin) || (x1 >=cxmax && x2 >=cxmax)) return;
        if ((y1 <=cymin && y2 <=cymin) || (y1 >=cymax && y2 >=cymax)) return;
        if ((z1 <=czmin && z2 <=czmin) || (z1 >=czmax && z2 >=czmax)) return;
        clipPS3D_in_xmin(&x1,&y1,&z1,&x2,&y2,&z2,cxmin,&p1_clipped,&p2_clipped);
        clipPS3D_in_ymin(&x1,&y1,&z1,&x2,&y2,&z2,cymin,&p1_clipped,&p2_clipped);
        clipPS3D_in_zmin(&x1,&y1,&z1,&x2,&y2,&z2,czmin,&p1_clipped,&p2_clipped);
        clipPS3D_in_xmax(&x1,&y1,&z1,&x2,&y2,&z2,cxmax,&p1_clipped,&p2_clipped);
        clipPS3D_in_ymax(&x1,&y1,&z1,&x2,&y2,&z2,cymax,&p1_clipped,&p2_clipped);
        clipPS3D_in_zmax(&x1,&y1,&z1,&x2,&y2,&z2,czmax,&p1_clipped,&p2_clipped);
        if ((x1 <=cxmin && x2 <=cxmin) || (x1 >=cxmax && x2 >=cxmax)) return;
        if ((y1 <=cymin && y2 <=cymin) || (y1 >=cymax && y2 >=cymax)) return;
        if ((z1 <=czmin && z2 <=czmin) || (z1 >=czmax && z2 >=czmax)) return;
      }

      /* rescale points */
      px = (xlog) ? CNlog10(x1) : x1;
      py = (ylog) ? CNlog10(y1) : y1;
      pz = (zlog) ? CNlog10(z1) : z1;
      trn_world_to_PS(px,py,pz,&x1,&y1);

      /* rescale points */
      px = (xlog) ? CNlog10(x2) : x2;
      py = (ylog) ? CNlog10(y2) : y2;
      pz = (zlog) ? CNlog10(z2) : z2;
      trn_world_to_PS(px,py,pz,&x2,&y2);
   }

   /* Check to see if the line is inside the plot area */
   if ((x1 < rxmin && x2 < rxmin) || (x1 > rxmax && x2 > rxmax)) return;
   if ((y1 < rymin && y2 < rymin) || (y1 > rymax && y2 > rymax)) return;

   /* Do another clipping */
   clipPS3D_in_xmin(&x1,&y1,&z1,&x2,&y2,&z2,rxmin,&p1_clipped,&p2_clipped);
   clipPS3D_in_ymin(&x1,&y1,&z1,&x2,&y2,&z2,rymin,&p1_clipped,&p2_clipped);
   clipPS3D_in_xmax(&x1,&y1,&z1,&x2,&y2,&z2,rxmax,&p1_clipped,&p2_clipped);
   clipPS3D_in_ymax(&x1,&y1,&z1,&x2,&y2,&z2,rymax,&p1_clipped,&p2_clipped);

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
}

/*
 * Plot an annotation arrow
 */
static void plotPS3D_annot_arrow(AP, annotScale)
CNannotptr AP;
double annotScale;
{
   double cxmin,cxmax,cymin,cymax,czmin,czmax;
   double rxmin,rxmax,rymin,rymax;
   double px,py,pz,x1,y1,z1,x2,y2,z2;
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
      z1 = AP->pt1.z;
      x2 = AP->pt2.x;
      y2 = AP->pt2.y;
      z2 = AP->pt2.z;

      /* Do pre-clipping aaginst the real-world boundaries */
      if (AP->property.doclip) {
        /* Clipping boundary */
        cxmin = ((xlog) ? pow(10.0,xmin) : xmin);
        cxmax = ((xlog) ? pow(10.0,xmax) : xmax);
        cymin = ((ylog) ? pow(10.0,ymin) : ymin);
        cymax = ((ylog) ? pow(10.0,ymax) : ymax);
        czmin = ((zlog) ? pow(10.0,zmin) : zmin);
        czmax = ((zlog) ? pow(10.0,zmax) : zmax);

        if ((x1 <=cxmin && x2 <=cxmin) || (x1 >=cxmax && x2 >=cxmax)) return;
        if ((y1 <=cymin && y2 <=cymin) || (y1 >=cymax && y2 >=cymax)) return;
        if ((z1 <=czmin && z2 <=czmin) || (z1 >=czmax && z2 >=czmax)) return;
        clipPS3D_in_xmin(&x1,&y1,&z1,&x2,&y2,&z2,cxmin,&p1_clipped,&p2_clipped);
        clipPS3D_in_ymin(&x1,&y1,&z1,&x2,&y2,&z2,cymin,&p1_clipped,&p2_clipped);
        clipPS3D_in_zmin(&x1,&y1,&z1,&x2,&y2,&z2,czmin,&p1_clipped,&p2_clipped);
        clipPS3D_in_xmax(&x1,&y1,&z1,&x2,&y2,&z2,cxmax,&p1_clipped,&p2_clipped);
        clipPS3D_in_ymax(&x1,&y1,&z1,&x2,&y2,&z2,cymax,&p1_clipped,&p2_clipped);
        clipPS3D_in_zmax(&x1,&y1,&z1,&x2,&y2,&z2,czmax,&p1_clipped,&p2_clipped);
        if ((x1 <=cxmin && x2 <=cxmin) || (x1 >=cxmax && x2 >=cxmax)) return;
        if ((y1 <=cymin && y2 <=cymin) || (y1 >=cymax && y2 >=cymax)) return;
        if ((z1 <=czmin && z2 <=czmin) || (z1 >=czmax && z2 >=czmax)) return;
      }

      /* rescale points */
      px = (xlog) ? CNlog10(x1) : x1;
      py = (ylog) ? CNlog10(y1) : y1;
      pz = (zlog) ? CNlog10(z1) : z1;
      trn_world_to_PS(px,py,pz,&x1,&y1);

      /* rescale points */
      px = (xlog) ? CNlog10(x2) : x2;
      py = (ylog) ? CNlog10(y2) : y2;
      pz = (zlog) ? CNlog10(z2) : z2;
      trn_world_to_PS(px,py,pz,&x2,&y2);
   }

   /* Set the font */
   fontsize = AP->property.fontsize;
   if (AP->property.doscale) {
      fontsize = fontsize * annotScale;
      if (fontsize < 1.0e-10) fontsize = 1.0;
   }

   /* Draw the arrow */
   plotPS3D_arrow(x1,y1,x2,y2,
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
static void plotPS3D_arrow(x1,y1,x2,y2,
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
   double z1=0.0, z2=0.0;
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
   clipPS3D_in_xmin(&x1,&y1,&z1,&x2,&y2,&z2,rxmin,&p1_clipped,&p2_clipped);
   clipPS3D_in_ymin(&x1,&y1,&z1,&x2,&y2,&z2,rymin,&p1_clipped,&p2_clipped);
   clipPS3D_in_xmax(&x1,&y1,&z1,&x2,&y2,&z2,rxmax,&p1_clipped,&p2_clipped);
   clipPS3D_in_ymax(&x1,&y1,&z1,&x2,&y2,&z2,rymax,&p1_clipped,&p2_clipped);

   /* Draw the arrow origin */
   (void) PXlinetypPS(CN_LN_SOLID,1);
   PXlineColorPS(markcolor);
   if (!p1_clipped) PXmarkerPS(marktype,marksize,x1,y1);

   /* Draw the arrow line */
   PXlineColorPS(linecolor);
   PXdrawPS_line(x1,y1,x2,y2, linetype, linewidth);

   /* Draw the arrow head */
   if (!p2_clipped && arrowhead) {
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
static void plotPS3D_annot_point(AP, annotScale)
CNannotptr AP;
double annotScale;
{
   double  px,py,pz,x1,y1;
   double  rxmin, rxmax, rymin, rymax;
   double  cxmin,cxmax,cymin,cymax,czmin,czmax;
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
      /* Do a pre-clipping */
      if (AP->property.doclip) {
         /* Clipping boundary */
         cxmin = ((xlog) ? pow(10.0,xmin) : xmin);
         cxmax = ((xlog) ? pow(10.0,xmax) : xmax);
         cymin = ((ylog) ? pow(10.0,ymin) : ymin);
         cymax = ((ylog) ? pow(10.0,ymax) : ymax);
         czmin = ((zlog) ? pow(10.0,zmin) : zmin);
         czmax = ((zlog) ? pow(10.0,zmax) : zmax);
         if ((AP->pt1.x < cxmin || AP->pt1.x > cxmax) ||
             (AP->pt1.y < cymin || AP->pt1.y > cymax) ||
             (AP->pt1.z < czmin || AP->pt1.z > czmax))
            return;
      }

      /* rescale points */
      px = (xlog) ? CNlog10(AP->pt1.x) : AP->pt1.x;
      py = (ylog) ? CNlog10(AP->pt1.y) : AP->pt1.y;
      pz = (zlog) ? CNlog10(AP->pt1.z) : AP->pt1.z;
      trn_world_to_PS(px,py,pz,&x1,&y1);
   }

   /* Draw the markers */
   if ((rxmin < x1 && x1 < rxmax) && (rymin < y1 && y1 < rymax)) {
      (void) PXlinetypPS(CN_LN_SOLID,1);
      PXlineColorPS(AP->property.markcolor);
      PXmarkerPS((int)AP->property.marktype,
                 (int)AP->property.marksize,x1,y1);
 
      /* If there is text attached, draw it */
      if (AP->property.linelabel) {

         /* Set the text color */
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
static void plotPS3D_annot_text(AP, annotScale)
CNannotptr AP;
double annotScale;
{
   double  px,py,pz,x1,y1;
   double  rxmin, rxmax, rymin, rymax;
   double  cxmin,cxmax,cymin,cymax,czmin,czmax;
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
      /* Do a pre-clipping */
      if (AP->property.doclip) {
         /* Clipping boundary */
         cxmin = ((xlog) ? pow(10.0,xmin) : xmin);
         cxmax = ((xlog) ? pow(10.0,xmax) : xmax);
         cymin = ((ylog) ? pow(10.0,ymin) : ymin);
         cymax = ((ylog) ? pow(10.0,ymax) : ymax);
         czmin = ((zlog) ? pow(10.0,zmin) : zmin);
         czmax = ((zlog) ? pow(10.0,zmax) : zmax);
         if ((AP->pt1.x < cxmin || AP->pt1.x > cxmax) ||
             (AP->pt1.y < cymin || AP->pt1.y > cymax) ||
             (AP->pt1.z < czmin || AP->pt1.z > czmax))
            return;
      }

      /* rescale points */
      px = (xlog) ? CNlog10(AP->pt1.x) : AP->pt1.x;
      py = (ylog) ? CNlog10(AP->pt1.y) : AP->pt1.y;
      pz = (zlog) ? CNlog10(AP->pt1.z) : AP->pt1.z;
      trn_world_to_PS(px,py,pz,&x1,&y1);
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
static void clipPS3D_in_xmin(x1,y1,z1,x2,y2,z2,rxmin,p1_clipped,p2_clipped)
double *x1, *y1, *z1, *x2, *y2, *z2, rxmin;
int    *p1_clipped, *p2_clipped;
{
   double t;

   /* Clip the line against rxmin */
   if (*x1 < rxmin && *x2 > rxmin) {
      t = (rxmin - *x1)/(*x2 - *x1);
      *x1 = *x1 + t*(*x2 - *x1);
      *y1 = *y1 + t*(*y2 - *y1);
      *z1 = *z1 + t*(*z2 - *z1);
      if (*x1 < rxmin) *x1 = rxmin;
      *p1_clipped = CN_TRUE;
   } else if (*x2 < rxmin && *x1 > rxmin) {
      t = (rxmin - *x2)/(*x1 - *x2);
      *x2 = *x2 + t*(*x1 - *x2);
      *y2 = *y2 + t*(*y1 - *y2);
      *z2 = *z2 + t*(*z1 - *z2);
      if (*x2 < rxmin) *x2 = rxmin;
      *p2_clipped = CN_TRUE;
   }
}

/*
 * Clip a line against ymin
 */
static void clipPS3D_in_ymin(x1,y1,z1,x2,y2,z2,rymin,p1_clipped,p2_clipped)
double *x1, *y1, *z1, *x2, *y2, *z2, rymin;
int    *p1_clipped, *p2_clipped;
{
   clipPS3D_in_xmin(y1,x1,z1,y2,x2,z2,rymin,p1_clipped,p2_clipped);
}

/*
 * Clip a line against zmin
 */
static void clipPS3D_in_zmin(x1,y1,z1,x2,y2,z2,rzmin,p1_clipped,p2_clipped)
double *x1, *y1, *z1, *x2, *y2, *z2, rzmin;
int    *p1_clipped, *p2_clipped;
{
   clipPS3D_in_xmin(z1,y1,x1,z2,y2,x2,rzmin,p1_clipped,p2_clipped);
}

/*
 * Clip a line against xmax
 */
static void clipPS3D_in_xmax(x1,y1,z1,x2,y2,z2,rxmax,p1_clipped,p2_clipped)
double *x1, *y1, *z1, *x2, *y2, *z2, rxmax;
int    *p1_clipped, *p2_clipped;
{
   double t;

   /* Clip the line against rxmax */
   if (*x1 < rxmax && *x2 > rxmax) {
      t = (rxmax - *x2)/(*x1 - *x2);
      *x2 = *x2 + t*(*x1 - *x2);
      *y2 = *y2 + t*(*y1 - *y2);
      *z2 = *z2 + t*(*z1 - *z2);
      if (*x2 > rxmax) *x2 = rxmax;
      *p2_clipped = CN_TRUE;
   } else if (*x2 < rxmax && *x1 > rxmax) {
      t = (rxmax - *x1)/(*x2 - *x1);
      *x1 = *x1 + t*(*x2 - *x1);
      *y1 = *y1 + t*(*y2 - *y1);
      *z1 = *z1 + t*(*z2 - *z1);
      if (*x1 > rxmax) *x1 = rxmax;
      *p1_clipped = CN_TRUE;
   }
}

/*
 * Clip a line against ymax
 */
static void clipPS3D_in_ymax(x1,y1,z1,x2,y2,z2,rymax,p1_clipped,p2_clipped)
double *x1, *y1, *z1, *x2, *y2, *z2, rymax;
int    *p1_clipped, *p2_clipped;
{
   clipPS3D_in_xmax(y1,x1,z1,y2,x2,z2,rymax,p1_clipped,p2_clipped);
}

/*
 * Clip a line against zmax
 */
static void clipPS3D_in_zmax(x1,y1,z1,x2,y2,z2,rzmax,p1_clipped,p2_clipped)
double *x1, *y1, *z1, *x2, *y2, *z2, rzmax;
int    *p1_clipped, *p2_clipped;
{
   clipPS3D_in_xmax(z1,y1,x1,z2,y2,x2,rzmax,p1_clipped,p2_clipped);
}



/*
 * LABELS
 */

/*
 * Plot line labels on the side if necessary
 */
static void plotPS3D_sidelabels()
{
   short  contclip;
   double xoffset;
   int LABEL_FOUND = CNplotset_has_linelabels(plotdata);
   int FCONT_FOUND = CNplotset_has_colored_contours(plotdata);

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
 * Translation
 */

/*
 * Translate world to PS coordinates and apply log options
 * This is for local use only because it uses local static variables.
 */
static void trn_world_to_PS(x, y, z, X, Y)
double  x,  y, z;     /* Real-world coordinates               */
double *X, *Y;        /* PS-coordinates after transformation  */
{
   CNmatrix ndc_to_ps;
   CNcoord  newpt, ndcpt, point;

   /*
    * This transfo gets the coordinates in normalized device units (NDC)
    */
   point.x = x;
   point.y = y;
   point.z = z;
   ndcpt = CNtransform_point(&point,view_transfo);

   /*
    * Need one more transformation to map from unit coordinates to ps
    * coords
    */
   CNscale_window_to_viewport(ndc_to_ps,
                            0.0, 1.0, 0.0, 1.0,
                            (double)Pxmin, (double)Pxmax,
                            (double)Pymin, (double)Pymax);
   newpt = CNtransform_point(&ndcpt,ndc_to_ps);
   *X = newpt.x;
   *Y = newpt.y;
}

