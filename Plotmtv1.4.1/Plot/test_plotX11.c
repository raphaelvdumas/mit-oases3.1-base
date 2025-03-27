/*
 * test_plotX11.c - X11 routines for initializing an X11/Xlib application
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/keysym.h>
#include <X11/cursorfont.h>
#include "Bitmaps/toplvl.bmp"
#include "CNplot.h"
#include "plotX11.h"

/* colors */
#define DEFAULT_BORDER_COLOR  "Blue"
#define DEFAULT_BACK_COLOR    "MidNightBlue"
#define DEFAULT_FORE_COLOR    "White"
#define DEFAULT_MOUSE_COLOR   "White"
#define DEFAULT_ICON_COLOR    "Red"

/* plot information */
#define X_ORG            5
#define Y_ORG            5
#define X_DIM            600
#define Y_DIM            800

/* Misc */
#define DEFAULT_BORDER_WIDTH  3
#define Helv12B "-adobe-helvetica-bold-r*-120*-p-*"
#define Helv12 "-adobe-helvetica-medium-r*-120*-p-*"
#define DEFAULT_FONT          Helv12B
#define ALT_FONT              "8x13"

/* characters */
#define EVO 1
#define EVL 2
#define EVJ 3
#define EVK 4
#define EVH 5
#define EVF 6
#define EVS 7
#define EVD 8
#define EVA 9
#define EVX 10
#define EVY 11
#define EVZ 12

/*
 * X11-specific variables 
 */
static char     *font_name=NULL;                /* font for text/labels */
static char     *geometry=NULL;                 /* window geometry      */
static char     *border_color=NULL;
static char     *fore_color=NULL;
static char     *back_color=NULL;
static char     *mouse_color=NULL;
static char     *icon_color=NULL;
static int      border_width=0;                 /* window border_width */
static int      reverse=0;                      /* reverse flag        */

/*
 * Plot-control buttons
 */

#define REVERSE           0
#define NORMAL            1
#define NBUTTONS         13           /* Total no of buttons      */
#define NBUTTONS_1ST_ROW  3           /* 3 buttons on the 1st row */
#define NBUTTONS_PER_ROW  5           /* 5 buttons on 2nd/3rd row */
#define OFFSET            5
#define BUTTON_HEIGHT    20
static int         button_width[NBUTTONS];
static int         button_height[NBUTTONS];
static int         button_xoffset[NBUTTONS];
static int         button_yoffset[NBUTTONS];

/*
 * Plot-control Button defines
 */
#define QUIT_BUTTON        0
#define PRINT_BUTTON       1
#define PRINTTOFILE_BUTTON 2
#define TOGGLEVIEW_BUTTON  3
#define ZOOMIN_BUTTON      4
#define ZOOMOUT_BUTTON     5
#define FULLZOOM_BUTTON    6
#define PREVZOOM_BUTTON    7
#define VIEWCENTER_BUTTON  8 
#define VIEWLEFT_BUTTON    9 
#define VIEWRIGHT_BUTTON  10
#define VIEWUP_BUTTON     11
#define VIEWDOWN_BUTTON   12

/*
 * Plot-cycle Buttons
 */
static Window nextPlotButton=NULL;
static Window prevPlotButton=NULL;
static Window cycleInfoWindow=NULL;  /* For plotset order info */
#define CYCLE_BUTTON_WIDTH   30
#define CYCLE_BUTTON_HEIGHT  30
#define CYCLE_BUTTON_XOFFSET 10
#define CYCLE_BUTTON_YOFFSET 40

/*
 * More X11 variables
 */
static Display     *mydisplay;                 /* Display           */
static int         screen;                     /* screen            */
static Window      rtwindow;                   /* parent window     */
static Window      vwwindow;                   /* drawing window    */
static Window      buttons[NBUTTONS];          /* button windows    */
static GC          gc0, gc1, gcxor;            /* graphics context  */
static GC          bgc, bgcr;                  /* graphics context  */
static Pixmap      my_pixmap;                  /* A pixmap          */
static XFontStruct *font0, *font1;             /* font information  */
static XFontStruct *button_font;               /* font information  */
static int         win_width, win_height;      /* Window dimensions */
static XSizeHints  size_hints;                 /* window info for manager */

static int         X_xmin, X_xmax;             /* Window dimensions */
static int         X_ymin, X_ymax;             /* Window dimensions */
static int         use_pixmap=CN_TRUE;         /* Copy pixmap to window */
static int         new_session;                /* Session           */

/*
 * Procedure Declarations
 */
int                PXcreateXWindow();
static int         create_window();
static void        calc_button_dim();
static XFontStruct *load_font();
static XFontStruct *set_fonts();
static void        merge_options();
static void        do_Xloop();

static void        close_windows();
static void        paint_button();
static void        paint_cycleplot_button();
static int         check_buttons();
static void        rotate_picture();
static void        zoom_in();
static void        translate_X11_coord();
static void        translate_X113D_window();

static void        show_view();

static void        xs_watch_cursor();
static void        xs_normal_cursor();

/*
 * Postscript variables 
 */
static char   printer [CN_MAXCHAR];
static char   printCmd[CN_MAXCHAR];
static char   psfile  [CN_MAXCHAR];
static double scale;
static int    drawframe;
static int    landscape;
static int    colorps;
static int    printdate;

/*
 * Create an X11 window
 * This routine returns 1 if the X-window was successfully displayed,
 * and 0 when the window cannot be displayed.
 */
int PXcreateXWindow(pshead, pstail, progname, display_name,
                    plot_foreground, plot_background, nopixmap, 
                    x_psfile, x_printCmd, x_printer,
                    x_scale, x_drawframe, x_landscape, x_printdate, x_colorps,
                    x_geometry, x_reverse, 
                    debug)
CNpslistptr pshead, pstail;
char   *progname;              /* Program name          */
char   *display_name;          /* Display name          */
char   *plot_foreground;       /* Foreground color      */
char   *plot_background;       /* Background color      */
int    nopixmap;               /* Don't use pixmap      */
char   *x_psfile;              /* Postscript file name  */
char   *x_printCmd;            /* Postscript print cmd  */
char   *x_printer;             /* Postscript printer    */
double x_scale;                /* Postscript plot scale */
int    x_drawframe;            /* Postscript plot frame */
int    x_landscape;            /* Postscript landscape  */
int    x_printdate;            /* Postscript printdate  */
int    x_colorps;              /* Postscript color/bw   */
char   *x_geometry;            /* User-defined geometry */
int    x_reverse;              /* reverse fg/bg colors  */
int    debug;                  /* Debug flag            */
{
   int status;
   int nplots=0;

   /* Initialize */
   use_pixmap  = !nopixmap;
   if (debug && use_pixmap) 
   (void) fprintf(stdout,"Using pixmap-to-window drawing method\n");

   /* Copy the X11 options */
   geometry     = x_geometry;
   reverse      = x_reverse;

   /* Count the number of plots to be placed in the window */
   nplots = CNcount_pslists(pshead, pstail);
   if (nplots <= 0) {
      (void) fprintf(stderr, "Error! Nothing to plot!\n");
      status = 0;
      return(status);
   }

   /*
    * Try to open the window first; proceed with plot if successful
    */
   if ((status = create_window(progname,display_name,
                               plot_foreground,plot_background,
                               nplots,debug))==1) {

      /* 
       * Initialize 
       * This is used to ensure that a watch cursor gets created only once 
       */
      new_session = CN_TRUE;

      /* Copy the postscript variables to local storage */
      (void) strcpy(psfile  , x_psfile);
      (void) strcpy(printCmd, x_printCmd);
      (void) strcpy(printer , x_printer);
      scale     = x_scale;
      drawframe = x_drawframe;
      landscape = x_landscape;
      printdate = x_printdate;
      colorps   = x_colorps;

      /*
       * X procedures
       */
      (void) fprintf(stdout,"\n   Proceeding with X Plot...\n");
      do_Xloop(pshead, pstail, debug);
   }
   return(status);
}

/*
 * Create an X11 window
 */
static int create_window(progname,display_name,
                         plot_foreground,plot_background,
                         nplots, debug)
char *progname;              /* Program name     */
char *display_name;          /* Display name     */
char *plot_foreground;       /* Foreground color */
char *plot_background;       /* Background color */
int  nplots;                 /* No of plotsets   */
int  debug;                  /* Debug flag       */
{
   char        default_geometry[CN_MAXCHAR];
   XWMHints    wm_hints;                   /* window info for manager */
   XGCValues   gcvalues;
   Pixmap      icon_pixmap;
   int         x, y, width, height;
   int         i, yoffset;
   XSetWindowAttributes attributes;

   /* Open display first, and get screen info */
   if (!(mydisplay = XOpenDisplay(display_name))) {
      (void) fprintf(stderr,"\n%s:Could not open X11 display %s\n",
              progname,XDisplayName(display_name));
      return(0);
   }
   screen = DefaultScreen(mydisplay);

   /* Merge the options first */
   merge_options(progname);
   if (plot_foreground) fore_color = plot_foreground;
   if (plot_background) back_color = plot_background;

   /* Initialize the screen */
   if (!PXinitXWindow(mydisplay, screen, fore_color, back_color, reverse)) {
      (void) fprintf(stderr,"Window Color Allocation Error! Quitting now\n");
      exit(-1);
   }

   /* Default geometry */
   (void) sprintf(default_geometry,"=%dx%d+%d+%d",X_DIM,Y_DIM,X_ORG,Y_ORG);

   /* Initialize the window */
   if (!XGeometry(mydisplay,screen,geometry,default_geometry,border_width,
                 0,0,0,0,&x,&y,&width,&height)) {
      /* Usually don't get in here since default_geometry is set */
      x      = X_ORG;
      y      = Y_ORG;
      width  = X_DIM;
      height = Y_DIM;
   }
   rtwindow = XCreateSimpleWindow(mydisplay, RootWindow(mydisplay,screen),
                                x, y, width, height, border_width,
                                foreground_pixel, background_pixel);

   /*
    * Make plot-control buttons.
    * The number of buttons depend on the number of plotsets.
    *
    * There are a total of 11 buttons :
    *    Quit + Message            | Print    | Print to File
    *    2D/3D  | ZoomIn | ZoomOut | Fullzoom | PrevZoom/Hiddenline
    *    Center | Left   | Right   | Up       | Down 
    */
   /* Calculate the button dimensions */
   calc_button_dim(width, 
                   button_width, button_height,
                   button_xoffset, button_yoffset);
   /* Create the buttons */
   for (i=0; i<NBUTTONS; i++) {
      buttons[i]= XCreateSimpleWindow(mydisplay, rtwindow,
                     button_xoffset[i], button_yoffset[i],
                     button_width[i],   button_height[i], 1,
                     foreground_pixel, background_pixel);
   }

   /* Create the drawing window */
   yoffset = button_yoffset[NBUTTONS-1]+button_height[NBUTTONS-1] + OFFSET;
   vwwindow = XCreateSimpleWindow(mydisplay, rtwindow,
              OFFSET, yoffset, 
              width-2*OFFSET, height-(yoffset+OFFSET), 1,
              foreground_pixel, background_pixel);

   /* Create 2 buttons at the bottom of the window for cycling thru plots */
   if (nplots > 1) {
      /* Create these buttons only if there are 2 or more plotsets */
      prevPlotButton = XCreateSimpleWindow(mydisplay, rtwindow,
                       CYCLE_BUTTON_XOFFSET, 
                       height-CYCLE_BUTTON_YOFFSET,
                       CYCLE_BUTTON_WIDTH, CYCLE_BUTTON_HEIGHT, 1, 
                       foreground_pixel, background_pixel);
      cycleInfoWindow= XCreateSimpleWindow(mydisplay, rtwindow,
                       CYCLE_BUTTON_XOFFSET + CYCLE_BUTTON_WIDTH + 3, 
                       height-CYCLE_BUTTON_YOFFSET,
                       2*CYCLE_BUTTON_WIDTH, CYCLE_BUTTON_HEIGHT, 1, 
                       foreground_pixel, background_pixel);
      nextPlotButton = XCreateSimpleWindow(mydisplay, rtwindow,
                       CYCLE_BUTTON_XOFFSET + 3*CYCLE_BUTTON_WIDTH + 6, 
                       height-CYCLE_BUTTON_YOFFSET,
                       CYCLE_BUTTON_WIDTH, CYCLE_BUTTON_HEIGHT, 1, 
                       foreground_pixel, background_pixel);
   }

   /* Initialize size hint property for window manager */
   size_hints.flags      = PPosition | PSize | PMinSize;
   size_hints.x          = x;
   size_hints.y          = y;
   size_hints.width      = width;
   size_hints.height     = height;
   size_hints.min_width  = 300;
   size_hints.min_height = 300;

   /* Create pixmap of depth 1 (bitmap) for icon */
   icon_pixmap = XCreateBitmapFromData(mydisplay, rtwindow, 
                        toplvl_bits, toplvl_width, toplvl_height);

   /* set properties for window manager */
   XSetStandardProperties(mydisplay, rtwindow, "Plotmtv Program", "Plotmtv",
                          icon_pixmap, NULL, 0, &size_hints);

   /* Check backing-store, save under */
   if (DoesBackingStore(DefaultScreenOfDisplay(mydisplay))) {
      if (debug)
      (void) fprintf(stdout,"Backing Store has been enabled...\n");
      attributes.backing_store = Always; 
      XChangeWindowAttributes(mydisplay, rtwindow, CWBackingStore,
                              &attributes);
   }
   if (DoesSaveUnders(DefaultScreenOfDisplay(mydisplay))) {
      if (debug)
      (void) fprintf(stdout,"Save Under has been enabled...\n");
      attributes.save_under = True; 
      XChangeWindowAttributes(mydisplay, rtwindow, CWSaveUnder,
                              &attributes);
   }

   /* Initialize input hint property for window manager */
   wm_hints.flags = InputHint;
   wm_hints.input = True;
   XSetWMHints(mydisplay, rtwindow, &wm_hints);

   /* Select event types wanted */
   XSelectInput(mydisplay,rtwindow,
                ExposureMask |
                ButtonPressMask | ButtonMotionMask | ButtonReleaseMask | 
                KeyPressMask | StructureNotifyMask );
   XSelectInput(mydisplay,vwwindow,
                ExposureMask |
                ButtonPressMask | ButtonMotionMask | ButtonReleaseMask | 
                KeyPressMask );
   for (i=0; i<NBUTTONS; i++) {
      XSelectInput(mydisplay,buttons[i],ExposureMask | ButtonPressMask  |
                   ButtonReleaseMask);
   }
   if (prevPlotButton)
      XSelectInput(mydisplay,prevPlotButton ,ExposureMask | ButtonPressMask  |
                   ButtonReleaseMask);
   if (cycleInfoWindow)
      XSelectInput(mydisplay,cycleInfoWindow,ExposureMask | ButtonPressMask  |
                   ButtonReleaseMask);
   if (nextPlotButton)
      XSelectInput(mydisplay,nextPlotButton ,ExposureMask | ButtonPressMask  |
                   ButtonReleaseMask);

   if (use_pixmap) {
      /* Create a pixmap and GC for storing the main picture */
      my_pixmap = XCreatePixmap(mydisplay, RootWindow(mydisplay,screen), 
                                DisplayWidth (mydisplay,screen),
                                DisplayHeight(mydisplay,screen),
                                DisplayPlanes(mydisplay,screen));
   } else {
      my_pixmap = NULL;
   }

   /* Create GC for text and drawing */
   gc0 = XCreateGC(mydisplay, vwwindow, 0L, &gcvalues);
   gc1 = XCreateGC(mydisplay, vwwindow, 0L, &gcvalues);

   /* create 2 graphics contexts for buttons */
   bgc  = XCreateGC(mydisplay, rtwindow, 0L, &gcvalues);
   XSetForeground(mydisplay, bgc, background_pixel);
   bgcr = XCreateGC(mydisplay, rtwindow, 0L, &gcvalues);
   XSetForeground(mydisplay, bgcr, foreground_pixel);

   /* Create GC for rubber-banding */
   gcvalues.function = GXxor;
   gcxor = XCreateGC(mydisplay, vwwindow, GCFunction, &gcvalues);
   XSetForeground(mydisplay, gcxor, foreground_pixel ^ background_pixel);

   /* Find fonts and load it */
   font0 = set_fonts(mydisplay, gc0, font_name, ALT_FONT);
   font1 = set_fonts(mydisplay, gc1, "6x10",    ALT_FONT);
   if ((font0 == NULL) || (font1 == NULL)) {
      (void) fprintf(stderr,"\nSorry - couldn't load any X11 fonts\n");
      (void) fprintf(stderr,"Stopping X11 Plot.\n");
      return(0);
   }

   /* Set the fonts on the button GCs */
   button_font = set_fonts(mydisplay, bgc, Helv12, ALT_FONT);
   if (button_font == NULL) {
      (void) fprintf(stderr,"\nSorry - couldn't load control panel fonts\n");
      (void) fprintf(stderr,"Stopping X11 Plot.\n");
      return(0);
   }
   XSetFont(mydisplay, bgc, button_font->fid);
   XSetFont(mydisplay, bgcr, button_font->fid);

   /* specify foreground */
   XSetForeground(mydisplay, gc0, foreground_pixel);
   XSetForeground(mydisplay, gc1, foreground_pixel);

   /* Display the window */
   XMapSubwindows(mydisplay, rtwindow);
   XMapWindow(mydisplay, rtwindow);

   /* save the width and height fields */
   win_width  = width;
   win_height = height;

   /* return */
   return(1);
}

/* 
 * Calculate the position and width of buttons
 */
static void calc_button_dim(window_width,
                            button_width, button_height,
                            button_xoffset, button_yoffset)
int window_width;
int button_width[];
int button_height[];
int button_xoffset[];
int button_yoffset[];
{
   int btn_width, btn_height, offset, row, column, i, width_multiple;

   /* initialize */
   offset = OFFSET;
   btn_width  = (window_width - 2*offset) / NBUTTONS_PER_ROW;
   btn_height = BUTTON_HEIGHT; 

   column = 0;
   row    = 0;
   i      = 0;

   /* The first row is treated specially */
   while (i<NBUTTONS_1ST_ROW) {
      if (i==0) 
         width_multiple = NBUTTONS_PER_ROW - NBUTTONS_1ST_ROW + 1;
      else
         width_multiple = 1;
      button_width[i]   = btn_width*width_multiple;
      button_height[i]  = btn_height;
      button_xoffset[i] = offset + column*btn_width;
      button_yoffset[i] = offset + row*btn_height;
      column += width_multiple;
      i++;
   }

   /* Now do the rest of the columns */
   column = 0;
   row    = 1; 
   while (i<NBUTTONS) {
      width_multiple = 1;
      button_width[i]   = btn_width*width_multiple;
      button_height[i]  = btn_height;
      button_xoffset[i] = offset + column*btn_width;
      button_yoffset[i] = offset + row*btn_height;
      column += width_multiple;
      if (column >= NBUTTONS_PER_ROW) {
         column = 0;
         row    += 1;
      }
      i++;
   }

   /*
   for (i=0; i<NBUTTONS; i++) {
      (void) printf("Button %d : x=%3d y=%3d width=%3d height=%3d\n",i,
                    button_xoffset[i], button_yoffset[i], 
                    button_width[i], button_height[i]);
   }
    */
}

/*
 * Set the font in a gc
 */
static XFontStruct *set_fonts(display, gc, fontname, alt_font)
Display *display;
GC      gc;
char    *fontname;
char    *alt_font;
{
   XFontStruct *font_info;
   char        *default_font = "vtbold";

   if (!(font_info = load_font(display,fontname))){
      (void) fprintf(stderr,
             "Trying Alternate X11 font \"%s\"\n",alt_font);
      if (!(font_info = load_font(display,alt_font))){
         (void) fprintf(stderr,
                "Trying Alternate X11 font \"%s\"\n",default_font);
         if (!(font_info = load_font(display,default_font))){
            (void) fprintf(stderr,"Error! Couldn't load any fonts!\n");
            return(NULL);
         }
      }
   }
   XSetFont(display, gc, font_info->fid);
   return(font_info);
}

/*
 * Load a font
 */
static XFontStruct *load_font(display, fontname)
Display *display;
char    *fontname;
{
   XFontStruct *font_info = NULL;

   /* find a font and load it */
   font_info = XLoadQueryFont(display,fontname);
   if (font_info == NULL)
      (void) fprintf(stderr,
             "\nWarning : Could not open X11 font \"%s\"\n",fontname);

   /* return the font info */
   return(font_info);
}

/* 
 * Initialize and merge the various options 
 */
static void merge_options(progname)
char *progname;
{
   char    *option;

   /* process the various options */
   if (reverse < 0) {
      if ((option = XGetDefault(mydisplay,progname,"ReverseVideo")) != NULL)
         if (strcmp(option,"on")) reverse = 1;
   }
   if (!geometry) {
      option = XGetDefault(mydisplay,progname,"Geometry");
      geometry = option ? option : "=+5+5";
   }
   if (!font_name) {
      /* option = XGetDefault(mydisplay,progname,"BodyFont"); */
      font_name = DEFAULT_FONT;
   }
   if (!border_color) {
      option = XGetDefault(mydisplay,progname,"Border");
      border_color = option ? option : DEFAULT_BORDER_COLOR;
   }
   if (!back_color) {
      option = XGetDefault(mydisplay,progname,"Background");
      back_color = option ? option : DEFAULT_BACK_COLOR;
   }
   if (!fore_color) {
      option = XGetDefault(mydisplay,progname,"Foreground");
      fore_color = option ? option : DEFAULT_FORE_COLOR;
   }
   if (!mouse_color) {
      option = XGetDefault(mydisplay,progname,"Mouse");
      mouse_color = option ? option : DEFAULT_MOUSE_COLOR;
   }
   if (!icon_color) {
      option = XGetDefault(mydisplay,progname,"Icon");
      icon_color = option ? option : DEFAULT_ICON_COLOR;
   }
   if (border_width <= 0) {
      option = XGetDefault(mydisplay,progname,"BorderWidth");
      border_width = option ? atoi(option) : DEFAULT_BORDER_WIDTH;
   }
}


/*
 * enter X11 Loop
 */
/*ARGSUSED*/
static void do_Xloop(pshead, pstail, debug)
CNpslistptr pshead, pstail;
int          debug;
{
#  define   SMALLW 1
#  define   OK     0
   XEvent   event;
   KeySym   keysym;
   XComposeStatus compose;
   char     buffer[1];
   int      bufsize = 1;
   int      window_size = OK;
   int      Width, Height;       
   int      VwWidth, VwHeight;
   int      CrWidth, CrHeight;
   int      yoffset;
   int      i, close_down;
   unsigned int button;
   int      start_x, start_y, last_x, last_y;
   int      redraw;
   int      button_is_pressed=CN_FALSE;
   CNcoord  eyepos;
   CNplotsetptr pptr;
   CNpslistptr  psptr;
   int          plotno, nplots;

   /* Initialize the active plotset and plotset link */
   psptr  = pshead; 
   pptr   = psptr->Pptr;
   plotno = 1;
   nplots = CNcount_pslists(pshead, pstail);

   /*
    * When the window is first mapped, a ConfigureNotify event
    * is generated, followed by an Expose event.
    */
   if (debug) {
   (void) printf("root window = %d\n",rtwindow);
   (void) printf("view window = %d\n",vwwindow);
   for (i=0; i<NBUTTONS; i++)
   (void) printf("button %2d = %d\n",i, buttons[i]);
   }

   /* Initialize window size */
   Width    = win_width;
   Height   = win_height;
   CrWidth  = Width;
   CrHeight = Height;
   yoffset  = button_yoffset[NBUTTONS-1]+button_height[NBUTTONS-1] + OFFSET;
   VwWidth  = win_width - 2*OFFSET;
   VwHeight = win_height - (yoffset + OFFSET);
   X_xmin   = 0;
   X_xmax   = VwWidth;
   X_ymin   = 0;
   X_ymax   = VwHeight;

   /* Drawing flag */
   redraw   = CN_TRUE;

   /* Initial eye-position */
   eyepos   = pptr->view_pr->eyepos;

   /* event loop */
   while(1) {
      XNextEvent(mydisplay, &event);
      switch (event.type) {
      case Expose:
         /*
          * Window is exposed - redraw the picture
          */
         if (debug)
         (void) printf("Expose (count=%d window=%d)\n",
                       event.xexpose.count, event.xexpose.window);

         /* Unless this is the last contiguous expose, don't draw window */
         if (event.xexpose.count != 0)
            break;

         /* Paint the buttons */
         for (i=0; i<NBUTTONS; i++)
            if (event.xexpose.window == buttons[i])
               paint_button(buttons[i],NORMAL,i,pptr);

         /* Paint the nextPlot and prevPlot buttons */
         if (prevPlotButton && (event.xexpose.window == prevPlotButton))
             paint_cycleplot_button(prevPlotButton, NORMAL, plotno, nplots);
         if (cycleInfoWindow && (event.xexpose.window == cycleInfoWindow))
             paint_cycleplot_button(cycleInfoWindow, NORMAL, plotno, nplots);
         if (nextPlotButton && (event.xexpose.window == nextPlotButton))
             paint_cycleplot_button(nextPlotButton, NORMAL, plotno, nplots);

         /* Only do the rest for the view window */
         if (event.xexpose.window != vwwindow)
            break;

         /* get rid of all other Expose events on the queue */
         /*EMPTY*/
         while (XCheckTypedEvent(mydisplay, Expose, &event));

         /* window was resized too small to use */
         if (window_size == SMALLW) {
            Width    = X_DIM;
            Height   = X_DIM;
            yoffset  = button_yoffset[NBUTTONS-1]+button_height[NBUTTONS-1] +
                       OFFSET;
            VwWidth  = Width - 2*OFFSET;
            VwHeight = Height - (yoffset + OFFSET);
         }

         /* Draw the picture */
         show_view(pptr, &redraw, VwWidth, VwHeight, debug);

         /* Paint the buttons */
         for (i=0; i<NBUTTONS; i++)
            paint_button(buttons[i],NORMAL,i,pptr);

         /* Paint the cycle-plot buttons */
         if (prevPlotButton)
         paint_cycleplot_button(prevPlotButton,  NORMAL, plotno, nplots);
         if (cycleInfoWindow)
         paint_cycleplot_button(cycleInfoWindow, NORMAL, plotno, nplots);
         if (nextPlotButton)
         paint_cycleplot_button(nextPlotButton,  NORMAL, plotno, nplots);

         break;

      case ConfigureNotify:
         /*
          * Window has been resized - change width and height
          * This only affects the root window
          */
         if (debug)
         (void) printf("Configure-Notify (window=%d)\n",
                       event.xconfigure.window);

         /* Get the configured width and height */
         Width    = event.xconfigure.width;
         Height   = event.xconfigure.height;

         /* Check with current dimensions */
         if ((Width == CrWidth) && (Height == CrHeight))
            break;

         /* Reset the width and height */
         CrWidth  = Width;
         CrHeight = Height;
         if ((Width < size_hints.min_width) ||
             (Height < size_hints.min_height) ) 
            window_size = SMALLW;
         else 
            window_size = OK;

         /* Resize the buttons */
         calc_button_dim(Width, button_width, button_height,
                                button_xoffset, button_yoffset);
         for (i=0; i<NBUTTONS; i++)
            XMoveResizeWindow(mydisplay,buttons[i],
                              button_xoffset[i], button_yoffset[i],
                              button_width[i], button_height[i]);

         /* Resize the drawing window */
         yoffset  = button_yoffset[NBUTTONS-1]+button_height[NBUTTONS-1] +
                    OFFSET;
         VwWidth  = Width - 2*OFFSET;
         VwHeight = Height - (yoffset + OFFSET); 
         XMoveResizeWindow(mydisplay,vwwindow,
                           OFFSET,yoffset,
                           VwWidth, VwHeight);

         /* Resize the cycle-plot buttons */
         if (prevPlotButton) 
             XMoveResizeWindow(mydisplay, prevPlotButton,
                               CYCLE_BUTTON_XOFFSET,
                               Height-CYCLE_BUTTON_YOFFSET,
                               CYCLE_BUTTON_WIDTH, CYCLE_BUTTON_HEIGHT);
         if (cycleInfoWindow)
             XMoveResizeWindow(mydisplay, cycleInfoWindow,
                               CYCLE_BUTTON_XOFFSET + CYCLE_BUTTON_WIDTH + 3,
                               Height-CYCLE_BUTTON_YOFFSET,
                               2*CYCLE_BUTTON_WIDTH, CYCLE_BUTTON_HEIGHT);
         if (nextPlotButton)
             XMoveResizeWindow(mydisplay, nextPlotButton,
                               CYCLE_BUTTON_XOFFSET + 3*CYCLE_BUTTON_WIDTH + 6,
                               Height-CYCLE_BUTTON_YOFFSET,
                               CYCLE_BUTTON_WIDTH, CYCLE_BUTTON_HEIGHT);

         /* Free the old pixmap and create one the size of the window */
         if (use_pixmap) {
            if (my_pixmap) XFreePixmap(mydisplay, my_pixmap);
            my_pixmap = XCreatePixmap(mydisplay,
                                      RootWindow(mydisplay, screen),
                                      VwWidth, VwHeight, 
                                      DisplayPlanes(mydisplay,screen));
            XSetForeground(mydisplay, gc0, background_pixel);
            XFillRectangle(mydisplay, my_pixmap, gc0, 0, 0, VwWidth, VwHeight);
            XSetForeground(mydisplay, gc0, foreground_pixel);
         }

         /*
          * get rid of all other ConfigureNotify events on the queue
          * not really necc, but used 'cos XMoveResizeWindow() generates
          * ConfigureNotify events
          */
         /*EMPTY*/
         while (XCheckTypedEvent(mydisplay, ConfigureNotify, &event));

         /* Reset the draw-flag */
         redraw   = CN_TRUE;

         break;

      case KeyPress:
         /*
          * A key (on the keyboard) is pressed
          */
         XLookupString(&event,buffer,bufsize,&keysym,&compose);
         if ((keysym == XK_q) || (keysym == XK_Q)) {

            /* Quit */
            close_windows();
            return;

         } else if ((keysym == XK_n) || (keysym == XK_N)) {

            /* Cycle to the next plotset */
            if (psptr->next != NULL) {
               /* Reset the pointers */
               psptr = psptr->next;
               pptr  = psptr->Pptr;
               plotno++;

               /* Reset the eye position
               eyepos = pptr->view_pr->eyepos;

               /* Force redraw */
               XClearArea(mydisplay,vwwindow,0,0,VwWidth,VwHeight,True);

               /* Reset the draw-flag */
               redraw   = CN_TRUE;
            }

         } else if ((keysym == XK_p) || (keysym == XK_P)) {

            /* Cycle to the prev plotset */
            if (psptr->prev != NULL) {
               /* Reset the pointers */
               psptr = psptr->prev;
               pptr  = psptr->Pptr;
               plotno--;

               /* Reset the eye position
               eyepos = pptr->view_pr->eyepos;

               /* Force redraw */
               XClearArea(mydisplay,vwwindow,0,0,VwWidth,VwHeight,True);

               /* Reset the draw-flag */
               redraw   = CN_TRUE;
            }

         } else if ((keysym == XK_l) || (keysym == XK_L))
            rotate_picture(pptr,&eyepos,EVL,VwWidth,VwHeight,&redraw);
         else if ((keysym == XK_k) || (keysym == XK_K))
            rotate_picture(pptr,&eyepos,EVK,VwWidth,VwHeight,&redraw);
         else if ((keysym == XK_j) || (keysym == XK_J))
            rotate_picture(pptr,&eyepos,EVJ,VwWidth,VwHeight,&redraw);
         else if ((keysym == XK_h) || (keysym == XK_H))
            rotate_picture(pptr,&eyepos,EVH,VwWidth,VwHeight,&redraw);
         else if ((keysym == XK_o) || (keysym == XK_O))
            rotate_picture(pptr,&eyepos,EVO,VwWidth,VwHeight,&redraw);
         else if ((keysym == XK_f) || (keysym == XK_F))
            rotate_picture(pptr,&eyepos,EVF,VwWidth,VwHeight,&redraw);
         else if ((keysym == XK_d) || (keysym == XK_D))
            rotate_picture(pptr,&eyepos,EVD,VwWidth,VwHeight,&redraw);
         else if ((keysym == XK_s) || (keysym == XK_S))
            rotate_picture(pptr,&eyepos,EVS,VwWidth,VwHeight,&redraw);
         else if ((keysym == XK_a) || (keysym == XK_A))
            rotate_picture(pptr,&eyepos,EVA,VwWidth,VwHeight,&redraw);
         else if ((keysym == XK_x) || (keysym == XK_X))
            rotate_picture(pptr,&eyepos,EVX,VwWidth,VwHeight,&redraw);
         else if ((keysym == XK_y) || (keysym == XK_Y))
            rotate_picture(pptr,&eyepos,EVY,VwWidth,VwHeight,&redraw);
         else if ((keysym == XK_z) || (keysym == XK_Z))
            rotate_picture(pptr,&eyepos,EVZ,VwWidth,VwHeight,&redraw);
         break;

      case ButtonPress:
         /*
          * A mouse button is pressed
          */
         /* Check for button presses */
         if (check_buttons(&event, pptr, &eyepos, &close_down, 
                           VwWidth, VwHeight, &redraw, debug)) {
            if (close_down) {
               /* Close the windows and return */
               close_windows();
               return;
            }
            break;
         }

         /*
          * Check to see if the button was pressed in one of the cycle-plot
          * windows 
          */
         if (prevPlotButton && (event.xbutton.window == prevPlotButton)) {
            paint_cycleplot_button(prevPlotButton,REVERSE, plotno, nplots);
            button = NULL;

            /* Cycle to the prev plotset */
            if (psptr->prev != NULL) {

               /* Set the plotset to the previous plotset in the list */
               psptr = psptr->prev;
               pptr  = psptr->Pptr;
               plotno--;
 
               /* Reset the eye position
               eyepos = pptr->view_pr->eyepos;

               /* Force redraw */
               XClearArea(mydisplay,vwwindow,0,0,VwWidth,VwHeight,True);
 
               /* Reset the draw-flag */
               redraw   = CN_TRUE;
            }

            break;
         }
         if (cycleInfoWindow && (event.xbutton.window == cycleInfoWindow)) {
            /* Don't do anything */
            button = NULL;
            break;
         }
         if (nextPlotButton && (event.xbutton.window == nextPlotButton)) {
            paint_cycleplot_button(nextPlotButton,REVERSE, plotno, nplots);
            button = NULL;

            /* Cycle to the next plotset */
            if (psptr->next != NULL) {

               /* Set the plotset to the next plotset in the list */
               psptr = psptr->next;
               pptr  = psptr->Pptr;
               plotno++;
 
               /* Reset the eye position
               eyepos = pptr->view_pr->eyepos;

               /* Force redraw */
               XClearArea(mydisplay,vwwindow,0,0,VwWidth,VwHeight,True);
 
               /* Reset the draw-flag */
               redraw   = CN_TRUE;
            }

            break;
         }

         /* start the rubberband */
         button_is_pressed=CN_TRUE;
         button  = event.xbutton.button;
         start_x = event.xbutton.x;
         start_y = event.xbutton.y;
         last_x  = event.xbutton.x;
         last_y  = event.xbutton.y;
         if (debug) (void) printf("Button press\n");
         PXDrawRectangle(mydisplay, vwwindow, gcxor, 
                        start_x, start_y, last_x-start_x, last_y-start_y);
         break;

      case MotionNotify:
         /*
          * The pointer is moved while the mouse button is pressed
          * The button_is_pressed flag is used to indicate if the
          * button is pressed. On some machines, the motion-notify will
          * result in line-drawing even if the mouse button is not pressed.
          */
         if (button_is_pressed) {
            /* Erase the previous figure */
            if (debug) (void) printf("Button motions\n");
            PXDrawRectangle(mydisplay, vwwindow, gcxor, 
                           start_x, start_y, last_x-start_x, last_y-start_y);
            /* Update the last point */
            last_x = event.xbutton.x;
            last_y = event.xbutton.y;
            /* Redraw */
            PXDrawRectangle(mydisplay, vwwindow, gcxor, 
                           start_x, start_y, last_x-start_x, last_y-start_y);
         }
         break;

      case ButtonRelease:
         /*
          * The mouse button is released
          */
         if (event.xbutton.button == button) {
            /* Erase the previous figure */
            if (debug) (void) printf("Button release\n");
            button_is_pressed = CN_FALSE;
            PXDrawRectangle(mydisplay, vwwindow, gcxor, 
                           start_x, start_y, last_x-start_x, last_y-start_y);
            /* Update the last point */
            last_x = event.xbutton.x;
            last_y = event.xbutton.y;
            /* Redraw */
            PXDrawRectangle(mydisplay, vwwindow, gc, 
                           start_x, start_y, last_x-start_x, last_y-start_y);
            zoom_in(pptr, button, start_x, start_y, last_x, last_y, 
                    VwWidth, VwHeight, &redraw);
         }
         break;

      default:
         break;
      } /* end switch */
   } /* end while */
}

/*
 * close down
 */
static void close_windows()
{
   XUnloadFont(mydisplay, font0->fid);
   XUnloadFont(mydisplay, font1->fid);
   XUnloadFont(mydisplay, button_font->fid);
   XFreeGC(mydisplay, gc0);
   XFreeGC(mydisplay, gc1);
   XFreeGC(mydisplay, gcxor);
   XFreeGC(mydisplay, bgc);
   XFreeGC(mydisplay, bgcr);
   XCloseDisplay(mydisplay);
}

/*
 * paint the buttons
 */
static void paint_button(win, mode, i, pptr)
Window win;
int    mode, i;
CNplotsetptr pptr;
{
   GC   button_gc;
   char button_label[100];
   double theta, phi;
   int  font_height, text_ypos;

   font_height= button_font->max_bounds.ascent +
                button_font->max_bounds.descent;
   text_ypos  = BUTTON_HEIGHT/2 + (int)(0.5*font_height) - 2;

   if (mode == REVERSE) {
      XSetWindowBackground(mydisplay,win,foreground_pixel);
      button_gc = bgc;
   } else {
      XSetWindowBackground(mydisplay,win,background_pixel);
      button_gc = bgcr;
   }

   /*
    * clearing repaints the background,
    * but does NOT generate exposure events
    */
   XClearWindow(mydisplay,buttons[i]);

   /* Default label */
   (void) strcpy(button_label,"");

   /* assign button labels */
   switch (i) {
   case QUIT_BUTTON :
             /* Quit + Message */
             if (pptr->plottype == CN_PLOT3D) {
                /* Get the view angles */
                CNget_view_angles(pptr->view_pr, &theta, &phi);
                (void) sprintf(button_label,
                               "Quit :  Theta = %.3g   Phi = %.3g",theta,phi);
             } else {
                (void) strcpy(button_label,"Quit");
             }
             break;
   case PRINT_BUTTON :
             /* Print */
             (void) strcpy(button_label,"Print"); 
             break;
   case PRINTTOFILE_BUTTON :
             /* Print to File */
             (void) strcpy(button_label,"Print to File"); 
             break;
   case TOGGLEVIEW_BUTTON :
             /* 2D/3D Toggle */
             if (pptr->plottype == CN_PLOT3D)
                (void) strcpy(button_label,"3D Plot"); 
             else
                (void) strcpy(button_label,"2D Plot");
             break;
   case ZOOMIN_BUTTON :
             /* Zoom In */
             (void) strcpy(button_label,"Zoom In"); 
             break;
   case ZOOMOUT_BUTTON :
             /* Zoom Out */
             (void) strcpy(button_label,"Zoom Out"); 
             break;
   case FULLZOOM_BUTTON :
             /* Full Zoom */
             (void) strcpy(button_label,"Full Zoom"); 
             break;
   case PREVZOOM_BUTTON :
             /* Prevzoom/Hiddenline */
             if (pptr->plottype == CN_PLOT3D) {
                /* Hiddenline */
                if (pptr->view_pr->hiddenline)
                   (void) strcpy(button_label,"Hidden Lines");
                else
                   (void) strcpy(button_label,"Wire Frame");
             } else {
                /* Previous zoom */
                (void) strcpy(button_label,"Previous Zoom"); 
             }
             break;
   case VIEWCENTER_BUTTON :
             if (pptr->plottype == CN_PLOT3D) 
             (void)strcpy(button_label,"Center"); 
             else {
                if (pptr->plot_pr.xlog)
                (void)strcpy(button_label,"x-log"); 
                else
                (void)strcpy(button_label,"x-linear"); 
             }
             break;
   case VIEWLEFT_BUTTON :
             if (pptr->plottype == CN_PLOT3D) 
             (void)strcpy(button_label,"Left"  ); 
             else {
                if (pptr->plot_pr.ylog)
                (void)strcpy(button_label,"y-log"); 
                else
                (void)strcpy(button_label,"y-linear"); 
             }
             break;
   case VIEWRIGHT_BUTTON :
             if (pptr->plottype == CN_PLOT3D) 
             (void)strcpy(button_label,"Right" ); 
             else {
                if (pptr->plot_pr.zlog)
                (void)strcpy(button_label,"z-log"); 
                else
                (void)strcpy(button_label,"z-linear"); 
             }
             break;
   case VIEWUP_BUTTON :
             if (pptr->plottype == CN_PLOT3D) 
             (void)strcpy(button_label,"Up"    ); 
             else {
                if (pptr->plot_pr.equalscale)
                (void)strcpy(button_label,"Equal Scales"); 
                else
                (void)strcpy(button_label,"Fit to Page"); 
             }
             break;
   case VIEWDOWN_BUTTON :
             if (pptr->plottype == CN_PLOT3D) 
             (void)strcpy(button_label,"Down"  ); 
             else {
                if (pptr->plot_pr.grid)
                (void)strcpy(button_label,"Grid=On"); 
                else
                (void)strcpy(button_label,"Grid=Off"); 
             }
             break;
   default : (void)fprintf(stderr,
                           "Error! - Button %d has not been defined!\n",i);
             (void)strcpy(button_label,"");
             break;
   }

   /* draw text */
   XDrawString(mydisplay,buttons[i],button_gc,
               4, text_ypos, button_label,
               strlen(button_label));
}

/*
 * paint the nextPlot button
 */
static void paint_cycleplot_button(win, mode, plotno, nplots)
Window win;
int    mode;
int    plotno, nplots;
{
   XPoint points[10];
   int    npoints;
   GC     button_gc;
   char   text[100];
   int    text_len, text_width, text_xpos, text_ypos, font_height;
   int    i;
 
   /* Reset the mode */
   if ((win == prevPlotButton) && (plotno == 1))
      mode = REVERSE;
   if ((win == nextPlotButton) && (plotno == nplots))
      mode = REVERSE;

   if (mode == REVERSE) {
      XSetWindowBackground(mydisplay,win,foreground_pixel);
      button_gc = bgc;
   } else {
      XSetWindowBackground(mydisplay,win,background_pixel);
      button_gc = bgcr;
   }
 
   /*
    * clearing repaints the background,
    * but does NOT generate exposure events
    */
   XClearWindow(mydisplay, win);
   
   if (win == cycleInfoWindow) {
      /* Set the font on this GC */
      XSetFont(mydisplay, button_gc, font0->fid);

      /* just draw text */
      (void) sprintf(text, "%d of %d", plotno, nplots);
      text_len   = strlen(text);
      text_width = XTextWidth(font0, text, text_len);
      text_xpos  = (2*CYCLE_BUTTON_WIDTH - text_width) / 2;
      if (text_xpos <= 0) text_xpos = 1;
      font_height= font0->max_bounds.ascent + font0->max_bounds.descent;
      text_ypos  = CYCLE_BUTTON_HEIGHT/2 + (int)(0.5*font_height);
      XDrawString(mydisplay, win, button_gc,
                  text_xpos, text_ypos, text, strlen(text));

      /* Reset the font on this GC */
      XSetFont(mydisplay, button_gc, button_font->fid);

      return;
   }
 
   /* Draw an arrow (pointing down or up) */
   npoints = 8;
   points[0].x = 10;   points[0].y =  5;
   points[1].x = 10;   points[1].y = 15;
   points[2].x =  5;   points[2].y = 15;
   points[3].x = 15;   points[3].y = 25;
   points[4].x = 25;   points[4].y = 15;
   points[5].x = 20;   points[5].y = 15;
   points[6].x = 20;   points[6].y =  5;
   points[7].x = 10;   points[7].y =  5;

   /* If this is the nextPlot window, flip the y-values */
   if (win == prevPlotButton) {
      for (i=0; i<npoints; i++) 
         points[i].y = CYCLE_BUTTON_HEIGHT - points[i].y;
   }

   /* Fill the polygon */
   XFillPolygon(mydisplay, win, button_gc, 
                points, npoints, Nonconvex, CoordModeOrigin);
}


/*
 * Check for button press 
 * Use XClearArea() to generate an Expose event that will cause the picture
 * to redraw
 */
static int check_buttons(event, pptr, eyepos, 
                         close_down, VwWidth, VwHeight, redraw, debug)
XEvent       *event;
CNplotsetptr pptr;
CNcoord      *eyepos;
int          *close_down;
int          VwWidth, VwHeight;
int          *redraw;
int          debug;
{
   int i, match=CN_FALSE;
   unsigned int button;
   double xmin, xmax, ymax, ymin, zmin, zmax;
   double prev_vxmin, prev_vxmax, prev_vymin, prev_vymax;
   double mult, umult;
   char   error_message[CN_MAXCHAR];
   int    prtplot;

   /* Reinitialize */
   *close_down = CN_FALSE;

   /* Zoom and unzoom multiplers */
   mult = 0.25;
   umult = mult/(1.0-2.0*mult);

   /* Go thru the buttons */
   for (i=0; i<NBUTTONS; i++) {
      if (event->xbutton.window == buttons[i]) {
         match = CN_TRUE;
         paint_button(buttons[i],REVERSE,i,pptr);
         button = event->xbutton.button;

         /* get the matching button release on the same button */
         while (1) {
            /*EMPTY*/
            while (XCheckTypedEvent(mydisplay,ButtonPress,event));

            /* wait for release; if on correct button, exit */
            XMaskEvent(mydisplay, ButtonReleaseMask, event);
            if (event->xbutton.button == button) {
               paint_button(buttons[i],NORMAL,i,pptr);
               break;
            }
         }

         /* Act on the request */
         switch(i) {
         case QUIT_BUTTON :
                  /* Quit */
                  *close_down = CN_TRUE;
                  break;

         case PRINT_BUTTON :
                  /* Print to Printer */
                  prtplot = CN_TRUE;
                  PXplotps(psfile, printCmd, printer,
                           scale, drawframe,
                           landscape, printdate, PX_EPSI, colorps,
                           1, prtplot, pptr, error_message, debug);
                  break;

         case PRINTTOFILE_BUTTON :
                  /* Print to File */
                  prtplot = CN_FALSE;
                  PXplotps(psfile, printCmd, printer,
                           scale, drawframe,
                           landscape, printdate, PX_EPSI, colorps,
                           1, prtplot, pptr, error_message, debug);
                  break;

         case TOGGLEVIEW_BUTTON :
                  /* Toggle */
                  if (pptr->plottype == CN_PLOT2D)
                     pptr->plottype = CN_PLOT3D;
                  else             
                     pptr->plottype = CN_PLOT2D;
                  XClearArea(mydisplay,vwwindow,0,0,Width,Height,True);
                  *redraw = CN_TRUE;
                  break;

         case ZOOMIN_BUTTON :
                  /* Zoom in */
                  if (pptr->plottype == CN_PLOT3D) {
                    
                     xmin = pptr->view_pr->windscl_xl;
                     xmax = pptr->view_pr->windscl_xr;
                     ymin = pptr->view_pr->windscl_yb;
                     ymax = pptr->view_pr->windscl_yt;
                     pptr->view_pr->windscl_xl = xmin + mult*(xmax-xmin);
                     pptr->view_pr->windscl_xr = xmax - mult*(xmax-xmin);
                     pptr->view_pr->windscl_yb = ymin + mult*(ymax-ymin);
                     pptr->view_pr->windscl_yt = ymax - mult*(ymax-ymin);

                  } else {

                     /* Save the current zoom state */
                     pptr->plot_pr.prev_vxmin = pptr->plot_pr.vxmin;
                     pptr->plot_pr.prev_vxmax = pptr->plot_pr.vxmax;
                     pptr->plot_pr.prev_vymin = pptr->plot_pr.vymin;
                     pptr->plot_pr.prev_vymax = pptr->plot_pr.vymax;

                     xmin = pptr->plot_pr.vxmin;
                     xmax = pptr->plot_pr.vxmax;
                     if (pptr->plot_pr.xlog) {
                        if (xmax > 100*xmin) {
                        pptr->plot_pr.vxmin = 10.0*xmin;
                        pptr->plot_pr.vxmax = 0.10*xmax;
                        }
                     } else {
                        pptr->plot_pr.vxmin = xmin + mult*(xmax-xmin);
                        pptr->plot_pr.vxmax = xmax - mult*(xmax-xmin);
                     }

                     ymin = pptr->plot_pr.vymin;
                     ymax = pptr->plot_pr.vymax;
                     if (pptr->plot_pr.xlog) {
                        if (ymax > 100*ymin) {
                        pptr->plot_pr.vymin = 10.0*ymin;
                        pptr->plot_pr.vymax = 0.10*ymax;
                        }
                     } else {
                        pptr->plot_pr.vymin = ymin + mult*(ymax-ymin);
                        pptr->plot_pr.vymax = ymax - mult*(ymax-ymin);
                     }

                  }
                  XClearArea(mydisplay,vwwindow,0,0,VwWidth,VwHeight,True);
                  *redraw = CN_TRUE;
                  break;

         case ZOOMOUT_BUTTON :
                  /* Zoom out */
                  if (pptr->plottype == CN_PLOT3D) {
                    
                     xmin = pptr->view_pr->windscl_xl;
                     xmax = pptr->view_pr->windscl_xr;
                     ymin = pptr->view_pr->windscl_yb;
                     ymax = pptr->view_pr->windscl_yt;
                     pptr->view_pr->windscl_xl = xmin - umult*(xmax-xmin);
                     pptr->view_pr->windscl_xr = xmax + umult*(xmax-xmin);
                     pptr->view_pr->windscl_yb = ymin - umult*(ymax-ymin);
                     pptr->view_pr->windscl_yt = ymax + umult*(ymax-ymin);

                  } else {

                     /* Save the current zoom state */
                     pptr->plot_pr.prev_vxmin = pptr->plot_pr.vxmin;
                     pptr->plot_pr.prev_vxmax = pptr->plot_pr.vxmax;
                     pptr->plot_pr.prev_vymin = pptr->plot_pr.vymin;
                     pptr->plot_pr.prev_vymax = pptr->plot_pr.vymax;

                     xmin = pptr->plot_pr.vxmin;
                     xmax = pptr->plot_pr.vxmax;
                     if (pptr->plot_pr.xlog) {
                        pptr->plot_pr.vxmin = 0.10*xmin;
                        pptr->plot_pr.vxmax = 10.0*xmax;
                     } else {
                        pptr->plot_pr.vxmin = xmin - umult*(xmax-xmin);
                        pptr->plot_pr.vxmax = xmax + umult*(xmax-xmin);
                     }

                     ymin = pptr->plot_pr.vymin;
                     ymax = pptr->plot_pr.vymax;
                     if (pptr->plot_pr.ylog) {
                        pptr->plot_pr.vymin = 0.10*ymin;
                        pptr->plot_pr.vymax = 10.0*ymax;
                     } else {
                        pptr->plot_pr.vymin = ymin - umult*(ymax-ymin);
                        pptr->plot_pr.vymax = ymax + umult*(ymax-ymin);
                     }

                  }
                  XClearArea(mydisplay,vwwindow,0,0,VwWidth,VwHeight,True);
                  *redraw = CN_TRUE;
                  break;

         case FULLZOOM_BUTTON :
                  /* Full Zoom */
                  if (pptr->plottype == CN_PLOT3D) {

                     /* Reset the 3D window */
                     pptr->view_pr->windscl_xl = -CN_WINDOWSCALE;
                     pptr->view_pr->windscl_xr =  CN_WINDOWSCALE;
                     pptr->view_pr->windscl_yb = -CN_WINDOWSCALE;
                     pptr->view_pr->windscl_yt =  CN_WINDOWSCALE;

                  } else {

                     /* Save the current zoom state */
                     pptr->plot_pr.prev_vxmin = pptr->plot_pr.vxmin;
                     pptr->plot_pr.prev_vxmax = pptr->plot_pr.vxmax;
                     pptr->plot_pr.prev_vymin = pptr->plot_pr.vymin;
                     pptr->plot_pr.prev_vymax = pptr->plot_pr.vymax;

                     /* Reset the boundaries */
                     CNset_plotset_boundaries(pptr,
                               &xmin, &xmax, &ymin, &ymax, &zmin, &zmax);

                     pptr->plot_pr.vxmin = xmin;
                     pptr->plot_pr.vxmax = xmax;
                     pptr->plot_pr.vymin = ymin;
                     pptr->plot_pr.vymax = ymax;

                  }
                  XClearArea(mydisplay,vwwindow,0,0,VwWidth,VwHeight,True);
                  *redraw = CN_TRUE;
                  break;

         case PREVZOOM_BUTTON :
                  /* Previous Zoom / Hiddenline */
                  if (pptr->plottype == CN_PLOT3D) {

                     /* Hiddenline */
                     if (pptr->view_pr->hiddenline)
                        pptr->view_pr->hiddenline = CN_FALSE;
                     else
                        pptr->view_pr->hiddenline = CN_TRUE;

                     XClearArea(mydisplay,vwwindow,0,0, VwWidth,VwHeight,True);
                     *redraw = CN_TRUE;

                  } else {

                     /* Previous Zoom */

                     /* Save the current state */
                     prev_vxmin = pptr->plot_pr.vxmin;
                     prev_vxmax = pptr->plot_pr.vxmax;
                     prev_vymin = pptr->plot_pr.vymin;
                     prev_vymax = pptr->plot_pr.vymax;

                     /* Reset the boundaries */
                     pptr->plot_pr.vxmin = pptr->plot_pr.prev_vxmin;
                     pptr->plot_pr.vxmax = pptr->plot_pr.prev_vxmax;
                     pptr->plot_pr.vymin = pptr->plot_pr.prev_vymin;
                     pptr->plot_pr.vymax = pptr->plot_pr.prev_vymax;

                     /* Save the current state */
                     pptr->plot_pr.prev_vxmin = prev_vxmin;
                     pptr->plot_pr.prev_vxmax = prev_vxmax;
                     pptr->plot_pr.prev_vymin = prev_vymin;
                     pptr->plot_pr.prev_vymax = prev_vymax;

                  }
                  XClearArea(mydisplay,vwwindow,0,0,VwWidth,VwHeight,True);
                  *redraw = CN_TRUE;
                  break;

         case VIEWCENTER_BUTTON :
                  /* Center */
                  if (pptr->plottype == CN_PLOT3D) {
                     pptr->view_pr->eyepos = *eyepos;
                     XClearArea(mydisplay,vwwindow,0,0,VwWidth,VwHeight,True);
                     *redraw = CN_TRUE;
                  } else {
                     if (pptr->plot_pr.xlog)
                        pptr->plot_pr.xlog = CN_FALSE;
                     else 
                        pptr->plot_pr.xlog = CN_TRUE;
                     XClearArea(mydisplay,vwwindow,0,0,VwWidth,VwHeight,True);
                     *redraw = CN_TRUE;
                  }
                  break;

         case VIEWLEFT_BUTTON :
                  /* Left 10 degrees */
                  if (pptr->plottype == CN_PLOT3D) {
                     CNrotate_view(pptr->view_pr,-10.0,  0.0);
                     XClearArea(mydisplay,vwwindow,0,0,VwWidth,VwHeight,True);
                     *redraw = CN_TRUE;
                  } else {
                     if (pptr->plot_pr.ylog)
                        pptr->plot_pr.ylog = CN_FALSE;
                     else 
                        pptr->plot_pr.ylog = CN_TRUE;
                     XClearArea(mydisplay,vwwindow,0,0,VwWidth,VwHeight,True);
                     *redraw = CN_TRUE;
                  }
                  break;

         case VIEWRIGHT_BUTTON :
                  /* Right 10 degrees */
                  if (pptr->plottype == CN_PLOT3D) {
                     CNrotate_view(pptr->view_pr, 10.0,  0.0);
                     XClearArea(mydisplay,vwwindow,0,0,VwWidth,VwHeight,True);
                     *redraw = CN_TRUE;
                  } else {
                     if (pptr->plot_pr.zlog)
                        pptr->plot_pr.zlog = CN_FALSE;
                     else 
                        pptr->plot_pr.zlog = CN_TRUE;
                     XClearArea(mydisplay,vwwindow,0,0,VwWidth,VwHeight,True);
                     *redraw = CN_TRUE;
                  }
                  break;

         case VIEWUP_BUTTON :
                  /* Up 10 degrees */
                  if (pptr->plottype == CN_PLOT3D) {
                     CNrotate_view(pptr->view_pr,  0.0,-10.0);
                     XClearArea(mydisplay,vwwindow,0,0,VwWidth,VwHeight,True);
                     *redraw = CN_TRUE;
                  } else {
                     if (pptr->plot_pr.equalscale) {
                        pptr->plot_pr.equalscale = CN_FALSE;
                        pptr->plot_pr.fitpage    = CN_TRUE; 
                     } else {
                        pptr->plot_pr.equalscale = CN_TRUE;
                        pptr->plot_pr.fitpage    = CN_FALSE; 
                     }
                     XClearArea(mydisplay,vwwindow,0,0,VwWidth,VwHeight,True);
                     *redraw = CN_TRUE;
                  }
                  break;

         case VIEWDOWN_BUTTON :
                  /* Down 10 degrees */
                  if (pptr->plottype == CN_PLOT3D) {
                     CNrotate_view(pptr->view_pr,  0.0, 10.0);
                     XClearArea(mydisplay,vwwindow,0,0,VwWidth,VwHeight,True);
                     *redraw = CN_TRUE;
                  } else {
                     if (pptr->plot_pr.grid)
                        pptr->plot_pr.grid = CN_FALSE;
                     else 
                        pptr->plot_pr.grid = CN_TRUE;
                     XClearArea(mydisplay,vwwindow,0,0,VwWidth,VwHeight,True);
                     *redraw = CN_TRUE;
                  }
                  break;

         default: /* Do nothing */
                  break;
         }
      }
   }
   return(match);
}

/*
 * Rotate the view 
 */
static void rotate_picture(pptr,eyepos,mode,VwWidth, VwHeight, redraw)
CNplotsetptr pptr;
CNcoord      *eyepos;
int          mode;
int          VwWidth, VwHeight;
int          *redraw;
{
   if (pptr->plottype != CN_PLOT3D) return;

   switch (mode) {
   case EVL  : CNrotate_view(pptr->view_pr, 10.0,  0.0);
               *redraw = CN_TRUE;
               break;
   case EVH  : CNrotate_view(pptr->view_pr,-10.0,  0.0);
               *redraw = CN_TRUE;
               break;
   case EVJ  : CNrotate_view(pptr->view_pr,  0.0, 10.0);
               *redraw = CN_TRUE;
               break;
   case EVK  : CNrotate_view(pptr->view_pr,  0.0,-10.0);
               *redraw = CN_TRUE;
               break;
   case EVF  : CNrotate_view(pptr->view_pr, 90.0,  0.0);
               *redraw = CN_TRUE;
               break;
   case EVA  : CNrotate_view(pptr->view_pr,-90.0,  0.0);
               *redraw = CN_TRUE;
               break;
   case EVD  : CNrotate_view(pptr->view_pr,  0.0, 90.0);
               *redraw = CN_TRUE;
               break;
   case EVS  : CNrotate_view(pptr->view_pr,  0.0,-90.0);
               *redraw = CN_TRUE;
               break;
   case EVO  : pptr->view_pr->eyepos = *eyepos;
               *redraw = CN_TRUE;
               break;
   case EVX  : pptr->view_pr->eyepos.x = 1.0;
               pptr->view_pr->eyepos.y = 0.0;
               pptr->view_pr->eyepos.z = 0.0;
               *redraw = CN_TRUE;
               break;
   case EVY  : pptr->view_pr->eyepos.x = 0.0;
               pptr->view_pr->eyepos.y = 1.0;
               pptr->view_pr->eyepos.z = 0.0;
               *redraw = CN_TRUE;
               break;
   case EVZ  : pptr->view_pr->eyepos.x = 0.0;
               pptr->view_pr->eyepos.y = 0.0;
               pptr->view_pr->eyepos.z = 1.0;
               *redraw = CN_TRUE;
               break;
   default   : break;
   }
   if (*redraw) XClearArea(mydisplay,vwwindow,0,0,VwWidth,VwHeight,True);
}

/* 
 * zoom into a region
 */
static void zoom_in(pptr, button, start_x, start_y, last_x, last_y, 
                    VwWidth, VwHeight, redraw)
CNplotsetptr pptr;
unsigned int button;
int          start_x, start_y, last_x, last_y, VwWidth, VwHeight;
int          *redraw;
{
   double xmin, xmax, ymin, ymax, dx, dy;
   double x1, y1, x2, y2;
   int    X1, Y1, X2, Y2;
   short  probability_plot;
   double zoom_factor = 0.25;

   /* Get the upper/lower bounds */
   X1 = (start_x < last_x) ? start_x : last_x;
   X2 = (start_x < last_x) ? last_x  : start_x;
   Y1 = (start_y < last_y) ? start_y : last_y;
   Y2 = (start_y < last_y) ? last_y  : start_y;

   /* Select the zoom factor */
   if (button == Button3) {
      /* Pan and zoom out */
      zoom_factor = 1.00;
   } else if (button == Button2) {
      /* Pan only */
      zoom_factor = 0.50;
   } else {
      /* Pan and zoom */
      zoom_factor = 0.25;
   }

   /* Translate the coordinates */
   if (pptr->plottype == CN_PLOT3D) {
      /* Zoom in on a 3D plot */
      translate_X113D_window(X1, Y1, &x1, &y1,
              X_xmin,    X_xmax,
              X_ymin,    X_ymax,
              pptr->view_pr->windscl_xl,
              pptr->view_pr->windscl_xr,
              pptr->view_pr->windscl_yb,
              pptr->view_pr->windscl_yt);

      translate_X113D_window(X2, Y2, &x2, &y2,
              X_xmin,    X_xmax,
              X_ymin,    X_ymax,
              pptr->view_pr->windscl_xl,
              pptr->view_pr->windscl_xr,
              pptr->view_pr->windscl_yb,
              pptr->view_pr->windscl_yt);

      /* If (X1,Y1) = (X2,Y2) center and zoom */
      if ((X1==X2) && (Y1==Y2)) {
         x1 -= zoom_factor*(pptr->view_pr->windscl_xr -
                            pptr->view_pr->windscl_xl);
         y1 -= zoom_factor*(pptr->view_pr->windscl_yt -
                            pptr->view_pr->windscl_yb);
         x2 += zoom_factor*(pptr->view_pr->windscl_xr -
                            pptr->view_pr->windscl_xl);
         y2 += zoom_factor*(pptr->view_pr->windscl_yt -
                            pptr->view_pr->windscl_yb);
      }

      /* Set boundaries of the window */
      xmin = (x1 < x2) ? x1 : x2;
      xmax = (x1 > x2) ? x1 : x2;
      ymin = (y1 < y2) ? y1 : y2;
      ymax = (y1 > y2) ? y1 : y2;

      /* Make sure window is square */
      dx   = xmax - xmin;
      dy   = ymax - ymin;
      if (dx > dy) {           /* Adjust dy */
         dy   = dx;
         ymin = 0.5*(ymin+ymax) - 0.5*dx;
         ymax = ymin + dy;
      } else if (dy > dx) {    /* Adjust dx */
         dx   = dy;
         xmin = 0.5*(xmin+xmax) - 0.5*dy;
         xmax = xmin + dx;
      }

      /* Now check the window size */
      if (dx < CN_SMALL) {
         (void) fprintf(stderr,
                "Error!  The window dimension must be greater than 0!\n");
         return;
      }

      /* Change the window */
      pptr->view_pr->windscl_xl = xmin;
      pptr->view_pr->windscl_xr = xmax;
      pptr->view_pr->windscl_yb = ymin;
      pptr->view_pr->windscl_yt = ymax;

   } else {

      /* Probability plot ? */
      if (pptr->plotformat == CN_PROBABILITY_PLOT)
         probability_plot = CN_TRUE;
      else 
         probability_plot = CN_FALSE;

      /* Zoom in on a 2D plot */
      translate_X11_coord((double)X1, (double)Y1, &x1, &y1,
              X_xmin,    X_xmax,
              X_ymin,    X_ymax,
              pptr->plot_pr.pxmin,
              pptr->plot_pr.pxmax,
              pptr->plot_pr.pymin,
              pptr->plot_pr.pymax,
              pptr->plot_pr.xlog,
              pptr->plot_pr.ylog,
              pptr->plot_pr.xflip,
              pptr->plot_pr.yflip, 
              probability_plot);

      translate_X11_coord((double)X2, (double)Y2, &x2, &y2,
              X_xmin,    X_xmax,
              X_ymin,    X_ymax,
              pptr->plot_pr.pxmin,
              pptr->plot_pr.pxmax,
              pptr->plot_pr.pymin,
              pptr->plot_pr.pymax,
              pptr->plot_pr.xlog,
              pptr->plot_pr.ylog,
              pptr->plot_pr.xflip,
              pptr->plot_pr.yflip,
              probability_plot);

      /* If (X1,Y1) = (X2,Y2) center and zoom */
      if ((X1==X2) && (Y1==Y2)) {
         if (pptr->plot_pr.xlog) {
            dx  = zoom_factor*(pptr->plot_pr.pxmax - pptr->plot_pr.pxmin);
            dx  = pow(10.0,dx);
            x1  = x1/dx;
            x2  = x2*dx;
         } else {
            x1 -= zoom_factor*(pptr->plot_pr.pxmax - pptr->plot_pr.pxmin);
            x2 += zoom_factor*(pptr->plot_pr.pxmax - pptr->plot_pr.pxmin);
         }

         if (pptr->plot_pr.ylog) {
            dy  = zoom_factor*(pptr->plot_pr.pymax - pptr->plot_pr.pymin);
            dy  = pow(10.0,dy);
            y1  = y1*dy;
            y2  = y2/dy;
         } else {
            y1 -= zoom_factor*(pptr->plot_pr.pymax - pptr->plot_pr.pymin);
            y2 += zoom_factor*(pptr->plot_pr.pymax - pptr->plot_pr.pymin);
         }
      }

      /* Set boundaries of the window */
      xmin = (x2 > x1) ? x1 : x2;
      xmax = (x2 < x1) ? x1 : x2;
      ymin = (y2 > y1) ? y1 : y2;
      ymax = (y2 < y1) ? y1 : y2;
      if ((xmax - xmin) < 1.0e-99) {
         (void) fprintf(stderr,"Error! Zoom area has zero width!\n");
         return;
      }
      if ((ymax - ymin) < 1.0e-99) {
         (void) fprintf(stderr,"Error! Zoom area has zero height!\n");
         return;
      }

      /* Save the current state */
      pptr->plot_pr.prev_vxmin = pptr->plot_pr.vxmin;
      pptr->plot_pr.prev_vxmax = pptr->plot_pr.vxmax;
      pptr->plot_pr.prev_vymin = pptr->plot_pr.vymin;
      pptr->plot_pr.prev_vymax = pptr->plot_pr.vymax;

      /* Reset the viewport */
      pptr->plot_pr.vxmin = xmin;
      pptr->plot_pr.vxmax = xmax;
      pptr->plot_pr.vymin = ymin;
      pptr->plot_pr.vymax = ymax;

   }

   /* Redraw */
   *redraw = CN_TRUE;
   XClearArea(mydisplay,vwwindow,0,0,VwWidth,VwHeight,True);
}

/*
 * Translate from X11 coordinates to real coordinates
 */
static void translate_X11_coord(xp, yp, xw, yw,
                                Xxmin, Xxmax, Xymin, Xymax,
                                xmin, xmax, ymin, ymax,
                                xlog, ylog, xflip, yflip, probability_plot)
double  xp,  yp;                   /* X11 plot coordinates         */
double *xw, *yw;                   /* World coordinates            */
int     Xxmin, Xxmax, Xymin, Xymax;
double  xmin, xmax, ymin, ymax;
short   xlog, ylog;
short   xflip, yflip;
short   probability_plot;
{
   /*
    * Translate X11 to world coordinates and apply log options
    */
   PXtranslate_X11_to_world(xp, yp, xw, yw,
                            Xxmin, Xxmax, Xymin, Xymax,
                            xmin, xmax, ymin, ymax,
                            xlog, ylog, xflip, yflip, probability_plot);
}

/*
 * Translate from X11 coordinates to real coordinates
 * This is for a 3D view window, so there is no real 1-to-1 correspondence
 * between the box and the selected window.
 */
static void translate_X113D_window(X, Y, x, y,
                                  Xxmin, Xxmax,
                                  Xymin, Xymax,
                                  window_xmin, window_xmax,
                                  window_ymin, window_ymax)
int    X, Y;
double *x, *y;
int    Xxmin, Xxmax, Xymin, Xymax;
double window_xmin, window_xmax, window_ymin, window_ymax;
{
   /*
    * The current window has already a mapping to the X-dimensions, i.e.
    *    window.xmin -> Xxmin
    *    window.xmax -> Xxmax
    * Thus x can be found from a simple linear transfo.
    */
   *x = window_xmin + (window_xmax-window_xmin)*(X-Xxmin)/(double)(Xxmax-Xxmin);
   *y = window_ymin + (window_ymax-window_ymin)*(Xymax-Y)/(double)(Xymax-Xymin);
}


static void show_view(pptr, redraw, VwWidth, VwHeight, debug)
CNplotsetptr pptr;
int          *redraw;
int          VwWidth, VwHeight;       
int          debug;
{
   int xxmin, xxmax, xymin, xymax;
   int frame_only = CN_FALSE;
   int printdate = CN_TRUE;

   if (debug) 
   (void) printf("redrawing window... redraw=%d\n",*redraw);

   if (use_pixmap) {
      /*
       * This places a copy of the view-window in a pixmap.
       * When the view-window is exposed, the pixmap is copied back,
       * thus resulting in faster redraw
       */

      if (*redraw) {

         /*
          * Redraw the view-window and copy to the pixmap 
          */

         /* Set to watch cursor */
         xs_watch_cursor(mydisplay, vwwindow);

         /* Reset the pixmap */
         XSetForeground(mydisplay, gc0, background_pixel);
         XFillRectangle(mydisplay, my_pixmap, gc0, 0, 0, VwWidth, VwHeight);
         XSetForeground(mydisplay, gc0, foreground_pixel);

         /* Draw the plot */
         (void) PXdrawplotX(pptr,
                            mydisplay, vwwindow, my_pixmap,
                            gc0, gc1, font0, font1, 
                            VwWidth, VwHeight,
                            &xxmin, &xxmax, &xymin, &xymax, frame_only,
                            printdate);
         X_xmin = xxmin;
         X_xmax = xxmax;
         X_ymin = xymin;
         X_ymax = xymax;

         /* Reset redraw flag */
         *redraw = CN_FALSE;

         /* Reset to normal cursor */
         xs_normal_cursor(mydisplay, vwwindow);

      } else {
         /*
          * Copy the pixmap to the view window
          * This happens only if the window has been exposed
          */
         XCopyArea(mydisplay, my_pixmap, vwwindow, gc0, 0, 0,  VwWidth,
                   VwHeight, 0, 0);
      }
   } else { /* if (!use_pixmap) */
      /*
       * This uses the direct method
       * The redraw flag is ignored 
       */

      /* Set to watch cursor */
      xs_watch_cursor(mydisplay, vwwindow);
 
      /* Draw the plot */
      (void) PXdrawplotX(pptr,
                         mydisplay, vwwindow, my_pixmap,
                         gc0, gc1, font0, font1,
                         VwWidth, VwHeight,
                         &xxmin, &xxmax, &xymin, &xymax, frame_only, 
                         printdate);
      X_xmin = xxmin;
      X_xmax = xxmax;
      X_ymin = xymin;
      X_ymax = xymax;

      /* Reset redraw flag */
      *redraw = CN_FALSE;

      /* Reset to normal cursor */
      xs_normal_cursor(mydisplay, vwwindow);
   }
}

/*
 * Change the pointer
 */
/*ARGSUSED*/
static void xs_watch_cursor(display, window)
Display *display;
Window  window;
{
   static Cursor watch_cursor=NULL;
 
   /* Check the cursor */
   if (new_session) {
      watch_cursor = NULL;
      new_session  = CN_FALSE;
   }

   /* Create the cursor */
   if (!watch_cursor) watch_cursor = XCreateFontCursor(display,XC_watch);
 
   XDefineCursor(display, window, watch_cursor);
   XFlush(display);
}

/*
 * Reset the pointer
 */
/*ARGSUSED*/
static void xs_normal_cursor(display, window)
Display *display;
Window  window;
{
   XUndefineCursor(display, window);
   XFlush(display);
}

