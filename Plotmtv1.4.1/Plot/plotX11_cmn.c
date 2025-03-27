/*
 * plotX11_cmn.c - X11 routines used by both 2D and 3D X11 plotting.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "plotX11.h"
#include "CNplot.h"

#include "Bitmaps/stippleA.bmp"
#include "Bitmaps/stippleB.bmp"
#include "Bitmaps/stippleC.bmp"
#include "Bitmaps/stippleD.bmp"
#include "Bitmaps/stippleE.bmp"
#include "Bitmaps/stippleF.bmp"
#include "Bitmaps/stippleG.bmp"

/*
 * Global variables
 */
unsigned long background_pixel;               /* color of background     */
unsigned long foreground_pixel;               /* color of foreground     */
unsigned long colors[PX_MAX_COLORS];          /* color pixel values      */
int           dark_background;                /* background hints        */
 
/* Widgets and X11-related variables */
Display     *display;                       /* display */
Window      window;                         /* parent window */
Pixmap      pixmap;                         /* pixmap to draw into */
XFontStruct *font_info;                     /* font information */
XFontStruct *lblfont_info;                  /* label font information */
GC          gc, gcl;                        /* graphics context */
int         font_height, font_width;        /* More font info   */
int         lblfont_height, lblfont_width;  /* More font info   */

/* Font array */
XFontStruct* fontArr[PX_MAX_FONTS];         /* Font array */

/* The dimensions of the Xwindow plot */
int Xxmin, Xxmax, Xymin, Xymax, Width, Height;

/* line-types */
#  define DASHED        2
#  define DOTTED        2
#  define DOT_DASHED    4
#  define LONG_DOTTED   2
#  define DOUBLE_DOTTED 4
#  define LONG_DASHED   2
#  define DOT_DASHED2   4
#  define TRIPLE_DOTTED 6
#  define DOTDOT_DASHED 6
#  define GRID_DOTTED   2

static char dash_list_dashed[DASHED]               = {6,6};
static char dash_list_dotted[DOTTED]               = {1,6};
static char dash_list_dot_dashed[DOT_DASHED]       = {6,2,2,2};
static char dash_list_long_dotted[LONG_DOTTED]     = {2,2};
static char dash_list_double_dotted[DOUBLE_DOTTED] = {2,2,2,6};
static char dash_list_long_dashed[LONG_DASHED]     = {8,2};
static char dash_list_dot_dashed2[DOT_DASHED2]     = {9,6,2,6};
static char dash_list_triple_dotted[TRIPLE_DOTTED] = {1,1,1,1,1,3};
static char dash_list_dotdot_dashed[DOTDOT_DASHED] = {3,3,3,3,6,3};
static char dash_list_grid_dotted[GRID_DOTTED]     = {1,2};

/*
 * FORWARD DECLARATIONS
 */
int          PXinitXWindow();
static int   load_palette();
static void  load_cmap();
static int   load_stipples();
static void  load_line_colors();
static char  *hex_string();
#ifdef ANALYTIC_COLORMAP
static void  colorscale1();
static void  colorscale2();
#endif
static void  colorscale_wrb();
static void  colorscale_rgb();

#include "cmap.h"

static void load_fonts();
static XFontStruct* load_font();

int          PXlinetypX();
void         PXmarkerX();
void         PXnamedColorX();
void         PXlineColorX();
void         PXfillColorX();
void         PXsetColorX();
int          PXpolyColorIndexX();

void         PXfillX_polygon();
static void  PXfilltypX();

void         PXplotX_linelabels();
void         PXplotX_contscale();

void         PXtranslate_world_to_X11();
static void _PXtranslate_world_to_X11();
void         PXtranslate_X11_to_world();
static void _PXtranslate_X11_to_world();

int          PXdrawplotX();
static void  draw_empty_view();

/*
 * The first 2 colors are reserved for background, foreground.
 * The next 6 colors are named colors which do not fit into the
 * color-range defined by the following 32 colors
 * The next 32 are loaded from a palette, ranging from blue to red
 */

/* colors */
#define DEFAULT_FORE_COLOR    "White"
#define DEFAULT_BACK_COLOR    "#191970"
#define COLOR_1               "MediumBlue"
#define COLOR_2               "Yellow"
#define COLOR_3               "#00ffff"         /* "cyan"          */
#define COLOR_4               "Green"
#define COLOR_5               "Red"
#define COLOR_6               "#1e90ff"         /* "DodgerBlue"    */
#define COLOR_7               "#ffa500"         /* "Orange"        */
#define COLOR_8               "magenta"
#define COLOR_9               "#ffb6c1"         /* "LightPink"     */
#define COLOR_10              "#afeeee"         /* "PaleTurquoise" */
#define COLOR_11              "#d2661e"         /* "chocolate"     */
#define FORE_C                0
#define BACK_C                1
#define NAMED_C1              2
#define NAMED_C2              3
#define NAMED_C3              4
#define NAMED_C4              5
#define NAMED_C5              6
#define NAMED_C6              7
#define NAMED_C7              8
#define NAMED_C8              9
#define NAMED_C9             10
#define NAMED_C10            11
#define NAMED_C11            12

/* Fonts */
#define ALT_FONT             "6x13"
#define NAMED_FONT_01        "nil2"
#define NAMED_FONT_02        "5x8"
#define NAMED_FONT_03        "6x9"
#define NAMED_FONT_04        "6x10"
#define NAMED_FONT_05        "6x12"
#define NAMED_FONT_06        "6x13"
#define NAMED_FONT_07        "7x14"
#define NAMED_FONT_08        "9x15"
#define NAMED_FONT_09        "10x20"
#define NAMED_FONT_10        "12x24"
#define FONT_SZ_00              0
#define FONT_SZ_08              1
#define FONT_SZ_09              2
#define FONT_SZ_10              3
#define FONT_SZ_12              4
#define FONT_SZ_13              5
#define FONT_SZ_14              6
#define FONT_SZ_15              7
#define FONT_SZ_20              8
#define FONT_SZ_24              9

/* Stipples */
static Pixmap stippleA;     /* Square grid      */
static Pixmap stippleB;     /* Diagonal lines   */
static Pixmap stippleC;     /* Diagonal lines   */
static Pixmap stippleD;     /* Diagonal grid    */
static Pixmap stippleE;     /* Vertical Lines   */
static Pixmap stippleF;     /* Horizontal Lines */
static Pixmap stippleG;     /* Dotted lines     */



/*
 * X11 INITIALIZATION
 */

/*
 * allocate the colors on color display 
 */
int PXinitXWindow(display, screen, plot_foreground, plot_background, reverse)
Display *display;
int     screen;
char    *plot_foreground, *plot_background;
int     reverse;
{
   XColor      cdef;
   Colormap    cmap;
   int         i, depth;

   char     *fore_color;                  /* Foreground color     */
   char     *back_color;                  /* Background color     */
   char     *tmp_color;                   /* Temporary pointer    */
   char     *clr_name[PX_MAX_NAMED_COLORS];  /* color names          */

 
   /* get info on depth and colormap */
   depth = DisplayPlanes(display,screen);
   cmap  = DefaultColormap(display,screen);

   /* Initialize colors */
   fore_color   = plot_foreground ? plot_foreground : DEFAULT_FORE_COLOR;
   back_color   = plot_background ? plot_background : DEFAULT_BACK_COLOR;
   dark_background = 1;
   if (reverse) {
      tmp_color = fore_color;
      fore_color = back_color;
      back_color = tmp_color;
      dark_background = 0;
   }

   /* 
    * associate named colors with pixel values 
    */
   if (depth == 1) {     
      /* one-plane monochrome */
      colors[FORE_C]    = WhitePixel(display,screen);
      colors[BACK_C]    = BlackPixel(display,screen);
      colors[NAMED_C1]  = colors[FORE_C];
      colors[NAMED_C2]  = colors[FORE_C];
      colors[NAMED_C3]  = colors[FORE_C];
      colors[NAMED_C4]  = colors[FORE_C];
      colors[NAMED_C5]  = colors[FORE_C];
      colors[NAMED_C6]  = colors[FORE_C];
      colors[NAMED_C7]  = colors[FORE_C];
      colors[NAMED_C8]  = colors[FORE_C];
      colors[NAMED_C9]  = colors[FORE_C];
      colors[NAMED_C10] = colors[FORE_C];
      colors[NAMED_C11] = colors[FORE_C];

   } else {   
      /* color */
      clr_name[FORE_C]   = fore_color;
      clr_name[BACK_C]   = back_color;
      clr_name[NAMED_C1] = COLOR_1;
      clr_name[NAMED_C2] = COLOR_2;
      clr_name[NAMED_C3] = COLOR_3;
      clr_name[NAMED_C4] = COLOR_4;
      clr_name[NAMED_C5] = COLOR_5;
      clr_name[NAMED_C6] = COLOR_6;
      clr_name[NAMED_C7] = COLOR_7;
      clr_name[NAMED_C8] = COLOR_8;
      clr_name[NAMED_C9] = COLOR_9;
      clr_name[NAMED_C10]= COLOR_10;
      clr_name[NAMED_C11]= COLOR_11;
      for (i=0; i<PX_MAX_NAMED_COLORS; i++)
         if (XParseColor(display,cmap,clr_name[i],&cdef) &&
             XAllocColor(display,cmap,&cdef)) {
            colors[i] = cdef.pixel;
            if (i==BACK_C) {
               if (cdef.red + cdef.green + cdef.blue < 90000) {
                  /* Dark background */
                  dark_background = 1;
               } else {
                  /* Light background */
                  dark_background = 0;
               }
            }
         } else {
            (void) fprintf(stderr,"\nError: Can't allocate X11 color %s\n",
                    clr_name[i]);
            if (i==FORE_C) {
               (void) fprintf(stderr,
                      "\n***    Substituting %s color with white\n",
                        clr_name[i]);
               colors[i]       = WhitePixel(display,screen);
            }
            if (i==BACK_C) {
               (void) fprintf(stderr,
                      "\n***    Substituting %s color with black\n",
                        clr_name[i]);
               colors[i]       = BlackPixel(display,screen);
               dark_background = 1;
            } else {
               if (dark_background) {
               (void) fprintf(stderr,
                      "\n***    Substituting %s color with white\n",
                        clr_name[i]);
               colors[i]       = WhitePixel(display,screen);
               } else {
               (void) fprintf(stderr,
                      "\n***    Substituting %s color with black\n",
                        clr_name[i]);
               colors[i]       = BlackPixel(display,screen);
               }
            }
         } /* end if */
   }  /* end if */

   /* assign pixel values */
   foreground_pixel  = colors[FORE_C];
   background_pixel  = colors[BACK_C];

   /* Allocate the non-named colors */
   if (!load_palette(display)) {
      (void) fprintf(stderr,"Error: Unable to load X11 color palette\n");
      return(0);
   }

   /* Fill in the line colors */
   load_line_colors();

   /* Load stipple patterns */
   if (!load_stipples(display,screen)) {
      (void) fprintf(stderr,"Error: Unable to load X11 stipple patterns\n");
      return(0);
   }

   /* Load the fonts */
   load_fonts(display);

   /* return */
   return(1);
}
   
/*
 * Load stipple patterns 
 */
static int load_stipples(display,screen) 
Display *display;
int     screen;
{
   stippleA    = XCreateBitmapFromData(
                 display, RootWindow(display,screen),
                 stippleA_bits,
                 stippleA_width, stippleA_height);
   stippleB    = XCreateBitmapFromData(
                 display, RootWindow(display,screen),
                 stippleB_bits,
                 stippleB_width, stippleB_height);
   stippleC    = XCreateBitmapFromData(
                 display, RootWindow(display,screen),
                 stippleC_bits,
                 stippleC_width, stippleC_height);
   stippleD    = XCreateBitmapFromData(
                 display, RootWindow(display,screen),
                 stippleD_bits,
                 stippleD_width, stippleD_height);
   stippleE    = XCreateBitmapFromData(
                 display, RootWindow(display,screen),
                 stippleE_bits,
                 stippleE_width, stippleE_height);
   stippleF    = XCreateBitmapFromData(
                 display, RootWindow(display,screen),
                 stippleF_bits,
                 stippleF_width, stippleF_height);
   stippleG    = XCreateBitmapFromData(
                 display, RootWindow(display,screen),
                 stippleG_bits,
                 stippleG_width, stippleG_height);

   /* return */
   return(1);
}

/*
 * Load a color palette 
 */
static int load_palette(display)
Display       *display;
{
   int         screen;
   Colormap    cmap;
   int         depth;
   XColor      cdef;
   int         i, r, g, b;


   /* get info on depth and colormap */
   screen  = DefaultScreen(display);
   depth   = DisplayPlanes(display,screen);
   cmap    = DefaultColormap(display,screen);

   /* Load a color palette */
   for (i=0; i<PX_MAX_FILL_COLORS; i++) {

      /* Get the rgb values and allocate a color */
      load_cmap(i, &r, &g, &b, depth); 
      cdef.red   = r * 256;
      cdef.green = g * 256;
      cdef.blue  = b * 256;
      cdef.flags = DoRed | DoGreen | DoBlue;
      if (XAllocColor(display,cmap,&cdef))
         colors[i+PX_MAX_NAMED_COLORS] = cdef.pixel;
      else if (XParseColor(display,cmap,hex_string(r,g,b),&cdef) &&
               XAllocColor(display,cmap,&cdef)) {
         colors[i+PX_MAX_NAMED_COLORS] = cdef.pixel;
      } else {
         (void) fprintf(stderr,"\nError: Can't allocate X11 color \n");
         (void) fprintf(stderr,"with indices r=%d g=%d b=%d\n",r,g,b);
         (void) fprintf(stderr,"*** Substituting with foreground color\n");
         colors[i+PX_MAX_NAMED_COLORS] = colors[FORE_C];
      }
   }

   /* return OK */
   return(1);
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
 */
static void colorscale_wrb(c,r,g,b)
int c;
int *r, *g, *b;
{
   switch(c) {
   case  0: *r=  0; *g=120; *b=255; break; /* Light Blue */
   case  1: *r=  0; *g= 90; *b=255; break; 
   case  2: *r=  0; *g=  0; *b=255; break; /* Blue */
   case  3: *r=  0; *g=  0; *b=230; break; 
   case  4: *r=  0; *g=  0; *b=205; break;
   case  5: *r=  0; *g=  0; *b=180; break; /* Navy */

   case  6: *r= 90; *g=  0; *b=170; break; 
   case  7: *r=120; *g=  0; *b=180; break; 
   case  8: *r=150; *g=  0; *b=180; break; 
   case  9: *r=180; *g=  0; *b=180; break; /* Magenta */
   case 10: *r=180; *g= 90; *b=180; break; 
   case 11: *r=180; *g= 50; *b=180; break; 
   case 12: *r=180; *g= 50; *b=150; break; 
   case 13: *r=180; *g= 50; *b=120; break; 
   case 14: *r=170; *g= 50; *b= 90; break; 

   case 15: *r=180; *g=  0; *b=  0; break; /* Dark red */
   case 16: *r=205; *g=  0; *b=  0; break; 
   case 17: *r=230; *g=  0; *b=  0; break; 
   case 18: *r=255; *g=  0; *b=  0; break; /* Red */
   case 19: *r=255; *g=100; *b=  0; break; /* Orange red */
   case 20: *r=255; *g=120; *b=  0; break; 
   case 21: *r=255; *g=140; *b=  0; break; /* Dark orange */
   case 22: *r=255; *g=165; *b=  0; break; /* Orange */
   case 23: *r=255; *g=185; *b=  0; break; 
   case 24: *r=255; *g=205; *b=  0; break; 
   case 25: *r=255; *g=215; *b=  0; break; 
   case 26: *r=255; *g=235; *b=  0; break; 
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
 */
static void colorscale_rgb(c,r,g,b)
int c;
int *r, *g, *b;
{
   switch(c) {
   case  0: *r=  0; *g=  0; *b=128; break; /* Navy */
   case  1: *r=  0; *g=  0; *b=170; break; /* Navy */
   case  2: *r=  0; *g=  0; *b=205; break; /* Medium Blue */
   case  3: *r=  0; *g=  0; *b=255; break; /* Blue */
   case  4: *r=  0; *g=100; *b=255; break; 
   case  5: *r=  0; *g=144; *b=255; break;
   case  6: *r=  0; *g=191; *b=255; break; /* Deep sky blue */
   case  7: *r=  0; *g=210; *b=255; break; 
   case  8: *r=  0; *g=230; *b=255; break; 
   case  9: *r=  0; *g=255; *b=255; break; /* Cyan */
   case 10: *r=  0; *g=255; *b=192; break; 
   case 11: *r=  0; *g=255; *b=128; break; 
   case 12: *r=  0; *g=255; *b= 64; break; 
   case 13: *r=  0; *g=255; *b=  0; break; /* Green */
   case 14: *r=127; *g=255; *b= 20; break; /* chartreuse */
   case 15: *r=173; *g=255; *b= 47; break; /* Green Yellow */
   case 16: *r=210; *g=255; *b= 47; break; 
   case 17: *r=220; *g=255; *b= 20; break; 
   case 18: *r=255; *g=255; *b=  0; break; /* Yellow */
   case 19: *r=255; *g=240; *b=  0; break; 
   case 20: *r=255; *g=228; *b=  0; break; 
   case 21: *r=255; *g=215; *b=  0; break; 
   case 22: *r=255; *g=195; *b=  0; break; 
   case 23: *r=255; *g=165; *b=  0; break; /* Orange */
   case 24: *r=255; *g=140; *b=  0; break; /* Dark orange */
   case 25: *r=255; *g=100; *b=  0; break; /* Orange red */
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
 * Get the hex string corresponding to a r,g,b spec
 */
static char *hex_string(r,g,b)
int r,g,b;
{
   char rlabel[10], glabel[10], blabel[10];
   char hex_str[10];
 
   /* Copy the rgb to char strings */
   (void) sprintf(rlabel,"00%x",r);
   (void) sprintf(glabel,"00%x",g);
   (void) sprintf(blabel,"00%x",b);

   /* The hex label is composed of last 2 chars from r,g,b */
   (void) sprintf(hex_str,"#000000");
   hex_str[1] = rlabel[strlen(rlabel)-2];
   hex_str[2] = rlabel[strlen(rlabel)-1];
   hex_str[3] = glabel[strlen(glabel)-2];
   hex_str[4] = glabel[strlen(glabel)-1];
   hex_str[5] = blabel[strlen(blabel)-2];
   hex_str[6] = blabel[strlen(blabel)-1];

   return(hex_str);
}


/*
 * Fill the color array with line colors 
 * These colors are chosen from named colors and fill colors
 */
static void load_line_colors()
{
   int i,c;

   /*
    * Match the linecol to a unique color in such a way that 2 adjacent
    * colors differ significantly (i.e. high contrast).  
    * There are 11 line colors 
    */

   /*
    * Dark background :
    *   yellow
    *   cyan
    *   green
    *   red
    *   blue
    */

   for (i=0; i<PX_MAX_LINE_COLORS; i++) {
      c = i + PX_MAX_NAMED_COLORS + PX_MAX_FILL_COLORS;
      switch (i) {
      case 0 : colors[c] = colors[NAMED_C2];                 /* yellow     */
               break;
      case 1 : colors[c] = colors[NAMED_C3];                 /* cyan       */
               break;
      case 2 : colors[c] = colors[NAMED_C4];                 /* Green      */
               break;
      case 3 : colors[c] = colors[NAMED_C5];                 /* red        */
               break;
      case 4 : colors[c] = colors[NAMED_C6];                 /* dodgerBlue */
               break;
      case 5 : colors[c] = colors[NAMED_C7];                 /* orange     */
               break;
      case 6 : colors[c] = colors[NAMED_C8];                 /* magenta    */
               break;
      case 7 : colors[c] = colors[NAMED_C9];                 /* Pink       */
               break;
      case 8 : colors[c] = colors[NAMED_C10];                /* Turquoise  */
               break;
      case 9 : colors[c] = colors[NAMED_C11];                /* chocolate  */
               break;
      default: colors[c] = colors[NAMED_C2];                 /* yellow     */
               break;
      }
   }
}


/*
 * Fill the font array with fonts of different sizes
 * These sizes are chosen from the standard font map  
 */
static void load_fonts(display)
Display       *display;
{
   int i;

   /* Try to load the alternate font first */
   XFontStruct* altFont=NULL;
   altFont = load_font(display, ALT_FONT);
   if (altFont == NULL) 
      altFont = load_font(display, "vtbold");

   /* Load the fonts */
   for (i=0; i<PX_MAX_FONTS; i++) {
      /* Allocate the font */
      switch (i+1) {
      case  1: fontArr[i] = load_font(display, NAMED_FONT_01);  break;
      case  2: fontArr[i] = load_font(display, NAMED_FONT_02);  break;
      case  3: fontArr[i] = load_font(display, NAMED_FONT_03);  break;
      case  4: fontArr[i] = load_font(display, NAMED_FONT_04);  break;
      case  5: fontArr[i] = load_font(display, NAMED_FONT_05);  break;
      case  6: fontArr[i] = load_font(display, NAMED_FONT_06);  break;
      case  7: fontArr[i] = load_font(display, NAMED_FONT_07);  break;
      case  8: fontArr[i] = load_font(display, NAMED_FONT_08);  break;
      case  9: fontArr[i] = load_font(display, NAMED_FONT_09);  break;
      case 10: fontArr[i] = load_font(display, NAMED_FONT_10);  break;
      default: fontArr[i] = load_font(display, NAMED_FONT_07);  break;
      }
      /* Load an alternate font if necessary */
      if (fontArr[i] == NULL) fontArr[i] = altFont;
   }
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
 * SET COLORS/PATTERNS
 */

/* 
 * set the line type and linewidth
 */
int PXlinetypX(linetyp, ithk)
int linetyp;
int ithk;
{
   int linepattern = CN_TRUE;

   /* Reset the linewidth if necessary */
   if (ithk <= 0) ithk = 1;
   if (ithk == 1) ithk = 0;

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
            XSetLineAttributes(display,gc,ithk,LineSolid,CapButt,JoinBevel);
            linepattern = CN_FALSE;
            break;
   case CN_LN_SOLID : 
            XSetLineAttributes(display,gc,ithk,LineSolid,CapButt,JoinBevel);
            break;
   case CN_LN_DASHED : 
            XSetLineAttributes(display,gc,ithk,LineOnOffDash,CapButt,JoinBevel);
            XSetDashes(display,gc,ithk,dash_list_dashed,DASHED);
            break;
   case CN_LN_DOTTED : 
            XSetLineAttributes(display,gc,ithk,LineOnOffDash,CapButt,JoinBevel);
            XSetDashes(display,gc,ithk,dash_list_dotted,DOTTED);
            break;
   case CN_LN_DOTDASH : 
            XSetLineAttributes(display,gc,ithk,LineOnOffDash,CapButt,JoinBevel);
            XSetDashes(display,gc,ithk,dash_list_dot_dashed,DOT_DASHED);
            break;
   case CN_LN_LONGDOT : 
            XSetLineAttributes(display,gc,ithk,LineOnOffDash,CapButt,JoinBevel);
            XSetDashes(display,gc,ithk,dash_list_long_dotted,LONG_DOTTED);
            break;
   case CN_LN_DOUBLEDOT : 
            XSetLineAttributes(display,gc,ithk,LineOnOffDash,CapButt,JoinBevel);
            XSetDashes(display,gc,ithk,dash_list_double_dotted,DOUBLE_DOTTED);
            break;
   case CN_LN_LONGDASH : 
            XSetLineAttributes(display,gc,ithk,LineOnOffDash,CapButt,JoinBevel);
            XSetDashes(display,gc,ithk,dash_list_long_dashed,LONG_DASHED);
            break;
   case CN_LN_DOTDASH2 : 
            XSetLineAttributes(display,gc,ithk,LineOnOffDash,CapButt,JoinBevel);
            XSetDashes(display,gc,ithk,dash_list_dot_dashed2,DOT_DASHED2);
            break;
   case CN_LN_TRIPLEDOT : 
            XSetLineAttributes(display,gc,ithk,LineOnOffDash,CapButt,JoinBevel);
            XSetDashes(display,gc,ithk,dash_list_triple_dotted,TRIPLE_DOTTED);
            break;
   case CN_LN_DOTDOTDASH: 
            XSetLineAttributes(display,gc,ithk,LineOnOffDash,CapButt,JoinBevel);
            XSetDashes(display,gc,ithk,dash_list_dotdot_dashed,DOTDOT_DASHED);
            break;
   case CN_GD_DOTTED   : 
            XSetLineAttributes(display,gc,ithk,LineOnOffDash,CapButt,JoinBevel);
            XSetDashes(display,gc,ithk,dash_list_grid_dotted,GRID_DOTTED);
            break;
   default: XSetLineAttributes(display,gc,ithk,LineSolid,CapButt,JoinBevel);
            break;
   }

   /* Return linepattern */
   return(linepattern);
}

/* 
 * Draw a marker 
 */
void PXmarkerX(markertype,markersize,x,y)
int markertype,markersize,x,y;
{
   XPoint points[10];
   int    npoints=0;
   int    len=2;
   double scl;

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
   case CN_MK_NONE : 
            /* don't draw */
            break;
   case CN_MK_DOT : 
            /* A single point */
            if (window)
            XDrawPoint(display,window,gc,x,y);
            if (pixmap)
            XDrawPoint(display,pixmap,gc,x,y);
            break;
   case CN_MK_CROSS : 
            /* A Cross */
            len = (int)(3*scl);
            if (window) {
            XDrawLine(display,window,gc,x-len,y    , x+len,y  );
            XDrawLine(display,window,gc,x    ,y-len, x    ,y+len);
            }
            if (pixmap) {
            XDrawLine(display,pixmap,gc,x-len,y    , x+len,y  );
            XDrawLine(display,pixmap,gc,x    ,y-len, x    ,y+len);
            }
            break;
   case CN_MK_X : 
            /* An X */
            len = (int)(2*scl);
            if (window) {
            XDrawLine(display,window,gc,x-len,y-len, x+len,y+len);
            XDrawLine(display,window,gc,x+len,y-len, x-len,y+len);
            }
            if (pixmap) {
            XDrawLine(display,pixmap,gc,x-len,y-len, x+len,y+len);
            XDrawLine(display,pixmap,gc,x+len,y-len, x-len,y+len);
            }
            break;
   case CN_MK_SQUARE1 :
            /* An unfilled rectangle */
            len = (int)(2*scl);
            if (window)
            PXDrawRectangle(display,window,gc,x-len,y-len,2*len,2*len);
            if (pixmap)
            PXDrawRectangle(display,pixmap,gc,x-len,y-len,2*len,2*len);
            break;
   case CN_MK_SQUARE2 :
            /* A filled rectangle */
            len = (int)(2*scl);
            if (window)
            XFillRectangle(display,window,gc,x-len,y-len,2*len+1,2*len+1);
            if (pixmap)
            XFillRectangle(display,pixmap,gc,x-len,y-len,2*len+1,2*len+1);
            break;
   case CN_MK_DIAMOND1 :
            /* An unfilled diamond */
            len = (int)(3*scl);
            npoints = 5;
            points[0].x = x-len;  points[0].y = y    ;
            points[1].x = x    ;  points[1].y = y-len;
            points[2].x = x+len;  points[2].y = y    ;
            points[3].x = x    ;  points[3].y = y+len;
            points[4].x = x-len;  points[4].y = y    ;
            if (window)
            XDrawLines(display,window,gc,
                         points,npoints,CoordModeOrigin);
            if (pixmap)
            XDrawLines(display,pixmap,gc,
                         points,npoints,CoordModeOrigin);
            break;
   case CN_MK_DIAMOND2 :
            /* A filled diamond */
            len = (int)(3*scl);
            npoints = 5;
            points[0].x = x-len;  points[0].y = y    ;
            points[1].x = x    ;  points[1].y = y-len;
            points[2].x = x+len;  points[2].y = y    ;
            points[3].x = x    ;  points[3].y = y+len;
            points[4].x = x-len;  points[4].y = y    ;
            if (window) {
            XDrawLines(display,window,gc,
                         points,npoints,CoordModeOrigin);
            XFillPolygon(display,window,gc,
                         points,npoints,Convex,CoordModeOrigin);
            }
            if (pixmap) {
            XDrawLines(display,pixmap,gc,
                         points,npoints,CoordModeOrigin);
            XFillPolygon(display,pixmap,gc,
                         points,npoints,Convex,CoordModeOrigin);
            }
            break;
   case CN_MK_TRIANGLE1 :
            /* An unfilled triangle */
            len = (int)(3*scl);
            npoints = 4;
            points[0].x = x    ;  points[0].y = y-len;
            points[1].x = x-len;  points[1].y = y+len*2/3;
            points[2].x = x+len;  points[2].y = y+len*2/3;
            points[3].x = x    ;  points[3].y = y-len;
            if (window)
            XDrawLines(display,window,gc,
                         points,npoints,CoordModeOrigin);
            if (pixmap)
            XDrawLines(display,pixmap,gc,
                         points,npoints,CoordModeOrigin);
            break;
   case CN_MK_TRIANGLE2 :
            /* A filled triangle */
            len = (int)(3*scl);
            npoints = 4;
            points[0].x = x    ;  points[0].y = y-len;
            points[1].x = x-len;  points[1].y = y+len*2/3;
            points[2].x = x+len;  points[2].y = y+len*2/3;
            points[3].x = x    ;  points[3].y = y-len;
            if (window) {
            XDrawLines(display,window,gc,
                         points,npoints,CoordModeOrigin);
            XFillPolygon(display,window,gc,
                         points,npoints,Convex,CoordModeOrigin);
            }
            if (pixmap) {
            XDrawLines(display,pixmap,gc,
                         points,npoints,CoordModeOrigin);
            XFillPolygon(display,pixmap,gc,
                         points,npoints,Convex,CoordModeOrigin);
            }
            break;
   case CN_MK_ITRIANGLE1 :
            /* An unfilled triangle */
            len = (int)(3*scl);
            npoints = 4;
            points[0].x = x    ;  points[0].y = y+len;
            points[1].x = x-len;  points[1].y = y-len*2/3;
            points[2].x = x+len;  points[2].y = y-len*2/3;
            points[3].x = x    ;  points[3].y = y+len;
            if (window)
            XDrawLines(display,window,gc,
                         points,npoints,CoordModeOrigin);
            if (pixmap)
            XDrawLines(display,pixmap,gc,
                         points,npoints,CoordModeOrigin);
            break;
   case CN_MK_ITRIANGLE2 :
            /* A filled triangle */
            len = (int)(3*scl);
            npoints = 4;
            points[0].x = x    ;  points[0].y = y+len;
            points[1].x = x-len;  points[1].y = y-len*2/3;
            points[2].x = x+len;  points[2].y = y-len*2/3;
            points[3].x = x    ;  points[3].y = y+len;
            if (window) {
            XDrawLines(display,window,gc,
                         points,npoints,CoordModeOrigin);
            XFillPolygon(display,window,gc,
                         points,npoints,Convex,CoordModeOrigin);
            }
            if (pixmap) {
            XDrawLines(display,pixmap,gc,
                         points,npoints,CoordModeOrigin);
            XFillPolygon(display,pixmap,gc,
                         points,npoints,Convex,CoordModeOrigin);
            }
            break;
   case CN_MK_CIRCLE1 :
            /* An unfilled circle */
            len = (int)(2*scl);
            npoints = 9;
            points[0].x = x-len/2;  points[0].y = y+len;
            points[1].x = x-len  ;  points[1].y = y+len/2;
            points[2].x = x-len  ;  points[2].y = y-len/2;
            points[3].x = x-len/2;  points[3].y = y-len;
            points[4].x = x+len/2;  points[4].y = y-len;
            points[5].x = x+len  ;  points[5].y = y-len/2;
            points[6].x = x+len  ;  points[6].y = y+len/2;
            points[7].x = x+len/2;  points[7].y = y+len;
            points[8].x = x-len/2;  points[8].y = y+len;
            if (window)
            XDrawLines(display,window,gc,
                         points,npoints,CoordModeOrigin);
            if (pixmap)
            XDrawLines(display,pixmap,gc,
                         points,npoints,CoordModeOrigin);
            break;
   case CN_MK_CIRCLE2 :
            /* A filled circle */
            len = (int)(2*scl);
            npoints = 9;
            points[0].x = x-len/2;  points[0].y = y+len;
            points[1].x = x-len  ;  points[1].y = y+len/2;
            points[2].x = x-len  ;  points[2].y = y-len/2;
            points[3].x = x-len/2;  points[3].y = y-len;
            points[4].x = x+len/2;  points[4].y = y-len;
            points[5].x = x+len  ;  points[5].y = y-len/2;
            points[6].x = x+len  ;  points[6].y = y+len/2;
            points[7].x = x+len/2;  points[7].y = y+len;
            points[8].x = x-len/2;  points[8].y = y+len;
            if (window) {
            XDrawLines(display,window,gc,
                         points,npoints,CoordModeOrigin);
            XFillPolygon(display,window,gc,
                         points,npoints,Convex,CoordModeOrigin);
            }
            if (pixmap) {
            XDrawLines(display,pixmap,gc,
                         points,npoints,CoordModeOrigin);
            XFillPolygon(display,pixmap,gc,
                         points,npoints,Convex,CoordModeOrigin);
            }
            break;
   default: break;
   }
}


/*
 * Set a namedcolor
 */
/*ARGSUSED*/
void PXnamedColorX(namedcol)
int namedcol;
{
   int index;

   /*
    * -1 is the background color
    *  0 is the foreground color
    *  1-11 are named colors (1=medblue, 2=yellow...)
    */

   /* Index to the appropriate color */
   if (namedcol <= -1) {
      /* Background color (X black) */
      index = 1;
 
   } else if (namedcol == 0) {
      /* Foreground color (X white) */
      index = 0;

   } else {
      index = PXnamedColorIndex(namedcol);
   }

   XSetForeground(display,gc,colors[index]);
}

/*
 * Set a linecolor 
 *   linecolor=-1 gives the background color (PS white)
 *   linecolor= 0 gives the foreground color (PS black)
 *   linecolor= 1-10 (MAX_LINE_COLORS=10) gives a line color
 *   linecolor > 10 cycles through the 10 specified colors
 *   color11=color1, etc.
 */
void PXlineColorX(linecol)
int linecol;
{
   int index;

   /*
    * -1 is the background color
    *  0 is the foreground color
    *  1-10 are line colors
    */

   if (linecol <= -1) {
      /* Background color (X black) */
      index = 1;

   } else if (linecol == 0) {
      /* Foreground color (X white) */
      index = 0;

   } else {
      /* Index to the appropriate color */ 
      index = PXlineColorIndex(linecol);

   }

   XSetForeground(display,gc,colors[index]);
}

/*
 * Set a linecolor 
 *   linecolor=-1 gives the background color (PS white)
 *   linecolor= 0 gives the foreground color (PS black)
 *   linecolor= 1-10 (MAX_LINE_COLORS=10) gives a line color
 *   linecolor > 10 cycles through the 10 specified colors
 *   color11=color1, etc.
 */
int PXgetlineColorX(linecol)
int linecol;
{
   int index;

   /*
    * -1 is the background color
    *  0 is the foreground color
    *  1-10 are line colors
    */

   if (linecol <= -1) {
      /* Background color (X black) */
      index = 1;

   } else if (linecol == 0) {
      /* Foreground color (X white) */
      index = 0;

   } else {
      /* Index to the appropriate color */ 
      index = PXlineColorIndex(linecol);

   }

   return(colors[index]);
}


/*
 * choose a fillcolor for a gradual ramp 
 *   fillcolor=-1 gives the background color (PS white)
 *   fillcolor= 0 gives the foreground color (PS black)
 *   fillcolor= 1-32 (MAX_FILL_COLORS=32) gives a fill color
 *   linecolor > 32 cycles through the 32 specified colors
 *   color33=color1, etc.
 */
void PXfillColorX(fillcol)
int fillcol;
{
   int index;

   /*
    * -1 is the background color
    *  0 is the foreground color
    *  1-32 are fill colors
    */

   if (fillcol <= -1) {
      /* Background color (X black) */
      index = 1;

   } else if (fillcol == 0) {
      /* Foreground color (X white) */
      index = 0;

   } else {
      /* Index to the appropriate color */
      index = PXfillColorIndex(fillcol);

   }

   XSetForeground(display,gc,colors[index]);
}


/*
 * Set a color
 *   0 = white (foreground color)
 *   1 = black (background color
 *   2-12 are named colors
 *   13-22 are line colors
 *   23-54 are fill colors
 */ 
void PXsetColorX(col)
int col;
{
   int index;

   /*
   if (index < PX_MAX_NAMED_COLORS)
      PXnamedColorX(index);
   else if (index < PX_MAX_FILL_COLORS+PX_MAX_NAMED_COLORS)
      PXfillColorX(index-PX_MAX_NAMED_COLORS+1);
   else
      PXlineColorX(index-PX_MAX_FILL_COLORS-PX_MAX_NAMED_COLORS+1);
    */

   if (col <= -1) {
      /* Background color (X black) */
      index = 1;

   } else if (col == 0) {
      /* Foreground color (X white) */
      index = 0;

   } else {
      /* Index to the appropriate color */
      index = col % (PX_MAX_COLORS);

   }

   XSetForeground(display,gc,colors[index]);
   return;
}


/*
 * Set the index to a fill color - used primarily for drawing polygons
 * Note:
 *   PXlinecolorX(1)                    => yellow (C)
 *   PXsetcolorX(PXpolyColorIndexX(1)) => yellow (C)
 */
int PXpolyColorIndexX(polycolor)
int polycolor;
{
   /* This just returns the lineColorIndex */
   return(PXlineColorIndex(polycolor));
}

/*
 * Set a font in the Gcl font
 * PXsetLabelAnnot(0) resets the font to the normal font.
 */
XFontStruct* PXsetAnnotFontX(fontsize)
int fontsize;
{
   XFontStruct* font=0;

   /* Look up the font */
   if (fontsize <= 0) {
       font = lblfont_info;
   } else if (fontsize < 4) {
       font = fontArr[FONT_SZ_00];
   } else if (fontsize <= 6) {
       font = fontArr[FONT_SZ_08];
   } else if (fontsize <= 8) {
       font = fontArr[FONT_SZ_09];
   } else if (fontsize <= 10) {
       font = fontArr[FONT_SZ_10];
   } else if (fontsize <= 12) {
       font = fontArr[FONT_SZ_12];
   } else if (fontsize <= 13) {
       font = fontArr[FONT_SZ_13];
   } else if (fontsize <= 15) {
       font = fontArr[FONT_SZ_14];
   } else if (fontsize <= 16) {
       font = fontArr[FONT_SZ_15];
   } else if (fontsize <= 20) {
       font = fontArr[FONT_SZ_20];
   } else {
       font = fontArr[FONT_SZ_24];
   } 

   /* If the font is not found, set to the default font */
   if (font == 0) font = lblfont_info;

   /* Set the font */
   XSetFont(display, gcl, font->fid);

   /* Return the font */
   return font;
}


/*
 * USEFUL DRAWING PRIMITIVES
 */

/*
 * Convenience function to fill a polygon
 */
void PXfillX_polygon(points, npoints,
                     filltype, fillcolor, linestyle, linecolor, linethick)
XPoint points[];
int    npoints;
int    filltype, fillcolor, linestyle, linecolor, linethick;
{
   /* Need 2 or more points */
   if (npoints  <= 2) return;

   /* if filltype and linestyle are both NONE (0), return now */
   if ((filltype==CN_FILL_NONE) && (linestyle==CN_LN_NONE)) return;

   /*
    * Now set up the fills
    * Reset the filltype if necessary
    *   Filltype =  0 is no Fill
    *   Filltype = 1-8 is various Filltypes
    *   Filltype 9 => Filltype 1 (Filltypes cycle through 1-8)
    */
   if (filltype != CN_FILL_NONE)
      filltype = (filltype - 1) % CN_FILL_TYPES + 1;

   /*
    * Color the polygon
    */
   if (filltype != CN_FILL_NONE) {

      /*
       * Color it solid first
       */

      /* Set the color */
      PXsetColorX(fillcolor);

      /* Set the filltype */
      PXfilltypX(CN_FILL_SOLID);

      /* Fill the polygon */
      if (window)
      XFillPolygon(display,window,gc,points,npoints,Complex,CoordModeOrigin);
      if (pixmap)
      XFillPolygon(display,pixmap,gc,points,npoints,Complex,CoordModeOrigin);

      /* 
       * Now fill with the appropriate stipple
       */
      if (filltype != CN_FILL_SOLID) {

         /* Set the color */
         if (!dark_background) {
            if (fillcolor==0)   PXsetColorX(-1);
            else                PXsetColorX(0);
         } else {
            if (fillcolor== -1) PXsetColorX(0);
            else                PXsetColorX(-1);
         }

         /* Set the filltype */
         PXfilltypX(filltype);

         /* Fill the polygon */
         if (window)
         XFillPolygon(display,window,gc,points,npoints,Complex,CoordModeOrigin);
         if (pixmap)
         XFillPolygon(display,pixmap,gc,points,npoints,Complex,CoordModeOrigin);
     }

      /* Reset the filltype */
      PXfilltypX(CN_FILL_SOLID);

      /* Reset the color */
      PXsetColorX(0);
   }

   /*
    * Draw the outline of the polygon
    */
   if (linestyle != CN_LN_NONE) {
      /* Set the color */
      PXsetColorX(linecolor);

      /* Set the linetype */
      (void) PXlinetypX(linestyle,linethick);

      /* plot the lines */
      if (window)
      XDrawLines(display,window,gc,points,npoints,CoordModeOrigin);
      if (pixmap)
      XDrawLines(display,pixmap,gc,points,npoints,CoordModeOrigin);

      /* Reset the linetype */
      (void) PXlinetypX(CN_LN_SOLID,1);

      /* Reset the color */
      PXsetColorX(0);
   }
}


/* 
 * Set the fill-style 
 */
static void PXfilltypX(filltype)
int    filltype;
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

   /* Set the patterns */
   switch (filltype) {
   case CN_FILL_NONE: 
            /* Do nothing */
            break;
   case CN_FILL_SOLID :
            /* Solid Fill */
            XSetFillStyle(display, gc, FillSolid);
            break;
   case CN_FILL_SQUARE :
            /* Square grid */
            XSetFillStyle(display, gc, FillStippled);
            XSetStipple(display, gc, stippleA); break;
   case CN_FILL_DIAG1  :
            /* Diagonal lines */
            XSetFillStyle(display, gc, FillStippled);
            XSetStipple(display, gc, stippleB); break;
   case CN_FILL_DIAG2  :
            /* Diagonal lines */
            XSetFillStyle(display, gc, FillStippled);
            XSetStipple(display, gc, stippleC); break;
   case CN_FILL_CHKRBD :
            /* Diagonal lines */
            XSetFillStyle(display, gc, FillStippled);
            XSetStipple(display, gc, stippleD); break;
   case CN_FILL_VERT   :
            /* Vertical lines */
            XSetFillStyle(display, gc, FillStippled);
            XSetStipple(display, gc, stippleE); break;
   case CN_FILL_HORZ   :
            /* Horizontal lines */
            XSetFillStyle(display, gc, FillStippled);
            XSetStipple(display, gc, stippleF); break;
   case CN_FILL_DOTTED :
            /* Dots */
            XSetFillStyle(display, gc, FillStippled);
            XSetStipple(display, gc, stippleG); break;
   default: break;
   }
}



/*
 * Shared 2D/3D functions
 */

/* 
 * Plot line-labels in X11
 */
/*ARGSUSED*/
void PXplotX_linelabels(plotdata, hiddenline, 
                        Xxmin, Xxmax, Xymin, Xymax, xoffset)
CNplotsetptr plotdata;
short        hiddenline;
int    Xxmin, Xxmax, Xymin, Xymax;
int    xoffset;
{
   int    x1, y1, width, height, y1a, y1b, y1c;
   int    text_len, text_xpos, text_ypos;
   XPoint points[10];
   int    npoints;
   int    linepat, colrinc, lineinc;
   int    linetype, linecolor, marktype, markcolor, fillcolor, filltype;
   int    marksize;
   int    applyfill;


   CNdslistptr  DS;
   CNcurveptr   C;

   /*
    * The label must fit inside DEFAULT_BDR_DIM+LABEL_WIDTH = 50+30
    * Leave 50 units for label and 30 units for lines/markers
    */
   /* Initial Points */
   width  = 20;
   height = 25;
   x1     = Xxmax + xoffset;
   y1     = Xymin + height;

   /* Go thru each 2D dataset */
   colrinc = 0;
   lineinc = 0;
   for (DS=plotdata->datahead; DS!=NULL; DS=DS->next) {

      /* Plot only the 2D/3D plot labels */
      if ((DS->Dptr->datatype==CN_PLOT2D) || 
          (DS->Dptr->datatype==CN_PROBAB) ||
          (DS->Dptr->datatype==CN_BARCHART) ||
          (DS->Dptr->datatype==CN_PLOT3D)){

         /* Curve properties inherited from the dataset */
         applyfill = DS->Dptr->data_pr.applyfill;

         /* Curve set */
         for (C=DS->Dptr->curvehead; C!=NULL; C=C->next) {

            y1a = y1 - font_info->max_bounds.ascent;
            y1b = y1 - height;
            y1c = 0.5*(y1a+y1b);

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
               linepat = PXlinetypX(linetype,C->curv_pr.linewidth);
               PXlineColorX(linecolor);
               if (linepat) {
                  if (window)
                  XDrawLine(display,window,gc, x1,y1c,x1+width,y1c);
                  if (pixmap)
                  XDrawLine(display,pixmap,gc, x1,y1c,x1+width,y1c);
               }

               /* Draw the markers */
               (void) PXlinetypX(CN_LN_SOLID,1);
               PXlineColorX(markcolor);
               PXmarkerX(marktype,marksize,x1      ,y1c);
               PXmarkerX(marktype,marksize,x1+width,y1c);

            } else {

               /* Draw the rectangle */
               linepat = PXlinetypX(linetype,C->curv_pr.linewidth);
               npoints=5;
               points[0].x = x1      ;  points[0].y = y1a;
               points[1].x = x1      ;  points[1].y = y1b;
               points[2].x = x1+width;  points[2].y = y1b;
               points[3].x = x1+width;  points[3].y = y1a;
               points[4].x = x1      ;  points[4].y = y1a;
               PXfillX_polygon(points,npoints,
                               filltype, PXlineColorIndex(fillcolor),
                               CN_LN_NONE,0,1);

               /* Set the line color */
               PXlineColorX(linecolor);

               /* Draw the outline */
               if (linepat) {
                  if (window)
                  PXDrawRectangle(display,window,gc,x1,y1a,width,y1b-y1a);
                  if (pixmap)
                  PXDrawRectangle(display,pixmap,gc,x1,y1a,width,y1b-y1a);
               }

               /* Draw the markers */
               (void) PXlinetypX(CN_LN_SOLID,1);
               PXlineColorX(markcolor);
               PXmarkerX(marktype,marksize,x1      ,y1a);
               PXmarkerX(marktype,marksize,x1+width,y1a);
               PXmarkerX(marktype,marksize,x1      ,y1b);
               PXmarkerX(marktype,marksize,x1+width,y1b);

            }

            /* Reset the foreground */
            PXsetColorX(0);

            /* Draw the text */
            text_len   = CNstrlen(C->curv_pr.linelabel);
            /*
            text_xpos  = x1 + 1.3*width;
            text_ypos  = y1 + 0.5*font_info->max_bounds.ascent;
             */
            text_xpos  = x1;
            text_ypos  = y1;

            if (window)
            XDrawString(display,window,gc,
                        text_xpos,text_ypos,C->curv_pr.linelabel,text_len);
            if (pixmap)
            XDrawString(display,pixmap,gc,
                        text_xpos,text_ypos,C->curv_pr.linelabel,text_len);

            /* Reset the position */
            y1 += 1.4*height;
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
   PXsetColorX(0);

}

/*
 * Plot a color-scale in X11
 */
/*ARGSUSED*/
void PXplotX_contscale(cstephead, csteptail, 
                       Xxmin, Xxmax, Xymin, Xymax, xoffset, contclip)
CNcontstepptr cstephead, csteptail;
int    Xxmin, Xxmax, Xymin, Xymax;
int    xoffset;
short  contclip;
{
   CNcontstepptr C;
   double x0, y0, xwid, ywid;
   int    xl, yl;
   int    text_xpos, text_ypos, text_len;
   char   text[10];
   int    i, colr, inc, nctrs;

   /* Error check */
   if (cstephead == 0) return;

   /* Count no of steps */
   nctrs = CNcount_contsteps(cstephead, csteptail) - 1;

   /*
    * The label must fit inside DEFAULT_BDR_DIM+LABEL_WIDTH = 50+30
    * Leave 50 units for label and 30 units for lines/markers
    * There are NCTRS+1 bands.
    */
   ywid  = (Xymax-Xymin)/(double)(nctrs+1);
   xwid  = 20;
   xl    = (int)(xwid);
   yl    = (int)(ywid) + 1;

   /* Initial position of the scale */
   x0    = Xxmax + xoffset;

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
      y0 = Xymax - i*ywid - ywid;
      if (y0 < Xymin) y0 = Xymin;

      PXfillColorX(colr);
      if (contclip && (C==cstephead || C==csteptail)) PXfillColorX(-1);
      if (window)
      XFillRectangle(display,window,gc,(int)x0,(int)y0,xl,yl);
      if (pixmap)
      XFillRectangle(display,pixmap,gc,(int)x0,(int)y0,xl,yl);

      inc = (nctrs > 15) ? 3 : 1;
      if ((i%inc == 0 || i == 0) && (i != nctrs)) {
         /* Label the rectangle */
         (void) sprintf(text,"%.3g",C->value);
         text_len   = strlen(text);
         text_xpos  = (int)(x0 + xl + 4);
         text_ypos  = (int)(y0 + 0.5*lblfont_height);
         PXsetColorX(0);
         if (window) {
         XDrawString(display,window,gc,text_xpos,text_ypos,text,text_len);
         XDrawLine(display,window,gc,
                   (int)(x0-2),(int)(y0),
                   (int)(x0),(int)(y0));
         XDrawLine(display,window,gc,
                   (int)(x0+xl-2),(int)(y0),
                   (int)(x0+xl),(int)(y0));
         }
         if (pixmap) {
         XDrawString(display,pixmap,gc,text_xpos,text_ypos,text,text_len);
         XDrawLine(display,pixmap,gc,
                   (int)(x0-2),(int)(y0),
                   (int)(x0),(int)(y0));
         XDrawLine(display,pixmap,gc,
                   (int)(x0+xl-2),(int)(y0),
                   (int)(x0+xl),(int)(y0));
         }
      }

      /* Increment the step number */
      i++;
   }

   /* Draw a big rectangle to outline the colors */
   PXsetColorX(0);
   y0   = Xymin;
   yl   = Xymax - Xymin;
   if (window)
   PXDrawRectangle(display,window,gc,(int)(x0),(int)(y0),xl, yl);
   if (pixmap)
   PXDrawRectangle(display,pixmap,gc,(int)(x0),(int)(y0),xl, yl);
}

/*
 * Plot an annotation text-label.
 * The font-size of this text-label is scalable.
 * Default is to center-justify the text.
 */
void PXplotX_scalable_font(xc, yc, label,
                           fontsize,
                           leftJustify, rightJustify,
                           topJustify, bottomJustify)
double xc;             /* x-coordinate     */
double yc;             /* y-coordinate     */
char*  label;          /* text label       */
int    fontsize;       /* font size        */
int    leftJustify;    /* left-justified   */
int    rightJustify;   /* right-justified  */
int    topJustify;     /* top-justified    */
int    bottomJustify;  /* bottom-justified */
{
   int text_len, text_width, text_xpos, text_ypos;
   XFontStruct* font;
 
   /* Set the font */
   font = PXsetAnnotFontX(fontsize);
 
   /* Calculate the position of the text */
   text_len   = strlen(label);
   text_width = XTextWidth(font, label, text_len);

   /* Adjust the left-right location */
   if (leftJustify) {
      /* Left-justified */
      text_xpos = xc + 6;
   } else if (rightJustify) {
      /* Right-justified */
      text_xpos = xc - text_width - 6;
   } else {
      /* Center-justified */
      text_xpos = xc - text_width/2;
   }
 
   /* Adjust the top-bottom location */
   if (topJustify) {
      /* Print the text on top of the point */
      text_ypos = yc + font->max_bounds.ascent + 2;
   } else if (bottomJustify) {
      /* Print the text on bottom of the point */
      text_ypos = yc - font->max_bounds.ascent - 2;
   } else {
      /* Center-justified */
      text_ypos = yc + 0.5*font->max_bounds.ascent;
   }
 
   /* Draw the text */
   if (window)
   XDrawString(display,window,gcl,
               text_xpos,text_ypos,label,text_len);
   if (pixmap)
   XDrawString(display,pixmap,gcl,
               text_xpos,text_ypos,label,text_len);
 
   /* Reset the font */
   (void) PXsetAnnotFontX(0);
}



/*
 * TRANSLATION ROUTINES
 */


/* 
 * Translate world to X11 coordinates and apply log options
 */
void PXtranslate_world_to_X11(xw, yw, xp, yp,
                        Xxmin, Xxmax, Xymin, Xymax,
                        xmin, xmax, ymin, ymax,
                        xlog, ylog, xflip, yflip)
double  xw,  yw;                   /* World coordinates            */
double *xp, *yp;                   /* X11 plot coordinates         */
int     Xxmin, Xxmax, Xymin, Xymax;/* X11 viewport                 */
double  xmin,  xmax,  ymin,  ymax; /* world (linear/log) viewport  */
short   xlog,  ylog;               /* Logarithmic scales           */
short   xflip, yflip;              /* xmin is mapped to Xmax       */
{
   double x_min, x_max, y_min, y_max, x1, y1, tmp;

   /* 
    * Linear scale :
    *        xmin......x......xmax
    *          |       |        |
    *       Xxmin......X.....Xxmax
    *
    * Log scale :
    *        xmin..... x..... xmax
    *          |       |        |
    *       Xxmin......X.....Xxmax
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
   _PXtranslate_world_to_X11(x1, y1, xp, yp,
                      Xxmin, Xxmax, Xymin, Xymax,
                      x_min, x_max, y_min, y_max);
}

/*
 * Translate from real coordinates to X11 coordinates
 */
static void _PXtranslate_world_to_X11(xw, yw, xp, yp,
                        Xxmin, Xxmax, Xymin, Xymax,
                        xmin, xmax, ymin, ymax)
double  xw,  yw;                   /* World coordinates            */
double *xp, *yp;                   /* X11 plot coordinates         */
int     Xxmin, Xxmax, Xymin, Xymax;
double  xmin, xmax, ymin, ymax;
{
   double xval, yval;

   /*
    * Translate from X11 coordinate to real coordinates.
    * The X11 is special in that ymin corresponds to Xymax etc.
    */
   PXtranslate_range(&xval,(double)Xxmin,(double)Xxmax,xw,xmin,xmax);
   PXtranslate_range(&yval,(double)Xymax,(double)Xymin,yw,ymin,ymax);
   *xp = xval;
   *yp = yval;
} 

/* 
 * Translate X11 to world coordinates and apply log options
 */
void PXtranslate_X11_to_world(xp, yp, xw, yw,
                        Xxmin, Xxmax, Xymin, Xymax,
                        xmin, xmax, ymin, ymax,
                        xlog, ylog, xflip, yflip, probability_plot)
double  xp,  yp;                   /* X11 plot coordinates         */
double *xw, *yw;                   /* World coordinates            */
int     Xxmin, Xxmax, Xymin, Xymax;/* X11 viewport                 */
double  xmin,  xmax,  ymin,  ymax; /* world (linear/log) viewport  */
short   xlog,  ylog;               /* Logarithmic scales           */
short   xflip, yflip;              /* xmin is mapped to Xmax       */
short   probability_plot;          /* The y-axis is on probability */
{
   double x_min, x_max, y_min, y_max, x1, y1, tmp;
   int    error;

   /* 
    * Linear scale :
    *        xmin......x......xmax
    *          |       |        |
    *       Xxmin......X.....Xxmax
    *
    * Log scale :
    *        xmin..... x..... xmax
    *          |       |        |
    *       Xxmin......X.....Xxmax
    *    where xmin is the rounded log(real_xmin)
    */

   /* Get the actual world viewport to be translated */
   x_min =  xmin; x_max =  xmax; 
   y_min =  ymin; y_max =  ymax;

   if (probability_plot) {
      /* Figure out the y-axes limits for the probability plot */
      y_min = CNnorm_vert_distance(ymin, &error);
      y_max = CNnorm_vert_distance(ymax, &error);
   }

   /* Interchange min and max limits if necessary */
   if (xflip){ tmp= x_max;       x_max = x_min; x_min = tmp;   }
   if (yflip){ tmp= y_max;       y_max = y_min; y_min = tmp;   }

   /* Now translate from X11 coords to world coordinates */
   _PXtranslate_X11_to_world(xp, yp, &x1, &y1,
                    Xxmin, Xxmax, Xymin, Xymax,
                    x_min, x_max, y_min, y_max);

   if (probability_plot) {
      /* Now translate to probability scale */
      y1 = 0.5*erf(y1/sqrt(2.0)) + 0.5;
   }

   /* Now get the linear world coordinates (inverse log if necessary) */
   if (xlog) *xw = pow(10.0,x1);
   else      *xw = x1;
   if (ylog) *yw = pow(10.0,y1);
   else      *yw = y1;
   if (probability_plot) *yw = y1;
}

/*
 * Translate from X11 coordinates to real coordinates
 * This does the actual translation.
 */
static void _PXtranslate_X11_to_world(xp, yp, xw, yw, 
                             Xxmin, Xxmax, Xymin, Xymax,
                             xmin, xmax, ymin, ymax)
double  xp,  yp;                   /* X11 plot coordinates         */
double *xw, *yw;                   /* World coordinates            */
int     Xxmin, Xxmax, Xymin, Xymax;
double  xmin, xmax, ymin, ymax;
{
   double xval, yval; 

   /* 
    * Translate from X11 coordinate to real coordinates.
    * The X11 is special in that ymin corresponds to Xymax etc.
    */
   PXtranslate_range(&xval,xmin,xmax,(double)xp,(double)Xxmin,(double)Xxmax);
   PXtranslate_range(&yval,ymin,ymax,(double)yp,(double)Xymax,(double)Xymin);
   *xw = xval;
   *yw = yval; 
}



/*
 * Draw the plot in X
 */
int PXdrawplotX(pptr, 
                display, window, pixmap, 
                gc0, gc1, font0, font1, Width, Height,
                xxmin, xxmax, xymin, xymax, frame_only, printdate)
CNplotsetptr pptr;
Display      *display;
Window       window;
Pixmap       pixmap;
GC           gc0, gc1;
XFontStruct  *font0, *font1;
int          Width, Height;
int          *xxmin, *xxmax, *xymin, *xymax;
int          frame_only;
int          printdate;
{

   /* Error */
   if ((pptr == NULL) || (pptr->datahead == NULL)) {
      draw_empty_view(display, window, pixmap,
                      gc0, gc1, font0, font1, Width, Height);
      *xxmin = 0;
      *xxmax = 0;
      *xymin = Width;
      *xymax = Height;
      return(0);
   }

   if (pptr->plottype == CN_PLOT3D) {

      /* draw the plot in 3D */
      PXdrawplotX3D(pptr,
                    printdate,
                    display, window, pixmap,
                    gc0, gc1, font0, font1, Width, Height,
                    xxmin, xxmax, xymin, xymax, frame_only);

   } else {

      PXdrawplotX2D(pptr,
                    printdate,
                    display, window, pixmap,
                    gc0, gc1, font0, font1, Width, Height,
                    xxmin, xxmax, xymin, xymax);
   }

   /* return */
   return(1);
}

/*
 * Draw frame and a warning message
 */
/*ARGSUSED*/
static void draw_empty_view(display, window, pixmap,
                            gc0, gc1, font0, font1, Width, Height)
Display      *display;
Window       window;
Pixmap       pixmap;
GC           gc0, gc1;
XFontStruct  *font0, *font1;
int          Width, Height;
{
   int  i, Offset = 20;
   int    font_height;
   int    text_len, text_width, text_xpos, text_ypos;

   static char *warning[] = {
      "Warning : No data to plot!",
      ""};

   /* Draw a frame */
   if (window)
   PXDrawRectangle(display, window, gc0,
                  Offset, Offset, Width-2*Offset, Height-2*Offset);
   if (pixmap)
   PXDrawRectangle(display, pixmap, gc0,
                  Offset, Offset, Width-2*Offset, Height-2*Offset);

   /* Labels */
   font_height = font0->max_bounds.ascent +
                 font0->max_bounds.descent;

   /* Warning Label */
   text_ypos  = Height/2 - 3*font_height;
   i=0;
   while (warning[i][0] != '\0') {
      text_len   = strlen(warning[i]);
      text_width =
      XTextWidth(font0,warning[i],text_len);
      text_xpos  = (Width - text_width)/2;
      text_ypos  = text_ypos + 2*font_height;
      if (window)
      XDrawString(display, window, gc0,
                  text_xpos,text_ypos,warning[i],text_len);
      if (pixmap)
      XDrawString(display, pixmap, gc0,
                  text_xpos,text_ypos,warning[i],text_len);
      i++;
   }
}


/*
 * Draw a rectangle in X 
 * Make sure that dx, dy are always positive
 */
void PXDrawRectangle(display, window, gc, x, y, dx, dy)
Display *display;
Window  window;
GC      gc;
int     x, y, dx, dy;
{
   if (dx==0 || dy==0) return;
   if (dx < 0) {
      x  = x + dx;
      dx = -dx;
   }
   if (dy < 0) {
      y  = y + dy;
      dy = -dy;
   }
   /*
   printf("drawing rectangle x=%d y=%d dx=%d dy=%d\n",x,y,dx,dy);
    */
   XDrawRectangle(display, window, gc, x, y, dx, dy);
}

