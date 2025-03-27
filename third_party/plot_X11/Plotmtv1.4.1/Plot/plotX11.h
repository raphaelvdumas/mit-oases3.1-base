/*
 * plotX11.h - X11 variables and declarations used by all X11 routines.
 */

#ifndef plotX11_defined
#define plotX11_defined

#include <X11/Intrinsic.h>
#include <X11/Shell.h>
#include <X11/StringDefs.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>

#include "PXplot.h"

/*
 * EXTERNAL DECLARATIONS
 */
extern int  PXAllocColors();
extern int  PXlinetypX();
extern void PXmarkerX();
extern void PXnamedColorX();
extern void PXlineColorX();
extern void PXfillColorX();
extern void PXsetColorX();
extern int  PXpolyColorIndexX();
extern XFontStruct* PXsetAnnotFontX();

extern void PXfillX_polygon();

extern void PXplotX_linelabels();
extern void PXplotX_contscale();
extern void PXplotX_scalable_font();

extern void PXtranslate_world_to_X11();
extern void PXtranslate_X11_to_world();

extern int  PXdrawplotX();
extern void PXdrawplotX2D();
extern void PXdrawplotX3D();

extern unsigned long background_pixel;             /* color of background  */
extern unsigned long foreground_pixel;             /* color of foreground  */
extern unsigned long colors[PX_MAX_COLORS];        /* color pixel values   */
extern int           dark_background;              /* background hints     */

/* Widgets and X11-related variables */
extern Display     *display;                       /* display */
extern Window      window;                         /* parent window */
extern Pixmap      pixmap;                         /* pixmap to draw into */
extern XFontStruct *font_info;                     /* font information */
extern XFontStruct *lblfont_info;                  /* label font information */
extern GC          gc, gcl;                        /* graphics context */
extern int         font_height, font_width;        /* More font info   */
extern int         lblfont_height, lblfont_width;  /* More font info   */

/* font array */
extern XFontStruct* fontArr[PX_MAX_FONTS];         /* Font array */

/* plot information */
#define X_ORG            5
#define Y_ORG            5
#define X_DIM            600
#define Y_DIM            800
#define DEFAULT_BDR_DIM  70
#define LABEL_WIDTH      30

/* Misc */
#define DEFAULT_BORDER_WIDTH  3
#define MAXCHR                CN_MAXCHAR

/* The dimensions of the Xwindow plot */
extern int Xxmin, Xxmax, Xymin, Xymax, Width, Height;

#endif /* plotX11_defined */

