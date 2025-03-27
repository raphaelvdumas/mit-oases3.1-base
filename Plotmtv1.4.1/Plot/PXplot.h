/*
 * PXplot.h - external procedure declarations
 */

#ifndef PXplot_defined
#define PXplot_defined

/* Postscript variables */
#define PX_RAW     0
#define PX_EPSI    1
#define PX_EPSF    2

/*
 * The colors are stored in a array with 55 elements.
 * The first 13 are named colors: 0 = foreground, 1 = background and
 *    the remaining 11 are colors like yellow, red, etc.
 * The next 32 colors are fill colors which ramp from blue to red.
 * The next 10 colors are pixel values selected from among the previous 42
 * colors to provide a set of contrasting colors for lines.
 *
 * To access the colors, the user sets the line color from -1 to inf;
 * -1 returns the background color, 0 returns the foreground color, and
 * 1-inf returns one of 10 line colors.
 */

/*
 * Line/fill colors
 */
#define PX_MAX_NAMED_COLORS      13
#define PX_MAX_FILL_COLORS       32
#define PX_MAX_LINE_COLORS       10
#define PX_MAX_COLORS PX_MAX_NAMED_COLORS+PX_MAX_FILL_COLORS+PX_MAX_LINE_COLORS

/*
 * Fonts
 */
#define PX_MAX_FONTS 10

/*
 * Data-structure for axis label placement
 */
#define PX_MAX_LABELS           100  /* Max number of labels on an axis */
typedef struct PXlabel_strct {
   double value;
   double x;
   double y;
} PXlabel;
 
/* X11 Initialization */
extern int  PXinitXWindow();

/* X11 Rectangles (for rubberbands) */
extern void PXDrawRectangle();

/* Postscript plotting */
extern void PXplotps();
extern void PXplotps_mult();

/* X11 plotting */
extern int  PXdrawplotX();

/* Translation */
extern void PXtranslate_world_to_X11();
extern void PXtranslate_X11_to_world();

/* Generic viewport utilities for plotting */
extern void PXcheck_viewport();
extern void PXconvert_viewport_to_log();
extern void PXtranslate_range();
extern void PXget_autorange();
extern void PXidentify_view_planes();
extern void PXfind_outer_axes();
extern void PXquery_contours();
extern int  PXquery_labels();

extern int  PXnamedColorIndex();
extern int  PXlineColorIndex();
extern int  PXfillColorIndex();

extern void PXadd_axislabel();
extern void PXfind_axis_precision();
extern void PXmodify_explabel();

#endif /* PXplot_defined */

