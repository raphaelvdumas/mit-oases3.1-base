/*
 * plotps.h - variables and declarations used by all PostScript routines
 */

#include <stdio.h>
#include <strings.h>
#include <math.h>

#include "PXplot.h"

#ifndef plotps_defined
#define plotps_defined

/*
 * EXTERNAL DECLARATIONS
 */
extern void   PXplotps2D();
extern void   PXplotps3D();

extern int    PXlinetypPS();
extern void   PXmarkerPS();
extern void   PXnamedColorPS();
extern void   PXlineColorPS();
extern void   PXfillColorPS();
extern void   PXsetColorPS();
extern int    PXpolyColorIndexPS();
extern void   PXsetAnnotFontPS();

extern void   PXfillPS_polygon();
extern void   PXdrawPS_line();

extern void   PXplotPS_linelabels();
extern void   PXplotPS_contscale();
extern void   PXplotPS_scalable_font();

extern void   PXtranslate_world_to_PS();
extern void   PXtranslate_PS_to_world();

extern char  *PXmodifyPS_string();

typedef struct _PSPoint {
   double x;
   double y;
} PSPoint;

/* Borders */
#define PX_DIM   600
#define PY_DIM   800
#define PXL_DIM  800
#define PYL_DIM  600
#define PBDR_DIM 100

/* Misc */
#define MAXCHR                CN_MAXCHAR

/* Plot scale */
extern double scale;
extern double fscale;

/* Landscape */
extern short landscape;

/* Print the date */
extern short printdate;

/* Color */
extern short pscolor;

/* PS page dimensions */
extern double  Pgxmin, Pgxmax, Pgymin, Pgymax;

/* FILE */
extern FILE *ips;

/* Font sizes */
#define PS_TOPFONT   25
#define PS_SIDEFONT  20
#define PS_AXISFONT  15
#define PS_MKRSFONT  12
#define PS_ANNOTFONT 12
#define PS_DATEFONT  10
#define PS_CTRFONT    8

#endif /* plotps_defined */

