/*
 * CNspline.h - spline definitions and subroutines
 */

#ifndef CNspline_defined
#define CNspline_defined

/* Spline types */
#define CN_SPLINETYPES  6        /* No of spline types       */
#define CN_SP_NONE      0        /* No spline                */
#define CN_SP_CUBICB    1        /* Cubic B-spline           */
#define CN_SP_DBLCUBICB 2        /* Doubled Cubic B-spline   */
#define CN_SP_QUADRB    3        /* Quadratic B-spline       */
#define CN_SP_CTROM     4        /* Catmull-Rom spline       */
#define CN_SP_CBBEZIER  5        /* Cubic Bezier spline      */
#define CN_SP_QDBEZIER  6        /* Quadratic Bezier spline  */

/* Spline procedures */
extern char         *CNsplinetype();
extern void         CNcreate_spline();
extern void         CNmake_cubic_B_spline();
extern void         CNmake_closed_cubic_B_spline();
extern void         CNmake_double_cubic_B_spline();
extern void         CNmake_double_closed_cubic_B_spline();
extern void         CNmake_quadr_B_spline();
extern void         CNmake_closed_quadr_B_spline();
extern void         CNmake_ctrom_spline();
extern void         CNmake_closed_ctrom_spline();
extern void         CNmake_quadr_bezier_spline();
extern void         CNmake_closed_quadr_bezier_spline();
extern void         CNmake_cubic_bezier_spline();
extern void         CNmake_closed_cubic_bezier_spline();

#endif /* CNspline_defined */

