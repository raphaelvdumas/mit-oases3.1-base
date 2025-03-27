/*
 * plot3D.h - definitions for 3D plots
 *
 * This requires the inclusion of "CNdata.h"
 */

#ifndef CNplot3D_defined
#define CNplot3D_defined

#define CN_NDIM   4                       /* The Matrix size        */
typedef double CNmatrix[CN_NDIM][CN_NDIM];/* The matrix data struct */

/* projection style */
#define CN_PARALLEL    0
#define CN_PERSPECTIVE 1

/* Window size */
#define CN_WINDOWSCALE 0.7

/*
 * xmin xmax etc are plot-boundaries.  These are in fact copied from
 * the dataset's plot-boundaries (pxmin etc)
 * Note that the plot-boundaries are always recalculated everytime
 * a 3D plot is made.
 */

/* Flags argument for changing view_structure */
#define CNeyepos     (1L << 0)    /* User-specified eye-position       */
#define CNviewctr    (1L << 1)    /* User-specified view-center        */
#define CNviewup     (1L << 2)    /* User-specified viewup vector      */
#define CNrealbnd    (1L << 3)    /* User-specified real-world boundary*/
#define CNviewport   (1L << 4)    /* User-specified viewport           */
#define CNwindow     (1L << 5)    /* User-specified window             */
#define CNleft_hw    (1L << 6)    /* User-specified left-hand world    */
#define CNaxisscl    (1L << 7)    /* User-specified axis-scale         */
#define CNaxislbl    (1L << 8)    /* User-specified axis-label         */
#define CNaxismvt    (1L << 9)    /* User-specified axis-movement      */
#define CNaxisguides (1L << 10)   /* User-specified 3D Guides          */
#define CNhiddenln   (1L << 11)   /* User-specified hiddenline         */
#define CNpaintcb    (1L << 12)   /* User-specified paintcube          */
#define CNproject    (1L << 13)   /* User-specified projection style   */
#define CNxaxisscl   (1L << 14)   /* User-specified x-axis-scale       */
#define CNyaxisscl   (1L << 15)   /* User-specified y-axis-scale       */
#define CNzaxisscl   (1L << 16)   /* User-specified z-axis-scale       */

typedef struct CNview_strct {           /* View parameters        */
   long     flag;
   CNcoord  eyepos;
   CNcoord  viewcenter;
   CNcoord  viewup;
   double   xmin, xmax;
   double   ymin, ymax;
   double   zmin, zmax;
   double   windscl_xl, windscl_xr;
   double   windscl_yb, windscl_yt;
   double   prev_windscl_xl, prev_windscl_xr;
   double   prev_windscl_yb, prev_windscl_yt;
   double   window_xl, window_xr;
   double   window_yb, window_yt;
   double   viewport_xl, viewport_xr;
   double   viewport_yb, viewport_yt;
   double   xaxis_scale;
   double   yaxis_scale;
   double   zaxis_scale;
   short    left_handed_world;
   short    axis_scale;
   short    axis_label;
   short    axis_movement;
   short    axis_guides;
   short    hiddenline;
   short    paint_cube;
   short    projection;
   CNmatrix view_transfo;
} CNview;

typedef struct CNview_strct *CNviewptr;

/* External declarations */
extern CNviewptr CNcreate_view();
extern void      CNdelete_view();
extern void      CNinitialize_view();
extern void      CNreinitialize_view();
extern void      CNcalculate_view_transfo();
extern void      CNcalculate_inv_view_transfo();
extern void      CNscale_window_to_viewport();
extern void      CNscale_viewport_to_window();
extern void      CNcalculate_eyepos();
extern void      CNrotate_view();
extern void      CNget_view_angles();
extern void      CNcopy_matrix();
extern CNcoord   CNtransform_point();

#endif /* CNplot3D_defined */

