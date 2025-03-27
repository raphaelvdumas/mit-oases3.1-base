/*
 * CNproperty.h - definitions for object properties. 
 *
 * This file requires the inclusion of "CNdata.h"
 */

#ifndef CNproperty_defined
#define CNproperty_defined

#include "CNaxislabel.h"

/*
 * CURVES
 */

/* 
 * The properties of the curve are stored in the CNcurve_prop data-structure. 
 * Changes to the property are made using the flags, as is done in X11.
 * The use of this structure allows us to read in the property of the
 * curve first before attaching the properties to the curve.
 * The properties of a curve may be changed either by seting a property  
 * directly (curve->curve_prop.linetype=1) or by initializing a default
 * curve property and attaching that property to the curve.
 */

/* Flags argument for changing curve_property */
#define CNlinelabel (1L << 0)    /* User-specified line label         */
#define CNlinewidth (1L << 1)    /* User-specified line width         */
#define CNlinetype  (1L << 2)    /* User-specified line type          */
#define CNlinecolor (1L << 3)    /* User-specified line color         */
#define CNmarksize  (1L << 4)    /* User-specified marker size        */
#define CNmarktype  (1L << 5)    /* User-specified marker type        */
#define CNmarkcolor (1L << 6)    /* User-specified marker color       */
#define CNfilltype  (1L << 7)    /* User-specified fill type          */
#define CNfillcolor (1L << 8)    /* User-specified fill color         */

typedef struct CNcurve_property_strct {
   long   flag;
   char   *linelabel;                  /* Line label    */
   short  linewidth;                   /* Line width    */
   short  linetype;                    /* Line type     */
   short  linecolor;                   /* Line color    */
   short  marksize;                    /* Marker Type   */
   short  marktype;                    /* Marker Type   */
   short  markcolor;                   /* Marker Color  */
   short  filltype;                    /* Fill type     */
   short  fillcolor;                   /* Fill color    */
} CNcurve_property;


/*
 * The global-curve-property contains info on global control of all
 * curves in a dataset.  The global command is only used once,
 * i.e. it behaves like a single loop acting on all the curves in the
 * dataset (e.g. set all linetypes=1). As a result we don't have to
 * keep the global-curve property with the dataset. 
 */

/* Flags argument for changing curve_property */
#define CNlnlabel    (1L <<  0)   /* User-specified line label         */
#define CNminlntyp   (1L <<  1)   /* User-specified 1st linetype       */
#define CNmaxlntyp   (1L <<  2)   /* User-specified last linetype      */
#define CNlinechg    (1L <<  3)   /* User-specified Linechange         */
#define CNlnwidth    (1L <<  4)   /* User-specified linewidth          */
#define CNlntype     (1L <<  5)   /* User-specified linetype           */
#define CNlncolor    (1L <<  6)   /* User-specified linecolor          */
#define CNminmktyp   (1L <<  7)   /* User-specified 1st markertype     */
#define CNmaxmktyp   (1L <<  8)   /* User-specified last markertype    */
#define CNmarkchg    (1L <<  9)   /* User-specified markerchange       */
#define CNmksize     (1L << 10)   /* User-specified marksize           */
#define CNmktype     (1L << 11)   /* User-specified marktype           */
#define CNmkcolor    (1L << 12)   /* User-specified markcolor          */
#define CNminfltyp   (1L << 13)   /* User-specified 1st filltype       */
#define CNmaxfltyp   (1L << 14)   /* User-specified last filltype      */
#define CNfillchg    (1L << 15)   /* User-specified fillchange         */
#define CNfltype     (1L << 16)   /* User-specified filltype           */
#define CNflcolor    (1L << 17)   /* User-specified fillcolor          */

typedef struct CNgbcurve_property_strct {
   long   flag;
   char   *lnlabel;                    /* Line label                    */
   short  minlntyp;                    /* Line type of 1st curve        */
   short  maxlntyp;                    /* Line type of last curve       */
   short  linechg;                     /* Change the line type?         */
   short  lnwidth;                     /* Line width for all curves     */
   short  lntype;                      /* Line type  for all curves     */
   short  lncolor;                     /* Line color for all curves     */
   short  minmktyp;                    /* Markertype of 1st curve       */
   short  maxmktyp;                    /* Markertype of last curve      */
   short  markchg;                     /* Change the marker type?       */
   short  mksize;                      /* Marker size  for all curves   */
   short  mktype;                      /* Marker type  for all curves   */
   short  mkcolor;                     /* Marker color for all curves   */
   short  minfltyp;                    /* Filltype of 1st curve         */
   short  maxfltyp;                    /* Filltype of last curve        */
   short  fillchg;                     /* Change the fill type?         */
   short  fltype;                      /* Fill type  for all curves     */
   short  flcolor;                     /* Fill color for all curves     */
} CNgbcurve_property;


/* 
 * The dataset contains all the information needed for a single plot,
 * including relevant options, linked lists.
 */

/* 
 * The properties of the dataset are stored in the CNproperty data-structure. 
 * Changes to the property are made using the flags, as is done in X11.
 * The use of this structure allows us to read in the property of the
 * dataset first before attaching the properties to the dataset.
 * The properties of a dataset may be changed either by accessing the dataset
 * directly (dataset->property.linetype=1) or by setting global properties here.
 */

/* Flags argument for changing dataset_property */
#define CNstepmethod (1L << 0)    /* User-specified contour step method    */
#define CNctrlevel   (1L << 1)    /* User-specified contour steps          */
#define CNcstep      (1L << 2)    /* User-specified contour step size      */
#define CNnstep      (1L << 3)    /* User-specified no of contour steps    */
#define CNlogzstep   (1L << 4)    /* User-specified log(z) step size       */
#define CNcmax       (1L << 5)    /* User-specified max contour level      */
#define CNcmin       (1L << 6)    /* User-specified min contour level      */
#define CNlogx       (1L << 7)    /* User-specified log interpolation in x */
#define CNlogy       (1L << 8)    /* User-specified log interpolation in y */
#define CNlogz       (1L << 9)    /* User-specified log interpolation in z */
#define CNlinetypes  (1L << 10)   /* User-specified # of linetypes         */
#define CNcontstyle  (1L << 11)   /* User-specified contour fill style     */
#define CNcontlabel  (1L << 12)   /* User-specified contour label          */
#define CNcontintrp  (1L << 13)   /* User-specified contour interpolation  */
#define CNcontclip   (1L << 14)   /* User-specified contour clipping       */
#define CNcontbitmap (1L << 15)   /* User-specified contour bitmap format  */
#define CNmeshplot   (1L << 16)   /* User-specified mesh plot              */
#define CNboundary   (1L << 17)   /* User-specified boundary plot          */
#define CNregbound   (1L << 18)   /* User-specified region boundary plot   */
#define CNfillbnd    (1L << 19)   /* User-specified fill boundary plot     */
#define CNvlog       (1L << 20)   /* User-specified log-vector             */
#define CNvlogscale  (1L << 21)   /* User-specified log-vector scale       */
#define CNvscale     (1L << 22)   /* User-specified linear-vector scale    */
#define CNvhead      (1L << 23)   /* User-specified vector head type       */
#define CNvtail      (1L << 24)   /* User-specified vector tail type       */
#define CNsplinetyp  (1L << 25)   /* User-specified splinetype             */
#define CNapplyfill  (1L << 26)   /* User-specified fill application       */

/* Second set of parameters... */
#define CNpr_ptID    (1L << 0)    /* User-specified ID-print flag          */
#define CNpr_ndID    (1L << 1)    /* User-specified ID-print flag          */
#define CNpr_trID    (1L << 2)    /* User-specified ID-print flag          */
#define CNpr_rtID    (1L << 3)    /* User-specified ID-print flag          */
#define CNpr_rgID    (1L << 4)    /* User-specified ID-print flag          */
#define CNpr_cvID    (1L << 5)    /* User-specified ID-print flag          */
#define CNbinwidth   (1L << 6)    /* User-specified histogram bin start    */
#define CNbinstart   (1L << 7)    /* User-specified histogram bin start    */
#define CNbarmin     (1L << 8)    /* User-specified barchart bar min val   */
#define CNplotannot  (1L << 9)    /* User-specified annotation plot        */

typedef struct CNdataset_property_strct {
   long   flag1;
   long   flag2;
   short  contstyle;    /* CONTOUR */  /* Plot style for contours        */
   short  stepmethod;                  /* Contour step method            */
   char   *contours;                   /* Individual contour steps       */
   double cstep;                       /* Contour step size for contour  */
   int    nsteps;                      /* No of contour steps            */
   int    logzstep;                    /* logz contour step size         */
   double cmax;                        /* Maximum contour value          */
   double cmin;                        /* Minimum contour value          */
   short  logx;                        /* logarithmic interpolation in x */
   short  logy;                        /* logarithmic interpolation in y */
   short  logz;                        /* logarithmic interpolation in z */
   short  linetypes;                   /* No of linetypes                */
   short  contlabel;                   /* Print the contour labels       */
   short  contintrp;                   /* Interpolation method           */
   short  contclip;                    /* Clip contours in t             */
   short  contbitmap;                  /* Bitmap format for filled ctrs  */
   short  meshplot;     /* MESH    */  /* Plot the mesh grid?            */
   short  boundary;                    /* Plot the mesh boundary?        */
   short  regbound;                    /* Plot the region boundary?      */
   short  fillbnd;                     /* Fill the mesh boundary?        */
   short  vlog;         /* VECTORS */  /* Log-scale for vectors          */
   double vlogscale;                   /* Log-scale factor for vectors   */
   double vscale;                      /* Linear-scl factor for vectors  */
   short  vhead;                       /* Draw arrow-heads for vectors   */
   short  vtail;                       /* Draw arrow-tails for vectors   */
   short  applyfill;    /* CURVES  */  /* Apply fill to all curves       */
   short  splinetyp;                   /* For splines                    */
   short  pr_ptID;      /* DEBUG   */  /* Plot the mesh point-ID data    */
   short  pr_ndID;                     /* Plot the mesh node-ID data     */
   short  pr_trID;                     /* Plot the mesh tria-ID data     */
   short  pr_rtID;                     /* Plot the mesh rect-ID data     */
   short  pr_rgID;                     /* Plot the mesh region-ID data   */
   short  pr_cvID;                     /* Plot the curve IDs             */
   double binwidth;     /*HISTOGRAM*/  /* Histogram bin width            */
   double binstart;                    /* Histogram bin start            */
   double barmin;       /* BARCHART*/  /* Bar chart bar min values       */
   short  plotannot;    /* MISC    */  /* Plot annotations               */
} CNdataset_property;


/* 
 * The properties of the plot are stored in the CNproperty data-structure. 
 * Changes to the property are made using the flags, as is done in X11.
 * The use of this structure allows us to read in the property of the
 * plot first before attaching the properties to the dataset.
 * The properties of a dataset may be changed either by accessing the dataset
 * directly (dataset->property.linetype=1) or by setting global properties here.
 */

/*
 * vxmin, vxmax etc are used to determine the viewport of the data, i.e.
 * the boundaries of the data to be plotted.
 * pxmin, pxmax are similar but actually represent the plot boundaries.
 * The plot boundaries, take into account logarithms, (i.e. if
 * xlog is set then pxmin = CNlog10(vxmin) etc), boundary rounding, min/max
 * swapping and lots of other error-checking to ensure correct bounds.  
 * pxmin etc can be derived from vxmin etc but not easily so that is why
 * we carry around plot and view boundaries.
 */

/* Flags argument for changing plotset_property */
#define CNtitle      (1L << 0)    /* User-specified titles             */
#define CNgrid       (1L << 1)    /* User-specified grid flag          */
#define CNxflip      (1L << 2)    /* User-specified xflip              */
#define CNyflip      (1L << 3)    /* User-specified yflip              */
                                  /* zflip doesn't make sense...       */
#define CNxabs       (1L << 4)    /* User-specified x-absolute         */
#define CNyabs       (1L << 5)    /* User-specified y-absolute         */
#define CNzabs       (1L << 6)    /* User-specified z-absolute         */
#define CNxlog       (1L << 7)    /* User-specified xlog flag          */
#define CNylog       (1L << 8)    /* User-specified ylog flag          */
#define CNzlog       (1L << 9)    /* User-specified zlog flag          */
#define CNxticks     (1L << 10)   /* User-specified no of xticks       */
#define CNyticks     (1L << 11)   /* User-specified no of yticks       */
#define CNzticks     (1L << 12)   /* User-specified no of zticks       */
#define CNxautorange (1L << 13)   /* User-specified x-axis autorange   */
#define CNyautorange (1L << 14)   /* User-specified y-axis autorange   */
#define CNzautorange (1L << 15)   /* User-specified z-axis autorange   */
#define CNequalscale (1L << 16)   /* User-specified equalscale flag    */
#define CNfitpage    (1L << 17)   /* User-specified fit to plot page   */
#define CNxyratio    (1L << 18)   /* User-specified plot x-y ratio     */
#define CNxscale     (1L << 19)   /* User-specified x-scale factor     */
#define CNyscale     (1L << 20)   /* User-specified y-scale factor     */
#define CNzscale     (1L << 21)   /* User-specified z-scale factor     */
#define CNvxmin      (1L << 22)   /* User-specified viewport xmin      */
#define CNvxmax      (1L << 23)   /* User-specified viewport xmax      */
#define CNvymin      (1L << 24)   /* User-specified viewport ymin      */
#define CNvymax      (1L << 25)   /* User-specified viewport ymax      */
#define CNvzmin      (1L << 26)   /* User-specified viewport zmin      */
#define CNvzmax      (1L << 27)   /* User-specified viewport zmax      */
#define CNoverlay    (1L << 28)   /* User-specified dataset overlay    */

/* Second set of parameters... (ouch!) */
#define CNsidelabel  (1L <<  0)   /* User-specified plot side label    */
#define CNslabellen  (1L <<  1)   /* User-specified side label length  */
#define CNsetaxislbl (1L <<  2)   /* User-specified axislabel          */
#define CNinnerticks (1L <<  3)   /* Ticks inside the plot             */

typedef struct CNplotset_property_strct {
   long   flag1;
   long   flag2;
   char   *xlabel;                     /* x-axis label                  */
   char   *ylabel;                     /* y-axis label                  */
   char   *zlabel;                     /* z-axis label                  */
   char   *toplabel;                   /* top    label                  */
   char   *comment;                    /* comment (upper right of plot) */
   char   *subtitle;                   /* subtitle (underneath title)   */
   short  grid;                        /* draw the grid?                */
   short  xflip;                       /* x-axis is flipped             */
   short  yflip;                       /* y-axis is flipped             */
                                       /* z-axis is NOT flipped         */
   short  xabs;                        /* plot absolute values on x-axis*/
   short  yabs;                        /* plot absolute values on y-axis*/
   short  zabs;                        /* plot absolute values on z-axis*/
   short  xlog;                        /* log scale on the x-axis       */
   short  ylog;                        /* log scale on the y-axis       */
   short  zlog;                        /* log scale on the z-axis       */
   short  xticks;                      /* number of ticks on x-axis     */
   short  yticks;                      /* number of ticks on y-axis     */
   short  zticks;                      /* number of ticks on z-axis     */
   short  xautorange;                  /* autoranging on x-axis         */
   short  yautorange;                  /* autoranging on y-axis         */
   short  zautorange;                  /* autoranging on z-axis         */
   short  equalscale;                  /* equalscales on plot axes      */
   short  fitpage;                     /* fit to page                   */
   double xyratio;                     /* y/x ratio on plot axes        */
   double xscale;                      /* scale-factor for x-axis       */
   double yscale;                      /* scale-factor for y-axis       */
   double zscale;                      /* scale-factor for z-axis       */
   short  overlay;                     /* Overlay datasets?             */
   short  sidelabel;                   /* Plot side labels?             */
   int    slabellen;                   /* Side label length             */
   short  innerticks;                  /* Ticks inside plot             */
   double vxmin;                       /* Viewport                      */
   double vxmax;                       /* Viewport                      */
   double vymin;                       /* Viewport                      */
   double vymax;                       /* Viewport                      */
   double vzmin;                       /* Viewport                      */
   double vzmax;                       /* Viewport                      */
   double prev_vxmin;                  /* Previous Viewport             */
   double prev_vxmax;                  /* Previous Viewport             */
   double prev_vymin;                  /* Previous Viewport             */
   double prev_vymax;                  /* Previous Viewport             */
   double prev_vzmin;                  /* Previous Viewport             */
   double prev_vzmax;                  /* Previous Viewport             */
   double pxmin;                       /* Plot Viewport                 */
   double pxmax;                       /* Plot Viewport                 */
   double pymin;                       /* Plot Viewport                 */
   double pymax;                       /* Plot Viewport                 */
   double pzmin;                       /* Plot Viewport                 */
   double pzmax;                       /* Plot Viewport                 */

   CNaxislabelptr xlabelhead;          /* Xaxis plot labels - list head */
   CNaxislabelptr xlabeltail;          /* Xaxis plot labels - list tail */
   CNaxislabelptr ylabelhead;          /* Yaxis plot labels - list head */
   CNaxislabelptr ylabeltail;          /* Yaxis plot labels - list tail */
   CNaxislabelptr zlabelhead;          /* Zaxis plot labels - list head */
   CNaxislabelptr zlabeltail;          /* Zaxis plot labels - list tail */
} CNplotset_property;

/* Utility functions for setting properties of objects */
extern CNplotset_property *CNmake_plotset_property();
extern void         CNdelete_plotset_property();
extern void         CNdelete_plotset_property_fields();
extern void         CNset_default_plotset_property();
extern void         CNprint_plotset_property();
extern void         CNset_plotset_titles();
extern void         CNset_plotset_property();
extern int          CNparse_plotset_property();
extern void         CNprint_plotset_keywords();
extern void         CNwrite_plotset_options();

extern CNdataset_property *CNmake_dataset_property();
extern void         CNdelete_dataset_property();
extern void         CNdelete_dataset_property_fields();
extern void         CNset_default_dataset_property();
extern void         CNprint_dataset_property();
extern void         CNset_dataset_property();
extern int          CNparse_dataset_property();
extern void         CNprint_dataset_keywords();
extern void         CNwrite_dataset_options();

extern void         CNdelete_gbcurve_property();
extern void         CNdelete_gbcurve_property_fields();
extern void         CNset_default_gbcurve_property();
extern void         CNprint_gbcurve_property();
extern void         CNset_gbcurve_property();
extern int          CNparse_gbcurve_property();
extern void         CNprint_gbcurve_keywords();

extern CNcurve_property *CNmake_curve_property();
extern void         CNdelete_curve_property();
extern void         CNdelete_curve_property_fields();
extern void         CNset_default_curve_property();
extern void         CNprint_curve_property();
extern void         CNset_curve_property();
extern void         CNcopy_curve_property();
extern int          CNparse_curve_property();
extern void         CNprint_curve_keywords();
extern void         CNwrite_curve_options();

extern void         CNset_view_property();
extern int          CNparse_view_property();
extern void         CNprint_view_keywords();
extern void         CNwrite_view_options();

#endif /* CNproperty_defined */

