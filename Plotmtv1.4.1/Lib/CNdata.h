/*
 * CNdata.h - definitions for data, constant, macro 
 */

#ifndef CNdata_defined
#define CNdata_defined

#include <math.h>

/*
 * Constants 
 */
#define CN_MAXCHAR    1000       /* No of characters in a word */
#define CN_MAXWORDS   100        /* No of words in an array    */
#define CN_TRUE       1          /* True                       */
#define CN_FALSE      0          /* False                      */
#define CN_ERROR     -1          /* Error                      */
#define CN_SUCCESS    1          /* Success                    */
#define CN_FAIL       0          /* Failed                     */

#define CN_LARGE      1.0e99     /* A large number             */
#define CN_SMALL      1.0e-30    /* A small number             */
#define CN_SMALLER    1.0e-50    /* A very small number        */
#define CN_TINY       1.0e-99    /* A really tiny number       */

#define CN_HIGH       2          /* Contour-boundary interpolation style */
#define CN_LOW        1          /* Contour-boundary interpolation style */
#define CN_NONE       0          /* Contour-boundary interpolation style */

/* Data types */
#define CN_CONTOUR    1          /* contour plot                   */
#define CN_PLOT2D     2          /* 2D plot                        */
#define CN_PLOT3D     3          /* 3D plot                        */
#define CN_PIF_PARENT 4          /* triangular mesh data (parent)  */
#define CN_PIF_CHILD  5          /* pif mesh derived from a parent */
#define CN_GRID4D     6          /* 4D Grid data                   */
#define CN_VECTOR     7          /* Vector plot                    */
#define CN_PROBAB     8          /* Probability plot               */
#define CN_HISTOGRAM  9          /* Histogram plot                 */
#define CN_BARCHART   10         /* Bar Chart                      */
#define CN_MESH4D_P   11         /* 4D Mesh data (parent)          */
#define CN_MESH4D_C   12         /* 4D Mesh quantity data (child)  */
#define CN_POLYGON    13         /* Polygons (used for contours)   */

/* Data type */
#define CN_TRIA_STR   1          /* Triangle structure             */
#define CN_RECT_STR   2          /* Rectangle structure            */

/* Plot types */
#define CN_SCIENTIFIC_PLOT  1    /* Scientific (default) plot      */
#define CN_PROBABILITY_PLOT 2    /* Probability plot               */
#define CN_HISTOGRAM_PLOT   3    /* Histogram plot                 */
#define CN_BARCHART_PLOT    4    /* Bar Chart                      */

#define CN_NSLICE     0          /* No Slice defined               */
#define CN_XSLICE     1          /* Slice along X                  */
#define CN_YSLICE     2          /* Slice along Y                  */
#define CN_ZSLICE     3          /* Slice along Z                  */

#define CN_LINECONT   1          /* Line contours                  */
#define CN_FILLCONT   2          /* Filled contours                */
#define CN_PLOTMESH   3          /* Surface Plot                   */
#define CN_LNFILLCONT 4          /* Filled and line contours       */

#define CN_STEPSIZE   1          /* Use defined contour stepsize   */
#define CN_NUMSTEPS   2          /* Use defined num-steps          */
#define CN_LOGSTEPS   3          /* Use logarithm steps            */
#define CN_USERDEFN   4          /* User-defined contour steps     */

#define CN_RECTRECT   0          /* Rect-rect     contour interpolation */
#define CN_RECT2TRIA  1          /* Rect-2tria    contour interpolation */
#define CN_RECT4TRIA  2          /* Rect-4tria    contour interpolation */
#define CN_RECTFLAT   3          /* Rect-flatrect contour interpolation */
#define CN_RECT2MAT   4          /* Rect-mat-rect contour interpolation */

#define CN_INT_TRIA   1          /* Triangle-plane intersection method */
#define CN_INT_SEGM   2          /* Segment-plane intersection method  */

#define CN_MAXSTEPS   51         /* Max no of contour steps     */
#define CN_IDLSTEPS   10         /* Ideal no of contour steps   */

#define CN_FILE       1          /* Read from file              */
#define CN_PIPE       2          /* Read from pipe              */

#define CN_NONAME     "NoName"

/* Titles (for plots) */
#define CN_DEF_XLABEL "X-Axis"
#define CN_DEF_YLABEL "Y-Axis"
#define CN_DEF_ZLABEL "Z-Axis"
#define CN_DEF_TLABEL "PLOT"

/* PI */
#define CN_PI         3.14159265358979323846 /* PI                     */
#define CN_DEG2RAD    CN_PI/180.0            /* Degrees to Radians     */
#define CN_RAD2DEG    180.0/CN_PI            /* Radians to Degrees     */

/*
 * Macros
 */
#define BOOLEAN_VALUE(x)   ((x) ? "TRUE" : "FALSE")
#define SMALLER_OF(x,y)    ((x) < (y) ? (x) : (y))
#define MINOF3(a,b,c)      (SMALLER_OF(SMALLER_OF(a,b),c))
#define LARGER_OF(x,y)     ((x) > (y) ? (x) : (y))
#define MAXOF3(a,b,c)      (LARGER_OF(LARGER_OF(a,b),c))

#define EQUAL(x,y)      ((fabs((double)(x-y)) < CN_TINY) ? CN_TRUE : CN_FALSE)
#define NEQUAL(x,y)     ((fabs((double)(x-y)) > CN_TINY) ? CN_TRUE : CN_FALSE)

/*
 * Line types for plot
 */
#define CN_LN_NONE         0
#define CN_LN_SOLID        1
#define CN_LN_DASHED       2  
#define CN_LN_DOTTED       3
#define CN_LN_DOTDASH      4 
#define CN_LN_LONGDOT      5 
#define CN_LN_DOUBLEDOT    6
#define CN_LN_LONGDASH     7
#define CN_LN_DOTDASH2     8
#define CN_LN_TRIPLEDOT    9
#define CN_LN_DOTDOTDASH   10
#define CN_LN_TYPES        10
#define CN_GD_DOTTED      -1

/*
 * Marker types for plot
 */
#define CN_MK_NONE         0
#define CN_MK_DOT          1
#define CN_MK_CROSS        2
#define CN_MK_X            3
#define CN_MK_SQUARE1      4    
#define CN_MK_SQUARE2      5  
#define CN_MK_DIAMOND1     6 
#define CN_MK_DIAMOND2     7 
#define CN_MK_TRIANGLE1    8 
#define CN_MK_TRIANGLE2    9 
#define CN_MK_ITRIANGLE1   10 
#define CN_MK_ITRIANGLE2   11 
#define CN_MK_CIRCLE1      12
#define CN_MK_CIRCLE2      13
#define CN_MK_TYPES        13

/*
 * Fill Styles
 */
#define CN_FILL_NONE       0
#define CN_FILL_SOLID      1
#define CN_FILL_SQUARE     2
#define CN_FILL_DIAG1      3
#define CN_FILL_DIAG2      4
#define CN_FILL_CHKRBD     5
#define CN_FILL_VERT       6
#define CN_FILL_HORZ       7
#define CN_FILL_DOTTED     8
#define CN_FILL_TYPES      8
#define CN_FL_TYPES CN_FILL_TYPES

/*
 * Distinct colors
 */
#define CN_MAX_LINE_COLORS 10
#define CN_LN_COLRS CN_MAX_LINE_COLORS
#define CN_MK_COLRS CN_MAX_LINE_COLORS
#define CN_FL_COLRS CN_MAX_LINE_COLORS

/*
 * Distinct sizes
 */
#define CN_LN_SIZES 10
#define CN_MK_SIZES 10


/*
 * The basic element is a point, which is a point in space with
 * x,y,z cartesian coordinates.
 * A node contains a value (e.g. potential) in physical space (x,y,z).
 */

/* x-y-z coordinates */
typedef struct CNcoord_strct {
   double  x;
   double  y;
   double  z;
} CNcoord;

#endif /* CNdata_defined */
