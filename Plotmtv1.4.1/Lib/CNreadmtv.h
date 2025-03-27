/*
 * CNreadmtv.h - routines to read mtv-format data.
 */

#ifndef CNreadmtv_defined
#define CNreadmtv_defined

#define MAX_VALS 100

/* Local data struct */
typedef struct _CNdouble_strct {
   double val;
   short  def;
} CNdouble;

extern void CNread_plotmtv();
extern void CNread_plotmtv_from_file();
extern void CNread_plotmtv_from_pipe();
extern void CNread_plotmtv_data();

extern int CNmtv_read_curve2D_data();
extern int CNmtv_read_curve3D_data();
extern int CNmtv_read_contcurve_data();
extern int CNmtv_read_curve_data();
extern int CNmtv_read_column_data();
extern int CNmtv_read_contour_data();
extern int CNmtv_read_grid4D_data();
extern int CNmtv_read_vector_data();
extern int CNmtv_read_mtvmesh_data();
extern int CNmtv_read_probab_data();
extern int CNmtv_read_histogram_data();
extern int CNmtv_read_barchart_data();
extern int CNmtv_read_mesh4D_data();
extern int CNmtv_read_polygon_data();

extern int     CNmtv_check_points();
extern int    *CNmtv_read_binary_int_array();
extern double *CNmtv_read_binary_dbl_array();
extern void    CNmtv_read_line();
extern void    CNmtv_filter_array();
extern void    CNmtv_adjust_boundaries();
extern void    CNtriangulate_polygon();

extern void CNwrite_plotmtv();
extern void CNwrite_plotmtv_to_file();
extern void CNwrite_plotmtv_to_pipe();
extern void CNwrite_plotmtv_data();

#endif /* CNreadmtv_defined */
