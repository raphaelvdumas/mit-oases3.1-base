/*
 * External declarations for reading & writing MTV structures in XDR format
 */

#ifndef CNxdr_defined
#define CNxdr_defined

#define LABELLEN 100   /* For strings */

#define bool_t int

extern void CNread_plotxdr();
extern void CNread_plotxdr_from_file();
extern void CNread_plotxdr_from_pipe();
extern void CNread_plotxdr_data();
extern void CNwrite_plotxdr();
extern void CNwrite_plotxdr_to_file();
extern void CNwrite_plotxdr_to_pipe();
extern void CNwrite_plotxdr_data();

extern bool_t xdr_datasetlist();
extern bool_t xdr_dataset();

extern bool_t xdr_contour_dataset();
extern bool_t xdr_gdcontour_dataset();
extern bool_t xdr_cvcontour_dataset();

extern bool_t xdr_curve2D_dataset();
extern bool_t xdr_curve3D_dataset();
extern bool_t xdr_curve_dataset();
extern bool_t xdr_curvelist();
extern bool_t xdr_curve();
extern bool_t xdr_pointlist();

extern bool_t xdr_grid4D_dataset();
extern bool_t xdr_grid4Darr();

extern bool_t xdr_mesh_dataset();
extern bool_t xdr_quant_dataset();
extern bool_t xdr_mesh_regions();

extern bool_t xdr_vector_dataset();
extern bool_t xdr_vectorlist();

extern bool_t xdr_probability_dataset();
extern bool_t xdr_histogram_dataset();
extern bool_t xdr_barchart_dataset();

extern bool_t xdr_polygon_dataset();
extern bool_t xdr_polygonlist();
extern bool_t xdr_polygon();

extern bool_t xdr_mesh4D_dataset();

extern bool_t xdr_intarr();
extern bool_t xdr_dblarr();
extern bool_t xdr_alloc_dblarr();
extern bool_t xdr_alloc_string();
extern bool_t xdr_curve_prop();
extern bool_t xdr_plotset_prop();
extern bool_t xdr_view_prop();
extern bool_t xdr_dataset_prop();
extern bool_t xdr_properties();

#endif /* CNxdr_defined */

