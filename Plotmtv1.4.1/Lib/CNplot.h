/*
 * CNplot.h - definitions for data, constant, macro and procedures
 *            used for plotting 2D and 3D data
 *            This file contains declarations for the more complex
 *            data-structures, including datasets and plotsets.
 */

#ifndef CNplot_defined
#define CNplot_defined

/*
 * Include constant definitions
 */
#include "CNdata.h"

/*
 * Include axis label datatypes and procedure declarations
 */
#include "CNaxislabel.h"

/*
 * Include property definitions
 */
#include "CNproperty.h"

/*
 * Include basic datatypes - nodes, triangles, etc
 */
#include "CNdatatypes.h"

/*
 * Include barchart datatypes and procedure declarations
 */
#include "CNbarchart.h"

/*
 * Include grid datatypes and procedure declarations
 */
#include "CNgrid.h"

/*
 * Include mesh datatypes and procedure declarations
 */
#include "CNmesh.h"

/*
 * Include mesh4D datatypes and procedure declarations
 */
#include "CNmesh4D.h"

/*
 * Include histogram datatypes and procedure declarations
 */
#include "CNhistogram.h"

/*
 * Include contour level datatypes and procedure declarations
 */
#include "CNcontlist.h"

/*
 * Include annotation datatypes and procedure declarations
 */
#include "CNannotate.h"

/* 
 * Include intersection procedure declarations
 */
#include "CNintersect.h"

/*
 * Include MTV-format reader
 */
#include "CNreadmtv.h"

/*
 * Include 3D matrix/data structures
 */
#include "CNplot3D.h"

/*
 * Include quantities and node-values
 */
#include "CNquant.h"

/*
 * Include cube data-structures and procedure declarations
 */
#include "CNcube.h"

/* 
 * Include probe data-structures and procedure declarations
 */
#include "CNprobe.h"

/* 
 * Include rounding procedure declarations
 */
#include "CNround.h"

/* 
 * Include sorting procedure declarations
 */
#include "CNsort.h"

/*
 * Include Splines 
 */
#include "CNspline.h"

/* 
 * Include string declarations
 */
#include "CNstring.h"

/*
 * Include vector declarations
 */
#include "CNvector.h"

/*
 * Include vector declarations
 */
#include "CNpvector.h"

/*
 * Include XDR read/write routines
 */
#include "CNxdr.h"



/*
 * A dataset contains a collection of data, including
 *       - list of triangles
 *       - list of rectangles
 *       - list of curves
 * The dataset also has properties associated with it, stored in the
 * CNdataset_property structure.  See "CNproperty.h" for more information
 * on how to manipulate the properties of the dataset.
 *
 * The dataset also contains plotting information, such as labels.
 * This is necessary because each dataset could itself be a single plot,
 * so the plot (plotset) must inherit information from the dataset.
 *
 * The true boundaries of the data are stored in bxmin, bxmax etc.
 * The desired plot boundaries are stored in the plot_property.
 */

/* data sets */
typedef struct CNdataset_strct {
   short  ID;
   short  flag;
   short  datatype;
   short  paneltype;
   short  fieldID;
   char   *filename;                       /* Original plot file           */
   char   *label;                          /* Descriptive Label            */
   double bxmin;                           /* Plot boundary                */
   double bxmax;                           /* Plot boundary                */
   double bymin;                           /* Plot boundary                */
   double bymax;                           /* Plot boundary                */
   double bzmin;                           /* Plot boundary                */
   double bzmax;                           /* Plot boundary                */
   CNdataset_property        data_pr;      /* Dataset property             */
   CNplotset_property        plot_pr;      /* Plot property                */
   CNviewptr                 view_pr;      /* 3D view information          */
   struct CNpoint_strct      *pointhead;   /* The points of tria/rectangles*/
   struct CNpoint_strct      *pointtail;
   struct CNnode_strct       *nodehead;    /* The nodes of tria/rectangles */
   struct CNnode_strct       *nodetail;
   struct CNsegm_strct       *segmhead;    /* segments of triangles        */
   struct CNsegm_strct       *segmtail;
   struct CNptseg_strct      *ptseghead;   /* point-segments of triangles  */
   struct CNptseg_strct      *ptsegtail;
   struct CNtria_strct       *triahead;    /* The triangular mesh          */
   struct CNtria_strct       *triatail;
   struct CNrect_strct       *recthead;    /* The rectangular mesh         */
   struct CNrect_strct       *recttail;
   struct CNelem_strct       *elemhead;    /* Rectangle/Traingle containers*/
   struct CNelem_strct       *elemtail;
   struct CNpoly_strct       *polyhead;    /* The polygonal mesh           */
   struct CNpoly_strct       *polytail;
   struct CNcurve_strct      *curvehead;   /* A set of curves, xyz triples */
   struct CNcurve_strct      *curvetail;
   struct CNquant_strct      *quanthead;   /* A set of quantities for nodes*/
   struct CNquant_strct      *quanttail;
   struct CNregion_strct     *regionhead;  /* Region boundaries            */
   struct CNregion_strct     *regiontail;
   struct CNdataset_strct    *parent;      /* Parent for PIF-type mesh     */
   struct CNquant_strct      *quantity;    /* Pointer to a quantity        */
   struct CNgrid4D_strct     *grid;        /* Pointer to a grid            */
   struct CNmesh4D_strct     *mesh4D;      /* Pointer to a mesh4D grid     */
   struct CNhistogram_strct  *histogram;   /* Pointer to a histogram       */
   struct CNbarchart_strct   *barchart;    /* Pointer to a barchart        */
   struct CNvecbox_strct     *vecbox;      /* A set of vectors             */
   struct CNcontstep_strct   *cstephead;   /* A list of contour steps      */
   struct CNcontstep_strct   *csteptail;   /* A list of contour steps      */
   struct CNannotation_strct *annothead;   /* Pointer to an annotation list*/
   struct CNannotation_strct *annottail;   /* Pointer to an annotation list*/
   struct CNdataset_strct    *next;
   struct CNdataset_strct    *prev;
} CNdataset;
typedef struct CNdataset_strct *CNdatasetptr;



/*
 * A typical linked-list data-structure, e.g. a list of triangles,
 * has pointers to the head and tail of the list, e.g triahead, triatail.
 * But we sometimes need to build another list of triangles, without
 * duplicating the current list.  Therefore the new list contains
 * POINTERS to the triangles; this preserve the original order of the list.
 *
 * Example :
 *    list of pointers to the triangle data-structure.
 *           triahead->T->T->triatail
 *
 *    list of pointers to the triangle-pointers
 *           tlisthead->TL->TL->tlisttail
 *              |        |   |     |
 *              T        T   T     T
 */


/* list of dataset pointers - contains list of datasetptrs */
typedef struct CNdslist_strct {
   short  flag;
   short  focus;
   struct CNdataset_strct *Dptr;
   struct CNdslist_strct  *next;
   struct CNdslist_strct  *prev;
} CNdslist;
typedef struct CNdslist_strct *CNdslistptr;



/*
 * A plotset contains multiple instances of datasets
 */

/* A plotset */
typedef struct CNplotset_strct {
   short     ID;
   short     flag;
   short     plottype;
   short     plotformat;
   char     *label;
   char     *userdata;                   /* Pointer to user-defined data */
   CNplotset_property      plot_pr;      /* Plot property                */
   CNviewptr               view_pr;      /* 3D view information          */
   struct CNannotation_strct *annothead; /* Pointer to an annotation list*/
   struct CNannotation_strct *annottail; /* Pointer to an annotation list*/
   struct CNdslist_strct  *datahead;
   struct CNdslist_strct  *datatail;
   struct CNplotset_strct *next;
   struct CNplotset_strct *prev;
} CNplotset;
typedef struct CNplotset_strct *CNplotsetptr;


/* list of plotset pointers - contains list of plotsetptrs */
typedef struct CNpslist_strct {
   short  flag;
   struct CNplotset_strct *Pptr;
   struct CNpslist_strct  *next;
   struct CNpslist_strct  *prev;
} CNpslist;
typedef struct CNpslist_strct *CNpslistptr;




/*
 * EXTERNAL PROCEDURE DECLARATIONS
 */

extern CNdatasetptr CNmake_dataset();
extern int          CNinsert_dataset();
extern void         CNprint_dataset_list();
extern void         CNprint_dataset();
extern void         CNdelete_dataset();
extern void         CNdelete_dataset_list();
extern int          CNcount_datasets();
extern void         CNstore_dataset();
extern void         CNappend_datasets();
extern char        *CNdatatype();
extern void         CNprint_dataset_list_size();
extern int          CNdataset_size();

extern CNdslistptr  CNinsert_dslist();
extern void         CNprint_dslist_list();
extern void         CNdelete_dslist();
extern void         CNdelete_dslist_list();
extern int          CNcount_dslists();

extern CNplotsetptr CNmake_plotset();
extern CNplotsetptr CNinsert_plotset();
extern void         CNprint_plotset_list();
extern void         CNprint_plotset();
extern void         CNdelete_plotset();
extern void         CNdelete_plotset_list();
extern int          CNcount_plotsets();

extern CNpslistptr  CNinsert_pslist();
extern void         CNprint_pslist_list();
extern void         CNdelete_pslist();
extern void         CNdelete_pslist_list();
extern int          CNcount_pslists();

/* 
 * array creation 
 */
extern int         *CNcreate_1D_int_array();
extern void         CNset_1D_int_array_value();
extern int          CNget_1D_int_array_value();
extern void         CNfree_1D_int_array();
extern void         CNget_1D_int_array_maxmin();

extern double      *CNcreate_2D_double_array();
extern double      *CNcreate_1D_double_array();
extern void         CNset_1D_double_array_value();
extern double       CNget_1D_double_array_value();
extern void         CNfree_1D_double_array();
extern void         CNget_1D_double_array_maxmin();

extern double      *CNcreate_2D_double_array();
extern void         CNset_2D_double_array_value();
extern double       CNget_2D_double_array_value();
extern void         CNfree_2D_double_array();
extern void         CNget_2D_double_array_maxmin();

extern CNpointptr  *CNcreate_pointptr_array();
extern void         CNset_pointptr_array_value();
extern CNpointptr   CNget_pointptr_array_value();
extern void         CNfree_pointptr_array();

extern CNnodeptr   *CNcreate_nodeptr_array();
extern void         CNset_nodeptr_array_value();
extern CNnodeptr    CNget_nodeptr_array_value();
extern void         CNfree_nodeptr_array();

extern CNsegmptr   *CNcreate_segmptr_array();
extern void         CNset_segmptr_array_value();
extern CNsegmptr    CNget_segmptr_array_value();
extern void         CNfree_segmptr_array();

/* 
 * Read data in "contour" and "pdraw"(triangles only) format, and save
 * as collection of rectangles/triangles in a dataset.
 */
/* Read from contour files */
extern CNdatasetptr CNread_contour();
extern CNdatasetptr CNread_contour_from_file();
extern CNdatasetptr CNread_contour_from_pipe();
extern CNdatasetptr CNread_as_contour();
extern CNdatasetptr CNread_bn_contour();
extern CNdatasetptr CNread_3Dcontour_from_file();
extern CNdatasetptr CNread_3Dcontour_from_pipe();
extern CNdatasetptr CNread_as_3Dcontour();
extern CNdatasetptr CNread_bn_3Dcontour();

/* Store the contour data */
extern CNdatasetptr CNget_rectilinear_contour_data();
extern CNdatasetptr CNget_triangular_contour_data();
extern void         CNreconstruct_contour_mesh();
extern void         CNpartition_rectilinear_data();

/* 
 * Read data in "drawplot" format, and save
 * as collection of curves in a dataset.
 */
/* Read from plot2D files */
extern CNdatasetptr CNread_plot2D();
extern CNdatasetptr CNread_plot2D_from_file();
extern CNdatasetptr CNread_plot2D_from_pipe();
extern CNdatasetptr CNread_as_plot2D();
extern CNdatasetptr CNread_bn_plot2D();

/* 
 * Read data in "pdraw" format, and save
 * as collection of curves in a dataset.
 */
/* Read from plot3D files */
extern CNdatasetptr CNread_plot3D();
extern CNdatasetptr CNread_plot3D_from_file();
extern CNdatasetptr CNread_plot3D_from_pipe();
extern CNdatasetptr CNread_as_plot3D();
extern CNdatasetptr CNread_bn_plot3D();

/* 
 * Read data in "pdraw" format, and save
 * as collection of curves in a dataset.
 */
/* Read from plot3D files */
extern CNdatasetptr CNread_plot3Dfile();
extern CNdatasetptr CNread_as_plot3D();
extern CNdatasetptr CNread_bn_plot3D();

/*
 * Manipulate 4D grid data 
 */
/* Store and manipulate grid data */
extern CNdatasetptr CNget_volumetric_grid_data();
extern CNsliceptr   CNslice_grid4D_x();
extern CNsliceptr   CNslice_grid4D_y();
extern CNsliceptr   CNslice_grid4D_z();
extern CNdatasetptr CNcreate_dataset_from_slice();
extern CNdatasetptr CNslice_grid4D_dataset();

/*
 * Manipulate 4D grid data 
 */
extern CNdatasetptr CNcreate_mesh4D_parent();
extern CNdatasetptr CNcreate_mesh4D_child();
extern CNdatasetptr CNslice_mesh4D_dataset();

/*
 * Read data in hierarchical PIF format
 * and save as a mesh with quantities and node-values
 */
extern int          CNread_MTVmesh();
extern int          CNread_MTVmesh_from_file();
extern int          CNread_MTVmesh_from_pipe();
extern CNdatasetptr CNread_MTVmesh_data();
extern int          CNread_MTVquantities();

/*
 * Read data in hierarchical PIF format (SUPREM4)
 * and save as a mesh with quantities and node-values
 */
extern int          CNread_SUPmesh();
extern int          CNread_SUPmesh_from_file();
extern int          CNread_SUPmesh_from_pipe();
extern CNdatasetptr CNread_SUPmesh_data();

/* 
 * Manipulate PIF datasets
 */
extern int          CNexpand_pif_dataset();
extern void         CNcreate_pif_quantity_dataset();
extern CNquantptr   CNselect_pif_quantity();
extern int          CNfill_pif_quantity_dataset();
extern void         CNapply_mesh_properties();
extern void         CNfill_mesh_dataset();

/*
 * Contour manipulation
 */
extern void CNselect_contour_step();
extern void CNselect_cstep();
extern void CNslice_contours();
extern void CNfind_contour();
extern void CNsort_lines();


/*
 * Expand the contours/triangle lists and save to files
 */
extern int  CNexpand_mesh_from_dataset();
extern int  CNexpand_regions_from_dataset();
extern int  CNexpand_regions();
extern int  CNexpand_sqmesh();
extern int  CNexpand_curves_from_dataset();
extern int  CNexpand_curves();
extern void CNcalc_single_cont();
extern int  CNprint_single_cont();
#define CNexpand_mesh CNexpand_mesh_xyt
extern int  CNexpand_mesh_xyz();
extern int  CNexpand_mesh_xzt();
extern int  CNexpand_mesh_yzt();
extern int  CNexpand_mesh_xyt();

/* 
 * Useful utilities for managing structures
 */
extern CNnodeptr  CNcommon_node();
extern void       CNreset_curves();
extern void       CNget_elemlist_maxmin();
extern void       CNget_elem_maxmin();
extern void       CNget_trialist_maxmin();
extern void       CNget_tria_maxmin();
extern void       CNget_tria_xmaxmin();
extern void       CNget_tria_ymaxmin();
extern void       CNget_tria_zmaxmin();
extern void       CNget_tria_tmaxmin();
extern void       CNget_rect_maxmin();
extern void       CNget_rect_xmaxmin();
extern void       CNget_rect_ymaxmin();
extern void       CNget_rect_zmaxmin();
extern void       CNget_rect_tmaxmin();
extern void       CNget_poly_maxmin();
extern void       CNget_poly_xmaxmin();
extern void       CNget_poly_ymaxmin();
extern void       CNget_poly_zmaxmin();
extern void       CNget_poly_tmaxmin();
extern void       CNget_nodelist_tmaxmin();
extern void       CNget_curvelist_maxmin();
extern void       CNget_curve_maxmin();
extern void       CNget_pointlist_maxmin();
extern void       CNget_vectorlist_maxmin();
extern void       CNcopy_abslog_pointlist();
extern void       CNcopy_pointlist();
extern void       CNbuild_elemlist();

/*
 * List manipulation utilities
 */
extern void CNgenerate_boundary();
extern void CNgenerate_boundary_from_nodes();
extern void CNgenerate_segment_list();
extern void CNfind_boundary_segments();

extern void CNgenerate_boundary_from_points();
extern void CNgenerate_ptseg_list();
extern void CNfind_boundary_ptsegs();
extern void CNfind_boundary_polys();
extern void CNmat_boundary();
extern void CNmat_exp_boundary();


/*
 * Time
 */
extern void CNget_localtime();


/*
 * System
 */
extern void CNget_hostname();
extern void CNget_sysname();


/*
 * Probability routine
 */
extern double CNnorm_vert_distance();


/*
 * Misc Dataset/Plotset manipulation utilities
 */
extern int  CNplotset_has_colored_contours();
extern int  CNplotset_has_linelabels();
extern int  CNplotset_has_contour();
extern void CNset_plotset_boundaries();
extern void CNset_plotset_viewport();
extern void CNreset_plotset_viewport();
extern CNdatasetptr CNcopy_dataset();
extern int          CNtransform_dataset();

#endif /* CNplot_defined */


