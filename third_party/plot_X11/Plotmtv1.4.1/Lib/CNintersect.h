/*
 * CNintersect.h - procedure declarations for intersection, clipping
 *                 and cutline routines.
 */

#ifndef CNintersect_defined
#define CNintersect_defined

/* Misc */
extern void CNnormalize_plane();
extern int  CNlongline();

/* Intersection procedures */
extern int  CNfind_tria_intsct_plane();
extern int  CNpoly3_intsct_plane();
extern int  CNpoly4_intsct_plane();
extern int  CNline_intsct_plane();

/* Cutline */
extern void         CNget_cutline();
extern void         CNcut_polygons();

/* Clipping */
extern void         CNclip_tria();
extern void         CNclip_rect();
extern void         CNclip_poly();
extern void         CNclip_curve();
extern void         CNclip_pointlist();
extern void         CNclip_x();
extern void         CNclip_y();
extern void         CNclip_z();
extern void         CNclip_t();
extern int          CNtria_in_bounds();
extern int          CNtria_in_xbounds();
extern int          CNtria_in_ybounds();
extern int          CNtria_in_zbounds();
extern int          CNtria_in_tbounds();
extern int          CNrect_in_bounds();
extern int          CNrect_in_xbounds();
extern int          CNrect_in_ybounds();
extern int          CNrect_in_zbounds();
extern int          CNrect_in_tbounds();
extern int          CNpoly_in_bounds();
extern int          CNcurve_in_bounds();
extern int          CNpointlist_in_bounds();

#endif /* CNintersect_defined */
