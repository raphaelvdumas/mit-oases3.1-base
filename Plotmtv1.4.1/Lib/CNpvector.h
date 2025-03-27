/*
 * CNpvector.h - vector procedure declarations
 */

#ifndef CNpvector_defined
#define CNpvector_defined

extern int      CNnormalize_vector();
extern double   CNvector_lengthsq();
extern double   CNvector_angle();
extern double   CNvector_dotproduct();
extern void     CNconv_tria_to_plane();
extern void     CNfind_normal_vector();
extern CNcoord  CNfind_line_vector();
extern CNcoord  CNcross_product();

#endif /* CNpvector_defined */
