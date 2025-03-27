/*
 * CNround.c - procedure declarations for rounding
 */

#ifndef CNround_defined
#define CNround_defined

extern double CNround_to_decimal();
extern void   CNget_autorange();
extern double CNauto_round_down();
extern double CNauto_round_up();
extern double CNround_down();
extern double CNround_up();
extern double CNlog10();

extern double CNinterp_distance();
extern int    CNlogmode2();
extern int    CNlogmode3();
extern int    CNlogmode4();
extern int    CNsign();
extern double CNlogabs();
extern double CNlog_abs();
extern double CNlog_1plusabs();
extern double CNinvlogabs();
extern double CNinvlog_abs();
extern double CNinvlog_1plusabs();

#endif /* CNround_defined */

