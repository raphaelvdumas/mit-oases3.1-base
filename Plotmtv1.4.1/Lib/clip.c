/*
 * clip.c - subroutine to clip a polygon against a z-plane
 *          Someday will exend this to other than z
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "CNdata.h"
#include "CNproperty.h"
#include "CNdatatypes.h"
#include "CNintersect.h"
#include "CNround.h"

static void create_node();
static void copy_point_to_node();
static void clip_x();
static void clip_y();
static void clip_z();
static void clip_t();
static void get_poly_maxmin();
static void get_pointlist_maxmin();

/**********************************************************************/
/*                                                                    */
/*  This routine acts as the interface to the polygon-clipping subr.  */
/*  It takes a triangle, clips it against the boundaries,             */
/*  and returns a modified vertice/node list.                         */
/*                                                                    */
/*  Note that a node has four values : x,y,z,t.                       */
/*     The first 3 values are the physical coordinates of the node.   */
/*     If the node coordinates (x,y,z) fall outside the physical      */
/*     clipping boundary, don't clip.  Similarly, if the node's       */
/*     t-value falls outside the boundary (tmin,tmax) no clipping     */
/*     is performed.                                                  */
/*                                                                    */
/* CNclip_rect DOES not work correctly if the rectangle is            */
/* saddle-shaped.  This turns up only for simple meshes...            */
/*                                                                    */
/**********************************************************************/

void CNclip_tria(T, nd_head, nd_tail, 
                 xmin, xmax, ymin, ymax, zmin, zmax, tmin, tmax, 
                 xclip, yclip, zclip, tclip, 
                 logx,  logy,  logz,  logt,  verbose)
CNtriaptr T;                  /* Triangle to be clipped          */
CNnodeptr *nd_head;           /* List of vertices after clipping */
CNnodeptr *nd_tail;           /* List of vertices after clipping */
double    xmin;               /* Min x-clipping plane            */
double    xmax;               /* Max x-clipping plane            */
double    ymin;               /* Min y-clipping plane            */
double    ymax;               /* Max y-clipping plane            */
double    zmin;               /* Min z-clipping plane            */
double    zmax;               /* Max z-clipping plane            */
double    tmin;               /* Min t-clipping plane            */
double    tmax;               /* Max t-clipping plane            */
int       xclip;              /* Clip in x                       */
int       yclip;              /* Clip in y                       */
int       zclip;              /* Clip in z                       */
int       tclip;              /* Clip in t                       */
short     logx;               /* Log interpolation for Clip in x */
short     logy;               /* Log interpolation for Clip in y */
short     logz;               /* Log interpolation for Clip in z */
short     logt;               /* Log interpolation for Clip in t */
int       verbose;            /* Print out debugging info        */
{
   int       clip_in_x = 0, clip_in_y = 0, clip_in_z = 0, clip_in_t = 0;
   int       signx, signy, signz, signt;
   int       xlogmode, ylogmode, zlogmode, tlogmode;
   CNnodeptr N;

   /*
    * The points are saved in a temporary node list.
    * The node list is continually deleted and recreated, so 
    * potentially this can be quite expensive.
    * To make sure that the nodes are completely removed, 
    * we remove the nodes completely, using CNremove_node()
    */

   /* Initialize first */
   *nd_head = NULL;
   *nd_tail = NULL;

   /* Check to see if any clipping is requested */
   if ((xclip || yclip || zclip || tclip)==0) return;

   /* Check the triangle to see if it should be clipped */
   clip_in_x = CNtria_in_xbounds(T,xmin,xmax);
   clip_in_y = CNtria_in_ybounds(T,ymin,ymax);
   clip_in_z = CNtria_in_zbounds(T,zmin,zmax);
   clip_in_t = CNtria_in_tbounds(T,tmin,tmax);

   /* If the triangle is wholly outside the clipping planes, return null list */
   if ((clip_in_x && clip_in_y && clip_in_z && clip_in_t) == 0) { 
      if (verbose) 
         (void) fprintf(stdout,"The triangle is outside - no clipping performed\n");
      return;
   }

   /* Transfer the triangle vertices to a new node list */
   create_node(nd_head, nd_tail, T->n1, 0);
   create_node(nd_head, nd_tail, T->n2, 1);
   create_node(nd_head, nd_tail, T->n3, 2);
   create_node(nd_head, nd_tail, T->n1, 4);

   /*
    * Convert the numbers to log if necessary then interpolate
    */
   if (logx) {
      signx    = CNsign(T->n1->coord->x);
      xlogmode = CNlogmode3(T->n1->coord->x,T->n2->coord->x,T->n3->coord->x);
      xmin     = CNlogabs(xmin, xlogmode);
      xmax     = CNlogabs(xmax, xlogmode);
   }
   if (logy) {
      signy    = CNsign(T->n1->coord->y);
      ylogmode = CNlogmode3(T->n1->coord->y,T->n2->coord->y,T->n3->coord->y);
      ymin     = CNlogabs(ymin, ylogmode);
      ymax     = CNlogabs(ymax, ylogmode);
   }
   if (logz) {
      signz    = CNsign(T->n1->coord->z);
      zlogmode = CNlogmode3(T->n1->coord->z,T->n2->coord->z,T->n3->coord->z);
      zmin     = CNlogabs(zmin, zlogmode);
      zmax     = CNlogabs(zmax, zlogmode);
   }
   if (logt) {
      signt    = CNsign(T->n1->t);
      tlogmode = CNlogmode3(T->n1->t,T->n2->t,T->n3->t);
      tmin     = CNlogabs(tmin, tlogmode);
      tmax     = CNlogabs(tmax, tlogmode);
   }
   if (logx || logy || logz || logt) {
      for (N=(*nd_head); N!=NULL; N=N->next) {
         if (logx) N->coord->x = CNlogabs(N->coord->x, xlogmode);
         if (logy) N->coord->y = CNlogabs(N->coord->y, ylogmode);
         if (logz) N->coord->z = CNlogabs(N->coord->z, zlogmode);
         if (logt) N->t        = CNlogabs(N->t       , tlogmode);
      }
   }

   /* 
    * Now do the clipping 
    * If the triangle is wholly inside the clipping planes, don't clip 
    * A single clipping could change a closed curve into an open curve,
    * so if this happens, add an extra node to close the curve.
    */
   if (xclip && clip_in_x==1) CNclip_x(nd_head, nd_tail, xmin, xmax, verbose);
   if (yclip && clip_in_y==1) CNclip_y(nd_head, nd_tail, ymin, ymax, verbose);
   if (zclip && clip_in_z==1) CNclip_z(nd_head, nd_tail, zmin, zmax, verbose);
   if (tclip && clip_in_t==1) CNclip_t(nd_head, nd_tail, tmin, tmax, verbose);

   /*
    * Now reconvert the results back to linear scale
    */
   if (logx || logy || logz || logt) {
      for (N=(*nd_head); N!=NULL; N=N->next) {
         if (logx) N->coord->x = CNinvlogabs(N->coord->x,signx, xlogmode);
         if (logy) N->coord->y = CNinvlogabs(N->coord->y,signy, ylogmode);
         if (logz) N->coord->z = CNinvlogabs(N->coord->z,signz, zlogmode);
         if (logt) N->t        = CNinvlogabs(N->t       ,signt, tlogmode);
      }
   }
}

/**********************************************************************/
/*                                                                    */
/*  This routine acts as the interface to the polygon-clipping subr.  */
/*  It takes a rectangle, clips it against the boundaries             */
/*  and returns a modified vertice/node list.                         */
/*                                                                    */
/**********************************************************************/

void CNclip_rect(R, nd_head, nd_tail, 
                 xmin, xmax, ymin, ymax, zmin, zmax, tmin, tmax,
                 xclip, yclip, zclip, tclip, 
                 logx,  logy,  logz,  logt,  verbose)
CNrectptr R;                  /* Rectangle to be clipped         */
CNnodeptr *nd_head;           /* List of vertices after clipping */
CNnodeptr *nd_tail;           /* List of vertices after clipping */
double    xmin;               /* Min x-clipping plane            */
double    xmax;               /* Max x-clipping plane            */
double    ymin;               /* Min y-clipping plane            */
double    ymax;               /* Max y-clipping plane            */
double    zmin;               /* Min z-clipping plane            */
double    zmax;               /* Max z-clipping plane            */
double    tmin;               /* Min t-clipping plane            */
double    tmax;               /* Max t-clipping plane            */
int       xclip;              /* Clip in x                       */
int       yclip;              /* Clip in y                       */
int       zclip;              /* Clip in z                       */
int       tclip;              /* Clip in t                       */
short     logx;               /* Log interpolation for Clip in x */
short     logy;               /* Log interpolation for Clip in y */
short     logz;               /* Log interpolation for Clip in z */
short     logt;               /* Log interpolation for Clip in t */
int       verbose;            /* Print out debugging info        */
{
   int       clip_in_x = 0, clip_in_y = 0, clip_in_z = 0, clip_in_t = 0;
   int       signx, signy, signz, signt;
   int       xlogmode, ylogmode, zlogmode, tlogmode;
   CNnodeptr N;

   /*
    * The points are saved in a temporary node list.
    * The node list is continually deleted and recreated, so 
    * potentially this can be quite expensive.
    * To make sure that the nodes are completely removed, 
    * we remove the nodes completely, using CNremove_node()
    */

   /* Initialize first */
   *nd_head = NULL;
   *nd_tail = NULL;

   /* Check to see if any clipping is requested */
   if ((xclip || yclip || zclip || tclip)==0) return;

   /* Check the rectangle to see if it should be clipped */
   clip_in_x = CNrect_in_xbounds(R,xmin,xmax);
   clip_in_y = CNrect_in_ybounds(R,ymin,ymax);
   clip_in_z = CNrect_in_zbounds(R,zmin,zmax);
   clip_in_t = CNrect_in_tbounds(R,tmin,tmax);

   /* 
    * If the rectangle is wholly outside the clipping planes, 
    * return null list 
    */
   if ((clip_in_x && clip_in_y && clip_in_z && clip_in_t) == 0) { 
      if (verbose) 
         (void) fprintf(stdout,"The rectangle is outside - no clipping performed\n");
      return;
   }

   /* Transfer the rectangle vertices to a new node list */
   create_node(nd_head, nd_tail, R->n1, 0);
   create_node(nd_head, nd_tail, R->n2, 1);
   create_node(nd_head, nd_tail, R->n3, 2);
   create_node(nd_head, nd_tail, R->n4, 3);
   create_node(nd_head, nd_tail, R->n1, 4);

   /*
    * Convert the numbers to log if necessary then interpolate
    */
   if (logx) {
      signx    = CNsign(R->n1->coord->x);
      xlogmode = CNlogmode4(R->n1->coord->x,R->n2->coord->x,
                            R->n3->coord->x,R->n4->coord->x);
      xmin     = CNlogabs(xmin, xlogmode);
      xmax     = CNlogabs(xmax, xlogmode);
   }
   if (logy) {
      signy    = CNsign(R->n1->coord->y);
      ylogmode = CNlogmode4(R->n1->coord->y,R->n2->coord->y,
                            R->n3->coord->y,R->n4->coord->y);
      ymin     = CNlogabs(ymin, ylogmode);
      ymax     = CNlogabs(ymax, ylogmode);
   }
   if (logz) {
      signz    = CNsign(R->n1->coord->z);
      zlogmode = CNlogmode4(R->n1->coord->z,R->n2->coord->z,
                            R->n3->coord->z,R->n4->coord->z);
      zmin     = CNlogabs(zmin, zlogmode);
      zmax     = CNlogabs(zmax, zlogmode);
   }
   if (logt) {
      signt    = CNsign(R->n1->t);
      tlogmode = CNlogmode4(R->n1->t,R->n2->t,R->n3->t,R->n4->t);
      tmin     = CNlogabs(tmin, tlogmode);
      tmax     = CNlogabs(tmax, tlogmode);
   }
   if (logx || logy || logz || logt) {
      for (N=(*nd_head); N!=NULL; N=N->next) {
         if (logx) N->coord->x = CNlogabs(N->coord->x, xlogmode);
         if (logy) N->coord->y = CNlogabs(N->coord->y, ylogmode);
         if (logz) N->coord->z = CNlogabs(N->coord->z, zlogmode);
         if (logt) N->t        = CNlogabs(N->t       , tlogmode);
      }
   }

   /* 
    * Now do the clipping 
    * If the rectangle is wholly inside the clipping planes, don't clip 
    * To get consistent results during zoom (i.e when x,y bnds change)
    * clip against z first.  This is necessary for rectangles and polygons.
    */
   if (tclip && clip_in_t==1) CNclip_t(nd_head, nd_tail, tmin, tmax, verbose);
   if (zclip && clip_in_z==1) CNclip_z(nd_head, nd_tail, zmin, zmax, verbose);
   if (xclip && clip_in_x==1) CNclip_x(nd_head, nd_tail, xmin, xmax, verbose);
   if (yclip && clip_in_y==1) CNclip_y(nd_head, nd_tail, ymin, ymax, verbose);

   /*
    * Now reconvert the results back to linear scale
    */
   if (logx || logy || logz || logt) {
      for (N=(*nd_head); N!=NULL; N=N->next) {
         if (logx) N->coord->x = CNinvlogabs(N->coord->x,signx, xlogmode);
         if (logy) N->coord->y = CNinvlogabs(N->coord->y,signy, ylogmode);
         if (logz) N->coord->z = CNinvlogabs(N->coord->z,signz, zlogmode);
         if (logt) N->t        = CNinvlogabs(N->t       ,signt, tlogmode);
      }
   }
}

/**********************************************************************/
/*                                                                    */
/*  This routine acts as the interface to the polygon-clipping subr.  */
/*  It takes a polygon, clips it against the boundaries               */
/*  and returns a modified vertice/node list.                         */
/*                                                                    */
/**********************************************************************/

void CNclip_poly(P, nd_head, nd_tail, 
                 xmin, xmax, ymin, ymax, zmin, zmax, tmin, tmax,
                 xclip, yclip, zclip, tclip, verbose)
CNpolyptr P;                  /* Polygon to be clipped           */
CNnodeptr *nd_head;           /* List of vertices after clipping */
CNnodeptr *nd_tail;           /* List of vertices after clipping */
double    xmin;               /* Min x-clipping plane            */
double    xmax;               /* Max x-clipping plane            */
double    ymin;               /* Min y-clipping plane            */
double    ymax;               /* Max y-clipping plane            */
double    zmin;               /* Min z-clipping plane            */
double    zmax;               /* Max z-clipping plane            */
double    tmin;               /* Min t-clipping plane            */
double    tmax;               /* Max t-clipping plane            */
int       xclip;              /* Clip in x                       */
int       yclip;              /* Clip in y                       */
int       zclip;              /* Clip in z                       */
int       tclip;              /* Clip in t                       */
int       verbose;            /* Print out debugging info        */
{
   int clip = 0;
   int xin=0, yin=0, zin=0, tin=0;
   CNnlistptr Nd;

   /*
    * The points are saved in a temporary node list.
    * The node list is continually deleted and recreated, so 
    * potentially this can be quite expensive.
    * To make sure that the nodes are completely removed, 
    * we remove the nodes completely, using CNremove_node()
    */

   /* Initialize first */
   *nd_head = NULL;
   *nd_tail = NULL;

   /* Check to see if any clipping is requested */
   if ((xclip || yclip || zclip || tclip)==0) return;

   /* Check the bounds of the polygon */
   clip = CNpoly_in_bounds(P,xmin,xmax,ymin,ymax,zmin,zmax,tmin,tmax,
                           &xin,&yin,&zin,&tin);

   /* If the poly is out of bounds return */
   if (clip==0) return;

   /* Transfer the nodelist to a new list */
   for (Nd=P->nlisthead; Nd!=NULL; Nd=Nd->next)
      create_node(nd_head, nd_tail, Nd->N, 0);

   /*
    * Now do the clipping
    * If the rectangle is wholly inside the clipping planes, don't clip
    * To get consistent results during zoom (i.e when x,y bnds change)
    * clip against z first.  This is necessary for rectangles and polygons.
    */
   if (tclip && tin==1) CNclip_t(nd_head, nd_tail, tmin, tmax, verbose);
   if (zclip && zin==1) CNclip_z(nd_head, nd_tail, zmin, zmax, verbose);
   if (xclip && xin==1) CNclip_x(nd_head, nd_tail, xmin, xmax, verbose);
   if (yclip && yin==1) CNclip_y(nd_head, nd_tail, ymin, ymax, verbose);
}


/**********************************************************************/
/*                                                                    */
/*  This routine acts as the interface to the polygon-clipping subr.  */
/*  It takes a list of points, clips it against the boundaries        */
/*  and returns a modified vertice/node list.                         */
/*                                                                    */
/*  This routine is a little dangerous because sometimes it returns   */
/*  a new point list, and sometimes it returns the original pointlist */
/*                                                                    */
/**********************************************************************/

void CNclip_curve(C, pt_head, pt_tail, 
                 xmin, xmax, ymin, ymax, zmin, zmax,
                 xclip, yclip, zclip, verbose)
CNcurveptr C;                 /* Curve to be clipped             */
CNpointptr *pt_head;          /* List of vertices after clipping */
CNpointptr *pt_tail;          /* List of vertices after clipping */
double    xmin;               /* Min x-clipping plane            */
double    xmax;               /* Max x-clipping plane            */
double    ymin;               /* Min y-clipping plane            */
double    ymax;               /* Max y-clipping plane            */
double    zmin;               /* Min z-clipping plane            */
double    zmax;               /* Max z-clipping plane            */
int       xclip;              /* Clip in x                       */
int       yclip;              /* Clip in y                       */
int       zclip;              /* Clip in z                       */
int       verbose;            /* Print out debugging info        */
{
   CNnodeptr  nd_head=NULL, nd_tail=NULL, N;
   CNpointptr P;
   int xin, yin, zin, clip;

   /*
    * The points are saved in a temporary node list.
    * The node list is continually deleted and recreated, so 
    * potentially this can be quite expensive.
    * To make sure that the nodes are completely removed, 
    * we remove the nodes completely, using CNremove_node()
    */

   /* Initialize first */
   nd_head  = NULL;
   nd_tail  = NULL;
   *pt_head = NULL;
   *pt_tail = NULL;

   /* Check to see if any clipping is requested */
   if ((xclip || yclip || zclip)==0) {
      *pt_head = C->pointhead;
      *pt_tail = C->pointtail;
      return;
   }

   /* Check to see if the curve's point list is empty */
   if (C->pointhead == NULL) {
      *pt_head = NULL;
      *pt_tail = NULL;
      return;
   }

   /* Check the bounds of the curve */
   clip = CNcurve_in_bounds(C,xmin,xmax,ymin,ymax,zmin,zmax,&xin,&yin,&zin);

   /* If the curve is out of bounds return */
   if (clip==0) {
      *pt_head = NULL;
      *pt_tail = NULL;
      return;
   }

   /* Copy the curve pointlist to a new list */
   for (P=C->pointhead; P!=NULL; P=P->next) 
       copy_point_to_node(&nd_head, &nd_tail, P, 0);
   if (C->curv_pr.filltype != CN_FILL_NONE)
       copy_point_to_node(&nd_head, &nd_tail, C->pointhead, 0);

   /* 
    * Now do the clipping 
    * If the curve is wholly inside the clipping planes, don't clip 
    * To get consistent results during zoom (i.e when x,y bnds change)
    * clip against z first.  This is necessary for rectangles and polygons.
    */
   if (zclip && zin==1) CNclip_z(&nd_head, &nd_tail, zmin, zmax, verbose);
   if (xclip && xin==1) CNclip_x(&nd_head, &nd_tail, xmin, xmax, verbose);
   if (yclip && yin==1) CNclip_y(&nd_head, &nd_tail, ymin, ymax, verbose);

   /* 
    * Transfer this information back to a point list
    */
   for (N=nd_head; N!=NULL; N=N->next) 
      (void) CNinsert_tailpoint(pt_head,pt_tail,
                         N->coord->x,N->coord->y,N->coord->z,0);

   /*
    * Delete the node list 
    */
   CNremove_node_list(&nd_head, &nd_tail);
}


/**********************************************************************/
/*                                                                    */
/*  This routine acts as the interface to the polygon-clipping subr.  */
/*  It takes a list of points, clips it against the boundaries        */
/*  and returns a modified vertice/node list.                         */
/*  The original list is modified, so be careful!                     */
/*                                                                    */
/**********************************************************************/

void CNclip_pointlist(pointhead, pointtail, 
                      xmin, xmax, ymin, ymax, zmin, zmax,
                      xclip, yclip, zclip, verbose)
CNpointptr *pointhead;        /* List of vertices before & after clipping */
CNpointptr *pointtail;        /* List of vertices before & after clipping */
double     xmin;              /* Min x-clipping plane            */
double     xmax;              /* Max x-clipping plane            */
double     ymin;              /* Min y-clipping plane            */
double     ymax;              /* Max y-clipping plane            */
double     zmin;              /* Min z-clipping plane            */
double     zmax;              /* Max z-clipping plane            */
int        xclip;             /* Clip in x                       */
int        yclip;             /* Clip in y                       */
int        zclip;             /* Clip in z                       */
int        verbose;           /* Print out debugging info        */
{
   CNnodeptr  nd_head=NULL, nd_tail=NULL, N;
   CNpointptr P;
   int xin, yin, zin, clip;

   /*
    * The points are saved in a temporary node list.
    * The node list is continually deleted and recreated, so 
    * potentially this can be quite expensive.
    * To make sure that the nodes are completely removed, 
    * we remove the nodes completely, using CNremove_node()
    */

   /* Check to see if the point list is empty */
   if ((*pointhead) == NULL) {
      return;
   }

   /* Check to see if any clipping is requested */
   if ((xclip || yclip || zclip)==0) {
      CNdelete_point_list(pointhead, pointtail);
      return;
   }

   /* Check the bounds of the curve */
   clip = CNpointlist_in_bounds(*pointhead, *pointtail,
                                xmin,xmax,ymin,ymax,zmin,zmax,&xin,&yin,&zin);

   /* If the curve is out of bounds return */
   if (clip==0) {
      CNdelete_point_list(pointhead, pointtail);
      return;
   }

   /* Copy the curve pointlist to a new list */
   for (P=(*pointhead); P!=NULL; P=P->next)
      copy_point_to_node(&nd_head, &nd_tail, P, 0);

   /* 
    * Now do the clipping 
    * If the curve is wholly inside the clipping planes, don't clip 
    * To get consistent results during zoom (i.e when x,y bnds change)
    * clip against z first.  This is necessary for rectangles and polygons.
    *
    * Note that the clipping works on a new list of points/nodes
    * and does not affect the original point list.  When we are done,
    * copy the list back.
    */
   if (zclip && zin==1) CNclip_z(&nd_head, &nd_tail, zmin, zmax, verbose);
   if (xclip && xin==1) CNclip_x(&nd_head, &nd_tail, xmin, xmax, verbose);
   if (yclip && yin==1) CNclip_y(&nd_head, &nd_tail, ymin, ymax, verbose);

   /* 
    * Transfer this information back to a point list
    */
   /* Delete the current point list */
   CNdelete_point_list(pointhead, pointtail);
   for (N=nd_head; N!=NULL; N=N->next) 
      (void) CNinsert_tailpoint(pointhead,pointtail,
                                N->coord->x,N->coord->y,N->coord->z,0);

   /*
    * Delete the node list 
    */
   CNremove_node_list(&nd_head, &nd_tail);
}


/**********************************************************************/
/*                                                                    */
/*  This routine acts as the interface to the polygon-clipping subr.  */
/*  It takes a list of vertices/nodes, clips them against the         */
/*  boundaries (x,y, or z) and returns a modified vertice/node list.  */
/*                                                                    */
/**********************************************************************/

/*
 * Clip in x
 */
void CNclip_x(oldnd_head, oldnd_tail, xmin, xmax, verbose)
CNnodeptr *oldnd_head;        /* List of vertices to be clipped  */
CNnodeptr *oldnd_tail;        /* List of vertices to be clipped  */
double    xmin;               /* Min x-clipping plane            */
double    xmax;               /* Max x-clipping plane            */
int       verbose;            /* Print out debugging info        */
{
   CNnodeptr newnd_head=NULL; /* List of vertices after clipping */
   CNnodeptr newnd_tail=NULL; /* List of vertices after clipping */
   int       closed;

   /* Check to see if the node list is empty */
   if (*oldnd_head == NULL) return;

   /* Check to see if this is a closed curve */
   closed = !CNlongline((*oldnd_head)->coord,(*oldnd_tail)->coord,1.0e-5);

   /* Clip the bottom */
   clip_x(oldnd_head, oldnd_tail, &newnd_head, &newnd_tail, xmin, 1.0);
   if (closed && newnd_head!=NULL && 
       CNlongline(newnd_head->coord,newnd_tail->coord,1.0e-5))
       create_node(&newnd_head, &newnd_tail, newnd_head, (newnd_tail->ID)++);

   /* Print out the lists */
   if (verbose) {
      (void) fprintf(stdout, "Clipping (in x) between %f - %f\n",xmin,xmax);
      (void) fprintf(stdout, "Node-list before clipping\n");
      CNprint_node_list(*oldnd_head, *oldnd_tail);
      (void) fprintf(stdout, "Node-list after clipping\n");
      CNprint_node_list( newnd_head,  newnd_tail);
   }

   /* Reset the node-pointers */
   CNremove_node_list(oldnd_head, oldnd_tail);
   *oldnd_head = newnd_head;
   *oldnd_tail = newnd_tail;
   newnd_head  = NULL;
   newnd_tail  = NULL;

   /* Clip the top */
   clip_x(oldnd_head, oldnd_tail, &newnd_head, &newnd_tail, xmax, -1.0);
   if (closed && newnd_head!=NULL && 
       CNlongline(newnd_head->coord,newnd_tail->coord,1.0e-5))
       create_node(&newnd_head, &newnd_tail, newnd_head, (newnd_tail->ID)++);

   /* Print out the lists */
   if (verbose) {
      (void) fprintf(stdout, "Node-list before clipping\n");
      CNprint_node_list(*oldnd_head, *oldnd_tail);
      (void) fprintf(stdout, "Node-list after clipping\n");
      CNprint_node_list( newnd_head,  newnd_tail);
   }

   /* Reset the node-pointers */
   CNremove_node_list(oldnd_head, oldnd_tail);
   *oldnd_head = newnd_head;
   *oldnd_tail = newnd_tail;
   newnd_head  = NULL;
   newnd_tail  = NULL;
}


/*
 * Clip in y
 */
void CNclip_y(oldnd_head, oldnd_tail, ymin, ymax, verbose)
CNnodeptr *oldnd_head;        /* List of vertices to be clipped  */
CNnodeptr *oldnd_tail;        /* List of vertices to be clipped  */
double    ymin;               /* Min y-clipping plane            */
double    ymax;               /* Max y-clipping plane            */
int       verbose;            /* Print out debugging info        */
{
   CNnodeptr newnd_head=NULL; /* List of vertices after clipping */
   CNnodeptr newnd_tail=NULL; /* List of vertices after clipping */
   int       closed;

   /* Check to see if the node list is empty */
   if (*oldnd_head == NULL) return;

   /* Check to see if this is a closed curve */
   closed = !CNlongline((*oldnd_head)->coord,(*oldnd_tail)->coord,1.0e-5);

   /* Clip the bottom */
   clip_y(oldnd_head, oldnd_tail, &newnd_head, &newnd_tail, ymin, 1.0);
   if (closed && newnd_head!=NULL && 
       CNlongline(newnd_head->coord,newnd_tail->coord,1.0e-5))
       create_node(&newnd_head, &newnd_tail, newnd_head, (newnd_tail->ID)++);

   /* Print out the lists */
   if (verbose) {
      (void) fprintf(stdout, "Clipping (in y) between %f - %f\n",ymin,ymax);
      (void) fprintf(stdout, "Node-list before clipping\n");
      CNprint_node_list(*oldnd_head, *oldnd_tail);
      (void) fprintf(stdout, "Node-list after clipping\n");
      CNprint_node_list( newnd_head,  newnd_tail);
   }

   /* Reset the node-pointers */
   CNremove_node_list(oldnd_head, oldnd_tail);
   *oldnd_head = newnd_head;
   *oldnd_tail = newnd_tail;
   newnd_head  = NULL;
   newnd_tail  = NULL;

   /* Clip the top */
   clip_y(oldnd_head, oldnd_tail, &newnd_head, &newnd_tail, ymax, -1.0);
   if (closed && newnd_head!=NULL && 
       CNlongline(newnd_head->coord,newnd_tail->coord,1.0e-5))
       create_node(&newnd_head, &newnd_tail, newnd_head, (newnd_tail->ID)++);

   /* Print out the lists */
   if (verbose) {
      (void) fprintf(stdout, "Node-list before clipping\n");
      CNprint_node_list(*oldnd_head, *oldnd_tail);
      (void) fprintf(stdout, "Node-list after clipping\n");
      CNprint_node_list( newnd_head,  newnd_tail);
   }

   /* Reset the node-pointers */
   CNremove_node_list(oldnd_head, oldnd_tail);
   *oldnd_head = newnd_head;
   *oldnd_tail = newnd_tail;
   newnd_head  = NULL;
   newnd_tail  = NULL;
}


/*
 * Clip in z
 */
void CNclip_z(oldnd_head, oldnd_tail, zmin, zmax, verbose)
CNnodeptr *oldnd_head;        /* List of vertices to be clipped  */
CNnodeptr *oldnd_tail;        /* List of vertices to be clipped  */
double    zmin;               /* Min z-clipping plane            */
double    zmax;               /* Max z-clipping plane            */
int       verbose;            /* Print out debugging info        */
{
   CNnodeptr newnd_head=NULL; /* List of vertices after clipping */
   CNnodeptr newnd_tail=NULL; /* List of vertices after clipping */
   int       closed;

   /* Check to see if the node list is empty */
   if (*oldnd_head == NULL) return;

   /* Check to see if this is a closed curve */
   closed = !CNlongline((*oldnd_head)->coord,(*oldnd_tail)->coord,1.0e-5);

   /* Clip the bottom */
   clip_z(oldnd_head, oldnd_tail, &newnd_head, &newnd_tail, zmin, 1.0);
   if (closed && newnd_head!=NULL && 
       CNlongline(newnd_head->coord,newnd_tail->coord,1.0e-5))
       create_node(&newnd_head, &newnd_tail, newnd_head, (newnd_tail->ID)++);

   /* Print out the lists */
   if (verbose) {
      (void) fprintf(stdout, "Clipping (in z) between %f - %f\n",zmin,zmax);
      (void) fprintf(stdout, "Node-list before clipping\n");
      CNprint_node_list(*oldnd_head, *oldnd_tail);
      (void) fprintf(stdout, "Node-list after clipping\n");
      CNprint_node_list( newnd_head,  newnd_tail);
   }

   /* Reset the node-pointers */
   CNremove_node_list(oldnd_head, oldnd_tail);
   *oldnd_head = newnd_head;
   *oldnd_tail = newnd_tail;
   newnd_head  = NULL;
   newnd_tail  = NULL;

   /* Clip the top */
   clip_z(oldnd_head, oldnd_tail, &newnd_head, &newnd_tail, zmax, -1.0);
   if (closed && newnd_head!=NULL && 
       CNlongline(newnd_head->coord,newnd_tail->coord,1.0e-5))
       create_node(&newnd_head, &newnd_tail, newnd_head, (newnd_tail->ID)++);

   /* Print out the lists */
   if (verbose) {
      (void) fprintf(stdout, "Node-list before clipping\n");
      CNprint_node_list(*oldnd_head, *oldnd_tail);
      (void) fprintf(stdout, "Node-list after clipping\n");
      CNprint_node_list( newnd_head,  newnd_tail);
   }

   /* Reset the node-pointers */
   CNremove_node_list(oldnd_head, oldnd_tail);
   *oldnd_head = newnd_head;
   *oldnd_tail = newnd_tail;
   newnd_head  = NULL;
   newnd_tail  = NULL;
}


/*
 * Clip in t
 */
void CNclip_t(oldnd_head, oldnd_tail, tmin, tmax, verbose)
CNnodeptr *oldnd_head;        /* List of vertices to be clipped  */
CNnodeptr *oldnd_tail;        /* List of vertices to be clipped  */
double    tmin;               /* Min t-clipping plane            */
double    tmax;               /* Max t-clipping plane            */
int       verbose;            /* Print out debugging info        */
{
   CNnodeptr newnd_head=NULL; /* List of vertices after clipping */
   CNnodeptr newnd_tail=NULL; /* List of vertices after clipping */
   int       closed;

   /* Check to see if the node list is empty */
   if (*oldnd_head == NULL) return;

   /* Check to see if this is a closed curve */
   closed = !CNlongline((*oldnd_head)->coord,(*oldnd_tail)->coord,1.0e-5);

   /* Clip the bottom */
   clip_t(oldnd_head, oldnd_tail, &newnd_head, &newnd_tail, tmin, 1.0);
   if (closed && newnd_head!=NULL && 
       CNlongline(newnd_head->coord,newnd_tail->coord,1.0e-5))
       create_node(&newnd_head, &newnd_tail, newnd_head, (newnd_tail->ID)++);

   /* Print out the lists */
   if (verbose) {
      (void) fprintf(stdout, "Clipping (in t) between %f - %f\n",tmin,tmax);
      (void) fprintf(stdout, "Node-list before clipping\n");
      CNprint_node_list(*oldnd_head, *oldnd_tail);
      (void) fprintf(stdout, "Node-list after clipping\n");
      CNprint_node_list( newnd_head,  newnd_tail);
   }

   /* Reset the node-pointers */
   CNremove_node_list(oldnd_head, oldnd_tail);
   *oldnd_head = newnd_head;
   *oldnd_tail = newnd_tail;
   newnd_head  = NULL;
   newnd_tail  = NULL;

   /* Clip the top */
   clip_t(oldnd_head, oldnd_tail, &newnd_head, &newnd_tail, tmax, -1.0);
   if (closed && newnd_head!=NULL && 
       CNlongline(newnd_head->coord,newnd_tail->coord,1.0e-5))
       create_node(&newnd_head, &newnd_tail, newnd_head, (newnd_tail->ID)++);

   /* Print out the lists */
   if (verbose) {
      (void) fprintf(stdout, "Node-list before clipping\n");
      CNprint_node_list(*oldnd_head, *oldnd_tail);
      (void) fprintf(stdout, "Node-list after clipping\n");
      CNprint_node_list( newnd_head,  newnd_tail);
   }

   /* Reset the node-pointers */
   CNremove_node_list(oldnd_head, oldnd_tail);
   *oldnd_head = newnd_head;
   *oldnd_tail = newnd_tail;
   newnd_head  = NULL;
   newnd_tail  = NULL;
}

/**********************************************************************/
/*                                                                    */
/*  This routine clips a polygon against a given set of boundaries.   */
/*  This routine assumes that the list of points passed is NOT        */
/*  closed.  A closed polygon is clipped by specifying the first      */
/*  point twice.                                                      */
/*                                                                    */
/*  Based on Sutherland and Hodgmann ACM Jan, 1974.                   */
/*  Based on implementation in SUPREM4 11/86                          */
/*                                                                    */
/*  If (sign > 0) all points greater than z_clip are "inside"         */
/*  If (sign < 0) all points less    than z_clip are "inside"         */
/*                                                                    */
/**********************************************************************/

#define a 4-dimensional datapoint
typedef struct _CLdatapt_strct {
   double x;
   double y;
   double z;
   double t;
} CLdatapt;

/*
 * Clip in x 
 */
/*ARGSUSED*/
static void clip_x(oldnd_head, oldnd_tail, newnd_head, newnd_tail, val, sign)
CNnodeptr *oldnd_head;        /* List of vertices to be clipped  */
CNnodeptr *oldnd_tail;        /* List of vertices to be clipped  */
CNnodeptr *newnd_head;        /* List of vertices after clipping */
CNnodeptr *newnd_tail;        /* List of vertices after clipping */
double    val;                /* The value to be clipped against */
double    sign;               /* The sign of the clipping        */
                              /*    zval*sign < 0 for outside    */
{
   CNnodeptr N;               /* The node/vertex pointer         */
   CLdatapt  curr;            /* The current point coords        */
   CLdatapt  newpt;           /* The new     point coords        */
   double    alpha;           /* interpolation value             */
   int       last;            /* Boolean for last point in or out*/
   int       this;            /* Boolean for this point in or out*/
   int       ndID=0;          /* ID of the newly created nodes   */

   /* Check to see if the node list is empty */
   if (*oldnd_head == NULL) return;

   /* Start with the first point */
   curr.x = (*oldnd_head)->coord->x;
   curr.y = (*oldnd_head)->coord->y;
   curr.z = (*oldnd_head)->coord->z;
   curr.t = (*oldnd_head)->t;

   /* initialize the last point by checking the first */
   last =  ( (curr.x - val) * sign ) >= 0.0; 

   /* Check for every point */
   for (N=(*oldnd_head); N!=NULL; N=N->next) {

      /* Check the current point */
      this = ( (N->coord->x - val) * sign ) >= 0.0;

      /* If this point and the last are on different sides, add intersection */
      if (this != last) {
         /* Compute and add intersection */
         alpha = (val - curr.x) / (N->coord->x - curr.x);
         newpt.x = val; 
         newpt.y = curr.y + alpha*(N->coord->y - curr.y);
         newpt.z = curr.z + alpha*(N->coord->z - curr.z);
         newpt.t = curr.t + alpha*(N->t        - curr.t);
         (void)CNcreate_tailnode(newnd_head, newnd_tail, 
               newpt.x, newpt.y, newpt.z, newpt.t, ndID++);
      }

      /* If this point is in, add it */
      if (this) 
         (void)CNcreate_tailnode(newnd_head, newnd_tail, 
               N->coord->x, N->coord->y, N->coord->z, N->t, ndID++);


      /* Reset the current point */
      last = this;
      curr.x = N->coord->x;
      curr.y = N->coord->y;
      curr.z = N->coord->z;
      curr.t = N->t;
   }
}


/*
 * Clip in y
 */
/*ARGSUSED*/
static void clip_y(oldnd_head, oldnd_tail, newnd_head, newnd_tail, val, sign)
CNnodeptr *oldnd_head;        /* List of vertices to be clipped  */
CNnodeptr *oldnd_tail;        /* List of vertices to be clipped  */
CNnodeptr *newnd_head;        /* List of vertices after clipping */
CNnodeptr *newnd_tail;        /* List of vertices after clipping */
double    val;                /* The value to be clipped against */
double    sign;               /* The sign of the clipping        */
                              /*    zval*sign < 0 for outside    */
{
   CNnodeptr N;               /* The node/vertex pointer         */
   CLdatapt  curr;            /* The current point coords        */
   CLdatapt  newpt;           /* The new     point coords        */
   double    alpha;           /* interpolation value             */
   int       last;            /* Boolean for last point in or out*/
   int       this;            /* Boolean for this point in or out*/
   int       ndID=0;          /* ID of the newly created nodes   */

   /* Check to see if the node list is empty */
   if (*oldnd_head == NULL) return;

   /* Start with the first point */
   curr.x = (*oldnd_head)->coord->x;
   curr.y = (*oldnd_head)->coord->y;
   curr.z = (*oldnd_head)->coord->z;
   curr.t = (*oldnd_head)->t;

   /* initialize the last point by checking the first */
   last =  ( (curr.y - val) * sign ) >= 0.0; 

   /* Check for every point */
   for (N=(*oldnd_head); N!=NULL; N=N->next) {

      /* Check the current point */
      this = ( (N->coord->y - val) * sign ) >= 0.0;

      /* If this point and the last are on different sides, add intersection */
      if (this != last) {
         /* Compute and add intersection */
         alpha = (val - curr.y) / (N->coord->y - curr.y);
         newpt.x = curr.x + alpha*(N->coord->x - curr.x);
         newpt.y = val; 
         newpt.z = curr.z + alpha*(N->coord->z - curr.z);
         newpt.t = curr.t + alpha*(N->t        - curr.t);
         (void)CNcreate_tailnode(newnd_head, newnd_tail, 
               newpt.x, newpt.y, newpt.z, newpt.t, ndID++);
      }

      /* If this point is in, add it */
      if (this) 
         (void)CNcreate_tailnode(newnd_head, newnd_tail,
               N->coord->x, N->coord->y, N->coord->z, N->t, ndID++);


      /* Reset the current point */
      last = this;
      curr.x = N->coord->x;
      curr.y = N->coord->y;
      curr.z = N->coord->z;
      curr.t = N->t;
   }
}


/*
 * Clip in z
 */
/*ARGSUSED*/
static void clip_z(oldnd_head, oldnd_tail, newnd_head, newnd_tail, val, sign)
CNnodeptr *oldnd_head;        /* List of vertices to be clipped  */
CNnodeptr *oldnd_tail;        /* List of vertices to be clipped  */
CNnodeptr *newnd_head;        /* List of vertices after clipping */
CNnodeptr *newnd_tail;        /* List of vertices after clipping */
double    val;                /* The value to be clipped against */
double    sign;               /* The sign of the clipping        */
                              /*    zval*sign < 0 for outside    */
{
   CNnodeptr N;               /* The node/vertex pointer         */
   CLdatapt  curr;            /* The current point coords        */
   CLdatapt  newpt;           /* The new     point coords        */
   double    alpha;           /* interpolation value             */
   int       last;            /* Boolean for last point in or out*/
   int       this;            /* Boolean for this point in or out*/
   int       ndID=0;          /* ID of the newly created nodes   */

   /* Check to see if the node list is empty */
   if (*oldnd_head == NULL) return;

   /* Start with the first point */
   curr.x = (*oldnd_head)->coord->x;
   curr.y = (*oldnd_head)->coord->y;
   curr.z = (*oldnd_head)->coord->z;
   curr.t = (*oldnd_head)->t;

   /* initialize the last point by checking the first */
   last =  ( (curr.z - val) * sign ) >= 0.0; 

   /* Check for every point */
   for (N=(*oldnd_head); N!=NULL; N=N->next) {

      /* Check the current point */
      this = ( (N->coord->z - val) * sign ) >= 0.0;

      /* If this point and the last are on different sides, add intersection */
      if (this != last) {
         /* Compute and add intersection */
         alpha = (val - curr.z) / (N->coord->z - curr.z);
         newpt.x = curr.x + alpha*(N->coord->x - curr.x);
         newpt.y = curr.y + alpha*(N->coord->y - curr.y);
         newpt.z = val; 
         newpt.t = curr.t + alpha*(N->t        - curr.t);
         (void)CNcreate_tailnode(newnd_head, newnd_tail,
               newpt.x, newpt.y, newpt.z, newpt.t, ndID++);

      }

      /* If this point is in, add it */
      if (this) 
         (void)CNcreate_tailnode(newnd_head, newnd_tail,
               N->coord->x, N->coord->y, N->coord->z, N->t, ndID++);

      /* Reset the current point */
      last = this;
      curr.x = N->coord->x;
      curr.y = N->coord->y;
      curr.z = N->coord->z;
      curr.t = N->t;
   }
}


/*
 * Clip in t
 */
/*ARGSUSED*/
static void clip_t(oldnd_head, oldnd_tail, newnd_head, newnd_tail, val, sign)
CNnodeptr *oldnd_head;        /* List of vertices to be clipped  */
CNnodeptr *oldnd_tail;        /* List of vertices to be clipped  */
CNnodeptr *newnd_head;        /* List of vertices after clipping */
CNnodeptr *newnd_tail;        /* List of vertices after clipping */
double    val;                /* The value to be clipped against */
double    sign;               /* The sign of the clipping        */
                              /*    tval*sign < 0 for outside    */
{
   CNnodeptr N;               /* The node/vertex pointer         */
   CLdatapt  curr;            /* The current point coords        */
   CLdatapt  newpt;           /* The new     point coords        */
   double    alpha;           /* interpolation value             */
   int       last;            /* Boolean for last point in or out*/
   int       this;            /* Boolean for this point in or out*/
   int       ndID=0;          /* ID of the newly created nodes   */

   /* Check to see if the node list is empty */
   if (*oldnd_head == NULL) return;

   /* Start with the first point */
   curr.x = (*oldnd_head)->coord->x;
   curr.y = (*oldnd_head)->coord->y;
   curr.z = (*oldnd_head)->coord->z;
   curr.t = (*oldnd_head)->t;

   /* initialize the last point by checking the first */
   last =  ( (curr.t - val) * sign ) >= 0.0; 

   /* Check for every point */
   for (N=(*oldnd_head); N!=NULL; N=N->next) {

      /* Check the current point */
      this = ( (N->t        - val) * sign ) >= 0.0;

      /* If this point and the last are on different sides, add intersection */
      if (this != last) {
         /* Compute and add intersection */
         alpha = (val - curr.t) / (N->t        - curr.t);
         newpt.x = curr.x + alpha*(N->coord->x - curr.x);
         newpt.y = curr.y + alpha*(N->coord->y - curr.y);
         newpt.z = curr.z + alpha*(N->coord->z - curr.z);
         newpt.t = val; 
         (void)CNcreate_tailnode(newnd_head, newnd_tail,
               newpt.x, newpt.y, newpt.z, newpt.t, ndID++);

      }

      /* If this point is in, add it */
      if (this) 
         (void)CNcreate_tailnode(newnd_head, newnd_tail,
               N->coord->x, N->coord->y, N->coord->z, N->t, ndID++);

      /* Reset the current point */
      last = this;
      curr.x = N->coord->x;
      curr.y = N->coord->y;
      curr.z = N->coord->z;
      curr.t = N->t;
   }
}


/**********************************************************************/
/*                                                                    */
/*  The following routines are used to find out if a triangle or a    */
/*  rectangle is inside or outside a given volume.  The routines      */
/*  take a triangle or rectangle and compares its nodes against       */
/*  the given boundary values.                                        */
/*                                                                    */
/*  Return status :                                                   */
/*     0 : Outside                                                    */
/*     1 : Inside/Intersecting                                        */
/*     2 : Wholly inside                                              */
/*                                                                    */
/**********************************************************************/

/*
 * Check the triangle in x,y,z, and t
 */
int CNtria_in_bounds(T,xmin,xmax,ymin,ymax,zmin,zmax,tmin,tmax)
CNtriaptr T;                  /* Triangle to be checked          */
double    xmin, xmax;         /* Min and Max x-coordinates       */
double    ymin, ymax;         /* Min and Max y-coordinates       */
double    zmin, zmax;         /* Min and Max z-coordinates       */
double    tmin, tmax;         /* Min and Max t-values            */
{
   int inside, clip_in_x, clip_in_y, clip_in_z, clip_in_t;

   /* Check the triangle to see if it should be clipped */
   clip_in_x = CNtria_in_xbounds(T,xmin,xmax);
   clip_in_y = CNtria_in_ybounds(T,ymin,ymax);
   clip_in_z = CNtria_in_zbounds(T,zmin,zmax);
   clip_in_t = CNtria_in_tbounds(T,tmin,tmax);

   /* Return */
   inside = clip_in_x && clip_in_y && clip_in_z && clip_in_t;
   return(inside);
}

/*
 * Check the triangle in x
 */
int CNtria_in_xbounds(T,xmin,xmax)
CNtriaptr T;                  /* Triangle to be checked          */
double    xmin, xmax;         /* Min and Max x-coordinates       */
{
   int inside;

   if ( (T->n1->coord->x > xmax) && (T->n2->coord->x > xmax) &&
        (T->n3->coord->x > xmax) )
      inside = 0;
   else
   if ( (T->n1->coord->x < xmin) && (T->n2->coord->x < xmin) &&
        (T->n3->coord->x < xmin) )
      inside = 0;
   else
   if ( ((T->n1->coord->x <= xmax) &&
         (T->n2->coord->x <= xmax) &&
         (T->n3->coord->x <= xmax)) &&
        ((T->n1->coord->x >= xmin) &&
         (T->n2->coord->x >= xmin) &&
         (T->n3->coord->x >= xmin)) ) 
      /* The rectangle lies wholly inside the clipping planes */
      inside = 2;
   else
      inside = 1;

   /* Return the result */
   return(inside);
}

/*
 * Check the triangle in y
 */
int CNtria_in_ybounds(T,ymin,ymax)
CNtriaptr T;                  /* Triangle to be checked          */
double    ymin, ymax;         /* Min and Max y-coordinates       */
{
   int inside;

   if ( (T->n1->coord->y > ymax) && (T->n2->coord->y > ymax) &&
        (T->n3->coord->y > ymax) )
      inside = 0;
   else
   if ( (T->n1->coord->y < ymin) && (T->n2->coord->y < ymin) &&
        (T->n3->coord->y < ymin) )
      inside = 0;
   else
   if ( ((T->n1->coord->y <= ymax) &&
         (T->n2->coord->y <= ymax) &&
         (T->n3->coord->y <= ymax)) &&
        ((T->n1->coord->y >= ymin) &&
         (T->n2->coord->y >= ymin) &&
         (T->n3->coord->y >= ymin)) ) 
      /* The rectangle lies wholly inside the clipping planes */
      inside = 2;
   else
      inside = 1;

   /* Return the result */
   return(inside);
}

/*
 * Check the triangle in z
 */
int CNtria_in_zbounds(T,zmin,zmax)
CNtriaptr T;                  /* Triangle to be checked          */
double    zmin, zmax;         /* Min and Max z-coordinates       */
{
   int inside;

   if ( (T->n1->coord->z > zmax) && (T->n2->coord->z > zmax) &&
        (T->n3->coord->z > zmax) )
      inside = 0;
   else
   if ( (T->n1->coord->z < zmin) && (T->n2->coord->z < zmin) &&
        (T->n3->coord->z < zmin) )
      inside = 0;
   else
   if ( ((T->n1->coord->z <= zmax) &&
         (T->n2->coord->z <= zmax) &&
         (T->n3->coord->z <= zmax)) &&
        ((T->n1->coord->z >= zmin) &&
         (T->n2->coord->z >= zmin) &&
         (T->n3->coord->z >= zmin)) ) 
      /* The rectangle lies wholly inside the clipping planes */
      inside = 2;
   else
      inside = 1;

   /* Return the result */
   return(inside);
}

/*
 * Check the triangle in t
 */
int CNtria_in_tbounds(T,tmin,tmax)
CNtriaptr T;                  /* Triangle to be checked          */
double    tmin, tmax;         /* Min and Max t-values            */
{
   int inside;

   if ( (T->n1->t > tmax) && (T->n2->t > tmax) &&
        (T->n3->t > tmax) )
      inside = 0;
   else
   if ( (T->n1->t < tmin) && (T->n2->t < tmin) &&
        (T->n3->t < tmin) )
      inside = 0;
   else
   if ( ((T->n1->t <= tmax) &&
         (T->n2->t <= tmax) &&
         (T->n3->t <= tmax)) &&
        ((T->n1->t >= tmin) &&
         (T->n2->t >= tmin) &&
         (T->n3->t >= tmin)) ) 
      /* The rectangle lies wholly inside the clipping planes */
      inside = 2;
   else
      inside = 1;

   /* Return the result */
   return(inside);
}

/*
 * Do the same for rectangles
 */

/*
 * Check the rectangle in x,y,z and t
 */
int CNrect_in_bounds(R,xmin,xmax,ymin,ymax,zmin,zmax,tmin,tmax)
CNrectptr R;                  /* Rectangle to be checked         */
double    xmin, xmax;         /* Min and Max x-coordinates       */
double    ymin, ymax;         /* Min and Max y-coordinates       */
double    zmin, zmax;         /* Min and Max z-coordinates       */
double    tmin, tmax;         /* Min and Max t-values            */
{
   int inside, clip_in_x, clip_in_y, clip_in_z, clip_in_t;

   /* Check the rectangle to see if it should be clipped */
   clip_in_x = CNrect_in_xbounds(R,xmin,xmax);
   clip_in_y = CNrect_in_ybounds(R,ymin,ymax);
   clip_in_z = CNrect_in_zbounds(R,zmin,zmax);
   clip_in_t = CNrect_in_tbounds(R,tmin,tmax);

   /* Return */
   inside = clip_in_x && clip_in_y && clip_in_z && clip_in_t;
   return(inside);
}

/*
 * Check the rectangle in x
 */
int CNrect_in_xbounds(R,xmin,xmax)
CNrectptr R;                  /* Rectangle to be checked         */
double    xmin, xmax;         /* Min and Max x-coordinates       */
{
   int inside;

   if ( (R->n1->coord->x > xmax) && (R->n2->coord->x > xmax) &&
        (R->n3->coord->x > xmax) && (R->n4->coord->x > xmax) )
      inside = 0;
   else
   if ( (R->n1->coord->x < xmin) && (R->n2->coord->x < xmin) &&
        (R->n3->coord->x < xmin) && (R->n4->coord->x < xmin) )
      inside = 0;
   else
   if ( ((R->n1->coord->x <= xmax) &&
         (R->n2->coord->x <= xmax) &&
         (R->n3->coord->x <= xmax) &&
         (R->n4->coord->x <= xmax))  &&
        ((R->n1->coord->x >= xmin) &&
         (R->n2->coord->x >= xmin) &&
         (R->n3->coord->x >= xmin) &&
         (R->n4->coord->x >= xmin)) )
      /* The rectangle lies wholly inside the clipping planes */
      inside = 2;
   else
      inside = 1;

   /* Return the result */
   return(inside);
}

/*
 * Check the rectangle in y
 */
int CNrect_in_ybounds(R,ymin,ymax)
CNrectptr R;                  /* Rectangle to be checked         */
double    ymin, ymax;         /* Min and Max y-coordinates       */
{
   int inside;

   if ( (R->n1->coord->y > ymax) && (R->n2->coord->y > ymax) &&
        (R->n3->coord->y > ymax) && (R->n4->coord->y > ymax) )
      inside = 0;
   else
   if ( (R->n1->coord->y < ymin) && (R->n2->coord->y < ymin) &&
        (R->n3->coord->y < ymin) && (R->n4->coord->y < ymin) )
      inside = 0;
   else
   if ( ((R->n1->coord->y <= ymax) &&
         (R->n2->coord->y <= ymax) &&
         (R->n3->coord->y <= ymax) &&
         (R->n4->coord->y <= ymax))  &&
        ((R->n1->coord->y >= ymin) &&
         (R->n2->coord->y >= ymin) &&
         (R->n3->coord->y >= ymin) &&
         (R->n4->coord->y >= ymin)) )
      /* The rectangle lies wholly inside the clipping planes */
      inside = 2;
   else
      inside = 1;

   /* Return the result */
   return(inside);
}

/*
 * Check the rectangle in z
 */
int CNrect_in_zbounds(R,zmin,zmax)
CNrectptr R;                  /* Rectangle to be checked         */
double    zmin, zmax;         /* Min and Max z-coordinates       */
{
   int inside;

   if ( (R->n1->coord->z > zmax) && (R->n2->coord->z > zmax) &&
        (R->n3->coord->z > zmax) && (R->n4->coord->z > zmax) )
      inside = 0;
   else
   if ( (R->n1->coord->z < zmin) && (R->n2->coord->z < zmin) &&
        (R->n3->coord->z < zmin) && (R->n4->coord->z < zmin) )
      inside = 0;
   else
   if ( ((R->n1->coord->z <= zmax) &&
         (R->n2->coord->z <= zmax) &&
         (R->n3->coord->z <= zmax) &&
         (R->n4->coord->z <= zmax))  &&
        ((R->n1->coord->z >= zmin) &&
         (R->n2->coord->z >= zmin) &&
         (R->n3->coord->z >= zmin) &&
         (R->n4->coord->z >= zmin)) )
      /* The rectangle lies wholly inside the clipping planes */
      inside = 2;
   else
      inside = 1;

   /* Return the result */
   return(inside);
}

/*
 * Check the rectangle in t
 */
int CNrect_in_tbounds(R,tmin,tmax)
CNrectptr R;                  /* Rectangle to be checked         */
double    tmin, tmax;         /* Min and Max t-values            */
{
   int inside;

   if ( (R->n1->t > tmax) && (R->n2->t > tmax) &&
        (R->n3->t > tmax) && (R->n4->t > tmax) )
      inside = 0;
   else
   if ( (R->n1->t < tmin) && (R->n2->t < tmin) &&
        (R->n3->t < tmin) && (R->n4->t < tmin) )
      inside = 0;
   else
   if ( ((R->n1->t <= tmax) &&
         (R->n2->t <= tmax) &&
         (R->n3->t <= tmax) &&
         (R->n4->t <= tmax))  &&
        ((R->n1->t >= tmin) &&
         (R->n2->t >= tmin) &&
         (R->n3->t >= tmin) &&
         (R->n4->t >= tmin)) )
      /* The rectangle lies wholly inside the clipping planes */
      inside = 2;
   else
      inside = 1;

   /* Return the result */
   return(inside);
}


/*
 * Do the same for polygons 
 */

/*
 * Check the polygon 
 */
int CNpoly_in_bounds(P,xmin,xmax,ymin,ymax,zmin,zmax,tmin,tmax,
                     xin,yin,zin,tin)
CNpolyptr P;                  /* Polygon to be checked           */
double    xmin, xmax;         /* Min and Max x-coordinates       */
double    ymin, ymax;         /* Min and Max y-coordinates       */
double    zmin, zmax;         /* Min and Max z-coordinates       */
double    tmin, tmax;         /* Min and Max t-values            */
int       *xin,*yin,*zin,*tin;/* inside/outside flags            */
{
   double cxmin, cxmax, cymin, cymax, czmin, czmax, ctmin, ctmax;
   int    inside;

   /* Get the bounds of the polygon first */
   get_poly_maxmin(P,&cxmin,&cxmax,&cymin,&cymax,&czmin,&czmax,&ctmin,&ctmax);

   /* Initialize inside/outside flags */
   *xin = 0;
   *yin = 0;
   *zin = 0;
   *tin = 0;

   if      (cxmax < xmin || cxmin > xmax) *xin = 0;  /* Outside       */
   else if (cxmin > xmin && cxmax < xmax) *xin = 2;  /* wholly inside */
   else                                   *xin = 1;  /* Partially in  */

   if      (cymax < ymin || cymin > ymax) *yin = 0;  /* Outside       */
   else if (cymin > ymin && cymax < ymax) *yin = 2;  /* wholly inside */
   else                                   *yin = 1;  /* Partially in  */

   if      (czmax < zmin || czmin > zmax) *zin = 0;  /* Outside       */
   else if (czmin > zmin && czmax < zmax) *zin = 2;  /* wholly inside */
   else                                   *zin = 1;  /* Partially in  */

   if      (ctmax < tmin || ctmin > tmax) *tin = 0;  /* Outside       */
   else if (ctmin > tmin && ctmax < tmax) *tin = 2;  /* wholly inside */
   else                                   *tin = 1;  /* Partially in  */

   /* Return the result */
   inside = (*xin && *yin && *zin && *tin);
   return(inside);
}

/*
 * Find the min and max of a polygon
 */
static void get_poly_maxmin(P,xmin,xmax,ymin,ymax,zmin,zmax,tmin,tmax)
CNpolyptr P;
double    *xmin,*xmax,*ymin,*ymax,*zmin,*zmax,*tmin,*tmax;
{
   CNnlistptr Nd;

   /* Reinitialize */
   *xmin = *ymin = *zmin = *tmin =  CN_LARGE;
   *xmax = *ymax = *zmax = *tmax = -CN_LARGE;

   /* Search all the nodes */
   for (Nd=P->nlisthead; Nd!=NULL; Nd=Nd->next) {
      if (Nd->N->coord->x < *xmin) *xmin = Nd->N->coord->x;
      if (Nd->N->coord->x > *xmax) *xmax = Nd->N->coord->x;
      if (Nd->N->coord->y < *ymin) *ymin = Nd->N->coord->y;
      if (Nd->N->coord->y > *ymax) *ymax = Nd->N->coord->y;
      if (Nd->N->coord->z < *zmin) *zmin = Nd->N->coord->z;
      if (Nd->N->coord->z > *zmax) *zmax = Nd->N->coord->z;
      if (Nd->N->t        < *tmin) *tmin = Nd->N->t       ;
      if (Nd->N->t        > *tmax) *tmax = Nd->N->t       ;
   }
}


/*
 * Do the same for curves 
 */

/*
 * Check the curve 
 * This is a straight interface to CNpointlist_in_bounds()
 */
int CNcurve_in_bounds(C,
                      xmin,xmax,ymin,ymax,zmin,zmax,
                      xin,yin,zin)
CNcurveptr C;                 /* Curve to be checked             */
double    xmin, xmax;         /* Min and Max x-coordinates       */
double    ymin, ymax;         /* Min and Max y-coordinates       */
double    zmin, zmax;         /* Min and Max z-coordinates       */
int       *xin,*yin,*zin;     /* inside/outside flags            */
{
   return(CNpointlist_in_bounds(C->pointhead, C->pointtail,
                                xmin,xmax,ymin,ymax,zmin,zmax,
                                xin,yin,zin));
}



/*
 * Do the same for a list of points
 */

/*
 * Check the pointlist
 */
/*ARGSUSED*/
int CNpointlist_in_bounds(pointhead, pointtail,
                          xmin,xmax,ymin,ymax,zmin,zmax,
                          xin,yin,zin)
CNpointptr pointhead;         /* Point list to be checked        */
CNpointptr pointtail;         /* Point list to be checked        */
double     xmin, xmax;        /* Min and Max x-coordinates       */
double     ymin, ymax;        /* Min and Max y-coordinates       */
double     zmin, zmax;        /* Min and Max z-coordinates       */
int        *xin,*yin,*zin;    /* inside/outside flags            */
{
   double cxmin, cxmax, cymin, cymax, czmin, czmax;
   int    inside;

   /* Get the bounds of the curve first */
   get_pointlist_maxmin(pointhead, pointtail,
                        &cxmin,&cxmax,&cymin,&cymax,&czmin,&czmax);

   /* Initialize inside/outside flags */
   *xin = 0;
   *yin = 0;
   *zin = 0;

   if      (cxmax < xmin || cxmin > xmax) *xin = 0;  /* Outside       */
   else if (cxmin > xmin && cxmax < xmax) *xin = 2;  /* wholly inside */
   else                                   *xin = 1;  /* Partially in  */

   if      (cymax < ymin || cymin > ymax) *yin = 0;  /* Outside       */
   else if (cymin > ymin && cymax < ymax) *yin = 2;  /* wholly inside */
   else                                   *yin = 1;  /* Partially in  */

   if      (czmax < zmin || czmin > zmax) *zin = 0;  /* Outside       */
   else if (czmin > zmin && czmax < zmax) *zin = 2;  /* wholly inside */
   else                                   *zin = 1;  /* Partially in  */

   /* Return the result */
   inside = (*xin && *yin && *zin);
   return(inside);
}


/*
 * Find the min and max of a pointlist
 */
/*ARGSUSED*/
static void get_pointlist_maxmin(pointhead,pointtail,
                                 xmin,xmax,ymin,ymax,zmin,zmax)
CNpointptr pointhead, pointtail;
double    *xmin,*xmax,*ymin,*ymax,*zmin,*zmax;
{
   CNpointptr P;

   /* Reinitialize */
   *xmin = *ymin = *zmin =  CN_LARGE;
   *xmax = *ymax = *zmax = -CN_LARGE;

   /* Search all the points */
   for (P=pointhead; P!=NULL; P=P->next) {
      if (P->x < *xmin) *xmin = P->x;
      if (P->x > *xmax) *xmax = P->x;
      if (P->y < *ymin) *ymin = P->y;
      if (P->y > *ymax) *ymax = P->y;
      if (P->z < *zmin) *zmin = P->z;
      if (P->z > *zmax) *zmax = P->z;
   }
}


/* copy the contents of a node to a new node */
static void create_node(nd_head, nd_tail, N, ID)
CNnodeptr *nd_head, *nd_tail, N;
int       ID;
{
   (void)CNcreate_tailnode(nd_head, nd_tail, 
                           N->coord->x, N->coord->y, N->coord->z, N->t, ID);
}

/* copy the contents of a point to a new node */
static void copy_point_to_node(nd_head, nd_tail, P, ID)
CNnodeptr *nd_head, *nd_tail;
CNpointptr P;
int        ID;
{
   (void)CNcreate_tailnode(nd_head, nd_tail, 
                           P->x, P->y, P->z, P->z, ID);
}

