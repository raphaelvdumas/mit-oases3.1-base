/*
 * Point - find out the z-value of a point inside a triangle/rectangle
 */

#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <malloc.h>
#include "CNplot.h"

void CNprobe_dataset();
int  CNpoint_in_region();
static int  point_in_mesh();
static int  point_in_grid();
static int  point_in_tria();
static int  point_in_rect();
static int  point_in_poly();
 
static CNprobeptr make_probe();
static void print_probe();

int CNprobe_dataset_zave();

/*
 * Probe a dataset
 */
void CNprobe_dataset(Dptr, x, y, probehead, probetail, verbose)
CNdatasetptr Dptr;                    /* Dataset to be probed */
double       x, y;                    /* Probe coordinates    */
CNprobeptr   *probehead, *probetail;  /* Results              */
int          verbose;
{
   /* The probe will either work on triangles etc or on a grid array */
   if (Dptr->triahead || Dptr->recthead || Dptr->polyhead) {
      /* Probe the dataset mesh */
      (void) point_in_mesh(x,y,
                           Dptr->triahead, Dptr->triatail,
                           Dptr->recthead, Dptr->recttail,
                           Dptr->polyhead, Dptr->polytail,
                           Dptr->regionhead, Dptr->regiontail,
                           probehead, probetail, 
                           Dptr->data_pr.logx,
                           Dptr->data_pr.logy,
                           Dptr->data_pr.logz,
                           (int) Dptr->ID, 
                           verbose);
   } else if (Dptr->grid && Dptr->datatype==CN_CONTOUR) {
      /* Probe the dataset mesh */
      (void) point_in_grid(x,y,
                           Dptr->grid,
                           probehead, probetail, 
                           Dptr->data_pr.logx,
                           Dptr->data_pr.logy,
                           Dptr->data_pr.logz,
                           (int) Dptr->ID, 
                           verbose);
   }

   if (verbose) CNprint_probe_list(*probehead, *probetail);
}


/*
 * Check to see if a point in the mesh coincides with a region
 */
int CNpoint_in_region(Dptr,x,y,regID,verbose)
CNdatasetptr Dptr;                    /* Dataset to be probed */
double       x, y;                    /* Probe coordinates    */
int          regID;
int          verbose;
{
   CNprobeptr probehead=NULL, probetail=NULL, P;
   int        MATCHFOUND=CN_FALSE;

   /* Probe the mesh */
   (void) CNprobe_dataset(Dptr, x, y, &probehead, &probetail, verbose);

   /* Check the probe-points to see if the regionID coincides */
   for (P=probehead; P!=NULL && !MATCHFOUND; P=P->next) {
      if ((P->T != NULL) && (P->T->region == regID))
         MATCHFOUND = CN_TRUE;
      if ((P->R != NULL) && (P->R->region == regID))
         MATCHFOUND = CN_TRUE;
   }

   /* Delete the list */
   CNdelete_probe_list(&probehead, &probetail);

   /* return */
   return(MATCHFOUND);
}


/*
 * Search thru the mesh for an intersection with a line ax+by+d=0 (const z)
 */
/*ARGSUSED*/
static int point_in_mesh(x,y,
                         triahead,triatail,
                         recthead,recttail,
                         polyhead,polytail,
                         regionhead,regiontail,
                         probehead, probetail, 
                         logx, logy, logz,
                         dataID, verbose)
double       x, y;                    /* Probe coordinates        */
CNtriaptr    triahead, triatail;      /* Triangle  list           */
CNrectptr    recthead, recttail;      /* Rectangle list           */
CNpolyptr    polyhead, polytail;      /* Polygon   list           */
CNregionptr  regionhead, regiontail;  /* Region list              */
CNprobeptr   *probehead, *probetail;  /* Results                  */
short        logx, logy, logz;        /* Log/Linear interpolation */
int          dataID;
int          verbose;
{
   CNpointptr  pointhead=NULL, pointtail=NULL, P;
   CNtriaptr   T;
   CNrectptr   R;
   CNpolyptr   Po;
   CNregionptr reg, Treg=NULL;
   int         probe_count, new_probe_points;
   int         TREG_FOUND;

   /* Count the number of items in the probelist */
   probe_count = CNcount_probes(*probehead, *probetail);

   /* Go thru the triangles first */
   for (T=triahead; T!=NULL; T=T->next) {
      /* Search for point-intersections */
      (void) point_in_tria(T,x,y,&pointhead,&pointtail,
                           logx, logy, logz, verbose);

      /* Find the region */
      TREG_FOUND = CN_FALSE;
      for (reg=regionhead; reg!=NULL; reg=reg->next) 
         if (reg->ID == T->region) {
            TREG_FOUND = CN_TRUE;
            Treg       = reg;
         }
      if (!TREG_FOUND) Treg = NULL;

      /* Place the results in a probe-list */
      for (P=pointhead; P!=NULL; P=P->next) 
         (void) CNinsert_probe(probehead, probetail,
                               P->x,P->y,P->z,
                               T,(CNrectptr)NULL,(CNpolyptr)NULL,Treg,dataID);

      /* Remove everything in the pointlist */
      CNdelete_point_list(&pointhead, &pointtail);
   }

   /* Go thru the rectangles next */
   Treg = NULL;
   for (R=recthead; R!=NULL; R=R->next) {
      /* Search for point-intersections */
      (void) point_in_rect(R,x,y,&pointhead,&pointtail,
                           logx, logy, logz, verbose);

      /* Find the region */
      TREG_FOUND = CN_FALSE;
      for (reg=regionhead; reg!=NULL; reg=reg->next)
         if (reg->ID == R->region) {
            TREG_FOUND = CN_TRUE;
            Treg       = reg;
         }
      if (!TREG_FOUND) Treg = NULL;

      /* Place the results in a probe-list */
      for (P=pointhead; P!=NULL; P=P->next) 
         (void) CNinsert_probe(probehead, probetail,
                               P->x,P->y,P->z,
                               (CNtriaptr)NULL,R,(CNpolyptr)NULL,Treg,dataID);
 
      /* Remove everything in the pointlist */
      CNdelete_point_list(&pointhead, &pointtail);
   }

   /* Go thru the polygons next */
   for (Po=polyhead; Po!=NULL; Po=Po->next) {
      /* Search for point-intersections */
      (void) point_in_poly(Po,x,y,&pointhead,&pointtail,
                           logx, logy, logz, verbose);

      /* Find the region */
      TREG_FOUND = CN_FALSE;
      for (reg=regionhead; reg!=NULL; reg=reg->next) 
         if (reg->ID == Po->region) {
            TREG_FOUND = CN_TRUE;
            Treg       = reg;
         }
      if (!TREG_FOUND) Treg = NULL;

      /* Place the results in a probe-list */
      for (P=pointhead; P!=NULL; P=P->next) 
         (void) CNinsert_probe(probehead, probetail,
                               P->x,P->y,P->z,
                               (CNtriaptr)NULL,(CNrectptr)NULL,Po,Treg,dataID);

      /* Remove everything in the pointlist */
      CNdelete_point_list(&pointhead, &pointtail);
   }


   /* Now count the number of probe-points */
   new_probe_points = CNcount_probes(*probehead, *probetail) - probe_count;
   
   if (verbose) 
      (void) fprintf(stdout,"Found %d probe-points\n",new_probe_points);

   /* Return */
   return(new_probe_points);
}


/*
 * Search thru the grid for an intersection with a line ax+by+d=0 (const z)
 */
/*ARGSUSED*/
static int point_in_grid(x,y,
                         grid,
                         probehead, probetail, 
                         logx, logy, logz,
                         dataID, verbose)
double       x, y;                    /* Probe coordinates        */
CNgrid4Dptr  grid;                    /* A grid array structure   */
CNprobeptr   *probehead, *probetail;  /* Results                  */
short        logx, logy, logz;        /* Log/Linear interpolation */
int          dataID;
int          verbose;
{
   CNrectptr   recthead=NULL, recttail=NULL, R;
   CNnodeptr   nodehead=NULL, nodetail=NULL;
   CNpointptr  pointhead=NULL, pointtail=NULL, P;
   CNpointptr  new_pointhead=NULL, new_pointtail=NULL;
   CNpointptr  pt00, pt01, pt10, pt11;
   CNnodeptr   nd00, nd01, nd10, nd11;
   double      xa, xb, xc, x1, x2, ya, yb, yc, y1, y2, z;
   int         probe_count, new_probe_points;
   int         nx, ny, i, j;

   /* Count the number of items in the probelist */
   probe_count = CNcount_probes(*probehead, *probetail);

   /* Go thru the grid-array and construct rectangles that contain the point */
   nx = grid->nx;
   ny = grid->ny;
   for (i=0; i<nx; i++) {
      /* x-coordinates of rectangle */
      xb    = CNget_1D_double_array_value(grid->xarray,i  ,nx);
      if (i>0)
         xa = CNget_1D_double_array_value(grid->xarray,i-1,nx);
      else
         xa = xb;
      if (i<nx-1)
         xc = CNget_1D_double_array_value(grid->xarray,i+1,nx);
      else
         xc = xb;

      /* Corners */
      x1 = 0.5*(xa + xb);
      x2 = 0.5*(xb + xc);

      /* Filter */
      if ((x > LARGER_OF(x1,x2)) || (x < SMALLER_OF(x1,x2))) continue;

      /* Go thru the ygrid */
      for (j=0; j<ny; j++) {
         /* Find the corners of the rectangle (centered about [i,j]) */
 
         /* y-coordinates of rectangle */
         yb    = CNget_1D_double_array_value(grid->yarray,j  ,ny);
         if (j>0)
            ya = CNget_1D_double_array_value(grid->yarray,j-1,ny);
         else
            ya = yb;
         if (j<ny-1)
            yc = CNget_1D_double_array_value(grid->yarray,j+1,ny);
         else
            yc = yb;
 
         /* corners */
         y1 = 0.5*(ya + yb);
         y2 = 0.5*(yb + yc);

         /* Filter */
         if ((y > LARGER_OF(y1,y2)) || (y < SMALLER_OF(y1,y2))) continue;

         /* z-value */
         z = CNget_1D_double_array_value(grid->zarray, (i+j*nx),nx*ny);

         /* Create the rectangle */
         pt00 = CNinsert_point(&pointhead,&pointtail,x1,y1,z,0);
         nd00 = CNinsert_tailnode(&nodehead,&nodetail,pt00,z,0);
         pt01 = CNinsert_point(&pointhead,&pointtail,x2,y1,z,0);
         nd01 = CNinsert_tailnode(&nodehead,&nodetail,pt01,z,0);
         pt11 = CNinsert_point(&pointhead,&pointtail,x2,y2,z,0);
         nd11 = CNinsert_tailnode(&nodehead,&nodetail,pt11,z,0);
         pt10 = CNinsert_point(&pointhead,&pointtail,x1,y2,z,0);
         nd10 = CNinsert_tailnode(&nodehead,&nodetail,pt10,z,0);
         (void) CNinsert_rect(&recthead,&recttail, nd00,nd01,nd11,nd10,0);
      }
   }

   /* Go thru the rectangles next */
   for (R=recthead; R!=NULL; R=R->next) {
      /* Search for point-intersections */
      (void) point_in_rect(R,x,y,&new_pointhead,&new_pointtail,
                           logx, logy, logz, verbose);

      /* Place the results in a probe-list */
      for (P=new_pointhead; P!=NULL; P=P->next) 
         (void) CNinsert_probe(probehead, probetail,
                               P->x,P->y,P->z,
                               (CNtriaptr)NULL,R,(CNpolyptr)NULL,
                               (CNregionptr)NULL, dataID);
 
      /* Remove everything in the pointlist */
      CNdelete_point_list(&new_pointhead, &new_pointtail);
   }

   /* Remove all the rects nodes and points */
   CNdelete_point_list(&pointhead, &pointtail);
   CNdelete_node_list(&nodehead, &nodetail);
   CNdelete_rect_list(&recthead, &recttail);

   /* Now count the number of probe-points */
   new_probe_points = CNcount_probes(*probehead, *probetail) - probe_count;
   
   if (verbose) 
      (void) fprintf(stdout,"Found %d probe-points\n",new_probe_points);

   /* Return */
   return(new_probe_points);
}

/*
 * Find the z-value of a point inside a triangle
 * Store the result in a pointlist, for compatibilty with the
 * point-rectangle routine.
 */
/*ARGSUSED*/
static int point_in_tria(T,x,y,pointhead,pointtail,
                         logx, logy, logz, verbose)
CNtriaptr  T;
double     x, y;                    /* The x-y coordinates      */
CNpointptr *pointhead, *pointtail;  /* Point list               */
short      logx, logy, logz;        /* Log/Linear interpolation */
int        verbose;
{
   double     xmax, xmin, ymax, ymin, dx, t, z, px, py;
   CNpoint    pt1, pt2, pt3, ptmin, ptmax;
   int        signz;
   int        xlogmode, ylogmode, zlogmode;
   int        intsct;

   /* Check the triangle */
   if (T==NULL) return(0);

   /* Check the boundaries of the triangle first */
   CNget_tria_xmaxmin(T,&xmin,&xmax);
   CNget_tria_ymaxmin(T,&ymin,&ymax);
   if (x < xmin || x > xmax) return(0);
   if (y < ymin || y > ymax) return(0);

   /* Copy the point coordinates */
   pt1 = *(T->n1->coord);   pt1.z = T->n1->t;
   pt2 = *(T->n2->coord);   pt2.z = T->n2->t;
   pt3 = *(T->n3->coord);   pt3.z = T->n3->t;

   /*
    * Convert the numbers to log if necessary then interpolate
    */
   px = x;
   py = y;
   if (logx) {
      xlogmode = CNlogmode3(pt1.x, pt2.x, pt3.x);
      pt1.x = CNlogabs(pt1.x, xlogmode);
      pt2.x = CNlogabs(pt2.x, xlogmode);
      pt3.x = CNlogabs(pt3.x, xlogmode);
      px    = CNlogabs(x    , xlogmode);
   }
   if (logy) {
      ylogmode = CNlogmode3(pt1.y, pt2.y, pt3.y);
      pt1.y = CNlogabs(pt1.y, ylogmode);
      pt2.y = CNlogabs(pt2.y, ylogmode);
      pt3.y = CNlogabs(pt3.y, ylogmode);
      py    = CNlogabs(y    , ylogmode);
   }
   if (logz) {
      signz    = CNsign(pt1.z);
      zlogmode = CNlogmode3(pt1.z, pt2.z, pt3.z);
      pt1.z = CNlogabs(pt1.z, zlogmode);
      pt2.z = CNlogabs(pt2.z, zlogmode);
      pt3.z = CNlogabs(pt3.z, zlogmode);
   }

   /* Now check for an intersection with the segments */
   intsct = CNpoly3_intsct_plane(0.0,1.0,0.0,-py,
                                 &pt1,&pt2,&pt3,&ptmin,&ptmax,
                                 verbose);

   /* This should give me a line parallel to the x-axis */
   if (intsct != 1) return(0);

   /* Reorder ptmin and ptmax in x */
   if (ptmin.x > ptmax.x) {
      pt1   = ptmax;
      ptmax = ptmin;
      ptmin = pt1;
   }

   /* if the point is outside the x-limits of the intersection, return */
   if (px < ptmin.x || x > ptmax.x) return(0);

   /* Now get the intersection */
   dx = ptmax.x - ptmin.x;
   if (fabs(dx) < CN_SMALLER) t = 0.0;
   else                       t = (px - ptmin.x) / dx;
   z = ptmin.z + t*(ptmax.z - ptmin.z);

   /* Reconvert the results */
   if (logz) z = CNinvlogabs(z,signz,zlogmode);

   /* Put the coords inside a point-list */
   (void) CNinsert_point(pointhead, pointtail, x, y, z, 0);

   /* Return */
   return(1);
}

/*
 * Find the z-value of a point inside a rectangle
 * There could be more than 1 point, so store the results in a pointlist
 */
/*ARGSUSED*/
static int point_in_rect(R,x,y,pointhead,pointtail,
                         logx, logy, logz, verbose)
CNrectptr  R;
double     x, y;                    /* The x-y coordinates      */
CNpointptr *pointhead, *pointtail;  /* Point list               */
short      logx, logy, logz;        /* Log/Linear interpolation */
int        verbose;
{
   double     xmax, xmin, ymax, ymin, dx, t, z, px, py;
   CNpoint    pt1, pt2, pt3, pt4, newpt;
   CNpoint    points[10], ptmin, ptmax;
   CNpointptr P;
   int        signz;
   int        xlogmode, ylogmode, zlogmode;
   int        i, npts=0, intsct, FOUND=CN_FALSE, PTFOUND=CN_FALSE;

   /* Check the rectangle */
   if (R==NULL) return(0);

   /* Check the boundaries of the rectangle first */
   CNget_rect_xmaxmin(R,&xmin,&xmax);
   CNget_rect_ymaxmin(R,&ymin,&ymax);
   if (x < xmin || x > xmax) return(0);
   if (y < ymin || y > ymax) return(0);

   /* Copy the point coordinates */
   pt1 = *(R->n1->coord);   pt1.z = R->n1->t;
   pt2 = *(R->n2->coord);   pt2.z = R->n2->t;
   pt3 = *(R->n3->coord);   pt3.z = R->n3->t;
   pt4 = *(R->n4->coord);   pt4.z = R->n4->t;

   /*
    * Convert the numbers to log if necessary then interpolate
    */
   px = x;
   py = y;
   if (logx) {
      xlogmode = CNlogmode4(pt1.x, pt2.x, pt3.x, pt4.x);
      pt1.x = CNlogabs(pt1.x, xlogmode);
      pt2.x = CNlogabs(pt2.x, xlogmode);
      pt3.x = CNlogabs(pt3.x, xlogmode);
      pt4.x = CNlogabs(pt4.x, xlogmode);
      px    = CNlogabs(x    , xlogmode);
   }
   if (logy) {
      ylogmode = CNlogmode4(pt1.y, pt2.y, pt3.y, pt4.y);
      pt1.y = CNlogabs(pt1.y, ylogmode);
      pt2.y = CNlogabs(pt2.y, ylogmode);
      pt3.y = CNlogabs(pt3.y, ylogmode);
      pt4.y = CNlogabs(pt4.y, ylogmode);
      py    = CNlogabs(y    , ylogmode);
   }
   if (logz) {
      signz    = CNsign(pt1.z);
      zlogmode = CNlogmode4(pt1.z, pt2.z, pt3.z, pt4.z);
      pt1.z = CNlogabs(pt1.z, zlogmode);
      pt2.z = CNlogabs(pt2.z, zlogmode);
      pt3.z = CNlogabs(pt3.z, zlogmode);
      pt4.z = CNlogabs(pt4.z, zlogmode);
   }

   /* Now check for an intersection with the segments */
   intsct = CNpoly4_intsct_plane(0.0,1.0,0.0,-py,
                                 &pt1,&pt2,&pt3,&pt4,
                                 points, &npts, 
                                 verbose);

   /* This should give me lines parallel to the x-axis */
   if (intsct) {
      for (i=0; i<npts; i=i+2) {
         /* Skip if the line is too short */
         if (!CNlongline(&(points[i]),&(points[i+1]),1.0e-5)) continue;

         /* skip if the point is outside the x-limits of the intersection */
         if (points[i].x > points[i+1].x) {
            ptmin = points[i+1];
            ptmax = points[i];
         } else {
            ptmin = points[i];
            ptmax = points[i+1];
         }
         if (px < ptmin.x || px > ptmax.x) continue; 

         /* Now get the intersection */
         dx = ptmax.x - ptmin.x;
         if (fabs(dx) < CN_SMALLER) t = 0.0;
         else                       t = (px - ptmin.x) / dx;
         z = ptmin.z + t*(ptmax.z - ptmin.z);

         /* Reconvert the results */
         if (logz) z = CNinvlogabs(z,signz,zlogmode);

         /* Put the coords inside a point-structure for comparison */
         newpt.x = x;
         newpt.y = y;
         newpt.z = z;
         
         /* Find out if the point is already in the list */
         PTFOUND = CN_FALSE;
         for (P=(*pointhead); P!=NULL && !PTFOUND; P=P->next) 
             if (!CNlongline(P,&newpt,1.0e-5)) PTFOUND = CN_TRUE; 

         /* Put the coords inside a point-list */
         if (!PTFOUND) {
            (void) CNinsert_point(pointhead, pointtail, x, y, z, 0);
            FOUND = CN_TRUE;
         }
      }
   }
   return(FOUND);
}

/*
 * Find the z-value of a point inside a polygon 
 * There could be more than 1 point, so store the results in a pointlist
 */
/*ARGSUSED*/
static int point_in_poly(P,x,y,pointhead,pointtail, 
                         logx, logy, logz, verbose)
CNpolyptr  P;
double     x, y;                    /* The x-y coordinates      */
CNpointptr *pointhead, *pointtail;  /* Point list               */
int        verbose;
short      logx, logy, logz;        /* Log/Linear interpolation */
{
   double     xmax, xmin, ymax, ymin;
   CNrectptr  recthead=NULL, recttail=NULL, R;
   CNtriaptr  triahead=NULL, triatail=NULL, T;
   int        npts, FOUND=CN_FALSE;

   /* Check the polygon */
   if (P==NULL) return(0);

   /* Check the boundaries of the polygon first */
   CNget_poly_xmaxmin(P,&xmin,&xmax);
   CNget_poly_ymaxmin(P,&ymin,&ymax);
   if (x < xmin || x > xmax) return(0);
   if (y < ymin || y > ymax) return(0);

   /* Reuse, reuse...  Put the polygon in a triangle or rectangle */
   npts = CNcount_nlists(P->nlisthead, P->nlisttail);
   if ((npts > 1) && (P->nlisthead->N == P->nlisttail->N)) npts--;
   if (npts == 3) {
      /* Put this polygon in a triangle */
      T = CNinsert_tria(&triahead, &triatail,
                        P->nlisthead->N,
                        P->nlisthead->next->N,
                        P->nlisthead->next->next->N,
                        P->region, P->ID);

      /* Intersection check */
      FOUND = point_in_tria(T,x,y,pointhead,pointtail,
                            logx, logy, logz, verbose);

      /* Delete the triangle list */
      CNdelete_tria_list(&triahead, &triatail);

   } else if (npts == 4) {
      /* Put this polygon into a rectangle */
      R = CNinsert_rect(&recthead, &recttail,
                        P->nlisthead->N,
                        P->nlisthead->next->N,
                        P->nlisthead->next->next->N,
                        P->nlisthead->next->next->next->N,
                        P->ID);

      /* Intersection check */
      FOUND = point_in_rect(R,x,y,pointhead,pointtail,
                            logx, logy, logz, verbose);

      /* Delete the rectangle list */
      CNdelete_rect_list(&recthead, &recttail);

   } else {
      FOUND = CN_FALSE;
   }
   return(FOUND);
}


/*
 * Linked list procedures
 */

/*
 * Allocate room for a probe
 */
static CNprobeptr make_probe(x,y,z,T,R,P,reg,ID)
double      x,y,z;
CNtriaptr   T;
CNrectptr   R;
CNpolyptr   P;
CNregionptr reg;
int         ID;
{
   CNprobeptr newptr;
   unsigned int size = sizeof(CNprobe);
 
   if ((newptr = (CNprobeptr)malloc(size))!=NULL) {
      newptr->ID   = ID;
      newptr->x    = x;
      newptr->y    = y;
      newptr->z    = z;
      newptr->T    = T;
      newptr->R    = R;
      newptr->P    = P;
      newptr->reg  = reg;
      newptr->next = NULL;
      newptr->prev = NULL;
   }
   return(newptr);
}

/*
 * Insert a probe at the tail of the current probe list
 */
CNprobeptr CNinsert_probe(probe_listhead, probe_listtail, 
                          x, y, z, T, R, P, reg, ID)
CNprobeptr *probe_listhead, *probe_listtail;
double      x, y, z;
CNtriaptr   T; 
CNrectptr   R; 
CNpolyptr   P;
CNregionptr reg;
int         ID;
{
   CNprobeptr next,A,B;
 
   A = *probe_listtail;
   if ((B=make_probe(x,y,z,T,R,P,reg,ID))!=NULL) {
      if (A==NULL) {
         *probe_listhead = B;
         *probe_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next != NULL) next->prev = B;
         if (B->next == NULL) *probe_listtail = B;
      }
   }
   return(B);
}


/*
 * Delete probe at address L
 */
void CNdelete_probe(probe_listhead, probe_listtail, L)
CNprobeptr *probe_listhead, *probe_listtail;
CNprobeptr L;
{
   CNprobeptr prev,next;
 
   prev = L->prev;
   next = L->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (L== *probe_listhead) *probe_listhead = next;
   if (L== *probe_listtail) *probe_listtail = prev;
   free ((char*)L);
   L = NULL;
}
 
 
/*
 * Delete all the probes in the list
 */
void CNdelete_probe_list(probe_listhead, probe_listtail)
CNprobeptr *probe_listhead, *probe_listtail;
{
   CNprobeptr P;
   while ((P = *probe_listhead) != NULL)
      CNdelete_probe(probe_listhead, probe_listtail, P);
}
 
 
/*
 * Print out the list of probes
 */
/*ARGSUSED*/
void CNprint_probe_list(probe_listhead, probe_listtail)
CNprobeptr probe_listhead, probe_listtail;
{
   CNprobeptr P;
 
   (void) fprintf(stdout,"   Found %d intersections :\n",
           CNcount_probes(probe_listhead, probe_listtail));

   for (P=probe_listhead; P!=NULL; P=P->next)
      print_probe(P);
}
 
 
/*
 * print the coordinates of a probe
 */
static void print_probe(pt)
CNprobeptr pt;
{
   (void) fprintf(stdout,"Probe Coordinates : (%g %g %g)",pt->x,pt->y,pt->z);
   if (pt->T != NULL)
   (void) fprintf(stdout,"   Tria ID=%d  Reg ID=%d",pt->T->ID,pt->T->region);
   if (pt->T != NULL && pt->reg != NULL)
   (void) fprintf(stdout,"   Mat Name=\"%s\"",pt->reg->matname);
   if (pt->R != NULL)
   (void) fprintf(stdout,"   Rect ID=%d",pt->R->ID);
   (void) fprintf(stdout,"\n");
}
 
 
/*
 * Count the number of probes in the list
 */
/*ARGSUSED*/
int CNcount_probes(probe_listhead, probe_listtail)
CNprobeptr probe_listhead, probe_listtail;
{
   CNprobeptr P;
   int        count = 0;
 
   for (P=probe_listhead; P!=NULL; P=P->next) count++;
 
   return(count);
}


/*
 * MISC PROBE ROUTINES
 */

/*
 * Determine the average Z-value of a dataset within a specified window
 */
int CNprobe_dataset_zave(Dptr, xmin, xmax, ymin, ymax, zave,
                         npoints)
CNdatasetptr Dptr;                    /* Dataset to be probed */
double       xmin, xmax;              /* x-boundary           */
double       ymin, ymax;              /* y-boundary           */
double       *zave;                   /* average z-value      */
int          *npoints;                /* No of points counted */
{
   double     ave  = 0.0;
   int        npts = 0;
   int        i, j, k;
   double     x, y, z;  
   int        FOUND;
   CNnodeptr  N;

   /* Error check */
   if (Dptr == NULL) {
      (void) fprintf(stderr,"Error! Cannot analyze NULL dataset!\n");
      return (CN_FALSE);
   }

   /* Initialize */
   *zave    = 0.0;
   *npoints = 0;
   FOUND    = CN_FALSE;

   /* Branch according to dataset-type */
   if (Dptr->datatype == CN_CONTOUR) {
      /* This will work for CONTOUR datasets */

      if (Dptr->nodehead != NULL) {

         /* Go thru all the nodes */
         FOUND = CN_TRUE;
         for (N=Dptr->nodehead; N!=NULL; N=N->next) {
            if ((N->coord->x >= xmin && N->coord->x <= xmax) && 
                (N->coord->y >= ymin && N->coord->y <= ymax)) { 
               ave += N->t;
               npts++;
            }
         }
     
         /* Ave z-value */
         if (npts > 0)
            *zave = ave/(double)npts;
         *npoints = npts;

      } else if (Dptr->grid && (Dptr->datatype == CN_CONTOUR)) {

         /* 
          * Sometimes the CONTOUR dataset just contains a grid and
          * no actual points. This often happens when the grid is too
          * large to store as rectangles/triangles
          */

         /* Go thru the grid */
         FOUND = CN_TRUE;
         for (i=0; i<Dptr->grid->nx; i++) {
            for (j=0; j<Dptr->grid->ny; j++) {
               x = CNget_1D_double_array_value(Dptr->grid->xarray,i  ,
                                               Dptr->grid->nx);
               y = CNget_1D_double_array_value(Dptr->grid->yarray,j  , 
                                               Dptr->grid->ny);
               if ((x >= xmin && x <= xmax) && 
                   (y >= ymin && y <= ymax)) { 
                  k = i+j*Dptr->grid->nx;
                  z = CNget_1D_double_array_value(
                                               Dptr->grid->zarray,k,
                                               Dptr->grid->nx*Dptr->grid->ny);
                  ave += z;
                  npts++;
               }
            }
         }

         /* Ave z-value */
         if (npts > 0)
            *zave = ave/(double)npts;
         *npoints = npts;

      }
   }  

   /* Return */
   return (FOUND);
}
