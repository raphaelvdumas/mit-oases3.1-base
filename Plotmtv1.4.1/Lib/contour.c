/*
 * contour.c - manipulate the contour data
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "CNplot.h"

void CNslice_contours();
void CNfind_contour();
void CNsort_lines();
static void insert_tailpoint();
static void add_to_tailplot();
static void insert_headpoint();
static void add_to_headplot();
static CNlineptr matching_line();
static void find_rect_intsct();
static void find_tria_intsct();
static int plane_intsct_line();


/****************************************************************/
/**********                                            **********/
/**********         General contour subroutines        **********/
/**********                                            **********/
/****************************************************************/


/*
 * Select the contour step size and 
 * cut up the triangles contained in Dptr along the z-planes
 */
void CNslice_contours(Dptr,verbose)
CNdatasetptr Dptr;
int          verbose;
{
   CNcontstepptr C;
   int i;

   /*
    * Get rid of any pre-existing contour curves first 
    */
   CNdelete_curve_list(&(Dptr->curvehead), &(Dptr->curvetail));

   /*
    * Delete the prexisting cstep list 
    */
   if (Dptr->data_pr.stepmethod != CN_USERDEFN) 
   CNdelete_contstep_list(&(Dptr->cstephead), &(Dptr->csteptail));

   if (verbose) {
   (void) fprintf(stdout,"\n   Taking multiple slices of the dataset\n");
   (void) fprintf(stdout,"      Dataset Information:\n");
   (void) fprintf(stdout,"      DATA ID     = %d\n",Dptr->ID);
   (void) fprintf(stdout,"      Origin      = %s\n",Dptr->filename);
   (void) fprintf(stdout,"      Step Method = %d\n",Dptr->data_pr.stepmethod);
   (void) fprintf(stdout,"      LogZ Interp = %s\n",
                  BOOLEAN_VALUE(Dptr->data_pr.logz));
   }

   /*
    * Select the contour step size
    * This is done ONLY if the steps have NOT been specified individually
    */
   if (Dptr->data_pr.stepmethod != CN_USERDEFN) 
   CNselect_contour_step(&(Dptr->data_pr.cmin), 
                         &(Dptr->data_pr.cmax),
                         Dptr->data_pr.stepmethod,
                         &(Dptr->data_pr.cstep),
                         &(Dptr->data_pr.nsteps),
                         Dptr->data_pr.logzstep,
                         &(Dptr->cstephead), &(Dptr->csteptail), verbose);

   /* Error check */
   if (Dptr->cstephead == NULL)
   (void) fprintf(stderr,"Warning! No contour curves found!\n");

   /*
    * now find the contours 
    */
   i = 0;
   for (C=Dptr->cstephead; C!=NULL; C=C->next) {
      if ((C->value > Dptr->bzmin) && (C->value < Dptr->bzmax))
         CNfind_contour(Dptr, C->value, i, &(C->curv_pr));
      i++;
   }
}


/* 
 * find all the line-segments that are formed when triangle intersects z=level 
 */
void CNfind_contour(Dptr,level,lineID,curv_pr)
CNdatasetptr     Dptr;
double           level;
int              lineID;
CNcurve_property *curv_pr;
{
   CNlineptr  line_listhead=NULL, line_listtail=NULL;
   CNtriaptr  T;
   CNrectptr  R;
   char       label[CN_MAXCHAR];
   int        linetyp;
   double     delta;

   /*
    * Delta is the smallest dimension of the data
    */
   delta = MINOF3(fabs(Dptr->bxmax - Dptr->bxmin), 
                  fabs(Dptr->bymax - Dptr->bymin),
                  fabs(Dptr->data_pr.cmax - Dptr->data_pr.cmin));

   /* 
    * find all the line-segments of the rectangles that intersect a plane
    */
   if (Dptr->recthead != NULL) {
      for (R=Dptr->recthead; R!=NULL; R=R->next)
         find_rect_intsct(level,R,&line_listhead,&line_listtail,delta,
                          Dptr->data_pr.logx,
                          Dptr->data_pr.logy,
                          Dptr->data_pr.logz);
   }

   /* 
    * find all the line-segments of the triangles that intersect a plane
    */
   if (Dptr->triahead != NULL) {
      for (T=Dptr->triahead; T!=NULL; T=T->next)
         find_tria_intsct(level,T,&line_listhead,&line_listtail,delta,
                          Dptr->data_pr.logx,
                          Dptr->data_pr.logy,
                          Dptr->data_pr.logz);
   }
   
   /* 
    * The linetype is determined by the order of each slice 
    * The default is 2 linetypes (major and minor)
    */
   if (Dptr->data_pr.linetypes <=0 || Dptr->data_pr.linetypes > 3) 
      Dptr->data_pr.linetypes = 2;
   if (Dptr->data_pr.linetypes <= 2) 
      linetyp = lineID % Dptr->data_pr.linetypes;
   else if (Dptr->data_pr.linetypes == 3) {
      linetyp = lineID % 4;
      if (linetyp == 3) linetyp = 1;
   }
   switch (linetyp) {
   case 0 : linetyp = CN_LN_SOLID;    break;
   case 1 : linetyp = CN_LN_DOTTED;   break;
   case 2 : linetyp = CN_LN_DASHED;   break;
   case 3 : linetyp = CN_LN_DOTDASH;  break;
   default: linetyp = CN_LN_SOLID;    break;
   }

   /*
    * now sort out the list of line-segments 
    * pass the boundary of the data so that the line-segments can be
    * joined on a relative basis rather than an absolute one.
    */
   (void) sprintf(label,"%.3g",level);
   CNsort_lines(&line_listhead, &line_listtail,
                &(Dptr->curvehead), &(Dptr->curvetail),
                delta, label, linetyp, lineID, curv_pr);
}

/*
 * sort out the list of lines to form a joined curve
 * Delta is the smallest dimension of the data, provided for 
 * comparison.
 */
void CNsort_lines(linehead, linetail, curvehead, curvetail,
                  delta, label, linetyp, lineID, curv_pr)
CNlineptr        *linehead, *linetail;
CNcurveptr       *curvehead, *curvetail;
double           delta;
char             *label;
int              linetyp,lineID;
CNcurve_property *curv_pr;
{
   CNlineptr  L;
   CNcurveptr C;
   CNpoint    PT1,PT2;

   while ((L = *linehead)!=NULL) {
      /* Insert the curve */
      C = CNinsert_curve(curvehead, curvetail,lineID);

      /* Apply properties to the curve */
      CNdestroy_string(C->curv_pr.linelabel);
      C->curv_pr.linelabel = CNcreate_string(label);
      C->curv_pr.linetype  = linetyp;
      C->curv_pr.linecolor = linetyp;
      if ((curv_pr != NULL) && (curv_pr->flag != 0))
         CNset_curve_property(&(C->curv_pr), curv_pr);

      /* Put in points */
      PT1 = L->pt1;
      PT2 = L->pt2;
      insert_tailpoint(&(C->pointhead), &(C->pointtail), &PT1, 0);
      insert_tailpoint(&(C->pointhead), &(C->pointtail), &PT2, 0);
      CNdelete_line(linehead, linetail, L);
      add_to_tailplot(linehead, linetail, &PT2, C, delta);
      add_to_headplot(linehead, linetail, &PT1, C, delta);
   }
}


/* 
 * add to the tail of the plot list 
 */
static void
add_to_tailplot(line_listhead, line_listtail, pt, C, delta)
CNlineptr *line_listhead, *line_listtail;
CNpoint   *pt;
CNcurveptr C;
double     delta;
{
   CNlineptr L;

   /* add the further point at every loop iteration */
   while ((L=matching_line(line_listhead,pt,delta))!=NULL) {
      if (CNlongline(&(L->pt1),pt,delta) ) {
         insert_tailpoint(&(C->pointhead), &(C->pointtail), &(L->pt1), 0);
         *pt = L->pt1;
      } else {
         insert_tailpoint(&(C->pointhead), &(C->pointtail), &(L->pt2), 0);
         *pt = L->pt2;
      }
      CNdelete_line(line_listhead, line_listtail, L);
   }
   return;
}

/* 
 * add a point to the tail of the pointlist
 */
static void insert_tailpoint(pointhead, pointtail, pt, ID)
CNpointptr *pointhead, *pointtail;
CNpoint    *pt;
int ID;
{
   (void)CNinsert_tailpoint(pointhead,pointtail,pt->x,pt->y,pt->z,ID);
}

/* add to the head of the plot list */
static void
add_to_headplot(line_listhead, line_listtail, pt, C, delta)
CNlineptr *line_listhead, *line_listtail;
CNpoint   *pt;
CNcurveptr C;
double     delta;
{
   CNlineptr L;

   /* add the further point at every loop iteration */
   while ((L=matching_line(line_listhead,pt,delta))!=NULL) {
      if (CNlongline(&(L->pt1),pt,delta) ) {
         insert_headpoint(&(C->pointhead), &(C->pointtail), &(L->pt1), 0);
         *pt = L->pt1;
      } else {
         insert_headpoint(&(C->pointhead), &(C->pointtail), &(L->pt2), 0);
         *pt = L->pt2;
      }
      CNdelete_line(line_listhead, line_listtail, L);
   }
   return;
}

/* 
 * add a point to the head of the pointlist
 */
static void insert_headpoint(pointhead, pointtail, pt, ID)
CNpointptr *pointhead, *pointtail;
CNpoint    *pt;
int ID;
{
   (void)CNinsert_headpoint(pointhead,pointtail,pt->x,pt->y,pt->z,ID);
}

/* Return line containing data point that matches a given point */
static CNlineptr matching_line(line_listhead,pt,delta)
CNlineptr *line_listhead;
CNpoint   *pt;
double    delta;
{
   int       CNlongline();
   CNlineptr L,LF;
   int       FOUND = CN_FALSE;

   /*
    * Given a point and a line, find out if the point coincides with
    * the points on either end of the line. To do this, do a quick check in
    * x - if the x-coordinates are relatively close, then do a more
    * careful check using CNlongline
    */

   /* check lengths of lines */
   for (L=(*line_listhead); L!=NULL && !FOUND; L=L->next) {
      /* check the first point */
      if (fabs(L->pt1.x - pt->x) < CN_SMALL) 
         FOUND = !(CNlongline(&(L->pt1),pt,delta));
      if (!FOUND) {
         /* now check the second point */
         if (fabs(L->pt2.x - pt->x) < CN_SMALL) 
            FOUND = !(CNlongline(&(L->pt2),pt,delta));
      }
      if (FOUND) LF = L;
   }
   
   /* return the line */
   if (!FOUND) LF = NULL;
   return(LF);
}

/* 
 * Find the line of intersection between a rectangle and a plane 
 * This routine is a simplfication of CNpoly4_intsct_plane()
 * and returns the segments of intersection with the z-plane.
 */
static void find_rect_intsct(z,R,line_listhead,line_listtail,delta,
                             logx, logy, logz)
double    z;
CNrectptr R;
CNlineptr *line_listhead, *line_listtail;
double    delta;
short     logx, logy, logz;     /* Log/Linear interpolation flags */
{
   CNpoint  point[10];
   CNpoint  pt1, pt2, pt3, pt4, pta;
   int      lg01, lg12, lg20;
   int      i;
   
   /* If the nocont flag is set, skip - this is for mesh-based datasets */
   if (R->nocont) return;

   /*
    * Check the points to see if they are all above or below the cut-plane
    * Also if all the points have the same z-value => no intersection
    */
   if ( (R->n1->t > z && R->n2->t > z && R->n3->t > z && R->n4->t > z) ||
        (R->n1->t < z && R->n2->t < z && R->n3->t < z && R->n4->t < z) ||
        ((fabs(R->n1->t - R->n2->t) < CN_SMALLER) &&
         (fabs(R->n2->t - R->n3->t) < CN_SMALLER) &&
         (fabs(R->n3->t - R->n4->t) < CN_SMALLER) &&
         (fabs(R->n4->t - R->n1->t) < CN_SMALLER)) )
      return;

   /*
    * Copy the coordinate data to new points.
    * The true z-value is stored inside n->t;
    * replace the point's z-value with this
    */
   pt1 = *(R->n1->coord);        pt1.z = R->n1->t;
   pt2 = *(R->n2->coord);        pt2.z = R->n2->t;
   pt3 = *(R->n3->coord);        pt3.z = R->n3->t;
   pt4 = *(R->n4->coord);        pt4.z = R->n4->t;

   /*
    * now go thru each segment and find intersections
    */
   i=0;
   if (plane_intsct_line(z,&pt1,&pt2,&pta,logx,logy,logz)) point[i++]=pta;
   if (plane_intsct_line(z,&pt2,&pt3,&pta,logx,logy,logz)) point[i++]=pta;
   if (plane_intsct_line(z,&pt3,&pt4,&pta,logx,logy,logz)) point[i++]=pta;
   if (plane_intsct_line(z,&pt4,&pt1,&pta,logx,logy,logz)) point[i++]=pta;

   /*
    * There can be 0 to 4 intersections
    */
   if (i==4) {
      /*
       * Four intersections - match up pairs based on whether the
       * vertice in between 2 intersections is high or low.
       * pt1 is the vertice between point[0] and point[3]
       *
       * There is a possibility that some points are the same due to
       * floating-point error - handle these cases too
       */
      if (pt1.z > z) {
         /* Pairs are (p0,p1) (p2,p3) */
         if (CNlongline(&(point[0]),&(point[1]),delta) && 
             CNlongline(&(point[2]),&(point[3]),delta)) {
            CNinsert_line(line_listhead,line_listtail,&(point[0]),&(point[1]));
            CNinsert_line(line_listhead,line_listtail,&(point[2]),&(point[3]));
         } else {
            CNinsert_line(line_listhead,line_listtail,&(point[0]),&(point[2]));
         }
      } else {
         /* Pairs are (p0,p3) (p1,p2) */
         if (CNlongline(&(point[0]),&(point[3]),delta) && 
             CNlongline(&(point[1]),&(point[2]),delta)) {
            CNinsert_line(line_listhead,line_listtail,&(point[0]),&(point[3]));
            CNinsert_line(line_listhead,line_listtail,&(point[1]),&(point[2]));
         } else {
            CNinsert_line(line_listhead,line_listtail,&(point[0]),&(point[1]));
         }
      }

   } else if (i==3) {
      /*
       * Three intersections - check the distance between intersections
       * to screen out floating-point-error.
       * Possible that 3 points are on the plane...
       */
      lg01 = CNlongline(&(point[0]),&(point[1]),delta);
      lg12 = CNlongline(&(point[1]),&(point[2]),delta);
      lg20 = CNlongline(&(point[2]),&(point[0]),delta);

      /* If there are 3 long-lines add all 3 to the list */
      if (lg01 && lg12 && lg20) {
         if (lg01) {
            CNinsert_line(line_listhead,line_listtail,&(point[0]),&(point[1]));
         }
         if (lg12) {
            CNinsert_line(line_listhead,line_listtail,&(point[1]),&(point[2]));
         }
         if (lg20) {
            CNinsert_line(line_listhead,line_listtail,&(point[2]),&(point[0]));
         }
      } else if (!lg01) {
         /* pt0 = pt1, ln12 = ln20 */
         if (lg12) {
            CNinsert_line(line_listhead,line_listtail,&(point[1]),&(point[2]));
         }
      } else if (!lg12) {
         /* pt1 = pt2, ln01 = ln20 */
         if (lg01) {
            CNinsert_line(line_listhead,line_listtail,&(point[0]),&(point[1]));
         }
      } else if (!lg20) {
         /* pt0 = pt2, ln01 = ln21 */
         if (lg01) {
            CNinsert_line(line_listhead,line_listtail,&(point[0]),&(point[1]));
         }
      }

   } else if (i==2) {
 
      /*
       * Two intersections - just return 2
       */
      if (CNlongline(&(point[0]),&(point[1]),delta))
         CNinsert_line(line_listhead,line_listtail,&(point[0]),&(point[1]));

   } else if (i!=0) {
 
      /*
       * One intersection - must be floating point error where
       * the plane is at the tip of the triangle
       */
 
      (void) fprintf(stderr,
                     "Warning: Nonstandard No of intersections = %5d\n",i);
 
   /*EMPTY*/
   } else if (i==0) {
      /*
       * No intersections
       */
   }
}

/* 
 * Find the line of intersection between a triangle and a plane 
 * This routine is a simplfication of CNpoly3_intsct_plane()
 * and returns the segments of intersection with the z-plane
 */
static void find_tria_intsct(z,T,line_listhead,line_listtail,
                             delta, logx, logy, logz)
double    z;
CNtriaptr T;
CNlineptr *line_listhead, *line_listtail;
double    delta;
short     logx, logy, logz;     /* Log/Linear interpolation flags */
{
   CNpoint   intpts[3],pta,pt1,pt2,pt3;
   int       i=0;

   /* If the nocont flag is set, skip - this is for mesh-based datasets */
   if (T->nocont) return;

   /*
    * Check the points to see if they are all above or below the cut-plane
    * Also if all the points have the same z-value => no intersection
    */
   if ( (T->n1->t > z && T->n2->t > z && T->n3->t > z) || 
        (T->n1->t < z && T->n2->t < z && T->n3->t < z) ||
        ((fabs(T->n1->t - T->n2->t) < CN_SMALLER) &&
         (fabs(T->n2->t - T->n3->t) < CN_SMALLER) &&
         (fabs(T->n3->t - T->n1->t) < CN_SMALLER)) )
      return;

   /*
    * Copy the coordinate data to new points.
    * The true z-value is stored inside n->t;
    * replace the point's z-value with this
    */
   pt1 = *(T->n1->coord);        pt1.z = T->n1->t;
   pt2 = *(T->n2->coord);        pt2.z = T->n2->t;
   pt3 = *(T->n3->coord);        pt3.z = T->n3->t;

   /* 
    * now go thru each segment and find intersections
    */
   if (plane_intsct_line(z,&pt1,&pt2,&pta,logx,logy,logz)) intpts[i++]=pta;
   if (plane_intsct_line(z,&pt2,&pt3,&pta,logx,logy,logz)) intpts[i++]=pta;
   if (plane_intsct_line(z,&pt3,&pt1,&pta,logx,logy,logz)) intpts[i++]=pta;

   /*
    * There can be 0 to 3 intersections
    */
   if (i==3) {
      /* 
       * Three intersections - two of these must be pretty close
       * Plane probably intersects tria at one of the points 
       */
      if (CNlongline(&(intpts[0]),&(intpts[1]),delta))
         CNinsert_line(line_listhead,line_listtail,&(intpts[0]),&(intpts[1]));
      else if (CNlongline(&(intpts[1]),&(intpts[2]),delta))
         CNinsert_line(line_listhead,line_listtail,&(intpts[1]),&(intpts[2]));
      else if (CNlongline(&(intpts[2]),&(intpts[0]),delta))
         CNinsert_line(line_listhead,line_listtail,&(intpts[2]),&(intpts[0]));
      else
         /* 
          * (void) fprintf(stderr,"Warning: Zero line length\n");
          */
         ;
   } else if (i==2) {

      /*
       * Two intersections - just return 2
       */
      if (CNlongline(&(intpts[0]),&(intpts[1]),delta))
         CNinsert_line(line_listhead,line_listtail,&(intpts[0]),&(intpts[1]));
 
   } else if (i!=0) {

      /*
       * One intersection - must be floating point error where
       * the plane is at the tip of the triangle
       */

      (void) fprintf(stderr,
                     "Warning: Nonstandard No of intersections = %5d\n",i);

   /*EMPTY*/
   } else if (i==0) {
      /*
       * No intersections
       */
   }
}


/* 
 * find out the intersection point between a line and a z-plane 
 */
static int plane_intsct_line(z,pt1,pt2,pt3,logx,logy,logz)
double   z;
CNpoint  *pt1,*pt2,*pt3;
short    logx, logy, logz;
{
   int    intsct = CN_TRUE;
   double t;
   double pt1x, pt1y, pt1z, pt2x, pt2y, pt2z, z0;
   int    xlogmode, ylogmode, zlogmode;

   if ((pt1->z > z && pt2->z > z) || 
       (pt1->z < z && pt2->z < z) ||
       (fabs(pt1->z - pt2->z) < CN_SMALLER) )
      intsct = CN_FALSE;

   if (intsct) {
      if (!logx && !logy && !logz) {
         /*
          * Use linear interpolation to find the intersection 
          */
         t = (z - pt1->z) /(pt2->z - pt1->z);
         if (t < 0.5) {
            pt3->x = pt1->x + t*(pt2->x - pt1->x);
            pt3->y = pt1->y + t*(pt2->y - pt1->y);
            pt3->z = z;
         } else {
            /*
             * This is to prevent problems when t is very small,
             * e.g. t=1e-20, so that 1-t=1.  In such cases, the order
             * of points makes a difference.  This if-else statement
             * circumvents the problem by switching the order of points.
             */
            t = (z - pt2->z) /(pt1->z - pt2->z);
            pt3->x = pt2->x + t*(pt1->x - pt2->x);
            pt3->y = pt2->y + t*(pt1->y - pt2->y);
            pt3->z = z;
         }

      } else {
         /* 
          * Convert the numbers to log then interpolate 
          */
         if (logx) {
            xlogmode = CNlogmode2(pt1->x, pt2->x);
            pt1x = CNlogabs(pt1->x, xlogmode);
            pt2x = CNlogabs(pt2->x, xlogmode);
         } else {
            pt1x = pt1->x;
            pt2x = pt2->x;
         }
         if (logy) {
            ylogmode = CNlogmode2(pt1->y, pt2->y);
            pt1y = CNlogabs(pt1->y, ylogmode);
            pt2y = CNlogabs(pt2->y, ylogmode);
         } else {
            pt1y = pt1->y;
            pt2y = pt2->y;
         }
         if (logz) {
            zlogmode = CNlogmode2(pt1->z, pt2->z);
            pt1z = CNlogabs(pt1->z, zlogmode);
            pt2z = CNlogabs(pt2->z, zlogmode);
            z0   = CNlogabs(z     , zlogmode);
         } else {
            pt1z = pt1->z;
            pt2z = pt2->z;
            z0   = z;
         }

         /* Use linear interpolation to find the intersection */
         t = (z0 - pt1z) /(pt2z - pt1z);
         pt3->x = pt1x + t*(pt2x - pt1x);
         pt3->y = pt1y + t*(pt2y - pt1y);
         pt3->z = z;

         /* Reconvert the numbers */
         if (logx) pt3->x = CNinvlogabs(pt3->x,CNsign(pt1->x),xlogmode);
         if (logy) pt3->y = CNinvlogabs(pt3->y,CNsign(pt1->y),ylogmode);

     }
   }
   return(intsct);
}

