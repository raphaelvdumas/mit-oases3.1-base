/*
 * list1.c - linked list procedures building and maintaining lists
 *           of simple geometrical objects 
 *              points, nodes, segments, triangles, rectangles, polygons
 *              and curves.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include "CNdata.h"
#include "CNproperty.h"
#include "CNdatatypes.h"

static void print_point();
static void insert_headnode();
static void insert_tailnode();
static CNsegmptr make_segm();
static CNptsegptr make_ptseg();
static CNlineptr make_line();

/*
 * POINT DATA STRUCTURE
 *    points are used to store the physical coordinates (x,y,z)
 */

/* 
 * Allocate room for a point
 */
CNpointptr CNmake_point(x,y,z,ID)
double x,y,z; 
int    ID;
{
   CNpointptr newptr;
   unsigned int size = sizeof(CNpoint);

   if ((newptr = (CNpointptr)malloc(size))!=NULL) {
      newptr->ID   = ID;
      newptr->flag = 0;
      newptr->x    = x;
      newptr->y    = y;
      newptr->z    = z;
      newptr->next = NULL;
      newptr->prev = NULL;
   }
   return(newptr);
}

/*  
 * Insert a point at the tail of the current point list
 */
CNpointptr CNinsert_tailpoint(point_listhead, point_listtail, x, y, z, ID)
CNpointptr *point_listhead, *point_listtail;
double     x, y, z;
int        ID;
{
   CNpointptr CNmake_point();
   CNpointptr next,A,B;

   A = *point_listtail;
   if ((B=CNmake_point(x,y,z,ID))!=NULL) {
      if (A==NULL) {
         *point_listhead = B;
         *point_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next != NULL) next->prev = B;
         if (B->next == NULL) *point_listtail = B;
      }
   }
   return(B);
}


/*
 * Insert a point at the head of the current point list
 */
CNpointptr CNinsert_headpoint(point_listhead, point_listtail, x, y, z, ID)
CNpointptr *point_listhead, *point_listtail;
double     x, y, z;
int        ID;
{
   CNpointptr A,B;

   A = *point_listhead;
   if ((B=CNmake_point(x,y,z,ID))!=NULL) {
      if (A==NULL) {
         *point_listhead = B;
         *point_listtail = B;
      } else {
         B->next = A;
         A->prev = B;
         *point_listhead = B;
      }
   }
   return(B);
}


/*
 * Delete point at address L 
 */
void CNdelete_point(point_listhead, point_listtail, L)
CNpointptr *point_listhead, *point_listtail;
CNpointptr L;
{
   CNpointptr prev,next;

   prev = L->prev;
   next = L->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (L== *point_listhead) *point_listhead = next;
   if (L== *point_listtail) *point_listtail = prev;
   free ((char*)L);
   L = NULL;
}


/* 
 * Delete all the points in the list
 */
void CNdelete_point_list(point_listhead, point_listtail)
CNpointptr *point_listhead, *point_listtail;
{
   CNpointptr P;

   while ((P = *point_listhead) != NULL)
      CNdelete_point(point_listhead, point_listtail, P);
}


/* 
 * Print out the list of points 
 */
/*ARGSUSED*/
void CNprint_point_list(point_listhead, point_listtail)
CNpointptr point_listhead, point_listtail;
{
   CNpointptr P;

   for (P=point_listhead; P!=NULL; P=P->next) 
      print_point(P);
}


/* 
 * print the coordinates of a point
 */
static void print_point(pt)
CNpointptr pt;
{
   (void) fprintf(stdout,"x= %10.5g y= %10.5g z= %10.5g\n",pt->x,pt->y,pt->z);
}


/*
 * Count the number of points in the list 
 */
/*ARGSUSED*/
int CNcount_points(point_listhead, point_listtail)
CNpointptr point_listhead, point_listtail;
{
   CNpointptr P;
   int        count = 0;

   for (P=point_listhead; P!=NULL; P=P->next) count++;

   return(count);
}


/*
 * NODE DATA STRUCTURE
 *    nodes are used to store all the points of varios objects, such as
 *    segments, curves, triangles, rectangles, polygons and curves.
 *    A node contains a single value as well as physical coordinates.
 */


/* 
 * Allocate room for a node 
 */
CNnodeptr CNmake_node(pos,val,ID)
CNpointptr pos;
double     val;
int        ID;
{
   CNnodeptr newptr;
   unsigned int size = sizeof(CNnode);

   if ((newptr = (CNnodeptr)malloc(size))!=NULL) {
      newptr->ID        = ID;
      newptr->flag      = 0;
      newptr->ptID      = 0;
      newptr->coord     = pos;
      newptr->t         = val;
      newptr->next      = NULL;
      newptr->prev      = NULL;
   }
   return(newptr);
}


/*  
 * Insert a node at the tail of the current node list
 */
static void insert_tailnode(node_listhead, node_listtail, B)
CNnodeptr *node_listhead, *node_listtail, B;
{
   CNnodeptr next,A;

   A = *node_listtail;
   if (B!=NULL) {
      if (A==NULL) {
         *node_listhead = B;
         *node_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next != NULL) next->prev = B;
         if (B->next == NULL) *node_listtail = B;
      }
   }
}


/*  
 * Insert a node at the head of the current node list
 */
static void insert_headnode(node_listhead, node_listtail, B)
CNnodeptr *node_listhead, *node_listtail, B;
{
   CNnodeptr A;

   A = *node_listhead;
   if (B!=NULL) {
      if (A==NULL) {
         *node_listhead = B;
         *node_listtail = B;
      } else {
         B->next = A;
         A->prev = B;
         *node_listhead = B;
      }
   }
}


/*  
 * Insert a node at the tail of the current node list
 */
CNnodeptr CNinsert_tailnode(node_listhead, node_listtail, pos, val, ID)
CNnodeptr *node_listhead, *node_listtail;
CNpointptr pos;
double     val;
int        ID;
{
   CNnodeptr   CNmake_node();
   CNnodeptr   B;

   if ((B=CNmake_node(pos,val,ID))!=NULL) 
      insert_tailnode(node_listhead, node_listtail, B);
   return(B);
}


/*  
 * Insert a node at the head of the current node list
 */
CNnodeptr CNinsert_headnode(node_listhead, node_listtail, pos, val, ID)
CNnodeptr *node_listhead, *node_listtail;
CNpointptr pos;
double     val;
int        ID;
{
   CNnodeptr   CNmake_node();
   CNnodeptr   B;

   if ((B=CNmake_node(pos,val,ID))!=NULL) 
      insert_headnode(node_listhead, node_listtail, B);
   return(B);
}


/*
 * Delete node at address L
 */
void CNdelete_node(node_listhead, node_listtail, L)
CNnodeptr *node_listhead, *node_listtail;
CNnodeptr L;
{
   CNnodeptr prev,next;

   prev = L->prev;
   next = L->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (L== *node_listhead) *node_listhead = next;
   if (L== *node_listtail) *node_listtail = prev;
   free ((char*)L);
   L = NULL;
}


/*
 * Delete all the nodes in the list
 */
void CNdelete_node_list(node_listhead, node_listtail)
CNnodeptr *node_listhead, *node_listtail;
{
   CNnodeptr N;

   while ((N = *node_listhead) != NULL)
      CNdelete_node(node_listhead, node_listtail, N);
}


/* 
 * Allocate room for a node and a point at the same time
 */
CNnodeptr CNcreate_node(x,y,z,t,ID)
double x,y,z,t;
int    ID;
{
   CNnodeptr newptr;
   CNpointptr point;

   if ((point=CNmake_point(x,y,z,ID))==NULL) return(NULL);
   newptr = CNmake_node(point,t,ID);
   return(newptr);
}

/*  
 * Insert a node at the tail of the current node list
 */
CNnodeptr CNcreate_tailnode(node_listhead, node_listtail, x,y,z,t, ID)
CNnodeptr *node_listhead, *node_listtail;
double     x,y,z,t;
int        ID;
{
   CNnodeptr   CNcreate_node();
   CNnodeptr   B;

   if ((B=CNcreate_node(x,y,z,t,ID))!=NULL) 
      insert_tailnode(node_listhead, node_listtail, B);
   return(B);
}


/*  
 * Insert a node at the head of the current node list
 */
CNnodeptr CNcreate_headnode(node_listhead, node_listtail, x,y,z,t, ID)
CNnodeptr *node_listhead, *node_listtail;
double     x,y,z,t;
int        ID;
{
   CNnodeptr   CNcreate_node();
   CNnodeptr   B;

   if ((B=CNcreate_node(x,y,z,t,ID))!=NULL) 
      insert_headnode(node_listhead, node_listtail, B);
   return(B);
}


/* 
 * Remove a node totally - delete the point too!
 */
void CNremove_node(node_listhead, node_listtail, L)
CNnodeptr *node_listhead, *node_listtail;
CNnodeptr L;
{
   CNpointptr P;

   P = L->coord;
   if (P->next==NULL && P->prev==NULL) {
      free ((char*)P);
      P = NULL;
   }
   CNdelete_node(node_listhead, node_listtail, L);
}


/*
 * Delete all the nodes in the list - remove the points too
 */
void CNremove_node_list(node_listhead, node_listtail)
CNnodeptr *node_listhead, *node_listtail;
{
   CNnodeptr N;

   while ((N = *node_listhead) != NULL)
      CNremove_node(node_listhead, node_listtail, N);
}


/*
 * Print out the list of nodes
 */
/*ARGSUSED*/
void CNprint_node_list(node_listhead, node_listtail)
CNnodeptr node_listhead, node_listtail;
{
   void CNprint_node();
   CNnodeptr N;

   for (N=node_listhead; N!=NULL; N=N->next)
      CNprint_node(N);
   (void) fprintf(stdout,"\n");
   (void) fflush(stdout);
}


/* 
 * print a node 
 */
void CNprint_node(N)
CNnodeptr N;
{
   (void) fprintf(stdout,"ID=%3d  Coord=(",N->ID);
   (void) fprintf(stdout,"%10.5f, ",N->coord->x);
   (void) fprintf(stdout,"%10.5f, ",N->coord->y);
   (void) fprintf(stdout,"%10.5f"  ,N->coord->z);
   (void) fprintf(stdout,")  Val=%10.5f\n",N->t);
   (void) fflush(stdout);
}


/*
 * Count the number of nodes in the list 
 */
/*ARGSUSED*/
int CNcount_nodes(node_listhead, node_listtail)
CNnodeptr node_listhead, node_listtail;
{
   CNnodeptr  N;
   int        count = 0;

   for (N=node_listhead; N!=NULL; N=N->next) count++;

   return(count);
}



/*
 * SEGMENTS
 *    A segment contains 2 nodes 
 *    Node segments are used primarily for finding the boundary of
 *    triangles in a single region.  The segments are attached to each
 *    other to form a nodelist (sequence of connected nodes).
 */


/*
 * Allocate room for a segment 
 */
static CNsegmptr make_segm(n1,n2,ID)
CNnodeptr n1, n2;
int       ID;
{
   CNsegmptr newptr;
   unsigned int size = sizeof(CNsegm);

   if ((newptr = (CNsegmptr)malloc(size))!=NULL) {
      newptr->ID        = ID;
      newptr->flag      = 0;
      newptr->n1        = n1;
      newptr->n2        = n2;
      newptr->nbrtri[0] = NULL;
      newptr->nbrtri[1] = NULL;
      newptr->numtri    = 0;
      newptr->next      = NULL;
      newptr->prev      = NULL;
   }
   return(newptr);
}


/*  
 * Insert a segm at the tail of the current segment list
 */
CNsegmptr CNinsert_segm(segm_listhead, segm_listtail, n1, n2, ID)
CNsegmptr *segm_listhead, *segm_listtail;
CNnodeptr n1, n2;
int       ID;
{
   CNsegmptr next,A,B;

   A = *segm_listtail;
   if ((B=make_segm(n1,n2,ID))!=NULL) {
      if (A==NULL) {
         *segm_listhead = B;
         *segm_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next != NULL) next->prev = B;
         if (B->next == NULL) *segm_listtail = B;
      }
   }
   return(B);
}


/*
 * Delete segm at address L 
 */
void CNdelete_segm(segm_listhead, segm_listtail, L)
CNsegmptr *segm_listhead, *segm_listtail;
CNsegmptr L;
{
   CNsegmptr prev,next;

   prev = L->prev;
   next = L->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (L== *segm_listhead) *segm_listhead = next;
   if (L== *segm_listtail) *segm_listtail = prev;
   free ((char*)L);
   L = NULL;
}


/* 
 * Delete all the segments in the list
 */
void CNdelete_segm_list(segm_listhead, segm_listtail)
CNsegmptr *segm_listhead, *segm_listtail;
{
   CNsegmptr S;

   while ((S = *segm_listhead) != NULL)
      CNdelete_segm(segm_listhead, segm_listtail, S);
}


/* 
 * Print out the list of segments 
 */
/*ARGSUSED*/
void CNprint_segm_list(segm_listhead, segm_listtail)
CNsegmptr segm_listhead, segm_listtail;
{
   void CNprint_segm();
   CNsegmptr S;

   for (S=segm_listhead; S!=NULL; S=S->next) 
      CNprint_segm(S);
}


/*
 * print a segment 
 */
void CNprint_segm(S)
CNsegmptr S;
{
   int     i;

   (void) fprintf(stdout,"   SegmID# %3d   Nodes (%3d %3d)   Triangles (",
                  S->ID, S->n1->ID, S->n2->ID);
   for (i=0; i< S->numtri; i++)
      (void) fprintf(stdout,"%3d ",S->nbrtri[i]->ID);
   (void) fprintf(stdout,")\n");
   (void) fflush(stdout);
}


/*
 * Count the number of segms in the list 
 */
/*ARGSUSED*/
int CNcount_segms(segm_listhead, segm_listtail)
CNsegmptr segm_listhead, segm_listtail;
{
   CNsegmptr  S;
   int        count = 0;

   for (S=segm_listhead; S!=NULL; S=S->next) count++;

   return(count);
}



/*
 * POINT-SEGMENTS
 *    A point-segment contains 2 points
 *    Point segments are used primarily for finding the boundary of
 *    triangles in one or more adjoining regions.  
 */


/*
 * Allocate room for a point-segment 
 */
static CNptsegptr make_ptseg(p1,p2,ID)
CNpointptr p1, p2;
int       ID;
{
   CNptsegptr newptr;
   unsigned int size = sizeof(CNptseg);

   if ((newptr = (CNptsegptr)malloc(size))!=NULL) {
      newptr->ID         = ID;
      newptr->flag       = 0;
      newptr->boundary   = CN_FALSE;
      newptr->p1         = p1;
      newptr->p2         = p2;
      newptr->nbrelem[0] = NULL;
      newptr->nbrelem[1] = NULL;
      newptr->numelem    = 0;
      newptr->next       = NULL;
      newptr->prev       = NULL;
   }
   return(newptr);
}


/*  
 * Insert a point-segment at the tail of the current point-segment list
 */
CNptsegptr CNinsert_ptseg(ptseg_listhead, ptseg_listtail, p1, p2, ID)
CNptsegptr *ptseg_listhead, *ptseg_listtail;
CNpointptr p1, p2;
int        ID;
{
   CNptsegptr next,A,B;

   A = *ptseg_listtail;
   if ((B=make_ptseg(p1,p2,ID))!=NULL) {
      if (A==NULL) {
         *ptseg_listhead = B;
         *ptseg_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next != NULL) next->prev = B;
         if (B->next == NULL) *ptseg_listtail = B;
      }
   }
   return(B);
}


/*
 * Delete point-segment at address L 
 */
void CNdelete_ptseg(ptseg_listhead, ptseg_listtail, L)
CNptsegptr *ptseg_listhead, *ptseg_listtail;
CNptsegptr L;
{
   CNptsegptr prev,next;

   prev = L->prev;
   next = L->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (L== *ptseg_listhead) *ptseg_listhead = next;
   if (L== *ptseg_listtail) *ptseg_listtail = prev;
   free ((char*)L);
   L = NULL;
}


/* 
 * Delete all the point-segments in the list
 */
void CNdelete_ptseg_list(ptseg_listhead, ptseg_listtail)
CNptsegptr *ptseg_listhead, *ptseg_listtail;
{
   CNptsegptr S;

   while ((S = *ptseg_listhead) != NULL)
      CNdelete_ptseg(ptseg_listhead, ptseg_listtail, S);
}


/* 
 * Print out the list of point-segments 
 */
/*ARGSUSED*/
void CNprint_ptseg_list(ptseg_listhead, ptseg_listtail)
CNptsegptr ptseg_listhead, ptseg_listtail;
{
   void CNprint_ptseg();
   CNptsegptr S;

   for (S=ptseg_listhead; S!=NULL; S=S->next) 
      CNprint_ptseg(S);
}


/*
 * print a point-segment 
 */
void CNprint_ptseg(S)
CNptsegptr S;
{
   int     i;

   (void) fprintf(stdout,"   PtsegID# %3d   Points (%3d %3d)   Elements (",
                  S->ID, S->p1->ID, S->p2->ID);
   for (i=0; i< S->numelem; i++) {
      if ((S->nbrelem[i]->type == CN_TRIA_STR) &&
          (S->nbrelem[i]->tria != NULL)) {
         (void) fprintf(stdout,"T%d ",S->nbrelem[i]->tria->ID);
      } else if ((S->nbrelem[i]->type == CN_RECT_STR) &&
          (S->nbrelem[i]->rect != NULL)) {
         (void) fprintf(stdout,"R%d ",S->nbrelem[i]->rect->ID);
      }
   }
   (void) fprintf(stdout,")\n");
   (void) fflush(stdout);
}


/*
 * Count the number of point-segments in the list 
 */
/*ARGSUSED*/
int CNcount_ptsegs(ptseg_listhead, ptseg_listtail)
CNptsegptr ptseg_listhead, ptseg_listtail;
{
   CNptsegptr  S;
   int        count = 0;

   for (S=ptseg_listhead; S!=NULL; S=S->next) count++;

   return(count);
}



/*
 * LINES
 *    A line contains 2 points/vertices
 *    The contouring works by slicing up a list of triangles, which
 *    generates a list of lines.  The lines are attached to each
 *    other to form a curve.
 *    The same is true for cutline and intersection procedures
 */


/*
 * Allocate room for a line
 */
static CNlineptr make_line(pt1, pt2)
CNpoint   *pt1, *pt2;
{
   CNlineptr newptr;
   unsigned int size = sizeof(CNline);

   if ((newptr = (CNlineptr)malloc(size))!=NULL) {
      newptr->pt1       = *pt1;
      newptr->pt2       = *pt2;
      newptr->next      = NULL;
      newptr->prev      = NULL;
   }
   return(newptr);
}


/*
 * Insert a line at the tail of the current line list
 */
void CNinsert_line(line_listhead, line_listtail, pt1, pt2)
CNlineptr *line_listhead, *line_listtail;
CNpoint   *pt1, *pt2;
{
   CNlineptr next,A,B;

   A = *line_listtail;
   if ((B=make_line(pt1,pt2))!=NULL) {
      if (A==NULL) {
         *line_listhead = B;
         *line_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next != NULL) next->prev = B;
         if (B->next == NULL) *line_listtail = B;
      }
   }
}


/*
 * Delete all the lines in the list
 */
void CNdelete_line_list(line_listhead, line_listtail)
CNlineptr *line_listhead, *line_listtail;
{
   CNlineptr L;

   while ((L = *line_listhead) != NULL)
      CNdelete_line(line_listhead, line_listtail, L);
}


/*
 * Delete line at address L
 */
void CNdelete_line(line_listhead, line_listtail, L)
CNlineptr *line_listhead, *line_listtail;
CNlineptr L;
{
   CNlineptr prev,next;

   prev = L->prev;
   next = L->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (L== *line_listhead) *line_listhead = next;
   if (L== *line_listtail) *line_listtail = prev;
   free ((char*)L);
   L = NULL;
}


/*
 * Print out the list of lines
 */
/*ARGSUSED*/
void CNprint_line_list(line_listhead, line_listtail)
CNlineptr line_listhead, line_listtail;
{
   CNlineptr L;

   for (L=line_listhead; L!=NULL; L=L->next) {
      print_point(&(L->pt1));
      print_point(&(L->pt2));
      (void) fprintf(stdout,"\n");
      (void) fflush(stdout);
   }
}


/*
 * Count the number of lines in the list
 */
/*ARGSUSED*/
int CNcount_lines(line_listhead, line_listtail)
CNlineptr line_listhead, line_listtail;
{
   CNlineptr P;
   int        count = 0;

   for (P=line_listhead; P!=NULL; P=P->next) count++;

   return(count);
}



/*
 * TRIANGLES
 *    The triangle is the basic mesh element for a 3D surface.
 *    A triangle contains 3 nodes 
 */


/*
 * triangles are used to store surface information 
 */
CNtriaptr CNmake_tria(n1,n2,n3,region,ID)
CNnodeptr n1, n2, n3;
int       region;
int       ID;
{
   CNtriaptr newptr;
   unsigned int size = sizeof(CNtria);

   if ((newptr = (CNtriaptr)malloc(size))!=NULL) {
      newptr->ID        = ID;
      newptr->flag      = 0;
      newptr->region    = region;
      newptr->nocont    = CN_FALSE;
      newptr->zave      = 0.0;
      newptr->n1ID      = 0;
      newptr->n2ID      = 0;
      newptr->n3ID      = 0;
      newptr->n1        = n1;
      newptr->n2        = n2;
      newptr->n3        = n3;
      newptr->R         = NULL;
      newptr->next      = NULL;
      newptr->prev      = NULL;
   }
   return(newptr);
}


/* 
 * Insert a tria at the tail of the current triangle list 
 */
CNtriaptr CNinsert_tria(tria_listhead,tria_listtail,n1,n2,n3,region,ID)
CNtriaptr *tria_listhead, *tria_listtail;
CNnodeptr n1, n2, n3;
int       region;
int       ID;
{
   CNtriaptr CNmake_tria();
   CNtriaptr next,A,B;

   A = *tria_listtail;
   if ((B=CNmake_tria(n1,n2,n3,region,ID))!=NULL) {
      if (A==NULL) {
         *tria_listhead = B;
         *tria_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next    != NULL) next->prev = B;
         if (B->next == NULL) *tria_listtail = B;
      }
   }
   return(B);
}


/* 
 * Delete tria 
 */
void CNdelete_tria(tria_listhead, tria_listtail, T)
CNtriaptr *tria_listhead, *tria_listtail;
CNtriaptr T;
{
   CNtriaptr prev,next;

   prev = T->prev;
   next = T->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (T==*tria_listhead) *tria_listhead = next;
   if (T==*tria_listtail) *tria_listtail = prev;

   /* Now delete T */
   free ((char*)T);
}


/* 
 * Delete all the triangles in the list
 */
void CNdelete_tria_list(tria_listhead, tria_listtail)
CNtriaptr *tria_listhead, *tria_listtail;
{
   CNtriaptr T;

   while ((T = *tria_listhead) != NULL)
      CNdelete_tria(tria_listhead, tria_listtail, T);
}


/* 
 * print out the list of triangles 
 */
/*ARGSUSED*/
void CNprint_tria_list(tria_listhead, tria_listtail)
CNtriaptr tria_listhead, tria_listtail;
{
   void CNprint_tria();
   CNtriaptr T;

   for (T=tria_listhead; T!=NULL; T=T->next) 
      CNprint_tria(T);
}


/*
 * print a triangle 
 */
void CNprint_tria(T)
CNtriaptr T;
{
   (void) fprintf(stdout,"   TriaID# %3d   Nodes (%3d %3d %3d)  Reg %d\n",
                  T->ID, T->n1->ID,T->n2->ID,T->n3->ID,T->region);
   (void) fflush(stdout);
}


/*
 * Count the number of trias in the list 
 */
/*ARGSUSED*/
int CNcount_trias(tria_listhead, tria_listtail)
CNtriaptr tria_listhead, tria_listtail;
{
   CNtriaptr  T;
   int        count = 0;

   for (T=tria_listhead; T!=NULL; T=T->next) count++;

   return(count);
}


/*
 * RECTANGLES
 *    Most contour data comes from rectangular grids.  
 *    A rectangle contains 4 ordered nodes 
 */


/*
 * rectangles are used to store surface information 
 */
CNrectptr CNmake_rect(n1,n2,n3,n4,ID)
CNnodeptr n1, n2, n3, n4;
int       ID;
{
   CNrectptr newptr;
   unsigned int size = sizeof(CNrect);

   if ((newptr = (CNrectptr)malloc(size))!=NULL) {
      newptr->ID     = ID;
      newptr->flag   = 0;
      newptr->region = 0;
      newptr->nocont = CN_FALSE;
      newptr->zave   = 0.0;
      newptr->n1ID   = 0;
      newptr->n2ID   = 0;
      newptr->n3ID   = 0;
      newptr->n4ID   = 0;
      newptr->n1     = n1;
      newptr->n2     = n2;
      newptr->n3     = n3;
      newptr->n4     = n4;
      newptr->R      = NULL;
      newptr->next   = NULL;
      newptr->prev   = NULL;
   }
   return(newptr);
}


/* 
 * Insert a rect at the tail of the current rectangle list 
 */
CNrectptr CNinsert_rect(rect_listhead,rect_listtail,n1,n2,n3,n4,ID)
CNrectptr *rect_listhead, *rect_listtail;
CNnodeptr n1, n2, n3, n4;
int       ID;
{
   CNrectptr CNmake_rect();
   CNrectptr next,A,B;

   A = *rect_listtail;
   if ((B=CNmake_rect(n1,n2,n3,n4,ID))!=NULL) {
      if (A==NULL) {
         *rect_listhead = B;
         *rect_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next    != NULL) next->prev = B;
         if (B->next == NULL) *rect_listtail = B;
      }
   }
   return(B);
}


/* 
 * Delete rect 
 */
void CNdelete_rect(rect_listhead, rect_listtail, T)
CNrectptr *rect_listhead, *rect_listtail;
CNrectptr T;
{
   CNrectptr prev,next;

   prev = T->prev;
   next = T->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (T==*rect_listhead) *rect_listhead = next;
   if (T==*rect_listtail) *rect_listtail = prev;

   /* Now delete T */
   free ((char*)T);
}


/* 
 * Delete all the rectangles in the list
 */
void CNdelete_rect_list(rect_listhead, rect_listtail)
CNrectptr *rect_listhead, *rect_listtail;
{
   CNrectptr T;

   while ((T = *rect_listhead) != NULL)
      CNdelete_rect(rect_listhead, rect_listtail, T);
}


/* 
 * print out the list of rectangles 
 */
/*ARGSUSED*/
void CNprint_rect_list(rect_listhead, rect_listtail)
CNrectptr rect_listhead, rect_listtail;
{
   void CNprint_rect();
   CNrectptr T;

   for (T=rect_listhead; T!=NULL; T=T->next) 
      CNprint_rect(T);
}


/*
 * print a rectangle 
 */
void CNprint_rect(T)
CNrectptr T;
{
   (void) fprintf(stdout,"   rectID# %3d   Nodes (%3d %3d %3d %3d)\n",
                  T->ID, T->n1->ID,T->n2->ID,T->n3->ID,T->n4->ID);
   (void) fflush(stdout);
}


/*
 * Count the number of rects in the list 
 */
/*ARGSUSED*/
int CNcount_rects(rect_listhead, rect_listtail)
CNrectptr rect_listhead, rect_listtail;
{
   CNrectptr  R;
   int        count = 0;

   for (R=rect_listhead; R!=NULL; R=R->next) count++;

   return(count);
}



/*
 * ELEMENTS
 *    An element is a container class which contains either a rectangle
 *    or a triangle.
 */


/*
 * elements are used to store surface information 
 */
CNelemptr CNmake_elem(T, R, type)
CNtriaptr T;
CNrectptr R;
int       type;
{
   CNelemptr newptr;
   unsigned int size = sizeof(CNelem);

   if ((newptr = (CNelemptr)malloc(size))!=NULL) {
      newptr->type   = type;
      newptr->flag   = 0;
      newptr->zave   = 0.0;
      newptr->tria   = T;
      newptr->rect   = R;
      newptr->next   = NULL;
      newptr->prev   = NULL;
   }
   return(newptr);
}


/* 
 * Insert a elem at the tail of the current element list 
 */
CNelemptr CNinsert_elem(elem_listhead,elem_listtail,T, R, type)
CNelemptr *elem_listhead, *elem_listtail;
CNtriaptr T;
CNrectptr R;
int       type;
{
   CNelemptr CNmake_elem();
   CNelemptr next,A,B;

   A = *elem_listtail;
   if ((B=CNmake_elem(T,R,type))!=NULL) {
      if (A==NULL) {
         *elem_listhead = B;
         *elem_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next    != NULL) next->prev = B;
         if (B->next == NULL) *elem_listtail = B;
      }
   }
   return(B);
}


/* 
 * Delete elem 
 */
void CNdelete_elem(elem_listhead, elem_listtail, T)
CNelemptr *elem_listhead, *elem_listtail;
CNelemptr T;
{
   CNelemptr prev,next;

   prev = T->prev;
   next = T->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (T==*elem_listhead) *elem_listhead = next;
   if (T==*elem_listtail) *elem_listtail = prev;

   /* Now delete T */
   free ((char*)T);
}


/* 
 * Delete all the elements in the list
 */
void CNdelete_elem_list(elem_listhead, elem_listtail)
CNelemptr *elem_listhead, *elem_listtail;
{
   CNelemptr T;

   while ((T = *elem_listhead) != NULL)
      CNdelete_elem(elem_listhead, elem_listtail, T);
}


/* 
 * print out the list of elements 
 */
/*ARGSUSED*/
void CNprint_elem_list(elem_listhead, elem_listtail)
CNelemptr elem_listhead, elem_listtail;
{
   void CNprint_elem();
   CNelemptr T;

   for (T=elem_listhead; T!=NULL; T=T->next) 
      CNprint_elem(T);
}


/*
 * print a element 
 */
void CNprint_elem(E)
CNelemptr E;
{
   if (E->type == CN_TRIA_STR) 
      CNprint_tria(E->tria);
   else if (E->type == CN_RECT_STR) 
      CNprint_rect(E->rect);
}


/*
 * Count the number of elems in the list 
 */
/*ARGSUSED*/
int CNcount_elems(elem_listhead, elem_listtail)
CNelemptr elem_listhead, elem_listtail;
{
   CNelemptr  R;
   int        count = 0;

   for (R=elem_listhead; R!=NULL; R=R->next) count++;

   return(count);
}



/*
 * POLYGON DATA STRUCTURE
 *    polygons are used to store nodes
 *    A polygon contains a list of connected nlists, which point to
 *    nodes contained in lists elsewhere.
 *    Polygons do not contain plotting information.
 */

/* 
 * Allocate room for a polygon 
 */
CNpolyptr CNmake_poly(nlisthead, nlisttail, region, ID)
CNnlistptr nlisthead, nlisttail;
int       region, ID;
{
   CNpolyptr newptr;
   unsigned int size = sizeof(CNpoly);

   if ((newptr = (CNpolyptr)malloc(size))!=NULL) {
      newptr->ID        = ID;
      newptr->flag      = 0;
      newptr->region    = region;
      newptr->fill      = 0;
      newptr->zave      = 0.0;
      newptr->nlisthead = nlisthead;
      newptr->nlisttail = nlisttail;
      newptr->next      = NULL;
      newptr->prev      = NULL;
   }
   return(newptr);
}


/* 
 * Insert a polygon at the tail of the current polygon list 
 */
CNpolyptr CNinsert_poly(poly_listhead, poly_listtail, 
                        nlisthead, nlisttail, region, ID)
CNpolyptr *poly_listhead, *poly_listtail;
CNnlistptr nlisthead, nlisttail;
int        region, ID;
{
   CNpolyptr CNmake_poly();
   CNpolyptr next,A,B;

   A = *poly_listtail;
   if ((B=CNmake_poly(nlisthead, nlisttail, region, ID))!=NULL) {
      if (A==NULL) {
         *poly_listhead = B;
         *poly_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next != NULL) next->prev = B;
         if (B->next == NULL) *poly_listtail = B;
      }
   }
   return(B);
}


/*
 * Delete poly
 */
void CNdelete_poly(poly_listhead, poly_listtail, P)
CNpolyptr *poly_listhead, *poly_listtail;
CNpolyptr P;
{
   CNpolyptr prev,next;

   /* Delete all the nlists in the list */
   CNdelete_nlist_list(&(P->nlisthead), &(P->nlisttail));

   /* Reset the pointers */
   prev = P->prev;
   next = P->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (P==*poly_listhead) *poly_listhead = next;
   if (P==*poly_listtail) *poly_listtail = prev;

   /* Now delete P */
   free ((char*)P);
}


/* 
 * Delete all the polygons in the list
 */
void CNdelete_poly_list(poly_listhead, poly_listtail)
CNpolyptr *poly_listhead, *poly_listtail;
{
   CNpolyptr P;

   while ((P = *poly_listhead) != NULL)
      CNdelete_poly(poly_listhead, poly_listtail, P);
}


/* 
 * print the polygon 
 */
/*ARGSUSED*/
void CNprint_poly_list(poly_listhead, poly_listtail)
CNpolyptr poly_listhead, poly_listtail;
{
   CNpolyptr  P;
   CNnlistptr NL;

   for (P=poly_listhead; P!=NULL; P=P->next) {
      (void) fprintf(stdout,"Polygon ID = %d   Nodes = (",P->ID);
      for (NL=P->nlisthead; NL!=NULL; NL=NL->next)
         (void) fprintf(stdout," %d", NL->N->ID);
      (void) fprintf(stdout,")\n");
      (void) fflush(stdout);
   }
}


/*
 * Count the number of polys in the list 
 */
/*ARGSUSED*/
int CNcount_polys(poly_listhead, poly_listtail)
CNpolyptr poly_listhead, poly_listtail;
{
   CNpolyptr  P;
   int        count = 0;

   for (P=poly_listhead; P!=NULL; P=P->next) count++;

   return(count);
}


/*
 * Return the size of all polys in the list 
 */
/*ARGSUSED*/
int CNpoly_list_size(poly_listhead, poly_listtail)
CNpolyptr poly_listhead, poly_listtail;
{
   CNpolyptr C;
   int        size = 0;

   for (C=poly_listhead; C!=NULL; C=C->next) 
      size += CNpoly_size(C);

   return(size);
}


/*
 * Return the (approximate) size of the polygon
 */
int CNpoly_size(Cptr)
CNpolyptr Cptr;
{
   int    size, nnlists;

   if (Cptr == NULL) return(0);

   /* The basic size */
   size = sizeof(CNpoly);

   /* Count the contents of the poly */
   nnlists = CNcount_nlists(Cptr->nlisthead,Cptr->nlisttail);

   /* Count the size of the components */
   size += nnlists*sizeof(CNnlist);

   /* return */
   return(size);
}

/*
 * Store the linked list
 */
void CNstore_poly(poly_listhead, poly_listtail, Pptr)
CNpolyptr *poly_listhead, *poly_listtail, Pptr;
{
   CNpolyptr A, B, next;

   A = *poly_listtail;
   if ((B=Pptr)!=NULL) {
      if (A==NULL) {
         *poly_listhead = B;
         *poly_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next != NULL) next->prev = B;
         if (B->next == NULL) *poly_listtail = B;
      }
   }
}



/*
 * CURVE PLOT DATA STRUCTURE
 *    curves are used to store the nodes of each separate contour or
 *    plot-curve. Unlike the polygon, the curve contains actual plotting
 *    information, such as linewidth, linetype.
 */

/* 
 * Allocate room for a curve 
 */
CNcurveptr CNmake_curve(ID)
int ID;
{
   CNcurveptr newptr;
   unsigned int size = sizeof(CNcurve);

   if ((newptr = (CNcurveptr)malloc(size))!=NULL) {

      /* Curve ID */
      newptr->ID = ID;

      /* For editing */
      newptr->focus = CN_FALSE;

      /* For sorting */
      newptr->zave = 0.0;

      /* Set the default property */
      CNset_default_curve_property(&(newptr->curv_pr));

      /* Set the linked lists */
      newptr->pointhead  = NULL;
      newptr->pointtail  = NULL;
      newptr->next       = NULL;
      newptr->prev       = NULL;
   }
   return(newptr);
}


/* 
 * Insert a curve at the tail of the current curve list 
 */
CNcurveptr CNinsert_curve(curve_listhead, curve_listtail, ID)
CNcurveptr *curve_listhead, *curve_listtail;
int        ID;
{
   CNcurveptr CNmake_curve();
   CNcurveptr next,A,B;

   A = *curve_listtail;
   if ((B=CNmake_curve(ID))!=NULL) {
      if (A==NULL) {
         *curve_listhead = B;
         *curve_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next != NULL) next->prev = B;
         if (B->next == NULL) *curve_listtail = B;
      }
   }
   return(B);
}


/*
 * Delete curve
 */
void CNdelete_curve(curve_listhead, curve_listtail, C)
CNcurveptr *curve_listhead, *curve_listtail;
CNcurveptr C;
{
   void     CNdelete_node();
   CNcurveptr prev,next;

   /* delete all the points in C */
   CNdelete_point_list(&(C->pointhead), &(C->pointtail));

   /* Delete the curve's linelabel */
   CNdelete_curve_property_fields(&(C->curv_pr));

   prev = C->prev;
   next = C->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (C==*curve_listhead) *curve_listhead = next;
   if (C==*curve_listtail) *curve_listtail = prev;

   /* Now delete C */
   free ((char*)C);
}


/* 
 * Delete all the curves in the list
 */
void CNdelete_curve_list(curve_listhead, curve_listtail)
CNcurveptr *curve_listhead, *curve_listtail;
{
   CNcurveptr C;

   while ((C = *curve_listhead) != NULL)
      CNdelete_curve(curve_listhead, curve_listtail, C);
}


/* 
 * print the curves 
 */
/*ARGSUSED*/
void CNprint_curve_list(curve_listhead, curve_listtail, verbose)
CNcurveptr curve_listhead, curve_listtail;
int        verbose;
{
   CNcurveptr C;

   for (C=curve_listhead; C!=NULL; C=C->next) {
      CNprint_curve(C,verbose);
   }
}


/* 
 * print the curves 
 */
void CNprint_curve(Cptr,verbose)
CNcurveptr Cptr;
int        verbose;
{
   (void) fprintf(stdout,"Curve %d:\n",Cptr->ID);
   (void) fprintf(stdout,"No of points = %d\n",
                  CNcount_points(Cptr->pointhead, Cptr->pointtail));
   if (verbose) {
   CNprint_curve_property(&(Cptr->curv_pr));
   CNprint_point_list(Cptr->pointhead, Cptr->pointtail);
   }
}


/*
 * Count the number of curves in the list 
 */
/*ARGSUSED*/
int CNcount_curves(curve_listhead, curve_listtail)
CNcurveptr curve_listhead, curve_listtail;
{
   CNcurveptr C;
   int        count = 0;

   for (C=curve_listhead; C!=NULL; C=C->next) count++;

   return(count);
}


/*
 * Return the size of all curves in the list 
 */
/*ARGSUSED*/
int CNcurve_list_size(curve_listhead, curve_listtail)
CNcurveptr curve_listhead, curve_listtail;
{
   CNcurveptr C;
   int        size = 0;

   for (C=curve_listhead; C!=NULL; C=C->next) 
      size += CNcurve_size(C);

   return(size);
}


/*
 * Return the (approximate) size of the curve
 */
int CNcurve_size(Cptr)
CNcurveptr Cptr;
{
   int    size, npoints;

   if (Cptr == NULL) return(0);

   /* The basic size */
   size = sizeof(CNcurve);

   /* Count the contents of the curve */
   npoints = CNcount_points(Cptr->pointhead,Cptr->pointtail);

   /* Count the size of the components */
   size += npoints*sizeof(CNpoint);

   /* return */
   return(size);
}


/*
 * Store the linked list
 */
void CNstore_curve(curve_listhead, curve_listtail, Cptr, verbose)
CNcurveptr *curve_listhead, *curve_listtail, Cptr;
int        verbose;
{
   CNcurveptr A, B, next;

   A = *curve_listtail;
   if ((B=Cptr)!=NULL) {
      if (A==NULL) {
         *curve_listhead = B;
         *curve_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next != NULL) next->prev = B;
         if (B->next == NULL) *curve_listtail = B;
      }
   }

   /* Things are going well here */
   if (verbose)
      CNprint_curve_list(*curve_listhead, *curve_listtail, 0);
}


/*
 * Append one linked list to another
 */
/*ARGSUSED*/
void CNappend_curves(targethead, targettail, curvehead, curvetail, verbose)
CNcurveptr *targethead, *targettail;
CNcurveptr curvehead, curvetail;
int        verbose;
{
   CNcurveptr Cptr, next;

   /*
    * The insertion routine resets the (Cptr) pointer's links, so
    * we cannot use a for loop.
    */

   /* Append the lists */
   Cptr = curvehead;
   while (Cptr!=NULL) {
      next = Cptr->next;
      Cptr->next = NULL;
      Cptr->prev = NULL;
      CNstore_curve(targethead, targettail, Cptr, 0);
      Cptr = next;
   }

   /* Things are going well here */
   if (verbose)
      CNprint_curve_list(*targethead, *targettail, 0);
}

