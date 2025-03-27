/*
 * list2.c - linked list procedures building and maintaining lists
 *           of complex objects 
 *              datasets, plotsets.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include "CNplot.h"

static CNdslistptr make_dslist();
static CNpslistptr make_pslist();

/*
 * DATASET DATA STRUCTURE
 *    A dataset contains a set of curves and additional data related to
 *    a single plot.
 */


/* 
 * Allocate room for the dataset 
 */
CNdatasetptr CNmake_dataset(filename,label,datatype,
                            bxmin,bxmax,bymin,bymax,bzmin,bzmax,
                            vxmin,vxmax,vymin,vymax,vzmin,vzmax,ID)
char   *filename;                             /* Associated file   */
char   *label;                                /* Descriptive label */
int    datatype;                              /* Data type         */
double bxmin,bxmax,bymin,bymax,bzmin,bzmax;   /* Plot boundaries   */
double vxmin,vxmax,vymin,vymax,vzmin,vzmax;   /* Plot viewport     */
int    ID;                                    /* Data-set ID       */
{
   CNdatasetptr newptr;
   unsigned int size = sizeof(CNdataset);

   if ((newptr = (CNdatasetptr)malloc(size))!=NULL) {
      newptr->filename  = CNcreate_string(filename);
      newptr->label     = CNcreate_string(label);
      newptr->datatype  = datatype;
      newptr->flag      = 0;
      newptr->ID        = ID;

      /* For GUI panel selection */
      newptr->paneltype = -1;

      /* For datasets derived from quantities */
      newptr->fieldID   = 0;

      /* Plot boundaries */
      newptr->bxmin     = bxmin;
      newptr->bxmax     = bxmax;
      newptr->bymin     = bymin;
      newptr->bymax     = bymax;
      newptr->bzmin     = bzmin;
      newptr->bzmax     = bzmax;

      /* Set the default data property */
      CNset_default_dataset_property(&(newptr->data_pr));
    
      /* Set the default plot property */
      CNset_default_plotset_property(&(newptr->plot_pr));
    
      /* Plot viewport   */
      newptr->plot_pr.vxmin      = vxmin;
      newptr->plot_pr.vxmax      = vxmax;
      newptr->plot_pr.vymin      = vymin;
      newptr->plot_pr.vymax      = vymax;
      newptr->plot_pr.vzmin      = vzmin;
      newptr->plot_pr.vzmax      = vzmax;

      /* Previous Plot viewport   */
      newptr->plot_pr.prev_vxmin = vxmin;
      newptr->plot_pr.prev_vxmax = vxmax;
      newptr->plot_pr.prev_vymin = vymin;
      newptr->plot_pr.prev_vymax = vymax;
      newptr->plot_pr.prev_vzmin = vzmin;
      newptr->plot_pr.prev_vzmax = vzmax;

      /* 3D view */
      newptr->view_pr = CNcreate_view();

      /* Linked-lists */
      newptr->pointhead = NULL;
      newptr->pointtail = NULL;
      newptr->nodehead  = NULL;
      newptr->nodetail  = NULL;
      newptr->segmhead  = NULL;
      newptr->segmtail  = NULL;
      newptr->ptseghead = NULL;
      newptr->ptsegtail = NULL;
      newptr->triahead  = NULL;
      newptr->triatail  = NULL;
      newptr->recthead  = NULL;
      newptr->recttail  = NULL;
      newptr->elemhead  = NULL;
      newptr->elemtail  = NULL;
      newptr->polyhead  = NULL;
      newptr->polytail  = NULL;
      newptr->curvehead = NULL;
      newptr->curvetail = NULL;
      newptr->quanthead = NULL;
      newptr->quanttail = NULL;
      newptr->regionhead= NULL;
      newptr->regiontail= NULL;
      newptr->parent    = NULL;
      newptr->quantity  = NULL;
      newptr->grid      = NULL;
      newptr->mesh4D    = NULL;
      newptr->histogram = NULL;
      newptr->barchart  = NULL;
      newptr->vecbox    = NULL;
      newptr->cstephead = NULL;
      newptr->csteptail = NULL;
      newptr->annothead = NULL;
      newptr->annottail = NULL;
      newptr->next      = NULL;
      newptr->prev      = NULL;
   }
   return(newptr);
}


/* 
 * Insert a dataset at the tail of the current list 
 * Return 0 if failed.
 */
int CNinsert_dataset(data_listhead, data_listtail, Dptr)
CNdatasetptr *data_listhead, *data_listtail;
CNdatasetptr Dptr;
{
   CNdatasetptr next,A;

   A = *data_listtail;
   if (Dptr != NULL) {
      if (A==NULL) {
         *data_listhead = Dptr;
         *data_listtail = Dptr;
      } else {
         next = A->next;
         Dptr->next = next;
         Dptr->prev = A;
         A->next    = Dptr;
         if (next != NULL) next->prev = Dptr;
         if (Dptr->next == NULL) *data_listtail = Dptr;
      }
      return(1);
   } else {
     (void) fprintf(stderr,
                    "   ***Error! Null pointer passed to insert_dataset()\n");
     return(0);
   }
}


/*
 * print the data in the data set 
 */
/*ARGSUSED*/
void CNprint_dataset_list(data_listhead, data_listtail,verbose)
CNdatasetptr data_listhead, data_listtail;
int          verbose;
{
   CNdatasetptr Dptr;
  
   for (Dptr=data_listhead; Dptr!=NULL; Dptr=Dptr->next) 
      CNprint_dataset(Dptr,verbose);
} 


/*
 * print the data in the data set 
 */
/*ARGSUSED*/
void CNprint_dataset(Dptr,verbose)
CNdatasetptr Dptr;
{
   int    npoints, nnodes, nsegms, nptsegs, ntrias, nrects;
   int    ncurves, nquants, nregions, ncsteps, nannots;
  
   (void) fprintf(stdout,"\n");
   (void) fprintf(stdout,"   DATA ID   = %d\n",Dptr->ID);
   (void) fprintf(stdout,"   DATA TYPE = %s\n",CNdatatype(Dptr->datatype));
   (void) fprintf(stdout,"   Origin    = %s\n",Dptr->filename);
   (void) fprintf(stdout,"   Label     = %s\n",Dptr->label);

   /* Count the contents of the dataset */
   npoints = CNcount_points(Dptr->pointhead,  Dptr->pointtail);
   nnodes  = CNcount_nodes (Dptr->nodehead,   Dptr->nodetail );
   nsegms  = CNcount_segms (Dptr->segmhead,   Dptr->segmtail );
   nptsegs = CNcount_ptsegs(Dptr->ptseghead,  Dptr->ptsegtail );
   ntrias  = CNcount_trias (Dptr->triahead,   Dptr->triatail );
   nrects  = CNcount_rects (Dptr->recthead,   Dptr->recttail );
   ncurves = CNcount_curves(Dptr->curvehead,  Dptr->curvetail);
   nquants = CNcount_quants(Dptr->quanthead,  Dptr->quanttail);
   nregions= CNcount_regions(Dptr->regionhead,Dptr->regiontail);
   ncsteps = CNcount_contsteps(Dptr->cstephead,Dptr->csteptail);
   nannots = CNcount_annotations(Dptr->annothead,Dptr->annottail);

   /* Print the contents of the dataset */
   if (npoints > 0) (void) fprintf(stdout,"   No of points = %d\n", npoints);
   if (nnodes  > 0) (void) fprintf(stdout,"   No of nodes  = %d\n", nnodes );
   if (nsegms  > 0) (void) fprintf(stdout,"   No of segms  = %d\n", nsegms );
   if (nptsegs > 0) (void) fprintf(stdout,"   No of ptsegs = %d\n", nptsegs);
   if (ntrias  > 0) (void) fprintf(stdout,"   No of trias  = %d\n", ntrias );
   if (nrects  > 0) (void) fprintf(stdout,"   No of rects  = %d\n", nrects );
   if (ncurves > 0) (void) fprintf(stdout,"   No of curves = %d\n", ncurves);
   if (nquants > 0) (void) fprintf(stdout,"   No of quants = %d\n", nquants);
   if (nregions> 0) (void) fprintf(stdout,"   No of regions= %d\n", nregions);
   if (ncsteps > 0) (void) fprintf(stdout,"   No of csteps = %d\n", ncsteps);
   if (nannots > 0) (void) fprintf(stdout,"   No of annots = %d\n", nannots);

   if (verbose) {
      if (Dptr->datatype == CN_CONTOUR) 
         CNprint_dataset_property(&(Dptr->data_pr));
      (void) fprintf(stdout,"xmin = %7.3g   xmax = %7.3g\n", 
                     Dptr->bxmin,Dptr->bxmax);
      (void) fprintf(stdout,"ymin = %7.3g   ymax = %7.3g\n", 
                     Dptr->bymin,Dptr->bymax);
      (void) fprintf(stdout,"zmin = %7.3g   zmax = %7.3g\n", 
                     Dptr->bzmin,Dptr->bzmax);
      CNprint_curve_list(Dptr->curvehead, Dptr->curvetail,0);
   }
} 


/*
 * delete the data set 
 */
void CNdelete_dataset(data_listhead, data_listtail, Dptr)
CNdatasetptr *data_listhead, *data_listtail;
CNdatasetptr Dptr;
{
   void       CNdelete_tria();
   CNdatasetptr prev,next;

   /* Free the char strings */
   CNdestroy_string(Dptr->filename);
   CNdestroy_string(Dptr->label);

   /* Delete the annotation list first */
   CNdelete_annotation_list(&(Dptr->annothead), &(Dptr->annottail));

   /* Free the cstep list */
   CNdelete_contstep_list(&(Dptr->cstephead), &(Dptr->csteptail));

   /* Free the vector-box */
   if (Dptr->vecbox != NULL) CNdelete_vectorbox(Dptr->vecbox);

   /* Free the barchart */
   if (Dptr->barchart != NULL) CNdelete_barchart(Dptr->barchart);

   /* Free the histogram */
   if (Dptr->histogram != NULL) CNdelete_histogram(Dptr->histogram);

   /* Free the mesh4D */
   if (Dptr->mesh4D != NULL) CNdelete_mesh4D(Dptr->mesh4D);

   /* Free the grid */
   if (Dptr->grid != NULL) CNdelete_grid4D(Dptr->grid);

   /* delete all the regionptrs in Dptr */
   CNdelete_region_list(&(Dptr->regionhead),&(Dptr->regiontail));

   /* delete all the quantptrs in Dptr */
   CNdelete_quant_list(&(Dptr->quanthead),&(Dptr->quanttail));

   /* delete all the curveptrs in Dptr */
   CNdelete_curve_list(&(Dptr->curvehead),&(Dptr->curvetail));

   /* delete all the polygons in Dptr */
   CNdelete_poly_list(&(Dptr->polyhead),&(Dptr->polytail));

   /* delete all the elements in Dptr */
   CNdelete_elem_list(&(Dptr->elemhead),&(Dptr->elemtail));

   /* delete all the rectangles in Dptr */
   CNdelete_rect_list(&(Dptr->recthead),&(Dptr->recttail));

   /* delete all the triangles in Dptr */
   CNdelete_tria_list(&(Dptr->triahead),&(Dptr->triatail));

   /* delete all the point-segments in Dptr */
   CNdelete_ptseg_list(&(Dptr->ptseghead),&(Dptr->ptsegtail));

   /* delete all the segments in Dptr */
   CNdelete_segm_list(&(Dptr->segmhead),&(Dptr->segmtail));

   /* delete all the nodes in Dptr */
   CNdelete_node_list(&(Dptr->nodehead),&(Dptr->nodetail));

   /* delete all the points in Dptr */
   CNdelete_point_list(&(Dptr->pointhead),&(Dptr->pointtail));

   /* Delete the strings attached to the plotset property */
   CNdelete_plotset_property_fields(&(Dptr->plot_pr));

   /* Delete the strings attached to the dataset property */
   CNdelete_dataset_property_fields(&(Dptr->data_pr));

   /* Delete the view */
   CNdelete_view(Dptr->view_pr);

   /* Reset the pointers */
   prev = Dptr->prev;
   next = Dptr->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (Dptr==*data_listhead) *data_listhead = next;
   if (Dptr==*data_listtail) *data_listtail = prev;

   /* Now delete Dptr */
   free ((char*)Dptr);
}


/* 
 * delete the entire list of datasets 
 */
void CNdelete_dataset_list(data_listhead, data_listtail)
CNdatasetptr *data_listhead, *data_listtail;
{ 
   void CNdelete_dataset();

   while (*data_listhead != NULL) 
      CNdelete_dataset(data_listhead, data_listtail, *data_listhead);
}


/*
 * Count the number of datasets in the list
 */
/*ARGSUSED*/
int CNcount_datasets(data_listhead, data_listtail)
CNdatasetptr data_listhead, data_listtail;
{
   CNdatasetptr P;
   int          count = 0;
 
   for (P=data_listhead; P!=NULL; P=P->next) count++;
 
   return(count);
}

/* 
 * Store the linked list in the contour data structure 
 */
void CNstore_dataset(datahead, datatail, Dptr, verbose)
CNdatasetptr *datahead, *datatail, Dptr;
int          verbose;
{
   if (Dptr == NULL) {
      /* Read error - print out warning message */
      (void) fprintf(stderr, "   ***Error during data read!\n");
   } else if (!CNinsert_dataset(datahead, datatail, Dptr)) {
      (void) fprintf(stderr,"   ***Error: Couldn't add pointer to list\n");
      return;   
   } else {
      /* Things are going well here */
      /*
      if (verbose) (void) fprintf(stdout,"Verbose=ON!\n");
       */
      if (verbose)
         CNprint_dataset_list(*datahead, *datatail, 0);
   }
}


/* 
 * Append one linked list to another 
 */
/*ARGSUSED*/
void CNappend_datasets(targethead, targettail, datahead, datatail, verbose)
CNdatasetptr *targethead, *targettail;
CNdatasetptr datahead, datatail;
int          verbose;
{
   CNdatasetptr Dptr, next;

   /* 
    * The insertion routine resets the (Dptr) pointer's links, so 
    * we cannot use a for loop.
    */

   /* Append the lists */
   Dptr = datahead; 
   while (Dptr!=NULL) {
      next = Dptr->next;
      Dptr->next = NULL;
      Dptr->prev = NULL;
      CNstore_dataset(targethead, targettail, Dptr, 0);
      Dptr = next;
   }

   /* Things are going well here */
   if (verbose)
      CNprint_dataset_list(*targethead, *targettail, 0);
}


/*
 * Return a string denoting the plot-type
 */
char *CNdatatype(datatype)
int datatype;
{
   char *data;

   switch (datatype) {
   case CN_CONTOUR    : data = "Contour";  break;
   case CN_PLOT2D     : data = "2D Curve";  break;
   case CN_PLOT3D     : data = "3D Curve";  break;
   case CN_GRID4D     : data = "4D Grid";  break;
   case CN_VECTOR     : data = "Vector" ;  break;
   case CN_PROBAB     : data = "Probability" ;  break;
   case CN_HISTOGRAM  : data = "Histogram" ;  break;
   case CN_BARCHART   : data = "Bar Chart" ;  break;
   case CN_PIF_PARENT : data = "Triangular Mesh (P)";  break;
   case CN_PIF_CHILD  : data = "Triangular Mesh (C)";  break;
   case CN_MESH4D_P   : data = "4D Tensor-prod Mesh (P)";  break;
   case CN_MESH4D_C   : data = "4D Tensor-prod Mesh (C)";  break;
   case CN_POLYGON    : data = "Polygon Mesh";  break;
   default            : data = "Null Data";  break;
   }

   return(data);
}


/*
 * print the dataset sizes
 */
/*ARGSUSED*/
void CNprint_dataset_list_size(data_listhead, data_listtail)
CNdatasetptr data_listhead, data_listtail;
{
   CNdatasetptr Dptr;
   int          size, totsize=0;

   for (Dptr=data_listhead; Dptr!=NULL; Dptr=Dptr->next) {
      size = CNdataset_size(Dptr);
      totsize += size;
      (void) fprintf(stdout,"Dataset %d (%-19s) : %d\n",
                     Dptr->ID,CNdatatype(Dptr->datatype),size);
   }   
   (void) fprintf(stdout,"Total size = %d\n",totsize);
}


/*
 * Return the (approximate) size of the dataset
 */
int CNdataset_size(Dptr)
CNdatasetptr Dptr;
{
   int    size;
   int    npoints, nnodes, nsegms, ntrias, nrects;
   
   if (Dptr == NULL) return(0);

   /* The basic size */
   size = sizeof(CNdataset);
   
   /* Count the contents of the dataset */
   npoints = CNcount_points(Dptr->pointhead,Dptr->pointtail);
   nnodes  = CNcount_nodes (Dptr->nodehead, Dptr->nodetail );
   nsegms  = CNcount_segms (Dptr->segmhead, Dptr->segmtail );
   ntrias  = CNcount_trias (Dptr->triahead, Dptr->triatail );
   nrects  = CNcount_rects (Dptr->recthead, Dptr->recttail );

   /* Count the size of the components */
   size += npoints*sizeof(CNpoint);
   size += nnodes *sizeof(CNnode );
   size += nsegms *sizeof(CNsegm );
   size += ntrias *sizeof(CNtria );
   size += nrects *sizeof(CNrect );

   /* Curves and quantities contain more linked lists so...*/
   size +=  CNcurve_list_size(Dptr->curvehead,Dptr->curvetail);
   size +=  CNquant_list_size(Dptr->quanthead,Dptr->quanttail);
   size +=  CNregion_list_size(Dptr->regionhead,Dptr->regiontail);

   /* return */
   return(size);
}


/*
 * DATASET LIST
 *    The dataset-list contains pointers to a set of datasetptrs
 */

/*
 * dataset-lists are used to store pointers to datasets
 */
static
CNdslistptr make_dslist(Dptr)
CNdatasetptr Dptr; 
{
   CNdslistptr newptr;
   unsigned int size = sizeof(CNdslist);

   if ((newptr = (CNdslistptr)malloc(size))!=NULL) {
      newptr->flag  = 0;
      newptr->focus = CN_FALSE;
      newptr->Dptr  = Dptr;
      newptr->next  = NULL;
      newptr->prev  = NULL;
   }
   return(newptr);
}


/* 
 * Insert a dslist at the tail of the current dataset list 
 */
CNdslistptr CNinsert_dslist(dslist_listhead,dslist_listtail,Dptr)
CNdslistptr *dslist_listhead, *dslist_listtail;
CNdatasetptr  Dptr; 
{
   CNdslistptr next,A,B;

   A = *dslist_listtail;
   if ((B=make_dslist(Dptr))!=NULL) {
      if (A==NULL) {
         *dslist_listhead = B;
         *dslist_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next    != NULL) next->prev = B;
         if (B->next == NULL) *dslist_listtail = B;
      }
   }
   return(B);
}


/* 
 * Delete dslist 
 */
void CNdelete_dslist(dslist_listhead, dslist_listtail, T)
CNdslistptr *dslist_listhead, *dslist_listtail;
CNdslistptr T;
{
   CNdslistptr prev,next;

   prev = T->prev;
   next = T->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (T==*dslist_listhead) *dslist_listhead = next;
   if (T==*dslist_listtail) *dslist_listtail = prev;

   /* Now delete T */
   free ((char*)T);
}


/* 
 * Delete all the datasets in the list
 */
void CNdelete_dslist_list(dslist_listhead, dslist_listtail)
CNdslistptr *dslist_listhead, *dslist_listtail;
{
   CNdslistptr T;

   while ((T = *dslist_listhead) != NULL)
      CNdelete_dslist(dslist_listhead, dslist_listtail, T);
}


/* 
 * print out the list of datasets 
 */
/*ARGSUSED*/
void CNprint_dslist_list(dslist_listhead, dslist_listtail)
CNdslistptr dslist_listhead, dslist_listtail;
{
   CNdslistptr DS;

   (void) fprintf(stdout,"Dataset list:");
   for (DS=dslist_listhead; DS!=NULL; DS=DS->next) 
      (void) fprintf(stdout," %d", (DS->Dptr==NULL) ? -1 : DS->Dptr->ID);
   (void) fprintf(stdout,"\n");
   (void) fflush(stdout);
}


/*
 * Count the number of dslists in the list
 */
/*ARGSUSED*/
int CNcount_dslists(data_listhead, data_listtail)
CNdslistptr data_listhead, data_listtail;
{
   CNdslistptr P;
   int         count = 0;
 
   for (P=data_listhead; P!=NULL; P=P->next) count++;
 
   return(count);
}



/*
 * PLOT SETS 
 *    A plot set contains a linked list of pointers to data-sets.  
 */

/*
 * A plotset contains a linked list of pointers to datasets
 */
CNplotsetptr CNmake_plotset(name,ID)
char      *name;
int       ID;
{
   CNplotsetptr newptr;
   unsigned int size = sizeof(CNplotset);

   if ((newptr = (CNplotsetptr)malloc(size))!=NULL) {
      newptr->ID           = ID;
      newptr->flag         = 0;
      newptr->plottype     = CN_PLOT2D;
      newptr->plotformat   = CN_SCIENTIFIC_PLOT;
      newptr->label        = CNcreate_string(((name==NULL)?CN_NONAME:name));

      /* Set the default plot property */
      CNset_default_plotset_property(&(newptr->plot_pr));

      /* User Data */
      newptr->userdata     = NULL;

      /* 3D view */
      newptr->view_pr      = CNcreate_view();

      /* Linked lists */
      newptr->annothead    = NULL;
      newptr->annottail    = NULL;
      newptr->datahead     = NULL;
      newptr->datatail     = NULL;
      newptr->next         = NULL;
      newptr->prev         = NULL;
   }
   return(newptr);
}


/* 
 * Insert a plotset at the tail of the current plotset list 
 */
CNplotsetptr CNinsert_plotset(plot_listhead,plot_listtail,name,ID)
CNplotsetptr *plot_listhead, *plot_listtail;
char         *name;
int          ID;
{
   CNplotsetptr next,A,B;

   A = *plot_listtail;
   if ((B=CNmake_plotset(name,ID))!=NULL) {
      if (A==NULL) {
         *plot_listhead = B;
         *plot_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next    != NULL) next->prev = B;
         if (B->next == NULL) *plot_listtail = B;
      }
   }
   return(B);
}


/* 
 * Delete plotset 
 */
void CNdelete_plotset(plot_listhead, plot_listtail, Pptr)
CNplotsetptr *plot_listhead, *plot_listtail;
CNplotsetptr Pptr;
{
   CNplotsetptr prev,next;

   /* Delete the annotation list first */
   CNdelete_annotation_list(&(Pptr->annothead), &(Pptr->annottail));

   /* Delete the dataset list first */
   CNdelete_dslist_list(&(Pptr->datahead), &(Pptr->datatail));

   /* Delete the view */
   CNdelete_view(Pptr->view_pr);

   /* Delete the strings attached to the plotset property */
   CNdelete_plotset_property_fields(&(Pptr->plot_pr));

   /* Delete the plotset label */
   CNdestroy_string(Pptr->label);

   prev = Pptr->prev;
   next = Pptr->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (Pptr==*plot_listhead) *plot_listhead = next;
   if (Pptr==*plot_listtail) *plot_listtail = prev;

   /* Now delete Pptr */
   free ((char*)Pptr);
}


/* 
 * Delete all the plotsets in the list
 */
void CNdelete_plotset_list(plot_listhead, plot_listtail)
CNplotsetptr *plot_listhead, *plot_listtail;
{
   CNplotsetptr Pptr;

   while ((Pptr = *plot_listhead) != NULL)
      CNdelete_plotset(plot_listhead, plot_listtail, Pptr);
}


/* 
 * print out the list of plotsets 
 */
/*ARGSUSED*/
void CNprint_plotset_list(plotset_listhead, plotset_listtail)
CNplotsetptr plotset_listhead, plotset_listtail;
{
   void CNprint_plotset();
   CNplotsetptr Pptr;

   for (Pptr=plotset_listhead; Pptr!=NULL; Pptr=Pptr->next) 
      CNprint_plotset(Pptr);
}


/*
 * print a plotset 
 */
void CNprint_plotset(Pptr)
CNplotsetptr Pptr;
{
   (void) fprintf(stdout,"   plotsetID# %3d  ",Pptr->ID);
   CNprint_dslist_list(Pptr->datahead, Pptr->datatail);
   (void) fflush(stdout);
}


/*
 * Count the number of plotsets in the list
 */
/*ARGSUSED*/
int CNcount_plotsets(plot_listhead, plot_listtail)
CNplotsetptr plot_listhead, plot_listtail;
{
   CNplotsetptr P;
   int          count = 0;
 
   for (P=plot_listhead; P!=NULL; P=P->next) count++;
 
   return(count);
}
 


/*
 * PLOTSET LIST
 *    The plotset-list contains pointers to a set of plotsetptrs
 */

/*
 * plotset-lists are used to store pointers to plotsets
 */
static
CNpslistptr make_pslist(Pptr)
CNplotsetptr Pptr; 
{
   CNpslistptr newptr;
   unsigned int size = sizeof(CNpslist);

   if ((newptr = (CNpslistptr)malloc(size))!=NULL) {
      newptr->flag  = 0;
      newptr->Pptr  = Pptr;
      newptr->next  = NULL;
      newptr->prev  = NULL;
   }
   return(newptr);
}


/* 
 * Insert a pslist at the tail of the current plotset list 
 */
CNpslistptr CNinsert_pslist(pslist_listhead,pslist_listtail,Pptr)
CNpslistptr *pslist_listhead, *pslist_listtail;
CNplotsetptr  Pptr; 
{
   CNpslistptr next,A,B;

   A = *pslist_listtail;
   if ((B=make_pslist(Pptr))!=NULL) {
      if (A==NULL) {
         *pslist_listhead = B;
         *pslist_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next    != NULL) next->prev = B;
         if (B->next == NULL) *pslist_listtail = B;
      }
   }
   return(B);
}


/* 
 * Delete pslist 
 */
void CNdelete_pslist(pslist_listhead, pslist_listtail, T)
CNpslistptr *pslist_listhead, *pslist_listtail;
CNpslistptr T;
{
   CNpslistptr prev,next;

   prev = T->prev;
   next = T->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (T==*pslist_listhead) *pslist_listhead = next;
   if (T==*pslist_listtail) *pslist_listtail = prev;

   /* Now delete T */
   free ((char*)T);
}


/* 
 * Delete all the plotsets in the list
 */
void CNdelete_pslist_list(pslist_listhead, pslist_listtail)
CNpslistptr *pslist_listhead, *pslist_listtail;
{
   CNpslistptr T;

   while ((T = *pslist_listhead) != NULL)
      CNdelete_pslist(pslist_listhead, pslist_listtail, T);
}


/* 
 * print out the list of plotsets 
 */
/*ARGSUSED*/
void CNprint_pslist_list(pslist_listhead, pslist_listtail)
CNpslistptr pslist_listhead, pslist_listtail;
{
   CNpslistptr PS;

   (void) fprintf(stdout,"plotset list:");
   for (PS=pslist_listhead; PS!=NULL; PS=PS->next) 
      (void) fprintf(stdout," %d", (PS->Pptr==NULL) ? -1 : PS->Pptr->ID);
   (void) fprintf(stdout,"\n");
   (void) fflush(stdout);
}

/*
 * Count the number of pslists in the list
 */
/*ARGSUSED*/
int CNcount_pslists(plot_listhead, plot_listtail)
CNpslistptr plot_listhead, plot_listtail;
{
   CNpslistptr P;
   int         count = 0;
 
   for (P=plot_listhead; P!=NULL; P=P->next) count++;
 
   return(count);
}

