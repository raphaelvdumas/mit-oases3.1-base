/*
 * read_plot2D.c - read 2D drawplot input data from a file 
 *
 * The routines here read in the data in either binary of ascii
 * formats.  The data is saved in the
 * "CNdataset" data-structure, pointed to by "CNdatasetptr".  
 * Each individual curve is stored in a point list.
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "CNplot.h"

static void readerr();
static void read_dataset_options();

/*
 * read the ascii/binary data from a file/pipe
 * return NULL upon failure
 */
CNdatasetptr CNread_plot2D(filename, fp, source, binary, ID, verbose)
char   *filename;      /* The name of the file                 */
FILE   *fp;            /* The pipe descriptor                  */
int    source;         /* Data source : CN_FILE or CN_PIPE     */
int    binary;         /* Binary/Ascii flag (Ascii=0,Binary=1) */
int    ID;             /* data ID                              */
int    verbose;        /* Verbosity flag                       */
{
   CNdatasetptr DS;

   /* Branch according to file/pipe source */
   if (source == CN_FILE) {

      /* Read from file */
      DS = CNread_plot2D_from_file(filename, binary, ID, verbose);

   } else {

      /* Read from pipe */
      DS = CNread_plot2D_from_pipe(fp, binary, ID, verbose);

   }

   /* Return the result */
   return(DS);
}


/*
 * read the data from a file 
 * return NULL upon failure
 */
CNdatasetptr CNread_plot2D_from_file(filename, binary, ID, verbose)
char   *filename;      /* The name of the file                 */
int    binary;         /* Binary/Ascii flag (Ascii=0,Binary=1) */
int    ID;             /* Data ID                              */
int    verbose;        /* Verbosity flag                       */
{
   FILE         *fp;
   CNdatasetptr Dptr;
   int          lineno=1;
   int          fp_ispipe=0;

   /* open the file */
   if (!CNopen_file(filename,&fp,&fp_ispipe)) return(NULL);

   /* Now process the file */
   if (!binary) 
      /* ascii data */
      Dptr = CNread_as_plot2D(fp,&lineno,filename,ID,verbose);
   else
      /* binary data*/
      Dptr = CNread_bn_plot2D(fp,filename,ID,verbose);

   /* close the file */
   CNclose_file(fp, fp_ispipe);

   /* Print out warning message */
   if (Dptr == NULL) 
      (void) fprintf(stderr,
             "   ***Error! Plot2D file-read was unsuccessful.\n");

   /* return */
   return(Dptr);
}


/* 
 * read the data from a pipe 
 * return NULL upon failure
 */
CNdatasetptr CNread_plot2D_from_pipe(fp, binary, ID, verbose)
FILE   *fp;            /* The pipe-descriptor                  */
int    binary;         /* Binary/Ascii flag (Ascii=0,Binary=1) */
int    ID;             /* Data ID                              */
int    verbose;        /* Verbosity flag                       */
{
   CNdatasetptr Dptr;
   int          lineno=1;

   /* check the pipe */
   if (fp == NULL) {
      (void) fprintf(stderr,"Error! Cannot read from NULL pipe!\n");
      return(NULL);
   }

   /* Now process the pipe */
   if (!binary) 
      /* ascii data */
      Dptr = CNread_as_plot2D(fp,&lineno,"PIPE",ID,verbose);
   else
      /* binary data*/
      Dptr = CNread_bn_plot2D(fp,"PIPE",ID,verbose);

   /* Print out warning message */
   if (Dptr == NULL) 
      (void) fprintf(stderr,
             "   ***Error! Plot2D pipe-read was unsuccessful.\n");

   /* return */
   return(Dptr);
}


/* 
 * read in the data in ascii format
 * return NULL  upon failure
 */
CNdatasetptr CNread_as_plot2D(fp, lineno, filename, ID, verbose)
FILE   *fp;            /* File pointer                         */
int    *lineno;        /* Current line in the ascii file       */
char   *filename;      /* The name of the file (to be saved)   */
int    ID;             /* Data ID                              */
int    verbose;        /* Verbosity flag                       */
{
   CNdatasetptr Dptr = NULL;
   CNcurveptr   curvehead = NULL, curvetail = NULL, Cptr = NULL;
   CNviewptr          view_pr;
   CNcurve_property   cv_property;
   CNgbcurve_property gb_property;
   CNdataset_property ds_property;
   CNplotset_property pt_property;
   double       xmin, xmax, ymin, ymax, zmin=0.0, zmax=0.0;
   double       bxmin, bxmax, bymin, bymax;
   double       x, y, z;
   int          ncurves=0, npts=0;
   int          i=0,m=0;

   bxmin =  CN_LARGE;
   bxmax = -CN_LARGE;
   bymin =  CN_LARGE;
   bymax = -CN_LARGE;

   /* Set the view parameters */
   view_pr = CNcreate_view();

   /* Set the properties of the plotset */
   CNset_default_plotset_property(&pt_property);

   /* Set the properties of the dataset */
   CNset_default_dataset_property(&ds_property);

   /* Set the properties of the curve */
   CNset_default_curve_property(&cv_property);

   /* Set the global properties of the curve */
   CNset_default_gbcurve_property(&gb_property);

   /* Read options related to this dataset */
   read_dataset_options(fp,lineno,
                        view_pr,&pt_property,&ds_property,&cv_property,
                        &gb_property, verbose);

   /* Read xmin, xmax, ymin, ymax */
   if (CNrd_dbl(fp,&xmin,lineno)<=0) {
      CNerr_message(4,*lineno); 
      CNdelete_plotset_property_fields(&pt_property);
      CNdelete_dataset_property_fields(&ds_property);
      CNdelete_gbcurve_property_fields(&gb_property);
      CNdelete_curve_property_fields(&cv_property);
      CNdelete_view(view_pr); 
      return(NULL);
   }
   if (CNrd_dbl(fp,&xmax,lineno)<=0) {
      CNerr_message(4,*lineno); 
      CNdelete_plotset_property_fields(&pt_property);
      CNdelete_dataset_property_fields(&ds_property);
      CNdelete_gbcurve_property_fields(&gb_property);
      CNdelete_curve_property_fields(&cv_property);
      CNdelete_view(view_pr); 
      return(NULL);
   }
   if (CNrd_dbl(fp,&ymin,lineno)<=0) {
      CNerr_message(4,*lineno); 
      CNdelete_plotset_property_fields(&pt_property);
      CNdelete_dataset_property_fields(&ds_property);
      CNdelete_gbcurve_property_fields(&gb_property);
      CNdelete_curve_property_fields(&cv_property);
      CNdelete_view(view_pr); 
      return(NULL);
   }
   if (CNrd_dbl(fp,&ymax,lineno)<=0) {
      CNerr_message(4,*lineno); 
      CNdelete_plotset_property_fields(&pt_property);
      CNdelete_dataset_property_fields(&ds_property);
      CNdelete_gbcurve_property_fields(&gb_property);
      CNdelete_curve_property_fields(&cv_property);
      CNdelete_view(view_pr); 
      return(NULL);
   }
   if (verbose) {
      (void) fprintf(stdout,"xmin = %f  xmax = %f  ",xmin,xmax);
      (void) fprintf(stdout,"ymin = %f  ymax = %f",ymin,ymax);
      (void) fprintf(stdout,"...line=%d\n",*lineno);
   }

   /* Read options related to this dataset */
   read_dataset_options(fp,lineno,
                        view_pr,&pt_property,&ds_property,&cv_property,
                        &gb_property, verbose);

   /* Read the number of curves */
   if (CNrd_dbl(fp,&x,lineno)<=0) {
      readerr(i,npts,m,ncurves,3,*lineno); 
      CNdelete_plotset_property_fields(&pt_property);
      CNdelete_dataset_property_fields(&ds_property);
      CNdelete_gbcurve_property_fields(&gb_property);
      CNdelete_curve_property_fields(&cv_property);
      CNdelete_view(view_pr); 
      return(NULL);
   }
   if ((ncurves = (int)x)    <=0) {
      readerr(i,npts,m,ncurves,1,*lineno); 
      CNdelete_plotset_property_fields(&pt_property);
      CNdelete_dataset_property_fields(&ds_property);
      CNdelete_gbcurve_property_fields(&gb_property);
      CNdelete_curve_property_fields(&cv_property);
      CNdelete_view(view_pr); 
      return(NULL);
   }
   if (verbose)
   (void) fprintf(stdout,"Reading %d curves...line=%d\n",ncurves,*lineno);

   /* Read and store the curves */
   for (m=0; m<ncurves; m++) {

      /* Reinitialize, primarily for error-checking */
      i    = 0;
      npts = 0;

      /* Read options related to this dataset/curve */
      read_dataset_options(fp,lineno,
                           view_pr,&pt_property,&ds_property,&cv_property,
                           &gb_property, verbose);

      /* Read the number of points */
      if (CNrd_dbl(fp,&x,lineno)<=0) {
         readerr(i,npts,m,ncurves,3,*lineno); 
         CNdelete_plotset_property_fields(&pt_property);
         CNdelete_dataset_property_fields(&ds_property);
         CNdelete_gbcurve_property_fields(&gb_property);
         CNdelete_curve_property_fields(&cv_property);
         CNdelete_view(view_pr); 
         return(NULL);
      }
      if ((npts    = (int)x)    <=0) {
         readerr(i,npts,m,ncurves,2,*lineno); 
         CNdelete_plotset_property_fields(&pt_property);
         CNdelete_dataset_property_fields(&ds_property);
         CNdelete_gbcurve_property_fields(&gb_property);
         CNdelete_curve_property_fields(&cv_property);
         CNdelete_view(view_pr); 
         return(NULL);
      }
      if (verbose) 
      (void) fprintf(stdout,"Reading %d points...line=%d\n",npts,*lineno);

      /* Read options related to this dataset/curve */
      read_dataset_options(fp,lineno,
                           view_pr,&pt_property,&ds_property,&cv_property,
                           &gb_property, verbose);

      /* Allocate a curve data structure */
      Cptr = CNinsert_curve(&curvehead, &curvetail,m);

      /* Apply the curve options to the curve */
      CNset_curve_property(&(Cptr->curv_pr),&cv_property);

      /* Reset the properties of the curve */
      CNdelete_curve_property_fields(&cv_property);
      CNset_default_curve_property(&cv_property);

      /* Read curve points */
      for (i=0; i<npts; i++) {
         if (CNrd_dbl(fp,&x,lineno)<=0) {
            readerr(i,npts,m,ncurves,3,*lineno); 
            CNdelete_plotset_property_fields(&pt_property);
            CNdelete_dataset_property_fields(&ds_property);
            CNdelete_gbcurve_property_fields(&gb_property);
            CNdelete_curve_property_fields(&cv_property);
            CNdelete_view(view_pr); 
            return(NULL);
         }
         if (CNrd_dbl(fp,&y,lineno)<=0) {
            readerr(i,npts,m,ncurves,3,*lineno); 
            CNdelete_plotset_property_fields(&pt_property);
            CNdelete_dataset_property_fields(&ds_property);
            CNdelete_gbcurve_property_fields(&gb_property);
            CNdelete_curve_property_fields(&cv_property);
            CNdelete_view(view_pr); 
            return(NULL);
         }
         if (x < bxmin) bxmin = x;
         if (x > bxmax) bxmax = x;
         if (y < bymin) bymin = y;
         if (y > bymax) bymax = y;
         z = 0.0;
         (void)CNinsert_point(&(Cptr->pointhead),&(Cptr->pointtail),x,y,z,i);
      }
   }

   /* Set zmax-zmin to the smaller of (xmax-xmin, ymax-ymin) */
   if (fabs(ymax-ymin) > fabs(xmax-xmin)) 
      zmax = 0.5*fabs(xmax-xmin);
   else
      zmax = 0.5*fabs(ymax-ymin);
   zmin = -zmax;
   
   /* Store the entire dataset in a dataset */
   Dptr = CNmake_dataset(filename,CNstring_concat(filename),CN_PLOT2D,
                         bxmin,bxmax,bymin,bymax,zmin,zmax,
                         xmin,xmax,ymin,ymax,zmin,zmax,ID);
   if (Dptr != NULL) {
      Dptr->curvehead = curvehead;
      Dptr->curvetail = curvetail;

      /* Apply the view options to the dataset */
      CNset_view_property(Dptr->view_pr,view_pr);

      /* Apply the plotset options to the dataset */
      CNset_plotset_property(&(Dptr->plot_pr),&pt_property);

      /* Apply the dataset options to the dataset */
      CNset_dataset_property(&(Dptr->data_pr),&ds_property);

      /* Apply the global properties to the individual curves */
      if (gb_property.flag != 0)
      CNreset_curves(Dptr->curvehead, Dptr->curvetail, &gb_property, 0);

      /* Print out the dataset */
      if (verbose) CNprint_dataset(Dptr, 0);
   }

   /* Reset the property structures */
   CNdelete_plotset_property_fields(&pt_property);
   CNdelete_dataset_property_fields(&ds_property);
   CNdelete_gbcurve_property_fields(&gb_property);
   CNdelete_curve_property_fields(&cv_property);

   /* Free the view structure */
   CNdelete_view(view_pr);

   /* return */
   return(Dptr);
}


/*
 * Set the options for the dataset 
 */
static void read_dataset_options(fp,lineno,view_pm,pt_prop,ds_prop,cv_prop,
                                 gb_prop,vbs)
FILE *fp;
int  *lineno;
CNviewptr          view_pm;
CNplotset_property *pt_prop;
CNdataset_property *ds_prop;
CNcurve_property   *cv_prop;
CNgbcurve_property *gb_prop;
int                vbs;
{
   char *argtbl[CN_MAXWORDS], *valtbl[CN_MAXWORDS];
   char line[CN_MAXCHAR];
   char newheader[CN_MAXCHAR];
   int  nargs = 0, nvals = 0;
   int  i;
   int  status;

   while ((status=CNread_option(fp,lineno,line,CN_MAXCHAR))!=EOF && status) {
      
      /* CNparse_line wants "command arg=val arg=val" so create a new header */
      (void) sprintf(newheader, "PLOT2D %s",line);

      /* Get the argument-value pairs from the line */
      if (CNparse_line(newheader, CN_MAXCHAR,
                       &nargs, argtbl, CN_MAXWORDS,
                       &nvals, valtbl, CN_MAXWORDS)) {

         /* Look for plotset/dataset/curve arguments */
         for (i=0; i<nargs; i++) {
            if (!CNparse_view_property      (view_pm,argtbl[i],valtbl[i],vbs))
             if (!CNparse_plotset_property  (pt_prop,argtbl[i],valtbl[i],vbs))
              if (!CNparse_dataset_property (ds_prop,argtbl[i],valtbl[i],vbs))
               if (!CNparse_gbcurve_property(gb_prop,argtbl[i],valtbl[i],vbs))
                if (!CNparse_curve_property (cv_prop,argtbl[i],valtbl[i],vbs))
                  (void) fprintf(stderr,"warning : Invalid option \"%s=%s\"\n",
                                argtbl[i],valtbl[i]);
         }

         /* Clear the tables */
         CNfreewords(&nargs, argtbl);
         CNfreewords(&nvals, valtbl);
      }
   }
}


/*
 * Print out an error message
 */
static void readerr(i,npts,m,ncvs,mode,lineno)
int i,npts,m,ncvs,mode,lineno;
{
   (void) fprintf(stderr,
   "Incorrect data at data-point #%d (of %d) in curve #%d (of %d)\n",
   i+1,npts,m+1,ncvs);

   /* This would come only at the beginning of the read */
   if (mode==3) {
      if (ncvs==0) (void) fprintf(stderr,"Expecting No. of curves...\n");
      if (npts==0) (void) fprintf(stderr,"Expecting No. of points in curve %d...\n",m);
      if (m<=ncvs && ncvs!=0) (void) fprintf(stderr,"Expecting more curves...\n");
      if (i<=npts && npts!=0) (void) fprintf(stderr,"Expecting more data points...\n");
   }
   CNerr_message(mode,lineno);
}


/* 
 * read in the data in binary format
 * return NULL upon failure
 */
/*ARGSUSED*/
CNdatasetptr CNread_bn_plot2D(fp, filename, ID, verbose)
FILE   *fp;            /* File pointer                         */
char   *filename;      /* The name of the file (to be saved)   */
int    ID;             /* Data ID                              */
int    verbose;        /* Verbosity flag                       */
{
#define         NHEADER  5
   CNdatasetptr Dptr = NULL;
   CNcurveptr   curvehead = NULL, curvetail = NULL, Cptr = NULL;
   double       x,y,z;
   double       xmin, xmax, ymin, ymax, zmin=0.0, zmax=0.0;
   double       bxmin, bxmax, bymin, bymax;
   double       header[NHEADER];
   double       *arrptr;
   int          ncv, npts;
   int          i, m;

   /* Initialize boundary values */
   bxmin =  CN_LARGE;
   bxmax = -CN_LARGE;
   bymin =  CN_LARGE;
   bymax = -CN_LARGE;

   /* 
    * read header info from the file.
    * read the number of x and y points in the grid
    */
   if (fread((char *)header,sizeof(double),NHEADER,fp) != NHEADER) {
      (void) fprintf(stderr,"   ***Binary read error of header info!\n");
      return(NULL);
   }
   xmin = header[0];
   xmax = header[1];
   ymin = header[2];
   ymax = header[3];
   ncv  = (int)header[4];

   /* Check number of curves */
   if (ncv <= 0) {
      (void) fprintf(stderr,"No of plot curves is less than or equal to 0 : ");
      (void) fprintf(stderr,"ncurves=%d\n",ncv);
      return(NULL);
   }

   /*
    * Read the points and store them
    */
   for (m=0; m<ncv; m++) {
      /* Allocate a curve data structure */
      Cptr = CNinsert_curve(&curvehead, &curvetail,m);

      /* Read the number of points */
      if (fread((char *)header,sizeof(double),1,fp) != 1) {
         (void) fprintf(stderr,"   ***Binary read error of header info (npts)!\n");
         return(NULL);
      }

      /* Check number of points */
      npts = (int)header[0];
      if (npts <= 0) {
         (void) fprintf(stderr,"No of plot points is less than or equal to 0 : ");
         (void) fprintf(stderr,"npoints=%d\n",npts);
         return(NULL);
      }

      /* allocate space for the data in a 1D array */
      arrptr  = CNcreate_1D_double_array(npts*2);

      /* Read the x-y values and store them in a temp array */
      if (fread((char *)arrptr,sizeof(double),npts*2,fp) != npts*2) {
         (void) fprintf(stderr,"   ***Binary read error of data array!\n");
         return(NULL);
      }

      /* Convert and store the real data points */
      for (i=0; i<npts*2; i=i+2) {
         x = arrptr[i];
         y = arrptr[i+1];
         if (x < bxmin) bxmin = x;
         if (x > bxmax) bxmax = x;
         if (y < bymin) bymin = y;
         if (y > bymax) bymax = y;
         z = 0.0;
         (void)CNinsert_point(&(Cptr->pointhead),&(Cptr->pointtail),x,y,z,i/2);
      }

      /* free the 1D array */
      CNfree_1D_double_array(arrptr);
   }

   /* Set zmax-zmin to the smaller of (xmax-xmin, ymax-ymin) */
   if (fabs(ymax-ymin) > fabs(xmax-xmin)) 
      zmax = 0.5*fabs(xmax-xmin);
   else
      zmax = 0.5*fabs(ymax-ymin);
   zmin = -zmax;

   /* Store the entire dataset in a dataset */
   Dptr = CNmake_dataset(filename,CNstring_concat(filename),CN_PLOT2D,
                bxmin,bxmax,bymin,bymax,zmin,zmax,
                xmin,xmax,ymin,ymax,zmin,zmax,ID);
   Dptr->curvehead = curvehead;
   Dptr->curvetail = curvetail;

   /* return */
   return(Dptr);
}

