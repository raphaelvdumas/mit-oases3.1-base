/*
 * read_cont.c - read contour input data from a file 
 *
 * The routines here read in the data in either binary of ascii
 * formats.  If the data is in a rectangular mesh, the mesh is fractured
 * into triangles.  In either case, the triangles are saved in the
 * "CNdataset" data-structure, pointed to by "CNdatasetptr".  The original
 * rectangular mesh is NOT saved.
 *
 * The format used is that or the "contour" program, i.e.
 *    xmin, xmax, ymin, ymax
 *    nxpts, nypts,
 *    z(0,0), z(1,0)... z(m,0)
 *    z(0,1)...
 *    Z(0,n)...     ... z(m,n)
 *
 * Note that the routines here do NOT slice the data up; all these routines
 * do is (a) open the file
 *       (b) read the data
 *       (c) arrange the data in triangles/rectangles
 *       (d) store the data.
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "CNplot.h"

static void Readerr();

/*
 * read the ascii/binary grid/3D data from a file/pipe
 * return NULL upon failure
 */
CNdatasetptr CNread_contour(filename, fp, source, threed, binary,
                            joincurve, interp, ID, verbose)
char   *filename;      /* The name of the file                 */
FILE   *fp;            /* The pipe descriptor                  */
int    source;         /* Data source : CN_FILE or CN_PIPE     */
int    threed;         /* Data in 3D format?                   */
int    binary;         /* Binary/Ascii flag (Ascii=0,Binary=1) */
int    joincurve;      /* Contour-boundary interpolation style */
int    interp;         /* Interpolate mesh midpoint            */
int    ID;             /* data ID                              */
int    verbose;        /* Verbosity flag                       */
{
   CNdatasetptr DS;

   /* Branch according to file/pipe source */
   if (source == CN_FILE) {

      /* Read from file */
      if (threed)
         DS = CNread_3Dcontour_from_file(filename, binary, ID, verbose);
      else
         DS = CNread_contour_from_file(filename, binary,
                                       joincurve, interp, ID, verbose);
   } else {

      /* Read from pipe */
      if (threed)
         DS = CNread_3Dcontour_from_pipe(fp, binary, ID, verbose);
      else
         DS = CNread_contour_from_pipe(fp, binary,
                                       joincurve, interp, ID, verbose);
    } 

   /* Return the result */
   return(DS);
}



/* 
 * read the data from a file 
 * return NULL upon failure
 */
CNdatasetptr CNread_contour_from_file(filename, binary, 
                                joincurve, interp, ID, verbose)
char   *filename;      /* The name of the file                 */
int    binary;         /* Binary/Ascii flag (Ascii=0,Binary=1) */
int    joincurve;      /* Contour-boundary interpolation style */
int    interp;         /* Interpolate mesh midpoint            */
int    ID;             /* data ID                              */
int    verbose;        /* Verbosity flag                       */
{
   FILE        *fp;
   CNdatasetptr Dptr;
   int          lineno=1;
   int          fp_ispipe=0;

   /* open the file */
   if (!CNopen_file(filename,&fp,&fp_ispipe)) return(NULL);

   /* Now process the file */
   if (!binary) 
      /* ascii data */
      Dptr = CNread_as_contour(fp,&lineno,filename,
                               joincurve,interp,ID,verbose);
   else
      /* binary data*/
      Dptr = CNread_bn_contour(fp,filename,joincurve,interp,ID,verbose);

   /* close the file */
   CNclose_file(fp, fp_ispipe);

   /* Print out warning message */
   if (Dptr == NULL) 
      (void) fprintf(stderr,
             "   ***Error! Contour-mesh file-read was unsuccessful.\n");

   /* return */
   return(Dptr);
}


/* 
 * read the data from a pipe 
 * return NULL upon failure
 */
CNdatasetptr CNread_contour_from_pipe(fp, binary, 
                                joincurve, interp, ID, verbose)
FILE   *fp;            /* The pipe-descriptor                  */
int    binary;         /* Binary/Ascii flag (Ascii=0,Binary=1) */
int    joincurve;      /* Contour-boundary interpolation style */
int    interp;         /* Interpolate mesh midpoint            */
int    ID;             /* data ID                              */
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
      Dptr = CNread_as_contour(fp,&lineno,"PIPE",
                               joincurve,interp,ID,verbose);
   else
      /* binary data*/
      Dptr = CNread_bn_contour(fp,"PIPE",joincurve,interp,ID,verbose);

   /* Print out warning message */
   if (Dptr == NULL) 
      (void) fprintf(stderr,
             "   ***Error! Contour-mesh pipe-read was unsuccessful.\n");

   /* return */
   return(Dptr);
}


/* 
 * read in the data in ascii format
 * return NULL  upon failure
 */
CNdatasetptr CNread_as_contour(fp,lineno,filename,
                               joincurve,interp_midpt,ID,verbose)
FILE   *fp;            /* File pointer                         */
int    *lineno;        /* Current line in the ascii file       */
char   *filename;      /* The name of the file (to be saved)   */
int    joincurve;      /* Contour-boundary interpolation style */
int    interp_midpt;   /* Interpolate the midpoint of the mesh */
int    ID;             /* data ID                              */
int    verbose;        /* Verbosity flag                       */
{
   CNdatasetptr CNget_rectilinear_contour_data();
   double      *CNcreate_1D_double_array(); 
 
   CNdatasetptr Dptr = NULL;
   double       *xgrid_arr=NULL, *ygrid_arr=NULL, *zgrid_arr=NULL;
   double       xmin, xmax, ymin, ymax;
   double       z, rnx, rny, dx, dy;
   int          nx, ny;
   int          i,j;

   /* Read xmin, xmax, ymin, ymax */
   if (CNrd_dbl(fp,&xmin,lineno)<=0) {CNerr_message(4,*lineno); return(NULL);}
   if (CNrd_dbl(fp,&xmax,lineno)<=0) {CNerr_message(4,*lineno); return(NULL);}
   if (CNrd_dbl(fp,&ymin,lineno)<=0) {CNerr_message(4,*lineno); return(NULL);}
   if (CNrd_dbl(fp,&ymax,lineno)<=0) {CNerr_message(4,*lineno); return(NULL);}
   if (verbose) {
      (void) fprintf(stdout,"xmin = %f  xmax = %f  ",xmin,xmax);
      (void) fprintf(stdout,"ymin = %f  ymax = %f",ymin,ymax);
      (void) fprintf(stdout,"...line=%d\n",*lineno);
   }

   /* Read the number of gridpoints */
   if (CNrd_dbl(fp,&rnx,lineno)<=0) {CNerr_message(5,*lineno); return(NULL);}
   if (CNrd_dbl(fp,&rny,lineno)<=0) {CNerr_message(5,*lineno); return(NULL);}
   nx   = (int)rnx;
   ny   = (int)rny;
   if ((nx <= 0) || (ny <= 0)) {
      (void) fprintf(stderr,
             "No of array gridpoints is less than or equal to 0 : ");
      (void) fprintf(stderr,"nx=%d ny=%d\n",nx,ny);
      return(NULL);
   }
   if (verbose)
      (void) fprintf(stdout,
             "Reading %d x %d z-values...line=%d\n",nx,ny,*lineno);

   /* allocate space for the data in a 1D array */
   xgrid_arr = CNcreate_1D_double_array(nx);
   ygrid_arr = CNcreate_1D_double_array(ny);
   zgrid_arr = CNcreate_1D_double_array(nx*ny);

   /* Fill x and y arrays */
   dx = (xmax - xmin)/(double)(nx-1);
   dy = (ymax - ymin)/(double)(ny-1);
   for (i=0; i<nx; i++) xgrid_arr[i] = xmin + i*dx;
   for (j=0; j<ny; j++) ygrid_arr[j] = ymin + j*dy;

   /* 
    * read the intensity values and save in a 1D array
    * data goes from i=0 to i=npts-1
    *   a[0,0], a[1,0], a[2,0]...
    */
   for (j=0; j<ny; j++)
   for (i=0; i<nx; i++) {
      if (!CNrd_dbl(fp,&z,lineno)) {
         CNerr_message(3,*lineno); 
         CNfree_1D_double_array(xgrid_arr);
         CNfree_1D_double_array(ygrid_arr);
         CNfree_1D_double_array(zgrid_arr);
         return(NULL);
      }
      CNset_1D_double_array_value(zgrid_arr,i+j*nx,nx*ny,&z);
   }

   /* rearrange the data into a collection of triangles */
   Dptr = CNget_rectilinear_contour_data(filename,CNstring_concat(filename),
                xgrid_arr, ygrid_arr, zgrid_arr,
                nx,ny,joincurve,interp_midpt,ID);

   /* Print out the curve */
   if (verbose) CNprint_dataset(Dptr,0);

   /* free the 1D array */
   if (Dptr == NULL) {
   CNfree_1D_double_array(xgrid_arr);
   CNfree_1D_double_array(ygrid_arr);
   CNfree_1D_double_array(zgrid_arr);
   }

   /* return */
   return(Dptr);
}


/* 
 * read in the data in binary format
 * return NULL upon failure
 */
/*ARGSUSED*/
CNdatasetptr CNread_bn_contour(fp,filename,
                               joincurve,interp_midpt,ID,verbose)
FILE   *fp;            /* File pointer                         */
char   *filename;      /* The name of the file (to be saved)   */
int    joincurve;      /* Contour-boundary interpolation style */
int    interp_midpt;   /* Interpolate the midpoint of the mesh */
int    ID;             /* data ID                              */
int    verbose;        /* Verbosity flag                       */
{
   CNdatasetptr CNget_rectilinear_contour_data();
   double      *CNcreate_1D_double_array(); 
   void         free_1D_double_array();

   CNdatasetptr Dptr = NULL;
   double       *xgrid_arr=NULL, *ygrid_arr=NULL, *zgrid_arr=NULL;

#define         NHEADER  6
   double       xmin, xmax, ymin, ymax, dx, dy;
   double       header[NHEADER];
   int          nx, ny, npoints;
   int          i, j;

   /* 
    * read header info from the file.
    * read the number of x and y points in the grid
    */
   if (fread((char *)header,sizeof(double),NHEADER,fp) != NHEADER) {
      (void) fprintf(stderr,"   ***Binary read error of header info!\n");
      exit(1);
   }
   xmin = header[0];
   xmax = header[1];
   ymin = header[2];
   ymax = header[3];
   nx   = (int)header[4];
   ny   = (int)header[5];

   /* Check the number of grid points */
   if ((nx <= 0) || (ny <= 0)) {
      (void) fprintf(stderr,
             "No of array gridpoints is less than or equal to 0 : ");
      (void) fprintf(stderr,"nx=%d ny=%d\n",nx,ny);
      return(NULL);
   }

   /* allocate space for the data in a 1D array */
   npoints = nx*ny;
   xgrid_arr = CNcreate_1D_double_array(nx);
   ygrid_arr = CNcreate_1D_double_array(ny);
   zgrid_arr = CNcreate_1D_double_array(nx*ny);

   /* Fill x and y arrays */
   dx = (xmax - xmin)/(double)(nx-1);
   dy = (ymax - ymin)/(double)(ny-1);
   for (i=0; i<nx; i++) xgrid_arr[i] = xmin + i*dx;
   for (j=0; j<ny; j++) ygrid_arr[j] = ymin + j*dy;

   /* 
    * read the intensity values and save in a 1D array
    * data goes from i=0 to i=npts-1
    */
   if (fread((char *)zgrid_arr,sizeof(double),npoints,fp) != npoints) {
      (void) fprintf(stderr,"   ***Binary read error of data array!\n");
      CNfree_1D_double_array(xgrid_arr);
      CNfree_1D_double_array(ygrid_arr);
      CNfree_1D_double_array(zgrid_arr);
      return(NULL);
   }

   /* rearrange the data into a collection of triangles */
   Dptr = CNget_rectilinear_contour_data(filename,CNstring_concat(filename),
                xgrid_arr, ygrid_arr, zgrid_arr,
                nx,ny,joincurve,interp_midpt,ID);

   /* free the 1D array */
   CNfree_1D_double_array(xgrid_arr);
   CNfree_1D_double_array(ygrid_arr);
   CNfree_1D_double_array(zgrid_arr);

   /* return */
   return(Dptr);
}



/* 
 * read the data from a file in 3D format (list of triangles)
 * return NULL upon failure
 *
 * These routines should be replaced in the future by more
 * general routines that read "pdraw" type data, i.e. lists of 3D points
 */
CNdatasetptr CNread_3Dcontour_from_file(filename, binary, ID, verbose)
char   *filename;      /* The name of the file                 */
int    binary;         /* Binary/Ascii flag (Ascii=0,Binary=1) */
int    ID;             /* data ID                              */
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
      Dptr = CNread_as_3Dcontour(fp,&lineno,filename,ID,verbose); 
   else 
      /* binary data */
      Dptr = CNread_bn_3Dcontour(fp,filename,ID,verbose); 

   /* close the file */
   CNclose_file(fp, fp_ispipe);

   /* Print out warning message */
   if (Dptr == NULL) 
      (void) fprintf(stderr,"   ***Error! 3D file-read was unsuccessful.\n");

   /* return */
   return(Dptr);
}


/* 
 * read the data from a file in 3D format (list of triangles)
 * return NULL upon failure
 *
 * These routines should be replaced in the future by more
 * general routines that read "pdraw" type data, i.e. lists of 3D points
 */
CNdatasetptr CNread_3Dcontour_from_pipe(fp, binary, ID, verbose)
FILE   *fp;            /* The pipe-descriptor                  */
int    binary;         /* Binary/Ascii flag (Ascii=0,Binary=1) */
int    ID;             /* data ID                              */
int    verbose;        /* Verbosity flag                       */
{
   CNdatasetptr Dptr;
   int          lineno=1;

   /* check the pipe */
   if (fp == NULL) {
      (void) fprintf(stderr,"Error! Cannot read from NULL pipe!\n");
      return(NULL);
   }

   /* Now process the file */
   if (!binary) 
      /* ascii data */
      Dptr = CNread_as_3Dcontour(fp,&lineno,"PIPE",ID,verbose); 
   else 
      /* binary data */
      Dptr = CNread_bn_3Dcontour(fp,"PIPE",ID,verbose); 

   /* Print out warning message */
   if (Dptr == NULL) 
      (void) fprintf(stderr,"   ***Error! 3D pipe-read was unsuccessful.\n");

   /* return */
   return(Dptr);
}


/* 
 * read in the data in ascii 3D format
 * return NULL  upon failure
 */
CNdatasetptr CNread_as_3Dcontour(fp,lineno,filename,ID,verbose)
FILE   *fp;            /* File pointer                         */
int    *lineno;        /* Current line in the ascii file       */
char   *filename;      /* The name of the file (to be saved)   */
int    ID;             /* data ID                              */
int    verbose;        /* Verbosity flag                       */
{
   CNdatasetptr CNget_triangular_contour_data();
 
   CNdatasetptr Dptr = NULL;
   CNtriaptr    triahead = NULL, triatail = NULL;
   CNnodeptr    nodehead = NULL, nodetail = NULL;
   CNnodeptr    nd0, nd1, nd2;
   CNpointptr   pointhead = NULL, pointtail = NULL;
   CNpointptr   pt;
   CNcoord      point[4];
   double       xmin, xmax, ymin, ymax, zmin, zmax;
   double       z, rncv;
   int          ncurves=0, npts=0;
   int          i=0,m=0, ptID=0, ndID=0, trID=0;

   /* read the boundary info */
   if (CNrd_dbl(fp,&xmin,lineno)<=0) {CNerr_message(4,*lineno); return(NULL);}
   if (CNrd_dbl(fp,&xmax,lineno)<=0) {CNerr_message(4,*lineno); return(NULL);}
   if (CNrd_dbl(fp,&ymin,lineno)<=0) {CNerr_message(4,*lineno); return(NULL);}
   if (CNrd_dbl(fp,&ymax,lineno)<=0) {CNerr_message(4,*lineno); return(NULL);}
   if (CNrd_dbl(fp,&zmin,lineno)<=0) {CNerr_message(4,*lineno); return(NULL);}
   if (CNrd_dbl(fp,&zmax,lineno)<=0) {CNerr_message(4,*lineno); return(NULL);}
   if (verbose) {
      (void) fprintf(stdout,"xmin = %f  xmax = %f  ",xmin,xmax);
      (void) fprintf(stdout,"ymin = %f  ymax = %f  ",ymin,ymax);
      (void) fprintf(stdout,"zmin = %f  zmax = %f",zmin,zmax);
      (void) fprintf(stdout,"...line=%d\n",*lineno);
   }

   /* Read the number of curves */
   if (CNrd_dbl(fp,&rncv,lineno)<=0)
      {Readerr(i,npts,m,ncurves,3,*lineno); return(NULL);}
   if ((ncurves = (int)rncv)     <=0)
      {Readerr(i,npts,m,ncurves,1,*lineno); return(NULL);}
   if (verbose) {
      (void) fprintf(stdout,"Reading %d curves...line=%d\n",ncurves,*lineno);
      (void) fprintf(stdout,"   Reading %d triangles from 3D dataset...\n",ncurves);
   }

   /* Reinitialize */
   zmin =  CN_LARGE;
   zmax = -CN_LARGE;

   for (m=0; m<ncurves; m++) {
      /* Reinitialize, primarily for error-checking */
      i    = 0;
      npts = 0;

      /* Read the number of points */
      if (CNrd_dbl(fp,&z,lineno)<=0)
         {Readerr(i,npts,m,ncurves,3,*lineno); return(NULL);}
      if ((npts    = (int)z)     <=0)
         {Readerr(i,npts,m,ncurves,2,*lineno); return(NULL);}
      if (verbose) (void) fprintf(stdout,"Reading %d points...line=%d\n",npts,*lineno);

      if (npts <= 0) {
         (void) fprintf(stderr,"   ***Error: Plot Points (%d) <= 0\n",npts);
         return(NULL);
      } else if (npts != 3 && npts != 4) {
         (void) fprintf(stderr,"   ***Error! 3D datasets can only consist ");
         (void) fprintf(stderr,"of 3D Triangles!\n");
         (void) fprintf(stderr,"      Cannot read %d-sided polygon...\n",npts);
         return(NULL);
      } else {
         for (i=0; i<npts; i++) {
            if (CNrd_dbl(fp,&z,lineno)<=0)
               {Readerr(i,npts,m,ncurves,3,*lineno); return(NULL);}
            point[i].x = z;
            if (CNrd_dbl(fp,&z,lineno)<=0)
               {Readerr(i,npts,m,ncurves,3,*lineno); return(NULL);}
            point[i].y = z;
            if (CNrd_dbl(fp,&z,lineno)<=0)
               {Readerr(i,npts,m,ncurves,3,*lineno); return(NULL);}
            point[i].z = z;

            if (point[i].z > zmax) zmax = point[i].z;
            if (point[i].z < zmin) zmin = point[i].z;
         }
         /* Save the triangle */
         pt   = CNinsert_point(&pointhead,&pointtail,
                               point[0].x,point[0].y,point[0].z,ptID++);
         nd0  = CNinsert_tailnode(&nodehead,&nodetail,pt,point[0].z,ndID++);
         pt   = CNinsert_point(&pointhead,&pointtail,
                               point[1].x,point[1].y,point[1].z,ptID++);
         nd1  = CNinsert_tailnode(&nodehead,&nodetail,pt,point[1].z,ndID++);
         pt   = CNinsert_point(&pointhead,&pointtail,
                               point[2].x,point[2].y,point[2].z,ptID++);
         nd2  = CNinsert_tailnode(&nodehead,&nodetail,pt,point[2].z,ndID++);
         (void)CNinsert_tria(&triahead,&triatail,nd0,nd1,nd2,0,trID++);
      }
   }
   if (verbose) {
   (void) fprintf(stdout,"   Finished reading %d triangles from 3D dataset\n",ncurves);
   (void) fflush(stdout);
   }

   /* slice and dice the triangles and place results in a linked list */
   Dptr = CNget_triangular_contour_data(filename,CNstring_concat(filename),
                xmin,xmax,ymin,ymax,zmin,zmax,
                triahead,triatail,ID);
   Dptr->pointhead = pointhead;
   Dptr->pointtail = pointtail;
   Dptr->nodehead  = nodehead;
   Dptr->nodetail  = nodetail;

   /* Print out the curve */
   if (verbose) CNprint_dataset(Dptr,0);

   /* return */
   return(Dptr);
}


/*
 * Print out an error message
 */
static void Readerr(i,npts,m,ncvs,mode,lineno)
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
CNdatasetptr CNread_bn_3Dcontour(fp,filename,ID,verbose)
FILE   *fp;            /* File pointer                         */
char   *filename;      /* The name of the file (to be saved)   */
int    ID;             /* data ID                              */
int    verbose;        /* Verbosity flag                       */
{
   (void) fprintf(stderr,"Sorry - cannt read 3D binary data yet!\n");
   return(NULL);
}

