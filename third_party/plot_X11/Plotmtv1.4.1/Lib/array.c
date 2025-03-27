/*
 * array.c - subroutines to dynamically allocate space for an array 
 */

/*
 * 2D array elements are stored by row then column, i.e.
 *   x[0,0], x[1,0], x[2,0]...
 *   x[0,1], x[1,1], x[2,1]...
 * This is to enable binary read of 2D array
 * Note that normally x[i][j] = x[0][0] + i*ny + j,
 * so the routines in this file are transposed, i.e.
 *                    x[i][j] = x[0][0] + j*nx + i,
 */

#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include "CNplot.h"

/**********************************************************/
/***           Routines for 1D "int" arrays             ***/
/**********************************************************/

/* 
 * Allocate room for nsize elements of type int in a 1D array 
 */
int *CNcreate_1D_int_array(size)
int size;
{
   int *newptr;
   unsigned int arr_size;
   
   /* figure out total number of elements */
   arr_size = (unsigned)(size * sizeof(int));
   
   /* allocate space using malloc(), and return a pointer */
   if ( (newptr = (int *)malloc(arr_size)) == NULL) {
     (void) fprintf(stderr,"Out of memory\n");
     exit(-1);
   }

   /* return the pointer */
   return(newptr);
}


/* 
 * set the value of an array element
 */
void CNset_1D_int_array_value(arrptr,i,isize,val)
int *arrptr,*val;
int i,isize;
{
   if (i<0 || i>=isize) {
      (void) fprintf(stderr,"Int Element [%d] is out of bounds!\n",i);
      exit(-1);
   }
   *(arrptr + i) = *val;
}


/*
 * get the value of an array element
 */
int CNget_1D_int_array_value(arrptr,i,isize)
int *arrptr;
int i,isize;
{
   int val;
   if (i<0 || i>=isize) {
      (void) fprintf(stderr,"Int Element [%d] is out of bounds!\n",i);
      exit(-1);
   }
   val = *(arrptr + i);
   return(val);
}


/*
 * free the array
 */
void CNfree_1D_int_array(arrptr)
int *arrptr;
{
   /* free the array */
   free((char *)arrptr);
}


/*
 * find the min and max values of the array
 */
void CNget_1D_int_array_maxmin(arrptr,isize,min,max)
int *arrptr;
int    isize;
int *min, *max;
{
   int CNget_1D_int_array_value();
   int z, z0=0;
   int i;

   /*
   *min =  CN_LARGE;
   *max = -CN_LARGE;
    */

   /* First element in the array */
   if (isize > 0) z0 = CNget_1D_int_array_value(arrptr,0,isize);

   /* Initialize to the first element */
   *min = z0;
   *max = z0;
   for (i=0; i<isize; i++) {
      z = CNget_1D_int_array_value(arrptr,i,isize);
      if (z < *min) *min = z;
      if (z > *max) *max = z;
   }
}


/*************************************************************/
/***           Routines for 1D "double" arrays             ***/
/*************************************************************/

/* 
 * Allocate room for nsize elements of type double in a 1D array 
 */
double *CNcreate_1D_double_array(size)
int size;
{
   double *newptr;
   unsigned int arr_size;
   
   /* figure out total number of elements */
   arr_size = (unsigned)(size * sizeof(double));
   
   /* allocate space using malloc(), and return a pointer */
   if ( (newptr = (double *)malloc(arr_size)) == NULL) {
     (void) fprintf(stderr,"Out of memory\n");
     exit(-1);
   }

   /* return the pointer */
   return(newptr);
}


/* 
 * set the value of an array element
 */
void CNset_1D_double_array_value(arrptr,i,isize,val)
double *arrptr,*val;
int i,isize;
{
   if (i<0 || i>=isize) {
      (void) fprintf(stderr,"Double Element [%d] is out of bounds!\n",i);
      exit(-1);
   }
   *(arrptr + i) = *val;
}


/*
 * get the value of an array element
 */
double CNget_1D_double_array_value(arrptr,i,isize)
double *arrptr;
int i,isize;
{
   double val;
   if (i<0 || i>=isize) {
      (void) fprintf(stderr,"Double Element [%d] is out of bounds!\n",i);
      exit(-1);
   }
   val = *(arrptr + i);
   return(val);
}


/*
 * free the array
 */
void CNfree_1D_double_array(arrptr)
double *arrptr;
{
   /* free the array */
   free((char *)arrptr);
}


/*
 * find the min and max values of the array
 */
void CNget_1D_double_array_maxmin(arrptr,isize,min,max)
double *arrptr;
int    isize;
double *min, *max;
{
   double CNget_1D_double_array_value();
   double z;
   int    i;

   *min =  CN_LARGE;
   *max = -CN_LARGE;
   for (i=0; i<isize; i++) {
      z = CNget_1D_double_array_value(arrptr,i,isize);
      if (z < *min) *min = z;
      if (z > *max) *max = z;
   }
}


/*************************************************************/
/***           Routines for 2D "double" arrays             ***/
/*************************************************************/

/* 
 * Allocate room for nsize elements of type double in a 2D array 
 */
double *CNcreate_2D_double_array(isize,jsize)
int isize, jsize;
{
   double *newptr;
   unsigned int arr_size;
   
   /* figure out total number of elements */
   arr_size = (unsigned)(isize*jsize)*sizeof(double);
   
   /* allocate space using malloc(), and return a pointer */
   if ( (newptr = (double *)malloc(arr_size)) == NULL) {
     (void) fprintf(stderr,"Out of memory\n");
     exit(-1);
   }

   /* return the pointer */
   return(newptr);
}


/* 
 * set the value of an array element
 */
void CNset_2D_double_array_value(arrptr,i,j,isize,jsize,val)
double *arrptr,*val;
int i,isize;
{
   if (i<0 || i>isize-1 || j<0 || j>jsize-1) {
      (void) fprintf(stderr,
      "Error: [%d][%d]-th array element is inaccessible\n", i,j);
      exit(-1);
   }

   *(arrptr + i*jsize + j) = *val;
}


/*
 * get the value of an array element
 */
double CNget_2D_double_array_value(arrptr,i,j,isize,jsize)
double *arrptr;
int    i,j,isize,jsize;
{
   double val;

   if (i<0 || i>isize-1 || j<0 || j>jsize-1) {
      (void) fprintf(stderr,
      "Error: [%d][%d]-th array element is inaccessible\n", i,j);
      exit(-1);
   }

   val = *(arrptr + i*jsize + j);
   return(val);
}


/*
 * free the array
 */
void CNfree_2D_double_array(arrptr)
double *arrptr;
{
   /* free the array */
   free((char *)arrptr);
}


/*
 * find the min and max values of the array
 */
void CNget_2D_double_array_maxmin(arrptr,isize,jsize,min,max)
double *arrptr;
int    isize,jsize;
double *min, *max;
{
   double CNget_2D_double_array_value();
   double z;
   int    i,j;

   *min =  CN_LARGE;
   *max = -CN_LARGE;
   for (i=0; i<isize; i++) 
   for (j=0; j<jsize; j++) {
      z = CNget_2D_double_array_value(arrptr,i,j,isize,jsize);
      if (z < *min) *min = z;
      if (z > *max) *max = z;
   }
}

 

/*************************************************************/
/***           Routines for 2D "pointptr" arrays           ***/
/*************************************************************/

/*
 * assign pointptr array[i][j]
 * isize, jsize are the maximum dimensions of the 2D array.
 */
CNpointptr *CNcreate_pointptr_array(isize,jsize)
int isize, jsize;
{
   CNpointptr  *newptr;
   unsigned int arr_size;

   /* figure out total no of elements */
   arr_size = (unsigned)(isize*jsize)*sizeof(CNpointptr);

   /* allocate space using malloc(), and return a pointer */
   if ( (newptr = (CNpointptr *)malloc(arr_size)) == NULL) {
      (void) fprintf(stderr,"Out of memory\n");
      exit(-1);
   }

   /* return the pointer */
   return(newptr);
}

/* 
 * assign a value to the array 
 */
void CNset_pointptr_array_value(arrptr,i,j,isize,jsize,val)
CNpointptr *arrptr,*val;
int       i,j,isize,jsize;
{
   if (i<0 || i>isize-1 || j<0 || j>jsize-1) {
      (void) fprintf(stderr,
      "Error: [%d][%d]-th array element is inaccessible\n", i,j);
      exit(-1);
   }

   *(arrptr + i*jsize + j) = *val;
}

/* 
 * get a value to the array 
 */
CNpointptr CNget_pointptr_array_value(arrptr,i,j,isize,jsize)
CNpointptr *arrptr;
int       i,j,isize,jsize;
{
   CNpointptr val;

   if (i<0 || i>isize-1 || j<0 || j>jsize-1) {
      (void) fprintf(stderr,
      "Error: [%d][%d]-th array element is inaccessible\n", i,j);
      exit(-1);
   }

   val = *(arrptr + i*jsize + j);
   return(val);
}

/*
 * Free the pointptr array 
 */
void CNfree_pointptr_array(arrptr)
CNpointptr *arrptr;
{
   /* free the array */
   free((char *)arrptr);
}


/*************************************************************/
/***           Routines for 2D "nodeptr" arrays            ***/
/*************************************************************/

/*
 * assign nodeptr array[i][j]
 * isize, jsize are the maximum dimensions of the 2D array.
 */
CNnodeptr *CNcreate_nodeptr_array(isize,jsize)
int isize, jsize;
{
   CNnodeptr  *newptr;
   unsigned int arr_size;

   /* figure out total no of elements */
   arr_size = (unsigned)(isize*jsize)*sizeof(CNnodeptr);

   /* allocate space using malloc(), and return a pointer */
   if ( (newptr = (CNnodeptr *)malloc(arr_size)) == NULL) {
      (void) fprintf(stderr,"Out of memory\n");
      exit(-1);
   }

   /* return the pointer */
   return(newptr);
}

/* 
 * assign a value to the array 
 */
void CNset_nodeptr_array_value(arrptr,i,j,isize,jsize,val)
CNnodeptr *arrptr,*val;
int       i,j,isize,jsize;
{
   if (i<0 || i>isize-1 || j<0 || j>jsize-1) {
      (void) fprintf(stderr,
      "Error: [%d][%d]-th array element is inaccessible\n", i,j);
      exit(-1);
   }

   *(arrptr + i*jsize + j) = *val;
}

/* 
 * get a value to the array 
 */
CNnodeptr CNget_nodeptr_array_value(arrptr,i,j,isize,jsize)
CNnodeptr *arrptr;
int       i,j,isize,jsize;
{
   CNnodeptr val;

   if (i<0 || i>isize-1 || j<0 || j>jsize-1) {
      (void) fprintf(stderr,
      "Error: [%d][%d]-th array element is inaccessible\n", i,j);
      exit(-1);
   }

   val = *(arrptr + i*jsize + j);
   return(val);
}

/*
 * Free the nodeptr array 
 */
void CNfree_nodeptr_array(arrptr)
CNnodeptr *arrptr;
{
   /* free the array */
   free((char *)arrptr);
}



/*************************************************************/
/***           Routines for 2D "segmptr" arrays            ***/
/*************************************************************/

/*
 * assign segmptr array[i][j]
 * isize, jsize are the maximum dimensions of the 2D array.
 */
CNsegmptr *CNcreate_segmptr_array(isize,jsize)
int isize, jsize;
{
   CNsegmptr  *newptr;
   unsigned int arr_size;

   /* figure out total no of elements */
   arr_size = (unsigned)(isize*jsize)*sizeof(CNsegmptr);

   /* allocate space using malloc(), and return a pointer */
   if ( (newptr = (CNsegmptr *)malloc(arr_size)) == NULL) {
      (void) fprintf(stderr,"Out of memory\n");
      exit(-1);
   }

   /* return the pointer */
   return(newptr);
}

/* 
 * assign a value to the array 
 */
void CNset_segmptr_array_value(arrptr,i,j,isize,jsize,val)
CNsegmptr *arrptr,*val;
int       i,j,isize,jsize;
{
   if (i<0 || i>isize-1 || j<0 || j>jsize-1) {
      (void) fprintf(stderr,
      "Error: [%d][%d]-th array element is inaccessible\n", i,j);
      exit(-1);
   }

   *(arrptr + i*jsize + j) = *val;
}

/* 
 * get a value to the array 
 */
CNsegmptr CNget_segmptr_array_value(arrptr,i,j,isize,jsize)
CNsegmptr *arrptr;
int       i,j,isize,jsize;
{
   CNsegmptr val;

   if (i<0 || i>isize-1 || j<0 || j>jsize-1) {
      (void) fprintf(stderr,
      "Error: [%d][%d]-th array element is inaccessible\n", i,j);
      exit(-1);
   }

   val = *(arrptr + i*jsize + j);
   return(val);
}

/*
 * Free the segmptr array 
 */
void CNfree_segmptr_array(arrptr)
CNsegmptr *arrptr;
{
   /* free the array */
   free((char *)arrptr);
}

