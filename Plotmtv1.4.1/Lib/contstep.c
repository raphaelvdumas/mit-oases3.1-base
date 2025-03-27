/*
 * contstep.c - select a contour step size given min and max
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "CNplot.h"

void   CNselect_contour_step();
void   CNselect_cstep();
static void use_const_cstep();
static void calculate_const_cstep();
static void use_const_nsteps();
static void calculate_const_nsteps();
static void calculate_log_steps();
static void fill_logctr_array();


/*
 * Select the optimum values of contour increments.
 * If stepmethod=logzsteps, then use calculate_log_steps() to select the 
 * contour increment.
 */
void CNselect_contour_step(zmin,zmax,
                           cn_stepmethod,cn_cstep,cn_nsteps,cn_logzstep,
                           cstephead,csteptail,verbose)
double        *zmin, *zmax;  /* Take contours between these limits */
short         cn_stepmethod; /* Contour step-size method           */
double        *cn_cstep;     /* Contour step size                  */
int           *cn_nsteps;    /* No of contour steps                */
int           cn_logzstep;   /* Number of contours per decade      */
CNcontstepptr *cstephead;    /* Linked List of contour steps       */
CNcontstepptr *csteptail;    /* Linked List of contour steps       */
int        verbose;
{
   void (*func_ptr)();

   func_ptr = (cn_stepmethod == CN_LOGSTEPS) ? calculate_log_steps : NULL;

   CNselect_cstep(zmin, zmax,
                  cn_stepmethod, cn_cstep, cn_nsteps, cn_logzstep,
                  func_ptr, cstephead, csteptail, verbose); 
}


/*
 * Select the optimum values of contour increments.
 * There should be about 10 contours - round cstep up to its
 * nearest decimal place (0.0156 -> 0.02, 0.014 -> 0.01)
 *
 * The contours are specified either by step-size or number-of-steps.
 * "stepmethod" defines the contour step style.
 * In either case, check and reset cstep/nsteps as necessary.
 * 
 * Return the results in an linked list
 */
void CNselect_cstep(zmin, zmax,
                    cn_stepmethod, cn_cstep, cn_nsteps, cn_logzstep,
                    cfunc, cstephead, csteptail, verbose)
double     *zmin, *zmax;     /* Take contours between these limits */
int        cn_stepmethod;    /* Contour step-size method           */
double     *cn_cstep;        /* Contour step size                  */
int        *cn_nsteps;       /* No of contour steps                */
int        cn_logzstep;      /* logz Contour step size             */
void       (*cfunc)();       /* Function for calculating contour   */
                             /* increments                         */
CNcontstepptr *cstephead;    /* Linked List of contour steps       */
CNcontstepptr *csteptail;    /* Linked List of contour steps       */
int    verbose;
{
   /*
    * There are 3 methods for calculating the contour step size.
    * These are :
    *      cn_stepmethod = CN_STEPSIZE (default)
    *      cn_stepmethod = CN_NUMSTEPS          
    *      cfunc         != NULL 
    *
    * If the user does not supply a function to calculate the
    * contour steps then the following logic applies:
    *
    *    If cn_stepmethod=CN_STEPSIZE then use a constant contour 
    *    step size of cstep.  This results in equally spaced contour 
    *    increments inside the given [zmin,zmax] limits.  Note that 
    *    the smallest contour is not necessarily equal to zmin, 
    *    i.e. cmin >= zmin, and cmax <= zmax.  This is necessary 
    *    to avoid contours with strange non-rounded values.
    *    For example, if zmin=0.51 and zmax=0.92 and cstep=0.1, then 
    *    we should get contours of 0.5, 0.6, 0.7, 0.8 and 0.9.
    *
    *    If on the other hand a given number of contour steps is 
    *    selected, i.e. cn_stepmethod=CN_NUMSTEPS, use nsteps contour
    *    steps.  Here the contours are selected inclusive of zmin, zmax,
    *    e.g. [0.51, 0.92] with 2 steps results in contours at 0.51, 0.92
    *    and 0.715.  If the user desires rounded numbers it is his 
    *    responsibility to determine and set the min and max contour levels.
    *
    * The contour increments can also be selected using the cfunc()
    * function.  For example, 
    *         zmin = 0.0
    *         zmax = 100.0
    *         contours of 0.0, 1.0, 10.0, 100.0 are desired.
    *         then clearly cfunc(n)=exp10(n), where n is the contour
    *         number.
    *         More generally, 
    *              cn = cfunc(n,zmin,zmax) 
    *                 = round(zmin) + exp10(n)
    *         In this case, the user is responsible for rounding zmin,zmax
    * The contour-function has priority over the first 2 methods of 
    * contour selection. So, even if cstep is chosen, and cfunc!=NULL,
    * the routine will use cfunc to calculate the contours.
    */    

   CNcontstepptr C;
   double        min, max, cstep;
   int           nsteps, i;
   int           CONT_FUNC=CN_FALSE;

   if (verbose) {
      (void) fprintf(stdout,"      cmin        = %g\n",*zmin);
      (void) fprintf(stdout,"      cmax        = %g\n",*zmax);
      if (cfunc != NULL) {
      (void) fprintf(stdout,"      Step method = %s\n","User-defined");
      } else {
      (void) fprintf(stdout,"      Step method = %d\n",cn_stepmethod);
      (void) fprintf(stdout,"        step size = %g\n",*cn_cstep);
      (void) fprintf(stdout,"        num steps = %d\n",*cn_nsteps);
      }
      (void) fprintf(stdout,"        logzstep  = %d\n",cn_logzstep);
   }

   /* Initialize local variables */
   max    = *zmax;
   min    = *zmin;
   cstep  = *cn_cstep;
   nsteps = *cn_nsteps;

   /* Interchange min and max if necessary */
   if (max < min) {
      (void) fprintf(stderr,
                     "   ***Error: contour-max < contour-min!\n");
      (void) fprintf(stderr,
                     "      Interchanging max and min contour limits...\n");
      max = *zmin;
      min = *zmax; 
      *zmin = min;
      *zmax = max;
   }

   /* If min == max then don't do anything */
   if (fabs(max - min) < CN_SMALL) {
      (void) fprintf(stderr,
                     "   ***Error: contour-max = contour-min!\n");
      (void) fprintf(stderr,
                     "      Cannot take contours of a horizontal plane!\n");
      return;
   }

   if (cfunc != NULL) {
      /* 
       * If the contour-function is specified, use it.
       * if this fails, ncontours will be zero, and the linear routines
       * will be used to select the contour increments based on 
       * whether cstep or nsteps are defined.
       */
      CONT_FUNC=CN_FALSE;
      if (verbose) (void) fprintf(stdout,"\nUsing user-defined function...");
      (*cfunc)(min,max,cn_logzstep,cstephead,csteptail);
      if ((*cstephead) != NULL) {
         CONT_FUNC=CN_TRUE;
      } else {
         (void) fprintf(stdout,
                 "Warning - No contours found using user-defined function!\n");
         (void) fprintf(stdout,
                 "Recalculating using constant-step size contours...\n");
      }
   }

   if (CONT_FUNC==CN_FALSE) {
      /*
       * Select the contour steps in linear increments
       */
      if (cn_stepmethod == CN_STEPSIZE) {
         /*
          * use constant contour step size 
          */
         use_const_cstep(min,max,&cstep,&nsteps,cstephead,csteptail);
         *cn_cstep = cstep;
      } else {
         /*
          * use constant number of contour steps 
          * and calculate cstep.
          */
         use_const_nsteps(min,max,&cstep,&nsteps,cstephead,csteptail);
         *cn_nsteps = nsteps;
      }
   }

   /* Print out the contour info */
   if (verbose) {
   (void) fprintf(stdout,"\n");
   (void) fprintf(stdout,"      Contours will be calculated");
   (void) fprintf(stdout," between the following limits:\n");
   (void) fprintf(stdout,"         Maximum value             = %.5g\n",max);
   (void) fprintf(stdout,"         Minimum value             = %.5g\n",min);
   if (cn_stepmethod==CN_LOGSTEPS) {
   (void) fprintf(stdout,"      Using log10 contour steps...\n");
   (void) fprintf(stdout,"         Contours per decibel      = %d\n",
                  cn_logzstep);
   } else if (CONT_FUNC == CN_TRUE) {
   (void) fprintf(stdout,"      Using function-valued contour steps...\n");
   (void) fprintf(stdout,"         Contour step size         = %.5g\n",cstep);
   } else {
   (void) fprintf(stdout,"      Using constant-sized contour steps...\n");
   (void) fprintf(stdout,"         Contour step size         = %.5g\n",cstep);
   }
   (void) fprintf(stdout,"         Number of contours        = %d\n",
                  CNcount_contsteps(*cstephead, *csteptail));
   (void) fprintf(stdout,"         Smallest-valued contour   = %.5g\n",
                  (*cstephead)->value);
   (void) fprintf(stdout,"         Largest -valued contour   = %.5g\n",
                  (*csteptail)->value);
   }

   if (verbose==2) {
      (void) fprintf(stdout,"\n   Contour steps are:\n");
      i = 0;
      for (C=(*cstephead); C!=NULL; C=C->next)
         (void) fprintf(stdout," i=%d  value=%g\n",i++,C->value);
   }
}

/*
 * Select the contour step size given the constant contour increment
 */
static void use_const_cstep(min,max,cstep,nsteps,cstephead,csteptail)
double        min, max;
double        *cstep;
int           *nsteps;
CNcontstepptr *cstephead;    /* Linked List of contour steps       */
CNcontstepptr *csteptail;    /* Linked List of contour steps       */
{
   int recalc=CN_FALSE;

   /*
    * If the step size is too small then use 10 steps 
    */
   if (*cstep < CN_SMALL) {
      (void) fprintf(stdout,
             "   ***Warning : Contour step size (%g) is too small!\n",*cstep);
      (void) fprintf(stdout,"   Recalculating the contour step size...\n");
      if (*nsteps <=0) *nsteps = CN_IDLSTEPS;
      *cstep = CNround_to_decimal((max - min)/(double)(*nsteps));
   } 

   /*
    * if the contour step size has already been defined, check the
    * number of contour steps.
    */
   *nsteps = (int)((max-min)/(*cstep));
   recalc  = CN_FALSE;
   if (*nsteps < 1) {
      (void) fprintf(stdout,
             "   ***Warning : Too few contours %d\n",*nsteps);
      recalc = CN_TRUE;
   } else if (*nsteps >= CN_MAXSTEPS) {
      (void) fprintf(stdout,
             "   ***Warning : Too many contours %d\n",*nsteps);
      (void) fprintf(stdout,
             "      Maximum no of contour steps is %d\n",CN_MAXSTEPS-1);
      recalc = CN_TRUE;
   } else {
      recalc = CN_FALSE;
   }

   /* 
    * Recalculate the contour step size if necessary 
    */
   if (recalc) {
      (void) fprintf(stdout,"   Recalculating the contour step size...\n");
      *nsteps = (*nsteps >= CN_MAXSTEPS) ? CN_MAXSTEPS-1 : CN_IDLSTEPS;
      *cstep  = CNround_to_decimal( (max - min)/(double)(*nsteps));
      *nsteps = (int) ((max - min)/(*cstep)) + 1;

      while (*nsteps >= CN_MAXSTEPS) {
         *nsteps = *nsteps/2;
         *cstep  = CNround_to_decimal( (max - min)/(double)(*nsteps));
         *nsteps = (int) ((max - min)/(*cstep)) + 1;
      }
   }

   /* Calculate the contour increments */
   calculate_const_cstep(min,max,*cstep,cstephead,csteptail);
}

/*
 * Use a linear function to calculate the contour steps using a
 * constant step size
 */
static void calculate_const_cstep(min,max,cstep,cstephead,csteptail)
double        min, max;         /* Take contours between these limits */
double        cstep;            /* Contour step size                  */
CNcontstepptr *cstephead;    /* Linked List of contour steps       */
CNcontstepptr *csteptail;    /* Linked List of contour steps       */
{
   double level;

   /*
    * find the contours 
    * The highest level is greater than max
    */
   level = floor(min/cstep + CN_SMALL)*cstep;
   while (level < min) level += cstep;
   while (level <= max+CN_SMALL) {
      (void) CNinsert_contstep(cstephead, csteptail, level);
      level += cstep;
   }
}


/*
 * Select the number of contour steps
 */
static void use_const_nsteps(min,max,cstep,nsteps,cstephead,csteptail)
double        min, max;
double        *cstep;
int           *nsteps;
CNcontstepptr *cstephead;    /* Linked List of contour steps       */
CNcontstepptr *csteptail;    /* Linked List of contour steps       */
{
   int recalc=CN_FALSE;

   /*
    * if the number of steps has already been defined, check it 
    */
   recalc = CN_FALSE;
   if (*nsteps < 0) {
      (void) fprintf(stdout,"   ***Warning : Too few  contours %d\n",*nsteps);
      recalc = CN_TRUE;
   }
   if (*nsteps >=  CN_MAXSTEPS) {
      (void) fprintf(stdout,"   ***Warning : Too many contours %d\n",*nsteps);
      recalc = CN_TRUE;
   }

   /* Recalculate the contour step size */
   if (recalc) {
      (void) fprintf(stdout,"   Recalculating the contour step size...\n");
      *nsteps = (*nsteps >= CN_MAXSTEPS) ? CN_MAXSTEPS-1 : CN_IDLSTEPS;
   }

   /* Calculate the contour increments */
   *cstep = (*nsteps == 0) ? 0.0 : (max-min)/(double)(*nsteps);
   calculate_const_nsteps(min,max,*nsteps,cstephead,csteptail);
}


/*
 * Use a linear function to calculate the contour steps using a
 * constant number of steps
 */
static void calculate_const_nsteps(min,max,nsteps,cstephead,csteptail)
double        min, max;      /* Take contours between these limits */
int           nsteps;        /* Number of Contour steps            */
CNcontstepptr *cstephead;    /* Linked List of contour steps       */
CNcontstepptr *csteptail;    /* Linked List of contour steps       */
{
   int    i=0;
   double dz, value;

   /*
    * find the contours - there are nsteps+1 lines
    */
   if (nsteps == 0) {
      (void) CNinsert_contstep(cstephead, csteptail, 0.5*(max+min));
   } else {
      dz = (max - min)/(double)(nsteps);
      for (i=0; i<=nsteps; i++) {
         value = min + i*dz;
         if (i==nsteps) value = max;   /* To prevent floating point overflow */
         (void) CNinsert_contstep(cstephead, csteptail, value);
      }
   }  
}


/*
 * log10 contour selection
 * The resultant cstep list has values that go from 
 *  small (cstephead) to large (csteptail)
 */
static void calculate_log_steps(zmin,zmax,logzstep,cstephead,csteptail)
double        zmin, zmax;    /* Take contours between these limits */
int           logzstep;      /* Contour step size                  */
CNcontstepptr *cstephead;    /* Linked List of contour steps       */
CNcontstepptr *csteptail;    /* Linked List of contour steps       */
{
   double min, max, tmp, cstep, tmin, t;
   int    minsign=1, maxsign=1;
   int    i=0, nsteps=0, ndbs;
   int    dbstep;
   double tmp_ctr[CN_MAXSTEPS];

   if (zmax < zmin) {
      /* Switch the two */
      tmp  = zmax;
      zmax = zmin;
      zmin = tmp;
   }

   /*
    * Log possiblilities :
    *    both zmin, zmax > 0  (zmin= 1e10, zmax= 1e20 => min= 10, max= 20)
    *    both zmin, zmax < 0  (zmin=-1e20, zmax=-1e10 => min=-20, max=-10)
    *    zmin < 0,  zmax > 0  (zmin=-1e20, zmax= 1e10 => min=-20, max= 10)
    */
   minsign = (zmin < 0) ? -1 : 1;
   maxsign = (zmax < 0) ? -1 : 1;
   if (zmin < 0 && zmax==0) maxsign= -1;
   zmin    = fabs(zmin);
   zmax    = fabs(zmax);

   if (zmax < zmin) {
      /* Switch the two */
      tmp  = zmax;
      zmax = zmin;
      zmin = tmp;
   }
   min     = CNlog10(zmin);
   max     = CNlog10(zmax);

   /* Adjust the no of steps in each decibel */
   dbstep = logzstep;
   if (dbstep < 1)  dbstep = 1;
   if (dbstep > 10) dbstep = 10;
 
   /* 
    * Each major step varies by one order of magnitude 
    * There are dbstep steps per order of magnitude
    */
   cstep = 1.0;

   if (minsign == maxsign) {
      /*
       * Max and min are of the same sign 
       */

      /* Calculate the contour steps */
      fill_logctr_array(zmin, zmax, max, cstep, dbstep, minsign,
                        tmp_ctr, &nsteps);

      /* Copy the array into the linked list */
      for (i=0; i<nsteps && i<CN_MAXSTEPS; i++)
         (void) CNinsert_contstep(cstephead, csteptail, tmp_ctr[i]);

   } else {
    
      /* 
       * zmin and zmax are of different sign
       */

#define larger_of(x,y)     ((x) > (y) ? (x) : (y))
#define smaller_of(x,y)    ((x) < (y) ? (x) : (y))

      /* Number of decibels down */
      ndbs = CN_MAXSTEPS/(double)(2*dbstep);
      if (fabs(max - min + 10) < ndbs) ndbs = fabs(max - min + 10);

      /* This is the lower-bounds */
      tmin = larger_of(min, max) - ndbs;

      /* Fill the interval from [min, 0] */
      t = (tmin < min) ? tmin : min - smaller_of(ndbs,5);
      fill_logctr_array(pow(10.0,t), zmin, min, cstep, dbstep, minsign,
                        tmp_ctr, &nsteps);
      for (i=0; i<nsteps && i<CN_MAXSTEPS; i++)
         (void) CNinsert_contstep(cstephead, csteptail, tmp_ctr[i]);

      /* Fill the interval from [0, max] */
      t = (tmin < max) ? tmin : max - smaller_of(ndbs,5);
      fill_logctr_array(pow(10.0,t), zmax, max, cstep, dbstep, maxsign,
                        tmp_ctr, &nsteps);
      for (i=0; i<nsteps && i<CN_MAXSTEPS; i++)
         (void) CNinsert_contstep(cstephead, csteptail, tmp_ctr[i]);
   }
} 

/* 
 * Calculate the contour steps with logarithmic steps
 */
static void fill_logctr_array(zmin,zmax,max,cstep,dbstep,minsign,ctr,nctrs)
double     zmin, zmax, max, cstep;
int        dbstep, minsign;
double ctr[];
int    *nctrs;
{
   double tmp_ctr[CN_MAXSTEPS];   /* Use a tmp array */
   double level, high_ctr, ratio, z;
   int    i, j, jmin, nsteps=0;

   /* 
    * This assumes
    * zmin, zmax have same sign,
    * zmin < zmax
    */

   /* 
    * Now calculate the contour steps 
    * There might be too many steps, so start from the maximum value
    * and work our way down until the array is filled up.
    * Fill the temp array first 
    */
   i     = 0;
   level = pow(10.0,(floor(max/cstep + CN_SMALL)*cstep + cstep));
   ratio = pow(10.0,-cstep);
   while (level > zmin && i<CN_MAXSTEPS) {
      z = level;
      if (z <= zmax) tmp_ctr[i++] = z;
      if (dbstep > 1 && i<CN_MAXSTEPS) {
         high_ctr = tmp_ctr[i-1];
         jmin = (dbstep==10) ? 1 : 0;
         for (j=dbstep-1; j>jmin && i<CN_MAXSTEPS; j--) {
            /*
             * 10e20, 3/4*10e20, 2/4*10e20, 1/4*10e20
             */
            if (((z=high_ctr*j/dbstep) >= zmin) && (z <= zmax))
               tmp_ctr[i++] = z;
         }
      }
      level = ratio*level;
   }
   nsteps = i;

   /* Now fill in the real array */
   if (minsign == -1) {
      for (i=0; i<nsteps; i++) 
         tmp_ctr[i] = minsign*tmp_ctr[i]; 
   }
   if (tmp_ctr[0] > tmp_ctr[nsteps-1]) {
      /* Reverse the array */
      for (i=0; i<nsteps; i++) 
         ctr[i] = tmp_ctr[nsteps-i-1];
   } else {
      /* Just copy the array */
      for (i=0; i<nsteps; i++) 
         ctr[i] = tmp_ctr[i];
   }

   *nctrs = nsteps;
}
