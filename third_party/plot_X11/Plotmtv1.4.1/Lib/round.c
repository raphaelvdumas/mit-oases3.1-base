/*
 * round.c - useful routines for rounding
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "CNround.h"
 
#define SMALL           1.0e-8
#define VERY_SMALL      1.0e-30
#define EXTREMELY_SMALL 1.0e-99

static double round_to_pow();

/*
 * round down a number to the nearest decimal place
 * i.e.   0.0145 -> 0.01, 0.017 -> 0.020
 */
double
CNround_to_decimal(x)
double x;
{
   double dpower, x2;
   int    power;

   /* error-checking */
   if (fabs(x) < SMALL) {
      return(x);
   }

   dpower  = log10(fabs(x));
   power   = (dpower > SMALL) ?  (int)dpower : (int)dpower - 1;
   x2      = round_to_pow(x,power);
   return(x2);
}

/* 
 * round down a number to the decimal place given by power
 * e.g round(1.0256,-2) means round to 2nd decimal -> 1.03
 */
static double round_to_pow(x,power)
double x;
int    power;
{
   double inc, xnew;

   inc  = pow(10.0,(double)power);
   xnew = inc * (floor(x/inc + 0.5 + SMALL));
   return(xnew);
}

/* 
 * autorange on the x-axis 
 */
void CNget_autorange(xmin,xmax,xmin2,xmax2,deltx)
double  xmin,xmax,*xmin2,*xmax2,*deltx;
{
   double dpower;
   int    power;

   /* error-checking */
   if (fabs(xmax-xmin) < EXTREMELY_SMALL) {
      (void) fprintf(stderr,"Log(zero) Error!  xmin = xmax = %g!\n",xmin);
      *xmin2 = xmin;
      *xmax2 = xmax;
   } else {
      /* round down xmin and round up xmax */
      dpower = CNlog10(xmax-xmin);
      power  = (dpower > -SMALL) ? (int)dpower : (int)dpower - 1;
      *xmin2 = CNround_down(xmin,power);
      *xmax2 = CNround_up(xmax,power);
      *deltx = pow(10.0,(double)power);
   }
}

/* round down a number automatically to the nearest log scale */
double CNauto_round_down(x)
double x;
{
   double x2, dpower;
   int    power;

   /* error-checking */
   if (fabs(x) < EXTREMELY_SMALL) {
      (void) fprintf(stderr,"Log(zero) Error!  x = %g!\n",x);
      return(x);
   } 

   /* round down xmin and round up xmax */
   dpower = CNlog10(x);   
   power  = (dpower > -SMALL) ? (int)dpower : (int)dpower - 1;
   x2     = pow(10.0,(double)power);
   return(x2);
}

/* round up a number automatically to the nearest log scale */
double CNauto_round_up(x)
double x;
{
   double x2, dpower;
   int    power;

   /* error-checking */
   if (fabs(x) < EXTREMELY_SMALL) {
      (void) fprintf(stderr,"Log(zero) Error!  x = %g!\n",x);
      return(x);
   } 

   /* round down xmin and round up xmax */
   dpower = log10(x);   
   power  = (dpower > -SMALL) ? (int)dpower : (int)dpower - 1;
   if ((fabs(dpower - (double)power)) < SMALL)
      x2     = pow(10.0,(double)power);
   else 
      x2     = pow(10.0,(double)(power+1));
   return(x2);
}

/* round down a number */
double CNround_down(x,power)
double x;
int    power;
{
   double inc, xnew;
   
   inc  = pow(10.0,(double)power);
   xnew = inc * (floor(x/inc + SMALL));
   return(xnew);
}

/* round up a number */
double CNround_up(x,power)
double x;
int    power;
{
   double inc, xnew;
   
   inc  = pow(10.0,(double)power);
   xnew = inc * (floor(x/inc - SMALL) + 1);
   return(xnew);
}


/*
 * Take the log10 of a number; substitute a small num if the value <= 0
 */
double CNlog10(x)
double x;
{
   double val;

   if (x < 0.0) {
      /* Set this to a positive small number */
      x = VERY_SMALL;
      /*
      (void) fprintf(stderr,"   ***Error : x=%g - cannot take the log of ",x);
      (void) fprintf(stderr,"a number less or equal to zero!\n");
      (void) fprintf(stderr,"      Resetting x to %g\n",x);
       */
   } else if (x < EXTREMELY_SMALL) {
      /* x must be close to or equal to zero */
      x = EXTREMELY_SMALL;
   }

   val = log10(x);

   /* Return the number */
   return(val);
}



/*
 * Routines for log-interpolation
 */

/*
 * Find the independent parameter t where t is the ratio between
 * 2 distances (d-d1) and (d2-d1)
 * Assume that d2-d1 is nonzero
 * if d=d1, t=0;  d=d2, t=1
 */
double CNinterp_distance(d1,d2,d,loginterp)
double d1, d2, d;
short  loginterp;
{
   double t;
   double z, z1, z2;

   if (!loginterp) {

      /*
       * Linear interpolation 
       */
      if ((d1 == d2) && (d == d1)) 
         t = 0.5;
      else
         t = (d - d1)/(d2 - d1);

   } else {

      /* 
       * Use log interpolation 
       * The result depends on the sign of the 2 points d1 and d2
       */

      if ((d1 > 0.0) && (d2 > 0.0)) {

         /* Both end points are positive */
         if (d <= 0.0) 
            t = -1.0;
         else {
            z  =  CNlog10(d);
            z1 =  CNlog10(d1);
            z2 =  CNlog10(d2);
            if ((z1 == z2) && (z == z1)) 
               t = 0.5;
            else
               t = (z - z1)/(z2 - z1);
         }

      } else if ((d1 < 0.0) && (d2 < 0.0)) {

         /* Both end points are negative */
         if (d >= 0.0) 
            t = -1;
         else {
            z  =  CNlog10(-d);
            z1 =  CNlog10(-d1);
            z2 =  CNlog10(-d2);
            if ((z1 == z2) && (z == z1)) 
               t = 0.5;
            else
               t = (z - z1)/(z2 - z1);
         }

      } else if ( ((d1 < 0.0) && (d2 > 0.0)) ||
                  ((d1 > 0.0) && (d2 < 0.0)) ) {

         /*
          * This uses the formula :
          *     log(x) = sign(x) * log(1+abs(x))
          */

         /* One point is negative and the other is positive */
         if (d == 0.0) {
            z  =  0.0;
            z1 =  ((d1 < 0.0) ? -1.0 : 1.0) * CNlog10(fabs(d1));
            z2 =  ((d2 < 0.0) ? -1.0 : 1.0) * CNlog10(fabs(d2));
            if ((z1 == z2) && (z == z1)) 
               t = 0.5;
            else
               t = (z - z1)/(z2 - z1);
         } else {  
            z  =  ((d  < 0.0) ? -1.0 : 1.0) * CNlog10(1.0+fabs(d));
            z1 =  ((d1 < 0.0) ? -1.0 : 1.0) * CNlog10(1.0+fabs(d1));
            z2 =  ((d2 < 0.0) ? -1.0 : 1.0) * CNlog10(1.0+fabs(d2));
            if ((z1 == z2) && (z == z1)) 
               t = 0.5;
            else
               t = (z - z1)/(z2 - z1);
         }

      } else {

         /* Resort to linear interpolation */
         if ((d1 == d2) && (d == d1)) 
            t = 0.5;
         t = (d - d1)/(d2 - d1);

      }
   }

   /* Return */
   return(t);
}


/*
 * Figure out the log-function to use
 * return 1 to indicate true log, and 0 to indicate false log
 */
int CNlogmode2(z1,z2)
double z1, z2;
{
   int    mode;

   if ((z1 > 0.0) && (z2 > 0.0)) {
      /* All positive */
      mode = 1;
   } else if ((z1 < 0.0) && (z2 < 0.0)) {
      /* All negative */
      mode = 1;
   } else {
      /* The points differ in sign */
      mode = 0;
   }
   return(mode);
}


/*
 * Figure out the log-function to use
 * return 1 to indicate true log, and 0 to indicate false log
 */
int CNlogmode3(z1,z2,z3)
double z1, z2, z3;
{
   int    mode;

   if ((z1 > 0.0) && (z2 > 0.0) && (z3 > 0.0)) {
      /* All positive */
      mode = 1;
   } else if ((z1 < 0.0) && (z2 < 0.0) && (z3 < 0.0)) {
      /* All negative */
      mode = 1;
   } else {
      /* The points differ in sign */
      mode = 0;
   }
   return(mode);
}


/*
 * Figure out the log-function to use
 * return 1 to indicate true log, and 0 to indicate false log
 */
int CNlogmode4(z1,z2,z3,z4)
double z1, z2, z3, z4;
{
   int    mode;

   if ((z1 > 0.0) && (z2 > 0.0) && (z3 > 0.0) && (z4 > 0.0)) {
      /* All positive */
      mode = 1;
   } else if ((z1 < 0.0) && (z2 < 0.0) && (z3 < 0.0) && (z4 < 0.0)) {
      /* All negative */
      mode = 1;
   } else {
      /* The points differ in sign */
      mode = 0;
   }
   return(mode);
}


/*
 * Return the sign of a number
 * if x=0 return 1
 */
int CNsign(x)
double x;
{
   int y;
   y = (x >= 0.0) ? 1 : -1;
   return(y);
}

/*
 * Take the log of the absolute - use the logmode flag to determine function
 */
double CNlogabs(x,logmode)
double x;
int    logmode;
{
   /*
    * If logmode == 0 => then just do linear; don't convert to log
    */

   double y;
   /*
   y = (logmode) ? CNlog_abs(x) : CNlog_1plusabs(x);
    */
   y = (logmode) ? CNlog_abs(x) : x;
   return(y);
}

/*
 * Take the log of the absolute
 * Formula:
 *           y = sign(x) * log10(abs(x))
 */
double CNlog_abs(x)
double x;
{
   double y;
   y = CNsign(x) * CNlog10(fabs(x));
   return(y);
}

/*
 * Take the log of the absolute+1
 * Formula:
 *           y = sign(x) * log10(1+abs(x))
 *           y = sign(x) * log10(1+abs(x))
 *   or
 *
 *   x >=0   y = log10(1+x)
 *   x < 0   y = - log10(1-x)
 *
 * This formula minimizes the effect of having values that cross the x=0
 * plane, but is not good for values of x <= 1
 *
 * In addition if x>0, y>0 and x<0, y<0
 */
double CNlog_1plusabs(x)
double x;
{
   double y;
   y = CNsign(x) * CNlog10(1+fabs(x));
   return(y);
}

/*
 * Take the inv log of the absolute 
 *    - use the logmode flag to determine function
 */
double CNinvlogabs(y,signx,logmode)
double y;
int    signx;
int    logmode;
{
   /*
    * If logmode == 0 => then just do linear; don't convert to log
    */

   double x;
   /*
   x = (logmode) ? CNinvlog_abs(y,signx) : CNinvlog_1plusabs(y);
    */
   x = (logmode) ? CNinvlog_abs(y,signx) : y;
   return(x);
}

/*
 * Take the inv-log of the absolute
 * Formula:
 *           y = sign(x) * log10(abs(x))
 * or
 *           y = sign(x) * log10(sign(x) * x)
 *
 * To take the inverse, we need the sign of x
 */
double CNinvlog_abs(y,signx)
double y;
int    signx;
{
   double x;

   x = signx * pow(10.0,signx*y);
   return(x);
}

/*
 * Take the log of the absolute+1
 * Formula:
 *           y = sign(x) * log10(1+abs(x))
 *   or
 *
 *   x >=0   y = log10(1+x)
 *   x < 0   y = - log10(1-x)
 *
 * This formula minimizes the effect of having values that cross the x=0
 * plane, but is not good for values of x <= 1
 *
 * In addition if x>0, y>0 and x<0, y<0
 */
double CNinvlog_1plusabs(y)
double y;
{
   double x;
   if (y >= 0) 
      x =  pow(10.0, y) - 1;
   else
      x = -pow(10.0,-y) + 1;
   return(x);
}


