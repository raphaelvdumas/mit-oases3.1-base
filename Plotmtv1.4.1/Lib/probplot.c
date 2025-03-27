/*
 * probplot.c - routines used for probability plots
 *
 * Author:  Sean Casey
 *          Modified 6/93 by Kenny Toh
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define ERRORRETURN -9.0e30
#define MAXITER  3000
#ifndef EXIT_FAILURE
#  define EXIT_FAILURE 1
#  define EXIT_SUCCESS 0
#endif

#ifndef FLT_EPSILON
#  define FLT_EPSILON 1e-7
#endif

/* Local procedure declarations */
double CNnorm_vert_distance();
#ifdef TEST
static void calc_prob_axes();
#endif


/*
 *
 * double CNnorm_vert_distance(p, rc)
 * Function to return the vertical distance on a probability plot given
 * a fraction between 0 and 1.
 * No allocated memory.
 *
 * This returns a value between -5 and +5.  If x=0.5 (50%), f(x)=0
 */

double CNnorm_vert_distance(p, rc)
double p;        /* Fraction between 0 and 1 */
int   *rc;      /* Return code, gets EXIT_SUCCESS for successful termination */
{
    /* Simple Newton's method plus bisection.  */
    /* Taylored specifically to this application. */
    double   fb = 0, fu, fv;
    double   b, u, v;
    double   sqrt2, derivative, step;
    int   count = 2, nbis = 25, debug = 0;

    *rc = EXIT_FAILURE;
    sqrt2 = sqrt(2.0);
#ifdef DEBUG
    debug = 1;
#endif

    u = -5.0;   /* Upper and lower limits on solution. */
    v =  5.0;   /* These will reach alomost to 1 */

    fu = .5 * ( erf( u / sqrt2 ) + 1) - p;
    fv = .5 * ( erf( v / sqrt2 ) + 1) - p;
 
    if (fu * fv > 0 ) {
        *rc = EXIT_FAILURE;
        return(ERRORRETURN);
    }
    step = v - u;

    for ( count = 0; count <= MAXITER; count++) {

        if ( count % nbis == 0 ) {
            b = u + (v - u) / 2;   /* Bisect the interval. */
            if (debug)
                (void) printf("Bisect\n");
        } else {
            derivative = ( 1 / ( sqrt2 * sqrt(M_PI) * exp(b * b / 2) ) );
            if (debug)
                (void) printf(
                       "Derivative f'(b) = f'(%-8f) = %-10f  step=%-10f  ",
                       b, derivative, fb / derivative);
            step = fb / derivative;
            b -= fb / derivative ;
            if (debug)
                (void) printf("new b=%-10f\n", b);
            if (b < u || b > v) {
                if (debug)
                    (void) printf("b = %-10f   Out of bounds. Bisect.\n", b);
                b = u + (v - u) / 2;   /* Bisect the interval. */
            }
        }

        fb = .5 * ( erf( b / sqrt2 ) + 1 ) - p;

        if (fb * fu > 0) {
            fu = fb;
            u = b;
        } else {
            fv = fb;
            v = b;
        }

        if (debug)
            (void) printf(
                   "count=%d  b=%f fb=%-10f  u=%-10f  v=%-10f  (u-v)=%-10f\n",
                    count, b, fb, u, v, u - v);

        if ( fabs(v - u) <= FLT_EPSILON  || fabs(step) <= FLT_EPSILON ) {
            *rc = EXIT_SUCCESS;
            return b;
        }
    }
    *rc = EXIT_FAILURE;
    return(ERRORRETURN);
}

#ifdef TEST

/*
 * Figure out the position of the axes and tickmarks
 */
static void calc_prob_axes()
{
    /*
     * Figure out the axis position (0-1) for x=0.0001, 0.001, 0.01, 0.1,
     * 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.99, 0.999, 0.9999
     *
     * x=0.0001 => 0
     * x=0.9999 => 1
     */
    
    double upper_lim, lower_lim;
    double x, y, Y, delta;
    int    i,j;
    int    error;

    /* 
     * lower_lim => 0    upper_lim => 1
     *   (y-0)/(1-0) = (Y-lower_lim)/(upper_lim-lower_lim)
     */
    lower_lim = CNnorm_vert_distance(0.0001, &error);
    upper_lim = CNnorm_vert_distance(0.9999, &error);
    
    /* Calculate axes from 0.0001 (0.01%) to 0.10 (10%) */
    delta = 0.0001;
    for (j=0; j<3; j++) {
        x = delta;
        for (i=0; i<10; i++) {
            Y = CNnorm_vert_distance(x, &error);
            y = 100*(Y - lower_lim)/(upper_lim - lower_lim);
            (void) printf("%s x=%10.5f  y=%10.5f\n",(i==0 ? "M" : " "),x,y);
            x += delta; 
        }
        delta = delta*10.0;
        (void) printf("\n");
    }

    /* Calculate axes from 0.1 (10%) to 0.9 (90%) */
    delta = 0.01;
    for (j=1; j<9; j++) {
        x = 0.1*j;
        for (i=0; i<10; i++) {
            Y = CNnorm_vert_distance(x, &error);
            y = 100*(Y - lower_lim)/(upper_lim - lower_lim);
            (void) printf("%s x=%10.5f  y=%10.5f\n",(i==0 ? "M" : " "),x,y);
            x += delta;
        }
        (void) printf("\n");
    }

    /* Calculate axes from 0.9 (90%) to 0.9999 */
    delta = 0.01;
    for (j=0; j<3; j++) {
        x = 1.0 - 10.0*delta;
        for (i=0; i<10; i++) {
            Y = CNnorm_vert_distance(x, &error);
            y = 100*(Y - lower_lim)/(upper_lim - lower_lim);
            (void) printf("%s x=%10.5f  y=%10.5f\n",(i==0 ? "M" : " "),x,y);
            x += delta;
        }
        delta = delta*0.10;
        (void) printf("\n");
   } 
}

main(argc, argv)
int   argc;
char  *argv[];
{
    int   rc, n, i;
    if (argc < 2) {
        (void) printf("To test, type a.out <fraction value>, e.g., a.out .6\n");
        (void) printf("To test, type a.out <fraction value> <integer value>, e.g., a.out .6 10 \n");
        exit(1);
    }

    (void) printf("Input=%f  verticalDistance=%g\n", atof(argv[1]), CNnorm_vert_distance( atof(argv[1]) , &rc ) );
    if (rc == 0)
        (void) printf("SUCCESS\n");
    else
        (void) printf("FAILURE\n");
    if (argc > 2) {
        n = atoi(argv[2]);
        for (i = 1; i <= n; i++) {
             (void) printf("%-10f  verticalDistance=%-10f\n", 
                           (2.0 * i - 1.0) / (2.0 * n), 
                           CNnorm_vert_distance( (2.0 * i -1.0) / (2.0 * n), &rc));
             if (rc != 0)
                 (void) printf("FAILURE\n");
        }
    }

    /* Test the axes routine */
    calc_prob_axes();
 
    exit(0);
}


#endif

