#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>
#include <math.h>

/* Useful definitions */
#define dsqf(u1,v1,u2,v2) ((u2-u1)*(u2-u1) + (v2-v1)*(v2-v1))
#define spdt(u1,v1,u2,v2,u3,v3) ((u2-u1)*(u3-u1) + (v2-v1)*(v3-v1))
#define vpdt(u1,v1,u2,v2,u3,v3) ((v3-v1)*(u2-u1) - (u3-u1)*(v2-v1))
#define ESPLN 1.0e-6
#define MAXOF(a,b) ((a > b) ? a : b)
#define MINOF(a,b) ((a > b) ? b : a)

/* There's a bug in here somewhere - don't do this! */
#define NREP  0

/* Declarations */
static int idxchg();

/*
C THIS SUBROUTINE PERFORMS TRIANGULATION.  IT DIVIDES THE X-Y
C PLANE INTO A NUMBER OF TRIANGLES ACCORDING TO GIVEN DATA
C POINTS IN THE PLANE, DETERMINES LINE SEGMENTS THAT FORM THE
C BORDER OF DATA AREA, AND DETERMINES THE TRIANGLE NUMBERS
C CORRESPONDING TO THE BORDER LINE SEGMENTS.
C AT COMPLETION, POINT NUMBERS OF THE VERTEXES OF EACH TRIANGLE
C ARE LISTED COUNTER-CLOCKWISE.  POINT NUMBERS OF THE END POINTS
C OF EACH BORDER LINE SEGMENT ARE LISTED COUNTER-CLOCKWISE,
C LISTING ORDER OF THE LINE SEGMENTS BEING COUNTER-CLOCKWISE.
C THIS SUBROUTINE CALLS THE IDXCHG FUNCTION.
C THE INPUT PARAMETERS ARE
C     NDP = NUMBER OF DATA POINTS,
C     XD  = ARRAY OF DIMENSION NDP CONTAINING THE
C           X COORDINATES OF THE DATA POINTS,
C     YD  = ARRAY OF DIMENSION NDP CONTAINING THE
C           Y COORDINATES OF THE DATA POINTS.
C THE OUTPUT PARAMETERS ARE
C     NT  = NUMBER OF TRIANGLES,
C     IPT = INTEGER ARRAY OF DIMENSION 6*NDP-15, WHERE THE
C           POINT NUMBERS OF THE VERTEXES OF THE (IT)TH
C           TRIANGLE ARE TO BE STORED AS THE (3*IT-2)ND,
C           (3*IT-1)ST, AND (3*IT)TH ELEMENTS,
C           IT=1,2,...,NT,
C     NL  = NUMBER OF BORDER LINE SEGMENTS,
C     IPL = INTEGER ARRAY OF DIMENSION 6*NDP, WHERE THE
C           POINT NUMBERS OF THE END POINTS OF THE (IL)TH
C           BORDER LINE SEGMENT AND ITS RESPECTIVE TRIANGLE
C           NUMBER ARE TO BE STORED AS THE (3*IL-2)ND,
C           (3*IL-1)ST, AND (3*IL)TH ELEMENTS,
C           IL=1,2,..., NL.
C THE OTHER PARAMETERS ARE
C     IWL = INTEGER ARRAY OF DIMENSION 18*NDP USED
C           INTERNALLY AS A WORK AREA,
C     IWP = INTEGER ARRAY OF DIMENSION NDP USED
C           INTERNALLY AS A WORK AREA,
C     WK  = ARRAY OF DIMENSION NDP USED INTERNALLY AS A
C           WORK AREA.
C
C HISTORY                THE ORIGINAL VERSION OF BIVAR WAS WRITTEN BY
C                        HIROSHI AKIMA IN AUGUST 1975 AND REWRITTEN BY
C                        HIM IN LATE 1976.  IT WAS INCORPORATED INTO
C                        NCAR'S PUBLIC SOFTWARE LIBRARIES IN JANUARY
C                        1977.  IN AUGUST 1984 A NEW VERSION OF BIVAR,
C                        INCORPORATING CHANGES DESCRIBED IN THE ROCKY
C                        MOUNTAIN JOURNAL OF MATHEMATICS ARTICLE CITED
C                        BELOW, WAS OBTAINED FROM DR. AKIMA BY MICHAEL
C                        PERNICE OF NCAR'S SCIENTIFIC COMPUTING
C                        DIVISION, WHO EVALUATED IT AND MADE IT
C                        AVAILABLE IN FEBRUARY, 1985.
*
* Converted to C on 1/20/93, by Kenny Toh.
* This routine was originally written in Fortran.
*/
int idtang(ndp, xd, yd, nt, ipt0, nl, ipl0)
int    ndp;       /* Number of data points        */
double *xd;       /* x-coordinates of data points */
double *yd;       /* y-coordinates of data points */
int    *nt;       /* No of triangles created      */
int    **ipt0;    /* Point numbers of vertices    */
int    *nl;       /* Number of border line segmts */
int    **ipl0;    /* Point numbers of line segmts */
{
   double *wk;               /* double-precision work array */
   int    *iwp, *iwl;        /* integer work array          */
   int    *ipt, *ipl;        /* Local arrays                */
   double xdmp, ydmp;
   double dsqmn, dsqi;
   double wts, sp, vp;
   double x1, y1, x2, y2, x3, y3;
   int    ipmn1, ipmn2, ip1, ip2, ip3, jp1, jp2, ip, jp;
   int    jpmn, jpc, its, FOUND;
   int    nt0, ntt3;
   int    nl0, nlt3;
   int    ixvs, ixvspv, iliv, ilvs, nlsh, nlsht3, jl1, jl2;
   int    jwl, il, ilt3, ipl1, ipl2, it, itt3, nln, nlnt3, ipti, nlf;
   int    ntt3p3, irep, ilf, itt3r, ipt1, ipt2, ipt3, itf[3];
   int    it1t3, it2t3, ipti1, ipti2, iplj1, iplj2, jlt3, nlfc, ntf;
   int    jwl1mn, nlft2, jwl1;

   /* error check */
   if (ndp < 4) {
      (void) fprintf(stderr,
             "   ***Error - input parameter ndp out of range\n");
      return(0);
   }

   /* print message if more than 50 points */
   if (ndp >= 50) {
      (void) fprintf(stdout,"   Trying to triangulate %d points\n",ndp);
      (void) fprintf(stdout,"   Hmmm...  This may take some time...\n");
   }

   /* Create the work arrays */
   wk  = (double *)malloc((unsigned int)(ndp*sizeof(double)));
   iwp = (int    *)malloc((unsigned int)(ndp*sizeof(int   )));
   ipt = (int    *)malloc((unsigned int)((6*ndp-15+1)*sizeof(int   )));
   ipl = (int    *)malloc((unsigned int)((6*ndp   +1)*sizeof(int   )));
   iwl = (int    *)malloc((unsigned int)((18*ndp  +1)*sizeof(int   )));
   if (!wk || !iwp || !(ipt) || !(ipl) || !(iwl)) {
      (void) fprintf(stderr,"   ***Malloc error!\n");
      if (wk ) free((char *)wk);
      if (iwp) free((char *)iwp);
      if (ipt) free((char *)ipt);
      if (ipl) free((char *)ipl);
      if (iwl) free((char *)iwl);
      return(0);
   }

   /* Find the closest pair of datapoints and their midpoint */
   dsqmn = dsqf(xd[0],yd[0],xd[1],yd[1]);
   ipmn1 = 0;
   ipmn2 = 1;
   for (ip1=0; ip1<ndp-1; ip1++) {
      for (ip2=ip1+1; ip2<ndp; ip2++) {
         dsqi = dsqf(xd[ip1],yd[ip1],xd[ip2],yd[ip2]);
         if (dsqi == 0.0) {
            (void) fprintf(stderr,
                   "   ***Error - 2 of the input data points are identical\n");
            if (wk ) free((char *)wk);
            if (iwp) free((char *)iwp);
            if (ipt) free((char *)ipt);
            if (ipl) free((char *)ipl);
            if (iwl) free((char *)iwl);
            return(0);
         }
         if (dsqi < dsqmn) {
            dsqmn = dsqi;
            ipmn1 = ip1;
            ipmn2 = ip2;
         }
      }
   }
   xdmp = 0.5*(xd[ipmn1] + xd[ipmn2]);
   ydmp = 0.5*(yd[ipmn1] + yd[ipmn2]);
#ifdef DEBUG
   (void) printf("closest points are %d and %d\n",ipmn1, ipmn2);
#endif

   /* 
    * Sort the other NDP-2 data points in ascending order of
    * distance from the midpoint and store the sorted data point
    * numbers in the iwp array 
    */
   jp1=1;
   for (ip1=0; ip1<ndp; ip1++) {
      if (ip1==ipmn1 || ip1==ipmn2) continue;
      jp1++;
      iwp[jp1] = ip1;
      wk [jp1] = dsqf(xdmp, ydmp, xd[ip1], yd[ip1]);
   }
   for (jp1=2; jp1<ndp-1; jp1++) {
      dsqmn = wk[jp1];
      jpmn  = jp1;
      /* This loop finds the smallest remaining dist */
      for (jp2=jp1+1; jp2<ndp; jp2++) {
         if (wk[jp2] < dsqmn) {
            dsqmn = wk[jp2];
            jpmn  = jp2;
         }
      }
      /* interchange the smallest dist with the current dist */
      its      = iwp[jp1];
      iwp[jp1] = iwp[jpmn];
      iwp[jpmn]= its;   
      wts      = wk[jp1];
      wk[jp1]  = wk[jpmn];
      wk[jpmn] = wts;
   }
#ifdef DEBUG
   (void) printf("\nAfter distance sorting:\n");
   for (ip=2; ip<ndp; ip++)
      (void) printf("iwk[%d]=%d  wp[%d]=%g\n",ip,iwp[ip],ip,wk[ip]);
#endif

   /*
    * If necessary, modify the ordering in such a way that 
    * the first 3 data points are not collinear
    */
   x1    = xd[ipmn1];
   y1    = yd[ipmn1];
   x2    = xd[ipmn2];
   y2    = yd[ipmn2];
   FOUND = 0;
   its   = 2;
   for (jp=2; jp<ndp && !FOUND; jp++) {
      ip = iwp[jp];
      sp = spdt(xd[ip],yd[ip],x1,y1,x2,y2);
      vp = vpdt(xd[ip],yd[ip],x1,y1,x2,y2);
      /* vp tests the determinant of vectors 01, 02 
       * sp is the dot product of the two
       * Normally if the determinant is zero the 2 vectors are collinear
       * If the determinant is non zero, look at the dot product 
       */
      if (fabs(vp) > (fabs(sp)*ESPLN)) {
         FOUND = 1;
         jp1   = jp;
         its   = iwp[jp];
         wts   = wk[jp];
      }
   }
   if (!FOUND) {
      (void) fprintf(stderr,"   ***Error! All collinear data points\n");
      if (wk ) free((char *)wk);
      if (iwp) free((char *)iwp);
      if (ipt) free((char *)ipt);
      if (ipl) free((char *)ipl);
      if (iwl) free((char *)iwl);
      return(0);
   }
   if (jp1 != 2) {
      /* Shift the registers 
       * its and wts contain the values at index jp1
       */
      for (jpc=3; jpc<=jp1; jpc++) {
         jp = jp1 + 3 - jpc;
         iwp[jp] = iwp[jp-1];
         wk [jp] = wk [jp-1];
      }
      iwp[2] = its;
      wk[2]  = wts;
   }
#ifdef DEBUG
   (void) printf("\nAfter collinear modification:\n");
   for (ip=2; ip<ndp; ip++)
      (void) printf("iwk[%d]=%d  wp[%d]=%g\n",ip,iwp[ip],ip,wk[ip]);
#endif

   /*
    * Form the first triangle.  Store point numbers of the vertices of
    * the triangle in the ipt array, and store point numbers of the
    * border line segments and the triangle number in the ipl array
    */
   ip1 = ipmn1;
   ip2 = ipmn2;
   ip3 = iwp[2];
   if (vpdt(xd[ip1],yd[ip1],xd[ip2],yd[ip2],xd[ip3],yd[ip3]) < 0) {
      /* switch orientation of the segments */
      ip1 = ipmn2;
      ip2 = ipmn1;
   }
   /* Triangle */
   nt0     = 1;
   ntt3    = 3;
   ipt[1] = ip1; 
   ipt[2] = ip2; 
   ipt[3] = ip3; 
   /* Segment */
   nl0     = 3;
   nlt3    = 9;
   ipl[1] = ip1;
   ipl[2] = ip2;
   ipl[3] = 1;
   ipl[4] = ip2;
   ipl[5] = ip3;
   ipl[6] = 1;
   ipl[7] = ip3;
   ipl[8] = ip1;
   ipl[9] = 1;
  
   /*
    * Add the remaining NDP-3 data points one by one 
    */ 
   for (jp1=3; jp1<ndp; jp1++) {
      ip1 = iwp[jp1];
      x1  = xd[ip1];
      y1  = yd[ip1];

#ifdef DEBUG
      printf("jp1=%d\n",jp1);
      for (il=1; il<=ntt3; il++)
         printf("ipt[%d] = %d\n",il,ipt[il]);
      for (il=1; il<=nlt3; il++)
         printf("ipl[%d] = %d\n",il,ipl[il]);
#endif
      /* Determine the first invisible and visible border line segments
       * iliv, ilvs
       */
      FOUND = 0;
      for (il=1; il<=nl0 && !FOUND; il++) {
         ip2 = ipl[3*il-2];
         ip3 = ipl[3*il-1];
         x2  = xd[ip2];
         y2  = yd[ip2];
         x3  = xd[ip3];
         y3  = yd[ip3];
         sp  = spdt(x1,y1,x2,y2,x3,y3);
         vp  = vpdt(x1,y1,x2,y2,x3,y3);
         if (il == 1) {
            ixvs = 0;
            if (vp <= (fabs(sp)*(-ESPLN))) ixvs = 1;
            iliv = 1;
            ilvs = 1;
         } else {
            ixvspv = ixvs;
            if (vp <= (fabs(sp)*(-ESPLN))) {
               ixvs = 1;
               if (ixvspv != 1) {
                  ilvs = il;
                  if (iliv != 1) FOUND=1;     /* Break out */
               }
            } else {
               ixvs = 0;
               if (ixvspv != 0) {
                  iliv = il;
                  if (ilvs != 1) FOUND=1;     /* Break out */
               }
            }
         }
      } /* il loop */
      if (!FOUND && (iliv == 1) && (ilvs == 1)) ilvs = nl0; 
      if (ilvs < iliv) ilvs = ilvs+nl0;
     
#ifdef DEBUG
      (void)printf("iliv=%d  ilvs=%d\n",iliv,ilvs);
#endif
      /* Shift (rotate) the ipl array to have the invisible border
       * line segments contained in the first part of the ipl array
       */
      if (iliv != 1) {
         nlsh   = iliv - 1;
         nlsht3 = nlsh*3;
         for (jl1=1; jl1<=nlsht3; jl1++) {
            jl2 = jl1 + nlt3;
            ipl[jl2] = ipl[jl1];
         }
         for (jl1=1; jl1<=nlt3; jl1++) {
            jl2 = jl1 + nlsht3;
            ipl[jl1] = ipl[jl2];
         }
         ilvs = ilvs-nlsh;
      }
#ifdef DEBUG
      (void)printf("iliv=%d  ilvs=%d\n",iliv,ilvs);
      for (il=1; il<=nlt3; il++)
         printf("ipl[%d] = %d\n",il,ipl[il]);
#endif
     
      /* Add triangles to the ipt array, update border line 
       * segments in the ipl array, and set flags for the border line
       * segments to be reexamined in the iwl array 
       */
      jwl = 0;
      for (il=ilvs; il<=nl0; il++) {
         ilt3 = il*3;
         ipl1 = ipl[ilt3-2]; 
         ipl2 = ipl[ilt3-1]; 
         it   = ipl[ilt3  ]; 
         /* Add a triangle to the ipt array */
         nt0          = nt0+1;
         ntt3         = ntt3+3;
         ipt[ntt3-2] = ipl2;
         ipt[ntt3-1] = ipl1;
         ipt[ntt3  ] = ip1;
         /* Update border line segments in the ipl array */
         if (il == ilvs) {
            ipl[ilt3-1] = ip1;
            ipl[ilt3  ] = nt0;
         }
         if (il == nl0) {
            nln   = ilvs+1;
            nlnt3 = nln*3;
            ipl[nlnt3-2] = ip1;
            ipl[nlnt3-1] = ipl[1];
            ipl[nlnt3  ] = nt0;
         }
         /* Determine the vertex that does not lie on the border line segm */
         itt3 = it*3;
         ipti = ipt[itt3-2];
         if (ipti==ipl1 || ipti==ipl2)
            ipti = ipt[itt3-1];
         if (ipti==ipl1 || ipti==ipl2)
            ipti = ipt[itt3  ];
         /* Check if the exchange is necessary */
         if (idxchg(xd, yd, ip1, ipti, ipl1, ipl2) != 0) {
            /* Modify the ipt array when necessary */
            ipt[itt3-2] = ipti;
            ipt[itt3-1] = ipl1;
            ipt[itt3  ] = ip1;
            ipt[ntt3-1] = ipti;
            if (il == ilvs) ipl[ilt3] = it;
            if ((il==nl0) && (ipl[3]==it)) ipl[3] = nt0;
            /* Set flags in the iwl array */
            jwl = jwl+4;
            iwl[jwl-3] = ipl1;
            iwl[jwl-2] = ipti;
            iwl[jwl-1] = ipti;
            iwl[jwl  ] = ipl2;
         }
      }
      nl0  = nln;
      nlt3 = nlnt3;
      nlf  = jwl/2;

      /*
       * Improve triangulation 
       */             
#ifdef DEBUG
      if ((nlf != 0) && (nlf != nlfc))
         (void) fprintf(stdout,"Doing extra repetitions\n");
#endif
      nlfc   = nlf+1;
      ntt3p3 = ntt3+3;
      for (irep=0; irep<NREP && (nlf!=0) && (nlf!=nlfc); irep++) {
         for (ilf=1; ilf<=nlf; ilf++) {
            ipl1 = iwl[2*ilf-1];
            ipl2 = iwl[2*ilf  ];
            /* Locate in the ipt array 2 triangles on both sides of the
             * flagged line segment
             */
            ntf   = 0;
            FOUND = 0;
            for (itt3r=3; itt3r<=ntt3 && !FOUND; itt3r=itt3r+3) {
               itt3 = ntt3p3-itt3r;
               ipt1 = ipt[itt3-2];
               ipt2 = ipt[itt3-1];
               ipt3 = ipt[itt3  ];
               if ((ipl1==ipt1 || ipl1==ipt2 || ipl1==ipt3) ||
                   (ipl2==ipt1 || ipl2==ipt2 || ipl2==ipt3)) {
                  ntf = ntf+1;
                  itf[ntf] = itt3/3;
                  if (ntf==2) FOUND=1;  /* Break out */
               }
            }
            if (ntf < 2) continue;
           
            /* Determine the vertices of the triangles that do not lie
             * on the line segment 
             */
            it1t3 = itf[1]*3;
            ipti1 = ipt[it1t3-2];
            if (ipti1==ipl1 || ipti1==ipl2)
               ipti1 = ipt[it1t3-1]; 
            if (ipti1==ipl1 || ipti1==ipl2)
               ipti1 = ipt[it1t3  ]; 
            it2t3 = itf[2]*3;
            ipti2 = ipt[it2t3-2];
            if (ipti2==ipl1 || ipti2==ipl2)
               ipti2 = ipt[it2t3-1]; 
            if (ipti2==ipl1 || ipti2==ipl2)
               ipti2 = ipt[it2t3  ]; 
         
            /* Check if the exchange is necessary */
            if (idxchg(xd, yd, ipti1, ipti2, ipl1, ipl2) != 0) {
               /* Modify the ipt array when necessary */
               ipt[it1t3-2] = ipti1;
               ipt[it1t3-1] = ipti2;
               ipt[it1t3  ] = ipl1;
               ipt[it2t3-2] = ipti2;
               ipt[it2t3-1] = ipti1;
               ipt[it2t3  ] = ipl2;
               /* Set new flags in the iwl array */
               jwl = jwl+8;
               iwl[jwl-7] = ipl1;
               iwl[jwl-6] = ipti1;
               iwl[jwl-5] = ipti1;
               iwl[jwl-4] = ipl2;
               iwl[jwl-3] = ipl2;
               iwl[jwl-2] = ipti2;
               iwl[jwl-1] = ipti2;
               iwl[jwl  ] = ipl1;
               for (jlt3=3; jlt3<=nlt3; jlt3=jlt3+3) {
                  iplj1 = ipl[jlt3-2];
                  iplj2 = ipl[jlt3-1];
                  if (((iplj1==ipl1)&&(iplj2==ipti2)) ||
                      ((iplj2==ipl1)&&(iplj1==ipti2)))  
                     ipl[jlt3] = itf[1];
                  if (((iplj1==ipl2)&&(iplj2==ipti1)) ||
                      ((iplj2==ipl2)&&(iplj1==ipti1)))  
                     ipl[jlt3] = itf[2];
               }
            }
         }
         nlfc = nlf;
         nlf  = jwl/2;
         if (nlf != nlfc) {
            /* Reset the iwl array for the next round */
            jwl1mn = 2*nlfc+1;
            nlft2  = nlf*2;
            for (jwl1=jwl1mn; jwl1<=nlft2; jwl1++) {
               jwl = jwl1+1-jwl1mn;
               iwl[jwl] = iwl[jwl1];
            }
            nlf = jwl/2;
         }
      }
   }

   /* 
    * Rearrange the ipt array so that the vertices of each triangle are
    * listed counter-clockwise
    */
   for (itt3=3; itt3<=ntt3; itt3=itt3+3) {
      ip1 = ipt[itt3-2];
      ip2 = ipt[itt3-1];
      ip3 = ipt[itt3  ];
      if (vpdt(xd[ip1],yd[ip1],xd[ip2],yd[ip2],xd[ip3],yd[ip3]) < 0.0) {
         ipt[itt3-2] = ip2;
         ipt[itt3-1] = ip1;
      }     
   }
   *nt = nt0;
   *nl = nl0;
   *ipt0 = ipt;
   *ipl0 = ipl;

#ifdef DEBUG
   printf("\nend\n");
   for (il=1; il<=ntt3; il++)
      printf("ipt[%d] = %d\n",il,ipt[il]);
   for (il=1; il<=nlt3; il++)
      printf("ipl[%d] = %d\n",il,ipl[il]);
#endif

   /* Free work arrays */
   if (wk  ) free((char *)wk);
   if (iwp ) free((char *)iwp);
   if (iwl ) free((char *)iwl);

   /* print message if more than 100 points */
   if (ndp >= 50) {
      (void) fprintf(stdout,"   Done!  Found %d triangles\n",nt0);
   }

   /* Return */
   return(1);
}

/*
C THIS FUNCTION DETERMINES WHETHER OR NOT THE EXCHANGE OF TWO
C TRIANGLES IS NECESSARY ON THE BASIS OF MAX-MIN-ANGLE CRITERION
C BY C. L. LAWSON.
C THE INPUT PARAMETERS ARE
C     X,Y = ARRAYS CONTAINING THE COORDINATES OF THE DATA
C           POINTS,
C     I1,I2,I3,I4 = POINT NUMBERS OF FOUR POINTS P1, P2,
C           P3, AND P4 THAT FORM A QUADRILATERAL WITH P3
C           AND P4 CONNECTED DIAGONALLY.
C THIS FUNCTION RETURNS AN INTEGER VALUE 1 (ONE) WHEN AN EX-
C CHANGE IS NECESSARY, AND 0 (ZERO) OTHERWISE.
C DECLARATION STATEMENTS
*/
static int idxchg(x,y,i1,i2,i3,i4)
double *x, *y;       /* Coordinate arrays */
int    i1,i2,i3,i4;
{
   double x1, y1, x2, y2, x3, y3, x4, y4;
   double u1, u2, u3, u4;
   double a1sq, a2sq, a3sq, a4sq;
   double b1sq, b2sq, b3sq, b4sq;
   double c1sq, c2sq, c3sq, c4sq;
   double s1sq, s2sq, s3sq, s4sq;
   int    idx;

   /* Preliminary processing */
   x1 = x[i1];
   y1 = y[i1];
   x2 = x[i2];
   y2 = y[i2];
   x3 = x[i3];
   y3 = y[i3];
   x4 = x[i4];
   y4 = y[i4];

   /* Calculation */
   idx = 0;
   u3 = (y2-y3)*(x1-x3) - (x2-x3)*(y1-y3);
   u4 = (y1-y4)*(x2-x4) - (x1-x4)*(y2-y4);
   if (u3*u4 <= 0.0) return(idx);

   /* More Calculation */
   u1 = (y3-y1)*(x4-x1) - (x3-x1)*(y4-y1);
   u2 = (y4-y2)*(x3-x2) - (x4-x2)*(y3-y2);
   a1sq = dsqf(x1,y1,x3,y3);
   b1sq = dsqf(x1,y1,x4,y4);
   c1sq = dsqf(x3,y3,x4,y4);
   a2sq = dsqf(x2,y2,x4,y4);
   b2sq = dsqf(x2,y2,x3,y3);
   c2sq = c1sq;
   a3sq = b2sq;
   b3sq = a1sq;
   c3sq = dsqf(x2,y2,x1,y1);
   a4sq = b1sq;
   b4sq = a2sq;
   c4sq = c3sq;
   s1sq = u1*u1/(c1sq*MAXOF(a1sq,b1sq)); 
   s2sq = u2*u2/(c2sq*MAXOF(a2sq,b2sq)); 
   s3sq = u3*u3/(c3sq*MAXOF(a3sq,b3sq)); 
   s4sq = u4*u4/(c4sq*MAXOF(a4sq,b4sq)); 
   if ((MINOF(s3sq,s4sq)-MINOF(s1sq,s2sq)) > ESPLN)
      idx = 1;
   return(idx);
} 
