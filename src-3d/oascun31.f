      COMPLEX FUNCTION FILON(CFUN,CEX,ARG,N,FAC)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      COMPLEX AI,CSUM,CFUN(1),CEX(1),DFUN,DEX,DARG,DPROD
      DIMENSION ARG(1)
      AI=CMPLX(0.,1E0)
      CSUM=0E0
      DO 10 I=1,N-1
      DARG=-AI/(ARG(I+1)-ARG(I))
      DFUN=CFUN(I+1)-CFUN(I)
      DEX=CEX(I+1)-CEX(I)
      DPROD=CFUN(I+1)*CEX(I+1)-CFUN(I)*CEX(I)
      CSUM=CSUM+DARG*(DPROD-DARG*DFUN*DEX)
  10  CONTINUE
      FILON=FAC*CSUM
      RETURN
      END
      SUBROUTINE CMMUL(A,NRA,NCA,B,NCB,C)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      COMPLEX A(NRA,NCA),B(NCA,NCB),C(NRA,NCB)
      COMPLEX SUM
      DO 30 I=1,NRA
      DO 20 J=1,NCB
      SUM=CMPLX(0E0,0E0)
      DO 10 K=1,NCA
      SUM=SUM+A(I,K)*B(K,J)
  10  CONTINUE
      C(I,J)=SUM
  20  CONTINUE
  30  CONTINUE
      RETURN
      END        

      SUBROUTINE CTMMUL(A,NRA,NCA,B,NCB,C)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
c    
c     Subroutine for calculating the matrix product
c     of the complex transpose of A(NRA,NCA) and
c     the matrix B(NRA,NCB). The result is placed in
c     matrix C(NCA,NCB).
c
      COMPLEX A(NRA,NCA),B(NRA,NCB),C(NCA,NCB)
      COMPLEX SUM
      DO 30 I=1,NCA
      DO 20 J=1,NCB
      SUM=CMPLX(0E0,0E0)
      DO 10 K=1,NRA
      SUM=SUM+CONJG(A(K,I))*B(K,J)
  10  CONTINUE
      C(I,J)=SUM
  20  CONTINUE
  30  CONTINUE
      RETURN
      END        
        SUBROUTINE CMATIN(NM,A,AINV)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
        PARAMETER (NMAX=300)
        COMPLEX A(NM,NM),AINV(NM,NM)
C        DIMENSION AR(NMAX,NMAX),AI(NMAX,NMAX)
        DIMENSION BR(NMAX),BI(NMAX),CR(NMAX),
     1            CI(NMAX),IP(NMAX),IQ(NMAX)
C       DOUBLE PRECISION AR,AI,CR,CI,BR,BI,ZR,ZI,PR,PI
       AR(II,JJ)=REAL(AINV(II,JJ))
       AI(II,JJ)=RIMAG(AINV(II,JJ))
C    
C      REAL AND IMAGINARY PARTS OF MATRIX ADDRESSED BY 
C      STATEMENT FUNCTIONS
C
       IF (NM.GT.NMAX) THEN
         STOP '*** ORDER OF MATRIX TOO HIGH IN CMATIN ***'
       END IF
       DO 51 I=1,NM
        DO 51 J=1,NM
 51     AINV(I,J)=A(I,J)
       DO 1 K=1,NM
       PR=0.0D0
       PI=PR
       DO 100 I=K,NM
       DO 100 J=K,NM
       ZR=AR(I,J)
       ZI=AI(I,J)
       IF(ZR*ZR+ZI*ZI-PR*PR-PI*PI)100,100,101
 101    PR=AR(I,J)
       PI=AI(I,J)
        IP(K)=I
       IQ(K)=J
 100   CONTINUE
       IF(ABS(PR)+ABS(PI).LT.1.0D-37)GO TO 35
       IPK=IP(K)
       IQK=IQ(K)
       IF(IPK-K)200,299,200
 200   DO 201 J=1,NM
       ZR=AR(IPK,J)
       ZI=AI(IPK,J)
       AINV(IPK,J)=AINV(K,J)
       AINV(K,J)=CMPLX(ZR,ZI)
 201   CONTINUE
 299   CONTINUE
       IF(IQK-K)300,399,300
 300   DO 301 I=1,NM
       ZR=AR(I,IQK)
       ZI=AI(I,IQK)
       AINV(I,IQK)=AINV(I,K)
       AINV(I,K)=CMPLX(ZR,ZI)
 301   CONTINUE
 399   CONTINUE
        ZR=1.0D0/(PR*PR+PI*PI)
       DO 400 J=1,NM
       IF(J-K)403,402,403
 402   BR(J)=PR*ZR
       BI(J)=-PI*ZR
       CR(J)=1.0D0
       CI(J)=0.0D0
       GO TO 404
 403   BR(J)=-(AR(K,J)*PR+AI(K,J)*PI)*ZR
       BI(J)=-(AI(K,J)*PR-AR(K,J)*PI)*ZR
       CR(J)=AR(J,K)
       CI(J)=AI(J,K)
 404   AINV(K,J)=CMPLX(0E0,0E0)
       AINV(J,K)=CMPLX(0E0,0E0)
  400  CONTINUE
       DO 405 I=1,NM
       DO 405 J=1,NM
       AINV(I,J)=AINV(I,J)+CMPLX(CR(I)*BR(J)-CI(I)*BI(J),
     1       CR(I)*BI(J)+CI(I)*BR(J))
 405   CONTINUE
  1    CONTINUE
       K=NM
       DO 500 KM=1,NM
       IPK=IP(K)
       IQK=IQ(K)
       IF(IPK-K)501,502,501
 501   DO 503 I=1,NM
       ZR=AR(I,IPK)
       ZI=AI(I,IPK)
       AINV(I,IPK)=AINV(I,K)
       AINV(I,K)=CMPLX(ZR,ZI)
 503   CONTINUE
 502   IF(IQK-K)504,500,504
 504   DO 506 J=1,NM
       ZR=AR(IQK,J)
       ZI=AI(IQK,J)
       AINV(IQK,J)=AINV(K,J)
       AINV(K,J)=CMPLX(ZR,ZI)
 506   CONTINUE
 500   K=K-1
          RETURN
  35      PRINT 37
  37      FORMAT(' ***** MATRIX SINGULAR PROGRAM STOPS ***** ')
           STOP
          END
        SUBROUTINE CMINV(NM,A,AINV,IERR)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
        PARAMETER (NMAX=300)
        COMPLEX A(NM,NM),AINV(NM,NM)
C        DIMENSION AR(NMAX,NMAX),AI(NMAX,NMAX)
        DIMENSION BR(NMAX),BI(NMAX),CR(NMAX),
     1            CI(NMAX),IP(NMAX),IQ(NMAX)
C       DOUBLE PRECISION AR,AI,CR,CI,BR,BI,ZR,ZI,PR,PI
C    
C      REAL AND IMAGINARY PARTS OF MATRIX ADDRESSED BY 
C      STATEMENT FUNCTIONS
C
       AR(II,JJ)=REAL(AINV(II,JJ))
       AI(II,JJ)=RIMAG(AINV(II,JJ))

       IERR=0
       IF (NM.GT.NMAX) THEN
         STOP '*** ORDER OF MATRIX TOO HIGH IN CMINV ***'
       END IF
       DO 51 I=1,NM
        DO 51 J=1,NM
 51     AINV(I,J)=A(I,J)
       DO 1 K=1,NM
       PR=0.0D0
       PI=PR
       DO 100 I=K,NM
       DO 100 J=K,NM
       ZR=AR(I,J)
       ZI=AI(I,J)
       IF(ZR*ZR+ZI*ZI-PR*PR-PI*PI)100,100,101
 101    PR=AR(I,J)
       PI=AI(I,J)
        IP(K)=I
       IQ(K)=J
 100   CONTINUE
       IF(ABS(PR)+ABS(PI).LT.1.0D-20)GO TO 35
       IPK=IP(K)
       IQK=IQ(K)
       IF(IPK-K)200,299,200
 200   DO 201 J=1,NM
       ZR=AR(IPK,J)
       ZI=AI(IPK,J)
       AINV(IPK,J)=AINV(K,J)
       AINV(K,J)=CMPLX(ZR,ZI)
 201   CONTINUE
 299   CONTINUE
       IF(IQK-K)300,399,300
 300   DO 301 I=1,NM
       ZR=AR(I,IQK)
       ZI=AI(I,IQK)
       AINV(I,IQK)=AINV(I,K)
       AINV(I,K)=CMPLX(ZR,ZI)
 301   CONTINUE
 399   CONTINUE
        ZR=1.0D0/(PR*PR+PI*PI)
       DO 400 J=1,NM
       IF(J-K)403,402,403
 402   BR(J)=PR*ZR
       BI(J)=-PI*ZR
       CR(J)=1.0D0
       CI(J)=0.0D0
       GO TO 404
 403   BR(J)=-(AR(K,J)*PR+AI(K,J)*PI)*ZR
       BI(J)=-(AI(K,J)*PR-AR(K,J)*PI)*ZR
       CR(J)=AR(J,K)
       CI(J)=AI(J,K)
 404   AINV(K,J)=CMPLX(0E0,0E0)
       AINV(J,K)=CMPLX(0E0,0E0)
  400  CONTINUE
       DO 405 I=1,NM
       DO 405 J=1,NM
       AINV(I,J)=AINV(I,J)+CMPLX(CR(I)*BR(J)-CI(I)*BI(J),
     1                           CR(I)*BI(J)+CI(I)*BR(J))
 405   CONTINUE
  1    CONTINUE
       K=NM
       DO 500 KM=1,NM
       IPK=IP(K)
       IQK=IQ(K)
       IF(IPK-K)501,502,501
 501   DO 503 I=1,NM
       ZR=AR(I,IPK)
       ZI=AI(I,IPK)
       AINV(I,IPK)=AINV(I,K)
       AINV(I,K)=CMPLX(ZR,ZI)
 503   CONTINUE
 502   IF(IQK-K)504,500,504
 504   DO 506 J=1,NM
       ZR=AR(IQK,J)
       ZI=AI(IQK,J)
       AINV(IQK,J)=AINV(K,J)
       AINV(K,J)=CMPLX(ZR,ZI)
 506   CONTINUE
 500   K=K-1
          RETURN
  35      IERR=1
          RETURN
          END
      subroutine vdecim(a,i,b,j,nold,ndec,nnew)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      parameter (pi=3.14159)
c
c     subroutine for decimating a vector by running averaging
c     Output vector will be of dimension 1+(nold-1)/ndec 
C
      dimension a(1),b(1)
      dimension w(1001)
      np=2*ndec + 1
      nnew=1+(nold-1)/ndec
      if (nnew.gt.nold) then
        write(6,*) '>>>> ERROR in VDECIM: nnew > nold <<<<'
        stop
      else if (np.gt.1001) then
        stop '>>>> ERROR in VDECIM: Cannot decimate by more than 250'
      end if
c
c     determine window parameters
c
      faca=pi/(np-1)
      wsum=0E0
      do 10 l=1,np          
       w(l)=sin((l-1)*faca)**2
       wsum=wsum+w(l)
 10   continue
      fac=1E0/wsum 
      INEW=I*NDEC
      i1=1
      j1=1
      b(1)=a(1)
      i1=i1+INEW
      j1=j1+j
      do 20 l=2,nnew-1
        sum=0
        iof=-ndec*i
        do 15 k=-ndec,ndec
          sum=sum+w(ndec+1+k)*a(i1+iof)
          iof=iof+i
 15     continue
        b(j1)=fac*sum
        i1=i1+INEW
        j1=j1+j
 20   continue
      b(j1)=A(I1)
      return
      end
      subroutine vfltsm(a,b,nold,dold,nnew,dnew)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      parameter (pi=3.14159)
c
c     subroutine for decimating a vector by running averaging
c     Output vector will be of dimension 1+(nold-1)/ndec 
c     similar to vdecim, but sampling points do not have to
c     coincide.
C
      dimension a(1),b(1)
      dimension w(1001)
      ndec=dnew/dold
      ndec=max(ndec,2)
      np=2*ndec + 1
      if (np.gt.1001) then
        stop '>>>> ERROR in VFLTSM: Cannot decimate by more than 500'
      end if
c
c     determine window parameters
c
      faca=pi/(np-1)
      wsum=0E0
      do 10 l=1,np          
       w(l)=sin((l-1)*faca)**2
       wsum=wsum+w(l)
 10   continue
      fac=1E0/wsum
      od=1e0/dold 
      b(1)=a(1)
      do 20 l=2,nnew
        sum=0
        wsum=0
        do 15 k=-ndec,ndec
         xx=(l-1)*dnew+k*dold
         io=int(xx*od+1.0)
         io1=io+1
         if (io.gt.0.and.io1.le.nold) then
          xo=(io-1)*dold
          sum=sum+w(ndec+1+k)*(a(io)+(xx-xo)*(a(io1)-a(io))*od)
          wsum=wsum+w(ndec+1+k)
         end if
 15     continue
        b(l)=sum/wsum
 20   continue
      return
      end
      COMPLEX FUNCTION CINTPL(C,XMIN,ONODX,N,X)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
C *** INTERPOLATES IN A COMPLEX ARRAY C TO DETERMINE VALUE
C *** FOR ARGUMENT X
      COMPLEX C(N)
      RINDEX=(X-XMIN)*ONODX+1
      INDEX=INT(RINDEX)
      IF (INDEX.GE.N) THEN
C        WRITE(6,*) '>>> WARNING: ARRAY OVERFLOW IN CINTPL, ARG=',X
      END IF
c >>> 2 and 3 pt
      INDEX=MIN(INDEX,N-1)
c >>> 4 pt
      INDEX=MIN(INDEX,N-2)

      INDEX=MAX(INDEX,2) 
      REM=RINDEX-INDEX
c >>> 2 pt
c      CINTPL=(1E0-REM)*C(INDEX) + REM*C(INDEX+1)
c >>> 3 pt
c      CINTPL=  0.5*REM*(REM-1E0)*C(INDEX-1)
c     &       + (1E0-REM*REM)*C(INDEX)  
c     &       + 0.5*REM*(REM+1E0)*C(INDEX+1)
c >>> 4 pt
      CINTPL = -0.1666667*REM*(REM-1E0)*(REM-2E0)*C(INDEX-1)
     &       + 0.5* (REM*REM-1E0)*(REM-2E0)*C(INDEX)  
     &       - 0.5* REM*(REM+1E0)*(REM-2E0)*C(INDEX+1)  
     &       + 0.1666667*REM*(REM*REM-1E0)*C(INDEX+2)
      RETURN
      END
      REAL FUNCTION RINT_2pt(C,XMIN,ONODX,N,X)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
C *** INTERPOLATES IN A REAL ARRAY C TO DETERMINE VALUE
C *** FOR ARGUMENT X
c >>> 2 pt Lagrange
      REAL C(N)
      RINDEX=(X-XMIN)*ONODX+1
      INDEX=INT(RINDEX)
      IF (INDEX.GE.N) THEN
        WRITE(6,*) '>>> WARNING: ARRAY OVERFLOW IN RINTPL, ARG=',X
      END IF
      INDEX=MIN(INDEX,N-1)
      INDEX=MAX(INDEX,1) 
      REM=RINDEX-INDEX
      RINTPL=(1E0-REM)*C(INDEX) + REM*C(INDEX+1)
      RETURN
      END
      REAL FUNCTION RINT_3pt(C,XMIN,ONODX,N,X)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
C *** INTERPOLATES IN A REAL ARRAY C TO DETERMINE VALUE
C *** FOR ARGUMENT X
c >>> 3 pt Lagrange
      REAL C(N)
      RINDEX=(X-XMIN)*ONODX+1
      INDEX=IFIX(RINDEX)
      IF (INDEX.GT.N) THEN
        WRITE(6,*) '>>> WARNING: ARRAY OVERFLOW IN RINTPL, ARG=',X
      END IF
      INDEX=MIN(INDEX,N-1)
      INDEX=MAX(INDEX,2) 
      REM=RINDEX-INDEX
      RINTPL=  0.5*REM*(REM-1E0)*C(INDEX-1)
     &       + (1E0-REM*REM)*C(INDEX)  
     &       + 0.5*REM*(REM+1E0)*C(INDEX+1)
      RETURN
      END

      REAL FUNCTION RINTPL(C,XMIN,ONODX,N,X)
c      REAL FUNCTION RINT_4pt(C,XMIN,ONODX,N,X)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
C *** INTERPOLATES IN A REAL ARRAY C TO DETERMINE VALUE
C *** FOR ARGUMENT X
c >>> 4 pt Lagrange
      REAL C(N)
      RINDEX=(X-XMIN)*ONODX+1
      INDEX=IFIX(RINDEX)
      IF (INDEX.GT.N) THEN
        WRITE(6,*) '>>> WARNING: ARRAY OVERFLOW IN RINTPL, ARG=',X
      END IF
      INDEX=MIN(INDEX,N-2)
      INDEX=MAX(INDEX,2) 
      REM=RINDEX-INDEX
      RINTPL = -0.1666667*REM*(REM-1E0)*(REM-2E0)*C(INDEX-1)
     &       + 0.5* (REM*REM-1E0)*(REM-2E0)*C(INDEX)  
     &       - 0.5* REM*(REM+1E0)*(REM-2E0)*C(INDEX+1)  
     &       + 0.1666667*REM*(REM*REM-1E0)*C(INDEX+2)
      RETURN
      END
      SUBROUTINE PREPBF(RMAX,WKMAX)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      PARAMETER (NBDIV=40,nbfbuf=2048)
      INCLUDE 'compar.f'
      INCLUDE 'combes.f'
      REAL RJBUF(nbfbuf)
      RKMAX=RMAX*WKMAX
      NRKMAX=MAX(NINT(NBDIV*(RKMAX-brk_0)/(2*PI)),2)
      if (mbmaxi+mbf_0.gt.nbfbuf) then
       stop '>>> NBFBUF in PREPBF (oascun22) too small <<<'
      end if

      IF (NRKMAX.GT.NPBES) THEN
       WRITE(6,*) '>>> WARNING: NUMBER OF BESSEL INTERPOLATION'
       WRITE(6,*) '>>>          POINTS TRUNCATED TO'
       WRITE(6,*) '>>>         ', NBDIV*FLOAT(NPBES)/NRKMAX,
     &            ' PER LAMBDA'
       NRKMAX=NPBES
      END IF
      write(6,*) 'mbf_0,brk_0=',mbf_0,brk_0
      BESDRK=MAX(1E-6,RKMAX-brk_0)/(NRKMAX-1.001)
      ONODRK=1E0/BESDRK
      DO 10 II=1,NRKMAX
       RARG=brk_0+BESDRK*(II-1)
       CALL BESSJN(MBMAX+mbf_0,RARG,RJBUF)
       DO 9 M=1,MBMAXI
 9      BF(II,M)=RJBUF(M+mbf_0)
        if (debug) then
         write(91,*) rarg,bf(ii,1)
        end if
 10   CONTINUE
      RETURN
      END
      SUBROUTINE BESJ01(ARG,B0,B1)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      REAL ARG,B0,B1
      B0=BESSJ0(ARG)
      B1=BESSJ1(ARG)
      RETURN
      END
      REAL FUNCTION bessj0(x)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      REAL X,AX,Z
      REAL*8 XX,Y,ANS,ANS1,ANS2
      AX=ABS(X)
      if (ax.LT. 8.0) THEN
       y=x*x
       ans1=57568490574.0D0+y*(-13362590354.0D0+y*(651619640.7D0
     &      +y*(-11214424.18D0+y*(77392.33017D0+y*(-184.9052456D0)))))
       ans2=57568490411.0D0+y*(1029532985.0D0+y*(9494680.718D0
     &      +y*(59272.64853D0+y*(267.8532712D0+y*1.0D0))))
       ans=ans1/ans2
      else 
       z=8.0/ax
       y=z*z
       xx=ax-0.785398164
       ans1=1.0D0+y*(-0.1098628627D-2+y*(0.2734510407D-4
     &      +y*(-0.2073370639D-5+y*0.2093887211D-6)))
       ans2 = -0.1562499995D-1+y*(0.1430488765D-3
     &        +y*(-0.6911147651D-5+y*(0.7621095161D-6
     &        -y*0.934935152D-7)))
       ans=sqrt(0.636619772D0/ax)*(cos(xx)*ans1-z*sin(xx)*ans2)
      END IF
      BESSJ0=ANS
      RETURN
      END
      REAL FUNCTION bessj1(x)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      REAL X,ax,z
      REAL*8 xx,y,ans,ans1,ans2
      AX=ABS(X)
      if (ax.LT. 8.0) THEN
       y=x*x
       ans1=x*(72362614232.0D0+y*(-7895059235.0D0+y*(242396853.1D0
     &      +y*(-2972611.439D0+y*(15704.48260D0+y*(-30.16036606D0))))))
       ans2=144725228442.0D0+y*(2300535178.0D0+y*(18583304.74D0
     &      +y*(99447.43394D0+y*(376.9991397D0+y*1.0D0))))
       ans=ans1/ans2
      else 
       z=8.0/ax
       y=z*z
       xx=ax-2.356194491D0
       ans1=1.0D0+y*(0.183105D-2+y*(-0.3516396496D-4
     &      +y*(0.2457520174D-5+y*(-0.240337019D-6))))
       ans2=0.04687499995D0+y*(-0.2002690873D-3
     &      +y*(0.8449199096D-5+y*(-0.88228987D-6
     &      +y*0.105787412D-6)))
       ans=sqrt(0.636619772D0/ax)*(cos(xx)*ans1-z*sin(xx)*ans2)
       if (x .LT. 0.0) ans = -ans
      END IF
      BESSJ1=ANS
      RETURN
      END
      SUBROUTINE BESSJN(N,X,BJP)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      PARAMETER (IACC=40,BIGNO=1.E10,BIGNI=1.E-10)
      DIMENSION BJP(*)
      IF (ABS(X).LT.1E-6) THEN
       BJP(1)=1E0
       DO 8 JJ=2,N+1
 8     BJP(JJ)=0E0
      ELSE IF (N.EQ.0) THEN
       BJP(1)=BESSJ0(X)
      ELSE IF (N.EQ.1) THEN
       BJP(1)=BESSJ0(X)
       BJP(2)=BESSJ1(X)
      ELSE
       TOX=2./X
       IF(X.GT.FLOAT(N))THEN
        BJP(1)=BESSJ0(X)
        BJP(2)=BESSJ1(X)
        DO 10 J=1,N-1
          BJP(J+2)=J*TOX*BJP(J+1)-BJP(J)
10      CONTINUE
       ELSE
        M=2*((N+INT(SQRT(FLOAT(IACC*N))))/2)
        BESSJ=0.
        JSUM=0
        SUM=0.
        BJP1=0.
        BJ=1.
        DO 12 J=M,1,-1
          BJM1=J*TOX*BJ-BJP1
          BJP1=BJ
          BJ=BJM1
          IF(ABS(BJ).GT.BIGNO)THEN
            BJ=BJ*BIGNI
            BJP1=BJP1*BIGNI
            DO 11 JJ=J+2,N+1
 11         BJP(JJ)=BJP(JJ)*BIGNI
            SUM=SUM*BIGNI
          ENDIF
          IF(JSUM.NE.0)SUM=SUM+BJ
          JSUM=1-JSUM
          IF(J.LE.N) BJP(J+1)=BJP1
 12     CONTINUE
        BJP(1)=BJ
        SUM=2.*SUM-BJ
        DO 13 JJ=1,N+1
 13     BJP(JJ)=BJP(JJ)/SUM
       ENDIF
      END IF
      RETURN
      END


      FUNCTION bessy0(x)
      REAL bessy0,x
CU    USES bessj0
      REAL xx,z,bessj0
      DOUBLE PRECISION p1,p2,p3,p4,p5,q1,q2,q3,q4,q5,r1,r2,r3,r4,r5,r6,
     *s1,s2,s3,s4,s5,s6,y
      SAVE p1,p2,p3,p4,p5,q1,q2,q3,q4,q5,r1,r2,r3,r4,r5,r6,s1,s2,s3,s4,
     *s5,s6
      DATA p1,p2,p3,p4,p5/1.d0,-.1098628627d-2,.2734510407d-4,
     *-.2073370639d-5,.2093887211d-6/, q1,q2,q3,q4,q5/-.1562499995d-1,
     *.1430488765d-3,-.6911147651d-5,.7621095161d-6,-.934945152d-7/
      DATA r1,r2,r3,r4,r5,r6/-2957821389.d0,7062834065.d0,
     *-512359803.6d0,10879881.29d0,-86327.92757d0,228.4622733d0/,s1,s2,
     *s3,s4,s5,s6/40076544269.d0,745249964.8d0,7189466.438d0,
     *47447.26470d0,226.1030244d0,1.d0/
      if(x.lt.8.)then
        y=x**2
        bessy0=(r1+y*(r2+y*(r3+y*(r4+y*(r5+y*r6)))))/(s1+y*(s2+y*(s3+y*
     *(s4+y*(s5+y*s6)))))+.636619772*bessj0(x)*log(x)
      else
        z=8./x
        y=z**2
        xx=x-.785398164
        bessy0=sqrt(.636619772/x)*(sin(xx)*(p1+y*(p2+y*(p3+y*(p4+y*
     *p5))))+z*cos(xx)*(q1+y*(q2+y*(q3+y*(q4+y*q5)))))
      endif
      return
      END

      SUBROUTINE four1(data,nn,isign)
      INTEGER isign,nn
      REAL data(2*nn)
      INTEGER i,istep,j,m,mmax,n
      REAL tempi,tempr
      DOUBLE PRECISION theta,wi,wpi,wpr,wr,wtemp
      n=2*nn
      j=1
      do 11 i=1,n,2
        if(j.gt.i)then
          tempr=data(j)
          tempi=data(j+1)
          data(j)=data(i)
          data(j+1)=data(i+1)
          data(i)=tempr
          data(i+1)=tempi
        endif
        m=n/2
1       if ((m.ge.2).and.(j.gt.m)) then
          j=j-m
          m=m/2
        goto 1
        endif
        j=j+m
11    continue
      mmax=2
2     if (n.gt.mmax) then
        istep=2*mmax
        theta=6.28318530717959d0/(isign*mmax)
        wpr=-2.d0*sin(0.5d0*theta)**2
        wpi=sin(theta)
        wr=1.d0
        wi=0.d0
        do 13 m=1,mmax,2
          do 12 i=m,n,istep
            j=i+mmax
            tempr=sngl(wr)*data(j)-sngl(wi)*data(j+1)
            tempi=sngl(wr)*data(j+1)+sngl(wi)*data(j)
            data(j)=data(i)-tempr
            data(j+1)=data(i+1)-tempi
            data(i)=data(i)+tempr
            data(i+1)=data(i+1)+tempi
12        continue
          wtemp=wr
          wr=wr*wpr-wi*wpi+wr
          wi=wi*wpr+wtemp*wpi+wi
13      continue
        mmax=istep
      goto 2
      endif
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software 0H21.

      SUBROUTINE fourn(data,nn,ndim,isign)
      INTEGER isign,ndim,nn(ndim)
      REAL data(*)
      INTEGER i1,i2,i2rev,i3,i3rev,ibit,idim,ifp1,ifp2,ip1,ip2,ip3,k1,
     *k2,n,nprev,nrem,ntot
      REAL tempi,tempr
      DOUBLE PRECISION theta,wi,wpi,wpr,wr,wtemp
      ntot=1
      do 11 idim=1,ndim
        ntot=ntot*nn(idim)
11    continue
      nprev=1
      do 18 idim=1,ndim
        n=nn(idim)
        nrem=ntot/(n*nprev)
        ip1=2*nprev
        ip2=ip1*n
        ip3=ip2*nrem
        i2rev=1
        do 14 i2=1,ip2,ip1
          if(i2.lt.i2rev)then
            do 13 i1=i2,i2+ip1-2,2
              do 12 i3=i1,ip3,ip2
                i3rev=i2rev+i3-i2
                tempr=data(i3)
                tempi=data(i3+1)
                data(i3)=data(i3rev)
                data(i3+1)=data(i3rev+1)
                data(i3rev)=tempr
                data(i3rev+1)=tempi
12            continue
13          continue
          endif
          ibit=ip2/2
1         if ((ibit.ge.ip1).and.(i2rev.gt.ibit)) then
            i2rev=i2rev-ibit
            ibit=ibit/2
          goto 1
          endif
          i2rev=i2rev+ibit
14      continue
        ifp1=ip1
2       if(ifp1.lt.ip2)then
          ifp2=2*ifp1
          theta=isign*6.28318530717959d0/(ifp2/ip1)
          wpr=-2.d0*sin(0.5d0*theta)**2
          wpi=sin(theta)
          wr=1.d0
          wi=0.d0
          do 17 i3=1,ifp1,ip1
            do 16 i1=i3,i3+ip1-2,2
              do 15 i2=i1,ip3,ifp2
                k1=i2
                k2=k1+ifp1
                tempr=sngl(wr)*data(k2)-sngl(wi)*data(k2+1)
                tempi=sngl(wr)*data(k2+1)+sngl(wi)*data(k2)
                data(k2)=data(k1)-tempr
                data(k2+1)=data(k1+1)-tempi
                data(k1)=data(k1)+tempr
                data(k1+1)=data(k1+1)+tempi
15            continue
16          continue
            wtemp=wr
            wr=wr*wpr-wi*wpi+wr
            wi=wi*wpr+wtemp*wpi+wi
17        continue
          ifp1=ifp2
        goto 2
        endif
        nprev=n*nprev
18    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software 0H21.

      FUNCTION gasdev(idum)
      INTEGER idum
      REAL gasdev
CU    USES ran1
      INTEGER iset
      REAL fac,gset,rsq,v1,v2,ran1
      SAVE iset,gset
      DATA iset/0/
      if (iset.eq.0) then
1       v1=2.*ran1(idum)-1.
        v2=2.*ran1(idum)-1.
        rsq=v1**2+v2**2
        if(rsq.ge.1..or.rsq.eq.0.)goto 1
        fac=sqrt(-2.*log(rsq)/rsq)
        gset=v1*fac
        gasdev=v2*fac
        iset=1
      else
        gasdev=gset
        iset=0
      endif
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software 0H21.

      FUNCTION ran1(idum)
      INTEGER idum,IA,IM,IQ,IR,NTAB,NDIV
      REAL ran1,AM,EPS,RNMX
      PARAMETER (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836,
     *NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
      INTEGER j,k,iv(NTAB),iy
      SAVE iv,iy
      DATA iv /NTAB*0/, iy /0/
      if (idum.le.0.or.iy.eq.0) then
        idum=max(-idum,1)
        do 11 j=NTAB+8,1,-1
          k=idum/IQ
          idum=IA*(idum-k*IQ)-IR*k
          if (idum.lt.0) idum=idum+IM
          if (j.le.NTAB) iv(j)=idum
11      continue
        iy=iv(1)
      endif
      k=idum/IQ
      idum=IA*(idum-k*IQ)-IR*k
      if (idum.lt.0) idum=idum+IM
      j=1+iy/NDIV
      iy=iv(j)
      iv(j)=idum
      ran1=min(AM*iy,RNMX)
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software 0H21.

      FUNCTION gammln(xx)
      REAL gammln,xx
      INTEGER j
      DOUBLE PRECISION ser,stp,tmp,x,y,cof(6)
      SAVE cof,stp
      DATA cof,stp/76.18009172947146d0,-86.50532032941677d0,
     *24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2,
     *-.5395239384953d-5,2.5066282746310005d0/
      x=xx
      y=x
      tmp=x+5.5d0
      tmp=(x+0.5d0)*log(tmp)-tmp
      ser=1.000000000190015d0
      do 11 j=1,6
        y=y+1.d0
        ser=ser+cof(j)/y
11    continue
      gammln=tmp+log(stp*ser/x)
      return
      END
