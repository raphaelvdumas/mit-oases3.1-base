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
