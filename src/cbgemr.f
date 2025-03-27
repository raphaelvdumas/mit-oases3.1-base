      SUBROUTINE CBGEMR(A,AR,NR,N,NRHS,IBW,EPS)     
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
C     SPECIAL SOLVER MEANT FOR SAFARI PACKAGE       
C     SOLVES A SET OF LINEAR EQUATIONS, WHERE THE       
C     COEFFICIENT MATRIX IS OF BAND FORM WITH THE       
C     BAND-WIDTH IBW (NUMBER OF SUB- OR SUPER DIAGONALS)
C     BY GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING.    
C       
C     THE COEFFICIENT MATRIX SHOULD BE STORED IN BAND FORM  
C     FOR THIS SOLVER!!!
C *** REVISED 14.1.83 TO WORK WITH ONE-DIMENSIONAL ARRAYS
C *** CLEANED UP 8.1.89 TO IMPROVE ALLIANT OPTIMIZATION
C
      COMPLEX*16 V,XC,CC1,cc2,cc3
      complex xs,A(1),AR(NR,NRHS)      
      REAL X,ANORM
C *** DYNAMIC BUFFER ARRAY ONLY FOR ALLIANT
C      COMPLEX VR(NRHS)
C *** STATEMENT FUNCTION TO CALCULATE INDEX IN BANDED FORM
      IB(IR,IC) = (IBW+IC-IR)*NR+IR
cvd$r relation(NR.gt.0)
cvd$r relation(IBW.gt.0)
      EPPS=EPS
      EPS=0
      IBM=IBW   
      NRM1=NR-1
      DO 91 I=1,N       
C     --------------------------------- 
C     SEARCH FOR PIVOT ROW      
C     --------------------------------- 

      L1=MIN0(N,I+IBW)  
      ANORM=-1 
      II=IB(I,I)
      JI=II
cvd$  shortloop
CDIR$ SHORTLOOP
      DO 30 J=I,L1      
       xs=A(JI)
       JI=JI-NRM1
       X=ABS(REAL(xs))+ABS(RIMAG(xs))
       IF (X.GT.ANORM) THEN
        ANORM=X   
        K=J       
       END IF
 30   CONTINUE  
      IF (K.NE.I) THEN
C     -------------------------------   
C     CALCULATE NEW BAND-WIDTH  
C     -------------------------------   
       IBM=MAX0(IBM,K-I+IBW)     
C     ------------------------------    
C     INTERCHANGE ROWS I AND K  
C     ------------------------------    
       L2=MIN0(N,I+IBM)  
       IJ=II
       KJ=IB(K,I)
cvd$  nodepchk
CDIR$ IVDEP
cvd$  shortloop
CDIR$ SHORTLOOP
       DO 50 J=I,L2      
        Xs=A(IJ) 
        A(IJ)=A(KJ)     
        A(KJ)=Xs 
        IJ=IJ+NR
        KJ=KJ+NR
 50    CONTINUE  
       DO 51 J=1,NRHS
        Xs=AR(I,J) 
        AR(I,J)=AR(K,J)     
        AR(K,J)=Xs
 51    CONTINUE 
      END IF
C     -------------------------------   
C     TEST FOR SINGULARITY      
C     -------------------------------   
      IF (ANORM.LT.EPPS) THEN  
C     --------------------------------  
C     MATRIX IS SINGULAR
C     --------------------------------  
       EPS=1
       RETURN
      END IF  
C     --------------------------------  
C     REDUCTION OF REMAINING ROWS       
C     --------------------------------  
      V=1E0/A(II)
      A(II)=V
      L2=MIN(I+IBM,N)
cvd$  shortloop
CDIR$ SHORTLOOP
cvd$  nodepchk
CDIR$ IVDEP
cvd$  select(concurrent)
      DO 85 K=I+1,L1    
       KI=IB(K,I)
       CC1=-A(KI)
       xs=cc1
       RR=REAL(xs)
       RI=RIMAG(xs)
       IF ((RR.NE.0.0) .OR. (RI.NE.0.0)) THEN
        XC=CC1*V
        A(KI)=CMPLX(0.,0.)    
        IJ=II
        KJ=KI
cvd$  shortloop
CDIR$ SHORTLOOP
cvd$  nodepchk
CDIR$ IVDEP
cvd$  select(vector)
        DO 80 J=I+1,L2    
         IJ=IJ+NR
         KJ=KJ+NR
         A(KJ)=A(KJ)+XC*A(IJ)
 80     CONTINUE
        DO 81 J=1,NRHS
         AR(K,J)=AR(K,J)+XC*AR(I,J)
 81     CONTINUE
       END IF
 85   CONTINUE  
 91   CONTINUE  
C     -------------------------------   
C     BACK SUBSTITUTION 
C     -------------------------------   
      DO 100 K=1,NRHS
       CC2=AR(n,k)
       cc3=a(ib(n,n))
       ar(n,k)=cc2*cc3
c       AR(N,K)=AR(N,K)*A(IB(N,N))      
       DO 100 I=N-1,1,-1 
        L2=MIN(N,I+IBM)
        II=IB(I,I)
        V=(0.,0.)
        IJ=II
cvd$  shortloop
CDIR$ SHORTLOOP
cvd$  select(vector)
        DO 95 J=I+1,L2    
         IJ=IJ+NR
         CC2=AR(j,k)
         cc3=a(ij)
         v=v+cc2*cc3
c         V=V+A(IJ)*AR(J,K)
 95     CONTINUE
        cc2=ar(i,k)
        cc3=a(ii)
        ar(i,k)=(cc2-v)*cc3
c        AR(I,K)=(AR(I,K)-V)*A(II)  
 100  CONTINUE
      RETURN    
      END       

