C*********************************************
C
C     SIMMULATION OF APMATH64 LIBRARY ROUTINES
C     AND RELATED APAL64 ROUTINES.
C
C     H. SCHMIDT    SACLANTCEN    830217
C
C*********************************************
      SUBROUTINE CFFT(DATA,N,IFORW)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION DATA(2*N)
      ISIGN=-IFORW
      IP0=2
      IP3=IP0*N
      I3REV=1
      DO 50 I3=1,IP3,IP0
      IF(I3-I3REV)10,20,20
10    TEMPR=DATA(I3)
      TEMPI=DATA(I3+1)
      DATA(I3)=DATA(I3REV)
      DATA(I3+1)=DATA(I3REV+1)
      DATA(I3REV)=TEMPR
      DATA(I3REV+1)=TEMPI
20    IP1=IP3/2
30    IF(I3REV-IP1)50,50,40 
40    I3REV=I3REV-IP1       
      IP1=IP1/2             
      IF(IP1-IP0)50,30,30   
50    I3REV=I3REV+IP1       
      IP1=IP0               
60    IF(IP1-IP3)70,100,100 
70    IP2=IP1*2             
      THETA=6.283185307/FLOAT(ISIGN*IP2/IP0)
      SINTH=SIN(THETA/2.)
      WSTPR=-2.*SINTH*SINTH
      WSTPI=SIN(THETA)     
      WR=1.                
      WI=0.                
      DO 90 I1=1,IP1,IP0   
      DO 80 I3=I1,IP3,IP2  
      I2A=I3               
      I2B=I2A+IP1          
      TEMPR=WR*DATA(I2B)-WI*DATA(I2B+1)
      TEMPI=WR*DATA(I2B+1)+WI*DATA(I2B)
      DATA(I2B)=DATA(I2A)-TEMPR        
      DATA(I2B+1)=DATA(I2A+1)-TEMPI    
      DATA(I2A)=DATA(I2A)+TEMPR        
80    DATA(I2A+1)=DATA(I2A+1)+TEMPI    
      TEMPR=WR                
      WR=WR*WSTPR-WI*WSTPI+WR 
90    WI=WI*WSTPR+TEMPR*WSTPI+WI 
      IP1=IP2  
      GO TO 60 
100   RETURN   
      END      
      SUBROUTINE RFT(A,B,N,ISN)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
C    C,10/11/80.V1 RTD <PN> :FFT LIBRARY 
C    IF ISN=1,THIS SUBROUTINE COMPLETES THE FOURIER TRANSFORM
C    STORED ALTERNATELY IN ARRAYS A AND B, AND ARE FIRST     
C    TRANSFORMED BY A COMPLEX FOURIER TRANSFORM OF DIMENSION N.
C    THE COSINE COEFFICIENTS ARE IN A(1),A(2),...A(N+1) AND  
C     THE SINE COEFFICIENTS ARE IN B(1),B(2),...B(N+1). 
C    A TYPICAL CALLING SEQUENCE IS 
C         CALL FFT(A,B,N,N,N,1)
C         CALL RFT(A,B,N,1)
C    THE RESULTS SHOULD BE MULTIPLIED BY 0.5 /N TO GIVE THE
C    USUAL SCALING OF COEFFICIENTS.
C    IF ISN=-1, THE INVERSE TRANSFORMATION IS DONE, THE FIRST
C    STEP IN EVALUATING A REAL FOURIER SERIES.
C    A TYPICAL CALLING SEQUENCE IS
C         CALL RFT(A,B,N,-1)
C         CALL FFT(A,B,N,N,N,-1)
C    THE RESULTS SHOULD BE MULTIPLIED BY 0.5 TO GIVE THE USUAL
C    SCALING , AND THE TIME DOMAIN RESULTS ALTERNATE IN ARRAYS
C     A AND B, I.E.,A(1),B(1),A(2),B(2),...A(N),B(N).
C    THE DATA MAY ALTERNATIVELY BE STORED IN A SINGLE COMPLEX
C     ARRAY A, THEN THE MAGNITUDE OF ISN CHANGED TO TWO TO
C    GIVE THE CORRECT INDEXING INCREMENT AND A(2) USED TO PASS
C    THE INITIAL ADDRESS FOR THE SEQUENCE OF IMAGINARY
C    VALUES,E.G.
C          CALL FFT(A,A(2),N,N,N,2)
C          CALL RFT(A,A(2),N,2)
C    IN THIS CASE, THE COSINE AND SINE COEFFICIENTS ALTERNATE IN
C    A,  BY R.C. SINGLETON, STANFORD RESEARCH INSTITUTE, OCT. 1968
C
C NOTE FROM THE USERS:
C 
C     BUGS WERE FOUND IN THIS VERSION OF RFT, WHICH HAS BEEN USED IN
C    BASIC I,II,AND III.   THESE ARE:
C
C      1)  TWO VALUES OUTSIDE THE USER'S BUFFER, X(N+1) AND X(N+2) FOR
C     A TRANSFORM OF N DATA POINTS, ARE CLOBBERED IN AN ATTEMPT AT
C     SCRATCH STORAGE.  THIS HAS BEEN CORRECTED IN THE LATER VERSION, AN
C     THE OLD PROGRAM, IDENTICAL IN EXECUTION, BUT WHICH DID NOT CONTAIN
C     CORREKT THINKING IN ITS NONUSE OF COMMENTS, HAS BEEN DESTROYED.
C
C     2)  THE INVERSE TRANSFORM STATEMENTS SHOULD BE CHANGED, SO THAT
C     A(K),A(K+1) (BETWEEN STATEMENTS 5 & 10) IS DEFINED.
C
C     3)   ROUNDED ARITHMETIC SHOULD BE USED.
C
       DIMENSION A(1),B(1)
       INC=IABS(ISN)
       NK=N*INC+2
       NH=NK/2
       SD=-2.0*ATAN(1.0)/FLOAT(N)
       CD=2.0*SIN(SD)**2
       SD=SIN(SD+SD)
      IF(ISN.LT.0)GOTO 30
      CN=1.
C DO FIRST LOOP
C  STORING NYQUIST REAL IN IMAGINARY 0 SAMPLE & VICEVERSA
10    AA  =2.*(A(1)+B(1))
      B(1)=2.*(A(1)-B(1))
C
12    A(1)=AA
C INCREMENT SIN & COSINE
      SN=SD*CN
      CN=CN*(1.-CD)  
      L=1+INC
C MAIN LOOP
      DO 20 J=L,NH,INC
C OK, FROM HERE ON IT'S ALMOST THE SAME.
       K=NK-J
       AA=A(J)+A(K)
       AB=A(J)-A(K)
       BA=B(J)+B(K)
       BB=B(J)-B(K)
       RE=CN*BA+SN*AB
       FIM=SN*BA-CN*AB
       B(K)=FIM-BB 
       B(J)=FIM+BB
       A(K)=AA-RE
       A(J)=AA+RE
       AA=CN-(CD*CN+SD*SN)
       SN=(SD*CN-CD*SN)+SN
C    THE FOLLOWING THREE STATEMENTS COMPENSATE FOR TRUNCATION
C     ERROR.  IF ROUNDED ARITHMETIC IS USED, SUBSTITUTE
   20  CN=AA
C      CN=0.5/(AA**2+SN**2)+0.5
C      SN=CN*SN
C20    CN=CN*AA
       RETURN
C INVERSE TRANSFORM
 30    CN=-1.0
       SD=-SD
       AA  = A(1)+B(1)
       B(1)= A(1)-B(1) 
       GO TO 12
       END
      SUBROUTINE RFFT(A,NX,IFORW)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(NX)
      IF (IFORW.GT.0) THEN
      CALL CFFT(A,NX/2,1)
      CALL RFT(A(1),A(2),NX/2,2)
      ELSE
      CALL RFT(A(1),A(2),NX/2,-2)
      CALL CFFT(A,NX/2,-1)
      END IF
      RETURN
      END
      SUBROUTINE RFFTSC(A,NX,IPACK,IFAC)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(*)
      IF (IFAC.EQ.1) THEN
      FAC=.5/NX
      ELSE IF (IFAC.EQ.-1) THEN
      FAC=0.25/NX
      ELSE
      FAC=1E0
      END IF
      IF (IPACK.EQ.2) THEN
      A(2)=0E0
      ELSE IF (IPACK.EQ.3) THEN
      A(NX+1)=A(2)
      A(NX+2)=0E0
      A(2)=0E0
      ELSE IF (IPACK.EQ.-3) THEN
      A(2)=A(NX+1)
      ELSE IF (IPACK.EQ.-2) THEN
      A(2)=0E0
      ELSE
      END IF
      IF (IFAC.NE.0) THEN
      DO 10 I=1,NX
 10   A(I)=A(I)*FAC
      END IF
      RETURN
      END
C
C
      SUBROUTINE VSMUL(A,I,C,D,J,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),D(1)
      I1=1
      J1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      D(J1)=C*A(I1)
      I1=I1+I
      J1=J1+J
 10   CONTINUE
      RETURN
      END
C
C
      SUBROUTINE VMAX(A,I,C,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),D(1)
      I1=1
      C=-1E30
      DO 10 L=1,N
      C=MAX(C,A(I1))
      I1=I1+I
 10   CONTINUE
      RETURN
      END
C
C
      SUBROUTINE VMIN(A,I,C,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),D(1)
      I1=1
      C=1E30
      DO 10 L=1,N
      C=MIN(C,A(I1))
      I1=I1+I
 10   CONTINUE
      RETURN
      END
C
C
      SUBROUTINE VNEG(A,I,C,J,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),C(1)
      I1=1
      J1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      C(J1)=-A(I1)
      I1=I1+I
 10   J1=J1+J
      RETURN
      END
C
C
      SUBROUTINE VMOV(A,I,C,J,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),C(1)
      I1=1
      J1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      C(J1)=A(I1)
      I1=I1+I
 10   J1=J1+J
      RETURN
      END
C
C
      SUBROUTINE VSUM(A,I,C,J,N,H)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),C(1)
      I1=1
      J1=1
      SUM=0E0
      HH=H
      DO 10 L=1,N
      SUM=SUM+A(I1)*HH
      C(J1)=SUM
      I1=I1+I
 10   J1=J1+J
      RETURN
      END
C
C
      SUBROUTINE VTRAPZ(A,I,C,J,N,H)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),C(1)
      I1=1
      J1=1
      SUM=0E0
      HH=.5*H
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N-1
      SUM=SUM+HH*(A(I1)+A(I1+I))
      C(J1)=SUM
      I1=I1+I
 10   J1=J1+J
      c(1+(n-1)*j)=sum
      RETURN
      END
C
C
      SUBROUTINE VSQ(A,I,C,J,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),C(1)
      I1=1
      J1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      C(J1)=A(I1)*A(I1)
      I1=I1+I
 10   J1=J1+J
      RETURN
      END
C
C
      SUBROUTINE VSQRT(A,I,C,J,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),C(1)
      I1=1
      J1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      C(J1)=SQRT(A(I1))
      I1=I1+I
      J1=J1+J
  10  CONTINUE
      RETURN
      END
C
C
      SUBROUTINE VEXP(A,I,C,J,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),C(1)
      I1=1
      J1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      C(J1)=EXP(A(I1))
      I1=I1+I
      J1=J1+J
  10  CONTINUE
      RETURN
      END
C
C
      SUBROUTINE CVEXP(A,I,C,J,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),C(1)
      REAL*8 TWOPI,ONOTPI
      DATA TWOPI,ONOTPI /6.28318530717959D0,0.159154943091895D0/
      I1=1
      J1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
C      RR=A(I1)
      RR=A(I1)-INT(A(I1)*ONOTPI)*TWOPI
      C(J1)=COS(RR)
      C(J1+1)=SIN(RR)
      I1=I1+I
      J1=J1+J
  10  CONTINUE
      RETURN
      END
C
C
      SUBROUTINE CVSQRT(A,I,C,J,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      COMPLEX CC
      REAL A(1),C(1)
      I1=1
      J1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      CC=CMPLX(A(I1),A(I1+1))
      CC=SQRT(CC)
      C(J1)=REAL(CC)
      C(J1+1)=RIMAG(CC)
      I1=I1+I
      J1=J1+J
 10   CONTINUE
      RETURN
      END
C
C
      SUBROUTINE CVMOV(A,I,C,J,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),C(1)
      I1=1
      J1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      C(J1)=A(I1)
      C(J1+1)=A(I1+1)
      I1=I1+I
      J1=J1+J
  10  CONTINUE
      RETURN
      END
C
C
      SUBROUTINE CVNEG(A,I,C,J,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),C(1)
      I1=1
      J1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      C(J1)=-A(I1)
      C(J1+1)=-A(I1+1)
      I1=I1+I
      J1=J1+J
  10  CONTINUE
      RETURN
      END
C
C
      SUBROUTINE CVCONJ(A,I,C,J,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),C(1)
      I1=1
      J1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      C(J1)=A(I1)
      C(J1+1)=-A(I1+1)
      I1=I1+I
      J1=J1+J
  10  CONTINUE
      RETURN
      END
C
C
      SUBROUTINE CVMAGS(A,I,C,J,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),C(1)
      I1=1
      J1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      C(J1)=A(I1)*A(I1)+A(I1+1)*A(I1+1)
      I1=I1+I
      J1=J1+J
  10  CONTINUE
      RETURN
      END
C
C
      SUBROUTINE CVMAX(A,I,C,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1)
      I1=1
      C=0
      DO 10 L=1,N
      C=MAX(A(I1)*A(I1)+A(I1+1)*A(I1+1),C)
      I1=I1+I
  10  CONTINUE
      RETURN
      END
C
C
      SUBROUTINE VCLR(C,J,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION C(1)
      J1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      C(J1)=0.
      J1=J1+J
  10  CONTINUE
      RETURN
      END
C
C
      SUBROUTINE VFILL(A,C,J,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION C(1)
      J1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      C(J1)=A
      J1=J1+J
  10  CONTINUE
      RETURN
      END
C
C
      SUBROUTINE VRAMP(A,B,C,J,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION C(1)
      J1=1
      AA=A
      BB=B
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      C(J1)=AA+BB*(L-1)
      J1=J1+J
  10  CONTINUE
      RETURN
      END
C
C
      SUBROUTINE CVFILL(A,C,J,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(*),C(*)
      J1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      C(J1)=A(1)
      C(J1+1)=A(2)
      J1=J1+J
  10  CONTINUE
      RETURN
      END
C
C
      SUBROUTINE CVSMUL(A,I,C,D,J,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),D(1)
      I1=1
      J1=1
      CC=C
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      D(J1)=CC*A(I1)
      D(J1+1)=CC*A(I1+1)
      I1=I1+I
      J1=J1+J
 10   CONTINUE
      RETURN
      END
C
C
      SUBROUTINE VMUL(A,I,C,J,D,K,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),C(1),D(1)
      I1=1
      J1=1
      K1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      D(K1)=C(J1)*A(I1)
      I1=I1+I
      J1=J1+J
      K1=K1+K
 10   CONTINUE
      RETURN
      END
C
      SUBROUTINE VADD(A,I,C,J,D,K,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),C(1),D(1)
      I1=1
      J1=1
      K1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      D(K1)=C(J1)+A(I1)
      I1=I1+I
      J1=J1+J
      K1=K1+K
 10   CONTINUE
      RETURN
      END
C
C
      SUBROUTINE CVMEXP(A,I,C,J,D,K,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),C(1),D(1)
      REAL*8 TWOPI,ONOTPI
      DATA TWOPI,ONOTPI /6.28318530717959D0,0.159154943091895D0/
      I1=1
      J1=1
      K1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      RA=A(I1)-INT(A(I1)*ONOTPI)*TWOPI
      RC=C(J1)
      D(K1)=RC*COS(RA)
      D(K1+1)=RC*SIN(RA)
      I1=I1+I
      J1=J1+J
      K1=K1+K
 10   CONTINUE
      RETURN
      END
C
C
      SUBROUTINE CVMUL(A,I,C,J,D,K,N,IFL)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),C(1),D(1)
      I1=1
      J1=1
      K1=1
      IF (IFL.GE.0) THEN
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      RR=A(I1)*C(J1)-A(I1+1)*C(J1+1)
      RI=A(I1+1)*C(J1)+A(I1)*C(J1+1)
      D(K1)=RR
      D(K1+1)=RI
      I1=I1+I
      J1=J1+J
      K1=K1+K
 10   CONTINUE
      ELSE
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 20 L=1,N
      RR=A(I1)*C(J1)+A(I1+1)*C(J1+1)
      RI=-A(I1+1)*C(J1)+A(I1)*C(J1+1)
      D(K1)=RR
      D(K1+1)=RI
      I1=I1+I
      J1=J1+J
      K1=K1+K
 20   CONTINUE
      END IF
      RETURN
      END
C
C
      SUBROUTINE VCLIP(A,I,B,C,D,K,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),D(1)
      I1=1
      K1=1
      DO 10 L=1,N
      IF (A(I1).LT.B) THEN
      D(K1)=B
      ELSE IF (A(I1).GT.C) THEN
           D(K1)=C
      ELSE
      D(K1)=A(I1)
      END IF
      I1=I1+I
      K1=K1+K
 10   CONTINUE
      RETURN
      END
C
C
      SUBROUTINE CVSUB(A,I,C,J,D,K,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),C(1),D(1)
      I1=1
      J1=1
      K1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      D(K1)=A(I1)-C(J1)
      D(K1+1)=A(I1+1)-C(J1+1)
      I1=I1+I
      J1=J1+J
      K1=K1+K
 10   CONTINUE
      RETURN
      END
C
C
      SUBROUTINE CVADD(A,I,C,J,D,K,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),C(1),D(1)
      I1=1
      J1=1
      K1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      D(K1)=A(I1)+C(J1)
      D(K1+1)=A(I1+1)+C(J1+1)
      I1=I1+I
      J1=J1+J
      K1=K1+K
 10   CONTINUE
      RETURN
      END
C
C
      SUBROUTINE CRVMUL(A,I,C,J,D,K,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),C(1),D(1)
      I1=1
      J1=1
      K1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      RR=C(J1)
      D(K1)=RR*A(I1)
      D(K1+1)=RR*A(I1+1)
      I1=I1+I
      J1=J1+J
      K1=K1+K
 10   CONTINUE
      RETURN
      END
C
C
      SUBROUTINE CVRCIP(A,I,C,J,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),C(1)
C     REAL*8 RR,RR1,RR2
      I1=1
      J1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      RR1=ABS(A(I1))+ABS(A(I1+1))
      RR1=1E0/RR1
      RR=A(I1)*RR1
      RI=A(I1+1)*RR1
      RS=1E0/(RR*RR+RI*RI)
      C(J1)=RR1*(RR*RS)
      C(J1+1)=RR1*(-RI*RS)
      I1=I1+I
      J1=J1+J
  10  CONTINUE
      RETURN
      END
C
C
      SUBROUTINE CVIMVI(A,INDA,I,INDC,J,C,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),C(1)
      INTEGER INDA(1),INDC(1)
      I1=1
      J1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      II=INDA(I1)
      JJ=INDC(J1)
      C(JJ)=A(II)
      C(JJ+1)=A(II+1)
      I1=I1+I
      J1=J1+J
  10  CONTINUE
      RETURN
      END
C
      SUBROUTINE VSMA(A,I,B,C,J,D,K,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),C(1),D(1)
      I1=1
      J1=1
      K1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      D(K1)=B*A(I1)+C(J1)
      I1=I1+I
      J1=J1+J
      K1=K1+K
 10   CONTINUE
      RETURN
      END
C
C
      SUBROUTINE VALG10(A,I,C,J,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),C(1)
      I1=1
      J1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      C(J1)=LOG10(A(I1))
      I1=I1+I
 10   J1=J1+J
      RETURN
      END
      SUBROUTINE PKVAL(A,B,N,C,NP,R,MODE)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),B(1),C(1)
      RVAL=R
      RPEAK=1E0/R
      R=0.
      ITOG=MODE
      IF (ITOG.GT.0) THEN
      B(1)=0.
      ELSE
      B(1)=1E30
      END IF
      IP=1
      DO 10 I=1,N
      IF (ITOG.GT.0) THEN
        IF (A(I).GT.B(IP)) THEN
          B(IP)=A(I)
          C(IP)=I
          CHECK=RPEAK*B(IP)
        ELSE IF (A(I).LE.CHECK) THEN
          R=IP
          IP=IP+1
          ITOG=-ITOG
          IF (IP.GT.NP) RETURN
          B(IP)=A(I)
          C(IP)=I
        ELSE
        END IF
      ELSE
        IF (A(I).LT.B(IP)) THEN
          B(IP)=A(I)
          C(IP)=I
          CHECK=RVAL*B(IP)
        ELSE IF (A(I).GE.CHECK) THEN
          R=IP
          IP=IP+1
          ITOG=-ITOG
          IF (IP.GT.NP) RETURN
          B(IP)=A(I)
          C(IP)=I
        ELSE
        END IF
      END IF
 10   CONTINUE
      RETURN
      END
      SUBROUTINE CVIMOV(A,INDA,I,C,J,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),C(1)
      INTEGER INDA(1)
cvd$r relation(I.gt.0)
cvd$r relation(J.gt.1)
      I1=1
      J1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      II=INDA(I1)
      C(J1)=A(II)
      C(J1+1)=A(II+1)
      I1=I1+I
      J1=J1+J
  10  CONTINUE
      RETURN
      END
C
C
      SUBROUTINE CVMOVI(A,I,INDC,J,C,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      DIMENSION A(1),C(1)
      INTEGER INDC(1)
cvd$r relation(I.gt.1)
cvd$r relation(J.gt.0)
cvd$r permutation(INDC)
      I1=1
      J1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      JJ=INDC(J1)
      C(JJ)=A(I1)
      C(JJ+1)=A(I1+1)
      I1=I1+I
      J1=J1+J
  10  CONTINUE
      RETURN
      END
C
C
      SUBROUTINE CBGEBS(A,AR,NR,N,IBW,EPS)     
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      COMPLEX*16 V,XC,CC1,CC2,CC3
      COMPLEX A(1),AR(NR),XS      
      REAL X,ANORM
C     SPECIAL SOLVER MEANT FOR FIP SYSTEM       
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
       V=A(JI)
       X=ABS(REAL(A(JI)))+ABS(RIMAG(A(JI)))
       JI=JI-NRM1
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
C$DIR No_Recurrence
CDIR$ IVDEP
cvd$  shortloop
CDIR$ SHORTLOOP
       DO 50 J=I,L2      
        XS=A(IJ) 
        A(IJ)=A(KJ)     
        A(KJ)=XS 
        IJ=IJ+NR
        KJ=KJ+NR
 50    CONTINUE  
       XS=AR(I) 
       AR(I)=AR(K)     
       AR(K)=XS 
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
      V=1D0/A(II)
      A(II)=V
      L2=MIN(I+IBM,N)
cvd$  shortloop
CDIR$ SHORTLOOP
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
cvd$  select(concurrent)
      DO 85 K=I+1,L1    
       KI=IB(K,I)
       CC1=-A(KI)
       XS=CC1
       RR=REAL(XS)
       RI=RIMAG(XS)
       IF ((RR.NE.0.0) .OR. (RI.NE.0.0)) THEN
        XC=CC1*V
        A(KI)=CMPLX(0.,0.)    
        IJ=II
        KJ=KI
cvd$  shortloop
CDIR$ SHORTLOOP
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
cvd$  select(vector)
        DO 80 J=I+1,L2    
         IJ=IJ+NR
         KJ=KJ+NR
         A(KJ)=A(KJ)+XC*A(IJ)
 80     CONTINUE
        AR(K)=AR(K)+XC*AR(I)
       END IF
 85   CONTINUE  
 91   CONTINUE  
C     -------------------------------   
C     BACK SUBSTITUTION 
C     -------------------------------   
      CC2=AR(n)
      cc3=a(ib(n,n))
      ar(n)=cc2*cc3
c      AR(N)=AR(N)*A(IB(N,N))      
      DO 100 I=N-1,1,-1 
       L2=MIN(N,I+IBM)
       II=IB(I,I)
       IJ=II
       V=(0.,0.)
cvd$  shortloop
CDIR$ SHORTLOOP
cvd$  select(vector,concurrent)
       DO 95 J=I+1,L2    
        IJ=IJ+NR
        CC2=AR(j)
        cc3=a(ij)
        v=v+cc2*cc3
c        V=V+A(IJ)*AR(J)
 95    CONTINUE
       cc2=ar(i)
       cc3=a(ii)
       ar(i)=(cc2-v)*cc3
c       AR(I)=(AR(I)-V)*A(II)  
 100  CONTINUE
      RETURN    
      END       

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

      complex function cexpt(c)
      complex c
      real*4 es
      REAL*8 TWOPI,ONOTPI,RD,ed
      DATA TWOPI,ONOTPI /6.28318530717959D0,0.159154943091895D0/
      rr=real(c)
      ra=abs(rr)
c      ri=rimag(c)
      
      if (ra.lt.85E0) then
       ri=rimag(c)-int(rimag(c)*onotpi)*twopi
       cexpt=exp(cmplx(rr,ri))
c      else if (ra.lt.90.0) then
c       rd=rr
c       ri=rimag(c)-int(rimag(c)*onotpi)*twopi
c       ed=exp(rd)
c       es=ed
c       cexpt=es*(cmplx(cos(ri),sin(ri)))
      else
       cexpt=cmplx(0e0,0e0)
      end if
      return
      end

      real function atan2z(ri,rr)
      if (rr.eq.0E0.and.ri.eq.0E0) then
       atan2z=0e0
      else
       atan2z=atan2(ri,rr) 
      end if
      return
      end

      Integer Function Indexs(Str,Substr)
      Character*(*) Str,Substr
      L1=Len(Str)
      L2=Len(Substr)
      Do 10 I=L1-L2+1,1,-1
       If (Substr.Eq.Str(I:I+L2-1)) Then
        Indexs=I
        Return
       end if
 10   continue
      indexs=0
      return
      end

      complex function crexpi(r)
      parameter (nintp=500,nintp1=nintp+1)
      parameter (da=6.28318507/(nintp-1),onoda=1e0/da)
      complex c
      REAL*8 TWOPI,ONOTPI,RD,ed
      real ctab(nintp1),stab(nintp1)
      integer tabflag
      common /cstab/ ctab,stab,tabflag
      DATA TWOPI,ONOTPI /6.28318530717959D0,0.159154943091895D0/
      if (tabflag.ne.99) then
       do i=1,nintp1
        ctab(i)=cos((i-1)*da)
        stab(i)=sin((i-1)*da)
       end do
       tabflag=99
      end if
      ri=r-int(r*onotpi)*twopi
      if (ri.lt.0e0) ri=ri+twopi
      indx=ri*onoda+1
      dd=ri*onoda-(indx-1)
      cc=(1-dd)*ctab(indx) + dd*ctab(indx+1)
      cs=(1-dd)*stab(indx) + dd*stab(indx+1)
      crexpi=cmplx(cc,cs)
      return
      end

      SUBROUTINE CVEXPI(A,I,C,J,N)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      parameter (nintp=500,nintp1=nintp+1)
      parameter (da=6.28318507/(nintp-1),onoda=1e0/da)
      DIMENSION A(1),C(1)
      real ctab(nintp1),stab(nintp1)
      integer tabflag
      common /cstab/ ctab,stab,tabflag
      REAL*8 TWOPI,ONOTPI
      DATA TWOPI,ONOTPI /6.28318530717959D0,0.159154943091895D0/
      if (tabflag.ne.99) then
       do L=1,nintp1
        ctab(L)=cos((L-1)*da)
        stab(L)=sin((L-1)*da)
       end do
       tabflag=99
      end if
      I1=1
      J1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 L=1,N
      ri=A(I1)-INT(A(I1)*ONOTPI)*TWOPI
      if (ri.lt.0e0) ri=ri+twopi
      indx=ri*onoda+1
c      write(6,*) 'a,ri,indx=',a(i1),ri,indx
      dd=ri*onoda-(indx-1)
      c(j1)=(1-dd)*ctab(indx) + dd*ctab(indx+1)
      c(j1+1)=(1-dd)*stab(indx) + dd*stab(indx+1)
      I1=I1+I
      J1=J1+J
  10  CONTINUE
      RETURN
      END
      complex function sqrtt(z)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      complex z
      if (real(z).lt.0e0) then
       sqrtt = cmplx(0e0,1e0) * sqrt(-z)
      else
       sqrtt = sqrt(z)
      end if
      return
      end
      real function rimag(z)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                     Copyright                        *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
      complex z,zz
      real rz(2)
      equivalence (zz,rz(1))
      zz=z
      rimag=rz(2) 
      return
      end
      SUBROUTINE CV2MP(A,I,C,J,N)
      DIMENSION A(1),C(1)
      I1=1
      J1=1
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      do L=1,N
       amp=sqrt(A(I1)*A(I1)+A(I1+1)*A(I1+1))
       pha=atan2z(a(i1+1),a(i1))
       C(J1)=amp
       C(J1+1)=pha
       I1=I1+I
       J1=J1+J
      end do
      RETURN
      END
      
      complex function cmpint(z1,w1,z2,w2)
      parameter (pi= 3.1415926, pi2=6.2831853 )

c >>> interpolates with weights w1 and w2 between complex 
c     numbers z1 and z2 which are stored in Modulo/phase format.
c
      complex z1,z2
      real w1,w2
      r1=real(z1)
      p1=imag(z1)
      r2=real(z2)
      p2=imag(z2)
c >>> unwrap phase
      if ((p2-p1).gt.pi) p2=p2-pi2
      if ((p1-p2).gt.pi) p1=p1-pi2
      r=r1*w1+r2*w2
      p=p1*w1+p2*w2
      cmpint=cmplx(r*cos(p),r*sin(p))
      return
      end

