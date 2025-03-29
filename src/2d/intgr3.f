      SUBROUTINE INTGR3(CFILEX,CFFX,KR,EXPKR)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'combes.f'
      LOGICAL UPMV
      COMPLEX CFILEX(IR,MSUFT,ISROW,NOUT,2)
      COMPLEX CFFX(NOUT,IR,NPLOTS,MSUFT,ISROW)
      REAL KR(NPLOTS,MBMAXI,2)
      COMPLEX EXPKR(NPLOTS,MBMAXI,2)
      COMPLEX C1,C2,C3,C4      
c >>> dummy variables to force single precision trf file
      complex*8 cdummy(npar)
      real*4 adummy(2,npar)
c
C *** CHECK THAT ARRAY SIZES ARE SUFFICIENT
C
      IF (2*IR*MSUFT*ISROW*NOUT.GT.ISIZE) THEN
       STOP '>>> INTGR3: ISIZE TOO SMALL <<<'
      END IF
      IF (NOUT*IR*NPLOTS*MSUFT*ISROW.GT.NP3) THEN
       STOP '>>> INTGR3: NP TOO SMALL <<<'
      END IF
      IF (NPLOTS*MBMAXI.GT.NPHALF) THEN
       STOP '>>> INTGR3: NP TOO SMALL <<<'
      END IF
      NS=ICUT2-ICUT1+1
      IF (ICDR.EQ.1) THEN
       TERM=0.
      ELSE
       TERM=PI/4E0
      END IF
      CALL VCLR(CFILEX,1,4*IR*MSUFT*ISROW*NOUT)
      CALL VCLR(CFFX,1,2*NOUT*IR*NPLOTS*MSUFT*ISROW)
      UPMV=.FALSE.
      IF (NPAR.gt.3) THEN
        ICNT=0
        DO 2 I=1,NPAR
         IF (IOUT(I).GT.0) THEN
          ICNT=ICNT+1
          IF (I.EQ.3) THEN
           IUPV=ICNT
          ELSE IF (I.EQ.4) THEN
           IUMV=ICNT
           UPMV=.TRUE.
          ELSE
          END IF
         END IF
 2      CONTINUE
      END IF
C *** INTEGRATION FACTORS (FUNCTIONS OF RANGE ONLY)
      IF (INTTYP.EQ.2) THEN
       CALL VMOV(DLWVNO,0,FAC,1,NPLOTS)
      ELSE IF (ICDR.EQ.0) THEN
       DO 5 JR=1,NPLOTS
        RANGEM=R1+(JR-1)*DLRAN
        IF (RANGEM.GT.1E-3) THEN
         FAC(JR)= FNI5/SQRT(RANGEM)
        ELSE
         FAC(JR)=DLWVNO
        END IF
 5     CONTINUE
      ELSE
       CALL VMOV(FNI5,0,FAC,1,NPLOTS)
      END IF      
      IF (.NOT.NFLAG) THEN
       DO 6 JR=1,NPLOTS
        RANGEM=R1+(JR-1)*DLRAN
        FAC(JR)=FAC(JR)*EXP(RANGEM*OFFIMA)
c>>> other factor for negative k. oassp only (940613)
        FAC(JR+nplots)=FAC(JR)*EXP(-RANGEM*OFFIMA)
 6     CONTINUE
      ELSE
       DO 7 JR=1,NPLOTS
        RANGEM=R1+(JR-1)*DLRAN
        FAC(JR+NPLOTS)=EXP(RANGEM*OFFIMA)
        FAC(JR+2*NPLOTS)=1E0/FAC(JR+NPLOTS)
 7     CONTINUE
      END IF
c      write(6,*) 'INTGR3: offima=',offima
C *** REWIND KERNEL FILE
      CALL RWDBUF(LUTGRN)
      DO 100 JK=ICUT1,ICUT2
C *** POINTER TO SWITCHING BUFFERS
       IBINDX=MOD(JK-ICUT1,2)+1
       WN=WK0+(JK-1)*DLWVNO
       SQRWN=1E0
       IF (INTTYP.EQ.2) THEN
C *** FULL BESSEL FUNCTION INTEGRATION
        DO MB=1,MBMAXI
         DO JR=1,NPLOTS
          RANGEM=R1+(JR-1)*DLRAN
          RKL=WN*RANGEM
          EXPKR(JR,MB,IBINDX)=
     &         RINTPL(BF(1,MB),0E0,ONODRK,NRKMAX,RKL)
         end do
        end do
        if (debug) write(92,*) wn,real(expkr(nplots,1,ibindx))
       ELSE
        DO 20 MB=1,MBMAXI
         RST=(TERM+(MB-1)*PI/2E0-WN*R1)
         RSTP=-DLRAN*WN
         CALL VRAMP(RST,RSTP,KR(1,MB,IBINDX),1,NPLOTS)
 20     CONTINUE
        CALL CVEXP(KR(1,1,IBINDX),1,EXPKR(1,1,IBINDX),2,NPLOTS*MBMAXI)
        if (ICDR.EQ.0) THEN
         SQRWN=SQRT(abs(WN))
         IF (R1.LT.1E-3) THEN
          EXPKR(1,1,IBINDX)=sqrwn
          CALL CVFILL(CNUL,EXPKR(1,2,IBINDX),2*NPLOTS,MBMAXI-1)
         END IF
        ELSE IF (NFLAG) THEN
         MSIGN=-1
         DO 26 MB=1,MBMAXI
          MSIGN=-MSIGN
          DO 25 JRH=1,NPLOTS
           EXPKR(JRH,MB,IBINDX)=FAC(JRH+NPLOTS)*EXPKR(JRH,MB,IBINDX)
     &       +MSIGN*FAC(JRH+2*NPLOTS)*CONJG(EXPKR(JRH,MB,IBINDX))
 25       CONTINUE
 26      CONTINUE
        END IF
       END IF
C *** FILL INTO CURRENT BUFFER
       ICNT=0
       DO 40 I=1,NPAR
        IF (IOUT(I).GT.0) THEN
         ICNT=ICNT+1
         CALL RDBUF(LUTGRN,CFILEX(1,1,1,ICNT,IBINDX),2*IR*MSUFT*ISROW)
C *** vsmul COMMENTED OUT FOR TESTING WITH V12 calint ONLY
         if (inttyp.eq.2) then
          CALL VSMUL(CFILEX(1,1,1,ICNT,IBINDX),1,WN,
     &               CFILEX(1,1,1,ICNT,IBINDX),1,2*IR*MSUFT*ISROW)
         else if (icdr.eq.0) then
          CALL VSMUL(CFILEX(1,1,1,ICNT,IBINDX),1,SQRWN,
     &               CFILEX(1,1,1,ICNT,IBINDX),1,2*IR*MSUFT*ISROW)
         end if
        IF (JK.GT.ICUT1.OR.ICUT1.EQ.ICUT2) THEN
         DO 30 IS=1,ISROW
         DO 30 M=1,MSUFT
          MORD=M/2
          IF (I.LT.3.or.i.ge.5) THEN
           MINDX=MORD+1
           FC=0.5
          ELSE IF (I.EQ.3) THEN
           MINDX=MORD + 2
           FC=0.5
          ELSE IF (MORD.EQ.0) THEN
           MINDX=MORD+2
           FC=-0.5
          ELSE
           MINDX=MORD
           FC=0.5
          END IF
c >>> Particle velocities in micrometer/second
      if (i.ge.2.and.i.le.4) FC=1E6*FC
cvd$  select(concur)
         DO 30 JRH=1,NPLOTS
          RANGEM=R1+(JRH-1)*DLRAN
          IF (INTTYP.EQ.2.OR.INTTYP.EQ.0.OR.NFLAG.OR.
     &        RANGEM.LT.1E-3) THEN
cvd$  select(vector)
           DO 28 JRV=1,IR
c>>> use opposite contour offset for negative k. oassp only.
            if (wn.lt.0) then
             fccc=fac(jrh+nplots)
            else
             fccc=fac(jrh)
            end if
            CFFX(ICNT,JRV,JRH,M,IS)=CFFX(ICNT,JRV,JRH,M,IS)+ 
     &      FC*fccc*(CFILEX(JRV,M,IS,ICNT,1)*EXPKR(JRH,MINDX,1)+
     &               CFILEX(JRV,M,IS,ICNT,2)*EXPKR(JRH,MINDX,2))
 28        continue
          ELSE
C *** FILON INTEGRATION
           C1=-AI/(KR(JRH,MINDX,IBINDX)-KR(JRH,MINDX,3-IBINDX))
           C2=EXPKR(JRH,MINDX,IBINDX)-EXPKR(JRH,MINDX,3-IBINDX)
cvd$  select(vector)
           DO 29 JRV=1,IR
            C3=CFILEX(JRV,M,IS,ICNT,IBINDX)*EXPKR(JRH,MINDX,IBINDX)-
     &       CFILEX(JRV,M,IS,ICNT,3-IBINDX)*EXPKR(JRH,MINDX,3-IBINDX)
            C4=CFILEX(JRV,M,IS,ICNT,IBINDX)
     &        -CFILEX(JRV,M,IS,ICNT,3-IBINDX)
            CFFX(ICNT,JRV,JRH,M,IS)=CFFX(ICNT,JRV,JRH,M,IS)+ 
     &      2*FC*FAC(JRH)*C1*(C3-C1*C2*C4)
 29        continue
          END IF
 30      CONTINUE
        END IF
        end if
 40     CONTINUE
 100   CONTINUE  
c *** EXPANSION COEFFICIENTS FOR U AND V DECOUPLED
      IF (UPMV) THEN
       DO 200 IS=1,ISROW
       DO 200 M=1,MSUFT
cvd$  select(concur)
       DO 200 JRH=1,NPLOTS
cvd$  select(vector)
       DO 200 JRV=1,IR
        C1= 0.5*(CFFX(IUPV,JRV,JRH,M,IS)+CFFX(IUMV,JRV,JRH,M,IS))
        C2= 0.5*(CFFX(IUPV,JRV,JRH,M,IS)-CFFX(IUMV,JRV,JRH,M,IS))
        CFFX(IUPV,JRV,JRH,M,IS)=C1
        CFFX(IUMV,JRV,JRH,M,IS)=C2
 200   CONTINUE
      END IF
C *** WRITE TRANSFER FUNCTION FILE
       DO 300 IS=1,ISROW
       DO 300 M=1,MSUFT
       DO 300 JRH=1,NPLOTS
       DO 300 JRV=1,IR
        if (double_trf) then
         if (bintrf) then
          WRITE(LUTTRF) (CFFX(I,JRV,JRH,M,IS),I=1,NOUT)
         else
          WRITE(LUTTRF,*) (real(CFFX(I,JRV,JRH,M,IS)),
     &                    rimag(CFFX(I,JRV,JRH,M,IS)),I=1,NOUT)
         end if
        else
         do i=1,nout
          cdummy(i)=CFFX(I,JRV,JRH,M,IS)
         end do
         if (bintrf) then
          WRITE(LUTTRF) (cdummy(i),I=1,NOUT)
         else
          WRITE(LUTTRF,*) (real(cdummy(i)),
     &                   rimag(cdummy(i)),I=1,NOUT)
         end if
        end if
 300   CONTINUE
      RETURN
      END
