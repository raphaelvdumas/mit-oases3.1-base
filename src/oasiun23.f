      SUBROUTINE INTTLT(CFILEX,CFFX,KR,EXPKR)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      parameter (nwght=2048)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
      INCLUDE 'combes.f'
      LOGICAL UPMV
      COMPLEX CFILEX(IR,MSUFT,ISROW,NOUT,2)
      COMPLEX CFFX(NOUT,IR,NPLOTS,MSUFT,ISROW)
      REAL KR(NPLOTS,MBMAXI,2)
      COMPLEX EXPKR(NPLOTS,MBMAXI,2)
      COMPLEX C1,C2,C3,C4      
      real wght(nwght)
c >>> dummy variables to force single precision trf file
      complex*8 cdummy(npar)
      real*4 adummy(2,npar)
c
C *** CHECK THAT ARRAY SIZES ARE SUFFICIENT
C
      if (inttyp.eq.2) then
       IF (4*IR*MSUFT*ISROW*NOUT.GT.ISIZE) THEN
        STOP '>>> INTTLT: ISIZE TOO SMALL <<<'
       END IF
       IF (NPLOTS*MBMAXI.GT.NP/4) THEN
        STOP '>>> INTTLT: NP TOO SMALL <<<'
       END IF
      else
       IF (2*IR*MSUFT*ISROW*NOUT.GT.ISIZE) THEN
        STOP '>>> INTTLT: ISIZE TOO SMALL <<<'
       END IF
       IF (NPLOTS*MBMAXI.GT.NPHALF) THEN
        STOP '>>> INTTLT: NP TOO SMALL <<<'
       END IF
      end if
      IF (NOUT*IR*NPLOTS*MSUFT*ISROW.GT.NP3) THEN
       STOP '>>> INTTLT: NP TOO SMALL <<<'
      END IF

c 
c >>> For full Bessel integration, prepare weight array.
c >>> First 50% full Bessel. Last 50% Hanning weighted with
c >>> asymptotic Hankel function
c
      if (inttyp.eq.2) then
       dwght=(rkmax-brk_0)/(nwght-1)
       onodwg=1e0/dwght
       nwghth=nwght/2
       do i=1,nwghth
        wght(i)=1.0
        wght(i+nwghth)=0.5*( 1e0 + cos( ((i-1)*pi)/(nwghth-1) ) )
       end do
      end if

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


c      write(6,*) 'INTGR3: offima=',offima
C *** REWIND KERNEL FILE
      CALL RWDBUF(LUTGRN)
      DO 100 JK=ICUT1,ICUT2
C *** POINTER TO SWITCHING BUFFERS
       WN=WK0+(JK-1)*DLWVNO
       if (ICDR.EQ.0) THEN
        SQRWN=SQRT(abs(WN))
       else
        SQRWN=1E0
       end if
C *** FILL INTO CURRENT BUFFER
       ICNT=0
       DO 40 I=1,NPAR
        IF (IOUT(I).GT.0) THEN
         ICNT=ICNT+1
         CALL RDBUF(LUTGRN,CFILEX(1,1,1,ICNT,1),2*IR*MSUFT*ISROW)
C *** vsmul COMMENTED OUT FOR TESTING WITH V12 calint ONLY
         if (icdr.eq.0) then
          if (inttyp.eq.2) then
           CALL VSMUL(CFILEX(1,1,1,ICNT,1),1,WN,
     &               CFILEX(1,1,1,ICNT,2),1,2*IR*MSUFT*ISROW)
          end if
          CALL VSMUL(CFILEX(1,1,1,ICNT,1),1,SQRWN,
     &               CFILEX(1,1,1,ICNT,1),1,2*IR*MSUFT*ISROW)
         end if
cvd$  select(concur)
         do 30 jrv=1,IR
c >>> receiver offset due to array tilt
          rtlt=r1+ofstar(jrv)
c          if (jk.eq.icut2) then
c           write(6,*) 'jrv,rtlt=',jrv,rtlt
c          end if
c            rtlt=r1
C *** INTEGRATION FACTORS (FUNCTIONS OF RANGE ONLY)
          IF (ICDR.EQ.0) THEN
c >>> Cylindrical geometry
           if (INTTYP.EQ.2) then
            CALL VMOV(DLWVNO,0,FAC(1+3*nplots),1,NPLOTS)
           end if
           do JR=1,NPLOTS
            RANGEM=Rtlt+(JR-1)*DLRAN
            abs_r=abs(rangem)
            IF (abs_r.GT.1E-3) THEN
             FAC(JR)= FNI5/SQRT(abs_r)
            ELSE
             FAC(JR)=DLWVNO
            END IF
           end do
          ELSE
c >>> Plane geometry
           CALL VMOV(FNI5,0,FAC,1,NPLOTS)
          END IF     

          IF (.NOT.NFLAG) THEN
           DO 6 JR=1,NPLOTS
            RANGEM=Rtlt+(JR-1)*DLRAN
            abs_r=abs(rangem)
            eex=EXP(abs_r*OFFIMA)
c>>> other factor for negative k. oassp only (940613)
            FAC(JR+nplots)=FAC(JR)*EXP(-abs_r*OFFIMA)
            FAC(JR)=FAC(JR)*eex
            if (inttyp.eq.2) then 
             FAC(JR+3*nplots)=FAC(JR+3*nplots)*eex
            end if
 6         CONTINUE
          ELSE
           DO 7 JR=1,NPLOTS
            RANGEM=Rtlt+(JR-1)*DLRAN
            FAC(JR+NPLOTS)=EXP(RANGEM*OFFIMA)
            FAC(JR+2*NPLOTS)=1E0/FAC(JR+NPLOTS)
 7         CONTINUE
          END IF

          if (INTTYP.EQ.2) then
C *** FULL BESSEL FUNCTION INTEGRATION
           DO MB=1,MBMAXI
            DO JR=1,NPLOTS
             RANGEM=Rtlt+(JR-1)*DLRAN
             abs_r=abs(rangem)
             RKL=WN*abs_r
             if (rkl.lt.brk_0) then
              EXPKR(JR,MB,2)=0e0
             else if (rkl.lt.rkmax) then
              EXPKR(JR,MB,2)=
     &        RINTPL(BF(1,MB),brk_0,ONODRK,NRKMAX,RKL)
             end if
            end do
           end do
           if (debug) write(92,*) wn,real(expkr(nplots,1,2))
          end if
          do MB=1,MBMAXI
           do jr=1,nplots
             RANGEM=Rtlt+(JR-1)*DLRAN
             abs_r=abs(rangem)
             RKL=WN*abs_r
             kr(jr,MB,1)=TERM+(MB+mbf_0-1)*PI/2E0-rkl
           end do
          end do
          CALL CVEXP(KR(1,1,1),1,EXPKR(1,1,1),
     &                 2,NPLOTS*MBMAXI)
          if (ICDR.EQ.0) THEN
           SQRWN=SQRT(abs(WN))
           do jr=1,nplots
             RANGEM=Rtlt+(JR-1)*DLRAN
             abs_r=abs(rangem)
             IF (abs_r.LT.1E-3) THEN
              if (mbf_0.gt.0) then
               CALL CVFILL(CNUL,EXPKR(jr,1,1),2*NPLOTS,MBMAXI)
              else
               EXPKR(jr,1,1)=sqrwn
               CALL CVFILL(CNUL,EXPKR(jr,2,1),2*NPLOTS,MBMAXI-1)
              end if
             END IF
           end do 
          else if (NFLAG) then
           MSIGN=-1
           do MB=1,MBMAXI
            MSIGN=-MSIGN
            do JRH=1,NPLOTS
             EXPKR(JRH,MB,1)=FAC(JRH+NPLOTS)*EXPKR(JRH,MB,1)
     &        +MSIGN*FAC(JRH+2*NPLOTS)*CONJG(EXPKR(JRH,MB,1))
            end do
           end do
          END IF

           DO 29 IS=1,ISROW
           DO 29 M=1,MSUFT
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
            if (nwvno.eq.1
     &          .or.(jk.gt.icut1.and.jk.lt.icut2)) fc=fc*2.0
c >>> Particle velocities in micrometer/second
            if (i.ge.2.and.i.le.4) FC=1E6*FC
           DO 29 JRH=1,NPLOTS
            RANGEM=Rtlt+(JRH-1)*DLRAN
            abs_r=abs(rangem)
            RKL=WN*abs_r

            if (rangem.lt.0.0.and.(mod(mord,2).eq.1)) then
             fccr=-fc
            else
             fccr=fc
            end if

             if (INTTYP.EQ.2) then
              if (rkl.lt.brk_0) then
               CFFX(ICNT,JRV,JRH,M,IS)=cmplx(0e0,0e0)
               awg=1.0
              else if (rkl.lt.rkmax) then
cvd$  select(vector)
c>>> use opposite contour offset for negative k. oassp only.
               awg=RINTPL(wght,brk_0,onodwg,nwght,RKL)
               fccc=awg*fac(jrh+3*nplots)
               CFFX(ICNT,JRV,JRH,M,IS)=CFFX(ICNT,JRV,JRH,M,IS)+ 
     &         fccr*fccc*CFILEX(JRV,M,IS,ICNT,2)*EXPKR(JRH,MINDX,2)
              else
               awg=0e0
              end if
             else
              awg=0e0
             end if
cvd$  select(vector)

c>>> use opposite contour offset for negative k. oassp only.
             if (wn.lt.0) then
              fccc=fac(jrh+nplots)
             else
              fccc=(1e0-awg)*fac(jrh)
             end if
c For Filon multiply by sinc function in range
             if (inttyp.eq.1.and.rangem.ne.0) then
              fccc=fccc*(sin(0.5*dlwvno*rangem)/(0.5*dlwvno*rangem))**2
             end if
             CFFX(ICNT,JRV,JRH,M,IS)=CFFX(ICNT,JRV,JRH,M,IS)+ 
     &       fccr*fccc*CFILEX(JRV,M,IS,ICNT,1)*EXPKR(JRH,MINDX,1)
 29        continue

 30      CONTINUE
        end if
 40    CONTINUE
 100  CONTINUE  
c *** EXPANSION COEFFICIENTS FOR U AND V DECOUPLED
      IF (UPMV) THEN
       DO 200 IS=1,ISROW
       DO 200 M=1,MSUFT
cvd$  select(concur)
       DO 200 JRV=1,IR
        rtlt=r1+ofstar(jrv)
cvd$  select(vector)
        DO 200 JRH=1,NPLOTS
         rangem=rtlt+(jrh-1)*dlran
         C1= 0.5*(CFFX(IUPV,JRV,JRH,M,IS)+CFFX(IUMV,JRV,JRH,M,IS))
         C2= 0.5*(CFFX(IUPV,JRV,JRH,M,IS)-CFFX(IUMV,JRV,JRH,M,IS))
         if (rangem.lt.0.0) then
          CFFX(IUPV,JRV,JRH,M,IS)=-C1
          CFFX(IUMV,JRV,JRH,M,IS)=-C2
         else
          CFFX(IUPV,JRV,JRH,M,IS)=C1
          CFFX(IUMV,JRV,JRH,M,IS)=C2
         end if
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

      SUBROUTINE taupdgm(CFILEX,CFFX)
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
      COMPLEX CFILEX(IR,MSUFT,ISROW,NOUT)
      COMPLEX CFFX(NOUT,IR,NPLOTS,MSUFT,ISROW)
      COMPLEX C1,C2,C3,C4      
      complex expkr(nbfmax)
c >>> dummy variables to force single precision trf file
      complex*8 cdummy(npar)
      real*4 adummy(2,npar)
c
C *** CHECK THAT ARRAY SIZES ARE SUFFICIENT
C
      IF (IR*MSUFT*ISROW*NOUT.GT.ISIZE) THEN
       STOP '>>> taupdgm: ISIZE TOO SMALL <<<'
      END IF
      IF (NOUT*IR*NPLOTS*MSUFT*ISROW.GT.NP3) THEN
       STOP '>>> taupdgm: NP TOO SMALL <<<'
      END IF
      IF (NPLOTS*MBMAXI.GT.NPHALF) THEN
       STOP '>>> taupdgm: NP TOO SMALL <<<'
      END IF
      IF (ICDR.EQ.1) THEN
       TERM=0.
      ELSE
       TERM=PI/4E0
      END IF
      do 20 mb=1,mbmaxi
       expkr(mb)=exp(ai*(term+(mb-1)*pi*0.5e0))
 20   continue
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

C *** REWIND KERNEL FILE

      CALL RWDBUF(LUTGRN)

      DO 100 JRH=1,nplots

C *** read kernels
       ICNT=0
       DO 40 I=1,NPAR
        IF (IOUT(I).GT.0) THEN
         ICNT=ICNT+1
         CALL RDBUF(LUTGRN,CFILEX(1,1,1,ICNT),2*IR*MSUFT*ISROW)
         CALL VSMUL(CFILEX(1,1,1,ICNT),1,fni5,
     &              CFILEX(1,1,1,ICNT),1,2*IR*MSUFT*ISROW)

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
          if (nwvno.eq.1) fc=fc*2.0
c >>> Particle velocities in micrometer/second
          if (i.ge.2.and.i.le.4) FC=1E6*FC
cvd$  select(concur)
           DO 28 JRV=1,IR
            CFFX(ICNT,JRV,JRH,M,IS)=FC*CFILEX(JRV,M,IS,ICNT)*
     &                              expkr(mindx)
 28        continue
 30      continue
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
        do i=1,nout
          adummy(1,i)=1.0E6*real(CFFX(I,JRV,JRH,M,IS))
          adummy(2,i)=1.0E6*rimag(CFFX(I,JRV,JRH,M,IS))
        end do
        if (bintrf) then
         WRITE(LUTTRF) (adummy(1,i),adummy(2,i),I=1,NOUT)
        else
         WRITE(LUTTRF,*) (adummy(1,i),
     &                    adummy(2,i),I=1,NOUT)
        end if
 300   CONTINUE
      RETURN
      END

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
      parameter (nwght=2048)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'combes.f'
      LOGICAL UPMV
      COMPLEX CFILEX(IR,MSUFT,ISROW,NOUT,2)
      COMPLEX CFFX(NOUT,IR,NPLOTS,MSUFT,ISROW)
      REAL KR(NPLOTS,MBMAXI,2)
      COMPLEX EXPKR(NPLOTS,MBMAXI,2)
      COMPLEX C1,C2,C3,C4      
      real wght(nwght)
c >>> dummy variables to force single precision trf file
      complex*8 cdummy(npar)
      real*4 adummy(2,npar)
c
C *** CHECK THAT ARRAY SIZES ARE SUFFICIENT
C
      if (inttyp.eq.2) then
       IF (2*IR*MSUFT*ISROW*NOUT.GT.ISIZE) THEN
        STOP '>>> INTGR3: ISIZE TOO SMALL <<<'
       END IF
       IF (2*NPLOTS*MBMAXI.GT.NP) THEN
        STOP '>>> INTGR3: NP TOO SMALL <<<'
       END IF
      else
       IF (2*IR*MSUFT*ISROW*NOUT.GT.ISIZE) THEN
        STOP '>>> INTGR3: ISIZE TOO SMALL <<<'
       END IF
       IF (NPLOTS*MBMAXI.GT.NPHALF) THEN
        STOP '>>> INTGR3: NP TOO SMALL <<<'
       END IF
      end if
      IF (NOUT*IR*NPLOTS*MSUFT*ISROW.GT.NP3) THEN
       STOP '>>> INTGR3: NP TOO SMALL <<<'
      END IF

c 
c >>> For full Bessel integration, prepare weight array.
c >>> First 50% full Bessel. Last 50% Hanning weighted with
c >>> asymptotic Hankel function
c
      if (inttyp.eq.2) then
       dwght=(rkmax-brk_0)/(nwght-1)
       onodwg=1e0/dwght
       nwghth=nwght/2
       do i=1,nwghth
        wght(i)=1.0
        wght(i+nwghth)=0.5*( 1e0 + cos( ((i-1)*pi)/(nwghth-1) ) )
       end do
      end if

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
      IF (ICDR.EQ.0) THEN
c >>> Cylindrical geometry
       if (INTTYP.EQ.2) then
        CALL VMOV(DLWVNO,0,FAC(1+3*nplots),1,NPLOTS)
       end if
       do JR=1,NPLOTS
        RANGEM=R1+(JR-1)*DLRAN
        abs_r=abs(rangem)
        IF (abs_r.GT.1E-3) THEN
         FAC(JR)= FNI5/SQRT(abs_r)
        ELSE
         FAC(JR)=DLWVNO
        END IF
       end do
      ELSE
c >>> Plane geometry
       CALL VMOV(FNI5,0,FAC,1,NPLOTS)
      END IF     
 
      IF (.NOT.NFLAG) THEN
       DO 6 JR=1,NPLOTS
        RANGEM=R1+(JR-1)*DLRAN
        abs_r=abs(rangem)
        eex=EXP(abs_r*OFFIMA)
        if (inttyp.eq.2) then 
         FAC(JR+3*nplots)=FAC(JR+3*nplots)*eex
        end if
        FAC(JR)=FAC(JR)*eex
c>>> other factor for negative k. oassp only (940613)
        FAC(JR+nplots)=FAC(JR)*EXP(-abs_r*OFFIMA)
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
       WN=WK0+(JK-1)*DLWVNO
       SQRWN=1E0
       if (INTTYP.EQ.2) then
C *** FULL BESSEL FUNCTION INTEGRATION
        DO MB=1,MBMAXI
         DO JR=1,NPLOTS
          RANGEM=R1+(JR-1)*DLRAN
          abs_r=abs(rangem)
          RKL=WN*abs_r
          if (rkl.lt.brk_0) then
           EXPKR(JR,MB,2)=0e0
          else if (rkl.lt.rkmax) then
           EXPKR(JR,MB,2)=
     &         RINTPL(BF(1,MB),brk_0,ONODRK,NRKMAX,RKL)
          end if
         end do
        end do
        if (debug) write(92,*) wn,real(expkr(nplots,1,2))
       end if
       do MB=1,MBMAXI
           do jr=1,nplots
             RANGEM=R1+(JR-1)*DLRAN
             abs_r=abs(rangem)
             RKL=WN*abs_r
             kr(jr,MB,1)=TERM+(MB+mbf_0-1)*PI/2E0-rkl
           end do
       end do
       CALL CVEXP(KR(1,1,1),1,EXPKR(1,1,1),2,NPLOTS*MBMAXI)
       if (ICDR.EQ.0) THEN
        SQRWN=SQRT(abs(WN))
        do jr=1,nplots
         RANGEM=R1+(JR-1)*DLRAN
         abs_r=abs(rangem)
         IF (abs_r.LT.1E-3) THEN
          if (mbf_0.gt.0) then
           CALL CVFILL(CNUL,EXPKR(jr,1,1),2*NPLOTS,MBMAXI)
          else
           EXPKR(jr,1,1)=sqrwn
           CALL CVFILL(CNUL,EXPKR(jr,2,1),2*NPLOTS,MBMAXI-1)
          end if
         END IF
        end do
       else if (NFLAG) then
        MSIGN=-1
        do MB=1,MBMAXI
         MSIGN=-MSIGN
         do JRH=1,NPLOTS
          EXPKR(JRH,MB,1)=FAC(JRH+NPLOTS)*EXPKR(JRH,MB,1)
     &       +MSIGN*FAC(JRH+2*NPLOTS)*CONJG(EXPKR(JRH,MB,1))
         end do
        end do
       END IF

C *** FILL INTO CURRENT BUFFER
       ICNT=0
       DO 40 I=1,NPAR
        IF (IOUT(I).GT.0) THEN
         ICNT=ICNT+1
         CALL RDBUF(LUTGRN,CFILEX(1,1,1,ICNT,1),2*IR*MSUFT*ISROW)
C *** vsmul COMMENTED OUT FOR TESTING WITH V12 calint ONLY
         if (icdr.eq.0) then
          if (inttyp.eq.2) then
           CALL VSMUL(CFILEX(1,1,1,ICNT,1),1,WN,
     &               CFILEX(1,1,1,ICNT,2),1,2*IR*MSUFT*ISROW)
          end if
          CALL VSMUL(CFILEX(1,1,1,ICNT,1),1,SQRWN,
     &               CFILEX(1,1,1,ICNT,1),1,2*IR*MSUFT*ISROW)
         end if

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
          if (nwvno.eq.1.or.(jk.gt.icut1.and.jk.lt.icut2)) fc=fc*2.0
c >>> Particle velocities in micrometer/second
      if (i.ge.2.and.i.le.4) FC=1E6*FC
cvd$  select(concur)
         DO 30 JRH=1,NPLOTS
          RANGEM=R1+(JRH-1)*DLRAN
          abs_r=abs(rangem)
          RKL=WN*abs_r
          if (rangem.lt.0.0.and.(mod(mord,2).eq.1)) then
           fccr=-fc
          else
           fccr=fc
          end if

           if (INTTYP.EQ.2) then
            if (rkl.lt.brk_0) then
             do jrv=1,ir
              CFFX(ICNT,JRV,JRH,M,IS)=cmplx(0e0,0e0)
             end do
             awg=1.0
            else if (rkl.lt.rkmax) then
cvd$  select(vector)
             do JRV=1,IR
c>>> use opposite contour offset for negative k. oassp only.
              awg=RINTPL(wght,brk_0,onodwg,nwght,RKL)
              fccc=awg*fac(jrh+3*nplots)

              CFFX(ICNT,JRV,JRH,M,IS)=CFFX(ICNT,JRV,JRH,M,IS)+ 
     &        fccr*fccc*CFILEX(JRV,M,IS,ICNT,2)*EXPKR(JRH,MINDX,2)
             end do
            else
             awg=0e0
            end if
           else
            awg=0e0
           end if
cvd$  select(vector)
           do JRV=1,IR
c>>> use opposite contour offset for negative k. oassp only.
            if (wn.lt.0) then
             fccc=fac(jrh+nplots)
            else
             fccc=(1e0-awg)*fac(jrh)
            end if
c For Filon multiply by sinc function in range
            if (inttyp.eq.1.and.rangem.ne.0) then
             fccc=fccc*(sin(0.5*dlwvno*rangem)/(0.5*dlwvno*rangem))**2
            end if
            CFFX(ICNT,JRV,JRH,M,IS)=CFFX(ICNT,JRV,JRH,M,IS)+ 
     &      fccr*fccc*CFILEX(JRV,M,IS,ICNT,1)*EXPKR(JRH,MINDX,1)
           end do

 30      CONTINUE

        end if
 40     CONTINUE
 100   CONTINUE  
c *** EXPANSION COEFFICIENTS FOR U AND V DECOUPLED
      IF (UPMV) THEN
       DO 200 IS=1,ISROW
       DO 200 M=1,MSUFT
cvd$  select(concur)
       DO 200 JRH=1,NPLOTS
        rangem=r1+(jrh-1)*dlran
cvd$  select(vector)
        DO 200 JRV=1,IR
         C1= 0.5*(CFFX(IUPV,JRV,JRH,M,IS)+CFFX(IUMV,JRV,JRH,M,IS))
         C2= 0.5*(CFFX(IUPV,JRV,JRH,M,IS)-CFFX(IUMV,JRV,JRH,M,IS))
         if (rangem.lt.0.0) then
          CFFX(IUPV,JRV,JRH,M,IS)=-C1
          CFFX(IUMV,JRV,JRH,M,IS)=-C2
         else
          CFFX(IUPV,JRV,JRH,M,IS)=C1
          CFFX(IUMV,JRV,JRH,M,IS)=C2
         end if
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

      SUBROUTINE MORDER
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
      IF(SRCTYP.EQ.1.or.srctyp.eq.99) THEN
        MSUFT=1
      ELSEIF(SRCTYP.EQ.2) THEN
        MSUFT=3
      ELSEIF(SRCTYP.EQ.3) THEN
        MSUFT=5
      ELSEIF(SRCTYP.EQ.4) THEN
        MSUFT=5
      ELSEIF(SRCTYP.EQ.5 .OR. SRCTYP.EQ.6) THEN
        MSUFT=5
      ELSE
        WRITE(6,*) ' UNKNOWN SOURCE TYPE '
        STOP        
      ENDIF
      MSUFT=MIN(MSUFT,MMAX)
C
      MBMAX=MSUFT/2+1
      MBMAXI=MBMAX+1
      RETURN
      END
      SUBROUTINE TRFHEAD(trfext,TITLE,RD,RDLOW,R0,RSPACE,
     &                    NX,LX,MX,DT,FREQS,SD)
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
      INCLUDE 'comnrd.f'

      CHARACTER*(*) trfext
      CHARACTER*8 FILEID
      character*16 bufch
      CHARACTER*80 TITLE,filenm
      CHARACTER SIGNN
      INTEGER IPARM(12)
c >>> Dummy real*4 to force single prec trf file.
      real*4 dummy,adummy(10)
      if (double_trf) then
       bufch='d'//trfext
      else
       bufch=trfext
      end if
      inquire(1,name=filenm)
      ii=indexs(filenm,'.')
      if (bintrf) then
        OPEN(LUTTRF,FILE=Filenm(1:ii)//bufch,
     &          STATUS='UNKNOWN',FORM='UNFORMATTED')
cunix        OPEN(LUTTRF,FILE=FILEnm(1:ii)//trfext,
cunix     &       STATUS='UNKNOWN',FORM='UNFORMATTED')
        FILEID='PULSETRF'
        WRITE(LUTTRF) FILEID
        WRITE(LUTTRF) PROGNM
        WRITE(LUTTRF) NOUT
        ICNT=1
        DO 10 I=1,NPAR
         IF (IOUT(I).NE.0) THEN
           IPARM(ICNT)=I
           ICNT=ICNT+1
         END IF
 10     CONTINUE
        WRITE(LUTTRF) (IPARM(J),J=1,NOUT)
        WRITE(LUTTRF) TITLE
        SIGNN='+'
        WRITE(LUTTRF) SIGNN
        dummy=freqs
        WRITE(LUTTRF) dummy
        dummy=sd
        WRITE(LUTTRF) dummy
        adummy(1)=rd
        adummy(2)=rdlow
        WRITE(LUTTRF) adummy(1),adummy(2),IR
        IF (IR.LT.0) THEN
c         do L = 1,abs(ir)
c          dummy=rdc(L)
c          WRITE(LUTTRF) dummy
c         end do
         write(luttrf) (rdc(l),l=1,abs(ir))
        END IF
        adummy(1)=r0
        adummy(2)=rspace
        WRITE(LUTTRF) adummy(1),adummy(2),NPLOTS
        dummy=dt
        WRITE(LUTTRF) NX,LX,MX,dummy
        WRITE(LUTTRF) ICDR
        dummy=omegim
        WRITE(LUTTRF) dummy
C ***  EXTRA FIELDS ADDED 891211 HS
        WRITE(LUTTRF) MSUFT
        write(6,*) 'trfhead: msuft=',msuft
        WRITE(LUTTRF) ISROW
        write(LUTTRF) inttyp
c The following is a hack for having trf_reader_oases3d read multiple parameters
        idummy=nout
        DO 300 I=1,2
         WRITE(LUTTRF) IDUMMY
 300    CONTINUE
        dummy=0
        DO 400 I=1,5
         WRITE(LUTTRF) DUMMY
 400    CONTINUE
        else
        OPEN(LUTTRF,FILE=Filenm(1:ii)//bufch,
     &          STATUS='UNKNOWN',FORM='FORMATTED')
cunix        OPEN(LUTTRF,FILE=FILEnm(1:ii)//trfext,
cunix     &       STATUS='UNKNOWN',FORM='UNFORMATTED')
        FILEID='PULSETRF'
        WRITE(LUTTRF,'(1x,a)') FILEID
        WRITE(LUTTRF,'(1x,a)') PROGNM
        WRITE(LUTTRF,*) NOUT
        ICNT=1
        DO 11 I=1,NPAR
         IF (IOUT(I).NE.0) THEN
           IPARM(ICNT)=I
           ICNT=ICNT+1
         END IF
 11     CONTINUE
        WRITE(LUTTRF,*) (IPARM(J),J=1,NOUT)
        WRITE(LUTTRF,'(1x,a)') TITLE
        SIGNN='+'
        WRITE(LUTTRF,'(1x,a)') SIGNN
        WRITE(LUTTRF,*) FREQS
        WRITE(LUTTRF,*) SD
        WRITE(LUTTRF,*) RD,RDLOW,IR
        IF (IR.LT.0) THEN
         WRITE(LUTTRF,*) (RDC(L),L=1,ABS(IR))
        END IF
        WRITE(LUTTRF,*) R0,RSPACE,NPLOTS

        WRITE(LUTTRF,*) NX,LX,MX,DT
        WRITE(LUTTRF,*) ICDR
        WRITE(LUTTRF,*) OMEGIM
C ***  EXTRA FIELDS ADDED 891211 HS
        WRITE(LUTTRF,*) MSUFT
        write(6,*) 'trfhead: msuft=',msuft
        WRITE(LUTTRF,*) ISROW
        write(LUTTRF,*) inttyp
        idummy=0
        DO 301 I=1,2
         WRITE(LUTTRF,*) IDUMMY
 301    CONTINUE
        dummy=0e0
        DO 401 I=1,5
         WRITE(LUTTRF,*) DUMMY
 401    CONTINUE
        end if
        RETURN
        
        END
      SUBROUTINE PLINTM(DLWVNL,WK0L,SD,RD,TITLE,M,XLEN,YLEN)                 
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
      INCLUDE 'complo.f'
      DIMENSION YMAX(NPAR)               
      CHARACTER*80 TITLE
      CHARACTER*6 OPTION(2),OPT2(6)
      DATA OPT2 /'NINTGR','WINTGR','UINTGR','VINTGR','RINTGR',
     &           'KINTGR'/
C
      OPTION(1)=PROGNM
      DO 2400 I=1,NPAR
       IF (IOUT(I).GT.0) THEN
        CALL CVMAX(CFF(1,I),2,YMAX(I),NWVNO)   
       END IF
 2400 CONTINUE
C                
C XAXIS DEFINITION     
C
      IPLOT1=MAX(1,ICUT1-100)
      IPLOT2=MIN(NWVNO,ICUT2+100)
      WKMIN=WK0L+(IPLOT1-1)*DLWVNL
      NN=IPLOT2-IPLOT1+1
      XMAX=(WK0L+(NWVNO-1)*DLWVNL)
      XMIN=WK0L   
      CALL AUTOAX(XMIN,XMAX,XLEFT,XRIGHT,XINC,XDIV,NXDIF)
C
      DO 2701 I=1,NPAR                   
      IF (IOUT(I).EQ.0) GO TO 2701    
      IF (YMAX(I).LE.0) YMAX(I)=1E0
      OPTION(1)=PROGNM
      OPTION(2)=OPT2(I)
      IPLOT1=MAX(1,ICUT1-100)
      IPLOT2=MIN(NWVNO,ICUT2+100)
C                
C                
C  YAXIS DEFINITION                   
C                
      YMIN=0.0   
      YMAX(I)=SQRT(YMAX(I))           
      CALL AUTOAX(YMIN,YMAX(I),YLO,YUP,YINC,YDIV,NYDIF)
      IF(IPLOT1.EQ.2)IPLOT1=1         
c *** labels
      NLAB=4
      if (FREQ.GE.1e6) then
         WRITE(LAB(1),8102) FREQ*1e-6
      else if (FREQ.GE.1e3) then
         WRITE(LAB(1),8101) FREQ*1e-3
      else
         WRITE(LAB(1),810) FREQ
      end if
      WRITE(LAB(2),811) SD
      WRITE(LAB(3),812) RD
      WRITE(LAB(4),813) M
 810  FORMAT('Freq:',F7.1,' Hz$')
 8101 FORMAT('Freq:',F6.1,' kHz$')
 8102 FORMAT('Freq:',F6.1,' MHz$')
 811  FORMAT('SD:',F9.1,' m$')
 812  FORMAT('RD:',F9.1,' m$')
 813  FORMAT('M:',I10)
      PTIT='INTEGRAND'
      WRITE(XTXT,820) NXDIF
 820  FORMAT('Horizontal wavenumber (10**',I3,')$')
      WRITE(YTXT,821) NYDIF
 821  FORMAT('Modulus (10**',I3,')$')
      XTYP='LIN'
      YTYP='LIN'
      IGRID=0
      NC=1
C *** WRITE PLP FILE
      CALL PLPWRI(OPTION,PTIT,TITLE,NLAB,LAB,XLEN,YLEN,
     &                  IGRID,XLEFT,XRIGHT,XINC,XDIV,XTXT,XTYP,
     &                  YLO,YUP,YINC,YDIV,YTXT,YTYP,NC)
      CALL CVMAGS(CFF(IPLOT1,I),2,CFFS(1),1,NN)
      CALL VSQRT(CFFS(1),1,CFFS(1),1,NN)
      CALL PLTWRI(NN,WKMIN,DLWVNL,0.,0.,CFFS(1),1,CFFS(1),1)
 2701 CONTINUE
C                
      RETURN     
C                
      END        
      SUBROUTINE PLVINT(DLWVNL,WK0L,SD,RD,TITLE,M,XLEN,YLEN)                 
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
      INCLUDE 'complo.f'
      REAL YMAX               
      CHARACTER*80 TITLE
      CHARACTER*6 OPTION(2),OPT2
      DATA OPT2 /'VINTTY'/
C
      OPTION(1)=PROGNM
 2400 CONTINUE
C                
C XAXIS DEFINITION     
C
      IPLOT1=MAX(1,ICUT1-100)
      IPLOT2=MIN(NWVNO,ICUT2+100)
      WKMIN=WK0L+(IPLOT1-1)*DLWVNL
      NN=IPLOT2-IPLOT1+1
      XMAX=(WK0L+(NWVNO-1)*DLWVNL)
      XMIN=WK0L   
      CALL AUTOAX(XMIN,XMAX,XLEFT,XRIGHT,XINC,XDIV,NXDIF)
C
      OPTION(1)=PROGNM
      OPTION(2)=OPT2
      IPLOT1=MAX(1,ICUT1-100)
      IPLOT2=MIN(NWVNO,ICUT2+100)
C                
C                
C  YAXIS DEFINITION                   
C                   
      call CVMUL(CFF(1,1),2,CFF(1,2),2,CFFS(1),2,NWVNO,-1)
      CALL VNEG(CFFS(IPLOT1),2,CFFS(1),1,NN)
      CALL VMAX(CFFS(1),1,YMAX,NWVNO)   
      CALL VMIN(CFFS(1),1,YMIN,NWVNO)   
      CALL AUTOAX(YMIN,YMAX,YLO,YUP,YINC,YDIV,NYDIF)
      IF(IPLOT1.EQ.2)IPLOT1=1         
c *** labels
      NLAB=4
      if (FREQ.GE.1e6) then
         WRITE(LAB(1),8102) FREQ*1e-6
      else if (FREQ.GE.1e3) then
         WRITE(LAB(1),8101) FREQ*1e-3
      else
         WRITE(LAB(1),810) FREQ
      end if
      WRITE(LAB(2),811) SD
      WRITE(LAB(3),812) RD
      WRITE(LAB(4),813) M
 810  FORMAT('Freq:',F7.1,' Hz$')
 8101 FORMAT('Freq:',F6.1,' kHz$')
 8102 FORMAT('Freq:',F6.1,' MHz$')
 811  FORMAT('SD:',F9.1,' m$')
 812  FORMAT('RD:',F9.1,' m$')
 813  FORMAT('M:',I10)
      PTIT='VERTICAL INTENSITY'
      WRITE(XTXT,820) NXDIF
 820  FORMAT('Horizontal wavenumber (10**',I3,')$')
      WRITE(YTXT,821) NYDIF
 821  FORMAT('Intensity (10**',I3,')$')

      XTYP='LIN'
      YTYP='LIN'
      IGRID=0
      NC=1
C *** WRITE PLP FILE
      CALL PLPWRI(OPTION,PTIT,TITLE,NLAB,LAB,XLEN,YLEN,
     &                  IGRID,XLEFT,XRIGHT,XINC,XDIV,XTXT,XTYP,
     &                  YLO,YUP,YINC,YDIV,YTXT,YTYP,NC)
      CALL PLTWRI(NN,WKMIN,DLWVNL,0.,0.,CFFS(1),1,CFFS(1),1)
 2701 CONTINUE
C                
      RETURN     
C                
      END        
      SUBROUTINE MODES(IFI,NMMAX,NK,Kout,VKout)
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
      REAL Kout(NMMAX),VKout(NMMAX)
      real k(100),vk(100)
C
      CALL VCLR(K,1,100)
      CALL VCLR(VK,1,100)
      nnn=icut2-icut1+1
      do ii=1,npar
       if (iout(ii).gt.0) then
        ifi=ii
        go to 1
       end if
      end do
 1    if (determ) then     
c >>> the following is for using determinant
       call vmax(arg(icut1),1,ammax,nnn)
       CALL VMOV(ARG(ICUT1),1,FAC(nnn),-1,nnn)
c >>> find maxima of kernel for one parameter
      else
       call getknl(IR,1,1)
       CALL CVMAGS(CFF(icut1,IFI),2,FAC(nnn),-1,nnn)
      end if
      ANK=1.2
      CALL PKVAL(FAC,VK,nnn,K,100,ANK,-1)
      NK=ANK
      DO 10 I=1,NK
       ki=nnn-K(I)+icut1
       k(i)=ki
       if (debug) then
        wn=(wk0+(k(i)-1)*dlwvno)
        write(6,*) '>>> MODES: k,wn,vk=',k(i),wn,arg(ki)
       end if
 10    continue
c 
c >>> check that amplitudes of maxima are above tresshold 1%
c
c      i=2
c 20   if (i.gt.nk) go to 50
c      ik=k(i)
c      if (arg(ik).lt.ammax*0.01) then
c       do 25 j=i,nk-2
c        k(j)=k(j+2)
c        vk(j)=vk(j+2)
c 25     continue
c       nk=nk-2
c      else
c       i=i+2
c      end if
c      go to 20
c 50   continue
c      if (debug) then
      DO 70 I=2,NK,2
       wn=(wk0+(k(i)-1)*dlwvno)
       write(6,*) '>>> MODES: k,wn,Cp=',k(i),wn,
     &            2*pi*freq/wn
 70    continue
c      end if
       call vmov(k,1,kout,1,nk)
       call vmov(vk,1,vkout,1,nk)
      RETURN
      END
      SUBROUTINE EXTMDS(IC1,IC2)
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
C
      DO 20 I=1,npar
      IF (IOUT(I).EQ.0) GO TO 20
      IF (IC1.GT.1) CALL CVFILL(CMPLX(0.,0.),CFF(1,I),2,IC1)
      IF (IC2.LT.NWVNO) CALL CVFILL(CNUL,CFF(IC2+1,I),2,NWVNO-IC2)
      IPLOT1=NWVNO
      IPLOT2=0
      IF (IC1.LE.2.AND.IC2.GE.NWVNO) GO TO 20
      CALL CHERMIT(CFF(1,I),NWVNO,IC1,IC2,DLWVNO,WK0,
     1            IPLOT1,IPLOT2)
 20   CONTINUE
      RETURN
      END
      real function detmk(cak)
      INCLUDE 'compar.f'
      complex cak
      WVNO=CAK
c >>> One set of sources
      nsrow=1
      CALL INITS
      CALL BUILD
      CALL SOLVE
      detmk=detmnt
      return
      end
      real function absknl(cak,ifi)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      complex cak
      WVNO=CAK
c >>> One set of sources
      nsrow=1
      CALL INITS
      CALL BUILD
      CALL SOLVE
      call kernel(cfilek)
      INDXCF=IR+IR*MSUFT*ISROW*(ifi-1)
      absknl=real(cfilek(indxcf))**2 + rimag(cfilek(indxcf))**2
      return
      end
      SUBROUTINE IMPMOD(IFI,DLWVNL,DELFRQ,AKM,PHVEL,GRPVEL,NM)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      parameter (npsx=10, dsnpx=0.5)
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'

      DIMENSION AKM(1),PHVEL(1),GRPVEL(1)
      COMPLEX CIN
      COMPLEX CAK0,csave,dsave
      
      EQUIVALENCE (CAK0,AK0)
      CAK0=CMPLX(1E0,OFFIMA)
C
C
c      DF=DELFRQ/5.
      DF=Freq*0.01
      FSAVE=FREQ
      CSAVE=CSQ
      DSAVE=DSQ
C
C
      FREQ=FSAVE-DF
      DSQ=2*PI*FREQ
      CSQ=DSQ*DSQ
      CALL PINIT2
      DO 10 I=1,NM/2
       DS=DLWVNL
       AK0=AKM(2*I)
       if (determ) then
        rinold=detmk(cak0)     
       else
        rinold=absknl(cak0,ifi)
       end if
       if (debug) write(6,*) '>>> IMPMOD: 0,k,d=',l,ak0,rinold
       do 8 l=1,npsx
 3      ak0=ak0+ds
        if (determ) then
         rin=detmk(cak0)
        else
         rin=absknl(cak0,ifi)
        end if
        if (debug) write(6,*) '>>> IMPMOD: l,k,d=',l,ak0,rin
        if (rin.ge.rinold) then
         rinold=rin
         go to 3
        end if
        phvel(i)=ak0-ds
        rinold=rin
        ds=-ds*dsnpx
 8     continue
 10   CONTINUE
C
C
      FREQ=FSAVE+DF
      DSQ=2*PI*FREQ
      CSQ=DSQ*DSQ
      CALL PINIT2
      DO 20 I=1,NM/2
       DS=-DLWVNL
       AK0=AKM(2*I)
       if (determ) then
        rinold=detmk(cak0)     
       else
        rinold=absknl(cak0,ifi)
       end if
       if (debug) write(6,*) '>>> IMPMOD: 0,k,d=',l,ak0,rinold
       do 18 l=1,npsx
 13     ak0=ak0+ds
        if (determ) then
         rin=detmk(cak0)
        else
         rin=absknl(cak0,ifi)
        end if
        if (debug) write(6,*) '>>> IMPMOD: l,k,d=',l,ak0,rin
        if (rin.ge.rinold) then
         rinold=rin
         go to 13
        end if
        grpvel(i)=ak0-ds
        rinold=rin
        ds=-ds*dsnpx
 18    continue
      ak0=grpvel(i)
      IF ((AK0-PHVEL(I)).GT.1E-10) THEN
       GRPVEL(I)=4E0*PI*DF/(AK0-PHVEL(I))
      ELSE
       GRPVEL(I)=1E6
      END IF
      PHVEL(I)=4E0*PI*FSAVE/(AK0+PHVEL(I))
 20   CONTINUE

      FREQ=FSAVE
      DSQ=DSAVE
      CSQ=CSAVE
      RETURN
      END
      SUBROUTINE PLDISP(PHVEL,GRPVEL,NM,LX,MX,DELFRQ,
     1           XLEN,YLEN,XLEFT,XRIGHT,XINC,
     2           YDOWN,YUP,YINC,TITLE)
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
      INCLUDE 'complo.f'
      DIMENSION PHVEL(1),GRPVEL(1),X(NP2)
      CHARACTER*80 TITLE
      CHARACTER*6 OPTION(2),OPT2
      EQUIVALENCE (X(1),CFFS(1))
      DATA OPTION /'FIPP  ',' DISP '/
      OPTION(1)=PROGNM
      PTIT='DISPERSION CURVES'
      NLAB=0
      XTXT='Frequency (Hz)$'
      YTXT='Velocity (m/s)$'
      XTYP='LIN'
      YTYP='LIN'
      XDIV=1
      YDIV=1
      IGRID=1
      NC=2*NM
C *** WRITE PLP FILE
      CALL PLPWRI(OPTION,PTIT,TITLE,NLAB,LAB,XLEN,YLEN,
     &                  IGRID,XLEFT,XRIGHT,XINC,XDIV,XTXT,XTYP,
     &                  YDOWN,YUP,YINC,YDIV,YTXT,YTYP,NC)

      DO 20 I=1,NM
       LF=MX-LX+1
       TMIN=(LX-1)*DELFRQ
       DT=DELFRQ
C *** READ PHASE VELOCITIES
       REWIND 22
       JK=0
       DO 1300 J=LX,MX                 
        JK=JK+1
        READ(22,*) FR,LNM
        CALL VCLR(PHVEL,1,NM)
        READ(22,*) (PHVEL(III),III=1,LNM)
        X(JK)=PHVEL(I)
 1300  CONTINUE   
       CALL PLTWRI(LF,TMIN,DT,0.,0.,X(1),1,X(1),1)
C *** READ GROUP VELOCITIES
       REWIND 23
       JK=0
       DO 1400 J=LX,MX                 
        JK=JK+1
        READ(23,*) FR,LNM
        CALL VCLR(GRPVEL,1,NM)
        READ(23,*) (GRPVEL(III),III=1,LNM)
        X(JK)=GRPVEL(I)
 1400  CONTINUE   
       CALL PLTWRI(LF,TMIN,DT,0.,0.,X(1),1,X(1),1)
 20   CONTINUE
      RETURN
      END
      SUBROUTINE RDSTRF(FMIN,FMAX,NX,LX,MX,DT,dlfrqp)
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
c      INCLUDE 'compul.f'

      LOGICAL BINFILE
      COMMON /FILFRM/ BINFILE
      character*1 signn
      COMPLEX CARR(NP3)
      EQUIVALENCE (CARR(1),CFF(1,1))

C *** STATEMENT FUNCTION FOR INDEXING CARR
c      INDEXC(IND1,IND2,IND3,IND4)=IND1
c     &       +ls*((IND2-1)+NPLTS*((IND3-1)+NSIN*(IND4-1)))

C ***  OPEN TRF-FILE AND READ HEADING

      CALL READHEAD(ISTAT,nx,lxtrf,mxtrf,dt,nplts,nsin,msft,signn)
      write(6,*)
      write(6,*) '>>> Source TRF file header read'
      write(6,*) '    ISTAT =', istat
      write(6,*) '    LS =   ', ls
      write(6,*) '    NX =   ', nx
      write(6,*) '    LX =   ', lxtrf
      write(6,*) '    MX =   ', mxtrf
      write(6,*) '    DT = ', dt
      write(6,*) '    NPLOT =', nplts
      write(6,*) '    NSIN  =', nsin
      write(6,*) '    MSUFT =', msft
      write(6,*) '    SIGN  =', signn

C ***  CHECK SIZE OF INPUT BUFFERS

        NELM=NP3
        IF ((ls*NPLTS*NSIN*MSFT).GT.NELM) THEN
         WRITE(6,*) '>>> READTRF: ARRAY CFF TOO SMALL <<<'
         CLOSE(15,STATUS='KEEP')
         stop
        END IF
        NELM=ISIZE
        IF ((NPLTS*NSIN*MSFT).GT.NELM) THEN
         WRITE(6,*) '>>> READFMT: ARRAY CFILE TOO SMALL <<<'
         CLOSE(15,STATUS='KEEP')
         stop
        END IF
        if (msft.gt.1.or.nplts.gt.1) then
         write(6,*) '>>> Source trf file may only contain  <<<'
         write(6,*) '>>> one vertical source array         <<<'
         CLOSE(15,STATUS='KEEP')
         stop
        end if

        DLFRQP=1.0/REAL(DT*NX)
        DOMEGA=8.0*ATAN(1.0)*DLFRQP
c        OPTION(1)=PROGNM
        LX=nint(FMIN/DLFRQP+1)
        LX=MAX(LX,LXTRF,2)
        MX=NINT(FMAX/DLFRQP+1)
        MX=MIN(MX,MXTRF)
        NUMFR=MX-LX+1
*     Open asynchronous scratch file
        NBLOCKS=0.1*MEMLFT()
        CALL OPNBUF(81,2*NSIN,ls*NUMFR,NBLOCKS)

        DO 35 K=LXTRF,MXTRF
        do 40 M=1,MSFT
        DO 40 I=1,NPLTS
          DO 50 J=1,LS
           IF (BINFILE) THEN
            READ(15) (ARG(KK),FAC(KK),KK=1,NSIN)
           ELSE 
            READ(15,*) (ARG(KK),FAC(KK),KK=1,NSIN)
           END IF
           DO 45 KK=1,NSIN
            if (signn.eq.'-') then
             cfile(KK) = cmplx(arg(kk),-fac(kk))
            else
             cfile(kk) = cmplx(arg(kk),fac(kk))
            end if
 45        CONTINUE
           if (k.ge.lx.and.k.le.mx) then
            CALL WRBUF(81,CFILE,2*NSIN)
           end if
 50       CONTINUE
 40      CONTINUE
35      CONTINUE
        CLOSE(15)
        CALL ENFBUF(81)
        call rwdbuf(81)

        RETURN
        END
      SUBROUTINE READHEAD(ISTAT,nx,lxtrf,mxtrf,dt,nplts,nsin,msft,
     &                    signn)
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
      INCLUDE 'comnrd.f'
c      INCLUDE 'compul.f'


      LOGICAL BINFILE
      COMMON /FILFRM/ BINFILE
      character*1 signn
      CHARACTER*8 FILEID
      character*80 titsou,filenm
      character*6 progn
C *** STATEMENT FUNCTION FOR INDEXING CARR
c      INDEXC(IND1,IND2,IND3,IND4)=IND1
c     &       +IR*((IND2-1)+NPLTS*((IND3-1)+NSIN*(IND4-1)))
        ISTAT=0
        BINFILE=.TRUE.
        inquire(unit=1,name=filenm)
        ll=index(filenm,'.')
        filenm=filenm(1:ll)//'strf'
        BINFILE=.FALSE.
        OPEN(15,FILE=FILENm,STATUS='OLD',FORM='FORMATTED',ERR=995)
        READ(15,20) FILEID
        IF (FILEID.NE.'PULSETRF') THEN
          ISTAT=2
          WRITE(6,*) 'FORMATTED FILE'
          RETURN
        END IF
        READ(15,20) PROGN
        READ(15,*) NSIN
        nstrf=nsin
        READ(15,*) (isprm(J),J=1,NSIN)
        READ(15,20) TITsou
        READ(15,20) SIGNN
        READ(15,*) FCTRF
        READ(15,*) SDs
        READ(15,*) sD,sDLOW,ls
        IF (ls.GT.0) THEN
         IF (ls.GT.NRD) THEN
          WRITE(6,*) '>>> TOO MANY RECEIVERS IN FILE <<<'
          ISTAT=3
          RETURN
         END IF
         IF (ls.GT.1) THEN
          sDSTEP=(sDLOW-sD)/FLOAT(ls-1)
         ELSE
          sdSTEP=1.0
         ENDIF
         DO 105 L=1,ls
          sDC(L)=(L-1)*sDSTEP+sD
105      CONTINUE
        ELSE
         ls=ABS(ls)
         READ(15,*) (sDC(L),L=1,ls)
        END IF

        READ(15,*) s0,sSPACE,NPLTS
        READ(15,*)NX,LXTRF,MXTRF,DT
        READ(15,*) ICDRIN
        READ(15,*) OMEGI
C *** READ HEADER EXTENSION
        READ(15,*) MSFT
        READ(15,*) ISROW
        READ(15,*) Idummy
        DO 1300 I=1,2
         READ(15,*) IDUMMY
 1300     CONTINUE
        DO 1400 I=1,5
         READ(15,*) DUMMY
 1400     CONTINUE
 20     FORMAT(1X,A)
        if (signn.eq.'-') then
         omegim=-omegi
        else
         omegim=omegi
        end if
        RETURN

 995    ISTAT=1
        RETURN
        
        END
      SUBROUTINE SPULSE(FREQS,DT,NX,LX,MX)
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
      REAL FFS(2,NP)
      EQUIVALENCE (CFFS(1),FFS(1,1))
      if (istyp.eq.-1) then
       CALL CVFILL(CMPLX(1E0,0E0),CFFS,2,NX/2)
      ELSE
       CALL STERM(FREQS,DT,NX)
       CALL RFFT(CFFS,NX,1)
       CALL RFFTSC(CFFS,NX,1,1)
      END IF
      CALL CVFILL(CNUL,CFF,2,NX/2)
C     CALL HANN(FFS(1,LX),2,FFS(1,LX),2,MX-LX+1,1)
C     CALL HANN(FFS(2,LX),2,FFS(2,LX),2,MX-LX+1,1)
      CALL CVMOV(CFFS(LX),2,CFF(LX,1),2,MX-LX+1)
      IF (LX.EQ.2) CFF(1,1)=CFFS(1)
      CALL RFFT(CFF,NX,-1)
      RETURN
      END
      SUBROUTINE PULSE(FREQS,DT,NX)
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
      REAL FFS(2,NP)
      EQUIVALENCE (CFFS(1),FFS(1,1))

      if (istyp.eq.-1) then
       CALL CVFILL(CMPLX(1E0,0E0),CFFS,2,NX/2)
      ELSE
       CALL STERM(FREQS,DT,NX)
       CALL RFFT(CFFS,NX,1)
       CALL RFFTSC(CFFS,NX,1,1)
      END IF
      RETURN
      END
      SUBROUTINE STERM(FREQS,DELTAT,NXL)
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
c      INCLUDE 'compul.f'
      DIMENSION FF(NP2),AA(3)
      EQUIVALENCE (CFFS(1),FF(1))
      DATA AA /-.48829,.14128,-.01168/
      IF (ISTYP.GT.0) THEN
      TPF=2.0*PI*FREQS                  
      IF (ISTYP.NE.2) THEN
      OFR=1.0/FREQS       
      ELSE
      OFR=1.55/FREQS
        SUM0=-AA(1)-4*AA(2)-9*AA(3)
      END IF              
      FF(1)=0.0     
      DO 25 M=2,NXL  
      TM=(M-1)*DELTAT                   
      FF(M)=0.0     
      GOTO (10,20,30,40,50,60),ISTYP          
 10   IF (TM.LE.OFR) FF(M)=.75-COS(TM*TPF)+.25*COS(2*TM*TPF)
      GO TO 25      
 20   IF (TM.LE.OFR) THEN
        SUM=0.
        DO 21 IHH=1,3
        SUM=SUM-AA(IHH)*IHH*IHH*COS(2*PI/OFR*IHH*TM)
 21     CONTINUE
        FF(M)=SUM
      END IF
      GO TO 25      
 30    IF (TM.LE.OFR) FF(M)=SIN(TPF*TM) 
      GO TO 25      
 40   IF (TM.LE.(4*OFR)) FF(M)=SIN(TPF*TM)*.5*(1-COS(TPF*TM/4.))          
      GO TO 25
 50   IF (TM.LE.OFR) FF(M)=SIN(TPF*TM)-.5*SIN(2*TPF*TM)
      go to 25
 60   nper=nint((freqs*4.0)/(freq2-freq1))
      IF (TM.LE.(nper*OFR)) FF(M)=SIN(TPF*TM)*.5*(1-COS(TPF*TM/nper))          
 25   CONTINUE            
      ELSE
        CALL VCLR(FF,1,NXL)
        READ(66,*,END=61) (FF(M),M=1,NXL)
 61     CONTINUE
      END IF              
      RETURN        
      END           
