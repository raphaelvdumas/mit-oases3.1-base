      SUBROUTINE TLOSS()      
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
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comfip.f'
      INCLUDE 'combes.f'
      REAL TEN
      complex cc,ex,ex1,extab(np)
      logical dbconv
      EQUIVALENCE (LF,NUMFR),(NREC,ISPACE)
      REAL*8 TWOPI,ONOTPI
      real wght(nwght),bbuf(np)
c     real sqrk(np)
      DATA TWOPI,ONOTPI /6.28318530717959D0,0.159154943091895D0/
      DATA TEN /10./
      dbconv=.true.
      IF (DEBUG) WRITE(6,*) 'ENTER TLOSS'
      go to 1
      entry TLTRF()
      dbconv=.false.
      IF (DEBUG) WRITE(6,*) 'ENTER TLTRF'
 1    continue

c 
c >>> For full Bessel integration, prepare weight array.
c >>> First 50% full Bessel. Last 50% Hanning weighted with
c >>> asymptotic Hankel function
c
      if (inttyp.eq.2.and.icdr.eq.0) then
       dwght=(rkmax-brk_0)/(nwght-1)
       onodwg=1e0/dwght
       nwghth=nwght/2
       do i=1,nwghth
        wght(i)=1.0
        wght(i+nwghth)=0.5*( 1e0 + cos( ((i-1)*pi)/(nwghth-1) ) )
       end do
       d_kr=dlran*dlwvno
       do i=1,nwvno
        extab(i)=cexp(-cmplx(0e0,1e0)*(i-1)*d_kr )
       end do
      end if

C
C    CALCULATE WAVENUMBER FACTORS (ONLY FOR FIRST DEPTH)
C
      IF (NREC.EQ.1) THEN       
      IF (DEBUG) WRITE(6,*) '>>> TLOSS wavenumber factors'
       IF (ICDR.EQ.1) THEN
        TERM=0.
       ELSE
        TERM=(0.5+mbf_0)*PI/2E0
       END IF
       RSTP=-DLWVNO*R1
       CALL VRAMP(TERM,RSTP,ARG,1,NWVNO)
       CALL CVEXP(ARG,1,CBUF,2,NWVNO)
c *** sqr(k) for cylindrical geometry
       if (ICDR.eq.0) THEN
        CALL VRAMP(WK0,DLWVNO,fac,1,NWVNO)
        CALL VSQRT(fac,1,fac,1,NWVNO)
        CALL CRVMUL(CBUF,2,fac,1,CBUF,2,NWVNO)
       END IF
      IF (DEBUG) WRITE(6,*) '>>> TLOSS integration factors'
C *** INTEGRATION FACTORS
       DO 10 J=1,LF
        RANGEM=R1+(J-1)*DLRAN
        IF (ICDR.EQ.1.or.rangem.eq.0E0) THEN
         FAC(J)=FNI5
        ELSE
         FAC(J)=FNI5/SQRT(RANGEM)
        END IF
        if (rangem.ne.0e0.and.inttyp.eq.1) then
         th=0.5*dlwvno*rangem
         st=sin(th)
         fac(j)=fac(j)*(st*st)/(th*th)
        end if
 10    CONTINUE
      END IF
C
      IF (NFLAG) THEN
       NEGSP=-1
      ELSE
       NEGSP=1
      END IF
      IF (DEBUG) WRITE(6,*) '>>> TLOSS integration'
      DO 20 I=1,npar
       IF (IOUT(I).GT.0) THEN
C *** CLEAR BUFFER
        CALL VCLR(bbuf,1,2*LF)
        do 15 ISI=1,NEGSP,-2
         IF (DEBUG) WRITE(6,*) '>>> TLOSS: cvmul'
         CALL CVMUL(CBUF(1),2,CFF(1,I),2,CFFS(1),2,NWVNO,ISI)
         IF (DEBUG) WRITE(6,*) '>>> TLOSS: cfft'
         CALL CFFT(CFFS(1),NWVNO,ISI)
C *** MULTIPLY BY RANGE EXPONENTIALS
         IF (DEBUG) WRITE(6,*) '>>> TLOSS: range exponentials'
         DO 12 J=1,LF
          RANGEM=R1+(J-1)*DLRAN
          rr=isi*rangem*offima
          ri=-isi*rangem*wk0
          Ri=ri-INT(ri*ONOTPI)*TWOPI
          cc=CMPLX(rr,ri)
          if (debug) write(6,*) 'Tloss: cc =',cc
          CFFS(J)=CFFS(J)*CEXPT(cc)
 12      CONTINUE
         IF (DEBUG) WRITE(6,*) '>>> TLOSS: range exponentials done'
         IF (I.EQ.3.AND.ISI.LT.0) THEN
          CALL CVSUB(bbuf,2,CFFS,2,bbuf,2,LF)
         ELSE
          CALL VADD(bbuf,1,CFFS,1,bbuf,1,2*LF)
         END IF
 15     CONTINUE
C *** MULTIPLY BY INTEGRATION FACTORS
         IF (DEBUG) WRITE(6,*) '>>> TLOSS: crvmul'
        CALL CRVMUL(bbuf,2,FAC,1,CFFS(1),2,LF)
c
c >>> Last gasp of the FFP!! Here we compensate to full Hankel
c >>> transform at small kr
c
        if (inttyp.eq.2.and.icdr.eq.0) then
c         do jk=1,nwvno
c          wk=wk0+(jk-1)*dlwvno
c          sqrk(jk)=sqrt(wk)
c         end do
         do jr=1,lf
          indx_e=1
          rangem=R1+(jr-1)*DLRAN
          if (rangem.gt.0e0) then
           bkmx=rkmax/rangem
           ibkmx=int((bkmx-wk0)/dlwvno)+1
c           if (mod(jr-1,10).eq.0) write(6,*) 
c     &           'Hankel compensation, r=',rangem,' ibkmx=',ibkmx
           ex1=cexpt(-cmplx(0e0,1e0)*wk0*rangem)
           eex=exp(offima*rangem)
           do jk=1,min(nwvno,ibkmx)
            wk=wk0+(jk-1)*dlwvno
            rk=rangem*wk

c            indx_b=mod((jr-1)*(jk-1),nwvno)+1
            if (rk.ge.brk_0) then
c             write(6,*) 'brk_0=',brk_0
             if (rk.lt.rkmax) then
              awg=RINTPL(wght,brk_0,onodwg,nwght,rk)
             else
              awg=0e0
             end if
             if (i.eq.3.or.i.eq.7) then
              bes_J1= RINTPL(BF(1,2),brk_0,ONODRK,NRKMAX,rk)
              ex=ex1*cbuf(jk)*extab(indx_e)
              cffs(jr)=cffs(jr) + cff(jk,i)*awg*eex*
     &         (-ai*wk*bes_J1*dlwvno - ex*fac(jr))   
             else
              bes_J0= RINTPL(BF(1,1),brk_0,ONODRK,NRKMAX,rk)
              ex= ex1*cbuf(jk)*extab(indx_e)
              cffs(jr)=cffs(jr) + cff(jk,i)*awg*eex*
     &         ( wk*bes_J0*dlwvno - ex*fac(jr))   
c              ex=cexpt(cmplx(0e0,term-rk))
c              cffs(jr)=cffs(jr) + cff(jk,i)*awg*eex*
c     &         ( wk*bes_J0*dlwvno - sqrk(jk)*ex*fac(jr))   
             end if
            else
             cffs(jr)=0e0
            end if
            indx_e=indx_e+(jr-1)
            if (indx_e.gt.nwvno) indx_e=indx_e-nwvno
           end do
          else
          end if
         end do
        end if

        if (i.eq.2.or.i.eq.3) then
c >>> change velocities to micro-meter/sec
         call vsmul(cffs(1),1,1.0E6,cff(1,I),1,2*lf)
        else
         call vmov(cffs(1),1,cff(1,I),1,2*lf)
        end if
       END IF
 20   CONTINUE
C *** CONVERT TO TRANSMISSION LOSS
      IF (DEBUG) WRITE(6,*) '>>> TLOSS conversion'
      if (dbconv) then
      DO 30 I=1,npar
       IF (IOUT(I).GT.0) THEN
        CALL CVMAGS(CFF(1,I),2,ARG,1,LF)
        IF (DEPTAV.or.frcont) THEN
         LOGNUM=30+npar+I
         CALL WRBUF(LOGNUM,ARG,LF)
        END IF
        CALL VCLIP(ARG,1,1E-30,1E30,CFF(1,I),1,LF)
        CALL VALG10(CFF(1,I),1,CFF(1,I),1,LF)
        CALL VSMUL(CFF(1,I),1,TEN,CFF(1,I),1,LF)
        CALL VNEG(CFF(1,I),1,CFF(1,I),1,LF)
       END IF
 30   CONTINUE
      end if
      IF (DEBUG) WRITE(6,*) 'EXIT TLOSS'
      RETURN
      END
      SUBROUTINE GETTLAV        
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
      REAL TEN
      DIMENSION TLAV(NP2,3)
      EQUIVALENCE (TLAV(1,1),CFF(1,1))
      EQUIVALENCE (LF,NUMFR),(NREC,ISPACE)
      DATA TEN /10./
      IF (DEBUG) WRITE(6,*) 'ENTER GETTLAV'
      SFAC=1E0/IR
      DO 20 I=1,npar
      IF (IOUT(I).EQ.0) GO TO 20
        CALL VCLR(TLAV(1,I),1,LF)
        LOGNUM=30+npar+I
        CALL RWDBUF(LOGNUM)
        DO 10 JR=1,IR
          CALL RDBUF(LOGNUM,ARG,LF)
          CALL VSMA(ARG,1,SFAC,TLAV(1,I),1,TLAV(1,I),1,LF)
 10     CONTINUE
      CALL VALG10(TLAV(1,I),1,TLAV(1,I),1,LF)
      CALL VSMUL(TLAV(1,I),1,TEN,TLAV(1,I),1,LF)
      CALL VNEG(TLAV(1,I),1,TLAV(1,I),1,LF)
 20   CONTINUE
      RETURN
      END
      SUBROUTINE PHINT
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
      DIMENSION FFS(2,NP)
      EQUIVALENCE (CFFS(1),FFS(1,1))
      EQUIVALENCE (LF,NUMFR),(NREC,ISPACE)
      IF (DEBUG) WRITE(6,*) 'ENTER PHINT'
C
C    READ KERNELS FROM SCRATCH FILE 30
C
      DO 2 I=1,npar
       IF (IOUT(I).GT.0) THEN
         CALL VCLR(CFF(1,I),1,2*(ICUT1-1))
         CALL VCLR(CFF(ICUT2+1,I),1,2*(NWVNO-ICUT2))
       END IF
 2    CONTINUE
      IF (DEBUG) WRITE(6,*) 'RWDBUF'
      CALL RWDBUF(30)
      IF (DEBUG) WRITE(6,*) 'EXIT RWDBUF'
      DO 4 JR=ICUT1,ICUT2,NWSTEP
      DO 3 I=1,npar
      IF (IOUT(I).GT.0) THEN
       CALL RDBUF(30,CFILE,2*IR)
       CFF(JR,I)=CFILE(NREC)
      END IF
 3    CONTINUE
 4    CONTINUE
C 
C *** PADE APPROXIMATION
C
      IF (PADE) THEN
c       CALL SAFPADE
        stop '>>> PADE Approximation not implemented <<<'
      END IF
C
C *** HERMITE EXTRAPOLATION
C
      IF (ICUT1.GT.2.OR.ICUT2.LT.NWVNO) THEN
      IPL1=NWVNO
      IPL2=0
      DO 6 I=1,npar
      IF (IOUT(I).GT.0) THEN
       CALL CHERMIT(CFF(1,I),NWVNO,ICUT1,ICUT2,DLWVNO,
     1              WK0,IPL1,IPL2)
      END IF
 6    CONTINUE
      END IF
      RETURN
      END
      SUBROUTINE PLTLOS(LF,RMIN,RSTEP,TITLE,INR,
     1      XLEN,YLEN,XLEFT,XRIGHT,XINC,
     2      YUP,YDOWN,YINC,SD,RD)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
C *** TRANSMISSION LOSS VS RANGE     
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'complo.f'
      DIMENSION TLAV(NP2,npar)
      EQUIVALENCE (TLAV(1,1),CFF(1,1))
      CHARACTER*80 TITLE
      CHARACTER*6 OPTION(2)
      character optpar(12)
      DATA optpar(1) /'N'/,
     &     optpar(2) /'W'/,
     &     optpar(3) /'U'/,
     &     optpar(4) /'V'/,
     &     optpar(5) /'R'/,
     &     optpar(6) /'B'/,
     &     optpar(7) /'S'/
      OPTION(1)=PROGNM
      OPTION(2)=optpar(INR)//'TLRAN'
      IF (DEBUG) WRITE(6,*) 'ENTERING PLTLOS'
      PTIT='TRANSMISSION LOSS'
      I=MAX(INR,1)
      NLAB=3
      if (FREQ.GE.1e6) then
         WRITE(LAB(1),8102) FREQ*1e-6
      else if (FREQ.GE.1e3) then
         WRITE(LAB(1),8101) FREQ*1e-3
      else
         WRITE(LAB(1),810) FREQ
      end if
      WRITE(LAB(2),811) SD
      WRITE(LAB(3),812) RD
      XTXT='Range (km)$'
      GOTO (901,902,903,904,905,906,907),INR
 901  YTXT='Normal stress (dB//1Pa)$'
      GO TO 909
 902  YTXT='Vert. particle velocity (dB//1m/s)$'
      go to 909
 903  YTXT='Hor. particle velocity (dB//1m/s)$'
      go to 909
 904  YTXT='Tran. particle velocity (dB//1m/s)$'
      go to 909
 905  YTXT='Radial stress (dB//1Pa)$'
      GO TO 909
 906  YTXT='Bulk stress (dB//1Pa)$'
      GO TO 909
 907  YTXT='Shear stress (dB//1Pa)$'
 909  CONTINUE
      XTYP='LIN'
      YTYP='LIN'
      XDIV=1
      YDIV=1
      IGRID=0
      NC=1
C *** WRITE PLP FILE
      CALL PLPWRI(OPTION,PTIT,TITLE,NLAB,LAB,XLEN,YLEN,
     &                  IGRID,XLEFT,XRIGHT,XINC,XDIV,XTXT,XTYP,
     &                  YDOWN,YUP,YINC,YDIV,YTXT,YTYP,NC)
c      if (.not.rdoas) then
      CALL PLTWRI(LF,RMIN,RSTEP,0.,0.,TLAV(1,I),1,TLAV(1,I),1)
c      end if
C *** FORMATS
 810  FORMAT('Freq:',F7.1,' Hz$')
 8101 FORMAT('Freq:',F6.1,' kHz$')
 8102 FORMAT('Freq:',F6.1,' MHz$')
 811  FORMAT('SD:',F9.1,' m$')
 812  FORMAT('RD:',F9.1,' m$')
      RETURN
      END
      SUBROUTINE PLDAV(LF,RMIN,RSTEP,TITLE,INR,
     1      XLEN,YLEN,XLEFT,XRIGHT,XINC,
     2      YUP,YDOWN,YINC,SD)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
C *** DEPTH-AVERAGED LOSS VS RANGE
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'complo.f'
      DIMENSION TLAV(NP2,npar)
      EQUIVALENCE (TLAV(1,1),CFF(1,1))
      CHARACTER*80 TITLE
      CHARACTER*6 OPTION(2)
      character optpar(12)
      DATA optpar(1) /'N'/,
     &     optpar(2) /'W'/,
     &     optpar(3) /'U'/,
     &     optpar(4) /'V'/,
     &     optpar(5) /'R'/,
     &     optpar(6) /'B'/,
     &     optpar(7) /'S'/
      IF (DEBUG) WRITE(6,*) 'ENTERING PLDAV'
      OPTION(1)=PROGNM
      OPTION(2)=optpar(INR)//'TLDAV'
      PTIT='DEPTH AVERAGED LOSS'
      I=MAX(INR,1)
      NLAB=2
      if (FREQ.GE.1e6) then
         WRITE(LAB(1),8102) FREQ*1e-6
      else if (FREQ.GE.1e3) then
         WRITE(LAB(1),8101) FREQ*1e-3
      else
         WRITE(LAB(1),810) FREQ
      end if
      WRITE(LAB(2),811) SD
      XTXT='Range (km)$'
      GOTO (901,902,903,904,905,906),INR
 901  YTXT='Normal stress (dB//1Pa)$'
      GO TO 907
 902  YTXT='Vert. particle velocity (dB//1m/s)$'
      go to 907
 903  YTXT='Hor. particle velocity (dB//1m/s)$'
      go to 907
 904  YTXT='Tran. particle velocity (dB//1m/s)$'
      go to 907
 905  YTXT='Radial stress (dB//1Pa)$'
      GO TO 907
 906  YTXT='Bulk stress (dB//1Pa)$'
 907  CONTINUE
      XTYP='LIN'
      YTYP='LIN'
      XDIV=1
      YDIV=1
      IGRID=0
      NC=1
C *** WRITE PLP FILE
      CALL PLPWRI(OPTION,PTIT,TITLE,NLAB,LAB,XLEN,YLEN,
     &                  IGRID,XLEFT,XRIGHT,XINC,XDIV,XTXT,XTYP,
     &                  YDOWN,YUP,YINC,YDIV,YTXT,YTYP,NC)
      CALL PLTWRI(LF,RMIN,RSTEP,0.,0.,TLAV(1,I),1,TLAV(1,I),1)
C *** FORMATS
 810  FORMAT('Freq:',F7.1,' Hz$')
 8101 FORMAT('Freq:',F6.1,' kHz$')
 8102 FORMAT('Freq:',F6.1,' MHz$')
 811  FORMAT('SD:',F9.1,' m$')
      RETURN
      END
      SUBROUTINE PTLDEP(LF,RMIN,RSTEP,TITLE,INR,
     1      XLEN,YLEN,XLEFT,XRIGHT,XINC,
     2      YUP,YDOWN,YINC,SD,RD)
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
      INCLUDE 'complo.f'
      CHARACTER*80 TITLE
      CHARACTER*6 OPTION(2)
      real x(np2,npar)
      equivalence (x(1,1),cff(1,1))
      character optpar(12)
      DATA optpar(1) /'N'/,
     &     optpar(2) /'W'/,
     &     optpar(3) /'U'/,
     &     optpar(4) /'V'/,
     &     optpar(5) /'R'/,
     &     optpar(6) /'B'/,
     &     optpar(7) /'S'/
      IF (DEBUG) WRITE(6,*) 'ENTERING PLDEP'
      OPTION(1)=PROGNM
      OPTION(2)=optpar(INR)//'TLDEP'
      I=MAX(1,INR)
      if (debug) then
       write(6,*) '>>> PTLDEP <<<'
       write(6,*) 'rmin,rstep,lf=',rmin,rstep,lf
       do j=1,lf
        write(6,*) x(j,i)
       end do
      end if
      PTIT='TRANSMISSION LOSS'
 810  FORMAT('Freq:',F7.1,' Hz$')
 8101 FORMAT('Freq:',F6.1,' kHz$')
 8102 FORMAT('Freq:',F6.1,' MHz$')
 811  FORMAT('SD:',F9.1,' m$')
 812  FORMAT('Range:',F6.1,' km$')
      NLAB=3
      if (FREQ.GE.1e6) then
         WRITE(LAB(1),8102) FREQ*1e-6
      else if (FREQ.GE.1e3) then
         WRITE(LAB(1),8101) FREQ*1e-3
      else
         WRITE(LAB(1),810) FREQ
      end if
      WRITE(LAB(2),811) SD
      WRITE(LAB(3),812) RD
      GOTO (901,902,903,904,905,906),INR
 901  xTXT='Normal stress (dB//1Pa)$'
      GO TO 907
 902  xTXT='Vert. particle velocity (dB//1m/s)$'
      go to 907
 903  xTXT='Hor. particle velocity (dB//1m/s)$'
      go to 907
 904  xTXT='Tran. particle velocity (dB//1m/s)$'
      go to 907
 905  xTXT='Radial stress (dB//1Pa)$'
      GO TO 907
 906  xTXT='Bulk stress (dB//1Pa)$'
 907  CONTINUE
      YTXT='Depth (m)$'
      XTYP='LIN'
      YTYP='LIN'
      XDIV=1
      YDIV=1
      IGRID=0
      NC=1
C *** WRITE PLP FILE
      CALL PLPWRI(OPTION,PTIT,TITLE,NLAB,LAB,XLEN,YLEN,
     &                  IGRID,XLEFT,XRIGHT,XINC,XDIV,XTXT,XTYP,
     &                  YDOWN,YUP,YINC,YDIV,YTXT,YTYP,NC)
c      CALL PLTWRI(LF,0.,0.,RMIN,RSTEP,CFF(1,I),1,CFF(1,I),1)
      CALL PLTWRI(LF,0.,0.,0.,0.,CFF(1,I),1,RDC(1),1)
      RETURN
      END
      SUBROUTINE CONDRW(TITLE,NPX,NPY,NX,NY,XLEFT,XRIGHT   
     $,XSCALE,XINC,YUP,YDOWN,YSCALE,YINC,ZMIN              
     $,ZMAX,ZSTEP,FREQ,SD,RECUP,RECLO,X1,XL,PX,icdr)                 
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      DIMENSION SECTOR(28),PX(1)      
      CHARACTER*50 FILENM
      CHARACTER*4 TITLE(20),TITLEX(20),TITLEY(20)      
      DATA X1PL,Y1PL/2.,2.0/,HGTPT,HGTC,LABPT,NDIV,        
     *NARC/0.1,0.14,-3,1,5/,LABC,LWGT/-1,-1/,NSM/0/         
      DATA DUMMY /0./
      DATA TITLEX /'Rang','e (k','m)  ',17*'    '/
      DATA TITLEY /'Dept','h (m',')   ',17*'    '/
C                
C   FORMATS      
 401  FORMAT(1H ,F15.4,3X,'  NUMBER OF DATA POINTS ALONG THE X AXIS')           
 402  FORMAT(1H ,F15.4,3X,'  NUMBER OF DATA POINTS ALONG THE Y AXIS')           
403   FORMAT(1H ,F15.4,3X,'  DIVX ' )
404   FORMAT(1H ,F15.4,3X,'  DIVY ' )
405   FORMAT(1H ,F15.4,3X,'  FLAGRC ' )
406   FORMAT(1H ,F15.4,3X,'  RDUP ' )
407   FORMAT(1H ,F15.4,3X,'  RDLO ' )           
408   FORMAT(1H ,F15.4,3X,'  SOURCE DEPTH (M) ' )          
 409  FORMAT(1H ,F15.4,3X,'  NUMBER OF GRID POINTS ALONG THE X AXIS ' )         
 410  FORMAT(1H ,F15.4,3X,'  NUMBER OF GRID POINTS ALONG THE Y AXIS ' )         
  411 FORMAT(1H ,F15.4,3X,'  FREQUENCY (HZ)' )             
  412 FORMAT(1H ,F15.4,3X,'  DUMMY ' )
  413 FORMAT(1H ,F15.4,3X,'  CAY ' )
  414 FORMAT(1H ,F15.4,3X,'  NRNG ' )
  415 FORMAT(1H ,F15.4,3X,'  ZMIN ' ) 
  416 FORMAT(1H ,F15.4,3X,'  ZMAX ' ) 
  417 FORMAT(1H ,F15.4,3X,'  ZINC ' ) 
  418 FORMAT(1H ,F15.4,3X,'  X ORIGIN OF PLOT IN INCHES ' )
  419 FORMAT(1H ,F15.4,3X,'  DUMMY ' )
  420 FORMAT(1H ,F15.4,3X,'  Y ORIGIN OF PLOT IN INCHES ' )
  421 FORMAT(1H ,F15.4,3X,'  NSM   ' )
  422 FORMAT(1H ,F15.4,3X,'  HGTPT ' )
  423 FORMAT(1H ,F15.4,3X,'  HGTC ' ) 
  424 FORMAT(1H ,F15.4,3X,'  LABPT ' )
  425 FORMAT(1H ,F15.4,3X,'  NDIV ' ) 
  426 FORMAT(1H ,F15.4,3X,'  NARC ' ) 
  427 FORMAT(1H ,F15.4,3X,'  LABC ' ) 
  428 FORMAT(1H ,F15.4,3X,'  LWGT ' ) 
  800 FORMAT('CONDR,OAS,FMT,CPX              ')             
 801  FORMAT(A50)                
  850 FORMAT(20A4)                    
  900 FORMAT(1X,F15.4,3X,'  XLEFT',/,F15.4,4X,'  XRIGHT',/,F15.4,3X,            
     *'   XSCALE',/,F15.4,4X,'  XINC')
  901 FORMAT(1X,F15.4,3X,'  YUP',/,F15.4,4X,'  YDOWN',/,F15.4,3X,               
     *'   YSCALE',/,F15.4,4X,'  YINC')
  950 FORMAT(1H ,F15.4,1X,'    RMIN',/,F15.4,2X,'    RMAX')
      WRITE(28,800)             
      WRITE(28,850)TITLE              
      CALL VCLR(SECTOR,1,28)
      SECTOR(1)=NPX
      sector(2)=NPY                   
C                
C   SECTOR(4) IS A FLAG WHICH IS SET TO ZERO IN THE RANGE  
C   DEPENDENT VERSION OF SNAP FOR ALL SECTORS EXCEPT THE LAST                   
C   ONE. HERE IS USED TO INDICATE THAT THIS IS THE LAST SECTOR                  
       SECTOR(4)=1.0                  
c >>> set cyl/plane flag and dr
      sector(5)=icdr
      sector(6)=abs(xl-x1)*1.0E3/(npx-1)
      WRITE(29,444) (SECTOR(L),L=1,28)
 444  FORMAT(1H ,6G13.5)
      INQUIRE(UNIT=29,NAME=FILENM)
      WRITE(28,801) FILENM             
      IF (ABS(XL-X1).LT.1.0) THEN
      TITLEX(2)='e (m'
      TITLEX(3)=')   '
      DIVX=1E0
      ELSE
      TITLEX(2)='e (k'
      TITLEX(3)='m)  '
      DIVX=1E-3
      END IF
      DIVY=1E0
      CAY=5.
      NRNG=5
      FLAGRC=0.
      WRITE(28,850)TITLEX             
      R1=X1*1.0E3
      R2=XL*1.0E3
      WRITE(28,950)R1,R2              
      AX1=XLEFT*1.0E3                 
      AX2=XRIGHT*1.0E3                
      AX3=XSCALE*1.0E3                
      AX4=XINC*1.0E3                  
      WRITE(28,900)AX1,AX2,AX3,AX4    
      WRITE(28,850)TITLEY             
      WRITE(28,901)YUP,YDOWN,YSCALE,YINC                   
      WRITE(28,401)FLOAT(NPX)
      WRITE(28,402)FLOAT(NPY)
      WRITE(28,403)DIVX              
      WRITE(28,404)DIVY              
      WRITE(28,405)FLAGRC              
      WRITE(28,406)RECUP              
      WRITE(28,407)RECLO                   
      WRITE(28,408)SD     
C   NUMBER OF GRID POINTS ALONG THE X AXIS                 
      WRITE(28,409)FLOAT(NX)          
C   NUMBER OF GRID POINTS ALONG THE Y AXIS                 
      WRITE(28,410)FLOAT(NY)          
      WRITE(28,411)FREQ      
      WRITE(28,412)DUMMY              
      WRITE(28,413)CAY              
      WRITE(28,414)FLOAT(NRNG)              
      WRITE(28,415)ZMIN               
      WRITE(28,416)ZMAX               
      WRITE(28,417)ZSTEP              
C X ORIGIN  OF PLOT IN INCHES         
      WRITE(28,418)X1PL               
      WRITE(28,419)DUMMY              
C Y ORIGIN  OF PLOT IN INCHES         
      WRITE(28,420)Y1PL               
      WRITE(28,421)FLOAT(NSM)                
      WRITE(28,422)HGTPT              
      WRITE(28,423)HGTC               
      WRITE(28,424)FLOAT(LABPT)       
      WRITE(28,425)FLOAT(NDIV)        
      WRITE(28,426)FLOAT(NARC)        
      WRITE(28,427)FLOAT(LABC)        
      WRITE(28,428)FLOAT(LWGT)        
      RETURN     
      END       

      SUBROUTINE PLSPECT(DLWVNL,WK0L,SD,RD,TITLE      
     *,XLEN,YLEN,LAY)  
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
      INCLUDE 'comnla.f'
      INCLUDE 'complo.f'
      LOGICAL NEGASP
      DIMENSION SPEC(NP,2)  
      CHARACTER*80 TITLE
      CHARACTER*6 OPTION(2)
      equivalence (SPEC(1,1),CFFS(1))
      character optpar(12)
      DATA optpar(1) /'N'/,
     &     optpar(2) /'W'/,
     &     optpar(3) /'U'/,
     &     optpar(4) /'V'/,
     &     optpar(5) /'R'/,
     &     optpar(6) /'B'/,
     &     optpar(7) /'S'/
      IF (DEBUG) WRITE(6,*) 'ENTERING PLSPECT'
      OPTION(1)=PROGNM
C   
      NEGASP=.FALSE.
      OMR=180./PI
      IPLOT1=ICUT2
      IPLOT2=ICUT1
      DO 2401 II=ICUT1,ICUT2           
      WVNO=WK0L+(II-1)*DLWVNL
      SSS=WVNO/REAL(AK(LAY,1))
C      IF (SSS.LE.1.AND.SSS.GE.0) THEN
      IF (ABS(SSS).LE.1) THEN
       IF (SSS.LE.0E0) NEGASP=.TRUE.
       IPLOT1=MIN0(IPLOT1,II)
       IPLOT2=MAX0(IPLOT2,II)
       SPEC(II,1)=ACOS(SSS)
       CC=SIN(SPEC(II,1))
       SPEC(II,2)=REAL(AK(LAY,1))*CC**2
       SPEC(II,1)=OMR*SPEC(II,1)
      END IF
 2401  CONTINUE
C   
C XAXIS DEFINITION     
C   
      NN=IPLOT2-IPLOT1+1
      if (nn.le.1) return
      IF (NEGASP) THEN
       XRIGHT=0.0
       XLEFT=180.
       XINC=30.
      ELSE
       XLEFT=0.
       XRIGHT=90.
       XINC=15.
      END IF
      XDIV=1.
      NXDIF=0
C   
      DO 2701 I=1,npar    
      IF (IOUT(I).EQ.0) GO TO 2701        
      OPTION(2)=optpar(I)//'SPECT'
C   
C  YAXIS DEFINITION    
C   
      CALL CVMAGS(CFF(IPLOT1,I),2,ARG,1,NN)
      CALL VMUL(ARG,1,SPEC(IPLOT1,2),1,ARG,1,NN)

      if (db) then
       call vclip(arg,1,1e-30,1e30,arg,1,nn)
       call valg10(arg,1,arg,1,nn)
       call vsmul(arg,1,10.0,arg,1,nn)
       CALL VMAX(ARG,1,YMAX,NN)
       YMIN=YMAX-80.0
      else
       CALL VMAX(ARG,1,YMAX,NN)
       YMIN=0.0         
      end if
      CALL AUTOAX(YMIN,YMAX,YDOWN,YUP,YINC,YDIV,NYDIF)
      if (db) ydiv=1.0
      PTIT='ANGULAR SPECTRUM'
 810  FORMAT('Freq:',F7.1,' Hz$')
 8101 FORMAT('Freq:',F6.1,' kHz$')
 8102 FORMAT('Freq:',F6.1,' MHz$')
 811  FORMAT('SD:',F9.1,' m$')
 812  FORMAT('RD:',F9.1,' m$')
      NLAB=3
      if (FREQ.GE.1e6) then
         WRITE(LAB(1),8102) FREQ*1e-6
      else if (FREQ.GE.1e3) then
         WRITE(LAB(1),8101) FREQ*1e-3
      else
         WRITE(LAB(1),810) FREQ
      end if
      WRITE(LAB(2),811) SD
      WRITE(LAB(3),812) RD
      XTXT='Grazing angle (degrees)$'
      WRITE(YTXT,821) NYDIF
 821  FORMAT('Power (10**',I3,')$')
      XTYP='LIN'
      YTYP='LIN'
      XDIV=1
      IGRID=0
      NC=1
C *** WRITE PLP FILE
      CALL PLPWRI(OPTION,PTIT,TITLE,NLAB,LAB,XLEN,YLEN,
     &                  IGRID,XLEFT,XRIGHT,XINC,XDIV,XTXT,XTYP,
     &                  YDOWN,YUP,YINC,YDIV,YTXT,YTYP,NC)
      CALL PLTWRI(NN,0.,0.,0.,0.,SPEC(IPLOT1,1),1,ARG,1)
 2701 CONTINUE
      RETURN           
      END              










