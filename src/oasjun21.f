      SUBROUTINE REFLEC(ANGLE1,ANGLE2,IPARM) 
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
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
      COMPLEX RFCOEF,TRCOEF
      EXTERNAL RFCOEF,TRCOEF
      DIMENSION X(NP2,NPAR)
      EQUIVALENCE (CFF(1,1),X(1,1))
C       
      if (debug) write(6,*) '>>> Entering REFLEC <<<'
      if (rctable) then
c       if (iparm.eq.1) then
c        write(22,'(a)') 'P-P REFLECTION COEFFICIENT'
c       else
c        write(22,'(a)') 'P-SV REFLECTION COEFFICIENT'
c       end if
       write(22,'(1h ,f12.3,i6,a)') freq, nwvno,
     &            '  # Frequency, # of slownesses'
       write(23,'(1h ,f12.3,i6,a)') freq, nwvno,
     &            '  # Frequency, # of angles'
      end if
      IF (SCTOUT) THEN
c       CALL OPFILB(45,IOER)
       WRITE(45) FREQ,LAYS(1),NWVNO,NFLAG,fni5
      END IF
      if (slowrc) then
       angra=angle1*1e-3
       IF (NWVNO.GT.1) THEN
        DLANGLE=(ANGLE2-ANGLE1)/(NWVNO-1)
        dlangle=dlangle*1e-3
       ELSE
        DLANGLE=1
       END IF
      else
       ANGRA=ANGLE1*PI/180.0
       IF (NWVNO.GT.1) THEN
        DLANGLE=(ANGLE2-ANGLE1)/(NWVNO-1)
        DLANGLE=DLANGLE*PI/180.
       ELSE
        DLANGLE=1
       END IF
      end if
      OMR=180./PI
      DO 2400 II=1,NWVNO
      ANG=ANGRA+(II-1)*DLANGLE
      if (slowrc) then
c >>>  convert slowess to wavenumber
       wvno=ang*dsq
      else
c >>>  convert slowess to wavenumber
       if (laytyp(1).eq.5) then
        if (fs_switch(1)) then
         wvno=real(sqrt(ck2(1)))*cos(ang)
        else 
         wvno=real(sqrt(ck1(1)))*cos(ang)
        end if
c        if (debug) write(6,*) 'wvno=',wvno
c        wvno= cos(ang)*real(dsq) /v(1,2)
       else
        WVNO=REAL(AK(1,1))*COS(ANG)
       end if
      end if
c >>> One set of sources
      nsrow=1
      CALL INITS
      CALL BUILD  
      CALL SOLVE    
      IF (IERR.GT.0) RETURN
      if (transmit) then
       cff(ii,1)=trcoef(iparm)
      else
       cff(ii,1)=rfcoef(iparm)
      end if
c
c >>> write reflection coefficient to table files
c
      if (rctable) then
       if (slowrc) then
        slw=ang
        degang=slw*v(1,2)
c >>> real angle?
        if (abs(degang).le.1e0) then
         degang=acos(degang)*omr
        else
         degang=0e0
        end if
       else
         slw=real(ak(1,1))*cos(ang)/dsq      
         degang=ang*180.0/pi
       end if
       rmod=abs(cff(ii,1))
       phang=omr*atan2z(rimag(cff(ii,1)),real(cff(ii,1)))
       write(22,'(3f15.6)') slw*1e3,rmod,phang
       write(23,'(3f15.6)') degang,rmod,phang
      end if
c
c *** write scattered field to file 45
      IF (SCTOUT) CALL SCTRHS(ALFA(1))
      IF (DEBUG) WRITE(6,*) FREQ,ANG,ABS(CFF(II,1))
 2400  CONTINUE     

      CALL CVMAGS(CFF(1,1),2,CFF(1,2),1,NWVNO)
      call vclip(cff(1,2),1,1.0E-30,1.0E30,cff(1,2),1,nwvno)
      CALL VALG10(CFF(1,2),1,CFF(1,2),1,NWVNO)
      CALL VNEG(CFF(1,2),1,CFF(1,2),1,NWVNO)
      CALL VSMUL(CFF(1,2),1,10.0,CFF(1,2),1,NWVNO)
      DO 2500 II=1,NWVNO
      X(II,3)=OMR*ATAN2z(RIMAG(CFF(II,1)),REAL(CFF(II,1)))
 2500 CONTINUE
      WRITE(30) (X(JJ,2),JJ=1,NWVNO)
      WRITE(31) (X(JJ,3),JJ=1,NWVNO)

      RETURN
      END   
      COMPLEX FUNCTION RFCOEF(INT) 
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      complex cc

      if (debug) write(6,*) '>>> Entering RFCOEF <<<'

      if (laytyp(1).eq.5.and.fs_switch(1)) then
       cc=beta(1)
      else
       cc=alfa(1)
      end if

      is=1
      if (laytyp(1).eq.5) then
c >>> Biot layer
       if (int.eq.2) then
        RFCOEF=cc*SS(1,6,is)/(CPHFAC(1)*EXP(-ZLS(1)*cc))
       else if ((int.eq.3.and.fs_switch(1)).or.
     &          (int.eq.1.and.(.not.fs_switch(1)))) then
        RFCOEF=cc*SS(1,3,is)/(CPHFAC(1)*EXP(-ZLS(1)*cc))
       else
        RFCOEF=cc*SS(1,4,is)/(CPHFAC(1)*EXP(-ZLS(1)*cc))
       end if
      else
       if (int.eq.3) then
        rfcoef=cnul
       else
        RFCOEF=cc*SS(1,2+INT,is)/(CPHFAC(1)*EXP(-ZLS(1)*cc))
       end if
      end if
      if (debug) then 
       write(6,*) 'alfa,ss',cc,ss(1,2+int,is)
      end if
      RETURN
      END   

      COMPLEX FUNCTION TRCOEF(INT) 
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      complex cc

      if (debug) write(6,*) '>>> Entering TRCOEF <<<'

      if (laytyp(1).eq.5.and.fs_switch(1)) then
       cc=beta(1)
      else
       cc=alfa(1)
      end if

      is=1
      if (laytyp(numl).eq.5) then
c >>> Biot layer
       if (int.eq.2) then
        TRCOEF=cc*SS(NUML,5,is)/(CPHFAC(1)*EXP(-ZLS(1)*cc))
       else if ((int.eq.3.and.fs_switch(numl)).or.
     &          (int.eq.1.and.(.not.fs_switch(numl)))) then
        TRCOEF=cc*SS(NUML,1,is)/(CPHFAC(1)*EXP(-ZLS(1)*cc))
       else
        TRCOEF=cc*SS(NUML,2,is)/(CPHFAC(1)*EXP(-ZLS(1)*cc))
       end if
      else
       if (int.eq.3) then
        TRCOEF=cnul
       else
        TRCOEF=cc*SS(NUML,INT,is)/(CPHFAC(1)*EXP(-ZLS(1)*cc))
       end if       
      end if
      if (debug) then 
       write(6,*) 'alfa,ss',cc,ss(numl,int,is),trcoef
      end if
      RETURN
      END   

      SUBROUTINE PLRC(LF,RMIN,RSTEP,TITLE,INR,
     1      XLEN,YLEN,XLEFT,XRIGHT,XINC,
     2      YUP,YDOWN,YINC,FREQL)
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
      DIMENSION X(NP2,NPAR)
      CHARACTER*80 TITLE
      CHARACTER*6 OPTION(2),OPT2(3)
      EQUIVALENCE (X(1,1),CFF(1,1))
      DATA OPT2 /' P-P  ',' P-SV ',' P-SL '/
      OPTION(1)=PROGNM
      OPTION(2)=OPT2(INR)
      I=MAX(1,INR)
      if (transmit) then
       PTIT='TRANSMISSION COEFFICIENT'
      else
       PTIT='REFLECTION COEFFICIENT'
      end if
      NLAB=1
      WRITE(LAB(1),801) FREQL
 801  FORMAT('FREQ:',F7.1,' Hz$')
      if (slowrc) then
       xtxt='Slowness (s/km)$'
      else
       XTXT='Grazing angle (deg)$'
      end if
      YTXT='Magnitude$'
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
      CALL CVMAGS(CFF(1,1),2,ARG,1,LF)
      call vsqrt(arg,1,arg,1,lf)
      CALL PLTWRI(LF,RMIN,RSTEP,0.,0.,ARG(1),1,ARG(1),1)
      RETURN
      END
      SUBROUTINE PLREFL(LF,RMIN,RSTEP,TITLE,INR,
     1      XLEN,YLEN,XLEFT,XRIGHT,XINC,
     2      YUP,YDOWN,YINC,FREQL)
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
      DIMENSION X(NP2,NPAR)
      CHARACTER*80 TITLE
      CHARACTER*6 OPTION(2),OPT2(3)
      EQUIVALENCE (X(1,1),CFF(1,1))
      DATA OPT2 /' P-P  ',' P-SV ',' P-SL '/
      OPTION(1)=PROGNM
      OPTION(2)=OPT2(INR)
      I=MAX(1,INR)
      if (transmit) then
       PTIT='TRANSMISSION COEFFICIENT'
      else
       PTIT='REFLECTION COEFFICIENT'
      end if
      NLAB=1
      WRITE(LAB(1),801) FREQL
 801  FORMAT('FREQ:',F7.1,' Hz$')
      if (slowrc) then
       xtxt='Slowness (s/km)$'
      else
       XTXT='Grazing angle (deg)$'
      end if
      YTXT='Loss (dB)$'
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
      CALL PLTWRI(LF,RMIN,RSTEP,0.,0.,X(1,2),1,X(1,2),1)
      RETURN
      END
      SUBROUTINE PLREFP(LF,RMIN,RSTEP,TITLE,INR,
     1      XLEN,YLEN,XLEFT,XRIGHT,XINC,FREQL)
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
      DIMENSION X(NP2,NPAR)
      CHARACTER*80 TITLE
      CHARACTER*6 OPTION(2),OPT2(3)
      EQUIVALENCE (X(1,1),CFF(1,1))
      DATA OPT2 /' P-P  ',' P-SV ',' P-SL '/
      OPTION(1)=PROGNM
      OPTION(2)=OPT2(INR)
      I=MAX(1,INR)
      if (transmit) then
       PTIT='TRANSMISSION COEFFICIENT'
      else
       PTIT='REFLECTION COEFFICIENT'
      end if
      NLAB=1
      WRITE(LAB(1),801) FREQL
 801  FORMAT('FREQ:',F7.1,' Hz$')
      if (slowrc) then
       xtxt='Slowness (s/km)$'
      else
       XTXT='Grazing angle (deg)$'
      end if
      YTXT='Phase angle (deg)$'
      XTYP='LIN'
      YTYP='LIN'
      XDIV=1
      ydown=-180
      YUP=180
      YINC=90
      YDIV=1
      IGRID=0
      NC=1
C *** WRITE PLP FILE
      CALL PLPWRI(OPTION,PTIT,TITLE,NLAB,LAB,XLEN,YLEN,
     &                  IGRID,XLEFT,XRIGHT,XINC,XDIV,XTXT,XTYP,
     &                  YDOWN,YUP,YINC,YDIV,YTXT,YTYP,NC)
      CALL PLTWRI(LF,RMIN,RSTEP,0.,0.,X(1,3),1,X(1,3),1)
      RETURN
      END
      SUBROUTINE PLREFF(LF,RMIN,RSTEP,TITLE,INR,
     1      XLEN,YLEN,XLEFT,XRIGHT,XINC,
     2      YUP,YDOWN,YINC,ANGLE,IANGLE)
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
      DIMENSION X(NP2,NPAR)
      CHARACTER*80 TITLE
      CHARACTER*6 OPTION(2),OPT2(3)
      EQUIVALENCE (X(1,1),CFF(1,1))
      DATA OPT2 /' P-P  ',' P-SV ',' P-SL '/
      OPTION(1)=PROGNM
      OPTION(2)=OPT2(INR)
      I=MAX(1,INR)
      if (transmit) then
       PTIT='TRANSMISSION COEFFICIENT'
      else
       PTIT='REFLECTION COEFFICIENT'
      end if
      NLAB=1
      if (slowrc) then
       WRITE(LAB,800) ANGLE
 800   FORMAT('P:',F6.3,' km/s$')
      else
       WRITE(LAB,801) ANGLE
 801   FORMAT('ANGLE:',F5.1,' deg$')
      end if
      XTXT='Frequency (Hz)$'
      YTXT='Loss (dB)$'
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
      REWIND(30)
      DO 1350 J=1,LF     
      READ(30) (X(JJ,2),JJ=1,NWVNO)
      X(J,1)=X(IANGLE,2)       
 1350 CONTINUE  
      CALL PLTWRI(LF,RMIN,RSTEP,0.,0.,X(1,1),1,X(1,1),1)
      RETURN
      END
      SUBROUTINE PLRCF(LF,RMIN,RSTEP,TITLE,INR,
     1      XLEN,YLEN,XLEFT,XRIGHT,XINC,
     2      YUP,YDOWN,YINC,ANGLE,IANGLE)
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
      DIMENSION X(NP2,NPAR)
      CHARACTER*80 TITLE
      CHARACTER*6 OPTION(2),OPT2(3)
      EQUIVALENCE (X(1,1),CFF(1,1))
      DATA OPT2 /' P-P  ',' P-SV ',' P-SL '/
      OPTION(1)=PROGNM
      OPTION(2)=OPT2(INR)
      I=MAX(1,INR)
      if (transmit) then
       PTIT='TRANSMISSION COEFFICIENT'
      else
       PTIT='REFLECTION COEFFICIENT'
      end if
      NLAB=1
      if (slowrc) then
       WRITE(LAB,800) ANGLE
 800   FORMAT('P:',F6.3,' km/s$')
      else
       WRITE(LAB,801) ANGLE
 801   FORMAT('ANGLE:',F5.1,' deg$')
      end if
      XTXT='Frequency (Hz)$'
      YTXT='Magnitude$'
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
      REWIND(30)
      DO 1350 J=1,LF     
      READ(30) (X(JJ,2),JJ=1,NWVNO)
      X(J,1)=10.0**(-0.05*X(IANGLE,2))       
 1350 CONTINUE  
      CALL PLTWRI(LF,RMIN,RSTEP,0.,0.,X(1,1),1,X(1,1),1)
      RETURN
      END
      SUBROUTINE PLRFPH(LF,RMIN,RSTEP,TITLE,INR,
     1      XLEN,YLEN,XLEFT,XRIGHT,XINC,
     2      ANGLE,IANGLE)
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
      DIMENSION X(NP2,NPAR)
      CHARACTER*80 TITLE
      CHARACTER*6 OPTION(2),OPT2(3)
      EQUIVALENCE (X(1,1),CFF(1,1))
      DATA OPT2 /' P-P  ',' P-SV ',' P-SL '/
      OPTION(1)=PROGNM
      OPTION(2)=OPT2(INR)
      I=MAX(1,INR)
      if (transmit) then
       PTIT='TRANSMISSION COEFFICIENT'
      else
       PTIT='REFLECTION COEFFICIENT'
      end if
      NLAB=1
      if (slowrc) then
       WRITE(LAB,800) ANGLE
 800   FORMAT('P:',F6.3,' km/s$')
      else
       WRITE(LAB,801) ANGLE
 801   FORMAT('ANGLE:',F5.1,' deg$')
      end if
      XTXT='Frequency (Hz)$'
      YTXT='Phase angle (deg)$'
      YDOWN=-180.
      YUP=180.
      YINC=90.
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
      REWIND(31)
      DO 1350 J=1,LF     
      READ(31) (X(JJ,3),JJ=1,NWVNO)
      X(J,1)=X(IANGLE,3)       
 1350 CONTINUE  
      CALL PLTWRI(LF,RMIN,RSTEP,0.,0.,X(1,1),1,X(1,1),1)
      RETURN
      END

