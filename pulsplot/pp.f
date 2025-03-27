******************************************************************************
*                                                                            *
*     Datafile parameters:                                                   *
*        PROGNM  : Indicates the model used to create the data file.         *
*        TNORM   : Indicate calculated normal stress field.                  *
*        TVERT   : Indicate calculated vertical particle velocity field.     *
*        THORI   : Indicate calculated horizontal particle velocity field.   *
*        TITLE   : Title of run                                              *
*        SIGNN   : Indicates the imaginary SIGN used in transformation       *
*        FREQS   : Source centre frequency in Hz                             *
*        SD      : Source depth (mean of array) in m                         *
*        RD      : Depth of first receiver in m                              *
*        RDLOW   : Depth of last receiver in m                               *
*        IR      : Number of receivers                                       *
*        NX      : Number of time samples                                    *
*        LX      : Lower frequency number                                    *
*        MX      : Upper frequency number                                    *
*        DT      : Time sampling increment in s                              *
*        R0      : First receiver range in km                                *
*        RSPACE  : Receiver range increment in km                            *
*        NPLOTS  : Number of ranges                                          *
*        ICDR    : Indicates plane geometry or not (1/0)                     *
*                                                                            *
******************************************************************************
      PROGRAM PP
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'compul.f'
      INCLUDE 'complo.f'
      DIMENSION X(NP2,npar)
      EQUIVALENCE (X(1,1),CFF(1,1))
      DIMENSION UDSP(NP)
      EQUIVALENCE (UDSP(1),CFF(1,1))

      DIMENSION PX(MODULO)

      DIMENSION SCALEFACT(3),FCTORD(3),SCFACD(3),DSCFACT(3),IFLGD(3),
     1 FCTORR(3),SCFACR(3),RSCFACT(3),SCFACA(3),ASCFACT(3),IFLGR(3)

      CHARACTER*300 FILENUSP,OLDNUSP,FUSTMP
      character*30 undfil,shdfil,shdver,shdhor
      CHARACTER LETTER,cform,c_dummy
      CHARACTER PLOTTYPE
      CHARACTER*20 TITLEA,TITLEB
      CHARACTER*300 DUMMY,ascnam
      CHARACTER*120 COMMAND
      LOGICAL NEWFIL,shading,makeplot
      CHARACTER*3 TRACEFORM,CON_PACKGE,PLP_PACKGE
      DATA TRACEFORM /'ASC'/
c EKS:
      INTEGER PADFLAG,REM
      real matdum
      dimension matdum(NRD)
c max number of dimensions expected for matlab data variables   
      dimension IDIM(10)

*     Initiate some variables
      PLPFILE=.FALSE.
      CDRFILE=.FALSE.
      NEWFIL=.TRUE.
      OMEGIM=0E0
      FILENAME=' '
      OLDFILE=' '
      NEWNAME=' '
      FILEN=' '
      FILENUSP=' '
      undfil=' '
      shdhor=' '
      shdver=' '
      OLDNUSP=' '
      FUSTMP=' '
      TITLEC=' '
      DUMMY=' '
      LETTER='N'
      PLOTTYPE='N'
      decstr='N'
      logstr='N'
      PLOTOPT='CPX'
      CONTOPT='CPX'
      DYTXT='Depth (m)'
      RYTXT='Range (km)'
      AYTXT='Azimuth (deg)'
      PI=4.0*ATAN(1.0)
      CNUL=CMPLX(0.0,0.0)
      AI=CMPLX(0.0,1.0)
      ITSYP=6
      ISTMP=4
      isact=1
      isrow=1
      C0=0.0
      DC0=0.0
      RC0=0.0
      AC0=0.0
      DAMPMAX=0.0
      RAMPMAX=0.0
      AAMPMAX=0.0
      DO 11 I=1,3
      SCALEFACT(I)=1.0
      dbstack=60.0
      FCTORD(I)=0.0
      FCTORR(I)=0.0
      SCFACD(I)=0.0
      SCFACR(I)=0.0
      SCFACA(I)=0.0
      DSCFACT(I)=1.0
      RSCFACT(I)=1.0
      ASCFACT(I)=1.0
      IFLGD(I)=0
   11 IFLGR(I)=0
      IDEPTH=0
      IRANGE=0
      INSTYP=-1
      INIUSP=-1
      nazim=18
      nazim_tl=90
      XAXIS=20.0
      YAXIS=12.0
      YRAXISS=15.0
      YDAXISS=15.0
      YAAXISS=15.0
      YDINC=0E0
      YRINC=0E0
      YAINC=0E0
      XTRIGHT=0.0
      XDRIGHT=0.0
      XRRIGHT=0.0
      XARIGHT=0.0
      XIRIGHT=0.0
      XSARIGHT=0.0
      XCRIGHT=0.0
      XCAXIS=20.0
      YCAXIS=12.0
      XFAXIS=20.0
      SAVEFILE='N'
      ZMAX=-99.
      ZCMAX=ZMAX
      KSNAPSH1=0
      NORMK=-99
      NUMINT=21
      FMINOLD=0.0
      FMAXOLD=0.0
      IPLCNT=1
      tzmin=30.
      tzmax=90.
      tzinc=3.
      tymin=20.
      tymax=90.
      tyinc=10.
      tyaxis=12.
      undfpt=.false.
c
c Check for MTV option
c
      call getenv('CON_PACKGE',CON_PACKGE)
      mtvout = (CON_PACKGE.eq.'MTV'.or.CON_PACKGE.eq.'mtv')
      call getenv('PLP_PACKGE',PLP_PACKGE)  
      mtvplp = (PLP_PACKGE.eq.'MTV'.or.PLP_PACKGE.eq.'mtv')

*     Initialize FMS and open the form library
c
c >>> check for parameters from last time in sam directory
c
      open(89,file='pp.par',status='old',form='formatted',err=2789)
      read(89,'(a)',end=2790,err=2790) filename
      read(89,'(i5)',end=2790,err=2790) instyp
      read(89,'(a)',end=2790,err=2790) filenusp
      read(89,'(a)',end=2790,err=2790) plotopt
      read(89,'(a)',end=2790,err=2790) contopt
      read(89,'(a)',end=2790,err=2790) traceform
      read(89,'(i5)',end=2790,err=2790) nazim
      read(89,'(i5)',end=2790,err=2790) nazim_tl
      read(89,'(f15.6)',end=2790,err=2790) prange
      read(89,'(f15.6)',end=2790,err=2790) pdepth
      read(89,'(f15.6)',end=2790,err=2790) tzmin
      read(89,'(f15.6)',end=2790,err=2790) tzmax
      read(89,'(f15.6)',end=2790,err=2790) tzinc
      read(89,'(f15.6)',end=2790,err=2790) xcaxis
      read(89,'(f15.6)',end=2790,err=2790) ycaxis
      read(89,'(f15.6)',end=2790,err=2790) tymin
      read(89,'(f15.6)',end=2790,err=2790) tymax
      read(89,'(f15.6)',end=2790,err=2790) tyinc
      read(89,'(a)',end=2790,err=2790) decstr
      read(89,'(f15.6)',end=2790,err=2790) tshmin
      read(89,'(f15.6)',end=2790,err=2790) tshmax
      read(89,'(i5)',end=2790,err=2790) nframe
      read(89,'(a)',end=2790,err=2790) shdver
      read(89,'(a)',end=2790,err=2790) shdhor
      read(89,'(a)',end=2790,err=2790) logstr

      go to 2790
 2789 open(89,file='pp.par',status='unknown',form='formatted')
 2790 rewind(89)

3000  CONTINUE

      TSHIFT=1.0
      TSHTMP=0.0
      ITMP=-1
C-------------------------------------------------------------------

*     The main menu

C-------------------------------------------------------------------
      IF(INSTYP.NE.-1) THEN
        ISTYP=INSTYP
        FREQS=FQTMP
        frqdec=freqs
c        FREQS=FCTRF
      END IF

      CALL MENCRT('OASES PULSE POST-PROCESSOR',22)
      CALL TXTFLD(1,'File name:',FILENAME)
      CALL INTFLD(2,'Source type:',ISTYP,-1,6)
      CALL TXTFLD(3,'Source file:',FILENUSP)
      CALL FLTFLD(4,'Min frequency:',FMIN,' Hz')
      CALL FLTFLD(5,'Max frequency:',FMAX,' Hz')
      CALL FLTFLD(6,'Cen frequency:',FREQS,' Hz')
      call intfld(7,'Signal Plane:',isact,1,isrow)
      CALL TXTFLD(8,'Plot options:',PLOTOPT)
      CALL TXTFLD(9,'Contour options:',CONTOPT)
      CALL ACTFLD(10,'Depth stacked:',' ') 
      CALL ACTFLD(11,'Range stacked:',' ') 
      CALL ACTFLD(12,'Azimuth stacked:',' ') 
      CALL ACTFLD(13,'Individual:',' ') 
      call actfld(14,'Transmission Loss:',' ')
      CALL ACTFLD(15,'Snap shots:',' ') 
      CALL ACTFLD(16,'Source pulses:',' ') 
      call chafld(17,'Demodulation:',decstr,'Y/N')
      call chafld(18,'Log traces:',logstr,'Y/N')
      call chafld(19,'Save plot files:',savefile,'Y/N')
      call chafld(20,'Trace format:',traceform(1:1)
     +      ,'Asc/Bin/Cfs/Gld/Mat/Sdr')
      call actfld(21,'Add TRF files:',' ')
c      call actfld(21,'Multiply TRF files:',' ')
      CALL ACTFLD(22,'Exit PP:',' ') 

1110  CALL SHMENU(IRET)
      IF (IRET.EQ.1) THEN
       CALL CHKFILE(ISTAT)
       GO TO 1110
      ELSE IF (IRET.EQ.4) THEN
       CALL CHKFMIN(ISTAT)
       GO TO 1110
      ELSE IF (IRET.EQ.5) THEN
       CALL CHKFMAX(ISTAT)
       GO TO 1110
      ELSE IF (IRET.EQ.10) THEN
       TPLOT='D'       
      ELSE IF (IRET.EQ.11) THEN
       TPLOT='R'       
      ELSE IF (IRET.EQ.12) THEN
       TPLOT='A'       
      ELSE IF (IRET.EQ.13) THEN
       TPLOT='I'
      ELSE IF (IRET.EQ.14) THEN
       TPLOT='T'       
      ELSE IF (IRET.EQ.15) THEN
       TPLOT='C'    
      ELSE IF (IRET.EQ.16) THEN
       TPLOT='S'    
      else if (iret.eq.21) then   
       call addtrf()
       go to 1110
c      else if (iret.eq.21) then   
c       call multrf()
c       go to 1110
      ELSE IF (IRET.EQ.22) THEN
       GO TO 3010      
      ELSE
       GO TO 1110
      END IF

      CALL GETTXT(1,FILENAME)
      CALL CHKFILE(ISTAT)
      IF (ISTAT.NE.0) GO TO 3000

      CALL GETINT(2,ISTYP)
      INSTYP=ISTYP

      IF (ISTYP.EQ.0) THEN
       CALL GETTXT(3,FILENUSP)
C       CALL CHKPULSE(ISTAT)
       IF (ISTAT.NE.0) GO TO 3000
      END IF

      CALL GETFLT(4,FMIN)
      CALL CHKFMIN(ISTAT)
      IF (ISTAT.NE.0) GO TO 3000

      CALL GETFLT(5,FMAX)
      CALL CHKFMAX(ISTAT)
      IF (ISTAT.NE.0) GO TO 3000

      CALL GETFLT(6,FREQS)
C      CALL CHKFREQS(ISTAT)
      IF (ISTAT.NE.0) GO TO 3000
      FQTMP=FREQS
c >>> deconvolution frequency equal to center frequency
      frqdec=freqs

      call getint(7,isact)
c >>> Force read if new signal plane
      if (isact.ne.isold) then
       OLDFILE=' '
      end if


      CALL GETTXT(8,PLOTOPT)
      CALL GETTXT(9,CONTOPT)

      if (.not.mtvout) then
c Check for mtv in contour options
       do ii=1,len(contopt)-2
        if (contopt(ii:ii+2).eq.'MTV'
     &      .or.contopt(ii:ii+2).eq.'mtv') then
         mtvout=.true.
         go to 7555
        end if
       end do
 7555  continue
      end if

      if (.not.mtvplp) then
c Check for mtv in mplot options
       do ii=1,len(plotopt)-2
        if (plotopt(ii:ii+2).eq.'MTV'
     &      .or.plotopt(ii:ii+2).eq.'mtv') then
         mtvplp=.true.
         go to 7666
        end if
       end do
 7666  continue
      end if

c Check for prx in contour options for no interactive plots
       makeplot=.true.
       do ii=1,len(contopt)-2
        if (contopt(ii:ii+2).eq.'PRX'
     &      .or.contopt(ii:ii+2).eq.'prx') then
         makeplot=.false.
         go to 7556
        end if
       end do
 7556  continue

c >>> deconvolution
      call getcha(17,decstr)
      deconv=(decstr.eq.'Y')
c >>> Log trace plots
      call getcha(18,logstr)
      if (deconv) then
       log_ts=(logstr.eq.'Y')
      else
       write(6,*) '>>> Log traces only allowed for deconvolved signals!'
       log_ts=.false.
       logstr='N'
      end if
c >>> save plot files?
      call getcha(19,savefile)
c >>> Trace file format
      call getcha(20,cform)
      if (cform.eq.'A') then
       traceform = 'ASC'
      else if (cform.eq.'B') then
       traceform = 'BIN'
      else if (cform.eq.'C') then
       traceform = 'CFS'
      else if (cform.eq.'G') then
       traceform = 'GLD'
      else if (cform.eq.'M') then
       traceform = 'MAT'
      else if (cform.eq.'S') then
       traceform = 'SDR'
      else
       write(6,'(a,a,a)') '>>> ',cform,
     &                    ' - Unknown trace file format <<<'  
       write(6,'(a)') '>>>     Using ASCII (*.asc)           <<<'
       traceform = 'ASC'
      end if
c      Transmission loss plots
      IF (TPLOT.EQ.'T') THEN
        CALL MENCRT('TRANSMISSION LOSS',7)
        CALL ACTFLD(1,'TL vs Freq:',' ') 
        CALL ACTFLD(2,'TL vs Range:',' ') 
        CALL ACTFLD(3,'TL vs Depth:',' ') 
        CALL ACTFLD(4,'Depth Average:',' ') 
        CALL ACTFLD(5,'Depth-Range Contours:',' ') 
        CALL ACTFLD(6,'Range-Range Contours:',' ') 
        CALL ACTFLD(7,'Return:','PP main menu')

 1011   CALL SHMENU(IRET)
        if (iret.eq.1) then
         if (lx.eq.mx) then
          write(6,*) '>>> No frequency plot: lx = mx <<<'
          go to 3000
         end if
         tplot='f'
        else if (iret.eq.2) then
         tplot='r'
        else if (iret.eq.3) then
         if (ir.le.1) then
          write(6,*) '>>> No depth plot: IR le 1 <<<'
          go to 3000
         end if
         tplot='d'
        else if (iret.eq.4) then
         if (ir.le.1) then
          write(6,*) '>>> No depth average: IR le 1 <<<'
          go to 3000
         end if
         tplot='a'
        else if (iret.eq.5) then
         if (ir.le.1) then
          write(6,*) '>>> No vertical contours: IR le 1 <<<'
          go to 3000
         end if
         tplot='c'
        else if (iret.eq.6) then
         tplot='h'
        else
         go to 3000
        end if
        
      end if
c     Snap shots
      IF (TPLOT.EQ.'C') THEN
        if (lx.eq.mx) then
         write(6,*) '>>> Snap shots require multible frequencies <<<'
         go to 3000
        end if

        CALL MENCRT('PULSE SNAP-SHOTS',3)
        CALL ACTFLD(1,'Depth-Range:','Ver. plane') 
        CALL ACTFLD(2,'Range-Range:','Hor. plane ') 
        CALL ACTFLD(3,'Return:','PP main menu')

        CALL SHMENU(IRET)
        if (iret.eq.1) then
         if (ir.le.1) then
          write(6,*) '>>> No vertical contours: IR le 1 <<<'
          go to 3000
         end if
         tplot='C'
        else if (iret.eq.2) then
         tplot='H'
        else
         go to 3000
        end if
      end if
c
c >>> write selected parameters to file
c
      write(6,*) '>>> Writing parameter file <<<'
      rewind(89)
      write(89,'(a)') filename
      write(89,'(i5)') instyp
      write(89,'(a)') filenusp
      write(89,'(a)') plotopt
      write(89,'(a)') contopt
      write(89,'(a)') traceform
      write(89,'(i5)') nazim
      write(89,'(i5)') nazim_tl
      write(89,'(f15.6)') prange
      write(89,'(f15.6)') pdepth
      write(89,'(f15.6)') tzmin
      write(89,'(f15.6)') tzmax
      write(89,'(f15.6)') tzinc
      write(89,'(f15.6)') xcaxis
      write(89,'(f15.6)') ycaxis
      write(89,'(f15.6)') tymin
      write(89,'(f15.6)') tymax
      write(89,'(f15.6)') tyinc
      write(89,'(a)') decstr
      write(89,'(f15.6)') tshmin
      write(89,'(f15.6)') tshmax
      write(89,'(i5)') nframe
      write(89,'(a)') shdver
      write(89,'(a)') shdhor
      write(89,'(a)') logstr

      IF(FILENUSP.NE.OLDNUSP) THEN
        OLDNUSP=FILENUSP
        IF(INIUSP.GT.0) CLOSE(66,ERR=551)
 551    INIUSP=0
      ELSE
        INIUSP=1
      END IF

      ISTMP=ISTYP
      FUSTMP=FILENUSP
C
*     Read data from the chosen data file and place result in the 31-buffer
      IF(FILENAME.NE.OLDFILE.OR.FMIN.NE.FMINOLD.OR.FMAX.NE.FMAXOLD) THEN

        WRITE(6,*)
        WRITE(6,*)'   ***  Reading from data file  ***'

        NEWFIL=(OLDFILE.NE.FILENAME)
        if (newfil) then
         itmp=-1
         ksnapsh1=0
         zmax=-99.
         zcmax=zmax
         NCORMK=-99
        end if
        IF (OLDFILE.NE.' ') CALL CLSBUF(31)
C********************************************
        CALL READTRF()
        write(6,*) ' >>>> file read <<<'
C*********************************************************
        OLDFILE=FILENAME
        IF(IR.GT.1) THEN
          RDINC=(RDLOW-RD)/(IR-1)
        ELSE
          RDINC=1.0
        END IF
        FMINOLD=FMIN
        FMAXOLD=FMAX
        isold=isact

        DO 998 II=45,1,-1
        IJ1=II
        IF(FILENAME(II:II).EQ.' ') GO TO 998
        IJ1=IJ1-4
        GO TO 997
  998   CONTINUE
  997   IJ=0
        FILEN=' '
        DO 995 II=IJ1,1,-1
        IJ=IJ+1
        IJ2=II
c        IF ((FILENAME(II:II).NE.']').AND.(FILENAME(II:II).NE.'\')
c     &     .AND.(FILENAME(II:II).NE.'/')) GO TO 995
        if (FILENAME(II:II).NE.'/') GO TO 995
        IJ=IJ-1
        IJ2=II+1
        GO TO 994
  995   CONTINUE
  994   FILEN(1:IJ)=FILENAME(IJ2:IJ1)
        LFILNM=MIN(IJ,40)
        IJ=IJ+1
        PLOTTYPE=PCHOICE(1)
      ENDIF
C
*     File FOR066 is created for user defined source pulse.
      IF(ISTYP.EQ.0) THEN
        write(6,*) '>>> User-defined source pulse <<<',iniusp
        IF(INIUSP.LT.1) THEN
          OPEN(65,FILE=FILENUSP,STATUS='OLD',ERR=413)
          CALL INTERPOL(UDSP,NX,DT)
          OPEN(66,STATUS='SCRATCH',FORM='FORMATTED')
          DO 410 II=1,NX
  410     WRITE(66,*)UDSP(II)
          close(65)
          INIUSP=1
          go to 414
  413     CONTINUE
          write(6,*) 'Source file does not exist:',FILENUSP
          goto 1110
 414      continue
        END IF
      END IF
C
c        write(6,*) ' >>>> check for plot type <<<'

      IF (TPLOT.NE.'S') THEN
*     Default plot values
      write(6,*) '>>> checking options <<<'
        IF    (TPLOT.NE.'C'.and.tplot.ne.'c'.and.tplot.ne.'r'
     &    .and.tplot.ne.'d'.and.tplot.ne.'f'.and.tplot.ne.'a'
     &    .and.tplot.ne.'h'.and.tplot.ne.'H') THEN
          IF(TPLOT.EQ.'D'.AND.XDRIGHT.NE.0.0) GO TO 2500
          IF(TPLOT.EQ.'R'.AND.XRRIGHT.NE.0.0) GO TO 2500
          IF(TPLOT.EQ.'A'.AND.XRRIGHT.NE.0.0) GO TO 2500
          IF(TPLOT.EQ.'I'.AND.XIRIGHT.NE.0.0) GO TO 2500
          YAXISS=15.0
          IF(XTRIGHT.EQ.0.0) THEN
            XLEFT=0.0
            XRIGHT=REAL(NX*DT)
            XAXIS=20.0

*           Find nice format of XRIGHT
            IF (XRIGHT.LT.1.0) THEN
              XTMP=1.0/XRIGHT
              K=0
2201          IF (INT(XTMP/10.0).NE.0) THEN
                XTMP=XTMP/10.0
                K=K+1
                GO TO 2201
              ENDIF
              K=K+1
            ELSE
              XTMP=XRIGHT
              K=0
2211          IF (INT(XTMP/10.0).NE.0) THEN
                XTMP=XTMP/10.0
                K=K+1
                GO TO 2211
              ENDIF
              K=-K
            ENDIF

            XRIGHT=INT(XRIGHT*(10.0**K))*(10.0**(-K))
            XINC=REAL(INT((XRIGHT-XLEFT)*10))/50.0
            IF(XINC.EQ.0.0) XINC=ABS(XRIGHT-XLEFT)/4.
          ELSE
            XLEFT=XTLEFT
            XRIGHT=XTRIGHT
            XAXIS=XTAXIS
            XINC=XTINC
          END IF
          XSALEFT=0E0
          XSARIGHT=ABS(XRIGHT-XLEFT)
          XSAINC=XINC
          XSAAXIS=XAXIS
          YSAAXIS=YAXIS
          XSBAXIS=XAXIS
          YSBAXIS=YAXIS
          C0STMP=0
   
          IF (TPLOT.EQ.'R') THEN
           IF (YRINC.EQ.0E0.OR.NEWFIL) THEN
            if (inttyp.eq.-1) then
             RYTXT='Slowness (s/km)'
            else
             RYTXT='Range (km)'
            end if
c            YRUP=MAX(R0,R0+(NPLOTS-1)*RSPACE)+ABS(RSPACE)
c            YRDOWN=MIN(R0,R0+(NPLOTS-1)*RSPACE)-ABS(RSPACE)
c            YRINC=(NPLOTS-1)*ABS(RSPACE)/5.0
            rtmp=r0+(nplots-1)*rspace
            rt1=amin1(r0,rtmp)-abs(rspace)
            rt2=amax1(r0,rtmp)+abs(rspace)
            call AUTOAX(rt1,rt2,yrdown,yrup,yrINC,yDIV,NyDIF)
           END IF
            RSCALE=YRAXISS/ABS(YRUP-YRDOWN)
            IF(IR.GT.1) THEN
              DSCALE=YRAXISS/ABS(RDLOW-RD+2.*RDINC)
              DRRATIO=DSCALE/RSCALE
            ELSE
              DRRATIO=1.0/1000.0
            END IF
            IDEPTH=IR
            pdepth=rdlow
          ELSE IF (TPLOT.EQ.'D') THEN
             IF (YDINC.EQ.0E0.OR.NEWFIL) THEN
c              YDUP=min(RD,RDLOW)-ABS(RDINC)
c              YDDOWN=MAX(RD,RDLOW)+ABS(RDINC)
c              YDINC=ABS(YDUP-YDDOWN)/5.0
              rt1=min(rd,rdlow)-abs(rdinc)
              rt2=max(rd,rdlow)+abs(rdinc)
              call AUTOAX(rt1,rt2,ydup,yddown,ydINC,yDIV,NyDIF)
             END IF
              DSCALE=YDAXISS/ABS(YdUP-YdDOWN)
              IRANGE=NPLOTS
              prange=r0+(nplots-1)*rspace
              pdepth=rdlow
          ELSE IF (TPLOT.EQ.'A') THEN
             IF (YAINC.EQ.0E0.OR.NEWFIL) THEN
              rt1=-30
              rt2=360
              call AUTOAX(rt1,rt2,yadown,yaup,yaINC,yDIV,NyDIF)
             END IF
              ASCALE=YAAXISS/ABS(YAUP-YADOWN)
              IRANGE=NPLOTS
              IDEPTH=IR
              prange=r0+(nplots-1)*rspace
              pdepth=rdlow
          ELSE
              YAXIS=12.0
              IDEPTH=IR
              IRANGE=NPLOTS
              prange=r0+(nplots-1)*rspace
              pdepth=rdlow
              XFLEFT=FMIN
              XFRIGHT=FMAX
              XFINC=(XFRIGHT-XFLEFT)/4.0
              XFAXIS=15.0
              YFAXIS=12.0
          ENDIF
        ELSE IF (TPLOT.eq.'f') then
          IF(XFRIGHT.NE.0.0.and.freqs.eq.freq_old) GO TO 2530
            rt1=(lx-1)*domega/(2*pi)
            rt2=(mx-1)*domega/(2*pi)
            call AUTOAX(rt1,rt2,xfleft,xfright,xfINC,xDIV,NxDIF)
            pdepth=rdlow
            prange=r0+(nplots-1)*rspace
            freq_old=freqs
            freq=freqs
        ELSE IF (TPLOT.eq.'r') then
          IF(XCRIGHT.NE.0.0.and.freqs.eq.freq_old) GO TO 2530
            rtmp=r0+(nplots-1)*rspace
            rt1=amin1(r0,rtmp)
            rt2=amax1(r0,rtmp)
c            write(6,*) ' calling autoax'
            call AUTOAX(rt1,rt2,xcleft,xcright,xcINC,xDIV,NxDIF)
            pdepth=rdlow
            freq_old=freqs
            freq=freqs
        ELSE IF (TPLOT.eq.'a') then
          IF(XCRIGHT.NE.0.0.and.freqs.eq.freq_old) GO TO 2530
            rtmp=r0+(nplots-1)*rspace
            rt1=amin1(r0,rtmp)
            rt2=amax1(r0,rtmp)
            call AUTOAX(rt1,rt2,xcleft,xcright,xcINC,xDIV,NxDIF)
            freq_old=freqs
            freq=freqs
        ELSE IF (TPLOT.eq.'d') then
          IF(YCDOWN.NE.0.0.and.freqs.eq.freq_old) GO TO 2530
            prange=r0+(nplots-1)*rspace
            rt1=min(rd,rdlow)
            rt2=max(rd,rdlow)
            call AUTOAX(rt1,rt2,ycup,ycdown,ycINC,yDIV,NyDIF)
            freq_old=freqs
            freq=freqs
        ELSE IF (TPLOT.eq.'c'.or.TPLOT.eq.'C') then
          write(6,*) '>>> before autoax'
          IF(XCRIGHT.NE.0.0.and.YCDOWN.ne.0.0
     &       .and.freqs.eq.freq_old) GO TO 2530
            rtmp=r0+(nplots-1)*rspace
            rt2=amax1(r0,rtmp)
            if (msuft.gt.1) then
             rt1=-rt2
            else
             rt1=amin1(r0,rtmp)
            end if
            write(6,*) ' calling autoax',rt1,rt2

            call AUTOAX(rt1,rt2,xcleft,xcright,xcINC,xDIV,NxDIF)
            rt1=min(rd,rdlow)
            rt2=max(rd,rdlow)

            write(6,*) ' calling autoax',rt1,rt2

            call AUTOAX(rt1,rt2,ycup,ycdown,ycINC,yDIV,NyDIF)
            write(6,*) ' after autoax'
      
            freq_old=freqs
            freq=freqs
        ELSE IF (TPLOT.eq.'h'.or.tplot.eq.'H') then
c >>> TL or snap-shotin horizontal plane
          IF(XCRIGHT.NE.0.0.and.YCDOWN.ne.0.0
     &       .and.freqs.eq.freq_old) GO TO 2530
            rtmp=r0+(nplots-1)*rspace
            rt2=.71*amax1(r0,rtmp)
            call AUTOAX(0e0,rt2,xcdum,xcright,xcINC,xDIV,NxDIF)
            freq_old=freqs
            freq=freqs
        ELSE
          IF(XCRIGHT.NE.0.0) GO TO 2530
            rtmp=r0+(nplots-1)*rspace
            rt1=amin1(r0,rtmp)
            rt2=amax1(r0,rtmp)
            call AUTOAX(rt1,rt2,xleft,xright,xINC,xDIV,NxDIF)
            rt1=min(rd,rdlow)
            rt2=max(rd,rdlow)
            call AUTOAX(rt1,rt2,yup,ydown,yINC,yDIV,NyDIF)
        ENDIF
      ELSE
        IF(XSARIGHT.NE.0.0) GO TO 2540
        XALEFT=0.0
        XARIGHT=0.2
        XAINC=0.05
        XAAXIS=20.0
        YAAXIS=12.0
        C0TMP=0.0
        XBAXIS=20.0
        YBAXIS=12.0
      ENDIF
      GO TO 3019

 2500 XLEFT=XTLEFT
      XRIGHT=XTRIGHT
      XINC=XTINC
      XAXIS=XTAXIS
      IF(TPLOT.EQ.'D') THEN
c
c >>> Depth stacked
c
       YDOWN=YDDOWN
       YUP=YDUP
       YINC=YDINC
       YAXISS=YDAXISS
       DO 12 I=1,3
   12  SCALEFACT(I)=DSCFACT(I)
       IRANGE=IDRANGE
      else if (tplot.eq.'R') then
c
c >>> Range stacked
c
       YDOWN=YRDOWN
       YUP=YRUP
       YINC=YRINC
       YAXISS=YRAXISS
       DO 13 I=1,3
   13  SCALEFACT(I)=RSCFACT(I)
       IDEPTH=IRDEPTH
      else if (tplot.eq.'A') then
c
c >>> Azimuth stacked
c
       YDOWN=YADOWN
       YUP=YAUP
       YINC=YAINC
       YAXISS=YAAXISS
       DO 14 I=1,3
   14  SCALEFACT(I)=ASCFACT(I)
       IDEPTH=IADEPTH
       IRANGE=IARANGE
      else
c
c >>> Individual
c
       IRANGE=IIRANGE
       IDEPTH=IIDEPTH
      end if
      GO TO 3019
c
c >>> snap shots
c
 2530 XLEFT=XCLEFT
      XRIGHT=XCRIGHT
      XINC=XCINC
      YDOWN=YCDOWN
      YUP=YCUP
      YINC=YCINC
      XAXIS=XCAXIS
      YAXIS=YCAXIS
      TSHIFT=TCSHIFT
      ZMIN=ZCMIN
      ZMAX=ZCMAX
      NORMK=NCORMK
      NUMINT=NUMCINT
c >>> TL contours
      freq=freqs
      GO TO 3019

 2540 XALEFT=XSALEFT
      XARIGHT=XSARIGHT
      XAINC=XSAINC
      XAAXIS=XSAAXIS
      YAAXIS=YSAAXIS
      C0TMP=C0STMP
      XBAXIS=XSBAXIS
      YBAXIS=YSBAXIS

 3019 IFLAG=0                  
c            write(6,*) ' at 3019'
 3020 CONTINUE 

        DUMMY=PCHOICE(1)
        DO 801 JJ=2,NOUT
 801     DUMMY=DUMMY(1:(JJ-1)*2-1)//' '//PCHOICE(JJ)
*     Initiate the CFFS-table
c      write(6,*) 'calling pulse',freqs,dt,nx
      IF(TPLOT.NE.'S'.and.mx.gt.lx) CALL PULSE(FREQS,DT,NX)

c            write(6,*) ' making submenues'

C-------------------------------------------------------------------

*     Read submenu parameters

C-------------------------------------------------------------------
      IF (TPLOT.EQ.'D') THEN
*     Depth stacked plots
    
        CALL MENCRT('DEPTH STACKED PLOTS',18)
        CALL TXTFLD(1,'Plot title:',TITLE)
        CALL FLTFLD(2,'Min time:',XLEFT,'s')
        CALL FLTFLD(3,'Max time:',XRIGHT,'s')
        CALL FLTFLD(4,'Time tick inc:',XINC,'s')
        CALL FLTFLD(5,'t-axis length:',XAXIS,'cm')
        CALL FLTFLD(6,'Depth down:',YDDOWN,'m')
        CALL FLTFLD(7,'Depth up:',YDUP,'m')
        CALL FLTFLD(8,'Depth tick inc:',YDINC,'m')
        CALL FLTFLD(9,'d-axis length:',YDAXISS,'cm')
        CALL TXTFLD(10,'d-axis label:',DYTXT)
        CALL FLTFLD(11,'Red. velocity:',DC0,'m/s')
        if (inttyp.eq.-1) then
         CALL fltFLD(12,'Slowness:',prange,'s/km')
        else
         CALL fltFLD(12,'Range:',prange,'km')
        end if
        call FLTFLD(13,'Azimuth:',THETAD,'deg')
        CALL FLTFLD(14,'Scale factor:',DSCFACT(1),' ')
        CALL CHAFLD(15,'Parameter:',PLOTTYPE,DUMMY(1:2*NOUT-1))
        CALL ACTFLD(16,'Generate plot:',' ')
        CALL ACTFLD(17,'Trace file:',' ')
        CALL ACTFLD(18,'Return:','PP main menu')

 1019   CALL SHMENU(IRET)

        IF (IRET.EQ.18) THEN
         GO TO 3000      
        ELSE IF (IRET.LT.16) THEN
         GO TO 1019
        ELSE
        END IF

        CALL GETTXT(1,TITLE)

        CALL GETFLT(2,XLEFT)
        XTLEFT=XLEFT

        CALL GETFLT(3,XRIGHT)
        XTRIGHT=XRIGHT
        XDRIGHT=XRIGHT

        CALL GETFLT(4,XINC)
        XTINC=XINC

        CALL GETFLT(5,XAXIS)
        XTAXIS=XAXIS

        CALL GETFLT(6,YDOWN)
        YDDOWN=YDOWN

        CALL GETFLT(7,YUP)
        YDUP=YUP

        CALL GETFLT(8,YINC)
        YDINC=YINC

        CALL GETFLT(9,YAXISS)
        YDAXISS=YAXISS

        CALL GETTXT(10,DYTXT)

        CALL GETFLT(11,C0)
        DC0=C0

        CALL GETflt(12,prange)
        dif0=1e20
        do jran=1,nplots
         rang=r0+(jran-1)*rspace
         dif=abs(prange-rang)
         if (dif.lt.dif0) then
          irange=jran
          dif0=dif
         end if
        end do
        prange=r0+(irange-1)*rspace

c        CALL GETINT(12,IRANGE)
        IDRANGE=IRANGE

        CALL GETFLT(13,THETAD)
        THETA=THETAD*PI/180.0

        CALL GETFLT(14,SCALEFACT(1))
        CALL GETFLT(14,SCALEFACT(2))
        CALL GETFLT(14,SCALEFACT(3))
        DSCFACT(1)=SCALEFACT(1)
        DSCFACT(2)=SCALEFACT(2)
        DSCFACT(3)=SCALEFACT(3)

        CALL GETCHA(15,PLOTTYPE)

        IF (IRANGE.EQ.0) THEN
          ILOW=1
          IHIGH=NPLOTS
        ELSE
          ILOW=IRANGE
          IHIGH=IRANGE
        END IF
      ELSE IF (TPLOT.EQ.'A') THEN
*      Azimuth stacked plots
    
        CALL MENCRT('AZIMUTH STACKED PLOTS',18)
        CALL TXTFLD(1,'Plot title:',TITLE)
        CALL FLTFLD(2,'Min time:',XLEFT,'s')
        CALL FLTFLD(3,'Max time:',XRIGHT,'s')
        CALL FLTFLD(4,'Time tick inc:',XINC,'s')
        CALL FLTFLD(5,'t-axis length:',XAXIS,'cm')
        CALL FLTFLD(6,'Azimuth down:',YADOWN,'deg')
        CALL FLTFLD(7,'Azimuth up:',YAUP,'deg')
        CALL FLTFLD(8,'Azim. tick inc:',YAINC,'deg')
        CALL FLTFLD(9,'a-axis length:',YAAXISS,'cm')
        CALL FLTFLD(10,'Red. velocity:',AC0,'m/s')
        if (inttyp.eq.-1) then
         CALL fltFLD(11,'Slowness:',PRANGE,'s/km')
        else
         CALL fltFLD(11,'Range:',PRANGE,'km')
        end if
        call fltfld(12,'Depth:',PDEPTH,'m')
        call intfld(13,'No. of traces:',nazim,1,999)
        CALL FLTFLD(14,'Scale factor:',ASCFACT(1),' ')
        CALL CHAFLD(15,'Parameter:',PLOTTYPE,DUMMY(1:2*NOUT-1))
        CALL ACTFLD(16,'Generate plot:',' ')
        CALL ACTFLD(17,'Trace file:',' ')
        CALL ACTFLD(18,'Return:','PP main menu')

 1029   CALL SHMENU(IRET)

        IF (IRET.EQ.18) THEN
         GO TO 3000      
        ELSE IF (IRET.LT.16) THEN
         GO TO 1029
        ELSE
        END IF

        CALL GETTXT(1,TITLE)

        CALL GETFLT(2,XLEFT)
        XTLEFT=XLEFT

        CALL GETFLT(3,XRIGHT)
        XTRIGHT=XRIGHT
        XDRIGHT=XRIGHT

        CALL GETFLT(4,XINC)
        XTINC=XINC

        CALL GETFLT(5,XAXIS)
        XTAXIS=XAXIS

        CALL GETFLT(6,YDOWN)
        YADOWN=YDOWN

        CALL GETFLT(7,YUP)
        YAUP=YUP

        CALL GETFLT(8,YINC)
        YAINC=YINC

        CALL GETFLT(9,YAXISS)
        YAAXISS=YAXISS

        CALL GETFLT(10,C0)
        AC0=C0

        CALL GETflt(11,prange)
        dif0=1e20
        do jran=1,nplots
         rang=r0+(jran-1)*rspace
         dif=abs(prange-rang)
         if (dif.lt.dif0) then
          irange=jran
          dif0=dif
         end if
        end do
        prange=r0+(irange-1)*rspace

c        CALL GETINT(11,IRANGE)
        IARANGE=IRANGE

        CALL GETflt(12,pdepth)
        dif0=1e20
        do jdep=1,ir
         dif=abs(pdepth-rdc(jdep))
         if (dif.lt.dif0) then
          idepth=jdep
          dif0=dif
         end if
        end do
        pdepth=rdc(idepth)

c        CALL GETINT(12,IDEPTH)
        IADEPTH=IDEPTH

        call getint(13,nazim)

        CALL GETFLT(14,SCALEFACT(1))
        CALL GETFLT(14,SCALEFACT(2))
        CALL GETFLT(14,SCALEFACT(3))
        ASCFACT(1)=SCALEFACT(1)
        ASCFACT(2)=SCALEFACT(2)
        ASCFACT(3)=SCALEFACT(3)

        CALL GETCHA(15,PLOTTYPE)

        IF (IRANGE.EQ.0) THEN
          IRLOW=1
          IRHIGH=NPLOTS
        ELSE
          IRLOW=IRANGE
          IRHIGH=IRANGE
        END IF
        IF (IDEPTH.EQ.0) THEN
          IDLOW=1
          IDHIGH=IR
        ELSE
          IDLOW=IDEPTH
          IDHIGH=IDEPTH
        ENDIF
       ELSE IF (TPLOT.EQ.'R') THEN
*     Depth stacked plots
       if (inttyp.eq.-1) then
        CALL MENCRT('SLOWNESS STACKED SEISMOGRAMS (tau-p)',19)
       else
        CALL MENCRT('RANGE STACKED PLOTS',19)
       end if
        CALL TXTFLD(1,'Plot title:',TITLE)
        CALL FLTFLD(2,'Min time:',XLEFT,'s')
        CALL FLTFLD(3,'Max time:',XRIGHT,'s')
        CALL FLTFLD(4,'Time tick inc:',XINC,'s')
        CALL FLTFLD(5,'T-axis length:',XAXIS,'cm')
        CALL FLTFLD(6,'Range down:',YRDOWN,'km')
        CALL FLTFLD(7,'Range up:',YRUP,'km')
        CALL FLTFLD(8,'Range tick inc:',YRINC,'km')
        CALL FLTFLD(9,'R-axis length:',YRAXISS,'cm')
        CALL TXTFLD(10,'R-axis label:',RYTXT)
        CALL FLTFLD(11,'Red. velocity:',RC0,'m/s')
        CALL fltFLD(12,'Depth:',PDEPTH,'m')
        CALL FLTFLD(13,'Azimuth:',thetad,'deg')
        CALL FLTFLD(14,'Scale factor:',RSCFACT(1),' ')
        CALL CHAFLD(15,'Range scaling?',LETTER,'S/C/N')
        CALL CHAFLD(16,'Parameter:',PLOTTYPE,DUMMY(1:2*NOUT-1))
        CALL ACTFLD(17,'Generate plot:',' ')
        CALL ACTFLD(18,'Trace file:',' ')
        CALL ACTFLD(19,'Return:','PP main menu')

 1021   CALL SHMENU(IRET)

        IF (IRET.EQ.19) THEN
         GO TO 3000      
        ELSE IF (IRET.EQ.15) THEN
         CALL GETCHA(15,LETTER)
         IF (LETTER.EQ.'S') THEN
          ICDR=-1
         else if (LETTER.EQ.'C') THEN
          ICDR=0
         ELSE
          ICDR=1
          LETTER='N'
         END IF
         GO TO 1021
        ELSE IF (IRET.LT.17) THEN
         GO TO 1021
        ELSE
        END IF

        CALL GETTXT(1,TITLE)

        CALL GETFLT(2,XLEFT)
        XTLEFT=XLEFT

        CALL GETFLT(3,XRIGHT)
        XTRIGHT=XRIGHT
        XDRIGHT=XRIGHT

        CALL GETFLT(4,XINC)
        XTINC=XINC

        CALL GETFLT(5,XAXIS)
        XTAXIS=XAXIS

        CALL GETFLT(6,YDOWN)
        YRDOWN=YDOWN

        CALL GETFLT(7,YUP)
        YRUP=YUP

        CALL GETFLT(8,YINC)
        YRINC=YINC

        CALL GETFLT(9,YAXISS)
        YRAXISS=YAXISS

        CALL GETTXT(10,RYTXT)

        CALL GETFLT(11,C0)
        RC0=C0

        CALL GETflt(12,pdepth)
        dif0=1e20
        do jdep=1,ir
         dif=abs(pdepth-rdc(jdep))
         if (dif.lt.dif0) then
          idepth=jdep
          dif0=dif
         end if
        end do
        pdepth=rdc(idepth)

c        CALL GETINT(12,IDEPTH)
        IRDEPTH=IDEPTH

        call GETFLT(13,THETAD)
        THETA=THETAD*PI/180.

        CALL GETFLT(14,SCALEFACT(1))
        CALL GETFLT(14,SCALEFACT(2))
        CALL GETFLT(14,SCALEFACT(3))
        RSCFACT(1)=SCALEFACT(1)
        RSCFACT(2)=SCALEFACT(2)
        RSCFACT(3)=SCALEFACT(3)

        CALL GETCHA(15,LETTER)
        IF (LETTER.EQ.'S') THEN
           ICDR=-1
        ELSE IF (LETTER.EQ.'C') THEN
           ICDR=0
        ELSE
          ICDR=1
          LETTER='N'
        END IF

        CALL GETCHA(16,PLOTTYPE)

        IF (IDEPTH.EQ.0) THEN
          ILOW=1
          IHIGH=IR
        ELSE
          ILOW=IDEPTH
          IHIGH=IDEPTH
        ENDIF
      ELSE IF (TPLOT.EQ.'I') THEN
*      Individual timeseries
        CALL MENCRT('INDIVIDUAL TIMESERIES',14)
        CALL TXTFLD(1,'Plot title:',TITLE)
        CALL FLTFLD(2,'Min time:',XLEFT,'s')
        CALL FLTFLD(3,'Max time:',XRIGHT,'s')
        CALL FLTFLD(4,'Time tick inc:',XINC,'s')
        CALL FLTFLD(5,'T-axis length:',XAXIS,'cm')
        CALL FLTFLD(6,'Red. velocity:',C0,'m/s')
        CALL fltFLD(7,'Depth:',PDEPTH,'m')
        CALL fltFLD(8,'Range:',PRANGE,'km')
        CALL FLTFLD(9,'Azimuth:',thetad,'deg')
        CALL CHAFLD(10,'Parameter:',PLOTTYPE,DUMMY(1:2*NOUT-1))
        CALL ACTFLD(11,'Timeseries:',' ')
        call actfld(12,'Spectrum:',' ')
        call actfld(13,'Trace file:',' ')
        CALL ACTFLD(14,'Return:','PP main menu')

 1022   CALL SHMENU(IRET)

        IF (IRET.EQ.14) THEN
         GO TO 3000      
        ELSE IF (IRET.LT.11) THEN
         GO TO 1022
        ELSE
        END IF

        CALL GETTXT(1,TITLE)

        CALL GETFLT(2,XLEFT)
        XTLEFT=XLEFT

        CALL GETFLT(3,XRIGHT)
        XTRIGHT=XRIGHT
        XIRIGHT=XRIGHT

        CALL GETFLT(4,XINC)
        XTINC=XINC

        CALL GETFLT(5,XAXIS)
        XTAXIS=XAXIS

        CALL GETFLT(6,C0)
        TC0=C0

        CALL GETflt(7,pdepth)
        dif0=1e20
        do jdep=1,ir
         dif=abs(pdepth-rdc(jdep))
         if (dif.lt.dif0) then
          idepth=jdep
          dif0=dif
         end if
        end do
        pdepth=rdc(idepth)

c        CALL GETINT(7,IDEPTH)
        IIDEPTH=IDEPTH

        CALL GETflt(8,prange)
        dif0=1e20
        do jran=1,nplots
         rang=r0+(jran-1)*rspace
         dif=abs(prange-rang)
         if (dif.lt.dif0) then
          irange=jran
          dif0=dif
         end if
        end do
        prange=r0+(irange-1)*rspace

c        CALL GETINT(8,IRANGE)
        IIRANGE=IRANGE

        call GETFLT(9,THETAD)
        THETA=THETAD*PI/180.0

        CALL GETCHA(10,PLOTTYPE)

        IF (IDEPTH.EQ.0) THEN
          IDLOW=1
          IDHIGH=IR
        ELSE
          IDLOW=IDEPTH
          IDHIGH=IDEPTH
        ENDIF

        IF (IRANGE.EQ.0) THEN
          IRLOW=1
          IRHIGH=NPLOTS
        ELSE
          IRLOW=IRANGE
          IRHIGH=IRANGE
        ENDIF

       ELSE IF (TPLOT.EQ.'S') THEN
        OPTION(1)='SOURCE'
        WRITE(TITLEA,'(A,I2)') 'Pulse number',ISTMP
*      Source timeseries and spectra
        CALL MENCRT('SOURCE PULSES',10)
        CALL INTFLD(1,'Source type:',ISTMP,0,5)
        CALL TXTFLD(2,'Source file:',FUSTMP)
        CALL TXTFLD(3,'Plot title:',TITLEA)
        CALL FLTFLD(4,'Min time:',XALEFT,'s')
        CALL FLTFLD(5,'Max time:',XARIGHT,'s')
        CALL FLTFLD(6,'Time tick inc:',XAINC,'s')
        CALL FLTFLD(7,'T-axis length:',XAAXIS,'cm')
        CALL FLTFLD(8,'Y-axis length:',YAAXIS,'cm')
        CALL ACTFLD(9,'Generate plot:',' ')
        CALL ACTFLD(10,'Return:','PP main menu')

 1023   CALL SHMENU(IRET)

        IF (IRET.EQ.10) THEN
         GO TO 3000      
        ELSE IF (IRET.NE.9) THEN
         GO TO 1023
        ELSE
        END IF

        CALL GETINT(1,ISTMP)
        ISSTMP=ISTMP

        CALL GETTXT(2,FUSTMP)

        CALL GETTXT(3,TITLEA)

        CALL GETFLT(4,XALEFT)
        XSALEFT=XALEFT

        CALL GETFLT(5,XARIGHT)
        XSARIGHT=XARIGHT

        CALL GETFLT(6,XAINC)
        XSAINC=XAINC

        CALL GETFLT(7,XAAXIS)
        XSAAXIS=XAAXIS

        CALL GETFLT(8,YAAXIS)
        YSAAXIS=YAAXIS

        IF (ISTMP.EQ.0) then
          OPEN(65,FILE=FILENUSP,STATUS='OLD',ERR=1023)
          CALL INTERPOL(CFFS,NX,DT)
          close(65)
        END IF


      ELSE IF (TPLOT.EQ.'C') THEN
*     Vertical Snap shots

        CALL MENCRT('VERTICAL SNAP SHOTS',22)
        CALL TXTFLD(1,'Plot title:',TITLE)
        CALL FLTFLD(2,'Range left:',XCLEFT,'km')
        CALL FLTFLD(3,'Range right:',XCRIGHT,'km')
        CALL FLTFLD(4,'Range tick inc:',XCINC,'km')
        CALL FLTFLD(5,'R-axis length:',XCAXIS,'cm')
        CALL FLTFLD(6,'Depth down:',YCDOWN,'m')
        CALL FLTFLD(7,'Depth up:',YCUP,'m')
        CALL FLTFLD(8,'Depth tick inc:',YCINC,'m')
        CALL FLTFLD(9,'D-axis length:',YCAXIS,'cm')
        call FLTFLD(10,'Azimuth:',THETAD,'deg')
        CALL FLTFLD(11,'Time,frst frame:',TSHmin,'s')
        CALL FLTFLD(12,'Time,last frame:',TSHmax,'s')
        call intfld(13,'Number of frames:',nframe,1,99)

        IF (KSNAPSH1.NE.0) THEN
          CALL FLTFLD(14,'Max. level:',ZMAX,' ')
          CALL INTFLD(15,'Norm. exp.:',NORMK,-99,99)
        ELSE            
          CALL FLTFLD(14,'Max. level:',ZMAX,' ')
          CALL INTFLD(15,'Norm. exp.:',NORMK,-99,99)
        END IF

        CALL INTFLD(16,'No. contours:',NUMINT,1,21)
        CALL TXTFLD(17,'Undef. file:',UNDFIL)
        call txtfld(18,'Shade file:',shdver)
        CALL CHAFLD(19,'Parameter:',PLOTTYPE,DUMMY(1:2*NOUT-1))
        CALL CHAFLD(20,'Range scaling?',LETTER,'S/C/N')
        CALL ACTFLD(21,'Generate plot:',' ')
        CALL ACTFLD(22,'return:','PP main menu')

 1024  CALL SHMENU(IRET)

        IF (IRET.EQ.22) THEN
         GO TO 3000      
        ELSE IF (IRET.NE.21) THEN
         GO TO 1024
        ELSE
        END IF

        CALL GETTXT(1,TITLE)

        CALL GETFLT(2,XLEFT)
        if (xcleft.ne.xleft) ITMP=-1
        XCLEFT=XLEFT

        CALL GETFLT(3,XRIGHT)
        XCRIGHT=XRIGHT

        CALL GETFLT(4,XINC)
        XCINC=XINC

        CALL GETFLT(5,XAXIS)
        XCAXIS=XAXIS

        CALL GETFLT(6,YDOWN)
        YCDOWN=YDOWN

        CALL GETFLT(7,YUP)
        YCUP=YUP

        CALL GETFLT(8,YINC)
        YCINC=YINC

        CALL GETFLT(9,YAXIS)
        YCAXIS=YAXIS

        CALL GETFLT(10,THETAD)
        THETA=THETAD*PI/180.0

        CALL GETFLT(11,TSHmin)
        TCSHIFT=TSHmin

        CALL GETFLT(12,TSHmax)

        CALL GETINT(13,nframe)

        CALL GETFLT(14,XX)
        IF (KSNAPSH1.EQ.1.and.XX.NE.zmax) THEN
          ZMAX=XX
          ZCMAX=ZMAX
          if (deconv) then
           zmin=0e0
          else
           ZMIN=-ZMAX
          end if
          ZCMIN=ZMIN
        END IF

        CALL GETINT(15,IXX)
        IF (KSNAPSH1.EQ.1.and.IXX.NE.NORMK) THEN
          NORMK=IXX
          NCORMK=NORMK
        END IF

        CALL GETINT(16,NUMINT)
        NUMCINT=NUMINT

c
c >>> read undefined point file
c
         call gettxt(17,undfil)
         if (undfil.ne.' '.and.undfil(1:1).ne.char(0)) then
          open(71,file=undfil,status='old',form='formatted',err=7654)
          do 7653 i=1,ir
           read(71,'(80i1)',err=7655,end=7655) 
     &         (iundef(i+(j-1)*ir),j=1,nplots)
 7653     continue
          close(71,status='keep')
          undfpt=.true.
          go to 7656
 7655     close(71,status='keep')
 7654     undfpt=.false.
          write(6,*) 'Undef. file not found or incompatible' 
          go to 1024
 7656     continue
         else
          undfpt=.false.
         end if            

         call gettxt(18,shdver)

         if (shdver.ne.' '.and.shdver(1:1).ne.char(0)) then
          open(72,file=shdver,form='formatted',status='old',err=7755)
          read(72,'(a)',end=7754,err=7754) dummy
          if (dummy(1:6).ne.'BOTTOM') go to 7754
          close(72)
          shading=.true.
          go to 7756
 7754     write(6,*) '>>> Error: Shading file in wrong format <<<'
          shading=.false.
          close(72)
c          pause
c          go to 1024
 7755     write(6,*) '>>> Error: Shading file does not exist <<<'
          shading=.false.
c          pause
c          go to 1024
         else
          shading=.false.
         end if
      
 7756     CALL GETCHA(19,PLOTTYPE)

        CALL GETCHA(20,LETTER)
        IF (LETTER.EQ.'S') THEN
           ICDR=-1
        ELSE IF (LETTER.EQ.'C') THEN
           ICDR=0
        ELSE
          ICDR=1
          LETTER='N'
        END IF

      ELSE IF (TPLOT.EQ.'H') THEN
*     Vertical Snap shots

        CALL MENCRT('HORIZONTAL SNAP SHOTS',18)
        CALL TXTFLD(1,'Plot title:',TITLE)
        CALL FLTFLD(2,'Max Range:',XCRIGHT,'km')
        CALL FLTFLD(3,'Range tick inc:',XCINC,'km')
        CALL FLTFLD(4,'R-axis length:',XCAXIS,'cm')
        call INTFLD(5,'# azimuths:',nazim_tl,1,999)
        CALL fltFLD(6,'Depth:',PDEPTH,'m')

        CALL FLTFLD(7,'Time,frst frame:',TSHmin,'s')
        CALL FLTFLD(8,'Time,last frame:',TSHmax,'s')
        call intfld(9,'Number of frames:',nframe,1,99)

        IF (KSNAPSH1.NE.0) THEN
          CALL FLTFLD(10,'Max. level:',ZMAX,' ')
          CALL INTFLD(11,'Norm. exp.:',NORMK,-99,99)
        ELSE            
          CALL FLTFLD(10,'Max. level:',ZMAX,' ')
          CALL INTFLD(11,'Norm. exp.:',NORMK,-99,99)
        END IF

        CALL INTFLD(12,'No. contours:',NUMINT,1,21)
        CALL TXTFLD(13,'Undef. file:',UNDFIL)
        call txtfld(14,'Shade file:',shdhor)
        CALL CHAFLD(15,'Parameter:',PLOTTYPE,DUMMY(1:2*NOUT-1))
        CALL CHAFLD(16,'Range scaling?',LETTER,'S/C/N')
        CALL ACTFLD(17,'Generate plot:',' ')
        CALL ACTFLD(18,'return:','PP main menu')

 6024  CALL SHMENU(IRET)

        IF (IRET.EQ.18) THEN
         GO TO 3000      
        ELSE IF (IRET.NE.17) THEN
         GO TO 6024
        ELSE
        END IF

        CALL GETTXT(1,TITLE)

        CALL GETFLT(2,XCRIGHT)

        CALL GETFLT(3,XCINC)

        CALL GETFLT(4,XCAXIS)

        CALL GETINT(5,nazim_tl)

        CALL GETflt(6,pdepth)
        dif0=1e20
        do jdep=1,ir
         dif=abs(pdepth-rdc(jdep))
         if (dif.lt.dif0) then
          idepth=jdep
          dif0=dif
         end if
        end do
        pdepth=rdc(idepth)
        IRDEPTH=IDEPTH


        CALL GETFLT(7,TSHmin)
        TCSHIFT=TSHmin

        CALL GETFLT(8,TSHmax)

        CALL GETINT(9,nframe)

        CALL GETFLT(10,XX)
        IF (KSNAPSH1.EQ.1.and.XX.NE.zmax) THEN
          ZMAX=XX
          ZCMAX=ZMAX
          if (deconv) then
           zmin=0e0
          else
           ZMIN=-ZMAX
          end if
          ZCMIN=ZMIN
        END IF

        CALL GETINT(11,IXX)
        IF (KSNAPSH1.EQ.1.and.IXX.NE.NORMK) THEN
          NORMK=IXX
          NCORMK=NORMK
        END IF

        CALL GETINT(12,NUMINT)
        NUMCINT=NUMINT

c
c >>> read undefined point file
c
         call gettxt(13,undfil)
         if (undfil.ne.' '.and.undfil(1:1).ne.char(0)) then
          open(71,file=undfil,status='old',form='formatted',err=6654)
          do 6653 i=1,ir
           read(71,'(80i1)',err=6655,end=6655) 
     &         (iundef(i+(j-1)*ir),j=1,nplots)
 6653     continue
          close(71,status='keep')
          undfpt=.true.
          go to 6656
 6655     close(71,status='keep')
 6654     undfpt=.false.
          write(6,*) 'Undef. file not found or incompatible' 
          go to 6024
 6656     continue
         else
          undfpt=.false.
         end if            

         call gettxt(14,shdhor)
         if (shdhor.ne.' '.and.shdhor(1:1).ne.char(0)) then
          open(72,file=shdhor,form='formatted',status='old',err=6755)
          read(72,'(a)',end=6754,err=6754) dummy
          if (dummy(1:6).ne.'BOTTOM') go to 6754
          close(72)
          shading=.true.
          go to 6756
 6754     write(6,*) '>>> Error: Shading file in wrong format <<<'
          shading=.false.
          close(72)
c          pause
          go to 6024
 6755     write(6,*) '>>> Error: Shading file does not exist <<<'
          shading=.false.
c          pause
          go to 6024
 6756     continue
         else
          shading=.false.
         end if

        CALL GETCHA(15,PLOTTYPE)

        CALL GETCHA(16,LETTER)
        IF (LETTER.EQ.'S') THEN
           ICDR=-1
        ELSE IF (LETTER.EQ.'C') THEN
           ICDR=0
        ELSE
          ICDR=1
          LETTER='N'
        END IF

      ELSE IF (TPLOT.EQ.'c') THEN
c >>> TL contours
            write(6,*) ' calling mencrt'

         CALL MENCRT('TL CONTOURS',19)
         CALL TXTFLD(1,'Plot title:',TITLE)
         CALL FLTFLD(2,'Range left:',XCLEFT,'km')
         CALL FLTFLD(3,'Range right:',XCRIGHT,'km')
         CALL FLTFLD(4,'Range tick inc:',XCINC,'km')
         CALL FLTFLD(5,'R-axis length:',XCAXIS,'cm')
         CALL FLTFLD(6,'Depth down:',YCDOWN,'m')
         CALL FLTFLD(7,'Depth up:',YCUP,'m')
         CALL FLTFLD(8,'Depth tick inc:',YCINC,'m')
         CALL FLTFLD(9,'D-axis length:',YCAXIS,'cm')
         call FLTFLD(10,'Azimuth:',THETAD,'deg')
         call fltfld(11,'Frequency:',freq,'Hz')
         CALL FLTFLD(12,'Min. loss:',TZMIN,'dB')
         CALL FLTFLD(13,'Max. loss:',TZMAX,'dB')
         CALL FLTFLD(14,'Increment:',TZINC,'dB')
         CALL TXTFLD(15,'Undef. file:',UNDFIL)
         call txtfld(16,'Shade file:',shdver)
         CALL CHAFLD(17,'Parameter:',PLOTTYPE,DUMMY(1:2*NOUT-1))
         CALL ACTFLD(18,'Generate plot:',' ')
         CALL ACTFLD(19,'Return:','PP main menu')

 8024   CALL SHMENU(IRET)

        IF (IRET.EQ.19) THEN
         GO TO 3000      
        ELSE IF (IRET.NE.18) THEN
         GO TO 8024
        ELSE
        END IF

        CALL GETTXT(1,TITLE)

        CALL GETFLT(2,XCLEFT)

        CALL GETFLT(3,XCRIGHT)

        CALL GETFLT(4,XCINC)

        CALL GETFLT(5,XCAXIS)

        CALL GETFLT(6,YCDOWN)

        CALL GETFLT(7,YCUP)

        CALL GETFLT(8,YCINC)

        CALL GETFLT(9,YCAXIS)

        CALL GETFLT(10,THETAD)
        THETA=THETAD*PI/180.0


        CALL GETFLT(11,FREQ)
        tfreq=freq

        CALL GETFLT(12,tzmin)

        CALL GETFLT(13,tzmax)

        CALL GETFLT(14,tzinc)
c
c >>> read undefined point file
c
         call gettxt(15,undfil)
         if (undfil.ne.' '.and.undfil(1:1).ne.char(0)) then
          open(71,file=undfil,status='old',form='formatted',err=8654)
          do 8653 i=1,ir
           read(71,'(80i1)',err=8655,end=8655) 
     &         (iundef(i+(j-1)*ir),j=1,nplots)
 8653     continue
          close(71,status='keep')
          undfpt=.true.
          go to 8656
 8655     close(71,status='keep')
 8654     undfpt=.false.
          write(6,*) 'Undef. file not found or incompatible' 
          go to 8024
 8656     continue
         else
          undfpt=.false.
         end if            

         call gettxt(16,shdver)
         if (shdver.ne.' '.and.shdver(1:1).ne.char(0)) then
          open(72,file=shdver,form='formatted',status='old',err=8755)
          read(72,'(a)',end=8754,err=8754) dummy
          if (dummy(1:6).ne.'BOTTOM') go to 8754
          close(72)
          shading=.true.
          write(6,*) '>>> Shading activated <<<'
          go to 8756
 8754     write(6,*) '>>> Error: Shading file in wrong format <<<'
          shading=.false.
          close(72)
c          pause
          go to 8024
 8755     write(6,*) '>>> Error: Shading file does not exist <<<'
          shading=.false.
c          pause
          go to 8024
 8756     continue
         else
          shading=.false.
         end if

        CALL GETCHA(17,PLOTTYPE)

      ELSE IF (TPLOT.EQ.'h') THEN
c >>> TL contours in horizontal plane
         CALL MENCRT('TL CONTOURS',15)
         CALL TXTFLD(1,'Plot title:',TITLE)
         CALL FLTFLD(2,'Max Range:',XCRIGHT,'km')
         CALL FLTFLD(3,'Range tick inc:',XCINC,'km')
         CALL FLTFLD(4,'R-axis length:',XCAXIS,'cm')
         call INTFLD(5,'# azimuths:',nazim_tl,1,999)
         call fltfld(6,'Frequency:',freq,'Hz')
         CALL fltFLD(7,'Depth:',PDEPTH,'m')
         CALL FLTFLD(8,'Min. loss:',TZMIN,'dB')
         CALL FLTFLD(9,'Max. loss:',TZMAX,'dB')
         CALL FLTFLD(10,'Increment:',TZINC,'dB')
         CALL TXTFLD(11,'Undef. file:',UNDFIL)
         call txtfld(12,'Shade file:',shdhor)
         CALL CHAFLD(13,'Parameter:',PLOTTYPE,DUMMY(1:2*NOUT-1))
         CALL ACTFLD(14,'Generate plot:',' ')
         CALL ACTFLD(15,'Return:','PP main menu')

 8025    CALL SHMENU(IRET)

        IF (IRET.EQ.15) THEN
         GO TO 3000      
        ELSE IF (IRET.NE.14) THEN
         GO TO 8025
        ELSE
        END IF


        CALL GETTXT(1,TITLE)

        CALL GETFLT(2,XCRIGHT)

        CALL GETFLT(3,XCINC)

        CALL GETFLT(4,XCAXIS)

        CALL GETINT(5,nazim_tl)

        CALL GETFLT(6,FREQ)
        tfreq=freq

        CALL GETflt(7,pdepth)
        dif0=1e20
        do jdep=1,ir
         dif=abs(pdepth-rdc(jdep))
         if (dif.lt.dif0) then
          idepth=jdep
          dif0=dif
         end if
        end do
        pdepth=rdc(idepth)
        IRDEPTH=IDEPTH

        CALL GETFLT(8,tzmin)

        CALL GETFLT(9,tzmax)

        CALL GETFLT(10,tzinc)

c
c >>> read undefined point file
c
         call gettxt(11,undfil)
         if (undfil.ne.' '.and.undfil(1:1).ne.char(0)) then
          open(71,file=undfil,status='old',form='formatted',err=8054)
          do i=1,ir
           read(71,'(80i1)',err=8055,end=8055) 
     &         (iundef(i+(j-1)*ir),j=1,nplots)
          end do
          close(71,status='keep')
          undfpt=.true.
          go to 8056
 8055     close(71,status='keep')
 8054     undfpt=.false.
          write(6,*) 'Undef. file not found or incompatible' 
          go to 8025
 8056     continue
         else
          undfpt=.false.
         end if            

         call gettxt(12,shdhor)
         if (shdhor.ne.' '.and.shdhor(1:1).ne.char(0)) then
          open(72,file=shdhor,form='formatted',status='old',err=8155)
          read(72,'(a)',end=8154,err=8154) dummy
          if (dummy(1:6).ne.'BOTTOM') go to 8154
          close(72)
          shading=.true.
          go to 8156
 8154     write(6,*) '>>> Error: Shading file in wrong format <<<'
          shading=.false.
          close(72)
c         pause
          go to 8025
 8155     write(6,*) '>>> Error: Shading file does not exist <<<'
          shading=.false.
c         pause
          go to 8025
 8156     continue
         else
          shading=.false.
         end if

        CALL GETCHA(13,PLOTTYPE)

       ELSE IF (TPLOT.EQ.'f') THEN
c       Range TL plots

        CALL MENCRT('TRANSMISSION LOSS',15)
        CALL TXTFLD(1,'Plot title:',TITLE)
        CALL FLTFLD(2,'Freq left:',XFLEFT,'Hz')
        CALL FLTFLD(3,'Freq right:',XFRIGHT,'Hz')
        CALL FLTFLD(4,'Freq tick inc:',XFINC,'Hz')
        CALL FLTFLD(5,'R-axis length:',XFAXIS,'cm')
        CALL FLTFLD(6,'TL up:',tymin,'dB')
        CALL FLTFLD(7,'TL down:',tymax,'dB')
        CALL FLTFLD(8,'TL inc:',tyinc,'dB')
        CALL FLTFLD(9,'TL-axis length:',tyaxis,'cm')
        call FLTFLD(10,'Azimuth:',THETAD,'deg')
        call fltfld(11,'Range:',prange,'km')
        call fltfld(12,'Depth:',pdepth,'m')
        CALL CHAFLD(13,'Parameter:',PLOTTYPE,DUMMY(1:2*NOUT-1))
        CALL ACTFLD(14,'Generate plot:',' ')
        CALL ACTFLD(15,'Return:','PP main menu')

 8122   CALL SHMENU(IRET)
        IF (IRET.EQ.15) THEN
         GO TO 3000      
        ELSE IF (IRET.NE.14) THEN
         GO TO 8122
        ELSE
        END IF

        CALL GETTXT(1,TITLE)

        CALL GETFLT(2,XFLEFT)

        CALL GETFLT(3,XFRIGHT)

        CALL GETFLT(4,XFINC)

        CALL GETFLT(5,XFAXIS)

        CALL GETFLT(6,TYMIN)

        CALL GETFLT(7,TYMAX)

        CALL GETFLT(8,TYINC)

        CALL GETFLT(9,TYAXIS)

        CALL GETFLT(10,THETAD)
        THETA=THETAD*PI/180.0


        CALL GETFLT(11,prange)
        dif0=1e20
        do jran=1,nplots
         dif=abs(prange-(r0+(jran-1)*rspace))
         if (dif.lt.dif0) then
          irange=jran
          dif0=dif
         end if
        end do
        prange=r0+(irange-1)*rspace
        IDRANGE=irange

        CALL GETflt(12,pdepth)
        dif0=1e20
        do jdep=1,ir
         dif=abs(pdepth-rdc(jdep))
         if (dif.lt.dif0) then
          idepth=jdep
          dif0=dif
         end if
        end do
        pdepth=rdc(idepth)
        IRDEPTH=IDEPTH

        CALL GETCHA(13,PLOTTYPE)

       ELSE IF (TPLOT.EQ.'r') THEN
c       Range TL plots

        CALL MENCRT('TRANSMISSION LOSS',15)
        CALL TXTFLD(1,'Plot title:',TITLE)
        CALL FLTFLD(2,'Range left:',XCLEFT,'km')
        CALL FLTFLD(3,'Range right:',XCRIGHT,'km')
        CALL FLTFLD(4,'Range tick inc:',XCINC,'km')
        CALL FLTFLD(5,'R-axis length:',XCAXIS,'cm')
        CALL FLTFLD(6,'TL up:',tymin,'dB')
        CALL FLTFLD(7,'TL down:',tymax,'dB')
        CALL FLTFLD(8,'TL inc:',tyinc,'dB')
        CALL FLTFLD(9,'TL-axis length:',tyaxis,'cm')
        call FLTFLD(10,'Azimuth:',THETAD,'deg')
        call fltfld(11,'Frequency:',freq,'Hz')
        call fltfld(12,'Depth:',pdepth,'m')
        CALL CHAFLD(13,'Parameter:',PLOTTYPE,DUMMY(1:2*NOUT-1))
        CALL ACTFLD(14,'Generate plot:',' ')
        CALL ACTFLD(15,'Return:','PP main menu')

 8124   CALL SHMENU(IRET)
        IF (IRET.EQ.15) THEN
         GO TO 3000      
        ELSE IF (IRET.NE.14) THEN
         GO TO 8124
        ELSE
        END IF

        CALL GETTXT(1,TITLE)

        CALL GETFLT(2,XCLEFT)

        CALL GETFLT(3,XCRIGHT)

        CALL GETFLT(4,XCINC)

        CALL GETFLT(5,XCAXIS)

        CALL GETFLT(6,TYMIN)

        CALL GETFLT(7,TYMAX)

        CALL GETFLT(8,TYINC)

        CALL GETFLT(9,TYAXIS)

        CALL GETFLT(10,THETAD)
        THETA=THETAD*PI/180.0


        CALL GETFLT(11,FREQ)
        tfreq=freq

        CALL GETflt(12,pdepth)
        dif0=1e20
        do jdep=1,ir
         dif=abs(pdepth-rdc(jdep))
         if (dif.lt.dif0) then
          idepth=jdep
          dif0=dif
         end if
        end do
        pdepth=rdc(idepth)

        IRDEPTH=IDEPTH

        CALL GETCHA(13,PLOTTYPE)

       ELSE IF (TPLOT.EQ.'a') THEN
c       Range TL plots

        CALL MENCRT('DEPTH AVERAGED LOSS',14)
        CALL TXTFLD(1,'Plot title:',TITLE)
        CALL FLTFLD(2,'Range left:',XCLEFT,'km')
        CALL FLTFLD(3,'Range right:',XCRIGHT,'km')
        CALL FLTFLD(4,'Range tick inc:',XCINC,'km')
        CALL FLTFLD(5,'R-axis length:',XCAXIS,'cm')
        CALL FLTFLD(6,'TL up:',tymin,'dB')
        CALL FLTFLD(7,'TL down:',tymax,'dB')
        CALL FLTFLD(8,'TL inc:',tyinc,'dB')
        CALL FLTFLD(9,'TL-axis length:',tyaxis,'cm')
        call FLTFLD(10,'Azimuth:',THETAD,'deg')
        call fltfld(11,'Frequency:',freq,'Hz')
        CALL CHAFLD(12,'Parameter:',PLOTTYPE,DUMMY(1:2*NOUT-1))
        CALL ACTFLD(13,'Generate plot:',' ')
        CALL ACTFLD(14,'Return:','PP main menu')

 8123   CALL SHMENU(IRET)
        IF (IRET.EQ.14) THEN
         GO TO 3000      
        ELSE IF (IRET.NE.13) THEN
         GO TO 8123
        ELSE
        END IF

        CALL GETTXT(1,TITLE)

        CALL GETFLT(2,XCLEFT)

        CALL GETFLT(3,XCRIGHT)

        CALL GETFLT(4,XCINC)

        CALL GETFLT(5,XCAXIS)

        CALL GETFLT(6,TYMIN)

        CALL GETFLT(7,TYMAX)

        CALL GETFLT(8,TYINC)

        CALL GETFLT(9,TYAXIS)

        CALL GETFLT(10,THETAD)
        THETA=THETAD*PI/180.0


        CALL GETFLT(11,FREQ)
        tfreq=freq

        CALL GETCHA(12,PLOTTYPE)

       ELSE IF (TPLOT.EQ.'d') THEN
c       Depth TL plots

        CALL MENCRT('TRANSMISSION LOSS',15)
        CALL TXTFLD(1,'Plot title:',TITLE)
        CALL FLTFLD(2,'Depth up:',YCUP,'m')
        CALL FLTFLD(3,'Depth down:',YCDOWN,'m')
        CALL FLTFLD(4,'Depth tick inc:',YCINC,'m')
        CALL FLTFLD(5,'D-axis length:',YCAXIS,'cm')
        CALL FLTFLD(6,'TL min:',tymin,'dB')
        CALL FLTFLD(7,'TL max:',tymax,'dB')
        CALL FLTFLD(8,'TL inc:',tyinc,'dB')
        CALL FLTFLD(9,'TL-axis length:',tyaxis,'cm')
        call FLTFLD(10,'Azimuth:',THETAD,'deg')
        call fltfld(11,'Frequency:',freq,'Hz')
        call fltfld(12,'Range:',prange,'km')
        CALL CHAFLD(13,'Parameter:',PLOTTYPE,DUMMY(1:2*NOUT-1))
        CALL ACTFLD(14,'Generate plot:',' ')
        CALL ACTFLD(15,'Return:','PP main menu')

 8125   CALL SHMENU(IRET)
        IF (IRET.EQ.15) THEN
         GO TO 3000      
        ELSE IF (IRET.NE.14) THEN
         GO TO 8125
        ELSE
        END IF

        CALL GETTXT(1,TITLE)

        CALL GETFLT(2,YCUP)

        CALL GETFLT(3,YCDOWN)

        CALL GETFLT(4,YCINC)

        CALL GETFLT(5,YCAXIS)

        CALL GETFLT(6,TYMIN)

        CALL GETFLT(7,TYMAX)

        CALL GETFLT(8,TYINC)

        CALL GETFLT(9,TYAXIS)

        CALL GETFLT(10,THETAD)
        THETA=THETAD*PI/180.0

        CALL GETFLT(11,FREQ)
        tfreq=freq

        CALL GETflt(12,prange)
        dif0=1e20
        do jran=1,nplots
         dif=abs(prange-(r0+(jran-1)*rspace))
         if (dif.lt.dif0) then
          irange=jran
          dif0=dif
         end if
        end do
        prange=r0+(irange-1)*rspace

        IDRANGE=irange

        CALL GETCHA(13,PLOTTYPE)
      ELSE
       write(6,*) '>>> UNKNOWN OPTION ',TPLOT
       goto 3000
      END IF

      IF (TPLOT.NE.'S') THEN

        DO 901 JJ=1,NOUT
          IF (PLOTTYPE.EQ.PCHOICE(JJ)) THEN
            IPACT=JJ
            GO TO 902
          END IF
 901   CONTINUE
 902   CONTINUE

c        IF (      (TPLOT.EQ.'C'.or.TPLOT.EQ.'H')
c     &        .AND.(ITMP.EQ.-1)                   ) ITMP=IPACT
      ENDIF

      IF (      (TPLOT.NE.'C').AND.(TPLOT.NE.'H')
     &     .and.(tplot.ne.'c').and.(tplot.ne.'r') 
     &     .and.(tplot.ne.'f').and.(tplot.ne.'d') 
     &     .and.(tplot.ne.'a').and.(tplot.ne.'h')
     &     .AND.(TPLOT.NE.'S')                   ) THEN
*     Initiate variables
        LF0=1
c        LF=MIN(IFIX(ABS(XLEFT-XRIGHT)/DT+1.5),nx)
        lf=lf0+nx-1
        TMIN=AMIN1(XLEFT,XRIGHT)

      ENDIF

      WRITE(*,*)
      WRITE(*,*)'   ***  Working  ***'
      WRITE(*,*)
C-------------------------------------------------------------------

*     Plot the desired plots

C-------------------------------------------------------------------
      IF (TPLOT.EQ.'D') THEN
*     Depth stacked plots
        IF (IRET.EQ.16) THEN
c         write(6,*) 'calling opnplp'
         call OPNPLP
        ELSE
         PLPFILE=.FALSE.
        END IF
 1391   CONTINUE
        DO 140 K=ILOW,IHIGH
        RANGE=R0+(K-1)*RSPACE
        RANGEM=RANGE*1000.0
        IF (C0.GT.0.0) THEN
          TSHIFT=RANGEM/C0+TMIN
        ELSE
          TSHIFT=TMIN
        ENDIF
         IF (IRET.EQ.16) THEN
          CALL PLSTDEP(TITLE,IPARM(IPACT),XAXIS,YAXISS,XLEFT,XRIGHT,
     &                 XINC,YDOWN,YUP,YINC,IR,SD,RANGE,C0,THETAD)
         ELSE if (TRACEFORM.EQ.'GLD') THEN
c          CALL GLDMAKE(TITLE,1E0/DT,1,IR,NX,'f','r')
          CALL GLDMAKE(TITLE,1E0/DT,1,IR,lf-lf0+1,'f','r')
         ELSE if (TRACEFORM.EQ.'MAT') THEN
            call fmatopen
            call matcsetup(lenstr(TITLE),PADFLAG
     &           ,REM,5,"TITLE")
            call matcwrite(title,lenstr(title)
     &           ,PADFLAG,REM)
            call matcsetup(lenstr(PLOTTYPE)
     &           ,PADFLAG,REM,4,"TYPE")
            call matcwrite(plottype,lenstr(plottype)
     &           ,PADFLAG,REM)
            do 2000 iii=1,ir
               matdum(iii)=rdc(iii)
 2000       continue
            ndim = 2
            idim(1) = ir
            idim(2) = 1
            call matsetup(ndim,idim,PADFLAG,5,"DEPTH")
            call matwrite(matdum,ir,PADFLAG)
c prepare range array
            do 2001 iii=1,ir
               matdum(iii)=rangem
 2001      continue
            call matsetup(ndim,idim,PADFLAG,5,"RANGE")
            call matwrite(matdum,ir,PADFLAG)
c prepare az array
            do 2002 iii=1,ir
               matdum(iii)=thetad
 2002       continue
            call matsetup(ndim,idim,PADFLAG,5,"ANGLE")
            call matwrite(matdum,ir,PADFLAG)
            do 2003 iii=1,ir
               matdum(iii)=tshift
 2003       continue
            call matsetup(ndim,idim,PADFLAG,6,"TMSHFT")
            call matwrite(matdum,ir,PADFLAG)
            idim(1) = 1
            call matsetup(ndim,idim,PADFLAG,6,"DELTAT")
            call matwrite(dt,1,PADFLAG)
            idim(1) = lf-lf0+1
            idim(2) = ir
            call matsetup(ndim,idim,PADFLAG,4,"DATA")

         ELSE if (TRACEFORM.EQ.'SDR') THEN
          CALL SDRPREP
         ELSE if (TRACEFORM.EQ.'CFS') THEN
          CALL CFSPREP
         ELSE if (TRACEFORM.EQ.'BIN') THEN
          CALL BINMAKE(tshift,1E0/DT,1,IR,lf-lf0+1)
         else
          CALL ASCMAKE(TITLE,plottype,1E0/DT,1,IR,lf-lf0+1)
         END IF
          AMPMAX=0.0
c >>>   open temporary trace file
          nbuf=memlft()
          call opnbuf(71,nx,ir,nbuf)
          DO 150 J=1,IR
            CALL TIMSPU(IPACT,IPARM(IPACT),DOMEGA,TSHIFT,K,J,
     &                  THETA,NX,LX,MX)
            call wrbuf(71,x,nx)
            DO 122 JHS=LF0,LF
            VALUE=X(JHS,1)
            AMPMAX=AMAX1(AMPMAX,ABS(VALUE))
 122        CONTINUE
 150      continue
          IF (AMPMAX.NE.0.0) THEN
           if (log_ts) then
            FACTOR=ABS(YUP-YDOWN)/(IR*dbstack)
            troff=20.0*alog10(ampmax)-dbstack
           else
            FACTOR=ABS(YUP-YDOWN)/(IR*AMPMAX)
           end if
          ELSE
           FACTOR=1.0
          END IF
          FACTOR=SCALEFACT(1)*SIGN(FACTOR,YUP-YDOWN)
c          write(21,*) 'SCALING FACTOR:',FACTOR
          call enfbuf(71)
          call rwdbuf(71)
          do 151 j=1,ir
           call rdbuf(71,x,nx)
           IF (IRET.EQ.16) THEN
            OFFSET=RDC(J)
            CALL OUSTACK(LF0,LF,TMIN,DT,OFFSET,FACTOR,IPACT,PX,MODULO)
           ELSE if (TRACEFORM.EQ.'GLD') THEN
c     CALL GLDWRI(0,J-1,CFF(1,1),0,NX-1)  
             CALL GLDWRI(0,J-1,CFF(1,1),lf0-1,lf-1)
           ELSE if (TRACEFORM.EQ.'MAT') THEN
             if (j.eq.ir) then
               call matwrite(CFF(lf0,1),lf-lf0+1,1)
             else
               call matwrite(CFF(lf0,1),lf-lf0+1,0)
             endif
           ELSE IF (TRACEFORM.EQ.'SDR') THEN
            CALL SDRWRI(CFF,NX)
           ELSE IF (TRACEFORM.EQ.'CFS') THEN
            CALL CFSWRI(CFF,NX)
           ELSE IF (TRACEFORM.EQ.'BIN') THEN
            CALL BINWRI(cff,lf-lf0+1)
           else
            CALL ASCWRI(rangem,rdc(j),thetad,tshift,lf-lf0+1,CFF(1,1))
           END IF
151	  CONTINUE
c >>> close temporary trace file
          call clsbuf(71)
         IF (IRET.NE.16) THEN
          IF (TRACEFORM.EQ.'GLD') THEN
            CALL GLDCLS
          ELSE IF (TRACEFORM.EQ.'MAT') THEN
             print *,"wrote MAT FILE"
          ELSE IF (TRACEFORM.EQ.'SDR') THEN
            CALL VMOV(RANGEM,0,ARG,1,IR)
            CALL SDRCLS(ARG(1),RDC(1),IR,CFF,NX,DT)
          ELSE IF (TRACEFORM.EQ.'CFS') THEN
            CALL VMOV(RANGEM,0,ARG,1,IR)
            CALL CFSCLS(ARG(1),RDC(1),IR,CFF,NX,DT)
          ELSE IF (TRACEFORM.EQ.'BIN') THEN
            call bincls
          else
            call asccls
          END IF
         END IF
140     CONTINUE
      ELSE IF (TPLOT.EQ.'R') THEN
*     Range stacked plots
       IF (IRET.EQ.17) THEN
        CALL OPNPLP
       ELSE
        PLPFILE=.FALSE.
       END IF
 1392   CONTINUE
        DO 110 J=ILOW,IHIGH
c >>>   open temporary trace file
          nbuf=memlft()
          call opnbuf(71,nx,nplots,nbuf)

         IF (IRET.EQ.17) THEN
          CALL PLSTACK(TITLE,IPARM(IPACT),XAXIS,YAXISS,XLEFT,XRIGHT,
     1                 XINC,YDOWN,YUP,YINC,NPLOTS,SD,RDC(J),C0,THETAD,
     &                 1)
         ELSE IF (TRACEFORM.EQ.'GLD') THEN
c          CALL GLDMAKE(TITLE,1E0/DT,1,NPLOTS,NX,'f','r')
          CALL GLDMAKE(TITLE,1E0/DT,1,nplots,lf-lf0+1,'f','r')
         ELSE if (TRACEFORM.EQ.'MAT') THEN
            call fmatopen
            call matcsetup(lenstr(TITLE),PADFLAG
     &           ,REM,5,"TITLE")
            call matcwrite(title,lenstr(title)
     &           ,PADFLAG,REM)
            call matcsetup(lenstr(PLOTTYPE)
     &           ,PADFLAG,REM,4,"TYPE")
            call matcwrite(plottype,lenstr(plottype)
     &           ,PADFLAG,REM)
            do 2005 iii=1,nplots
               matdum(iii)=rdc(j)
 2005       continue
            ndim = 2
            idim(1) = nplots
            idim(2) = 1
            call matsetup(ndim,idim,PADFLAG,5,"DEPTH")
            call matwrite(matdum,nplots,PADFLAG)
c prepare range array
            do 2006 iii=1,nplots
               RANGE=R0+(iii-1)*RSPACE
               RANGEM=RANGE*1000.0
               matdum(iii)=rangem
 2006       continue
            call matsetup(ndim,idim,PADFLAG,5,"RANGE")
            call matwrite(matdum,nplots,PADFLAG)
c prepare az array
            do 2007 iii=1,nplots
               matdum(iii)=thetad
 2007       continue
            call matsetup(ndim,idim,PADFLAG,5,"ANGLE")
            call matwrite(matdum,nplots,PADFLAG)

            do 2008 iii=1,nplots
               RANGE=R0+(iii-1)*RSPACE
               RANGEM=RANGE*1000.0
               IF (C0.GT.0.0) THEN
                  TSHIFT=RANGEM/C0+TMIN
               ELSE
                  TSHIFT=TMIN
               ENDIF
               matdum(iii)=tshift
               write(6,*) 'r,tleft,tmin,tshift=',iii,tleft,tmin,tshift
 2008       continue
            call matsetup(ndim,idim,PADFLAG,6,"TMSHFT")
            call matwrite(matdum,nplots,PADFLAG)
            idim(1) = 1
            call matsetup(ndim,idim,PADFLAG,6,"DELTAT")
            call matwrite(dt,1,PADFLAG)

            idim(1) = lf-lf0+1
            idim(2) = nplots
            call matsetup(ndim,idim,PADFLAG,4,"DATA")

         ELSE IF (TRACEFORM.EQ.'SDR') THEN
          CALL SDRPREP
         ELSE if (TRACEFORM.EQ.'CFS') THEN
          CALL CFSPREP
         ELSE if (TRACEFORM.EQ.'BIN') THEN
          CALL BINMAKE(tmin,1E0/DT,1,nplots,lf-lf0+1)
         else
          CALL ASCMAKE(TITLE,plottype,1E0/DT,1,nplots,lf-lf0+1)
         END IF
          AMPMAX=0.0
c >>> reference range inverse
          onoran=1e0/amax1(abs(r0),abs(rspace))
          DO 120 K=1,NPLOTS
          RANGE=R0+(K-1)*RSPACE
          RANGEM=RANGE*1000.0
          IF (C0.GT.0.0) THEN
            TSHIFT=RANGEM/C0+TMIN
          ELSE
            TSHIFT=TMIN
          ENDIF
          if (icdr.eq.-1) then
           fref=range*onoran
          else if (icdr.eq.0) then
           fref=sqrt(range*onoran)
          else
           fref=1e0
          end if
          CALL TIMSPU(IPACT,IPARM(IPACT),DOMEGA,TSHIFT,K,J,
     &                THETA,NX,LX,MX)
          call wrbuf(71,x,nx)
          DO 108 JHS=LF0,LF
           VALUE=X(JHS,1)*fref
           AMPMAX=AMAX1(AMPMAX,ABS(VALUE))
 108      CONTINUE
 120     continue
         call enfbuf(71)
         call rwdbuf(71)     
         IF(AMPMAX.NE.0.0) THEN
          if (log_ts) then
           FCTOR=ABS(YUP-YDOWN)/(NPLOTS*dbstack)
           troff=20.0*alog10(ampmax)-dbstack
          else
           FCTOR=ABS(YUP-YDOWN)/(NPLOTS*AMPMAX)
          end if
         ELSE
          FCTOR=1.0
         END IF
         FCTOR=SCALEFACT(1)*SIGN(FCTOR,YUP-YDOWN)
c         write(21,*) 'scaling factor:',FCTOR

         do 121 k=1,nplots
          RANGE=R0+(K-1)*RSPACE
          RANGEM=RANGE*1000.0
          IF (C0.GT.0.0) THEN
            TSHIFT=RANGEM/C0+TMIN
          ELSE
            TSHIFT=TMIN
          ENDIF
          call rdbuf(71,x,nx)
          IF (IRET.EQ.17) THEN
           OFFSET=RANGE
           IF (ICDR.GT.0.or.log_ts) THEN
            FACTOR=FCTOR
           ELSE if (icdr.lt.0) then
            FACTOR=RANGE*onoran*FCTOR
           else
            FACTOR=sqrt(RANGE*onoran)*FCTOR
           END IF
           CALL OUSTACK(LF0,LF,TMIN,DT,OFFSET,FACTOR,IPACT,PX,MODULO)
          ELSE if (TRACEFORM.EQ.'GLD') THEN
c           CALL GLDWRI(0,K-1,CFF(1,1),0,NX-1)
            CALL GLDWRI(0,k-1,CFF(1,1),lf0-1,lf-1)
           ELSE if (TRACEFORM.EQ.'MAT') THEN
             if (k.eq.nplots) then
               call matwrite(CFF(lf0,1),lf-lf0+1,1)
             else
               call matwrite(CFF(lf0,1),lf-lf0+1,0)
             endif
          ELSE IF (TRACEFORM.EQ.'SDR') THEN
           CALL SDRWRI(CFF,NX)
          ELSE IF (TRACEFORM.EQ.'CFS') THEN
           CALL CFSWRI(CFF,NX)
          ELSE IF (TRACEFORM.EQ.'BIN') THEN
           CALL BINWRI(CFF,lf-lf0+1)
          else
           CALL ASCWRI(rangem,rdc(j),thetad,tshift,lf-lf0+1,CFF(1,1))
          END IF
121       CONTINUE
         call clsbuf(71)
         IF (IRET.NE.17) THEN
          IF (TRACEFORM.EQ.'GLD') THEN
            CALL GLDCLS
          ELSE IF (TRACEFORM.EQ.'MAT') THEN
             print *,"wrote MAT FILE"
          ELSE IF (TRACEFORM.EQ.'SDR') THEN
            CALL VMOV(RDC(J),0,FAC,1,NPLOTS)
            CALL VRAMP(R0,RSPACE,ARG,1,NPLOTS)
            CALL VSMUL(ARG,1,1E3,ARG,1,NPLOTS)
            CALL SDRCLS(ARG(1),FAC(1),NPLOTS,CFF,NX,DT)
          ELSE IF (TRACEFORM.EQ.'CFS') THEN
            CALL VMOV(RDC(J),0,FAC,1,NPLOTS)
            CALL VRAMP(R0,RSPACE,ARG,1,NPLOTS)
            CALL VSMUL(ARG,1,1E3,ARG,1,NPLOTS)
            CALL CFSCLS(ARG(1),FAC(1),NPLOTS,CFF,NX,DT)
          ELSE IF (TRACEFORM.EQ.'BIN') THEN
           CALL BINCLS
          else
            call asccls
          END IF
         END IF
110     CONTINUE
      ELSE IF (TPLOT.EQ.'A') THEN
*     Azimuth stacked plots
       IF (IRET.EQ.16) THEN
        CALL OPNPLP
       ELSE
        PLPFILE=.FALSE.
       END IF
        DO 175 J=IDLOW,IDHIGH
          DO 180 K=IRLOW,IRHIGH
          RANGE=R0+(K-1)*RSPACE
          RANGEM=RANGE*1000.0
          IF (C0.GT.0.0) THEN
            TSHIFT=RANGEM/C0+TMIN
          ELSE
            TSHIFT=TMIN
          ENDIF
c >>>   open temporary trace file
          nbuf=memlft()
          call opnbuf(71,nx,nazim,nbuf)
         IF (IRET.EQ.16) THEN
          CALL PLSTACK(TITLE,IPARM(IPACT),XAXIS,YAXISS,XLEFT,XRIGHT,
     1                 XINC,YDOWN,YUP,YINC,nazim,SD,RDC(J),C0,range,2)
         ELSE IF (TRACEFORM.EQ.'GLD') THEN
c          CALL GLDMAKE(TITLE,1E0/DT,1,nazim,NX,'f','r')
          CALL GLDMAKE(TITLE,1E0/DT,1,nazim,lf-lf0+1,'f','r')
         ELSE if (TRACEFORM.EQ.'MAT') THEN
            call fmatopen
            call matcsetup(lenstr(TITLE),PADFLAG
     &           ,REM,5,"TITLE")
            call matcwrite(title,lenstr(title)
     &           ,PADFLAG,REM)
            call matcsetup(lenstr(PLOTTYPE)
     &           ,PADFLAG,REM,4,"TYPE")
            call matcwrite(plottype,lenstr(plottype)
     &           ,PADFLAG,REM)
            do 2010 iii=1,nazim
               matdum(iii)=rdc(j)
 2010       continue
            ndim = 2
            idim(1) = nazim
            idim(2) = 1
            call matsetup(ndim,idim,PADFLAG,5,"DEPTH")
            call matwrite(matdum,nazim,PADFLAG)
c prepare range array
            do 2011 iii=1,nazim
               matdum(iii)=rangem
 2011      continue
            call matsetup(ndim,idim,PADFLAG,5,"RANGE")
            call matwrite(matdum,nazim,PADFLAG)
c prepare az array
            dlazim=(PI/180.)*(yaup-yadown)/nazim
            azim0= (pi/180.)*yadown+0.5*dlazim
            do 2012 iii=1,nazim
               azim=azim0+(iii-1)*dlazim
               matdum(iii)=azim
 2012       continue
            call matsetup(ndim,idim,PADFLAG,5,"ANGLE")
            call matwrite(matdum,nazim,PADFLAG)
            do 2013 iii=1,nazim
               matdum(iii)=tshift
 2013       continue
            call matsetup(ndim,idim,PADFLAG,6,"TMSHFT")
            call matwrite(matdum,nazim,PADFLAG)
            idim(1) = 1
            call matsetup(ndim,idim,PADFLAG,6,"DELTAT")
            call matwrite(dt,1,PADFLAG)

            idim(1)= lf-lf0+1
            idim(2) = nazim
            call matsetup(ndim,idim,PADFLAG,4,"DATA")

         ELSE IF (TRACEFORM.EQ.'SDR') THEN
          CALL SDRPREP
         ELSE if (TRACEFORM.EQ.'CFS') THEN
          CALL CFSPREP
         ELSE IF (TRACEFORM.EQ.'BIN') THEN
          CALL BINMAKE(tshift,1E0/DT,1,nazim,lf-lf0+1)
         else
          CALL ASCMAKE(TITLE,plottype,1E0/DT,1,nazim,lf-lf0+1)
         END IF
          AMPMAX=0.0
          dlazim=(PI/180.)*(yaup-yadown)/nazim
          azim0= (pi/180)*yadown+0.5*dlazim
         do 160 n=1,nazim
          azim=azim0+(n-1)*dlazim
c          write(6,*) 'Trace ',n
          CALL TIMSPU(IPACT,IPARM(IPACT),DOMEGA,TSHIFT,K,J,
     &                azim,NX,LX,MX)
          call wrbuf(71,x,nx)
          DO 168 JHS=LF0,LF
           VALUE=X(JHS,1)
           AMPMAX=AMAX1(AMPMAX,ABS(VALUE))
 168      CONTINUE
 160     continue
         call enfbuf(71)
         call rwdbuf(71)
         IF(AMPMAX.NE.0.0) THEN
          if (log_ts) then
           FCTOR=ABS(YUP-YDOWN)/(nazim*dbstack)
           troff=20.0*alog10(ampmax)-dbstack
          else
           FCTOR=ABS(YUP-YDOWN)/(nazim*AMPMAX)
          end if
         ELSE
          FCTOR=1.0
         END IF
         FCTOR=SCALEFACT(1)*SIGN(FCTOR,YUP-YDOWN)
c         write(21,*) 'scaling factor:',FCTOR
          dlazim=(yaup-yadown)/nazim
          azim0= yadown+0.5*dlazim
         do 181 n=1,nazim
          azim=azim0+(n-1)*dlazim
          call rdbuf(71,x,nx)
          IF (IRET.EQ.16) THEN
           OFFSET=azim
           FACTOR=FCTOR
           CALL OUSTACK(LF0,LF,TMIN,DT,OFFSET,FACTOR,IPACT,PX,MODULO)
          ELSE if (TRACEFORM.EQ.'GLD') THEN
c           CALL GLDWRI(0,K-1,CFF(1,1),0,NX-1)
            CALL GLDWRI(0,k-1,CFF(1,1),lf0-1,lf-1)
           ELSE if (TRACEFORM.EQ.'MAT') THEN
             if (n.eq.nazim) then
               call matwrite(CFF(lf0,1),lf-lf0+1,1)
             else
               call matwrite(CFF(lf0,1),lf-lf0+1,0)
             endif
          ELSE IF (TRACEFORM.EQ.'SDR') THEN
           CALL SDRWRI(CFF,NX)
          ELSE IF (TRACEFORM.EQ.'CFS') THEN
           CALL CFSWRI(CFF,NX)
          ELSE IF (TRACEFORM.EQ.'BIN') THEN
           CALL BINWRI(CFF,lf-lf0+1)
          else
           CALL ASCWRI(rangem,rdc(j),azim,tmin,lf-lf0+1,CFF(1,1))
          END IF
181       CONTINUE
         call clsbuf(71)
         IF (IRET.NE.16) THEN
          IF (TRACEFORM.EQ.'GLD') THEN
            CALL GLDCLS
          ELSE IF (TRACEFORM.EQ.'MAT') THEN
             print *,"wrote MAT FILE"
          ELSE IF (TRACEFORM.EQ.'SDR') THEN
            CALL VMOV(RDC(J),0,FAC,1,nazim)
            CALL VRAMP(azim0,dlazim,ARG,1,nazim)
            CALL VSMUL(ARG,1,1E3,ARG,1,nazim)
            CALL SDRCLS(ARG(1),FAC(1),nazim,CFF,NX,DT)
          ELSE IF (TRACEFORM.EQ.'CFS') THEN
            CALL VMOV(RDC(J),0,FAC,1,nazim)
            CALL VRAMP(azim0,dlazim,ARG,1,nazim)
            CALL VSMUL(ARG,1,1E3,ARG,1,nazim)
            CALL CFSCLS(ARG(1),FAC(1),nazim,CFF,NX,DT)
          ELSE IF (TRACEFORM.EQ.'BIN') THEN
           CALL BINCLS
          else
            call asccls
          END IF
         END IF
 180     continue
175     CONTINUE
      ELSE IF (TPLOT.EQ.'I') THEN
*     Individual timeseries
       IF (IRET.EQ.11.or.iret.eq.12) THEN
        CALL OPNPLP
       ELSE
        PLPFILE=.FALSE.
       END IF
        DO 240 JR=IRLOW,IRHIGH
        RANGE=R0+(JR-1)*RSPACE
        RANGEM=RANGE*1000.0
        IF (C0.GT.0.0) THEN
          TSHIFT=RANGEM/C0+TMIN
        ELSE
          TSHIFT=TMIN
        ENDIF
        DO 238 JD=IDLOW,IDHIGH
c >>>   open temporary trace file
          nbuf=memlft()
          call opnbuf(71,nx,1,nbuf)
          CALL TIMSPU(IPACT,IPARM(IPACT),DOMEGA,TSHIFT,JR,JD,
     &                THETA,NX,LX,MX)
          call wrbuf(71,x,nx)
          call enfbuf(71)
c         WRITE(6,*) 'CALLING PLPUPU'
          if (iret.eq.11) then
            CALL PLPUPU(NX,LF0,LF,TMIN,DT,TITLE,IPARM(IPACT),
     &                PX,MODULO,XAXIS,YAXIS,
     &                XLEFT,XRIGHT,XINC,RANGE,SD,RDC(JD),C0,THETAD)
          else if (iret.eq.12) then
           if (deconv) then
c >>> multiply by carrier
            omeg_c=freqs*2*pi
            do i=1,nx
             tt=tshift+(i-1)*dt
             x(i,1)=real(x(i,1)*exp(ai*omeg_c*tt))
            end do
           end if
           CALL RFFT(CFF,NX,1)
           CALL RFFTSC(CFF,NX,1,1)
           DLFRQP=1E0/(NX*DT)
           CALL PLFRSP(0E0,DLFRQP,NX/2,TITLE,PX,MODULO,XAXIS,YAXIS,
     &                 RANGE,SD,RDC(JD),THETAD)
          else if (iret.eq.13) then
           call rwdbuf(71)
           if (TRACEFORM.EQ.'GLD') THEN
c           CALL GLDMAKE(TITLE,1E0/DT,1,1,NX,'f','r')
            CALL GLDMAKE(TITLE,1E0/DT,1,1,lf-lf0+1,'f','r')
         ELSE if (TRACEFORM.EQ.'MAT') THEN
            call fmatopen
            call matcsetup(lenstr(TITLE),PADFLAG
     &           ,REM,5,"TITLE")
            call matcwrite(title,lenstr(title)
     &           ,PADFLAG,REM)
            call matcsetup(lenstr(PLOTTYPE)
     &           ,PADFLAG,REM,4,"TYPE")
            call matcwrite(plottype,lenstr(plottype)
     &           ,PADFLAG,REM)
            ndim = 2
            idim(1) = 1
            idim(2) = 1
            call matsetup(ndim,idim,PADFLAG,5,"DEPTH")
            call matwrite(rdc(jd),1,PADFLAG)
c prepare range array
            call matsetup(ndim,idim,PADFLAG,5,"RANGE")
            call matwrite(rangem,1,PADFLAG)
c prepare az array
            call matsetup(ndim,idim,PADFLAG,5,"ANGLE")
            call matwrite(thetad,1,PADFLAG)
            call matsetup(ndim,idim,PADFLAG,6,"TMSHFT")
            call matwrite(tshift,1,PADFLAG)
            call matsetup(ndim,idim,PADFLAG,6,"DELTAT")
            call matwrite(dt,1,PADFLAG)
            idim(1) = lf-lf0+1
            idim(2) = 1
            call matsetup(ndim,idim,PADFLAG,4,"DATA")

           ELSE if (TRACEFORM.EQ.'SDR') THEN
            CALL SDRPREP
           ELSE if (TRACEFORM.EQ.'CFS') THEN
            CALL CFSPREP
           ELSE IF (TRACEFORM.EQ.'BIN') THEN
            CALL BINMAKE(tshift,1E0/DT,1,1,lf-lf0+1)
           else
            CALL ASCMAKE(TITLE,plottype,1E0/DT,1,1,lf-lf0+1)
           END IF
c >>> write trace file
           if (TRACEFORM.EQ.'GLD') THEN
c            CALL GLDWRI(0,0,CFF(1,1),0,NX-1)
             CALL GLDWRI(0,0,CFF(1,1),lf0-1,lf-1)
           ELSE if (TRACEFORM.EQ.'MAT') THEN
               call matwrite(CFF(lf0,1),lf-lf0+1,1)
           ELSE IF (TRACEFORM.EQ.'SDR') THEN
            CALL SDRWRI(CFF,NX)
           ELSE IF (TRACEFORM.EQ.'CFS') THEN
            CALL CFSWRI(CFF,NX)
           ELSE IF (TRACEFORM.EQ.'BIN') THEN
            CALL BINWRI(CFF,lf-lf0+1)
           else
            CALL ASCWRI(rangem,rdc(jd),azim,tmin,lf-lf0+1,CFF(1,1))
           END IF
c >>> close trace file
           IF (TRACEFORM.EQ.'GLD') THEN
            CALL GLDCLS
          ELSE IF (TRACEFORM.EQ.'MAT') THEN
             print *,"wrote MAT FILE"
           ELSE IF (TRACEFORM.EQ.'SDR') THEN
            CALL VMOV(RDC(J),0,FAC,1,nazim)
            CALL VRAMP(azim0,dlazim,ARG,1,nazim)
            CALL VSMUL(ARG,1,1E3,ARG,1,nazim)
            CALL SDRCLS(ARG(1),FAC(1),nazim,CFF,NX,DT)
           ELSE IF (TRACEFORM.EQ.'CFS') THEN
            CALL VMOV(RDC(J),0,FAC,1,nazim)
            CALL VRAMP(azim0,dlazim,ARG,1,nazim)
            CALL VSMUL(ARG,1,1E3,ARG,1,nazim)
            CALL CFSCLS(ARG(1),FAC(1),nazim,CFF,NX,DT)
           ELSE IF (TRACEFORM.EQ.'BIN') THEN
            CALL BINCLS
           else
            call asccls
           END IF
          end if
          call clsbuf(71)
238     CONTINUE
240     CONTINUE
      ELSE IF (TPLOT.EQ.'S') THEN
*     Source pulses and spectra
        CALL OPNPLP
        LF0=1
        LF=IFIX(ABS(XALEFT-XARIGHT)/DT+1.5)
        DLFRQP=1.0/(DT*NX)
        ISTYP=ISTMP
*     Calculate the existing source pulses with normalized frequency spectra

        CALL SPULSE(FREQS)
c        WRITE(6,*) 'CALLING PLPUTMP'
        CALL PLPUPU(NX,1,LF-LF0+1,0.0,DT,TITLEA,0,PX,MODULO,XAAXIS,
     1   YAAXIS,0.0,ABS(XARIGHT-XALEFT),XAINC,0.001,SD,SD,C0TMP,FQTMP)
        YBAXIS=YAAXIS
        XBAXIS=XAAXIS
        TITLEB=TITLEA
        CALL PLFRTMP(0.,DLFRQP,NX/2,TITLEB,PX,MODULO,XBAXIS,YBAXIS,
     1               FMIN,FMAX)

      ELSE IF (TPLOT.EQ.'C') THEN
*     Snap shot plots
       dtframe=(tshmax-tshmin)/(nframe-1)
       ipart=0
       do iframe=1,nframe
        tshift=tshmin+(iframe-1)*dtframe
C        IPART=TSHIFT*100.
        IPART=IPART+1
        IJJ=LFILNM
        basename=FILEN(1:IJJ)//'_'//plottype//'_'
     &      //CHAR(48+IPART/10)//CHAR(48+MOD(IPART,10))

        XSCALE=ABS(XRIGHT-XLEFT)/XAXIS
        YSCALE=ABS(YUP-YDOWN)/YAXIS
        XL=R0+(NPLOTS-1)*RSPACE

        IF(KSNAPSH1.EQ.1.OR.ZMAX.NE.-99.) THEN
          BIGVAL=AMAX1(ABS(ZMIN),ABS(ZMAX))
          ZSTEP=2.*BIGVAL/REAL(NUMINT)
        END IF

        write(titlec,'(a,a,a,a,i4,a,f6.4,a)') 
     &        title(1:lenstr(title)),' - ',
     &        plottype,' - Az=',nint(thetad),' - T=',tshift,' s' 

        CALL DEMSNAPS(XLEFT,XRIGHT,XAXIS,XINC,XSCALE,
     1                 YDOWN,YUP,YAXIS,YINC,YSCALE,
     2                 ZMin,ZMax,ZSTEP,IPACT,XL,theta)
c
c >>> add shaded polygon
c
        if (shading) then
         open(72,file=shdver,status='old',form='formatted')
 7855    read(72,'(a)',end=7858,err=7858) dummy
         lll=lenstr(dummy)
         write(55,'(a)') dummy(1:lll)
         go to 7855
 7858    close(72)
        end if
c
c *** make contour plots in detached process
c
        CLOSE(55,STATUS='KEEP')
        CLOSE(17,STATUS='KEEP')
        COMMAND = 'cplot '//basename
        open (56,file='xect',status='unknown',form='formatted')
        write(56,'(a)') '#!/bin/csh'
        write(56,'(a)') command
        close(56,status='keep')
        call system('chmod +x xect')
        call system('csh xect')
        call system('rm -f xect')

        if (savefile.eq.'Y') then
         WRITE(6,'(1H ,3(/1H ,A))') 'SAVED IN FILES: ',
     &                                CDRNAME,BDRNAME
        else
         call opncdr
         close(55,status='delete')
         close(17,status='delete')
         close(4,status='delete')
        end if
       end do

      ELSE IF (TPLOT.EQ.'H') THEN
*     Snap shot plots
       dtframe=(tshmax-tshmin)/(nframe-1)
       ipart=0
       do iframe=1,nframe
        tshift=tshmin+(iframe-1)*dtframe
C        IPART=TSHIFT*100.
        IPART=IPART+1
        IJJ=LFILNM
        basename=FILEN(1:IJJ)//'_'//plottype//'_'
     &      //CHAR(48+IPART/10)//CHAR(48+MOD(IPART,10))

        XSCALE=ABS(XRIGHT-XLEFT)/XAXIS
        YSCALE=ABS(YUP-YDOWN)/YAXIS
        XL=R0+(NPLOTS-1)*RSPACE

        IF(KSNAPSH1.EQ.1.OR.ZMAX.NE.-99.) THEN
          BIGVAL=AMAX1(ABS(ZMIN),ABS(ZMAX))
          ZSTEP=2.*BIGVAL/REAL(NUMINT)
        END IF

        write(titlec,'(a,a,a,a,f6.1,a,f6.3,a)') 
     &        title(1:lenstr(title)),' - ',
     &        plottype,' - D=',pdepth,' - T=',tshift,' s' 

        CALL HORSNAPS(nazim_tl,irdepth,shading,shdhor,
     1                 XRIGHT,XAXIS,XINC,XSCALE,
     2                 ZMin,ZMax,ZSTEP,IPACT,XL)
c
c >>> add shaded polygon
c
        if (shading) then
         open(72,file=shdhor,status='old',form='formatted')
 6855    read(72,'(a)',end=6858,err=6858) dummy
         lll=lenstr(dummy)
         write(55,'(a)') dummy(1:lll)
         go to 6855
 6858    close(72)
        end if
c
c *** make contour plots in detached process
c
        CLOSE(55,STATUS='KEEP')
        CLOSE(17,STATUS='KEEP')

        if (.not.mtvout) then
         COMMAND = 'cplot '//basename
        else
         lfiln=LENSTR(basename)
         if (makeplot) then
          command='plotmtv -bg white -fg black '//
     &            '-noframe -nodate '//basename(1:lfiln)//'.mtv'
         else
          command='ls -lg '//basename(1:lfiln)//'.mtv'
         end if
        end if

        open (56,file='xect',status='unknown',form='formatted')
        write(56,'(a)') '#!/bin/csh'
        write(56,'(a)') command
        close(56,status='keep')
        call system('chmod +x xect')
        call system('csh xect')
        call system('rm -f xect')

        if (savefile.eq.'Y') then
         WRITE(6,'(1H ,3(/1H ,A))') 'SAVED IN FILES: ',
     &                                CDRNAME,BDRNAME
        else
         call opncdr
         close(55,status='delete')
         close(17,status='delete')
         close(4,status='delete')
        end if
       end do

      ELSE IF (TPLOT.EQ.'c') THEN
*     TL contour plots
        IPART=IPART+1
        IJJ=LFILNM
        basename=FILEN(1:IJJ)//'_'//plottype//'_'
     &      //CHAR(48+IPART/10)//CHAR(48+MOD(IPART,10))
        write(6,*) 'xcaxis,ycaxis=',xcaxis,ycaxis
        XSCALE=ABS(XCRIGHT-XCLEFT)/XCAXIS
        YSCALE=ABS(YCUP-YCDOWN)/YCAXIS
        XL=R0+(NPLOTS-1)*RSPACE
        write(titlec,'(a,a,a,a,i4)') title(1:lenstr(title)),' - ',
     &        plottype,' - Az=',nint(thetad) 
        write(6,*) 'calling tl_cont'
        CALL tl_cont(theta,freq,XCLEFT,XCRIGHT,XCAXIS,XCINC,XSCALE,
     1                 YCDOWN,YCUP,YCAXIS,YCINC,YSCALE,
     2                 tzmin,tzmax,tzinc,IPACT,XL)
        write(6,*) 'exiting tl_cont'
c
c >>> add shaded polygon
c
        if (shading) then
         write(6,*) '>>> Adding shading <<<'
         open(72,file=shdver,status='old',form='formatted')
 8855    read(72,'(a)',end=8858,err=8858) dummy
         lll=lenstr(dummy)
         write(55,'(a)') dummy(1:lll)
         go to 8855
 8858    close(72)
        end if
c
c *** make contour plots in detached process
c
        CLOSE(55,STATUS='KEEP')
        CLOSE(17,STATUS='KEEP')
        write(6,*) basename
c        COMMAND = 'cplot '//basename(1:lenstr(basename))
        write(command,'(a,a)') 'cplot ', basename(1:lenstr(basename))
        write(6,*) 'command=',command
        open (56,file='xect',status='unknown',form='formatted')
        write(56,'(a)') '#!/bin/csh'
        write(56,'(a)') command
        close(56,status='keep')
        write(6,*) 'calling system'
        call system('chmod +x xect')
        write(6,*) 'calling system'
        call system('csh xect')
        write(6,*) 'calling system'
        call system('rm -f xect')

        if (savefile.eq.'Y') then
         WRITE(6,'(1H ,3(/1H ,A))') 'SAVED IN FILES: ',
     &                                CDRNAME,BDRNAME
        else
         call opncdr
         close(55,status='delete')
         close(17,status='delete')
        end if

      ELSE IF (TPLOT.EQ.'h') THEN
*     TL contour plots
        IPART=IPART+1
        IJJ=LFILNM
        basename=FILEN(1:IJJ)//'_'//plottype//'_'
     &      //CHAR(48+IPART/10)//CHAR(48+MOD(IPART,10))

        XSCALE=2*XCRIGHT/XCAXIS
        XL=R0+(NPLOTS-1)*RSPACE
        write(titlec,'(a,a,a,a,i4)') title(1:lenstr(title)),' - ',
     &        plottype,' - Az=',nint(thetad) 

c       write(6,*) '>>>> Here the range-range contour plot is made, <<<'
c       write(6,*) 'nazim_tl,freq,irdepth=',nazim_tl,freq,irdepth
c       write(6,*) 'XCRIGHT,XCAXIS,XCINC,XSCALE=',
c     &             XCRIGHT,XCAXIS,XCINC,XSCALE
c       write(6,*) 'tzmin,tzmax,tzinc,IPACT,XL=',
c     &             tzmin,tzmax,tzinc,IPACT,XL
        CALL tl_h_cont(nazim_tl,freq,irdepth,shading,shdhor,
     1                 XCRIGHT,XCAXIS,XCINC,XSCALE,
     2                 tzmin,tzmax,tzinc,IPACT,XL)
c
c >>> add shaded polygon
c
         if (shading) then
          open(72,file=shdhor,status='old',form='formatted')
 8355     read(72,'(a)',end=8358,err=8358) dummy
          lll=lenstr(dummy)
          write(55,'(a)') dummy(1:lll)
          go to 8355
 8358     close(72)
         end if
c
c *** make contour plots in detached process
c
         CLOSE(55,STATUS='KEEP')
         CLOSE(17,STATUS='KEEP')

        if (.not.mtvout) then
         write(command,'(a,a)') 'cplot ', basename(1:lenstr(basename))
c         COMMAND = 'cplot '//basename
        else
         lfiln=LENSTR(basename)
        write(command,'(a,a,a)') 'plotmtv  -bg white -fg black '//
     &                           '-noframe -nodate ', 
     &                           basename(1:lfiln),'.mtv'
c         command='plotmtv '//basename(1:lfiln)//'.mtv'
        end if

        open (56,file='xect',status='unknown',form='formatted')
        write(56,'(a)') '#!/bin/csh'
        write(56,'(a)') command
        close(56,status='keep')
        call system('chmod +x xect')
        call system('csh xect')
        call system('rm -f xect')

        if (savefile.eq.'Y') then
         WRITE(6,'(1H ,3(/1H ,A))') 'SAVED IN FILES: ',
     &                                CDRNAME,BDRNAME
        else
         call opncdr
         close(55,status='delete')
         close(17,status='delete')
        end if

      ELSE IF (TPLOT.EQ.'f') THEN
c     TL vs freq plots
        CALL OPNPLP
        write(titlec,'(a,a,a,a,i4)') title(1:lenstr(title)),' - ',
     &        plottype,' - Az=',nint(thetad) 

        CALL tl_freq(theta,IDRANGE,IRDEPTH,
     1                 XFLEFT,XFRIGHT,XFAXIS,XFINC,
     1                 tymax,tymin,tyaxis,tyinc,IPACT)
      write(6,*) 'Making TL plot'
      ELSE IF (TPLOT.EQ.'r') THEN
c     TL vs range plots
        CALL OPNPLP
        write(titlec,'(a,a,a,a,i4)') title(1:lenstr(title)),' - ',
     &        plottype,' - Az=',nint(thetad) 

        CALL tl_range(theta,freq,IRDEPTH,
     1                 XCLEFT,XCRIGHT,XCAXIS,XCINC,
     1                 tymax,tymin,tyaxis,tyinc,IPACT)
      write(6,*) 'Making TL plot'
      ELSE IF (TPLOT.EQ.'a') THEN
c     TL vs range plots
        CALL OPNPLP
        write(titlec,'(a,a,a,a,i4)') title(1:lenstr(title)),' - ',
     &        plottype,' - Az=',nint(thetad) 

        CALL tl_dav(theta,freq,
     1                 XCLEFT,XCRIGHT,XCAXIS,XCINC,
     1                 tymax,tymin,tyaxis,tyinc,IPACT)
       write(6,*) 'Making TL plot'
      ELSE IF (TPLOT.EQ.'d') THEN
c     TL vs depth plots
        CALL OPNPLP
        write(titlec,'(a,a,a,a,i4)') title(1:lenstr(title)),' - ',
     &        plottype,' - Az=',nint(thetad) 

        CALL tl_depth(theta,freq,prange,IDRANGE,
     1                 YCUP,YCDOWN,YCAXIS,YCINC,
     1                 tymax,tymin,tyaxis,tyinc,IPACT)
      write(6,*) 'Making TL plot'
      ELSE
      END IF

       IF (PLPFILE) THEN
*     Indicate "end of file" in the 19-file
C     THIS IF BLOCK SHOULD BE COMMENTED IF INTERACTIVE PLOTTING DISABLED
 
        OPTION(2)='PLTEND'
        WRITE(19,170) OPTION
         REWIND(19)
         REWIND(20)
          CLOSE(19,STATUS='KEEP')
          CLOSE(20,STATUS='KEEP')
          if (mtvplp) then
           COMMAND = 'plp2mtv '
     &              //FILEN(1:LFILNM)//CHAR(48+IPLCNT/10)//
     &                           CHAR(48+MOD(IPLCNT,10))
          else
           COMMAND = 'mplot '
     &              //FILEN(1:LFILNM)//CHAR(48+IPLCNT/10)//
     &                           CHAR(48+MOD(IPLCNT,10))
          end if
          open (56,file='xect',status='unknown',form='formatted')
           write(56,'(a)') '#!/bin/csh'
           write(56,'(a)') command
           close(56,status='keep')
           call system('chmod +x xect')
           call system('csh xect')
           call system('rm -f xect')

         if (savefile.eq.'Y') then
          WRITE(6,'(1H ,3(/1H ,A))') 'SAVED IN FILES: ',PLPNAME,PLTNAME
          CLOSE(19,STATUS='KEEP')
          CLOSE(20,STATUS='KEEP')
         else
          COMMAND = 'rm -f '
     &              //FILEN(1:LFILNM)//CHAR(48+IPLCNT/10)//
     &                           CHAR(48+MOD(IPLCNT,10))//'.*'
           call system(command)
         end if
         IPLCNT=IPLCNT+1
         PLPFILE=.FALSE.
       ENDIF

      GO TO 3020                

3010  CONTINUE
c
c >>> write selected parameters to file
c
      rewind(89)
      write(89,'(a)') filename
      write(89,'(i5)') instyp
      write(89,'(a)') filenusp
      write(89,'(a)') plotopt
      write(89,'(a)') contopt
      write(89,'(a)') traceform
      write(89,'(i5)') nazim
      write(89,'(i5)') nazim_tl
      write(89,'(f15.6)') prange
      write(89,'(f15.6)') pdepth
      write(89,'(f15.6)') tzmin
      write(89,'(f15.6)') tzmax
      write(89,'(f15.6)') tzinc
      write(89,'(f15.6)') xcaxis
      write(89,'(f15.6)') ycaxis
      write(89,'(f15.6)') tymin
      write(89,'(f15.6)') tymax
      write(89,'(f15.6)') tyinc
      write(89,'(a)') decstr
      write(89,'(f15.6)') tshmin
      write(89,'(f15.6)') tshmax
      write(89,'(i5)') nframe
      write(89,'(a)') shdver
      write(89,'(a)') shdhor
      write(89,'(a)') logstr
      close(89)

       IF (PLPFILE) THEN
        OPTION(2)='PLTEND'
        WRITE(19,170) OPTION
170     FORMAT(1H ,2A6)
        IF(SAVEFILE.EQ.'Y') THEN
          CLOSE(19,STATUS='KEEP')
          CLOSE(20,STATUS='KEEP')
        ELSE
          CLOSE(19,STATUS='DELETE')
          CLOSE(20,STATUS='DELETE')
        END IF
       END IF

      WRITE(6,*)
      WRITE(6,*)'          ***  PULSPLOT STOP  ***'
      WRITE(6,*)
      WRITE(6,*)

      END
      SUBROUTINE OPNPLP
      INCLUDE 'compar.f'
      INCLUDE 'compul.f'
       character*80 infname
       IF(.NOT.PLPFILE) THEN
        PLPNAME=FILEN(1:LFILNM)//CHAR(48+IPLCNT/10)//
     &                           CHAR(48+MOD(IPLCNT,10))//'.plp'
        PLTNAME=FILEN(1:LFILNM)//CHAR(48+IPLCNT/10)//
     &                           CHAR(48+MOD(IPLCNT,10))//'.plt'
        infname=FILEN(1:LFILNM)//CHAR(48+IPLCNT/10)//
     &                           CHAR(48+MOD(IPLCNT,10))//'.inf'
        OPEN(19,FILE=PLPNAME,STATUS='UNKNOWN',FORM='FORMATTED')
        OPEN(20,FILE=PLTNAME,STATUS='UNKNOWN',FORM='FORMATTED')
        open(21,file=infname,status='UNKNOWN',FORM='FORMATTED')
        WRITE(19,*) MODULO,' MODULO'
        write(21,*) title
        PLPFILE=.TRUE.
       END IF
      RETURN
      END
      SUBROUTINE OPNCDR
      INCLUDE 'compar.f'
      INCLUDE 'compul.f'
       IF(.NOT.CDRFILE) THEN
        ll=lenstr(basename)
        CDRNAME=basename(1:ll)//'.cdr'
        BDRNAME=basename(1:ll)//'.bdr'
        ADRNAME=basename(1:ll)//'.adr'
        OPEN(55,FILE=CDRNAME,STATUS='UNKNOWN',FORM='FORMATTED')
        OPEN(17,FILE=BDRNAME,STATUS='UNKNOWN',FORM='FORMATTED')
        OPEN(4,FILE=ADRNAME,STATUS='UNKNOWN',FORM='FORMATTED')
        CDRFILE=.TRUE.
       END IF
      RETURN
      END
      SUBROUTINE AUTOAX(XMIN,XMAX,XLEFT,XRIGHT,XINC,XDIV,NXDIF)
C *** DETERMINE FACTOR
      ILOG=IFIX(ALOG10(XMAX-XMIN))
      IF ((XMAX-XMIN).LT.1.0) ILOG=ILOG-1
      IFAC=IFIX((XMAX-XMIN)/10.**ILOG)+1
      NXDIF=ILOG-1
      XDIV=10.**(-NXDIF)
C *** MAKE NICE AXIS
      XFAC=IFAC
      IF (IFAC.EQ.1) then
       XFAC=5
      else IF (IFAC.EQ.2) then
       XFAC=4
      else if (ifac.eq.3) then
       xfac=6
      else if (ifac.eq.4) then
       xfac=8
      else 
       xfac=ifac
      end if
      XINC=IFAC*10.**ILOG/XFAC
      IF (XMIN.GE.0E0) THEN
       XLEFT=IFIX(XMIN/XINC+0.01)*XINC
      ELSE
       XLEFT=-IFIX(-XMIN/XINC+0.99)*XINC
      END IF
      IF (XMAX.GE.0E0) THEN
       XRIGHT=IFIX(XMAX/XINC+0.99)*XINC
      ELSE
       XRIGHT=-IFIX(-XMAX/XINC+0.01)*XINC
      END IF
      RETURN
      END
      subroutine fmatopen()
      character*128 fname
      write(6,'(a)') 'Matlab file name (no extension)?'
      read(5,'(a)') fname
      fname=fname(1:lenstr(fname))//'.mat'
      call matopen(fname,lenstr(fname))
      return
      end


