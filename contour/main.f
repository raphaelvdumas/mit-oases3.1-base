      PARAMETER (NRANGE=1001,NZX=601,NZY=401,NZXY=NZX*NZY,
     & NPXYZ=100000,NLEV1=51, NDUMMY= 20)
      parameter (nbot_max = 5000, nbm= nbot_max+20)
      LOGICAL IB, BATCH
      INTEGER TTYPE,GTXTTY
      REAL LASPX, LASPY
      CHARACTER*3 NEW, ULC
      CHARACTER*3 XBTYPE,YBTYPE,FORM,EXTP,DEVICE,FONT,VUG,DEL,FAX,
     & NOWRT,ROT,BWCOL,LINEP,PACKGE, DUMMY(NDUMMY), LINCOL(8),
     & SDPLOT,RDPLOT,NCS,WDW,
     & ADD, OPENED, LASTP
      CHARACTER*80 TITLE,TITLEX,TITLEY
      CHARACTER*13 WDWMSG
      CHARACTER*80 OPTION,INPFILE,cbuf

      REAL*8 X, Y, S

      COMMON /BOTT/ XF(nbm), YF(nbm), NPBOTT, ISHADE, NPSH
      COMMON /MULBOTT/ LINES,numbot
      COMMON /DEV/ DEVICE
      COMMON /FLAGS/ ADD, OPENED, LASTP
      COMMON /FONTEX/ FONT
      COMMON /CHFLAG/ BWCOL, LINEP
      COMMON /HSFLAG/ IFIRST,ILAST,CYL,FOM,PRB,SEG,ISEG,
     &                IFR,SDFLAG,NCL
      COMMON /PARA/ LABPT,NSM,NDIV,CAY,NARC,NRNG,HGTPT,HGT,
     &              LABC(51),LWGT(51)
      COMMON /PARAC/ TITLE, SDPLOT, RDPLOT
      COMMON /PLT/ FACT,YASIZE,SCALF,SF,IB,ICOL
      COMMON /SH/ PF(440), QF(440), UF(440), VF(440)
      COMMON /SNAPSHOT/ SCFAC
      COMMON /STEP/ XSTEP, YSTEP
      COMMON /SWITCH/ NOWRT, ULC
      COMMON /XAX/ X1,XL,XLEFT,XRIGHT,XSCALE,XINC,DX,
     & X1PL,XLPL,NX,X1GRID,XLGRID,DIVX,XVAL(100),NXVAL
      COMMON /XAXC/ TITLEX, XBTYPE
      COMMON /XYS/ IND10(57,6)
      COMMON /YAX/ Y1,YL,YUP,YDOWN,YSCALE,YINC,DY,
     & Y1PL,YLPL,NY,Y1GRID,YLGRID,DIVY,YVAL(100),NYVAL
      COMMON /YAXC/ TITLEY, YBTYPE
      COMMON /ZAX/ ZMIN,ZMAX,ZINC,NLEV,ZLEV(NLEV1)
      DIMENSION Z(NZX,NZY),WRK(NPXYZ),XP(NPXYZ),YP(NPXYZ),
     & ZP(NPXYZ)
      EQUIVALENCE (WRK(1),XP(1))
      DIMENSION BUFFER(NRANGE),AA(NPXYZ),KN(NPXYZ),IM(NRANGE)
      EQUIVALENCE (BUFFER(1),IM(1))
      EQUIVALENCE (WRK(1),PF(1)), (WRK(441),QF(1)),
     & (WRK(881),UF(1)), (WRK(1321),VF(1))
c >>> define default raster package
      character*3 raspck
      data raspck /'MTV'/
      DATA SDFLAG,NCL/0.0,0/
      DATA CAY,NRNG/5.,5/
      DATA BWCOL,PACKGE,DEVICE,FORM,EXTP /'B/W','MIN',3*'   '/
      DATA WDW,WDWMSG/'   ','              '/
c      DATA WDW,WDWMSG/'DCW','DEC WINDOW   '/
CVWS      DATA WDW,WDWMSG/'VWS','VAX WINDOW   '/
      DATA OPENED, LASTP /'NO ','NO '/
      DATA DEL /'DEL'/
      DATA ROT, NOWRT, NCS, ULC/'   ','   ','   ','   '/
      DATA REVERS,CYL,FOM,PRB,SEG/0.0,0.0,0.0,0.0,0.0/
      DATA FONT,FAX/'SPX','A3 '/
      DATA ICOL/1/
      DATA LINCOL/'WHI','BLA','RED','GRE','BLU','CYA','MAG','YEL'/
      DATA LINEP/'   '/
      DATA XPIXEL,YPIXEL/0.3528,0.4233/
      DATA LINES,numbot/0,0/  
C
  200 FORMAT(A9, 20(1X,A3))
  220 FORMAT(1X,/,' *** VIEWGRAPH OPTION IS SET ***',/)
  300 FORMAT(1X,'  UNKNOWN OPTION CODE: ',A6,/,2X,' EXECUTION'
     & ,' IS TERMINATED ')
  400 FORMAT(///,1X,'  Do you want a UNIRAS plot?',/,
     & '  B/W : Black and white',/,
     & '  COL : Color',/,
     & '  LIN : Line Plot',/,
     & '  N   : None ' )
  410 FORMAT(A3)
  420 FORMAT(1X,/,' THE FOLLOWING PARAMETERS HAVE BEEN SET :',//,
     & '  PLOT PACKAGE    : ',A3,/,
     & '  COLOUR          : ',A3,/,
     & '  OUTPUT DEVICE   : ',A3)
  422 FORMAT(1X,/,' THE FOLLOWING PARAMETERS HAVE BEEN SET :',//,
     & '  PLOT PACKAGE    : ',A3,/,
     & '  COLOUR          : ',A3,/,
     & '  OUTPUT DEVICE   : ',A3,/,
     & '  WINDOW SYSTEM   : ',A3)
  430 FORMAT(1X,/,' THE FOLLOWING PARAMETERS HAVE BEEN SET :',//,
     & '  PLOT PACKAGE    : ',A3,/,
     & '  COLOUR          : ','B/W (Line plot)',/,
     & '  OUTPUT DEVICE   : ',A3)
  432 FORMAT(1X,/,' THE FOLLOWING PARAMETERS HAVE BEEN SET :',//,
     & '  PLOT PACKAGE    : ',A3,/,
     & '  COLOUR          : ','B/W (Line plot)',/,
     & '  OUTPUT DEVICE   : ',A3,/,
     & '  WINDOW SYSTEM   : ',A3)
  500 FORMAT(1H0,'Do you want a hardcopy?',
     &       /1H ,'Tektronix 4695 (A4)        : TEK',
     &       /1H ,'Tektronix 4695 Vugraph     : VUG',
     &       /1H ,'Colour Laser Printer (A4)  : CLA',
     &       /1H ,'Laser Printer (A4)         : LAS',
     &       /1H ,'Encaps Postscript file     : EPS',
     &       /1H ,'Postscript B/W (Landscape) : PSL',
     &       /1H ,'Postscript B/W (Portrait)  : PSP',
     &       /1H ,'Postscript COL (Portrait)  : CPS',
     &       /1H ,'None                       : N' )
  600 FORMAT(1X,//,' Z GRID IS NOW UNDERSAMPLED BECAUSE OF ',
     & 'PIXEL SIZE LIMITATIONS. ',/,
     & ' XAXIS = ',F8.2,' CM','    NEW NX = ',I3,/,
     & ' YAXIS = ',F8.2,' CM','    NEW NY = ',I3,/,
     & //)
  700 FORMAT(1X,//,' *** NO MORE CONTUR PLOTS IN FILE *** ')
C
      IB= BATCH()
C
      IFIRST=1
c      OPEN(UNIT=55,STATUS='OLD',SHARED,ERR=9000)
      call opfilr(55,ioerr)
      if (ioerr.ne.0) go to 9000
 1000 CONTINUE
1     READ(55,200,ERR=9100,END=9000) OPTION,(DUMMY(J),J=2,NDUMMY)
c >>> reset options
c >>> get default colour/bw
      call getenv('CON_BWCOL',cbuf)
      if (cbuf(1:3).eq.'COL'.or.cbuf(1:3).eq.'col') then
       bwcol='COL'
      else
       bwcol='B/W'
      end if
c >>> get default package
      call getenv('CON_PACKGE',cbuf)
      if (cbuf(1:3).eq.'MTV'.or.cbuf(1:3).eq.'mtv') then
       packge='MTV'
      else if (cbuf(1:3).eq.'UNI'.or.cbuf(1:3).eq.'uni') then
       packge='UNI'
      else
       packge='MIN'
      end if
c >>> get default device
      call getenv('CON_DEVICE',cbuf)
      IF( (cbuf(1:3) .EQ. 'VTT') .OR. (cbuf(1:3) .EQ. 'vtt') ) THEN
        DEVICE= 'VTT'
      else IF ((cbuf(1:3).EQ.'X11') .OR. (cbuf(1:3).EQ.'x11')) THEN
        DEVICE= 'VTT'
         WDW='DCW'
         WDWMSG='X-Windows    '
      ELSE IF( (cbuf(1:3) .EQ. 'TEK') .OR. (cbuf(1:3) .EQ. 'tek')) THEN
        DEVICE= 'TEK'
      ELSE IF( (cbuf(1:3) .EQ. 'T41') .OR. (cbuf(1:3) .EQ. 't41')) THEN
        DEVICE= 'T41'
      ELSE IF( (cbuf(1:3) .EQ. 'G41') .OR. (cbuf(1:3) .EQ. 'g41')) THEN
        DEVICE= 'G41'
      ELSE IF( (cbuf(1:3) .EQ. 'LAS') .OR. (cbuf(1:3).EQ.'las') ) THEN
        DEVICE= 'LAS'
      ELSE IF( (cbuf(1:3) .EQ. 'EPS') .OR. (cbuf(1:3) .EQ. 'eps')) THEN
        DEVICE= 'EPS'
      ELSE IF( (cbuf(1:3) .EQ. 'PSL') .OR. (cbuf(1:3) .EQ. 'psl')) THEN
        DEVICE= 'PSL'
      ELSE IF( (cbuf(1:3) .EQ. 'PSP') .OR. (cbuf(1:3) .EQ. 'psp')) THEN
        DEVICE= 'PSP'
      ELSE IF( (cbuf(1:3) .EQ. 'CPS') .OR. (cbuf(1:3) .EQ. 'cps')) THEN
        DEVICE= 'CPS'
      END IF
c >>> reset grid flag
      EXTP='   '
c
      linep='   '
      rot='   '
      ncs='   '
      revers=0
      numbot=0
      IF(OPTION(1:6) .EQ. ' EMPTY')   THEN
       IF(OPENED .EQ. 'YES')   CALL PLOT(0.0,0.0,999)
       STOP
      END IF
      DUMMY(1)=OPTION(7:9)
C
C   colour plot ?
C
      DO 1010   J=1, NDUMMY
      IF( (DUMMY(J) .EQ. 'COL') .OR. (DUMMY(J) .EQ. 'CLR') .OR.
     &    (DUMMY(J) .EQ. 'col') .OR. (DUMMY(J) .EQ. 'clr') )   THEN
        BWCOL='COL'
      END IF
 1010 CONTINUE
C
C   UNIRAS line plot ?
C
      DO 1011   J=1, NDUMMY
      IF((DUMMY(J) .EQ. 'LIN') .OR. (DUMMY(J) .EQ. 'lin'))   THEN
        LINEP='LIN'
      END IF
 1011 CONTINUE

C   REVERSAL OF COLOUR SCALE 
      DO 1012   J=1, NDUMMY
      IF( DUMMY(J) .EQ. 'REV' .OR. DUMMY(J) .EQ. 'rev' )   THEN
      REVERS=1.0
      END IF
 1012 CONTINUE

C   ROTATION OF PLOT
      DO 1013   J=1, NDUMMY
      IF( DUMMY(J) .EQ. 'ROT' .OR. DUMMY(J) .EQ. 'rot' )   THEN
        ROT='ROT'
      END IF
 1013 CONTINUE
C
C   SKIPPING PLOTTING OF COLOUR SCALE
      DO 1014   J=1, NDUMMY
      IF( DUMMY(J) .EQ. 'NCS' .OR. DUMMY(J) .EQ. 'ncs' )   THEN
       NCS='NCS'
      END IF
 1014 CONTINUE
C
C   CHECK FOR PLOT PACKAGE SUB-OPTION
C
      DO J=1, NDUMMY
       IF( DUMMY(J) .EQ. 'MIN' .OR. DUMMY(J) .EQ. 'min' )   PACKGE='MIN'
      end do
      DO 1020   J=1, NDUMMY
      IF( DUMMY(J) .EQ. 'UNI' .OR. DUMMY(J) .EQ. 'uni' )   PACKGE='UNI'
 1020 CONTINUE
      DO 1030   J=1, NDUMMY
      IF( DUMMY(J) .EQ. 'BIN' .OR. DUMMY(J) .EQ. 'bin' )   FORM='BIN'
 1030 CONTINUE
      DO 1040   J=1, NDUMMY
      IF( DUMMY(J) .EQ. 'GRD' .OR. DUMMY(J) .EQ. 'grd' )   EXTP='GRD'
 1040 CONTINUE
      DO   J=1, NDUMMY
      IF( DUMMY(J) .EQ. 'MTV' .OR. DUMMY(J) .EQ. 'mtv' )   packge='MTV'
      end do
C
C   CHECK FOR OUTPUT DEVICE SUB-OPTION
C
      DO 1050   J=1, NDUMMY
      IF( (DUMMY(J) .EQ. 'VTT') .OR. (DUMMY(J) .EQ. 'vtt') )       THEN
        DEVICE= 'VTT'
      else IF ((DUMMY(J).EQ.'X11') .OR. (DUMMY(J).EQ.'x11'))       THEN
        DEVICE= 'VTT'
         WDW='DCW'
         WDWMSG='X-Windows    '
      ELSE IF( (DUMMY(J) .EQ. 'TEK') .OR. (DUMMY(J) .EQ. 'tek') )  THEN
        DEVICE= 'TEK'
      ELSE IF( (DUMMY(J) .EQ. 'CLA') .OR. (DUMMY(J) .EQ. 'cla') )  THEN
        DEVICE= 'LAS'
        BWCOL='COL'
        PACKGE= 'UNI'
      ELSE IF( (DUMMY(J) .EQ. 'PHA') .OR. (DUMMY(J) .EQ. 'pha') )  THEN
        DEVICE= 'PHA'
        PACKGE= 'UNI'
      ELSE IF( (DUMMY(J) .EQ. 'PRX') .OR. (DUMMY(J) .EQ. 'prx') )  THEN
        DEVICE= 'PRX'
      ELSE IF( (DUMMY(J) .EQ. 'T41') .OR. (DUMMY(J) .EQ. 't41') )  THEN
        DEVICE= 'T41'
      ELSE IF( (DUMMY(J) .EQ. 'G41') .OR. (DUMMY(J) .EQ. 'g41') )  THEN
        DEVICE= 'G41'
      ELSE IF( (DUMMY(J) .EQ. 'LAS') .OR. (DUMMY(J).EQ.'las') )    THEN
        DEVICE= 'LAS'
        BWCOL='B/W'
      ELSE IF( (DUMMY(J) .EQ. 'EPS') .OR. (DUMMY(J) .EQ. 'eps') )  THEN
        DEVICE= 'EPS'
      ELSE IF( (DUMMY(J) .EQ. 'PSL') .OR. (DUMMY(J) .EQ. 'psl') )  THEN
        DEVICE= 'PSL'
      ELSE IF( (DUMMY(J) .EQ. 'PSP') .OR. (DUMMY(J).EQ.'psp') )    THEN
        DEVICE= 'PSP'
      ELSE IF( (DUMMY(J) .EQ. 'CPS') .OR. (DUMMY(J).EQ.'cps') )    THEN
        DEVICE= 'CPS'
      ELSE IF( (DUMMY(J) .EQ. 'CAL') .OR. (DUMMY(J).EQ.'cal') )    THEN
        DEVICE= 'CAL'
      END IF
 1050 CONTINUE
      IF(DEVICE.EQ.'   ')   THEN
       IF (IB)   THEN
        IF(BWCOL.EQ.'COL')   THEN
         DEVICE='CLA'
        ELSE
         DEVICE='LAS'
        END IF
       ELSE
        TTYPE=GTXTTY()
        IF( (TTYPE.EQ.240) .OR. (TTYPE.EQ.340) )   THEN
         DEVICE='VTT'
        ELSE IF (TTYPE.EQ.4105) THEN
         DEVICE='T41'
        ELSE IF (TTYPE.EQ.1011) THEN
         DEVICE='VTT'
         WDW='DCW'
         WDWMSG='X-Windows    '
        END IF
       END IF   
      END IF
C
C   CHECK FOR CHARACTERS FONT ON PLOTS
C
      DO 1090   J=1, NDUMMY
      IF(      DUMMY(J) .EQ. 'SPX' .OR. DUMMY(J) .EQ. 'spx' )   THEN
        FONT= 'SPX'
      ELSE IF( DUMMY(J) .EQ. 'CPX' .OR. DUMMY(J) .EQ. 'cpx' )   THEN
        FONT= 'CPX'
      ELSE IF( DUMMY(J) .EQ. 'DPX' .OR. DUMMY(J) .EQ. 'dpx' )   THEN
        FONT= 'DPX'
      END IF
 1090 CONTINUE
C
      DO 1100   J=1, NDUMMY
      IF(DUMMY(J) .EQ. 'VUG' .OR. DUMMY(J) .EQ. 'vug' )   THEN
       VUG='VUG'
       FAX='VG '
       WRITE(6,220)
      END IF
 1100 CONTINUE
      DO 1110   J=1, NDUMMY
      IF( DUMMY(J) .EQ. 'DEL' .OR. DUMMY(J) .EQ. 'del' )   DEL='DEL'
 1110 CONTINUE
      DO 1120   J=1, NDUMMY
      IF( DUMMY(J) .EQ. 'FA3' .OR. DUMMY(J) .EQ. 'fA3' )  FAX='A3 '
      IF( DUMMY(J). EQ. 'SEP' .OR. DUMMY(J) .EQ. 'sep' )  FAX='A3S'
      IF( DUMMY(J) .EQ. 'SEG' .OR. DUMMY(J) .EQ. 'seg' )  SEG=1.0
 1120 CONTINUE
      DO 1130   J=1, NDUMMY
      DO 1130   I=1, 8
      IF(DUMMY(J) .EQ. LINCOL(I))   THEN
      ICOL=I-1
      GO TO 1140
      END IF
 1130 CONTINUE
 1140 CONTINUE
C
C   FLAG FOR ADDITION OR SUBTRACTION OF CYLINDRICAL SPREADING
      DO 1150   J=1, NDUMMY
      IF( DUMMY(J) .EQ. 'CYL' .OR. DUMMY(J) .EQ. 'cyl' )   THEN
      CYL=1.0
      GO TO 1160
      END IF
 1150 CONTINUE
 1160 CONTINUE
C
C   FLAG FOR DISPLAY OF STATISTICS BASED ON FIGURE OF MERIT
C
      DO 1170   J=1, NDUMMY
      IF( DUMMY(J) .EQ. 'FOM' .OR. DUMMY(J) .EQ. 'fom' )   THEN
      FOM=1.0
      GO TO 1180
      END IF
 1170 CONTINUE
 1180 CONTINUE
C
      DO 1171   J=1, NDUMMY
      IF( DUMMY(J) .EQ. 'PRB' .OR. DUMMY(J) .EQ. 'prb' )   THEN
      PRB=1.0
      GO TO 1181
      END IF
 1171 CONTINUE
 1181 CONTINUE
C
C
C   FLAG FOR DISABLING THE WRITING OF TEXT ON THE UPPER WRITE HAND
C   CORNER OF PLOT
C
      DO 1190   J=1, NDUMMY
      IF( DUMMY(J) .EQ. 'NWR' .OR. DUMMY(J) .EQ. 'nwr' )   THEN
      NOWRT= 'NWR'
      GO TO 1200
      END IF
 1190 CONTINUE
 1200 CONTINUE
C
C
C   FLAG FOR DISABLING THE DRAWING OF CONTOUR LINES ON COLOUR PLOTS
C
      DO 1192   J=1, NDUMMY
      IF( DUMMY(J) .EQ. 'NCL' .OR. DUMMY(J) .EQ. 'ncl' )   THEN
       NCL=1
       GO TO 1220
      END IF
 1192 CONTINUE
 1220 CONTINUE

C   FLAG FOR OVERPLOTTING OF CONTOUR LINES FROM DIFFERENT CASES

      DO 1240   J=1, NDUMMY
      IF( DUMMY(J) .EQ. 'ADD' .OR. DUMMY(J) .EQ. 'add' )   THEN
       ADD='YES'
       LASTP= 'NO '
       GO TO 1260
      END IF
 1240 CONTINUE
      ADD= 'NO '
      LASTP= 'YES'
 1260 CONTINUE

C
C   TEXT ON UPPER LEFT HAND CORNER ?
C
      DO 1270  J=1, NDUMMY
      IF((DUMMY(J) .EQ. 'ULC') .OR. (DUMMY(J) .EQ. 'ulc'))   THEN
        ULC= 'ULC'
      END IF
 1270 CONTINUE


C
C   CHECK OF CONSISTENCY OF DEFAULT SUB-OPTIONS.
C
c      IF( DEVICE .EQ. 'LAS' .OR. DUMMY(J) .EQ. 'las' )    BWCOL='B/W'

c      IF(VUG.EQ.'VUG')   THEN
c       PACKGE='UNI'
c       DEVICE='TEK'
c      BWCOL='COL'
c      ELSE IF( BWCOL .EQ. 'COL' )   THEN
c       PACKGE='UNI'
c       IF(( DEVICE. EQ. 'PRX' ) .OR.
c     &    ( DEVICE .EQ. 'CAL'))
c     & DEVICE='TEK'
c      ELSE IF( DEVICE .EQ. 'CAL' )   THEN
c       PACKGE='MIN'
c       BWCOL='B/W'
c      END IF
C
C
      WRITE(6,420) PACKGE,BWCOL,DEVICE
C
C
CC ********************************************************
C   LOOP FOR CONDR, AMBDR, TDPEN, AND CONUS OPTION
C
      IF ((OPTION(1:5).EQ.'CONDR') .OR.
     &    (OPTION(1:5).EQ.'CONUS') .OR.
     &    (OPTION(1:5).EQ.'AMBDR') .OR.
     &    (OPTION(1:5).EQ.'TDPEN')     )   THEN

       CALL ICONDR(SD,NPX,NPY,NPXRD,NPYRD,NXIND,NXKAB,
     & NZX,NZY,Z,FREQ,EXTP,FORM,BUFFER,NRANGE)

       IF(FOM.GT.0.0)   GO TO 2000
        IF( (ABS(X1GRID - X1) .GT. ABS(1.0E-3*DX)) .OR. 
     &      (ABS(XLGRID - XL) .GT. ABS(1.0E-3*DX)) .OR.
     &      (ABS(Y1GRID - Y1) .GT. ABS(1.0E-3*DY)) .OR. 
     &      (ABS(YLGRID - YL) .GT. ABS(1.0E-3*DY)) )   THEN
        CALL PREGRD(Z,NPX,NPY,NPXRD,NPYRD,XP,YP,ZP,INDEX,NPXYZ)
        write(6,*) ' WARNING: the input data is being gridded '
        CALL ZGRID(AA,KN,IM,Z,NX,NY,X1GRID,XLGRID,Y1GRID,DX,DY,
     &  XP,YP,ZP,INDEX,CAY,NRNG)
       ELSE
        IF((NX.NE.NPX).OR.(NY.NE.NPY))   THEN
         IF((NX.LE.NPX).AND.(NY.LE.NPY))   THEN
          LU=22
          OPEN(UNIT=LU,STATUS='SCRATCH',FORM='UNFORMATTED')
          CALL SAMPLE(Z,NPX,NPY,NX,NY,X1GRID,XLGRID,Y1GRID,YLGRID,
     &                LU)
          CALL ORDER(Z,NX,NY,LU)
          CLOSE(LU)
         ELSE
          WRITE(6,*) ' THE GRID CAN ONLY BE INTERPOLATED '
          STOP
         END IF
        END IF
       END IF
       NPX=NX
       NPY=NY
       GO TO 2000
      END IF
C
      IF (OPTION(1:5).EQ.'CONTR')   THEN

       EXTP='GRD'
       CALL ICONTR(SD,NPX,NPY,NPXRD,NPYRD,NXIND,NXKAB,
     & NZX,NZY,Z,FREQ,EXTP,FORM,BUFFER,NRANGE,RD)
       CALL CALCXY(Z,ZP,XP,YP,NPX,NPY)
       INDEX=NPX*NPY
        write(6,*) ' WARNING: the input data is being gridded '
       CALL ZGRID(AA,KN,IM,Z,NX,NY,X1GRID,XLGRID,Y1GRID,DX,DY,
     & XP,YP,ZP,INDEX,CAY,NRNG)

       GO TO 2000
      END IF
C
C  ********************************************************
C   LOOP FOR EXPDR OPTION
C
      IF(OPTION(1:5).EQ.'EXPDR') THEN
       CALL IEXPDR(SD,INDEX,NXIND,NXKAB,NZX,NZY,FREQ,XP,YP,ZP)
       write(6,*) ' WARNING: the input data is being gridded '
       CALL ZGRID(AA,KN,IM,Z,NX,NY,X1GRID,XLGRID,YLGRID,DX,-DY,XP,
     & YP,ZP,INDEX,CAY,NRNG)
       GOTO 2000
      END IF
C
C *********************************************************
C  LOOP FOR CONFR, INPUT, STATIS, ERROR, DIFFER OPTION
C
      IF( OPTION(1:5).EQ.'CONFR'
     & .OR.OPTION(1:5).EQ.'CONDA'
     & .OR.OPTION(1:5).EQ.'CONFT'
     & .OR.OPTION(1:5).EQ.'INPUT'
     & .OR.OPTION(1:6).EQ.'STATIS'
     & .OR.OPTION(1:5).EQ.'ERROR'
     & .OR.OPTION(1:6).EQ.'DIFFER'
     & ) THEN
       CALL ICONFR(XP,YP,ZP,NPXYZ,NZX,NZY,NT,BUFFER,SD,RD,FORM)
       DX=(XLGRID-X1GRID)/(NX-1)
       DY=(YLGRID-Y1GRID)/(NY-1)
       write(6,*) ' WARNING: the input data is being gridded '
       CALL ZGRID(AA,KN,IM,Z,NX,NY,X1GRID,XLGRID,Y1GRID,DX,DY,XP,YP,
     & ZP,NT,CAY,NRNG)
       GOTO 2000
      END IF
C
C  ********************************************************
C   LOOP FOR PAREQ OPTION
C
      IF( (OPTION(1:5).EQ.'PAREQ') 
     & .OR.OPTION(1:5).EQ.'IFD  ')   THEN
       OPTION(10:10)=','
       OPTION(11:13)=DUMMY(2)
       INQUIRE(UNIT=55,NAME=INPFILE)
       DO 1300 J= 80, 1, -1
       IF(INPFILE(J:J) .NE. '.')   GO TO 1300
       IF(INPFILE(J+1:J+3) .EQ. 'DAT')   THEN
        stop 'STOP: IPEOLD not implemented'
c        CALL IPEOLD(Z,NZX,NZY,FREQ,SD,FORM)
        NPX=NX
        NPY=NY
       ELSE 
        CALL IPAREQ(SD,NPX,NPY,NPXRD,NPYRD,NXIND,NXKAB,
     &  NZX,NZY,Z,FREQ,EXTP,FORM,BUFFER,NRANGE)
       END IF
       GO TO 1400
 1300  CONTINUE
       WRITE(6,*) ' ERROR IN SEARCHING INPUT DATA FILE NAME '
       WRITE(6,*) INPFILE
       STOP
 1400  CONTINUE
       IF( (ABS(X1GRID - X1) .GT. ABS(1.0E-3*DX)) .OR. 
     &     (ABS(XLGRID - XL) .GT. ABS(1.0E-3*DX)) .OR.
     &     (ABS(Y1GRID - Y1) .GT. ABS(1.0E-3*DY)) .OR. 
     &     (ABS(YLGRID - YL) .GT. ABS(1.0E-3*DY)) )   THEN
        CALL PREGRD(Z,NPX,NPY,NPXRD,NPYRD,XP,YP,ZP,INDEX,NPXYZ)
        write(6,*) ' WARNING: the input data is being gridded '
        CALL ZGRID(AA,KN,IM,Z,NX,NY,X1GRID,XLGRID,Y1GRID,DX,DY,XP,YP,
     &  ZP,INDEX,CAY,NRNG)
       ELSE
        IF((NX.NE.NPX).OR.(NY.NE.NPY))   THEN
         IF((NX.LE.NPX).AND.(NY.LE.NPY))   THEN
          LU=22
          OPEN(UNIT=LU,STATUS='SCRATCH',FORM='UNFORMATTED')
          CALL SAMPLE(Z,NPX,NPY,NX,NY,X1GRID,XLGRID,Y1GRID,YLGRID,
     &                LU)
          CALL ORDER(Z,NX,NY,LU)
          CLOSE(LU)
         ELSE
          WRITE(6,*) ' THE GRID CAN ONLY BE INTERPOLATED '
          STOP
         END IF
        END IF
       END IF
       NPX=NX
       NPY=NY
       GOTO 2000
      END IF
C
C  *********************************************
C  LOOP FOR CONSV OPTION
C
      IF(OPTION(1:5) .EQ. 'CONSV')   THEN
       CALL ICONSV(XP,YP,ZP,NZX,NZY,NT)
       DX=(XLGRID-X1GRID)/(NX-1)
       DY=(YLGRID-Y1GRID)/(NY-1)
       write(6,*) ' WARNING: the input data is being gridded '
       CALL ZGRID(AA,KN,IM,Z,NX,NY,X1GRID,XLGRID,Y1GRID,DX,DY,XP,YP,
     & ZP,NT,CAY,NRNG)
       GOTO 2000
      END IF
C
      WRITE(6,300) OPTION(1:5)
      STOP

2000  CONTINUE


      NSMOOT=NSM
      XSAVE=X1PL
      YSAVE=Y1PL
C
      IF(PACKGE.EQ.'MIN')   THEN
C      LINE CONTOUR PLOT
       CALL SMOOTH(Z,NX,NY,NSMOOT)
       NSMOOT=0
CCCCCCC       IF(NPBOTT .GE. 2)   CALL WINDOW
       CALL BW(Z,FREQ,SD,RD,OPTION,XP,YP,ZP)
       IF(OPENED .EQ. 'YES')   GO TO 1000
C
       IF (.NOT.IB) THEN
        WRITE(6,'(a)') 'Do you want an raster plot?'
        write(6,'(a)') 'UNIRAS:        U/u'
        write(6,'(a)') 'PLOTMTV:       M/m'
        write(6,'(a)') 'None:          N/n'

        READ(5,410) NEW
        IF(NEW(1:1).EQ.'m' .OR. NEW(1:1).EQ.'M')   THEN
         PACKGE='MTV'
         BWCOL='COL'
         CALL BACKBOTT
        else IF(NEW(1:1).EQ.'U' .OR. NEW(1:1).EQ.'u')   THEN
         PACKGE='UNI'
         BWCOL='COL'
         CALL BACKBOTT
        END IF
       END IF
      END IF
C
      IF(PACKGE.EQ.'UNI')   THEN
       TEKPX=0.0847
       TEKPY=0.0847
       VTTPX=0.3379
       VTTPY=0.3380
       T41PX=0.3379
       T41PY=0.3380
       PHAPX=0.0847
       PHAPY=0.0847
       PS3PX=0.0847
       PS3PY=0.0847
       PS4PX=0.0847
       PS4PY=0.0847
       LASPX=0.2510
       LASPY=0.2510
  555  CONTINUE

C   PIXEL SIZE DEFINITION

       IF(LINEP .EQ. 'LIN')   THEN
        XPIXEL=1.0E-4
        YPIXEL=1.0E-4
        BWCOL='B/W'
       ELSE
        IF(DEVICE.EQ.'TEK')   THEN
         XPIXEL=TEKPX
         YPIXEL=TEKPY
        ELSE IF(DEVICE.EQ.'VTT')   THEN
         XPIXEL=VTTPX
         YPIXEL=VTTPY
        ELSE IF(DEVICE.EQ.'PSP')   THEN
         XPIXEL=PS3PX
         YPIXEL=PS3PY
        ELSE IF(DEVICE.EQ.'CPS')   THEN
         XPIXEL=PS3PX
         YPIXEL=PS3PY
        ELSE IF(DEVICE.EQ.'PSL')   THEN
         XPIXEL=PS4PX
         YPIXEL=PS4PY
        ELSE IF(DEVICE.EQ.'PHA')   THEN
         XPIXEL=PHAPX
         YPIXEL=PHAPY
        ELSE IF(DEVICE.EQ.'T41')   THEN
         XPIXEL=T41PX
         YPIXEL=T41PY
        ELSE IF(DEVICE.EQ.'LAS')   THEN
         XPIXEL=PS4PX
         YPIXEL=PS4PY
        ELSE IF(DEVICE.EQ.'CLA')   THEN
         XPIXEL=PS4PX
         YPIXEL=PS4PY
        ELSE IF(DEVICE.EQ.'EPS')   THEN
         XPIXEL=PS4PX
         YPIXEL=PS4PY
        END IF
c        IF(VUG.EQ.'VUG')   THEN
c         TEMP=XPIXEL
c         XPIXEL=YPIXEL
c         YPIXEL=TEMP
c        END IF
       END IF

       IF(LINEP .EQ. 'LIN')   THEN
        WRITE(6,432) PACKGE,DEVICE,WDWMSG
       ELSE
        WRITE(6,422) PACKGE,BWCOL,DEVICE,WDWMSG
       END IF 
       NPX=NX
       NPY=NY
       IF(XBTYPE.EQ.'LOG')   THEN
        XAXIS=ABS(ALOG(XLGRID/X1GRID)/ALOG(2.0))*XSCALE
C
        NPIXEL=IFIX(XAXIS/(0.4*XPIXEL))
C       XAXIS=NPIXEL*(0.4*XPIXEL)
C       XSCALE=(ALOG(2.0)*XAXIS)/ABS(ALOG(XLGRID/X1GRID))
C
       ELSE
        XAXIS=ABS((XLGRID-X1GRID)/XSCALE)
C
        NPIXEL=IFIX(XAXIS/(0.4*XPIXEL))
C       XAXIS=NPIXEL*(0.4*XPIXEL)
C       XSCALE=(XLGRID-X1GRID)/XAXIS
C
       END IF
       NX=MIN0(NX,NPIXEL)
C
       IF(YBTYPE.EQ.'LOG')   THEN
        YAXIS=ABS((YLGRID-Y1GRID)*YSCALE)

        NPIXEL=IFIX(YAXIS/(0.4*YPIXEL))
C       YAXIS=NPIXEL*(0.4*YPIXEL)
C       YSCALE=YAXIS/(YLGRID-Y1GRID)
C
       ELSE
        YAXIS=ABS((YLGRID-Y1GRID)/YSCALE)

        NPIXEL=IFIX(YAXIS/(0.4*YPIXEL))
C       YAXIS=NPIXEL*(0.4*YPIXEL)
C       YSCALE=(YLGRID-Y1GRID)/YAXIS
C 
       END IF
       NY=MIN0(NY,NPIXEL)
C
       IF((NX.NE.NPX).OR.(NY.NE.NPY))   THEN
        WRITE(6,600) XAXIS,NX,YAXIS,NY
        LU=22
        OPEN(UNIT=LU,STATUS='SCRATCH',FORM='UNFORMATTED')
        CALL SAMPLE(Z,NPX,NPY,NX,NY,X1,XL,Y1,YL,LU)
        CALL ORDER(Z,NX,NY,LU)
        CLOSE(LU)
        NPX=NX
        NPY=NY
       END IF
C
       CALL SMOOTH(Z,NX,NY,NSMOOT)
       NSMOOT=0
CCCCCCC       IF(NPBOTT .GE. 2)   CALL WINDOW
       CALL MAINRAS(Z,FREQ,SD,RD,OPTION,DEVICE,XPIXEL,
     & YPIXEL,VUG,DEL,WRK,NPXYZ,FAX,ROT,REVERS,NCS,WDW)
       CLOSE(UNIT=LU)

       IF (DEVICE.EQ.'T41' .OR. DEVICE.EQ.'VTT') THEN
        WRITE(6,500)
        read(5,410) new
        X1PL=XSAVE
        Y1PL=YSAVE

        IF(NEW.EQ.'PRX' .OR. NEW.EQ.'prx')   THEN
         PACKGE='UNI'
         BWCOL='B/W'
         DEVICE='PRX'
         CALL BACKBOTT
         GO TO 555
        ELSE IF (NEW.EQ.'TEK' .OR. NEW.EQ.'tek')   THEN
         PACKGE='UNI'
         DEVICE='TEK'
         CALL BACKBOTT
         GO TO 555
        ELSE IF (NEW.EQ.'CLA' .OR. NEW.EQ.'cla')   THEN
         PACKGE='UNI'
         DEVICE='LAS'
         BWCOL='COL'
         CALL BACKBOTT
         GO TO 555
        ELSE IF (NEW.EQ.'VUG' .OR. NEW.EQ.'vug')   THEN
         PACKGE='UNI'
         VUG= 'VUG'
         WRITE(6,220)
         FAX='VG '
         DEVICE='TEK'
         CALL BACKBOTT
         GO TO 555
        ELSE IF (NEW.EQ.'LAS' .OR. NEW.EQ.'las')   THEN
         PACKGE='UNI'
         BWCOL ='B/W'
         DEVICE='LAS'
         CALL BACKBOTT
         GO TO 555
        ELSE IF (NEW.EQ.'PHA' .OR. NEW.EQ.'pha')   THEN
         PACKGE='UNI'
         DEVICE='PHA'
         CALL BACKBOTT
         GO TO 555
        ELSE IF (NEW.EQ.'EPS' .OR. NEW.EQ.'eps')   THEN
         PACKGE='UNI'
C         BWCOL ='B/W'
         DEVICE='EPS'
         CALL BACKBOTT
         GO TO 555
        ELSE IF (NEW.EQ.'PSL' .OR. NEW.EQ.'psl')   THEN
         PACKGE='UNI'
         BWCOL ='B/W'
         DEVICE='PSL'
         CALL BACKBOTT
         GO TO 555
        ELSE IF (NEW.EQ.'PSP' .OR. NEW.EQ.'psp')   THEN
         PACKGE='UNI'
         BWCOL ='B/W'
         DEVICE='PSP'
         ROT='ROT'
         CALL BACKBOTT
         GO TO 555
        ELSE IF (NEW.EQ.'CPS' .OR. NEW.EQ.'cps')   THEN
         PACKGE='UNI'
         BWCOL ='COL'
         DEVICE='PSP'
         ROT='ROT'
         CALL BACKBOTT
         GO TO 555
        ELSE
        END IF
       END IF
      else if (packge.eq.'MTV') then
       CALL MAINMTV(Z,FREQ,SD,RD,OPTION,DEVICE,XPIXEL,
     & YPIXEL,VUG,DEL,WRK,NPXYZ,FAX,ROT,REVERS,NCS,WDW)

      END IF

c make matlab file
       write(6,*) ">>> Entering MAINMAT <<<"
       CALL MAINMAT(Z,FREQ,SD,RD,OPTION,DEVICE,XPIXEL,
     & YPIXEL,VUG,DEL,WRK,NPXYZ,FAX,ROT,REVERS,NCS,WDW)

c      STOP
       packge='MIN'
       device='   '
       bwcol='B/W'
       go to 1
 9000 CONTINUE
      WRITE(6,700) 
      STOP
 9100 CONTINUE
      IF(OPENED .EQ. 'YES')   CALL PLOT(0.0,0.0,999)
      STOP
      END
