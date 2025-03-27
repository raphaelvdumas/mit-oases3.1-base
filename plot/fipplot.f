      PARAMETER (maxexp=16,MAXNP=2**maxexp,NPLP=2*MAXNP)
 
      INTEGER OPENED, LMARK(20), RDSYMB
      REAL AX(NPLP), AY(NPLP), PF(NPLP), QF(NPLP)
      LOGICAL BATCH, IDT, EXLEG
c      EXTERNAL BATCH

      CHARACTER*1 CHARIN
      CHARACTER*2 FORMT, BDEV
      CHARACTER*3 SYMB(20), ADD, CURVET, URC, ULC, NWR
      CHARACTER*3 DEV, SIM(10), PLP_PACKGE
      CHARACTER*3 PLOP(21), FONT
      CHARACTER*3 XBTYPE, YBTYPE, ZBTYPE
      CHARACTER*5 MODE
      CHARACTER*9 SC
      CHARACTER*11 FACTORTMP
      CHARACTER*12 OPTS
      CHARACTER*20 LAB(20), LAB1(20)
      CHARACTER*50 FILENM
      CHARACTER*80 TIT,TITX,TITY,TITZ
      CHARACTER*80 USNAME
      CHARACTER*80 BUFF
      CHARACTER*80 HTXT

      include 'default.f'

      COMMON /SETARG/ TRACD, TRCMAX

      COMMON /SETPRM/ PLOP, ADD, BDEV, FACTORTMP,
     &  SYMB, DEV, FORMT, URC, ULC, NWR

      COMMON /SETLOG / IFLG13,IDT

      COMMON /SCALES/ YSCALETMP, DRRATIO, YOLDLEN,
     &  SCFAC, HSCFAC, IHSC, FACTOR, FACT, FACSCALE, SCALEFACT

      COMMON /SPOPT/ ISEG,ISNUM,ITRUNC

      COMMON /XAX/ X1,XL,XLEFT,XRIGHT,XSCALE,XINC,DX,
     & X1PL,XDUM4,IXDUM1,XDUM5,XDUM6,XDIV,XVAL(100),NXVAL
      COMMON /XAXC/ TITX,XBTYPE
      COMMON /YAX/ Y1,YL,YUP,YDOWN,YSCALE,YINC,DY,
     & Y1PL,YDUM4,IYDUM1,YDUM5,YDUM6,YDIV,YVAL(100),NYVAL,
     & YBOX
      COMMON /YAXC/ TITY,YBTYPE
      COMMON /ZAXC/ TITZ,ZBTYPE
      COMMON /ZAX/ ZMIN,ZMAX,ZLEN,ZINC,ZDIV
      COMMON /XYLAST/ XLAST,YLAST,XLN,YLN,ISGN
cray      integer GETENV,istat
      integer ifilenm(6)
      CHARACTER*6 ENVVAR(2)

      EXTERNAL DMOD

      DATA XOR,YOR /1.5,1.0/
      DATA MODE/'MODE$'/
      DATA LUPLP,LUPLT/19,20/
      DATA IFLG13/-1/,IDT/.FALSE./,EXLEG/.FALSE./
      DATA USNAME/'SACLANT, EMG'/
      DATA ISNUM,IFLAG/ 0, 0 /
      DATA OPENED,DIDASC/ 0, 0 /
      DATA ICURV/ 0 /
      DATA HGTC/ 0.1015 /

  110 FORMAT(1H ,'*******************************************',
     &      /1H ,'*** SAFARI/OASES CURVE PLOTTING PROGRAM ***',
     &      /1H ,'*******************************************')
  130 FORMAT(A80)
  140 FORMAT(1X,A12,21(1X,A3))
  200 FORMAT(1X,'  ADD ',1X,A3)
  220 FORMAT(A1)


      batch=.true.

C
C     OPEN PLOT FILES
C
      do 1111 iun=19,20
      WRITE(ENVVAR(1),'(A3,I3.3)') 'FOR',IUN
      CALL GETENV(ENVVAR(1),FILENM)
cray      idummy = GETENV(envvar,ifilenm, len( envvar( 1 ) ) )
      OPEN(UNIT=IUN,FILE=FILENM,STATUS='OLD',ERR=997)
 1111  continue
c
c Check for MTV option
c
      call getenv('PLP_PACKGE',PLP_PACKGE)
      if (PLP_PACKGE.eq.'MTV'.or.PLP_PACKGE.eq.'mtv') then
       do ii=len(filenm),1,-1
        if (filenm(ii:ii).eq.'.') then
         ll=ii-1
         go to 1112
        end if
       end do
 1112  call system('plp2mtv '//filenm(1:ll))
       stop
      end if

C     OPEN SCRATCH FILE FOR SEISPACK PLOT
      OPEN(30,STATUS='SCRATCH',FORM='UNFORMATTED')

      WRITE(6,110)
 1000 READ(LUPLP,130,END=9000) BUFF
      IF(BUFF(7:12).EQ.'PLTEND') GO TO 9000
      READ(BUFF,*,ERR=1100) MODULO,SCALEFACT,FACTOR,YSCALE,DRRATIO
      FACSCALE=YSCALE
      IFLAG=1
      GO TO 1200

 1100 CONTINUE
      READ(BUFF,*) MODULO
      SCALEFACT=1.0
      FACTOR=1.0
      FACSCALE=1.0
      DRRATIO=1.0
     
 1200 CONTINUE
      DO 1300 I=1,20
 1300 LAB(I)='NO LAB$'

 1400 CONTINUE
      REWIND(30)
      READ(LUPLP,140,END=9000)  OPTS,PLOP
      IF(OPTS(7:12) .EQ. 'PLTEND')   GO TO 9000

 1600 CONTINUE

      CALL DECODE(OPTS, ICURV, CURVET, EXLEG)

      IF( (DIDASC .GT. 0) .AND. (ADD .NE. 'ADD') .AND.
     &    (EXLEG  .EQV. .FALSE.) )   THEN
        CALL ENDGR(0)
        CALL LEGEND(SYMB,LMARK,ICURV,LAB1,HGTC)
        GO TO 3200
      END IF

 1800 CONTINUE

      CALL INPUT(HTXT,TIT,NLAB,OPTS,FREQ,H0,H1,R1,
     &           LAB,LAB1,ICURV,XLEN,YLEN,IGRID,NC,FMIN,FMAX,LUPLP)

      IF(((OPTS(8:12) .EQ. 'PROFL')   .AND.
     &    (PLOP(1)    .EQ. 'RDP'  ))  .OR.
     &   ((OPTS(1:7)  .EQ. 'MOCASS,') .AND.
     &    (PLOP(1)    .EQ. 'PRO'   )))
     & CALL PROFLR(*1400,OPTS,OUTDEV,FONT,
     &             HTXT,NC,LUPLP,LUPLT,
     &             XLEFT,XRIGHT,XLEN,XINC,XDIV,
     &             YUP,YDOWN,YLEN,YINC,YDIV,
     &             ZMIN,ZMAX,ZLEN,ZINC,ZDIV,
     &             PF,QF)
      

      IF(OPTS(8:12) .EQ. 'MODES') CALL MODPLT(*1400,HTXT,OPTS,FREQ,H0,
     & H1,R1,NC,HGTC,XLEN,YLEN,NOP,LUPLT,OPENED,ICURV)

      CALL INITPLT(XLEN,YLEN,HTXT,OPTS,LAB,NLAB,
     &             TIT,TST,HGTC,VXSTA,VYSTA,YSTA,XSTA,OPENED,NT)

      DO 3000 IC=1,NC
 
      IF( NOP .EQ. 0)   THEN
        IF(ICOLF.GT.0)  THEN
          ICOL=2+MOD(IC-1,6)
        ELSE IF (ICOLF.EQ.-1)   THEN
          ICOL=ICOLC
        END IF
        CALL SETCOL(ICOL)
      END IF
      
      CALL REDATA(N,NC,IC,INDEX,XMIN,YMIN,FMIN,FMAX,LMARK(ICURV),
     &            RDSYMB,AX,AY,LUPLP,LUPLT)

      CALL PLOTFIP(*1400,IC,OPTS,TIT,NT,TST,HGTC,MAXNP,
     &             N,AX,AY,PF,QF,INDEX,LMARK(ICURV),RDSYMB,LAB,SIM,XSTA,
     &             VXSTA,VYSTA,YSTA,ICURV,FMIN,FMAX,HTXT)

 3000 CONTINUE

      IF(ADD .EQ. 'ADD')   GO TO 1400
 
 3200 CONTINUE

      IF (NOP.NE.1 .AND. IUNI.NE.1) THEN
        CALL DMOD(BDEV)
        IF (IDT .EQV. .FALSE. ) THEN
c            CALL ENDPL(-1)
            CALL ENDPL(0)
        ELSE
            CALL ENDPL(0)
        END IF
        OPENED=0
        ICURV=0
        IF(DIDASC .GT. 0)   THEN
          DIDASC=0
          ICURV=1
          SYMB(ICURV)=CURVET
          GO TO 1800
        END IF 

        IF (.NOT.BATCH.AND.
     &   (OPTS(9:12).EQ.'RUNI'.OR.OPTS(9:12).EQ.'DUNI')) THEN
c     &   (OPTS(9:12).EQ.'STCK'.OR.OPTS(9:12).EQ.'STDP')) THEN
          WRITE(6,*) 'Do you want a SEISPACK plot?'
          write(6,*) 
          WRITE(6,*) 'T: TEKTRONIX 4112 terminal'
          write(6,*) 'V: VT240 terminal'
          WRITE(6,*) 'P: PRINTRONIX (Black and white)'
          WRITE(6,*) 'C: TEKTRONIX 4691 (colour)'
          WRITE(6,*) 'X: CALCOMP 5105 (b/w)'
          write(6,*) 'N: No'
          READ(5,220) CHARIN
          IHCPY=1

          IF (CHARIN.EQ.'P'.OR.CHARIN.EQ.'p') then
            IUNI=1
            DEV='PRX'
          ELSE IF (CHARIN.EQ.'C'.OR.CHARIN.EQ.'c') THEN
            IUNI=1
            DEV='COL'
          ELSE IF (CHARIN.EQ.'X'.OR.CHARIN.EQ.'x') THEN
            IUNI=1
            DEV='C50'
          ELSE IF (CHARIN.EQ.'T'.OR.CHARIN.EQ.'t') THEN
            IUNI=1
            DEV='G41'
            IHCPY=0
          ELSE IF (CHARIN.EQ.'V'.OR.CHARIN.EQ.'v') THEN
            IUNI=1
            DEV='VTT'
            IHCPY=0
          ELSE
            IUNI=0
          END IF
        END IF
      END IF

      IF (IUNI.EQ.1) THEN
        TRACMM=YLEN*10.*ABS(TRACD)/ABS(YUP-YDOWN)
        SAMPMM=XLEN*10.*ABS(DX)/ABS(XRIGHT-XLEFT)

        IF (IHOR.EQ.0) THEN
          IDIRX=NINT(SIGN(1.0,TRACD*(YUP-YDOWN)))
          IDIRY=NINT(SIGN(1.0,XLEFT-XRIGHT))
          ORIX=25.+((NC-1.)*.5*(1-IDIRX)+0.5)*TRACMM
          ORIY=25.+(NSP-1.)*.5*(1-IDIRY)*SAMPMM
        ELSE
          IDIRX=1
          IDIRY=NINT(SIGN(1.0,TRACD*(YUP-YDOWN)))
          ORIX=25.
          ORIY=25.+((NC-1.)*.5*(1-IDIRY)+0.5)*TRACMM
        END IF

        TRACD=ABS(TRACD)
        DZ=TRACD/8.
        T0=MIN(XLEFT,XRIGHT)
        DT0=ABS(XINC)

        IF (ITAX.GT.0) THEN        
          NTS=NINT(FLOAT(NSP)*ABS(DT0)/ABS(XRIGHT-XLEFT))
        ELSE
          NTS=0
        END IF

        NDEC=1
        WRITE(6,*) '*** CALLING SEISPACK ***'
 4000   CALL SEISL(DEV,IHOR,TIT,USNAME,NC,NSP,TRACMM,SAMPMM,TRACD,
     &             ORIX,ORIY,
     &             IDIRX,IDIRY,DZ,KFORM,NEGPOS,KFILL,T0,DT0,NTS,NDEC,
     &             ICON,AY,30,NLAB,LAB,FORMT)
        IF (IHCPY.EQ.0) THEN
          WRITE(6,*) 'Do you want a hardcopy?'
          write(6,*) 
          WRITE(6,*) 'P: PRINTRONIX (Black and white)'
          WRITE(6,*) 'C: TEKTRONIX 4691 (colour)'
          WRITE(6,*) 'X: CALCOMP 5105 (b/w)'
          WRITE(6,*) 'V: VIEWGRAPH ( 4691 colour, scaled to A4 format)'
          write(6,*) 'N: No'
          READ(5,220) CHARIN
          IHCPY=1
          IF (CHARIN.EQ.'P'.OR.CHARIN.EQ.'p') then
            IUNI=1
            DEV='PRX'
          ELSE IF (CHARIN.EQ.'C'.OR.CHARIN.EQ.'c') THEN
            IUNI=1
            DEV='COL'
          ELSE IF (CHARIN.EQ.'X'.OR.CHARIN.EQ.'x') THEN
            IUNI=1
            DEV='C50'
          ELSE IF (CHARIN.EQ.'V'.OR.CHARIN.EQ.'v') THEN
            IUNI=1
            DEV='COL'
            FORMT='VG'
          ELSE
            GO TO 4200
          END IF
          GO TO 4000
        END IF
      END IF

 4200 CONTINUE

      IF(IFLAG.EQ.1) THEN
        GO TO 1000
      ELSE
        GO TO 1400
      END IF

 9000 CONTINUE

      IF( (OPENED .GT. 0) .AND. (EXLEG .EQV. .FALSE.) )   THEN
        CALL ENDGR(0)
        CALL LEGEND(SYMB,LMARK,ICURV,LAB1,HGTC)
        DIDASC=0
        NOP=0
        GO TO 3200
      END IF
      go to 999
 997  WRITE(6,*) '>>>> PLOT PARAMETER FILE COULD NOT BE OPENED <<<<'
      GO TO 999
 998  WRITE(6,*) '>>>> PLOT DATA FILE COULD NOT BE OPENED <<<<'
      GO TO 999
 999  CONTINUE
      END
