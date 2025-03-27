      SUBROUTINE DECODE(OPTS, ICURV, CURVET, EXLEG)
C

      LOGICAL BATCH,IDT,EXLEG
c      EXTERNAL BATCH

      CHARACTER*2 FORMT,BDEV
      CHARACTER*3 DEV, SYMB(20), ADD, PLOP(21), CURVET, URC, ULC, NWR
      CHARACTER*11 FACTORTMP
      CHARACTER*12 OPTS

      include 'default.f'
      COMMON /SCALES/ YSCALETMP, DRRATIO, YOLDLEN,
     &  SCFAC, HSCFAC, IHSC, FACTOR, FACT, FACSCALE, SCALEFACT

      COMMON /SETPRM/ PLOP, ADD, BDEV, FACTORTMP,
     %  SYMB, DEV, FORMT, URC, ULC, NWR

      COMMON /SPOPT/ ISEG,ISUNM,ITRUNC
      COMMON /SETLOG /IFLG13,IDT

      batch=.true.

C
  200 FORMAT(E10.3)
  220 FORMAT(1X,' ******* ',/,
     % ' WARNING: SUB OPTION "ADD" NOT ALLOWED FOR OPTION ',A5,
     % ' ******* ')
  240 FORMAT(1X,A12)

C
      
      CALL RESTORE(CURVET)

      IF( (OPTS(1:5).EQ.'PAREQ' .AND. PLOP(1).EQ.'RAN'   .OR.
     &     OPTS(3:5).EQ.'IFD'   .AND. PLOP(1).EQ.'RAN' ) .AND.
     &        IFLG13.EQ.-1 )  IFLG13 = 0

      DO 1600 J=1,21
      IF((OPTS(1:6) .EQ. 'MOCASS') .AND.
     %     ( PLOP(J) .EQ. 'TP ') .OR.
     %     ( PLOP(J) .EQ. 'BP ') .OR. 
     %     ( PLOP(J) .EQ. 'RP ') .OR. 
     %     ( PLOP(J) .EQ. ' P ')) THEN
        EXLEG = .TRUE.
      ELSE IF (PLOP(J).EQ.'COL')   THEN
        ICOLF=1
      ELSE IF (PLOP(J).EQ.'NDT')   THEN
        IDT= .TRUE.      
      ELSE IF (PLOP(J).EQ.'CPX')   THEN
        ICHTYP=3
      ELSE IF (PLOP(J).EQ.'SPX')   THEN
        ICHTYP=1
      ELSE IF (PLOP(J).EQ.'DPX')   THEN
        ICHTYP=2
      ELSE IF (PLOP(J).EQ.'ITA')   THEN
        ICHTYP=4
      ELSE IF (PLOP(J).EQ.'IXA')   THEN
        IXAX=1
      ELSE IF (PLOP(J).EQ.'IYA')   THEN
        IYAX=1
      ELSE IF (PLOP(J).EQ.'SCA')   THEN
        IHSC=1
      ELSE IF (PLOP(J).EQ.'THF')   THEN
        ITFRAM=1
      ELSE IF (PLOP(J).EQ.'MRK')   THEN
        IMARK=10
        ISYMB=1
      ELSE IF (PLOP(J).EQ.'DSD')   THEN
        IDASH=1
      ELSE IF (PLOP(J).EQ.'NOP')   THEN
        NOP=1
      ELSE IF (PLOP(J).EQ.'SEG')   THEN
        ISEG=1
      ELSE IF (PLOP(J).EQ.'TAX')   THEN
        ITAX=1
      ELSE IF (PLOP(J).EQ.'TCT')   THEN
        ITRUNC=1
      ELSE IF (PLOP(J).EQ.'F13')   THEN
        IFLG13=0
      ELSE IF (PLOP(J).EQ.'ADD')   THEN
        IF( (OPTS(8:12) .EQ. 'TLRAN') .OR.
     %      (OPTS(8:12) .EQ. 'TLAVF') .OR.
     %      (OPTS(8:12) .EQ. 'TLAVR') .OR.
     %      (OPTS(8:12) .EQ. 'TLDEP') .OR.
     %      ( PLOP(1) .EQ. 'RAN' .AND. OPTS(3:5) .EQ. 'IFD'   ) .OR.
     %      ( PLOP(1) .EQ. 'DEP' .AND. OPTS(3:5) .EQ. 'IFD'   ) .OR.
     %      ( PLOP(1) .EQ. 'RAN' .AND. OPTS(1:5) .EQ. 'PAREQ' ) .OR.
     %      ( PLOP(1) .EQ. 'DEP' .AND. OPTS(1:5) .EQ. 'PAREQ' ))  THEN
          ADD='ADD'
        ELSE
          WRITE(6,220) OPTS(8:12)
        END IF
      ELSE IF ( (PLOP(J).EQ.'CDS') .OR.
     %          (PLOP(J).EQ.'DOT') .OR.
     %          (PLOP(J).EQ.'DSH') .OR.
     %          (PLOP(J).EQ.'CDO') )    THEN
        CURVET=PLOP(J)
      ELSE IF (PLOP(J).EQ.'RED')   THEN
        ICOLF=-1
        ICOLC=2
      ELSE IF (PLOP(J).EQ.'GRE')   THEN
        ICOLF=-1
        ICOLC=3
      ELSE IF (PLOP(J).EQ.'BLU')   THEN
        ICOLF=-1
        ICOLC=4
      ELSE IF (PLOP(J).EQ.'CYA')   THEN
        ICOLF=-1
        ICOLC=5
      ELSE IF (PLOP(J).EQ.'MAG')   THEN
        ICOLF=-1
        ICOLC=6
      ELSE IF (PLOP(J).EQ.'YEL')   THEN
        ICOLF=-1
        ICOLC=7

C   WARNING: SUBOPTION NAME "UNI" CAN BE USED ONLY ON INSTALLATIONS
C            WHERE THE  "UNIRAS" PLOTTING PACKAGE IS AVAILABLE 
C            IN SUCH A CASE SUBROUTINE "SEISL.FOR" SHOULD ALSO BE USED
C            TO REPLACE SUB "SEISL.MIN".

      ELSE IF (PLOP(J).EQ.'UNI'.AND.
     &        (OPTS(9:12).EQ.'STCK'.OR.OPTS(9:12).EQ.'STDP'))   THEN
        IUNI=1
      ELSE IF (PLOP(J).EQ.'CON')   THEN
        ICON=1
        IF (KFORM.GT.1) KFORM=1
      ELSE IF (PLOP(J).EQ.'KT3'.AND.ICON.EQ.0)   THEN
        KFORM=3
      ELSE IF (PLOP(J).EQ.'KT0')   THEN
        KFORM=0
      ELSE IF (PLOP(J).EQ.'KT1')   THEN
        KFORM=1
      ELSE IF (PLOP(J).EQ.'SHD')   THEN
        fill=.true.
      ELSE IF (PLOP(J).EQ.'NEG')   THEN
        NEGPOS=-1
        ipos=0
      ELSE IF (PLOP(J).EQ.'POS')   THEN
        NEGPOS=1
        ineg=0
      ELSE IF (PLOP(J).EQ.'VTT')   THEN
        DEV='VTT'
        BDEV='VT'
        IHCPY=0
      ELSE IF (PLOP(J).EQ.'T41')   THEN
        DEV='T41'
        IHCPY=0
      ELSE IF (PLOP(J).EQ.'G41')   THEN
        DEV='G41'
        IHCPY=0
      ELSE IF (PLOP(J).EQ.'PRX')   THEN
        DEV='PRX'
        BDEV='PC'
      ELSE IF (PLOP(J).EQ.'C50')   THEN
        DEV='C50'
      ELSE IF (PLOP(J).EQ.'TEK')   THEN
        DEV='COL'
        BDEV='T '
      ELSE IF (PLOP(J).EQ.'VUG')   THEN
        FORMT='VG'
        DEV='COL'
        BDEV='VG'
      ELSE IF (PLOP(J).EQ.'URC')   THEN
        URC='URC'
      ELSE IF (PLOP(J).EQ.'ULC')   THEN
        ULC='ULC'
c        NWR='NWR'
        IDT= .TRUE.
      ELSE IF (PLOP(J).EQ.'NWR')   THEN
        NWR='NWR'
      ELSE IF (PLOP(J).EQ.'RES')   THEN
        IF (.NOT.BATCH) THEN
          WRITE(6,*) OPTS(6:12),' SCALING FACTOR?'
          READ(5,*) SCFAC
        ELSE
          WRITE(6,*) OPTS(6:12),' SCALING FACTOR?'
          READ(5,*,END=1400) SCFAC
          GO TO 1500
 1400     SCFAC=1.0
          GO TO 1600
 1500     CONTINUE
        END IF
        WRITE(FACTORTMP,200)FACTOR
        FACTOR=FACTOR/SCALEFACT*SCFAC
        SCALEFACT=SCFAC
      END IF
 1600 CONTINUE

      IF(NOP .EQ. 1)   THEN
        IF(DIDASC .GT. 0)   ADD='ADD'
        RETURN
      ELSE
        IF(ADD .EQ. 'ADD')   THEN
          DIDASC=1
          ICURV=ICURV+1
          SYMB(ICURV)=CURVET
        ELSE
          IF(DIDASC .EQ. 0)   THEN
            ICURV=1
            SYMB(ICURV)=CURVET
            RETURN
          END IF

        END IF

      END IF           

      RETURN
      END
