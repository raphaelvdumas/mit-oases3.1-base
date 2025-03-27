C NAME ST4695
C PACK DRIVER
C COMP VAX
C VERS 861002
C HIST 5V1,5V1A,5V2,5V4
C---------------------------------------------------------------------C 
C                                                                     C 
C                    U N I R A S   S O F T W A R E                    C 
C                                                                     C 
C     THE   CONTENTS   OF   THIS   DOCUMENT  ARE  PROPRIETARY  TO     C 
C                                                                     C 
C                         U N I R A S  A/S                            C 
C                                                                     C 
C     AND  ARE  NOT  TO  BE  DISCLOSED  TO  OTHERS  OR  USED  FOR     C 
C     PURPOSES  OTHER  THAN  DESCRIBED  IN  AGREEMENT  OR WRITTEN     C 
C     APPROVAL  OF  THE  OWNERS.                                      C 
C                                                                     C 
C---------------------------------------------------------------------C 
C
      SUBROUTINE ST4695(*)
C---------------------------------------------------------------------C
C
C FUNCTION:
C
C   DRIVER FOR TEKTRONIX 4695 INK JET HARDCOPY RUNNING TROUGH
C   TEKTRONIX 4105/4107/4109
C   THIS DRIVER IS WRITTEN FOR 32 BIT COMPUTERS ONLY
C
C REFERS TO:
C
C   GETSCA
C   I4695,O4695,F4695
C---------------------------------------------------------------------C
C NSTRIP IS NUMBER OF WORDS IN Y DIRECTION (32 32 BIT WORDS = 1024 PX)
C---------------------------------------------------------------------C
C
      PARAMETER (NSTRIP=32)
C
      CHARACTER*1 KODE
      INTEGER IPARM(7),INSIZE(4)
      INTEGER CYAN(4,NSTRIP),MAG(4,NSTRIP)
      INTEGER YEL(4,NSTRIP),BLACK(4,NSTRIP)
      INTEGER SETPAR(4),KEYL(4),COPYHC(14),EOFSTR(6),SETFLG(4),EOF
      INTEGER RESPAR(4),RESFLG(4)
      INTEGER CODET(4),CODEE(4),LF24(24),KEYUL(4)
      LOGICAL INVERT,PRON
C
      COMMON /ISCANX/ ISCAN(NSTRIP,3,100)
C
      EQUIVALENCE (ISCAN,IPARM)
c
c     HS 880107   Output file variable name
c
      integer getpid
      character*40 filenm
      common /oufiln/ nrofpl,filenm
C---------------------------------------------------------------------C
C     DATA INITILIZATIONS OF ESCAPE COMMANDS
C---------------------------------------------------------------------C
      DATA SETPAR/27,78,80,52/
      DATA RESPAR/27,78,80,48/
      DATA KEYL  /27,75,76,49/
      DATA KEYUL /27,75,76,48/
      DATA COPYHC/27,74,67,51,
     *           72,79,58,50,84,79,51,72,67,58/
      DATA EOFSTR/27,78,69,49,69,59/
      DATA EOF/91/
      DATA SETFLG/27,78,70,49/
      DATA RESFLG/27,78,70,51/
      DATA CODET/27,37,33,48/
      DATA CODEE/27,37,33,50/
      DATA LF24/24*10/
      data nrofpl /0/
c
c     determine output file name
c
c      filenm='tek'//char(65+nrofpl)
      write(filenm,888) char(65+nrofpl),getpid()
      nrofpl=nrofpl+1
 888  format('tek',A1,I5.5)
C---------------------------------------------------------------C
C     GET RASTER DATA BASE PARAMETERS
C---------------------------------------------------------------C
      CALL RDVINF(NPLPXX,NPLPXY,KODE,NBITPL,NLEVEL)
      CALL RCOINF(NBITW,NPAGXP,NPAGYP)
      CALL RIOINF(LUIN,LUOUT,PRON)
      NPAGWD=NPAGYP/NBITW
C---------------------------------------------------------------C
C     CHECK IF ADDITIVE DATA BASE (INVERT=.TRUE.)
C---------------------------------------------------------------C
      INVERT=KODE.EQ.'E'
C---------------------------------------------------------------C
C     TEST IF THE DRIVER IS PROPER FOR THE RASTER-DATABASE      C
C---------------------------------------------------------------C
      IF(NBITPL.NE.3.OR.(KODE.NE.'D'.AND.KODE.NE.'E')) GOTO 9999
C---------------------------------------------------------------C
C     PRINT GENERAL INFORMATION
C---------------------------------------------------------------C
      IF (PRON) WRITE(LUOUT,905)
905   FORMAT('1U N I R A S   TEKTRONIX 4695 DRIVER IS INITIALIZED')
C---------------------------------------------------------------C
C     PRINT PLOT OVER USED PAGES                                C
C---------------------------------------------------------------C
      CALL GSYSPR
C---------------------------------------------------------------C
C     FIND NUMBER OF USED PAGES                                 C
C---------------------------------------------------------------C
      CALL GSYSMN(IXMIN,IYMIN)
      CALL GSYSMX(IXMAX,IYMAX)
      NPGX=IXMAX-IXMIN+1
      NPGY=IYMAX-IYMIN+1
C---------------------------------------------------------------C
C     CALCULATE NUMBER OF SECTIONS IN Y DIRECTION
C---------------------------------------------------------------C
      IPLY=(NPGY*NPAGWD*NBITW-1)/1024+1
      IF (NPLPXY.LE.1024.AND.
     *      IY1+(NPLPXY-1)/NBITW-IYMAX*NPAGWD.LE.1) IPLY=1
      NPLPXY=1024
C
      LIN=NPGX*NPAGXP
      IF (PRON) WRITE(LUOUT,906) LIN,IPLY
906   FORMAT(///' ',I5,' RASTER SCAN LINES ARE BEING PROCESSED'/
     *       ' ',I5,' SECTION(S) IN THE Y DIRECTION ARE NEEDED')
C
      IY2=(IYMIN-1)*NPAGWD
C---------------------------------------------------------------C
C     ASSIGN CHANNEL AND SEND COPY COMMAND
C---------------------------------------------------------------C
      IF (PRON) WRITE(LUOUT,909)
909   FORMAT(/' THE KEYBOARD IS NOW LOCKED'/
     *        ' PLEASE WAIT UNTIL THE PLOT HAS FINISHED')
C
      IDEVTY=0
      CALL UDOPEN(*1,NDEV,filenm(1:9),IDEVTY)
c      CALL UDIPUT(*1,NDEV,CODET,4)
c      CALL UDIPUT(*1,NDEV,KEYL,4)
c      CALL UDIPUT(*1,NDEV,EOFSTR,6)
c      CALL UDIPUT(*1,NDEV,SETPAR,4)
c      CALL UDIPUT(*1,NDEV,SETFLG,4)
c      CALL UDWAIT(*1,NDEV,100)
c      CALL UDIPUT(*1,NDEV,COPYHC,14)
c      CALL UDWAIT(*1,NDEV,20)
C---------------------------------------------------------------C
C     LOOP OVER PLOT IN Y-DIRECKTION                            C
C---------------------------------------------------------------C
      DO 800 IPL=1,IPLY
        IY1=IY2+1
        IY2=MIN0(IY1+(NPLPXY-1)/NBITW,IYMAX*NPAGWD)
C---------------------------------------------------------------C
C     SELECT ALL COLORS
C---------------------------------------------------------------C
        INK=0
C
        LIN=NPGX*NPAGXP
C---------------------------------------------------------------C
C     SETUP PARAMETERS FOR GETSCA
C---------------------------------------------------------------C
        IPARM(1)=-IY1
        IPARM(2)=-IY2
        IPARM(3)=-NSTRIP
        IPARM(4)=NPAGXP
        IPARM(5)=0
        IPARM(6)=IXMIN
        IPARM(7)=0
C
        IBAND=1
        NWD=(IY2-IY1+1)
C       IF (IPLY.EQ.1) NWD=1024/NBITW
        CALL I4695(*1,NDEV,NWD)
C---------------------------------------------------------------C
C     LOOP FOR XPAGES
C---------------------------------------------------------------C
        DO 300 IPAGE=IXMIN,IXMAX
C---------------------------------------------------------------C
C     GET 100 RASTER SCAN LINES
C---------------------------------------------------------------C
          CALL GETSCA(ISCAN,INK,IBAND)
          IF(IBAND.EQ.-1) GO TO 800
          IBAND=IBAND+1
C---------------------------------------------------------------C
C     LOOP FOR NPAGXP LINES
C---------------------------------------------------------------C
          DO 400 LINE=1,NPAGXP
C---------------------------------------------------------------C
C     PROCESS ONE LINE
C---------------------------------------------------------------C
            DO 450 IWD=1,NWD
              KC=ISCAN(IWD,1,LINE)
              KM=ISCAN(IWD,2,LINE)
              KY=ISCAN(IWD,3,LINE)
C---------------------------------------------------------------C
C     INVERT IF RGB-PICTURE
C---------------------------------------------------------------C
              IF(INVERT) THEN
                KC=NOT(KC)
                KM=NOT(KM)
                KY=NOT(KY)
              END IF
C---------------------------------------------------------------C
C     EXTRACT BLACK & REMOVE FROM CMY
C---------------------------------------------------------------C
              KB=NOT(IAND(KC,IAND(KM,KY)))
              KC=IAND(KC,KB)
              KM=IAND(KM,KB)
              KY=IAND(KY,KB)
              KB=NOT(KB)
              DO 100 K=1,NBITW/8
                CYAN(K,IWD)=IAND(ISHFT(KC,-(NBITW-K*8)),255)
                MAG(K,IWD)=IAND(ISHFT(KM,-(NBITW-K*8)),255)
                YEL(K,IWD)=IAND(ISHFT(KY,-(NBITW-K*8)),255)
                BLACK(K,IWD)=IAND(ISHFT(KB,-(NBITW-K*8)),255)
100           CONTINUE
C
450         CONTINUE
C---------------------------------------------------------------C
C     OUTPUT
C---------------------------------------------------------------C
            CALL O4695(*1,NDEV,BLACK)
            CALL O4695(*1,NDEV,MAG)
            CALL O4695(*1,NDEV,YEL)
            CALL O4695(*1,NDEV,CYAN)
C---------------------------------------------------------------C
C     END OF LOOP FOR NPAGXP LINES
C---------------------------------------------------------------C
400       CONTINUE
C---------------------------------------------------------------C
C     END OF LOOP FOR NPAGXP PAGES
C---------------------------------------------------------------C
300     CONTINUE
C---------------------------------------------------------------C
C    END OF  LOOP OVER PLOT IN Y-DIRECKTION
C---------------------------------------------------------------C
        CALL F4695(*1,NDEV)
800   CONTINUE
C---------------------------------------------------------------C
C    DEASSIGN CHANNEL
C---------------------------------------------------------------C
      CALL UDIPUT(*1,NDEV,EOF,1)
      CALL UDIPUT(*1,NDEV,EOFSTR,3)
c      CALL UDIPUT(*1,NDEV,48,1)
c      CALL UDIPUT(*1,NDEV,KEYUL,4)
c      CALL UDIPUT(*1,NDEV,RESPAR,4)
c      CALL UDIPUT(*1,NDEV,RESFLG,4)
c      CALL UDWAIT(*1,NDEV,100)
c      CALL UDIPUT(*1,NDEV,CODEE,4)
c      CALL UDIPUT(*1,NDEV,LF24,24)
      CALL UDCLOS(*1,NDEV)
      IF (PRON) WRITE(LUOUT,910)
910   FORMAT(/' PLOT OK')
      RETURN
C---------------------------------------------------------------C
C     ERROR MESSAGE
C---------------------------------------------------------------C
9999  CONTINUE
      WRITE(LUOUT,9998)
9998  FORMAT(' THE RASTER DATA BASE IS IMPROPER FOR THIS DRIVER')
      RETURN 1
C---------------------------------------------------------------C
C     ERROR EXIT
C---------------------------------------------------------------C
1     CALL RSYSPA('C-ST4695-SYSABORT')
      RETURN 1
C
      END
C NAME I4695
C
C---------------------------------------------------------------C
C     OUTPUT ROUTINES FOR THE TEK4695
C---------------------------------------------------------------C
      SUBROUTINE I4695(*,NDEV,NWD)
C
      CHARACTER*3 CLEN
      INTEGER RASTER(6),LF,MLF(2)
      INTEGER BUFFER(*)
C
      DATA RASTER/27,73,4*0/
      DATA MLF/27,65/, LF/10/, NBYTE/0/, NLF/0/
      DATA LINE/0/, INK/0/
C
      NWORD=NWD
      NBYTE=NWD*4
      DO 100 I=1,NLF
        CALL UDIPUT(*1,NDEV,LF,1)
100   CONTINUE
      LINE=0
      INK=0
      RETURN
C
      ENTRY O4695(*,NDEV,BUFFER)
C
C---------------------------------------------------------------C
C     IF BUFFER IS ALL ZERO IGNORE IT
C---------------------------------------------------------------C
      DO 200 I=NBYTE,1,-1
        IF(BUFFER(I).NE.0) GOTO 300
200   CONTINUE
      GOTO 500
C---------------------------------------------------------------C
C     OUTPUT BUFFER
C---------------------------------------------------------------C
300   CONTINUE
      NBOUT=I
      DO 310 I=1,NBOUT
        IF (BUFFER(I).EQ.91) BUFFER(I)=89
310   CONTINUE
      WRITE(CLEN,'(I3)') NBOUT
      DO 400 I=1,3
        IF(CLEN(I:I).NE.' ') THEN
c >>> this routine does not exist in v6.2. Brute force comment out.
c >>> hs 911028
c          CALL GASCII(KHELP,CLEN,I)
           khelp=ichar(clen(i:i))
          RASTER(I+3)=KHELP
        ELSE
          RASTER(I+3)=48
        ENDIF
400   CONTINUE
C
      RASTER(3)=INK*4+LINE+48
      CALL UDIPUT(*1,NDEV,RASTER,6)
      CALL UDIPUT(*1,NDEV,BUFFER,NBOUT)
C---------------------------------------------------------------C
C     UPDATE FOR NEXT LINE
C---------------------------------------------------------------C
500   CONTINUE
      INK=MOD(INK+1,4)
      IF(INK.NE.0) RETURN
      LINE=MOD(LINE+1,4)
      IF(LINE.NE.0) RETURN
      CALL UDIPUT(*1,NDEV,MLF,2)
      RETURN
C
      ENTRY F4695(*,NDEV)
C
      DO 600 I=1,NLF
        CALL UDIPUT(*1,NDEV,LF,1)
600   CONTINUE
C
      RETURN
C---------------------------------------------------------------C
C     ERROR EXIT
C---------------------------------------------------------------C
1     CALL RSYSPA('C-I4695-SYSABORT')
      RETURN 1
C
      END
