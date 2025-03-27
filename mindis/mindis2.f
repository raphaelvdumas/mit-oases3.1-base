C
C     ***MINDIS - SACLANTCEN GRAPH PLOTTING PACKAGE
C
C     ***JOHN STEIERT   -   10-DEC-81
C
C     ***AMMENDMENTS:
C
C      JS  09-JUN-82   USE RVE COMMAND TO MAKE FASTER
C      JS  10-JUN-82   STOP EXTENDING COMMANDS OVER LINES (FOR 4025)
C      JS   7-JUN-83   Standard Fortran version
C      JS  15-JUN-83   Change CHARACTER*100 to CHARACTER*(*)
C                      Make BLOCKDATA into a subroutine
C      JS  25-JUL-83   New routine TKXSTP to set pen colour
C      JS  31-Oct-83   Allow use of Tektronix 4112 terminal.
C      JS  16-Jul-84   Allow use of Digital VT240 terminal.
C      JS   1-Jan-85   Use REGIS commands with VT240 terminal.
C                      With VT240 set buffer length to 80.
C                      Rename T27XSH to TKXSHR
C                      Put messages into STOP statements
C      JS   5-Jan-85   Put routines to handle reading cursor
C      JS  19-Feb-85   Modify T27XXY - didn't work well on VAX
C      JS  23-Sep-85   Don't change scrolling
C                      Eliminate null vectors on VT240
C                      Handle Tektronix 4105
C      HS  27-jun-90   Polygon fill for 4105
c      HS  15-Mar-91   X-windows driver implemented. Must link
c                      with gistart.c and -lX
C -----------------------------------------------------
C
C     General Routines
C     ================
C
C -----------------------------------------------------
C
      SUBROUTINE TKXINI
C     ***ROUTINE TO INITIALISE TEKTRONIX SCREEN FOR GRAPHICS
      INCLUDE 'CTLDT.FOR'
      INTEGER NPNTS, OLDX, OLDY, STRTX, STRTY, CURSX, CURSY
      COMMON /TKXDAT/ NPNTS, OLDX, OLDY, STRTX, STRTY, CURSX, CURSY
      Integer ixxterm,iyxterm
      COMMON /XTERMS/ ixxterm,iyxterm
C
      NPNTS=0
      OLDX=0
      OLDY=0
      STRTX=0
      STRTY=0
      CURSX=0
      CURSY=0
      CALL TXINIT
      if (ttype.eq.1011) then
c >>> x-window open
       call gistart(20,30,ixxterm,iyxterm)
      else IF (TTYPE.EQ.4105) then
       CALL T05XIN
      else IF (TTYPE.EQ.4112) then
       CALL T12XIN
      else IF (TTYPE.EQ.4010.OR.TTYPE.EQ.4014) then
       CALL T10XCR
      else IF (TTYPE.EQ.240) then
       CALL V24XIN
      else IF (TTYPE.EQ.4025.OR.TTYPE.EQ.4027) then
       CALL T27XIN
      end if
      call TKXOPB
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE TKXSIZ(RXSIZ,RYSIZ)
C     ***Routine to return screen size
      INCLUDE 'CTLDT.FOR'
      Integer ixxterm,iyxterm
      COMMON /XTERMS/ ixxterm,iyxterm
C
      IF (TTYPE.EQ.4025.OR.TTYPE.EQ.4027) THEN
           RXSIZ=640.0
           RYSIZ=415.0
      ELSEIF (TTYPE.EQ.4105.OR.TTYPE.EQ.4112.OR.TTYPE/100.EQ.40) THEN
           RXSIZ=4096.0
           RYSIZ=3072.0-300.0
      ELSEIF (TTYPE.EQ.240) THEN
           RXSIZ=799.0
           RYSIZ=479.0-60.0
      else if (ttype.eq.1011) then
           RXSIZ=ixxterm
           RYSIZ=iyxterm
      ENDIF
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE TKXMOV(IX,IY)
C     ***ROUTINE TO MOVE CURSOR ON SCREEN
      INCLUDE 'CTLDT.FOR'
      INTEGER NPNTS, OLDX, OLDY, STRTX, STRTY, CURSX, CURSY
      COMMON /TKXDAT/ NPNTS, OLDX, OLDY, STRTX, STRTY, CURSX, CURSY
      Integer ixxterm,iyxterm
      COMMON /XTERMS/ ixxterm,iyxterm
C
      NPNTS=0
      STRTX=IX
      STRTY=IY
      IF (TTYPE.EQ.1011) CALL DRAWLINE(IX,iyxterm-IY,3)
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE TKXDRW(IX,IY)
C     ***ROUTINE TO DRAW LINE ON SCREEN
      INCLUDE 'CTLDT.FOR'
      Integer ixxterm,iyxterm
      COMMON /XTERMS/ ixxterm,iyxterm
      IF (TTYPE.EQ.1011) THEN
       CALL DRAWLINE(IX,iyxterm-IY,2)
      ELSE IF (TTYPE.EQ.4105.OR.TTYPE.EQ.4112) THEN
       CALL T12XDR(IX,IY)
      ELSE IF (TTYPE.EQ.4010) THEN
       CALL T10XDR(IX,IY)
      ELSE IF (TTYPE.EQ.4014) THEN
       CALL T12XDR(IX,IY)
      ELSE IF (TTYPE.EQ.240) THEN
       CALL V24XDR(IX,IY)
      ELSE IF (TTYPE.EQ.4025.OR.TTYPE.EQ.4027) THEN
       CALL T27XDR(IX,IY)
      END IF
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE TKXEPA
C     *** FILL PANEL ON TERMINALS
      INCLUDE 'CTLDT.FOR'
      IF (TTYPE.EQ.4105.OR.TTYPE.EQ.4112) CALL T12EPA
      RETURN
      END
      SUBROUTINE TKXCLR
C     ***ROUTINE TO CLEAR TEKTRONIX SCREEN
      CHARACTER ACHAR
      INTEGER NPNTS, OLDX, OLDY, STRTX, STRTY, CURSX, CURSY
      COMMON /TKXDAT/ NPNTS, OLDX, OLDY, STRTX, STRTY, CURSX, CURSY
      INCLUDE 'CTLDT.FOR'
C
      CALL TKXOPB
      WRITE(6,100)
 100      FORMAT(' Clearing screen - Type return to continue')
      READ(5,200) ACHAR
 200      FORMAT(1A1)
      IF (TTYPE.EQ.4025.OR.TTYPE.EQ.4027) CALL T27XCR
      IF (TTYPE.EQ.4105.OR.TTYPE.EQ.4112) CALL T12XCR
      IF (TTYPE.EQ.4010.OR.TTYPE.EQ.4014) CALL T10XCR
      IF (TTYPE.EQ.240) CALL V24XCR
      CALL TKXOPB
c      CALL TKXDLY
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE TKXSTP(IPEN)
C     ***Routine to set plot colour to IPEN (ie COL C<IPEN-1>)
      CHARACTER*10 CMD
      INTEGER NPNTS, OLDX, OLDY, STRTX, STRTY, CURSX, CURSY
      COMMON /TKXDAT/ NPNTS, OLDX, OLDY, STRTX, STRTY, CURSX, CURSY
      INCLUDE 'CTLDT.FOR'
      character*4 csi
      character*2 st
      character*3 buffer
C
      IF (LTTCRT.AND.(TTYPE.EQ.4027)) THEN
           NPNTS=0
           WRITE(CMD,100) IPEN-1
 100           FORMAT('COL C',I1)
           INCHAR=6
           CALL T27XCM(CMD,INCHAR)
      else if (LTTCRT.AND.(TTYPE.EQ.4105)) THEN
           NPNTS=0
           WRITE(CMD,200) CHAR(27),MOD(IPEN,8)
 200       FORMAT(A1,'ML',I1)
           CALL TKXSTR(CMD(1:4))
c >>> polygon fill
c >>> for em4105 emulator
c            csi=char(27)//'[5|'
c            st=char(27)//'\'
c            call tkxopb
c            write(buffer,'(i3)') -mod(ipen,8)
c            call tkxstr(csi//'FILLPAT'//buffer//st)
      else if (LTTCRT.and.(ttype.eq.1011)) then
           if (ipen.eq.1) then
            ipenl=7
           else if (ipen.eq.0) then
            ipenl=0
           else           
            ipenl=mod(ipen-1,8)
           end if
           call lxcol(ipenl)
      ENDIF
      WRITE(48) 6, FLOAT(IPEN), 0.0
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE TKXOPB
C     ***ROUTINE TO FORCE OUT BUFFER TO TEKTRONIX SCREEN
      CHARACTER*200 LINE
      INTEGER LPTR, LLTH
      COMMON /TKXDT1/ LINE, LPTR, LLTH
      INTEGER NPNTS, OLDX, OLDY, STRTX, STRTY, CURSX, CURSY
      COMMON /TKXDAT/ NPNTS, OLDX, OLDY, STRTX, STRTY, CURSX, CURSY
      INCLUDE 'CTLDT.FOR'
C
      IF (TTYPE.EQ.4105.OR.TTYPE.EQ.4112.or.ttype/100.eq.40) THEN
           LPTR=LPTR+1
C          ***OUTPUT (US) CHAR
           LINE(LPTR:LPTR)=CHAR(31)
           ENDIF
C
C     ***With VT240 need to put command to enter REGIS at start of
C     ***line, and at the end need to exit REGIS - otherwise any
C     ***possible user output will not be visible.
      IF (TTYPE.EQ.240) THEN
           WRITE(6,100) CHAR(27),CHAR(27), 'P' , 'p' ,
     $                  (LINE(I:I),I=1,LPTR),
     $                  CHAR(27),CHAR(92)
      ELSE
C
C          ***Check if first char of line is ESC - for modem lines
C          ***this causes problems.
C           IF (LINE(1:1).EQ.CHAR(27)) THEN
C                WRITE(6,100) CHAR(27),(LINE(I:I),I=1,LPTR)
 100                FORMAT(' ',200A1)
C           ELSE
                WRITE(6,100) (LINE(I:I),I=1,LPTR)
C                ENDIF
           ENDIF
      LPTR=0
      NPNTS=0
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE TKXCLS
C     ***ROUTINE TO CLOSE TEKTRONIX SCREEN
      CHARACTER ACHAR
      INTEGER NPNTS, OLDX, OLDY, STRTX, STRTY, CURSX, CURSY
      COMMON /TKXDAT/ NPNTS, OLDX, OLDY, STRTX, STRTY, CURSX, CURSY
      INCLUDE 'CTLDT.FOR'
C
      CALL TKXOPB
      WRITE(6,100)
 100      FORMAT(' Clearing screen - Type return to continue')
      READ(5,200) ACHAR
 200      FORMAT(1A1)
      IF (TTYPE.EQ.1011) THEN 
c       CALL QUITX()
       CALL QUITX
      ELSE IF (TTYPE.EQ.4105) THEN 
       CALL T05XCS
       CALL TKXOPB
c      CALL TKXDLY
      ELSE IF (TTYPE.EQ.4112) THEN
       CALL T12XCS
       CALL TKXOPB
c      CALL TKXDLY
      ELSE IF (TTYPE.EQ.4010.OR.TTYPE.EQ.4014) THEN
       CALL T10XCS
       CALL TKXOPB
      ELSE IF (TTYPE.EQ.240) THEN
       CALL V24XCS
       CALL TKXOPB
      ELSE IF (TTYPE.EQ.4025.OR.TTYPE.EQ.4027) THEN
       CALL T27XCS
       CALL TKXOPB
      END IF
      RETURN
      END
      SUBROUTINE TKXDLY
C
C    Delay after clearing Tektronix screens
C
      PARAMETER (DELAY=0.25)


c     Next 3 lines does not work with LINUX due to the TIME function.
c     Instead use the ETIME routine
c     INTEGER TIME
c     D0=TIME()
c     D1=TIME()

      REAL TME(2)
      D0=ETIME(TME)
 1    D1=ETIME(TME)
      IF (D1.LT.D0+DELAY) GO TO 1

      RETURN
      END 
C
C -----------------------------------------------------
C
C      SUBROUTINE BLCKX4
      BLOCK DATA BLCKX4
      CHARACTER*200 LINE
      INTEGER LPTR, LLTH
      COMMON /TKXDT1/ LINE, LPTR, LLTH
      INTEGER OLDHIY,OLDEXT,OLDLOY,OLDHIX,OLDLOX
      COMMON /CX4112/ OLDHIY,OLDEXT,OLDLOY,OLDHIX,OLDLOX
      Integer ixxterm,iyxterm
      COMMON /XTERMS/ ixxterm,iyxterm
C
      DATA LPTR, LLTH /0, 130/
      DATA  OLDHIY,OLDEXT,OLDLOY,OLDHIX,OLDLOX /-1,-1,-1,-1,-1/
Cbig-xterm        data ixxterm,iyxterm /850,600/
Csmall-xterm      data ixxterm,iyxterm /567,400/
      data ixxterm,iyxterm /850,600/
      END
C
C -----------------------------------------------------
C
      SUBROUTINE TKXBYT(IVAL)
C     ***Routine to put character of value IVAL into buffer
      INTEGER IVAL
      CHARACTER*200 LINE
      INTEGER LPTR,LLTH
      COMMON /TKXDT1/ LINE, LPTR, LLTH
C
      IF (LPTR+3.GT.LLTH) CALL TKXOPB
C      IF (IVAL.EQ.127) THEN
C          ***Special action for <DEL> : output <ESC> ?
C           LINE(LPTR+1:LPTR+1)=CHAR(27)
C           LINE(LPTR+2:LPTR+2)='?'
C           LPTR=LPTR+2
C      ELSE
           LPTR=LPTR+1
           LINE(LPTR:LPTR)=CHAR(IVAL)
C           ENDIF
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE TKXSTR(CBUFF)
C     ***Routine to put characters from CBUFF into buffer
      CHARACTER*(*) CBUFF
C
      ILEN=LEN(CBUFF)
      DO 1 I=1,ILEN
           CALL TKXBYT(ICHAR(CBUFF(I:I)))
 1         CONTINUE
      RETURN
      END
C
C -----------------------------------------------------
C
      INTEGER FUNCTION TKXSHR(CMD, LGTH, LONESP)
C     *** ROUTINE TO SHRINK COMMAND BY TAKING OUT SPACES
C     *** If LONESP is true then one space will be left
      CHARACTER*(*) CMD
      INTEGER LGTH, BPTRI, BPTRO
      LOGICAL LONESP
C
      BPTRI=1
      BPTRO=0
      IF (BPTRI.GE.LGTH) GOTO 3
C
      DO 2 I=1,1000
           BPTRO=BPTRO+1
           IF (BPTRO.NE.BPTRI) CMD(BPTRO:BPTRO)=CMD(BPTRI:BPTRI)
           IF (CMD(BPTRO:BPTRO).EQ.' ') THEN
                IF (.NOT.LONESP) BPTRO=BPTRO-1
                DO 1 J=1,1000
                     BPTRI=BPTRI+1
                     IF (BPTRI.GT.LGTH) GOTO 3
                     IF (CMD(BPTRI:BPTRI).NE.' ') GOTO 2
 1                   CONTINUE
                STOP 'TKXSHR: command too long (1)'
                ENDIF
           BPTRI=BPTRI+1
           IF (BPTRI.GT.LGTH) GOTO 3
 2         CONTINUE
      STOP 'TKXSHR: command too long (2)'
C
 3    CONTINUE
      TKXSHR=BPTRO
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE TKXXYI(X,Y,CHARIN,COLOUR)
C     ***Generalised routine to read XY cursor
      REAL X,Y
      CHARACTER*1 CHARIN
      INTEGER COLOUR
      INCLUDE 'CTLDT.FOR'
C
      IF (TTYPE.EQ.4025.OR.TTYPE.EQ.4027) THEN
           CALL T27XXY(X,Y,CHARIN,COLOUR)
      ELSEIF (TTYPE.EQ.4105.OR.TTYPE.EQ.4112) THEN
           CALL T12XXY(X,Y,CHARIN,COLOUR)
      ELSEIF (TTYPE.EQ.240) THEN
           CALL V24XXY(X,Y,CHARIN,COLOUR)
      ELSE
           WRITE(6,100)
 100           FORMAT(' No routine to read cursor for this terminal')
           ENDIF
      RETURN
      END
C
C -----------------------------------------------------
C
C     Tektronix 4025/4027 Routines
C     ============================
C
C -----------------------------------------------------
C
      SUBROUTINE T27XIN
C     ***Routine to initialise Tektronix 4025/4027 screen.
      CALL T27XCM('WOR 31',6)
      CALL T27XCM('GRA 1,31',8)
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE T27XCR
C     ***Routine to clear Tektronix 4025/4027 screen.
      CALL T27XCM('WOR 0',5)
      CALL T27XCM('WOR 31',6)
      CALL T27XCM('GRA 1,31',8)
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE T27XCS
C     ***Routine to close Tektronix 4025/4027 screen.
      CALL T27XCM('WOR 0',5)
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE T27XDR(IX,IY)
C     ***Routine to draw line on Tektronix 4025/4027 screen.
      CHARACTER*30 XLINE
      INTEGER NPNTS, OLDX, OLDY, STRTX, STRTY, CURSX, CURSY
      COMMON /TKXDAT/ NPNTS, OLDX, OLDY, STRTX, STRTY, CURSX, CURSY
C
      IF (NPNTS.EQ.0) THEN
           WRITE(XLINE,100) STRTX-OLDX, STRTY-OLDY
 100           FORMAT('RVE',2I5)
           INCHAR=13
           CALL T27XCM(XLINE,INCHAR)
           OLDX=STRTX
           OLDY=STRTY
           ENDIF
      WRITE(XLINE,110) IX-OLDX, IY-OLDY
 110      FORMAT(2I5)
      INCHAR=13
      CALL T27XAV(XLINE,INCHAR)
      NPNTS=NPNTS+1
      OLDX=IX
      OLDY=IY
      STRTX=IX
      STRTY=IY
      IF (NPNTS.GT.90) THEN
           CALL TKXOPB
           ENDIF
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE T27XCM(CMD, LGTH)
C     ***ROUTINE TO BUFFER COMMANDS FOR OUTPUT TO TEK 4027
      CHARACTER*(*) CMD
      INTEGER LGTH
      CHARACTER CHAR
      CHARACTER*200 LINE
      INTEGER LPTR, LLTH
      INTEGER TKXSHR
      COMMON /TKXDT1/ LINE, LPTR, LLTH
C
      IF (LGTH.GT.0) INLGTH=TKXSHR(CMD,LGTH,.TRUE.)
      IF (LGTH.LE.0) INLGTH=LGTH
      IF ((LPTR+INLGTH+1.GT.LLTH).OR.(INLGTH.LT.0)) THEN
           CALL TKXOPB
           IF (INLGTH.LT.0) RETURN
           ENDIF
      LINE(LPTR+1:LPTR+1)=CHAR(20)
      LINE(LPTR+2:LPTR+1+INLGTH)=CMD(1:INLGTH)
      LPTR=LPTR+1+INLGTH
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE T27XAV(CMD, LGTH)
C     ***ROUTINE TO ADD VECTOR DETAILS TO OUTPUT BUFFER FOR TEK 4027
      CHARACTER*(*) CMD
      INTEGER LGTH
      CHARACTER CHAR
      CHARACTER*200 LINE
      INTEGER LPTR, LLTH
      INTEGER TKXSHR
      COMMON /TKXDT1/ LINE, LPTR, LLTH
      character*132 unixbuffer
      INCLUDE 'CTLDT.FOR'
C
      IF (LGTH.LT.0) STOP 'T27XAV: length less than 0'
      INLGTH=TKXSHR(CMD,LGTH,.TRUE.)
      IF (LPTR+INLGTH+1.GT.LLTH) THEN
           IF (TTYPE.EQ.4027) THEN
                LINE(LPTR+1:LPTR+2)='&('
                LPTR=LPTR+2
                CALL TKXOPB
                LPTR=1
                LINE(1:1)=')'
           ELSE
                CALL TKXOPB
                unixbuffer='RVE 0 0'
                unixbuffer(8:7+INLGTH)=CMD(1:INLGTH)
                CALL T27XCM(unixbuffer,INLGTH+7)
                RETURN
                ENDIF
           ENDIF
      LINE(LPTR+1:LPTR+INLGTH)=CMD(1:INLGTH)
      LPTR=LPTR+INLGTH
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE T27XXY(X,Y,CHARIN,COLOUR)
C     ***Routine to read XY cursor for Tektronix 4027
      REAL X,Y
      CHARACTER*1 CHARIN
      INTEGER COLOUR
      CHARACTER*30 BUFFER
      INTEGER NPNTS, OLDX, OLDY, STRTX, STRTY, CURSX, CURSY
      COMMON /TKXDAT/ NPNTS, OLDX, OLDY, STRTX, STRTY, CURSX, CURSY
C
C     *** First, move cursor to old cross-hair cursor position
      WRITE(BUFFER,100) CURSX-OLDX, CURSY-OLDY
 100      FORMAT('RVE',2I5)
      INCHAR=13
      CALL T27XCM(BUFFER,INCHAR)
      OLDX=CURSX
      OLDY=CURSY
C
      CALL TXCTLT(.FALSE.)
      CALL T27XCM('ENA 1',5)
      CALL TKXOPB
      READ(5,110) BUFFER
 110      FORMAT(1A30)
      CALL TXCTLT(.TRUE.)
      IF (BUFFER.EQ.'     ') READ(5,110) BUFFER
      IPTR=INDEX(BUFFER,',')
      IF (IPTR.EQ.0) STOP 'Error - Cursor read error'
      IPTR=IPTR+1
      READ(BUFFER(IPTR:LEN(BUFFER)),120) ICH,IX,IY,ICOL
 120      FORMAT(4(I3,1X))
      X=FLOAT(IX)
      Y=FLOAT(IY)
      CHARIN=CHAR(ICH)
      COLOUR=ICOL
      STRTX=IX
      STRTY=IY
      OLDX=IX
      OLDY=IY
      CURSX=IX
      CURSY=IY
      RETURN
      END
C
C -----------------------------------------------------
C
C     Tektronix 4112 Routines
C     =======================
C
C -----------------------------------------------------
C
      SUBROUTINE T12XPT(IX,IY)
C     ***ROUTINE TO OUTPUT COORDINATES FOR POINT X,Y
      INTEGER OLDHIY,OLDEXT,OLDLOY,OLDHIX,OLDLOX
      LOGICAL LOGHIY,LOGEXT,LOGLOY,LOGHIX,LOGLOX
      COMMON /CX4112/ OLDHIY,OLDEXT,OLDLOY,OLDHIX,OLDLOX
C
C     IFLD GETS N BITS STARTING AT NSTART TH BIT FROM RIGHT
      IFLD(NSTART,NBITS,IWORD)=MOD(IWORD/(2**(NSTART-NBITS)),2**NBITS)+(
     2(1-ISIGN(1,+IWORD))/2)*(2**NBITS-1)
C     ***USE ABOVE CARD IF YOUR MACHINE HAS ONES COMPLEMENT ARITHMETIC
c    2  (1-ISIGN(1,IWORD))/2)*(2**NBITS-MIN0(1,
c    X   MOD(-IWORD,2**(NSTART-NBITS))))
C     ***USE ABOVE CARD IF YOUR MACHINE HAS TWOS COMPLEMENT ARITHMETIC
C
      IEXT=96+(IFLD(2,2,IY)*4)+IFLD(2,2,IX)
      LOGEXT=(IEXT.NE.OLDEXT)
      GO TO 1
C *** ENTRY FOR LOW RESOLUTION 4010 COORDINATES
      ENTRY T10XPT(IX,IY)
      IEXT=0
      LOGEXT=.FALSE.

 1    IHIY=32+IFLD(12,5,IY)
      ILOY=96+IFLD(7,5,IY)
      IHIX=32+IFLD(12,5,IX)
      ILOX=64+IFLD(7,5,IX)
C
      LOGHIY=(IHIY.NE.OLDHIY)
      LOGLOY=(ILOY.NE.OLDLOY)
      LOGHIX=(IHIX.NE.OLDHIX)
C
      LOGLOX=.TRUE.
C
      IF (LOGEXT) LOGLOY=.TRUE.
      IF (LOGHIX) LOGLOY=.TRUE.
C
C<>      WRITE(6,100) IX,IY
C<> 100      FORMAT(' IX = ',O20,' IY = ',O20)
C<>      WRITE(6,110) IHIY,IEXT,ILOY,IHIX,ILOX
C<> 110      FORMAT(' IHIY = ',O7,' IEXT = ',O7,
C<>     $           ' ILOY = ',O7,' IHIX = ',O7,' ILOX = ',O7)
C<>      WRITE(6,120) LOGHIY,LOGEXT,LOGLOY,LOGHIX,LOGLOX
C<> 120      FORMAT(' LOGHIY = ',L1,4X,' LOGEXT = ',L1,4X,
C<>     $           ' LOGLOY = ',L1,4X,' LOGHIX = ',L1,4X,
C<>     $           ' LOGLOX = ',L1)
      IF (LOGHIY) CALL TKXBYT(IHIY)
      IF (LOGEXT) CALL TKXBYT(IEXT)
      IF (LOGLOY) CALL TKXBYT(ILOY)
      IF (LOGHIX) CALL TKXBYT(IHIX)
      IF (LOGLOX) CALL TKXBYT(ILOX)
C
c     For PCPLOT 4105 emulator, all bytes have to be sent
c
      OLDHIY=IHIY
      OLDEXT=IEXT
      OLDLOY=ILOY
      OLDHIX=IHIX
      OLDLOX=ILOX
C
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE T12XRS
C     ***RESET MEMORY FOR PREVIOUS POINT PLOTTED
      INTEGER OLDHIY,OLDEXT,OLDLOY,OLDHIX,OLDLOX
      COMMON /CX4112/ OLDHIY,OLDEXT,OLDLOY,OLDHIX,OLDLOX
      OLDHIY=-1
      OLDEXT=-1
      OLDLOY=-1
      OLDHIX=-1
      OLDLOX=-1
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE T12XIN
C     ***Routine to initialise Tektronix 4112 screen.
C     ***Output <esc>KA1   - Enable dialogue area
C     ***Note: output <del> after <esc> to avoid interception
C     ***      by the concentrator
      CALL T12XCB
      CALL TKXBYT(27)
C      CALL TKXBYT(255)
      CALL TKXSTR('KA1')
C     ***Output <esc>LL3   - Set dialogue area lines = 3
      CALL T12XCB
      CALL TKXBYT(27)
      CALL TKXSTR('LL3')
C     ***Output <esc>LV1   - Set dialogue area visible
      CALL T12XCB
      CALL TKXBYT(27)
      CALL TKXSTR('LV1')
c *** clear graphics area
      call t10xcr
      CALL TKXOPB
      CALL TKXDLY
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE T12XCR
C     ***Routine to clear Tektronix 4112 screen.
C     ***Output <esc>LZ    - Clear dialogue area
      CALL T12XCB
      CALL TKXBYT(27)
      CALL TKXSTR('LZ')
      GO TO 1
C     ***Output <esc><FF>  - Clear screen
      ENTRY T10XCR
 1    CALL T12XCB
      CALL TKXBYT(27)
      CALL TKXBYT(12)
      CALL TKXOPB
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE T12XCS
C     ***Routine to close Tektronix 4112 screen.
C     ***Output <esc>LLB2  - Set dialogue area lines = 34
      CALL T12XCB
      CALL TKXBYT(27)
      CALL TKXSTR('LLB2')
C     ***Output <esc>LV1   - Set dialogue area visibility
      CALL T12XCB
      CALL TKXBYT(27)
      CALL TKXSTR('LV1')
      RETURN
      ENTRY T10XCS
      CALL T12XCB
      CALL TKXBYT(27)
      CALL TKXBYT(12)
C *** ESC140 BACK TO VT100 MODE
      CALL TKXBYT(27)
      CALL TKXSTR('2')
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE T12XDR(IX,IY)
C     ***Routine to draw line on Tektronix 4112 screen.
      INTEGER NPNTS, OLDX, OLDY, STRTX, STRTY, CURSX, CURSY
      COMMON /TKXDAT/ NPNTS, OLDX, OLDY, STRTX, STRTY, CURSX, CURSY
      character*4 csi
      character*2 st
      character*10 buffer
      logical panel
      common /polfil/ panel
C
 1    CONTINUE
c      CALL T12XCB
      IF (NPNTS.EQ.0) THEN
c          ***Polygon fill
c           if (panel) then
c >>> for em4105 emulator
c            csi=char(27)//'[5|'
c            st=char(27)//'\'
c            call tkxopb
c            write(buffer,'(2i5)') strtx,strty
c            call tkxstr(csi//'BEGINPAN'//buffer//',0'//st)
c           end if
C          ***Output <GS> char
           CALL TKXBYT(29)
           CALL T12XPT(STRTX,STRTY+300)
           NPNTS=NPNTS+1
           OLDX=STRTX
           OLDY=STRTY
           ENDIF
      CALL T12XCB
      IF (NPNTS.EQ.0) GOTO 1
      CALL T12XPT(IX,IY+300)
      NPNTS=NPNTS+1
      CALL T12XCB
      OLDX=IX
      OLDY=IY
      STRTX=IX
      STRTY=IY
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE T12epa
C     ***Routine to fill polygon on 41 series
      character*4 csi
      character*2 st
c >>> for em4105 emulator
c            csi=char(27)//'[5|'
c            st=char(27)//'\'
c            call tkxopb
c            call tkxstr(csi//'ENDPAN'//st)
      return
      end
c
C -----------------------------------------------------
C
      SUBROUTINE T10XDR(IX,IY)
C     ***Routine to draw line on Tektronix 4110 screen.
      INTEGER NPNTS, OLDX, OLDY, STRTX, STRTY, CURSX, CURSY
      COMMON /TKXDAT/ NPNTS, OLDX, OLDY, STRTX, STRTY, CURSX, CURSY
C
 1    CONTINUE
      CALL T12XCB
      IF (NPNTS.EQ.0) THEN
C          ***Output <GS> char
           CALL TKXBYT(29)
           CALL T10XPT(STRTX,STRTY+300)
           NPNTS=NPNTS+1
           OLDX=STRTX
           OLDY=STRTY
           ENDIF
      CALL T12XCB
      IF (NPNTS.EQ.0) GOTO 1
      CALL T10XPT(IX,IY+300)
      NPNTS=NPNTS+1
      CALL T12XCB
      OLDX=IX
      OLDY=IY
      STRTX=IX
      STRTY=IY
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE T12XCB
C     ***Routine to check buffer while outputting to 4112
      CHARACTER*200 LINE
      INTEGER LPTR, LLTH
      COMMON /TKXDT1/ LINE, LPTR, LLTH
      INTEGER NPNTS, OLDX, OLDY, STRTX, STRTY, CURSX, CURSY
      COMMON /TKXDAT/ NPNTS, OLDX, OLDY, STRTX, STRTY, CURSX, CURSY
C
      IF (LLTH-LPTR.LT.10) THEN
        NPNTS=0
        CALL T12XRS
        CALL TKXOPB
      END IF
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE T12XXY(X,Y,CHARIN,COLOUR)
C     ***Routine to read XY cursor for Tektronix 4112
      REAL X,Y
      CHARACTER*1 CHARIN
      INTEGER COLOUR
      CHARACTER*20 BUFFER
C
      CALL TKXBYT(27)
      CALL TKXBYT(26)
      CALL TKXOPB
      READ(5,100) BUFFER
 100      FORMAT(1A20)
      COLOUR=0
      CHARIN=BUFFER(1:1)
      IHX=MOD(ICHAR(BUFFER(2:2)),32)
      ILX=MOD(ICHAR(BUFFER(3:3)),32)
      IHY=MOD(ICHAR(BUFFER(4:4)),32)
      ILY=MOD(ICHAR(BUFFER(5:5)),32)
      X=FLOAT((32*IHX+ILX)*4)
      Y=FLOAT(((32*IHY+ILY)*4)-300)
      CURSX=X
      CURSY=Y
      RETURN
      END
C
C -----------------------------------------------------
C
C     Tektronix 4105 Routines
C     =======================
C
      SUBROUTINE T05XIN
C     ***Routine to initialise Tektronix 4105 screen
C     ***Output <esc>%!0   - Set terminal to Tek command mode
      CALL TKXOPB
      CALL TKXSTR(CHAR(27)//'%!0')
C
C     ***Output <esc>RW<0,0><4095,3132> - Set window
      CALL TKXSTR(CHAR(27)//'RW')
      CALL T12XPT(0,0)
      CALL T12XPT(4095,3132)
C
      CALL T12XIN
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE T05XCR
C     ***Routine to clear Tektronix 4105 screen.
C     ***Output <esc>LZ    - Clear dialogue area
      CALL T12XCB
      CALL TKXSTR(CHAR(27)//'LZ')
C     ***Output <esc>FF  - Clear screen
      CALL T12XCB
      CALL TKXSTR(CHAR(27)//'FF')
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE T05XCS
C     ***Routine to close Tektronix 4105 screen
      CALL T12XCR
C
C     ***Output <esc>%!2   - Set terminal to VT100 mode
      CALL T12XCB
      CALL TKXSTR(CHAR(27)//'%!2')
      CALL TKXSTR(CHAR(27)//'KV')
      RETURN
      END
C
C -----------------------------------------------------
C
C     Digital VT240 Routines
C     ======================
C
C -----------------------------------------------------
C
      SUBROUTINE V24XIN
C     ***Routine to initialise Digital VT240 screen.
      CHARACTER*200 LINE
      INTEGER LPTR, LLTH
      COMMON /TKXDT1/ LINE, LPTR, LLTH
C
C     ***Set line length to 80 - VAX changes terminal format when 132.
C     ***Length set to 74 to allow enter REGIS command.
      LLTH=74
C     ***Output <esc>[?4l sequence to set jump scrolling
C     CALL TKXSTR(CHAR(27)//'[?4l')
C     ***Output <esc>[22;24r sequence to set scrolling area.
      CALL TKXSTR(CHAR(27)//'[22;24r')
C     ***Output <esc>[24;0H sequence to move text cursor.
      CALL TKXSTR(CHAR(27)//'[24;0H')
C     ***Output <esc>Pp sequence to enter REGIS
      CALL TKXSTR(CHAR(27)//'Pp')
C     ***Output P[0,419] sequence to move to MINDIS [0,0]
      CALL TKXSTR('P[0,419]')
C     ***Force clear of screen
      CALL V24XCR
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE V24XCR
C     ***Routine to clear Digital VT240 screen.
C     ***Output W(E,S1,I0,P0) sequence - Write erase,Shading,Intensity=0
C                                                            Pattern=0
      CALL TKXSTR('W(E,S1,I0,P0)')
C     ***Output P[0,419]V[,0][799][,419][0]- blanks out graphics area
      CALL TKXSTR('P[0,419]V[,0][799][,419][0]')
C     ***Output W(V,S0,I3,P1) sequence - Write replace, No shading
C                                        Intensity=3, Pattern=1
      CALL TKXSTR('W(V,S0,I3,P1)')
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE V24XCS
C     ***Routine to close Digital VT240 screen.
C
      CALL V24XCR
      CALL TKXOPB
C     ***Output <esc>[0;24r sequence to set whole screen scrolled
      CALL TKXSTR(CHAR(27)//'[0;24r')
C     ***Output <esc>[24;0H sequence to move text cursor
      CALL TKXSTR(CHAR(27)//'[24;0H')
C     ***Output <esc>[?4h sequence to set smooth scrolling
C     CALL TKXSTR(CHAR(27)//'[?4h')
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE V24XDR(IX,IY)
C     ***Routine to draw line on Digital VT240 screen.
      CHARACTER*30 XLINE
      INTEGER TKXSHR
      INTEGER NPNTS, OLDX, OLDY, STRTX, STRTY, CURSX, CURSY
      COMMON /TKXDAT/ NPNTS, OLDX, OLDY, STRTX, STRTY, CURSX, CURSY
C
      IF ((NPNTS.EQ.0).AND.(STRTX.EQ.OLDX).AND.(STRTY.EQ.OLDY)
     $         .AND.(STRTX.EQ.IX).AND.(STRTY.EQ.IY)) RETURN
      IF (NPNTS.EQ.0) THEN
           IXDIF1=STRTX-OLDX
           IYDIF1=-(STRTY-OLDY)
           IF ((IXDIF1.EQ.0).AND.(IYDIF1.EQ.0)) THEN
                WRITE(XLINE,100,ERR=95)
 100                FORMAT(SP,'V',19X)
           ELSEIF (IXDIF1.EQ.0) THEN
                WRITE(XLINE,102,ERR=95) IYDIF1
 102                FORMAT(SP,'P[,',I5,']V',10X)
           ELSEIF (IYDIF1.EQ.0) THEN
                WRITE(XLINE,104,ERR=95) IXDIF1
 104                FORMAT(SP,'P[',I5,']V',11X)
           ELSE
                WRITE(XLINE,106,ERR=95) IXDIF1,IYDIF1
 106                FORMAT(SP,'P[',I5,',',I5,']V',5X)
                ENDIF
           ILEN=TKXSHR(XLINE(1:15),15,.FALSE.)
           CALL TKXSTR(XLINE(1:ILEN))
           OLDX=STRTX
           OLDY=STRTY
           ENDIF
C
C     ***Check for null vector
      IXDIFF=IX-OLDX
      IYDIFF=-(IY-OLDY)
      IF ((NPNTS.GT.0).AND.(IXDIFF.EQ.0).AND.(IYDIFF.EQ.0)) GOTO 5
      IF ((IXDIFF.EQ.0).AND.(IYDIFF.EQ.0)) THEN
           WRITE(XLINE,110,ERR=97)
 110           FORMAT(SP,'[,]',17X)
      ELSEIF (IXDIFF.EQ.0) THEN
           WRITE(XLINE,112,ERR=97)
           WRITE(XLINE,112,ERR=97) IYDIFF
 112           FORMAT(SP,'[,',I5,']',12X)
      ELSEIF (IYDIFF.EQ.0) THEN
           WRITE(XLINE,114,ERR=97) IXDIFF
 114           FORMAT(SP,'[',I5,']',13X)
      ELSE
           WRITE(XLINE,116,ERR=97) IXDIFF,IYDIFF
 116           FORMAT(SP,'[',I5,',',I5,']',7X)
           ENDIF
      ILEN=TKXSHR(XLINE(1:13),13,.FALSE.)
      CALL TKXSTR(XLINE(1:ILEN))
      NPNTS=NPNTS+1
 5    CONTINUE
      OLDX=IX
      OLDY=IY
      STRTX=IX
      STRTY=IY
      RETURN
C
 95   CONTINUE
      WRITE(6,*) 'V24XDR: IXDIF1 = ',IXDIF1,' IYDIF1 = ',IYDIF1
      STOP 'V24XDR: Error generating P command'
C
 97   CONTINUE
      WRITE(6,*) 'V24XDR: IXDIFF = ',IXDIFF,' IYDIFF = ',IYDIFF
      STOP 'V24XDR: Error generating V command'
      END
C
C -----------------------------------------------------
C
      SUBROUTINE V24XXY(X,Y,CHARIN,COLOUR)
C     ***Routine to read XY cursor for Digital VT240
      REAL X,Y
      CHARACTER*1 CHARIN
      INTEGER COLOUR, TKXSHR
      CHARACTER*20 BUFFER
      INTEGER NPNTS, OLDX, OLDY, STRTX, STRTY, CURSX, CURSY
      COMMON /TKXDAT/ NPNTS, OLDX, OLDY, STRTX, STRTY, CURSX, CURSY
      CHARACTER*4 FMT(4)
      DATA FMT /'(I0)','(I1)','(I2)','(I3)'/
C
C     *** First, move cursor to old cross-hair position
      WRITE(BUFFER,100,ERR=95) CURSX-OLDX,-(CURSY-OLDY)
 100      FORMAT(SP,'P[',I5,',',I5,']',6X)
      ILEN=TKXSHR(BUFFER(1:14),14,.FALSE.)
      CALL TKXSTR(BUFFER(1:ILEN))
      OLDX=CURSX
      OLDY=CURSY
C
      CALL TKXSTR('R(P(I))')
      CALL TKXOPB
      READ(5,110) BUFFER
 110      FORMAT(1A20)
      COLOUR=0
      CHARIN=BUFFER(1:1)
      IL=LEN(BUFFER)
      I1=INDEX(BUFFER(2:IL),'[')+1
      I2=INDEX(BUFFER(2:IL),',')+1
      I3=INDEX(BUFFER(2:IL),']')+1
      IF (I1.EQ.IL.OR.I2.EQ.IL.OR.I3.EQ.IL)
     $      STOP 'Error reading cursor - syntax 1'
      IF (.NOT.(I1.LT.I2 .AND. I2.LT.I3))
     $      STOP 'Error reading cursor - syntax 2'
      READ(BUFFER(I1+1:I2-1),FMT(I2-I1)) IX
      READ(BUFFER(I2+1:I3-1),FMT(I3-I2)) IY
      X=FLOAT(IX)
      Y=FLOAT(419-IY)
      CURSX=X
      CURSY=Y
      RETURN
C
 95   CONTINUE
      STOP 'V24XXY: Error generating P command'
      END
C
C -----------------------------------------------------
C

