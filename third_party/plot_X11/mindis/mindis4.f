C
C     ***MINDIS - SACLANTCEN GRAPH PLOTTING PACKAGE
C
C     ***JOHN STEIERT   -   10-DEC-81
C
C     ***AMMENDMENTS:
C
C      JS   4-NOV-82   1) PLOT CORRECT SYMBOL
C                      2) INSERT "=",":","," SYMBOLS
C                      3) PUT DOTS ON SMALL I AND J
C                      4) CHANGE SYMXDT COMMON TO SMXDAT
C      JS   7-JUN-83   Standard Fortran version
C      JS  15-Jun-83   Handle illegal characters properly
C      JS  13-Feb-84   Use the Hershey character generator
C      JS  20-Feb-84   1) Allow for symbol expansion and rotation
C                      2) New routine CHXALF for setting fonts
C      JS  23-Mar-84   Allow 9 shift chars instead of 6.
C
C -----------------------------------------------------
C
C     ***ROUTINES TO HANDLE DRAWING TEXTUAL CHARACTERS
C     ***AND MARKERS.
C
      SUBROUTINE CHXINI
C     ***ROUTINE TO INITIALISE CHARACTER POINTER ARRAYS
      INTEGER SMCHRS(122), SYMBLS(15)
      COMMON /SMXDAT/ SMCHRS, SYMBLS
C
C     ***SET UP PLOTTING SYMBOL CHARACTERS
      IPTR=0
      DO 59 I=1,15
           IPTR=IPTR+1
           SYMBLS(I)=IPTR
 55        CONTINUE
                IPTR=IPTR+1
                IVAL=SMCHRS(IPTR)
                IF (IVAL.NE.99) GOTO 55
 59        CONTINUE
      RETURN
      END
C
C *********************************************************
C
      SUBROUTINE CHXSYM(X,Y,ISYM)
C     ***ROUTINE TO PLOT SYMBOL <ISYM> AT POINT <X,Y>
      INTEGER SMCHRS(122), SYMBLS(15)
      COMMON /SMXDAT/ SMCHRS, SYMBLS
C      WRITE(6,100) ISYM,X,Y
C 100      FORMAT(' Plot char ',I3,' at (',2E12.3,' )')
      ISYMPT=SYMBLS(ISYM+1)
      DO 1 I=0,999
           IVAL=SMCHRS(ISYMPT+I)
           IF (IVAL.EQ.99) RETURN
           CALL CHXOS1(X,Y,IVAL)
 1         CONTINUE
      RETURN
      END
C
      SUBROUTINE CHXOS1(X,Y,IVAL)
C     ***ROUTINE TO OUTPUT LINE/MOVE
      INCLUDE 'SYMDT.FOR'
      INCLUDE 'CTLDT.FOR'
      LOGICAL LDRAW
      LDRAW=.TRUE.
      IF (IVAL.LT.0) THEN
           LDRAW=.FALSE.
           IVAL=-IVAL
           ENDIF
      IY1=(IVAL-1)/5
      IX1=IVAL-(IY1*5)-1
      IY1=4-IY1
      IX2=IX1-2
      IY2=IY1-2
      X2=FLOAT(IX2)*SYMSTP*SYMSCL
      Y2=FLOAT(IY2)*SYMSTP*SYMSCL
      IF (SYMANG.NE.0) THEN
           RTHETA=SYMANG
           IF (X2.NE.0.0) RPHI=ATAN(Y2/X2)
           IF (X2.EQ.0.0) RPHI=RPI/2.0
           RLEN=(X2**2.0+Y2**2.0)**0.5
           X2=COS(RTHETA+RPHI)*RLEN
           Y2=SIN(RTHETA+RPHI)*RLEN
           ENDIF
C      WRITE(6,100) LDRAW,X2,Y2
C 100      FORMAT(' DRAW = ',1L1,' REL X,Y = ',2F10.3)
      IF (LDRAW) THEN
           CALL BLXDRW(X+X2, Y+Y2)
      ELSE
           CALL BLXMOV(X+X2, Y+Y2)
           ENDIF
      RETURN
      END
C
C **************************************************************
C
      SUBROUTINE CHXLIN(X,Y,LINE)
C     ***ROUTINE TO OUTPUT <LINE> ON PLOTTER
      INCLUDE 'MSGDT.FOR'
      INCLUDE 'CTLDT.FOR'
      CHARACTER*(*) LINE
      CHARACTER*100 SP
      LOGICAL LUPCAS, LITALI
      CHARACTER*1 NXTCHR
      INTEGER CKXSHF
      LOGICAL TRACE2
      COMMON /TRXDT2/ TRACE2
      INTEGER STGREK, STCASE, STPOSN, STFONT
      INTEGER XXGREK, XXCASE, XXPOSN, XXFONT
      COMMON /CHXCMN/ STGREK, STCASE, STPOSN, STFONT,
     $                XXGREK, XXCASE, XXPOSN, XXFONT
      CHARACTER*1 CHBUFF(200)
      COMMON /CHXCM2/ CHBUFF, IBFPTR
C
      SP ='   '
      DO 1 LLEN=1, LEN(line)
           IF (LINE(LLEN:LLEN).EQ.'$') then
            ilen=llen
            GOTO 2
           end if
 1         CONTINUE
      STOP '$ not found in string'
C
 2    CONTINUE
      ILEN=ILEN-1
      IF (TRACE2) WRITE(6,100) X,Y, LINE(1:ILEN),SP(ILEN+1:100)
 100      FORMAT(' CHXLIN called: X = ',F10.5,' Y = ',F10.5/
     $           ' LINE = ',A,A)
C
      IBFPTR=0
      XXGREK=2
      XXPOSN=1
      XXCASE=2
      XXFONT=1
      IF (CHALPH.EQ.1) THEN
C          ***Simplex
           STFONT=1
      ELSEIF (CHALPH.EQ.2) THEN
C          ***Complex
           STFONT=2
      ELSEIF (CHALPH.EQ.3) THEN
C          ***Duplex
           STFONT=3
      ELSE
           STOP 'Illegal alphabet type'
           ENDIF
      STGREK=2
      STCASE=2
      STPOSN=1
C
      IF (LFONT(1)) CALL CHXSTF(FONTNO(1))
      DO 3 I=1, ILEN
           NXTCHR=LINE(I:I)
           IFONTN=CKXSHF(NXTCHR)
           IF (IFONTN.NE.0) THEN
C
C               ***Shift character in
c                write(6,*) 'CHXLIN: CHXSTF called'
                CALL CHXSTF(IFONTN)
           ELSE
C
C               ***Normal character: Handle it.
                IF ((NXTCHR.GE.'0'.AND.NXTCHR.LE.'9') .OR.
     $               NXTCHR.EQ.' ' .OR.
     $               NXTCHR.EQ.'+' .OR. NXTCHR.EQ.'-' .OR.
     $               NXTCHR.EQ.'/' .OR. NXTCHR.EQ.'=' .OR.
     $               NXTCHR.EQ.'(' .OR. NXTCHR.EQ.')' .OR.
     $               NXTCHR.EQ.'*' .OR. NXTCHR.EQ.',' .OR.
     $               NXTCHR.EQ.'.' .OR. NXTCHR.EQ.'$')
     $          THEN
                     CALL CHXCHK
                     CALL CHXOPC(NXTCHR)
                ELSEIF ((NXTCHR.GE.'A'.AND.NXTCHR.LE.'Z').OR.
     $                  (NXTCHR.GE.'a'.AND.NXTCHR.LE.'z')) THEN
                     LUPCAS=(NXTCHR.GE.'A'.AND.NXTCHR.LE.'Z')
                     IF (.NOT.LUPCAS)
     $                  NXTCHR=CHAR(ICHAR(NXTCHR)-ICHAR('a')+ICHAR('A'))
                     ISVCAS=STCASE
                     IF (.NOT.LUPCAS) STCASE=1
                     CALL CHXCHK
                     CALL CHXOPC(NXTCHR)
                     STCASE=ISVCAS
                ELSE
                     ISVGRE=STGREK
                     ISVCAS=STCASE
                     ISVFON=STFONT
                     STGREK=1
                     STCASE=1
                     STFONT=4
                     LITALI=(ISVGRE.EQ.2.AND.ISVFON.EQ.4)
                     IF (NXTCHR.EQ.'!') THEN
                          IF (LITALI) THEN
                               NXTCHR='N'
                          ELSE
                               NXTCHR='H'
                               ENDIF
                     ELSEIF (NXTCHR.EQ.'?') THEN
                          IF (LITALI) THEN
                               NXTCHR='O'
                          ELSE
                               NXTCHR='I'
                               ENDIF
                     ELSEIF (NXTCHR.EQ.':') THEN
                          IF (LITALI) THEN
                               NXTCHR='L'
                          ELSE
                               NXTCHR='F'
                               ENDIF
                     ELSEIF (NXTCHR.EQ.';') THEN
                          IF (LITALI) THEN
                               NXTCHR='M'
                          ELSE
                               NXTCHR='G'
                               ENDIF
                     ELSEIF (NXTCHR.EQ.'$') THEN
                         NXTCHR='C'
                     ELSEIF (NXTCHR.EQ.'%') THEN
                         NXTCHR='J'
                     ELSEIF (NXTCHR.EQ.'&') THEN
                         NXTCHR='K'
                     ELSEIF (NXTCHR.EQ.'<') THEN
                         NXTCHR='A'
                         STCASE=2
                     ELSEIF (NXTCHR.EQ.'>') THEN
                         NXTCHR='B'
                         STCASE=2
                     ELSE
                          WRITE(6,110) NXTCHR, ICHAR(NXTCHR)
 110                          FORMAT(' Illegal character: ',A1,I6)
                          NXTCHR=CHAR(0)
                          ENDIF
                     IF (ICHAR(NXTCHR).NE.0) THEN
                          CALL CHXCHK
                          CALL CHXOPC(NXTCHR)
                          ENDIF
                     STGREK=ISVGRE
                     STCASE=ISVCAS
                     STFONT=ISVFON
                     ENDIF
                ENDIF
 3         CONTINUE
      CALL CHXOPL(X,Y,MSGHGT,MSGANG*180.0/RPI)
      RETURN
      END
C
c      SUBROUTINE BLCKX3
      block data BLCKX3
      INTEGER SMCHRS(122), SYMBLS(15)
      COMMON /SMXDAT/ SMCHRS, SYMBLS
      DATA SMCHRS /  -1,  5, 25, 21,  1, 99,
     $               -2,  4, 10, 20, 24, 22, 16,  6,  2, 99,
     $              -21,  3, 25, 21, 99,
     $               -3, 23,-11, 15, 99,
     $               -1, 25,-21,  5, 99,
     $              -11,  3, 15, 23, 11, 99,
     $               -1,  5, 23,  1, 99,
     $               -1,  5, 25, 21,  1, 25, -5, 21, 99,
     $               -1, 25,-21,  5,-11, 15, 99,
     $              -11,  3, 15, 23, 11, 15, -3, 23, 99,
     $               -2,  4, 10, 20, 24, 22, 16,  6,  2,
     $               -3, 23,-11, 15, 99,
     $               -1,  5, 23, 25,  3, 21, 23,  1, 99,
     $               -1,  5, 25, 21,  1, -3, 23,-11, 15, 99,
     $               -2,  4, 10, 20, 24, 22, 16,  6,  2,
     $               -1, 25,-21,  5, 99,
     $              -21,  1,  5, 25, 21,  3, 25, 99
     $              /
C
      END
C
C -----------------------------------------------------
C
      SUBROUTINE CHXALF(IFONT,ALPHA,LSHIFT)
C     ***Routine to set font IFONT to ALPHA using shift char LSHIFT.
      CHARACTER*(*) ALPHA
      CHARACTER*1 LSHIFT
      INCLUDE 'MSGDT.FOR'
      CHARACTER*6 ALPHAS(12),alfbuf
      DATA ALPHAS / 'STANDA', 'L/CSTD', 'ITALIC', 'L/CITA',
     $              'SPECIA', 'MATHEM', 'GREEK ', 'L/CGRE',
     $              'SUBSCR', 'SUPERS', 'ENDSCR', 'BACKSP' /
C
      IF (LSHIFT.EQ.'$') STOP 'Shift character must not be $'
      IF (IFONT.LT.1.OR.IFONT.GT.9) STOP 'Font no. out of range'
      DO 1 IFONTN=1,12
           alfbuf=alphas(ifontn)
           iibuf=ifontn
           IF (ALPHA.EQ.alfbuf(1:len(ALPHA)))then
c            write(6,*) 'Font ',alpha,' is defined as shift: ',lshift
            GOTO 2
           end if
 1         CONTINUE
      WRITE(6,*) 'Invalid font: ',ALPHA
      STOP
C
 2    CONTINUE
      LFONT(IFONT)=.TRUE.
      CHSHFT(IFONT)=LSHIFT
      FONTNO(IFONT)=iibuf
c      write(6,*) ifont,lfont(ifont),chshft(ifont),fontno(ifont)
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE CHXCHK()
C     ***Routine to output required control characters
      INTEGER XSUBSC, XSUPER, XUPPER, XLOWER, XCOMPL, XGREEK, XSIMPL
      INTEGER XBACKS, XITALI, XDUPLX, XTERM,  XENDSU, XENDGR
      PARAMETER (XSUBSC=14,XSUPER=15,XUPPER=16,XLOWER=17,XCOMPL=18)
      PARAMETER (XGREEK=19,XSIMPL=20,XBACKS=21,XITALI=22,XDUPLX=23)
      PARAMETER (XTERM =24,XENDSU=25,XENDGR=26)
C
      INTEGER STGREK, STCASE, STPOSN, STFONT
      INTEGER XXGREK, XXCASE, XXPOSN, XXFONT
      COMMON /CHXCMN/ STGREK, STCASE, STPOSN, STFONT,
     $                XXGREK, XXCASE, XXPOSN, XXFONT
C
      IF (XXPOSN.NE.STPOSN) THEN
           IF (STPOSN.EQ.1) THEN
                CALL CHXOPC(CHAR(XENDSU))
           ELSEIF (STPOSN.EQ.2) THEN
                CALL CHXOPC(CHAR(XSUBSC))
           ELSE
                CALL CHXOPC(CHAR(XSUPER))
                ENDIF
           XXPOSN=STPOSN
           ENDIF
      IF (XXCASE.NE.STCASE) THEN
           IF (STCASE.EQ.1) THEN
                CALL CHXOPC(CHAR(XLOWER))
           ELSE
                CALL CHXOPC(CHAR(XUPPER))
                ENDIF
           XXCASE=STCASE
           ENDIF
      IF (XXFONT.NE.STFONT) THEN
           IF (STFONT.EQ.1) THEN
                CALL CHXOPC(CHAR(XSIMPL))
           ELSEIF (STFONT.EQ.2) THEN
                CALL CHXOPC(CHAR(XCOMPL))
           ELSEIF (STFONT.EQ.3) THEN
                CALL CHXOPC(CHAR(XDUPLX))
           ELSE
                CALL CHXOPC(CHAR(XITALI))
                ENDIF
           XXFONT=STFONT
           ENDIF
      IF (XXGREK.NE.STGREK) THEN
           IF (STGREK.EQ.1) THEN
                CALL CHXOPC(CHAR(XGREEK))
           ELSE
                CALL CHXOPC(CHAR(XENDGR))
                ENDIF
           XXGREK=STGREK
           ENDIF
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE CHXOPC(CHARAC)
C     ***Subroutine to put output character into buffer
      CHARACTER*1 CHARAC
      CHARACTER*1 CHBUFF(200)
      COMMON /CHXCM2/ CHBUFF, IBFPTR
      IBFPTR=IBFPTR+1
      IF (IBFPTR.GT.200) STOP 'Char buffer overflow'
C     IF (ICHAR(CHARAC).LE.26) WRITE(6,*) 'Ctl char: ',ICHAR(CHARAC)
C      IF (ICHAR(CHARAC).GT.26) WRITE(6,*) 'Normal char: ',CHARAC
      CHBUFF(IBFPTR)=CHARAC
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE CHXOPL(X,Y,HEIGHT,THETA)
C     ***Routine to output buffer.
      INTEGER XTERM
      PARAMETER (XTERM =24)
      CHARACTER*1 CHBUFF(200)
      COMMON /CHXCM2/ CHBUFF, IBFPTR
      CALL CHXOPC(CHAR(XTERM))
      CALL CGXSMB(X,Y,HEIGHT,CHBUFF,THETA,IBFPTR)
      RETURN
      END
C
C -----------------------------------------------------
C
      INTEGER FUNCTION CKXSHF(CHR)
C     ***Routine to check if shift character has been received.
      INCLUDE 'MSGDT.FOR'
      CHARACTER*1 CHR
c      write(6,*) 'CKXSHF: CHR= ',CHR
      DO 1 I=1,9
           IF (LFONT(I)) THEN
                IF (CHSHFT(I).EQ.CHR) then
c                 write(6,*) 'CKXSHF: Shift character',CHR,' found'
                 GOTO 2
                end if
           ENDIF
 1    CONTINUE
      CKXSHF=0
      RETURN
C
 2    CONTINUE
      CKXSHF=FONTNO(I)
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE CHXSTF(IFONT)
C     ***Routine to switch to font IFONT
      INTEGER XBACKS
      PARAMETER (XBACKS=21)
      INTEGER STGREK, STCASE, STPOSN, STFONT
      INTEGER XXGREK, XXCASE, XXPOSN, XXFONT
      COMMON /CHXCMN/ STGREK, STCASE, STPOSN, STFONT,
     $                XXGREK, XXCASE, XXPOSN, XXFONT
      INCLUDE 'MSGDT.FOR'
C
      IF (IFONT.EQ.9) THEN
C          ***Subscript
           STPOSN=2
      ELSEIF (IFONT.EQ.10) THEN
C          ***Superscript
           STPOSN=3
      ELSEIF (IFONT.EQ.11) THEN
C          ***End of subscript/superscript
           STPOSN=1
      ELSEIF (IFONT.EQ.7) THEN
C          ***Greek
           STGREK=1
           STCASE=2
           IF (CHALPH.EQ.3) THEN
C               ***Duplex
                STFONT=2
           ELSE
C               ***Simplex or complex
                STFONT=CHALPH
                ENDIF
      ELSEIF (IFONT.EQ.8) THEN
C          ***Lower case greek
           STGREK=1
           STCASE=1
           IF (CHALPH.EQ.3) THEN
C               ***Duplex
                STFONT=2
           ELSE
C               ***Simplex or complex
                STFONT=CHALPH
                ENDIF
      ELSEIF (IFONT.EQ.6) THEN
C          ***Mathematical
           STGREK=1
           STCASE=2
           STFONT=4
      ELSEIF (IFONT.EQ.5) THEN
C          ***Special
           STGREK=1
           STCASE=1
           STFONT=4
      ELSEIF (IFONT.EQ.3) THEN
C          ***Italic
           STGREK=2
           STCASE=2
           STFONT=4
      ELSEIF (IFONT.EQ.4) THEN
C          ***Lower case italic
           STGREK=2
           STCASE=1
           STFONT=4
      ELSEIF (IFONT.EQ.1) THEN
C          ***Standard
           STGREK=2
           STCASE=2
           STFONT=CHALPH
      ELSEIF (IFONT.EQ.2) THEN
C          ***Lower case standard
           STGREK=2
           STCASE=1
           STFONT=CHALPH
      ELSEIF (IFONT.EQ.12) THEN
C          ***Backspace
           CALL CHXOPC(CHAR(XBACKS))
      ELSE
           STOP 'Illegal font no'
           ENDIF
      RETURN
      END
C
C -----------------------------------------------------
C
      REAL FUNCTION CHXLTH(LINE)
C     ***Routine to find length of line LINE
      CHARACTER*(*) LINE
      DIMENSION IA(256),IB(60),OLDX(6),OLDY(6)
      LOGICAL LNGTH,FIRST
      COMMON /G99SAV/ XL,OLDX,OLDY,IA,IB,NWORD,NCHAR,NCWD,IOUT
      COMMON /G99LOG/ FIRST,LNGTH
C
      LNGTH=.TRUE.
      CALL CHXLIN(0.0,0.0,LINE)
      CHXLTH=XL
      LNGTH=.FALSE.
      RETURN
      END








