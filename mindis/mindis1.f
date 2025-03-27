C
C     ***MINDIS - SACLANTCEN GRAPH PLOTTING PACKAGE
C
C     ***JOHN STEIERT   -   02-DEC-81
C
C -----------------------------------------------------
C
C     AMMENDED:
C           JS   02-FEB-82   USE CHANNEL 48,99 (NOT 10,11)
C           JS   23-MAR-82   GET NO OF PLACES RIGHT IN CART LABELS
C           JS   08-JUL-82   CORRECT ROUNDING ERROR ON LABEL NUMBERS
C           JS   02-AUG-82   DO WINDOWING WHEN PLOTTING POINTS
C           JS   22-SEP-82   PLXCLR CALLS TKXCLS INSTEAD OF TKXCLR
C                            PLXCLS NO LONGER CALLS TKXCLS
C           JS   04-NOV-82   OUTPUT MARKERS CORRECTLY FOR SPLINE
C           JS   25-JAN-83   MAKE VECTOR RTN OUTPUT DOTTED LINES ETC
C           JS    2-Jun-83   Standard FORTRAN version.
C           JS    9-Jun-83   Change data files to Ascii (not Binary)
C                            Allow Printronix to be used for hardcopy
C                            Use TERMXT routine to find terminal type
C           JS   15-Jun-83   Prompt for security level
C                            Output run id and security level on plot
C           JS   25-Jul-83   Return to Binary data files
C           JS   23-Aug-83   Allow Applicon to be used as hardcopy
C           JS    6-Oct-83   Don't reset trace flags - set F in Batch
C           JS   31-Oct-83   Allow use of Tektronix 4112 terminal.
C           JS   21-Feb-84   1) Set alphabet to Simplex before plot title
C                            2) Call CHXLTH to calculate text lengths.
C           JS    9-Mar-84   Put underlining into PLXHDG
C           JS   16-Mar-84   In PLXVLS use new routines UNXMOV, DRW + CVT
C           JS   20-Mar-84   Fix problem with last axis point not output.
C           JS   26-Mar-84   Split DRXCAX into DRXCXX and DRXCXY.
C           JS   29-Mar-84   1) allow axes not to be drawn.
C                            2) allow variable length lines in story etc.
C                            3) allow variable inter-line spacing in story etc.
C           JS    6-Apr-84   Allow messages to be offset.
C           JS   16-Jul-84   Allow use of Digital VT240 terminal.
C           JS    5-Jan-85   Routines to read XY cursor
C           JS   15-Feb-85   Use Tektronix 4691 instead of Applicon.
C           JS   24-Sep-85   Allow use of Tektronix 4105
C           JS   25-Sep-85   Allow use of Central Printronix
C	    JS   15-May-86   Move hardcopy device selection to MINDIS6
C	    JS   23-May-86   Allow optional page border
C
C -----------------------------------------------------
C
      SUBROUTINE GRXINI
C     ***GRAPHICS INITIALISATION - LEVEL 2
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'NAMDT.FOR'
      CHARACTER*10 REPLY
      CHARACTER*1 PADCHS, CHAR
      CHARACTER*6 RUNIDX
      LOGICAL GTXBCH
      LOGICAL TRACE, TRACE2
      INTEGER GTXTTY
      CHARACTER*5 GTXMOD
      COMMON /TRXDAT/ TRACE
      COMMON /TRXDT2/ TRACE2
      COMMON /GRXDT3/ RSCALE
C
      IF (TRACE2) WRITE(6,100)
 100      FORMAT(' GRXINI called')
C
      LBATCH=GTXBCH()
C     ***GET RUN IDENTIFICATION
C     ***NB: TRICKY COS V8 + V9 COMPILERS RETURN IT IN DIFF PLACES
      CALL GTXRID(RUNIDX, PLTRIL)
      PADCHS=CHAR(20)
      DO 1 I=1, 6
           IF (RUNIDX(I:I).GT.PADCHS) GOTO 2
 1         CONTINUE
 2    CONTINUE
      PLTRID=RUNIDX(I:I+PLTRIL-1)
      DEFHCD=GTXMOD()
      IF (.NOT.LBATCH) THEN
           IF (SECRTY.LT.0) THEN
                TTYPE=GTXTTY()
                LTTCRT=((TTYPE.EQ.4025).OR.(TTYPE.EQ.4027).OR.
     $                  (TTYPE.EQ.4105).OR.(TTYPE.EQ.4112).OR.
     $                  (TTYPE.EQ.4010).or.(TTYPE.EQ.4014).OR.
     $                  (TTYPE.EQ.240).or.(ttype.eq.1011))
                IF (.NOT.LTTCRT) WRITE(6,*) ' Non-graphics terminal - ',
     $                                      'plotting done on Plotter'
c 6              WRITE(6,120)
c 120                FORMAT(' Plot security classification ',
c    $                '(0=Unclass,1=Conf...)')
c               READ(5,200) SECRTY, REPLY
c200                FORMAT(I1,1A10)
c               TRACE =(REPLY(2:2).EQ.'Y')
c               TRACE2=(REPLY(3:3).EQ.'Y')
c               IF ((SECRTY.LT.0).OR.(SECRTY.GT.3)) GOTO 6
                SECRTY=0
                ENDIF
      ELSE
C          ***BATCH RUN - SET VALUES
c           WRITE(6,122)
c122           FORMAT(/' SECURITY = UNCLASSIFIED - SEE COM DEPT',
c    $                 ' FOR OTHER SECURITY RATINGS'/)
           SECRTY=0
           TRACE =.FALSE.
           TRACE2=.FALSE.
           LTTCRT=.FALSE.
           ENDIF
      RSCALE=1.0
      IF (LTTCRT) THEN
           CALL TKXINI
           ENDIF
      CALL PLXFOP
      WRITE(48) 2,FLOAT(SECRTY),0.0
      CALL CHXINI
      CALL TKXSTP(1)
      RETURN
C
C     ***Dummy call to a routine which force loads modules.
 19   CALL GNXINI
      END
C
C -----------------------------------------------------
C
      SUBROUTINE DRXBLN(IBLNO, IFRAME)
C     ***DRAW FRAME AROUND BLANK AREA
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'BLNKD.FOR'
      LOGICAL LTMP
      LOGICAL TRACE2
      COMMON /TRXDT2/ TRACE2
      IF (TRACE2) WRITE(6,100) IBLNO, IFRAME
 100      FORMAT(' DRXBLN CALLED: IBLNO = ',I5,' IFRAME = ',I5)
C
      IF (IFRAME.NE.0) THEN
           LTMP=BLNKON(IBLNO)
           BLNKON(IBLNO)=.FALSE.
           DO 1 I=1, IFRAME
                DVAL=FLOAT(I-1)*0.01
                CALL BLXMOV(BLNKX1(IBLNO)+DVAL,BLNKY1(IBLNO)+DVAL)
                CALL BLXDRW(BLNKX1(IBLNO)+DVAL,BLNKY2(IBLNO)-DVAL)
                CALL BLXDRW(BLNKX2(IBLNO)-DVAL,BLNKY2(IBLNO)-DVAL)
                CALL BLXDRW(BLNKX2(IBLNO)-DVAL,BLNKY1(IBLNO)+DVAL)
                CALL BLXDRW(BLNKX1(IBLNO)+DVAL,BLNKY1(IBLNO)+DVAL)
 1              CONTINUE
           BLNKON(IBLNO)=LTMP
           ENDIF
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE PLXVLS(XARAY,YARAY,NPNTS,IMARK)
C     ***ROUTINE TO PLOT VALUES
      INCLUDE 'POLDT.FOR'
      INCLUDE 'CARTD.FOR'
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'SIZDT.FOR'
      REAL XARAY(NPNTS),YARAY(NPNTS)
      REAL RARRYC(400), RARRYW(400), RARRYV(5), RARRYD(2)
      LOGICAL LOGSAV
      LOGICAL TRACE2
      COMMON /TRXDT2/ TRACE2
C
      IF (TRACE2) WRITE(6,100) NPNTS, IMARK,
     $                         (XARAY(I),YARAY(I),I=1,NPNTS)
 100      FORMAT(' PLXVLS called: NPNTS = ',I5,' IMARK = ',I5/
     $           ' Data values:'/(' ',E14.7,' ',E14.7))
C
      IF (CHECK) WRITE(6,105)
 105      FORMAT(' Note: out-of-bound reporting ',
     $           'not yet implemented')
      LOGSAV=LWNDOW
      SPLAX1=SPLAX1-0.5
      SPLAX2=SPLAX2+0.5
      SPLAY1=SPLAY1-0.5
      SPLAY2=SPLAY2+0.5
      LWNDOW=.TRUE.
      IF (IMARK.GE.0) THEN
C          ***DRAW LINES
           IF (INTERP.EQ.0) THEN
C
C               ***STRAIGHT LINES BETWEEN POINTS
                CALL UNXMOV(XARAY(1),YARAY(1),.TRUE.)
                DO 2 I=2,NPNTS
                     CALL UNXDRW(XARAY(I),YARAY(I),.TRUE.)
 2                   CONTINUE
           ELSEIF (INTERP.EQ.1) THEN
C
C               ***CUBIC SPLINE INTERPOLATION
C
                RARRYD(1)=0.0
                RARRYD(2)=0.0
                stop '>>>> SPLINE INTERPOLATION NOT IMPLEMENTED <<<<'
c                CALL SPLN1(NPNTS,XARAY,YARAY,2,RARRYD,RARRYC,RARRYW)
                RVALMI=XARAY(1)
                RVALMX=XARAY(NPNTS)
                RSTEP=(RVALMX-RVALMI)/FLOAT((NPNTS-1)*5)
                CALL UNXMOV(XARAY(1),YARAY(1),.TRUE.)
                DO 6 I=1,((NPNTS-1)*5)
                     RARRYV(1)=RVALMI+(FLOAT(I)*RSTEP)
                     RARRYV(5)=2.0
c                     CALL SPLN2(NPNTS,XARAY,YARAY,RARRYC,RARRYV)
                     X=(RVALMI+(FLOAT(I)*RSTEP))
                     Y=RARRYV(2)
                     CALL UNXDRW(X,Y,.TRUE.)
 6                   CONTINUE
                ENDIF
           ENDIF
C
      IF (IMARK.NE.0) THEN
C          ***DRAW MARKERS
           IMARK1=IABS(IMARK)
           DO 7 I=1, NPNTS, IMARK1
                CALL UNXCVT(XARAY(I),YARAY(I),X,Y)
                CALL CHXSYM(X,Y,CURSYM)
 7              CONTINUE
           ENDIF
      LWNDOW=LOGSAV
      SPLAX1=SPLAX1+0.5
      SPLAX2=SPLAX2-0.5
      SPLAY1=SPLAY1+0.5
      SPLAY2=SPLAY2-0.5
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE PLXVLP(XARAY,YARAY,NPNTS)
C     ***ROUTINE TO DRAW FILLED POLYGON
      INCLUDE 'POLDT.FOR'
      INCLUDE 'CARTD.FOR'
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'SIZDT.FOR'
      REAL XARAY(NPNTS),YARAY(NPNTS)
      REAL RARRYC(400), RARRYW(400), RARRYV(5), RARRYD(2)
      LOGICAL LOGSAV
      LOGICAL TRACE2
      COMMON /TRXDT2/ TRACE2
C
      IF (TRACE2) WRITE(6,100) NPNTS,
     $                         (XARAY(I),YARAY(I),I=1,NPNTS)
 100      FORMAT(' PLXVPS called: NPNTS = ',I5/
     $           ' Data values:'/(' ',E14.7,' ',E14.7))
C
      IF (CHECK) WRITE(6,105)
 105      FORMAT(' Note: out-of-bound reporting ',
     $           'not yet implemented')
      LOGSAV=LWNDOW
      SPLAX1=SPLAX1-0.5
      SPLAX2=SPLAX2+0.5
      SPLAY1=SPLAY1-0.5
      SPLAY2=SPLAY2+0.5
      LWNDOW=.TRUE.
C >>> START NEW PATH FOR DEFINING FILL AREA
      CALL BGNFLL
C          ***DRAW LINES
           IF (INTERP.EQ.0) THEN
C
C               ***STRAIGHT LINES BETWEEN POINTS
                CALL UNXMOV(XARAY(1),YARAY(1),.FALSE.)
                DO 2 I=2,NPNTS
                     CALL UNXDRW(XARAY(I),YARAY(I),.FALSE.)
 2                   CONTINUE
           ELSEIF (INTERP.EQ.1) THEN
C
C               ***CUBIC SPLINE INTERPOLATION
C
                RARRYD(1)=0.0
                RARRYD(2)=0.0
                stop '>>>> SPLINE INTERPOLATION NOT IMPLEMENTED <<<<'
c                CALL SPLN1(NPNTS,XARAY,YARAY,2,RARRYD,RARRYC,RARRYW)
                RVALMI=XARAY(1)
                RVALMX=XARAY(NPNTS)
                RSTEP=(RVALMX-RVALMI)/FLOAT((NPNTS-1)*5)
                CALL UNXMOV(XARAY(1),YARAY(1),.FALSE.)
                DO 6 I=1,((NPNTS-1)*5)
                     RARRYV(1)=RVALMI+(FLOAT(I)*RSTEP)
                     RARRYV(5)=2.0
c                     CALL SPLN2(NPNTS,XARAY,YARAY,RARRYC,RARRYV)
                     X=(RVALMI+(FLOAT(I)*RSTEP))
                     Y=RARRYV(2)
                     CALL UNXDRW(X,Y,.FALSE.)
 6                   CONTINUE
                ENDIF
C >>> FILL POLYGON
      CALL ENDFLL
C

      LWNDOW=LOGSAV
      SPLAX1=SPLAX1+0.5
      SPLAX2=SPLAX2-0.5
      SPLAY1=SPLAY1+0.5
      SPLAY2=SPLAY2-0.5
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE PLXCLS
C     ***ROUTINE TO DO FINAL PLOT CLOSING
      INCLUDE 'CTLDT.FOR'
      LOGICAL TRACE2
      COMMON /TRXDT2/ TRACE2
      IF (TRACE2) WRITE(6,100)
 100      FORMAT(' PLXCLS called')
      IF (LTTCRT) CALL TKXOPB
C     IF (LTTCRT) CALL TKXCLS              ***JS 22-SEP-82***
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE PRXSUM
C     ***ROUTINE TO PRINT PLOT SUMMARY
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'NAMDT.FOR'
      CHARACTER*17 ASECUR
      LOGICAL TRACE2
      COMMON /TRXDT2/ TRACE2
C
      IF (TRACE2) WRITE(6,100)
 100      FORMAT(' PRXSUM called')
C
      ASECUR='MIT MINDIS'
c      IF (SECRTY.EQ.0) ASECUR='NATO Unclassified'
c      IF (SECRTY.EQ.1) ASECUR='NATO Confidential'
c      IF (SECRTY.EQ.2) ASECUR='NATO Secret      '
c      IF (SECRTY.EQ.3) ASECUR='NATO Cosmic      '
      WRITE (6,200) PLTNO, PLTDAT(1:2), PLTDAT(3:4), PLTDAT(5:6),
     $                     PLTTIM(1:2), PLTTIM(3:4), PLTTIM(5:6),
     $               PLTRID, ASECUR
 200      FORMAT('                     Summary'/
     $           '                     ======='//
     $           ' Plot no  = ',I6/
     $           ' Date     = ',1A2,'-',1A2,'-',1A2/
     $           ' Time     = ',1A2,'.',1A2,'.',1A2/
     $           ' Run id   = ',1A6/
     $           ' Security = ',1A17)
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE DRXPLI
C     ***ROUTINE TO OUTPUT PLOT ID ON PLOTTER
      INCLUDE 'SIZDT.FOR'
      INCLUDE 'NAMDT.FOR'
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'MSGDT.FOR'
      LOGICAL LOGSAV
      CHARACTER*80 IDLINE
      CHARACTER*17 ASECUR
      LOGICAL TRACE2
      COMMON /TRXDT2/ TRACE2
C
      IF (TRACE2) WRITE(6,100)
 100      FORMAT(' DRXPLI called')
C
      CHALPH=1
      LFONT(1)=.FALSE.
      ASECUR='MIT MINDIS'
c      IF (SECRTY.EQ.0) ASECUR='NATO UNCLASSIFIED'
c      IF (SECRTY.EQ.1) ASECUR='NATO CONFIDENTIAL'
c      IF (SECRTY.EQ.2) ASECUR='NATO SECRET      '
c      IF (SECRTY.EQ.3) ASECUR='NATO COSMIC      '
      WRITE(IDLINE,200) PLTRID,PLTNO,PLTDAT(1:2),PLTDAT(3:4),
     $                  PLTDAT(5:6),PLTTIM(1:2),PLTTIM(3:4),PLTTIM(5:6),
     $                  ASECUR
 200      FORMAT(1A6,'  Plot no ',I3,'  ',1A2,'-',1A2,'-',1A2,'  ',
     $                          1A2,'.',1A2,'.',1A2,'   ',1A17,' $')
      inchar=38
      IF (UPRGHT) THEN
           X=0.2
           Y=0.1
           ANGLE=90.0
      ELSE
           X=0.1
           Y=PHYYLN-0.2
           ANGLE=0.0
           ENDIF
      ROLDAN=RSXANG(ANGLE)
      ROLDHT=RSXHGT(0.12)
      X=X-XPHORG
      Y=Y-YPHORG
      LOGSAV=LWNDOW
      LWNDOW=.FALSE.
      CALL CHXLIN(X,Y,IDLINE)
      LWNDOW=LOGSAV
      RTMP=RSXANG(ROLDAN)
      RTMP=RSXHGT(ROLDHT)
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE PLXCLR
C     ***ROUTINE TO CLEAR SCREEN ON GRAPHICS TERMINAL
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'NAMDT.FOR'
      CHARACTER*10 REPLY
      LOGICAL TRACE2
      COMMON /TRXDT2/ TRACE2
C
      IF (TRACE2) WRITE(6,100)
 100      FORMAT(' PLXCLR called')
C
      IF (LTTCRT) CALL TKXOPB
      CALL HCXCHK
      IF (LTTCRT) CALL TKXCLS
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE DRXFRM
C     ***ROUTINE TO DRAW FRAME
      INCLUDE 'SIZDT.FOR'
      INCLUDE 'CTLDT.FOR'
      LOGICAL TRACE2
      COMMON /TRXDT2/ TRACE2
      IF (TRACE2) WRITE(6,100)
 100      FORMAT(' DRXFRM called')
      DO 1 I=1,3
           DVAL=FLOAT(I-1)*0.01
           CALL BLXMOV(SPLAX1+DVAL,SPLAY1+DVAL)
           CALL BLXDRW(SPLAX1+DVAL,SPLAY2-DVAL)
           CALL BLXDRW(SPLAX2-DVAL,SPLAY2-DVAL)
           CALL BLXDRW(SPLAX2-DVAL,SPLAY1+DVAL)
           CALL BLXDRW(SPLAX1+DVAL,SPLAY1+DVAL)
 1         CONTINUE
C      CALL DELAY(10000)
      IF (LTTCRT) CALL TKXOPB
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE DRXCAX
C     ***ROUTINE TO DRAW NORMAL CARTESIAN AXES
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'SIZDT.FOR'
      INCLUDE 'CARTD.FOR'
      INCLUDE 'NAMDT.FOR'
      INTEGER GTXPLC
      INTEGER TTLLEN
      LOGICAL TRACE2
      COMMON /TRXDT2/ TRACE2
C
      IF (TRACE2) WRITE(6,100)
 100      FORMAT(' DRXCAX called')
C
      IF (LAXIS) THEN
           CALL DRXCXX(SPLAX1,SPLAY1,SPLAX2)
           CALL DRXCXY(SPLAX1,SPLAY1,SPLAY2)
           ENDIF
C
C     ***NOW DRAW TITLE (IF ONE EXISTS)
      TTLLEN=IABS(PLTTLL)
      IF (TTLLEN.GT.1) THEN
C          ***SET UP ANGLE AND HEIGHT
           CHHGT=RSXHGT(0.0)
           CHHGT1=CHHGT*1.5
           RTMP=RSXHGT(CHHGT1)
           ANGSAV=RSXANG(0.0)
C          ***NOW FIND EXTENT OF FREE AREA
           RLOW=YMAXPT
           RHIGH=PHYYLN-YPHORG
           IF (.NOT.UPRGHT) RHIGH=RHIGH-0.5
C          ***NOW FIND POSITION FOR MESSAGE
           RYDIFF=(RHIGH-RLOW-CHHGT1)/2.0
           IF (RYDIFF.GT.CHHGT1) RYDIFF=CHHGT1
           RYPOS=RLOW+RYDIFF
C$$        RLLEN=FLOAT(TTLLEN)*(CHHGT1)*5.0/6.0
           RLLEN=CHXLTH(PLTTL(1:TTLLEN)//'$')
           RXPOS=SPLAX1+((SPLAX2-SPLAX1-RLLEN)/2.0)
           CALL CHXLIN(RXPOS,RYPOS,PLTTL(1:TTLLEN)//'$')
           RTMP=RSXANG(ANGSAV)
           RTMP=RSXHGT(CHHGT)
           ENDIF
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE DRXCXX(XSTRT,YSTRT,XFINI)
C     ***Routine to output X axis.
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'SIZDT.FOR'
      INCLUDE 'CARTD.FOR'
      INCLUDE 'NAMDT.FOR'
      INTEGER GTXPLC
C
C     ***FIRST HANDLE X-AXIS DRAWING
      IF (XTIKS.NE.0) THEN
C          ***FIRST SET MIN + MAX DRAWING AREA
           YSAVMN=YMINPT
           YSAVMX=YMAXPT
           YMINPT=YSTRT
           YMAXPT=YSTRT
C          ***DRAW X-AXIS
           CALL BLXMOV(XSTRT,YSTRT)
           CALL BLXDRW(XFINI,YSTRT)
C          ***DRAW AXIS TICKS AND VALUES
           YVAL=-0.05
           IF (XTIKS.EQ.2) YVAL=-YVAL
           YVAL1=YSTRT
           YVAL2=YSTRT+YVAL
           XVAL=XSTRT
           IDPLCS=GTXPLC(CTXORG,CTXSTP,LINTX)
           IDIR=2
           IF (YVAL.LT.0) IDIR=IDIR+2
           CHHGT=RSXHGT(0.0)
           RTMP=RSXHGT(CHHGT*5.0/7.0)
 1         CONTINUE
                CALL BLXMOV(XVAL,YVAL1)
                CALL BLXDRW(XVAL,YVAL2)
                RVAL=(CTXORG+(XVAL*CTXSTP/CTXPST))*1.0000001
                CALL DRXCAL(RVAL,IDPLCS,IDIR,XVAL,YVAL1)
                XVAL=XVAL+ABS(CTXPST)
                IF (XVAL.LE.XFINI*1.000001) GOTO 1
           RTMP=RSXHGT(CHHGT)
           IF (XADTKS.NE.0) THEN
C               ***DRAW ADDITIONAL TICKS
                YVAL=-0.025
                IF (XTIKS.EQ.2) YVAL=-YVAL
                YVAL1=YSTRT
                YVAL2=YSTRT+YVAL
                XVAL =XSTRT
                XSTP =ABS(CTXPST)/XADTKS
                IXCNT=0
 22             CONTINUE
                     IF (IXCNT.NE.0) THEN
                          CALL BLXMOV(XVAL,YVAL1)
                          CALL BLXDRW(XVAL,YVAL2)
                          ENDIF
                     IF (IXCNT.EQ.XADTKS) IXCNT=0
                     IXCNT=IXCNT+1
                     XVAL=XVAL+XSTP
                     IF (XVAL.LE.XFINI*1.000001) GOTO 22
                ENDIF
C          ***HANDLE DRAWING OF X AXIS LABEL
           ILABLN=IABS(PLXLBL)
           IF (ILABLN.NE.0) THEN
                ANGSAV=RSXANG(0.0)
C               ***FIND POSITION FOR LABEL
                IF (XTIKS.EQ.2) THEN
C                    ***TITLE ABOVE AXIS
                     YPOSN=YMAXPT+(CHHGT*5.0/7.0)
                ELSE
C                    ***TITLE BELOW AXIS
                     YPOSN=YMINPT-(CHHGT*(1.0+(5.0/7.0)))
                     ENDIF
C$$             XLEN=FLOAT(ILABLN)*CHHGT*5.0/6.0
                XLEN=CHXLTH(PLXLAB(1:ILABLN)//'$')
                XPOSN=XSTRT+(XFINI-XSTRT-XLEN)/2.0
                CALL CHXLIN(XPOSN,YPOSN,PLXLAB(1:ILABLN)//'$')
                RTMP=RSXANG(ANGSAV)
                ENDIF
           IF (YMINPT.LT.YSAVMN) YMINPT=YSAVMN
           IF (YMAXPT.GT.YSAVMX) YMAXPT=YSAVMX
           ENDIF
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE DRXCXY(XSTRT,YSTRT,YFINI)
C     ***Routine to output Y axis.
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'SIZDT.FOR'
      INCLUDE 'CARTD.FOR'
      INCLUDE 'NAMDT.FOR'
      INTEGER GTXPLC
C
C     ***HANDLE Y-AXIS DRAWING
      IF (YTIKS.NE.0) THEN
C          ***FIRST SET MIN + MAX DRAWING AREA
           XSAVMN=XMINPT
           XSAVMX=XMAXPT
           XMINPT=XSTRT
           XMAXPT=XSTRT
C          ***DRAW Y-AXIS
           CALL BLXMOV(XSTRT,YSTRT)
           CALL BLXDRW(XSTRT,YFINI)
C          ***DRAW AXIS TICKS AND VALUES
           XVAL=-0.05
           IF (YTIKS.EQ.2) XVAL=-XVAL
           XVAL1=XSTRT
           XVAL2=XSTRT+XVAL
           YVAL=YSTRT
           IDPLCS=GTXPLC(CTYORG,CTYSTP,LINTY)
           IDIR=1
           IF (XVAL.LT.0) IDIR=IDIR+2
           CHHGT=RSXHGT(0.0)
           RTMP=RSXHGT(CHHGT*5.0/7.0)
 2         CONTINUE
                CALL BLXMOV(XVAL1,YVAL)
                CALL BLXDRW(XVAL2,YVAL)
                RVAL=(CTYORG+(YVAL*CTYSTP/CTYPST))*1.0000001
                CALL DRXCAL(RVAL,IDPLCS,IDIR,XVAL1,YVAL)
                YVAL=YVAL+ABS(CTYPST)
                IF (YVAL.LE.YFINI*1.000001) GOTO 2
           RTMP=RSXHGT(CHHGT)
           IF (YADTKS.NE.0) THEN
C               ***DRAW ADDITIONAL TICKS
                XVAL=-0.025
                IF (YTIKS.EQ.2) XVAL=-XVAL
                XVAL1=XSTRT
                XVAL2=XSTRT+XVAL
                YVAL =YSTRT
                YSTP =ABS(CTYPST)/YADTKS
                IYCNT=0
 21             CONTINUE
                     IF (IYCNT.NE.0) THEN
                          CALL BLXMOV(XVAL1,YVAL)
                          CALL BLXDRW(XVAL2,YVAL)
                          ENDIF
                     IF (IYCNT.EQ.YADTKS) IYCNT=0
                     IYCNT=IYCNT+1
                     YVAL=YVAL+YSTP
                     IF (YVAL.LE.YFINI*1.000001) GOTO 21
                ENDIF
C          ***HANDLE DRAWING OF Y AXIS LABEL
           ILABLN=IABS(PLYLBL)
           IF (ILABLN.NE.0) THEN
                ANGSAV=RSXANG(90.0)
C               ***FIND POSITION FOR LABEL
                IF (YTIKS.EQ.2) THEN
C                    ***TITLE TO RIGHT OF AXIS
                     XPOSN=XMAXPT+(CHHGT*5.0/7.0)
                ELSE
C                    ***TITLE TO LEFT OF AXIS
                     XPOSN=XMINPT-(CHHGT*(1.0+(5.0/7.0)))
                     ENDIF
C$$             YLEN=FLOAT(ILABLN)*CHHGT*5.0/6.0
                YLEN=CHXLTH(PLYLAB(1:ILABLN)//'$')
                YPOSN=SPLAY1+(SPLAY2-SPLAY1-YLEN)/2.0
                CALL CHXLIN(XPOSN,YPOSN,PLYLAB(1:ILABLN)//'$')
                RTMP=RSXANG(ANGSAV)
                ENDIF
           IF (XMINPT.LT.XSAVMN) XMINPT=XSAVMN
           IF (XMAXPT.GT.XSAVMX) XMAXPT=XSAVMX
           ENDIF
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE DRXPGD(IXGRID, IYGRID)
C     ***ROUTINE TO DRAW POLAR GRID
      INCLUDE 'POLDT.FOR'
      INCLUDE 'SIZDT.FOR'
      INCLUDE 'CTLDT.FOR'
      REAL POXRAD
      LOGICAL LOGSAV
      LOGICAL TRACE2
      COMMON /TRXDT2/ TRACE2
C
      IF (TRACE2) WRITE(6,100) IXGRID, IYGRID
 100      FORMAT(' DRXPGD called: IXGRID = ',I6,' IYGRID = ',I6)
C
      LOGSAV=LWNDOW
      LWNDOW=.TRUE.
      RVAL1=ABS(POLXDI-SPLAX1)
      RVAL2=ABS(POLXDI-SPLAX2)
      RVAL3=ABS(POLYDI-SPLAY1)
      RVAL4=ABS(POLYDI-SPLAY2)
      RMAX=RVAL1
      IF (RMAX.LT.RVAL2) RMAX=RVAL2
      IF (RMAX.LT.RVAL3) RMAX=RVAL3
      IF (RMAX.LT.RVAL4) RMAX=RVAL4
      THSTEP=10.0/FLOAT(IXGRID)
      RSTEP = 1.0/FLOAT(IYGRID)
C
C     ***DRAW STRAIGHT LINES OUT FROM CENTRE
      THETA=0.0
 1    CONTINUE
           CALL POXMOV(0.2,POXRAD(THETA))
           CALL POXDRW(RMAX,POXRAD(THETA))
           THETA=THETA+THSTEP
           IF (THETA.LT.360.) GOTO 1
C
C     ***DRAW CIRCLES
      RADIUS=RSTEP
 2    CONTINUE
           CALL POXMOV(RADIUS,POXRAD(0.0))
           DO 3 ITHETA=1,360
                CALL POXDRW(RADIUS,POXRAD(FLOAT(ITHETA)))
 3              CONTINUE
           RADIUS=RADIUS+RSTEP
           IF (RADIUS.LE.RMAX) GOTO 2
      LWNDOW=LOGSAV
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE DRXCGD(IXGRID, IYGRID)
C     ***ROUTINE TO DRAW CARTESIAN GRID
      INCLUDE 'CARTD.FOR'
      INCLUDE 'SIZDT.FOR'
      LOGICAL TRACE2
      COMMON /TRXDT2/ TRACE2
C
      IF (TRACE2) WRITE(6,100) IXGRID, IYGRID
 100      FORMAT(' DRXCGD called: IXGRID = ',I6,' IYGRID = ',I6)
C
      XSTP=ABS(CTXPST/IXGRID)
      XVAL=SPLAX1
 1    CONTINUE
           XVAL=XVAL+XSTP
           IF (XVAL.GE.SPLAX2) GOTO 2
           CALL INXMOV(XVAL,SPLAY1)
           CALL INXDRW(XVAL,SPLAY2)
           GOTO 1
 2    CONTINUE
C
      YSTP=ABS(CTYPST/IYGRID)
      YVAL=SPLAY1
 3    CONTINUE
           YVAL=YVAL+YSTP
           IF (YVAL.GE.SPLAY2) GOTO 4
           CALL INXMOV(SPLAX1,YVAL)
           CALL INXDRW(SPLAX2,YVAL)
           GOTO 3
 4    CONTINUE
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE PLXHDG
C     ***ROUTINE TO OUTPUT HEADING
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'NAMDT.FOR'
      INCLUDE 'SIZDT.FOR'
      LOGICAL TRACE2
      COMMON /TRXDT2/ TRACE2
      IF (TRACE2) WRITE(6,100)
 100      FORMAT(' PLXHDG called')
C
C     ***FIRST NEED TO DECIDE WHERE EXACTLY TO PUT THE HEADING
C     ***START OFF BY CALCULATING OVERALL HEIGHT OF HEADING
      TOTLEN=0.0
      DO 1 I=1, HDGLNS
           TOTLEN=TOTLEN+(0.07*HDGHGT(I)*1.5)
 1         CONTINUE
      TOTLEN=TOTLEN-(0.07*HDGHGT(HDGLNS)*0.5)
      IF (TRACE2) WRITE(6,110) TOTLEN
 110      FORMAT(' Total height = ',F12.4)
C
C     ***NOW GET THE SIZE OF THE FREE AREA FOR THE MESSAGE
      RLOW=YMAXPT
      RHIGH=PHYYLN-YPHORG
      IF (.NOT.UPRGHT) RHIGH=RHIGH-0.5
C      WRITE(6,199) UPRGHT
C 199      FORMAT(' Upright = ',1L1)
      IF (TRACE2) WRITE(6,120) RLOW, RHIGH
 120      FORMAT(' Area: Bottom = ',F12.4,' Top = ',F12.4)
C
C     ***NOW POSITION THE MESSAGE IN THIS AREA
      RDIFF=RHIGH-RLOW
      HGT1=HDGHGT(1)*0.07
      HGT2=HDGHGT(HDGLNS)*0.07
      RLEN1=(RDIFF-TOTLEN)*(HGT1/(HGT1+HGT2))
      RTOP=RHIGH-RLEN1
      RYPOS=RTOP
      IF (TRACE2) WRITE(6,130) HGT1, HGT2, RLEN1, RYPOS
 130      FORMAT(' Ratio Top:Bottom = ',2F12.4,' Top len = ',F12.4,
     $           ' High Posn = ',F12.4)
C
C     ***NOW OUTPUT THE TEXT LINE BY LINE
      SAVHGT=RSXHGT(0.0)
      DO 2 I=1, HDGLNS
           RYPOS=RYPOS-(0.07*HDGHGT(I))
           RTMP=RSXHGT(HDGHGT(I)*0.07)
C$$        RLLEN=PLHDGL(I)*(0.07*HDGHGT(I)*5.0/6.0)
           RLLEN=CHXLTH(PLHDG(I)(1:PLHDGL(I))//'$')
           RXPOS=SPLAX1+(SPLAX2-SPLAX1-RLLEN)/2.0
           CALL CHXLIN(RXPOS,RYPOS,PLHDG(I)(1:PLHDGL(I))//'$')
           RYPOS=RYPOS-(0.07*HDGHGT(I)*0.5)
           IF (HDGULS(I).GE.1) THEN
                RYPOSZ=RYPOS+(HDGHGT(I)*0.07/3.)
                CALL INXMOV(RXPOS,RYPOSZ)
                CALL INXDRW(RXPOS+RLLEN,RYPOSZ)
                ENDIF
           IF (HDGULS(I).GE.2) THEN
                RYPOSZ=RYPOS+(HDGHGT(I)*0.07/5.)
                CALL INXMOV(RXPOS,RYPOSZ)
                CALL INXDRW(RXPOS+RLLEN,RYPOSZ)
                ENDIF
 2         CONTINUE
      RTMP=RSXHGT(SAVHGT)
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE PLXLST(IPARAY,NLINES,XPOS,YPOS)
C     ***ROUTINE TO OUTPUT LSTORY
      INCLUDE 'MSGDT.FOR'
      INTEGER IPARAY(1000)
      CHARACTER*100 XXCHRS
      INTEGER XXINTS(25)
      EQUIVALENCE (XXCHRS,XXINTS)
      LOGICAL TRACE2
      COMMON /TRXDT2/ TRACE2
      IF (TRACE2) THEN
           WRITE(6,100)  NLINES, XPOS, YPOS
 100           FORMAT(' PLXLST called: NLINES = ',I5,
     $                ' XPOS = ',F10.5,' YPOS = ',F10.5)
           DO 1 I=1,NLINES
                CALL CYXAIN(IPARAY(((I-1)*STILLN)+1),XXINTS,25)
                ICHCNT=ICHAR(XXCHRS(1:1))
                ICHHGT=ICHAR(XXCHRS(2:2))
                WRITE(6,110) I,ICHCNT,ICHHGT,XXCHRS(3:3+ICHCNT)
 110                FORMAT(' Line ',I3,' No of chars = ',I3,
     $                     ' Height = ',I3,' Line = ',1A100)
 1              CONTINUE
           ENDIF
C
      DO 2 I=1, NLINES
           CALL CYXAIN(IPARAY(((I-1)*STILLN)+1),XXINTS,25)
           ICHCNT=ICHAR(XXCHRS(1:1))
           ICHHGT=ICHAR(XXCHRS(2:2))
           YDIFF=FLOAT(NLINES-I)*LINSPC*MSGHGT
           CALL CHXLIN(XPOS,YPOS+YDIFF,XXCHRS(3:3+ICHCNT)//'$')
 2         CONTINUE
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE PLXSTO(IPARAY,NLINES,XPOS,YPOS)
C     ***ROUTINE TO OUTPUT STORY
      INCLUDE 'MSGDT.FOR'
      INTEGER IPARAY(1000)
      REAL LLENS(20), MAXLTH
      CHARACTER*100 XXCHRS
      INTEGER XXINTS(25)
      EQUIVALENCE (XXCHRS,XXINTS)
      LOGICAL TRACE2
      COMMON /TRXDT2/ TRACE2
C
      IF (TRACE2) THEN
           WRITE(6,100)  NLINES, XPOS, YPOS
 100           FORMAT(' PLXSTO called: NLINES = ',I5,
     $                ' XPOS = ',F10.5,' YPOS = ',F10.5)
           DO 1 I=1,NLINES
                CALL CYXAIN(IPARAY(((I-1)*STILLN)+1),XXINTS,25)
                ICHCNT=ICHAR(XXCHRS(1:1))
                ICHHGT=ICHAR(XXCHRS(2:2))
                WRITE(6,110) I,ICHCNT,ICHHGT,XXCHRS(3:3+ICHCNT)
 110                FORMAT(' Line ',I3,' No of chars = ',I3,
     $                     ' Height = ',I3,' Line = ',1A100)
 1              CONTINUE
           ENDIF
C
C     ***START OF BY GETTING LINE LENGTHS
      MAXLTH=0.0
      DO 2 I=1, NLINES
           CALL CYXAIN(IPARAY(((I-1)*STILLN)+1),XXINTS,25)
           ICHCNT=ICHAR(XXCHRS(1:1))
C$$        LLENS(I)=FLOAT(ICHCNT)*MSGHGT*(5.0/6.0)
           LLENS(I)=CHXLTH(XXCHRS(3:3+ICHCNT)//'$')
           IF (LLENS(I).GT.MAXLTH) MAXLTH=LLENS(I)
 2         CONTINUE
C
C     ***NOW OUTPUT LINES
C     ***NB - CENTRES ON LONGEST LINE
      DO 3 I=1,NLINES
           CALL CYXAIN(IPARAY(((I-1)*STILLN)+1),XXINTS,25)
           ICHCNT=ICHAR(XXCHRS(1:1))
           YDIFF=FLOAT(NLINES-I)*LINSPC*MSGHGT
           XDIFF=(MAXLTH/2.0)-(LLENS(I)/2.0)
           CALL CHXLIN(XPOS+XDIFF,YPOS+YDIFF,
     $                 XXCHRS(3:3+ICHCNT)//'$')
 3         CONTINUE
C
      RETURN
      END
C
      SUBROUTINE CYXAIN(IARRY1,IARRY2,ILEN)
C     ***ROUTINE TO COPY ARRAY OF INTEGERS
      INTEGER IARRY1(ILEN), IARRY2(ILEN)
      DO 1 I=1,ILEN
           IARRY2(I)=IARRY1(I)
 1         CONTINUE
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE PLXMSG(LMESS,IMESS,XPOS,YPOS)
C     ***ROUTINE TO PLOT MESSAGE
      CHARACTER*(*) LMESS
      INCLUDE 'MSGDT.FOR'
      LOGICAL TRACE2
      COMMON /TRXDT2/ TRACE2
      character*132 buffer
      IF (TRACE2) WRITE(6,100) IMESS, XPOS, YPOS, LMESS(1:IMESS)
 100      FORMAT(' PLXMSG called: IMESS = ',I5,' XPOS = ',F10.5,
     $           ' YPOS = ',F10.5/' LMESS = ',1A150)
C
      buffer=LMESS(1:IMESS)
      buffer(IMESS+1:IMESS+1)='$'
      CALL CHXLIN(XPOS+XMGOFS,YPOS+YMGOFS,buffer(1:IMESS+1))
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE DRXPAX
C     ***ROUTINE TO DRAW POLAR AXES
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'POLDT.FOR'
      INCLUDE 'SIZDT.FOR'
      INCLUDE 'NAMDT.FOR'
      INTEGER GTXPLC
      INTEGER TTLLEN
      LOGICAL TRACE2
      COMMON /TRXDT2/ TRACE2
      IF (TRACE2) WRITE(6,100)
 100      FORMAT(' DRXPAX called')
C
C     ***Draw X and Y axes
      IF (LAXIS) THEN
C
C        ***HANDLE X-AXIS DRAWING
         IF (XTIKS.NE.0) THEN
C             ***FIRST SET MIN + MAX DRAWING AREA
              YSAVMN=YMINPT
              YSAVMX=YMAXPT
              YMINPT=POLYDI
              YMAXPT=POLYDI
C             ***DRAW X-AXIS
              CALL BLXMOV(SPLAX1,POLYDI)
              CALL BLXDRW(SPLAX2,POLYDI)
C             ***DRAW AXIS TICKS AND VALUES
              YVAL=-0.05
              IF (XTIKS.EQ.2) YVAL=-YVAL
              YVAL1=POLYDI
              YVAL2=POLYDI+YVAL
              XVAL=POLXDI
              IDPLCS=GTXPLC(POLORV,POLSTP,LINTX)
              IDIR=2
              IF (YVAL.LT.0) IDIR=IDIR+2
              CHHGT=RSXHGT(0.0)
              RTMP =RSXHGT(CHHGT*5.0/7.0)
 1            CONTINUE
                   XVAL=XVAL+1.0
                   IF (XVAL.GT.SPLAX2) GOTO 2
                   CALL BLXMOV(XVAL,YVAL1)
                   CALL BLXDRW(XVAL,YVAL2)
                   RVAL=(POLORV+((XVAL-POLXDI)*POLSTP))*1.0000001
                   CALL DRXCAL(RVAL,IDPLCS,IDIR,XVAL,YVAL1)
                   GOTO 1
C
 2            CONTINUE
              XVAL=POLXDI
 3            CONTINUE
                   XVAL=XVAL-1.0
                   IF (XVAL.LT.SPLAX1) GOTO 4
                   CALL BLXMOV(XVAL,YVAL1)
                   CALL BLXDRW(XVAL,YVAL2)
                   RVAL=(POLORV+((POLXDI-XVAL)*POLSTP))*1.0000001
                   CALL DRXCAL(RVAL,IDPLCS,IDIR,XVAL,YVAL1)
                   GOTO 3
C
 4            CONTINUE
              RTMP=RSXHGT(CHHGT)
              IF (XADTKS.NE.0) THEN
C                  ***DRAW ADDITIONAL TICKS
                   YVAL=-0.025
                   IF (XTIKS.EQ.2) YVAL=-YVAL
                   YVAL1=POLYDI
                   YVAL2=POLYDI+YVAL
                   XVAL =POLXDI
                   XSTP =1.0/XADTKS
                   IXCNT=0
 22                CONTINUE
                        IF (IXCNT.NE.0) THEN
                             CALL BLXMOV(XVAL,YVAL1)
                             CALL BLXDRW(XVAL,YVAL2)
                             ENDIF
                        IF (IXCNT.EQ.XADTKS) IXCNT=0
                        IXCNT=IXCNT+1
                        XVAL=XVAL+XSTP
                        IF (XVAL.LE.SPLAX2*1.000001) GOTO 22
                   XVAL =POLXDI
                   IXCNT=0
 23                CONTINUE
                        IF (IXCNT.NE.0) THEN
                             CALL BLXMOV(XVAL,YVAL1)
                             CALL BLXDRW(XVAL,YVAL2)
                             ENDIF
                        IF (IXCNT.EQ.XADTKS) IXCNT=0
                        IXCNT=IXCNT+1
                        XVAL=XVAL-XSTP
                        IF (XVAL.GE.SPLAX1) GOTO 23
                   ENDIF
C             ***HANDLE DRAWING OF X AXIS LABEL
              ILABLN=IABS(PLXLBL)
              IF (ILABLN.NE.0) THEN
                   ANGSAV=RSXANG(0.0)
C                  ***FIND POSITION FOR LABEL
                   IF (XTIKS.EQ.2) THEN
C                       ***TITLE ABOVE AXIS
                        YPOSN=YMAXPT+(CHHGT*5.0/7.0)
                   ELSE
C                       ***TITLE BELOW AXIS
                        YPOSN=YMINPT-(CHHGT*(1.0+(5.0/7.0)))
                        ENDIF
C$$                XLEN=FLOAT(ILABLN)*CHHGT*5.0/6.0
                   XLEN=CHXLTH(PLXLAB(1:ILABLN)//'$')
                   XSPAC1=SPLAX2-POLXDI
                   XSPAC2=SPLAX1-POLXDI
                   XSPAC3=XSPAC1
                   IF (ABS(XSPAC2).GT.XSPAC3) XSPAC3=XSPAC2
                   XPOSN=POLXDI+(XSPAC3-XLEN)/2.0
                   CALL CHXLIN(XPOSN,YPOSN,PLXLAB(1:ILABLN)//'$')
                   RTMP=RSXANG(ANGSAV)
                   ENDIF
              IF (YMINPT.LT.YSAVMN) YMINPT=YSAVMN
              IF (YMAXPT.GT.YSAVMX) YMAXPT=YSAVMX
              ENDIF
C
C        ***HANDLE Y-AXIS DRAWING
         IF (YTIKS.NE.0) THEN
C             ***FIRST SET MIN + MAX DRAWING AREA
              XSAVMN=XMINPT
              XSAVMX=XMAXPT
              XMINPT=POLXDI
              XMAXPT=POLXDI
C             ***DRAW Y-AXIS
              CALL BLXMOV(POLXDI,SPLAY1)
              CALL BLXDRW(POLXDI,SPLAY2)
C             ***DRAW AXIS TICKS AND VALUES
              XVAL=-0.05
              IF (YTIKS.EQ.2) XVAL=-XVAL
              XVAL1=POLXDI
              XVAL2=POLXDI+XVAL
              YVAL=POLYDI
              IDPLCS=GTXPLC(POLORV,POLSTP,LINTY)
              IDIR=1
              IF (XVAL.LT.0) IDIR=IDIR+2
              CHHGT=RSXHGT(0.0)
              RTMP =RSXHGT(CHHGT*5.0/7.0)
 5            CONTINUE
                   YVAL=YVAL+1.0
                   IF (YVAL.GT.SPLAY2) GOTO 6
                   CALL BLXMOV(XVAL1,YVAL)
                   CALL BLXDRW(XVAL2,YVAL)
                   RVAL=(POLORV+((YVAL-POLYDI)*POLSTP))*1.0000001
                   CALL DRXCAL(RVAL,IDPLCS,IDIR,XVAL1,YVAL)
                   GOTO 5
C
 6            CONTINUE
              YVAL=POLYDI
 7            CONTINUE
                   YVAL=YVAL-1.0
                   IF (YVAL.LT.SPLAY1) GOTO 8
                   CALL BLXMOV(XVAL1,YVAL)
                   CALL BLXDRW(XVAL2,YVAL)
                   RVAL=(POLORV+((POLYDI-YVAL)*POLSTP))*1.0000001
                   CALL DRXCAL(RVAL,IDPLCS,IDIR,XVAL1,YVAL)
                   GOTO 7
C
 8            CONTINUE
              RTMP=RSXHGT(CHHGT)
              IF (YADTKS.NE.0) THEN
C                  ***DRAW ADDITIONAL TICKS
                   XVAL=-0.025
                   IF (YTIKS.EQ.2) XVAL=-XVAL
                   XVAL1=POLXDI
                   XVAL2=POLXDI+XVAL
                   YVAL =POLYDI
                   YSTP =1.0/YADTKS
                   IYCNT=0
 24                CONTINUE
                        IF (IYCNT.NE.0) THEN
                             CALL BLXMOV(XVAL1,YVAL)
                             CALL BLXDRW(XVAL2,YVAL)
                             ENDIF
                        IF (IYCNT.EQ.YADTKS) IYCNT=0
                        IYCNT=IYCNT+1
                        YVAL=YVAL+YSTP
                        IF (YVAL.LE.SPLAY2*1.000001) GOTO 24
                   YVAL =POLYDI
                   IYCNT=0
 25                CONTINUE
                        IF (IYCNT.NE.0) THEN
                             CALL BLXMOV(XVAL1,YVAL)
                             CALL BLXDRW(XVAL2,YVAL)
                             ENDIF
                        IF (IYCNT.EQ.YADTKS) IYCNT=0
                        IYCNT=IYCNT+1
                        YVAL=YVAL-YSTP
                        IF (YVAL.GE.SPLAY1) GOTO 25
                   ENDIF
C             ***HANDLE DRAWING OF Y AXIS LABEL
              ILABLN=IABS(PLYLBL)
              IF (ILABLN.NE.0) THEN
                   ANGSAV=RSXANG(90.0)
C                  ***FIND POSITION FOR LABEL
                   IF (YTIKS.EQ.2) THEN
C                       ***TITLE TO RIGHT OF AXIS
                        XPOSN=XMAXPT+(CHHGT*5.0/7.0)
                   ELSE
C                       ***TITLE TO LEFT OF AXIS
                        XPOSN=XMINPT-(CHHGT*(1.0+(5.0/7.0)))
                        ENDIF
C$$                YLEN=FLOAT(ILABLN)*CHHGT*5.0/6.0
                   YLEN=CHXLTH(PLYLAB(1:ILABLN)//'$')
                   YSPAC1=SPLAY2-POLYDI
                   YSPAC2=SPLAY1-POLYDI
                   YSPAC3=YSPAC1
                   IF (ABS(YSPAC2).GT.YSPAC3) YSPAC3=YSPAC2
                   YPOSN=POLYDI+(YSPAC3-YLEN)/2.0
                   CALL CHXLIN(XPOSN,YPOSN,PLYLAB(1:ILABLN)//'$')
                   RTMP=RSXANG(ANGSAV)
                   ENDIF
              IF (XMINPT.LT.XSAVMN) XMINPT=XSAVMN
              IF (XMAXPT.GT.XSAVMX) XMAXPT=XSAVMX
              ENDIF
           ENDIF
C
C     ***NOW DRAW TITLE (IF ONE EXISTS)
      TTLLEN=IABS(PLTTLL)
      IF (TTLLEN.GT.1) THEN
C          ***SET UP ANGLE AND HEIGHT
           CHHGT=RSXHGT(0.0)
           CHHGT1=CHHGT*1.5
           RTMP=RSXHGT(CHHGT1)
           ANGSAV=RSXANG(0.0)
C          ***NOW FIND EXTENT OF FREE AREA
           RLOW=YMAXPT
           RHIGH=PHYYLN-YPHORG
           IF (.NOT.UPRGHT) RHIGH=RHIGH-0.5
C          ***NOW FIND POSITION FOR MESSAGE
           RYDIFF=(RHIGH-RLOW-CHHGT1)/2.0
           IF (RYDIFF.GT.CHHGT1) RYDIFF=CHHGT1
           RYPOS=RLOW+RYDIFF
C$$        RLLEN=FLOAT(TTLLEN)*(CHHGT1)*5.0/6.0
           RLLEN=CHXLTH(PLTTL(1:TTLLEN)//'$')
           RXPOS=SPLAX1+((SPLAX2-SPLAX1-RLLEN)/2.0)
           CALL CHXLIN(RXPOS,RYPOS,PLTTL(1:TTLLEN)//'$')
           RTMP=RSXANG(ANGSAV)
           RTMP=RSXHGT(CHHGT)
           ENDIF
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE DRXVEC(X1,Y1,X2,Y2,IVEC,PHYSUN)
C     ***ROUTINE TO DRAW A VECTOR
      LOGICAL PHYSUN
      LOGICAL TRACE2
      COMMON /TRXDT2/ TRACE2
      IF (TRACE2) WRITE(6,100) X1,Y1,X2,Y2,IVEC
 100      FORMAT(' DRXVEC called: X1 = ',F10.5,' Y1 = ',F10.5,
     $           ' X2 = ',F10.5,' Y2 = ',F10.5,' IVEC = ',I3,
     $           ' PHYSUN = ',L1)
C
      IF (PHYSUN) THEN
           CALL INXMOV(X1,Y1)
           CALL INXDRW(X2,Y2)
      ELSE
           CALL UNXMOV(X1,Y1,.FALSE.)
           CALL UNXDRW(X2,Y2,.FALSE.)
           ENDIF
      IF (IVEC.NE.0) WRITE(6,200)
 200       FORMAT(' ***Note: arrow heads not yet available')
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE PLXCHK
C     ***ROUTINE TO CHECK AXIS SIZES ETC.
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'SIZDT.FOR'
      INCLUDE 'NAMDT.FOR'
      CHARACTER*140 TITLE
      REAL RCH1, RCH2
      LOGICAL TRACE2
      COMMON /TRXDT2/ TRACE2
      COMMON /GRXDT3/ RSCALE
      Integer ixxterm,iyxterm
      COMMON /XTERMS/ ixxterm,iyxterm
C
      IF (TRACE2) WRITE(6,100)
 100      FORMAT(' PLXCHK called')
      IF (LTTCRT) THEN
c >>> make a fitting size of x-window
c           if (phyxln.ge.phyyln*1.5) then
c            ixxterm=850
c            iyxterm=ixxterm*(phyyln/phyxln)
c           else if (phyxln.ge.phyyln) then
c            iyxterm=600
c            ixxterm=iyxterm*(phyxln/phyyln)
c           else if (phyyln.ge.phyxln*1.5) then
c            iyxterm=850
c            ixxterm=iyxterm*(phyxln/phyyln)
c           else
c            ixxterm=600
c            iyxterm=iyxterm*(phyyln/phyxln)
c           end if
           CALL TKXSIZ(RXSIZ,RYSIZ)
c           xs=max(phyxln,10.0)
c           ys=max(phyyln,10.0)
c           xscale=rxsiz/xs
c           yscale=rysiz/ys
           XSCALE=RXSIZ/PHYXLN
           YSCALE=RYSIZ/PHYYLN
c           RSCALE=XSCALE
c           IF (YSCALE.LT.RSCALE) RSCALE=YSCALE
            rscale=min(xscale,yscale)
           ENDIF
C
C     ***OUTPUT DETAILS TO PLOT FILE
      WRITE(48) 1,PHYXLN,PHYYLN
      NTIMES=(PLTTLL+1)/2
      TITLE=PLTTL(1:PLTTLL)
      DO 1 I=1, NTIMES
           IPTR1=(I-1)*2+1
           IPTR2=IPTR1+1
           RCH1=FLOAT(ICHAR(TITLE(IPTR1:IPTR1)))
           RCH2=FLOAT(ICHAR(TITLE(IPTR2:IPTR2)))
           WRITE(48) 5, RCH1, RCH2
 1         CONTINUE
C
      IF (LBRDR) THEN
           CALL X1XMOV(-XPHORG,-YPHORG)
           CALL X1XDRW(-XPHORG,PHYYLN-YPHORG)
           CALL X1XDRW(PHYXLN-XPHORG,PHYYLN-YPHORG)
           CALL X1XDRW(PHYXLN-XPHORG,-YPHORG)
           CALL X1XDRW(-XPHORG,-YPHORG)
           ENDIF
      XMINPT=SPLAX1
      XMAXPT=SPLAX2
      YMINPT=SPLAY1
      YMAXPT=SPLAY2
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE PLXTR1
C     ***ROUTINE TO SWITCH ON TRACING AT THE HIGHEST LEVEL
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      TRACE=.TRUE.
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE PLXTR2
C     ***ROUTINE TO SWITCH ON TRACING AT THE SECOND LEVEL
      LOGICAL TRACE2
      COMMON /TRXDT2/ TRACE2
      TRACE2=.TRUE.
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE X1XMOV(X,Y)
C     ***ROUTINE TO MOVE CURSOR TO (X,Y) POSITION (INCHES)
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'SIZDT.FOR'
      COMMON /GRXDT3/ RSCALE
      COMMON /GRXDTP/ XCURPO, YCURPO
C
      XCURPO=X
      YCURPO=Y
      IF (X.LT.XMINPT) XMINPT=X
      IF (X.GT.XMAXPT) XMAXPT=X
      IF (Y.LT.YMINPT) YMINPT=Y
      IF (Y.GT.YMAXPT) YMAXPT=Y
      IF (LTTCRT) THEN
           IX=IFIX(((X+XPHORG)*RSCALE)+0.5)
           IY=IFIX(((Y+YPHORG)*RSCALE)+0.5)
           CALL TKXMOV(IX,IY)
           ENDIF
      WRITE(48) 3, X+XPHORG, Y+YPHORG
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE X1XDRW(X,Y)
C     ***ROUTINE TO DRAW LINE TO (X,Y) POSITION (INCHES)
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'SIZDT.FOR'
      COMMON /GRXDT3/ RSCALE
      COMMON /GRXDTP/ XCURPO, YCURPO
C
      IF ((X.EQ.XCURPO).AND.(Y.EQ.YCURPO)) RETURN
      XCURPO=X
      YCURPO=Y
      IF (X.LT.XMINPT) XMINPT=X
      IF (X.GT.XMAXPT) XMAXPT=X
      IF (Y.LT.YMINPT) YMINPT=Y
      IF (Y.GT.YMAXPT) YMAXPT=Y
      IF (LTTCRT) THEN
           IX=IFIX(((X+XPHORG)*RSCALE)+0.5)
           IY=IFIX(((Y+YPHORG)*RSCALE)+0.5)
           CALL TKXDRW(IX,IY)
           ENDIF
      WRITE(48) 4, X+XPHORG, Y+YPHORG
      RETURN
      END
c
c
c
      subroutine bgnfll
c     *** start polygon definition
      REAL OLDX, OLDY
      COMMON /BLXDAT/ OLDX, OLDY
      logical panel
      common /polfil/ panel
      panel=.true.
c >>> force an initial move
      OLDX=0
      OLDY=0
      write(48) 7, 0.0, 0.0
      return
      entry endfll
c     *** make filled polygon
      call tkxepa
      panel=.false.
      OLDX=0
      OLDY=0
      write(48) 8, 0.0, 0.0
      return
      end
C
C -----------------------------------------------------
C
      SUBROUTINE PLXERR(IERRNO)
C     ***ROUTINE TO REPORT ERROR
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      IF (TRACE) WRITE(6,100) IERRNO
 100      FORMAT(' ***Error no: ',I6,' reported by MINDIS')
      RETURN
      END
C
C -----------------------------------------------------
C
      REAL FUNCTION RSXANG(RANGLE)
C     ***ROUTINE TO RESET ANGLE FOR CHAR OUTPUT
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'MSGDT.FOR'
      RSXANG=(MSGANG/RPI)*180.
      MSGANG=(RANGLE/180.)*RPI
      RETURN
      END
C
C -----------------------------------------------------
C
      REAL FUNCTION RSXHGT(RHGHT)
C     ***ROUTINE TO RESET MESSAGE HEIGHT
      INCLUDE 'MSGDT.FOR'
      RSXHGT=MSGHGT
      MSGHGT=RHGHT
      MSGSTP=RHGHT/6.0
      RETURN
      END
C
C -----------------------------------------------------
C
      INTEGER FUNCTION GTXPLC(RBASEV, RSTEPV, LINTAX)
C     ***ROUTINE TO FIND NO OF SIGNIFICANT DIGITS FOR AXIS LABELS
      REAL RBASEV, RSTEPV
      LOGICAL LINTAX
      GTXPLC=0
      IF (LINTAX) RETURN
      RBVAL=ABS(RBASEV)
      RSVAL=ABS(RSTEPV)
      RBVAL=RBVAL-INT(RBVAL+0.001)
      RSVAL=RSVAL-INT(RSVAL+0.001)
C
C     ***NOW LOOK TO SEE NUMBER OF SIGNIFICANT PLACES NEEDED
C     ***FIRST - LOOK AT STEP VALUE
      RVAL=RSVAL
      ISTSIG=0
      IF (RVAL.EQ.0) GOTO 5
      DO 1 I=1,6
           RVAL=RVAL*10.0
           IRVAL=INT(RVAL+0.001)
           RVAL=RVAL-IRVAL
           IF (IRVAL.NE.0) GOTO 2
 1         CONTINUE
      I=6
      GOTO 4
C
 2    CONTINUE
      IF (RVAL.LT.0.00001) GOTO 4
      DO 3 I=I+1,6
           RVAL=RVAL*10.0
           IRVAL=INT(RVAL+0.001)
           RVAL=RVAL-IRVAL
           IF (RVAL.LT.0.00001) GOTO 4
 3         CONTINUE
      I=6
 4    CONTINUE
      ISTSIG=I
 5    CONTINUE
C
C     ***NOW LOOK AT BASE VALUE
      RVAL=RBVAL
      IBASIG=0
      IF (RVAL.EQ.0) GOTO 91
      DO 6 I=1,6
           RVAL=RVAL*10.0
           IRVAL=INT(RVAL+0.001)
           RVAL=RVAL-IRVAL
           IF (IRVAL.NE.0) GOTO 7
 6         CONTINUE
      I=6
      GOTO 9
C
 7    CONTINUE
      IF (RVAL.LT.0.00001) GOTO 9
      DO 8 I=I+1,6
           RVAL=RVAL*10.0
           IRVAL=INT(RVAL+0.001)
           RVAL=RVAL-IRVAL
           IF (RVAL.LT.0.00001) GOTO 9
 8         CONTINUE
      I=6
 9    CONTINUE
      IBASIG=I
 91   CONTINUE
C
C     ***NOW DETERMINE WHICH VALUE TO USE
      GTXPLC=MAX0(1,ISTSIG,IBASIG)
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE DRXCAL(RVAL,IDPLCS,IDIR,X,Y)
C     ***ROUTINE TO OUTPUT LABEL AT POINT (X,Y)
      CHARACTER*30 LABEL
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'MSGDT.FOR'
C
C     ***FIRST CONVERT LABEL INTO ASCII PRINTING FORMAT
      RVAL1=RVAL+SIGN((5.0/(10.0**(IDPLCS+1))),RVAL)
      WRITE(LABEL,100) RVAL1
 100      FORMAT(F18.8)
      INCHRS=18
C     ***NOW FIND LIMITS OF ACTUAL CHARACTERS
      DO 1 I=1,30
           IF (LABEL(I:I).NE.' ') GOTO 2
 1         CONTINUE
      STOP 'ERROR: DRXCAL: No non-space char in label string'
 2    CONTINUE
      IF (LABEL(I:I).EQ.'.') THEN
           I=I-1
           LABEL(I:I)='0'
           ENDIF
      DO 3 J=I,30
           IF (LABEL(J:J).EQ.'.') GOTO 4
 3         CONTINUE
      STOP 'ERROR: DRXCAL: No full-stop in label string'
 4    CONTINUE
C
C     ***GET READY FOR PRINTING
      INPLCS=IDPLCS
      IF (INPLCS.EQ.0) INPLCS=-1
C%%   WIDTH=((FLOAT(J-I+1+INPLCS)*5.0)-1.0)*MSGSTP
      WIDTH=CHXLTH(LABEL(I:J+INPLCS)//'$')
      D=0.05+MSGHGT/2.0
      RANGL=XLBANG
      IF ((IDIR.EQ.1).OR.(IDIR.EQ.3)) RANGL=YLBANG
      RSAVAN=RSXANG(RANGL)
      RH1=MSGHGT*SIN(MSGANG)
      RH2=MSGHGT*COS(MSGANG)
      RL1=WIDTH*COS(MSGANG)
      RL2=WIDTH*SIN(MSGANG)
      IF (IDIR.EQ.1) THEN
C          ***LABEL TO RIGHT OF Y AXIS
           RLVAL1=0.0
           RLVAL2=0.0
           IF (SIN(MSGANG).GT.0.0) RLVAL1=1.0
           IF (COS(MSGANG).LT.0.0) RLVAL2=1.0
           X1=X+D+(ABS(RH1)*RLVAL1)+(ABS(RL1)*RLVAL2)
           Y1=Y-((RH2+RL2)/2.0)
      ELSEIF (IDIR.EQ.3) THEN
C          ***LABEL TO LEFT OF Y AXIS
           RLVAL1=0.0
           RLVAL2=0.0
           IF (SIN(MSGANG).LT.0.0) RLVAL1=1.0
           IF (COS(MSGANG).GT.0.0) RLVAL2=1.0
           X1=X-D-(ABS(RH1)*RLVAL1)-(ABS(RL1)*RLVAL2)
           Y1=Y-((RH2+RL2)/2.0)
      ELSEIF (IDIR.EQ.2) THEN
C          ***LABEL ABOVE X AXIS
           RLVAL1=0.0
           RLVAL2=0.0
           IF (COS(MSGANG).LT.0.0) RLVAL1=1.0
           IF (SIN(MSGANG).LT.0.0) RLVAL2=1.0
           X1=X+((RH1-RL1)/2.0)
           Y1=Y+D+(ABS(RH2)*RLVAL1)+(ABS(RL2)*RLVAL2)
      ELSEIF (IDIR.EQ.4) THEN
C          ***LABEL BELOW X AXIS
           RLVAL1=0.0
           RLVAL2=0.0
           IF (COS(MSGANG).GT.0.0) RLVAL1=1.0
           IF (SIN(MSGANG).GT.0.0) RLVAL2=1.0
           X1=X+((RH1-RL1)/2.0)
           Y1=Y-D-(ABS(RH2)*RLVAL1)-(ABS(RL2)*RLVAL2)
           ENDIF
C
C     ***OUTPUT THE LABEL
      CALL CHXLIN(X1,Y1,LABEL(I:J+INPLCS)//'$')
C
C     ***RESET AND RETURN TO CALLER
      RTMP=RSXANG(RSAVAN)
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE GTXXYC(X,Y,CHARAC,COLOUR,PHYSUN)
C     ***Routine to read XY cursor
      REAL X,Y
      CHARACTER*1 CHARAC
      INTEGER COLOUR
      LOGICAL PHYSUN
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'SIZDT.FOR'
      COMMON /GRXDT3/ RSCALE
C
      CALL TKXXYI(X1,Y1,CHARAC,COLOUR)
      X2=(X1/RSCALE)-XPHORG
      Y2=(Y1/RSCALE)-YPHORG
      IF (.NOT.PHYSUN) THEN
           CALL INXCVT(X2,Y2,X2,Y2)
           ENDIF
      X=X2
      Y=Y2
      RETURN
      END
C
C -----------------------------------------------------
C
