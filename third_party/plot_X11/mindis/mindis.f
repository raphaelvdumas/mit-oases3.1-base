C
C     ***MINDIS - SACLANTCEN GRAPH PLOTTING PACKAGE
C
C     ***JOHN STEIERT   -   02-DEC-81
C
C     *** AMMENDED:
C
C          JS     13-APR-82   ENDPL: ALWAYS CALL PLXCLR.
C          JS     22-NOV-82   RESET: ALLOW RESET OF SPLINE + POLORG
C          JS      2-Jun-83   Standard FORTRAN version
C          JS     15-Jun-83   Initialise SECRTY using DATA statement
C                             Call GTXDAT instead of ADATE
C          JS     25-Jul-83   New routine SETCOL
C          JS     27-Sep-83   Improve LEVERR and ISTRLN routines.
C          JS     01-Nov-83   Initialise LTTCRT in DATA statement.
C          JS     20-Feb-84   New routines:  BASALF, COMPLX, DUPLX,
C                                            MXiALF, ROTPIC, SCLPIC,
C                                            SIMPLX
C          JS     15-Mar-84   New routines:  INCH,   METRIC
C          JS     19-Mar-84   New routines:  RLVEC,  STRTPT, CONNPT,
C                                            XAXANG
C          JS     23-Mar-84   Allow 9 MXiALF calls instead of 6.
C          JS     26-Mar-84   Make additional x and y axes work.
C          JS     28-Mar-84   New routines:  LINEST, LINESP, NOAXIS
C          JS      2-Apr-84   New routines:  GRAPH,  XGRAF,  YGRAF,
C                                            XSTAXS, YSTAXS, RLMESS,
C                                            SHIFT
C          JS      5-Jan-84   New routines:  XYINPP, XYINPU
C          JS     14-Jan-84   New routine:   TBFOUT
C	   JS	  23-May-86   New routine:   NOBRDR, XMESS, DMOD
C          JS                 Change plot filename to YMDDHHMMSS
C          JS     27-May-86   Correct bugs in cursor routines XYINPP, XYINPU
c          HS     19-aug-93   Added Porters extensions at end of file
C
C -----------------------------------------------------------
C
      SUBROUTINE ANGLE(ANG)
C     ***ROUTINE TO ROTATE TEXT IN MESSAGES
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'MSGDT.FOR'
C
      IF (TRACE) WRITE(6,100) ANG
 100      FORMAT(' ANGLE called: ANG = ',E14.7)
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('ANGLE ','1,2 OR 3$')
C
      MSGANG=(ANG/180.0)*RPI
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE BASALF(ALPHA)
C     ***ROUTINE TO SET BASIC ALPHABET
      CHARACTER*(*) ALPHA
      CHARACTER*20 SP
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      SP='   '
      ISLEN=ISTRLN(ALPHA,20)
      IF (TRACE) WRITE(6,100) ALPHA(1:ISLEN),SP(1:20-ISLEN)
 100      FORMAT(' BASALF called: '/
     $           ' ALPHA = ',A,A)
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('BASALF','1,2 OR 3$')
C
      CALL CHXALF(1,ALPHA,')')
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE BGNPL(IPLOT)
C     ***ROUTINE TO BEGIN A PLOT
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'BLNKD.FOR'
      INCLUDE 'CARTD.FOR'
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'MSGDT.FOR'
      INCLUDE 'NAMDT.FOR'
      INCLUDE 'POLDT.FOR'
      INCLUDE 'SIZDT.FOR'
      INCLUDE 'SYMDT.FOR'
C
      CALL OUTXID
      IF (TRACE) WRITE(6,100) IPLOT
 100     FORMAT(' BGNPL called: IPLOT= ',I10)
      IF (PLTLVL.NE.0.AND.PLTLVL.NE.1) CALL LEVERR('BGNPL ','0 OR 1$')
C
      RPI=ACOS(0.0)*2.0
      PHYXLN=8.5
      PHYYLN=11.0
      BLNKON(1)=.FALSE.
      BLNKON(2)=.FALSE.
      BLNKON(3)=.FALSE.
      BLNKON(4)=.FALSE.
      LWNDOW=.FALSE.
      BLSCLF=1.0
      SYMANG=0.0
      MSGANG=0.0
      UPRGHT=.TRUE.
      CHECK =.TRUE.
      LPHORG=.FALSE.
      XPHORG=0.0
      YPHORG=0.0
      LSYMSP=.FALSE.
      CURSYM=0
      NXTSYM=0
      LINTX =.FALSE.
      LINTY =.FALSE.
      ADDXLN=0.0
      ADDYLN=0.0
      XLBANG=0.0
      YLBANG=90.0
      XTIKS =1
      YTIKS =1
      XADTKS=0
      YADTKS=0
      HDGLNS=-1
      HDGULS(1)=0
      HDGULS(2)=0
      HDGULS(3)=0
      HDGULS(4)=0
      HDGHGT(1)=1
      HDGHGT(2)=1
      HDGHGT(3)=1
      HDGHGT(4)=1
      HDGCLI=0
C      LTTCRT=.FALSE.
      MSGHGT=0.14
      MSGSTP=0.14/6.0
      CHALPH=2
      SYMHGT=0.07
      SYMSTP=0.07/4.0
      SYMSCL=1.0
      POLARS=.FALSE.
      PLTTYP=0
      LINTYP=1
      INTERP=0
      LAXIS =.TRUE.
      LBRDR =.TRUE.
      STCLLN=40
      STILLN=11
      LINSPC=1.5
      XMGOFS=0.0
      YMGOFS=0.0
C
C     ***Create filename as follows:
C        <y><hex month><dd><hh><mm><ss>
      CALL GTXDAT(PLTDAT,PLTTIM)
      PLTFNM(1:1)=PLTDAT(6:6)
      READ(PLTDAT,200) IMO
 200      FORMAT(I2)
      IF (IMO.LT.10) THEN
           WRITE(PLTFNM(2:2),201) IMO
 201           FORMAT(1I1)
      ELSEIF (IMO.EQ.10) THEN
           PLTFNM(2:2)='A'
      ELSEIF (IMO.EQ.11) THEN
           PLTFNM(2:2)='B'
      ELSEIF (IMO.EQ.12) THEN
           PLTFNM(2:2)='C'
      ELSE
           PLTFNM(2:2)='X'
           ENDIF
      PLTFNM(3:4)=PLTDAT(3:4)
      PLTFNM(5:10)=PLTTIM(1:6)
C
C      PLTFNM=PLTDAT(1:4)//PLTTIM(1:6)
C      SECRTY=0
      CALL GRXINI
      IF (TRACE) WRITE(6,105) PLTFNM
 105      FORMAT(' PLTFNM = ',1A10)
      PLTNO=IABS(IPLOT)
      IF (IPLOT.GE.0) WRITE(6,110) PLTNO
 110      FORMAT(' Plot no ',I6,' starting')
      LBGNPL=.TRUE.
      PLTLVL=1
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE BLNK1(XLEFT,XRIGHT,YLOWER,YUPPER,IFRAME)
C     ***ROUTINE TO BLANK OUT AREA
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      CHARACTER*6 PRCNAM
      INTEGER IBLKNO
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'BLNKD.FOR'
C
      IBLKNO=1
      PRCNAM='BLNK1'
      GOTO 1
C
      ENTRY BLNK2(XLEFT,XRIGHT,YLOWER,YUPPER,IFRAME)
      IBLKNO=2
      PRCNAM='BLNK2'
      GOTO 1
C
      ENTRY BLNK3(XLEFT,XRIGHT,YLOWER,YUPPER,IFRAME)
      IBLKNO=3
      PRCNAM='BLNK3'
      GOTO 1
C
      ENTRY BLNK4(XLEFT,XRIGHT,YLOWER,YUPPER,IFRAME)
      IBLKNO=4
      PRCNAM='BLNK4'
C
 1    CONTINUE
      IF (TRACE) WRITE(6,100) PRCNAM, XLEFT, XRIGHT, YLOWER, YUPPER,
     $                        IFRAME
 100      FORMAT(' ',1A6,' called: Params = ',4E14.7,I10)
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR(PRCNAM,'1,2 OR 3$')
C
      BLNKON(IBLKNO)=.TRUE.
      BLNKX1(IBLKNO)=XLEFT*SCLUNI
      BLNKX2(IBLKNO)=XRIGHT*SCLUNI
      BLNKY1(IBLKNO)=YLOWER*SCLUNI
      BLNKY2(IBLKNO)=YUPPER*SCLUNI
      IF (IFRAME.GE.0) CALL DRXBLN(IBLKNO,IFRAME)
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE BLOWUP(XY)
C     ***ROUTINE TO SCALE PLOT
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100) XY
 100      FORMAT(' BLOWUP called: XY = ',E14.7)
      IF (PLTLVL.NE.1) CALL LEVERR('BLOWUP','1$')
C
      BLSCLF=XY
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE CHNDOT
C     ***ROUTINE TO SET LINE TO CHAIN-DOTTED
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100)
 100      FORMAT(' CHNDOT called')
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('CHNDOT','1,2 OR 3$')
C
      LINTYP=3
      CALL PTXSET(LINTYP)
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE CHNDSH
C     ***ROUTINE TO SET LINE TO CHAIN-DASHED
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100)
 100      FORMAT(' CHNDSH called')
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('CHNDSH','1,2 OR 3$')
C
      LINTYP=4
      CALL PTXSET(LINTYP)
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE COMPLX
C     ***Routine to select Complex character set.
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'MSGDT.FOR'
C
      IF (TRACE) WRITE(6,100)
 100      FORMAT(' COMPLX called')
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('COMPLX','1,2 OR 3$')
C
      CHALPH=2
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE CONNPT(X,Y)
C     ***Routine to draw to point (x,y) in physical units.
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100) X,Y
 100      FORMAT(' CONNPT called: X = ',E14.7,' Y = ',E14.7)
      IF (PLTLVL.NE.2.AND.PLTLVL.NE.3) CALL LEVERR('CONNPT','2 OR 3$')
C
      CALL BLXDRW(X*SCLUNI,Y*SCLUNI)
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE CURVE(XARAY, YARAY, NPNTS, IMARK)
C     ***ROUTINE TO DRAW A CURVE
      REAL XARAY(NPNTS), YARAY(NPNTS)
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100) NPNTS,IMARK,(XARAY(I),YARAY(I),I=1,NPNTS)
 100      FORMAT(' CURVE called: NPNTS = ',I10,' IMARK = ',I10/
     $           ' Data points ='/(' ',E14.7,' ',E14.7))
      IF (PLTLVL.NE.3) CALL LEVERR('CURVE ','3$')
C
      IF (.NOT.LSYMSP) CURSYM=NXTSYM
      NXTSYM=NXTSYM+1
      IF (NXTSYM.GT.14) NXTSYM=0
      CALL PLXVLS(XARAY,YARAY,NPNTS,IMARK)
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE POLYGO(XARAY, YARAY, NPNTS)
C     ***ROUTINE TO DRAW A FILLED POLYGON
C        POLYGON IS FILLED WITH CURRENT COLOR.
      REAL XARAY(NPNTS), YARAY(NPNTS)
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100) NPNTS,(XARAY(I),YARAY(I),I=1,NPNTS)
 100      FORMAT(' POLYGO called: NPNTS = ',I10,/
     $           ' Data points ='/(' ',E14.7,' ',E14.7))
      IF (PLTLVL.NE.3) CALL LEVERR('POLYGO','3$')
C
      CALL PLXVLP(XARAY,YARAY,NPNTS)
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE DASH
C     ***ROUTINE TO SET LINE TO DASHED
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100)
 100      FORMAT(' DASH called')
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('DASH  ','1,2 OR 3$')
C
      LINTYP=2
      CALL PTXSET(LINTYP)
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE DMOD(DEV)
C     ***ROUTINE TO SET DEFAULT MINDIS OUTPUT DEVICE
      CHARACTER*(*) DEV
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100) DEV
 100      FORMAT(' DMOD called: DEV = ',1A20)
      IF (PLTLVL.NE.2.AND.PLTLVL.NE.3) CALL LEVERR('DMOD  ','2 OR 3$')
C
      DEFHCD=DEV
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE DONEPL
C     ***ROUTINE TO TERMINATE A PLOT
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100)
 100      FORMAT(' DONEPL called')
      IF (PLTLVL.NE.1) CALL LEVERR('DONEPL','1$')
C
      IF (LTTCRT) CALL PLXCLS
      PLTLVL=0
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE DOT
C     ***ROUTINE TO SET LINE TO DOTTED
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100)
 100      FORMAT(' DOT called')
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('DOT   ','1,2 OR 3$')
C
      LINTYP=1
      CALL PTXSET(LINTYP)
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE DUPLX
C     ***Routine to select Duplex character set.
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'MSGDT.FOR'
C
      IF (TRACE) WRITE(6,100)
 100      FORMAT(' DUPLX called')
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('DUPLX','1,2 OR 3$')
C
      CHALPH=3
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE ENDGR(IPLOT)
C     ***ROUTINE TO END A PLOT AND CREATE NEW PHYSICAL ORIGIN
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100) IPLOT
 100      FORMAT(' ENDGR called: IPLOT = ',I10)
      IF (PLTLVL.NE.3) CALL LEVERR('ENDGR ','3$')
C
      IF (IPLOT.NE.0) THEN
           PLTNO=IABS(IPLOT)
           IF (IPLOT.GT.0) CALL PRXSUM
           ENDIF
      PLTLVL=1
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE ENDPL(IPLOT)
C     ***ROUTINE TO END A PLOT
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100) IPLOT
 100      FORMAT(' ENDPL called: IPLOT = ',I10)
      IF (PLTLVL.NE.3) CALL LEVERR('ENDPL ','3$')
C
      CALL TKXSTP(1)
      IF (IPLOT.NE.0) THEN
           PLTNO=IABS(IPLOT)
           CALL DRXPLI
           IF (IPLOT.GT.0) CALL PRXSUM
           ENDIF
      CALL PLXCLR
      PLTLVL=1
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE FRAME
C     ***ROUTINE TO DRAW FRAME AROUND SUB-PLOT AREA
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100)
 100      FORMAT(' FRAME called')
      IF (PLTLVL.NE.2.AND.PLTLVL.NE.3) CALL LEVERR('FRAME ','2 OR 3$')
C
      CALL DRXFRM
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE GRAF(XORIG,XSTP,XMAX,YORIG,YSTP,YMAX)
C     ***ROUTINE TO SET LINEAR AXES IN USER'S UNITS
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'CARTD.FOR'
      INCLUDE 'SIZDT.FOR'
C
      IF (TRACE) WRITE(6,100) XORIG,XSTP,XMAX,YORIG,YSTP,YMAX
 100      FORMAT(' GRAF called: XORIG,XSTP,XMAX= ',3E14.7/
     $           '              YORIG,YSTP,YMAX= ',3E14.7)
      IF (PLTLVL.NE.2) CALL LEVERR('GRAF  ','2$')
C
      CTXORG=XORIG
      CTYORG=YORIG
      CTXSTP=XSTP
      CTYSTP=YSTP
      CTXMAX=XMAX
      CTYMAX=YMAX
      CTXPST=(CTXSTP/(CTXMAX-CTXORG))*(SPLAX2-SPLAX1)
      CTYPST=(CTYSTP/(CTYMAX-CTYORG))*(SPLAY2-SPLAY1)
      CALL DRXCAX
      PLTLVL=3
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE GRAPH(XORIG,XSTEP,YORIG,YSTEP)
C     ***ROUTINE TO SET LINEAR AXES IN UNITS/INCH
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'CARTD.FOR'
      INCLUDE 'SIZDT.FOR'
C
      IF (TRACE) WRITE(6,100) XORIG,XSTEP,YORIG,YSTEP
 100      FORMAT(' GRAPH called: XORIG,XSTEP= ',2E14.7/
     $           '              YORIG,YSTEP= ',2E14.7)
      IF (PLTLVL.NE.2) CALL LEVERR('GRAPH ','2$')
C
      CTXORG=XORIG
      CTYORG=YORIG
      CTXSTP=XSTEP
      CTYSTP=YSTEP
      CTXMAX=XORIG+(SPLAX2-SPLAX1)*XSTEP
      CTYMAX=YORIG+(SPLAY2-SPLAY1)*YSTEP
      CTXPST=1.0
      CTYPST=1.0
      CALL DRXCAX
      PLTLVL=3
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE GRID(IXGRID,IYGRID)
C     ***ROUTINE TO DRAW A GRID
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100) IXGRID, IYGRID
 100      FORMAT(' GRID called: IXGRID = ',I10,' IYGRID = ',I10)
      IF (PLTLVL.NE.3) CALL LEVERR('GRID  ','3$')
C
      IF (PLTTYP.EQ.1) THEN
           CALL DRXPGD(IXGRID,IYGRID)
      ELSE
           CALL DRXCGD(IXGRID,IYGRID)
           ENDIF
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE HEADIN(LXHEAD,IXHEAD,IHITE,NLINES)
C     ***ROUTINE TO OUTPUT A ONE LINE HEADING
      CHARACTER*(*) LXHEAD
      CHARACTER*72 SPACES
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'NAMDT.FOR'
C
      SPACES='                               '
      ISLEN=IABS(IXHEAD)
      IF (ISLEN.EQ.100) THEN
           ISLEN=ISTRLN(LXHEAD,72)
           ENDIF
      IF (TRACE) WRITE(6,100) IXHEAD,IHITE,NLINES,LXHEAD(1:ISLEN),
     $                                 SPACES(1:72-ISLEN)
 100      FORMAT(' HEADIN called: IXHEAD = ',I4,' IHITE = ',I4,
     $           ' NLINES = ',I4/' LXHEAD = ',A,A)
      IF (PLTLVL.NE.2.AND.PLTLVL.NE.3) CALL LEVERR('HEADIN','2 OR 3$')
C
      IF (HDGLNS.EQ.-1) THEN
           HDGLNS=NLINES
           HDGCLI=0
           ENDIF
      HDGCLI=HDGCLI+1
      IF ((IHITE.LT.0).OR.(IXHEAD.LT.0)) HDGULS(HDGCLI)=1
      IF ((IHITE.LT.0).AND.(IXHEAD.LT.0)) HDGULS(HDGCLI)=2
      HDGHGT(HDGCLI)=IABS(IHITE)
      PLHDG(HDGCLI) =LXHEAD
      PLHDGL(HDGCLI)=ISLEN
      IF (HDGCLI.EQ.HDGLNS) CALL PLXHDG
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE HEIGHT(HITE)
C     ***ROUTINE TO SET CHARACTER HEIGHT FOR TEXT
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'MSGDT.FOR'
C
      IF (TRACE) WRITE(6,100) HITE
 100      FORMAT(' HEIGHT called: HITE = ',E14.7)
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('HEIGHT','1,2 OR 3$')
C
      MSGHGT=HITE*SCLUNI
      MSGSTP=MSGHGT/6.0
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE INCH
C     ***ROUTINE TO SET LENGTHS IN INCHES
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100)
 100      FORMAT(' INCH called')
      IF (PLTLVL.NE.0) CALL LEVERR('INCH','0$')
C
      SCLUNI=1.0
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE INTAXS
C     ***ROUTINE TO SET INTEGER LABELLING ON AXES
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100)
 100      FORMAT(' INTAXS called')
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('INTAXS','1,2 OR 3$')
C
      LINTX=.TRUE.
      LINTY=.TRUE.
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE LINES(LSTRNG,IPARAY,ILINE)
C     ***ROUTINE TO PACK A LINE OF TEXT
      CHARACTER*(*) LSTRNG
      CHARACTER*100 SP
      INTEGER IPARAY(1000)
      CHARACTER*100 STORAG
      CHARACTER CHAR
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'MSGDT.FOR'
C
      SP='   '
      ISLEN=ISTRLN(LSTRNG,72)
      IF (TRACE) WRITE(6,100) ILINE, LSTRNG(1:ISLEN),SP(1:72-ISLEN)
 100      FORMAT(' LINES called: ILINE = ',I4,' LSTRNG = ',A,A)
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('LINES ','1,2 OR 3$')
C
      STORAG(1:1)=CHAR(ISLEN)
      STORAG(2:2)=CHAR(IFIX(MSGHGT*100.))
      STORAG(3:3+ISLEN)=LSTRNG(1:ISLEN)
      CALL CYXSTI(STORAG,IPARAY(((ILINE-1)*STILLN)+1),15)
      RETURN
      END
C
      SUBROUTINE CYXSTI(CHRS,INTS,ILEN)
C     ***ROUTINE TO COPY CHARACTERS INTO INTEGERS
      CHARACTER*(*) CHRS
      INTEGER INTS(ILEN)
      INTEGER ICHRS(20)
      CHARACTER*100 CCHRS
      EQUIVALENCE (CCHRS,ICHRS)
      CCHRS=CHRS
      DO 1 I=1, ILEN
           INTS(I)=ICHRS(I)
 1         CONTINUE
      RETURN
      END
C
C -----------------------------------------------------------
C
      INTEGER FUNCTION LINEST(IPARAY, NWORDS, IMAX)
C     ***Routine to set line length for STORY etc.
      INTEGER IPARAY(NWORDS)
      INCLUDE 'MSGDT.FOR'
      INCLUDE 'CTLDT.FOR'
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INTEGER ISP
      CHARACTER*4 CSP
      EQUIVALENCE (ISP,CSP)
      DATA CSP / '    ' /
C
      IF (TRACE) WRITE(6,100) NWORDS, IMAX
 100      FORMAT(' LINEST called: NWORDS = ',I5,' IMAX = ',I5)
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('LINEST','1,2 or 3$')
      STCLLN=IMAX
      STILLN=(IMAX+2+3)/4
      LINEST=NWORDS/STILLN
      DO 1 I=1,NWORDS
           IPARAY(I)=ISP
 1         CONTINUE
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE LINESP(YRATIO)
C     ***Routine to set interline spacing for STORY etc.
      INCLUDE 'MSGDT.FOR'
      INCLUDE 'CTLDT.FOR'
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
C
      IF (TRACE) WRITE(6,100) YRATIO
 100      FORMAT(' LINESP called: YRATIO = ',E14.7)
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('LINESP','1,2 or 3$')
      LINSPC=YRATIO
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE LSTORY(IPARAY,NLINES,XPOS,YPOS)
C     ***ROUTINE TO PLOT A BLOCK OF TEXT WITH STRAIGHT LEFT MARGIN
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100) NLINES, XPOS, YPOS
 100      FORMAT(' LSTORY called: NLINES = ',I5,
     $           ' XPOS, YPOS = ',2E14.7)
      IF (PLTLVL.NE.2.AND.PLTLVL.NE.3) CALL LEVERR('LSTORY','2 OR 3$')
C
      CALL PLXLST(IPARAY,NLINES,XPOS*SCLUNI,YPOS*SCLUNI)
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE MARKER(ISYM)
C     ***ROUTINE TO SET CURVE MARKER TYPE
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100) ISYM
 100      FORMAT(' MARKER called: ISYM = ',I4)
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('MARKER','1,2 OR 3$')
C
      CURSYM=ISYM
      LSYMSP=.TRUE.
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE MESSAG(LMESS,IMESS,XPOS,YPOS)
C     ***ROUTINE TO PLOT A MESSAGE IN INCHES FROM PHYSICAL ORIGIN
      CHARACTER*(*) LMESS
      CHARACTER*150 SP
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      SP='   '
      ISLEN=IMESS
      IF (ISLEN.EQ.100) ISLEN=ISTRLN(LMESS,150)
      IF (TRACE)WRITE(6,100)XPOS,YPOS,LMESS(1:ISLEN),SP(1:150-ISLEN)
 100      FORMAT(' MESSAG called: XPOS,YPOS = ',2E14.7/
     $           ' LMESS = ',A,A)
      IF (PLTLVL.NE.2.AND.PLTLVL.NE.3) CALL LEVERR('MESSAG','2 OR 3$')
C
      CALL PLXMSG(LMESS,ISLEN,XPOS*SCLUNI,YPOS*SCLUNI)
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE METRIC
C     ***ROUTINE TO SET LENGTHS IN CENTIMETRES
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100)
 100      FORMAT(' METRIC called')
      IF (PLTLVL.NE.0) CALL LEVERR('METRIC','0$')
C
      SCLUNI=0.3937
      RETURN
      END
C
C
C -----------------------------------------------------------
C
      SUBROUTINE MIXALF(ALPHA)
C     ***ROUTINE TO SET SECOND ALPHABET
      CHARACTER*(*) ALPHA
      CHARACTER*20 SP
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      SP='   '
      ISLEN=ISTRLN(ALPHA,20)
      IF (TRACE) WRITE(6,100) ALPHA(1:ISLEN),SP(1:20-ISLEN)
 100      FORMAT(' MIXALF called: '/
     $           ' ALPHA = ',A,A)
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('MIXALF','1,2 OR 3$')
C
      CALL CHXALF(2,ALPHA,'(')
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE MX1ALF(ALPHA,LSHIFT)
C     ***ROUTINES TO SPECIFY ALPHABETS
      CHARACTER*(*) ALPHA
      CHARACTER*1 LSHIFT
      CHARACTER*20 SP
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
      CHARACTER*6 PRCNAM
C
      IALPH=1
      PRCNAM='MX1ALF'
      GOTO 1
C
      ENTRY MX2ALF(ALPHA,LSHIFT)
      IALPH=2
      PRCNAM='MX2ALF'
      GOTO 1
C
      ENTRY MX3ALF(ALPHA,LSHIFT)
      IALPH=3
      PRCNAM='MX3ALF'
      GOTO 1
C
      ENTRY MX4ALF(ALPHA,LSHIFT)
      IALPH=4
      PRCNAM='MX4ALF'
      GOTO 1
C
      ENTRY MX5ALF(ALPHA,LSHIFT)
      IALPH=5
      PRCNAM='MX5ALF'
      GOTO 1
C
      ENTRY MX6ALF(ALPHA,LSHIFT)
      IALPH=6
      PRCNAM='MX6ALF'
      GOTO 1
C
      ENTRY MX7ALF(ALPHA,LSHIFT)
      IALPH=7
      PRCNAM='MX7ALF'
      GOTO 1
C
      ENTRY MX8ALF(ALPHA,LSHIFT)
      IALPH=8
      PRCNAM='MX8ALF'
      GOTO 1
C
      ENTRY MX9ALF(ALPHA,LSHIFT)
      IALPH=9
      PRCNAM='MX9ALF'
C
 1    CONTINUE
      SP='   '
      ISLEN=ISTRLN(ALPHA,20)
      IF (TRACE) WRITE(6,100) PRCNAM, ALPHA(1:ISLEN),SP(1:20-ISLEN),
     $                        LSHIFT
 100      FORMAT(' ',1A6,' called: '/
     $           ' ALPHA = ',A,A,'  LSHIFT = ',1A1)
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR(PRCNAM,'1,2 OR 3$')
C
      CALL CHXALF(IALPH,ALPHA,LSHIFT)
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE NOAXIS
C     ***Routine to suppress output of axes
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100)
 100      FORMAT(' NOAXIS called')
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('NOAXIS','1,2 OR 3$')
C
      LAXIS=.FALSE.
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE NOBRDR
C     ***Routine to suppress output of page border outline
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100)
 100      FORMAT(' NOBRDR called')
      IF (PLTLVL.NE.1) CALL LEVERR('NOBRDR','1$')
C
      LBRDR=.FALSE.
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE NOCHEK
C     ***ROUTINE TO SUPPRESS LISTING OF POINTS OUT OF RANGE
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100)
 100      FORMAT(' NOCHEK called')
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('NOCHEK','1,2 OR 3$')
C
      CHECK=.FALSE.
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE OREL(XOREL,YOREL)
C     ***ROUTINE TO DEFINE PHYSICAL ORIGIN RELATIVE TO PREVIOUS
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'SIZDT.FOR'
C
      IF (TRACE) WRITE(6,100) XOREL, YOREL
 100      FORMAT(' OREL called: XOREL = ',E14.7,' YOREL = ',E14.7)
      IF (PLTLVL.NE.1) CALL LEVERR('OREL','1$')
C
      LPHORG=.TRUE.
      XPHORG=XPHORG+(XOREL*SCLUNI)
      YPHORG=YPHORG+(YOREL*SCLUNI)
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE PAGE(PAGEX,PAGEY)
C     ***ROUTINE TO DEFINE PAGE BORDER
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'SIZDT.FOR'
C
      IF (TRACE) WRITE(6,100) PAGEX,PAGEY
 100      FORMAT(' PAGE called: PAGEX = ',E14.7,' PAGEY = ',E14.7)
      IF (PLTLVL.NE.1) CALL LEVERR('PAGE  ','1$')
C
      PHYXLN=PAGEX*SCLUNI
      PHYYLN=PAGEY*SCLUNI
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE PHYSOR(XOR,YOR)
C     ***ROUTINE TO DEFINE PHYSICAL ORIGIN ABSOLUTELY
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'SIZDT.FOR'
C
      IF (TRACE) WRITE(6,100) XOR, YOR
 100      FORMAT(' PHYSOR called: XOR = ',E14.7,' YOR = ',E14.7)
      IF (PLTLVL.NE.1) CALL LEVERR('PHYSOR','1$')
C
      LPHORG=.TRUE.
      XPHORG=XOR*SCLUNI
      YPHORG=YOR*SCLUNI
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE POLAR(THETA,RSTEP,XDIST,YDIST)
C     ***ROUTINE TO SET UP POLAR AXIS
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'POLDT.FOR'
C
      IF (TRACE) WRITE(6,100) THETA,RSTEP,XDIST,YDIST
 100      FORMAT(' POLAR called: THETA = ',E14.7,' RSTEP = ',E14.7,
     $           ' XDIST = ',E14.7,' YDIST = ',E14.7)
      IF (PLTLVL.NE.2) CALL LEVERR('POLAR ','2$')
C
      POLARS=.TRUE.
      PLTTYP=1
      POLTHE=THETA
      POLSTP=RSTEP*SCLUNI
      POLXDI=XDIST*SCLUNI
      POLYDI=YDIST*SCLUNI
      CALL DRXPAX
      PLTLVL=3
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE POLORG(ORG)
C     ***ROUTINE TO SET RADIUS VALUE AT POLAR ORIGIN
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'POLDT.FOR'
C
      IF (TRACE) WRITE(6,100) ORG
 100      FORMAT(' POLORG called: ORG = ',E14.7)
      IF (PLTLVL.NE.1.AND.PLTLVL.NE.2) CALL LEVERR('POLORG','1 OR 2$')
C
      POLORV=ORG
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE RESET(NAME)
C     ***ROUTINE TO RESET A PARAMETER
      CHARACTER*(*) NAME
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'BLNKD.FOR'
      INCLUDE 'POLDT.FOR'
C
      IF (TRACE) WRITE(6,100) NAME
 100      FORMAT(' RESET called: NAME = ',1A6)
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('RESET ','1,2 OR 3$')
C
      IF (NAME.EQ.'BLNK1') THEN
           IF (.NOT.BLNKON(1)) WRITE(6,200) 'BLNK1 '
 200            FORMAT(' Warning: Reset ',1A6,': not set')
           BLNKON(1)=.FALSE.
      ELSEIF (NAME.EQ.'BLNK2') THEN
           IF (.NOT.BLNKON(2)) WRITE(6,200) 'BLNK2 '
           BLNKON(2)=.FALSE.
      ELSEIF (NAME.EQ.'BLNK3') THEN
           IF (.NOT.BLNKON(3)) WRITE(6,200) 'BLNK3 '
           BLNKON(3)=.FALSE.
      ELSEIF (NAME.EQ.'BLNK4') THEN
           IF (.NOT.BLNKON(4)) WRITE(6,200) 'BLNK4 '
           BLNKON(4)=.FALSE.
      ELSEIF (NAME.EQ.'BLNKS') THEN
           BLNKON(1)=.FALSE.
           BLNKON(2)=.FALSE.
           BLNKON(3)=.FALSE.
           BLNKON(4)=.FALSE.
      ELSEIF (NAME.EQ.'MARKER') THEN
           IF (.NOT.LSYMSP) WRITE(6,200) 'MARKER'
           LSYMSP=.FALSE.
      ELSEIF (NAME.EQ.'BLOWUP') THEN
           BLSCLF=1.0
      ELSEIF (NAME.EQ.'DOT') THEN
           IF (LINTYP.NE.1) WRITE(6,200) 'DOT   '
           LINTYP=0
           CALL PTXSET(LINTYP)
      ELSEIF (NAME.EQ.'DASH') THEN
           IF (LINTYP.NE.2) WRITE(6,200) 'DASH  '
           LINTYP=0
           CALL PTXSET(LINTYP)
      ELSEIF (NAME.EQ.'CHNDOT') THEN
           IF (LINTYP.NE.3) WRITE(6,200) 'CHNDOT'
           LINTYP=0
           CALL PTXSET(LINTYP)
      ELSEIF (NAME.EQ.'CHNDSH') THEN
           IF (LINTYP.NE.4) WRITE(6,200) 'CHNDSH'
           LINTYP=0
           CALL PTXSET(LINTYP)
      ELSEIF (NAME.EQ.'SPLINE') THEN
           IF (INTERP.NE.1) WRITE(6,200) 'SPLINE'
           INTERP=0
      ELSEIF (NAME.EQ.'POLORG') THEN
           IF (POLORV.NE.0.0) WRITE(6,200) 'POLORG'
           POLORV=0.0
      ELSEIF (NAME.EQ.'INCH') THEN
           SCLUNI=1.0
      ELSEIF (NAME.EQ.'METRIC') THEN
           SCLUNI=1.0
      ELSEIF (NAME.EQ.'NOAXIS') THEN
           LAXIS=.TRUE.
      ELSEIF (NAME.EQ.'NOBRDR') THEN
           LBRDR=.TRUE.
      ELSEIF (NAME.EQ.'LINESP') THEN
           LINSPC=1.5
      ELSEIF (NAME.EQ.'LINEST') THEN
           STCLLN=40
           STILLN=11
      ELSEIF (NAME.EQ.'SHIFT') THEN
           XMGOFS=0.0
           YMGOFS=0.0
      ELSE
C           ***ERROR - TYPE NOT RECOGNISED
           WRITE(6,210) NAME
 210           FORMAT(' Error: Reset type not known: ',1A6)
           ENDIF
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE RLVEC(X1,Y1,X2,Y2,IVEC)
C     ***ROUTINE TO DRAW A VECTOR IN USER UNITS
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100) X1,Y1,X2,Y2,IVEC
 100      FORMAT(' RLVEC called: (X1,Y1) = ',2E14.7,
     $           ' (X2,Y2) = ',2E14.7,' IVEC = ',I6)
      IF (PLTLVL.NE.3) CALL LEVERR('RLVEC','3$')
C
      CALL DRXVEC(X1,Y1,X2,Y2,IVEC,.FALSE.)
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE ROTPIC(ANG)
C     ***Routine cause markers to be rotated
      REAL ANG
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'SYMDT.FOR'
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100) ANG
 100      FORMAT(' ROTPIC called: ANG = ',E14.7)
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('ROTPIC','1,2 OR 3$')
C
      SYMANG=(ANG/180.0)*RPI
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE RLMESS(LMESS,IMESS,XVAL,YVAL)
C     ***ROUTINE TO PLOT A MESSAGE AT POINT SPECIFIED IN USER UNITS
      CHARACTER*(*) LMESS
      CHARACTER*150 SP
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      SP='   '
      ISLEN=IMESS
      IF (ISLEN.EQ.100) ISLEN=ISTRLN(LMESS,150)
      IF (TRACE)WRITE(6,100)XVAL,YVAL,LMESS(1:ISLEN),SP(1:150-ISLEN)
 100      FORMAT(' RLMESS called: XVAL,YVAL = ',2E14.7/
     $           ' LMESS = ',A,A)
      IF (PLTLVL.NE.3) CALL LEVERR('MESSAG','3$')
C
      CALL UNXCVT(XVAL,YVAL, XPOS,YPOS)
      CALL PLXMSG(LMESS,ISLEN,XPOS,YPOS)
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE SCLPIC(FAC)
C     ***Routine to set size multiplication factor for markers
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'SYMDT.FOR'
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100) FAC
 100      FORMAT(' SCLPIC called: FAC = ',E14.7)
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('SCLPIC','1,2 OR 3$')
C
      SYMSCL=FAC
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE SETCOL(ICOL)
C     ***Routine to set colour of pen
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
      IF (TRACE) WRITE(6,100) ICOL
 100      FORMAT(' SETCOL called: ICOL = ',I3)
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('SETCOL','1,2 or 3$')
      CALL TKXSTP(ICOL)
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE SHIFT(XOFF,YOFF)
C     ***Routine to specify offset for messages.
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'MSGDT.FOR'
      INCLUDE 'CTLDT.FOR'
      IF (TRACE) WRITE(6,100) XOFF,YOFF
 100      FORMAT(' SHIFT called: XOFF,YOFF = ',2E14.7)
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('SHIFT ','1,2 or 3$')
      XMGOFS=XOFF*SCLUNI
      YMGOFS=YOFF*SCLUNI
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE SIMPLX
C     ***Routine to select Simplex character set.
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'MSGDT.FOR'
C
      IF (TRACE) WRITE(6,100)
 100      FORMAT(' SIMPLX called')
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('SIMPLX','1,2 OR 3$')
C
      CHALPH=1
      RETURN
      END
C -----------------------------------------------------------
C
      SUBROUTINE SPLINE
C     ***ROUTINE TO SET INTERPOLATION TO CUBIC SPLINE
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100)
 100      FORMAT(' SPLINE called')
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('SPLINE','1,2 OR 3$')
      INTERP=1
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE STORY(IPARAY,NLINES,XPOS,YPOS)
C     ***ROUTINE TO PLOT A CENTRED BLOCK OF TEXT
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100) NLINES, XPOS, YPOS
 100      FORMAT(' STORY called: NLINES = ',I5,
     $           ' XPOS, YPOS = ',2E14.7)
      IF (PLTLVL.NE.2.AND.PLTLVL.NE.3) CALL LEVERR('STORY','2 OR 3$')
C
      CALL PLXSTO(IPARAY,NLINES,XPOS*SCLUNI,YPOS*SCLUNI)
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE STRTPT(X,Y)
C     ***Routine to move to point (x,y) in physical units.
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100) X,Y
 100      FORMAT(' STRTPT called: X = ',E14.7,' Y = ',E14.7)
      IF (PLTLVL.NE.2.AND.PLTLVL.NE.3) CALL LEVERR('STRTPT','2 OR 3$')
C
      CALL BLXMOV(X*SCLUNI,Y*SCLUNI)
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE TBFOUT
C     ***Routine to force out terminal buffer
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100)
 100      FORMAT(' TBFOUT called')
      IF (PLTLVL.NE.2.AND.PLTLVL.NE.3) CALL LEVERR('TBFOUT','2 OR 3$')
C
      IF (LTTCRT) CALL TKXOPB
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE TITLE(LTITLE,ITITLE,LXNAME,IXNAME,
     $                 LYNAME,IYNAME,XAXIS,YAXIS)
C     ***ROUTINE TO SPECIFY SUBPLOT AREA
      CHARACTER*(*) LTITLE, LXNAME, LYNAME
      CHARACTER*72 SP
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'NAMDT.FOR'
      INCLUDE 'SIZDT.FOR'
C
      SP='    '
      ISLEN1=IABS(ITITLE)
      ISLEN2=IABS(IXNAME)
      ISLEN3=IABS(IYNAME)
      IF (ISLEN1.EQ.100) ISLEN1=ISTRLN(LTITLE,72)
      IF (ISLEN2.EQ.100) ISLEN2=ISTRLN(LXNAME,72)
      IF (ISLEN3.EQ.100) ISLEN3=ISTRLN(LYNAME,72)
      IF (TRACE) WRITE(6,100) ITITLE,IXNAME,IYNAME,XAXIS,YAXIS
      IF (TRACE) WRITE(6,200) LTITLE(1:ISLEN1),SP(1:72-ISLEN1)
      IF (TRACE) WRITE(6,201) LXNAME(1:ISLEN2),SP(1:72-ISLEN2)
      IF (TRACE) WRITE(6,202) LYNAME(1:ISLEN3),SP(1:72-ISLEN3)
 100      FORMAT(' TITLE called: ITITLE = ',I4,' IXNAME = ',I4,
     $           ' IYNAME = ',I4,' XAXIS = ',E14.7,
     $           ' YAXIS = ',E14.7)
 200      FORMAT(' LTITLE = ',A,A)
 201      FORMAT(' LXNAME = ',A,A)
 202      FORMAT(' LYNAME = ',A,A)
      IF (PLTLVL.NE.1) CALL LEVERR('TITLE ','1$')
C
      IF (.NOT.LBGNPL) CALL BGNPL(1)
      IF (ITITLE.LT.0) THEN
           UPRGHT=.FALSE.
           RTMP=PHYXLN
           PHYXLN=PHYYLN
           PHYYLN=RTMP
           ENDIF
      XTIKS=1
      YTIKS=1
      IF (IXNAME.EQ.0) XTIKS=0
      IF (IYNAME.EQ.0) YTIKS=0
      IF (IXNAME.LT.0) XTIKS=2
      IF (IYNAME.LT.0) YTIKS=2
      XAXLEN=XAXIS*SCLUNI
      YAXLEN=YAXIS*SCLUNI
      PLTTL=LTITLE
      PLTTLL=ISLEN1
      PLXLAB=LXNAME
      PLXLBL=ISLEN2
      PLYLAB=LYNAME
      PLYLBL=ISLEN3
C
C     ***POSITION SUBPLOT AREA
      IF (.NOT.LPHORG) THEN
           IF (UPRGHT) THEN
                XMARG=0.5
                YMARG=0.0
           ELSE
                XMARG=0.0
                YMARG=0.5
                ENDIF
           RHGT=YAXIS*SCLUNI*1.143
           YDIF=PHYYLN-RHGT-YMARG
           YPHORG=YDIF/2.0
           XDIF=PHYXLN-XAXIS*SCLUNI-XMARG
           XPHORG=XMARG+(XDIF/2)
           ENDIF
      SPLAY1=0.0
      SPLAY2=YAXIS*SCLUNI
      SPLAX1=0.0
      SPLAX2=XAXIS*SCLUNI
      PLTLVL=2
      CALL PLXCHK
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE VECTOR(X1,Y1,X2,Y2,IVEC)
C     ***ROUTINE TO DRAW A VECTOR IN PHYSICAL UNITS
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100) X1,Y1,X2,Y2,IVEC
 100      FORMAT(' VECTOR called: (X1,Y1) = ',2E14.7,
     $           ' (X2,Y2) = ',2E14.7,' IVEC = ',I6)
      IF (PLTLVL.NE.2.AND.PLTLVL.NE.3) CALL LEVERR('VECTOR','2 OR 3$')
C
      CALL DRXVEC(X1*SCLUNI,Y1*SCLUNI,X2*SCLUNI,Y2*SCLUNI,IVEC,.TRUE.)
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE XAXANG(ANG)
C     ***ROUTINE TO PUT ANGLED LABELS ON X AXIS
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100) ANG
 100      FORMAT(' XAXANG called: ANG = ',E14.7)
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('XAXANG','1,2 OR 3$')
C
      XLBANG=ANG
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE XGRAF(XORIG,XSTP,XMAX,YORIG,YSTEP)
C     ***ROUTINE TO SET LINEAR AXES IN COMBINATION OF GRAF AND GRAPH
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'CARTD.FOR'
      INCLUDE 'SIZDT.FOR'
C
      IF (TRACE) WRITE(6,100) XORIG,XSTP,XMAX,YORIG,YSTEP
 100      FORMAT(' XGRAF called: XORIG,XSTP,XMAX= ',3E14.7/
     $           '              YORIG,YSTEP= ',2E14.7)
      IF (PLTLVL.NE.2) CALL LEVERR('XGRAF ','2$')
C
      CTXORG=XORIG
      CTYORG=YORIG
      CTXSTP=XSTP
      CTYSTP=YSTEP
      CTXMAX=XMAX
      CTYMAX=YORIG+(SPLAY2-SPLAY1)*YSTEP
      CTXPST=(CTXSTP/(CTXMAX-CTXORG))*(SPLAX2-SPLAX1)
      CTYPST=1.0
      CALL DRXCAX
      PLTLVL=3
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE XGRAXS(XORIG,XSTP,XMAX,XAXIS,LXNAME,IXNAME,XPOS,YPOS)
C     ***ROUTINE TO SUPPLY ADDITIONAL AXES OF THE GRAF TYPE
      CHARACTER*(*) LXNAME
      CHARACTER*72 SP
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'CARTD.FOR'
      INCLUDE 'SIZDT.FOR'
      INCLUDE 'NAMDT.FOR'
C
      SP='    '
      ISLEN=IABS(IXNAME)
      IF (ISLEN.EQ.100) ISLEN=ISTRLN(LXNAME,72)
      IF (TRACE) WRITE(6,100) XORIG, XSTP,XMAX,XAXIS,XPOS,YPOS,IXNAME,
     $             LXNAME(1:ISLEN),SP(1:72-ISLEN)
 100      FORMAT(' XGRAXS called: XORIG = ',E14.7,
     $           ' XSTP = ',E14.7/
     $           ' XMAX = ',E14.7,' XAXIS = ',E14.7,
     $           ' XPOS = ',E14.7/
     $           ' YPOS = ',E14.7,' IXNAME = ',I4/
     $           ' LXNAME = ',A,A)
      IF (PLTLVL.NE.3) CALL LEVERR('XGRAXS','3$')
C
      CTXORG=XORIG
      CTXSTP=XSTP
      CTXMAX=XMAX
      CTXPST=(CTXSTP/(CTXMAX-CTXORG))*(SPLAX2-SPLAX1)
      XAXLEN=XAXIS*SCLUNI
      PLXLAB=LXNAME
      PLXLBL=ISLEN
      ADDXLN=XPOS*SCLUNI
      ADDYLN=YPOS*SCLUNI
      XTIKS=1
      IF (IXNAME.EQ.0) XTIKS=0
      IF (IXNAME.LT.0) XTIKS=2
      CALL DRXCXX(XPOS*SCLUNI,YPOS*SCLUNI,(XPOS+XAXIS)*SCLUNI)
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE XINTAX
C     ***ROUTINE TO SET INTEGER LABELLING ON X-AXIS
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100)
 100      FORMAT(' XINTAX called')
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('XINTAX','1,2 OR 3$')
C
      LINTX=.TRUE.
      RETURN
      END
C
C -----------------------------------------------------------
C
      REAL FUNCTION XMESS(LMES,IMES)
C     ***ROUTINE TO SET RETURN LENGTH OF STRING
      CHARACTER*(*) LMES
      INTEGER IMES
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,*) 'XMESS called: LMES = ',LMES,' IMES = ',IMES
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('XMESS','1,2 OR 3$')
      ISLEN=IMES
      IF (ISLEN.EQ.100) ISLEN=ISTRLN(LMES,150)
C
c      XMESS=CHXLTH(LMES(1:ISLEN)//'$')/SCLUNI
      LMES(ISLEN+1:ISLEN+1)='$'
      XMESS=CHXLTH(LMES(1:ISLEN+1))/SCLUNI
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE XSTAXS(XORIG,XSTEP,XAXIS,LXNAME,IXNAME,XPOS,YPOS)
C     ***ROUTINE TO SUPPLY ADDITIONAL AXES OF THE GRAPH TYPE
      CHARACTER*(*) LXNAME
      CHARACTER*72 SP
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'CARTD.FOR'
      INCLUDE 'SIZDT.FOR'
      INCLUDE 'NAMDT.FOR'
C
      SP='    '
      ISLEN=IABS(IXNAME)
      IF (ISLEN.EQ.100) ISLEN=ISTRLN(LXNAME,72)
      IF (TRACE) WRITE(6,100) XORIG, XSTEP,XAXIS,XPOS,YPOS,IXNAME,
     $             LXNAME(1:ISLEN),SP(1:72-ISLEN)
 100      FORMAT(' XSTAXS called: XORIG = ',E14.7,
     $           ' XSTEP = ',E14.7/
     $           ' XAXIS = ',E14.7,
     $           ' XPOS = ',E14.7/
     $           ' YPOS = ',E14.7,' IXNAME = ',I4/
     $           ' LXNAME = ',A,A)
      IF (PLTLVL.NE.3) CALL LEVERR('XSTAXS','3$')
C
      CTXORG=XORIG
      CTXSTP=XSTEP
      CTXMAX=XORIG+(SPLAX2-SPLAX1)*XSTEP
      CTXPST=1.0
      XAXLEN=XAXIS*SCLUNI
      PLXLAB=LXNAME
      PLXLBL=ISLEN
      ADDXLN=XPOS*SCLUNI
      ADDYLN=YPOS*SCLUNI
      XTIKS=1
      IF (IXNAME.EQ.0) XTIKS=0
      IF (IXNAME.LT.0) XTIKS=2
      CALL DRXCXX(XPOS*SCLUNI,YPOS*SCLUNI,(XPOS+XAXIS)*SCLUNI)
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE XTICKS(ITICKS)
C     ***ROUTINE TO DRAW ADDITIONAL TICK MARKS
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100) ITICKS
 100      FORMAT(' XTICKS called: ITICKS = ',I6)
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('XTICKS','1,2 OR 3$')
C
      XADTKS=ITICKS
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE XYINPP(X,Y,CHARAC,COLOUR)
C     ***Routine to return position of XY cursor in physical units
      REAL X,Y,X1,Y1
      CHARACTER*1 CHARAC
      INTEGER COLOUR
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100)
 100      FORMAT(' XYINPP called')
      IF (PLTLVL.LT.2.OR.PLTLVL.GT.3) CALL LEVERR('XYINPP','2 OR 3$')
      CALL GTXXYC(X1,Y1,CHARAC,COLOUR,.TRUE.)
      X=X1/SCLUNI
      Y=Y1/SCLUNI
      IF (TRACE) WRITE(6,110) X,Y,CHARAC,COLOUR
 110       FORMAT(' XYINPP returning: X = ',E14.7,' Y = ',E14.7,
     $            ' CHARAC = ',O4,' COLOUR = ',I3)
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE XYINPU(X,Y,CHARAC,COLOUR)
C     ***Routine to return position of XY cursor in user units
      REAL X,Y
      CHARACTER*1 CHARAC
      INTEGER COLOUR
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100)
 100      FORMAT(' XYINPU called')
      IF (PLTLVL.NE.3) CALL LEVERR('XYINPU','3$')
      CALL GTXXYC(X,Y,CHARAC,COLOUR,.FALSE.)
      IF (TRACE) WRITE(6,110) X,Y,CHARAC,COLOUR
 110       FORMAT(' XYINPU returning: X = ',E14.7,' Y = ',E14.7,
     $            ' CHARAC = ',O4,' COLOUR = ',I3)
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE YAXANG(ANG)
C     ***ROUTINE TO PUT ANGLED LABELS ON Y AXIS
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100) ANG
 100      FORMAT(' YAXANG called: ANG = ',E14.7)
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('YAXANG','1,2 OR 3$')
C
      YLBANG=ANG
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE YGRAF(XORIG,XSTEP,YORIG,YSTP,YMAX)
C     ***ROUTINE TO SET LINEAR AXES IN COMBINATION OF GRAF AND GRAPH
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'CARTD.FOR'
      INCLUDE 'SIZDT.FOR'
C
      IF (TRACE) WRITE(6,100) XORIG,XSTEP,YORIG,YSTP,YMAX
 100      FORMAT(' YGRAF called: XORIG,XSTEP= ',2E14.7/
     $           '              YORIG,YSTP,YMAX= ',3E14.7)
      IF (PLTLVL.NE.2) CALL LEVERR('YGRAF ','2$')
C
      CTXORG=XORIG
      CTYORG=YORIG
      CTXSTP=XSTEP
      CTYSTP=YSTP
      CTXMAX=XORIG+(SPLAX2-SPLAX1)*XSTEP
      CTYMAX=YMAX
      CTXPST=1.0
      CTYPST=(CTYSTP/(CTYMAX-CTYORG))*(SPLAY2-SPLAY1)
      CALL DRXCAX
      PLTLVL=3
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE YGRAXS(YORIG,YSTP,YMAX,YAXIS,LYNAME,IYNAME,XPOS,YPOS)
C     ***ROUTINE TO SUPPLY ADDITIONAL AXES OF THE GRAF TYPE
      CHARACTER*(*) LYNAME
      CHARACTER*72 SP
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'CARTD.FOR'
      INCLUDE 'SIZDT.FOR'
      INCLUDE 'NAMDT.FOR'
C
      SP='  '
      ISLEN=IABS(IYNAME)
      IF (ISLEN.EQ.100) ISLEN=ISTRLN(LYNAME,72)
      IF (TRACE) WRITE(6,100) YORIG, YSTP,YMAX,YAXIS,XPOS,YPOS,IYNAME,
     $             LYNAME(1:ISLEN),SP(1:72-ISLEN)
 100      FORMAT(' YGRAXS called: YORIG = ',E14.7,
     $           ' YSTP = ',E14.7/
     $           ' YMAX = ',E14.7,' YAXIS = ',E14.7,
     $           ' XPOS = ',E14.7/
     $           ' YPOS = ',E14.7,' IYNAME = ',I4/
     $           ' LYNAME = ',A,A)
      IF (PLTLVL.NE.3) CALL LEVERR('YGRAXS','3$')
C
      CTYORG=YORIG
      CTYSTP=YSTP
      CTYMAX=YMAX
      CTYPST=(CTYSTP/(CTYMAX-CTYORG))*(SPLAY2-SPLAY1)
      YAXLEN=YAXIS*SCLUNI
      PLYLAB=LYNAME
      PLYLBL=ISLEN
      PLYLBL=ISLEN
      ADDXLN=XPOS*SCLUNI
      ADDYLN=YPOS*SCLUNI
      YTIKS=1
      IF (IYNAME.EQ.0) YTIKS=0
      IF (IYNAME.LT.0) YTIKS=2
      CALL DRXCXY(XPOS*SCLUNI,YPOS*SCLUNI,(YPOS+YAXIS)*SCLUNI)
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE YINTAX
C     ***ROUTINE TO SET INTEGER LABELLING ON Y-AXIS
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100)
 100      FORMAT(' YINTAX called')
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('YINTAX','1,2 OR 3$')
C
      LINTY=.TRUE.
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE YSTAXS(YORIG,YSTEP,YAXIS,LYNAME,IYNAME,XPOS,YPOS)
C     ***ROUTINE TO SUPPLY ADDITIONAL AXES OF THE GRAPH TYPE
      CHARACTER*(*) LYNAME
      CHARACTER*72 SP
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'CARTD.FOR'
      INCLUDE 'SIZDT.FOR'
      INCLUDE 'NAMDT.FOR'
C
      SP='    '
      ISLEN=IABS(IYNAME)
      IF (ISLEN.EQ.100) ISLEN=ISTRLN(LYNAME,72)
      IF (TRACE) WRITE(6,100) YORIG, YSTEP,YAXIS,XPOS,YPOS,IYNAME,
     $             LYNAME(1:ISLEN),SP(1:72-ISLEN)
 100      FORMAT(' YSTAXS called: YORIG = ',E14.7,
     $           ' YSTEP = ',E14.7/
     $           ' YAXIS = ',E14.7,
     $           ' XPOS = ',E14.7/
     $           ' YPOS = ',E14.7,' IYNAME = ',I4/
     $           ' LYNAME = ',A,A)
      IF (PLTLVL.NE.3) CALL LEVERR('YSTAXS','3$')
C
      CTYORG=YORIG
      CTYSTP=YSTEP
      CTYMAX=YORIG+(SPLAY2-SPLAY1)*YSTEP
      CTYPST=1.0
      YAXLEN=YAXIS*SCLUNI
      PLYLAB=LYNAME
      PLYLBL=ISLEN
      ADDXLN=XPOS*SCLUNI
      ADDYLN=YPOS*SCLUNI
      YTIKS=1
      IF (IYNAME.EQ.0) YTIKS=0
      IF (IYNAME.LT.0) YTIKS=2
      CALL DRXCXY(XPOS*SCLUNI,YPOS*SCLUNI,(YPOS+YAXIS)*SCLUNI)
      RETURN
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE YTICKS(ITICKS)
C     ***ROUTINE TO DRAW ADDITIONAL TICK MARKS
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      IF (TRACE) WRITE(6,100) ITICKS
 100      FORMAT(' YTICKS called: ITICKS = ',I6)
      IF (PLTLVL.LT.1.OR.PLTLVL.GT.3) CALL LEVERR('YTICKS','1,2 OR 3$')
C
      YADTKS=ITICKS
      RETURN
      END
C
C -----------------------------------------------------------
C
      INTEGER FUNCTION ISTRLN(STR,ILEN)
C     ***ROUTINE TO RETURN LENGTH OF STRING
      CHARACTER*(*) STR
C
      IPTR=INDEX(STR,'$')
      ILGT=LEN(STR)+1
      IF ((IPTR.EQ.0).OR.(IPTR.GT.ILEN)) IPTR=ILGT
      ISTRLN=IPTR-1
      RETURN
      END
C
C -----------------------------------------------------------
C
c      SUBROUTINE BLCKX1
      block data BLCKX1
      LOGICAL TRACE
      COMMON /TRXDAT/ TRACE
      INCLUDE 'CTLDT.FOR'
C
      DATA TRACE /.FALSE./
      DATA LBGNPL /.FALSE./
      DATA PLTLVL /0/
      DATA SECRTY / -999 /
      DATA SCLUNI / 1.0 /
      DATA LTTCRT /.FALSE./
      END
C
C -----------------------------------------------------------
C
      SUBROUTINE LEVERR(SUBNAM,LVLS)
C     ***ROUTINE TO REPORT ROUTINE CALLED FROM INCORRECT LEVEL
      CHARACTER*(*) SUBNAM, LVLS
      INTEGER LVLLEN
      CHARACTER*15 LEVCPY
      INCLUDE 'CTLDT.FOR'
C
      LEVCPY='      '
      LVLLEN=ISTRLN(LVLS,15)
      LEVCPY=LVLS(1:LVLLEN)
      WRITE(6,100) SUBNAM,PLTLVL,LEVCPY
 100      FORMAT(' Error: ',1A6,' called from level ',I2,
     $           ', must be called from level ',1A15)
      STOP
      END
C
C -------------------------------------------------------------
c The following is Mike Porters extensions necessary for KRAKEN
c -------------------------------------------------------------
c
      SUBROUTINE ROUNDER( XMIN, XMAX, XSTEP )

C     COMPUTE ROUNDED VALUES FOR CALL TO AXIS
C     (AND NO IT DOESN'T WORK VERY WELL)

      INTEGER NINTVL(0:9)
      DATA NINTVL / 5, 4, 4, 6, 4, 5, 6, 7, 4, 6 /

      DELX = XMAX - XMIN
      NSIG = LOG10( MAX( ABS(XMIN/DELX), ABS(XMAX/DELX) ) ) + 1
      IF ( XMIN .LT. XMAX ) THEN
         XMIN = RNDDN( XMIN, NSIG )
         XMAX = RNDUP( XMAX, NSIG )
      ELSE
         XMIN = RNDUP( XMIN, NSIG )
         XMAX = RNDDN( XMAX, NSIG )
      ENDIF

C     SELECT NUMBER OF TICKS BASED ON DELX

      DELX = ABS( ROUND( XMAX-XMIN, 1 ) )
C     ------ TAKE THE FIRST DIGIT OF DELX
      SCALE = 10.0** FLOAT(INT( -LOG10( ABS(DELX) ) ))
      IF ( ABS(DELX) .LT. 1.0 ) SCALE = 10.0*SCALE
      XSTEP = (XMAX-XMIN)/NINTVL( MOD(NINT( SCALE*DELX ), 10) )
C      PRINT *,delx,scale,nint(scale*delx),xstep

      RETURN
      END

      FUNCTION ROUND( X, IDIG )

C     IDIG: ROUND X TO IDIG DIGITS
C     THRESH: (MACHINE DEPENDENT) = 1/(LARGEST INTEGER)

      PARAMETER ( THRESH = 1.0E-10 )

      IF ( ABS( X ) .LT. THRESH ) THEN
         ROUND = 0.0
      ELSE IF ( ABS( X ) .GT. 1.0/THRESH ) THEN
         ROUND = X
      ELSE
         ITENS = NINT( -LOG10( ABS(X) ) + IDIG )
         ROUND = FLOAT( NINT( X*10.0**ITENS ) ) * 10.0**(-ITENS)
      ENDIF
C      PRINT *,'ROUND', X, ROUND

      RETURN
      END

      FUNCTION RNDDN( X, IDIG )

C     IDIG: ROUND DOWN X TO IDIG DIGITS
C     THRESH: (MACHINE DEPENDENT) = 1/(LARGEST INTEGER)

      PARAMETER ( THRESH = 1.0E-10 )

      IF ( ABS( X ) .LT. THRESH ) THEN
C        ------ X REAL SMALL
         RNDDN = 0.0
      ELSE IF ( ABS( X ) .GT. 1.0/THRESH ) THEN
C        ------ X REAL BIG
         RNDDN = X
      ELSE IF ( X .GT. 0.0 ) THEN
C        ------ X POSITIVE
         ITENS = NINT( -LOG10( ABS(X) ) + IDIG )
         RNDDN = FLOAT( INT( X*10.0**ITENS + 0.0001 ) ) * 10.0**(-ITENS)
      ELSE
C        ------ X NEGATIVE
         ITENS = NINT( -LOG10( ABS(X) ) + IDIG )
         RNDDN = FLOAT( INT( X*10.0**ITENS - 0.9999 ) ) * 10.0**(-ITENS)
      ENDIF
C      PRINT *,'RNDDN', X, RNDDN

      RETURN
      END

      FUNCTION RNDUP( X, IDIG )

C     IDIG: ROUND UP X TO IDIG DIGITS
C     THRESH: (MACHINE DEPENDENT) = 1/(LARGEST INTEGER)

      PARAMETER ( THRESH = 1.0E-10 )

      IF ( ABS( X ) .LT. THRESH ) THEN
C        ------ X REAL SMALL
         RNDUP = 0.0
      ELSE IF ( ABS( X ) .GT. 1.0/THRESH ) THEN
C        ------ X REAL BIG
         RNDUP = X
      ELSE IF ( X .GT. 0.0 ) THEN
C        ------ X POSITIVE
         ITENS = NINT( -LOG10( ABS(X) ) + IDIG )
         RNDUP = FLOAT( INT( X*10.0**ITENS + 0.9999 ) ) * 10.0**(-ITENS)
      ELSE
C        ------ X NEGATIVE
         ITENS = NINT( -LOG10( ABS(X) ) + IDIG )
         RNDUP = FLOAT( INT( X*10.0**ITENS - 0.0001 ) ) * 10.0**(-ITENS)
      ENDIF
C      PRINT *,'RNDUP', X, RNDUP

      RETURN
      END

      SUBROUTINE T4010

C     PUTS A VT340 TERMINAL IN 4010 EMULATION MODE

      CHARACTER T40*9, ESC*1
      ESC = char(27)
      T40 = '[?38h'
      PRINT *, ESC//T40

      RETURN
      END

      SUBROUTINE VT340

C     RESETS THE TERMINAL

      CHARACTER VT*9, ESC*1
      esc=char(27)
      VT = '[?38l'
      PRINT *, ESC//VT

      RETURN
      END

      SUBROUTINE REALNO( X, I, A, B)

C     EMULATES DISSPLA REALNO FUNCTION

      CHARACTER LMESS*80,ffmt*32
      
C     ENCODE

      J = LOG10( MAX( ABS(X), 1.0 ) ) + 2
      IF ( I .GE. 0 ) THEN
C        ------ FLOATING POINT
         call fmtsub2(ffmt,'F',(i+j),i)
         WRITE( LMESS, ffmt ) X
      ELSE
C        ------ EXPONENT FORM
         call fmtsub2(ffmt,'E',(i+7),i)
         WRITE( LMESS, ffmt ) X
      ENDIF

C     PRINT

      IMESS = I+7
      CALL MESSAG( LMESS, IMESS, A, B )

      RETURN
      END

      SUBROUTINE INTNO( INUM, A, B)

C     EMULATES DISSPLA INTNO FUNCTION

      CHARACTER LMESS*80,ffmt*32

C     ENCODE

      J = LOG10( MAX( ABS(REAL(INUM)), 1.0 ) ) + 1
      call fmtsub1(ffmt,'I',j)
      WRITE( LMESS, ffmt ) INUM

C     PRINT

      IMESS = J
      CALL MESSAG( LMESS, IMESS, A, B )

      RETURN
      END

      SUBROUTINE RLREAL( X, I, A, B)

C     EMULATES DISSPLA RLREAL FUNCTION

      CHARACTER LMESS*80,ffmt*32

C     ENCODE

      J = LOG10( MAX( ABS(X), 1.0 ) ) + 2
      IF ( I .GE. 0 ) THEN
C        ------ FLOATING POINT
         call fmtsub2(ffmt,'F',(i+j),i)
         WRITE( LMESS, ffmt ) X
      ELSE
C        ------ EXPONENT FORM
         call fmtsub2(ffmt,'E',(i+7),i)
         WRITE( LMESS, ffmt ) X
      ENDIF

C     PRINT

      IMESS = I+7
      CALL RLMESS( LMESS, IMESS, A, B )

      RETURN
      END

      SUBROUTINE RLINT( INUM, A, B)

C     EMULATES DISSPLA INTNO FUNCTION

      CHARACTER LMESS*80,ffmt*32

C     ENCODE

      J = LOG10( MAX( ABS(REAL(INUM)), 1.0 ) ) + 1
      call fmtsub1(ffmt,'I',j)
      WRITE( LMESS, ffmt ) INUM

C     PRINT

      IMESS = J
      CALL RLMESS( LMESS, IMESS, A, B )

      RETURN
      END

      SUBROUTINE AREA2D( XLEN, YLEN )

C     Routine to define plot size

      CALL TITLE( ' $', 0, ' $', -100, ' $', -100, XLEN, YLEN )

      RETURN
      END

      SUBROUTINE COMPRS

C     COPPER PENNY FUSE

      DATA IPLOT / 1 /
      SAVE IPLOT

      CALL BGNPL( IPLOT )
      IPLOT = IPLOT + 1

      RETURN
      END

      SUBROUTINE UNITS( SCALE )

C     EMULATES DISPLA UNITS ROUTINE FOR SELECTING METRIC OR US

      CHARACTER SCALE*4

      IF ( SCALE .EQ. 'CENT' .OR. SCALE(1:2) .EQ. 'CM' ) CALL METRIC
C      IF ( SCALE .EQ. 'INCH' .OR. SCALE(1:2) .EQ. 'IN' ) CALL INCH

      RETURN
      END

      SUBROUTINE SETCLR( LCOLOR )

C     EMULATES DISPLA SETCLR ROUTINE FOR SELECTING COLORS

      CHARACTER LCOLOR*8

      IF ( LCOLOR(1:5) .EQ. 'BLACK' ) CALL SETCOL( 1 )
      IF ( LCOLOR(1:3) .EQ. 'RED' ) CALL SETCOL( 2 )
      IF ( LCOLOR(1:5) .EQ. 'GREEN' ) CALL SETCOL( 3 )
      IF ( LCOLOR(1:4) .EQ. 'BLUE' ) CALL SETCOL( 4 )
      IF ( LCOLOR(1:4) .EQ. 'CYAN' ) CALL SETCOL( 5 )
      IF ( LCOLOR(1:7) .EQ. 'MAGENTA' ) CALL SETCOL( 6 )
      IF ( LCOLOR(1:6) .EQ. 'YELLOW' ) CALL SETCOL( 7 )
C     ------ NON-STANDARDS
      IF ( LCOLOR(1:5) .EQ. 'BROWN' ) CALL SETCOL( 8 )
      IF ( LCOLOR(1:7) .EQ. 'LTGREEN' ) CALL SETCOL( 9 )
      IF ( LCOLOR(1:7) .EQ. 'MARINE' ) CALL SETCOL( 10 )
      IF ( LCOLOR(1:5) .EQ. 'BLUE2' ) CALL SETCOL( 11 )

      RETURN
      END

      SUBROUTINE GRACE( X )

C     Set the grace value for plot clipping

      COMMON /PARAMS/ GRACES

      GRACES = X

      RETURN
      END

      SUBROUTINE SHDCHR( ANGRAY, NANGS, GAPRAY, NGAPS )

C     COPPER PENNY FUSE

      PRINT *,'Unimplemented routine called: SHDCHR'

      RETURN
      END

      SUBROUTINE HWSCAL( IPSCAL )

C     COPPER PENNY FUSE

      PRINT *,'Unimplemented routine called: HWSCAL'

      RETURN
      END

      SUBROUTINE XNONUM

C     COPPER PENNY FUSE

      PRINT *,'Unimplemented routine called: XNONUM'

      RETURN
      END

      SUBROUTINE YNONUM

C     COPPER PENNY FUSE

      PRINT *,'Unimplemented routine called: YNONUM'

      RETURN
      END

      SUBROUTINE ZNONUM

C     COPPER PENNY FUSE

      PRINT *,'Unimplemented routine called: ZNONUM'

      RETURN
      END

      FUNCTION XINT( INUM )

C     COPPER PENNY FUSE

      XINT = 0.2
      PRINT *,'Unimplemented routine called: XINT'

      RETURN
      END

      FUNCTION XREAL( ANUM, IPLACE )

C     COPPER PENNY FUSE

      XREAL = 0.2
      PRINT *,'Unimplemented routine called: XREAL'

      RETURN
      END

      FUNCTION XPOSN( XVAL, YVAL )

C     COPPER PENNY FUSE

      XPOSN = 0.0
      PRINT *,'Unimplemented routine called: XPOSN'

      RETURN
      END

      FUNCTION YPOSN( XVAL, YVAL )

C     COPPER PENNY FUSE

      YPOSN = 0.0
      PRINT *,'Unimplemented routine called: YPOSN'

      RETURN
      END

      SUBROUTINE THKRND( FACTOR )

C     COPPER PENNY FUSE

      RETURN
      END

      SUBROUTINE BSCALE( XTIMES, YTIMES )

C     COPPER PENNY FUSE

      RETURN
      END

      SUBROUTINE BLPOLY( XARAY, YARAY, NPTS, FRM )

C     COPPER PENNY FUSE

      REAL XARAY(*), YARAY(*)

      RETURN
      END

      SUBROUTINE BLREC( XORG, YORG, WIDE, HIGH, FRM )

C     ROUTINE TO BLANK OUT A RECTANGULAR AREA

      XLEFT = XORG
      XRIGHT = XORG + WIDE
      YLOWER = YORG
      YUPPER = YORG + HIGH
      IFRAME = FRM
      CALL BLNK1( XLEFT, XRIGHT, YLOWER, YUPPER, IFRAME )

      RETURN
      END

      SUBROUTINE SWISSM

C     SELECTS SWISSM FONT

      RETURN
      END

      SUBROUTINE ZUSE( LSTRGZ, ISTRGZ )

C     COPPER PENNY FUSE

      CHARACTER*40 LSTRGZ
      RETURN
      END

      SUBROUTINE Z1USE( LSTRGZ, ISTRGZ )

C     COPPER PENNY FUSE

      CHARACTER*40 LSTRGZ
      RETURN
      END

      SUBROUTINE Z2USE( LSTRGZ, ISTRGZ )

C     COPPER PENNY FUSE

      CHARACTER*40 LSTRGZ
      RETURN
      END

      SUBROUTINE Z3USE( LSTRGZ, ISTRGZ )

C     COPPER PENNY FUSE

      CHARACTER*40 LSTRGZ
      RETURN
      END

      SUBROUTINE Z4USE( LSTRGZ, ISTRGZ )

C     COPPER PENNY FUSE

      CHARACTER*40 LSTRGZ
      RETURN
      END

      SUBROUTINE Z5USE( LSTRGZ, ISTRGZ )

C     COPPER PENNY FUSE

      CHARACTER*40 LSTRGZ
      RETURN
      END

      SUBROUTINE Z6USE( LSTRGZ, ISTRGZ )

C     COPPER PENNY FUSE

      CHARACTER*40 LSTRGZ
      RETURN
      END

      SUBROUTINE HWROT( IPLROT )

C     COPPER PENNY FUSE

      CHARACTER*40 IPLROT
      RETURN
      END

      SUBROUTINE POPNAM( LSTRING, LEN )

C     COPPER PENNY FUSE

      CHARACTER LSTRING*80
      PRINT *,'Unimplemented routine called: POPNAM'

      RETURN
      END



      SUBROUTINE XNAME( LXNAME,IXNAME )

C     ***ROUTINE TO SPECIFY X-AXIS LABEL

      IMPLICIT NONE
      CHARACTER*(*) LXNAME
      INTEGER IXNAME
      INTEGER ISLEN, ISTRLN
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'NAMDT.FOR'
      INCLUDE 'SIZDT.FOR'

      ISLEN=IABS(IXNAME)
      IF (ISLEN.EQ.100) ISLEN=ISTRLN(LXNAME,72)

      XTIKS=1
      IF (IXNAME.EQ.0) XTIKS=0
      IF (IXNAME.LT.0) XTIKS=2
      PLXLAB=LXNAME
      PLXLBL=ISLEN

      RETURN
      END

      SUBROUTINE YNAME( LYNAME,IYNAME )

C     ***ROUTINE TO SPECIFY Y-AXIS LABEL

      IMPLICIT NONE
      CHARACTER*(*) LYNAME
      INTEGER IYNAME
      INTEGER ISLEN, ISTRLN
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'NAMDT.FOR'
      INCLUDE 'SIZDT.FOR'

      ISLEN=IABS(IYNAME)
      IF (ISLEN.EQ.100) ISLEN=ISTRLN(LYNAME,72)

      YTIKS=1
      IF (IYNAME.EQ.0) YTIKS=0
      IF (IYNAME.LT.0) YTIKS=2
      PLYLAB=LYNAME
      PLYLBL=ISLEN

      RETURN
      END
c
c     supplement subroutines for g77  
c
      subroutine fmtsub1(a,b,m)
      character*(*) a,b
      character*256  tmp
      integer m,i,ii,la
      la=len(a)
      do i=1,256
         tmp(i:i)=char(32)
      end do
      write(tmp,1000) b(1:1),m
 1000 format('(',a,i10,')')
      ii=0
      do i=1,256
         if (tmp(i:i).ne.char(32)) then
            ii=ii+1
            if(ii.gt.la) stop '**** fmtsub1 error ****'
            a(ii:ii)=tmp(i:i)
         end if
      end do
      if(ii.lt.la) then
         do i=ii+1,la
            a(i:i)=char(0)
         end do
      endif
      return
      end
c
      subroutine fmtsub2(a,b,m,n)
      character*(*) a,b
      character*256  tmp
      integer m,n,i,ii,la
      la=len(a)
      do i=1,256
         tmp(i:i)=char(32)
      end do
      write(tmp,1000) b(1:1),m,n
 1000 format('(',a,i10,'.',i10,')')
      ii=0
      do i=1,256
         if (tmp(i:i).ne.char(32)) then
            ii=ii+1
            if(ii.gt.la) stop '**** fmtsub1 error ****'
            a(ii:ii)=tmp(i:i)
         end if
      end do
      if(ii.lt.la) then
         do i=ii+1,la
            a(i:i)=char(0)
         end do
      endif
      return
      end


