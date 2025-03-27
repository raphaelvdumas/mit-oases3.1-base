C
C     ***MINDIS - SACLANTCEN GRAPH PLOTTING PACKAGE
C
C     ***JOHN STEIERT   -   10-DEC-81
C
C -----------------------------------------------------
C
C     ***AMMENDMENTS:
C
C      JS   7-JUN-83   Standard Fortran version
C      JS  16-Mar-84   New routines UNXMOV, UNXDRW and UNXCVT.
C      JS   5-Jan-84   New routines: CAXTOP, INXCVT
C      JS  23-May-86   Bug fix: UNXCVT didn't handle Polars right
C      JS  27-May-86   Bug fix: INXCVT didn't handle Polars right
C
C -----------------------------------------------------
C
C     ***ROUTINES FOR HANDLING POLAR CO-ORDINATES
C
      SUBROUTINE POXTOC(R,THETA,X,Y)
C     ***ROUTINE TO CONVERT POLAR COORDINATE POINT TO CARTESIAN
      INCLUDE 'POLDT.FOR'
      X=POLXDI+R*COS(THETA)
      Y=POLYDI+R*SIN(THETA)
      RETURN
      END
C
      SUBROUTINE CAXTOP(X,Y,R,THETA)
C     ***Routine to convert cartesian coordinate point to polar
      INCLUDE 'POLDT.FOR'
      INCLUDE 'CTLDT.FOR'
C
      R=SQRT(ABS((X-POLXDI)**2.0+(Y-POLYDI)**2.0))
      IF ((X-POLXDI).EQ.0.0) THEN
           TH1=RPI/2.0
      ELSE
           TH1=ATAN(ABS((Y-POLYDI)/(X-POLXDI)))
           ENDIF
      IF ((X-POLXDI).LT.0.0 .AND. (Y-POLYDI).GE.0.0) TH1=(RPI-TH1)
      IF ((X-POLXDI).LT.0.0 .AND. (Y-POLYDI).LT.0.0) TH1=(RPI+TH1)
      IF ((X-POLXDI).GE.0.0 .AND. (Y-POLYDI).LT.0.0) TH1=(2*RPI)-TH1
      THETA=TH1
      RETURN
      END
C
      REAL FUNCTION POXRAD(DEGS)
C     ***ROUTINE TO CONVERT DEGREES TO RADIANS
      POXRAD=(DEGS/180.)*ACOS(0.0)*2.0
      RETURN
      END
C
      SUBROUTINE POXMOV(R,THETA)
C     ***ROUTINE TO MOVE CURSOR TO SCREEN POSITION (R,THETA)
      CALL POXTOC(R,THETA,X,Y)
      CALL INXMOV(X,Y)
      RETURN
      END
C
      SUBROUTINE POXDRW(R,THETA)
C     ***ROUTINE TO DRAW LINE TO SCREEN POSITION (R,THETA)
      CALL POXTOC(R,THETA,X,Y)
      CALL INXDRW(X,Y)
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE UNXMOV(X,Y,PATTRN)
C     ***Routine to move to point (X,Y) in user units.
      REAL X,Y
      LOGICAL PATTRN
C
      IFLAG=1
      GOTO 1
C
      ENTRY UNXDRW(X,Y,PATTRN)
C     ***Routine to draw to point (X,Y) in user units.
      IFLAG=2
 1    CONTINUE
      CALL UNXCVT(X,Y,X1,Y1)
      IF (PATTRN) THEN
           IF (IFLAG.EQ.1) THEN
                CALL INXMOV(X1,Y1)
           ELSE
                CALL INXDRW(X1,Y1)
                ENDIF
      ELSE
           IF (IFLAG.EQ.1) THEN
                CALL BLXMOV(X1,Y1)
           ELSE
                CALL BLXDRW(X1,Y1)
                ENDIF
           ENDIF
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE UNXCVT(X,Y,X1,Y1)
C     ***Routine to convert from user units to inches.
      INCLUDE 'POLDT.FOR'
      INCLUDE 'CARTD.FOR'
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'SIZDT.FOR'
C
      IF (POLARS) THEN
           RVAL=(Y-POLORV)/POLSTP
           THET=X*POLTHE
           CALL POXTOC(RVAL,THET,X1,Y1)
      ELSE
           XFACT=CTXPST/CTXSTP
           YFACT=CTYPST/CTYSTP
           X1=(X-CTXORG)*XFACT
           Y1=(Y-CTYORG)*YFACT
           ENDIF
      RETURN
      END
C
C -----------------------------------------------------
C
      SUBROUTINE INXCVT(X,Y,X1,Y1)
C     ***Routine to convert from inches to user units
      INCLUDE 'POLDT.FOR'
      INCLUDE 'CARTD.FOR'
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'SIZDT.FOR'
      IF (POLARS) THEN
           CALL CAXTOP(X,Y,RVAL,THET)
           Y1=(RVAL*POLSTP)+POLORV
           X1=THET/POLTHE
      ELSE
           XFACT=CTXPST/CTXSTP
           YFACT=CTYPST/CTYSTP
           X1=(X/XFACT)+CTXORG
           Y1=(Y/YFACT)+CTYORG
           ENDIF
      RETURN
      END
C
C -----------------------------------------------------
C
C     ***ROUTINES TO HANDLE PATTERNED LINES
C
C     *** NB: X,Y COORDINATES GIVEN IN INCHES
C
      SUBROUTINE INXMOV(X,Y)
C     ***ROUTINE TO MOVE GRAPHICS CURSOR WHILE USING PATTERNED LINES
      COMMON /PATXDT/ ROLDX, ROLDY, USED, IPATNO, SUM, ICURPT
      ROLDX=X
      ROLDY=Y
      USED=0.0
      SUM=0.0
      ICURPT=1
      RETURN
      END
C
      SUBROUTINE PTXSET(IPNO)
C     ***ROUTINE TO SET PATTERN TYPE FOR LINES
      COMMON /PATXDT/ ROLDX, ROLDY, USED, IPATNO, SUM, ICURPT
      IPATNO=IPNO
      USED=0.0
      SUM=0.0
      ICURPT=1
      RETURN
      END
C
      SUBROUTINE INXDRW(X,Y)
C     ***ROUTINE TO DRAW LINE USING PATTERNED LINES
      COMMON /PATXDT/ ROLDX, ROLDY, USED, IPATNO, SUM, ICURPT
      COMMON /PATXD1/ NPARTS(5), RPATNS(10,5), RPATLN(5)
      COMMON /PATXD2/ RMX, RCX, RMY, RCY, RSGNX, RSGNY
C
C     ***FIRST HANDLE CASE OF SOLID LINE
C      WRITE(6,105) ROLDX,ROLDY, X,Y
C 105      FORMAT(' DRAWING FROM ',2E16.9,' TO ',2E16.9)
      IF (IPATNO.EQ.0) THEN
           CALL BLXMOV(ROLDX,ROLDY)
           CALL BLXDRW(X,Y)
           ROLDX=X
           ROLDY=Y
           RETURN
           ENDIF
C
C     ***INITIALISE FOR PATTERNED LINE
      RLINLT=((X-ROLDX)**2+(Y-ROLDY)**2)**0.5
      TODRAW=RLINLT
      IF (ROLDX-X.NE.0) RMX=(ROLDY-Y)/(ROLDX-X)
      IF (ROLDX-X.EQ.0) RMX=1.0
      RCX=Y-(RMX*X)
      IF (ROLDY-Y.NE.0) RMY=(ROLDX-X)/(ROLDY-Y)
      IF (ROLDY-Y.EQ.0) RMY=1.0
      RCY=X-(RMY*Y)
      RSGNX=SIGN(1.0,X-ROLDX)
      RSGNY=SIGN(1.0,Y-ROLDY)
C      WRITE(6,100) RMX,RCX,RMY,RCY,RSGNX,RSGNY
C 100      FORMAT( ' RMX,RCX = ',2F9.3,' RMY,RCY = ',2F9.3,
C     #            'RSGNX,RSGNY = ',2F9.3)
C
 1    CONTINUE
      DIFF=USED-SUM
      RLEFT=RPATNS(ICURPT,IPATNO)-DIFF
      IF (TODRAW.GT.RLEFT) THEN
           CALL PHXDRW(RLEFT)
           SUM=SUM+RPATNS(ICURPT,IPATNO)
           USED=USED+RLEFT
           TODRAW=TODRAW-RLEFT
           ICURPT=ICURPT+1
           IF (ICURPT.GT.NPARTS(IPATNO)) THEN
                ICURPT=1
                USED=0.0
                SUM=0.0
                ENDIF
           GOTO 1
      ELSE
C          ***END OF LINE TO BE DRAWN FALLS IN CURRENT PART
           CALL PHXDRW(TODRAW)
           USED=USED+TODRAW
           ROLDX=X
           ROLDY=Y
           RETURN
           ENDIF
      END
C
      SUBROUTINE PHXDRW(RLEN)
C     ***ROUTINE TO PUT IN NEXT PART OF LINE
      LOGICAL LDRAW
      COMMON /PATXDT/ ROLDX, ROLDY, USED, IPATNO, SUM, ICURPT
      COMMON /PATXD2/ RMX, RCX, RMY, RCY, RSGNX, RSGNY
      LDRAW=((ICURPT-((ICURPT/2)*2)).EQ.1)
      X1=ROLDX
      Y1=ROLDY
      CALL BLXMOV(X1,Y1)
      IF (RMX.EQ.0) THEN
C           WRITE(6,100) 1
C 100          FORMAT (' CASE ',I3)
           Y2=RCX
           X2=ROLDX+RSGNX*RLEN
      ELSEIF (RMY.EQ.0) THEN
C           WRITE(6,100) 2
           X2=RCY
           Y2=ROLDY+RSGNY*RLEN
      ELSE
C           WRITE(6,100) 3
           RTHETA=ATAN(RMX)
           X2=ROLDX+RLEN*RSGNX*ABS(COS(RTHETA))
           Y2=ROLDY+RLEN*RSGNY*ABS(SIN(RTHETA))
           ENDIF
      IF (LDRAW) THEN
           CALL BLXDRW(X2,Y2)
      ELSE
           CALL BLXMOV(X2,Y2)
           ENDIF
      ROLDX=X2
      ROLDY=Y2
      RETURN
      END
C
c      SUBROUTINE BLCKX2
      block data BLCKX2
      COMMON /PATXD1/ NPARTS(5), RPATNS(10,5), RPATLN(5)
      DATA NPARTS / 2, 2, 4, 4, 0 /
      DATA RPATLN / 0.06, 0.10, 0.50, 1.00, 0.00 /
      DATA RPATNS / 0.02, 0.04, 0.00, 0.00, 0.00,
     #              0.00, 0.00, 0.00, 0.00, 0.00,
     #              0.06, 0.04, 0.00, 0.00, 0.00,
     #              0.00, 0.00, 0.00, 0.00, 0.00,
     #              0.40, 0.04, 0.02, 0.04, 0.00,
     #              0.00, 0.00, 0.00, 0.00, 0.00,
     #              0.82, 0.06, 0.06, 0.06, 0.00,
     #              0.00, 0.00, 0.00, 0.00, 0.00,
     #              0.00, 0.00, 0.00, 0.00, 0.00,
     #              0.00, 0.00, 0.00, 0.00, 0.00/
      END
C
C -----------------------------------------------------
C
C     ***ROUTINES TO HANDLE BLANKING OF LINES
C
      SUBROUTINE BLXMOV(X,Y)
C     ***ROUTINE TO HANDLE BLANKING ETC WHEN MOVING CURSOR
      REAL OLDX, OLDY
      COMMON /BLXDAT/ OLDX, OLDY
      OLDX=X
      OLDY=Y
      RETURN
      END
C
      SUBROUTINE BLXDRW(X,Y)
C     ***ROUTINE TO HANDLE BLANKING ETC WHEN DRAWING LINE
      REAL OLDX, OLDY
      COMMON /BLXDAT/ OLDX, OLDY
      CALL BLXDR1(OLDX,OLDY,X,Y)
      OLDX=X
      OLDY=Y
      RETURN
      END
C
      SUBROUTINE BLXDR1(X1,Y1,X2,Y2)
C     ***ROUTINE TO DRAW LINE TAKING ACCOUNT OF BLANK AREAS
      REAL XINTSC(8), YINTSC(8)
      INTEGER BLNKNO(8), BLXGTI
      LOGICAL INBOX(4),PNT1IN,PNT2IN, BLXINB
      REAL XPNTS(20), YPNTS(20)
      LOGICAL XYMOVE(20), LREVER
      INCLUDE 'BLNKD.FOR'
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'SIZDT.FOR'
      COMMON /BLNKX1/ X1A,X2A,Y1A,Y2A,RM,RC,RM1,RC1
C
C      WRITE(6,*)'BLXDR1 FROM ',X1,Y1,' TO ',X2,Y2
C     ***INITIALISATION
      IPNTNO=0
      IF (X1.LE.X2) THEN
           LREVER=.FALSE.
           X1A=X1
           X2A=X2
           Y1A=Y1
           Y2A=Y2
      ELSE
           LREVER=.TRUE.
           X1A=X2
           X2A=X1
           Y1A=Y2
           Y2A=Y1
           ENDIF
      IF (X1A-X2A.NE.0) RM=(Y1A-Y2A)/(X1A-X2A)
      IF (X1A-X2A.EQ.0) RM=0.0
      RC=Y1A-(RM*X1A)
      IF (Y1A-Y2A.NE.0) RM1=(X1A-X2A)/(Y1A-Y2A)
      IF (Y1A-Y2A.EQ.0) RM1=0.0
      RC1=X1A-(RM1*Y1A)
C
C     ***MAKE SURE PLOT FITS IN WINDOW
      IF (LWNDOW) CALL BLXWND(X1A,Y1A,X2A,Y2A)
C
C     ***LOOK FOR INTERSECTION POINTS
 15   IXNO=0
      DO 1 I=1,4
           IF (BLNKON(I)) THEN
                PNT1IN=BLXINB(X1A,Y1A,I)
                PNT2IN=BLXINB(X2A,Y2A,I)
                IF (PNT1IN.AND.PNT2IN) RETURN
                IF (PNT1IN) ITMP=BLXGTI(I,X1A,Y1A,1)
                IF (PNT2IN) ITMP=BLXGTI(I,X2A,Y2A,1)
                IF (PNT1IN.OR.PNT2IN) THEN
                     GOTO 15
                ELSE
                     IXX=IXNO+1
                     INOX=BLXGTI(I,XINTSC(IXX),YINTSC(IXX),2)
                     IF (INOX.EQ.2) THEN
                          BLNKNO(IXNO+1)=I
                          BLNKNO(IXNO+2)=I
                          IXNO=IXNO+2
                          ENDIF
                     IF ((INOX.NE.0).AND.(INOX.NE.2)) CALL PLXERR(1)
                     ENDIF
                ENDIF
 1         CONTINUE
C
C     ***SORT INTERSECTIONS INTO MONOTONIC INCREASING ORDER OF X
      DO 2 I=1, IXNO-1
           DO 2 J=I+1, IXNO
                IF ((XINTSC(I).GT.XINTSC(J)).OR.
     #              ((XINTSC(I).EQ.XINTSC(J)).AND.
     #               (YINTSC(I).GT.YINTSC(J)))) THEN
                     RTMP=XINTSC(I)
                     XINTSC(I)=XINTSC(J)
                     XINTSC(J)=RTMP
                     RTMP=YINTSC(I)
                     YINTSC(I)=YINTSC(J)
                     YINTSC(J)=RTMP
                     ITMP=BLNKNO(I)
                     BLNKNO(I)=BLNKNO(J)
                     BLNKNO(J)=ITMP
                     ENDIF
 2              CONTINUE
C
C     ***NOW DRAW LINE
      XSTART=X1A
      YSTART=Y1A
      IXPTR=0
 3    CONTINUE
C      CALL X1XMOV(XSTART,YSTART)
      IPNTNO=IPNTNO+1
      XPNTS(IPNTNO)=XSTART
      YPNTS(IPNTNO)=YSTART
      XYMOVE(IPNTNO)=.TRUE.
      IF (IXPTR.LT.IXNO) THEN
           IXPTR=IXPTR+1
           XEND=XINTSC(IXPTR)
           YEND=YINTSC(IXPTR)
      ELSE
           XEND=X2A
           YEND=Y2A
           ENDIF
C      CALL X1XDRW(XEND,YEND)
      IPNTNO=IPNTNO+1
      XPNTS(IPNTNO)=XEND
      YPNTS(IPNTNO)=YEND
      XYMOVE(IPNTNO)=.FALSE.
      IF (IXPTR.GE.IXNO) GOTO 7
C
C     ***MOVE TO START OF NEXT LINE - I.E. BOX EXIT POINT
      DO 4 I=1,4
 4         INBOX(I)=.FALSE.
      INBOX(BLNKNO(IXPTR))=.TRUE.
 5    CONTINUE
           IXPTR=IXPTR+1
           IF (IXPTR.GT.IXNO) CALL PLXERR(3)
           INBOX(BLNKNO(IXPTR))=.NOT.INBOX(BLNKNO(IXPTR))
           DO 6 I=1,4
 6              IF (INBOX(I)) GOTO 5
C          ***END POINT OF BOX REACHED
           XSTART=XINTSC(IXPTR)
           YSTART=YINTSC(IXPTR)
           GOTO 3
C
C     *** Output points
 7    CONTINUE
      IF (LREVER) THEN
           CALL X1XMOV(XPNTS(IPNTNO),YPNTS(IPNTNO))
           DO 72 I=IPNTNO-1,1,-1
                IF (XYMOVE(I+1)) THEN
                     CALL X1XMOV(XPNTS(I),YPNTS(I))
                ELSE
                     CALL X1XDRW(XPNTS(I),YPNTS(I))
                     ENDIF
 72             CONTINUE
      ELSE
           DO 74 I=1,IPNTNO
                IF (XYMOVE(I)) THEN
                     CALL X1XMOV(XPNTS(I),YPNTS(I))
                ELSE
                     CALL X1XDRW(XPNTS(I),YPNTS(I))
                     ENDIF
 74             CONTINUE
           ENDIF
      RETURN
      END
C
      INTEGER FUNCTION BLXGTI(IBLNO,XARR,YARR,INOV)
C     ***ROUTINE TO FIND INTERSECTIONS BETWEEN A LINE
C     ***AND A BLANK BOX
      REAL XARR(INOV),YARR(INOV)
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'BLNKD.FOR'
      COMMON /BLNKX1/ X1A,X2A,Y1A,Y2A,RM,RC,RM1,RC1
      BLXGTI=0
      IF (Y1A.LT.Y2A) THEN
           YMIN=Y1A
           YMAX=Y2A
      ELSE
           YMIN=Y2A
           YMAX=Y1A
           ENDIF
      XVAL=BLNKX1(IBLNO)
      IF ((XVAL.GE.X1A).AND.(XVAL.LE.X2A)) THEN
      YVAL=BLXCLY(XVAL,RM,RC)
      IF ((YVAL.GE.YMIN).AND.(YVAL.LE.YMAX)) THEN
      IF ((YVAL.GT.BLNKY1(IBLNO)).AND.(YVAL.LT.BLNKY2(IBLNO))) THEN
C          ***STORE INTERSECTION DETAILS
           BLXGTI=BLXGTI+1
           IF (BLXGTI.GT.INOV) CALL PLXERR(2)
           XARR(BLXGTI)=XVAL
           YARR(BLXGTI)=YVAL
           ENDIF
           ENDIF
           ENDIF
      XVAL=BLNKX2(IBLNO)
      IF ((XVAL.GE.X1A).AND.(XVAL.LE.X2A)) THEN
      YVAL=BLXCLY(XVAL,RM,RC)
      IF ((YVAL.GE.YMIN).AND.(YVAL.LE.YMAX)) THEN
      IF ((YVAL.GT.BLNKY1(IBLNO)).AND.(YVAL.LT.BLNKY2(IBLNO))) THEN
C          ***STORE INTERSECTION DETAILS
           BLXGTI=BLXGTI+1
           IF (BLXGTI.GT.INOV) CALL PLXERR(2)
           XARR(BLXGTI)=XVAL
           YARR(BLXGTI)=YVAL
           ENDIF
           ENDIF
           ENDIF
      YVAL=BLNKY1(IBLNO)
      IF ((YVAL.GE.YMIN).AND.(YVAL.LE.YMAX)) THEN
      XVAL=BLXCLX(YVAL,RM1,RC1)
      IF ((XVAL.GE.X1A).AND.(XVAL.LE.X2A)) THEN
      IF ((XVAL.GT.BLNKX1(IBLNO)).AND.(XVAL.LT.BLNKX2(IBLNO))) THEN
C          ***STORE INTERSECTION DETAILS
           BLXGTI=BLXGTI+1
           IF (BLXGTI.GT.INOV) CALL PLXERR(2)
           XARR(BLXGTI)=XVAL
           YARR(BLXGTI)=YVAL
           ENDIF
           ENDIF
           ENDIF
      YVAL=BLNKY2(IBLNO)
      IF ((YVAL.GE.YMIN).AND.(YVAL.LE.YMAX)) THEN
      XVAL=BLXCLX(YVAL,RM1,RC1)
      IF ((XVAL.GE.X1A).AND.(XVAL.LE.X2A)) THEN
      IF ((XVAL.GT.BLNKX1(IBLNO)).AND.(XVAL.LT.BLNKX2(IBLNO))) THEN
C          ***STORE INTERSECTION DETAILS
           BLXGTI=BLXGTI+1
           IF (BLXGTI.GT.INOV) CALL PLXERR(2)
           XARR(BLXGTI)=XVAL
           YARR(BLXGTI)=YVAL
           ENDIF
           ENDIF
           ENDIF
      RETURN
      END
C
      LOGICAL FUNCTION BLXINB(X,Y,IBLNO)
C     ***ROUTINE TO CHECK IF POINT IS INSIDE BLANK BOX
      INCLUDE 'CTLDT.FOR'
      INCLUDE 'BLNKD.FOR'
      COMMON /BLNKX1/ X1A,X2A,Y1A,Y2A,RM,RC,RM1,RC1
      BLXINB=.TRUE.
      IF (((X.GT.BLNKX1(IBLNO)).AND.(X.LT.BLNKX2(IBLNO))).AND.
     #    ((Y.GT.BLNKY1(IBLNO)).AND.(Y.LT.BLNKY2(IBLNO)))) RETURN
      BLXINB=.FALSE.
      RETURN
      END
C
      REAL FUNCTION BLXCLX(Y,RM1,RC1)
C     ***ROUTINE TO GET X VALUE FOR SUPPLIED Y
      BLXCLX=RM1*Y+RC1
      RETURN
      END
C
      REAL FUNCTION BLXCLY(X,RM,RC)
C     ***ROUTINE TO GET Y VALUE FOR SUPPLIED X
      BLXCLY=RM*X+RC
      RETURN
      END
C
      SUBROUTINE BLXWND(X1,Y1,X2,Y2)
C     ***ROUTINE TO CLIP PLOT TO PAGE BOUNDARY
      CALL BLXWD1(X1,Y1)
      CALL BLXWD1(X2,Y2)
      RETURN
      END
C
      SUBROUTINE BLXWD1(X,Y)
C     ***ROUTINE TO MOVE POINT <X,Y> TOWARDS PAGE BOUNDARY
      INCLUDE 'SIZDT.FOR'
      COMMON /BLNKX1/ X1A,X2A,Y1A,Y2A,RM,RC,RM1,RC1
      IF (X.LT.SPLAX1) THEN
           X=SPLAX1
           Y=BLXCLY(X,RM,RC)
      ELSEIF (X.GT.SPLAX2) THEN
           X=SPLAX2
           Y=BLXCLY(X,RM,RC)
           ENDIF
      IF (Y.LT.SPLAY1) THEN
           Y=SPLAY1
           X=BLXCLX(Y,RM1,RC1)
      ELSEIF (Y.GT.SPLAY2) THEN
           Y=SPLAY2
           X=BLXCLX(Y,RM1,RC1)
           ENDIF
      RETURN
      END
