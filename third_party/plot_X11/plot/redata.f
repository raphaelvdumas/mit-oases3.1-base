      SUBROUTINE REDATA(N,NC,IC,INDEX,XMIN,YMIN,FMIN,FMAX,MARK,
     % RDSYMB,AX,AY,LUPLP,LUPLT)
C

      INTEGER RDSYMB

      CHARACTER*12 OPTS
      CHARACTER*80 HTXT,BUFF

      REAL BUFFIN(3)
      REAL AX(1),AY(1)

      COMMON /SCALES/ YSCALETMP, DRRATIO, YOLDLEN,
     &  SCFAC, HSCFAC, IHSC, FACTOR, FACT, FACSCALE, SCALEFACT

      COMMON /SPOPT/ ISEG, ISUM, ITRUNC

      include 'default.f'

      COMMON /SETARG/ TRACD, TRCMAX

      COMMON/XAX/X1,XL,XLEFT,XRIGHT,XSCALE,XINC,DX,
     %  X1PL,XDUM4,IXDUM1,XDUM5,XDUM6,XDIV,XVAL(100),NXVAL
      COMMON/YAX/Y1,YL,YUP,YDOWN,YSCALE,YINC,DY,
     %  Y1PL,YDUM4,IYDUM1,YDUM5,YDUM6,YDIV,YVAL(100),NYVAL,
     %  YBOX


  130 FORMAT(6(1X,E12.5))
  140 FORMAT(4X,F7.1)
  150 FORMAT(A80)
  160 FORMAT(1X,A80)
  170 FORMAT(1X,A20)
  180 FORMAT(1X,A3)
  230 FORMAT(4X,F7.1)
  240 FORMAT(6(1X,E12.5))


      
      READ(LUPLP,150) BUFF 
      CALL RFFORM(BUFF,80,BUFFIN,1,3,ERROR)
      N=NINT(BUFFIN(1))

      IF(NOP .EQ. 0)   THEN

C       N < 0 : ONLY MARKERS PLOTTED
        IF (N.LT.0) THEN
          RDSYMB= 1
          MARK=NINT(BUFFIN(3))
          MARKABS=MOD(ABS(MARK),15)
          MARK=SIGN(MARKABS,MARK)
          N=ABS(N)
        ELSE
          RDSYMB= 0
          IF(ISYMB .GT. 0)   THEN
C            MARK= ISYMB
             MARK= IMARK
            ISYMB=ISYMB+1
          ELSE
             MARK=15
          END IF
        END IF
        NSPP=NINT(BUFFIN(2))
        INDEX=NINT(BUFFIN(3))

      ELSE
        N=ABS(N)

      END IF

      READ(LUPLP,*) XMIN
      READ(LUPLP,*) DX
      READ(LUPLP,*) YMIN
      READ(LUPLP,*) DY
      XMIN=XMIN*XDIV
      DX=DX*XDIV
      YMIN=YMIN*YDIV
      DY=DY*YDIV

      IF(HTXT.EQ.'SOURCE SPECTRUM') THEN
        READ(LUPLP,*)FMIN
        READ(LUPLP,*)FMAX
        FMIN=FMIN*XDIV
        FMAX=FMAX*XDIV
      END IF
C
C    READING OF DATA FILE FROM UNIT LUPLT=20 IF "DX" AND/OR "DY" EQUAL
C    TO ZERO; OTHERWISE DATA IS COMPUTED INTERNALLY TO THE PROGRAM
C
      IF (DX.EQ.0) THEN
        READ(LUPLT,*) (AX(J),J=1,N)
        DO 2000 J=1,N
        AX(J)=(AX(J)*xdiv*factor+XMIN)*FACSCALE
 2000   CONTINUE
        xoffset(ic)=xmin*facscale
      ELSE
        DO 2200 J=1,N
 2200   AX(J)=XMIN+(J-1)*DX
        xoffset(ic)=xmin
      END IF
      IF (DY.EQ.0) THEN
        READ(LUPLT,*) (AY(J),J=1,N)
        IF(OPTS(8:12) .EQ. 'ANGLE')   GO TO 2600
        IF (NOP .EQ. 0) THEN
          DO 2400 J=1,N
          AY(J)=AY(J)*YDIV*FACTOR*FACSCALE
 2400     CONTINUE

C         PREPARE FOR SEISPACK PLOT

          WRITE(30) (AY(J),J=1,N)
          IF (ITRUNC.EQ.1) THEN
            CALL VCLIP(AY,1,-TRCMAX,TRCMAX,AY,1,N)
          END IF
          IF (NC.GT.1) THEN
            IF (IC.EQ.1) YOLD=YMIN
            IF (IC.EQ.1) THEN
              NSP=N 
              TRACD=abs(YUP-YDOWN)/(NC-1)
            END IF
          ELSE
            NSP=N
            TRACD=abs(YUP-YDOWN)
          END IF
 2600     CONTINUE
C
C         ADD OFFSET 'YMIN' FOR DISSPLA PLOT
C
          DO 2800 J=1,N
 2800     AY(J)=AY(J)+YMIN*facscale
          yoffset(ic)=ymin*facscale
        END IF
      ELSE
        DO 3000 J=1,N
 3000   AY(J)=YMIN+(J-1)*DY
        yoffset(ic)=ymin
      END IF
      RETURN
      END
