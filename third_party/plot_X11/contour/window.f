      SUBROUTINE WINDOW
      parameter (nbot_max = 5000, nbm= nbot_max+20)

      COMMON /BOTT/ XF(nbm), YF(nbm), NPBOTT, ISHADE, NPSH
      COMMON /SH/ PF(440), QF(440), UF(440), VF(440)
      COMMON /XAX/X1,XL,XLEFT,XRIGHT,XSCALE,XINC,DX,
     & X1PL,XLPL,NX,X1GRID,XLGRID,DIVX,XVAL(100),NXVAL
      COMMON /YAX/Y1,YL,YUP,YDOWN,YSCALE,YINC,DY,
     & Y1PL,YLPL,NY,Y1GRID,YLGRID,DIVY,YVAL(100),NYVAL

      NPSH=0
      IF(NPBOTT.LT.2) RETURN

C   CHECKING DATA AGAINST PLOT COORDINATES

      XMIN=MIN(XLEFT,XRIGHT)
      XMAX=MAX(XLEFT,XRIGHT)
      YMIN=MIN(YUP,YDOWN)
      YMAX=MAX(YUP,YDOWN)

C   "X" COORDINATES

      JJ=0
      DO 3000   I=1,NPBOTT-1

CASE WITH POINT LESS THAN XMIN
      IF(XF(I) .LT. XMIN)   THEN

       IF(XF(I+1) .GT. XMIN)   THEN
        JJ=JJ+1
        QF(JJ)=YF(I)+(YF(I+1)-YF(I))*(XMIN-XF(I))/(XF(I+1)-XF(I))
        PF(JJ)=XMIN
        IF(XF(I+1) .GT. XMAX)   THEN
         JJ=JJ+1
         QF(JJ)=YF(I)+(YF(I+1)-YF(I))*(XMAX-XF(I))/(XF(I+1)-XF(I))
         PF(JJ)=XMAX
        END IF
       END IF

CASE WITH POINT GREATER THAN XMAX
      ELSE IF(XF(I) .GT. XMAX)   THEN
       IF(XF(I+1) .LT. XMAX)   THEN
        JJ=JJ+1
        QF(JJ)=YF(I+1)+(YF(I)-YF(I+1))*(XMAX-XF(I+1))/(XF(I)-XF(I+1))
        PF(JJ)=XMAX
        IF(XF(I+1) .LT. XMIN)   THEN
         JJ=JJ+1
         QF(JJ)=YF(I+1)+(YF(I)-YF(I+1))*(XMIN-XF(I+1))/(XF(I)-XF(I+1))
         PF(JJ)=XMIN
        END IF
       END IF

CASE WITH POINT INSIDE OR ON BORDERS
      ELSE
       JJ=JJ+1
       PF(JJ)=XF(I)
       QF(JJ)=YF(I)

       IF( (XF(I+1) .LT. XMIN) .AND. (XF(I) .NE. XMIN) )   THEN
        JJ=JJ+1
        QF(JJ)=YF(I+1)+(YF(I)-YF(I+1))*(XMIN-XF(I+1))/(XF(I)-XF(I+1))
        PF(JJ)=XMIN
       ELSE IF( (XF(I+1) .GT. XMAX) .AND. (XF(I) .NE. XMAX) )   THEN
        JJ=JJ+1
        QF(JJ)=YF(I)+(YF(I+1)-YF(I))*(XMAX-XF(I))/(XF(I+1)-XF(I))
        PF(JJ)=XMAX
       END IF
      END IF
 3000 CONTINUE

      IF((XF(NPBOTT).LE.XMAX) .AND. (XF(NPBOTT).GE.XMIN))   THEN
       JJ=JJ+1
       PF(JJ)=XF(NPBOTT)
       QF(JJ)=YF(NPBOTT)
      END IF

C   "Y" COORDINATES
      NPSH=0
      DO 5000   I=1,JJ-1

      IF(QF(I) .LT. YMIN)   THEN
       IF(QF(I+1) .GT. YMIN)   THEN
        NPSH=NPSH+1
        UF(NPSH)=PF(I)+(PF(I+1)-PF(I))*(YMIN-QF(I))/(QF(I+1)-QF(I))
        VF(NPSH)=YMIN
        IF(QF(I+1) .GT. YMAX)   THEN
         NPSH=NPSH+1
         UF(NPSH)=PF(I)+(PF(I+1)-PF(I))*(YMAX-QF(I))/(QF(I+1)-QF(I))
         VF(NPSH)=YMAX
        END IF
       END IF

      ELSE IF(QF(I) .GT. YMAX)   THEN
       IF(QF(I+1) .LT. YMAX)   THEN
        NPSH=NPSH+1
        UF(NPSH)=PF(I+1)+(PF(I)-PF(I+1))*(YMAX-QF(I+1))/(QF(I)-QF(I+1))
        VF(NPSH)=YMAX
        IF(QF(I+1) .LT. YMIN)   THEN
         NPSH=NPSH+1
         UF(NPSH)=PF(I+1)+(PF(I)-PF(I+1))*(YMIN-QF(I+1))/(QF(I)-QF(I+1))
         VF(NPSH)=YMIN
        END IF
       END IF

      ELSE
       NPSH=NPSH+1
       VF(NPSH)=QF(I)
       UF(NPSH)=PF(I)
       IF( (QF(I+1) .LT. YMIN) .AND. (QF(I) .NE. YMIN) )  THEN
        NPSH=NPSH+1
        UF(NPSH)=PF(I+1)+(PF(I)-PF(I+1))*(YMIN-QF(I+1))/(QF(I)-QF(I+1))
        VF(NPSH)=YMIN
       ELSE IF( (QF(I+1) .GT. YMAX) .AND. (VF(I) .NE. YMAX) )  THEN
        NPSH=NPSH+1
        UF(NPSH)=PF(I)+(PF(I+1)-PF(I))*(YMAX-QF(I))/(QF(I+1)-QF(I))
        VF(NPSH)=YMAX
       END IF
      END IF
 5000 CONTINUE

      IF( (QF(JJ) .LE. YMAX) .AND. (QF(JJ) .GE. YMIN) )   THEN
       NPSH=NPSH+1
       UF(NPSH)=PF(JJ)
       VF(NPSH)=QF(JJ)
      END IF
C

      RETURN
      END
