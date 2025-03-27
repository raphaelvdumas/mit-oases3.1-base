      SUBROUTINE SCISSOR(AX,XLEFT,XRIGHT,AY,YUP,YDOWN,
     & NPIN,PF,QF,NPOUT)

      REAL*4 AX(1),AY(1),PF(1),QF(1)

C   CHECKING DATA AGAINST PLOT COORDINATES

      XMIN=MIN(XLEFT,XRIGHT)
      XMAX=MAX(XLEFT,XRIGHT)
      YMIN=MIN(YUP,YDOWN)
      YMAX=MAX(YUP,YDOWN)

C   "X" COORDINATES

      JJ=0
      DO 3000   I=1,NPIN-1

CASE WITH POINT LESS THAN XMIN
      IF(AX(I) .LT. XMIN)   THEN

       IF(AX(I+1) .GT. XMIN)   THEN
        JJ=JJ+1
        QF(JJ)=AY(I)+(AY(I+1)-AY(I))*(XMIN-AX(I))/(AX(I+1)-AX(I))
        PF(JJ)=XMIN
        IF(AX(I+1) .GT. XMAX)   THEN
         JJ=JJ+1
         QF(JJ)=AY(I)+(AY(I+1)-AY(I))*(XMAX-AX(I))/(AX(I+1)-AX(I))
         PF(JJ)=XMAX
        END IF
       END IF

CASE WITH POINT GREATER THAN XMAX
      ELSE IF(AX(I) .GT. XMAX)   THEN
       IF(AX(I+1) .LT. XMAX)   THEN
        JJ=JJ+1
        QF(JJ)=AY(I+1)+(AY(I)-AY(I+1))*(XMAX-AX(I+1))/(AX(I)-AX(I+1))
        PF(JJ)=XMAX
        IF(AX(I+1) .LT. XMIN)   THEN
         JJ=JJ+1
         QF(JJ)=AY(I+1)+(AY(I)-AY(I+1))*(XMIN-AX(I+1))/(AX(I)-AX(I+1))
         PF(JJ)=XMIN
        END IF
       END IF

CASE WITH POINT INSIDE OR ON BORDERS
      ELSE
       JJ=JJ+1
       PF(JJ)=AX(I)
       QF(JJ)=AY(I)

       IF( (AX(I+1) .LT. XMIN) .AND. (AX(I) .NE. XMIN) )   THEN
        JJ=JJ+1
        QF(JJ)=AY(I+1)+(AY(I)-AY(I+1))*(XMIN-AX(I+1))/(AX(I)-AX(I+1))
        PF(JJ)=XMIN
       ELSE IF( (AX(I+1) .GT. XMAX) .AND. (AX(I) .NE. XMAX) )   THEN
        JJ=JJ+1
        QF(JJ)=AY(I)+(AY(I+1)-AY(I))*(XMAX-AX(I))/(AX(I+1)-AX(I))
        PF(JJ)=XMAX
       END IF
      END IF
 3000 CONTINUE

      IF((AX(NPIN).LE.XMAX) .AND. (AX(NPIN).GE.XMIN))   THEN
       JJ=JJ+1
       PF(JJ)=AX(NPIN)
       QF(JJ)=AY(NPIN)
      END IF

C   "Y" COORDINATES
      NPOUT=0
      DO 5000   I=1,JJ-1

      IF(QF(I) .LT. YMIN)   THEN
       IF(QF(I+1) .GT. YMIN)   THEN
        NPOUT=NPOUT+1
        AX(NPOUT)=PF(I)+(PF(I+1)-PF(I))*(YMIN-QF(I))/(QF(I+1)-QF(I))
        AY(NPOUT)=YMIN
        IF(QF(I+1) .GT. YMAX)   THEN
         NPOUT=NPOUT+1
         AX(NPOUT)=PF(I)+(PF(I+1)-PF(I))*(YMAX-QF(I))/(QF(I+1)-QF(I))
         AY(NPOUT)=YMAX
        END IF
       END IF

      ELSE IF(QF(I) .GT. YMAX)   THEN
       IF(QF(I+1) .LT. YMAX)   THEN
        NPOUT=NPOUT+1
        AX(NPOUT)=PF(I+1)+(PF(I)-PF(I+1))*(YMAX-QF(I+1))/(QF(I)-QF(I+1))
        AY(NPOUT)=YMAX
        IF(QF(I+1) .LT. YMIN)   THEN
         NPOUT=NPOUT+1
         AX(NPOUT)=PF(I+1) + 
     &   (PF(I)-PF(I+1))*(YMIN-QF(I+1))/(QF(I)-QF(I+1))
         AY(NPOUT)=YMIN
        END IF
       END IF

      ELSE
       NPOUT=NPOUT+1
       AY(NPOUT)=QF(I)
       AX(NPOUT)=PF(I)
       IF( (QF(I+1) .LT. YMIN) .AND. (QF(I) .NE. YMIN) )  THEN
        NPOUT=NPOUT+1
        AX(NPOUT)=PF(I+1)+(PF(I)-PF(I+1))*(YMIN-QF(I+1))/(QF(I)-QF(I+1))
        AY(NPOUT)=YMIN
       ELSE IF( (QF(I+1) .GT. YMAX) .AND. (AY(I) .NE. YMAX) )  THEN
        NPOUT=NPOUT+1
        AX(NPOUT)=PF(I)+(PF(I+1)-PF(I))*(YMAX-QF(I))/(QF(I+1)-QF(I))
        AY(NPOUT)=YMAX
       END IF
      END IF
 5000 CONTINUE

      IF( (QF(JJ) .LE. YMAX) .AND. (QF(JJ) .GE. YMIN) )   THEN
       NPOUT=NPOUT+1
       AX(NPOUT)=PF(JJ)
       AY(NPOUT)=QF(JJ)
      END IF

      RETURN
      END
