      SUBROUTINE REDATA(Z,NPX,NPY,
     & NPXRD,IY1,IY2,NPYRD,SECTOR,FORM,FLAGRC)
      CHARACTER*3 FORM
      DIMENSION Z(NPXRD,NPYRD),SECTOR(28)

      IF(FLAGRC.EQ.0.0)   THEN

       IF(FORM.EQ.'BIN')   THEN

 1000   READ(17)SECTOR
        NP=SECTOR(1)
        IF(NP.LT.NPXRD)   THEN
         WRITE(6,*) 'WARNING: NP.LT.NPXRD  - STOP IN REDATA '
         WRITE(6,*) ' NP= ',NP, ' NPXRD= ',NPXRD
         STOP
        END IF
        DO 1200 IY=1,IY1-1
        READ(17)(DUMMY,IX=1,NP)
 1200   CONTINUE
        DO 1400 IY=NPYRD,1,-1
        READ(17)(Z(IX,IY),IX=1,NPXRD),(DUMMY,IX=NPXRD+1,NP)
 1400   CONTINUE
        IF(SECTOR(4).LT.1.0)   GO TO 1000

       ELSE
        
 2000   READ(17,*)SECTOR
        NP=SECTOR(1)
        IF(NP.LT.NPXRD)   THEN
         WRITE(6,*) 'WARNING: NP.LT.NPXRD  - STOP IN REDATA '
         WRITE(6,*) ' NP= ',NP, ' NPXRD= ',NPXRD
         STOP
        END IF
        DO 2200 IY=1,IY1-1
        READ(17,*)(DUMMY,IX=1,NP)
 2200   CONTINUE
        DO 2400 IY=NPYRD,1,-1
        READ(17,*)(Z(IX,IY),IX=1,NPXRD),(DUMMY,IX=NPXRD+1,NP)
 2400   CONTINUE
        IF(SECTOR(4).LT.1.0)   GO TO 2000

       END IF

      ELSE

       IF(FORM.EQ.'BIN')   THEN

 3000   READ(17)SECTOR
        NP=SECTOR(1)
        IF(NP.NE.NPXRD)   THEN
         WRITE(6,*)  ' WARNING NP.NE.NPXRD STOP IN REDATA'
         WRITE(6,*) ' NP= ',NP, ' NPXRD= ',NPYRD
         STOP
        END IF
        DO 3200 IX=1,NPXRD
        READ(17)   (DUMMY,I=1,IY1-1),
     &                (Z(IX,IY),IY=NPYRD,1,-1),
     &                (DUMMY,I=IY2+1,NPY)  
 3200   CONTINUE
        IF(SECTOR(4).LT.1.0)   GO TO 3000

       ELSE

 4000   READ(17,*)SECTOR
        NP=SECTOR(1)
        IF(NP.NE.NPXRD)   THEN
         WRITE(6,*)  ' WARNING NP.NE.NPXRD STOP IN REDATA'
         WRITE(6,*) ' NP= ',NP, ' NPXRD= ',NPXRD
         STOP
        END IF
        DO 4200 IX=1,NPXRD
        READ(17,*)   (DUMMY,I=1,IY1-1),
     &                (Z(IX,IY),IY=NPYRD,1,-1),
     &                (DUMMY,I=IY2+1,NPY) 
 4200   CONTINUE
        IF(SECTOR(4).LT.1.0)   GO TO 4000

       END IF

      END IF

      RETURN
      END
