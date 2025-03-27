      PROGRAM CALPLOT
C
C     ***DISPLA - SACLANTCEN VERSION
C
C     ***JOHN STEIERT   -   15-DEC-81
C
C -----------------------------------------------------
C
C     AMMENDED:
C              JS  02-FEB-82    USE CHANNEL 48 (NOT 10)
C              JS  03-MAR-82    DON'T MOVE IF ALREADY AT POINT
C              JS   9-Jun-83    Standard FORTRAN version
C              HS  25-Aug-87    Changed to subroutine for generating
C                               Calcomp plots directly
C              HS  30-Jan-89    Changed back to program to be chained
C                               from MINDIS
C              HS  31-Jan-89    Automatic spawn of laser hardcopy
C -----------------------------------------------------
C
C     ***PROGRAM TO PLOT DISPLA PLOT ON CALCOMP PLOTTER
C
      LOGICAL TRACE
      LOGICAL LPGSET, LSCSET, LNWPLT
      CHARACTER*150 TITLE
      CHARACTER*150 SP
      CHARACTER CH1, CH2
      CHARACTER*80 FILENM
      CHARACTER*20 COMFILE,CALFILE
      REAL XSIZ(5),YSIZ(5)
      REAL CURXPT,CURYPT
      INTEGER TTLPTR
      INTEGER SECRTY
      integer getpid
C
C     ***INITIALISATION
      TRACE=.TRUE.
      TRACE=.FALSE.
      SP='    '
      CONVFC=0.039
      YSIZ(1)=210.*CONVFC
      YSIZ(2)=297.*CONVFC
      YSIZ(3)=420.*CONVFC
      YSIZ(4)=594.*CONVFC
      YSIZ(5)=841.*CONVFC
      XSIZ(1)=297.*CONVFC
      XSIZ(2)=420.*CONVFC
      XSIZ(3)=594.*CONVFC
      XSIZ(4)=841.*CONVFC
      XSIZ(5)=1189.*CONVFC
      LPGSET=.FALSE.
      LSCSET=.FALSE.
      LNWPLT=.FALSE.
      TTLPTR=0
      SECRTY=0
      CURXPT=-10.0
      CURYPT=-10.0
C *** GET FILENAME FROM VALUE OF ENV. VARIABLE FOR048
      call getenv('FOR048',filenm)
       write(6,*) 'MINDIS file:',filenm
      open(unit=48,file=filenm,status='old',form='unformatted')
C      REWIND(48)
 1    CONTINUE
        READ(48) ICMD, RVAL1, RVAL2
        IF (TRACE) THEN
          IF (ICMD.EQ.1) THEN
             WRITE(6,100) RVAL1, RVAL2
 100           FORMAT(' CMD 1: PAGE SIZE: X= ',F10.5,' Y= ',F10.5)
          ELSEIF (ICMD.EQ.2) THEN
             WRITE(6,110) IFIX(RVAL1)
 110           FORMAT(' CMD 2: SECURITY: VALUE = ',I3)
          ELSEIF (ICMD.EQ.3) THEN
             WRITE(6,120) RVAL1, RVAL2
 120           FORMAT(' CMD 3: MOVE: X= ',F10.5,' Y= ',F10.5)
          ELSEIF (ICMD.EQ.4) THEN
             WRITE(6,130) RVAL1, RVAL2
 130           FORMAT(' CMD 4: DRAW: X= ',F10.5,' Y= ',F10.5)
          ELSEIF (ICMD.EQ.5) THEN
             WRITE(6,140) RVAL1, RVAL2
 140           FORMAT(' CMD 5: TITLE: ',2A1)
          ELSEIF (ICMD.EQ.99) THEN
             WRITE(6,150)
 150           FORMAT(' CMD 99: END OF FILE')
             STOP
          ELSE
             WRITE(6,160) ICMD, RVAL1, RVAL2
 160           FORMAT(' CMD ',I5,' RVAL1 = ',F10.5,' RVAL2 = ',F10.5)
             ENDIF
          ENDIF
C
C       ***PROCESS LINE
        IF (ICMD.EQ.1) THEN
C
C          ***CMD 1: SPECIFY PAGE SIZE
           LPGSET=.TRUE.
           XLNGTH=RVAL1
           YLNGTH=RVAL2
           XPLEN=RVAL1
           YPLEN=RVAL2
           ISHEET=0
           IF (XPLEN.LT.YPLEN) THEN
                ISHEET=8
                RTMP=XPLEN
                XPLEN=YPLEN
                YPLEN=RTMP
                ENDIF
           DO 50 I=1,5
                IF (XSIZ(I).GE.XPLEN) GOTO 52
 50             CONTINUE
           WRITE(6,401) XPLEN
 401           FORMAT(' ERROR: X DIMENSION TOO LARGE: ',F10.3)
           STOP
C
 52        CONTINUE
           DO 54 J=I,5
                IF (YSIZ(J).GE.YPLEN) GOTO 56
 54             CONTINUE
           WRITE(6,403) YPLEN
 403           FORMAT(' ERROR: Y DIMENSION TOO LARGE: ',F10.3)
           STOP
C
 56        CONTINUE
           ISHEET=ISHEET+5-J
           WRITE(6,405) ISHEET
 405           FORMAT(' PLOT SHEET SIZE PARAMETER = ',I4)
        ELSEIF (ICMD.EQ.2) THEN
C
C          ***CMD 2: SET SECURITY LEVEL
           LSCSET=.TRUE.
           SECRTY=IFIX(RVAL1)
        ELSEIF (ICMD.EQ.3) THEN
C
C          ***CMD 3: MOVE PEN
           IF (.NOT.LNWPLT) THEN
                LNWPLT=.TRUE.
                IF (.NOT.LSCSET) THEN
                      WRITE(6,601)
  601                     FORMAT(' ERROR: SECURITY CLASS NOT SET')
                      STOP
                      ENDIF
                IF (.NOT.LPGSET) THEN
                      WRITE(6,603)
 603                      FORMAT(' ERROR: PAGE SIZE NOT SET')
                      STOP
                      ENDIF
                 call pinit()
C                CALL NEWPLT(ISHEET,TITLE,TTLPTR,SECRTY)
c                CALL GPRNT
c                CALL GOPEN
c                RVAL1X=XLNGTH/CONVFC
c                RVAL2X=YLNGTH/CONVFC
c                CALL GLIMIT(0.0,RVAL1X,0.0,RVAL2X,0.0,0.0)
c                CALL GSIZE(RVAL1X,RVAL2X,0.0)
c                CALL GORIG(0.0,0.0)
c                CALL GSCALE
                WRITE(6,701) TITLE(1:TTLPTR)//SP(TTLPTR+1:80)
 701                FORMAT(' PLOT TITLE = ',1A60)
                ENDIF
           IF (((RVAL1.LT.0.0).OR.(RVAL1.GT.XLNGTH)) .OR.
     $         ((RVAL2.LT.0.0).OR.(RVAL2.GT.YLNGTH))) THEN
                WRITE(6,630) RVAL1, RVAL2
 630                FORMAT(' ERROR: ATTEMPT TO MOVE ',
     $                     ' OUTSIDE SPECIFIED PLOT AREA.',' (X,Y)=',
     $                     2F10.3)
           ELSE
                IF ((RVAL1.NE.CURXPT).OR.(RVAL2.NE.CURYPT)) THEN
                     CALL PLOT(RVAL1,RVAL2,3)
c                     CALL GVECT(RVAL1*25.4,RVAL2*25.4,0)
                     CURXPT=RVAL1
                     CURYPT=RVAL2
                     ENDIF
                ENDIF
        ELSEIF (ICMD.EQ.4) THEN
C
C          ***CMD 4: DRAW LINE
           IF (.NOT.LNWPLT) THEN
                LNWPLT=.TRUE.
                IF (.NOT.LSCSET) THEN
                      WRITE(6,605)
  605                     FORMAT(' ERROR: SECURITY CLASS NOT SET')
                      STOP
                      ENDIF
                IF (.NOT.LPGSET) THEN
                      WRITE(6,607)
 607                      FORMAT(' ERROR: PAGE SIZE NOT SET')
                      STOP
                      ENDIF
                call pinit()
C                CALL NEWPLT(ISHEET,TITLE,TTLPTR,SECRTY)
c                CALL GPRNT
c                CALL GOPEN
c                RVAL1X=XLNGTH/CONVFC
c                RVAL2X=YLNGTH/CONVFC
c                CALL GSIZE(RVAL1X,RVAL2X,0.0)
c                CALL GORIG(0.0,0.0)
c                CALL GSCALE
                WRITE(6,701) TITLE(1:TTLPTR)//SP(TTLPTR+1:80)
                ENDIF
           IF (((RVAL1.LT.0.0).OR.(RVAL1.GT.XLNGTH)) .OR.
     $         ((RVAL2.LT.0.0).OR.(RVAL2.GT.YLNGTH))) THEN
                WRITE(6,632) RVAL1, RVAL2
 632                FORMAT(' ERROR: ATTEMPT TO DRAW ',
     $                     ' OUTSIDE SPECIFIED PLOT AREA.',' (X,Y)=',
     $                     2F10.3)
           ELSE
                CALL PLOT(RVAL1,RVAL2,2)
c                     CALL GVECT(RVAL1*25.4,RVAL2*25.4,1)
                CURXPT=RVAL1
                CURYPT=RVAL2
                ENDIF
        ELSEIF (ICMD.EQ.5) THEN
C
C          ***CMD 5: SPECIFY TITLE
           CH1=CHAR(IFIX(RVAL1))
           CH2=CHAR(IFIX(RVAL2))
           TITLE(TTLPTR+1:TTLPTR+1)=CH1
           TITLE(TTLPTR+2:TTLPTR+2)=CH2
           TTLPTR=TTLPTR+2
        ELSEIF (ICMD.EQ.99) THEN
C
C          ***CMD 99: END OF PLOT
            call pend()
C           CALL PLOT(0.,0.,999)
c           CALL GCLOSE
           GOTO 99
           ENDIF
        GOTO 1
C
C     ***END OF RUN
 99   CONTINUE
       close(48,status='delete')
C *** SPAWN PROCESS FOR LASER HARDCOPY
       ID=getpid()
       write(comfile,'(A3,I5.5)') 'com',ID
       if (ID.LT.10) THEN
       write(calfile,'(A5,I1)') 'plotA',ID
       ELSE IF (ID.LT.100) THEN
       write(calfile,'(A5,I2)') 'plotA',ID
       ELSE IF (ID.LT.1000) THEN
       write(calfile,'(A5,I3)') 'plotA',ID
       ELSE IF (ID.LT.10000) THEN
       write(calfile,'(A5,I4)') 'plotA',ID
       ELSE
       write(calfile,'(A5,I5)') 'plotA',ID
       END IF
       open(unit=47,file=comfile,status='new')
       write(47,'(A1)') '#'
       write(47,'(A,A)') 'lplot ',calfile
       write(47,'(A,A)') 'rm ',calfile
       write(47,'(A,A)') 'rm ',comfile
       close(47,status='keep')
       call chmod(comfile,'00755')
       call system(comfile//' &')
C      return
      END
