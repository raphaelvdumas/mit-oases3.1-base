C
C     ***MINDIS - SACLANTCEN VERSION
C
C     ***JOHN STEIERT   -   15-DEC-81
C
C -----------------------------------------------------
C
C     AMMENDED:
C              JS  02-FEB-82    USE CHANNEL 98 (NOT 10)
C              JS  03-MAR-82    DON'T MOVE IF ALREADY AT POINT
C              JS   9-Jun-83    Standard FORTRAN version
C                               Use Ascii files (not Binary)
C              JS   1-Nov-83    Use Binary files (not Ascii)
C              JS   8-Nov-83    Applicon version
C              JS  18-Feb-85    Tektronix 4691 version
C	       JS   3-Oct-85    Change line width to 2 pixels
C              JS  11-Apr-86    Allow for UNIRAS title at bottom of page
C	       JS  15-May-86	Use LIB$STOP to create error if Page size wrong
C				Use GROTA if appropriate.
C	       JS  15-Oct-86	Call GLOCKI to maintain MINDIS plot-id
c              HS  13-May-88    Changed to drive the 4696
C
C -----------------------------------------------------
C
C     ***PROGRAM TO PLOT DISPLA PLOT ON Tektronix 4696 PLOTTER
C
      LOGICAL TRACE
      LOGICAL LPGSET, LSCSET, LNWPLT
      CHARACTER*80 FILENM,comfile
      CHARACTER*150 TITLE
      CHARACTER*150 SP
      CHARACTER CH1, CH2
      REAL XSIZ(5),YSIZ(5)
      REAL CURXPT,CURYPT
      INTEGER TTLPTR
      INTEGER SECRTY
      INTEGER IPENNO
      LOGICAL LSTPEN
      LOGICAL ROTATE
      integer getpid
      character*40 nm4695
      common /oufiln/ nrofpl,nm4695
      
C
C     ***INITIALISATION
      TRACE=.TRUE.
      TRACE=.FALSE.
      LSTPEN=.FALSE.
      IPENNO=0
      SP='    '
      CONVFC=0.039
      YSIZ(1)=242.52*CONVFC
      XSIZ(1)=420.00*CONVFC
      LPGSET=.FALSE.
      LSCSET=.FALSE.
      LNWPLT=.FALSE.
      TTLPTR=0
      SECRTY=0
      CURXPT=-10.0
      CURYPT=-10.0
C *** GET MINDIS FILE NAME FROM ENV. VARIABLE FOR048
      call getenv('FOR048',FILENM)
      OPEN(UNIT=48,FILE=FILENM,STATUS='OLD',FORM='UNFORMATTED')
C
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
          ELSEIF (ICMD.EQ.6) THEN
             WRITE(6,150) IFIX(RVAL1)
 150             FORMAT(' CMD 6: SET PEN. ICOL= ',I3)
          ELSEIF (ICMD.EQ.99) THEN
             WRITE(6,160)
 160           FORMAT(' CMD 99: END OF FILE')
             STOP
          ELSE
             WRITE(6,170) ICMD, RVAL1, RVAL2
 170           FORMAT(' CMD ',I5,' RVAL1 = ',F10.5,' RVAL2 = ',F10.5)
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
	   ROTATE=.FALSE.
	   IF (XPLEN.GT.XSIZ(1).OR.YPLEN.GT.YSIZ(1)) THEN
		IF (XPLEN.LT.YSIZ(1).AND.YPLEN.LT.XSIZ(1)) THEN
			XPLEN=RVAL2
			YPLEN=RVAL1
			ROTATE=.TRUE.
			ENDIF
		ENDIF
           IF (XPLEN.GT.XSIZ(1)) THEN
	        WRITE(6,401) XPLEN,XSIZ(1)
 401 		    FORMAT(' Error: x dimension too large: ',F10.3,
     $			   ' Max size = ',F10.3)
c		*** Give error 4004
C		CALL LIB$STOP(%val((16*16*8)* (2**3) + 4))
	        STOP
		ENDIF
C
	   IF (YPLEN.GT.YSIZ(1)) THEN
		WRITE(6,403) YPLEN,YSIZ(1)
 403		    FORMAT(' Error: y dimension too large: ',F10.3,
     $			   ' MAX SIZE = ',F10.3)
c		*** Give error 4004
C		CALL LIB$STOP(%val((16*16*8)* (2**3) + 4))
		STOP
		ENDIF
C
 56        CONTINUE
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
                CALL groute('SEL GT4695; EXIT')
                CALL GOPEN
		IF (ROTATE) CALL GROTA
		CALL GEOWID(-2.0)
                RVAL1X=XLNGTH/CONVFC
                RVAL2X=YLNGTH/CONVFC
                CALL GSIZE(RVAL1X,RVAL2X,0.0)
                CALL GLIMIT(0.0,RVAL1X,0.0,RVAL2X,0.0,0.0)
		IF (ROTATE) THEN
	                CALL GORIG(0.0,0.0)
		ELSE
	                CALL GORIG(0.0,10.0)
			ENDIF
                CALL GSCALE
C		CALL GPLTNM(title)
                IF (LSTPEN) THEN
                     LSTPEN=.FALSE.
                     CALL GWICOL(-2.0,IPENNO)
                     ENDIF
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
                     CALL GVECT(RVAL1*25.4, (RVAL2*25.4), 0)
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
                CALL GROUTE('SEL GT4695; EXIT')
                CALL GOPEN
		IF (ROTATE) CALL GROTA
		CALL GEOWID(-2.0)
                RVAL1X=XLNGTH/CONVFC
                RVAL2X=YLNGTH/CONVFC
                CALL GSIZE(RVAL1X,RVAL2X,0.0)
                CALL GLIMIT(0.0,RVAL1X,0.0,RVAL2X,0.0,0.0)
		IF (ROTATE) THEN
	                CALL GORIG(0.0,0.0)
		ELSE
	                CALL GORIG(0.0,10.0)
			ENDIF
                CALL GSCALE
C		CALL GPLTNM(title)
                IF (LSTPEN) THEN
                     LSTPEN=.FALSE.
                     CALL GWICOL(-2.0,IPENNO)
                     ENDIF
                WRITE(6,701) TITLE(1:TTLPTR)//SP(TTLPTR+1:80)
                ENDIF
           IF (((RVAL1.LT.0.0).OR.(RVAL1.GT.XLNGTH)) .OR.
     $         ((RVAL2.LT.0.0).OR.(RVAL2.GT.YLNGTH))) THEN
                WRITE(6,632) RVAL1, RVAL2
 632                FORMAT(' ERROR: ATTEMPT TO DRAW ',
     $                     ' OUTSIDE SPECIFIED PLOT AREA.',' (X,Y)=',
     $                     2F10.3)
           ELSE
                     CALL GVECT(RVAL1*25.4, (RVAL2*25.4), 1)
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
        ELSEIF (ICMD.EQ.6) THEN
C
C          ***CMD 6: SET PEN COLOUR
           IF (.NOT.LNWPLT) THEN
                LSTPEN=.TRUE.
                IPENNO=IFIX(RVAL1)
           ELSE
		CALL GWICOL(-2.0,IFIX(RVAL1))
                ENDIF
        ELSEIF (ICMD.EQ.99) THEN
C
C          ***CMD 99: END OF PLOT
C           CALL PLOT(0.,0.,999)
           CALL QT4695
           CALL GCLOSE
C
           GOTO 99
           ENDIF
        GOTO 1
C
C     ***END OF RUN
 99   CONTINUE
c >>> print out the file on Tektronix 4695
c     print command lptk must be defined in .login
C *** SPAWN PROCESS FOR Tektronix HARDCOPY
       ID=getpid()
       write(comfile,'(A3,I5.5)') 'com',ID
       open(unit=47,file=comfile,status='new',form='formatted')
       write(47,'(A1)') '#'
       write(47,'(A,A)') 'lptk ',nm4695(1:9)
       write(47,'(A,A)') 'rm ',nm4695(1:9)
       write(47,'(A,A)') 'rm ',comfile
       close(47,status='keep')
       call chmod(comfile,'00755')
       call system('csh '//comfile//' &')
cray   call ishell('csh '//comfile //' &')
      END
