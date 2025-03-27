      PROGRAM MINPOST
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
C              HS   8-MAY-90    Direct Post-script driver
c	       HS  25-jun-90	type 7-8 added for polygon filling
C -----------------------------------------------------
C
C     ***PROGRAM TO PLOT DISPLA PLOT ON PostScript printer
C
      LOGICAL TRACE
      LOGICAL LPGSET, LSCSET, LNWPLT, POLYG, LSTPEN, INCURV
      CHARACTER*150 TITLE
      CHARACTER*150 SP
      CHARACTER CH1, CH2
      CHARACTER*80 FILENM
      CHARACTER*20 COMFILE,PLOTFILE
      character*12 font
      character*6 envvar(2)
      REAL XSIZ(5),YSIZ(5)
      REAL CURXPT,CURYPT
      INTEGER TTLPTR
      INTEGER SECRTY
      integer getpid,pitch
C *** LASER PRINTER DEFINITIONS
      DATA DEFXSCALE /612.0/
      DATA DEFYSCALE /792.0/
      DATA XMARG,YMARG /39.0,20.0/
      DATA FONT /'/Times-Roman'/
      data pitch /10/
      data widthlin /.5/
      data scalfac /100.0/
C
C     ***INITIALISATION
c
       POLYG=.FALSE.
      scalmarg=amin1((defxscale-xmarg)/defxscale,
     &               (defyscale-ymarg)/defyscale)
       ID=getpid()
       write(comfile,'(A3,I5.5)') 'com',ID
       write(plotfile,'(A5,I5.5)') 'plotA',ID
       
c       if (ID.LT.10) THEN
c       write(plotfile,'(A5,I1)') 'plotA',ID
c       ELSE IF (ID.LT.100) THEN
c       write(plotfile,'(A5,I2)') 'plotA',ID
c       ELSE IF (ID.LT.1000) THEN
c       write(plotfile,'(A5,I3)') 'plotA',ID
c       ELSE IF (ID.LT.10000) THEN
c       write(plotfile,'(A5,I4)') 'plotA',ID
c       ELSE
c       write(plotfile,'(A5,I5)') 'plotA',ID
c       END IF
c *** open postscript file
       OPEN(UNIT=49,FILE=PLOTFILE,FORM='FORMATTED',STATUS='UNKNOWN')
C *** WRITE DEFINITIONS TO POSTSCRIPT FILE
c       WRITE(49,'(A)') ' ',' '
       WRITE(49,'(A)') '%!'
       WRITE(49,'(A)') '/tlwcontext save def'
       write(49,'(f4.1,a)') widthlin,' setlinewidth'
       write(49,'(a)') '/m {moveto} def'
       write(49,'(a)') '/l {lineto} def'
       write(49,'(a)') '/r {rmoveto} def'
       write(49,'(a)') '/s {show} def'
       write(49,'(a)') '/csm {currentpoint stroke moveto} def'
       WRITE(49,'(A)') '/st {stroke} def'
       WRITE(49,'(A)') '/np {newpath} def'
       WRITE(49,'(A)') '/cp {closepath} def'
       WRITE(49,'(A)') '/sg {setgray} def'
c >>> Define color scale
       WRITE(49,'(2A)') '/graycol {dup dup currentrgbcolor 4 -2 roll',
     &                   ' mul 4 -2 roll'
       WRITE(49,'(A)')  'mul 4 -2 roll mul setrgbcolor} bind def'
       WRITE(49,'(A)') '/col-1 {} def'
       WRITE(49,'(A)') '/col0 {0 0 0 setrgbcolor} bind def'
       WRITE(49,'(A)') '/col1 {1 0 0 setrgbcolor} bind def'
       WRITE(49,'(A)') '/col2 {0 1 0 setrgbcolor} bind def'
       WRITE(49,'(A)') '/col3 {0 0 1 setrgbcolor} bind def'
       WRITE(49,'(A)') '/col4 {0 1 1 setrgbcolor} bind def'
       WRITE(49,'(A)') '/col5 {1 0 1 setrgbcolor} bind def'
       WRITE(49,'(A)') '/col6 {1 1 0 setrgbcolor} bind def'
c white       WRITE(49,'(A)') '/col7 {1 1 1 setrgbcolor} bind def'
       WRITE(49,'(A)') '/col7 {0 0 0 setrgbcolor} bind def'
       WRITE(49,'(A)') '/col8 {.68 .85 .9 setrgbcolor} bind def'
       WRITE(49,'(A)') '/col9 {0 .39 0 setrgbcolor} bind def'
       WRITE(49,'(A)') '/col10 {.65 .17 .17 setrgbcolor} bind def'
       WRITE(49,'(A)') '/col11 {1 .51 0 setrgbcolor} bind def'
       WRITE(49,'(A)') '/col12 {.63 .13 .94 setrgbcolor} bind def'
       WRITE(49,'(A)') '/col13 {1 .75 .8 setrgbcolor} bind def'
       WRITE(49,'(A)') '/col14 {.7 .13 .13 setrgbcolor} bind def'
       WRITE(49,'(A)') '/col15 {1 .84 0 setrgbcolor} bind def'
      TRACE=.TRUE.
      TRACE=.FALSE.
      SP='    '
      CONVFC=0.039
c A4 size
c      YSIZ(1)=210.*CONVFC
c      YSIZ(2)=297.*CONVFC
c Letter size
      ysiz(1)=((DEFXSCALE-xmarg)/72.0)*.965
      ysiz(2)=((DEFYSCALE-ymarg)/72.0)*.965
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
      envvar(1)='FOR048'
      lenf=8
cray      call getenv(envvar,filenm,lenf)
      call getenv(envvar(1),filenm)
       write(6,*) 'MINDIS file:',filenm
      open(unit=48,file=filenm,status='old',form='unformatted')
C      REWIND(48)
 1    CONTINUE
        READ(48, END=6789) ICMD, RVAL1, RVAL2
        go to 6790
 6789   icmd=99
 6790   IF (TRACE) THEN
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
c           ISHEET=ISHEET+5-J
c           WRITE(6,405) ISHEET
c 405           FORMAT(' PLOT SHEET SIZE PARAMETER = ',I4)
c *** postscript page sizes
            write(49,'(a)') 'initmatrix'
           if (rval2.lt.rval1) then
            rv1=max(rval1,ysiz(2))
            rv2=max(rval2,ysiz(1))
            xscale=(defyscale)/rv1
            yscale=(defxscale)/rv2
            write(49,'(a)') '90 rotate'
            write(49,'(f10.3,f10.3,a)') 0e0,-defxscale,' translate'
            write(49,'(f10.3,f10.3,a)') ymarg/2,xmarg/2,' translate'
           else
            rv1=max(rval1,ysiz(1))
            rv2=max(rval2,ysiz(2))
            xscale=(defxscale)/rv1
            yscale=(defyscale)/rv2
            write(49,'(f10.3,f10.3,a)') xmarg/2,ymarg/2,' translate'
           end if
           scale=amin1(xscale,yscale)
           widthnew=widthlin/scale
           pitchnew=pitch/scale
           write(49,'(f10.3,f10.3,a)') scalmarg,scalmarg,' scale'
           write(49,'(f10.3,f10.3,a)') scale,scale,' scale'

           write(49,'(f10.3,a)') widthnew,' setlinewidth'
           write(49,'(a,a)') font,' findfont'
           write(49,'(f10.3,a)') pitchnew,' scalefont'
           write(49,'(a)') 'setfont'
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
c                 call pinit()
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
                ELSE
                ENDIF
           IF (((RVAL1.LT.0.0).OR.(RVAL1.GT.XLNGTH)) .OR.
     $         ((RVAL2.LT.0.0).OR.(RVAL2.GT.YLNGTH))) THEN
                WRITE(6,630) RVAL1, RVAL2
 630                FORMAT(' ERROR: ATTEMPT TO MOVE ',
     $                     ' OUTSIDE SPECIFIED PLOT AREA.',' (X,Y)=',
     $                     2F10.3)
           ELSE
                IF ((RVAL1.NE.CURXPT).OR.(RVAL2.NE.CURYPT).or.
     &              (npcnt.ge.1400)) then
c                     CALL PLOT(RVAL1,RVAL2,3)
c           write(49,'(2(f7.3),a)') rval1*scale,rval2*scale,' m'
C *** STROKE FOR EARLIER CURVE
                 IF (.NOT.POLYG.AND.INCURV) THEN                 
                  WRITE(49,'(A)') 'st'
                  INCURV=.FALSE.
                 end if
           write(49,'(a)') 'np'
c           write(49,'(2(f7.3),a)') rval1,rval2,' m'
            if (rval1.le.9.999) then
             if (rval2.le.9.999) then
              write(49,'(f5.3,f6.3,a)') rval1,rval2,' m'
             else
              write(49,'(f5.3,f7.3,a)') rval1,rval2,' m'
             end if
            else
             if (rval2.le.9.999) then
              write(49,'(f6.3,f6.3,a)') rval1,rval2,' m'
             else
              write(49,'(f6.3,f7.3,a)') rval1,rval2,' m'
             end if
            end if
            npcnt=1
c                     CALL GVECT(RVAL1*25.4,RVAL2*25.4,0)
                     CURXPT=RVAL1
                     CURYPT=RVAL2
                     if (polyg) then
                      xmin=min(xmin,rval1)
                      xmax=max(xmax,rval1)
                      ymin=min(ymin,rval2)
                      ymax=max(ymax,rval2) 
                     end if  
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
c                call pinit()
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

c            write(49,'(2(f7.3),a)') rval1*scale,Rval2*scale,' l'

            if (rval1.le.9.999) then
             if (rval2.le.9.999) then
              write(49,'(f5.3,f6.3,a)') rval1,rval2,' l'
             else
              write(49,'(f5.3,f7.3,a)') rval1,rval2,' l'
             end if
            else
             if (rval2.le.9.999) then
              write(49,'(f6.3,f6.3,a)') rval1,rval2,' l'
             else
              write(49,'(f6.3,f7.3,a)') rval1,rval2,' l'
             end if
            end if
            npcnt=npcnt+1
              IF (.NOT.POLYG) THEN
c                write(49,'(a)') 'csm'
                 INCURV=.TRUE.
              END IF

c                CALL PLOT(RVAL1,RVAL2,2)
c                     CALL GVECT(RVAL1*25.4,RVAL2*25.4,1)
                CURXPT=RVAL1
                CURYPT=RVAL2
                     if (polyg) then
                      xmin=min(xmin,rval1)
                      xmax=max(xmax,rval1)
                      ymin=min(ymin,rval2)
                      ymax=max(ymax,rval2) 
                     end if  
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
C          ***CMD 6: SET PEN COLOR
                 IF (INCURV) THEN                 
                  WRITE(49,'(A)') 'st'
                  INCURV=.FALSE.
                 end if
                curxpt=-1
                curypt=-1
           IF (.NOT.LNWPLT) THEN
            LSTPEN=.TRUE.
            IPENNO=NINT(RVAL1)
           ELSE
c >>> Greytone
c            GREY=FLOAT(MOD(NINT(RVAL1),16)-1)/15.0
c            WRITE(49,'(F3.1,A)') GREY,' sg'
c >>> Color Postscript
            ICOL=mod(nint(rval1)-1,16)
            if (icol.lt.10) then
             write(49,'(a3,i1)') 'col',icol
            else 
             write(49,'(a3,i2)') 'col',icol
            end if
           end if
        ELSEIF (ICMD.EQ.7) THEN
C          ***CMD 7: Start filled polygon
                 IF (INCURV) THEN                 
                  WRITE(49,'(A)') 'st'
                  INCURV=.FALSE.
                 end if
            POLYG=.TRUE.
c            WRITE(49,'(A)') 'np'
            xmin=1e10
            ymin=1e10
            xmax=-1e10
            ymax=-1e10
            curxpt=-1
            curypt=-1
        ELSEIF (ICMD.EQ.8) THEN
C          ***CMD 8: Close and fill polygon
            POLYG=.false.
            incurv=.false.
            if ((xmax-xmin).gt.widthnew.and.
     &          (ymax-ymin).gt.widthnew) then
             WRITE(49,'(A)') 'cp'
             WRITE(49,'(a)') 'fill'
            else
             write(49,'(a)') 'st'
            end if
            curxpt=0.0
            curypt=0.0
        ELSEIF (ICMD.EQ.99) THEN
C
C          ***CMD 99: END OF PLOT
c            call pend()
                 IF (INCURV) THEN                 
                  WRITE(49,'(A)') 'st'
                  INCURV=.FALSE.
                 end if
             write(49,'(a)') 'showpage'
             write(49,*)
C           CALL PLOT(0.,0.,999)
c           CALL GCLOSE
           GOTO 99
           ENDIF
        GOTO 1
C
C     ***END OF RUN
 99   CONTINUE
        close(49,status='keep')
        close(48,status='delete')
C *** SPAWN PROCESS FOR LASER HARDCOPY
       open(unit=47,file=comfile,status='new',form='formatted')
       write(47,'(A1)') '#'
       write(47,'(A,A)') 'lpr ',plotfile
       write(47,'(A,A)') 'rm ',plotfile
       write(47,'(A,A)') 'rm ',comfile
       close(47,status='keep')
c       call chmod(comfile,'00755')
       call system('chmod 755 '//comfile)
       call system('csh '//comfile//' &')
cray   call ishell('csh '//comfile //' &')
C      return
      END
