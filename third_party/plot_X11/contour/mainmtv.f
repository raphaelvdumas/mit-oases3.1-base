      SUBROUTINE MAINMTV(Z,FREQ,SD,RD,OPTION,DEVICE,
     & XPIXEL,YPIXEL,VUG,DEL,ZBUF,NPXYZ,FAX,ROT,REVERS,NCS,WDW)
      PARAMETER ( NCLZ=19,NCLZP1=NCLZ+1,NLEV1=51 )
      parameter (nbot_max = 5000, nbm= nbot_max+20)

      LOGICAL BATCH, IB
      EXTERNAL BATCH
      character*6 envvar
      character*80 filenm,psname
      CHARACTER*3 XBTYPE,YBTYPE,BWCOL,LINEP,DEVICE,FONT,VUG,DEL,
     & FAX, ROT, NOWRT, SDPLOT, RDPLOT, NCS, WDW, ULC
      CHARACTER*3 MODE
      CHARACTER*4 DUMMY
      CHARACTER*40 DIDASC
      CHARACTER*80 OPTION
      CHARACTER*80 UNIINF, UNIWRK, UNIRST
      CHARACTER*80 TITLE, TITLEX, TITLEY
      character*80 mtvflags

      REAL ZBUF(NPXYZ)
      DIMENSION KOLORS(NCLZP1),ZCLASS(NCLZ),Z(1)
      REAL TRPAX(4), TRPAY(4)

      common /unplct/ nrofpl
      COMMON /BOTT/ XF(nbm), YF(nbm), NPBOTT, ISHADE, NPSH
      COMMON /SH/ PF(440), QF(440), UF(440), VF(440)
      COMMON /MULBOTT/ LINES,numbot
      COMMON /FLSUNI/ UNIINF,UNIWRK,UNIRST
      COMMON /FONTEX/ FONT
      COMMON /CHFLAG/ BWCOL, LINEP
      COMMON /HSFLAG/ IFIRST,ILAST,CYL,FOM,PRB,SEG,ISEG,
     &                IFR,SDFLAG,NCL
      COMMON /PARA/ LABPT,NSM,NDIV,CAY,NARC,NRNG,HGTPT,HGT,
     &              LABC(51),LWGT(51)
      COMMON /PARAC/ TITLE, SDPLOT, RDPLOT
      COMMON /PLT/ FACT,YASIZE,SCALF,SF,IB,PLT
      COMMON /SNAPSHOT/ SCFAC
      COMMON /SWITCH/ NOWRT, ULC
      COMMON /XAX/ X1,XL,XLEFT,XRIGHT,XSCALE,XINC,DX,
     &             X1PL,XLPL,NX,X1GRID,XLGRID,DIVX,XVAL(100),NXVAL
      COMMON /XAXC/ TITLEX,XBTYPE
      COMMON /YAX/ Y1,YL,YUP,YDOWN,YSCALE,YINC,DY,
     &             Y1PL,YLPL,NY,Y1GRID,YLGRID,DIVY,YVAL(100),NYVAL
      COMMON /YAXC/ TITLEY,YBTYPE
      COMMON /ZAX/ ZMIN,ZMAX,ZINC,NLEV,ZLEV(NLEV1)

      DATA DIST1/30.0/
      DATA AXWID/ 0.4 /
C
  100 FORMAT('F=',F7.1,'Hz',2X,'SD=',F6.1,A3)
  120 FORMAT('F=',F7.1,'Hz')
  140 FORMAT('F=',F7.1,'Hz',2X,'SD=',F6.1,A3,2X,'SF=10**',I2)
  160 FORMAT('SD=',F6.1,A3)
  200 FORMAT('SD=',F6.1,A3,2X,' RD=',F6.1,A3)
  210 FORMAT('F=',F7.1,'Hz',2X,'SD=',F6.1,A3,2X,' RD=',F6.1,A3)
  300 FORMAT(1X,'REVISE NUMBER OF "Z" LEVELS.',/,'EXECUTION ',
     & 'TERMINATED - ERROR IN SUB. MAINMTV',/)
  400 FORMAT(A4)
  500 FORMAT(1X,/,'  TO CLOSE THE GRAPHIC WINDOW ASSIGN THE INPUT FOCUS',
     & /,'   TO THE WINDOW AND PRESS RETURN ')
  600 FORMAT(1X,/,' *** WARNING: BOTTOM SHADING NOT ALLOWED WITH',
     & ' VIEWGRAPHS *** ',/) 
      ratio=1.0
C
      luncdr=55
      mtvlun=14
      WRITE(ENVVAR,'(A3,I3.3)') 'FOR',luncdr
      CALL GETENV(ENVVAR,FILENM)
      IF (FILENM.EQ.' ') THEN
       filenm='cplot'//'_'//char(65+nrofpl)//'.mtv'
       OPEN(UNIT=mtvlun,FILE=filenm,STATUS='UNKNOWN',FORM='FORMATTED')
       inquire(unit=mtvlun,name=filenm)
        nrofpl=nrofpl+1
      ELSE
       filenm=filenm(1:(lenstr(filenm)-4))//'_'//char(65+nrofpl)//
     &                                      '.mtv'
       OPEN(UNIT=mtvlun,FILE=FILENM,STATUS='UNKNOWN',
     &      FORM='FORMATTED')
        nrofpl=nrofpl+1
      END IF
C
      DIDASC(1:1)='$'
      IF( (OPTION(1:5).EQ.'CONDR') .OR.
     &    (OPTION(1:5).EQ.'EXPDR') .OR.
     &    (OPTION(1:5).EQ.'IFD  ') .OR.
     &    (OPTION(1:5).EQ.'PAREQ') .OR.
     &    (OPTION(1:5).EQ.'TDPEN')     )   THEN
       IF((FOM.LT.1.0 .OR. SD.GT.0.0) .AND. (SDFLAG .LT. 1.0)) THEN
        IF(OPTION(1:5).EQ.'CONDR'.AND.SCFAC.NE.0.0) THEN
          IEXPSCFAC=ALOG10(SCFAC)
          WRITE(DIDASC,140)FREQ,SD,SDPLOT,IEXPSCFAC
        ELSE
          WRITE(DIDASC,100)FREQ,SD,SDPLOT
        END IF
       ELSE
        WRITE(DIDASC,120) FREQ
       END IF
      ELSE
       IF  (OPTION(1:5).EQ.'CONFR' )   THEN
        WRITE(DIDASC,200)SD,SDPLOT,RD,RDPLOT
       ELSEIF  (OPTION(1:5).EQ.'CONDA' )   THEN
        WRITE(DIDASC,160)SD,SDPLOT
       ELSEIF  (OPTION(1:5).EQ.'CONTR' )   THEN
        WRITE(DIDASC,210)FREQ,SD,SDPLOT,RD,RDPLOT
       END IF
      END IF
      write(14,'(a)') '$DATA=CONTOUR'
      
      write(14,'(a,a,a)') '%subtitle="',didasc(1:lenstr(didasc)),'"'
C
      IF(NLEV.GT.NLEV1-1)   THEN
       WRITE(6,300)
       STOP
      END IF
C
C   limits for color scale MUST BE in ascending order
      Z1=MIN(ZMIN,ZMAX)
      Z2=MAX(ZMIN,ZMAX)
      ZSTEP=ABS(ZINC)
      if (revers.gt.0.5) then
       write(14,'(a,f9.2)') '%cmin= ',z1
       write(14,'(a,f9.2)') '%cmax= ',z2
       write(14,'(a,f9.2)') '%cstep= ',zstep
      else
       write(14,'(a,f9.2)') '%cmin= ',-z2
       write(14,'(a,f9.2)') '%cmax= ',-z1
       write(14,'(a,f9.2)') '%cstep= ',zstep
      end if
      CALL UNDEF(Z,NX,NY)
 
      write(14,'(a,f9.2)') '%xmin= ',x1grid*divx
      write(14,'(a,f9.2)') '%xmax= ',xlgrid*divx

      IF  (OPTION(1:5).EQ.'CONFR' )   THEN
       if (y1grid.gt.ylgrid) then
        write(14,'(a,f9.2)') '%ymin= ',ylgrid*divy*alog10(2.0)
        write(14,'(a,f9.2)') '%ymax= ',y1grid*divy*alog10(2.0)
        write(14,'(a)') '%yflip=T'
       else
        write(14,'(a,f9.2)') '%ymin= ',y1grid*divy*alog10(2.0)
        write(14,'(a,f9.2)') '%ymax= ',ylgrid*divy*alog10(2.0)
       end if
      else             
       if (y1grid.gt.ylgrid) then
        write(14,'(a,f9.2)') '%ymin= ',ylgrid*divy
        write(14,'(a,f9.2)') '%ymax= ',y1grid*divy
        write(14,'(a)') '%yflip=T'
       else
        write(14,'(a,f9.2)') '%ymin= ',y1grid*divy
        write(14,'(a,f9.2)') '%ymax= ',ylgrid*divy
       end if
      end if

      write(14,'(a,i4)') '%nx= ',nx
      write(14,'(a,i4)') '%ny= ',ny
      
       IF(LINEP .NE. 'LIN')   THEN
        write(14,'(a)') '%contfill=T'
       END IF
c >>> plot layout parameters
      xyratio=abs(((ylgrid-y1grid)*xscale)/((xlgrid-x1grid)*yscale))
      write(14,'(a)') '%equalscale=F'
      write(14,'(a)') '%fitpage=F'
      write(14,'(a,f8.2)') '%xyratio=',xyratio


      write(14,'(a,a,a)') '%toplabel="',title(1:lenstr(title)),'"'
      write(14,'(a,a,a)') '%xlabel="',titlex(1:lenstr(titlex)),'"'
      IF  (OPTION(1:5).EQ.'CONFR' )   THEN
       write(14,'(a,a,a)') '%ylabel="log(f)"'
      else
       write(14,'(a,a,a)') '%ylabel="',titley(1:lenstr(titley)),'"'
      end if
      IF(YBTYPE.EQ.'LOG')   THEN
        write(14,'(a)') '%logy=T'
      END IF
      IF(XBTYPE.EQ.'LOG')   THEN
        write(14,'(a)') '%logx=T'
      END IF

c
c >>> Bottom shading
c
 1100 CONTINUE
      CALL BOTTOM(*1150)
      CALL WINDOW
      IF(NPSH.LE.2)   GO TO 1100
      do i=1,npsh
       xf(i)=uf(i)*divx
       yf(i)=vf(i)*divy
      end do
      xf(npsh+1)=xf(1)
      yf(npsh+1)=yf(1)
      write(6,*) 'Number of points:',npsh
      do i=1,npsh
       write(14,'(a,4(g10.3,a))') '@line x1=',xf( i ),' y1=',yf( i ),
     &                            ' x2=',xf(i+1),' y2=',yf(i+1),
     &                            ' linetype=1 linewidth=2 linecolor=0'  
      end do
      go to 1100
 1150 CONTINUE

      IF( ((OPTION(1:5).EQ.'CONDR')    .OR.
     &     (OPTION(1:5).EQ.'IFD  ')    .OR.
     &     (OPTION(1:5).EQ.'EXPDR')    .OR.
     &     (OPTION(1:5).EQ.'PAREQ')    .OR.
     &     (OPTION(1:5).EQ.'TDPEN'))   .AND.
     &     (FOM.LT.1.0 .OR. SD.GT.0.0) .AND.
     &     (SDFLAG .LT. 1.0) )                THEN
       IF(SD.GE.MIN(YUP,YDOWN).AND.SD.LE.MAX(YUP,YDOWN) )   THEN
        write(14,'(a,f9.3,a,f9.3,a)') '@point x1=',xleft*divx,
     &        ' y1=',sd,' markertype=12 markersize=5'
       END IF
      END IF
      if (y1grid.lt.ylgrid) then
       do iy=1,ny
        do ix=1,nx  
         if (revers.gt.0.5) then
          write(14,*) z(ix+(iy-1)*nx)
         else
          write(14,*) -z(ix+(iy-1)*nx)
         end if
        end do
       end do
      else
       do iy=ny,1,-1
        do ix=1,nx  
         if (revers.gt.0.5) then
          write(14,*) z(ix+(iy-1)*nx)
         else
          write(14,*) -z(ix+(iy-1)*nx)
         end if
        end do
       end do
      end if


      close(14)
c
c make plot
c
    
      if (device.eq.'VTT'.and. WDW.eq.'DCW') then
       if (BWCOL.eq.'B/W') then
        call system('plotmtv -bg white -fg black -nodate '//
     &              filenm(1:lenstr(filenm)))
       else
        call system('plotmtv -bg white -fg black -nodate -colorps '//
     &              filenm(1:lenstr(filenm)))
       end if 
      else if (device.eq.'LAS') then
       if (BWCOL.eq.'B/W') then
        call system(
     &    'plotmtv -bg white -fg black -landscape -noxplot -print '//
     &              filenm(1:lenstr(filenm)))
       else
c        write(psname,101) char(65+nrofpl),getpid()
        psname=filenm(1:(lenstr(filenm)-3))//'cps'
        call system(
     &   'plotmtv -bg white -fg black -landscape -noxplot -colorps '//
     &              filenm(1:lenstr(filenm)))
        call system('mv dataplot.ps '//psname)
        write(6,*)
        write(6,'(1H ,a,a)') 'Saved in Postscript file: ',psname
       end if
      else if (device.eq.'PSL') then
c        write(psname,102) char(65+nrofpl),getpid()
       if (BWCOL.eq.'B/W') then
        psname=filenm(1:(lenstr(filenm)-3))//'ps'
        call system(
     &   'plotmtv -bg white -fg black -nodate -landscape -noxplot '//
     &              filenm(1:lenstr(filenm)))
        call system('mv dataplot.ps '//psname)
        write(6,*)
        write(6,'(1H ,a,a)') 'Saved in Postscript file: ',psname
       else
        psname=filenm(1:(lenstr(filenm)-3))//'cps'
        call system('plotmtv -bg white -fg black -nodate '//
     &              ' -landscape -noxplot -colorps'//
     &              filenm(1:lenstr(filenm)))
        call system('mv dataplot.ps '//psname)
        write(6,*)
        write(6,'(1H ,a,a)') 'Saved in Postscript file: ',psname
       end if
      else if (device.eq.'PSP') then
c        write(psname,102) char(65+nrofpl),getpid()
       if (BWCOL.eq.'B/W') then
        psname=filenm(1:(lenstr(filenm)-3))//'ps'
        call system('plotmtv -bg white -fg black -nodate -noxplot '//
     &              filenm(1:lenstr(filenm)))
        call system('mv dataplot.ps '//psname)
        write(6,*)
        write(6,'(1H ,a,a)') 'Saved in Postscript file: ',psname
       else
        psname=filenm(1:(lenstr(filenm)-3))//'cps'
        call system('plotmtv -bg white -fg black -nodate '//
     &              '-noxplot -colorps'//
     &              filenm(1:lenstr(filenm)))
        call system('mv dataplot.ps '//psname)
        write(6,*)
        write(6,'(1H ,a,a)') 'Saved in Postscript file: ',psname
       end if
      end if
 101  format('cps.',A1,I5.5)
 102  format('ps.',A1,I5.5)

      RETURN
      END



      INTEGER FUNCTION LENSTR(STR)
      CHARACTER*(*) STR
      DO 10 I=LEN(STR),1,-1
       IF (STR(I:I).NE.' ') THEN
        LENSTR=I
        RETURN
       END IF
 10   CONTINUE
      LENSTR=0
      RETURN
      END
