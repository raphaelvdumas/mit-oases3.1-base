      SUBROUTINE MAINMAT(Z,FREQ,SD,RD,OPTION,DEVICE,
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
      character*80 buf

      REAL ZBUF(NPXYZ)
      DIMENSION KOLORS(NCLZP1),ZCLASS(NCLZ),Z(1)
      REAL TRPAX(4), TRPAY(4)
      real matbuf(100000)
      integer idim(3)
      integer ndim
      integer padflag, rem

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
     & 'TERMINATED - ERROR IN SUB. MAINMAT',/)
  400 FORMAT(A4)
  500 FORMAT(1X,/,'  TO CLOSE THE GRAPHIC WINDOW ASSIGN THE INPUT FOCUS',
     & /,'   TO THE WINDOW AND PRESS RETURN ')
  600 FORMAT(1X,/,' *** WARNING: BOTTOM SHADING NOT ALLOWED WITH',
     & ' VIEWGRAPHS *** ',/) 
      ratio=1.0
C
      if (nrofpl .gt. 0) then
       nrofpl = nrofpl-1
      end if

      luncdr=55
      WRITE(ENVVAR,'(A3,I3.3)') 'FOR',luncdr
      CALL GETENV(ENVVAR,FILENM)
      IF (FILENM.EQ.' ') THEN
       filenm='cplot'//'_'//char(65+nrofpl)//'.mat'
        nrofpl=nrofpl+1
      ELSE
       filenm=filenm(1:(lenstr(filenm)-4))//'_'//char(65+nrofpl)//
     &                                      '.mat'
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
c
c Make mat file
c
      padflag = 0
      call matopen(filenm,lenstr(filenm))
c Plot type
       call matcsetup(5,padflag,rem,6,"OPTION")
       call matcwrite(option,5,padflag,rem)      
c title
       call matcsetup(lenstr(title),padflag,rem,5,"TITLE")
       call matcwrite(title,lenstr(title),padflag,rem)      
c subtitle
       call matcsetup(lenstr(didasc),padflag,rem,8,"SUBTITLE")
       call matcwrite(didasc,lenstr(didasc),padflag,rem)      
C xlabel
       call matcsetup(lenstr(titlex),padflag,rem,6,"TITLEX")
       call matcwrite(titlex,lenstr(titlex),padflag,rem)      
C ylabel
       call matcsetup(lenstr(titley),padflag,rem,6,"TITLEY")
       call matcwrite(titley,lenstr(titley),padflag,rem)      

      IF(NLEV.GT.NLEV1-1)   THEN
       WRITE(6,300)
       STOP
      END IF
C
C   limits for color scale MUST BE in ascending order
      Z1=MIN(ZMIN,ZMAX)
      Z2=MAX(ZMIN,ZMAX)
      ZSTEP=ABS(ZINC)

      ndim = 2
      idim(1) = 1
      idim(2) = 1
       call matsetup(ndim,idim,padflag,4,"CMIN")
       call matwrite(z1,1,padflag)
       call matsetup(ndim,idim,padflag,4,"CMAX")
       call matwrite(z2,1,padflag)
       call matsetup(ndim,idim,padflag,5,"CSTEP")
       call matwrite(z1,1,padflag)

      CALL UNDEF(Z,NX,NY)
 
       call matsetup(ndim,idim,padflag,4,"XMIN")
       call matwrite(x1grid*divx,1,padflag)
       call matsetup(ndim,idim,padflag,4,"XMAX")
       call matwrite(xlgrid*divx,1,padflag)

       call matsetup(ndim,idim,padflag,6,"REVERS")
       call matwrite(revers,1,padflag)

      IF  (OPTION(1:5).EQ.'CONFR' )   THEN
         call matsetup(ndim,idim,padflag,4,"YMIN")
         call matwrite(y1grid*divy*alog10(2.0),1,padflag)
         call matsetup(ndim,idim,padflag,4,"YMAX")
         call matwrite(ylgrid*divy*alog10(2.0),1,padflag)
      else             
         call matsetup(ndim,idim,padflag,4,"YMIN")
         call matwrite(y1grid*divy,1,padflag)
         call matsetup(ndim,idim,padflag,4,"YMAX")
         call matwrite(ylgrid*divy,1,padflag)
      end if

         call matsetup(ndim,idim,padflag,2,"NX")
         call matwrite(real(nx),1,padflag)
         call matsetup(ndim,idim,padflag,2,"NY")
         call matwrite(real(ny),1,padflag)
      
c >>> plot layout parameters
      xyratio=abs(((ylgrid-y1grid)*xscale)/((xlgrid-x1grid)*yscale))
       call matsetup(ndim,idim,padflag,7,"XYRATIO")
       call matwrite(xyratio,1,padflag)


c
c >>> Bottom shading
c     disabled
      goto 1150
      ibot=0
 1100 CONTINUE
      CALL BOTTOM(*1150)
      CALL WINDOW
      IF(NPSH.LE.2)   GO TO 1100
      ibot=ibot+1
      do i=1,npsh
       xf(i)=uf(i)*divx
       yf(i)=vf(i)*divy
      end do
      xf(npsh+1)=xf(1)
      yf(npsh+1)=yf(1)
      idim(2) = npsh+1
      write(buf,'(a,i2.2)') "SHADE_X_",ibot
       call matsetup(ndim,idim,padflag,10,buf)
       call matwrite(xf,idim(2),padflag)
      write(buf,'(a,i2.2)') "SHADE_Y_",ibot
       call matsetup(ndim,idim,padflag,10,buf)
       call matwrite(yf,idim(2),padflag)
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
         idim(2)=1
         call matsetup(ndim,idim,padflag,2,"SD")
         call matwrite(SD,1,padflag)
       END IF
      END IF
      idim(1)=nx
      idim(2)=ny
      call matsetup(ndim,idim,padflag,4,"DATA")
      do iy=1,ny-1
       do ix=1,nx
         matbuf(ix) = z(ix+(iy-1)*nx)
       end do
       call matwrite(matbuf,nx,0)
      end do        
       do ix=1,nx
         matbuf(ix) = z(ix+(ny-1)*nx)
       end do
       call matwrite(matbuf,nx,1)
               
      RETURN
      END




