      SUBROUTINE PLINTGR(DLWVNL,WK0L,SD,RD,TITLE,XLEN,YLEN)                 
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'complo.f'
      DIMENSION YMAX(npar)               
      CHARACTER*80 TITLE
      CHARACTER*6 OPTION(2)
      character optpar(12)
      DATA optpar(1) /'N'/,
     &     optpar(2) /'W'/,
     &     optpar(3) /'U'/,
     &     optpar(4) /'V'/,
     &     optpar(5) /'R'/,
     &     optpar(6) /'B'/,
     &     optpar(7) /'S'/
      OPTION(1)=PROGNM

      iou4=iout(4)
      if (determ) then
       call vclr(cff(1,4),1,2*nwvno)
       call vmov(arg(1),1,cff(1,4),2,nwvno)
       iout(4)=1
      end if

      DO 2400 I=1,NPAR                   
      IF (IOUT(I).EQ.1) THEN
        CALL CVMAX(CFF(1,I),2,YMAX(I),NWVNO)   
      END IF
 2400  CONTINUE  
C                
C XAXIS DEFINITION     
C
      IPLOT1=MAX(1,ICUT1-100)
      IPLOT2=MIN(NWVNO,ICUT2+100)
      WKMIN=WK0L+(IPLOT1-1)*DLWVNL
      NN=IPLOT2-IPLOT1+1
      XMAX=(WK0L+(NWVNO-1)*DLWVNL)
      XMIN=WK0L   
      CALL AUTOAX(XMIN,XMAX,XLEFT,XRIGHT,XINC,XDIV,NXDIF)
C
      DO 2701 I=1,npar                   
      IF (IOUT(I).EQ.0) GO TO 2701    
      IF (YMAX(I).LE.0) YMAX(I)=1E0
      IPLOT1=MAX(1,ICUT1-100)
      IPLOT2=MIN(NWVNO,ICUT2+100)
C                
      OPTION(2)=optpar(I)//'INTGR'
C                
C  YAXIS DEFINITION                   
C                
      YMIN=0.0   
      YMAX(I)=SQRT(YMAX(I))           
      CALL AUTOAX(YMIN,YMAX(I),YLO,YUP,YINC,YDIV,NYDIF)
      IF(IPLOT1.EQ.2)IPLOT1=1         
c *** labels
      NLAB=3
      if (FREQ.GE.1e6) then
         WRITE(LAB(1),8102) FREQ*1e-6
      else if (FREQ.GE.1e3) then
         WRITE(LAB(1),8101) FREQ*1e-3
      else
         WRITE(LAB(1),810) FREQ
      end if
      WRITE(LAB(2),811) SD
      WRITE(LAB(3),812) RD
 810  FORMAT('Freq:',F7.1,' Hz$')
 8101 FORMAT('Freq:',F6.1,' kHz$')
 8102 FORMAT('Freq:',F6.1,' MHz$')
 811  FORMAT('SD:',F9.1,' m$')
 812  FORMAT('RD:',F9.1,' m$')
      if (i.eq.4.and.determ) then
       ptit='DETERMINANT'
      else
       PTIT='INTEGRAND'
      end if
      WRITE(XTXT,820) NXDIF
 820  FORMAT('Horizontal wavenumber (10**',I3,')$')
      WRITE(YTXT,821) NYDIF
 821  FORMAT('Modulus (10**',I3,')$')
      XTYP='LIN'
      YTYP='LIN'
      IGRID=0
      NC=1
C *** WRITE PLP FILE
      CALL PLPWRI(OPTION,PTIT,TITLE,NLAB,LAB,XLEN,YLEN,
     &                  IGRID,XLEFT,XRIGHT,XINC,XDIV,XTXT,XTYP,
     &                  YLO,YUP,YINC,YDIV,YTXT,YTYP,NC)
      CALL CVMAGS(CFF(IPLOT1,I),2,CFFS(1),1,NN)
      CALL VSQRT(CFFS(1),1,CFFS(1),1,NN)
      CALL PLTWRI(NN,WKMIN,DLWVNL,0.,0.,CFFS(1),1,CFFS(1),1)
 2701 CONTINUE
C                
      RETURN     
C                
      END        
      SUBROUTINE PLINLOG(DLWVNL,WK0L,SD,RD,TITLE,XLEN,YLEN)                 
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'complo.f'
      DIMENSION YMAX(npar)               
      CHARACTER*80 TITLE
      CHARACTER*6 OPTION(2)
      character optpar(12)
      DATA optpar(1) /'N'/,
     &     optpar(2) /'W'/,
     &     optpar(3) /'U'/,
     &     optpar(4) /'V'/,
     &     optpar(5) /'R'/,
     &     optpar(6) /'B'/,
     &     optpar(7) /'S'/
      OPTION(1)=PROGNM

      DO 2400 I=1,NPAR                   
      IF (IOUT(I).EQ.1) THEN
        CALL CVMAX(CFF(1,I),2,YMAX(I),NWVNO)   
      END IF
 2400  CONTINUE  
C                
C XAXIS DEFINITION     
C
      IPLOT1=MAX(1,ICUT1-100)
      IPLOT2=MIN(NWVNO,ICUT2+100)
      WKMIN=WK0L+(IPLOT1-1)*DLWVNL
      NN=IPLOT2-IPLOT1+1
      XMAX=(WK0L+(NWVNO-1)*DLWVNL)
      XMIN=WK0L   
      CALL AUTOAX(XMIN,XMAX,XLEFT,XRIGHT,XINC,XDIV,NXDIF)
C
      DO 2701 I=1,NPAR                   
      IF (IOUT(I).EQ.0) GO TO 2701    
      IF (YMAX(I).LE.0) YMAX(I)=1E0
      IPLOT1=MAX(1,ICUT1-100)
      IPLOT2=MIN(NWVNO,ICUT2+100)
C                
      OPTION(2)=optpar(I)//'INTLG'
C                
C  YAXIS DEFINITION                   
C                
      YMAX(I)=10.0*LOG10(YMAX(I))           
      YMIN=YMAX(I)-100.   
      CALL AUTOAX(YMIN,YMAX(I),YLO,YUP,YINC,YDIV,NYDIF)
      YDIV=1E0
      NYDIF=0
      IF(IPLOT1.EQ.2)IPLOT1=1         
c *** labels
      NLAB=3
      if (FREQ.GE.1e6) then
         WRITE(LAB(1),8102) FREQ*1e-6
      else if (FREQ.GE.1e3) then
         WRITE(LAB(1),8101) FREQ*1e-3
      else
         WRITE(LAB(1),810) FREQ
      end if
      WRITE(LAB(2),811) SD
      WRITE(LAB(3),812) RD
 810  FORMAT('Freq:',F7.1,' Hz$')
 8101 FORMAT('Freq:',F6.1,' kHz$')
 8102 FORMAT('Freq:',F6.1,' MHz$')
 811  FORMAT('SD:',F9.1,' m$')
 812  FORMAT('RD:',F9.1,' m$')
      PTIT='WAVENUMBER KERNEL'
      WRITE(XTXT,820) NXDIF
 820  FORMAT('Horizontal wavenumber (10**',I3,')$')
      WRITE(YTXT,821) 
 821  FORMAT('Power (dB)$')
      XTYP='LIN'
      YTYP='LIN'
      IGRID=0
      NC=1
C *** WRITE PLP FILE
      CALL PLPWRI(OPTION,PTIT,TITLE,NLAB,LAB,XLEN,YLEN,
     &                  IGRID,XLEFT,XRIGHT,XINC,XDIV,XTXT,XTYP,
     &                  YLO,YUP,YINC,YDIV,YTXT,YTYP,NC)
      CALL CVMAGS(CFF(IPLOT1,I),2,CFFS(1),1,NN)
      CALL VCLIP(CFFS,1,1E-35,1E35,CFFS,1,NN)
      CALL VALG10(CFFS(1),1,CFFS(1),1,NN)
      CALL VSMUL(CFFS,1,1E1,CFFS,1,NN)
      CALL PLTWRI(NN,WKMIN,DLWVNL,0.,0.,CFFS(1),1,CFFS(1),1)
 2701 CONTINUE
C                
      RETURN     
C                
      END        
      SUBROUTINE PLOGLOG(WK0L,DLWVNL,NWL,RD,TITLE,xtit,ytit,XLEN,YLEN)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'complo.f'

      CHARACTER*80 TITLE,xtit,ytit
      CHARACTER*6 OPTION(2)
      OPTION(1)=PROGNM

        CALL CVMAX(CFF(1,1),2,YMAX,NWL)
        ymax=sqrt(YMAX)   
        IF (YMAX.LE.0) YMAX=1E0
C                
C XAXIS DEFINITION     
C
      XMAX=log10(WK0L+(NWL-1)*DLWVNL)
      XMIN=log10(wk0L)   
      CALL AUTOAX(XMIN,XMAX,XLEFT,XRIGHT,XINC,XDIV,NXDIF)
      XDIV=1E0
      NXDIF=0
C
      OPTION(2)=' POWER'
C                
C  YAXIS DEFINITION                   
C                
      YMAX=LOG10(YMAX)           
      YMIN=YMAX-6.   
      CALL AUTOAX(YMIN,YMAX,YLO,YUP,YINC,YDIV,NYDIF)
      YDIV=1E0
      NYDIF=0
c *** labels
      NLAB=1
      WRITE(LAB(1),812) RD
 812  FORMAT('D =',F7.1,' m$')
      PTIT='POWER SPECTRUM'
      XTYP='LIN'
      XTXT=xtit(1:lenstr(xtit))//'$'
      YTYP='LIN'
      YTXT=ytit(1:lenstr(ytit))//'$'
      IGRID=1
      NC=1
C *** WRITE PLP FILE
      CALL PLPWRI(OPTION,PTIT,TITLE,NLAB,LAB,XLEN,YLEN,
     &                  IGRID,XLEFT,XRIGHT,XINC,XDIV,XTXT,XTYP,
     &                  YLO,YUP,YINC,YDIV,YTXT,YTYP,NC)
      CALL CVMAGS(CFF(1,1),2,CFFS(1),1,Nwl)
      CALL VCLIP(CFFS,1,1E-35,1E35,CFFS,1,Nwl)
      CALL VALG10(CFFS(1),1,CFFS(1),1,Nwl)
      call vsmul(cffs(1),1,5E-1,cffs(1),1,nwl)

      call vramp(wk0l,dlwvnl,cff(1,2),1,nwl)
      CALL VALG10(CFF(1,2),1,CFF(1,2),1,Nwl)
      CALL PLTWRI(Nwl,0.,0.,0.,0.,CFF(1,2),1,CFFS(1),1)
      RETURN     
      END        

      SUBROUTINE PREQV(NUML1,NUMI1)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'

      WRITE(21,*) 'NUML=',NUML,'  (',NUML1,')'
      WRITE(21,*) 'NUMI=',NUMI,'  (',NUMI1,')'
      WRITE(21,*) 'NEQ= ',NEQ
      WRITE(21,*) 'NNA= ',NNA
      WRITE(21,*) 'IBW= ',IBW
      WRITE(21,*) 'IRHCOL= ',IRHCOL
      WRITE(21,*) 'NNB= ',NNB
      WRITE(21,*) 'EPS= ',EPS
      WRITE(21,100) 
 100  FORMAT(1H ,' I   ISTART IRST ICST NCL  NRI')
      DO 10 I=1,NUML
 10   WRITE(21,*) I,ISTART(I),IRST(I),ICST(I),
     1              NCL(I),NRI(I)
      WRITE(21,300) 
 300  FORMAT(1H0,' J  ICP IDP')
      DO 20 J=1,NEQ+1
 20   WRITE(21,*) J,ICP(J),IDP(J),RHS(J)
      WRITE(21,*) 'K     IRN(K)'
      DO 30 K=1,NNA
 30   WRITE(21,*) K,IRN(K)
      WRITE(21,888) 'INDA=',(INDA(J),J=1,NNA)
      WRITE(21,888) 'INDR=',(INDR(J),J=1,NEQ)
      WRITE(21,888) 'INDS=',(INDS(J),J=1,NEQ)
      WRITE(21,888) 'INDB=',(INDB(J),J=1,NNA)
C888  FORMAT(1H ,A6,(6F6.1))
 888  FORMAT(1H ,A6,(6I6))
      RETURN
      END
       SUBROUTINE CONDRB(NP1,NP2,NPX,PX)                   
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
       DIMENSION PX(1)                
       DO 1000 I=NP1,NP2              
       PX(I-NP1+1)=PX(I)              
1000   CONTINUE  
       WRITE(29,444) (PX(L),L=1,NPX)
 444  FORMAT(1H ,6G13.5)
       RETURN    
       END       
      SUBROUTINE PLPROF(TITLE,XLEN,YLEN,XLEFT,XRIGHT,XINC,
     2                  YUP,YDOWN,YINC)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'complo.f'
      DIMENSION VEL(NLA10),DEP(NLA10)
      CHARACTER*80 TITLE
      CHARACTER*6 OPTION(2),OPT2(NPAR)
      OPTION(1)=PROGNM
      OPTION(2)='PROFIL'
      PTIT='VELOCITY PROFILE'
      NLAB=0
      XTXT='Velocity (m/s)$'
      XTYP='LIN'
      XDIV=1
      YTXT='Depth (m)$'
      YTYP='LIN'
      YDIV=1 
      IGRID=0
      NC=2
C *** WRITE PLP FILE
      CALL PLPWRI(OPTION,PTIT,TITLE,NLAB,LAB,XLEN,YLEN,
     &                  IGRID,XLEFT,XRIGHT,XINC,XDIV,XTXT,XTYP,
     &                  YDOWN,YUP,YINC,YDIV,YTXT,YTYP,NC)
      VMAX=MAX(XLEFT,XRIGHT)
      VMIN=MIN(XLEFT,XRIGHT)
      CALL RECEIV(V,NUML,YUP,LAYU,ZZ)
      CALL RECEIV(V,NUML,YDOWN,LAYD,ZZ)
      LL=MIN0(LAYD,LAYU)
      LU=MAX0(LAYD,LAYU)
C
C     INTERPOLATE FOR NON-ISOVELOCITY LAYERS
C
      LF=0
      DO 10 L=LL,LU
C
C     UPPER HALFSPACE
C
        IF (L.EQ.1) THEN
          LF=LF+2
          VEL(LF-1)=V(L,2)
          VEL(LF)  =V(L,2)
          DEP(LF-1)=MIN(YUP,YDOWN)
          DEP(LF)  =V(L+1,1)
C
C     LAYERS
C
        ELSE IF (L.LT.NUML) THEN
          IF (V(L,3).GT.-1E-10) THEN
            LF=LF+2
            VEL(LF-1)=V(L,2)
            VEL(LF)  =V(L,2)
            DEP(LF-1)=V(L,1)
            DEP(LF)  =V(L+1,1)
          ELSE
            B=1E0/(V(L,2)**2)
            A=(1E0/(V(L,3)**2)-B)/NDV
            DELT=(V(L+1,1)-V(L,1))/NDV
            DO 11 J=0,NDV
              LF=LF+1
              VEL(LF)=SQRT(1E0/(A*J+B))
              DEP(LF)=V(L,1)+DELT*J
 11         CONTINUE
          END IF
C
C     LOWER HALF SPACE
C
        ELSE
          LF=LF+2
          VEL(LF-1)=V(L,2)
          VEL(LF)  =V(L,2)
          DEP(LF-1)=V(L,1)
          DEP(LF)  =MAX(YUP,YDOWN)
        END IF
 10   CONTINUE
      CALL PLTWRI(LF,0.,0.,0.,0.,VEL,1,DEP,1)
C
C     SHEAR SPEED PROFILE
C
      LF=2*(IABS(LAYD-LAYU)+1)
      DO 30 I=LL,LU
      IF (V(I,3).GT.0) THEN
       VEL(2*(I-LL+1)-1)=V(I,3)
       VEL(2*(I-LL+1))  =V(I,3)
      ELSE
       VEL(2*(I-LL+1)-1)=0
       VEL(2*(I-LL+1))  =0
      END IF
 30   CONTINUE
      DO 40 I=LL+1,LU-1
      DEP(2*(I-LL+1)-1)=V(I,1)
      DEP(2*(I-LL+1))  =V(I+1,1)
 40   CONTINUE
      DEP(2)=V(LL+1,1)
      DEP(LF-1)=V(LU,1)
      DEP(1)=MIN(YUP,YDOWN)
      DEP(LF)=MAX(YUP,YDOWN)
      CALL PLTWRI(LF,0.,0.,0.,0.,VEL,1,DEP,1)
      RETURN
      END
      SUBROUTINE INTCON(TITLE,NPX,NPY,NX,NY,XLEFT,XRIGHT 
     $,XSCALE,XINC,YUP,YDOWN,YSCALE,YINC,ZMIN            
     $,ZMAX,ZSTEP,FREQ,SD,RECUP,RECLO,X1,XL,IPARM,IXATYP,IYATYP)        
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      DIMENSION SECTOR(28)                   
      CHARACTER*50 FILENM
      character*3 parc(12)
      CHARACTER*4 TITLE(20)
      CHARACTER*80 TITLEX,TITLEY    
      DATA X1PL,Y1PL/2.,2.0/,HGTPT,HGTC,LABPT,NDIV,      
     *NARC/0.1,0.14,-3,1,5/,LABC,LWGT/-1,-1/,NSM/0/       
      DATA DUMMY /0./
      DATA PARC(1)   /'PRS'/,
     &     parc(2)   /'VER'/,
     &     parc(3)   /'HOR'/
      IF (IYATYP.EQ.1) THEN
        TITLEY='Frequency (Hz)'
      ELSE 
        TITLEY='Depth (m)'  
      END IF
      IF (IXATYP.EQ.1) THEN
        TITLEX='Slowness (s/km)'
      ELSE
        TITLEX='Wavenumber (km^-1)'
      END IF
C          
C   FORMATS
 401  FORMAT(1H ,F15.4,3X,'  NUMBER OF DATA POINTS ALONG THE X AXIS')           
 402  FORMAT(1H ,F15.4,3X,'  NUMBER OF DATA POINTS ALONG THE Y AXIS')           
403   FORMAT(1H ,F15.4,3X,'  DIVX ' )                   
404   FORMAT(1H ,F15.4,3X,'  DIVY ' )                   
405   FORMAT(1H ,F15.4,3X,'  FLAGRC ' )                   
406   FORMAT(1H ,F15.4,3X,'  RDUP ' )                   
407   FORMAT(1H ,F15.4,3X,'  RDLO ' )         
408   FORMAT(1H ,F15.4,3X,'  SOURCE DEPTH (M) ' )        
 409  FORMAT(1H ,F15.4,3X,'  NUMBER OF GRID POINTS ALONG THE X AXIS ' )         
 410  FORMAT(1H ,F15.4,3X,'  NUMBER OF GRID POINTS ALONG THE Y AXIS ' )         
  411 FORMAT(1H ,F15.4,3X,'  FREQUENCY (HZ)' )           
  412 FORMAT(1H ,F15.4,3X,'  DUMMY ' )                   
  413 FORMAT(1H ,F15.4,3X,'  CAY ' )                   
  414 FORMAT(1H ,F15.4,3X,'  NRNG ' )                   
  415 FORMAT(1H ,F15.4,3X,'  ZMIN ' )                    
  416 FORMAT(1H ,F15.4,3X,'  ZMAX ' )                    
  417 FORMAT(1H ,F15.4,3X,'  ZINC ' )                    
  418 FORMAT(1H ,F15.4,3X,'  X ORIGIN OF PLOT IN INCHES ' )                     
  419 FORMAT(1H ,F15.4,3X,'  DUMMY ' )                   
  420 FORMAT(1H ,F15.4,3X,'  Y ORIGIN OF PLOT IN INCHES ' )                     
  421 FORMAT(1H ,F15.4,3X,'  NSM   ' )                   
  422 FORMAT(1H ,F15.4,3X,'  HGTPT ' )                   
  423 FORMAT(1H ,F15.4,3X,'  HGTC ' )                    
  424 FORMAT(1H ,F15.4,3X,'  LABPT ' )                   
  425 FORMAT(1H ,F15.4,3X,'  NDIV ' )                    
  426 FORMAT(1H ,F15.4,3X,'  NARC ' )                    
  427 FORMAT(1H ,F15.4,3X,'  LABC ' )                    
  428 FORMAT(1H ,F15.4,3X,'  LWGT ' )                    
  800 FORMAT('CONDR,',A3,',FMT,REV')         
 801  FORMAT(A50)            
  850 FORMAT(20A4)
  860 FORMAT(A80)                
  900 FORMAT(1X,F15.4,3X,'  XLEFT',/,F15.4,4X,'  XRIGHT',/,F15.4,3X,            
     *'   XSCALE',/,F15.4,4X,'  XINC')                   
  901 FORMAT(1X,F15.4,3X,'  YUP',/,F15.4,4X,'  YDOWN',/,F15.4,3X,               
     *'   YSCALE',/,F15.4,4X,'  YINC')                   
  950 FORMAT(1H ,F15.4,1X,'    RMIN',/,F15.4,2X,'    RMAX')                     
      WRITE(28,800) PARC(IPARM)         
      WRITE(28,850)TITLE          
      CALL VCLR(SECTOR,1,28)
      SECTOR(1)=NPX               
C          
C   SECTOR(4) IS A FLAG WHICH IS SET TO ZERO IN THE RANGE
C   DEPENDENT VERSION OF SNAP FOR ALL SECTORS EXCEPT THE LAST                   
C   ONE. HERE IS USED TO INDICATE THAT THIS IS THE LAST SECTOR                  
       SECTOR(4)=1.0              
      WRITE(29,444) (SECTOR(L),L=1,28)
 444  FORMAT(1H ,6G13.5)
      INQUIRE(UNIT=29,NAME=FILENM)
      WRITE(28,801) FILENM         
      DIVX=1E0
      DIVY=1E0
      CAY=5.
      NRNG=5
      FLAGRC=0.
      WRITE(28,860)TITLEX         
      R1=X1*1.0E3                 
      R2=XL*1.0E3                 
      WRITE(28,950)R1,R2          
      AX1=XLEFT*1.0E3             
      AX2=XRIGHT*1.0E3            
      AX3=XSCALE*1.0E3            
      AX4=XINC*1.0E3              
      WRITE(28,900)AX1,AX2,AX3,AX4
      WRITE(28,860)TITLEY         
      WRITE(28,901)YUP,YDOWN,YSCALE,YINC                 
      WRITE(28,401)FLOAT(NPX)
      WRITE(28,402)FLOAT(NPY)
      WRITE(28,403)DIVX          
      WRITE(28,404)DIVY          
      WRITE(28,405)FLAGRC          
      WRITE(28,406)RECUP          
      WRITE(28,407)RECLO                 
      WRITE(28,408)SD 
C   NUMBER OF GRID POINTS ALONG THE X AXIS               
      WRITE(28,409)FLOAT(NX)      
C   NUMBER OF GRID POINTS ALONG THE Y AXIS               
      WRITE(28,410)FLOAT(NY)      
      WRITE(28,411)FREQ  
      WRITE(28,412)DUMMY          
      WRITE(28,413)CAY          
      WRITE(28,414)FLOAT(NRNG)          
      WRITE(28,415)ZMIN           
      WRITE(28,416)ZMAX           
      WRITE(28,417)ZSTEP          
C X ORIGIN  OF PLOT IN INCHES     
      WRITE(28,418)X1PL           
      WRITE(28,419)DUMMY          
C Y ORIGIN  OF PLOT IN INCHES     
      WRITE(28,420)Y1PL           
      WRITE(28,421)FLOAT(NSM)            
      WRITE(28,422)HGTPT          
      WRITE(28,423)HGTC           
      WRITE(28,424)FLOAT(LABPT)   
      WRITE(28,425)FLOAT(NDIV)    
      WRITE(28,426)FLOAT(NARC)    
      WRITE(28,427)FLOAT(LABC)    
      WRITE(28,428)FLOAT(LWGT)    
      RETURN                      
      END  
      SUBROUTINE AUTOAX(XMIN,XMAX,XLEFT,XRIGHT,XINC,XDIV,NXDIF)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
C *** DETERMINE FACTOR
      ILOG=INT(LOG10(XMAX-XMIN))
      IF ((XMAX-XMIN).LT.1.0) ILOG=ILOG-1
      IFAC=INT((XMAX-XMIN)/10.**ILOG)+1
      NXDIF=ILOG-1
      XDIV=10.**(-NXDIF)
C *** MAKE NICE AXIS
      XFAC=IFAC
      IF (IFAC.EQ.1) XFAC=5
      IF (IFAC.EQ.2) XFAC=4
      IF (IFAC.GT.5) XFAC=IFAC/2.0
      XINC=IFAC*10.**ILOG/XFAC
      IF (XMIN.GE.0E0) THEN
       XLEFT=INT(XMIN/XINC+0.01)*XINC
      ELSE
       XLEFT=-INT(-XMIN/XINC+0.99)*XINC
      END IF
      IF (XMAX.GE.0E0) THEN
       XRIGHT=INT(XMAX/XINC+0.99)*XINC
      ELSE
       XRIGHT=-INT(-XMAX/XINC+0.01)*XINC
      END IF
      RETURN
      END
      SUBROUTINE PLPWRI(OPTION,PTIT,TITLE,NLAB,LAB,XLEN,YLEN,
     &                  IGRID,XLEFT,XRIGHT,XINC,XDIV,XTXT,XTYP,
     &                  YLO,YUP,YINC,YDIV,YTXT,YTYP,NC)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      character*3 xopt(6)
      common /PLXOPT/ xopt
      CHARACTER*6 OPTION(2)
      CHARACTER*16 LAB(20)
      CHARACTER*80 TITLE,PTIT,XTXT,YTXT
      CHARACTER*3 XTYP,YTYP
C
C     WRITES GENERAL PLP FILE
C
      WRITE(19,777) OPTION,(xopt(j),j=1,6)
      WRITE(19,778) PTIT
      WRITE(19,778) TITLE
      WRITE(19,6010) NLAB,'NUMBER OF LABELS'
      DO 10 ILAB=1,NLAB
 10   WRITE(19,779) LAB(ILAB)
      WRITE(19,6030) XLEN,'XLEN'
      WRITE(19,6030) YLEN,'YLEN'
      WRITE(19,6010) IGRID,'GRID TYPE. 0: NO GRID'
      WRITE(19,6030) XLEFT,'XLEFT'
      WRITE(19,6030) XRIGHT,'XRIGHT'
      WRITE(19,6030) XINC,'XINC'
      WRITE(19,6030) XDIV,'XDIV'
      WRITE(19,778) XTXT
      WRITE(19,780) XTYP
      WRITE(19,6030) YLO,'YDOWN'
      WRITE(19,6030) YUP,'YUP'
      WRITE(19,6030) YINC,'YINC'
      WRITE(19,6030) YDIV,'YDIV'
      WRITE(19,778) YTXT
      WRITE(19,780) YTYP
      WRITE(19,6010) NC,'NC'
C *** FORMATS
 777  FORMAT(1H ,2A6,6(',',a3))
 778  FORMAT(1H ,A80)
 779  FORMAT(1H ,A16)
 780  FORMAT(1H ,A3)
 6010 FORMAT(1H ,I8,10X,A40)
 6020 FORMAT(1H ,F15.6,3X,A40)
 6030 FORMAT(1H ,G15.6,3X,A40)
      RETURN
      END
      SUBROUTINE PLTWRI(N,XOFF,DX,YOFF,DY,X,IX,Y,IY)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      DIMENSION X(1),Y(1)
      WRITE(19,6010) N,'N'
      WRITE(19,6030) XOFF,'XOFF'
      WRITE(19,6030) DX,'DX'
      WRITE(19,6030) YOFF,'YOFF'
      WRITE(19,6030) DY,'DY'
      IF (DX.EQ.0E0) WRITE(20,444)(X(II),II=1,N,IX)                    
      IF (DY.EQ.0E0) WRITE(20,444)(Y(II),II=1,N,IY)
      write(20,*)
 444  FORMAT(1H ,G13.6)
 6010 FORMAT(1H ,I8,10X,A40)
 6030 FORMAT(1H ,G15.6,3X,A40)
      RETURN
      END
      SUBROUTINE Pldetm(DLWVNL,WK0L,TITLE,XLEN,YLEN)                 
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'complo.f'

      CHARACTER*80 TITLE
      CHARACTER*6 OPTION(2),OPT2
      DATA OPT2 /'DETERM'/
C
      OPTION(1)=PROGNM

        CALL VMAX(arg(1),1,YMAX,NWVNO)   
C                
C XAXIS DEFINITION     
C
      IPLOT1=MAX(1,ICUT1-100)
      IPLOT2=MIN(NWVNO,ICUT2+100)
      WKMIN=WK0L+(IPLOT1-1)*DLWVNL
      NN=IPLOT2-IPLOT1+1
      XMAX=(WK0L+(NWVNO-1)*DLWVNL)
      XMIN=WK0L   
      CALL AUTOAX(XMIN,XMAX,XLEFT,XRIGHT,XINC,XDIV,NXDIF)
C
      IF (YMAX.LE.0) YMAX=1E0
      OPTION(1)=PROGNM
      OPTION(2)=OPT2
      IPLOT1=MAX(1,ICUT1-100)
      IPLOT2=MIN(NWVNO,ICUT2+100)
C                
C                
C  YAXIS DEFINITION                   
C                
      YMIN=0.0   
      CALL AUTOAX(YMIN,YMAX,YLO,YUP,YINC,YDIV,NYDIF)
      IF(IPLOT1.EQ.2)IPLOT1=1         
c *** labels
      NLAB=1
      if (FREQ.GE.1e6) then
         WRITE(LAB(1),8102) FREQ*1e-6
      else if (FREQ.GE.1e3) then
         WRITE(LAB(1),8101) FREQ*1e-3
      else
         WRITE(LAB(1),810) FREQ
      end if
 810  FORMAT('Freq:',F7.1,' Hz$')
 8101 FORMAT('Freq:',F6.1,' kHz$')
 8102 FORMAT('Freq:',F6.1,' MHz$')
 811  FORMAT('SD:',F9.1,' m$')
 812  FORMAT('RD:',F9.1,' m$')
       ptit='DETERMINANT'
      WRITE(XTXT,820) NXDIF
 820  FORMAT('Horizontal wavenumber (10**',I3,')$')
      WRITE(YTXT,821) NYDIF
 821  FORMAT('Modulus (10**',I3,')$')
      XTYP='LIN'
      YTYP='LIN'
      IGRID=0
      NC=1
C *** WRITE PLP FILE
      CALL PLPWRI(OPTION,PTIT,TITLE,NLAB,LAB,XLEN,YLEN,
     &                  IGRID,XLEFT,XRIGHT,XINC,XDIV,XTXT,XTYP,
     &                  YLO,YUP,YINC,YDIV,YTXT,YTYP,NC)
      CALL PLTWRI(NN,WKMIN,DLWVNL,0.,0.,arg(1),1,arg(1),1)
 2701 CONTINUE
C                
      RETURN     
C                
      END        
      SUBROUTINE CONFAW(INR,NPX,NPY,AMN,AMX,XLEFT,XRIGHT,XINC,
     1                  XLEN,FMIN,FMAX,YDOWN,YUP,YINC,YLEN,
     2                  ZMIN,ZMAX,ZINC,TITLE,DLANGLE)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      DIMENSION X(NP2,3)
      CHARACTER*80 TITLE,TITLEX,TITLEY,HEADING
      CHARACTER*3 XTYP,YTYP,CDUM
      CHARACTER*40 LAB
      CHARACTER*6 OPTION(2),OPT2(3)
      EQUIVALENCE (X(1,1),CFF(1,1))
      DATA OPT2 /'P-P   ','PSV   ','      '/
      DATA XTYP,YTYP /'LIN','LOG'/
      OPTION(1)='CONFR,'
      OPTION(2)=OPT2(INR)
      CDUM=opt2(inr)
      I=INR
      IF (INR.EQ.0) I=1
c >>> Force MINDIS for log y-axis plots
c      WRITE(28,'(a6,a3,a)') OPTION(1),opt2(inr)(1:3),',MIN'
      WRITE(28,'(a6,a3,a)') OPTION(1),opt2(inr)(1:3),','
      WRITE(28,778) TITLE
 777  FORMAT(2A6,'                                           ')
 778  FORMAT(A80)
 779  FORMAT(A40)
 780  FORMAT(A3)
      WRITE(28,6010) NPX,'NPX'
      WRITE(28,6010) NPY,'NPY'
      WRITE(28,6010) NPX,'NX'
      WRITE(28,6010) NPY,'NY'
      WRITE(28,6030) 1000.*AMN,'MINIMUM ANGLE'
      WRITE(28,6030) 1000.*AMX,'MAXIMUM ANGLE'
      WRITE(28,6030) 1000.*XLEFT,'XLEFT'
      WRITE(28,6030) 1000.*XRIGHT,'XRIGHT'
      XSCALE=ABS(XRIGHT-XLEFT)/XLEN
      WRITE(28,6030) 1000.*XSCALE,'XSCALE'
      WRITE(28,6030) 1000.*XINC,'XINC'
      if (slowrc) then
       titlex='Slowness (s/km)'
      else
       TITLEX='Grazing angle (degrees)'
      end if
      WRITE(28,778) TITLEX
      WRITE(28,780) XTYP
      WRITE(28,6030) FMIN,'MINIMUM FREQUENCY'
      WRITE(28,6030) FMAX,'MAXIMUM FREQUENCY'
      WRITE(28,6030) YDOWN,'YDOWN'
      WRITE(28,6030) YUP,'YUP'
C     FCC=LOG10(ABS(YUP/YDOWN))/LOG10(2.0)
C     YSCALE=ABS(YLEN/FCC)
      WRITE(28,6030) YLEN,'LENGTH OF Y-AXIS in CM'
      WRITE(28,6030) YINC,'YINC'
      TITLEY='Frequency (Hz)'
      WRITE(28,778) TITLEY
      WRITE(28,780) YTYP
      WRITE(28,6010) 5,'CAY'
      WRITE(28,6010) 5,'NRNG'
      WRITE(28,6010) 0,'NSM'
      WRITE(28,6030) ZMAX,'ZMIN'
      WRITE(28,6030) ZMIN,'ZMAX'
      WRITE(28,6030) ZINC,'ZINC'
      WRITE(28,6030) 2.0,'X1PL'
      WRITE(28,6030) 2.0,'Y1PL'
      WRITE(28,6030) 0.1,'HGTPT'
      WRITE(28,6030) 0.14,'HGTC'
      WRITE(28,6010) -3,'LABPT'
      WRITE(28,6010) 1,'NDIV'
      WRITE(28,6010) 5,'NARC'
      LABC=-1
      IF (ZINC.LT.1.0) LABC=1
      WRITE(28,6010) LABC,'LABC'
      WRITE(28,6010) -1,'LWGT'
      WRITE(28,6030) 0.,'DUMMY'
      WRITE(28,6030) 0.,'DUMMY'
      WRITE(28,811)
 811  FORMAT('FILENAME')
      WRITE(28,6030) 1000.*DLANGLE
 6010 FORMAT(1H ,I8,8X,A40)
 6020 FORMAT(1H ,F15.6,1X,A40)
 6030 FORMAT(1H ,G15.6,1X,A40)
      RETURN
      END
      SUBROUTINE CONFR(INR,NPX,NPY,AMN,AMX,XLEFT,XRIGHT,XINC,
     1                  XLEN,FMIN,FMAX,YDOWN,YUP,YINC,YLEN,
     2                  ZMIN,ZMAX,ZINC,TITLE,DLrange,sdl,rdl)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      DIMENSION X(NP2,3)
      CHARACTER*80 TITLE,TITLEX,TITLEY,HEADING
      CHARACTER*3 XTYP,YTYP
      CHARACTER*40 LAB
      CHARACTER*6 OPTION(2)
      EQUIVALENCE (X(1,1),CFF(1,1))
      character optpar(12)
      DATA optpar(1) /'N'/,
     &     optpar(2) /'W'/,
     &     optpar(3) /'U'/,
     &     optpar(4) /'V'/,
     &     optpar(5) /'R'/,
     &     optpar(6) /'B'/,
     &     optpar(7) /'S'/
      DATA XTYP,YTYP /'LIN','LOG'/

      OPTION(1)='CONFR,'
      OPTION(2)=optpar(INR)//'STR'

      I=INR
      IF (INR.EQ.0) I=1
      WRITE(28,777) OPTION
      WRITE(28,778) TITLE
 777  FORMAT(2A6,'                                           ')
 778  FORMAT(A80)
 779  FORMAT(A40)
 780  FORMAT(A3)
      WRITE(28,6010) NPX,'NPX'
      WRITE(28,6010) NPY,'NPY'
      WRITE(28,6010) NPX,'NX'
      WRITE(28,6010) NPY,'NY'
      WRITE(28,6030) 1000.*AMN,'MINIMUM RANGE'
      WRITE(28,6030) 1000.*AMX,'MAXIMUM RANGE'
      WRITE(28,6030) 1000.*XLEFT,'XLEFT'
      WRITE(28,6030) 1000.*XRIGHT,'XRIGHT'
      XSCALE=ABS(XRIGHT-XLEFT)/XLEN
      WRITE(28,6030) 1000.*XSCALE,'XSCALE'
      WRITE(28,6030) 1000.*XINC,'XINC'
      TITLEX='Range (km)'
      WRITE(28,778) TITLEX
      WRITE(28,780) XTYP
      WRITE(28,6030) FMIN,'MINIMUM FREQUENCY'
      WRITE(28,6030) FMAX,'MAXIMUM FREQUENCY'
      WRITE(28,6030) YDOWN,'YDOWN'
      WRITE(28,6030) YUP,'YUP'
C     FCC=LOG10(ABS(YUP/YDOWN))/LOG10(2.0)
C     YSCALE=ABS(YLEN/FCC)
      WRITE(28,6030) YLEN,'LENGTH OF Y-AXIS IN CM'
      WRITE(28,6030) YINC,'YINC'
      TITLEY='Frequency (Hz)'
      WRITE(28,778) TITLEY
      WRITE(28,780) YTYP
      WRITE(28,6010) 5,'CAY'
      WRITE(28,6010) 5,'NRNG'
      WRITE(28,6010) 0,'NSM'
      WRITE(28,6030) ZMIN,'ZMIN'
      WRITE(28,6030) ZMAX,'ZMAX'
      WRITE(28,6030) ZINC,'ZINC'
      WRITE(28,6030) 2.0,'X1PL'
      WRITE(28,6030) 2.0,'Y1PL'
      WRITE(28,6030) 0.1,'HGTPT'
      WRITE(28,6030) 0.14,'HGTC'
      WRITE(28,6010) -3,'LABPT'
      WRITE(28,6010) 1,'NDIV'
      WRITE(28,6010) 5,'NARC'
      LABC=-1
      IF (ZINC.LT.1.0) LABC=1
      WRITE(28,6010) LABC,'LABC'
      WRITE(28,6010) -1,'LWGT'
      WRITE(28,6030) sdl,'SD'
      WRITE(28,6030) rdl,'RD'
      WRITE(28,811)
 811  FORMAT('FILENAME')
      WRITE(28,6030) 1000.*DLrange
 6010 FORMAT(1H ,I8,8X,A40)
 6020 FORMAT(1H ,F15.6,1X,A40)
 6030 FORMAT(1H ,G15.6,1X,A40)
      RETURN
      END
      SUBROUTINE CONFAB(NPX,FREQL)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      DIMENSION X(NP2,3)
      EQUIVALENCE (X(1,1),CFF(1,1))
      DIMENSION SECTOR(28)
      CALL VCLR(SECTOR,1,28)
      SECTOR(1)=NPX
      SECTOR(2)=FREQL
      SECTOR(3)=1.
      WRITE(29,444) (SECTOR(JJ),JJ=1,28)
      WRITE(29,444) (X(JJ,2),JJ=1,NPX)
 444  FORMAT(1H ,6G13.5)
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



