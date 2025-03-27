      PROGRAM OAST35
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                 					C
C                     OASES V-3.5                       C
C							C
C        Ocean Acoustic and Seismic Exploration      	C
C                      Synthesis        		C
C							C
C                        1990				C
C							C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     
C     3-d - TRANSVERSE ISOTROPY - CW VERSION          
      
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
      COMPLEX SLOW
      LOGICAL CFRFL,ICONTU,TRFOUT,PLPOUT,CDROUT,GENTS,AUSAMP
      CHARACTER*50 FILENM
      DIMENSION X(NP2,NPAR),FF(2,NP3),PX(MODULO)   
      DIMENSION CONMAX(NPAR)
      COMPLEX CTRF(NPAR)
      DIMENSION FFS(2,NP),XS(NP2),AKM(100),AKM1(100),AKM2(100)           
      DIMENSION GRPVEL(NMOD),PHVEL(NMOD)
      CHARACTER*16 TITLEY(NPAR)
      CHARACTER*6  OPTION(2)
      CHARACTER*80 TITLE
      EQUIVALENCE (X(1,1),CFF(1,1)),(XS(1),CFFS(1))              
      EQUIVALENCE (FF(1,1),CFF(1,1)),(FFS(1,1),CFFS(1))              
C
C     COMPLEX FREQUENCY INTEGRATION ADDED AS OPTION 'O' 880414
C
      COMPLEX CCSQ,CDSQ,CC

      CHARACTER*6 TRFFILE(7)
      DATA TRFFILE /'trf   ',
     &              'trfdc ',
     &              'trfds ',
     &              'trfdh ',
     &              'trfuc ',
     &              'trfus ',
     &              'trfuh '/
      DATA OPTION /'FIPP  ','      '/         
      DATA TITLEY /'NORMAL STRESS  (',        
     1             'VERTICAL VEL.  (',        
c2d     2             'RADIAL VEL.    (',        
     2             'Um + Vm        (',        
     3             'Um - Vm        (',        
     4             'RADIAL STRESS  (',        
     &             'BULK STRESS    ('/        
      DATA XAXI,YAXI /20.0,12.0/
      DATA CONMAX /NPAR*-1E20/
C           
C ********************FORMATS*******************               
C           
C           
 210  FORMAT(/1H ,
     &       'OASP3D PULSE PROGRAM. Version 3.5, Update 14-Jul-1992',
     &      //1H ,A)         
500   FORMAT(/1H ,'SOURCE CENTRE FREQUENCY:',F10.2,' HZ ')             
550   FORMAT(/1H ,3X,'NW',5X,' ICW1',5X,' ICW2',
     &       5X,'ICUT1',5X,'ICUT2',/(1X,I5,4I10))  
  600 FORMAT(//,'  CMIN = ',G16.6,' M/S ',/,'  CMAX = ',G16.6,' M/S ')          
C           
C           
C **********************************************               
C           
C
C     CHECK THAT NP IS INTEGER POWER OF 2
C     
      IPOW=0
 992  IPOW=IPOW+1
      II=2**IPOW
      IF (II.LT.NP) GO TO 992
      IF (II.NE.NP) THEN
        WRITE(6,*)
        WRITE(6,*) 
     &   '>>>> FATAL: PARAMETER NP MUST BE INTEGER POWER OF 2 <<<<'
        WRITE(6,*)
     &   '>>>>        CHANGE IN ALL SOURCE FILES AND RELINK   <<<<'      
      END IF
C           
      ITXX=NP2
      PI=4E0*ATAN(1E0)      
      AI=CMPLX(0.,1.)        
      CNUL=CMPLX(0.,0.)
      IR=1  
      LS=1
      DELTA=1.
      THETA=0.
      FOCDEP=0.
      LTYP=1
      LINA=0
      srctyp=1
      MSUFT=1
      MBMAX=1
      MBMAXI=2
      ISROW=1
C
C     DEFAULT: ISOTROPIC LAYERS
C
      NTISOL=0
C           
C           
C           
      CALL OPFILR(1,IOER)
      IF (IOER.GT.0) STOP '>>>> ERROR: INPUT FILE NOT FOUND <<<<'
      READ(1,'(A)')TITLE       
      WRITE(6,210)TITLE      
      CALL GETOPT(IGRP,ISTACK,IBODY,ISDEP,IPROF,ICNTIN,CFRFL,ICONTU,
     &            TRFOUT,PLPOUT,CDROUT,GENTS)
c2d      if (sctout) then
c2d       call opfilb(45,ioer)
c2d      end if
      IF (ICNTIN.GT.0) THEN
        READ(1,*) FREQS,OFFDBIN
      ELSE
        OFFDBIN=0E0
        READ(1,*)FREQS       
      END IF
      TOTTIM=0.
      WRITE(6,500)FREQS      
C           
C     READ IN ENVIRONMENTAL DATA
C
      CALL INENVI
C
C     READ IN SOURCE AND RECEIVER DATA
C
      CALL INSRC(SD)
      CALL INREC(RD,RDLOW,idummy)

      WRITE(6,910) NUML,LS,IR
 910  FORMAT(/1H ,'NUMBER OF LAYERS:      ',I3,
     1      /1H ,'NUMBER OF SOURCES:     ',I3,
     2      /1H ,'NUMBER OF RECEIVERS:   ',I3)
      NUMI=NUML-1            
C           
c     Wavenumber sampling parameters
c
      READ(1,*)CMININ,CMAXIN   
      READ(1,*)NWVNO,ICW1,ICW2,INTF
      AUSAMP=(NWVNO.LT.1)  
      PLPOUT=PLPOUT.OR.((INTF.GT.0).AND.(.NOT.ICONTU))
      NWVNO=MIN0(NWVNO,NP)

c *** RANGE PARAMETERS

      READ(1,*) R0,RSPACE,NPLOTS
      R1=R0*1000.            
      DLFREQ=1E0
      NX=1024
      LX=101
      MX=101
      dlfreq=freqs/(lx-1)
      dt=1e0/(nx*dlfreq)     
      LXP1=MAX(2,LX)        
      FREQM=DLFREQ*(MX-1)    
      FREQ0=DLFREQ*(LXP1-1)    
      freq1=freq0
      freq2=freqm
c
c >>> Read sources from strf file and make frequency sampling 
c     compatible
c
      if (trfsou) then
       call rdstrf(freq0,freqm,nx,lxp1,mx,dt,dlfreq)
       write(6,*) 'Sources read from strf file:', ls
       write(6,*) 'LXP1 =', lxp1
       write(6,*) 'MX =', mx
       sd=sdc(1)

C
C     DETERMINATION OF SOURCE LAYERS
C
       WRITE(6,908)
 908   FORMAT(/1H ,'SOURCE DATA:',//1H ,'  N0. ','   DEPTH  ',
     1       'LAYER','      ZU        ZL')
       DO 906 I=1,LS
 906    CALL SOURCE(V,NUML,SDC(I),LAYS(I),ZUS(I),ZLS(I))
       WRITE(6,907) 1,SDC(1),LAYS(1),ZUS(1),ZLS(1)
       IF (LS.GT.1) THEN
        WRITE(6,907) LS,SDC(LS),LAYS(LS),ZUS(LS),ZLS(LS)
       END IF
 907   FORMAT(1H ,I6,F10.1,I6,2F10.1)
      else if (rghsou) then
c >>>  roughness source
c >>>  the fourier coefficients are in files 11-14
c >>>  intrgh: the number of the rough interface
c >>>  dlkrgh: radial wavenumber sampling
c >>>  nrgh:   number of wavenumber samples (incl. zero)
c >>>  mrgh:   number of fourier orders
       read(5,*) intrgh, dlkrgh, nrgh, mrgh
      end if
c 
c >>> make tables for dispersive media
c
      if (idlmax.gt.0) then
       call dltable(lxp1,mx,dlfreq)
      end if 

      IF (AUSAMP) THEN
C *** AUTOMATIC SAMPLING
       AUSAMP=.TRUE.
       CFRFL=.TRUE.
       ICNTIN=0
       CALL VMAX(V(1,2),1,CREF,NUML)
       RANREF=CREF*(NX*DT)
c >>> max and min ranges
       rm =0e0
       rmi=1e20
       do 988 ii=1,nplots
        rr=abs(1E3*(r0+(ii-1)*rspace))
        rm=max(rm,rr)
        if (rr.gt.0e0) rmi=min(rmi,rr)
 988    continue
       if (rmi.gt.1e19) rmi=0e0

       RANREF=RANREF+RM
       OFFDBIN=0E0
       WRITE(6,*)
       WRITE(6,*) '>>> AUTOMATIC SAMPLING '
       WRITE(6,*) '    COMPLEX FREQUENCY ENABLED'
       WRITE(6,*) '    COMPLEX WAVENUMBER DISABLED'
       write(6,*) '    REFERENCE SPEED:',CREF
       WRITE(6,*) '    REFERENCE RANGE:',RANREF
       FREQ=FREQM
       CALL AUTSAM(CMININ,CMAXIN,rmi,RANREF,CMIN,CMAX,
     &             NWVNO,ICW1,ICW2,ibody)
       WRITE(6,*) '    MAX NO. OF WAVENUMBERS:',NWVNO
       ICUT1=1
       ICUT2=NWVNO
      ELSE
       CMIN=CMININ
       CMAX=CMAXIN
      END IF 
C
C     CHECK WHETHER TO INVOKE DEFAULT CONTOUR OFFSET
C
      IF (ICNTIN.GT.0.AND.OFFDBIN.LT.1E-10) THEN
        OFFDB=60.0*V(LAYS((LS-1)/2+1),2)*(1E0/CMIN-1E0/CMAX)/NWVNO
        WRITE(6,*) 
        WRITE(6,*) 'DEFAULT CONTOUR OFFSET APPLIED,',OFFDB,
     &             ' dB/wavelength'
      ELSE
        OFFDB=OFFDBIN
      END IF
C
C     MOVE FREQUENCY INTEGRATION CONTOUR TO ATTENUATE
C     WRAP-AROUND BY FACTOR 50
C
      if (.not.trfsou) then
       IF (CFRFL) THEN
        OMEGIM=-ALOG(50.0)*DLFREQ
       ELSE
        OMEGIM=0.0
       END IF
      else
       write(6,*)
       write(6,*) '>>> Imag(omega) = ', omegim,' from source TRF file'
      end if
      NIPLOT=0       
      IF (INTF.GT.0) NIPLOT=(MX-LXP1)/INTF+1  
c
c *** dispersion curves
c
      IF (IGRP.GT.0) THEN
       READ(1,*) NMODES
       NMMAX=NMODES*2+1
       READ(1,*) FLEFT,FRIGHT,FAXIS,FINC
       READ(1,*) VDOWN,VUP,VAXIS,VINC
      END IF
C ***
      IF (CMIN.EQ.0) STOP '*** CMIN MUST BE NON-ZERO ***'
      IF (((CMAX.GT.0).AND.(CMIN.GT.0)).OR.
     1    ((CMAX.LT.0).AND.(CMIN.LT.0))) THEN
       NFLAG=.FALSE.
       WK0 = 2*PI*FREQM / CMAX
       WKMAX = 2*PI*FREQM / CMIN               
      ELSE
       IF (CMIN.LE.0) STOP '*** CMIN/CMAX CONFLICT ***'
       WKMAX=2*PI*FREQM/CMIN
       CMAX=1E8
       CMAXIN=1E8
       WK0=2*PI*FREQM/CMAX
       NFLAG=.TRUE.
      END IF
      IF (NWVNO.GT.1) THEN
        DLWVNO = ( WKMAX - WK0 ) / ( FLOAT(NWVNO-1) )            
      ELSE
        DLWVNO=1.0
      END IF
      DLRAN=1E3*RSPACE
      RRMAX=R1+(NPLOTS-1)*DLRAN
      if (tilt) rrmax=max(rrmax+abs(ofstar(1)),rrmax+abs(ofstar(ir)))

      IF (.NOT.AUSAMP) THEN
C           
C *** EXTEND WAVENUMBER INTERVAL FOR TAPERING
C
       IF (ICW1.LT.2) THEN
         ICUT1=ICW1
       ELSE
c         ICUT1=MAX(1,ICW1-NINT(0.05*NWVNO))
         icut1=1 
       END IF
       IF (ICW2.EQ.NWVNO) THEN
         ICUT2=ICW2
       ELSE
c         ICUT2=MIN(NWVNO,ICW2+NINT(0.05*NWVNO))
         icut2=nwvno
       END IF
       WRITE(6,600)CMIN,CMAX  
       WRITE(6,550)NWVNO,ICW1,ICW2,ICUT1,ICUT2           
       IF (NFLAG) THEN
         WRITE(6,*) 'NEGATIVE SPECTRUM BY SYMMETRY'
       END IF
      END IF
 
      WRITE(6,9980) DT,TMIN,DLFREQ,FREQ0,FREQM,NIPLOT,NOUT,NPLOTS,
     1            NOUT       
 9980 FORMAT(/1H ,'TIME STEP:        ',F12.6,' SECS',           
     1      /1H ,'MIN. TIME:        ',F12.6,' SECS',           
     1      /1H ,'FREQUENCY STEP:   ',F12.6,' HZ',             
     2      /1H ,'MIN. FREQUENCY:   ',F12.6,' HZ',             
     3      /1H ,'MAX. FREQUENCY:   ',F12.6,' HZ',             
     4      //1H ,'INTEGRAND PLOTS:  ',I5,' (*',I1,')',         
     5      /1H ,'PULSE PLOTS:      ',I5,' (*',I1,')')         
      WRITE(6,9979) R0,RSPACE         
 9979 FORMAT(/1H ,'MINIMUM RANGE:    ',F12.6,' KM',             
     1      /1H ,'RANGE STEP:       ',F12.6,' KM')             
c      LXP1=LX                
c      IF (LX.LE.1) LXP1=2
      NUMFR=MX-LXP1+1
C
C     OPEN FILES
C
      IF (PLPOUT) THEN
       CALL OPFILW(19,IOER)
       CALL OPFILW(20,IOER)
       WRITE(19,6010) MODULO,'MODULO'
 6010  FORMAT(1H ,I8,10X,A40)
      END IF
      IF (DEBUG) CALL OPFILW(21,IOER)
C
C     OPEN TRANSFER FUNCTION SCRATCH FILES
C
      IF (DECOMP) THEN
        NCOMPO=NLEQ+1
      ELSE
        NCOMPO=1
      END IF
C
C     PLOT OF VELOCITY PROFILE IF OPTION 'Z' WAS CHOSEN
C
      IF (IPROF.GT.0) THEN
        READ(1,*) VPLEFT,VPRIGHT,VPLEN,VPINC
        READ(1,*) DPUP,DPLO,DPLEN,DPINC
        CALL PLPROF(TITLE,VPLEN,DPLEN,VPLEFT,VPRIGHT,VPINC,
     2              DPUP,DPLO,DPINC)
      END IF
C
C     PREPARE FOR FREQUENCY/SLOWNESS CONTOURS OF INTEGRANDS
C
      ICONTU=ICONTU.AND.(INTF.GT.0)
C
C     OPEN SCRATCH FILE FOR INTEGRAND CONTOUR DATA
C
       IF (ICONTU) THEN
         NN=ICUT2-ICUT1+1
         NDEC=NN/NCONM
         IF (NDEC.GT.0) THEN
           NCON=(NN-1)/NDEC+1
         ELSE
           NDEC=1
           NCON=NN
         END IF
        WRITE(6,*) 'NCON=',NCON
        CALL OPNBUF(27,NCON,NIPLOT*NOUT,100)
        CALL OPFILW(28,IOER)
        CALL OPFILW(29,IOER)
       END IF
      CALL MORDER()
      write(6,*)
      write(6,*) 'Source type:  ',SRCTYP
      write(6,*) 'Fourier terms:',msuft
      write(6,*)
C
C     GENERATE TRANSFER FUNCTION FILE FOR POST-PROCESSOR
C
      if (nplots.gt.0) then
      DO 760 I=1,NCOMPO
       LUTTRF=LUTRF+I-1
       IF (TRFOUT) THEN
        CALL TRFHEAD(TRFFILE(I),TITLE,RD,RDLOW,R0,RSPACE,
     &              NX,LXP1,MX,DT,FREQS,SD)
       END IF
 760  CONTINUE
      end if
C *** INITIALIZE POINTERS AND PARAMETERS          
      CALL PINIT1
      IF (DEBUG) CALL PREQV(NUML,NUMI)
C
C     SLOWNESS DIAGRAM FOR TRANS. ISOTR. MEDIA
C
      IF (NTISOL.GT.0.and.IPROF.GT.0) THEN
       CALL SNSDGM(title)
      END IF
C *** PREPARE BESSEL FUNCTIONS FOR FULL INTEGRATION
      IF (INTTYP.EQ.2) THEN
       CALL PREPBF(RRMAX,WKMAX*1.2)
       ICDR=1
      END IF
C *** FREQUENCY LOOP
      DO 15 JJ=LXP1,MX       
      CALL CLTIME
      nactf=jj
      KPLOT=0                
      IF (NIPLOT.LE.0) GO TO 14               
      IF (MOD(JJ-LXP1,INTF).EQ.0) KPLOT=1        
 14   CONTINUE               
      FREQ=(JJ-1)*DLFREQ     
      DSQ=2E0*PI*FREQ+CMPLX(0E0,OMEGIM)
      RDSQ=DSQ
      CSQ=DSQ*DSQ            
C *** WAVENUMBER SAMPLING
      IF (AUSAMP) THEN
C ***  AUTOMATIC SAMPLING
       WRITE(6,*)
       WRITE(6,*) '>>> FREQUENCY:',FREQ

       IF (IBODY.GT.0) THEN
C ***   SLOWNESS INTEGRATION
        CALL AUTSAM(CMININ,CMAXIN,rmi,RANREF,CMIN,CMAX,
     &              NWVNO,ICW1,ICW2,ibody)
       ELSE
C ***   WAVENUMBER INTEGRATION
        CALL AUTSAM(CMININ*FREQ/FREQM,CMAXIN,rmi,RANREF,CMIN,CMAX,
     &              NWVNO,ICW1,ICW2,ibody)
       END IF
       WRITE(6,*) '    CMIN,CMAX:  ',CMIN,CMAX
       WRITE(6,*) '    WAVENUMBERS:',NWVNO,ICW1,ICW2
       ICUT1=1
       ICUT2=NWVNO
       WK0=RDSQ/CMAX           
       WKMAX=RDSQ/CMIN
       IF (NWVNO.GT.1) THEN
        DLWVNO = ( WKMAX - WK0 ) / ( FLOAT(NWVNO-1) )            
       ELSE
        DLWVNO=1.0
       END IF
       IF (ICNTIN.GT.0) THEN
        OFFDB=60.0*V(LAYS((LS-1)/2+1),2)*(1E0/CMIN-1E0/CMAX)/NWVNO
        WRITE(6,*) 'DEFAULT CONTOUR OFFSET APPLIED,',OFFDB,
     &             ' dB/wavelength'
       END IF
      ELSE
       IF (.NOT.NFLAG) THEN
        IF (CMAX.GT.0) THEN
         WK0=RDSQ/CMAX           
        ELSE
         WK0=RDSQ/CMIN-(NWVNO-1)*DLWVNO
        END IF
       END IF
       IF (IBODY.GT.0) THEN
        WK0=RDSQ/CMAX           
        WKMAX=RDSQ/CMIN
        IF (NWVNO.GT.1) THEN
         DLWVNO = ( WKMAX - WK0 ) / ( FLOAT(NWVNO-1) )            
        ELSE
         DLWVNO=1.0
        END IF
       END IF
      END IF
      CALL PINIT2            
 9988 FORMAT(1H ,'FREQ. NO.',I4,' : ',F10.3,' HZ',             
     1       F10.3,' SECS')
      CALL CALIN3
      CALL CHKSOL
C *** TOTAL FIELD FOR PLOTS ETC.
      LUTGRN=LUGRN
C *** DISPERSION
      IF (IGRP.GT.0) THEN
       CALL MODES(IFI,NMMAX,NM,AKM,AKM1)
       DO 8 J=1,NM
 8      AKM(J)=(AKM(J)-1)*DLWVNO+WK0
       CALL IMPMOD(IFI,DLWVNO,DLFREQ,AKM,PHVEL,GRPVEL,NM)
       WRITE(22,*) FREQ,NM/2
       WRITE(22,6000) (PHVEL(J),J=1,NM/2)
       WRITE(23,*) FREQ,NM/2
       WRITE(23,6000) (GRPVEL(J),J=1,NM/2)
 6000  FORMAT(1H ,4E16.6)
      END IF
C *** INTEGRAND PLOTS     
      IF (KPLOT.EQ.1) THEN
       DO 9933 M=1,MSUFT
       CALL GETKNL(IR,M,1)
        IF (ICONTU) THEN
          NN=ICUT2-ICUT1+1
          DO 510 I=1,NPAR
          IF (IOUT(I).GT.0.AND.M.EQ.1) THEN
            if (I.gt.1.and.i.lt.5) then
c >>> change velocities to micro-meter/sec in kernels
             call vsmul(cff(icut1,I),1,1E6,cff(icut1,I),1,2*nn)
            end if 
            CALL CVMAGS(CFF(ICUT1,I),2,FFS,1,NN)
            CALL VALG10(FFS,1,FFS,1,NN)
            CALL VSMUL(FFS,1,1E1,FFS,1,NN)
            CALL VDECIM(FFS,1,FFS,1,NN,NDEC,NC)
            CALL WRBUF(27,FFS,NC)
            CALL VMAX(FFS,1,AAA,NC)
            CONMAX(I)=MAX(CONMAX(I),AAA)
          END IF
510       CONTINUE
c2d        else if (determ) then
c2d          call pldetm(dlwvno,wk0,title,xaxi,yaxi)
        ELSE
          CALL PLINTM(DLWVNO,WK0,SD,RDC(IR),TITLE,M,   
     1             XAXI,YAXI)
        END IF
 9933  CONTINUE
      END IF
      DO 351 I=NCOMPO-1,0,-1
       LUTGRN=LUGRN+I
       LUTTRF=LUTRF+I
       IF (NPLOTS.GT.0) THEN
        if (inttyp.eq.-1) then
         call taupdgm(cfile,cff)
        else if (tilt) then
         CALL INTtlt(CFILE,CFF,ARG,CBUF)
        else
         CALL INTGR3(CFILE,CFF,ARG,CBUF)
        end if
        CALL CLSBUF(LUTGRN)
       ELSE 
        CALL CLSBUF(LUTGRN)
       END IF
 351  CONTINUE
      CALL RDTIME(T1)
      TOTTIM=TOTTIM+T1
      WRITE(6,9988) JJ,FREQ,T1
 15   CONTINUE               
      IF (NPLOTS.GT.0) THEN
       DO 352 I=0,NCOMPO-1
        LUTTRF=LUTRF+I
        CLOSE(LUTTRF,STATUS='KEEP')
 352   CONTINUE
      END IF

      IF (ICONTU) CALL ENFBUF(27)

      CALL CLTIME

C*****  DISPERSION CURVES

      IF (IGRP.GT.0) THEN
       CALL PLDISP(PHVEL,GRPVEL,NMODES,LXP1,MX,DLFREQ,
     &             FAXIS,VAXIS,FLEFT,FRIGHT,FINC,
     &             VDOWN,VUP,VINC,TITLE)
      END IF
C*****  CONTOUR PLOTS OF INTEGRANDS VERSUS FREQUENCY

      IF (ICONTU) THEN
C
C     SLOWNESS OR WAVENUMBER AXIS
C
        NN=ICUT2-ICUT1+1
        WN1=WK0+(ICUT1-1)*DLWVNO
        WN2=WK0+(NCON-1)*NDEC*DLWVNO
        IF (IBODY.GT.0) THEN
          WN1=WN1/(2E0*PI*FREQM)
          WN2=WN2/(2E0*PI*FREQM)      
        END IF
        DO 710 I=1,NPAR
         IF (IOUT(I).LE.0) GO TO 710
          CALL RWDBUF(27)
          IF (WN1.LT.WN2*1E-1) THEN
            XLEFT=0
          ELSE
            XLEFT=INT(WN1*1E4)*1E-4
          END IF
          XRIGHT=WN2
          XINC=INT(XRIGHT*1E4/5E0)*1E-4
          XSCALE=(XRIGHT-XLEFT)/20.0
          IF (FREQM.LE.FREQ0/10E0) THEN
            YDOWN=0E0
          ELSE
            YDOWN=FREQM
          END IF
          IF (FREQ0.LE.FREQM/10E0) THEN
            YUP=0E0
          ELSE
            YUP=FREQ0
          END IF
          YINC=INT(ABS(YDOWN-YUP)*1E1/5E0)*1E-1
          YSCALE=ABS(YDOWN-YUP)/12.0
          ZMIN=INT(CONMAX(I))
          ZINC=10E0
          ZMAX=ZMIN-ZINC*10E0
          CALL INTCON(TITLE,NCON,NIPLOT,NCON,NIPLOT,XLEFT,XRIGHT,
     1                XSCALE,XINC,YUP,YDOWN,YSCALE,YINC,ZMIN,ZMAX,
     2                ZINC,0.0,0,(LXP1-1)*DLFREQ,FREQM,WN1,
     3                WN2,I,IBODY,1)
          DO 709 IFREQ=1,NIPLOT
            DO 708 J=1,NPAR
              IF (IOUT(J).LE.0) GO TO 708
              CALL RDBUF(27,FFS,NCON)
              IF (J.EQ.I) THEN
                WRITE(29,'(1X,6G13.5)') (XS(JJJ),JJJ=1,NCON)
              END IF
 708        CONTINUE
 709      CONTINUE
 710    CONTINUE
        CLOSE(28)
        CLOSE(29)
      END IF
C
C     THAT'S IT FOLKS
C
      IF (PLPOUT) THEN
       OPTION(2)='PLTEND'
       WRITE(19,777) OPTION
 777   FORMAT(1H ,2A6)
      END IF
C           
C           
      CALL RDTIME(T1)
      TOTTIM=TOTTIM+T1
      WRITE(6,9960) TOTTIM
 9960 FORMAT(/1H ,'*** OASES PULSE FINISHED ***',
     1      //1H ,'    TOTAL TIME: ',F10.3,' SECS')
      END   
C           
      SUBROUTINE GETOPT(IGRP,ISTACK,IBODY,ISDEP,IPROF,ICNTIN,CFRFL,
     1                  ICONTU,TRFOUT,PLPOUT,CDROUT,GENTS)      
C           
C     INPUT OF OPTIONS       
C           
      INCLUDE 'compar.f'
      LOGICAL CFRFL,ICONTU,TRFOUT,PLPOUT,CDROUT,GENTS
      CHARACTER*1 OPT(40)
      logical momsou
      common /msrclo/ momsou
      WRITE(6,300)           
 300  FORMAT(/1H ,'OPTIONS:',/)                
      INTTYP=0
      NOUT=0
      IREF=0
      ISTYP=-1           
      ICDR=0
      ISTACK=-1
      IBODY=0
      ISDEP=0
      IGRP=0
      IPROF=0
      ICNTIN=0
      CFRFL=.FALSE.
      SHEAR=.FALSE.
      momsou=.false.
      ICONTU=.FALSE.
      DECOMP=.FALSE.
      TRFOUT=.TRUE.
      PLPOUT=.FALSE.
      CDROUT=.FALSE.
      GENTS=.FALSE.
      DETERM=.FALSE.
      sctout=.false.
      DO 10 I=1,NPAR            
 10   IOUT(I)=0              
      READ(1,200) OPT        
 200  FORMAT(40A1)           
      DO 50 I=1,40           
      IF (OPT(I).EQ.'B') THEN
       IF (IBODY.GT.0) GO TO 50              
       IBODY=1
       WRITE(6,202)           
 202   FORMAT(1H ,'SLOWNESS INTEGRATION')
      ELSE IF (OPT(I).EQ.'A') THEN
       IF (IBODY.GT.0) GO TO 50              
       IBODY=2
       WRITE(6,201)           
 201   FORMAT(1H ,'ANDYs INTEGRATION')
      ELSE IF (OPT(I).EQ.'N') THEN
       IF (IOUT(1).GT.0) GO TO 50              
       NOUT=NOUT+1            
       IOUT(1)=1              
       WRITE(6,301)           
 301   FORMAT(1H ,'NORMAL STRESS')             
      ELSE IF (OPT(I).EQ.'V') THEN
       IF (IOUT(2).GT.0) GO TO 50              
       NOUT=NOUT+1            
       IOUT(2)=1              
       WRITE(6,302)           
 302   FORMAT(1H ,'VERTICAL VELOCITY')        
      ELSE IF (OPT(I).EQ.'H') THEN             
       IF (IOUT(3).GT.0) GO TO 50
       IF (IOUT(3).GT.0.or.IOUT(NPAR).GT.0) GO TO 50              
c2d       NOUT=NOUT+1            
c2d       IOUT(3)=1              
       NOUT=NOUT+2            
       IOUT(3)=1
       IOUT(4)=1
       WRITE(6,303)           
 303   FORMAT(1H ,'HORIZONTAL VELOCITY') 
      ELSE IF (OPT(I).EQ.'R') THEN
       IF (IOUT(5).GT.0) GO TO 50              
       NOUT=NOUT+1            
       IOUT(5)=1              
       WRITE(6,306)           
 306   FORMAT(1H ,'RADIAL STRESS')             
      ELSE IF (OPT(I).EQ.'K') THEN
       IF (IOUT(6).GT.0) GO TO 50              
       NOUT=NOUT+1            
       IOUT(6)=1              
       WRITE(6,3061)           
 3061  FORMAT(1H ,'BULK STRESS')             
      ELSE IF (OPT(I).EQ.'G') THEN
       IF (IGRP.GT.0) GO TO 50
       IGRP=1
       PLPOUT=.TRUE.
       DETERM=.TRUE.
       WRITE(6,307)
 307   FORMAT(1H ,'DISPERSION CURVES')     
      ELSE IF (OPT(I).EQ.'L') THEN
       IF (LINA.GT.0) GO TO 50
       LINA=1
       extlar=.false.
       WRITE(6,308)
 308   FORMAT(1H ,'VERTICAL SOURCE ARRAY - Internal')
      ELSE IF (OPT(I).EQ.'l') THEN
       IF (LINA.GT.0) GO TO 50
       LINA=1
       extlar=.true.
       WRITE(6,309)
 309   FORMAT(1H ,'VERTICAL SOURCE ARRAY - External')
      ELSE IF (OPT(I).EQ.'s') THEN
       IF (LINA.GT.0) GO TO 50
       LINA=1
       trfsou=.true.
       WRITE(6,'(a)') 'VERTICAL SOURCE ARRAY - trf-file'
      ELSE IF (OPT(I).EQ.'P') THEN
       IF (ICDR.GT.0) GO TO 50
       ICDR=1
       WRITE(6,313)
 313   FORMAT(1H ,'PLANE GEOMETRY')
      ELSE IF (OPT(I).EQ.'Z') THEN
       IF (IPROF.GT.0) GO TO 50
       IPROF=1
       PLPOUT=.TRUE.
       WRITE(6,314)
 314   FORMAT(1H ,'PLOT OF VELOCITY PROFILES')
      ELSE IF (OPT(I).EQ.'J') THEN
        ICNTIN=1
        WRITE(6,315)
 315    FORMAT(1H ,'COMPLEX INTEGRATION CONTOUR')
      ELSE IF (OPT(I).EQ.'F') THEN
        INTTYP=1
        WRITE(6,316)
 316    FORMAT(1H ,'FILON INTEGRATION SCHEME')
      ELSE IF (OPT(I).EQ.'f') THEN
        INTTYP=2
        WRITE(6,3161)
 3161    FORMAT(1H ,'FULL BESSEL INTEGRATION SCHEME')
      ELSE IF (OPT(I).EQ.'O') THEN
        CFRFL=.TRUE.
        WRITE(6,317)
 317    FORMAT(1H ,'COMPLEX FREQUENCY INTEGRATION')
c2d      ELSE IF (OPT(I).EQ.'X'.or.opt(i).eq.'2') THEN
c2d        IF (SHEAR) GO TO 50
c2d        srctyp = 2
c2d        SHEAR=.TRUE.
c2d        WRITE(6,318)
c2d 318    FORMAT(1H ,'VERTICAL POINT FORCE IN  SOLID MEDIA')
c2d      ELSE IF (OPT(I).EQ.'m'.or.opt(i).eq.'3') THEN
c2d        IF (momsou) GO TO 50
c2d        srctyp = 3
c2d        momsou=.TRUE.
c2d        WRITE(6,3181)
c2d 3181   FORMAT(1H ,'MOMENT FORCE IN  SOLID MEDIA')
      ELSE IF (OPT(I).EQ.'C') THEN
        IF (ICONTU) GO TO 50
        ICONTU=.TRUE.
        CDROUT=.TRUE.
        WRITE(6,319)
 319    FORMAT(1H ,'FREQUENCY/SLOWNESS CONTOURS OF INTEGRANDS')
      ELSE IF (OPT(I).EQ.'U') THEN
        IF (DECOMP) GO TO 50
        DECOMP=.TRUE.
        WRITE(6,320)
 320    FORMAT(1H ,'WAVE FIELD DECOMPOSITION')
        IF (.NOT.TRFOUT) THEN
         TRFOUT=.TRUE.
         WRITE(6,321)
        END IF
      ELSE IF (OPT(I).EQ.'T') THEN
        IF (TILT) GO TO 50
        TILT=.TRUE.
        WRITE(6,321)
 321    FORMAT(1H ,'Tilted receiver arrays')
      ELSE IF (OPT(I).EQ.'Q') THEN
        IF (debug) GO TO 50
        debug=.TRUE.
        WRITE(6,322)
 322    FORMAT(1H ,'>>> debugging <<<')
      else if (opt(i).eq.'t') then
        inttyp=-1
        write(6,'(A)') 'tau-p SEISMOGRAMS'
c2d      ELSE IF (OPT(I).EQ.'s') THEN
c2d       IF (SCTOUT) GO TO 50
c2d       SCTOUT=.TRUE.
c2d       WRITE(6,3092)
c2d 3092  FORMAT(1H ,'OUTPUT OF SCATTERING DISCONTINUITIES')
      ELSE
      END IF
 50   CONTINUE               
      GENTS=(ISTACK.GT.-1)
      IF (ISTYP.LT.0) THEN   
      ISTYP=2                
      WRITE(6,305) ISTYP     
      END IF
 305  FORMAT(1H ,'SOURCE TYPE:',I2)           
      IF (INTTYP.EQ.2) THEN
       ICNTIN=0
      END IF
      IF (NOUT.NE.0) RETURN  
      IOUT(1)=1              
      NOUT=1
      WRITE(6,301)           
      RETURN
      END

      SUBROUTINE AUTSAM(C1,C2,rmin,RMAX,CMIN,CMAX,NW,IC1,IC2,ibody)
c      PARAMETER (RFAC=1.5)
      PARAMETER (NR=150)
c >>> Minimum # of k, # of periods of exponential over taper interval
      PARAMETER (NKMIN=200,WTAPER=10.0)
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      if (inttyp.eq.1) then
        RFAC=1e0
      else
        RFAC=1.5e0
      end if
C *** DETERMINE WAVENUMBER SAMPLING INTERVAL
      DK=2*PI/(RFAC*RMAX)
c >>> Determine tapering interval from 
c     minimum source receiver separations
c >>> Depth-separation
      zsep=1E20
      do 200 isou=1,ls
       do 200 ircv=1,ir
        zsep=min(zsep,abs(rdc(ircv)-sdc(isou)))
 200    continue
      write(6,*) '>>> Reference depth for tapering:', zsep,' m'
      rref=rmin+wtaper*zsep
c >>> if zero set to  one  wavelength
      if (rref.eq.0e0) rref= v(lays(1),2)/freq
      write(6,*) '>>> Reference range for tapering:', rref,' m'
      taperk=2*pi*wtaper/rref
C *** INTEGRATION LIMITS
      WN1=2*PI*FREQ/C2
      if (ibody.eq.2) then
       WN2=2*PI*(FREQ+0.2*freq2)/C1
      else
       WN2=2*PI*FREQ/C1
      end if
c      if (ibody.gt.0) then
c       WNMAX=(1.1+(freq2-freq)/freq2*0.9)*(WN2-WN1)+WN1
c      else
c       WNMAX=1.1*(WN2-WN1)+WN1
c      end if
c      WNMIN=WN1-0.1*(WN2-WN1)
      wnmax=wn2+taperk
      wnmin=wn1-taperk
      WNMIN=MAX(WNMIN,DK*1e-3)
      DK=MIN(DK,(WNmax-WNmin)/(NKMIN-1))
C *** NUMBER OF WAVENUMBER SAMPLING POINTS
       NW=(WNMAX-WNMIN)/DK+1
       WNMAX=WNMIN+(NW-1)*DK
       IC1=(WN1-WNMIN)/(WNMAX-WNMIN)*(NW-1)+1
       IC1=MAX(IC1,1)
       IC2=(WN2-WNMIN)/(WNMAX-WNMIN)*(NW-1)+1
       IC2=MIN(IC2,NWVNO)
       CMIN=2*PI*FREQ/WNMAX
       CMAX=2*PI*FREQ/WNMIN
       RETURN
       END

      BLOCK DATA BLKPUL
      INCLUDE 'compar.f'
C
C**** DEFINITION OF MAX REAL ARGUMENT TO THE EXPONENTIAL FUNCTION
      COMMON /ARGMAX/ AM
C**** THE FOLLOWING DEFINITION SHOULD BE USED FOR THE FPS164
CFPS  DATA AM /300./
C**** THE FOLLOWING DEFINITION SHOULD BE USED FOR THE VAX
      DATA AM /65./     
      DATA OMEGIM /0.0/
c2d      DATA PROGNM /'OASP14'/
      DATA PROGNM /'OASP3 '/
      DATA LUGRN,LUTRF,LUTGRN,LUTTRF /30,50,30,50/
      DATA SHEAR,DECOMP,SCTOUT,NFLAG /.FALSE.,.FALSE.,.FALSE.,.FALSE./
      DATA MSUFT,MBMAX,MBMAXI,SRCTYP,ISROW,ISINC /1,1,2,1,1,0/
      END
