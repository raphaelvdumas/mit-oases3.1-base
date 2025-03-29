
      SUBROUTINE OASPL
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
C     
C     2-d - PORO-ELASTIC - PULSE VERSION          
C     inttyp=-1 : tau-p seismograms
c     970912   Patch scattering incident field modified to Jayong Lee's
c              form (OASK).
      
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'


      COMPLEX SLOW
      LOGICAL CFRFL,ICONTU,PLPOUT,CDROUT,GENTS,AUSAMP,PAR
      CHARACTER*50 FILENM
      DIMENSION X(NP2,NPAR),FF(2,NP3),PX(MODULO)   
      DIMENSION CONMAX(NPAR)
      COMPLEX CTRF(NPAR)
      DIMENSION FFS(2,NP),XS(NP2),AKM(100),AKM1(100),AKM2(100)           
      DIMENSION GRPVEL(NMOD),PHVEL(NMOD)
      CHARACTER*16 TITLEY(12)
      CHARACTER*6  OPTION(2)
      CHARACTER*80 TITLE
      EQUIVALENCE (X(1,1),CFF(1,1)),(XS(1),CFFS(1))              
      EQUIVALENCE (FF(1,1),CFF(1,1)),(FFS(1,1),CFFS(1))              
C
C     COMPLEX FREQUENCY INTEGRATION ADDED AS OPTION 'O' 880414
C
      COMPLEX CCSQ,CDSQ,CC

      CHARACTER*6 TRFFILE(5)
      DATA TRFFILE /'trf   ',
     &              'trfdc ',
     &              'trfds ',
     &              'trfuc ',
     &              'trfus '/
      DATA OPTION /'OASP  ','      '/         
      DATA TITLEY(1) /'NORMAL STRESS  ('/,        
     1     TITLEY(2) /'VERTICAL VEL.  ('/,        
     2     TITLEY(3) /'RADIAL VEL.    ('/,        
     3     TITLEY(4) /'Um - Vm        ('/,        
     4     TITLEY(5) /'RADIAL STRESS  ('/,        
     &     TITLEY(6) /'BULK PRESSURE  ('/,        
     &     TITLEY(7) /'SHEAR STRESS   ('/        
      DATA XAXI,YAXI /20.0,12.0/
      DATA CONMAX /NPAR*-1E20/
C           
C ********************FORMATS*******************               
C           
C           
 209  FORMAT(1H ,'**********************************',
     &      /1H ,'*       OASES Version ',f3.1,'       *',
     &      /1H ,'**********************************',
     &     //1H ,'Pulse Module OASP' )
 210  FORMAT(//1H ,A)         
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
      TMIN=0e0
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
c >>> Initialize Bessel function origin
      brk_0=0e0
      mbf_0=0
c >>> Initialize roughness spectra
      goff=.false.
      pierson=.false.
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
      write(6,209) version
      WRITE(6,210)TITLE      
      CALL GETOPT(IGRP,ISTACK,IBODY,ISDEP,IPROF,ICNTIN,CFRFL,ICONTU,
     &            PLPOUT,CDROUT,GENTS,PAR)
      if (doppler) then
        read(1,*) freqs,offdbin,istyp,vsou,vrec
      else IF (ICNTIN.GT.0) THEN
        READ(1,*) FREQS,OFFDBIN
      ELSE
        OFFDBIN=0E0
        READ(1,*)FREQS       
      END IF
      TOTTIM=0.
      WRITE(6,500)FREQS      
      if (doppler) then
       write(6,*) 'Source type:            ',istyp
       write(6,*) 'Source velocity:   ',vsou
       write(6,*) 'Receiver velocity: ',vrec
      end if
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
C ***
      IF (CMININ.EQ.0) STOP '*** CMIN MUST BE NON-ZERO ***'
      IF (((CMAXIN.GT.0).AND.(CMININ.GT.0)).OR.
     1    ((CMAXIN.LT.0).AND.(CMININ.LT.0))) THEN
       NFLAG=.FALSE.
      ELSE
       IF (CMININ.LE.0) STOP '*** CMIN/CMAX CONFLICT ***'
       CMAXIN=1E8
       NFLAG=.TRUE.
      END IF

      PLPOUT=PLPOUT.OR.((INTF.GT.0).AND.(.NOT.ICONTU))
      NWVNO=MIN0(NWVNO,NP)
c *** TIME-FREQUENCY-RANGE PARAMETERS
      READ(1,*) NX,FR1,FR2,DT,R0,RSPACE,NPLOTS
c >>> definitions changed for tau-p seismograms
      if (inttyp.eq.-1) then
       nwvno=max(10,nwvno)
       icw1=1
       icw2=nwvno
       ausamp=.false.
c >>>  slowness integration defaulted
       ibody=1
c >>> receiver ranges replaced by slowness
       r0=1e3/cmaxin
       rspace=(1e3/cminin - r0)/(nwvno-1)
       nplots=nwvno
      end if 
  
C *** DETERMINE FREQUENCY SAMPLING FOR AUTOMATIC SAMPLING

c >>> Sampling must be better than Nyquist
       if (dt.gt.0e0) then
 989     if (dt.gt.2.5/fr2) then
          dt=dt*0.5
          nx=nx*2
          go to 989
         end if
       else
        dt=2.5/fr2
       end if

      IF (nx.lt.0) THEN
       CALL VMAX(V(1,2),1,CREF,NUML)
       RM=MAX(ABS(R0),ABS(R0+(NPLOTS-1)*RSPACE))*1E3
       TREQ=RM/CREF
       NX=nint(TREQ/DT)

       IPOW=9
 990   IPOW=IPOW+1
       II=2**IPOW
       IF (II.LT.NX) GO TO 990
       NX=II

       WRITE(6,*)
       WRITE(6,*) '>>> AUTOMATIC FREQUENCY SAMPLING PARAMETERS'
       WRITE(6,*) '     NX=',NX
       write(6,*) '     DT=',DT
      ELSE
C
C     CHECK THAT NX IS INTEGER POWER OF 2
C
       IPOW=0
  991  IPOW=IPOW+1
       II=2**IPOW
       IF (II.LT.NX) GO TO 991
       IF (II.NE.NX) THEN
        NX=II
        WRITE(6,*)
        WRITE(6,*) '>>>> NT MUST BE INTEGER POWER OF 2, CHANGED TO',
     &             NX,' <<<<'      
       END IF
       NX=MIN0(NX,2*NP)
      END IF
      R1=R0*1000.            
      DLFREQ=1E0/(DT*NX)     
      MX=nint(FR2/DLFREQ)+1
      LX=nint(FR1/DLFREQ)+1
      LX=MAX0(LX,1)
      MX=MIN0(MX,NX/2)
      LXP1=MAX(2,LX)
      if ((fr2-fr1).lt.dlfreq) then
       mx=lxp1
      end if        
C      FREQM=DLFREQ*(MX-1)
C     Gaude Hope patch for paroases    
      IF (PAR) then
       FREQM=FREQS
      ELSE
       FREQM=DLFREQ*(MX-1)
      end if
      FREQ0=DLFREQ*(LXP1-1)    
      freq1=freq0
      freq2=freqm
c >>> create file for scattering rhs
      if (sctout) then
       call opfilb(45,ioer)
       call opfilb(46,ioer)
       write(45) nx,fr1,fr2,dt
      end if
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
c >>> If option J, use complex wn, otherwize complex frequency.
       if (ICNTIN.eq.1) then
c >>> Transmission loss computation
        CFRFL=.FALSE.
c >>> max and min ranges
        rm =0e0
        do ii=1,nplots
         rr=abs(1E3*(r0+(ii-1)*rspace))
         rm=max(rm,rr)
        end do
        rmi=abs(rspace*1e3)
        ranref=3*rm
        cref=0e0
       else 
        CFRFL=.TRUE.
        ICNTIN=0
        cref=0e0
        do 981 l=1,numl
         if (v(L,2).lt.2E4) cref=max(cref,v(L,2))
 981     continue
        RANREF=CREF*(NX*DT)
c >>> max and min ranges
        rm =0e0
        rmi=max(abs(rspace*1e3),ranref*0.1)
        do ii=1,nplots
         rr=abs(1E3*(r0+(ii-1)*rspace))
         rm=max(rm,rr)
        end do
c       RANREF=RANREF+RM
c       ranref=min(ranref+rm,6*rm)
        ranref=max(ranref,6*rm)
       end if
       OFFDBIN=0E0
       WRITE(6,*)
       WRITE(6,*) '>>> AUTOMATIC SAMPLING '
       if (ICNTIN.eq.1) then
        write(6,*) '    Real Frequency Integration <<<' 
        write(6,*) '    Complex Wavenumber Integration <<<'
       else 
        WRITE(6,*) '    Complex Frequency Integration'
        WRITE(6,*) '    Real Wavenumber Integration'
       end if
       write(6,*) '    REFERENCE SPEED:',CREF
       WRITE(6,*) '    REFERENCE RANGE:',RANREF
       FREQ=FREQM
       CALL AUTSAM(CMININ,CMAXIN,rmi,RANREF,CMIN,CMAX,
     &             NWVNO,ICW1,ICW2,ibody)
       write(6,*) 'autinp:',cminin,cmaxin,rmi,ranref,cmin,cmax,
     &              nwvno,icw1,icw2,ibody
       WRITE(6,*) '    MAX NO. OF WAVENUMBERS:',NWVNO
       ICUT1=1
       ICUT2=NWVNO
      ELSE
       CMIN=CMININ
       CMAX=CMAXIN
      END IF 
       write(6,*) '>>> Automatic Sampling <<<'
       write(6,*) '    Cmin,Cmax=',cmin,cmax
       WRITE(6,*) '    NO. OF WAVENUMBERS:',NWVNO
       write(6,*) '    ICW1,ICW2=    ',icw1,icw2
C
C     CHECK WHETHER TO INVOKE DEFAULT CONTOUR OFFSET
C
      IF (ICNTIN.GT.0.AND.OFFDBIN.LT.1E-10) THEN
       if (INTTYP.eq.2) then
        write(6,*) 'Bessel Integration'
        OFFDB=40.0*V(LAYS((LS-1)/2+1),2)*(1E0/CMIN-1E0/CMAX)/NWVNO
       else
        OFFDB=60.0*V(LAYS((LS-1)/2+1),2)*(1E0/CMIN-1E0/CMAX)/NWVNO
       end if
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
        OMEGIM=-LOG(50.0)*DLFREQ
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
      WK0 = 2*PI*FREQM / CMAX
      WKMAX = 2*PI*FREQM / CMIN               
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
      END IF
       WRITE(6,600)CMIN,CMAX  
       WRITE(6,550)NWVNO,ICW1,ICW2,ICUT1,ICUT2           
      IF (NFLAG) THEN
       WRITE(6,*) 'NEGATIVE SPECTRUM BY SYMMETRY'
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
        NCOMPO=5
      ELSE
        NCOMPO=1
      END IF
      LUGRN=30
      LUTRF=35
C
C     PLOT OF VELOCITY PROFILE IF OPTION 'Z' WAS CHOSEN
C
      IF (IPROF.GT.0) THEN
        READ(1,*) VPLEFT,VPRIGHT,VPLEN,VPINC
        READ(1,*) DPUP,DPLO,DPLEN,DPINC
        CALL PLPROF(TITLE,VPLEN,DPLEN,VPLEFT,VPRIGHT,VPINC,
     2              DPUP,DPLO,DPINC)
      END IF

c
c >>> Patch scattering stuff. Transferred from Jayong's OASK
c
      if (outpot) then
C
C     READ patch parameters
C
        READ(1,*) PCENTER,INPATCH,NFFT_X,NFFT_Y,SPLEN_X,SPLEN_Y
        PCENTER = ABS(PCENTER)
        NFFT_X = ABS(NFFT_X)
        NFFT_Y = ABS(NFFT_Y)
        SPLEN_X = ABS(SPLEN_X)
        SPLEN_Y = ABS(SPLEN_Y)
        IF (INPATCH.GT.NUMI.OR.INPATCH.LT.1) STOP 'INVALID INPATCH'
        INPATCH1 = INPATCH + 1
        PATCHX1 = PCENTER*1.E3 - SPLEN_X/2.
        PATCHX2 = PCENTER*1.E3 + SPLEN_X/2.
        PATCHY1 = -SPLEN_Y/2.
        PATCHY2 =  SPLEN_Y/2.
        CALL OPFILB(60,IOER)
        WRITE(60) INPATCH,NUMFR,ICDR
        WRITE(60) NFFT_X,NFFT_Y,SPLEN_X,SPLEN_Y
        WRITE(60) PATCHX1,PATCHX2,PATCHY1,PATCHY2
        WRITE(60) (V(INPATCH,J),J=1,6)
        WRITE(60) (V(INPATCH1,J),J=1,6)
        IF (INPATCH1.LE.NUML) THEN
           WRITE(60) V(INPATCH1+1,1)
        ELSE
           WRITE(60) 0.
        END IF
        WRITE(6,*) 
        WRITE(6,*) '>>>> PATCH PARAMETERS <<<<'
        WRITE(6,*) '     PCENTER (KM), INPATCH = ',PCENTER,INPATCH
        WRITE(6,*) '     NFFT_X, SPLEN_X (M)   = ',NFFT_X,SPLEN_X
        WRITE(6,*) '     NFFT_Y, SPLEN_Y (M)   = ',NFFT_Y,SPLEN_Y
        WRITE(6,*)
      end if
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

c >>> for patch output no sources in last two layers
      if (outpot) then
       if ( nosou(inpatch).gt.0 .or. nosou(inpatch1).gt.0) then
        write(6,*) '>>> For patch output no sources in patch layers'
        stop
       end if
      end if
C
C     SLOWNESS DIAGRAM FOR TRANS. ISOTR. MEDIA
C
      IF (NTISOL.GT.0.and.IPROF.GT.0) THEN
       CALL SNSDGM(title)
      END IF
C *** PREPARE BESSEL FUNCTIONS FOR FULL INTEGRATION
      IF (INTTYP.EQ.2) THEN
c       CALL PREPBF(RRMAX,WKMAX*1.3)
c >>> v2.2 has tapered full Bessel below kr=20*pi
       mbf_0=0
       brk_0=max(0e0,mbf_0-10.0*pi)
       bfrmax=(10*pi+mbf_0)/wkmax
       CALL PREPBF(bfrmax,WKMAX)
       ICDR=0
       write(6,*) '>>> Bessel functions tabulated <<<'
       write(6,*) '>>> Max problem kr:', wkmax*rrmax,' m^-1'
       write(6,*) '>>> Max Bessel kr: ', bfrmax*wkmax,' m^-1'
      END IF
c
c >>> source spectrum for doppler compensation
c
      if (doppler) then
       call spulse(freqs,dt,nx,lxp1,mx)
      end if

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

c     Get dispersive layer parameters
c      call get_disper(jj)
      
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
        CALL AUTSAM(CMININ*FREQ/FREQM,CMAXIN*freq/freqm,
     &              rmi,RANREF,CMIN,CMAX,
     &              NWVNO,ICW1,ICW2,ibody)
       END IF

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
        if (INTTYP.eq.2) then
         OFFDB=40.0*V(LAYS((LS-1)/2+1),2)*(1E0/CMIN-1E0/CMAX)/NWVNO
        else
         OFFDB=60.0*V(LAYS((LS-1)/2+1),2)*(1E0/CMIN-1E0/CMAX)/NWVNO
        end if
        WRITE(6,*) 'DEFAULT CONTOUR OFFSET APPLIED,',OFFDB,
     &             ' dB/wavelength'
       END IF
       IF (NFLAG) THEN
        WRITE(6,*) 'NEGATIVE SPECTRUM BY SYMMETRY'
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

      WRITE(6,*) '    Cmin,Cmax:  ',CMIN,CMAX
      write(6,*) '    K_r0,dK_r:  ',wk0,dlwvno
      WRITE(6,*) '    Wavenumber samples:',NWVNO,ICW1,ICW2
      write(6,*)

      CALL PINIT2            

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
       CALL GETKNL(IR/2+1,M,1)
        IF (ICONTU) THEN
          NN=ICUT2-ICUT1+1
          DO 510 I=1,NPAR
          IF (IOUT(I).GT.0.AND.M.EQ.1) THEN
            if (I.gt.1.and.i.lt.5) then
c >>> change velocities to micro-meter/sec in kernels
             call vsmul(cff(icut1,I),1,1E6,cff(icut1,I),1,2*nn)
            end if 
            CALL CVMAGS(CFF(ICUT1,I),2,FFS,1,NN)
            call vclip(ffs,1,1E-30,1E30,ffs,1,nn)
            CALL VALG10(FFS,1,FFS,1,NN)
            CALL VSMUL(FFS,1,1E1,FFS,1,NN)
            CALL VDECIM(FFS,1,FFS,1,NN,NDEC,NC)
            CALL WRBUF(27,FFS,NC)
            CALL VMAX(FFS,1,AAA,NC)
            CONMAX(I)=MAX(CONMAX(I),AAA)
          END IF
510       CONTINUE
        else if (determ) then
          call pldetm(dlwvno,wk0,title,xaxi,yaxi)
        ELSE
          CALL PLINTM(DLWVNO,WK0,SD,RDC(IR/2+1),TITLE,M,   
     1             XAXI,YAXI)
          IF (IOUT(1).GT.0 .AND. IOUT(2).GT.0) THEN
C Plot vertical intensity vs wavenumber
            CALL PLVINT(DLWVNO,WK0,SD,RDC(IR/2+1),TITLE,M,   
     1                  XAXI,YAXI)
          END IF
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
 9988 FORMAT(1H ,'FREQ. NO.',I4,' : ',F10.3,' HZ',             
     1       F10.3,' SECS')
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
c
c >>> close patch file
c
      if (outpot) then
       close(60)
      end if
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
     1                  ICONTU,PLPOUT,CDROUT,GENTS,PAR)      
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
C           
C     INPUT OF OPTIONS       
C           
      INCLUDE 'compar.f'
      LOGICAL CFRFL,ICONTU,PLPOUT,CDROUT,GENTS,PAR
      CHARACTER*1 OPT(40)
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
      mom_sou=.false.
      ICONTU=.FALSE.
      DECOMP=.FALSE.
      TRFOUT=.TRUE.
      PLPOUT=.FALSE.
      CDROUT=.FALSE.
      GENTS=.FALSE.
      PAR=.FALSE.
      DETERM=.FALSE.
      sctout=.false.
      double_trf=.false.
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
       NOUT=NOUT+1            
       IOUT(3)=1              
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
 3061  FORMAT(1H ,'BULK PRESSURE')             
      ELSE IF (OPT(I).EQ.'S') THEN
       IF (IOUT(7).GT.0) GO TO 50              
       NOUT=NOUT+1            
       IOUT(7)=1              
       WRITE(6,'(1h ,a)') 'SHEAR STRESS'             
      ELSE IF (OPT(I).EQ.'G') THEN
       IF (IGRP.GT.0) GO TO 50
       IGRP=1
       PLPOUT=.TRUE.
c >>> enable the following statement if dispersion should use
c     determinant
c       DETERM=.TRUE.
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
      ELSE IF (OPT(I).EQ.'v') THEN
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
      else if (opt(i).eq.'8') then
        if (.not.double_trf) then
         double_trf=.true.
         write(6,'(1h ,a)') 'DOUBLE PRECISION TRF FILE'
        end if
      ELSE IF (OPT(I).EQ.'X'.or.opt(i).eq.'2') THEN
        IF (SHEAR) GO TO 50
        srctyp = 2
        SHEAR=.TRUE.
        ver_for=.true.
        WRITE(6,318)
 318    FORMAT(1H ,'VERTICAL POINT FORCE IN  SOLID MEDIA')
      ELSE IF (opt(i).eq.'h'.or.opt(i).eq.'3') THEN
        IF (hor_for) GO TO 50
        srctyp=3
        hor_for=.true.
        WRITE(6,3180)
 3180   FORMAT(1H ,'HORIZONTAL POINT FORCE IN SOLID MEDIA')
      ELSE IF (opt(i).eq.'4') THEN
        IF (dip_sou) GO TO 50
        srctyp=4
        dip_sou=.TRUE.
        WRITE(6,3171)
 3171   FORMAT(1H ,'DIP-SLIP SOURCE IN SOLID MEDIA')
      ELSE IF (OPT(I).EQ.'m'.or.opt(i).eq.'5') THEN
        IF (mom_sou) GO TO 50
        srctyp=5
        mom_sou=.TRUE.
        WRITE(6,3172)
 3172   FORMAT(1H ,'MOMENT SOURCE IN SOLID MEDIA')
      ELSE IF (OPT(I).EQ.'C') THEN
        IF (ICONTU) GO TO 50
        ICONTU=.TRUE.
        CDROUT=.TRUE.
        WRITE(6,319)
 319   FORMAT(1H ,'FREQUENCY/SLOWNESS CONTOURS OF INTEGRANDS')
      ELSE IF (OPT(I).EQ.'r') THEN
        IF (rftvty) GO TO 50
        rftvty=.TRUE.
        WRITE(6,3191)
 3191   FORMAT(1H ,'Reflectivity Seismograms (No Source Contribution)')
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
      else if (opt(i).eq.'d') then
        doppler=.true.
        write(6,'(A)') 'Moving source and receivers'
      else if (opt(i).eq.'x') then
        extrap=.true.
        write(6,'(A)') 'Using kernel extrapolation'
      ELSE IF (OPT(I).EQ.'s') THEN
       IF (SCTOUT) GO TO 50
       SCTOUT=.TRUE.
       WRITE(6,3092)
 3092  FORMAT(1H ,'OUTPUT OF SCATTERING DISCONTINUITIES')
      ELSE IF (OPT(I).EQ.'g') THEN
       IF (goff) GO TO 50
       goff=.true.
       WRITE(6,'(a)') 'Goff-Jordan power spectrum'
      else if (opt(i).eq.'E') then
        if (outpot) go to 50
        outpot=.true.
        write(6,'(1h ,a,f6.1,a)') 
     &        'Output of patch scattering potentials'
C >>> Gaute Hope patch for paroases
      ELSE IF (opt(i).EQ.'|') THEN
        PAR=.true.
        write (6,*) 'IN PARALLEL JOB (only for internal use!)'
      ELSE
      END IF
 50   CONTINUE               
      GENTS=(ISTACK.GT.-1)
      IF (ISTYP.LT.0) THEN   
       ISTYP=2                
      END IF
c      IF (INTTYP.EQ.2) THEN
c       ICNTIN=0
c       write(6,*) 
c     &   '>>> Warning: Real contour for Bessel Integration <<<'
c      END IF
      IF (NOUT.NE.0) RETURN  
      IOUT(1)=1              
      NOUT=1
      WRITE(6,301)           
      RETURN
      END

      SUBROUTINE AUTSAM(C1,C2,rmin,RMAX,CMIN,CMAX,NW,IC1,IC2,ibody)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
c      PARAMETER (RFAC=1.5)
      PARAMETER (NR=150)
c >>> Minimum # of k, # of periods of exponential over taper interval
      PARAMETER (NKMIN=200,WTAPER=10.0)
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      write(6,*) '>>> Reference range for sampling:',rmax,' m'
      if (inttyp.eq.1) then
        RFAC=1e0
      else if (inttyp.eq.2) then
c        RFAC=3E0
        RFAC=1.5e0
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
       IC2=MIN(IC2,NW)
       CMIN=2*PI*FREQ/WNMAX
       CMAX=2*PI*FREQ/WNMIN
       RETURN
       END


      BLOCK DATA BLKPUL
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
C
C**** DEFINITION OF MAX REAL ARGUMENT TO THE EXPONENTIAL FUNCTION
      COMMON /ARGMAX/ AM
C**** THE FOLLOWING DEFINITION SHOULD BE USED FOR THE FPS164
CFPS  DATA AM /300./
C**** THE FOLLOWING DEFINITION SHOULD BE USED FOR THE VAX
      DATA AM /65./     
      DATA OMEGIM /0.0/
      DATA PROGNM /'OASP17'/
C3D   DATA PROGNM /'OASP3 '/
      DATA LUGRN,LUTRF,LUTGRN,LUTTRF /30,35,30,35/
      DATA SHEAR,DECOMP,SCTOUT,NFLAG /.FALSE.,.FALSE.,.FALSE.,.FALSE./
      DATA MSUFT,MBMAX,MBMAXI,SRCTYP,ISROW,ISINC /1,1,2,1,1,0/
      data bintrf /.true./
      END
