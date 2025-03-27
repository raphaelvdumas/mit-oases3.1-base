      PROGRAM OASPS30
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
C     2-d - TRANSVERSE ISOTROPY - PULSE VERSION          
C     Version for computing reverberation realizations
c     from roughness and volume inhomogeneities
      PARAMETER (SQ2PI=2.506628,ONEO2PI=1.591549E-1)
c sizes moved to compar.f 051806 HS
c      parameter (nkrexp=14,nkzexp=9,nkrm=2**nkrexp,nkzm=2**nkzexp,
c     &           nkrz=2*nkrm*nkzm)
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
      include 'combes.f'

      COMPLEX SLOW
      LOGICAL CFRFL,ICONTU,PLPOUT,CDROUT,GENTS,AUSAMP
      CHARACTER*50 FILENM
      DIMENSION X(NP2,NPAR),FF(2,NP3),PX(MODULO)   
      DIMENSION CONMAX(NPAR)
      COMPLEX CTRF(NPAR)
      COMPLEX CRAN(2*NP,3),CDUMMY

      DIMENSION FFS(2,NP),XS(NP2),AKM(100),AKM1(100),AKM2(100)           
      DIMENSION GRPVEL(NMOD),PHVEL(NMOD)
      CHARACTER*16 TITLEY(12)
      CHARACTER*6  OPTION(2)
      CHARACTER*80 TITLE
      logical cylgeo,true_ran,volume,density
      integer iseed
      save iseed
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
     &     TITLEY(6) /'BULK STRESS    ('/,        
     &     TITLEY(7) /'SHEAR STRESS   ('/        
      DATA XAXI,YAXI /20.0,12.0/
      DATA CONMAX /NPAR*-1E20/
C           
C ********************FORMATS*******************               
C           
C           
 210  FORMAT(/1H ,
     &       'OASES PULSE PROGRAM. Version 1.7, Update 10-Mar-1994',
     &      //1H ,A)         
500   FORMAT(/1H ,'SOURCE CENTRE FREQUENCY:',F10.2,' HZ ')             
550   FORMAT(/1H ,3X,'NW',5X,' ICW1',5X,' ICW2',
     &       5X,'ICUT1',5X,'ICUT2',/(1X,I5,4I10))  
  600 FORMAT(//,'  CMIN = ',G16.6,' M/S ',/,'  CMAX = ',G16.6,' M/S ')          
C           
C           
C **********************************************               
C           
c      call cltime()
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
      cylgeo=.true.
c >>> Initialize Bessel function origin
      brk_0=0e0
      msft=1
      mbf_0=msft/2
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
      IF (IOER.GT.0) STOP '>>>> ERROR: INPUT FILE 1 NOT FOUND <<<<'
      CALL OPFILb(45,IOER)
      CALL OPFILb(46,IOER)
      if (bistat) CALL OPFILb(47,IOER)
      IF (IOER.GT.0) STOP '>>>> ERROR: INPUT FILE 45 NOT FOUND <<<<'
      READ(1,'(A)')TITLE       
      WRITE(6,210)TITLE      
      CALL GETOPT(IGRP,ISTACK,IBODY,ISDEP,IPROF,ICNTIN,CFRFL,ICONTU,
     &            PLPOUT,CDROUT,GENTS,cylgeo,true_ran)
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
      write(6,*) 'calling INSRC'
      CALL INSRC(SD)
      write(6,*) 'calling INREC'
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
      msft_0=intf
      mbf_0=msft_0/2
      intf=0

      AUSAMP=(NWVNO.LT.1)  
      PLPOUT=PLPOUT.OR.((INTF.GT.0).AND.(.NOT.ICONTU))
      NWVNO=MIN0(NWVNO,NP)

c *** TIME-FREQUENCY-RANGE PARAMETERS
      READ(1,*) NX,FR1,FR2,DT,R0,RSPACE,NPLOTS
      read(45) nx_in_file,fr1_in_file,fr2_in_file,dt_in_file
      if ((nx.ne.nx_in_file).or.(abs(dt-dt_in_file).gt.1e-3*dt)) then
       write(6,*) '>>> WARNING: Frequency sampling mismatch    <<<'
       write(6,*) '>>>          Replaced by values in rhs-file <<<'
      end if
      nx=nx_in_file
      fr1=fr1_in_file
      fr2=fr2_in_file
      dt=dt_in_file
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

c >>> Restrictions for cylindrical geometry
      if (cylgeo) then
       icdr=0
       if (cmaxin.lt.cminin) then
        write(6,*) 
     &    '>>> No negative spectrum in cylindrical geometry <<<'
       end if
C >>> For cylindrical geometry first wavenumber must be ~ 0.
       cmaxin=1e12
c >>> and Bessel integration forced
       if (inttyp.ne.2) then
        inttyp=2
        write(6,*) '>>> Full Hankel transform forced <<<'
       end if
      else
       icdr=1
      end if

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
      FREQM=DLFREQ*(MX-1)    
      FREQ0=DLFREQ*(LXP1-1)    
      freq1=freq0
      freq2=freqm

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
     &                      nwvno,icw1,icw2,ibody
       write(6,*) '>>> Automatic Sampling <<<'
       write(6,*) '    Cmin,Cmax=',cmin,cmax
       WRITE(6,*) '    NO. OF WAVENUMBERS:',NWVNO
       write(6,*) '    ICW1,ICW2=    ',icw1,icw2

       if (cylgeo) then
c >>> Cylindrical geometry only positive wavenumbers
        icut1=1
        icut2=nwvno
       else
c >>> Plane geometry: scattering kernels not symmetric
        nflag=.false.
        cmax=-cmin
        icw1=nwvno-icw2+1
        icw2=icw2+nwvno
        nwvno=2*nwvno
        ICUT1=1
        ICUT2=NWVNO
       end if
      ELSE
       nflag=.false.
       CMIN=CMININ
       CMAX=CMAXIN
       ICUT1=1
       ICUT2=NWVNO
      END IF 
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

       if (cylgeo) then
        write(6,*) '>>> Cylindrical Geometry <<<'
       else
        write(6,*) '>>> Plane Geometry <<<'
       end if


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
c
c >>> source spectrum for doppler compensation
c
      if (doppler) then
       call spulse(freqs,dt,nx,lxp1,mx)
      end if

C >>> select wavenumber sampling for rough surface
       FREQ=FREQM
C >>> MAKE RANDOM PHASES
      if (true_ran) then
       iseed=-itran()
      else
       ISEED=-123-msft_0
      end if
      write(6,*) 'Time:',t1
      write(6,*) 'Random number generator seed:',iseed
c      ISEED=-9
      wnk1=0
      dlwh=dlwvno
      nwvnor=2
 1110 nwvnor=2*nwvnor
      if (nwvnor.lt.nwvno) go to 1110
       
      wnk2=dlwh*(nwvnor-1)
C >>> interface info from file 45
      read(45) ff1,nnn,mmm,ldummy,dlmean
      read(45) cdummy,INTFCE
      rewind(45)
      read(45) nx_in_file,fr1_in_file,fr2_in_file,dt_in_file
      write(6,*) 'rough(intfce)',rough(intfce),'clen(intfce)'
     c     ,clen(intfce)
      volume=((rough(intfce).lt.0E0).and.(clen(intfce).lt.0E0))
      write(6,*) 'volume=',volume

      if (volume) then
       write(*,*) 'nwvnor=',nwvnor
       write(*,*) 'nnkr=',nnkr
       if (nwvnor.gt.nnkr) stop '>>> Too many wavenumbers <<<'
       ln=intfce
       nvol=6
       write(6,*) '>>> Volume scattering Layer:',ln
       write(6,'(a,f6.1,a)') '>>> L_z  = ',-fac(1+(ln-1)*nvol),' m'
       write(6,'(a,f6.1,a)') '>>> L_x  = ',-fac(2+(ln-1)*nvol),' m'
       write(6,'(a,f6.1,a)') '>>> Skew = ', fac(3+(ln-1)*nvol),' deg'
       write(6,'(a,f6.1,a)') '>>> Dim  = ', fac(4+(ln-1)*nvol),'  '
       write(6,'(a,e6.1,a)') '>>> dc/c = ', fac(5+(ln-1)*nvol),'  '
       write(6,'(a,f6.1,a)') '>>> Gamm = ', fac(6+(ln-1)*nvol),'  '

       r_l=abs(clen(ln))
       z_l=abs(rough(ln))
       skew=fac(3+(ln-1)*nvol)
       if (goff) then
        amodel=fac(4+(ln-1)*nvol)
       else
        amodel=-1
       end if
       pow=fac(5+(ln-1)*nvol)
       gama=fac(6+(ln-1)*nvol)

       ttt=v(ln+1,1)-v(ln,1)
       nkzm_req=1
 1117  nkzm_req=nkzm_req*2
       if (ttt/(1.0*nkzm_req).gt.(v(ln,2)/freqm/4.0)) go to 1117
       nkzm_req=nkzm_req/2
 1118  nkzm_req=nkzm_req*2
       if (ttt/(1.0*nkzm_req).gt.(z_l/4.0)) go to 1118
       write(*,*) "nkzm_req=",nkzm_req,"layer velocity=",v(ln,2)
       write(*,*) "layer thickness=",ttt,"max freq=",freqm
       write(*,*) "vertical correlation=",z_l,"dz=",ttt/(1.0*nkzm_req)

       call pv(nwvnor,2*pi/dlwh,nkzm_req,ttt,
     &         r_l,z_l,amodel,skew,cylgeo,iseed)

       write(6,*) '>>> EXIT PV <<<'
      else
       write(*,*) 'nwvnor=',nwvnor
       write(*,*) 'nnkr=',nnkr
       if (nwvnor.gt.nnkr) stop '>>> Too many wavenumbers <<<'

       ln=intfce
       nvol=3
       write(6,*) '>>> Roughness scattering Layer:',ln
       RG2=ROUGH(INTFCE)**2
       write(6,*) 'intfce,rg2,corl=',intfce,rg2,clen(intfce)
       nkzm_req=1
       skew=0e0
       nvol=3
       r_l=abs(clen(ln))
       z_l=1E10
       if (goff) then
        amodel=fac(3+(ln-1)*nvol)
       else
        amodel=-1
       end if
       pow=sqrt(rg2)
       call pv(nwvnor,2*pi/dlwh,nkzm_req,0e0,
     &         r_l,z_l,amodel,skew,cylgeo,iseed)
       write(6,*) '>>> EXIT PV <<<'
      end if

c >>> disable roughness
c      if (volume) then      
c       do ill=1,numl
c        rough(ill)=0e0
c        rough2(ill)=0e0
c       end do
c      end if

C >>> disable sources for scattered field only
      if (sctout) then
       do 1112 ii=1,4
 1112   numts(ii)=0
       do 1113 ii=1,numl
 1113   nosou(ii)=0
      end if

C *** PREPARE BESSEL FUNCTIONS FOR FULL INTEGRATION
      IF (cylgeo) THEN
c       CALL PREPBF(RRMAX,WKMAX*1.3)
c >>> v2.2 has tapered full Bessel below kr=20*pi
       brk_0=max(0e0,mbf_0-10.*pi)
       bfrmax=(10*pi+mbf_0)/wkmax
       CALL PREPBF(bfrmax,WKMAX)
       write(6,*) '>>> Bessel functions tabulated <<<'
       write(6,*) '>>> Fourier order:',mbf_0,' <<<'
       write(6,*) '>>> Max problem kr:', wkmax*ranref
       write(6,*) '>>> Max Bessel kr: ', bfrmax*wkmax
       write(6,*) '>>> Min Bessel kr: ', brk_0
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

      if (cylgeo) then
       write(6,*) '>>> Cylindrical Geometry <<<'
      else
       write(6,*) '>>> Plane Geometry <<<'
      end if
      WRITE(6,*) '    Cmin,Cmax:  ',CMIN,CMAX
      write(6,*) '    K_r0,dK_r:  ',wk0,dlwvno
      WRITE(6,*) '    Wavenumber samples:',NWVNO,ICW1,ICW2
      write(6,*)

      CALL PINIT2            
 
      if (volume) then
       if (cylgeo) then
        write(6,*) 'dlwvno,dlwh=',dlwvno,dlwh
        call CALSVC(dlwh,nwvnor,ttt,nkzm_req,gama,pow)
       else
        call CALSVP(dlwh,nwvnor,ttt,nkzm_req,gama,pow)
       end if
      else if (cylgeo) then
        call CALSRC(dlwh,nwvnor,pow)
      else
        call CALSRP(dlwh,nwvnor,pow)
      end if

      CALL CHKSOL
C *** TOTAL FIELD FOR PLOTS ETC.
      LUTGRN=LUGRN

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
 9988 FORMAT(1H ,'FREQ. NO.',I4,' : ',F10.3,' HZ',             
     1       F10.3,' SECS')
c >>> end of frequency loop <<<
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
     1                  ICONTU,PLPOUT,CDROUT,GENTS,cylgeo,true_ran)      
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
      LOGICAL CFRFL,ICONTU,PLPOUT,CDROUT,GENTS,cylgeo,true_ran
      CHARACTER*1 OPT(40)
      logical momsou
      common /msrclo/ momsou
      WRITE(6,300)           
 300  FORMAT(/1H ,'OPTIONS:',/)                
      INTTYP=0
      NOUT=0
      IREF=0
      ISTYP=-1           
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
      bistat=.false.
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
 3061  FORMAT(1H ,'BULK STRESS')             
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
       IF (.not.cylgeo) GO TO 50
       cylgeo=.false.
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
      ELSE IF (OPT(I).EQ.'X'.or.opt(i).eq.'2') THEN
        IF (SHEAR) GO TO 50
        srctyp = 2
        SHEAR=.TRUE.
        WRITE(6,318)
 318    FORMAT(1H ,'VERTICAL POINT FORCE IN  SOLID MEDIA')
      ELSE IF (OPT(I).EQ.'m'.or.opt(i).eq.'3') THEN
        IF (momsou) GO TO 50
        srctyp = 3
        momsou=.TRUE.
        WRITE(6,3181)
 3181   FORMAT(1H ,'MOMENT FORCE IN  SOLID MEDIA')
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
      else if (opt(i).eq.'d') then
        doppler=.true.
        write(6,'(A)') 'Moving source and receivers'
      ELSE IF (OPT(I).EQ.'s') THEN
       IF (SCTOUT) GO TO 50
       SCTOUT=.TRUE.
       WRITE(6,3092)
 3092  FORMAT(1H ,'OUTPUT OF SCATTERED FIELD ONLY')
      ELSE IF (OPT(I).EQ.'g') THEN
       IF (goff) GO TO 50
       goff=.true.
       WRITE(6,'(a)') 'Goff-Jordan power spectrum'
      ELSE IF (OPT(I).EQ.'p') THEN
       IF (rescat) GO TO 50
       rescat=.true.
       WRITE(6,'(a)') 'Perturbed boundary operator for scattering'
      ELSE IF (OPT(I).EQ.'r') THEN
       IF (true_ran) GO TO 50
       true_ran=.true.
       WRITE(6,'(a)') 'True random number generator'
      ELSE IF (OPT(I).EQ.'b') THEN
       IF (bistat) GO TO 50
       bistat=.true.
       WRITE(6,'(a)') 'Bistatic computation'
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

      FUNCTION RAN2(IDUM)
      PARAMETER (M=714025,IA=1366,IC=150889,RM=1.4005112E-6)
      DIMENSION IR(97)
      common /ran2cm/ iff,ir
      IF(IDUM.LT.0.OR.IFF.EQ.0)THEN
        IFF=1
        IDUM=MOD(IC-IDUM,M)
        DO 11 J=1,97
          IDUM=MOD(IA*IDUM+IC,M)
          IR(J)=IDUM
11      CONTINUE
        IDUM=MOD(IA*IDUM+IC,M)
        IY=IDUM
      ENDIF
      J=1+(97*IY)/M
      IF(J.GT.97.OR.J.LT.1) then
       write(6,*) 'RAN2: J=',j
       PAUSE
      end if
      IY=IR(J)
      RAN2=IY*RM
      IDUM=MOD(IA*IDUM+IC,M)
      IR(J)=IDUM
      RETURN
      END

      Integer function Itran()
c
c     returns an integer between 0 and 9999 dependent on date and time,
c     to be used as a 'random' random number generator seed
c
      character*30 dum
      call system('date > dum.dum ')
      call system('cat dum.dum ')
      open(10,file='dum.dum',form='formatted',status='old')
      read(10,'(a)') dum
      close(10,status='delete')
c      write(6,*) dum
      itran=0
      do i=1,len(dum)
       itran=itran+i*(30-i)*ichar(dum(i:i))
      end do
c      write(6,*) itran
      itran=mod(itran,10000)
      return
      end

      block data ran2da
      DIMENSION IR(97)
      common /ran2cm/ iff,ir
      DATA IFF,ir /0,97*0/
      end
