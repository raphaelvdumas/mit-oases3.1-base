      PROGRAM OASTL
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
C     UNIX-FORTRAN - TRANSVERSE ISOTROPY VERSION
C     Version 2.0, Update 12.Sep.97     
c     Multible frequencies implemented
c     970912   Patch scattering incident field modified to Jayong Lee's
c              form (OASK).
C          
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
      INCLUDE 'comfip.f'

C
      COMPLEX SLOW
      CHARACTER*50 FILENM,trcfil,brcfil
      DIMENSION CONMAX(npar)
      DIMENSION X(NP2,NPAR),FF(2,NP3),PX(MODULO)              
      DIMENSION RDUP(npar),RDDOWN(npar),CYAXIS(npar),RDINC(npar)
      DIMENSION YUP(npar),YDOWN(npar),YAXIS(npar),YINC(npar)
      DIMENSION ZMIN(npar),ZMAX(npar),ZSTEP(npar)
      COMPLEX CTRF(npar)
      DIMENSION FFS(2,NP),XS(NP2),AKM(100),AKM1(100),AKM2(100)     
      CHARACTER*16 TITLEY(4)
      CHARACTER*6  OPTION(2)
      CHARACTER*4 TITLE(20)
      character*80 atitle,ctitle,stitle
      character*4 pident(npar)
      LOGICAL ICONTU,AUSAMP,cfrfl
      EQUIVALENCE (NREC,ISPACE),(LF,NUMFR)
      EQUIVALENCE (FF(1,1),CFF(1,1)),(FFS(1,1),CFFS(1))
      EQUIVALENCE (X(1,1),CFF(1,1)),(XS(1),CFFS(1))        
      equivalence (atitle,title(1))
      DATA CONMAX /npar*-1E30/
      DATA TITLE /20*'    '/      
      DATA OPTION /'OASTL ','      '/                    
      data pident /' Szz',' w  ',' u  ',' v  ',
     &             ' Sxx',' P  ',' S  '/                     
C          
C ********************FORMATS*******************         
C          
C          
 200  FORMAT(20A4)                
 209  FORMAT(1H ,'**********************************',
     &      /1H ,'*       OASES Version ',f3.1,'       *',
     &      /1H ,'**********************************',
     &       //1H ,'Transmission Loss Module OAST' )
 210  FORMAT(//1H ,20A4)   
350   FORMAT(//1H ,'    DEPTH        ALPHA       BETA      ATTENA       AT        
     1TENB         RHO       ROUGHNESS'//(1H ,3F12.5,2F12.8,2F12.5))  
500   FORMAT(//1H ,'Minimum Frequency:    ',F10.2,' HZ ',
     &        /1H ,'Maximum Frequency:    ',F10.2,' HZ ',
     &        /1H ,'Number of Frequencies:',I7 )       
550   FORMAT(//1H ,3X,'NW',5X,'  IC1',5X,'  IC2',/(1X,I5,2I10))                   
  600 FORMAT(//,'  CMIN = ',G15.6,' M/S ',/,'  CMAX = ',G15.6,' M/S ')          
C          
C          
C **********************************************         
C          
      DEBUG=.FALSE. 
      DECOMP=.FALSE.
      AUSAMP=.FALSE.
      LUGRN=30
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
     &   '>>>>        CHANGE IN FILE compar.f AND RELINK.     <<<<'      
      END IF
C          
      MODU=MODULO
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
      NFLAG=.FALSE.
      ICNTIN=0
      tottim=0e0
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
      CALL OPFILR(1,IOER)
      IF (IOER.NE.0) STOP '>>>> ERROR: .dat FILE NOT FOUND <<<<' 
      READ(1,200)TITLE            
      ltit=lenstr(atitle)
      write(ctitle,'(a,a)') atitle(1:ltit),' - '
      WRITE(6,209) version           
      WRITE(6,210) TITLE           
      CALL GETOPT(IPROF,ICNTIN,ICONTU,cfrfl)
c >>> Multible frequencies introduced 920910

      if (doppler) then
        READ(1,*) FREQ1,FREQ2,NFREQ,offdb,vrec
        vsou=vrec
      else IF (ICNTIN.GT.0.or.cfrfl) THEN
        READ(1,*) FREQ1,FREQ2,NFREQ,offdb
      ELSE
        OFFDB=0E0
        READ(1,*) FREQ1,FREQ2,NFREQ
      END IF


      IF (FREQ1*FREQ2.EQ.0.0) THEN
        STOP '*** FREQUENCIES MUST BE NON-ZERO, ABORTING ***'
      ELSE IF (FRCONT) THEN
        IF (NFREQ.LE.1) THEN
          STOP '*** CONTOURS REQUIRE NRFR>1 YOU STUPID FOOL ***'
        END IF
        F1LOG=LOG(FREQ1)
        F2LOG=LOG(FREQ2)
        DFLOG=(F2LOG-F1LOG)/(NFREQ-1)
      ELSE
      END IF
      IF (NFREQ.GT.1) THEN
       DLFREQ=(FREQ2-FREQ1)/(NFREQ-1)
      Else
       DLFREQ=1.
      END IF

      WRITE(6,500)FREQ1,freq2,nfreq
            
       IF (CFRFL) THEN
        OMEGIM=-LOG(50.0)*offdb
        offdb=0
       ELSE
        OMEGIM=0.0
       END IF
 
c >>> create file for scattering rhs
      if (sctout) then
       call opfilb(45,ioer)
       write(45) nfreq,freq1,freq2,1e0/(nfreq*dlfreq)
      end if

C           
C     READ IN ENVIRONMENTAL DATA
C
      CALL INENVI
C          
C     SOURCE AND RECEIVER DATA
C
      CALL INSRC(SD)

c
c >>> Read sources from strf file and make frequency sampling 
c     compatible
c
      if (trfsou) then
       call rdstrf(freq1,freq2,nx,lxp1,mx,dt,dlfreq)
       write(6,*) 'Sources read from strf file:', ls
       write(6,*) 'LXP1 =', lxp1
       write(6,*) 'MX =', mx
       sd=sdc(1)
       freq1=(lxp1-1)/(nx*dt)
       freq2=(mx-1)/(nx*dt)
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

       write(6,*)
       WRITE(6,500)FREQ1,freq2,nfreq
       write(6,*) '>>> Imag(omega) = ', omegim,' from source TRF file'
      end if

      CALL INREC(RD,RDLOW,INTF)

      NUMI=NUML-1
C          
C          
      READ(1,*)CMININ,CMAXIN        
      IF (CMININ.EQ.0) STOP '*** CMIN MUST BE NON-ZERO ***'
      IF (((CMAXIN.GT.0).AND.(CMININ.GT.0)).OR.
     1    ((CMAXIN.LT.0).AND.(CMININ.LT.0))) THEN
       IF (CMAXIN.LT.0.AND.ICDR.NE.1) THEN
        STOP '*** NEGATIVE SPECTRUM ONLY ALLOWED FOR PLANE GEOMETRY ***'
       END IF
       NFLAG=.FALSE.
      ELSE
       IF (ICDR.NE.1) THEN
        STOP '*** NEGATIVE SPECTRUM ONLY ALLOWED FOR PLANE GEOMETRY ***'
       END IF
       IF (CMININ.LE.0) STOP '*** CMIN/CMAX CONFLICT ***'
       CMAXIN=1E8
       NFLAG=.TRUE.
      END IF

      READ(1,*)NWVNOin,ICW1,ICW2       
C
C     RANGE DATA
C
      READ(1,*) XLEFT,XRIGHT,XAXIS,XINC
c 
c >>> plot data
c
      IF ((DEPTAV.and.(.not.frcont)).OR.PLTL.OR.PLKERN
     &    .OR.ANSPEC.OR.TLDEP) THEN
        DO 980 I=1,NPAR
        IF (IOUT(I).EQ.0) GO TO 980
        READ(1,*) YUP(I),YDOWN(I),YAXIS(I),YINC(I)
        YIAXIS=YAXIS(I)
 980    CONTINUE
      END IF
      IF (DRCONT.OR.TLDEP.OR.ICONTU) THEN
C
C     DEPTH AXES
C
        READ(1,*) RDUP(1),RDDOWN(1),CYAXIS(1),RDINC(1)
      END IF
C
C     DETERMINE MAXIMUM NUMBER OF TLDEP PLOTS
C
      NTLDEP=INT(ABS((XRIGHT-XLEFT)/XINC))+1
C        
      IF (DRCONT.or.frcont) THEN
        XSCALE=ABS(XRIGHT-XLEFT)/XAXIS
        if (drcont) YSCALE=ABS(RDUP(1)-RDDOWN(1))/CYAXIS(1)
        DO 990 I=1,npar
        IF (IOUT(I).EQ.0) GO TO 990
        READ(1,*) ZMIN(I),ZMAX(I),ZSTEP(I)
 990    CONTINUE
      END IF
c
c >>> open scratch file for frequency-range contours
c
      if (frcont) then
       open(unit=72,FORM='UNFORMATTED',STATUS='SCRATCH')
       open(unit=73,FORM='UNFORMATTED',STATUS='SCRATCH')
      end if
C          
C     OPEN PLOT FILES
C
      CALL OPFILW(19,IOER)
      CALL OPFILW(20,IOER)
      WRITE(19,6010) MODU,'MODU'
C
C     OPEN CHECK FILE
C
      IF (DEBUG) CALL OPFILW(21,IOER)
C
C     PLOT OF VELOCITY PROFILE IF OPTION 'Z' WAS CHOSEN
C
      IF (IPROF.GT.0) THEN
        READ(1,*) VPLEFT,VPRIGHT,VPLEN,VPINC
        READ(1,*) DPUP,DPLO,DPLEN,DPINC
        CALL PLPROF(TITLE,VPLEN,DPLEN,VPLEFT,VPRIGHT,VPINC,
     2              DPUP,DPLO,DPINC)
      END IF
 6010 FORMAT(1H ,I8,10X,A40)
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
        WRITE(60) INPATCH,NFREQ,ICDR
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
c
c >>> open contour files
c
      IF (DRCONT.or.FRCONT.OR.ICONTU) THEN
        CALL OPFILW(28,IOER)
        CALL OPFILW(29,IOER)
      END IF

c
c >>> Initialize
c
      CALL PINIT1
      if (DEBUG) CALL PREQV(NUML,NUMI)

c
c >>> Prepare Bessel functions for full Hankel transform
c
      IF (INTTYP.EQ.2) THEN
        wkmax=2*pi*freq2/cminin
        rrmax=1E3*xright
c       CALL PREPBF(RRMAX,WKMAX*1.3)
c >>> v2.2 has tapered full Bessel below kr=10*pi
       mbf_0=0
       brk_0=max(0e0,mbf_0-10.*pi)
       bfrmax=(10*pi+mbf_0)/wkmax
       CALL PREPBF(bfrmax,WKMAX)
       ICDR=0
       write(6,*) '>>> Bessel functions tabulated <<<'
       write(6,*) '>>> Max problem kr:', wkmax*rrmax,' m^-1'
       write(6,*) '>>> Max Bessel kr: ', bfrmax*wkmax,' m^-1'
      END IF

c >>> for patch output no sources in last two layers
      if (outpot) then
       if ( nosou(inpatch).gt.0 .or. nosou(inpatch1).gt.0) then
        write(6,*) '>>> For patch output no sources in patch layers'
        write(6,*) 'in,in2,n1,n2=',inpatch,inpatch1,
     &             nosou(inpatch),nosou(inpatch1)
        stop
       end if
      end if
C
C     SLOWNESS DIAGRAM FOR TRANS. ISOTR. MEDIA
C
      IF (NTISOL.GT.0.and.IPROF.GT.0) THEN
       CALL SNSDGM(title)
      END IF

      IF (TABLERC) THEN
C >>>  Tabulated Top  Reflection Coefficient
       call settrc()
      ELSE IF (BOTTOMRC) THEN
C >>>  Tabulated Bottom  Reflection Coefficient
       call setbrc()
      END IF

c >>>
c >>> Frequency loop
c >>>
      DO 15 JJ=1,NFREQ

      ftime=0e0
      CALL CLTIME
      IF (FRCONT) THEN
        FREQ=EXP(F1LOG+(JJ-1)*DFLOG)
      ELSE
        FREQ=FREQ1+(JJ-1)*DLFREQ
      END IF
      write(6,309) freq
 309  FORMAT(//1H ,'Frequency: ',F10.2,' Hz')

       IF (TABLERC) THEN
c >>>   Read Top reflection coefficient table
        call gettrc()
       ELSE IF (BOTTOMRC) THEN
c >>>   Read Bottom reflection coefficient table
        call getbrc()
       ENDIF

c 
c >>> make tables for dispersive media
c
      if (idlmax.gt.0) then
       call dltable(2,2,freq)
      end if 
      nactf=2
      IF (NWVNOin.LT.0) THEN
C ***  AUTOMATIC SAMPLING
       AUSAMP=.TRUE.
c ***  PADE APPROXIMATION
       IF (NWVNOin.NE.-1) THEN
        PADE=.TRUE.
        NPADE=-NWVNOin
        npadat=icw1
        npait=icw2
       END IF
C ***  FORCE COMPLEX CONTOUR
       ICNTIN=1
       RMAXA=MAX(ABS(XLEFT),ABS(XRIGHT))
       RMINA=RMAXA-ABS(XRIGHT-XLEFT)
       OFFDB=0E0
       CALL AUTSAM(CMININ,CMAXIN,RMINA,RMAXA,CMIN,CMAX,NWVNO,ICW1,ICW2)
      ELSE
       nwvno=nwvnoin
       CMIN=CMININ
       CMAX=CMAXIN
       NWVNO=MIN0(NWVNO,NP)
       ICW2=MIN0(NWVNO,ICW2)
       ICW1=MAX0(1,ICW1)
       if (nwvno.eq.1) icntin=0
      END IF
c >>> Hanning tapering
       icut1=1
       icut2=nwvno
C *** FOR TL TAPERING BY CHERMIT 
c      ICUT1=ICW1
c      ICUT2=ICW2
C
C     CHECK THAT NWVNO IS INTEGER POWER OF 2
C
      IPOW=-1
 991  IPOW=IPOW+1
      II=2**IPOW
      IF (II.LT.NWVNO) GO TO 991
      IF (II.NE.NWVNO) THEN
        NWVNO=II
        WRITE(6,*)
        WRITE(6,*) '>>>> NW MUST BE INTEGER POWER OF 2, CHANGED TO',
     &             NWVNO,' <<<<'      
      END IF
      IF (NWVNO.GT.NP) STOP '>>> TOO MANY WAVENUMBERS <<<'
      IF (.NOT.NFLAG) THEN
       WK0 = 2*PI*FREQ / CMAX
       WKMAX = 2*PI*FREQ / CMIN               
      ELSE
       WKMAX=2*PI*FREQ/CMIN
       dlwvno=wkmax/(nwvno-0.5)
       WK0=0.5*dlwvno
       CMAX=2*PI*FREQ/WK0
       ICUT1=1
       ICW1=1
      END IF
      WRITE(6,600)CMIN,CMAX       
      WRITE(6,550)NWVNO,ICW1,ICW2                      
      IF (NFLAG) WRITE(6,*) '*** NEGATIVE SPECTRUM BY SYMMETRY ***'
      if (nwvno.gt.1) then
       DLWVNO = ( WKMAX - WK0 ) / ( FLOAT(NWVNO-1) )      
      else 
       DLWVNO = 1
      end if

      DLRAN=2E0*PI/(NWVNO*DLWVNO) 
      RSTEP=DLRAN*1.0E-3          
      R0=XLEFT
      IF (ICDR.EQ.0) THEN
        R0=MAX(R0,RSTEP)
      END IF
      R1=R0*1E3
c >>> patch range for opt E is half window range
      rpat=(xleft+xright)*0.5*1E3
      LF=NWVNO                    
      LF=INT((XRIGHT-XLEFT)/RSTEP+1.5)                     
      lf=min(lf+(lf/nconm),nwvno)
      RANMAX=NWVNO*DLRAN
      WRITE(6,360) DLRAN,RANMAX*1E-3
 360  FORMAT(1H ,' ',/1H ,'RANGE STEP:   ',F12.3,' m',
     &               /1H ,'MAX FFT RANGE:',F12.3,' km')
C
C     IF OPTION 'J', SET DEFAULT OFFDB
C
      IF (ICNTIN.GT.0) THEN
       IF (OFFDB.LT.1E-10) THEN
        OFFDB=60.0*V(LAYS((LS-1)/2+1),2)/(FREQ*RANMAX)
        WRITE(6,*)
        WRITE(6,*) 'THE DEFAULT CONTOUR OFFSET IS APPLIED'
       ELSE
        WRITE(6,*)
        WRITE(6,*) 'THE USER DEFINED CONTOUR OFFSET IS APPLIED'
       END IF
       ATTMAX=OFFDB*FREQ*RANMAX/V(LAYS((LS-1)/2+1),2)
       WRITE(6,361) OFFDB,ATTMAX,ATTMAX*XRIGHT*1E3/RANMAX
 361   FORMAT(1H ,'CONTOUR OFFSET:         ',F12.6,' dB/wavelength',
     &       /1H ,'AT MAX FFT RANGE:       ',F12.2,' dB',
     &       /1H ,'AT MAX WINDOW RANGE:    ',F12.2,' dB',
     &       /1H ,'>> NOTE THAT COMPENSATION IS AUTOMATIC <<')
      END IF

      IF (DEPTAV.or.frcont) THEN
C
C     OPEN SCRATCH FILE FOR LINEAR TL DATA
C              
       DO 988 I=1,npar
        IF (IOUT(I).NE.0) THEN
          LOGNUM=30+npar+I
          CALL OPNBUF(LOGNUM,LF,IR,(NP-1)/64+1)
        END IF
 988   CONTINUE
      END IF

      IF (DRCONT.OR.TLDEP) THEN
C     
C     OPEN SCRATCH FILES FOR TRANSMISSION LOSS DATA
C
       DO 989 I=1,npar
        IF (IOUT(I).NE.0) THEN
          LOGNUM=30+2*npar+I
          CALL OPNBUF(LOGNUM,LF,IR,(NP-1)/64+1)
        END IF
 989  CONTINUE
      END IF
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
        CALL OPNBUF(27,NCON,IR*NOUT,100)
       END IF

c
c >>> Compute Green's functions
c
c      DSQ=2E0*PI*FREQ             
      DSQ=2E0*PI*FREQ+CMPLX(0E0,OMEGIM)
      RDSQ=DSQ
      CSQ=DSQ*DSQ                 

      CALL PINIT2                 
      CALL CLTIME

c *** kernels
      CALL CALIN3
c ***
      CALL RDTIME(T1)
      TOTTIM=TOTTIM+T1
      CALL CHKSOL
      WRITE(6,310) T1
 310  FORMAT(//1H ,'INTEGRANDS BUILT, TIME: ',F12.3,' SECS.')
c
c     SINGLE PLANE WAVE CASE
c
      IF (NWVNOIN.EQ.1) THEN
         WRITE(6,*)
         WRITE(6,*) 'SINGLE PLANE WAVE CASE : NO Integration'
         go to 15
      end if
C
C     WAVENUMBER INTEGRATION LOOP
C
       DO NREC=1,IR
        CALL CLTIME
        CALL PHINT()
Cs3
C     PLOT OF HANKEL OR FOURIER TRANSFORMS
C
        IF (PLKERN) THEN
         IF (MOD(NREC-1,INTF).EQ.0) THEN
            stitle=ctitle(1:lenstr(ctitle))
          CALL PLINTGR(DLWVNO,WK0,SD,RDC(NREC),sTITLE,XAXIS,YIAXIS)
         END IF
        END IF
C
C     PLOT OF ANGULAR SPECTRA
C
        IF (ANSPEC) THEN
         IF (MOD(NREC-1,INTF).EQ.0) THEN
            stitle=ctitle(1:lenstr(ctitle))             
          CALL PLSPECT(DLWVNO,WK0,SD,RDC(NREC),sTITLE,
     1               XAXIS,YIAXIS,LAY(NREC))
         END IF
        END IF
C
C     DECIMATE INTEGRAND FOR DEPTH CONTOUR PLOT
C
        IF (ICONTU) THEN
         NN=ICUT2-ICUT1+1
         do I=1,npar
          IF (IOUT(I).GT.0) THEN
            CALL CVMAGS(CFF(ICUT1,I),2,FFS,1,NN)
            CALL VCLIP(FFS,1,1E-20,1E20,FFS,1,NN)
            CALL VALG10(FFS,1,FFS,1,NN)
            CALL VSMUL(FFS,1,1E1,FFS,1,NN)
            CALL VDECIM(FFS,1,FFS,1,NN,NDEC,NC)
            CALL WRBUF(27,FFS,NC)
            CALL VMAX(FFS,1,AAA,NC)
            CONMAX(I)=MAX(CONMAX(I),AAA)
          END IF
         end do
        END IF
C
C     TRANSMISSION LOSS CALCULATION
C
        CALL TLOSS

        IF (PLTL) THEN
         IF (MOD(NREC-1,INTF).EQ.0) THEN
          do I=1,npar
           if (IOUT(I).gt.0) then
            stitle=ctitle(1:lenstr(ctitle))//pident(i)             
            CALL PLTLOS(LF,R0,RSTEP,sTITLE,I,XAXIS,YAXIS(I),
     1            XLEFT,XRIGHT,XINC,YUP(I),YDOWN(I),YINC(I),
     2            SD,RDC(NREC))
           end if
          end do
         END IF
        END IF
C
C     WRITE TRANSMISSION LOSSES TO SCRATCH FILES
C     FOR DRCONT AND TLDEP
C
        if (DRCONT.OR.TLDEP) then
         do I=1,npar
          if (IOUT(I).gt.0) then
           CALL WRBUF(30+2*npar+I,X(1,I),LF)
          end if
         end do
        end if
        CALL RDTIME(T2)
        ftime=ftime+T2
        WRITE(6,320) NREC,T2
 320  FORMAT(1H ,'RECEIVER NO. ',I4,' TIME=',F8.3,' SECONDS')
c <<< end nrec loop
       end do
       tottim=tottim+ftime
C
C     CLOSE SCRATCH FILES
C
      IF (DRCONT.OR.TLDEP) THEN
      DO 45 I=1,npar
        IF (IOUT(I).NE.0) THEN
          CALL ENFBUF(30+2*npar+I)
        END IF
 45   CONTINUE
      END IF
      IF (ICONTU) CALL ENFBUF(27)
C
C     PLOT DEPTH-AVERAGED LOSS
C
      IF (DEPTAV.or.frcont) THEN
        DO 46 I=1,npar
         IF (IOUT(I).NE.0) THEN
          CALL ENFBUF(30+npar+I)
         END IF
 46     CONTINUE
      end if

      if (deptav) then
       write(6,*) 'DEPTH-AVERAGED LOSS'
        CALL GETTLAV
        DO 50 I=1,npar
        IF (IOUT(I).EQ.0) GO TO 50
        if (.not.frcont) then
         stitle=ctitle(1:lenstr(ctitle))//pident(i)             
         CALL PLDAV(LF,R0,RSTEP,sTITLE,I,XAXIS,YAXIS(I),
     1           XLEFT,XRIGHT,XINC,YUP(I),YDOWN(I),YINC(I),SD)
        else
         dnew=(xright-xleft)/(nconm-1)
         lfn=nconm
         CALL Vfltsm(X(1,i),Xs,LF,rstep,lfn,dnew)
         write(73) (xs(l),l=1,lfn)
        end if
 50     CONTINUE
      END IF
C
C     PLOT TRANSMISSION LOSS OVER DEPTH
C     INTERPOLATION PERFORMED BETWEEN DATA POINTS
C
      IF (TLDEP) THEN
       write(6,*) 'LOSS VS DEPTH'
        DO 60 L=1,NTLDEP
         RTLDEP=XLEFT+(L-1)*XINC
         ITL=INT((RTLDEP-R0)/RSTEP)+1
         IF (ITL.GT.0.AND.ITL.LT.LF) THEN
           RTL=R0+(ITL-1)*RSTEP
           RAT=(RTLDEP-RTL)/RSTEP
           DO 59 I=1,npar
            IF (IOUT(I).NE.0) THEN
              CALL RWDBUF(30+2*npar+I)
              DO 58 JR=1,IR
                CALL RDBUF(30+2*npar+I,XS,LF)
                X(JR,I)= XS(ITL)+RAT*(XS(ITL+1)-XS(ITL))
 58           CONTINUE
            stitle=ctitle(1:lenstr(ctitle))//pident(i)             
        CALL PTLDEP(IR,RDC(1),RDSTEP,STITLE,I,XAXIS,CYAXIS(1),
     1            YDOWN(I),YUP(I),YINC(I),RDUP(1),RDDOWN(1),RDINC(1),
     2            SD,RTLDEP)
            END IF
 59        CONTINUE
         END IF
 60     CONTINUE
      END IF

C*****  CONTOUR PLOTS OF INTEGRANDS VERSUS depth

      IF (ICONTU) THEN
       write(6,*) 'INTEGRAND CONTOURS VS DEPTH'
        WN1=WK0+(ICUT1-1)*DLWVNO
        WN2=WK0+(NCON-1)*NDEC*DLWVNO
C
C     SLOWNESS AXIS
C
        WN1=WN1/(2E0*PI*FREQ)
        WN2=WN2/(2E0*PI*FREQ)      
        IF (WN1.LT.WN2*1E-1) THEN
          XLFTI=0
        ELSE
          XLFTI=INT(WN1*1E4)*1E-4
        END IF
        XRGHTI=WN2
        XINCI=INT(XRGHTI*1E4/5E0)*1E-4
        XSCALI=(XRGHTI-XLFTI)/20.0
        DO 710 I=1,npar
         IF (IOUT(I).LE.0) GO TO 710
          CALL RWDBUF(27)
          ZMINI=INT(CONMAX(I))
          ZINCI=10E0
          ZMAXI=ZMINI-ZINCI*10E0
          stitle=ctitle(1:lenstr(ctitle))//pident(i)             
          CALL INTCON(stitle,NCON,IR,NCON,IR,
     1                XLFTI,XRGHTI,XSCALI,XINCI,
     2                RDUP(1),RDDOWN(1),YSCALE,RDINC(1),
     3                ZMINI,ZMAXI,ZINCI,FREQ,SD,
     4                RD,RDLOW,WN1,WN2,I,1,0)
          DO 709 JR=1,IR
            DO 708 J=1,npar
              IF (IOUT(J).LE.0) GO TO 708
              CALL RDBUF(27,FFS,NCON)
              IF (J.EQ.I) THEN
                WRITE(29,'(1X,6G13.5)') (XS(JJJ),JJJ=1,NCON)
              END IF
 708        CONTINUE
 709      CONTINUE
 710    CONTINUE
      END IF
C
C     GENERATE TL-CONTOUR PLOT FILES
C
      IF (DRCONT) THEN
       write(6,*) 'DEPTH-RANGE CONTOURED LOSS'
        NDECc=LF/NCONM
        IF (NDECc.GT.0) THEN
          NCON=(LF-1)/NDECc+1
        ELSE
          NDECc=1
          NCON=LF
        END IF
        XXL=R0
        XXM=R0+(ncon-1)*RSTEP*ndecc
        WDEPTH=0.0

        DO 69 I=1,npar
          IF (IOUT(I).NE.0) THEN
            CALL RWDBUF(30+2*npar+I)
            stitle=ctitle(1:lenstr(ctitle))//pident(i)             
            CALL CONDRW(stitle,NCON,IR,NCON,IR,XLEFT,
     &            XRIGHT,XSCALE,XINC,
     1            RDUP(1),RDDOWN(1),YSCALE,RDINC(1),ZMIN(I),ZMAX(I),
     2            ZSTEP(I),FREQ,SD,RD,RDLOW,XXL,XXM,PX,icdr)
            DO 68 JR=1,IR
              CALL RDBUF(30+2*npar+I,XS,LF)
              CALL VDECIM(XS,1,XS,1,LF,NDECC,NC)
              CALL CONDRB(1,NCON,NCON,XS)
 68         CONTINUE
          END IF
 69     CONTINUE
      END IF
c
c >>> Write TL to file 72 for option frcont
c
      if (frcont) then
        dnew=(xright-xleft)/(nconm-1)
        lfn=nconm
        DO 79 I=1,npar
          IF (IOUT(I).NE.0) THEN
            CALL RWDBUF(30+npar+I)
            DO 78 JR=1,IR
c >>> read linear field and sample
              CALL RDBUF(30+npar+I,XS,LF)
              CALL Vfltsm(XS,X,LF,rstep,lfn,dnew)
c >>> convert to loss
              CALL VCLIP(x,1,1E-20,1E20,x,1,lfn)
              CALL VALG10(x,1,x,1,lfn)
              CALL VSMUL(x,1,10.0,x,1,lfn)
              CALL VNEG(x,1,x,1,lfn)
              write(72) (x(j,1),j=1,lfn)
 78         CONTINUE
          END IF
 79     CONTINUE
      end if
C
C     close and Rewind SCRATCH FILES
C
      CALL CLSBUF(30)

      IF (ICONTU) CALL CLSBUF(27)

      IF (DRCONT.OR.TLDEP) THEN
       DO 80 I=npar,1,-1
        IF (IOUT(I).NE.0) THEN
          CALL clsBUF(30+2*npar+I)
        END IF
 80    CONTINUE
      END IF

      IF (DEPTAV.or.frcont) THEN
       DO 81 I=npar,1,-1
        IF (IOUT(I).NE.0) THEN
          CALL clsBUF(30+npar+I)
        END IF
 81    CONTINUE
      END IF

c >>>
c >>> End of frequency loop
c
 15   continue
c 
c >>> close patch file
c
       if (outpot) then
        close(60)
       end if
c
c >>> Contours vs frequency and range
c
      if (frcont) then
       write(6,*) 'FREQUENCY-RANGE CONTOURED LOSS'
       dnew=(xright-xleft)/(nconm-1)
       lfn=nconm
       if (.not.deptav) then
       do 18 jr=1,ir
       do 18 j=1,npar
        if (iout(j).ne.0) then
         CALL CONFR(j,nconm,NFREQ,xleft,xright,
     1              XLEFT,XRIGHT,XINC,XAXIS,FREQ1,FREQ2,
     2              freq1,freq2,2.0,9.0,ZMIN(j),ZMAX(j),Zstep(j),
     3              TITLE,dnew,sd,rdc(jr))
         rewind(72)
         do 17 jf=1,nfreq
          FREQ=EXP(F1LOG+(Jf-1)*DFLOG)
          DO 17 I=1,npar
           IF (iout(i).ne.0) THEN
            DO 16 LR=1,IR
             read(72) (x(jj,2),jj=1,lfn)
             if (i.eq.j.and.lr.eq.jr) then
              call confab(lfn,freq)
             end if
 16         CONTINUE
           END IF
 17       CONTINUE
        end if
 18    continue
       end if
c
c >>> Frequency-range contours for depth average
c
       if (deptav) then
       write(6,*) 'FREQUENCY-RANGE CONTOURED AVERAGE LOSS'
        do 28 j=1,npar
        if (iout(j).ne.0) then
         CALL CONFR(j,nconm,NFREQ,xleft,xright,
     1              XLEFT,XRIGHT,XINC,XAXIS,FREQ1,FREQ2,
     2              freq1,freq2,1.0,9.0,ZMIN(j),ZMAX(j),Zstep(j),
     3              TITLE,dnew,sd,0.0)
         rewind(73)
         do 27 jf=1,nfreq
          FREQ=EXP(F1LOG+(Jf-1)*DFLOG)
          DO 27 I=1,npar
           IF (iout(i).ne.0) THEN
             read(73) (x(jj,2),jj=1,lfn)
             if (i.eq.j) then
              call confab(lfn,freq)
             end if
           END IF
 27       CONTINUE
         end if
 28     continue
       end if
      end if
      OPTION(2)='PLTEND'
      WRITE(19,777) OPTION
 777  FORMAT(1H ,2A6)
C          
C          
      WRITE(6,9960)
 9960 FORMAT(//1H ,'*** OASTL FINISHED ***')
C
C     CLOSE PLOT FILES
C
      CLOSE(UNIT=19,STATUS='KEEP')
      CLOSE(UNIT=20,STATUS='KEEP')
      CLOSE(UNIT=21,STATUS='KEEP')
      IF (DRCONT) THEN
        CLOSE(UNIT=28,STATUS='KEEP')
        CLOSE(UNIT=29,STATUS='KEEP')
      END IF
      WRITE(6,9962) TOTTIM
 9962 FORMAT(//1H ,'*** TOTAL TIME: ',F10.3,' SECONDS ***')
      END  
C          
      SUBROUTINE GETOPT(IPROF,ICNTIN,ICONTU,cfrfl)
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
      INCLUDE 'comfip.f'
      CHARACTER*1 OPT(40)
      LOGICAL ICONTU,cfrfl
      WRITE(6,300)                
 300  FORMAT(//1H ,'OPTIONS:',/)    
      DEBUG=.FALSE.
      DEPTAV=.FALSE.
      DRCONT=.FALSE.
      FRCONT=.false.
      PLTL=.FALSE.
      PLKERN=.FALSE.
      ANSPEC=.FALSE.
      TLDEP=.FALSE.
      SHEAR=.FALSE.
      mom_sou=.FALSE.
      dip_sou=.false.
      ICONTU=.FALSE.
      SCTOUT=.FALSE.
      NOUT=0                      
      IREF=0                      
      ISTYP=0                     
      ICDR=0
      IPROF=0
      ICNTIN=0
      cfrfl=.false.
      srctyp=1
      DO 10 I=1,npar                 
 10   IOUT(I)=0                   
      READ(1,200) OPT             
 200  FORMAT(40A1)                
      DO 50 I=1,40                
      IF (OPT(I).EQ.'N') THEN 
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
       NOUT=NOUT+1                 
       IOUT(3)=1                   
       WRITE(6,303)                
 303   FORMAT(1H ,'HORIZONTAL VELOCITY')                 
      ELSE IF (OPT(I).EQ.'R') THEN
       IF (IOUT(5).GT.0) GO TO 50              
       NOUT=NOUT+1            
       IOUT(5)=1              
       WRITE(6,3031)           
 3031  FORMAT(1H ,'RADIAL STRESS')             
      ELSE IF (OPT(I).EQ.'K') THEN
       IF (IOUT(6).GT.0) GO TO 50              
       NOUT=NOUT+1            
       IOUT(6)=1              
       WRITE(6,3032)           
 3032  FORMAT(1H ,'BULK PRESSURE')             
      ELSE IF (OPT(I).EQ.'S') THEN
       IF (IOUT(7).GT.0) GO TO 50              
       NOUT=NOUT+1            
       IOUT(7)=1              
       WRITE(6,'(1h ,a)') 'SHEAR STRESS'             
      ELSE IF (OPT(I).EQ.'A') THEN 
       IF (DEPTAV) GO TO 50     
       DEPTAV=.TRUE.
       WRITE(6,304)                
 304   FORMAT(1H ,'DEPTH AVERAGE')
      ELSE IF (OPT(I).EQ.'C') THEN 
       IF (DRCONT) GO TO 50    
       DRCONT=.TRUE.
       WRITE(6,305)           
 305   FORMAT(1H ,'DEPTH-RANGE CONTOURS')
      ELSE IF (OPT(I).EQ.'o') THEN 
       IF (FRCONT) GO TO 50    
       FRCONT=.TRUE.
       WRITE(6,3051)           
 3051  FORMAT(1H ,'FREQUENCY-RANGE CONTOURS')
      ELSE IF (OPT(I).EQ.'c') THEN 
       IF (ICONTU) GO TO 50    
       ICONTU=.TRUE.
       WRITE(6,306)           
 306   FORMAT(1H ,'DEPTH INTEGRAND CONTOURS')
      ELSE IF (OPT(I).EQ.'T') THEN 
       IF (PLTL) GO TO 50    
       PLTL=.TRUE.
       WRITE(6,307)           
 307   FORMAT(1H ,'TRANSMISSION LOSS')
      ELSE IF (OPT(I).EQ.'I') THEN
       IF (PLKERN) GO TO 50
       PLKERN=.TRUE.
       WRITE(6,309)
 309   FORMAT(1H ,'HANKEL TRANSFORM INTEGRANDS')
      ELSE IF (OPT(I).EQ.'P') THEN
       IF (ICDR.GT.0) GO TO 50
       ICDR=1
       WRITE(6,313)
 313   FORMAT(1H ,'PLANE GEOMETRY')
      ELSE IF (OPT(I).EQ.'L') THEN
       IF (LINA.GT.0) GO TO 50
       LINA=1
       extlar=.false.
       WRITE(6,'(1H ,a)') 'VERTICAL SOURCE ARRAY - Internal'
      ELSE IF (OPT(I).EQ.'l') THEN
       IF (LINA.GT.0) GO TO 50
       LINA=1
       extlar=.true.
       WRITE(6,'(1H ,a)') 'VERTICAL SOURCE ARRAY - External'
      ELSE IF (OPT(I).EQ.'v') THEN
       IF (LINA.GT.0) GO TO 50
       LINA=1
       trfsou=.true.
       WRITE(6,'(a)') 'VERTICAL SOURCE ARRAY - trf-file'
      ELSE IF (OPT(I).EQ.'a') THEN
       IF (ANSPEC) GO TO 50
       ANSPEC=.TRUE.
       WRITE(6,3091)
 3091  FORMAT(1H ,'ANGULAR SPECTRA')
      ELSE IF (OPT(I).EQ.'s') THEN
       IF (SCTOUT) GO TO 50
       SCTOUT=.TRUE.
       WRITE(6,3092)
 3092  FORMAT(1H ,'OUTPUT OF SCATTERING DISCONTINUITIES')
      ELSE IF (OPT(I).EQ.'Z') THEN
       IF (IPROF.GT.0) GO TO 50
       IPROF=1
       WRITE(6,314)
 314   FORMAT(1H ,'PLOT OF VELOCITY PROFILES')
      ELSE IF (OPT(I).EQ.'J') THEN
        IF (ICNTIN.GT.0) GO TO 50
        ICNTIN=1
        WRITE(6,315)
 315    FORMAT(1H ,'COMPLEX INTEGRATION CONTOUR')
      ELSE IF (OPT(I).EQ.'D') THEN
        IF (TLDEP) GO TO 50
        TLDEP=.TRUE.
        WRITE(6,316)
 316    FORMAT(1H ,'TRANSMISSION LOSS AS FUNCTION OF DEPTH')
      ELSE IF (OPT(I).EQ.'O') THEN
        CFRFL=.TRUE.
        WRITE(6,'(a)') 'COMPLEX FREQUENCY'
      else if (opt(i).eq.'d') then
        if (doppler) go to 50
        doppler=.true.
        write(6,'(1h ,a,f6.1,a)') 'Doppler compensation, speed',
     &                            vrec,' m/s'
      else if (opt(i).eq.'E') then
        if (outpot) go to 50
        outpot=.true.
        write(6,'(1h ,a,f6.1,a)') 
     &        'Output of patch scattering potentials'
      ELSE IF (OPT(I).EQ.'X'.or.opt(i).eq.'2') THEN
        IF (hor_for) GO TO 50
        srctyp=2
        ver_for=.true.
        SHEAR=.TRUE.
        WRITE(6,317)
 317    FORMAT(1H ,'VERTICAL POINT FORCE IN SOLID MEDIA')
      ELSE IF (opt(i).eq.'h'.or.opt(i).eq.'3') THEN
        IF (hor_for) GO TO 50
        srctyp=3
        hor_for=.true.
        WRITE(6,3170)
 3170   FORMAT(1H ,'HORIZONTAL POINT FORCE IN SOLID MEDIA')
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
      ELSE IF (OPT(I).EQ.'Q') THEN
        IF (DEBUG) GO TO 50
        DEBUG=.TRUE.
        WRITE(6,318)
 318    FORMAT(1H ,'***** DEBUGGING *****')
      ELSE IF (OPT(I).EQ.'F') THEN
c        INTTYP=1
        WRITE(6,319)
 319    FORMAT(1H ,'*** FILON INTEGRATION SCHEME DISABLED IN OAST ***')
      ELSE IF (OPT(I).EQ.'f') THEN
        INTTYP=2
        WRITE(6,3161)
 3161    FORMAT(1H ,'FULL HANKEL TRANSFORM')
      ELSE IF (OPT(I).EQ.'i') THEN
        icerc=.true.
        WRITE(6,320)
 320    FORMAT(1H ,'LePage ice reflection coefficient')
      ELSE IF (OPT(I).EQ.'r') THEN
        freerc=.true.
        WRITE(6,321)
 321    FORMAT(1H ,'LePage rough surface reflection coefficient')
      ELSE IF (OPT(I).EQ.'t') THEN
        tablerc=.true.
        WRITE(6,322)
 322    FORMAT(1H ,'TABULATED SURFACE REFLECTION COEFFICIENT')
      ELSE IF (OPT(I).EQ.'b') THEN
        bottomrc=.true.
        WRITE(6,323)
 323    FORMAT(1H ,'TABULATED BOTTOM REFLECTION COEFFICIENT')
      ELSE IF (OPT(I).EQ.'g') THEN
       IF (goff) GO TO 50
       goff=.true.
       WRITE(6,'(a)') 'Goff-Jordan power spectrum'
      ELSE IF (OPT(I).EQ.'p') THEN
       IF (pierson) GO TO 50
       pierson=.true.
       WRITE(6,'(a)') 'Pierson-Moskowitz roughness spectrum'
      ELSE IF (OPT(I).NE.' ') THEN
        WRITE(6,399) OPT(I)
 399    FORMAT(1H ,'>>>> UNKNOWN OPTION: ',A1,' <<<<')
      ELSE
      END IF
 50   CONTINUE
      if (cfrfl) then
       icntin=0
      end if                    
      IF (NOUT.NE.0) RETURN       
      IOUT(1)=1                   
      NOUT=1                      
      WRITE(6,301)                
      RETURN                      
      END  
      SUBROUTINE AUTSAM(C1,C2,RMIN,RMAX,CMIN,CMAX,NW,IC1,IC2)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
c      PARAMETER (RFAC=3.0)
      PARAMETER (NR=300,NKMIN=2**8)
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
C *** DETERMINE WAVENUMBER SAMPLING INTERVAL
      if (inttyp.eq.2) then
       rfac=6.0
      else
       rfac=3.0
      end if
      DK=2*PI/(RFAC*RMAX*1E3)
      CREFA=v(lays((LS-1)/2+1),2)
      DK=MIN(DK,2*PI*FREQ/(CREFA*NKMIN))
C *** MINIMUM WAVENUMBER INTERVAL
      DR=1E3*(RMAX-RMIN)/(NR-1)
      WM=2*PI/DR
C *** INTEGRATION LIMITS
      WN1=2*PI*FREQ/C2
      WN2=2*PI*FREQ/C1
      WNMAX=MAX(WM,1.1*(WN2-WN1)+WN1)
      WNMIN=MIN(WNMAX-WM,WN1-0.1*(WN2-WN1))
      WNMIN=MAX(WNMIN,DK*0.5)
C *** NUMBER OF WAVENUMBER SAMPLING POINTS
       NW1=(WNMAX-WNMIN)/DK+1
       NW=NKMIN/2
 1     NW=NW*2
       IF (NW.LT.NW1) GO TO 1
       WNMAX=WNMIN+(NW-1)*DK
       IC1=(WN1-WNMIN)/(WNMAX-WNMIN)*(NW-1)+1
       IC2=(WN2-WNMIN)/(WNMAX-WNMIN)*(NW-1)+1
       IC1=MAX(1,IC1)
       IC2=MIN(NW,IC2)
       CMIN=2*PI*FREQ/WNMAX
       CMAX=2*PI*FREQ/WNMIN
       RETURN
       END
      BLOCK DATA SAFBK1
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
      INCLUDE 'comdat.f'
      END




