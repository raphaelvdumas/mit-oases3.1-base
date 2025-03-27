      PROGRAM SCATFLD
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
C     VERSION FOR CALCULATING SCATTERED FIELD
C     USED AS POST-PROCESSOR TO THE STANDARD CODES
C     Version 2.0, Update 9-Apr-1996     
C          
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
      INCLUDE 'comfip.f'
      INCLUDE 'corrfn.f'
      INCLUDE 'recarr.f'
C
      COMPLEX SLOW
      CHARACTER*50 FILENM
      DIMENSION CONMAX(npar)
      DIMENSION X(ITWONP,npar),FF(2,NP3),PX(MODULO)              
      DIMENSION RDUP(npar),RDDOWN(npar),CYAXIS(npar),RDINC(npar)
      DIMENSION ZMIN(npar),ZMAX(npar),ZSTEP(npar)
      COMPLEX CTRF(npar)
      DIMENSION FFS(2,NP),XS(NP2),AKM(100),AKM1(100),AKM2(100)     
      CHARACTER*16 TITLEY(4)
      CHARACTER*6  OPTION(2)
      CHARACTER*80 TITLE,XTIT,YTIT
      LOGICAL ICONTU,REVERB,CCONTU,DCONTU,nfmean,covar,PLPOWER
      EQUIVALENCE (NREC,ISPACE),(LF,NUMFR)
      EQUIVALENCE (FF(1,1),CFF(1,1)),(FFS(1,1),CFFS(1))
      EQUIVALENCE (X(1,1),CFF(1,1)),(XS(1),CFFS(1))        
      DATA CONMAX /npar*-1E30/
      DATA OPTION /'SCFLD ','      '/                    
C          
C ********************FORMATS*******************         
C          
C          
 200  FORMAT(A)                
 210  FORMAT(//1H ,'OASS Version 2.0, Update 30-May-1997',//1H ,A)   
350   FORMAT(//1H ,'    DEPTH        ALPHA       BETA      ATTENA       AT        
     1TENB         RHO       ROUGHNESS'//(1H ,3F12.5,2F12.8,2F12.5))  
500   FORMAT(//1H ,'SOURCE FREQUENCY:',F10.2,' HZ ')       
550   FORMAT(//1H ,3X,'NW',5X,'  IC1',5X,'  IC2',/(1X,I5,2I10))                   
  600 FORMAT(//,'  CMIN = ',G15.6,' M/S ',/,'  CMAX = ',G15.6,' M/S ')          
C          
C          
C **********************************************         
C          
      DEBUG=.FALSE. 
      DECOMP=.FALSE.
      LUGRN=30
      XAXIS=20.0
      YIAXIS=12.0
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
      ITXX=ITWONP
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
c >>> Initialize roughness spectra
      goff=.false.
      pierson=.false.
C
C     DEFAULT: ISOTROPIC LAYERS
C
      NTISOL=0
C          
      CALL OPFILR(1,IOER)
      IF (IOER.NE.0) STOP '>>>> ERROR: INPUT FILE NOT FOUND <<<<' 
      READ(1,200)TITLE            
      WRITE(6,210)TITLE           
      CALL GETOPT(IPROF,ICNTIN,ICONTU,REVERB,CCONTU,DCONTU,
     &            covar,PLPOWER)
      IF (ICNTIN.GT.0) THEN
        READ(1,*) FREQ,OFFDB
      ELSE
        OFFDB=0E0
        READ(1,*)FREQ             
      END IF
      WRITE(6,500)FREQ            
C           
C     READ IN ENVIRONMENTAL DATA
C
      CALL INENVI
C         
C     NO SOURCES
C 
      LS=0
      CALL OPFILB(45,IOER)
      IF (IOER.NE.0) THEN
       STOP '>>> FILE 45 NOT FOUND. ABORTING <<<'
      ELSE
       nfreq=1
       nx=nfreq
       write(6,*) '>>> reading rhs file <<<'
       read(45) nx_in_file,fr1_in_file,fr2_in_file,dt_in_file
       if ((nx.ne.nx_in_file)
     &     .or.(abs(freq-fr1_in_file).gt.1e-3*freq)) then
        write(6,*) '>>> WARNING: Frequency sampling mismatch    <<<'
        write(6,*) '>>>          Replaced by values in rhs-file <<<'
       end if
       write(6,*) nx,nx_infile
       write(6,*) freq,fr1_in_file
       nx=nx_in_file
       freq=fr1_in_file
       READ(45) FFF,LAYS(1),nkmean,nfmean,fni5
       if (lays(1).eq.1) then
        sdc(1)=v(lays(1),1)-1.0
       else
        sdc(1)=v(lays(1),1)+0.001
       end if
      END IF
      IF (abs(FFF-FREQ).gt.1e-3*freq) then
       write(6,*) '>>> FREQUENCY MISMATCH. ABORTING <<<'
       write(6,*) '    Input freq =  ',freq
       write(6,*) '    In file freq =',fff
       stop
      end if

C *** PHASE VELOCITY AND INTERFACE
      READ(1,*) CPHASE,INTFCE
      WRITE(6,*) 'CPHASE,INTFCE=',CPHASE,INTFCE

C
C     RECEIVER DATA
C
C *** 3-DIMENSIONAL ARRAY
C
      CALL INPRCV

      IF (IR.GT.NRD) THEN
       WRITE(6,*) '*** TOO MANY RECEIVERS ***'
       STOP
      END IF
      IF (REVERB.AND.(NOUT.GT.1)) THEN
       WRITE(6,*) '*** INTEGRATION ONLY ALLOWED FOR   ***'
       WRITE(6,*) '*** ONE FIELD PARAMETER - ABORTING ***'
       STOP
      END IF
C          
C     LAYER NUMBERS OF SOURCE AND RECEIVER ARE DETERMINED
C          
      WRITE(6,910) NUML,IR
 907  FORMAT(1H ,I6,G15.6,I6,2G15.6)
 910  FORMAT(//1H ,'NUMBER OF LAYERS:      ',I3,           
     2      /1H ,'NUMBER OF RECEIVERS:   ',I3)           
      WRITE(6,918)
 918  FORMAT(//1H ,'RECEIVER DATA:',//1H ,'  NO. ','        DEPTH  ',
     1       'LAYER','           Z')
        DO 920 JJ=1,IR
        CALL RECEIV(V,NUML,RDC(JJ),LAY(JJ),Z(JJ))                 
        WRITE(6,907) JJ,RDC(JJ),LAY(JJ),Z(JJ)
 920    CONTINUE

      NUMI=NUML-1                 
C          
C          
      READ(1,*)CMIN,CMAX        
      READ(1,*)NWVNO,ICUT1,ICUT2       
      WK0 = 2*PI*FREQ / CMAX
      WKMAX = 2*PI*FREQ / CMIN               
C
C
C     FOR REVERBERATION COMPUTATION, ADJUST WAVENUMBER PARAMETERS AND
C     CREATE ASSYNCRONOUS FILE WITH RIGHT HAND SIDES
C
      IF (REVERB) THEN
       KCNT=0
 50    READ(45,END=51) CBUF(1),INNR,(CBUF(II),II=2,9)
       IF (INNR.EQ.INTFCE) THEN
        KCNT=KCNT+1
        IF (KCNT.EQ.1) THEN
          ETA=REAL(CBUF(1))
          OFFIMA=RIMAG(CBUF(1))
        END IF
        IF (KCNT.EQ.2) DLWVNO=ABS(REAL(CBUF(1))-ETA)
       END IF
       GO TO 50
 51    NWVNO=(WKMAX-WK0)/DLWVNO+1
       WK0=NINT((WK0-ETA)/DLWVNO)*DLWVNO+ETA
       WKMAX=WK0+(NWVNO-1)*DLWVNO
c ***  use same complex contour as for mean field
       OFFDB=OFFIMA*(8.68588964*V(LAYS(1),2))/FREQ
       ICUT1=1
       ICUT2=NWVNO
       REWIND(45)
       CALL OPNBUF(25,16,KCNT,200)
       read(45) nx_in_file,fr1_in_file,fr2_in_file,dt_in_file
       READ(45) FFF,LAYS(1),nkmean,nfmean,dummy
 52    READ(45,END=53) CBUF(1),INNR,(CBUF(II),II=2,9)
       IF (INNR.EQ.INTFCE) CALL WRBUF(25,CBUF(2),16)
       GO TO 52     
 53    CALL ENFBUF(25)
      ELSE
       NWVNO=MIN0(NWVNO,NP)
       ICUT2=MIN0(NWVNO,ICUT2)
       ICUT1=MAX0(1,ICUT1)
       DLWVNO = ( WKMAX - WK0 ) / ( FLOAT(NWVNO-1) )      
      END IF
      WRITE(6,600)CMIN,CMAX       
      WRITE(6,550)NWVNO,ICUT1,ICUT2                      

C          
C     OPEN PLOT FILES
C
      CALL OPFILW(19,IOER)
      CALL OPFILW(20,IOER)
      WRITE(19,6010) MODU,'MODU'

C
C     PLOT OF ROUGHNESS POWER SPECTRA
C
       IF (PLPOWER) THEN
        do il=2,numl
c         write(6,*) il,clen(il)
         if (clen(il).lt.1E5.and.clen(il).gt.1E-5) then
          nn=nwvno/2
          do ik=1,nn
           wn=ik*dlwvno
           cff(ik,1)=p(wn,il)
          end do
          xtit='Log k'
          ytit='Log h'
          CALL PLOGLOG(DLWVNO,dlwvno,nn,v(il,1),
     &                 TITLE,xtit,ytit,12.0,12.0)
         end if
        end do
       END IF
C
C *** AXIS PARAMETERS FOR REVERBERATION VS. RANGE PLOTS
C
      IF (reverb) THEN
       read(1,*) RMIN,RMAX,NR1
       if (ccontu) then
        READ(1,*) DR2,NR2
       end if
       LF=NR1
       R0=RMIN
       RSTEP=(RMAX-RMIN)/(LF-1)
       R1=R0*1E3
       DLRAN=RSTEP*1E3
      END IF
      IF ((ICONTU.AND.(.NOT.REVERB)).OR.CCONTU.or.DCONTU) THEN
C
C     OPEN CONTOUR PLOT FILES
C
        CALL OPFILW(28,IOER)
        CALL OPFILW(29,IOER)
      END IF

      if (covar) then
        call opfilw(24,ioer)
      end if

      IF (ICONTU.AND.(.NOT.REVERB)) THEN
C
C     DEPTH AXES
C
c >>> 940801 Changed to auto sampling
c        READ(1,*) RDUP(1),RDDOWN(1),CYAXIS(1),RDINC(1)
        cyaxis(1)=12.0
        CALL AUTOAX(RDC(1),RDC(ir),RDUP,RDDOWN,RDINC,YDIV,NYDIF)
        YSCALE=ABS(RDUP(1)-RDDOWN(1))/CYAXIS(1)
      END IF
C
C     OPEN SCRATCH FILE FOR INTEGRAND CONTOUR DATA
C
       IF (ICONTU.AND.(.NOT.REVERB)) THEN
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
C          
      NFLAG=.FALSE.
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
      CALL PINIT1
      if (DEBUG) CALL PREQV(NUML,NUMI)
      do ila=1,numl
       nosou(ila)=0
      end do
C
C     SLOWNESS DIAGRAM FOR TRANS. ISOTR. MEDIA
C
      IF (NTISOL.GT.0.and.IPROF.GT.0) THEN
       CALL SNSDGM(title)
      END IF

      DSQ=2E0*PI*FREQ             
      CSQ=DSQ*DSQ                 
      CALL PINIT2                 
      CALL CLTIME
      TOTTIM=0
      IF (.not.REVERB) THEN
        ETA=DSQ/CPHASE
        CALL CALSIN(ETA,INTFCE)
        CALL CHKSOL
        CALL RDTIME(T1)
        TOTTIM=TOTTIM+T1
        WRITE(6,310) T1
      END IF
 310  FORMAT(//1H ,'INTEGRANDS BUILT, TIME: ',F12.3,' SECS.')
C
C     reverberant energy CALCULATION
C
      IF (REVERB) THEN
       IF (CCONTU) THEN
        DO 19 NREC=1,IR
         CALL CLTIME
         CALL REVRAN(INTFCE,eta,kcnt,NREC,NR1,DR2,NR2)
         CALL AUTOAX(RMIN,RMAX,XLEFT,XRIGHT,XINC,XDIV,NXDIF)
         CALL AUTOAX(0E0,(NR2-1)*DR2,YUP,YDOWN,YINC,YDIV,NYDIF)
c         CALL VMIN(CFF(1,2),1,TLMIN,LF)
c         ZMIN(1)=INT(TLMIN/3E0)*3E0
         ZMIN(1)=0
         ZMAX(1)=ZMIN(1)+10
         ZSTEP(1)=1E0
         XAXIS=20
         YAXIS=12
         XSCALE=ABS(XRIGHT-XLEFT)/XAXIS
         YSCALE=ABS(YUP-YDOWN)/YAXIS
         CALL CONDRW(TITLE,NR1,NR2,NR1,NR2,XLEFT,XRIGHT,XSCALE,XINC,
     1            YUP,YDOWN,YSCALE,YINC,ZMIN,ZMAX,
     2            ZSTEP,FREQ,SD,0E0,(NR2-1)*DR2,RMIN,RMAX,PX,icdr)
         DO 68 JR=1,NR2
          CALL CONDRB(1,NR1,NR1,Xs(1+(JR-1)*NR1))
 68      CONTINUE
         CALL RDTIME(T2)
         TOTTIM=TOTTIM+T2
         WRITE(6,320) NREC,T2
 19     CONTINUE
       END IF
       if (PLTL.OR.DCONTU) then
        CALL CLTIME
        CALL REVINT(INTFCE,eta,kcnt,NR1)
        IF (PLTL) THEN
         write(6,*) '>>> TL plots vs range'
         CALL AUTOAX(RMIN,RMAX,XLEFT,XRIGHT,XINC,XDIV,NXDIF)
         do nrec=1,ir
          if (debug) then
           write(6,*) 'NREC=', NREC
          end if
          CALL VMIN(xs(1+(nrec-1)*nr1),1,TLMIN,LF)
          CALL VMAX(xs(1+(nrec-1)*nr1),1,TLMAX,LF)
          if (debug) then
           write(6,*) 'TLMIN,TLMAX=',TLMIN,TLMAX
          end if
          CALL AUTOAX(TLMIN,TLMAX,YUP,YDOWN,YINC,YDIV,NYDIF)
          XAXIS=20
          YAXIS=12
          DO I=1,npar
           IF (IOUT(I).NE.0) THEN
            CALL VMOV(xs(1+(nrec-1)*nr1),1,CFF(1,I),1,LF)
            CALL PLTLOS(LF,R0,RSTEP,TITLE,I,XAXIS,YAXIS,
     1                 XLEFT,XRIGHT,XINC,YUP,YDOWN,YINC,
     2                 SD,RDC(NREC))
           END IF
          end do
         end do
        END IF
        if (DCONTU) then
         CALL AUTOAX(RMIN,RMAX,XLEFT,XRIGHT,XINC,XDIV,NXDIF)
         CALL AUTOAX(RDC(1),RDC(ir),YUP,YDOWN,YINC,YDIV,NYDIF)
         CALL VMIN(XS(1),1,TLMIN,nr1*ir)
         ZMIN(1)=INT(TLMIN/3E0)*3E0
         ZMAX(1)=ZMIN(1)+30
         ZSTEP(1)=3E0
         XAXIS=20
         YAXIS=12
         XSCALE=ABS(XRIGHT-XLEFT)/XAXIS
         YSCALE=ABS(YUP-YDOWN)/YAXIS
         CALL CONDRW(TITLE,NR1,IR,NR1,IR,XLEFT,XRIGHT,XSCALE,XINC,
     1            YUP,YDOWN,YSCALE,YINC,ZMIN,ZMAX,
     2            ZSTEP,FREQ,SD,RDC(1),RDC(ir),RMIN,RMAX,PX,icdr)
         DO JR=1,IR
          CALL CONDRB(1,NR1,NR1,XS(1+(JR-1)*NR1))
         end do
        end if
        CALL RDTIME(T2)
        TOTTIM=TOTTIM+T2
       end if
       if (covar) then
        CALL CLTIME
        CALL REVCOV(INTFCE,eta,kcnt)
        CALL RDTIME(T2)
        TOTTIM=TOTTIM+T2
c >>> write covariance matrix of reverberant field
        nfreq=1
        freq1=freq
        freq2=freq
        call putxsm(corrns,nrcv,1,ioer)
       end if
      ELSE
C
C     WAVENUMBER SPECTRUM OF SCATTERED FIELD FOR SINGLE PLANE WAVE
C
       DO 20 NREC=1,IR
       CALL CLTIME
       CALL PHINT(NFLAG)
       if (debug) write(6,*) 'phint done'
C
C     PLOT OF HANKEL OR FOURIER TRANSFORMS
C
       IF (PLKERN) THEN
        CALL PLINLOG(DLWVNO,WK0,SD,RDC(NREC),TITLE,XAXIS,YIAXIS)
       END IF
C
C     PLOT OF ANGULAR SPECTRA
C
       IF (ANSPEC) THEN
        CALL PLSPECT(DLWVNO,WK0,SD,RDC(NREC),TITLE,
     1               XAXIS,YIAXIS,LAY(NREC))
       END IF
C
C     DECIMATE INTEGRAND FOR DEPTH CONTOUR PLOT
C
       IF (ICONTU) THEN
        NN=ICUT2-ICUT1+1
        DO 510 I=1,npar
          IF (IOUT(I).GT.0) THEN
            CALL CVMAGS(CFF(ICUT1,I),2,FFS,1,NN)
            CALL VCLIP(FFS,1,1E-30,1E30,FFS,1,NN)
            CALL VALG10(FFS,1,FFS,1,NN)
            CALL VSMUL(FFS,1,1E1,FFS,1,NN)
            CALL VDECIM(FFS,1,FFS,1,NN,NDEC,NC)
            CALL WRBUF(27,FFS,NC)
            CALL VMAX(FFS,1,AAA,NC)
            CONMAX(I)=MAX(CONMAX(I),AAA)
          END IF
 510    CONTINUE
       END IF
       CALL RDTIME(T2)
       TOTTIM=TOTTIM+T2
       WRITE(6,320) NREC,T2
 320   FORMAT(1H ,'RECEIVER NO. ',I4,' TIME=',F8.3,' SECONDS')
 20    CONTINUE
      END IF
C
C     CLOSE SCRATCH FILES
C
      IF (ICONTU.AND.(.NOT.REVERB)) THEN
        CALL ENFBUF(27)
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
          CALL INTCON(TITLE,NCON,IR,NCON,IR,
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
      OPTION(2)='PLTEND'
      WRITE(19,777) OPTION
 777  FORMAT(1H ,2A6)
C          
C          
      WRITE(6,9960)
 9960 FORMAT(//1H ,'*** SCATFLD FINISHED ***')
C
C     CLOSE SCRATCH FILES
C
      IF (.NOT.REVERB) THEN
       CALL CLSBUF(30)
      END IF

      IF (ICONTU.AND.(.NOT.REVERB)) CALL CLSBUF(27)
C
C     CLOSE PLOT FILES
C
      CLOSE(UNIT=19,STATUS='KEEP')
      CLOSE(UNIT=20,STATUS='KEEP')
      CLOSE(UNIT=21,STATUS='KEEP')
      IF ((ICONTU.AND.(.NOT.REVERB)).OR.CCONTU.OR.DCONTU) THEN
        CLOSE(UNIT=28,STATUS='KEEP')
        CLOSE(UNIT=29,STATUS='KEEP')
      END IF
      WRITE(6,9962) TOTTIM
 9962 FORMAT(//1H ,'*** TOTAL TIME: ',F10.3,' SECONDS ***')
      END  
C          
      SUBROUTINE GETOPT(IPROF,ICNTIN,ICONTU,REVERB,CCONTU,DCONTU,
     &                  covar,PLPOWER)
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
      LOGICAL ICONTU,REVERB,CCONTU,DCONTU,covar,PLPOWER
      WRITE(6,300)                
 300  FORMAT(//1H ,'OPTIONS:',/)    
      DEBUG=.FALSE.
      DEPTAV=.FALSE.
      DRCONT=.FALSE.
      PLTL=.FALSE.
      PLKERN=.FALSE.
      ANSPEC=.FALSE.
      TLDEP=.FALSE.
      SHEAR=.FALSE.
      ICONTU=.FALSE.
      REVERB=.FALSE.
      PLTL=.FALSE.
      CCONTU=.false.
      DCONTU=.false.
      covar=.false.
      NOUT=0                      
      IREF=0                      
      ISTYP=0                     
      ICDR=0
      IPROF=0
      ICNTIN=0
      DO 10 I=1,npar                 
 10   IOUT(I)=0                   
      READ(1,200) OPT             
 200  FORMAT(40A1)                
      DO 50 I=1,40                
      IF (OPT(I).EQ.'c') THEN 
       IF (ICONTU) GO TO 50    
       ICONTU=.TRUE.
       WRITE(6,306)           
 306   FORMAT(1H ,'DEPTH INTEGRAND CONTOURS')
      ELSE IF (OPT(I).EQ.'r') THEN
       IF (PLTL) GO TO 50
       PLTL=.TRUE.
       IF (.NOT.REVERB) THEN
        REVERB=.TRUE.
        WRITE(6,308)
       END IF
       WRITE(6,307)
 307   FORMAT(1H ,'REVERBERANT FIELD VS RANGE')
      ELSE IF (OPT(I).EQ.'R') THEN
       IF (REVERB) GO TO 50
       REVERB=.TRUE.
       WRITE(6,308)
 308   FORMAT(1H ,'REVERBERANT FIELD')
      ELSE IF (OPT(I).EQ.'I') THEN
       IF (PLKERN) GO TO 50
       PLKERN=.TRUE.
       WRITE(6,309)
 309   FORMAT(1H ,'HANKEL TRANSFORM INTEGRANDS')
      ELSE IF (OPT(I).EQ.'C') THEN
       IF (CCONTU) GO TO 50
       CCONTU=.TRUE.
       IF (.NOT.REVERB) THEN
        REVERB=.TRUE.
        WRITE(6,308)
       END IF
       WRITE(6,310)
 310   FORMAT(1H ,'CONTOURS OF HORIZONTAL CORRELATION')
      ELSE IF (OPT(I).EQ.'D') THEN
       IF (DCONTU) GO TO 50
       DCONTU=.TRUE.
       IF (.NOT.REVERB) THEN
        REVERB=.TRUE.
        WRITE(6,308)
       END IF
       WRITE(6,'(a)') 'CONTOURS REVERBERATION INTENSITY'
      else if (opt(i).eq.'d') then
       db=.true.
       write(6,'(a)') 'dB scales of angular spectra'
      ELSE IF (OPT(I).EQ.'a') THEN
       IF (covar) GO TO 50
       covar=.TRUE.
       IF (.NOT.REVERB) THEN
        REVERB=.TRUE.
        WRITE(6,308)
       END IF
       WRITE(6,'(a)') 'REVERBERATION COVARIANCE'
      ELSE IF (OPT(I).EQ.'P') THEN
       IF (ICDR.GT.0) GO TO 50
       ICDR=1
       WRITE(6,313)
 313   FORMAT(1H ,'PLANE GEOMETRY')
      ELSE IF (OPT(I).EQ.'S') THEN
       IF (ANSPEC) GO TO 50
       ANSPEC=.TRUE.
       WRITE(6,3091)
 3091  FORMAT(1H ,'ANGULAR SPECTRA')
      ELSE IF (OPT(I).EQ.'k') THEN
       IF (PLPOWER) GO TO 50
       PLPOWER=.TRUE.
       WRITE(6,3092)
 3092  FORMAT(1H ,'PLOT OF POWER SPECTRA')
      ELSE IF (OPT(I).EQ.'Z') THEN
       IF (IPROF.GT.0) GO TO 50
       IPROF=1
       WRITE(6,314)
 314   FORMAT(1H ,'PLOT OF VELOCITY PROFILES')
      ELSE IF (OPT(I).EQ.'G'.or.OPT(I).EQ.'g') THEN
       IF (goff) GO TO 50
       goff=.true.
       WRITE(6,315)
 315   FORMAT(1H ,'Goff-Jordan power spectrum')
      ELSE IF (OPT(I).EQ.'p') THEN
       IF (rescat) GO TO 50
       rescat=.true.
       WRITE(6,'(a)') 'Perturbed boundary operator for scattering'
      ELSE IF (OPT(I).EQ.'Q') THEN
        IF (DEBUG) GO TO 50
        DEBUG=.TRUE.
        WRITE(6,318)
 318    FORMAT(1H ,'***** DEBUGGING *****')
      ELSE IF (OPT(I).NE.' ') THEN
        WRITE(6,399) OPT(I)
 399    FORMAT(1H ,'>>>> UNKNOWN OPTION: ',A1,' <<<<')
      ELSE
      END IF
 50   CONTINUE                    
      IF (NOUT.NE.0) RETURN       
      IOUT(1)=1                   
      NOUT=1                      
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

C**** DEFINITION OF MAX REAL ARGUMENT TO THE EXPONENTIAL FUNCTION
      COMMON /ARGMAX/ AM
C**** THE FOLLOWING DEFINITION SHOULD BE USED FOR THE FPS164
CFPS  DATA AM /300./
C**** THE FOLLOWING DEFINITION SHOULD BE USED FOR THE VAX
      DATA AM /65./     
      DATA PROGNM /'OASS  '/
      DATA OMEGIM /0.0/
      DATA LUGRN,LUTRF,LUTGRN,LUTTRF /30,35,30,35/
      DATA SHEAR,DECOMP,SCTOUT,NFLAG,PADE 
     &     /.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE./
      DATA MSUFT,MBMAX,MBMAXI,SRCTYP,ISROW,ISINC /1,1,2,1,1,0/

      END

