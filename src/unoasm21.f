      SUBROUTINE OASPEN
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                 					C
C                     OASES V-1.6                       C
C							C
C        Ocean Acoustic and Seismic Exploration      	C
C                      Synthesis        		C
C							C
C              (C) Henrik Schmidt 1992			C
C							C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     MFP and BEAMFORMING VERSION 1.6
C     ULTRIX VERSION
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C          
      parameter (nchlmx = 100)
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
      INCLUDE 'srccoo.f'
      INCLUDE 'recarr.f'
      INCLUDE 'corrfn.f'
      INCLUDE 'corrfs.f'
      INCLUDE 'noiprm.f'

      COMPLEX COMBUF(NRD)

      INTEGER SRTPUT,SRTGET                                         

      LOGICAL CRARAO,NPWBF,INTPLT,DRCONT,RRCONT
      LOGICAL ITYPLT,NFRPLT,GAMPLT
      LOGICAL LOGBUF
      LOGICAL GETFLG, PUTFLG,RINFLG,ROTFLG,BLURR

      CHARACTER*6  BOPT(10),CRAOPT(6)
      CHARACTER*6  OPTION(2)
      CHARACTER*80  TITLE
      character*100 titcdr
      DIMENSION TMP(NP3)
      DIMENSION IBOPT(10)
      DIMENSION X(ITWONP,3),FF(2,NP3),PX(MODULO)              
      DIMENSION RDUP(3),RDDOWN(3),CYAXIS(3),RDINC(3)
      DIMENSION FFS(2,NP),XS(ITWONP)
      dimension repchl(nchlmx)     

      COMMON /SCRFIL/ INPFIL,IOUFIL
      COMMON /BEAMFM/ MAXLIK,NBEAMF
      COMMON /REPPNT/ INDEX0,NUMREP,NPNT
      COMMON /REPLIC/ ZSMIN,ZSMAX,XSMIN,XSMAX,YSMIN,YSMAX,
     1                NSRCZ,NSRCX,NSRCY
      COMMON /GETPUT/ SRTPUT, SRTGET                                
      COMMON /RTITLE/ TITLE                                         
      COMMON /KSFPAR/ TOLR,CHL,chlsig
      COMMON /MCMLM/ MAXIN,MCMDIR,BLURR

      EQUIVALENCE (TMP(1),CFF(1,1))
      EQUIVALENCE (NREC,ISPACE),(LF,NUMFR)
      EQUIVALENCE (X(1,1),CFF(1,1)),(XS(1),CFFS(1))        
      EQUIVALENCE (FF(1,1),CFF(1,1)),(FFS(1,1),CFFS(1))        

      DATA BOPT   /' BE   ',' ML   ',' TML  ',' MCM  ',
     -             ' STR  ',5*'      '/
      DATA CRAOPT /' DX   ',' DY   ',' DZ   ',' DV   ',
     -             ' GAM  ',' LEV  ' /
          

C ********************  some FORMATS  *******************
          
200   FORMAT(a)                
210   FORMAT(//1H ,'OASES MATCHED FIELD V1.3',//1H ,a)   
220   FORMAT(1H ,F10.2,2X,F10.3,2X,F10.3,2X,F10.1)
310   FORMAT(//1H ,'INTEGRANDS BUILT, SD: ',F12.3,' M.',
     &                ' CPU=',F12.3)
350   FORMAT(//1H ,'    DEPTH        ALPHA       BETA      ATTENA     '
     -,          '  ATTENB         RHO       ROUGHNESS'//
     -       1H ,3F12.5,2F12.8,2F12.5  )  
500   FORMAT(1H ,' ',
     &      /1H ,'SOURCE FREQUENCY:',F10.2,' HZ ',
     &      /1H ,'------------------------------')
550   FORMAT(//1H ,3X,2HLS,5X,5HICUT1,5X,5HICUT2,/(1X,I5,2I10))
551   FORMAT(//1H ,'TOTAL   CONTINUOUS  DISCRETE',/(1X,I5,2I10))
600   FORMAT(//,'  CMIN = ',G15.6,' M/S ',/,'  CMAX = ',G15.6,' M/S ')
650   FORMAT(1H ,' CCUT = ',G15.6,' M/S')
6010  FORMAT(1H ,I8,10X,A40)
          
          
C ******************************************************         
          
      NBEAMF = 4
      INDEX0 = 0
      NUMREP = 0
      NPNT   = 0          
      INPFIL = 40
      IOUFIL = 41
      MODU   = MODULO
      ITXX   = ITWONP
      PI     = 4.0*ATAN(1.0)           
      AI     = CMPLX(0.,1.)             
      CNUL   = CMPLX(0.,0.)
      IR     = 1 
      LS     = 1
      LAYS(1)= 2
      DELTA  = 1.
      THETA  = 0.
      FOCDEP = 0.
      LTYP   = 1
      LINA   = 0
      NFLAG  = .FALSE.
      ICNTIN = 0

      SRTPUT = 0                                                    
      SRTGET = 0                                                    
      IDUM1  = 0                                                    
      IDUM2  = 0                                                    
          
C*****
C*  Read Input File  (unit 1 = input)
C*****          
          
c      OPEN(UNIT=1,STATUS='OLD')
      call opfilr(1,ierr)
      READ(1,200)   TITLE            
      WRITE(6,210)  TITLE           
      WRITE(26,200) TITLE

      CALL GETOPT (IPROF,ICNTIN,MFAC,IPARES,IBOPT,CRARAO,NPWBF,
     -             INTPLT,DRCONT,RRCONT,ITYPLT,NFRPLT,GAMPLT,
     -             GETFLG,PUTFLG,RINFLG,ROTFLG)                

C*****  READ IN FREQUENCY DATA

      IF (ICNTIN.GT.0) THEN
        READ(1,*) FREQ1,FREQ2,NFREQ,OFFDBIN
      ELSE
        OFFDBIN=0.0
        READ(1,*) FREQ1,FREQ2,NFREQ             
      END IF
      FREQ=0.5*(FREQ1+FREQ2)
      IF (NFREQ.GT.1) THEN
        DLFREQ=(FREQ2-FREQ1)/(NFREQ-1)
      ELSE
        DLFREQ=0.0
      END IF

C*****  READ IN COORDINATES FOR RECEIVER ARRAY

      CALL INPRCV

      IF (IR.GT.NRD) THEN
         WRITE(6,*) '*** TOO MANY RECEIVER DEPTHS ***'
         STOP
      END IF

C*****  Housekeeping for XSM From External File                     
C                                                                   
      IF ( GETFLG ) THEN                                            
C                                                                   
C                                                                   
C         *****  READ HEADER INFORMATION FROM XSM FILE              

          CALL GETXSM (CORRFD,NRCV,0,IERR)                          
C                                                                   
      END IF                                                        
C                                                                   
C*****  OPEN PLOT ANS CHECK FILES

      CALL OPFILW(19,IOER)
      CALL OPFILW(20,IOER)
      CALL OPFILW(21,IOER)

      WRITE(19,6010) MODU,'MODU'

C*****  READ IN SOURCE COORDINATES FOR WHICH REPLICA FIELDS
C       HAVE TO BE CALCULATED

      IF (IPARES.EQ.1) THEN
         NSRCZ=0
         NSRCY=0
         NSRCX=0
         READ(1,*) ZSMIN,ZSMAX,NSRCZ
         READ(1,*) XSMIN,XSMAX,NSRCX
         READ(1,*) YSMIN,YSMAX,NSRCY
c
c       set gain factor to 0 dB
c
        NLEVEL=1
        slevf(1)=1e0
c        READ(1,*) GNLEVEL
c        SLEVF(1)=10.0**(GNLEVEL/10.0)
        if (.not.npwbf) then
         read(1,*) cbeam
         write(6,*)
         write(6,*) '>>> Beamforming reference speed:',cbeam,' m/s'
        end if

C
C       READ IN REPLICA FILE HEADER FOR OPTION RINFLG
C
         IF (RINFLG) THEN
           CALL GETREP(COMBUF)
         END IF

C
C       WRITE OUT REPLICA FILE HEADER FOR OPTION ROTFLG
C
         IF (ROTFLG) THEN
           CALL PUTREP(COMBUF)
         END IF
C
         IF (NSRCZ.GT.1) THEN
            DO 601 I=1,NSRCZ
               ZSC(I)=(ZSMIN+(I-1)*(ZSMAX-ZSMIN)/FLOAT(NSRCZ-1))
601         CONTINUE
         ELSE
            ZSC(1)=ZSMIN
         END IF
         IF (NPWBF) THEN
            rfc=1E3
         ELSE 
            RFC=1E0
         END IF
         IF (NSRCX.GT.1) THEN
            DO 602 I=1,NSRCX
               XSC(I)=RFC*(XSMIN+(I-1)*(XSMAX-XSMIN)/FLOAT(NSRCX-1))
602         CONTINUE
         ELSE 
            XSC(1)=RFC*XSMIN
         END IF
         IF (NSRCY.GT.1) THEN
            DO 603 I=1,NSRCY
               YSC(I)=RFC*(YSMIN+(I-1)*(YSMAX-YSMIN)/FLOAT(NSRCY-1))
603         CONTINUE
         ELSE
            YSC(1)=RFC*YSMIN
         END IF


C*****  OPEN OUTPUT FILES for

        CALL OPFILW(26,IOER)
        CALL OPFILW(42,IOER)

C*****  DEPTH RANGE CONTOUR OPTION

        IF (DRCONT.OR.RRCONT) THEN
c           READ(1,*) XLEFT,XRIGHT,XAXIS,XINC
c           READ(1,*) RDUP(1),RDDOWN(1),CYAXIS(1),RDINC(1)
C          READ(1,*) ZMIN,ZMAX,ZSTEP
            call autoax(xsmin,xsmax,xleft,xright,xinc,xdiv,nxdif)
           if (rrcont) then
            call autoax(ysmin,ysmax,rdup(1),rddown(1),rdinc(1),
     &                  ydiv,nydif)
            xaxis=15.0
            cyaxis(1)=abs((rddown(1)-rdup(1))/(xright-xleft))*xaxis
           else if (.not.npwbf) then
              write(6,*)zsmin,zsmax,rdup(1),rddown(1),rdinc(1),
     &                   ydiv,nydif
              call autoax(zsmin,zsmax,rdup(1),rddown(1),rdinc(1),
     &                   ydiv,nydif)
            xaxis=15.0
            cyaxis(1)=abs((rddown(1)-rdup(1))/(xright-xleft))*xaxis
           else
            call autoax(zsmin,zsmax,rdup(1),rddown(1),rdinc(1),
     &                  ydiv,nydif)
            xaxis=18.0
            cyaxis(1)=11.0
           end if
           XSCALE=ABS(XRIGHT-XLEFT)/XAXIS
           YSCALE=ABS(RDUP(1)-RDDOWN(1))/CYAXIS(1)
           XXL=XSMIN
           XXM=XSMAX
           WDEPTH=0.0

C          *****  OPEN CONTOUR PLOT FILES

           CALL OPFILW(28,IOER)
           CALL OPFILW(29,IOER)

        END IF
      END IF
C
C     READ TOLERANCE AND COHERENCE DATA FOR
C     STOCHASTIC BEAMFORMING
C
      IF (IBOPT(5).GT.0) THEN
        READ(1,*) chlmin,chlmax,nchl
        READ(1,*) TOLR
        WRITE(6,*)
        WRITE(6,*) 'SCANNING COHERENCE LENGTHS:'
        write(6,*) 'Min. Coh. length:', chlmin
        write(6,*) 'Max. Coh. length:', chlmax
        write(6,*) 'Number of values:', nchl
        WRITE(6,*) 'TOLERANCE:             ',TOLR
        if (nchl.le.nchlmx) then
         dchl=(chlmax-chlmin)/(nchl-1)
         do ichl=1,nchl
          repchl(ichl)=chlmin+(ichl-1)*dchl
         end do
         CHL=1E0/(chlmin**2)
        else
         stop '>>> Too many coherence lengths for replicas <<<'
        end if
      END IF
      IF (BLURR) THEN
        READ(1,*) CHRLEN
        WRITE(6,*) 
        WRITE(6,*) 'SIGNAL COHERENCE LENGTH:',CHRLEN
        CHLSIG=1E0/(CHRLEN**2)
      END IF

C*****  OPEN SCRATCH FILE FOR NOISE AND FIELD CORRELLATION MATRICES

      LRECN=2*NRCV*NRCV
         ISI=NFREQ
         CALL OPNBUF(32,LRECN,ISI,max(ISIZE/64+1,nrcv*nrcv))
         IF (IPARES.GT.0) THEN
            LRECORD=2*NRCV
            CALL OPNBUF(33,LRECORD,ISI,ISIZE/64+1)
  
C           *****  OPEN SCRATCH FILE FOR REPLICA FIELDS
  
            ISI=NSRCZ*NSRCX*NSRCY*NFREQ
            CALL OPNBUF(31,LRECORD,ISI,500)
         END IF

C**********************  BEGIN FREQUENCY LOOP  *************************

      DO 20 IFR=1,NFREQ        

      TIMF=0
      CALL CLTIME
      FREQ=FREQ1+(IFR-1)*DLFREQ
      WRITE(6,500)FREQ  
      WRITE(26,*) FREQ,' FREQUENCY'          
      if (RINFLG) then
C
C         READ REPLICA FIELD IN FOR OPTION RINFLG
C
          WRITE(6,*)
          WRITE(6,*) 'REPLICA FIELDS READ IN'
          WRITE(6,*) '----------------------'
      
          DO 724 ISRCZ=1,NSRCZ
          DO 723 ISRCX=1,NSRCX
          DO 722 ISRCY=1,NSRCY
          CALL GETRDA(COMBUF)
          CALL WRBUF(31,COMBUF,2*NRCV)
          IF (ROTFLG) CALL PUTRDA(COMBUF)
 722      CONTINUE
 723      CONTINUE
 724      CONTINUE
      else

C           *****  CONVENTIONAL PLANE WAVE BEAMFORMING
            write(6,*) 
            write(6,*) 'Computing plane wave replicas'
            write(6,*) '-----------------------------'
            WNREF=2*PI*FREQ/cbeam
            velfac=1e0/(1000*cbeam)    
            DO 713 ISRCZ=1,NSRCZ
               THETA=ZSC(ISRCZ)*3.14159/180.0
               CTH=COS(THETA)
               STH=SIN(THETA)
               DO 712 ISRCX=1,NSRCX
                  PHI=XSC(ISRCX)*3.14159/180.0
                  CPHI=COS(PHI)
                  SPHI=SIN(PHI)
                  DO 711 IRCV=1,NRCV
                     COMBUF(IRCV)=gain(ircv)*EXP(AI*WNREF*(STH*DEP(IRCV) 
     -                                + CTH*CPHI*RAN(IRCV)
     -                                + CTH*SPHI*TRAN(IRCV)))
                    if (irtyp(ircv).eq.2) then
c                     x velocity
                     combuf(ircv)=ai*wnref*combuf(ircv)*velfac
     &                    *cth*cphi
                    else if (irtyp(ircv).eq.3) then
c                     y velocity
                     combuf(ircv)=ai*wnref*combuf(ircv)*velfac
     &                    *cth*sphi
                    else if (irtyp(ircv).eq.4) then
c                     z velocity
                     combuf(ircv)=ai*wnref*combuf(ircv)*velfac
     &                    *sth
                    else
                    end if
711               CONTINUE
                  CALL WRBUF(31,COMBUF,2*NRCV)
                  IF (ROTFLG) CALL PUTRDA(COMBUF)
712            CONTINUE
713         CONTINUE

      end if
      CALL RDTIME(T1)
      TIMF=TIMF+T1
      TOTTIM=TOTTIM+TIMF
      WRITE(6,311) FREQ,TIMF
311   FORMAT(1H ,'FREQ. ',F8.2,' Hz DONE,               CPU=',F12.3)

20    CONTINUE    

C************************* END of FREQUENCY LOOP **********************


C*****  ENDFILE ON BUFFER FILES

      CALL ENFBUF(32)
      IF (IPARES.GT.0) THEN
         CALL ENFBUF(31)
         CALL ENFBUF(33)
      END IF

C*****  PARAMETER ESTIMATION SECTION
      nlevel=1
      IF (IPARES.GT.0) THEN
         call cltime()

         WRITE(42,*) ZSMIN,ZSMAX,NSRCZ,'  ZSMIN,ZSMAX,NSRCZ'
         WRITE(42,*) XSMIN,XSMAX,NSRCX,'  XSMIN,XSMAX,NSRCX'
         WRITE(42,*) YSMIN,YSMAX,NSRCY,'  YSMIN,YSMAX,NSRCY'
         DO 61 MAXLIK=0,NBEAMF

            IF (IBOPT(MAXLIK+1).LT.1) GO TO 61
            DO 60 ILEVEL=1,NLEVEL
             if (maxlik.eq.4) then
              nloopc=nchl
             else
              nloopc=1
             end if
             do 60 iloopc=1,nloopc
               call cltime()
               WRITE(6,*) 
               WRITE(6,*) 'PARAMETER ESTIMATION'
               WRITE(6,*) '--------------------'
               WRITE(6,*) 'SOURCE LEVEL:',SLEVDB(ILEVEL),' dB'
               WRITE(6,*) 'BEAMFORMER:  ',BOPT(MAXLIK+1)
               if (maxlik.eq.4) then
c >>>           Tolerant beamformer
                chl=1e0/(repchl(iloopc)**2)
                write(6,*) 'COHERENCE LENGTH:',repchl(iloopc),' m'
                ll=lenstr(title)
                write(titcdr,'(a,a,f6.1)') title(1:ll),' - chl =',
     &                                              repchl(iloopc)
               else
                titcdr=title
               end if
C              *****  REWIND BUFFER FILES

               CALL RWDBUF(31)
               CALL RWDBUF(32)
               CALL RWDBUF(33)
               IF (CRARAO) THEN
                  CALL RWDBUF(36)
                  CALL RWDBUF(37)
                  CALL RWDBUF(38)
               END IF

C              *****  OPEN SCRATCH FILES FOR AMBIGUITY FUNCTION

               CALL OPNBUF(INPFIL,NSRCY,NSRCX*NSRCZ,100)

C              *****  FILL INPUT FILE WITH ZEROES

               CALL VCLR(PX,1,NSRCY)
               DO 21 J=1,NSRCX*NSRCZ
                  CALL WRBUF(INPFIL,PX,NSRCY)
21             CONTINUE
               CALL ENFBUF(INPFIL)

               WFREQ=1E0/NFREQ
               AMPMX=-1E20
               DO 50 IFR=1,NFREQ     
                  CALL CLTIME
                  FREQ=FREQ1+(IFR-1)*DLFREQ
                  CALL RWDBUF(INPFIL)
                  CALL OPNBUF(IOUFIL,NSRCY,NSRCX*NSRCZ,100)
                  IF (CRARAO.AND.MAXLIK.EQ.1) THEN
                     CALL OPNBUF(50,NSRCY,4*NSRCX*NSRCZ,100)
                  END IF

C                 ONLY SIMILARITY CALCULATION FOR MLM AND ONLY FOR 
C                 FIRST LEVEL AS IT IS INDEPENDENT OF THE SIGNAL LEVEL.

                  LOGBUF=GAMPLT.AND.(MAXLIK.EQ.1).AND.(ILEVEL.EQ.1)
                  IF (LOGBUF) THEN
                     CALL OPNBUF(51,2*NSRCY,NSRCX*NSRCZ,100)
                  END IF

                  write(6,*)'Calling Parest....'
                  CALL PAREST (NSRCZ,NSRCX,NSRCY,IFR,WFREQ,
     -                         SLEVF(ILEVEL),AMPMX,CRARAO,
     -                         LOGBUF,GETFLG,PUTFLG)

                  write(6,*)'Parest done'
                  CALL ENFBUF(IOUFIL)
                  CALL CLSBUF(INPFIL)
                  IF (CRARAO.AND.MAXLIK.EQ.1) THEN

C                    *****  DEPTH-RANGE CONTOURS OF CRAMER-RAO BOUNDS

                     IF (DRCONT) THEN
                        WRITE(6,*)'  DR - CONTOURS '
                        CALL ENFBUF(50)
                        IF (NPWBF) THEN
                           IAXT=1
                        ELSE
                           IAXT=3
                        END IF
                        DO 32 ICRAPA=1,4
                           CALL RWDBUF (50)
                           CALL CRADRW (TITLE,NSRCX,NSRCZ,NSRCX,NSRCZ,
     -                                  XLEFT,XRIGHT,XSCALE,XINC,
     -                                  RDUP(1),RDDOWN(1),YSCALE,
     -                                  RDINC(1),10.,40.,3.,FREQ,
     -                                  SLEVDB(ILEVEL),ZSMIN,ZSMAX,
     -                                  XXL,XXM,PX,CRAOPT(ICRAPA),IAXT)
                           DO 44 L=1,NSRCZ
                              DO 33 J=1,NSRCX
                                 DO 33 JJJ=1,4
                                    CALL RDBUF(50,PX,NSRCY)
                                    IF (ICRAPA.EQ.JJJ) THEN
                                       XS(J)=10.0*LOG10
     -                                                (MAX(1E-20,PX(1)))
                                    END IF
33                            CONTINUE
                              CALL CONDRB(1,NSRCX,NSRCX,XS)
44                         CONTINUE
32                      CONTINUE
                     END IF

C                    *****  RANGE-RANGE CONTOURS OF CRAMER-RAO BOUNDS
C
                     IF (RRCONT.AND.NPWBF) THEN
                        WRITE(6,*)'  RR - CONTOURS '
                        IAXT=2
                        DO 132 ICRAPA=1,4
                           CALL RWDBUF (50)
                           CALL CRADRW (TITLE,NSRCX,NSRCY,NSRCX,NSRCY,
     -                                  XLEFT,XRIGHT,XSCALE,XINC,
     -                                  RDUP(1),RDDOWN(1),YSCALE,
     -                                  RDINC(1),10.,40.,3.,FREQ,
     -                                  SLEVDB(ILEVEL),YSMIN,YSMAX,
     -                                  XXL,XXM,PX,CRAOPT(ICRAPA),IAXT)
                           DO 240 L=1,NSRCY
                              CALL RWDBUF(50)
                              DO 230 J=1,NSRCX
                                 DO 230 JJJ=1,4
                                    CALL RDBUF(50,PX,NSRCY)
                                    IF (ICRAPA.EQ.JJJ) THEN
                                       XS(J)=10.0*LOG10
     -                                              (MAX(1E-20,PX(L)))
                                    END IF
230                           CONTINUE
                              CALL CONDRB(1,NSRCX,NSRCX,XS)
240                        CONTINUE
132                     CONTINUE
                     END IF
                     CALL CLSBUF(50)
                  END IF
                  IF (LOGBUF) THEN

C                    *****  DEPTH-RANGE CONTOURS OF SIMILARITY FUNCTION 
C                           GAMMA (OPTION G)

                     IF (DRCONT) THEN
C                       WRITE(6,*)'  DR - CONTOURS '
                        CALL ENFBUF(51)
                        IF (NPWBF) THEN
                           IAXT=1
                        ELSE
                           IAXT=3
                        END IF
                        CALL CRADRW (TITLE,NSRCX,NSRCZ,NSRCX,NSRCZ,
     -                               XLEFT,XRIGHT,XSCALE,XINC,RDUP(1),
     -                               RDDOWN(1),YSCALE,RDINC(1),0.95,
     -                               0.05,0.1,FREQ,SLEVDB(ILEVEL),
     -                               ZSMIN,ZSMAX,XXL,XXM,PX,CRAOPT(5),
     -                               IAXT)
                        CALL RWDBUF(51)
                        DO 344 L=1,NSRCZ
                           DO 333 J=1,NSRCX
                              CALL RDBUF(51,PX,2*NSRCY)
                              XS(J)=PX(1)
333                        CONTINUE
                           CALL CONDRB(1,NSRCX,NSRCX,XS)
344                     CONTINUE
                        CALL CRADRW (TITLE,NSRCX,NSRCZ,NSRCX,NSRCZ,
     -                               XLEFT,XRIGHT,XSCALE,XINC,RDUP(1),
     -                               RDDOWN(1),YSCALE,RDINC(1),200.,
     -                               100.,10.,FREQ,SLEVDB(ILEVEL),
     -                               ZSMIN,ZSMAX,XXL,XXM,PX,CRAOPT(6),
     -                               IAXT)
                        CALL RWDBUF(51)
                        DO 345 L=1,NSRCZ
                           DO 334 J=1,NSRCX
                              CALL RDBUF(51,PX,2*NSRCY)
                              XS(J)=PX(1+NSRCY)
334                        CONTINUE
                           CALL CONDRB(1,NSRCX,NSRCX,XS)
345                     CONTINUE
                     END IF

C                    *****  RANGE-RANGE CONTOURS OF SIMILARITY FUNCTION
C                           GAMMA (OPTION G,R)

                     IF (RRCONT.AND.NPWBF) THEN
                        IAXT=2
                        CALL CRADRW (TITLE,NSRCX,NSRCY,NSRCX,NSRCY,
     -                               XLEFT,XRIGHT,XSCALE,XINC,RDUP(1),
     -                               RDDOWN(1),YSCALE,RDINC(1),0.95,
     -                               0.05,0.1,FREQ,SLEVDB(ILEVEL),
     -                               YSMIN,YSMAX,XXL,XXM,PX,CRAOPT(5),
     -                               IAXT)
                        DO 440 L=1,NSRCY
                           CALL RWDBUF(51)
                           DO 430 J=1,NSRCX
                              CALL RDBUF(51,PX,2*NSRCY)
                              XS(J)=PX(L)
430                        CONTINUE
                           CALL CONDRB(1,NSRCX,NSRCX,XS)
440                     CONTINUE

                        CALL CRADRW (TITLE,NSRCX,NSRCY,NSRCX,NSRCY,
     -                               XLEFT,XRIGHT,XSCALE,XINC,RDUP(1),
     -                               RDDOWN(1),YSCALE,RDINC(1),200.,
     -                               100.,10.,FREQ,SLEVDB(ILEVEL),
     -                               YSMIN,YSMAX,XXL,XXM,PX,CRAOPT(6),
     -                               IAXT)
                        DO 441 L=1,NSRCY
                           CALL RWDBUF(51)
                           DO 431 J=1,NSRCX
                              CALL RDBUF(51,PX,2*NSRCY)
                              XS(J)=PX(L+NSRCY)
431                        CONTINUE
                           CALL CONDRB(1,NSRCX,NSRCX,XS)
441                     CONTINUE
                     END IF
                     CALL CLSBUF(51)
                  END IF

C                 *****  SWAP INPUT AND OUTPUT FILES

                  III=INPFIL
                  INPFIL=IOUFIL
                  IOUFIL=III
c                  CALL RDTIME(T1)
c                  T1=T1*.182E-6
c                  TOTTIM=TOTTIM+T1
c                  WRITE(6,311) FREQ,T1
50             CONTINUE   


C              *****  DEPTH-RANGE CONTOURS OF AMBIGUITY FUNCTION

               CALL RWDBUF(INPFIL)
C              WRITE(6,*) NSRCZ,NSRCX,NSRCY,' NSRCZ,NSRCX,NSRCY'
               WRITE(42,*) XSIM,YSIM,ZSIM,SLEVDB(ILEVEL),
     -                     ' SOURCE'
               ZMAX=INT(AMPMX)
               ZMIN=ZMAX-10.
               ZSTEP=1.
               FRQM=0.5*(FREQ1+FREQ2)

               IF (DRCONT) THEN
                  IF (NPWBF) THEN
                     IAXT=1
                  ELSE
                     IAXT=3
                  END IF

                  CALL AMBDRW (TITcdr,NSRCX,NSRCZ,NSRCX,NSRCZ,XLEFT,
     -                         XRIGHT,XSCALE,XINC,RDUP(1),RDDOWN(1),
     -                         YSCALE,RDINC(1),ZMAX,ZMIN,ZSTEP,FRQM,
     -                         SLEVDB(ILEVEL),ZSMIN,ZSMAX,XXL,XXM,PX,
     -                         BOPT(MAXLIK+1),IAXT)
               END IF

               DO 40 L=1,NSRCZ
                  DO 30 J=1,NSRCX
                     CALL RDBUF(INPFIL,PX,NSRCY)
                     WRITE(42,'(1H ,8F8.2)') (PX(K),K=1,NSRCY)
                     XS(J)=PX(1)
                     TMP(L+(J-1)*NSRCZ)=PX(1)
30                CONTINUE


                  IF (DRCONT) THEN
                     CALL CONDRB(1,NSRCX,NSRCX,XS)
                  ELSE IF ((.NOT.NPWBF).AND.NSRCX.GT.1) THEN
                     call autoax(xsmin,xsmax,xleft,xright,xinc,
     -                           xdiv,nxdif)
                     CALL PLPWAB (TMP(L),NSRCZ,NSRCX,XXL,(XXM-XXL)/
     -                            (NSRCX-1),TITcdr,MAXLIK+1,PX,MODULO,
     -                            20.0,12.0,XLEFT,XRIGHT,XINC,ZMAX+2.,
     -                            ZMAX-18.,4.0,FRQM,2,SLEVDB(ILEVEL),
     -                            SNLEVDB,WNLEVDB)
                  END IF
40             CONTINUE

               IF ((.NOT.DRCONT).AND.(.NOT.NPWBF).AND.NSRCZ.GT.1) THEN 
                  call autoax(zsmin,zsmax,rdup(1),rddown(1),rdinc(1),
     -                        zdiv,nzdif)
                  DO 41 ISRCX=1,NSRCX
                  CALL PLPWAB (TMP(1+(ISRCX-1)*NSRCZ),1,NSRCZ,
     -                         ZSMIN,(ZSMAX-ZSMIN)/(NSRCZ-1),
     -                         TITcdr,MAXLIK+1,PX,MODULO,20.0,12.0,
     -                         RDUP(1),RDDOWN(1),RDINC(1),ZMAX+2.,
     -                         ZMAX-18.,4.0,FRQM,1,SLEVDB(ILEVEL),
     -                         SNLEVDB,WNLEVDB)
41                CONTINUE
               END IF

C              *****  RANGE-RANGE CONTOURS OF AMBIGUITY FUNCTION

               IF (RRCONT.AND.NPWBF) THEN
                  ZMAX=INT(AMPMX)
                  ZMIN=ZMAX-10.
                  ZSTEP=1.
                  FRQM=0.5*(FREQ1+FREQ2)
                  IAXT=2
                  CALL AMBDRW (TITcdr,NSRCX,NSRCY,NSRCX,NSRCY,XLEFT,
     -                         XRIGHT,XSCALE,XINC,RDUP(1),RDDOWN(1),
     -                         YSCALE,RDINC(1),ZMAX,ZMIN,ZSTEP,FRQM,
     -                         SLEVDB(ILEVEL),YSMIN,YSMAX,XXL,XXM,PX,
     -                         BOPT(MAXLIK+1),IAXT)

                  DO 140 L=1,NSRCY
                     CALL RWDBUF(INPFIL)
                     DO 130 J=1,NSRCX
                        CALL RDBUF(INPFIL,PX,NSRCY)
                        XS(J)=PX(L)
130                  CONTINUE
                     CALL CONDRB(1,NSRCX,NSRCX,XS)
140               CONTINUE
               END IF
               CALL CLSBUF(INPFIL)


      CALL RDTIME(T1)
      TOTTIM=TOTTIM+T1
      write(6,*) '>>> Done, CPU=',t1,' sec <<<'
60          CONTINUE  
61       CONTINUE     
      END IF          


      OPTION(2)='PLTEND'
      WRITE(19,777) OPTION(2)
      WRITE(20,777) OPTION(2)
      WRITE(6,9960)

 777  FORMAT(1H ,2A6)
 9960 FORMAT(//1H ,'*** SAFARI NOISE AND PARAM. ESTIM. FINISHED ***')

C*****  CLOSE BUFFER FILES

      CALL CLSBUF(32)
      IF (IPARES.GT.0) THEN
         CALL CLSBUF(31)
         CALL CLSBUF(33)
      END IF

      CLOSE(42,STATUS='KEEP')

      WRITE(6,9962) TOTTIM
 9962 FORMAT(//1H ,'*** TOTAL TIME: ',F10.3,' SECONDS ***')

      END  

C*********************** END OF SAFARI MAIN ROUTINE *******************
C


      SUBROUTINE GETOPT (IPROF,ICNTIN,MFAC,IPARES,IBOPT,CRARAO,NPWBF,
     -                   INTPLT,DRCONT,RRCONT,ITYPLT,NFRPLT,GAMPLT,
     -                   GETFLG,PUTFLG,RINFLG,ROTFLG)

C     INPUT OF OPTIONS
C          modified by Bruce H Pasewark   September 30, 1986
C              real (i.e. experimental)  field option added
C
C     current options : B C D E F G H I J M N O P Q R S T U W X Z (1-9)
C     future options  : A K L V Y
      INCLUDE 'compar.f'
      LOGICAL CRARAO,NPWBF,INTPLT,DRCONT,RRCONT,
     -        ITYPLT,NFRPLT,GAMPLT
      LOGICAL GETFLG, PUTFLG, RINFLG, ROTFLG                        
      LOGICAL SECCHAR, BLURR
      CHARACTER*1 OPT(40)

      DIMENSION IBOPT(10)

      COMMON /MCMLM/ MAXIN,MCMDIR,BLURR

      WRITE(6,300)                
 300  FORMAT(//1H ,'OPTIONS:',/)    

      MAXIN   = 1
c      IPRINT  = 1
c      IPRINT  = 0  for debug info (EKS)
      NOUT    = 0
      IREF    = 0
      ISTYP   = 0
      KPLOT   = 0
      ICDR    = 0
      IPROF   = 0
      ICNTIN  = 0
      MFAC    = 0
      IPARES  = 1
      NBOPT   = 0
      BLURR   = .FALSE.

      DO 10 I=1,3                 
         IOUT(I)  = 0     
10    CONTINUE
      DO 20 I=1,10
         IBOPT(I) = 0
 20   CONTINUE
      CRARAO = .FALSE.              
      GAMPLT = .FALSE.              
      NPWBF  = .TRUE.
      INTPLT = .FALSE.
      DRCONT = .FALSE.
      RRCONT = .FALSE.
      ITYPLT = .FALSE.
      NFRPLT = .FALSE.
      GETFLG = .TRUE.                                              
      PUTFLG = .FALSE.                                              
      RINFLG = .TRUE.                                              
      ROTFLG = .FALSE.                                              
      SHEAR=.FALSE.
      SECCHAR=.FALSE.

      READ(1,200) OPT             
 200  FORMAT(40A1)                

      DO 50 I=1,40   
         IF (SECCHAR) THEN
           SECCHAR=.FALSE.
           GO TO 50             
         ELSE IF (OPT(I).EQ.'D') THEN
            IF (.NOT.DRCONT) THEN
               DRCONT=.TRUE.
               WRITE(6,*) 'RANGE-DEPTH CONTOURS OF AMBIGUITY FUNCTION'
            END IF

         ELSE IF (OPT(I).EQ.'R'.OR.OPT(I).EQ.'r') THEN
            IF (.NOT.RRCONT) THEN
               RRCONT=.TRUE.
               WRITE(6,*) 'RANGE-RANGE CONTOURS OF AMBIGUITY FUNCTION'
            END IF

         ELSE IF (OPT(I).EQ.'X'.OR.OPT(I).EQ.'x') THEN
            IF (IPRINT.EQ.0) THEN
               IPRINT=1
               WRITE(6,*) 'EXPANDED PRINT-OUT'
            END IF

         ELSE IF (OPT(I).EQ.'B'.OR.OPT(I).EQ.'b') THEN
            IF (IBOPT(1).EQ.0) THEN
               NBOPT=NBOPT+1
               IBOPT(1)=1
               WRITE(6,*) 'BARTLETT ESTIMATE'
            END IF

         ELSE IF (OPT(I).EQ.'M'.OR.OPT(I).EQ.'m') THEN
            IF (IBOPT(2).EQ.0) THEN
               NBOPT=NBOPT+1
               IBOPT(2)=1
               WRITE(6,*) 'MAXIMUM LIKELIHOOD BEAMFORMING'
            END IF

c         ELSE IF (OPT(I).EQ.'T'.OR.OPT(I).EQ.'t') THEN
c            IF (IBOPT(3).EQ.0) THEN
c               NBOPT=NBOPT+1
c               IBOPT(3)=1
c               WRITE(6,*) 'TRUE MAXIMUM LIKELIHOOD BEAMFORMING'
c            END IF

         ELSE IF (OPT(I).EQ.'Q'.OR.OPT(I).EQ.'q') THEN
            IF (IBOPT(4).EQ.0) THEN
               NBOPT=NBOPT+1
               IBOPT(4)=1
               WRITE(6,*) 'MULTIBLE CONSTRAINT MLM BEAMFORMING'
               MCMDIR=7
               IF (OPT(I+1).EQ.'1'.OR.OPT(I+1).EQ.'2'.OR.
     &             OPT(I+1).EQ.'3'.OR.OPT(I+1).EQ.'0') THEN
                 MAXIN=ICHAR(OPT(I+1))-48
                 SECCHAR=.TRUE.
               ELSE
                 MAXIN=1
               END IF
               WRITE(6,*) 'MCM SPOT SIZE:',MAXIN
            END IF

         ELSE IF (OPT(I).EQ.'T'.OR.OPT(I).EQ.'t') THEN
            IF (IBOPT(5).EQ.0) THEN
               NBOPT=NBOPT+1
               IBOPT(5)=1
               WRITE(6,*) 'TOLERANT BEAMFORMING'
            END IF

         ELSE IF (OPT(I).EQ.'K'.OR.OPT(I).EQ.'k') THEN
            IF (.NOT.BLURR) THEN
               BLURR=.TRUE.
               WRITE(6,*) 'STOCHASTIC SIGNAL BLURRING'
            END IF

         ELSE IF (OPT(I).EQ.'C'.OR.OPT(I).EQ.'c') THEN
            IF (.NOT.CRARAO) THEN
               CRARAO=.TRUE.
               WRITE(6,*) 'CRAMER-RAO BOUNDS'
            END IF

         ELSE IF (OPT(I).EQ.'G'.OR.OPT(I).EQ.'c') THEN
            IF (.NOT.GAMPLT) THEN
               GAMPLT=.TRUE.
               WRITE(6,*) 'SIMILARITY FUNCTION GAMMA'
            END IF

         ELSE IF (OPT(I).EQ.'W'.OR.OPT(I).EQ.'w') THEN
            IF (NPWBF) THEN
               NPWBF=.FALSE.
               RINFLG=.FALSE.
               WRITE(6,*) 'CONVENTIONAL PLANE WAVE BEAMFORMING'
            END IF

         ELSE IF ( (OPT(I).EQ.'H')  .or.                            
     -             (OPT(I).EQ.'h') )      THEN                      
            GETFLG = .TRUE.                                         
            WRITE (6,*) 'READ FIELD XSM FROM EXTERNAL FILE'         

         ELSE IF ( (OPT(I).EQ.'O')  .or.                            
     -             (OPT(I).EQ.'o') )      THEN                      
            PUTFLG = .TRUE.                                         
            WRITE (6,*) 'WRITE SIM FIELD XSM TO EXTERNAL FILE'      

         ELSE IF ( (OPT(I).EQ.'E')  .or.                            
     -             (OPT(I).EQ.'e') )      THEN                      
            RINFLG = .TRUE.                                         
            WRITE (6,*) 'READ REPLICA FIELD FROM EXTERNAL FILE'     

         ELSE IF ( (OPT(I).EQ.'S')  .or.                            
     -             (OPT(I).EQ.'s') )      THEN                      
            ROTFLG = .TRUE.                                         
            WRITE (6,*) 'WRITE REPLICA FIELD TO EXTERNAL FILE'      

         END IF	

50    CONTINUE
C
      IF ((.NOT.NPWBF).AND.CRARAO) 
     1   STOP '*** NO CRAMER-RAO FOR PLANE WAVE BEAMFORMING ***'

      IF (NPWBF) WRITE(6,*) 'NON PLANE WAVE BEAMFORMING'

      IF (CRARAO.AND.(IPARES.LT.1.OR.IBOPT(2).LT.1)) THEN
        STOP '*** CRAMER-RAO BOUNDS ONLY FOR MAX.LIK. ***'
      END IF

      IF (MFAC.EQ.0) THEN
          MFAC=1
          WRITE(6,*) 'SOURCE DIRECTIONALITY, M=',MFAC
      END IF

      IF (NBOPT.LT.1) THEN
          NBOPT=NBOPT+1
          IBOPT(1)=1
          WRITE(6,*) 'BARTLETT ESTIMATE'
      END IF

      IF (PUTFLG) THEN                                              
            IF (IBOPT(1).EQ.0) THEN                                 
               NBOPT=NBOPT+1                                        
               IBOPT(1)=1                                           
               WRITE(6,*) 'BARTLETT ESTIMATE'                       
            END IF                                                  
      END IF                                                        

      IF (GETFLG) THEN                                              
         IF (GAMPLT) THEN                                           
            GAMPLT = .FALSE.                                        
            WRITE (6,*)                                             
            WRITE (6,*) '>>>>> XSM read from External File :'       
            WRITE (6,*) '      GAMMA PLOT OPTION SWITCHED OFF'      
            WRITE (6,*)                                             
         END IF                                                     
      END IF                                                        
     
      IF (RINFLG.AND.CRARAO) THEN
         CRARAO=.FALSE.
         WRITE(6,*)
         WRITE(6,*) '>>>>> Replica read from external file:'
         write(6,*) '      Cramer-Rao option switched off'
         WRITE(6,*)
      END IF 

      IF (NOUT.EQ.0) THEN
         IOUT(1)=1                   
         NOUT=1                      
      END IF

      RETURN                      
      END
