      SUBROUTINE CALSIC(cran,clp,wnlp,dlwran,nwvnor)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      PARAMETER (SQ2PI=2.506628,ONEO2PI=1.591549E-1)
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
      COMPLEX FACSQ
      COMPLEX DBDZ(4,np),BLC(4,np),CETA(np)
      COMPLEX BF1(4),BF2(4),CF1
      COMPLEX CRAN(1),clp(1),ccran,CC1,CC2,CC3,CC4,cintpl,ccsm
      logical nfmean
      onodlw=1e0/dlwran
      READ(45) FF1,NNN,nkmean,nfmean,dlmean
      if (abs(ff1-freq).gt.1e-3*freq) then
       write(6,*) '>>> ERROR: Frequency mismatch in rhs file <<<'
       stop
      end if

      do 1 nnk=1,nkmean
        READ(45) CF1,INTFCE,(BF1(JJ),JJ=1,4),(BF2(JJ),JJ=1,4)
        CETA(nnk)=CF1
        DO 2 JJ=1,4
         DBDZ(JJ,nnk)=BF1(JJ)
         BLC(JJ,nnk)=BF2(JJ)
 2      CONTINUE
 1    continue
c >>> re-scattering enabled 940602
      if (.not.rescat) then
       DO 3 II=1,NUML
 3     ROUGH2(II)=0
      end if
      NS=ICUT2-ICUT1+1
C *** OPEN SCRATCH FILE FOR KERNELS
      IF (DECOMP) THEN
       NGFILS=5
       IF (ISIZE.LT.IR*5*npar) STOP '>>> PARAMETER ISIZE TOO SMALL <<<'
      ELSE
       NGFILS=1
       IF (ISIZE.LT.IR*npar) STOP '>>> PARAMETER ISIZE TOO SMALL <<<'
      END IF
      LUOFF=LUGRN-1
c *** determine buffer size
      NBLOCKS=MEMLFT()/NGFILS-1 
      DO 5 IFC=1,NGFILS
       CALL OPNBUF(LUOFF+IFC,2*IR,NS*NOUT,NBLOCKS)
 5    CONTINUE
C *** WAVENUMBER RAMP
      CALL VRAMP(WK0,DLWVNO,FAC,1,NWVNO)
      NGVALS=2*IR*NPAR*NGFILS
C *** WAVENUMBER LOOP
c      write(47,*) numfr
c      write(47,*) real(dsq),rimag(dsq)
c      write(47,*) wk0,offima,dlwvno
c      write(47,*) icut2-icut1+1
      p_conv=1e0/real(pcorr)
      DO 20 II=ICUT1,ICUT2         
      WVNO=CMPLX(FAC(II),OFFIMA)
c >>> One set of sources
      nsrow=1
      CALL INITS
      CALL BUILD                  
C *** FILL IN SCATTERING RIGHT HAND SIDE
      CC1=CNUL
      CC2=CNUL
      CC3=CNUL
      CC4=CNUL
c >>> Cylindrical compensation
      fcq=max(dlwvno,abs(real(wvno)))
C >>> In monostatic cylindrical geometry only wavenumber sum contributes

      do 19 nnk=1,nkmean
       iphS=nint((real(wvno)+real(ceta(nnk)))*onodlw)
       IPH=IABS(IPHS)+1
       ISI=ISIGN(1,IPHS)
       DELW=REAL(CETA(NNK)) + REAL(WVNO)
c       CCRAN=CMPLX(REAL(CRAN(IPH)),RIMAG(CRAN(IPH))*ISI)
c >>> Interpolate spectrum. HS 980313
c >>> V 2.2 Tables stored in magnitude and phase
       ccran=cintpl(cran,0e0,onodlw,nwvnor,abs(delw))       
c       ccran=real(ccran)*cmplx(cos(rimag(ccran)),isi*sin(rimag(ccran)))
       ccsm=cintpl(clp,0e0,onodlw,nwvnor,abs(delw))       
c       ccsm=real(ccsm)*cmplx(cos(rimag(ccsm)),isi*sin(rimag(ccsm)))

c >>> Cylindrical compensation
       fck=max(dlwvno,abs(real(ceta(nnk))))
       ccran=ccran/sqrt(fck*fcq)
c >>> For small q and k use LePage transform weighted
       adlw=abs(delw)
       if (adlw.lt.wnlp) then
        ccran=( ccran*adlw + (wnlp-adlw)* ccsm)/wnlp        
       end if
c >>> Thats the only difference from calsip

       if (debug) then
        if (mod(ii-icut1,50).eq.0) then
         if (mod(nnk-1,50).eq.0) then
          write(6,*)'iq,ik,ip,ccran=',ii,nnk,iph,ccran
         end if
        end if
       end if
       CC1=CC1+
     &   CCRAN*(DBDZ(1,NNK)-AI*(WVNO+CETA(NNK))*BLC(1,NNK))
       CC2=CC2-
     &   CCRAN*(DBDZ(2,NNK)-AI*(WVNO+CETA(NNK))*BLC(2,NNK))
       CC3=CC3+
     &   CCRAN*(DBDZ(3,NNK)-AI*(WVNO+CETA(NNK))*BLC(3,NNK))
       CC4=CC4-
     &   CCRAN*(DBDZ(4,NNK)-AI*(WVNO+CETA(NNK))*BLC(4,NNK))
 19    CONTINUE

C *** NORMALIZE DISPLACEMENT RHS
      R(INTFCE-1,1,1)=R(INTFCE-1,1,1)+DISNRM*CC1*dlmean*oneo2pi
      R(INTFCE-1,2,1)=R(INTFCE-1,2,1)+DISNRM*CC2*dlmean*oneo2pi
      R(INTFCE-1,3,1)=R(INTFCE-1,3,1)+pcorr*strNRM*CC3*dlmean*oneo2pi
      R(INTFCE-1,4,1)=R(INTFCE-1,4,1)+pcorr*strNRM*CC4*dlmean*oneo2pi
      CALL SOLVE    
c >>> Compute wavefield potentials
        call wfield()
        write(48,*) p_conv*real(ss(1,3,1)),p_conv*rimag(ss(1,3,1))
c      IF (DEBUG) write(6,*) (ss(lay(1),jj),jj=1,4)
      IF (IERR.GT.0) RETURN
C      IF (DEBUG) WRITE(6,*) 'CALLING KERNEL'
      IF (DECOMP) THEN
       CALL KERDEC(CFILEK)
      ELSE
       CALL KERNEL(CFILEK)
      END IF
c *** tapering
      IF (II.LT.ICW1) THEN
       TFAC=0.5*(1E0+COS((II-ICW1)*PI/(ICUT1-ICW1-1)))
       CALL VSMUL(CFILEK,1,TFAC,CFILEK,1,NGVALS)
      ELSE IF (II.GT.ICW2) THEN
       TFAC=0.5*(1E0+COS((II-ICW2)*PI/(ICUT2-ICW2+1)))
       CALL VSMUL(CFILEK,1,TFAC,CFILEK,1,NGVALS)
      END IF
c >>> write to tmp files
      DO 10 IFC=1,NGFILS
       do 10 ipp=1,npar
       IF (IOUT(ipp).GT.0) THEN
        INDXCF=1+IR*(ipp-1+NPAR*(IFC-1))
        CALL WRBUF(LUOFF+IFC,CFILEK(INDXCF),2*IR)
       END IF
 10   CONTINUE
 20   CONTINUE     
      DO 30 IFC=1,NGFILS
      CALL ENFBUF(LUOFF+IFC)
 30   CONTINUE
C                   
      RETURN        
C                   
      END           

      SUBROUTINE CALSIP(cran,dlwran,nwvnor)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      PARAMETER (SQ2PI=2.506628,ONEO2PI=1.591549E-1)
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
      COMPLEX FACSQ
      COMPLEX DBDZ(4,np),BLC(4,np),CETA(np)
      COMPLEX BF1(4),BF2(4),CF1
      COMPLEX CRAN(1),ccran,CC1,CC2,CC3,CC4,cintpl
      logical nfmean
      onodlw=1e0/dlwran
      READ(45) FF1,NNN,nkmean,nfmean,dlmean
      if (abs(ff1-freq).gt.1e-3*freq) then
       write(6,*) '>>> ERROR: Frequency mismatch in rhs file <<<'
       stop
      end if
c
      write(*,*) 'nfmean=',nfmean
      if (nfmean) write(6,*) '>>> Mean negative spectrum included <<<'
      do 1 nnk=1,nkmean
        READ(45) CF1,INTFCE,(BF1(JJ),JJ=1,4),(BF2(JJ),JJ=1,4)
        CETA(nnk)=CF1
        DO 2 JJ=1,4
         DBDZ(JJ,nnk)=BF1(JJ)
         BLC(JJ,nnk)=BF2(JJ)
 2      CONTINUE
 1    continue
c >>> re-scattering enabled 940602
      if (.not.rescat) then
       DO 3 II=1,NUML
 3     ROUGH2(II)=0
      end if
      NS=ICUT2-ICUT1+1
C *** OPEN SCRATCH FILE FOR KERNELS
      IF (DECOMP) THEN
       NGFILS=5
       IF (ISIZE.LT.IR*5*npar) STOP '>>> PARAMETER ISIZE TOO SMALL <<<'
      ELSE
       NGFILS=1
       IF (ISIZE.LT.IR*npar) STOP '>>> PARAMETER ISIZE TOO SMALL <<<'
      END IF
      LUOFF=LUGRN-1
c *** determine buffer size
      NBLOCKS=MEMLFT()/NGFILS-1 
      DO 5 IFC=1,NGFILS
       CALL OPNBUF(LUOFF+IFC,2*IR,NS*NOUT,NBLOCKS)
 5    CONTINUE
C *** WAVENUMBER RAMP
      CALL VRAMP(WK0,DLWVNO,FAC,1,NWVNO)
      NGVALS=2*IR*NPAR*NGFILS
C *** WAVENUMBER LOOP
c      write(47,*) numfr
c      write(47,*) real(dsq),rimag(dsq)
c      write(47,*) wk0,offima,dlwvno
c      write(47,*) icut2-icut1+1
      p_conv=1e0/real(pcorr)
      DO 20 II=ICUT1,ICUT2         
      WVNO=CMPLX(FAC(II),OFFIMA)
c >>> One set of sources
      nsrow=1
      CALL INITS
      CALL BUILD                 
C *** FILL IN SCATTERING RIGHT HAND SIDE
      CC1=CNUL
      CC2=CNUL
      CC3=CNUL
      CC4=CNUL
      do 18 nnk=1,nkmean
       iphs=nint((real(wvno)-real(ceta(nnk)))*onodlw)
       IPH=IABS(IPHS)+1
       ISI=ISIGN(1,IPHS)
       DELW=REAL(CETA(NNK)) - REAL(WVNO)
c       CCRAN=CMPLX(REAL(CRAN(IPH)),RIMAG(CRAN(IPH))*ISI)
c >>> Interpolate spectrum. HS 980313
c >>> V 2.2 Tables stored in magnitude and phase
       ccran=cintpl(cran,0e0,onodlw,nwvnor,abs(delw))       
c       ccran=real(ccran)*cmplx(cos(rimag(ccran)),isi*sin(rimag(ccran)))

       if (debug) then
        if (mod(ii-icut1,50).eq.0) then
         if (mod(nnk-1,50).eq.0) then
          write(6,*)'iq,ik,ip,ccran=',ii,nnk,iph,ccran
         end if
        end if
       end if
       CC1=CC1+
     &   CCRAN*(DBDZ(1,NNK)-AI*(WVNO-CETA(NNK))*BLC(1,NNK))
       CC2=CC2+
     &   CCRAN*(DBDZ(2,NNK)-AI*(WVNO-CETA(NNK))*BLC(2,NNK))
       CC3=CC3+
     &   CCRAN*(DBDZ(3,NNK)-AI*(WVNO-CETA(NNK))*BLC(3,NNK))
       CC4=CC4+
     &   CCRAN*(DBDZ(4,NNK)-AI*(WVNO-CETA(NNK))*BLC(4,NNK))
 18   CONTINUE
C >>> NEGATIVE SPECTRUM OF MEAN FIELD
      IF (NFMEAN) THEN
      do 19 nnk=1,nkmean
       iphS=nint((real(wvno)+real(ceta(nnk)))*onodlw)
       IPH=IABS(IPHS)+1
       ISI=ISIGN(1,IPHS)
       DELW=REAL(CETA(NNK)) + REAL(WVNO)
c       CCRAN=CMPLX(REAL(CRAN(IPH)),RIMAG(CRAN(IPH))*ISI)
c >>> Interpolate spectrum. HS 980313
c >>> V 2.2 Tables stored in magnitude and phase
       ccran=cintpl(cran,0e0,onodlw,nwvnor,abs(delw))       
c       ccran=real(ccran)*cmplx(cos(rimag(ccran)),isi*sin(rimag(ccran)))

       if (debug) then
        if (mod(ii-icut1,50).eq.0) then
         if (mod(nnk-1,50).eq.0) then
          write(6,*)'iq,ik,ip,ccran=',ii,nnk,iph,ccran
         end if
        end if
       end if
       CC1=CC1+
     &   CCRAN*(DBDZ(1,NNK)-AI*(WVNO+CETA(NNK))*BLC(1,NNK))
       CC2=CC2-
     &   CCRAN*(DBDZ(2,NNK)-AI*(WVNO+CETA(NNK))*BLC(2,NNK))
       CC3=CC3+
     &   CCRAN*(DBDZ(3,NNK)-AI*(WVNO+CETA(NNK))*BLC(3,NNK))
       CC4=CC4-
     &   CCRAN*(DBDZ(4,NNK)-AI*(WVNO+CETA(NNK))*BLC(4,NNK))
 19    CONTINUE
      END IF
C *** NORMALIZE DISPLACEMENT RHS
      R(INTFCE-1,1,1)=R(INTFCE-1,1,1)+DISNRM*CC1*dlmean*oneo2pi
      R(INTFCE-1,2,1)=R(INTFCE-1,2,1)+DISNRM*CC2*dlmean*oneo2pi
      R(INTFCE-1,3,1)=R(INTFCE-1,3,1)+pcorr*strNRM*CC3*dlmean*oneo2pi
      R(INTFCE-1,4,1)=R(INTFCE-1,4,1)+pcorr*strNRM*CC4*dlmean*oneo2pi
      CALL SOLVE    
c >>> Compute wavefield potentials
        call wfield()
        write(48,*) p_conv*real(ss(1,3,1)),p_conv*rimag(ss(1,3,1))
c      IF (DEBUG) write(6,*) (ss(lay(1),jj),jj=1,4)
      IF (IERR.GT.0) RETURN
C      IF (DEBUG) WRITE(6,*) 'CALLING KERNEL'
      IF (DECOMP) THEN
       CALL KERDEC(CFILEK)
      ELSE
       CALL KERNEL(CFILEK)
      END IF
c *** tapering
      IF (II.LT.ICW1) THEN
       TFAC=0.5*(1E0+COS((II-ICW1)*PI/(ICUT1-ICW1-1)))
       CALL VSMUL(CFILEK,1,TFAC,CFILEK,1,NGVALS)
      ELSE IF (II.GT.ICW2) THEN
       TFAC=0.5*(1E0+COS((II-ICW2)*PI/(ICUT2-ICW2+1)))
       CALL VSMUL(CFILEK,1,TFAC,CFILEK,1,NGVALS)
      END IF
c >>> write to tmp files
      DO 10 IFC=1,NGFILS
       do 10 ipp=1,npar
       IF (IOUT(ipp).GT.0) THEN
        INDXCF=1+IR*(ipp-1+NPAR*(IFC-1))
        CALL WRBUF(LUOFF+IFC,CFILEK(INDXCF),2*IR)
       END IF
 10   CONTINUE
 20   CONTINUE     
      DO 30 IFC=1,NGFILS
      CALL ENFBUF(LUOFF+IFC)
 30   CONTINUE
C                   
      RETURN        
C                   
      END           

      SUBROUTINE CALSIN(ETAIN,INTFCE)    
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      PARAMETER (SQ2PI=2.506628,ONEO2PI=1.591549E-1)
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
      COMPLEX FACSQ
      COMPLEX DBDZ(4),BLC(4),CETA
      COMPLEX BF1(4),BF2(4),CF1
C
C *** SEARCH FOR RIGHT HAND SIDE WITH CLOSEST HOR., WAVE NO,
C
      DFOLD = 1E35
 1    READ(45,END=15) CF1,ININ,(BF1(JJ),JJ=1,4),(BF2(JJ),JJ=1,4)
      DFNEW = ABS(REAL(CF1)-ETAIN)
      IF (ININ.EQ.INTFCE.AND.DFNEW.LT.DFOLD) THEN
        CETA=CF1
        DO 2 JJ=1,4
         DBDZ(JJ)=BF1(JJ)
         BLC(JJ)=BF2(JJ)
 2      CONTINUE
      END IF
      DFOLD = DFNEW
      GO TO 1
 15   write(6,*) '*** eof reading file 45'
      WRITE(6,*) '*** PHASE VELOCITY:',DSQ/REAL(CETA)
      RG2=ROUGH2(INTFCE)
c >>> enable roughness 940602
      if (.not.rescat) then
       DO 3 II=1,NUML
 3     ROUGH2(II)=0
      end if
      NS=ICUT2-ICUT1+1
      nwstep=1
C *** OPEN SCRATCH FILE FOR KERNELS
      IF (DECOMP) THEN
       NGFILS=5
       IF (ISIZE.LT.IR*15) STOP '>>> PARAMETER ISIZE TOO SMALL <<<'
      ELSE
       NGFILS=1
       IF (ISIZE.LT.IR*3) STOP '>>> PARAMETER ISIZE TOO SMALL <<<'
      END IF
      LUOFF=LUGRN-1
c *** determine buffer size
      NBLOCKS=MEMLFT()/NGFILS-1 
      DO 5 IFC=1,NGFILS
       CALL OPNBUF(LUOFF+IFC,2*IR,NS*NOUT,NBLOCKS)
 5    CONTINUE
C *** WAVENUMBER RAMP
      CALL VRAMP(WK0,DLWVNO,FAC,1,NWVNO)
C *** WAVENUMBER LOOP
      DO 20 II=ICUT1,ICUT2         
      WVNO=CMPLX(FAC(II),OFFIMA)
      IF (DEBUG) then
       write(6,*) '>>>>>'
       write(6,*) ii,fac(ii)
       write(6,*) rg2,clen(intfce),
     &   p(real(wvno-ceta),intfce),fccs
      end if
c >>> One set of sources
      nsrow=1
      CALL INITS
      CALL BUILD                  
C *** FILL IN SCATTERING RIGHT HAND SIDE
      FCCS=SQRT(oneo2pi*RG2*P(REAL(WVNO-CETA),INTFCE))
      call vclr(R,1,4*NLA*2)
      DO 6 IPA=1,4
      R(INTFCE-1,IPA,1)=FCCS*(DBDZ(IPA)-AI*(WVNO-CETA)*BLC(IPA))
      IF (DEBUG) write(6,*) intfce,ipa,r(intfce-1,ipa,1)
 6    CONTINUE
C *** NORMALIZE DISPLACEMENT RHS
      R(INTFCE-1,1,1)=DISNRM*R(INTFCE-1,1,1)
      R(INTFCE-1,2,1)=DISNRM*R(INTFCE-1,2,1)
      R(INTFCE-1,3,1)=pcorr*strNRM*R(INTFCE-1,3,1)
      R(INTFCE-1,4,1)=pcorr*strNRM*R(INTFCE-1,4,1)
      CALL SOLVE    
      IF (DEBUG) write(6,*) (ss(1,jj,1),jj=1,4)
      IF (IERR.GT.0) RETURN
C      IF (DEBUG) WRITE(6,*) 'CALLING KERNEL'
c >>> Compute wavefield potentials
       call wfield()
      IF (DECOMP) THEN
       CALL KERDEC(CFILEK)
      ELSE
       CALL KERNEL(CFILEK)
      END IF
c >>> write to tmp files
      DO 10 IFC=1,NGFILS
       do 10 ipp=1,npar
       IF (IOUT(ipp).GT.0) THEN
        INDXCF=1+IR*(ipp-1+NPAR*(IFC-1))
        CALL WRBUF(LUOFF+IFC,CFILEK(INDXCF),2*IR)
        if (debug) then
         write(6,*) 'cfile=',cfilek(indxcf)
        end if
       END IF
 10   CONTINUE
 20   CONTINUE     
      DO 30 IFC=1,NGFILS
      CALL ENFBUF(LUOFF+IFC)
 30   CONTINUE
C                   
      RETURN        
C                   
      END           


      SUBROUTINE REVRAN(INTFCE,ETA0,KCNT,NREC,NR1,DR2,NR2)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
c
c >>> Computes the normalized correlation of the reverberant
c     field on a horizontal array of nr2 elements, placed at
c     nr1 ranges from the source.
c
      PARAMETER (SQ2PI=2.506628,ONEO2PI=1.591549E-1)
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
      parameter (nrhsbf=(npar-3)*np)
      COMPLEX FACSQ,cskern,crexpi,cc
      COMPLEX DBDZ(4),BLC(4),CETA
      COMMON /SCFRHS/ DBDZ,BLC,RG2,NKP,NKPTOT,NKSTEP,LREC
      DIMENSION FFS(NP2),ranexp(np,2)
      dimension covar(np),conorm(np)
      COMPLEX RHSBUF(Nrhsbf)
      EQUIVALENCE (RHSBUF(1),CFF(1,4))
      equivalence (covar(1),cff(1,1))
      equivalence (conorm(1),cffs(1))
      EQUIVALENCE (CFFS(1),FFS(1))
      EQUIVALENCE (IKPF,FFS(1))
      EQUIVALENCE (NZK,FFS(2))
      equivalence (ranexp(1,1),cff(1,3))
C *** SAVE ACTUAL ROUGHNESS AND DISABLE SCATTERING
      RG2=ROUGH2(INTFCE)
c >>> enable roughness 940602
      if (.not.rescat) then
       DO 3 II=1,NUML
 3     ROUGH2(II)=0
      end if
C
C *** MAKE ARRAY WITH SURFACE SPECTRUM
C
      NR12=NR1*NR2
      if (nr12.gt.np) stop '>>> NR1*NR2 too large <<<'
c >>> Clear covariance array
      call vclr(cff(1,1),1,2*nr12)
c >>> Integration constant
      FF=RG2*(DLWVNO)*(FNI5**2)/((2*PI)**3)
C >>> create range table and exponentials
      do inr=1,nr1
       do jnr=1,nr2
        index=inr+(jnr-1)*nr1
        arg(index)=r1+(inr-1)*dlran+(jnr-1)*dr2
        ranexp(index,1)=exp(arg(index)*offima)
        ranexp(index,2)=exp(-arg(index)*offima)
       end do
      end do
C *** WHICH PARAMETER IS BEING CALCULATED ?
      DO 2 I=1,npar
       IF (IOUT(I).NE.0) IPA=I
 2    CONTINUE
      NQ=ICUT2-ICUT1+1
      IF (ISIZE.LT.IR*npar) STOP '>>> PARAMETER ISIZE TOO SMALL <<<'
      ONEODK=1E0/DLWVNO
C *** DETERMINE WIDTH OF K' WINDOW
      if (debug) write(6,*) 'imx,dlwnk:',IMX(INTFCE),DLWNK(INTFCE)
      NKP=NINT((IMX(INTFCE)*DLWNK(INTFCE))*ONEODK)
      NKPTOT=NKP*2+1
      WRITE(6,*) 'NUMBER OF Q-K VALUES:',NKPTOT
C     DETERMINE NUMBER OF K VALUES TO BE TREATED SIMULTANEOUSLY
      NKSTEP=MIN(nrhsbf/NEQ,NKPTOT)
c >>> Number of simultaneous p-s
      npsim=min(np/nr12,NKSTEP)
      nkstep=npsim 
      write(6,*) 'NUMBER TREATED SIMULTANEOUSLY:',nkstep
c >>> p-loop
      DO 50 IP=-NKP,NKP,npsim
       write(6,*) 'ip=',ip
       call rdtime(tt1)
       ipmax=min(nkp,ip+npsim-1)
c >>> Clear integration array
       call vclr(cff(1,2),1,2*nr12*npsim)
c >>> q-loop
       do 40 iq=icut1,icut2
        Q=WK0+(IQ-1)*DLWVNO
        IF (DEBUG) WRITE(6,*) 'IQ=',IQ
        WVNO=CMPLX(q,sign(OFFIMA,q))
        IKMIN=NINT((ETA0-q)*ONEODK)
        IKMAX=IKMIN+KCNT-1
        IKPF=MAX(ip,IKMIN)
        IKPL=MIN(ipmax,IKMAX)
        NZK=MAX(0,IKPL-IKPF+1)
        IF (NZK.GT.0) THEN
         IF (DEBUG) WRITE(6,*) 'IKPF,IKPL,NZK=',IKPF,IKPL,NZK
         CALL RWDBUF(25)
c >>> One set of sources
         nsrow=1
         CALL INITS
         CALL BUILD      
         do ik=ikmin,min(IKPF-1,ikmax)
          call rdbuf(25,dbdz,16)
         end do            
c >>> exponentials        
         do irc=1,nr12
          fac(irc)=-q*arg(irc)
         end do
         call cvexpi(fac,1,cbuf,2,nr12)

c >>> compute scattering kernel 

C *** FILL IN SCATTERING RIGHT HAND SIDE
C *** K-WAVENUMBER LOOP
         DO 14 JPA=1,4
          CALL VCLR(R(1,JPA,1),1,2*NUML)
 14      CONTINUE
         call vclr(rhsbuf,1,2*neq*nzk)
         DO 16 IK=IKPF,IKPL
          CETA=WVNO+IK*DLWVNO
          CALL RDBUF(25,DBDZ,16)
          DO 15 JPA=1,4
           R(INTFCE-1,JPA,1)=(DBDZ(JPA)-AI*(WVNO-CETA)*BLC(JPA))
 15       CONTINUE
c          if (mod(iq,100).eq.0.and.ik.eq.0) then
c           write(82,*) 'intfce,iq:',intfce,iq
c           write(82,*) 'r:',(r(intfce-1,jpa,1),jpa=1,4)
c          end if
C *** NORMALIZE DISPLACEMENT RHS
          R(INTFCE-1,1,1)=DISNRM*R(INTFCE-1,1,1)
          R(INTFCE-1,2,1)=DISNRM*R(INTFCE-1,2,1)
          R(INTFCE-1,3,1)=pcorr*strNRM*R(INTFCE-1,3,1)
          R(INTFCE-1,4,1)=pcorr*strNRM*R(INTFCE-1,4,1)
          CALL CVIMOV(R,INDR,1,RHSBUF(1+(IK-IKPF)*NEQ),2,NEQ)
 16      CONTINUE
C *** THE FOLLOWING REPLACES THE CALL TO SOLVE
         CALL CVIMOV(ALO,INDA,1,WORK1,2,NNA)
         CALL CVFILL(CNUL,WORK2,2,NNB)
         CALL CVMOVI(WORK1,2,INDB,1,WORK2,NNA)
         EPS=1E-10
         CALL CBGEMR(WORK2,RHSBUF,NEQ,NEQ,NZK,IBW,EPS)
         IERR=EPS
         IF (IERR.NE.0) RETURN
c >>> add contribution to q-integral including symmetric part in 
c     k and q
         ise1=nint(1.5-sign(0.5,q))
         ise2=3-ise1
         DO 17 IK=IKPF,IKPL
          CALL CVMOVI(RHSBUF(1+(IK-IKPF)*NEQ),2,INDS,1,SS,NEQ)
c >>> Compute wavefield potentials
          call wfield()
          CALL KERNEL(CFILEK)
          INDXCF=nrec+(IPA-1)*IR
          iof=(ik-ip)*nr12
          do irc=1,nr12
           cff(irc+iof,2)=cff(irc+iof,2)
     &           +ranexp(irc,ise1)*cfilek(indxcf)*cbuf(irc)
     &           +ranexp(irc,ise2)*cfilek(indxcf)*conjg(cbuf(irc))
          end do
 17      CONTINUE
        end if
c >>> q-integral done
 40    continue
c >>> update p-integral
        do ipp=ip,ipmax
         iof=(ipp-ip)*nr12
         facin=FF*P(-IPP*DLWVNO,INTFCE)
         do inr=1,nr1
          do jnr=1,nr2
           index=inr+(jnr-1)*nr1
           cff(index,1)= cff(index,1)
     &        +facin*cff(inr+iof,2)*conjg(cff(index+iof,2))
          end do
         end do
        end do
       call rdtime(tt2)
       write(6,*) '>>> Done, CPU=',tt2-tt1,' secs <<<'
 50   continue

C *** CONVERT TO dB
      if (debug) WRITE(6,*) 'dB CONVERSION'
      CALL CVMAGS(CFF(1,1),2,CFF(1,2),1,NR12)
      CALL VCLIP(CFF(1,2),1,1E-30,1E30,CFF(1,2),1,NR12)
      CALL VALG10(CFF(1,2),1,CFF(1,1),1,NR12)
      CALL VSMUL(CFF(1,1),1,-5E0,CFF(1,1),1,NR12)
c >>> Normalized correllation for contour plots
         do inr=1,nr1
          do jnr=1,nr2
           index=inr+(jnr-1)*nr1
           conorm(index)= covar(index)-covar(inr)
          end do
         end do
      IF (DEBUG) WRITE(6,*) 'END REVRAN'
      RETURN
      END           
      SUBROUTINE REVINT(INTFCE,ETA0,KCNT,NR1)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
c
c >>> Computes the intensity of the reverberant field vs
c     range and depth in the waveguide
c
      PARAMETER (SQ2PI=2.506628,ONEO2PI=1.591549E-1)
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
      parameter (nrhsbf=(npar-3)*np)
      COMPLEX FACSQ,cskern,crexpi,cc
      COMPLEX DBDZ(4),BLC(4),CETA
      COMMON /SCFRHS/ DBDZ,BLC,RG2,NKP,NKPTOT,NKSTEP,LREC
      DIMENSION FFS(NP2),ranexp(np,2)
      dimension covar(np),conorm(np)
      COMPLEX RHSBUF(Nrhsbf)
      EQUIVALENCE (RHSBUF(1),CFF(1,4))
      equivalence (covar(1),cff(1,1))
      equivalence (conorm(1),cffs(1))
      EQUIVALENCE (CFFS(1),FFS(1))
      EQUIVALENCE (IKPF,FFS(1))
      EQUIVALENCE (NZK,FFS(2))
      equivalence (ranexp(1,1),cff(1,3))
C *** SAVE ACTUAL ROUGHNESS AND DISABLE SCATTERING
      RG2=ROUGH2(INTFCE)
c >>> enable roughness 940602
      if (.not.rescat) then
       DO 3 II=1,NUML
  3    ROUGH2(II)=0
      end if
C
C *** MAKE ARRAY WITH SURFACE SPECTRUM
C
      NR12=NR1*ir
      if (nr12.gt.np) stop '>>> REVINT: NR1*IR too large <<<'
c >>> Clear covariance array
      call vclr(cff(1,1),1,2*nr12)
c >>> Integration constant
      FF=RG2*(DLWVNO)*(FNI5**2)/((2*PI)**3)
C >>> create range table and exponentials
      do inr=1,nr1
        index=inr
        arg(index)=r1+(inr-1)*dlran
        ranexp(index,1)=exp(arg(index)*offima)
        ranexp(index,2)=exp(-arg(index)*offima)
        if (icdr.eq.0) then
         if (abs(arg(index)).lt.1e-3) then
          ranexp(index,1)=2*pi*dlwvno*ranexp(index,1)/fni5
          ranexp(index,2)=2*pi*dlwvno*ranexp(index,2)/fni5
         else
          ranexp(index,1)=2*pi*ranexp(index,1)/sqrt(arg(index))
          ranexp(index,2)=2*pi*ranexp(index,2)/sqrt(arg(index))
         end if
        end if           
      end do
C *** WHICH PARAMETER IS BEING CALCULATED ?
      DO 2 I=1,npar
       IF (IOUT(I).NE.0) IPA=I
 2    CONTINUE
      NQ=ICUT2-ICUT1+1
      IF (ISIZE.LT.IR*npar) STOP '>>> PARAMETER ISIZE TOO SMALL <<<'
      ONEODK=1E0/DLWVNO
C *** DETERMINE WIDTH OF K' WINDOW
      if (debug) write(6,*) 'imx,dlwnk:',IMX(INTFCE),DLWNK(INTFCE)
      NKP=NINT((IMX(INTFCE)*DLWNK(INTFCE))*ONEODK)
      NKPTOT=NKP*2+1
      WRITE(6,*) 'NUMBER OF Q-K VALUES:',NKPTOT
C     DETERMINE NUMBER OF K VALUES TO BE TREATED SIMULTANEOUSLY
      NKSTEP=MIN(nrhsbf/NEQ,NKPTOT)
c >>> Number of simultaneous p-s
      npsim=min(np/nr12,NKSTEP)
      nkstep=npsim 
      write(6,*) 'NUMBER TREATED SIMULTANEOUSLY:',nkstep
c >>> p-loop
      DO 50 IP=-NKP,NKP,npsim
       write(6,*) 'ip=',ip
       call rdtime(tt1)
       ipmax=min(nkp,ip+npsim-1)
c >>> Clear integration array
       call vclr(cff(1,2),1,2*nr12*npsim)
c >>> q-loop
       do 40 iq=icut1,icut2
        Q=WK0+(IQ-1)*DLWVNO
        IF (DEBUG) WRITE(6,*) 'IQ=',IQ
        WVNO=CMPLX(q,sign(OFFIMA,q))
        IKMIN=NINT((ETA0-q)*ONEODK)
        IKMAX=IKMIN+KCNT-1
        IKPF=MAX(ip,IKMIN)
        IKPL=MIN(ipmax,IKMAX)
        NZK=MAX(0,IKPL-IKPF+1)
        IF (NZK.GT.0) THEN
         IF (DEBUG) WRITE(6,*) 'IKPF,IKPL,NZK=',IKPF,IKPL,NZK
         CALL RWDBUF(25)
c >>> One set of sources
         nsrow=1
         CALL INITS
         CALL BUILD      
         do ik=ikmin,min(IKPF-1,ikmax)
          call rdbuf(25,dbdz,16)
         end do            
c >>> exponentials        
         do irc=1,nr1
          fac(irc)=-q*arg(irc)
         end do
         call cvexpi(fac,1,cbuf,2,nr1)
c >>> range dependent wavenumber factors
         if (icdr.eq.0) then
          sqrq=sqrt(abs(q))
          do irc=1,nr1
           if (abs(arg(irc)).lt.1e-3) then
             fac(irc)=0.5*abs(q)
           else
             fac(irc)=sqrq
           end if
          end do
         else
          do irc=1,nr1
           fac(irc)=1E0
          end do
         end if
c >>> compute scattering kernel 

C *** FILL IN SCATTERING RIGHT HAND SIDE
C *** K-WAVENUMBER LOOP
         DO 14 JPA=1,4
          CALL VCLR(R(1,JPA,1),1,2*NUML)
 14      CONTINUE
         call vclr(rhsbuf,1,2*neq*nzk)
         DO 16 IK=IKPF,IKPL
          CETA=WVNO+IK*DLWVNO
          CALL RDBUF(25,DBDZ,16)
          DO 15 JPA=1,4
           R(INTFCE-1,JPA,1)=(DBDZ(JPA)-AI*(WVNO-CETA)*BLC(JPA))
 15       CONTINUE
c          if (mod(iq,100).eq.0.and.ik.eq.0) then
c           write(82,*) 'intfce,iq:',intfce,iq
c           write(82,*) 'r:',(r(intfce-1,jpa,1),jpa=1,4)
c          end if
C *** NORMALIZE DISPLACEMENT RHS
          R(INTFCE-1,1,1)=DISNRM*R(INTFCE-1,1,1)
          R(INTFCE-1,2,1)=DISNRM*R(INTFCE-1,2,1)
          R(INTFCE-1,3,1)=pcorr*strNRM*R(INTFCE-1,3,1)
          R(INTFCE-1,4,1)=pcorr*strNRM*R(INTFCE-1,4,1)
          CALL CVIMOV(R,INDR,1,RHSBUF(1+(IK-IKPF)*NEQ),2,NEQ)
 16      CONTINUE
C *** THE FOLLOWING REPLACES THE CALL TO SOLVE
         CALL CVIMOV(ALO,INDA,1,WORK1,2,NNA)
         CALL CVFILL(CNUL,WORK2,2,NNB)
         CALL CVMOVI(WORK1,2,INDB,1,WORK2,NNA)
         EPS=1E-10
         CALL CBGEMR(WORK2,RHSBUF,NEQ,NEQ,NZK,IBW,EPS)
         IERR=EPS
         IF (IERR.NE.0) RETURN
c >>> add contribution to q-integral including symmetric part in 
c     k and q
         ise1=nint(1.5-sign(0.5,q))
         ise2=3-ise1
         DO 17 IK=IKPF,IKPL
          CALL CVMOVI(RHSBUF(1+(IK-IKPF)*NEQ),2,INDS,1,SS,NEQ)
c >>> Compute wavefield potentials
          call wfield()
          CALL KERNEL(CFILEK)
          iof=(ik-ip)*nr12
          do nrec=1,ir
           INDXCF=nrec+(IPA-1)*IR
           do irc=1,nr1
            iof2=irc+(nrec-1)*nr1+iof
            cff(iof2,2)=cff(iof2,2)+fac(irc)
     &        *( ranexp(irc,ise1)*cfilek(indxcf)*cbuf(irc)
     &                                                    )
c >>> 951018- HS: I haven'tgot the faintest idea why I had the following 
c     &          +ranexp(irc,ise2)*cfilek(indxcf)*conjg(cbuf(irc)))
           end do
          end do
 17      CONTINUE
        end if
c >>> q-integral done
 40    continue
c >>> update p-integral
        do ipp=ip,ipmax
         iof=(ipp-ip)*nr12
         facin=FF*P(-IPP*DLWVNO,INTFCE)
         do inr=1,nr1
          do nrec=1,ir
           index=inr+(nrec-1)*nr1
           cff(index,1)= cff(index,1)
     &        +facin*cff(index+iof,2)*conjg(cff(index+iof,2))
          end do
         end do
        end do
       call rdtime(tt2)
       write(6,*) '>>> Done, CPU=',tt2-tt1,' secs <<<'
 50   continue

C *** CONVERT TO dB
      if (debug) WRITE(6,*) 'dB CONVERSION'
      CALL CVMAGS(CFF(1,1),2,CFF(1,2),1,NR12)
      CALL VCLIP(CFF(1,2),1,1E-30,1E30,CFF(1,2),1,NR12)
      CALL VALG10(CFF(1,2),1,CFFs(1),1,NR12)
      CALL VSMUL(CFFs(1),1,-5E0,CFFs(1),1,NR12)
      IF (DEBUG) WRITE(6,*) 'END REVINT'
      RETURN
      END           

      SUBROUTINE REVCOV(INTFCE,ETA0,KCNT)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
c
c >>> Computes the intensity of the reverberant field vs
c     range and depth in the waveguide
c
      PARAMETER (SQ2PI=2.506628,ONEO2PI=1.591549E-1)
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
      INCLUDE 'corrfn.f'
      INCLUDE 'recarr.f'
      parameter (nocpd=2, nocpo=nocpd+1, nrhsbf=(npar-nocpd)*np)
      COMPLEX FACSQ,cskern,crexpi,cc
      COMPLEX DBDZ(4),BLC(4),CETA
      COMMON /SCFRHS/ DBDZ,BLC,RG2,NKP,NKPTOT,NKSTEP,LREC
      DIMENSION FFS(NP2),ranexp(np,2)
      dimension conorm(np)
      COMPLEX RHSBUF(Nrhsbf)
      equivalence (ranexp(1,1),cff(1,nocpd))
      EQUIVALENCE (RHSBUF(1),CFF(1,nocpo))
      equivalence (conorm(1),cffs(1))
      EQUIVALENCE (CFFS(1),FFS(1))
      EQUIVALENCE (IKPF,FFS(1))
      EQUIVALENCE (NZK,FFS(2))

C *** SAVE ACTUAL ROUGHNESS AND DISABLE SCATTERING
      RG2=ROUGH2(INTFCE)
c >>> enable roughness 940602
      if (.not.rescat) then
       DO 3 II=1,NUML
 3     ROUGH2(II)=0
      end if
C
C *** MAKE ARRAY WITH SURFACE SPECTRUM
C
      if (nrcv.gt.np) stop '>>> REVINT: NRCV too large <<<'
c >>> Clear covariance array
      call vclr(corrns,1,2*nrcv*nrcv)
c >>> Integration constant
      FF=RG2*(DLWVNO)*(FNI5**2)/((2*PI)**3)

C >>> create range table and exponentials
      do inr=1,nrcv
        index=inr
        arg(index)=sqrt(ran(inr)**2+tran(inr)**2)
        ranexp(index,1)=exp(arg(index)*offima)
        ranexp(index,2)=exp(-arg(index)*offima)
        if (icdr.eq.0) then
         if (abs(arg(index)).lt.1e-3) then
          ranexp(index,1)=2*pi*dlwvno*ranexp(index,1)/fni5
          ranexp(index,2)=2*pi*dlwvno*ranexp(index,2)/fni5
         else
          ranexp(index,1)=2*pi*ranexp(index,1)/sqrt(arg(index))
          ranexp(index,2)=2*pi*ranexp(index,2)/sqrt(arg(index))
         end if
        end if           
      end do

      NQ=ICUT2-ICUT1+1
      IF (ISIZE.LT.IR*npar) STOP '>>> PARAMETER ISIZE TOO SMALL <<<'
      ONEODK=1E0/DLWVNO
C *** DETERMINE WIDTH OF K' WINDOW
      if (debug) write(6,*) 'imx,dlwnk:',IMX(INTFCE),DLWNK(INTFCE)
      NKP=NINT((IMX(INTFCE)*DLWNK(INTFCE))*ONEODK)
      NKPTOT=NKP*2+1
      WRITE(6,*) 'NUMBER OF Q-K VALUES:',NKPTOT
C     DETERMINE NUMBER OF K VALUES TO BE TREATED SIMULTANEOUSLY
      NKSTEP=MIN(nrhsbf/NEQ,NKPTOT)
c >>> Number of simultaneous p-s
      npsim=min(np/nrcv,NKSTEP)
      nkstep=npsim 
      write(6,*) 'NUMBER TREATED SIMULTANEOUSLY:',nkstep
c >>> p-loop
      DO 50 IP=-NKP,NKP,npsim
       write(6,*) 'ip=',ip
       call rdtime(tt1)
       ipmax=min(nkp,ip+npsim-1)
c >>> Clear integration array
       call vclr(cff(1,1),1,2*nrcv*npsim)
       istpw=(icut2-icut1)/10 +1
c >>> q-loop
       do 40 iq=icut1,icut2
        if (mod(iq-icut1,istpw).eq.0) write(6,*) 'iq=',iq
        Q=WK0+(IQ-1)*DLWVNO
        IF (DEBUG) WRITE(6,*) 'IQ=',IQ
        WVNO=CMPLX(q,sign(OFFIMA,q))
        IKMIN=NINT((ETA0-q)*ONEODK)
        IKMAX=IKMIN+KCNT-1
        IKPF=MAX(ip,IKMIN)
        IKPL=MIN(ipmax,IKMAX)
        NZK=MAX(0,IKPL-IKPF+1)
        IF (NZK.GT.0) THEN
         IF (DEBUG) WRITE(6,*) 'IKPF,IKPL,NZK=',IKPF,IKPL,NZK
         CALL RWDBUF(25)
c >>> One set of sources
         nsrow=1
         CALL INITS
         CALL BUILD      
         do ik=ikmin,min(IKPF-1,ikmax)
          call rdbuf(25,dbdz,16)
         end do            
c >>> exponentials        
         do irc=1,nrcv
          fac(irc)=-q*arg(irc)
         end do
         call cvexpi(fac,1,cbuf,2,nrcv)
c >>> range dependent wavenumber factors
         if (icdr.eq.0) then
          sqrq=sqrt(abs(q))
          do irc=1,nrcv
           if (abs(arg(irc)).lt.1e-3) then
             fac(irc)=0.5*abs(q)
           else
             fac(irc)=sqrq
           end if
          end do
         else
          do irc=1,nrcv
           fac(irc)=1E0
          end do
         end if
c >>> compute scattering kernel 

C *** FILL IN SCATTERING RIGHT HAND SIDE
C *** K-WAVENUMBER LOOP
         DO 14 JPA=1,4
          CALL VCLR(R(1,JPA,1),1,2*NUML)
 14      CONTINUE
         call vclr(rhsbuf,1,2*neq*nzk)
         DO 16 IK=IKPF,IKPL
          CETA=WVNO+IK*DLWVNO
          CALL RDBUF(25,DBDZ,16)
          DO 15 JPA=1,4
           R(INTFCE-1,JPA,1)=(DBDZ(JPA)-AI*(WVNO-CETA)*BLC(JPA))
 15       CONTINUE
c          if (mod(iq,100).eq.0.and.ik.eq.0) then
c           write(82,*) 'intfce,iq:',intfce,iq
c           write(82,*) 'r:',(r(intfce-1,jpa,1),jpa=1,4)
c          end if
C *** NORMALIZE DISPLACEMENT RHS
          R(INTFCE-1,1,1)=DISNRM*R(INTFCE-1,1,1)
          R(INTFCE-1,2,1)=DISNRM*R(INTFCE-1,2,1)
          R(INTFCE-1,3,1)=pcorr*strNRM*R(INTFCE-1,3,1)
          R(INTFCE-1,4,1)=pcorr*strNRM*R(INTFCE-1,4,1)
          CALL CVIMOV(R,INDR,1,RHSBUF(1+(IK-IKPF)*NEQ),2,NEQ)
 16      CONTINUE
C *** THE FOLLOWING REPLACES THE CALL TO SOLVE
         CALL CVIMOV(ALO,INDA,1,WORK1,2,NNA)
         CALL CVFILL(CNUL,WORK2,2,NNB)
         CALL CVMOVI(WORK1,2,INDB,1,WORK2,NNA)
         EPS=1E-10
         CALL CBGEMR(WORK2,RHSBUF,NEQ,NEQ,NZK,IBW,EPS)
         IERR=EPS
         IF (IERR.NE.0) RETURN
c >>> add contribution to q-integral including symmetric part in 
c     k and q
         ise1=nint(1.5-sign(0.5,q))
         ise2=3-ise1
         DO 17 IK=IKPF,IKPL
          CALL CVMOVI(RHSBUF(1+(IK-IKPF)*NEQ),2,INDS,1,SS,NEQ)
c >>> Compute wavefield potentials
          call wfield()
          CALL KERNEL(CFILEK)
          iof=(ik-ip)*nrcv
          do irec=1,nrcv
           INDXCF=idep(irec)+(ircpar(irec)-1)*IR
           iof2=irec+iof
           cff(iof2,1)=cff(iof2,1)+fac(irec)
     &        *( ranexp(irec,ise1)*cfilek(indxcf)*cbuf(irec)
     &          +ranexp(irec,ise2)*cfilek(indxcf)*conjg(cbuf(irec)))
          end do
 17      CONTINUE
        end if
c >>> q-integral done
 40    continue
c >>> update p-integral
        do ipp=ip,ipmax
         iof=(ipp-ip)*nrcv
         facin=FF*P(-IPP*DLWVNO,INTFCE)
         do irec=1,nrcv
          do jrec=1,nrcv
           index=irec+(jrec-1)*nrcv
           corrns(index)= corrns(index)
     &        + facin*gain(irec)*gain(jrec)
     &         *cff(irec+iof,1)*conjg(cff(jrec+iof,1))
          end do
         end do
        end do
       call rdtime(tt2)
       write(6,*) '>>> Done, CPU=',tt2-tt1,' secs <<<'
 50   continue

C *** Normalized covariance
      if (debug) WRITE(6,*) 'Normalization'
         do jrec=1,nrcv
          write(24,'(1H ,a,i6)') 'receiver:',jrec
          indexj=jrec+(jrec-1)*nrcv
          do irec=1,nrcv
           index=irec+(jrec-1)*nrcv
           indexi=irec+(irec-1)*nrcv
           conorm(index)= real(corrns(index))
     &         /sqrt(real(corrns(indexi))*real(corrns(indexj)))
          end do
          index=1+(jrec-1)*nrcv
          i2=index+nrcv-1
          write(24,'(1H ,6f12.6)') (conorm(ii),ii=index,i2)
         end do
      IF (DEBUG) WRITE(6,*) 'END REVCOV'
      RETURN
      END           





