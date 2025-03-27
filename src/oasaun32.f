      SUBROUTINE inits
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
Cms
Cms  **** modified to include porous sediment layers - 6/29/92
Cms
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      INCLUDE 'comti.f'
      COMPLEX S
      COMPLEX TS2,CCC,TS,cc,cc3
Cms ** add dummy complex constants for porous sediment calculations
      COMPLEX DEL1, DEL2
      COMPLEX ZETAS,AIRYS,BIRYS,AIRYDS,BIRYDS,ZTAMS
      logical largek(nla)
      common /kflags/ largek,ksign
c      if (debug) write(6,*) '>>> Entering INITS <<<'
c
C     RESET DEPTH-DERIVATIVE FLAG
C
      IDERIV=0
      S=WVNO
      S2=S*S
      www=wvno
      ksign=nint(sign(RONE,www))
      onok=sign(RONE,www)/wvno
      do 1 ln=1,numl
       largek(ln)= ((laytyp(ln).eq.3).and.
     &              (abs(real(onok)*real(ak(ln,2))).le.5e-2) )
 1    continue
      THICK(1)=RNUL
      THICK(NUML)=RNUL
      do is=1,nsrow
       do IL=1,NUML
        do JJJ=1,NLEQ
         ROIN(IL,JJJ,is)=CMPLX(RNUL,RNUL)
         RUIN(IL,JJJ,is)=CMPLX(RNUL,RNUL)
        end do
        do JJJ=1,nleqh
         POIN(JJJ,IL,is)=CMPLX(RNUL,RNUL)
         PUIN(JJJ,IL,is)=CMPLX(RNUL,RNUL)
        end do
       end do
      end do
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 I=1,NUMT(1)
       LL=LAYT(I,1)
       if (flow_flag(ll)) then 
c        cang=1E0-min(1E0,(real(wvno)/real(ak(ll,1))))*
c     &           (flow_vel(ll)/v(ll,2))
c        cang=cang*cang
        flow_fac(ll)=(1E0-flow_vel(ll)*wvno/dsq)**2
        ALFA(LL)=SQRTT(S2-AK2(LL,1)*flow_fac(ll))
       else
        flow_fac(ll)=1e0
        ALFA(LL)=SQRTT(S2-AK2(LL,1))
       end if
       EZALFM(LL)=CEXPT(-ALFA(LL)*THICK(LL))
       alfinv(LL)=RONE/ALFA(LL)
 10   CONTINUE

C$DIR No_Recurrence
CDIR$ IVDEP
cvd$  select(concur)
      do J=1,NUMT(2)
       LN=LAYT(J,2)
       if (flow_flag(ln)) then 
        flow_fac(ln)=(1E0-flow_vel(ln)*real(wvno)/dsq)**2
       else
        flow_fac(ln)=1e0
       end if               
       ZETAU(LN)=CCO(LN)*S2-BCO(LN)*flow_fac(ln)
       CALL SCAIRY(ZETAU(LN),AIRYU(LN),BIRYU(LN),
     &              AIRYDU(LN),BIRYDU(LN),ZTAMU(LN))
       ZETAL(LN)=ZETAU(LN)-ACO(LN)*THICK(LN)*flow_fac(ln)
       CALL SCAIRY(ZETAL(LN),AIRYL(LN),BIRYL(LN),
     &              AIRYDL(LN),BIRYDL(LN),ZTAML(LN))
       IF (REAL(ACO(LN)).LT.0) THEN
        AISC(LN)=ZTAMU(LN)
        BISC(LN)=ZTAML(LN)
       ELSE
        AISC(LN)=ZTAML(LN)
        BISC(LN)=ZTAMU(LN)
       END IF
      end do
C
C     SOURCES IN AIRY LAYERS INCLUDED 850214
C
cvd$  noconcur     
cvd$  novector
CDIR$ NOVECTOR
      is=1
      do J=1,NUMTS(2)
       I=NSPNT(J,2)
       LN=LAYS(I)
       if (srctyp.eq.97) then
        cc3=-sval(i,1)
       else if (srctyp.eq.98.or.srctyp.eq.99) then
        cc3=-sval(i,1)
       else
        cc3=cphfac(i)
       end if
       ZETAS=CCO(LN)*S2-(ACO(LN)*ZUS(I)+BCO(LN))*flow_fac(ln)
       CALL SCAIRY(ZETAS,AIRYS,BIRYS,AIRYDS,BIRYDS,ZTAMS)
       CC=2E0*sign(1e0,real(aco(ln)))*CC3/((-AIRYS*BIRYDS
     1       +BIRYS*AIRYDS)*(-ACO(LN)))
       IF (IDERIV.NE.1) THEN
         CSAIR1(I)=CC*BIRYS
         CSAIR2(I)=CC*AIRYS
       ELSE
         CSAIR1(I)=-ACO(LN)*CC*BIRYDS
         CSAIR2(I)=-ACO(LN)*CC*AIRYDS
       END IF
       CSAIR3(I)=ZTAMS
      end do

cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 30 I=1,NUMT(3)
      LL=LAYT(I,3)
      ALFA(LL)=SQRTT(S2-AK2(LL,1))
      BETA(LL)=SQRTT(S2-AK2(LL,2))
      alfinv(LL)=RONE/ALFA(LL)
      betinv(LL)=RONE/BETA(LL)
      if (debug) write(6,*) 'arg,alfa=',s2-ak2(ll,1),alfa(ll)
 30   CONTINUE
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 31 I=1,NUMT(3)
      LL=LAYT(I,3)
      CON2(LL)=CON1(LL)*(2E0*S2-AK2(LL,2))
      CON3(LL)=CON1(LL)*2E0*S*ALFA(LL)
      CON4(LL)=CON1(LL)*2E0*S*BETA(LL)
      CON5(LL)=CON1(LL)*(2E0*ALFA(LL)*ALFA(LL)+AK2(LL,2))
 31   CONTINUE
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 32 I=1,NUMT(3)
      LL=LAYT(I,3)
      EZALFM(LL)=CEXPT(-ALFA(LL)*THICK(LL))
      EZBETM(LL)=CEXPT(-BETA(LL)*THICK(LL))
 32   CONTINUE
C
C     READ IN SLOWNESSES, DISP. AND STRESSES FOR ANISOTRIPIC LAYERS
C
        DO 40 I=1,NUMT(4)
         LAYN=LAYT(I,4)
         call VMOV(ELAG(1,LAYN),1,A,1,24)
         CALL TIEIGEN(WVNO)
         CALL VMOV(S3UP,1,ANSTD(1,LAYN),1,NTIPAR*2)
c        CALL RDBUF(41,ANSTD(1,LAYN),NTIPAR*2)
 40     CONTINUE
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
        DO 41 I=1,NUMT(4)
        LAYN=LAYT(I,4)
        ALFA(LAYN)=ANSTD(1,LAYN)
        BETA(LAYN)=ANSTD(2,LAYN)
        alfinv(LAYN)=RONE/ALFA(LAYN)
        betinv(LAYN)=RONE/BETA(LAYN)
 41     CONTINUE
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
        DO 42 I=1,NUMT(4)
        LAYN=LAYT(I,4)
        EZALFM(LAYN)=CEXPT(-THICK(LAYN)*ALFA(LAYN))
        EZBETM(LAYN)=CEXPT(-THICK(LAYN)*BETA(LAYN))
 42     CONTINUE
Cms
Cms  Calculate data for porous sediment layers
Cms
	DO 50 I=1,NUMT(5)
	  LL = LAYT(I,5)
	  ALFA(LL) = SQRTT(S2 - CK1(LL))
	  IF(REAL(ALFA(LL)).LT.RNUL) ALFA(LL) = -ALFA(LL)
          alfinv(LL)=RONE/ALFA(LL)
	  BETA(LL) = SQRTT(S2 - CK2(LL))
	  IF(REAL(BETA(LL)).LT.RNUL) BETA(LL) = -BETA(LL)
          betinv(LL)=RONE/BETA(LL)
	  GAMMA(LL) = SQRTT(S2 - CKS(LL))
	  IF(REAL(GAMMA(LL)).LT.RNUL) GAMMA(LL) = -GAMMA(LL)
          gaminv(LL)=RONE/gamma(LL)
	  DEL1 = (RHO(LL)* CSQ - CH(LL)*CK1(LL))
     &         /(RHOF(LL)*CSQ - BCC(LL)*CK1(LL))
	  DEL2 = (RHO(LL)* CSQ - CH(LL)*CK2(LL))
     &         /(RHOF(LL)*CSQ - BCC(LL)*CK2(LL))
	  CON2(LL) = CON1(LL)*(CK1(LL)*(BCC(LL)*DEL1 - CH(LL)) 
     &         + 2.*CMU(LL)*S*S)
	  CON3(LL) = CON1(LL)*(CK2(LL)*(BCC(LL)*DEL2 - CH(LL))
     &         + 2.*CMU(LL)*S*S)
	  CON4(LL) = CON1(LL)*2.*CMU(LL)*GAMMA(LL)*S
	  CON5(LL) = CON1(LL)*2.*CMU(LL)*ALFA(LL)*S
	  CON6(LL) = CON1(LL)*2.*CMU(LL)*BETA(LL)*S
	  CON7(LL) = CON1(LL)*CMU(LL)*(2.*S*S - CKS(LL))
	  CON8(LL) = CON1(LL)*CK1(LL)*(BCC(LL) - CM(LL)*DEL1)
	  CON9(LL) = CON1(LL)*CK2(LL)*(BCC(LL) - CM(LL)*DEL2)
	  CON10(LL) = CON1(LL)*(CK1(LL)*(BCC(LL)*DEL1 - CH(LL)) 
     &         - 2.*CMU(LL)*ALFA(LL)*ALFA(LL))
	  CON11(LL) = CON1(LL)*(CK2(LL)*(BCC(LL)*DEL2 - CH(LL))
     &         - 2.*CMU(LL)*BETA(LL)*BETA(LL))
	  DELA(LL) = DEL1*ALFA(LL)
	  DELB(LL) = DEL2*BETA(LL)
        EZALFM(LL)=CEXPT(-ALFA(LL)*THICK(LL))
        EZBETM(LL)=CEXPT(-BETA(LL)*THICK(LL))
        EZGAMM(LL)=CEXPT(-GAMMA(LL)*THICK(LL))
  50  CONTINUE
      RETURN
      END

      SUBROUTINE BUILD               
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
      INCLUDE 'comnrd.f'
      COMPLEX S
c      if (debug) write(6,*) '>>> Entering BUILD <<<'
      S=WVNO
C *** build local coefficient matrices
c       write(6,*) 'build: numt=',(numt(j),j=1,5)
       IF (NUMT(1).GT.0) CALL LIQLAY
       IF (NUMT(2).GT.0) CALL AIRLAY
       IF (NUMT(3).GT.0) CALL SOLLAY
       IF (NUMT(4).GT.0) CALL TISOLL
Cms  **** subroutine BIOLAY added 4/26/96
       IF (NUMT(5).GT.0) CALL BIOLAY
C *** SCATTERING PERTURBATION
      CALL SCATT
C *** NORMALIZE DISPLACEMENT EQUATIONS
c      IF (AKMAX.LT.ABS(WVNO).or.DETERM) THEN
c       DISNRM=RONE/ABS(WVNO)
c       STRNRM=(AKMAX/ABS(WVNO))**2
c      ELSE
       DISNRM=RONE/AKMAX
       STRNRM=RONE
c      END IF
      do is=1,nsrow
       do L=1,NUML
        do I=1,2
         ROIN(L,I,is)=ROIN(L,I,is)*DISNRM
         ROIN(L,I+2,is)=ROIN(L,I+2,is)*STRNRM
         RUIN(L,I,is)=RUIN(L,I,is)*DISNRM
         RUIN(L,I+2,is)=RUIN(L,I+2,is)*STRNRM
        end do
        ROIN(L,5,is)=ROIN(L,5,is)*DISNRM
        RUIN(L,5,is)=RUIN(L,5,is)*DISNRM
        ROIN(L,6,is)=ROIN(L,6,is)*STRNRM
        RUIN(L,6,is)=RUIN(L,6,is)*STRNRM
       end do
      end do
      do J=1,nleq
       do L=1,NUML
c >>> Displacements
        AUP(L,1,J)=AUP(L,1,J)*DISNRM
        AUP(L,2,J)=AUP(L,2,J)*DISNRM
        AUP(L,5,J)=AUP(L,5,J)*DISNRM
        ALO(L,1,J)=ALO(L,1,J)*DISNRM
        ALO(L,2,J)=ALO(L,2,J)*DISNRM
        ALO(L,5,J)=ALO(L,5,J)*DISNRM
c >>> Stresses
        AUP(L,3,J)=AUP(L,3,J)*STRNRM
        AUP(L,4,J)=AUP(L,4,J)*STRNRM
        AUP(L,6,J)=AUP(L,6,J)*STRNRM
        ALO(L,3,J)=ALO(L,3,J)*STRNRM
        ALO(L,4,J)=ALO(L,4,J)*STRNRM
        ALO(L,6,J)=ALO(L,6,J)*STRNRM
       end do
      end do
C *** MERGE RIGHT HAND SIDES
      do is=1,nsrow
       do J=1,NLEQ
        do I=1,NUMI
         R(I,J,is)=ROIN(I,J,is)+RUIN(I,J,is)
        end do
       end do
      end do
      RETURN        
      END           

      SUBROUTINE SOLLAY     
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
      INCLUDE 'comnrd.f'
      COMPLEX S
      LOGICAL IFS
      COMPLEX CC,CC0,CC1,CC2,CC3,cc4,
     &        kralf,krbet,kralf2,krbet2,ec,ece,pwvno
      logical largek(nla)
      common /kflags/ largek,ksign
c      if (debug) write(6,*) '>>> Entering SOLLAY <<<'
C     FIRST ROW IS VERTICAL DISPLACEMENT
cvd$r permutation(LAYT)
cvd$  nodepchk
C$DIR NO_RECURRENCE
CDIR$ IVDEP
      DO 10 j=1,NUMT(3)
       LN=LAYT(j,3)              
       if (.not.largek(ln)) then
        AUP(LN,1,1)=ALFA(LN)  
        AUP(LN,1,2)=-WVNO        
        AUP(LN,1,3)=-ALFA(LN) 
        AUP(LN,1,4)=-WVNO        
        if (debug) then
         write(6,*) (aup(ln,1,jj),jj=1,4)
        end if
C     SECOND ROW IS HORIZONTAL DISPLACEMENT           
        AUP(LN,2,1)=WVNO         
        AUP(LN,2,2)=-BETA(LN) 
        AUP(LN,2,3)=WVNO         
        AUP(LN,2,4)=BETA(LN)  
        if (debug) then
         write(6,*) (aup(ln,2,jj),jj=1,4)
        end if
C     THIRD ROW IS NORMAL STRESS  
        AUP(LN,3,1)=-CON2(LN)
        AUP(LN,3,2)=CON4(LN)
        AUP(LN,3,3)=-CON2(LN)
        AUP(LN,3,4)=-CON4(LN)
        if (debug) then
         write(6,*) (aup(ln,3,jj),jj=1,4)
        end if
C     LAST ROW IS SHEAR STRESS    
        AUP(LN,4,1)=-CON3(LN)
        AUP(LN,4,2)=CON2(LN)
        AUP(LN,4,3)=CON3(LN)
        AUP(LN,4,4)=CON2(LN)
        if (debug) then
         write(6,*) (aup(ln,4,jj),jj=1,4)
        end if
C     THE LOWER INTERFACE MATRIX FOLLOWS BY A CHANGE OF SIGN
C     AND MULTLIPICATION WITH THE EXPONENTIALS        
        DO 15 I=1,4           
         ALO(LN,I,1)=-AUP(LN,I,1)*EZALFM(LN)                 
         ALO(LN,I,2)=-AUP(LN,I,2)*EZBETM(LN)                 
         ALO(LN,I,3)=-AUP(LN,I,3)                 
         ALO(LN,I,4)=-AUP(LN,I,4)
         AUP(LN,I,3)=AUP(LN,I,3)*EZALFM(LN)
         AUP(LN,I,4)=AUP(LN,I,4)*EZBETM(LN)                 
 15     CONTINUE
       else
        if (debug) write(6,*) '>>> Large k approximation <<<'
c >>> large k values
        kralf=onok*ak(ln,1)
        krbet=onok*ak(ln,2)
        krbet2=krbet*krbet
        kralf2=kralf*kralf
        www=wvno
        pwvno=sign(RONE,www)*wvno
         ec=0.5*thick(ln)*pwvno*(kralf2-krbet2)
         ece=cexpt(ec)
c     first row is vertical displacement
        cc1=0.5*pwvno*kralf2
        AUP(LN,1,1)=-cc1
        AUP(LN,1,2)=alfa(ln)+pwvno       
        alo(ln,1,1)=-(pwvno*ec-cc1*ece)*ezbetm(ln)
        alo(ln,1,2)=-(ezalfm(ln)*alfa(ln)+ezbetm(ln)*pwvno) 
        Alo(LN,1,3)=-(-alfa(ln)-pwvno) 
        Alo(LN,1,4)=-(cc1)       
        aup(ln,1,3)=-alfa(ln)*ezalfm(ln)-pwvno*ezbetm(ln)
        aup(ln,1,4)=(-pwvno*ec+cc1*ece)*ezbetm(ln)
C     SECOND ROW IS HORIZONTAL DISPLACEMENT           
        cc2=0.5*pwvno*krbet2         
        AUP(LN,2,1)=cc2
        AUP(LN,2,2)=pwvno+beta(ln)
        alo(ln,2,1)=-(pwvno*ec+cc2)*ezbetm(ln)
        alo(ln,2,2)=-(pwvno*ezalfm(ln)+beta(ln)*ezbetm(ln))
        Alo(LN,2,3)=-(pWVNO+beta(ln))         
        Alo(LN,2,4)=-(cc2)
        aup(ln,2,3)=(pwvno*ezalfm(ln)+beta(ln)*ezbetm(ln))
        aup(ln,2,4)=(pwvno*ec+cc2)*ezbetm(ln)  
C     THIRD ROW IS NORMAL STRESS  
        cc1=0.25*(s2*krbet2)*krbet2*con1(ln)
        cc=ksign*con4(ln)
        AUP(LN,3,1)=cc1
        AUP(LN,3,2)=-con2(ln)-cc
        alo(ln,3,1)=-(-con2(ln)*ec+cc1)*ezbetm(ln)
        alo(ln,3,2)=-(-con2(ln)*ezalfm(ln)-cc*ezbetm(ln))
        Alo(LN,3,3)=-(-con2(ln)-cc)
        alo(ln,3,4)=-(cc1)
        AUP(LN,3,3)=-con2(ln)*ezalfm(ln)-con4(ln)*ezbetm(ln)
        aup(ln,3,4)=(-con2(ln)*ec+cc1)*ezbetm(ln)
C     LAST ROW IS SHEAR STRESS    
        cc=ksign*con3(ln)
        cc1=ak2(ln,1)*con1(ln)
        cc2=ak2(ln,2)*con1(ln)
        cc3=0.25*(s2*kralf2)*kralf2*con1(ln)
        cc4=2e0*s2*con1(ln)
        AUP(LN,4,1)=cc1-cc2-cc3
        AUP(LN,4,2)=-cc-con2(ln)
        alo(ln,4,1)=-(-cc4*ec+(cc1-cc3)*ece-cc2)*ezbetm(ln)
        alo(ln,4,2)=-(-cc*ezalfm(ln)-con2(ln)*ezbetm(ln))
        alo(ln,4,3)=-(cc+con2(ln))
        alo(ln,4,4)=-(-cc1+cc2+cc3)
        AUP(LN,4,3)=cc*ezalfm(ln)+con2(ln)*ezbetm(ln)
        AUP(LN,4,4)=(cc4*ec-(cc1-cc3)*ece+cc2)*ezbetm(ln)
c >>> change signs for negative wavenumbers
        if (ksign.lt.0) then
         do 9 m=1,4
          aup(ln,2,m)=-aup(ln,2,m)
          aup(ln,4,m)=-aup(ln,4,m)
          alo(ln,2,m)=-alo(ln,2,m)
          alo(ln,4,m)=-alo(ln,4,m)
 9       continue
        end if
       end if         
 10    continue
c >>> source terms
c      write(6,*) 'sollay: calling soudgm'
      call soudgm(3)
c
c >>> Compute DGM right hand sides for all interfaces
c
cvd$r permutation(LAYT)
cvd$  nodepchk
C$DIR NO_RECURRENCE
CDIR$ IVDEP
      do j=1,NUMT(3)
       LN=LAYT(j,3)              
       if (.not.rftvty .and. nosou(ln).gt.0) then
        do is=1,nsrow
c >>> Contribution from below interface       
         in=ln-1
         ruin(in,1,is) = alfa(ln)*puin(1,in,is) +     wvno*puin(2,in,is)
         ruin(in,2,is) =   - wvno*puin(1,in,is) - beta(ln)*puin(2,in,is)
         ruin(in,3,is) = con2(ln)*puin(1,in,is) + con4(ln)*puin(2,in,is)
         ruin(in,4,is) =-con3(ln)*puin(1,in,is) - con2(ln)*puin(2,in,is)
c >>> Contribution from above interface       
         in=ln
         roin(in,1,is) = alfa(ln)*poin(1,in,is) -     wvno*poin(2,in,is)
         roin(in,2,is) =     wvno*poin(1,in,is) - beta(ln)*poin(2,in,is)
         roin(in,3,is) =-con2(ln)*poin(1,in,is) + con4(ln)*poin(2,in,is)
         roin(in,4,is) =-con3(ln)*poin(1,in,is) + con2(ln)*poin(2,in,is)
        end do
       end if
      end do
      RETURN                
      END                   
      SUBROUTINE BIOLAY
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
Cms
Cms  Subroutine to calculate local "stiffness matrix" coefficients
Cms  for a porous sediment layer.  Sources are not
Cms  permitted in a porous sediment layer in the present version.
Cms  **** Modified and corrected  7/12/93.
Cms  **** Further corrections made on 10/25/94
CHS  >>> Explosive source exciting fast wave only implemented 961105

      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX S
Cms
      S = WVNO
      DO 10 J=1,NUMT(5)
        LN=LAYT(J,5)
Cms  fifth row is negative weighted (by porosity) pore fluid
Cms  displacement relative to frame (wsubf)
        AUP(LN,5,1) = DELA(LN)
        AUP(LN,5,2) = DELB(LN)
        AUP(LN,5,3) = -AUP(LN,5,1)
        AUP(LN,5,4) = -AUP(LN,5,2)
        AUP(LN,5,5) = -S*CGAMMA(J)
        AUP(LN,5,6) = AUP(LN,5,5)
Cms  last row is pore fluid pressure           
        AUP(LN,6,1) = -CON8(LN)
        AUP(LN,6,2) = -CON9(LN)
        AUP(LN,6,3) = -CON8(LN)
        AUP(LN,6,4) = -CON9(LN)
Cms  first row is fluid normal displacement (w - wsubf)
        AUP(LN,1,1) = ALFA(LN) - AUP(LN,5,1)
        AUP(LN,1,2) = BETA(LN) - AUP(LN,5,2)
        AUP(LN,1,3) = -ALFA(LN) - AUP(LN,5,3)
        AUP(LN,1,4) = -BETA(LN) - AUP(LN,5,4)
        AUP(LN,1,5) = -S - AUP(LN,5,5)
        AUP(LN,1,6) = -S - AUP(LN,5,6)
Cms  second row is frame shear displacement           
        AUP(LN,2,1) = S
        AUP(LN,2,2) = S
        AUP(LN,2,3) = S
        AUP(LN,2,4) = S
        AUP(LN,2,5) = -GAMMA(LN)
        AUP(LN,2,6) = GAMMA(LN)
Cms  third row is normal traction
        AUP(LN,3,1) = -CON2(LN)
        AUP(LN,3,2) = -CON3(LN)
        AUP(LN,3,3) = -CON2(LN)
        AUP(LN,3,4) = -CON3(LN)
        AUP(LN,3,5) = CON4(LN)
        AUP(LN,3,6) = -CON4(LN)
Cms  fourth row is shear traction   
        AUP(LN,4,1) = -CON5(LN)
        AUP(LN,4,2) = -CON6(LN)
        AUP(LN,4,3) = CON5(LN)
        AUP(LN,4,4) = CON6(LN)
        AUP(LN,4,5) = CON7(LN)
        AUP(LN,4,6) = CON7(LN)
Cms
Cms  lower interface matrix follows by a change of sign and
Cms  multiplication with exponentials - note that origin of 
Cms  downward waves is the top of the layer but origin of
Cms  upward waves is the bottom of the layer.
Cms
        DO 20 I=1,6
          ALO(LN,I,1) = -AUP(LN,I,1)*EZALFM(LN)
          ALO(LN,I,2) = -AUP(LN,I,2)*EZBETM(LN)
          ALO(LN,I,5) = -AUP(LN,I,5)*EZGAMM(LN)
          ALO(LN,I,3) = -AUP(LN,I,3)
          ALO(LN,I,4) = -AUP(LN,I,4)
          ALO(LN,I,6) = -AUP(LN,I,6)
          AUP(LN,I,3) =  AUP(LN,I,3)*EZALFM(LN)
          AUP(LN,I,4) =  AUP(LN,I,4)*EZBETM(LN)
          AUP(LN,I,6) =  AUP(LN,I,6)*EZGAMM(LN)
  20    CONTINUE
  10  CONTINUE
Cms  no sources allowed in porous sediment layers
CHS  >>> Explosive source added 110596
CHS  >>> All sources implemented 110896
      call soudgm(5)
c
c >>> Compute DGM right hand sides for all interfaces
c
cvd$r permutation(LAYT)
cvd$  nodepchk
C$DIR NO_RECURRENCE
CDIR$ IVDEP
      do j=1,NUMT(5)
       LN=LAYT(j,5)              
       if (.not.rftvty .and. nosou(ln).gt.0) then
        do is=1,nsrow
c >>> Contribution from below interface       
         in=ln-1
         ruin(in,1,is) = - (-alfa(ln)+dela(ln))*puin(1,in,is) 
     &                   - (-beta(ln)+delb(ln))*puin(2,in,is) 
     &                   - (-wvno+wvno*cgamma(j))*puin(3,in,is)
         ruin(in,2,is) = - wvno*(puin(1,in,is)+puin(2,in,is))
     &                   - gamma(ln)*puin(3,in,is)
         ruin(in,3,is) = - (- con2(ln))*puin(1,in,is)
     &                   - (- con3(ln))*puin(2,in,is) 
     &                   - (- con4(ln))*puin(3,in,is)
         ruin(in,4,is) = - (  con5(ln))*puin(1,in,is)
     &                   - (  con6(ln))*puin(2,in,is)
     &                   - (  con7(ln))*puin(3,in,is)
         ruin(in,5,is) = - (-dela(ln)) * puin(1,in,is) 
     &                   - (-delb(ln)) * puin(2,in,is) 
     &                   - (-wvno*cgamma(j)) * puin(3,in,is)
         ruin(in,6,is) = - (-con8(ln))*puin(1,in,is) 
     &                   - (-con9(ln))*puin(2,in,is)
c >>> Contribution from above interface       
         in=ln
         roin(in,1,is) =   (alfa(ln)-dela(ln))*poin(1,in,is) 
     &                   + (beta(ln)-delb(ln))*poin(2,in,is) 
     &                   + (-wvno+wvno*cgamma(j))*poin(3,in,is)
         roin(in,2,is) =   wvno*(poin(1,in,is)+poin(2,in,is))
     &                   - gamma(ln)*poin(3,in,is)
         roin(in,3,is) =   (- con2(ln))*poin(1,in,is)
     &                   + (- con3(ln))*poin(2,in,is) 
     &                   + (  con4(ln))*poin(3,in,is)
         roin(in,4,is) =   (- con5(ln))*poin(1,in,is)
     &                   + (- con6(ln))*poin(2,in,is)
     &                   + (  con7(ln))*poin(3,in,is)
         roin(in,5,is) =   (  dela(ln)) * poin(1,in,is) 
     &                   + (  delb(ln)) * poin(2,in,is) 
     &                   + (-wvno*cgamma(j)) * poin(3,in,is)
         roin(in,6,is) =   (-con8(ln))*poin(1,in,is) 
     &                   + (-con9(ln))*poin(2,in,is)
        end do
       end if
      end do

      RETURN
      END

      SUBROUTINE TISOLL     
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
      INCLUDE 'comnrd.f'
      COMPLEX CC,CC0,CC1,CC2,CC3
c      if (debug) write(6,*) '>>> Entering TISOLL <<<'
cvd$r permutation(LAYT)
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 I=1,NUMT(4)
       LN=LAYT(I,4)
C     FIRST ROW IS VERTICAL DISPLACEMENT              
       AUP(LN,1,1)=-ANSTD(7,LN)
       AUP(LN,1,2)=-ANSTD(8,LN)            
       AUP(LN,1,3)=-ANSTD(11,LN)
       AUP(LN,1,4)=-ANSTD(12,LN)
C     SECOND ROW IS HORIZONTAL DISPLACEMENT           
       AUP(LN,2,1)=-ANSTD(5,LN)
       AUP(LN,2,2)=-ANSTD(6,LN)
       AUP(LN,2,3)=-ANSTD(9,LN)
       AUP(LN,2,4)=-ANSTD(10,LN)
C     THIRD ROW IS NORMAL STRESS  
       AUP(LN,3,1)=-CON1(LN)*ANSTD(13,LN)
       AUP(LN,3,2)=-CON1(LN)*ANSTD(14,LN)
       AUP(LN,3,3)=-CON1(LN)*ANSTD(17,LN)
       AUP(LN,3,4)=-CON1(LN)*ANSTD(18,LN)
C     LAST ROW IS SHEAR STRESS    
       AUP(LN,4,1)=-CON1(LN)*ANSTD(15,LN)
       AUP(LN,4,2)=-CON1(LN)*ANSTD(16,LN)
       AUP(LN,4,3)=-CON1(LN)*ANSTD(19,LN)
       AUP(LN,4,4)=-CON1(LN)*ANSTD(20,LN)
 10   CONTINUE
C     THE LOWER INTERFACE MATRIX FOLLOWS BY A CHANGE OF SIGN
C     AND MULTLIPICATION WITH THE EXPONENTIALS        
cvd$r permutation(LAYT)
cvd$  nodepchk
C$DIR NO_RECURRENCE
CDIR$ IVDEP
      DO 15 I=1,4
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
       DO 15 J=1,NUMT(4)
        LN=LAYT(J,4)           
        ALO(LN,I,1)=-AUP(LN,I,1)*EZALFM(LN)                 
        ALO(LN,I,2)=-AUP(LN,I,2)*EZBETM(LN)                 
        ALO(LN,I,3)=-AUP(LN,I,3)                 
        ALO(LN,I,4)=-AUP(LN,I,4)                 
        AUP(LN,I,3)=AUP(LN,I,3)*EZALFM(LN)                 
        AUP(LN,I,4)=AUP(LN,I,4)*EZBETM(LN)                 
 15   CONTINUE
C 
C     Source contributions
C
      call soudgm(4)

cvd$r permutation(LAYT)
cvd$  nodepchk
C$DIR NO_RECURRENCE
CDIR$ IVDEP
      DO 50 j=1,NUMT(4)
       LN=LAYT(j,4)              
       if (.not.rftvty .and. nosou(ln).gt.0) then
        do is=1,nsrow
c >>> Contribution from below interface       
         in=ln-1
         ruin(in,1,is) =(anstd(11,ln)*puin(1,in,is)
     &                  +anstd(12,ln)*puin(2,in,is))
         ruin(in,2,is) =(anstd( 9,ln)*puin(1,in,is)
     &                  +anstd(10,ln)*puin(2,in,is))
         ruin(in,3,is) =(anstd(17,ln)*puin(1,in,is)
     &                  +anstd(18,ln)*puin(2,in,is))*con1(ln)
         ruin(in,4,is) =(anstd(19,ln)*puin(1,in,is)
     &                  +anstd(20,ln)*puin(2,in,is))*con1(ln)
c >>> Contribution from above interface       
         in=ln
         roin(in,1,is) =-(anstd( 7,ln)*poin(1,in,is)
     &                  + anstd( 8,ln)*poin(2,in,is))
         roin(in,2,is) =-(anstd( 5,ln)*poin(1,in,is)
     &                  + anstd( 6,ln)*poin(2,in,is))
         roin(in,3,is) =-(anstd(13,ln)*poin(1,in,is)
     &                  + anstd(14,ln)*poin(2,in,is))*con1(ln)
         roin(in,4,is) =-(anstd(15,ln)*poin(1,in,is)
     &                  + anstd(16,ln)*poin(2,in,is))*con1(ln)
        end do
       end if
 50   continue
      RETURN                
      END                   
      SUBROUTINE LIQLAY             
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
      INCLUDE 'comnrd.f'
      COMPLEX CC
      COMPLEX CC1,CC2,cc3
      complex surfrc
c      if (debug) write(6,*) '>>> Entering LIQLAY <<<'
C
C     ISOVELOCITY FLUID LAYERS
C
cvd$r permutation(LAYT)
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 10 I=1,NUMT(1)
       LN=LAYT(I,1)
C     FIRST ROW IS VERTICAL DISPLACEMENT  
       if (flow_flag(ln)) then
        AUP(LN,1,1)=ALFA(LN)/flow_fac(ln)               
        AUP(LN,1,3)=-ALFA(LN)/flow_fac(ln)               
       else               
        AUP(LN,1,1)=ALFA(LN)                
        AUP(LN,1,3)=-ALFA(LN)               
       end if
C     SECOND ROW IS HORIZONTAL DISPLACEMENT                  
       AUP(LN,2,1)=WVNO    
       AUP(LN,2,3)=WVNO    
C     THIRD ROW IS NORMAL STRESS          
       AUP(LN,3,1)=CON1(LN)
       AUP(LN,3,3)=CON1(LN)
C     VANISHING SHEAR STRESS
       AUP(LN,4,1)=RNUL
       AUP(LN,4,3)=RNUL
Cms
Cms  Fifth and sixth rows added to accomodate relative fluid
Cms  displacement and pore pressure in neighboring porous
Cms  sediment layers.  Fifth row is not involved in boundary 
Cms  conditions; sixth row corresponds to pore pressure.
Cms
       AUP(LN,6,1) = -AUP(LN,3,1)
       AUP(LN,6,3) = -AUP(LN,3,3)
 10   CONTINUE
C     THE LOWER INTERFACE MATRIX FOLLOWS BY A CHANGE OF SIGN 
C     AND MULTLIPICATION WITH THE EXPONENTIALS               
cvd$r permutation(LAYT)
cvd$  nodepchk
C$DIR NO_RECURRENCE
CDIR$ IVDEP
      DO 15 I=1,nleq      
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
       DO 15 J=1,NUMT(1)
        LN=LAYT(J,1)
        ALO(LN,I,1)=-AUP(LN,I,1)*EZALFM(LN)     
        ALO(LN,I,3)=-AUP(LN,I,3)     
        AUP(LN,I,3)=AUP(LN,I,3)*EZALFM(LN)     
 15   CONTINUE
C *** SOURCE TERMS
cvd$  novector
CDIR$ NOVECTOR
cvd$  noconcur
      is=1
      DO 40 J=1,NUMTS(1)
       I=NSPNT(J,1)
       LN=LAYS(I)
       CC1=ALFINV(LN)
        is=1
       if (srctyp.eq.97) then
        cc3=-sval(i,1)
        is=ivspnt(i)
       else if (srctyp.eq.98.or.srctyp.eq.99) then
        cc3=-sval(i,1)
c        if (i.eq.1) write(6,*) 'cc3=',cc3
       else if (nwvno.eq.1.and.(PROGNM(1:4).eq.'OAST'
     &          .or.prognm(1:4).eq.'OASP')) then
c        cc3=alfa(ln)
c >>> Jai's modification for patch scattering coefficients
        cc3=alfa(ln)/(v(ln,6)*(2.*pi*freq)**2)
       else
        cc3=cphfac(i)
       end if
        IF (LN.GT.1) THEN
         CC=CC3*CEXPT(-ZUS(I)*ALFA(LN))
         if (flow_flag(ln)) then
          RUIN(LN-1,1,is)=RUIN(LN-1,1,is) + cc/flow_fac(ln)               
         else
          RUIN(LN-1,1,is)=CC+RUIN(LN-1,1,is)
         end if
         RUIN(LN-1,2,is)=-WVNO*CC*CC1+RUIN(LN-1,2,is) 
         RUIN(LN-1,3,is)=-CON1(LN)*CC*CC1+RUIN(LN-1,3,is)          
Cms  add source effect on sixth row
         RUIN(LN-1,6,is) = -RUIN(LN-1,3,is)
        END IF
        IF (LN.LT.NUML) THEN
         CC=CC3*CEXPT(-ZLS(I)*ALFA(LN))
         if (flow_flag(ln)) then
          ROIN(LN,1,is)=ROIN(LN,1,is) + cc/flow_fac(ln)   
         else
          ROIN(LN,1,is)=CC+ROIN(LN,1,is)   
         end if
         ROIN(LN,2,is)=WVNO*CC*CC1+ROIN(LN,2,is)
         ROIN(LN,3,is)=CON1(LN)*CC*CC1+ROIN(LN,3,is)             
Cms  add source effect on sixth row
         ROIN(LN,6,is) = -ROIN(LN,3,is)
        END IF
 40   CONTINUE
c
c >>> Incorporate ice reflection coefficient at surface
c
      if (icerc) then
       cc=surfrc(1,freq,real(wvno))
       aup(2,3,1) = aup(2,3,1)/(-cc)
      else if (freerc) then
       cc=surfrc(2,freq,real(wvno))
       aup(2,3,1) = aup(2,3,1)/(-cc)
      else if (tablerc) then
       cc=surfrc(3,freq,real(wvno))
       aup(2,3,1) = aup(2,3,1)/(-cc)
      end if
c
c >>> tabulated bottom reflection coefficient
c
      if (bottomrc) then
       cc=surfrc(3,freq,real(wvno))
       alo(numl-1,3,3) = alo(numl-1,3,1)/(-cc)
      end if
      RETURN           
      END              

      complex function surfrc(inum,frq,wvn)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      include 'compar.f'
      include 'comnla.f'
c      if (debug) write(6,*) 'ENTERING SURFRC'
c >>> slowness in s/m
      slw=wvn/real(dsq)
c >>> grazing angle
      coth=abs(wvn/real(ak(2,1)))
      if (abs(coth).le.RONE) then
       th=acos(coth)
      else
       th=rnul
      end if
      if (inum.eq.1) then
c >>> 3 m x 22 m x 22 m rough ice cover (Le Page model)

c     Phase angle
       ac=(2.5214E-2 * frq**(1.0135))
       phi=pi - (ac * th )
c     Amplitude
       ac=(2.1768E-4 * frq**(1.9882))
       bc=(7.0317E-6 * frq**(2.5638))
       rr=RONE - ac * th - bc * th * th * th 
c     Complex reflection coefficient
       if (debug) write(6,*) 'frq,th,rr,phi=',frq,th*180/pi,rr,phi
       surfrc=rr*cmplx(cos(phi),sin(phi))
c >>> rough ice cover (Le Page model)
c     Phase angle
c       ac=(4.71E-5 * frq + 8.886E-4) * frq + 2.6983E-3
c       bc=(-5.28E-5 * frq + 2.54878E-2) * frq - 3.46717E-2
c       phi=pi - (ac * th + bc ) * th 
c       phi=-pi + (ac * th + bc ) * th 
c     Amplitude
c       ac=((-4E-7*frq+1.396E-4)*frq-3.3782E-3)*frq+1.48927E-2
c       bc=((-2E-7*frq+5.89E-5)*frq-2.403E-4)*frq+4.3863E-3
c       rr=RONE - (ac * th + bc ) * th 
c     Complex reflection coefficient
      else if (inum.eq.2) then
c >>> rough pressure release surface (Le Page model)
c     Phase angle
       ac=(-4.64E-5 * frq - 8.259E-4) * frq + 1.77012E-2
       bc=(2.36E-5 * frq + 2.84E-3) * frq - 1.1418E-2
       phi= - pi + (ac * th + bc ) * th 
c     Amplitude
       ac=(1.114E-4 * frq - 2.4865E-3) * frq+1.64949E-2
       bc=((-8.86E-8 *frq +1.5371E-5) * frq + 2.5865E-4) * frq 
     &     - 2.0518E-3
       rr=RONE - (ac * th + bc ) * th 
c     Complex reflection coefficient
       if (debug) write(6,*) 'frq,th,rr,phi=',frq,th,rr,phi
       surfrc=rr*cmplx(cos(phi),sin(phi))
      else if (inum.eq.3) then
c >>> linear interpolation of results read into rrefl and pphi
c      if (debug) write(6,*) 'CALLING REFCO'
      if (slowrc) then
       CALL REFCO(slw,rr,phi)
      else 
       CALL REFCO(th,rr,phi)
      end if
c     Phase angle phi

c     Amplitude rr

c     Complex reflection coefficient
       if (debug) write(6,*) 'frq,th,rr,phi=',frq,th,rr,phi
       surfrc=rr*cmplx(cos(phi),sin(phi))
      else
       surfrc=RONE
      end if
      return
      end

      SUBROUTINE REFCO(th,rr,phi)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c

C     Given an angle th, REFCO returns the magnitude and
C     phase of the reflection coefficient (rr, phi).

C     Uses linear interpolation using the two nearest abscissas 
      include 'compar.f'
      include 'comnla.f'
      ILEFT  = 1
      IRIGHT = numref
C      if (debug) write(6,*) 'ENTERING REFCO',th,rr,phi,numref
C     > ,tthet(10),rrefl(10),pphi(10),numref


C     Three cases: th left, in, or right of tabulated interval

      IF ( th .LT. TTHET( ILEFT ) ) THEN
         rr   = rrefl( ILEFT )
         phi = pphi( ILEFT )
      ELSE IF( th .GT. TTHET( IRIGHT ) ) THEN
         rr   = rrefl( IRIGHT )
         phi = pphi( IRIGHT )
      ELSE 

C        Search for bracketting abscissas:
C        Log base 2 (NPTS) stabs required for a bracket

C        ------ DO WHILE ...
 1000    IF ( ILEFT .NE. IRIGHT - 1 ) THEN
            IMID = ( ILEFT + IRIGHT ) / 2
            IF ( TTHET( IMID ) .GT. th ) THEN
               IRIGHT = IMID
            ELSE
               ILEFT  = IMID
            ENDIF
         GOTO 1000
         ENDIF

C        Linear interpolation for reflection coef

         ALPHA = ( th - TTHET( ILEFT ) ) /
     &           ( TTHET( IRIGHT ) - TTHET( ILEFT ) )
         rr   = ( 1 - ALPHA ) * rrefl( ILEFT ) + ALPHA * rrefl( IRIGHT )
c >>> unwrap phase
         ddd=pphi(iright)-pphi(ileft)
         if (ddd.lt.-pi) then 
          cor=2*pi
         else if (ddd.gt.pi) then
          cor=-2*pi
         else
          cor=0
         end if
         phi = (1-ALPHA)*pphi(ILEFT)+ALPHA*(pphi(IRIGHT)+cor)
C        ------ Problem: phi may change discontinuously ...

      ENDIF

      RETURN
      END

      SUBROUTINE AIRLAY             
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
      INCLUDE 'comnrd.f'
      COMPLEX CC
C
C     AIRY SOLUTION ADDED 840907
C
C     NOTE THE GRADIENT DEPENDENT COLUMN INTERCHANGE
C
      COMPLEX CC1,CC2,cc3
      COMPLEX      AIRU,BIRU,AIRDU,BIRDU
      COMPLEX      AIRL,BIRL,AIRDL,BIRDL
c      if (debug) write(6,*) '>>> Entering AIRLAY <<<'
cvd$r permutation(LAYT)
cvd$  cncall
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
cvd$  select(concur)
      DO 10 J=1,NUMT(2)
       LN=LAYT(J,2)
        CC1=CEXPT(AISC(LN)-ZTAMU(LN))
        CC2=CEXPT(ZTAMU(LN)-BISC(LN))
        AIRU=AIRYU(LN)*CC1
        AIRDU=AIRYDU(LN)*CC1
        BIRU=BIRYU(LN)*CC2
        BIRDU=BIRYDU(LN)*CC2
        CC1=CEXPT(AISC(LN)-ZTAML(LN))
        CC2=CEXPT(ZTAML(LN)-BISC(LN))
        AIRL=AIRYL(LN)*CC1
        AIRDL=AIRYDL(LN)*CC1
        BIRL=BIRYL(LN)*CC2
        BIRDL=BIRYDL(LN)*CC2
        IF (REAL(ACO(LN)).LT.0) THEN
          if (flow_flag(ln)) then
           AUP(LN,1,1)=AIRDU*ACO(LN)/flow_fac(ln)
           AUP(LN,1,3)=BIRDU*ACO(LN)/flow_fac(ln)
          else
           AUP(LN,1,1)=AIRDU*ACO(LN)
           AUP(LN,1,3)=BIRDU*ACO(LN)
          end if
          AUP(LN,2,1)=WVNO
          AUP(LN,3,1)=CON1(LN)*AIRU
          AUP(LN,3,3)=CON1(LN)*BIRU
        ELSE
          if (flow_flag(ln)) then
           AUP(LN,1,1)=BIRDU*ACO(LN)/flow_fac(ln)
           AUP(LN,1,3)=AIRDU*ACO(LN)/flow_fac(ln)
          else
           AUP(LN,1,1)=BIRDU*ACO(LN)
           AUP(LN,1,3)=AIRDU*ACO(LN)
          end if
          AUP(LN,3,1)=CON1(LN)*BIRU
          AUP(LN,3,3)=CON1(LN)*AIRU
        END IF
        AUP(LN,4,1)=RNUL
        AUP(LN,4,3)=RNUL
Cms
Cms  Fifth and sixth rows added to accomodate relative fluid
Cms  displacement and pore pressure in neighboring porous
Cms  sediment layers.  Fifth row is not involved in boundary 
Cms  conditions; sixth row corresponds to pore pressure.
Cms
         AUP(LN,6,1) = -AUP(LN,3,1)
         AUP(LN,6,3) = -AUP(LN,3,3)
        IF (REAL(ACO(LN)).LT.0) THEN
          if (flow_flag(ln)) then
           ALO(LN,1,1)=-AIRDL*ACO(LN)/flow_fac(ln)
           ALO(LN,1,3)=-BIRDL*ACO(LN)/flow_fac(ln)
          else
           ALO(LN,1,1)=-AIRDL*ACO(LN)
           ALO(LN,1,3)=-BIRDL*ACO(LN)
          end if
          ALO(LN,3,1)=-CON1(LN)*AIRL
          ALO(LN,3,3)=-CON1(LN)*BIRL
        ELSE
          if (flow_flag(ln)) then
           ALO(LN,1,1)=-BIRDL*ACO(LN)/flow_fac(ln)
           ALO(LN,1,3)=-AIRDL*ACO(LN)/flow_fac(ln)
          else
           ALO(LN,1,1)=-BIRDL*ACO(LN)
           ALO(LN,1,3)=-AIRDL*ACO(LN)
          end if
          ALO(LN,3,1)=-CON1(LN)*BIRL
          ALO(LN,3,3)=-CON1(LN)*AIRL
        END IF
        ALO(LN,4,1)=RNUL
        ALO(LN,4,3)=RNUL
Cms  here too!
        ALO(LN,6,1) = -ALO(LN,3,1)
        ALO(LN,6,3) = -ALO(LN,3,3)
 10   CONTINUE
C
C     SOURCES IN AIRY LAYERS INCLUDED 850214
C
cvd$  noconcur     
cvd$  novector
CDIR$ NOVECTOR
       is=1
       DO 15 J=1,NUMTS(2)
        I=NSPNT(J,2)
        if (srctyp.eq.97) then
         is=ivspnt(i)
        end if
        LN=LAYS(I)
        CC1=CSAIR1(I)*CEXPT(csair3(i)-BISC(LN))
        CC2=CSAIR2(I)*CEXPT(AISC(LN)-csair3(i))
        IF (REAL(ACO(LN)).GE.0) THEN
C       NEGATIVE VELOCITY GRADIENT
         if (flow_flag(ln)) then         
          RUIN(LN-1,1,is)=RUIN(LN-1,1,is)-ACO(LN)*CC1*AIRYDU(LN)
     &                    / flow_fac(ln)
          ROIN(LN,1,is)=ROIN(LN,1,is)+ACO(LN)*CC2*BIRYDL(LN)
     &                    / flow_fac(ln)
         else
          RUIN(LN-1,1,is)=RUIN(LN-1,1,is)-ACO(LN)*CC1*AIRYDU(LN)
          ROIN(LN,1,is)=ROIN(LN,1,is)+ACO(LN)*CC2*BIRYDL(LN)
         end if
         RUIN(LN-1,3,is)=RUIN(LN-1,3,is)-CON1(LN)*CC1*AIRYU(LN)
         ROIN(LN,3,is)=ROIN(LN,3,is)+CON1(LN)*CC2*BIRYL(LN)
        ELSE
         if (flow_flag(ln)) then         
          RUIN(LN-1,1,is)=RUIN(LN-1,1,is)-ACO(LN)*CC2*BIRYDU(LN)
     &                    / flow_fac(ln)
          ROIN(LN,1,is)=ROIN(LN,1,is)+ACO(LN)*CC1*AIRYDL(LN)
     &                    / flow_fac(ln)
         else
          RUIN(LN-1,1,is)=RUIN(LN-1,1,is)-ACO(LN)*CC2*BIRYDU(LN)
          ROIN(LN,1,is)=ROIN(LN,1,is)+ACO(LN)*CC1*AIRYDL(LN)
         end if
         RUIN(LN-1,3,is)=RUIN(LN-1,3,is)-CON1(LN)*CC2*BIRYU(LN)
         ROIN(LN,3,is)=ROIN(LN,3,is)+CON1(LN)*CC1*AIRYL(LN)
        END IF
Cms  add source effect on sixth row
        RUIN(LN-1,6,is) = -RUIN(LN-1,3,is)
        ROIN(LN,6,is) = -ROIN(LN,3,is)
 15    CONTINUE
      RETURN
      END              
      SUBROUTINE WFIELD        
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
Cms
Cms  Modified to include recievers in porous sediment layers
Cms  Does NOT include sources in sediment layers 6/2/93
Cms
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
Cms  Variables added to accomodate additional degrees of freedom in
Cms  sediment layers
      COMPLEX ERALFA,ERBETA,ERALFM,ERBETM,ERGAMA,ERGAMM
      COMPLEX CC,CC1,CC2,CC3,CC4,cc5
      logical largek(nla)
      common /kflags/ largek,ksign
c      if (debug) write(6,*) '>>> Entering WFIELD <<<'
      do is=1,nsrow
       call vclr(pot(1,1,is),1,2*ir*nleq)
      end do
C
C *** RECEIVERS IN ISOVELOCITY FLUID LAYERS
cvd$  permutation(NUMTR)
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
c       if (debug) write(6,*) 'nsrow=',nsrow,'numtr=',numtr(1)
      DO 10 J=1,NUMTR(1)
       INT=NRPNT(J,1)
       LL=LAY(INT)      
       ZZ=Z(INT)        
       IF (LL.NE.1) THEN
         ERALFM=CEXPT(-ZZ*ALFA(LL))
       ELSE
         ERALFM=RNUL
       END IF                 
       IF (LL.NE.NUML) THEN
         ERALFA=CEXPT((ZZ-THICK(LL))*ALFA(LL))
       ELSE
         ERALFA=RNUL
       END IF
       do is=1,nsrow
        POT(1,INT,is)=SS(LL,1,is)*ERALFM
        POT(3,INT,is)=SS(LL,3,is)*ERALFA
c        if (debug) write(6,*)'pot=',pot(1,int,is),pot(2,int,is)
       end do
C *** SOURCE CONTRIBUTION
       IF (.not.rftvty .and. NOSOU(LL).GT.0) THEN
         is=1
         CC1=ALFINV(LL)
         do I=IFSOU(LL),ILSOU(LL)
          if (srctyp.eq.97) then
           cc5=-sval(i,1)
           is=ivspnt(i)
          else if (srctyp.eq.98.or.srctyp.eq.99) then
           cc5=-sval(i,1)
       else if (nwvno.eq.1.and.(PROGNM(1:4).eq.'OAST'
     &          .or.prognm(1:4).eq.'OASP')) then
c           cc5=alfa(ll)
c >>> Jai's modification for patch scattering coefficients
           cc5=alfa(ll)/(v(ll,6)*(2.*pi*freq)**2)
          else
           cc5=cphfac(i)
          end if
          if (rdc(int).eq.sdc(i)) then
           pot(1,int,is)=pot(1,int,is)+0.5*cc5*cc1
           pot(3,int,is)=pot(3,int,is)+0.5*cc5*cc1
          else
           isi=nint(SIGN(1.0,rdc(int)-sdc(i)))
           iwav=2-isi
           CC=CC5*CEXPT(-ABS(sdc(I)-rdc(int))*ALFA(LL))
           POT(IWAV,INT,is)=POT(IWAV,INT,is)+CC*CC1
          end if
         end do
       END IF
 10    CONTINUE
C
C     AIRY SOLUTION IMPLEMENTED 840907
C
C *** RECEIVERS IN non-ISOVELOCITY FLUID LAYERS
cvd$  permutation(NRPNT)
cvd$  cncall
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
cvd$  select(concur)
      DO 20 J=1,NUMTR(2)
       INT=NRPNT(J,2)
       LL=LAY(INT)      
       ZZ=Z(INT)        
       ZETA(INT)=CCO(LL)*S2-(ZZ*ACO(LL)+BCO(LL))*flow_fac(ll)
       CALL SCAIRY(ZETA(INT),AIRY(INT),BIRY(INT),
     &             AIRYD(INT),BIRYD(INT),ZTAM(INT))
        CC1=CEXPT(AISC(LL)-ZTAM(INT))
        CC2=CEXPT(ZTAM(INT)-BISC(LL))
        IF ((REAL(ACO(LL))).LT.0) THEN
         do is=1,nsrow
          POT(1,INT,is)=SS(LL,1,is)*CC1*AIRY(INT)
          POT(3,INT,is)=SS(LL,3,is)*CC2*BIRY(INT)
          POT(2,INT,is)=SS(LL,1,is)*CC1*AIRYD(INT)
          POT(4,INT,is)=SS(LL,3,is)*CC2*BIRYD(INT)
         end do
        ELSE
         do is=1,nsrow
          POT(1,INT,is)=SS(LL,3,is)*CC1*AIRY(INT)
          POT(3,INT,is)=SS(LL,1,is)*CC2*BIRY(INT)
          POT(2,INT,is)=SS(LL,3,is)*CC1*AIRYD(INT)
          POT(4,INT,is)=SS(LL,1,is)*CC2*BIRYD(INT)
         end do
        END IF
C
C      SOURCES INCLUDED IN AIRY LAYERS 840214
C
        IF (.not.rftvty .and. NOSOU(LL).GT.0) THEN
         is=1
         DO 5 I=IFSOU(LL),ILSOU(LL)
          if (srctyp.eq.97) then
           is=ivspnt(i)
          end if
          IF (REAL(ACO(LL))*(rdc(int)-sdc(i)).LE.0.) THEN
           CC3=CEXPT(CSAIR3(I)-ZTAM(INT))
           POT(1,INT,is)=POT(1,INT,is)+AIRY(INT)*CSAIR1(I)*CC3
           POT(2,INT,is)=POT(2,INT,is)+AIRYD(INT)*CSAIR1(I)*CC3
          ELSE
           CC3=CEXPT(ZTAM(INT)-CSAIR3(I))
           POT(3,INT,is)=POT(3,INT,is)+BIRY(INT)*CSAIR2(I)*CC3
           POT(4,INT,is)=POT(4,INT,is)+BIRYD(INT)*CSAIR2(I)*CC3
          END IF
 5       CONTINUE
        END IF
 20   CONTINUE
C *** RECEIVERS IN SOLID LAYERS
C *** SOURCE TERMS
cvd$  permutation(NRPNT)
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 30 J=1,NUMTR(3)
       INT=NRPNT(J,3)
       LL=LAY(INT)      
       ZZ=Z(INT)        
        IF (LL.NE.1) THEN
         ERALFM=CEXPT(-ZZ*ALFA(LL))                 
         ERBETM=CEXPT(-ZZ*BETA(LL))            
        ELSE
         ERALFM=RNUL
         ERBETM=RNUL
        END IF
        IF (LL.NE.NUML) THEN
         ERALFA=CEXPT((ZZ-THICK(LL))*ALFA(LL))
         ERBETA=CEXPT((ZZ-THICK(LL))*BETA(LL))
        ELSE
         ERALFA=RNUL
         ERBETA=RNUL
        END IF
        if (.not.largek(ll)) then
         do is=1,nsrow
          POT(1,INT,is)=SS(LL,1,is)*ERALFM
          POT(2,INT,is)=SS(LL,2,is)*ERBETM
          POT(3,INT,is)=SS(LL,3,is)*ERALFA
          POT(4,INT,is)=SS(LL,4,is)*ERBETA
         end do
        else
         do is=1,nsrow
          POT(1,INT,is)=(SS(LL,1,is)+SS(ll,2,is))*ERALFM
          POT(2,INT,is)=ksign*(ss(ll,1,is)-SS(LL,2,is))*ERBETM
          POT(3,INT,is)=(SS(LL,3,is)+ss(ll,4,is))*ERALFA
          POT(4,INT,is)=ksign*(ss(ll,3,is)-SS(LL,4,is))*ERBETA
         end do
        end if
 30   CONTINUE
 
C *** SOURCE CONTRIBUTION

      call soufld(3)

C *** RECEIVERS IN TRANSVERSILY ISOTROPIC LAYERS
cvd$  permutation(NRPNT)
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO 40 J=1,NUMTR(4)
       INT=NRPNT(J,4)
       LL=LAY(INT)      
       ZZ=Z(INT)        
        IF (LL.NE.1) THEN
         ERALFM=CEXPT(-ZZ*ALFA(LL))                 
         ERBETM=CEXPT(-ZZ*BETA(LL))            
        ELSE
         ERALFM=RNUL
         ERBETM=RNUL
        END IF
        IF (LL.NE.NUML) THEN
         ERALFA=CEXPT((ZZ-THICK(LL))*ALFA(LL))
         ERBETA=CEXPT((ZZ-THICK(LL))*BETA(LL))
        ELSE
         ERALFA=RNUL
         ERBETA=RNUL
        END IF
        do is=1,nsrow
         POT(1,INT,is)=SS(LL,1,is)*ERALFM
         POT(2,INT,is)=SS(LL,2,is)*ERBETM
         POT(3,INT,is)=SS(LL,3,is)*ERALFA
         POT(4,INT,is)=SS(LL,4,is)*ERBETA
        end do
 40   continue
c
c     source contributions in TI media
c
      call soufld(4)

Cms
Cms *** recievers in porous sediment layers
Cms
cvd$  permutation(NRPNT)
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      do J=1,NUMTR(5)
       INT=NRPNT(J,5)
       LL=LAY(INT)      
       ZZ=Z(INT)        
        IF (LL.NE.1) THEN
         ERALFM=CEXPT(-ZZ*ALFA(LL))                 
         ERBETM=CEXPT(-ZZ*BETA(LL)) 
         ERGAMM=CEXPT(-ZZ*GAMMA(LL))
         if (debug) then
          write(6,*) 'zz=',zz
          write(6,*) 'alfa=',alfa(ll)
          write(6,*) 'expa=',eralfm
          write(6,*) 'beta=',beta(ll)
          write(6,*) 'expb=',erbetm
         end if
        ELSE
         ERALFM=RNUL
         ERBETM=RNUL
         ERGAMM=RNUL
        END IF
        IF (LL.NE.NUML) THEN
         ERALFA=CEXPT((ZZ-THICK(LL))*ALFA(LL))
         ERBETA=CEXPT((ZZ-THICK(LL))*BETA(LL))
         ERGAMA=CEXPT((ZZ-THICK(LL))*GAMMA(LL))
        ELSE
         ERALFA=RNUL
         ERBETA=RNUL
         ERGAMA=RNUL
        END IF
        do is=1,nsrow
         POT(1,INT,is)=SS(LL,1,is)*ERALFM
         POT(2,INT,is)=SS(LL,2,is)*ERBETM
         POT(3,INT,is)=SS(LL,3,is)*ERALFA
         POT(4,INT,is)=SS(LL,4,is)*ERBETA
         POT(5,INT,is)=SS(LL,5,is)*ERGAMM
         POT(6,INT,is)=SS(LL,6,is)*ERGAMA
        end do
      end do

c
c     source contributions in porous media
c
      call soufld(5)

      RETURN           
      END              

      SUBROUTINE KERNEL(CKERN)        
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
Cms
Cms  Modified to include recievers in porous sediment layers
Cms  Does NOT include sources in sediment layers
Cms
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX CKERN(IR,NPAR,nsrow)
      COMPLEX ERALFA,ERBETA,ERALFM,ERBETM 
Cms
Cms  Constants CC5 and CC6 added for sediment layers
Cms
      COMPLEX CC,CC1,CC2,CC3,CC4,CC5,CC6

      logical largek(nla)
      common /kflags/ largek,ksign
c      if (debug) write(6,*) '>>> Entering KERNEL <<<'
      call vclr(ckern,1,2*ir*npar*nsrow)
C
C *** RECEIVERS IN ISOVELOCITY FLUID LAYERS
cvd$  permutation(NUMTR)
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      DO J=1,NUMTR(1)
       INT=NRPNT(J,1)
       LL=LAY(INT)      
       do is=1,nsrow
        CKERN(INT,1,is)=-CON1(LL)*(POT(1,INT,is)+POT(3,INT,is))
        CKERN(INT,2,is)=ALFA(LL)*(-POT(1,INT,is)+POT(3,INT,is))
        CKERN(INT,3,is)=-WVNO*(POT(1,INT,is)+POT(3,INT,is))
        CKERN(INT,4,is)=-CKERN(int,3,is)
c For fluids radial stress is replaced by tube wave moment (kurkjian 94) 
        CKERN(INT,5,is)=CKERN(int,1,is)/(ai*ak(LL,1))
c Bulk pressure
        ckern(int,6,is)=-ckern(int,1,is)
       end do
      end do
C
C     AIRY SOLUTION IMPLEMENTED 840907
C
C *** RECEIVERS IN non-ISOVELOCITY FLUID LAYERS
cvd$  permutation(NRPNT)
cvd$  cncall
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
cvd$  select(concur)
      do J=1,NUMTR(2)
       INT=NRPNT(J,2)
       LL=LAY(INT)      
       do is=1,nsrow
        CKERN(INT,1,is)=-CON1(LL)*(POT(1,INT,is)+POT(3,INT,is))
        CKERN(INT,2,is)=-ACO(LL)*(POT(2,INT,is)+POT(4,INT,is))
        CKERN(INT,3,is)=-WVNO*(POT(1,INT,is)+POT(3,INT,is))          
        CKERN(INT,4,is)=-CKERN(int,3,is)
        CKERN(INT,5,is)=CKERN(int,1,is)/(ai*ak(ll,1))
c Bulk pressure
        ckern(int,6,is)=-ckern(int,1,is)
       end do
      end do
C *** RECEIVERS IN SOLID LAYERS
C *** SOURCE TERMS
cvd$  permutation(NRPNT)
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      do J=1,NUMTR(3)
       INT=NRPNT(J,3)
       LL=LAY(INT)
       do is=1,nsrow
        CC1=POT(1,INT,is)
        CC2=POT(2,INT,is)
        CC3=POT(3,INT,is)
        CC4=POT(4,INT,is)      
        if (iout(1).gt.0) then
         CKERN(INT,1,is)=(CON2(LL)*CC1-coN4(LL)*CC2)
     &              +(con2(ll)*cc3+con4(ll)*cc4)
        end if
        if (iout(2).gt.0) then
         CKERN(INT,2,is)=(ALFA(LL)*(-CC1)+WVNO*CC2)
     &              +(alfa(ll)*cc3+wvno*cc4)
        end if

        if (iout(3).gt.0) then
         CKERN(INT,3,is)=(-WVNO*CC1+BETA(LL)*CC2)
     &              +(-wvno*cc3-beta(ll)*cc4)
c >>> transverse displacement
         ckern(int,4,is)=-ckern(int,3,is)
        end if
c >>> radial stress
        if (iout(5).gt.0) then
         CKERN(INT,5,is)=(-CON5(LL)*CC1+coN4(LL)*CC2)
     &              +(-con5(ll)*cc3-con4(ll)*cc4)
        end if
c >>> bulk pressure
c 010827 HS changed to pressure from stress
        if (iout(6).gt.0) then
c         ckern(int,6,is)=-con6(ll)*(cc1+cc3)
         ckern(int,6,is)= con6(ll)*(cc1+cc3)
        end if
c >>> Stress equivalent of shear potential
        if (iout(7).gt.0) then
         ckern(int,7,is)=-con1(ll)*ak2(ll,2)*(cc2+cc4)
        end if
       end do
      end do
C *** RECEIVERS IN TRANSVERSILY ISOTROPIC LAYERS
cvd$  permutation(NRPNT)
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      do J=1,NUMTR(4)
       INT=NRPNT(J,4)
       LL=LAY(INT)      
       do is=1,nsrow
        CC1=POT(1,INT,is)
        CC2=POT(2,INT,is)
        CC3=POT(3,INT,is)
        CC4=POT(4,INT,is)
        if (iout(1).gt.0) then
         CKERN(INT,1,is)=CON1(LL)*
     &       (ANSTD(13,LL)*CC1+ANSTD(14,LL)*CC2+
     &        ANSTD(17,LL)*CC3+ANSTD(18,LL)*CC4)
        end if
        if (iout(2).gt.0) then
         CKERN(INT,2,is)=(ANSTD(7,LL)*CC1+ANSTD(8,LL)*CC2+
     &        ANSTD(11,LL)*CC3+ANSTD(12,LL)*CC4)
        end if
        if (iout(3).gt.0) then
         CKERN(INT,3,is)=(ANSTD(5,LL)*CC1+ANSTD(6,LL)*CC2+
     &        ANSTD(9,LL)*CC3+ANSTD(10,LL)*CC4)
         CKERN(INT,4,is)=-CKERN(int,3,is)
        end if
c *** radial stress *********************
        if (iout(5).gt.0) then
         CKERN(int,5,is)=CON1(LL)*
     &       (ANSTD(29,LL)*CC1+ANSTD(30,LL)*CC2+
     &        ANSTD(27,LL)*CC3+ANSTD(28,LL)*CC4)
        end if
c *** Bulk stress *********************
c >>> bulk pressure
c 010827 HS changed to pressure from stress
        if (iout(6).gt.0) then
         CKERN(int,6,is)= - CON1(LL)*
     &       (ANSTD(37,LL)*CC1+ANSTD(38,LL)*CC2+
     &        ANSTD(35,LL)*CC3+ANSTD(36,LL)*CC4)
        end if
c *** Shear stress *********************
        if (iout(7).gt.0) then
         CKERN(int,7,is)=CON1(LL)*
     &       (ANSTD(15,LL)*CC1+ANSTD(16,LL)*CC2+
     &        ANSTD(19,LL)*CC3+ANSTD(20,LL)*CC4)
        end if
       end do
      end do

Cms
Cms *** recievers in porous sediment layers
Cms
cvd$  permutation(NRPNT)
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      do J=1,NUMTR(5)
       INT=NRPNT(J,5)
       LL=LAY(INT)
       do is=1,nsrow
        CC1=POT(1,INT,is)
        CC2=POT(2,INT,is)
        CC3=POT(3,INT,is)
        CC4=POT(4,INT,is)      
        CC5=POT(5,INT,is)
        CC6=POT(6,INT,is)      
        CKERN(INT,1,is) = CON2(LL)*(CC1 + CC3)
     &     + CON3(LL)*(CC2 + CC4) + CON4(LL)*(-CC5 + CC6)
        CKERN(INT,2,is) = ALFA(LL)*(-CC1 + CC3)
     &     + BETA(LL)*(-CC2 + CC4) + WVNO*(CC5 + CC6)
c >>> here morrie had WVN0 instead of WVNO
        CKERN(INT,3,is) = -WVNO*(CC1 + CC2 + CC3 + CC4)
     &     + GAMMA(LL)*(CC5 - CC6)
        CKERN(INT,4,is) = -CKERN(INT,3,is)
        CKERN(INT,5,is) = CON10(LL)*(CC1 + CC3)
     &     + CON11(LL)*(CC2 + CC4) + CON4(LL)*(CC5 - CC6)
Cms
Cms  option K (bulk stress) yields negative pore fluid pressure in
Cms  sediment layers.
Cms
c >>> bulk pressure
c 010827 HS changed to pressure from stress
        CKERN(INT,6,is) = -(-CON8(LL)*(CC1 + CC3)
     &     - CON9(LL)*(CC2 + CC4))
        CKERN(INT,7,is) = CON5(LL)*(CC1 - CC3)
     &     + CON6(LL)*(CC2 - CC4) - CON7(LL)*(CC5 + CC6)
       end do
      end do

C *** CONVERT DISPLACEMENTS TO VELOCITIES
      do is=1,nsrow
       do J=1,IR
        CKERN(J,1,is)=CPFAC  * CKERN(J,1,is)
        CKERN(J,2,is)=CWUFAC * CKERN(J,2,is)
        CKERN(J,3,is)=CWUFAC * CKERN(J,3,is)
        CKERN(J,4,is)=CWUFAC * CKERN(J,4,is)
        CKERN(J,5,is)=CPFAC  * CKERN(J,5,is)
        ckern(j,6,is)=cpfac  * ckern(j,6,is)
        ckern(j,7,is)=cpfac  * ckern(j,7,is)
       end do
      end do

      RETURN           
      END              
      SUBROUTINE KERDEC(CKERN)        
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
C     DETERMINES THE DECOMPOSED WAVEFIELD KERNELS.
C     ON EXIT, THE PARAMETERS ARE PLACED IN ARRAY CKERN(I,J,K)
C     AS FOLLOWS:
C
C     CKERN(I,1,K)	NORMAL STRESS, RECEIVER I.
C     CKERN(I,2,K)      VERTICAL PARTICLE VELOCITY, RECEIVER I.
C     CKERN(I,3,K)	HORIZONTAL PARTICLE VELOCITY, RECEIVER I.
C
C     CKERN(I,J,1)	TOTAL KERNEL
C     CKERN(I,J,2)	DOWN-GOING COMPRESSIONAL WAVES ONLY
C     CKERN(I,J,3) 	DOWN-GOING SHEAR WAVES ONLY
C     CKERN(I,J,4)	UP-GOING COMPRESSIONAL WAVES ONLY
C     CKERN(I,J,5)	UP-GOING SHEAR WAVES ONLY
C 
Cms
Cms  Modified to include recievers in porous sediment layers
Cms  Does NOT include sources in sediment layers
Cms
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX CKERN(IR,npar,5,nsrow)
      COMPLEX ERALFA,ERBETA,ERALFM,ERBETM 
      COMPLEX CC,CC1,CC2,CC3,CC4,CC5,CC6
c
      CALL VCLR(CKERN,1,10*npar*ir*nsrow)
C *** RECEIVERS IN ISOVELOCITY FLUID LAYERS
cvd$  permutation(NUMTR)
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      do J=1,NUMTR(1)
       INT=NRPNT(J,1)
       LL=LAY(INT)      
       do is=1,nsrow
        CC1=POT(1,INT,is)
        CC3=POT(3,INT,is)
        CKERN(INT,1,2,is)=-CON1(LL)*CC1
        CKERN(INT,1,4,is)=-CON1(LL)*CC3
        CKERN(INT,2,2,is)=-ALFA(LL)*CC1
        CKERN(INT,2,4,is)= ALFA(LL)*CC3
        CKERN(INT,3,2,is)=-WVNO*CC1
        CKERN(INT,3,4,is)=-WVNO*CC3
        do ipp=2,5
         ckern(int,4,ipp,is)=-ckern(int,3,ipp,is)
         ckern(int,5,ipp,is)=ckern(int,1,ipp,is)/(ai*ak(LL,1))
         ckern(int,6,ipp,is)=-ckern(int,1,ipp,is)
        end do
       end do
      end do
C
C     AIRY SOLUTION IMPLEMENTED 840907
C
C *** RECEIVERS IN non-ISOVELOCITY FLUID LAYERS
cvd$  permutation(NUMTR)
      do J=1,NUMTR(2)
       INT=NRPNT(J,2)
       LL=LAY(INT)      
       do is=1,nsrow
        CKERN(INT,1,2,is)=-CON1(LL)*POT(1,INT,is)
        CKERN(INT,1,4,is)=-CON1(LL)*POT(3,INT,is)
        CKERN(INT,2,2,is)=-ACO(LL)*POT(2,INT,is)
        CKERN(INT,2,4,is)=-ACO(LL)*POT(4,INT,is)
        CKERN(INT,3,2,is)=-WVNO*POT(1,INT,is)
        CKERN(INT,3,4,is)=-WVNO*POT(3,INT,is)          
        do ipp=2,5
         ckern(int,4,ipp,is)=-ckern(int,3,ipp,is)
         ckern(int,5,ipp,is)=ckern(int,1,ipp,is)/(ai*ak(LL,1))
         ckern(int,6,ipp,is)=-ckern(int,1,ipp,is)
        end do
       end do
      end do

C *** RECEIVERS IN SOLID LAYERS
cvd$  permutation(NRPNT)
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      do J=1,NUMTR(3)
       INT=NRPNT(J,3)
       LL=LAY(INT)      
       do is=1,nsrow
        CC1=POT(1,INT,is)
        CC2=POT(2,INT,is)
        CC3=POT(3,INT,is)
        CC4=POT(4,INT,is)
        CKERN(INT,1,2,is)=CON2(LL)*CC1
        CKERN(INT,1,4,is)=CON2(LL)*CC3
        CKERN(INT,1,3,is)=-CON4(LL)*CC2
        CKERN(INT,1,5,is)=CON4(LL)*CC4
        CKERN(INT,2,2,is)=-ALFA(LL)*CC1
        CKERN(INT,2,4,is)=ALFA(LL)*CC3
        CKERN(INT,2,3,is)=WVNO*CC2
        CKERN(INT,2,5,is)=WVNO*CC4
        CKERN(INT,3,2,is)=-WVNO*CC1
        CKERN(INT,3,4,is)=-WVNO*CC3
        CKERN(INT,3,3,is)=BETA(LL)*CC2
        CKERN(INT,3,5,is)=-BETA(LL)*CC4
c >>> transverse displacement and stress
        do ipp=2,5
         ckern(int,4,ipp,is)=-ckern(int,3,ipp,is)
        end do
        CKERN(INT,5,2,is)=-CON5(LL)*CC1
        ckern(int,5,3,is)=coN4(LL)*CC2
        ckern(int,5,4,is)=-con5(ll)*cc3
        ckern(int,5,5,is)=-con4(ll)*cc4
        ckern(int,6,2,is)=con6(ll)*cc1
        ckern(int,6,4,is)=con6(ll)*cc3
c >>> Stress equivalent of shear potential
        ckern(int,7,3,is)=con1(ll)*ak2(ll,2)*cc2
        ckern(int,7,5,is)=con1(ll)*ak2(ll,2)*cc4
       end do
      end do
C *** RECEIVERS IN TRANSVERSILY ISOTROPIC LAYERS
cvd$  permutation(NRPNT)
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      do J=1,NUMTR(4)
       INT=NRPNT(J,4)
       LL=LAY(INT)      
       do is=1,nsrow
        CC1=POT(1,INT,is)
        CC2=POT(2,INT,is)
        CC3=POT(3,INT,is)
        CC4=POT(4,INT,is)
        CKERN(INT,1,2,is)=CON1(LL)*ANSTD(13,LL)*CC1
        CKERN(INT,1,3,is)=CON1(LL)*ANSTD(14,LL)*CC2
        CKERN(INT,1,4,is)=CON1(LL)*ANSTD(17,LL)*CC3
        CKERN(INT,1,5,is)=CON1(LL)*ANSTD(18,LL)*CC4
        CKERN(INT,2,2,is)=ANSTD(7,LL)*CC1
        CKERN(INT,2,3,is)=ANSTD(8,LL)*CC2
        CKERN(INT,2,4,is)=ANSTD(11,LL)*CC3
        CKERN(INT,2,5,is)=ANSTD(12,LL)*CC4
        CKERN(INT,3,2,is)=ANSTD(5,LL)*CC1
        CKERN(INT,3,3,is)=ANSTD(6,LL)*CC2
        CKERN(INT,3,4,is)=ANSTD(9,LL)*CC3
        CKERN(INT,3,5,is)=ANSTD(10,LL)*CC4
        do ipp=2,5
         ckern(int,4,ipp,is)=-ckern(int,3,ipp,is)
        end do
c *** radial stress *********************
        CKERN(int,5,2,is)=CON1(LL)*ANSTD(29,LL)*CC1
        CKERN(int,5,3,is)=CON1(LL)*ANSTD(30,LL)*CC2
        CKERN(int,5,4,is)=CON1(LL)*ANSTD(27,LL)*CC3
        CKERN(int,5,5,is)=CON1(LL)*ANSTD(28,LL)*CC4
c *** Bulk stress *********************
        CKERN(int,6,2,is)=-CON1(LL)*ANSTD(37,LL)*CC1
        CKERN(int,6,3,is)=-CON1(LL)*ANSTD(38,LL)*CC2
        CKERN(int,6,4,is)=-CON1(LL)*ANSTD(35,LL)*CC3
        CKERN(int,6,5,is)=-CON1(LL)*ANSTD(36,LL)*CC4
       end do
      end do
Cms
Cms *** recievers in porous sediment layers
Cms
cvd$  permutation(NRPNT)
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
      do J=1,NUMTR(5)
       INT=NRPNT(J,5)
       LL=LAY(INT)
       do is=1,nsrow
        CC1=POT(1,INT,is)
        CC2=POT(2,INT,is)
        CC3=POT(3,INT,is)
        CC4=POT(4,INT,is)      
        CC5=POT(5,INT,is)
        CC6=POT(6,INT,is)      
        CKERN(INT,1,2,is) = CON2(LL)*CC1 + CON3(LL)*CC2
        CKERN(INT,1,4,is) = CON2(LL)*CC3 + CON3(LL)*CC4
        CKERN(INT,1,3,is) = -CON4(LL)*CC5
        CKERN(INT,1,5,is) = CON4(LL)*CC6
        CKERN(INT,2,2,is) = -ALFA(LL)*CC1 - BETA(LL)*CC2
        CKERN(INT,2,4,is) = ALFA(LL)*CC3 + BETA(LL)*CC4
        CKERN(INT,2,3,is) = WVNO*CC5
        CKERN(INT,2,5,is) = WVNO*CC6
        CKERN(INT,3,2,is) = -WVNO*(CC1 + CC2)
        CKERN(INT,3,4,is) = -WVNO*(CC3 + CC4)
        CKERN(INT,3,3,is) = GAMMA(LL)*CC5
        CKERN(INT,3,5,is) = -GAMMA(LL)*CC6
        CKERN(INT,7,2,is) = CON5(LL)*CC1 + CON6(LL)*CC2
        CKERN(INT,7,4,is) = -CON5(LL)*CC3 - CON6(LL)*CC4
        CKERN(INT,7,3,is) = -CON7(LL)*CC5
        CKERN(INT,7,5,is) = -CON7(LL)*CC6
        do IDUM=2,5
         CKERN(INT,4,IDUM,is) = -CKERN(INT,3,IDUM,is)
        end do
        CKERN(INT,5,2,is) = CON10(LL)*CC1 + CON11(LL)*CC2
        CKERN(INT,5,4,is) = CON10(LL)*CC3 + CON11(LL)*CC4
        CKERN(INT,5,3,is) = CON4(LL)*CC5
        CKERN(INT,5,5,is) = -CON4(LL)*CC6
Cms
Cms  option K yields pore fluid pressure in
Cms  sediment layers.
Cms
c        CKERN(INT,6,2,is) = -CON8(LL)*CC1 - CON9(LL)*CC2
c        CKERN(INT,6,4,is) = -CON8(LL)*CC3 - CON9(LL)*CC4
        CKERN(INT,6,2,is) = CON8(LL)*CC1
        CKERN(INT,6,4,is) = CON8(LL)*CC3
Chs 970626 Shear wave component rpresenting slow wave
        CKERN(INT,6,3,is) = CON9(LL)*CC2
        CKERN(INT,6,5,is) = CON9(LL)*CC4


        CKERN(INT,7,2,is) = CON5(LL)*CC1 + CON6(LL)*CC2
        CKERN(INT,7,4,is) = -CON5(LL)*CC3 - CON6(LL)*CC4
        CKERN(INT,7,3,is) = -CON7(LL)*CC5
        CKERN(INT,7,5,is) = -CON7(LL)*CC6
       end do
      end do

C *** CONVERT DISPLACEMENTS TO VELOCITIES
      do is=1,nsrow
       do ipp=2,5
        do J=1,IR
         CKERN(J,1,ipp,is)=CPFAC  * CKERN(J,1,ipp,is)
         CKERN(J,2,ipp,is)=CWUFAC * CKERN(J,2,ipp,is)
         CKERN(J,3,ipp,is)=CWUFAC * CKERN(J,3,ipp,is)
         CKERN(J,4,ipp,is)=CWUFAC * CKERN(J,4,ipp,is)
         CKERN(J,5,ipp,is)=CPFAC  * CKERN(J,5,ipp,is)
         CKERN(J,6,ipp,is)=CPFAC  * CKERN(J,6,ipp,is)
         CKERN(J,7,ipp,is)=CPFAC  * CKERN(J,7,ipp,is)
        end do
       end do
      end do
C *** TOTAL FIELD
      do is=1,nsrow
       do ipp=2,5
        do iip=1,npar
         do J=1,IR
          CKERN(J,IIP,1,is)=CKERN(J,IIP,1,is)+CKERN(J,IIP,ipp,is)
         end do
        end do
       end do
      end do
      RETURN           
      END              
      SUBROUTINE SOLVE                  
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
c      if (debug) write(6,*) '>>> Entering SOLVE <<<'
      CALL CVIMOV(ALO,INDA,1,WORK1,2,NNA)
      CALL CVFILL(CNUL,WORK2,2,NNB)
      CALL CVMOVI(WORK1,2,INDB,1,WORK2,NNA)
      do is=1,nsrow
       CALL CVIMOV(R(1,1,is),INDR,1,RHS(1+(is-1)*neq),2,NEQ)
      end do
c >>>
c >>> normilization of determinant
c >>>
c      if (determ) then
c       prod=RONE
c       do 10 i=1,neq
c        sum=rnul
c        do 5 j=1,2*ibw+1
c         sum=sum+cabs(work2(i+(j-1)*neq))
c 5       continue
c        prod=prod*sum
c 10     continue
c      end if
      EPS=1E-33
      CALL CBGEMR(WORK2,RHS,NEQ,NEQ,nsrow,IBW,EPS)
      IERR=EPS
      IF (IERR.NE.0) RETURN
      do is=1,nsrow
       CALL CVMOVI(RHS(1+(is-1)*neq),2,INDS,1,SS(1,1,is),NEQ)
      end do
c
c *** compute determinant inverse
C
      IF (DETERM.or.debug) THEN
       DETMNT=RONE
       I1=IBW*NEQ+1
       DO 1000 I=I1,I1+NEQ-1
        DETMNT=DETMNT*(real(WORK2(I))**2 + rimag(work2(i))**2)
 1000  CONTINUE
        if (debug) write(6,*) 'detmnt=', detmnt
      END IF        
      RETURN        
      END           

      SUBROUTINE SOUDGM(ltp)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     SOUDGM : OASES VERSION
C
C     r.h.s. contribution from sources in solid media
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      complex monopole,dipole
C *** SOURCE TERMS
c      write(6,*) 'soudgm, laytyp=',ltp
      if (srctyp.eq.98.or.srctyp.eq.97) then
        do jj=1,numts(ltp)
         i=nspnt(jj,ltp)
         call soldis(i,sval(i,1),sval(i,2))
        end do
      else if (srctyp.eq.99) then
       if (nstrf.eq.3.and.isprm(1).eq.10.and.isprm(2).eq.11.and.
     &     isprm(3).eq.12) then
        do jj=1,numts(ltp)
         i=nspnt(jj,ltp)
         call axmom(i,sval(i,1)+sval(i,2),sval(i,1))
c          call axexpl(i,sval(i,1))
         aaa=abs(real(sval(i,3))) + abs(rimag(sval(i,3)))
         if (aaa.gt.1E-20) call vfor(i,sval(i,3))
        end do
       else if (nstrf.eq.2.and.isprm(1).eq.10.and.isprm(2).eq.11) then
        do jj=1,numts(ltp)
         i=nspnt(jj,ltp)
         call axmom(i,sval(i,1)+sval(i,2),sval(i,1))
        end do
       else if (nstrf.eq.2.and.isprm(1).eq.1.and.isprm(2).eq.5) then
        do jj=1,numts(ltp)
         i=nspnt(jj,ltp)
         call axmom(i,pcorr*(sval(i,1)+sval(i,2)),pcorr*sval(i,2))
        end do
       else if (nstrf.eq.1.and.(isprm(1).eq.13.or.isprm(1).eq.5)) then
c Tube wave seismic moment source (kurkjian 94)
        do jj=1,numts(ltp)
         i=nspnt(jj,ltp)
         ll=lays(i)
         monopole=(ak2(ll,2)/ak2(ll,1))*sval(i,1)
         dipole=-2e0*sval(i,1)
         call axmom(i,monopole+dipole,monopole)
        end do
       else
        do jj=1,numts(ltp)
         i=nspnt(jj,ltp)
         do j=1,nstrf
          if (isprm(j).eq.10) then
           call axmom(i,sval(i,j),sval(i,j))
          else if (isprm(j).eq.11) then
           call axmom(i,sval(i,j),cnul)
          else if (isprm(j).eq.12) then
           call vfor(i,sval(i,j))
          else if (isprm(j).eq.13) then
c Tube wave seismic moment source (kurkjian 94)
           ll=lays(i)
           monopole=(ak2(ll,2)/ak2(ll,1))*sval(i,1)
           dipole=-2e0*sval(i,1)
           call axmom(i,monopole+dipole,monopole)
          else
           stop ' >>> SOUDGM: Unknown source type in TRF file <<<'
          end if
         end do
        end do
       end if
      else
cvd$  novector
CDIR$ NOVECTOR
cvd$  noconcur
       DO J=1,NUMTS(ltp)
        I=NSPNT(J,ltp)
        IF (srctyp.eq.1) THEN
c         call axexpl(i,cmplx(4*pi,rnul))
         call axexpl(i,cmplx(RONE,rnul))
        else if (srctyp.eq.2) then
         call vfor(i,cmplx(RONE,rnul))
        else if (srctyp.eq.3) then
         call hfor(i,cmplx(RONE,rnul))
        else if (srctyp.eq.4) then
         call soldip(i,rone,rdelta)
        ELSE
         call axmom(i,cmplx(rone,rnul),cmplx(rone,rnul))
        END IF
       end do
      end if
      return
      end

      SUBROUTINE SOUFLD(ltp)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     SOUFLD : OASES VERSION
C
C     Field contributions from sources in solid media
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      complex monopole,dipole
C *** SOURCE TERMS
      if (srctyp.eq.98.or.srctyp.eq.97) then
        do jj=1,numts(ltp)
         i=nspnt(jj,ltp)
         call scndis(i,sval(i,1),sval(i,2))
        end do
      else if (srctyp.eq.99) then
       if (nstrf.eq.3.and.isprm(1).eq.10.and.isprm(2).eq.11.and.
     &     isprm(3).eq.12) then
        do jj=1,numts(ltp)
         i=nspnt(jj,ltp)
         call saxmom(i,sval(i,1)+sval(i,2),sval(i,1))
c          call saxexpl(i,sval(i,1))
         aaa=abs(real(sval(i,3))) + abs(rimag(sval(i,3)))
         if (aaa.gt.1E-20) call svfor(i,sval(i,3))
        end do
       else if (nstrf.eq.2.and.isprm(1).eq.10.and.isprm(2).eq.11) then
        do jj=1,numts(ltp)
         i=nspnt(jj,ltp)
         call saxmom(i,sval(i,1)+sval(i,2),sval(i,1))
        end do
       else if (nstrf.eq.2.and.isprm(1).eq.1.and.isprm(2).eq.5) then
        do jj=1,numts(ltp)
         i=nspnt(jj,ltp)
         call saxmom(i,pcorr*(sval(i,1)+sval(i,2)),pcorr*sval(i,2))
        end do
       else if (nstrf.eq.1.and.(isprm(1).eq.13.or.isprm(1).eq.5)) then
c Tube wave seismic moment source (kurkjian 94)
        do jj=1,numts(ltp)
         i=nspnt(jj,ltp)
         ll=lays(i)
         monopole=(ak2(ll,2)/ak2(ll,1))*sval(i,1)
         dipole=-2e0*sval(i,1)
         call saxmom(i,monopole+dipole,monopole)
        end do
       else
        do jj=1,numts(ltp)
         i=nspnt(jj,ltp)
         do j=1,nstrf
          if (isprm(j).eq.10) then
           call saxmom(i,sval(i,j),sval(i,j))
          else if (isprm(j).eq.11) then
           call saxmom(i,sval(i,j),cnul)
          else if (isprm(j).eq.12) then
           call svfor(i,sval(i,j))
          else if (isprm(j).eq.13) then
c Tube wave seismic moment source (kurkjian 94)
           ll=lays(i)
           monopole=(ak2(ll,2)/ak2(ll,1))*sval(i,1)
           dipole=-2e0*sval(i,1)
           call saxmom(i,monopole+dipole,monopole)
          else
           stop ' >>> WFIELD: Unknown source type in TRF file <<<'
          end if
         end do
        end do
       end if
      else
cvd$  permutation(NRPNT)
cvd$  concur
cvd$  nodepchk
C$DIR No_Recurrence
CDIR$ IVDEP
       DO J=1,NUMTs(ltp)
        I=NSPNT(J,ltp)
        IF (srctyp.eq.1) THEN
c         call saxexpl(i,cmplx(4*pi,rnul))
         call saxexpl(i,cmplx(RONE,rnul))
        ELSE if (srctyp.eq.2) then
         call svfor(i,cmplx(RONE,rnul))
        ELSE if (srctyp.eq.3) then
         call shfor(i,cmplx(RONE,rnul))
        ELSE if (srctyp.eq.4) then
         call scndip(i,RONE,rdelta)
        else
         call saxmom(i,cmplx(RONE,rnul),cmplx(RONE,rnul))
        end if
       end do
      end if
      return
      end 

      SUBROUTINE AXEXPL(i,expmom)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     AXEXPL : OASES VERSION
C
C     r.h.s. contribution from explosive source number i
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX ccp,expmom,cc,alf
c      if (debug) write(6,*) '>>> Entering AXEXPL <<<'
C
      LN=LAYS(I)
      is=1

      if (laytyp(ln).eq.5) then
       if (fs_switch(ln)) then
        CCP=BETINV(ln)*cphfac(i)*expmom
        alf=beta(ln)
        iwp=2
       else
        CCP=ALFINV(ln)*cphfac(i)*expmom
        alf=alfa(ln)
        iwp=1
       end if
      else if (laytyp(ln).eq.4) then
       alf=alfa(ln)
       iwp=1
       ccp=cphfac(i)*expmom/anstd(11,ln)
      else 
       alf=alfa(ln)
       iwp=1
       ccp=cphfac(i)*expmom*alfinv(ln)
      end if

      if (ln.gt.1) then
c >>> Upper interface potentials
         cc=-abs(v(ln,1)-sdc(i))*ALF
         PUIN(iwp,LN-1,is)=PUIN(iwp,LN-1,is)+
     &           ccp*CEXPT(cc)
      end if
c >>> Lower interface potentials
      if (ln.lt.NUML) then
         cc=-abs(v(ln+1,1)-sdc(i))*ALF
         POIN(iwp,LN,is)=POIN(iwp,LN,is)+
     &           ccp*CEXPT(cc)
      end if
      RETURN
      END

      SUBROUTINE VFOR(i,force)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     vfor : OASES VERSION
C
C     r.h.s. contribution from vertical force number i
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX ccp,ccs,force,alf,bet
c      if (debug) write(6,*) '>>> Entering VFOR <<<'
C
      LN=LAYS(I)
      is=1
      if (laytyp(ln).eq.5) then
       if (fs_switch(ln)) then
        alf=beta(ln)
        bet=gamma(ln)
        iwp=2
        iws=3
        ccp=cphfac(i)*force
        ccs=cphfac(i)*force*wvno/gamma(ln)
       else
        alf=alfa(ln)
        bet=gamma(ln)
        iwp=1
        iws=3
        ccp=cphfac(i)*force
        ccs=cphfac(i)*force*wvno/gamma(ln)
       end if
      else if (laytyp(ln).eq.4) then
       alf=alfa(ln)
       bet=beta(ln)
       iwp=1
       iws=2
       ccp=cphfac(i)*force*alfa(ln)/anstd(11,ln)
       ccs=cphfac(i)*force*s2*betinv(ln)/anstd(12,ln)
      else 
       alf=alfa(ln)
       bet=beta(ln)
       iwp=1
       iws=2
       ccp=cphfac(i)*force
       ccs=cphfac(i)*force*wvno*betinv(ln)
      end if

      if (ln.gt.1) then
c >>> Upper interface potentials
         PUIN(iwp,LN-1,is)=PUIN(iwp,LN-1,is)-
     &           ccp*CEXPT(-ABS(V(LN,1)-SDC(I))*ALF)
         PUIN(iws,LN-1,is)=PUIN(iws,LN-1,is)+
     &           ccs*CEXPT(-ABS(V(LN,1)-SDC(I))*BET)
      end if
c >>> Lower interface potentials
      if (ln.lt.NUML) then
         POIN(iwp,LN,is)=POIN(iwp,LN,is)+
     &           ccp*CEXPT(-ABS(V(LN+1,1)-SDC(I))*ALF)
         POIN(iws,LN,is)=POIN(iws,LN,is)+
     &           ccs*CEXPT(-ABS(V(LN+1,1)-SDC(I))*BET)
      end if
      RETURN
      END

      SUBROUTINE HFOR(i,force)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     hfor : OASES VERSION
C
C     r.h.s. contribution from in-plane horizontal force number i
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX ccp,ccs,force,alf,bet
c      if (debug) write(6,*) '>>> Entering HFOR <<<'
C
      LN=LAYS(I)
      is=1

      if (laytyp(ln).eq.5) then
       if (fs_switch(ln)) then
        alf=beta(ln)
        bet=gamma(ln)
        iwp=2
        iws=3
        ccp=ai*cphfac(i)*force*wvno*betinv(ln)
        ccs=ai*cphfac(i)*force
       else
        alf=alfa(ln)
        bet=gamma(ln)
        iwp=1
        iws=3
        ccp=ai*cphfac(i)*force*wvno*alfinv(ln)
        ccs=ai*cphfac(i)*force
       end if
      else if (laytyp(ln).eq.4) then
       alf=alfa(ln)
       bet=beta(ln)
       iwp=1
       iws=2
       ccp=ai*cphfac(i)*force*wvno/anstd(11,ln)
       ccs=ai*cphfac(i)*force*wvno/anstd(12,ln)
      else 
       alf=alfa(ln)
       bet=beta(ln)
       iwp=1
       iws=2
       ccp=ai*cphfac(i)*force*wvno*alfinv(ln)
       ccs=ai*cphfac(i)*force
      end if

      if (ln.gt.1) then
c >>> Upper interface potentials
         PUIN(iwp,LN-1,is)=PUIN(iwp,LN-1,is)+
     &           ccp*CEXPT(-ABS(V(LN,1)-SDC(I))*ALF)
         PUIN(iws,LN-1,is)=PUIN(iws,LN-1,is)-
     &           ccs*CEXPT(-ABS(V(LN,1)-SDC(I))*BET)
      end if
c >>> Lower interface potentials
      if (ln.lt.NUML) then
         POIN(iwp,LN,is)=POIN(iwp,LN,is)+
     &           ccp*CEXPT(-ABS(V(LN+1,1)-SDC(I))*ALF)
         POIN(iws,LN,is)=POIN(iws,LN,is)+
     &           ccs*CEXPT(-ABS(V(LN+1,1)-SDC(I))*BET)
      end if
      RETURN
      END

      SUBROUTINE AXMOM(i,vermom,hormom)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     axmom: OASES VERSION
C
C     r.h.s. contribution from moment source number i
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX vermom,hormom
      COMPLEX ccp,ccs,alf,bet,hsq
c      if (debug) write(6,*) '>>> Entering AXMOM <<<'
C
      LN=LAYS(I)
      is=1

      if (laytyp(ln).eq.5) then
       if (fs_switch(ln)) then
        alf=beta(ln)
        bet=gamma(ln)
        iwp=2
        iws=3
        ccp=cphfac(i)*betinv(ln)*
     &     (s2*(vermom-hormom) - ck2(ln)*(0.75*vermom+0.5*hormom))
        ccs=cphfac(i)*wvno*(hormom-vermom)
       else
        alf=alfa(ln)
        bet=gamma(ln)
        iwp=1
        iws=3
        ccp=cphfac(i)*alfinv(ln)*
     &     (s2*(vermom-hormom) - ck1(ln)*(0.75*vermom+0.5*hormom))
        ccs=cphfac(i)*wvno*(hormom-vermom)
       end if
      else if (laytyp(ln).eq.4) then
       alf=alfa(ln)
       bet=beta(ln)
       iwp=1
       iws=2
       hsq=(wvno-alfa(ln))*(wvno+alfa(ln))
       ccp=cphfac(i)* (s2*(vermom-hormom) - 
     &                 hsq*(0.75*vermom+0.5*hormom))/anstd(11,ln)
       ccs=cphfac(i)*s2*(hormom-vermom)/anstd(12,ln)
      else 
       alf=alfa(ln)
       bet=beta(ln)
       iwp=1
       iws=2
       ccp=cphfac(i)*alfinv(ln)*
     &     (s2*(vermom-hormom) - ak2(ln,1)*(0.75*vermom+0.5*hormom))
       ccs=cphfac(i)*wvno*(hormom-vermom)
      end if

      if (ln.gt.1) then
c >>> Upper interface potentials
         PUIN(iwp,LN-1,is)=PUIN(iwp,LN-1,is)+
     &           ccp*CEXPT(-ABS(V(LN,1)-SDC(I))*ALF)
         PUIN(iws,LN-1,is)=PUIN(iws,LN-1,is)+
     &           ccs*CEXPT(-ABS(V(LN,1)-SDC(I))*BET)
      end if
c >>> Lower interface potentials
      if (ln.lt.NUML) then
         POIN(iwp,LN,is)=POIN(iwp,LN,is)+
     &           ccp*CEXPT(-ABS(V(LN+1,1)-SDC(I))*ALF)
         POIN(iws,LN,is)=POIN(iws,LN,is)-
     &           ccs*CEXPT(-ABS(V(LN+1,1)-SDC(I))*BET)
      end if
      RETURN
      END

      SUBROUTINE saxexpl(i,expmom)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     SAXEXP : OASES VERSION
C             
C              MAY 15, 1988 MODIFIED BY JKIM
C              (1) SOURCES ALONG DEPTH TREATED SEPARATELY
C
C     IMPLEMENTING EXPLOSIVE SOURCE IN SOLID MEDIUM
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX CC,expmom,alf,ccp
c      if (debug) write(6,*) '>>> Entering SAXEXP <<<'
C
      LN=LAYS(I)
      is=1

      if (laytyp(ln).eq.5) then
       if (fs_switch(ln)) then
        CCP=BETINV(ln)*cphfac(i)*expmom
        alf=beta(ln)
        iwp=2
       else
        CCP=ALFINV(ln)*cphfac(i)*expmom
        alf=alfa(ln)
        iwp=1
       end if
      else if (laytyp(ln).eq.4) then
       alf=alfa(ln)
       iwp=1
       ccp=cphfac(i)*expmom/anstd(11,ln)
      else 
       alf=alfa(ln)
       iwp=1
       ccp=cphfac(i)*expmom*alfinv(ln)
      end if

      do IRCV=IFRCV(LN),ILRCV(LN)
       if (rdc(ircv).eq.sdc(i)) then
        pot(iwp,ircv,is)=pot(1,ircv,is)+0.5*ccp
        pot(iwp+2,ircv,is)=pot(3,ircv,is)+0.5*ccp
       else
        cc=-ABS(RDC(IRCV)-SDC(I))*ALFA(LN)
        cc=cexpt(cc)
        ISI=nint(SIGN(1.0,RDC(IRCV)-SDC(I)))
        iwavp=iwp+1-isi
        POT(iwavp,ircv,is)=POT(iwavp,ircv,is)+cc*ccp
       end if
      end do

      RETURN
      END

      SUBROUTINE SVFOR(i,force)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     Svfor : OASES VERSION
C
C     Field contribution from vertical force number i
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX ccp,ccs,force,alf,bet
c      if (debug) write(6,*) '>>> Entering SVFOR <<<'
C
      LN=LAYS(I)
      is=1

      if (laytyp(ln).eq.5) then
       if (fs_switch(ln)) then
        alf=beta(ln)
        bet=gamma(ln)
        iwp=2
        iws_b=5
        iws_a=6
        ccp=cphfac(i)*force
        ccs=cphfac(i)*force*wvno/gamma(ln)
       else
        alf=alfa(ln)
        bet=gamma(ln)
        iwp=1
        iws_b=5
        iws_a=6
        ccp=cphfac(i)*force
        ccs=cphfac(i)*force*wvno/gamma(ln)
       end if
      else if (laytyp(ln).eq.4) then
       alf=alfa(ln)
       bet=beta(ln)
       iwp=1
       iws_b=2
       iws_a=4
       ccp=cphfac(i)*force*alfa(ln)/anstd(11,ln)
       ccs=cphfac(i)*force*s2*betinv(ln)/anstd(12,ln)
      else 
       alf=alfa(ln)
       bet=beta(ln)
       iwp=1
       iws_b=2
       iws_a=4
       ccp=cphfac(i)*force
       ccs=cphfac(i)*force*wvno*betinv(ln)
      end if

      do IRCV=IFRCV(LN),ILRCV(LN)
       if (rdc(ircv).eq.sdc(i)) then
        pot(iwp,ircv,is)  =pot(1,ircv,is)+0.5*ccp
        pot(iws_b,ircv,is)=pot(iws_b,ircv,is)+0.5*ccs
        pot(iwp+2,ircv,is)=pot(iwp+2,ircv,is)-0.5*ccp
        pot(iws_a,ircv,is)=pot(iws_a,ircv,is)+0.5*ccs
       else
        ISI=nint(SIGN(1.0,RDC(IRCV)-SDC(I)))
        iwavp=iwp+1-isi
        if (laytyp(ln).eq.5) then
         iwavs=nint(5.5-isi*0.5)
        else
         iwavs=3-isi
        end if
        POT(iwavp,ircv,is)=POT(iwavp,ircv,is)+
     &       isi*ccp*CEXPT(-ABS(RDC(IRCV)-SDC(I))*ALF)
        POT(iwavs,ircv,is)=POT(iwavs,ircv,is)+
     &           ccs*CEXPT(-ABS(RDC(IRCV)-SDC(I))*BET)
        end if
      end do

      RETURN
      END

      SUBROUTINE SHFOR(i,force)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     SHFOR : OASES VERSION
C
C     Field contribution from in-plane horizontal force number i
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX ccp,ccs,force,alf,bet
c      if (debug) write(6,*) '>>> Entering SHFOR <<<'
C
      LN=LAYS(I)
      is=1

      if (laytyp(ln).eq.5) then
       if (fs_switch(ln)) then
        alf=beta(ln)
        bet=gamma(ln)
        iwp=2
        iws_b=5
        iws_a=6
        ccp=ai*cphfac(i)*force*wvno*alfinv(ln)
        ccs=ai*cphfac(i)*force
       else
        alf=alfa(ln)
        bet=gamma(ln)
        iwp=1
        iws_b=5
        iws_a=6
        ccp=ai*cphfac(i)*force*wvno*alfinv(ln)
        ccs=ai*cphfac(i)*force
       end if
      else if (laytyp(ln).eq.4) then
       alf=alfa(ln)
       bet=beta(ln)
       iwp=1
       iws_b=2
       iws_a=4
       ccp=ai*cphfac(i)*force*wvno/anstd(11,ln)
       ccs=ai*cphfac(i)*force*wvno/anstd(12,ln)
      else 
       alf=alfa(ln)
       bet=beta(ln)
       iwp=1
       iws_b=2
       iws_a=4
       ccp=ai*cphfac(i)*force*wvno*alfinv(ln)
       ccs=ai*cphfac(i)*force
      end if

      do IRCV=IFRCV(LN),ILRCV(LN)
       if (rdc(ircv).eq.sdc(i)) then
        pot(iwp,ircv,is)  =pot(iwp,ircv,is)+0.5*ccp
        pot(iws_b,ircv,is)=pot(iws_b,ircv,is)+0.5*ccs
        pot(iwp+2,ircv,is)=pot(iwp+1,ircv,is)+0.5*ccp
        pot(iws_a,ircv,is)=pot(iws_a,ircv,is)-0.5*ccs
       else
        ISI=nint(SIGN(1.0,RDC(IRCV)-SDC(I)))
        iwavp=iwp+1-isi
        if (laytyp(ln).eq.5) then
         iwavs=nint(5.5-isi*0.5)
        else
         iwavs=3-isi
        end if
        POT(iwavp,ircv,is)=POT(iwavp,ircv,is)+
     &           ccp*CEXPT(-ABS(RDC(IRCV)-SDC(I))*ALF)
        POT(iwavs,ircv,is)=POT(iwavs,ircv,is)+
     &       isi*ccs*CEXPT(-ABS(RDC(IRCV)-SDC(I))*BET)
       end if
      end do

      RETURN
      END

      SUBROUTINE Saxmom(i,vermom,hormom)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     SCNTEN : OASES VERSION
C
C     Kernel contibution from moment source number i
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX S,CC1,CC2
      COMPLEX vermom,hormom
      COMPLEX ccp,ccs,alf,bet,hsq
c      if (debug) write(6,*) '>>> Entering SAXMOM <<<'
C
      ln=lays(i)
      is=1

      if (laytyp(ln).eq.5) then
       if (fs_switch(ln)) then
        alf=beta(ln)
        bet=gamma(ln)
        iwp=2
        iws_b=5
        iws_a=6
        ccp=cphfac(i)*betinv(ln)*
     &     (s2*(vermom-hormom) - ck2(ln)*(0.75*vermom+0.5*hormom))
        ccs=cphfac(i)*wvno*(hormom-vermom)
       else
        alf=alfa(ln)
        bet=gamma(ln)
        iwp=1
        iws_b=5
        iws_a=6
        ccp=cphfac(i)*alfinv(ln)*
     &     (s2*(vermom-hormom) - ck1(ln)*(0.75*vermom+0.5*hormom))
        ccs=cphfac(i)*wvno*(hormom-vermom)
       end if
      else if (laytyp(ln).eq.4) then
       alf=alfa(ln)
       bet=beta(ln)
       iwp=1
       iws_b=2
       iws_a=4
       hsq=(wvno-alfa(ln))*(wvno+alfa(ln))
       ccp=cphfac(i)* (s2*(vermom-hormom) - 
     &                 hsq*(0.75*vermom+0.5*hormom))/anstd(11,ln)
       ccs=cphfac(i)*s2*(hormom-vermom)/anstd(12,ln)
      else 
       alf=alfa(ln)
       bet=beta(ln)
       iwp=1
       iws_b=2
       iws_a=4
       ccp=cphfac(i)*alfinv(ln)*
     &     (s2*(vermom-hormom) - ak2(ln,1)*(0.75*vermom+0.5*hormom))
       ccs=cphfac(i)*wvno*(hormom-vermom)
      end if

      do IRCV=IFRCV(LN),ILRCV(LN)
           ISI=nint(SIGN(1.0,RDC(IRCV)-SDC(I)))
           iwavp=iwp+1-isi
           if (laytyp(ln).eq.5) then
            iwavs=nint(5.5-isi*0.5)
           else
            iwavs=3-isi
           end if
           POT(iwavp,ircv,is)=POT(iwavp,ircv,is)+
     &           ccp*CEXPT(-ABS(RDC(IRCV)-SDC(I))*ALF)
           POT(iwavs,ircv,is)=POT(iwavs,ircv,is)-
     &       isi*ccs*CEXPT(-ABS(RDC(IRCV)-SDC(I))*BET)
      end do
c >>> end of routine SAXMOM <<<
      RETURN
      END


      SUBROUTINE SOLDIS(i,hordis,verdis)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     axmom: OASES VERSION
C
C     r.h.s. contribution from moment source number i
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX S,CC1,CC2,cc3,cc4,cuvp,covp,cuvs,covs,verdis,hordis,cfac
      complex ja,jb,cc
      complex alf,bet,alfi,beti,hsq,ksq

c      if (debug) write(6,*) '>>> Entering SOLDIS <<<'
C

      LN=LAYS(I)

      if (laytyp(ln).eq.5) then
       if (fs_switch(ln)) then
        alf=beta(ln)
        bet=gamma(ln)
        alfi=betinv(ln)
        beti=gaminv(ln)
        iwp=2
        iws=3
        hsq=ck2(ln)
        ksq=cks(ln)
       else
        alf=alfa(ln)
        bet=gamma(ln)
        alfi=alfinv(ln)
        beti=gaminv(ln)
        iwp=1
        iws=3
        hsq=ck1(ln)
        ksq=cks(ln)
       end if
      else if (laytyp(ln).eq.4) then
       alf=alfa(ln)
       bet=beta(ln)
       alfi=alfinv(ln)
       beti=betinv(ln)
       iwp=1
       iws=2
       hsq=(wvno-alfa(ln))*(wvno+alfa(ln))
       ksq=(wvno-beta(ln))*(wvno+beta(ln))
      else 
       alf=alfa(ln)
       bet=beta(ln)
       alfi=alfinv(ln)
       beti=betinv(ln)
       iwp=1
       iws=2
       hsq=ak2(ln,1)
       ksq=ak2(ln,2)
      end if

      cfac=1e0/ksq
      dz=rdc(2)-rdc(1)
      ja=1e0
      jb=1e0
c      cc=-ai*alf*dz*0.5
c      ja=sin(cc)/cc
c      cc=-ai*bet*dz*0.5
c      jb=sin(cc)/cc
c >>> horizontal displacement source
      CC1=-ja*hordis*(1e0+2e0*(s2-hsq)*cfac)*alfi
      cc2=-jb*hordis*2e0*wvno*cfac
c      cc3=-verdis*2e0*ai*wvno*cfac
c      cc4=-verdis*ai*(2e0*s2-ksq)*cfac*beti
      cc3=ja*verdis*2e0*ai*wvno*cfac
      cc4=jb*verdis*ai*(2e0*s2-ksq)*cfac*beti

       if (srctyp.eq.97) then
        isoff=ivspnt(i)-1
       else
        isoff=0
       end if
c       write(6,*) 'soldis, isoff=',isoff
       if (ln.gt.1) then
c >>> Upper interface potentials
        cuvp=CEXPT(-ABS(V(LN,1)-SDC(I))*ALF)
        cuvs=CEXPT(-ABS(V(LN,1)-SDC(I))*BET)
        is=isoff+1
        PUIN(iwp,LN-1,is)=PUIN(iwp,LN-1,is)+(cc1)*cuvp 
        PUIN(iws,LN-1,is)=PUIN(iws,LN-1,is)-(cc2)*cuvs
        is=isoff+2
        PUIN(iwp,LN-1,is)=PUIN(iwp,LN-1,is)+(-cc3)*cuvp 
        PUIN(iws,LN-1,is)=PUIN(iws,LN-1,is)-(-cc4)*cuvs
       end if
c >>> Lower interface potentials
       if (ln.lt.NUML) then
         covp=CEXPT(-ABS(V(LN+1,1)-SDC(I))*ALF)
         covs=CEXPT(-ABS(V(LN+1,1)-SDC(I))*BET)
         is=isoff+1
         POIN(iwp,LN,is)=POIN(iwp,LN,is)+(cc1)*covp
         POIN(iws,LN,is)=POIN(iws,LN,is)+(cc2)*covs
         is=isoff+2
         POIN(iwp,LN,is)=POIN(iwp,LN,is)+(cc3)*covp
         POIN(iws,LN,is)=POIN(iws,LN,is)+(cc4)*covs
       end if
      
      RETURN
      END

      SUBROUTINE SCNDIS(i,hordis,verdis)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     SCNDIS: OASES VERSION
C
C     r.h.s. contribution from displacement source number i
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX S,CC1,CC2,cc3,cc4,covp,covs,verdis,hordis,cfac
      complex ja,jb,cc
      complex alf,bet,alfi,beti,hsq,ksq

c      if (debug) write(6,*) '>>> Entering SOLDIS <<<'
C

      LN=LAYS(I)

      if (laytyp(ln).eq.5) then
       if (fs_switch(ln)) then
        alf=beta(ln)
        bet=gamma(ln)
        alfi=betinv(ln)
        beti=gaminv(ln)
        iwp=2
        iws_b=5
        iws_a=6
        hsq=ck2(ln)
        ksq=cks(ln)
       else
        alf=alfa(ln)
        bet=gamma(ln)
        alfi=alfinv(ln)
        beti=gaminv(ln)
        iwp=1
        iws_b=5
        iws_a=6
        hsq=ck1(ln)
        ksq=cks(ln)
       end if
      else if (laytyp(ln).eq.4) then
       alf=alfa(ln)
       bet=beta(ln)
       alfi=alfinv(ln)
       beti=betinv(ln)
       iwp=1
       iws_b=2
       iws_a=4
       hsq=(wvno-alfa(ln))*(wvno+alfa(ln))
       ksq=(wvno-beta(ln))*(wvno+beta(ln))
      else 
       alf=alfa(ln)
       bet=beta(ln)
       alfi=alfinv(ln)
       beti=betinv(ln)
       iwp=1
       iws_b=2
       iws_a=4
       hsq=ak2(ln,1)
       ksq=ak2(ln,2)
      end if

      cfac=1e0/ksq
      dz=rdc(2)-rdc(1)
      ja=1e0
      jb=1e0
c      cc=-ai*alf*dz*0.5
c      ja=sin(cc)/cc
c      cc=-ai*bet*dz*0.5
c      jb=sin(cc)/cc
c >>> horizontal displacement source
      CC1=-ja*hordis*(1e0+2e0*(s2-hsq)*cfac)*alfi
      cc2=-jb*hordis*2e0*wvno*cfac
c      cc3=-verdis*2e0*ai*wvno*cfac
c      cc4=-verdis*ai*(2e0*s2-ksq)*cfac*beti
      cc3=ja*verdis*2e0*ai*wvno*cfac
      cc4=jb*verdis*ai*(2e0*s2-ksq)*cfac*beti

       if (srctyp.eq.97) then
        isoff=ivspnt(i)-1
       else
        isoff=0
       end if

       do IRCV=IFRCV(LN),ILRCV(LN)
        if (rdc(ircv).eq.sdc(i)) then
         is=isoff+1
         pot(iwp,ircv,is)=pot(iwp,ircv,is)+0.5*(cc1)
         pot(iws_b,ircv,is)=pot(iws_b,ircv,is)+0.5*(cc2)
         pot(iwp+2,ircv,is)=pot(iwp+2,ircv,is)+0.5*(cc1)
         pot(iws_a,ircv,is)=pot(iws_a,ircv,is)+0.5*(-cc2)
         is=isoff+2
         pot(iwp,ircv,is)=pot(iwp,ircv,is)+0.5*(cc3)
         pot(iws_b,ircv,is)=pot(iws_b,ircv,is)+0.5*(cc4)
         pot(iwp+2,ircv,is)=pot(iwp+2,ircv,is)+0.5*(-cc3)
         pot(iws_a,ircv,is)=pot(iws_a,ircv,is)+0.5*(cc4)
        else
         isi=nint(SIGN(1.0,RDC(IRCV)-SDC(I)))
         iwavp=iwp+1-isi
         if (laytyp(ln).eq.5) then
          iwavs=nint(5.5-0.5*isi)
         else
          iwavs=3-isi
         end if
         covp=CEXPT(-ABS(RDC(IRCV)-SDC(I))*ALF)
         covs=CEXPT(-ABS(RDC(IRCV)-SDC(I))*BET)
         is=isoff+1
         POT(iwavp,ircv,is)=POT(iwavp,ircv,is)+ cc1*covp
         POT(iwavs,ircv,is)=POT(iwavs,ircv,is)+ isi*cc2*covs
         is=isoff+2
         POT(iwavp,ircv,is)=POT(iwavp,ircv,is)+isi*cc3*covp
         POT(iwavs,ircv,is)=POT(iwavs,ircv,is)+cc4*covs
        end if
       end do

      RETURN
      END

      SUBROUTINE SOLDIP(i,dip_mom,dip_angle)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     POINT DIP-SLIP SOURCE WITH ARBITRARY DIP ANGLE DELTA
C     IN ELASTIC MEDIUM
c     MODIFIED FOR OASES BY HS MARCH 27, 1990
c     MODIFIED FOR OASES V3.6 BY HS AUG 24, 1992
c     Modified for 2-D in-plane source. HS Oct 22, 1996.
c     rmom: Seismic moment amplitude. Phase in cphfac.
c     rdelta: Dip angle in radians.
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX S,CC1,CC2,cc3,cc4,cc5
      complex alf,bet,alfi,beti,hsq,ksq

      LN=LAYS(I)
      cc=dip_mom*cos(2.0*dip_angle)
      cs=dip_mom*sin(2.0*dip_angle)

      if (laytyp(ln).eq.5) then
       if (fs_switch(ln)) then
        alf=beta(ln)
        bet=gamma(ln)
        alfi=betinv(ln)
        beti=gaminv(ln)
        iwp=2
        iws=3
        ksq=cks(ln)
       else
        alf=alfa(ln)
        bet=gamma(ln)
        alfi=alfinv(ln)
        beti=gaminv(ln)
        iwp=1
        iws=3
        ksq=cks(ln)
       end if
      else if (laytyp(ln).eq.4) then
       alf=alfa(ln)
       bet=beta(ln)
       alfi=alfinv(ln)
       beti=betinv(ln)
       iwp=1
       iws=2
       ksq=(wvno-beta(ln))*(wvno+beta(ln))
      else 
       alf=alfa(ln)
       bet=beta(ln)
       alfi=alfinv(ln)
       beti=betinv(ln)
       iwp=1
       iws=2
       ksq=ak2(ln,2)
      end if

      cc3=(2.0*alf+s2*alfi)
      cc4=ksq/wvno
      cc5=2e0*s2-ksq

       if (ln.gt.1) then
        is=-1
c >>> interface above layer
        cc1=cphfac(i)*cexpt(-abs(v(ln,1)-sdc(i))*alf)
        cc2=cphfac(i)*cexpt(-abs(v(ln,1)-sdc(i))*bet)
c theta = 90 deg
c >>> m=1
c     Omnidirectional
c        m=1
         puin(iwp,ln-1,1)=puin(iwp,ln-1,1) - 0.5*cs*cc3*cc1
         puin(iws,ln-1,1)=puin(iws,ln-1,1) - is*1.5*cs*wvno*cc2      
c >>> m=2
c     i*cos(theta)
c >>> m=3
c     i*sin(theta)
c        m=3
        puin(iwp,ln-1,1)=puin(iwp,ln-1,1) - ai*is*2.0*cc*wvno*cc1
        puin(iws,ln-1,1)=puin(iws,ln-1,1) - ai*cc*cc5*beti*cc2      
c >>> m=4
c     - cos(2 theta) 
c        m=4
         puin(iwp,ln-1,1)=puin(iwp,ln-1,1) - 0.5*cs*s2*alfi*cc1
         puin(iws,ln-1,1)=puin(iws,ln-1,1) - is*0.5*cs*wvno*cc2      
c >>> m=5
c     - sin(2 theta)

       end if
       if (ln.lt.numl) then
        is=1
c >>> interface below layer
        cc1=cphfac(i)*cexpt(-abs(v(ln+1,1)-sdc(i))*alf)
        cc2=cphfac(i)*cexpt(-abs(v(ln+1,1)-sdc(i))*bet)
c >>> m=1
c     Omnidirectional
c        m=1
         poin(iwp,ln,1)=poin(iwp,ln,1) - 0.5*cs*cc3*cc1
         poin(iws,ln,1)=poin(iws,ln,1) - is*1.5*cs*wvno*cc2      
c >>> m=2
c     cos(theta)

c >>> m=3
c     sin(theta)
c        m=3
        poin(iwp,ln,1)=poin(iwp,ln,1) - ai*is*2.0*cc*wvno*cc1
        poin(iws,ln,1)=poin(iws,ln,1) - ai*cc*cc5*beti*cc2      
c >>> m=4
c     - cos(2 theta)
c        m=4
         poin(iwp,ln,1)=poin(iwp,ln,1) - 0.5*cs*s2*alfi*cc1
         poin(iws,ln,1)=poin(iws,ln,1) - is*0.5*cs*wvno*cc2      
c >>> m=5
c     - sin(2 theta)
       end if
C
      RETURN
      END

      SUBROUTINE SCNDIP(i,dip_mom,dip_angle)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     SCNDIP : OASES VERSION
C
C     IMPLEMENTING DIP-SLIP SOURCE IN SOLID MEDIUM, 120987 BY JKIM
C     MODIFIED BY FOR MOVING SOURCE MAY 15, 1988
c     Modified for 2-D in-plane source. HS Oct 22, 1996.
c     rmom: Seismic moment amplitude. Phase in cphfac.
c     rdelta: Dip angle in radians.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX CC1,CC2,CC3,CC4,cc5
      complex alf,bet,alfi,beti,hsq,ksq
C
cvd$  novector
cvd$  concur
cvd$  nodepchk
      LN=LAYS(I)

      cc=dip_mom*cos(2.0*dip_angle)
      cs=dip_mom*sin(2.0*dip_angle)

      if (laytyp(ln).eq.5) then
       if (fs_switch(ln)) then
        alf=beta(ln)
        bet=gamma(ln)
        alfi=betinv(ln)
        beti=gaminv(ln)
        iwp=2
        iws_b=5
        iws_a=6
        ksq=cks(ln)
       else
        alf=alfa(ln)
        bet=gamma(ln)
        alfi=alfinv(ln)
        beti=gaminv(ln)
        iwp=1
        iws_b=5
        iws_a=6
        ksq=cks(ln)
       end if
      else if (laytyp(ln).eq.4) then
       alf=alfa(ln)
       bet=beta(ln)
       alfi=alfinv(ln)
       beti=betinv(ln)
       iwp=1
       iws_b=2
       iws_a=4
       ksq=(wvno-beta(ln))*(wvno+beta(ln))
      else 
       alf=alfa(ln)
       bet=beta(ln)
       alfi=alfinv(ln)
       beti=betinv(ln)
       iwp=1
       iws_b=2
       iws_a=4
       ksq=ak2(ln,2)
      end if

      cc3=(2.0*alf+s2*alfi)
      cc4=ksq/wvno
      cc5=2e0*s2-ksq

      do IRCV=IFRCV(LN),ILRCV(LN)
cvd$  novector
       zdif=rdc(ircv)-sdc(i)
       is=nint(sign(1.0,zdif))
       iwav1=iwp+1-is
       if (laytyp(ln).eq.5) then
        iwav2=nint(5.5-is*0.5)
       else
        iwav2=iwav1+1
       end if

       cc1=cphfac(i)*cexpt(-abs(rdc(ircv)-sdc(i))*alf)
       cc2=cphfac(i)*cexpt(-abs(rdc(ircv)-sdc(i))*bet)

c >>> m=1
c     Omnidirectional
c        m=1
         pot(iwav1,ircv,1)=pot(iwav1,ircv,1) - 0.5*cs*cc3*cc1
         pot(iwav2,ircv,1)=pot(iwav2,ircv,1) - is*1.5*cs*wvno *cc2      
c >>> m=2
c     i cos(theta)
c >>> m=3
c     i sin(theta)
c        m=3
         pot(iwav1,ircv,1)=pot(iwav1,ircv,1)-ai*is*2.0*cc*wvno*cc1
         pot(iwav2,ircv,1)=pot(iwav2,ircv,1)-ai*cc*cc5*beti*cc2      
c >>> m=4
c     - cos(2 theta)
c        m=4
         pot(iwav1,ircv,1)=pot(iwav1,ircv,1) - 0.5*cs*s2*alfi*cc1
         pot(iwav2,ircv,1)=pot(iwav2,ircv,1) - is*0.5*cs*wvno*cc2      
c >>> m=5

      end do

      RETURN
      END


