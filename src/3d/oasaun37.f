      SUBROUTINE INITS
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      INCLUDE 'comti.f'
      COMPLEX S
      COMPLEX TS2,CCC,TS
      if (debug) write(6,*) 'Entering INITS'
c
C     RESET DEPTH-DERIVATIVE FLAG
C
      IDERIV=0
      S=WVNO
      S2=S*S
      THICK(1)=0E0
      THICK(NUML)=0E0
      do 2 ISS=1,ISROW
       DO 2 MM=1,MSUFT
cvd$  select(concur)
        DO 2 JJJ=1,NLEQ
         DO 2 IL=1,NUML
          R(IL,JJJ,MM,ISS)=CNUL
 2    CONTINUE
      do 3 mm=1,msuft
       do 3 il=1,numl
        do 3 jjj=1,NLEQH
         poin(jjj,il,mm)=cnul       
         puin(jjj,il,mm)=cnul
 3       continue
cvd$  nodepchk
      DO 10 I=1,NUMT(1)
       LL=LAYT(I,1)
       ALFA(LL)=CSQRT(S2-AK2(LL,1))
       EZALFM(LL)=CEXPT(-ALFA(LL)*THICK(LL))
       alfinv(ll)=1e0/alfa(ll)
 10   CONTINUE
cvd$  nodepchk
      DO 30 I=1,NUMT(3)
       LL=LAYT(I,3)
       ALFA(LL)=CSQRT(S2-AK2(LL,1))
       BETA(LL)=CSQRT(S2-AK2(LL,2))
       alfinv(ll)=1e0/alfa(ll)
       betinv(ll)=1e0/beta(ll)
 30   CONTINUE
cvd$  nodepchk
      DO 31 I=1,NUMT(3)
       LL=LAYT(I,3)
       CON2(LL)=CON1(LL)*(2E0*S2-AK2(LL,2))
       CON3(LL)=CON1(LL)*2E0*S*ALFA(LL)
       CON4(LL)=CON1(LL)*2E0*S*BETA(LL)
       con5(ll)=con1(ll)*(2e0*alfa(ll)*alfa(ll)+ak2(ll,2))
 31   CONTINUE
cvd$  nodepchk
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
         CALL VMOV(S3UP,1,ANSTD(1,LAYN),1,2*NTIPAR)
c         CALL RDBUF(41,ANSTD(1,LAYN),2*NTIPAR)
 40     CONTINUE
cvd$  nodepchk
        DO 41 I=1,NUMT(4)
         LAYN=LAYT(I,4)
         ALFA(LAYN)=ANSTD(1,LAYN)
         BETA(LAYN)=ANSTD(2,LAYN)
         GAMA(LAYN)=ANSTD(21,LAYN)
         alfinv(layn)=1e0/alfa(layn)
         betinv(layn)=1e0/beta(layn)
 41     CONTINUE
cvd$  nodepchk
        DO 42 I=1,NUMT(4)
         LAYN=LAYT(I,4)
         EZALFM(LAYN)=CEXPT(-THICK(LAYN)*ALFA(LAYN))
         EZBETM(LAYN)=CEXPT(-THICK(LAYN)*BETA(LAYN))
         EZGAMM(LAYN)=CEXPT(-THICK(LAYN)*GAMA(LAYN))
 42     CONTINUE
      RETURN
      END
      SUBROUTINE BUILD               
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      if (debug) write(6,*) 'Entering BUILD'
C *** build local coefficient matrices
       IF (NUMT(1).GT.0) CALL LIQLAY
       IF (NUMT(2).GT.0) CALL AIRLAY
       IF (NUMT(3).GT.0) CALL SOLLAY
       IF (NUMT(4).GT.0) CALL TISOLL
C *** NORMALIZE DISPLACEMENT EQUATIONS
      DO 530 I=1,NLEQ/2
       DO 510 ISS=1,ISROW
       DO 510 MM=1,MSUFT
       DO 510 L=1,NUML
        R(L,I,MM,ISS)=R(L,I,MM,ISS)*DISNRM
 510   CONTINUE
       DO 520 J=1,NLEQ
        do 520 L=1,NUML
         AUP(L,I,J)=AUP(L,I,J)*DISNRM
         ALO(L,I,J)=ALO(L,I,J)*DISNRM
 520   CONTINUE
 530  CONTINUE
      if (debug) write(6,*) 'Exiting BUILD'
      RETURN        
      END           

      SUBROUTINE SOLLAY     
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
cvd$r permutation(LAYT)
cvd$  nodepchk
      DO 10 I=1,NUMT(3)
       LN=LAYT(I,3)              

C     FIRST ROW IS VERTICAL DISPLACEMENT
       AUP(LN,1,1)=ALFA(LN)
       AUP(LN,1,2)=-WVNO
       AUP(LN,1,4)=-ALFA(LN)
       AUP(LN,1,5)=-WVNO
C     SECOND ROW IS HORIZONTAL (RADIAL+ANGULAR) DISPLACEMENT
       AUP(LN,2,1)=WVNO
       AUP(LN,2,2)=-BETA(LN)
       AUP(LN,2,3)=-WVNO
       AUP(LN,2,4)=WVNO
       AUP(LN,2,5)=BETA(LN)
       AUP(LN,2,6)=-WVNO
C     THIRD ROW IS HORIZONTAL (RADIAL-ANGULAR) DISPLACEMENT
       AUP(LN,3,1)=-WVNO
       AUP(LN,3,2)=BETA(LN)
       AUP(LN,3,3)=-WVNO
       AUP(LN,3,4)=-WVNO
       AUP(LN,3,5)=-BETA(LN)
       AUP(LN,3,6)=-WVNO
C     FOURTH ROW IS NORMAL STRESS
       AUP(LN,4,1)=-CON2(LN)
       AUP(LN,4,2)=CON4(LN)
       AUP(LN,4,4)=-CON2(LN)
       AUP(LN,4,5)=-CON4(LN)
C     FIFTH ROW IS SHEAR (HORIZONTAL+ANGULAR) STRESS
       AUP(LN,5,1)=-CON3(LN)
       AUP(LN,5,2)=CON2(LN)
       AUP(LN,5,3)=CON4(LN)*0.5E0
       AUP(LN,5,4)=CON3(LN)
       AUP(LN,5,5)=CON2(LN)
       AUP(LN,5,6)=-AUP(LN,5,3)
C     THE LAST ROW IS SHEAR (HORIZONTAL-ANGULAR) STRESS
       AUP(LN,6,1)=CON3(LN)
       AUP(LN,6,2)=-CON2(LN)
       AUP(LN,6,3)= AUP(LN,5,3)
       AUP(LN,6,4)=-CON3(LN)
       AUP(LN,6,5)=-CON2(LN)
       AUP(LN,6,6)= AUP(LN,5,6)
 10   CONTINUE
C     THE LOWER INTERFACE MATRIX FOLLOWS BY A CHANGE OF SIGN
C     AND MULTLIPICATION WITH THE EXPONENTIALS
      DO 15 I=1,NLEQ           
cvd$  nodepchk
       DO 15 J=1,NUMT(3)
        LN=LAYT(J,3)
        ALO(LN,I,1)=-AUP(LN,I,1)*EZALFM(LN)                 
        ALO(LN,I,2)=-AUP(LN,I,2)*EZBETM(LN)                 
        ALO(LN,I,3)=-AUP(LN,I,3)*EZBETM(LN)                 
        ALO(LN,I,4)=-AUP(LN,I,4)                 
        ALO(LN,I,5)=-AUP(LN,I,5)
        ALO(LN,I,6)=-AUP(LN,I,6)
        AUP(LN,I,4)=AUP(LN,I,4)*EZALFM(LN)
        AUP(LN,I,5)=AUP(LN,I,5)*EZBETM(LN)                 
        AUP(LN,I,6)=AUP(LN,I,6)*EZBETM(LN)                 
 15   CONTINUE
C *** SOURCES
      if (srctyp.eq.99) then
       if (nstrf.eq.3.and.isprm(1).eq.10.and.isprm(2).eq.11.and.
     &     isprm(3).eq.12) then
        do 205 jj=1,numts(3)
         i=nspnt(jj,3)
         call axmom(i,sval(i,1)+sval(i,2),sval(i,1))
         aaa=abs(real(sval(i,3))) + abs(aimag(sval(i,3)))
         if (aaa.gt.1E-20) call axfor(i,sval(i,3))
 205    continue
       else if (nstrf.eq.2.and.isprm(1).eq.10.and.isprm(2).eq.11) then
        do 206 jj=1,numts(3)
         i=nspnt(jj,3)
 206     call axmom(i,sval(i,1)+sval(i,2),sval(i,1))
       else
        do 210 jj=1,numts(3)
         i=nspnt(jj,3)
        do 210 j=1,nstrf
         if (isprm(j).eq.10) then
          call axmom(i,sval(i,j),sval(i,j))
         else if (isprm(j).eq.11) then
          call axmom(i,sval(i,j),cnul)
         else if (isprm(j).eq.12) then
          call axfor(i,sval(i,j))
         else
          stop ' >>> SOLLAY: Unknown source type in TRF file <<<'
         end if
 210    continue
       end if
      else IF(SRCTYP.EQ.1) THEN
        CALL SOLEXP
      ELSEIF(SRCTYP.EQ.2) THEN
        CALL SOLFOR
      ELSEIF(SRCTYP.EQ.3) THEN
        CALL SOLDIP
      ELSEIF(SRCTYP.EQ.4) THEN
        CALL SOLSTR
      ELSEIF(SRCTYP.EQ.5 .OR. SRCTYP.EQ.6) THEN
        CALL SOLTEN
      else if (srctyp.eq.7) then
c >>> seismic moment source
        call solmom()
      ENDIF
c
c >>> Compute DGM right hand sides for all interfaces
c
cvd$r permutation(LAYT)
cvd$  nodepchk
C$DIR NO_RECURRENCE
CDIR$ IVDEP
      is=1
      do 50 m=1,msuft
      DO 50 j=1,NUMT(3)
       LN=LAYT(j,3)              
       if (nosou(ln).gt.0) then
c >>> Contribution from below interface       
        if (ln.gt.1) then
         in=ln-1
         r(in,1,m,is) = r(in,1,m,is)
     &             + alfa(ln)*puin(1,in,m) +     wvno*puin(2,in,m)
         r(in,2,m,is) = r(in,2,m,is)
     &                 - wvno*puin(1,in,m) - beta(ln)*puin(2,in,m)
     &                 + wvno*puin(3,in,m)
         r(in,3,m,is) = r(in,3,m,is)
     &                 + wvno*puin(1,in,m) + beta(ln)*puin(2,in,m)
     &                 + wvno*puin(3,in,m)
         r(in,4,m,is) = r(in,4,m,is)
     &             + con2(ln)*puin(1,in,m) + con4(ln)*puin(2,in,m)
         r(in,5,m,is) = r(in,5,m,is)
     &             - con3(ln)*puin(1,in,m) - con2(ln)*puin(2,in,m)
     &             + 0.5*con4(ln)*puin(3,in,m)
         r(in,6,m,is) = r(in,6,m,is)
     &             + con3(ln)*puin(1,in,m) + con2(ln)*puin(2,in,m)
     &             + 0.5*con4(ln)*puin(3,in,m)
        end if
c >>> Contribution from above interface       
        if (ln.lt.numl) then
         in=ln
         r(in,1,m,is) = r(in,1,m,is)
     &             + alfa(ln)*poin(1,in,m) -     wvno*poin(2,in,m)
         r(in,2,m,is) = r(in,2,m,is)
     &                 + wvno*poin(1,in,m) - beta(ln)*poin(2,in,m)
     &                 - wvno*poin(3,in,m)
         r(in,3,m,is) = r(in,3,m,is)
     &                 - wvno*poin(1,in,m) + beta(ln)*poin(2,in,m)
     &                 - wvno*poin(3,in,m)
         r(in,4,m,is) = r(in,4,m,is)
     &             - con2(ln)*poin(1,in,m) + con4(ln)*poin(2,in,m)
         r(in,5,m,is) = r(in,5,m,is)
     &             - con3(ln)*poin(1,in,m) + con2(ln)*poin(2,in,m)
     &             + 0.5*con4(ln)*poin(3,in,m)
         r(in,6,m,is) = r(in,6,m,is)
     &             + con3(ln)*poin(1,in,m) - con2(ln)*poin(2,in,m)
     &             + 0.5*con4(ln)*poin(3,in,m)
        end if
       end if
 50   continue
      RETURN
      END

      SUBROUTINE SOLEXP
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     SOLEXP : OASES VERSION
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
      COMPLEX S,CC1,ccc1

      m=1
      S=WVNO
C
cvd$  novector
cvd$  noconcur
      DO 50 J=1,NUMTS(3)
       I=NSPNT(J,3)
       LN=LAYS(I)
       CCC1=alfinv(ln)
       cc1=cphfac(i)*CCC1
       if (ln.gt.1) then
c >>> Upper interface potentials
         PUIN(1,LN-1,m)=PUIN(1,LN-1,m)+
     &           cc1*CEXPT(-ABS(V(LN,1)-SDC(I))*ALFA(LN))
       end if
c >>> Lower interface potentials
       if (ln.lt.NUML) then
         POIN(1,LN,m)=POIN(1,LN,m)+
     &           cc1*CEXPT(-ABS(V(LN+1,1)-SDC(I))*ALFA(LN))
       end if
 50    continue
      RETURN
      END
      SUBROUTINE SOLFOR
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     SOLFOR : OASES VERSION
C
C     POINT FORCE SOURCE IN SOLID MEDIUM
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX S,CC1,CC2,cc3
C
      S=WVNO
cvd$  novector
cvd$  noconcur
      DO 40 J=1,NUMTS(3)
       I=NSPNT(J,3)
       LN=LAYS(I)
       cc3=ak2(ln,2)/(s*beta(ln))      
       if (ln.gt.1) then
        is=-1
c >>> interface above layer
        cc1=cphfac(i)*cexpt(-abs(v(ln,1)-sdc(i))*alfa(ln))
        cc2=cphfac(i)*cexpt(-abs(v(ln,1)-sdc(i))*beta(ln))
c >>> m=1
        m=1
        puin(1,ln-1,m)=puin(1,ln-1,m) + is* F3*cc1
        puin(2,ln-1,m)=puin(2,ln-1,m) + F3*cc2*s*betinv(ln)      
c >>> m=2
        m=2
        puin(1,ln-1,m)=puin(1,ln-1,m) + F1*cc1*s*alfinv(ln)
        puin(2,ln-1,m)=puin(2,ln-1,m) + is* F1*cc2
        puin(3,ln-1,m)=puin(3,ln-1,m) + F1*cc3*cc2      
c >>> m=3
        m=3
        puin(1,ln-1,m)=puin(1,ln-1,m) + F2*cc1*s*alfinv(ln)
        puin(2,ln-1,m)=puin(2,ln-1,m) + is* F2*cc2      
        puin(3,ln-1,m)=puin(3,ln-1,m) + F2*cc3*cc2      
       end if
       if (ln.lt.numl) then
        is=1
c >>> interface below layer
        cc1=cphfac(i)*cexpt(-abs(v(ln+1,1)-sdc(i))*alfa(ln))
        cc2=cphfac(i)*cexpt(-abs(v(ln+1,1)-sdc(i))*beta(ln))
c >>> m=1
        m=1
        poin(1,ln,m)=poin(1,ln,m) + is*F3*cc1
        poin(2,ln,m)=poin(2,ln,m) + F3*cc2*s*betinv(ln)      
c >>> m=2
        m=2
        poin(1,ln,m)=poin(1,ln,m) + F1*cc1*s*alfinv(ln)
        poin(2,ln,m)=poin(2,ln,m) + is*F1*cc2      
        poin(3,ln,m)=poin(3,ln,m) + F1*cc3*cc2      
c >>> m=3
        m=3
        poin(1,ln,m)=poin(1,ln,m) + F2*cc1*s*alfinv(ln)
        poin(2,ln,m)=poin(2,ln,m) + is*F2*cc2      
        poin(3,ln,m)=poin(3,ln,m) + F2*cc3*cc2      
       end if
 40   CONTINUE
C
      RETURN
      END
      SUBROUTINE SOLDIP
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     POINT DIP-SLIP SOURCE WITH ARBITRARY DIP ANGLE DELTA
C     IN ELASTIC MEDIUM
c     MODIFIED FOR OASES BY HS MARCH 27, 1990
c     MODIFIED FOR OASES V3.6 BY HS AUG 24, 1992
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX S,CC1,CC2,cc3,cc4,cc5
C
      S=WVNO
      DO 40 J=1,NUMTS(3)
       I=NSPNT(J,3)
       LN=LAYS(I)
       cc=rmom*cos(2.0*rdelta)
       cs=rmom*sin(2.0*rdelta)
       cc3=(2.0*alfa(ln)+s2*alfinv(ln))
       cc4=ak2(ln,2)/s
       cc5=2e0*s2-ak2(ln,2)
       if (ln.gt.1) then
        is=-1
c >>> interface above layer
        cc1=cphfac(i)*cexpt(-abs(v(ln,1)-sdc(i))*alfa(ln))
        cc2=cphfac(i)*cexpt(-abs(v(ln,1)-sdc(i))*beta(ln))
c >>> m=1
        m=1
        puin(1,ln-1,m)=puin(1,ln-1,m) - 0.5*cs*cc3*cc1
        puin(2,ln-1,m)=puin(2,ln-1,m) - is* 1.5*cs* s *cc2      
c >>> m=2

c >>> m=3
        m=3
        puin(1,ln-1,m)=puin(1,ln-1,m) - is* 2.0*cc*s*cc1
        puin(2,ln-1,m)=puin(2,ln-1,m) - cc*cc5*betinv(ln)*cc2      
        puin(3,ln-1,m)=puin(3,ln-1,m) - is* cc*cc4*cc2
c >>> m=4
        m=4
        puin(1,ln-1,m)=puin(1,ln-1,m) - 0.5*cs*s2*alfinv(ln)*cc1
        puin(2,ln-1,m)=puin(2,ln-1,m) - is* 0.5*cs*s*cc2      
        puin(3,ln-1,m)=puin(3,ln-1,m) 
     &                 - 0.5*cs*ak2(ln,2)*betinv(ln)*cc2
c >>> m=5

       end if
       if (ln.lt.numl) then
        is=1
c >>> interface below layer
        cc1=cphfac(i)*cexpt(-abs(v(ln+1,1)-sdc(i))*alfa(ln))
        cc2=cphfac(i)*cexpt(-abs(v(ln+1,1)-sdc(i))*beta(ln))
c >>> m=1
        m=1
        poin(1,ln,m)=poin(1,ln,m) - 0.5*cs*cc3*cc1
        poin(2,ln,m)=poin(2,ln,m) - is*1.5*cs* s *cc2      
c >>> m=2

c >>> m=3
        m=3
        poin(1,ln,m)=poin(1,ln,m) - is*2.0*cc*s*cc1
        poin(2,ln,m)=poin(2,ln,m) - cc*cc5*betinv(ln)*cc2      
        poin(3,ln,m)=poin(3,ln,m) - is*cc*cc4*cc2
c >>> m=4
        m=4
        poin(1,ln,m)=poin(1,ln,m) - 0.5*cs*s2*alfinv(ln)*cc1
        poin(2,ln,m)=poin(2,ln,m) - is*0.5*cs*s*cc2      
        poin(3,ln,m)=poin(3,ln,m) 
     &                 - 0.5*cs*ak2(ln,2)*betinv(ln)*cc2
c >>> m=5

       end if
 40   CONTINUE
C
      RETURN
      END
      SUBROUTINE SOLSTR
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     POINT STRIKE-SLIP SOURCE WITH ARBITRARY DIP ANGLE DELTA
C     IN ELASTIC MEDIUM
c     MODIFIED FOR OASES BY HS MARCH 27, 1990
c     Sep 15, 1994: Changed sign of P-potential in strike slip sources
c                   Now consistent with jkim appendix. 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX S,CC1,CC2,CC3,CC4,cc5
C
      S=WVNO
      DO 40 J=1,NUMTS(3)
       I=NSPNT(J,3)
       LN=LAYS(I)
       cc=rmom*cos(rdelta)
       cs=rmom*sin(rdelta)
       cc4=ak2(ln,2)/s
       cc5=2e0*s2-ak2(ln,2)
       if (ln.gt.1) then
        is=-1
c >>> interface above layer
        cc1=cphfac(i)*cexpt(-abs(v(ln,1)-sdc(i))*alfa(ln))
        cc2=cphfac(i)*cexpt(-abs(v(ln,1)-sdc(i))*beta(ln))
c >>> m=1
c >>> m=2
        m=2
        puin(1,ln-1,m)=puin(1,ln-1,m) + is* 2.0*cc*s*cc1
        puin(2,ln-1,m)=puin(2,ln-1,m) + cc*cc5*betinv(ln)*cc2      
        puin(3,ln-1,m)=puin(3,ln-1,m) + is* cc*cc4*cc2
c >>> m=3

c >>> m=4

c >>> m=5
        m=5
        puin(1,ln-1,m)=puin(1,ln-1,m) - cs*s2*alfinv(ln)*cc1
        puin(2,ln-1,m)=puin(2,ln-1,m) - is*cs*s*cc2      
        puin(3,ln-1,m)=puin(3,ln-1,m) 
     &                 - cs*ak2(ln,2)*betinv(ln)*cc2
       end if
       if (ln.lt.numl) then
        is=1
c >>> interface below layer
        cc1=cphfac(i)*cexpt(-abs(v(ln+1,1)-sdc(i))*alfa(ln))
        cc2=cphfac(i)*cexpt(-abs(v(ln+1,1)-sdc(i))*beta(ln))
c >>> m=1
c >>> m=2
        m=2
        poin(1,ln,m)=poin(1,ln,m) + is* 2.0*cc*s*cc1
        poin(2,ln,m)=poin(2,ln,m) + cc*cc5*betinv(ln)*cc2      
        poin(3,ln,m)=poin(3,ln,m) + is* cc*cc4*cc2
c >>> m=3
        m=3
c >>> m=4
        m=4
c >>> m=5
        m=5
        poin(1,ln,m)=poin(1,ln,m) - cs*s2*alfinv(ln)*cc1
        poin(2,ln,m)=poin(2,ln,m) - is*cs*s*cc2      
        poin(3,ln,m)=poin(3,ln,m) 
     &                 - cs*ak2(ln,2)*betinv(ln)*cc2
       end if
 40   CONTINUE
C
      RETURN
      END

      SUBROUTINE SOLTEN
C
C     IMPLEMENTING POINT TENSILE CRACK SOURCE WITH ARBITRARY DIP ANGLE DELTA
C     IN SOLID MEDIUM, 122687 BY JKIM
C     MODIFIED BY JKIM FOR MOVING STRIKE-SLIP CRACK MAY 15, 1988
c     MODIFIED FOR OASES BY HS SEP 7, 1994
C
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX S,CC1,CC2,CC3,CC4,cc5
      complex ctmom(3)
      real cc(3),cs(3)
C
      S=WVNO
C
      do k=1,3
        cc(k)=cos(2*tendel(k))
        cs(k)=sin(2*tendel(k))
      end do
      DO 40 J=1,NUMTS(3)
       I=NSPNT(J,3)
       LN=LAYS(I)
C
C     FOR TENSION CRACK, DEFAULT EQUIVALENT BODY FORCES ARE USED
C     WHILE, FOR SRCTYP=6, BODY FORCE COMPONENTS CAN BE SPECIFIED ACCORDINGLY
C
      IF(SRCTYP.EQ.5) THEN
        CTMOM(3)=1
        CTMOM(2)=ALAME(LN,1)/( ALAME(LN,1)+2.*ALAME(LN,2) )
        CTMOM(1)=CTMOM(2)
      ELSE
        do k=1,3
         ctmom(k)=tenmom(k)
        end do
      ENDIF
       cc3=s2-2.0*alfa(ln)*alfa(ln)
       cc4=s2+2.0*alfa(ln)*alfa(ln)
       cc5=beta(ln)*beta(ln)
        if (ln.gt.1) then
         is=-1
c >>> interface above layer
         cc1=cphfac(i)*cexpt(-abs(v(ln,1)-sdc(i))*alfa(ln))
         cc2=cphfac(i)*cexpt(-abs(v(ln,1)-sdc(i))*beta(ln))
         do k=1,3
c >>> m=1
         m=1
         puin(1,ln-1,m)=puin(1,ln-1,m) - 
     &                   ctmom(k)*0.25*(cc3-cc(k)*cc4)*alfinv(ln)*cc1
         puin(2,ln-1,m)=puin(2,ln-1,m) + 
     &                   ctmom(k)*.25*(1.0+3.0*cc(k))*is*s*cc2      
c >>> m=2
c >>> m=3
         if (k.gt.1) then
          m=3
          puin(1,ln-1,m)=puin(1,ln-1,m) - 
     &                   ctmom(k)*is*s*cs(k)*cc1
          puin(2,ln-1,m)=puin(2,ln-1,m) - 
     &                   ctmom(k)*0.5*(cc5+s2)*betinv(ln)*cs(k)*cc2
          puin(3,ln-1,m)=puin(3,ln-1,m) - 
     &                   ctmom(k)*0.5*is*(s2-cc5)*cs(k)*cc2/s
         end if
c >>> m=4
         rr1=1.0-cc(k)
         if (k.eq.1) rr1=-rr1
         m=4
         puin(1,ln-1,m)=puin(1,ln-1,m) - 
     &                   ctmom(k)*0.25*s2*alfinv(ln)*rr1*cc1
         puin(2,ln-1,m)=puin(2,ln-1,m) - 
     &                   ctmom(k)*0.25*is*s*rr1*cc2      
         puin(3,ln-1,m)=puin(3,ln-1,m) - 
     &                   ctmom(k)*0.25*(s2-cc5)*betinv(ln)*rr1*cc2
c >>> m=5
         end do
        end if
        if (ln.lt.numl) then
         is=1
c >>> interface below layer
         cc1=cphfac(i)*cexpt(-abs(v(ln+1,1)-sdc(i))*alfa(ln))
         cc2=cphfac(i)*cexpt(-abs(v(ln+1,1)-sdc(i))*beta(ln))
c >>> m=1
         do k=1,3
         m=1
         poin(1,ln,m)=poin(1,ln,m) - 
     &                  ctmom(k)*.25*(cc3-cc(k)*cc4)*alfinv(ln)*cc1
         poin(2,ln,m)=poin(2,ln,m) + 
     &                  ctmom(k)*.25*(1.0+3.0*cc(k))*is*s*cc2      
c >>> m=2
c >>> m=3
         if (k.gt.1) then
          m=3
          poin(1,ln,m)=poin(1,ln,m) - 
     &                  ctmom(k)*is*s*cs(k)*cc1
          poin(2,ln,m)=poin(2,ln,m) - 
     &                  ctmom(k)*0.5*(cc5+s2)*betinv(ln)*cs(k)*cc2
          poin(3,ln,m)=poin(3,ln,m) - 
     &                  ctmom(k)*0.5*is*(s2-cc5)*cs(k)*cc2/s
         end if
c >>> m=4
         rr1=1.0-cc(k)
         if (k.eq.1) rr1=-rr1
         m=4
         poin(1,ln,m)=poin(1,ln,m) - 
     &                  ctmom(k)*0.25*s2*alfinv(ln)*rr1*cc1
         poin(2,ln,m)=poin(2,ln,m) - 
     &                  ctmom(k)*0.25*is*s*rr1*cc2      
         poin(3,ln,m)=poin(3,ln,m) - 
     &                  ctmom(k)*0.25*(s2-cc5)*betinv(ln)*rr1*cc2
c >>> m=5
         end do
        end if
 40   CONTINUE
C
      RETURN
      END

      SUBROUTINE SOLMOM
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
C     SEISMIC MOMENT TENSOR SOURCE IN ELASTIC MEDIUM.
C     HS SEP 13, 1994
c     Sep 15, 1994: Changed sign of P-potential in strike slip sources
c                   Now consistent with jkim appendix. 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX S,CC1,CC2,CC3,CC4,cc5,cc6,cc7
      REAL tmom(3)
      real cc(3)
C
      S=WVNO
C
      CC(1)=-1.0
      CC(2)=-1.0
      CC(3)=1.0
      TMOM(1)=M11
      TMOM(2)=M22
      TMOM(3)=M33
      DO 40 J=1,NUMTS(3)
       I=NSPNT(J,3)
       LN=LAYS(I)
       cc3=s2-2.0*alfa(ln)*alfa(ln)
       cc4=s2+2.0*alfa(ln)*alfa(ln)
       cc5=beta(ln)*beta(ln)
       cc6=ak2(ln,2)/s
       cc7=2e0*s2-ak2(ln,2)
        if (ln.gt.1) then
         is=-1
c >>> interface above layer
         cc1=cphfac(i)*cexpt(-abs(v(ln,1)-sdc(i))*alfa(ln))
         cc2=cphfac(i)*cexpt(-abs(v(ln,1)-sdc(i))*beta(ln))
c >>> Normal components of tensor
         do k=1,3
c >>> m=1
         m=1
         puin(1,ln-1,m)=puin(1,ln-1,m) - 
     &                   tmom(k)*0.25*(cc3-cc(k)*cc4)*alfinv(ln)*cc1
         puin(2,ln-1,m)=puin(2,ln-1,m) + 
     &                   tmom(k)*.25*(1.0+3.0*cc(k))*is*s*cc2      
c >>> m=2
c >>> m=3
c >>> m=4
         if (k.lt.3) then
          rr1=1.0-cc(k)
          if (k.eq.1) rr1=-rr1
          m=4
          puin(1,ln-1,m)=puin(1,ln-1,m) - 
     &                   tmom(k)*0.25*s2*alfinv(ln)*rr1*cc1
          puin(2,ln-1,m)=puin(2,ln-1,m) - 
     &                   tmom(k)*0.25*is*s*rr1*cc2      
          puin(3,ln-1,m)=puin(3,ln-1,m) - 
     &                   tmom(k)*0.25*(s2-cc5)*betinv(ln)*rr1*cc2
         end if
c >>> m=5
         end do
c >>> moment component M12 (strike slip, t=pi/2)
c >>> m=1
c >>> m=2
c >>> m=3
c >>> m=4
c >>> m=5
         m=5
         puin(1,ln-1,m)=puin(1,ln-1,m) - m12*s2*alfinv(ln)*cc1
         puin(2,ln-1,m)=puin(2,ln-1,m) - is*m12*s*cc2      
         puin(3,ln-1,m)=puin(3,ln-1,m) 
     &                 - m12*ak2(ln,2)*betinv(ln)*cc2
c >>> moment component M13 (strike slip, t=0)
c >>> m=1
c >>> m=2
         m=2
         puin(1,ln-1,m)=puin(1,ln-1,m) + is*m13*2.0*s*cc1
         puin(2,ln-1,m)=puin(2,ln-1,m) + m13*cc7*betinv(ln)*cc2      
         puin(3,ln-1,m)=puin(3,ln-1,m) + is*m13*cc6*cc2
c >>> m=3
c >>> m=4
c >>> m=5
c >>> moment component M23 (dip slip, t=pi/2)
c >>> m=1
c >>> m=2
c >>> m=3
        m=3
        puin(1,ln-1,m)=puin(1,ln-1,m) + is*m23*2.0*s*cc1
        puin(2,ln-1,m)=puin(2,ln-1,m) + m23*cc7*betinv(ln)*cc2      
        puin(3,ln-1,m)=puin(3,ln-1,m) + is*m23*cc6*cc2
c >>> m=4
c >>> m=5
        end if
        if (ln.lt.numl) then
         is=1
c >>> interface below layer
         cc1=cphfac(i)*cexpt(-abs(v(ln+1,1)-sdc(i))*alfa(ln))
         cc2=cphfac(i)*cexpt(-abs(v(ln+1,1)-sdc(i))*beta(ln))
c >>> m=1
         do k=1,3
         m=1
         poin(1,ln,m)=poin(1,ln,m) - 
     &                  tmom(k)*.25*(cc3-cc(k)*cc4)*alfinv(ln)*cc1
         poin(2,ln,m)=poin(2,ln,m) + 
     &                  tmom(k)*.25*(1.0+3.0*cc(k))*is*s*cc2      
c >>> m=2
c >>> m=3
c >>> m=4
         if (k.lt.3) then
          rr1=1.0-cc(k)
          if (k.eq.1) rr1=-rr1
          m=4
          poin(1,ln,m)=poin(1,ln,m) - 
     &                  tmom(k)*0.25*s2*alfinv(ln)*rr1*cc1
          poin(2,ln,m)=poin(2,ln,m) - 
     &                  tmom(k)*0.25*is*s*rr1*cc2      
          poin(3,ln,m)=poin(3,ln,m) - 
     &                  tmom(k)*0.25*(s2-cc5)*betinv(ln)*rr1*cc2
         end if
c >>> m=5
         end do
c >>> moment component M12 (strike slip, t=pi/2)
c >>> m=1
c >>> m=2
c >>> m=3
c >>> m=4
c >>> m=5
         m=5
         poin(1,ln,m)=poin(1,ln,m) - m12*s2*alfinv(ln)*cc1
         poin(2,ln,m)=poin(2,ln,m) - is*m12*s*cc2      
         poin(3,ln,m)=poin(3,ln,m) 
     &                 - m12*ak2(ln,2)*betinv(ln)*cc2
c >>> moment component M13 (strike slip, t=0)
c >>> m=1
c >>> m=2
         m=2
         poin(1,ln,m)=poin(1,ln,m) + is*m13*2.0*s*cc1
         poin(2,ln,m)=poin(2,ln,m) + m13*cc7*betinv(ln)*cc2      
         poin(3,ln,m)=poin(3,ln,m) + is*m13*cc6*cc2
c >>> m=3
c >>> m=4
c >>> m=5
c >>> moment component M23 (dip slip, t=pi/2)
c >>> m=1
c >>> m=2
c >>> m=3
        m=3
        poin(1,ln,m)=poin(1,ln,m) + is*m23*2.0*s*cc1
        poin(2,ln,m)=poin(2,ln,m) + m23*cc7*betinv(ln)*cc2      
        poin(3,ln,m)=poin(3,ln,m) + is*m23*cc6*cc2
c >>> m=4
c >>> m=5
        end if
 40   CONTINUE
C
      RETURN
      END

       SUBROUTINE TISOLL     
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX CC,CC0,CC1,CC2,CC3
cvd$r permutation(LAYT)
cvd$  nodepchk
      DO 10 I=1,NUMT(4)
       LN=LAYT(I,4)
C     FIRST ROW IS VERTICAL DISPLACEMENT              
       AUP(LN,1,1)=-ANSTD(7,LN)
       AUP(LN,1,2)=-ANSTD(8,LN)            
       AUP(LN,1,4)=-ANSTD(11,LN)
       AUP(LN,1,5)=-ANSTD(12,LN)
C     SECOND ROW IS HORIZONTAL DISPLACEMENT           
       AUP(LN,2,1)=-ANSTD(5,LN)
       AUP(LN,2,2)=-ANSTD(6,LN)
       AUP(LN,2,3)=-ANSTD(24,LN)
       AUP(LN,2,4)=-ANSTD(9,LN)
       AUP(LN,2,5)=-ANSTD(10,LN)
       AUP(LN,2,6)=-ANSTD(23,LN)
C     THIRD ROW IS HORIZONTAL (RADIAL-ANGULAR) DISPLACEMENT
       AUP(LN,3,1)=ANSTD(5,LN)
       AUP(LN,3,2)=ANSTD(6,LN)
       AUP(LN,3,3)=-ANSTD(24,LN)
       AUP(LN,3,4)= ANSTD(9,LN)
       AUP(LN,3,5)= ANSTD(10,LN)
       AUP(LN,3,6)=-ANSTD(23,LN)
C     THIRD ROW IS NORMAL STRESS  
       AUP(LN,4,1)=-CON1(LN)*ANSTD(13,LN)
       AUP(LN,4,2)=-CON1(LN)*ANSTD(14,LN)
       AUP(LN,4,4)=-CON1(LN)*ANSTD(17,LN)
       AUP(LN,4,5)=-CON1(LN)*ANSTD(18,LN)
C     FIFTH ROW IS SHEAR STRESS    
       AUP(LN,5,1)=-CON1(LN)*ANSTD(15,LN)
       AUP(LN,5,2)=-CON1(LN)*ANSTD(16,LN)
       AUP(LN,5,3)=-CON1(LN)*ANSTD(26,LN)
       AUP(LN,5,4)=-CON1(LN)*ANSTD(19,LN)
       AUP(LN,5,5)=-CON1(LN)*ANSTD(20,LN)
       AUP(LN,5,6)=-CON1(LN)*ANSTD(25,LN)
C     SIXTH ROW IS SHEAR STRESS    
       AUP(LN,6,1)= CON1(LN)*ANSTD(15,LN)
       AUP(LN,6,2)= CON1(LN)*ANSTD(16,LN)
       AUP(LN,6,3)=-CON1(LN)*ANSTD(26,LN)
       AUP(LN,6,4)= CON1(LN)*ANSTD(19,LN)
       AUP(LN,6,5)= CON1(LN)*ANSTD(20,LN)
       AUP(LN,6,6)=-CON1(LN)*ANSTD(25,LN)
 10   continue
C     THE LOWER INTERFACE MATRIX FOLLOWS BY A CHANGE OF SIGN
C     AND MULTLIPICATION WITH THE EXPONENTIALS        
      DO 15 I=1,NLEQ           
cvd$  nodepchk
       DO 15 J=1,NUMT(4)
        LN=LAYT(J,4)
        ALO(LN,I,1)=-AUP(LN,I,1)*EZALFM(LN)                 
        ALO(LN,I,2)=-AUP(LN,I,2)*EZBETM(LN)                 
        ALO(LN,I,3)=-AUP(LN,I,3)*EZGAMM(LN)                 
        ALO(LN,I,4)=-AUP(LN,I,4)                 
        ALO(LN,I,5)=-AUP(LN,I,5)
        ALO(LN,I,6)=-AUP(LN,I,6)
        AUP(LN,I,4)=AUP(LN,I,4)*EZALFM(LN)
        AUP(LN,I,5)=AUP(LN,I,5)*EZBETM(LN)                 
        AUP(LN,I,6)=AUP(LN,I,6)*EZGAMM(LN)                 
 15   CONTINUE
      RETURN                
      END                   
      SUBROUTINE LIQLAY             
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX CC
      COMPLEX CC1,CC2
C
C     ISOVELOCITY FLUID LAYERS
C
cvd$r permutation(LAYT)
cvd$  nodepchk
      DO 10 I=1,NUMT(1)
       LN=LAYT(I,1)
C     FIRST ROW IS VERTICAL DISPLACEMENT  
       AUP(LN,1,1)=ALFA(LN)                
       AUP(LN,1,4)=-ALFA(LN)               
C     SECOND ROW IS HORIZONTAL DISPLACEMENT                  
       AUP(LN,2,1)=WVNO    
       AUP(LN,2,4)=WVNO    
C     THIRD ROW IS NORMAL STRESS          
       AUP(LN,4,1)=CON1(LN)
       AUP(LN,4,4)=CON1(LN)
C     VANISHING SHEAR STRESS
       AUP(LN,5,1)=0E0
       AUP(LN,5,4)=0E0
       AUP(LN,6,1)=0E0
       AUP(LN,6,4)=0E0
 10   CONTINUE
C     THE LOWER INTERFACE MATRIX FOLLOWS BY A CHANGE OF SIGN 
C     AND MULTLIPICATION WITH THE EXPONENTIALS               
      DO 15 I=1,NLEQ      
cvd$  nodepchk
       DO 15 J=1,NUMT(1)
        LN=LAYT(J,1)
        ALO(LN,I,1)=-AUP(LN,I,1)*EZALFM(LN)     
        ALO(LN,I,4)=-AUP(LN,I,4)     
        AUP(LN,I,4)=AUP(LN,I,4)*EZALFM(LN)     
 15   CONTINUE
C *** SOURCE TERMS
cvd$  novector
cvd$  noconcur
      DO 40 J=1,NUMTS(1)
       I=NSPNT(J,1)
       LN=LAYS(I)
       IS=(I-1)*ISINC+1
       M=1
       CC1=1E0/ALFA(LN)
        IF (LN.GT.1) THEN
         CC=CPHFAC(I)*CEXPT(-ZUS(I)*ALFA(LN))
         R(LN-1,1,M,IS)=CC+R(LN-1,1,M,IS)
         R(LN-1,4,M,IS)=-CON1(LN)*CC*CC1+R(LN-1,4,M,IS)          
        END IF
        IF (LN.LT.NUML) THEN
         CC=CPHFAC(I)*CEXPT(-ZLS(I)*ALFA(LN))
         R(LN,1,M,IS)=CC+R(LN,1,M,IS)   
         R(LN,4,M,IS)=CON1(LN)*CC*CC1+R(LN,4,M,IS)             
        END IF
 40   CONTINUE
      RETURN           
      END              
      SUBROUTINE AIRLAY             
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX CC
C
C     AIRY SOLUTION ADDED 840907
C
C     NOTE THE GRADIENT DEPENDENT COLUMN INTERCHANGE
C
      COMPLEX CC1,CC2
      COMPLEX ZETAU(NLA),AIRYU(NLA),BIRYU(NLA),
     &        AIRYDU(NLA),BIRYDU(NLA),ZTAMU(NLA)
      COMPLEX ZETAL(NLA),AIRYL(NLA),BIRYL(NLA),
     &        AIRYDL(NLA),BIRYDL(NLA),ZTAML(NLA)
      COMPLEX      AIRU,BIRU,AIRDU,BIRDU
      COMPLEX      AIRL,BIRL,AIRDL,BIRDL
      COMPLEX ZETAS,AIRYS,BIRYS,AIRYDS,BIRYDS,ZTAMS
cvd$r permutation(LAYT)
cvd$  cncall
cvd$  nodepchk
cvd$  select(concur)
      DO 10 J=1,NUMT(2)
       LN=LAYT(J,2)
        ZETAU(LN)=CCO(LN)*S2-BCO(LN)
        CALL SCAIRY(ZETAU(LN),AIRYU(LN),BIRYU(LN),
     &              AIRYDU(LN),BIRYDU(LN),ZTAMU(LN))
        ZETAL(LN)=ZETAU(LN)-ACO(LN)*THICK(LN)
        CALL SCAIRY(ZETAL(LN),AIRYL(LN),BIRYL(LN),
     &              AIRYDL(LN),BIRYDL(LN),ZTAML(LN))
        IF (REAL(ACO(LN)).LT.0) THEN
          AISC(LN)=ZTAMU(LN)
          BISC(LN)=ZTAML(LN)
        ELSE
          AISC(LN)=ZTAML(LN)
          BISC(LN)=ZTAMU(LN)
        END IF
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
          AUP(LN,1,1)=AIRDU*ACO(LN)
          AUP(LN,1,4)=BIRDU*ACO(LN)
          AUP(LN,2,1)=WVNO
          AUP(LN,4,1)=CON1(LN)*AIRU
          AUP(LN,4,4)=CON1(LN)*BIRU
        ELSE
          AUP(LN,1,1)=BIRDU*ACO(LN)
          AUP(LN,1,4)=AIRDU*ACO(LN)
          AUP(LN,4,1)=CON1(LN)*BIRU
          AUP(LN,4,4)=CON1(LN)*AIRU
        END IF
        IF (REAL(ACO(LN)).LT.0) THEN
          ALO(LN,1,1)=-AIRDL*ACO(LN)
          ALO(LN,1,4)=-BIRDL*ACO(LN)
          ALO(LN,4,1)=-CON1(LN)*AIRL
          ALO(LN,4,4)=-CON1(LN)*BIRL
        ELSE
          ALO(LN,1,1)=-BIRDL*ACO(LN)
          ALO(LN,1,4)=-AIRDL*ACO(LN)
          ALO(LN,4,1)=-CON1(LN)*BIRL
          ALO(LN,4,4)=-CON1(LN)*AIRL
        END IF
 10   CONTINUE
C
C     SOURCES IN AIRY LAYERS INCLUDED 850214
C
cvd$  noconcur     
cvd$  novector
       DO 15 J=1,NUMTS(2)
        I=NSPNT(J,2)
        LN=LAYS(I)
        IS=(I-1)*ISINC+1
        M=1
        ZETAS=CCO(LN)*S2-ACO(LN)*ZUS(I)-BCO(LN)
        CALL SCAIRY(ZETAS,AIRYS,BIRYS,AIRYDS,BIRYDS,ZTAMS)
        CC=2E0*CPHFAC(I)/((-AIRYS*BIRYDS
     1       +BIRYS*AIRYDS)*(-ACO(LN)))
        IF (IDERIV.NE.1) THEN
          CSAIR1(I)=CC*BIRYS
          CSAIR2(I)=CC*AIRYS
        ELSE
          CSAIR1(I)=-ACO(LN)*CC*BIRYDS
          CSAIR2(I)=-ACO(LN)*CC*AIRYDS
        END IF
        CSAIR3(I)=ZTAMS
        CC1=CSAIR1(I)*CEXPT(ZTAMS-BISC(LN))
        CC2=CSAIR2(I)*CEXPT(AISC(LN)-ZTAMS)
        IF (REAL(ACO(LN)).GE.0) THEN
C       NEGATIVE VELOCITY GRADIENT
          R(LN-1,1,M,IS)=R(LN-1,1,M,IS)-ACO(LN)*CC1*AIRYDU(LN)
          R(LN-1,4,M,IS)=R(LN-1,4,M,IS)-CON1(LN)*CC1*AIRYU(LN)
          R(LN,1,M,IS)=R(LN,1,M,IS)+ACO(LN)*CC2*BIRYDL(LN)
          R(LN,4,M,IS)=R(LN,4,M,IS)+CON1(LN)*CC2*BIRYL(LN)
        ELSE
          R(LN-1,1,M,IS)=R(LN-1,1,M,IS)-ACO(LN)*CC2*BIRYDU(LN)
          R(LN-1,4,M,IS)=R(LN-1,4,M,IS)-CON1(LN)*CC2*BIRYU(LN)
          R(LN,1,M,IS)=R(LN,1,M,IS)+ACO(LN)*CC1*AIRYDL(LN)
          R(LN,4,M,IS)=R(LN,4,M,IS)+CON1(LN)*CC1*AIRYL(LN)
        END IF
 15    CONTINUE
      RETURN
      END              
      SUBROUTINE WFIELD(is)        
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX ERALFA,ERBETA,ERALFM,ERBETM,ERGAMA,ERGAMM 
      COMPLEX CC,CC1,CC2,CC3,CC4,CC5,CC6
      if (debug) write(6,*) 'Entering WFIELD'
      call vclr(pot,1,2*nleq*nrd*msuft) 
C
C *** RECEIVERS IN ISOVELOCITY FLUID LAYERS
cvd$  select(concur)
      DO 10 M=1,MSUFT
cvd$  permutation(NUMTR)
cvd$  nodepchk
      DO 10 J=1,NUMTR(1)
       IRCV=NRPNT(J,1)
       LL=LAY(IRCV)      
       ZZ=Z(IRCV)        
       IF (LL.NE.1) THEN
         ERALFM=CEXPT(-ZZ*ALFA(LL))
       ELSE
         ERALFM=0E0
       END IF                 
       IF (LL.NE.NUML) THEN
         ERALFA=CEXPT((ZZ-THICK(LL))*ALFA(LL))
       ELSE
         ERALFA=0E0
       END IF
       pot(1,ircv,m)=SS(LL,1,M,IS)*ERALFM
       pot(4,ircv,m)=SS(LL,4,M,IS)*ERALFA
 10    CONTINUE
c >>> sources
       IF (NUMTS(1).GT.0.AND.NUMTR(1).GT.0) THEN
        CALL LCNEXP()
       END IF
C
C     AIRY SOLUTION IMPLEMENTED 840907
C
C *** RECEIVERS IN non-ISOVELOCITY FLUID LAYERS
      DO 20 M=1,MSUFT
cvd$  permutation(NRPNT)
cvd$  cncall
cvd$  nodepchk
cvd$  select(concur)
      DO 20 J=1,NUMTR(2)
       IRCV=NRPNT(J,2)
       LL=LAY(IRCV)      
       ZZ=Z(IRCV)        
       ZETA(IRCV)=CCO(LL)*S2-ZZ*ACO(LL)-BCO(LL)
       CALL SCAIRY(ZETA(IRCV),AIRY(IRCV),BIRY(IRCV),
     &             AIRYD(IRCV),BIRYD(IRCV),ZTAM(IRCV))
        CC1=CEXPT(AISC(LL)-ZTAM(IRCV))
        CC2=CEXPT(ZTAM(IRCV)-BISC(LL))


        IF ((REAL(ACO(LL))).LT.0) THEN
         POT(1,ircv,m)=SS(LL,1,m,is)*CC1*AIRY(ircv)
         POT(4,ircv,m)=SS(LL,4,m,is)*CC2*BIRY(ircv)
         POT(2,ircv,m)=SS(LL,1,m,is)*CC1*AIRYD(ircv)
         POT(5,ircv,m)=SS(LL,4,m,is)*CC2*BIRYD(ircv)
        ELSE
         POT(1,ircv,m)=SS(LL,4,m,is)*CC1*AIRY(ircv)
         POT(4,ircv,m)=SS(LL,1,m,is)*CC2*BIRY(ircv)
         POT(2,ircv,m)=SS(LL,4,m,is)*CC1*AIRYD(ircv)
         POT(5,ircv,m)=SS(LL,1,m,is)*CC2*BIRYD(ircv)
        END IF
 20   CONTINUE
c >>> sourtces
       IF (NUMTS(2).GT.0.AND.NUMTR(2).GT.0) THEN
        CALL ACNEXP()
       END IF
C *** RECEIVERS IN SOLID LAYERS
cvd$  select(concur)
      DO 30 M=1,MSUFT
cvd$  permutation(NRPNT)
cvd$  nodepchk
      DO 30 J=1,NUMTR(3)
       IRCV=NRPNT(J,3)
       LL=LAY(IRCV)      
       ZZ=Z(IRCV)        
        IF (LL.NE.1) THEN
         ERALFM=CEXPT(-ZZ*ALFA(LL))                 
         ERBETM=CEXPT(-ZZ*BETA(LL))            
        ELSE
         ERALFM=0E0
         ERBETM=0E0
        END IF
        IF (LL.NE.NUML) THEN
         ERALFA=CEXPT((ZZ-THICK(LL))*ALFA(LL))
         ERBETA=CEXPT((ZZ-THICK(LL))*BETA(LL))
        ELSE
         ERALFA=0E0
         ERBETA=0E0
        END IF
        pot(1,ircv,m)=SS(LL,1,M,IS)*ERALFM
        pot(2,ircv,m)=SS(LL,2,M,IS)*ERBETM
        pot(3,ircv,m)=SS(LL,3,M,IS)*ERBETM
        pot(4,ircv,m)=SS(LL,4,M,IS)*ERALFA
        pot(5,ircv,m)=SS(LL,5,M,IS)*ERBETA
        pot(6,ircv,m)=SS(LL,6,M,IS)*ERBETA
c       if (debug.and.ircv.eq.1) then
c        write(6,*) 'ln,m,is=',ll,m,is
c        write(6,*) 'ss=',(ss(ll,jjj,m,is),jjj=1,6)
c        write(6,*) 'pot=',(pot(jjj,ircv,m),jjj=1,6)
c       end if
 30   CONTINUE
c
C *** SOURCE CONTRIBUTIONS
      IF (NUMTS(3).GT.0.AND.NUMTR(3).GT.0) THEN
      if (srctyp.eq.99) then
       if (nstrf.eq.3.and.isprm(1).eq.10.and.isprm(2).eq.11.and.
     &     isprm(3).eq.12) then
        do 205 jj=1,numts(3)
         i=nspnt(jj,3)
         call saxmom(i,sval(i,1)+sval(i,2),sval(i,1))
         aaa=abs(real(sval(i,3))) + abs(aimag(sval(i,3)))
         if (aaa.gt.1E-20) call saxfor(i,sval(i,3))
 205    continue
       else if (nstrf.eq.2.and.isprm(1).eq.10.and.isprm(2).eq.11) then
        do 206 jj=1,numts(3)
         i=nspnt(jj,3)
 206     call saxmom(i,sval(i,1)+sval(i,2),sval(i,1))
       else
        do 210 jj=1,numts(3)
         i=nspnt(jj,3)
        do 210 j=1,nstrf
         if (isprm(j).eq.10) then
          call saxmom(i,sval(i,j),sval(i,j))
         else if (isprm(j).eq.11) then
          call saxmom(i,sval(i,j),cnul)
         else if (isprm(j).eq.12) then
          call saxfor(i,sval(i,j))
         else
          stop ' >>> wfield: Unknown source type in TRF file <<<'
         end if
 210    continue
       end if
      else IF (SRCTYP.EQ.1) THEN
        CALL SCNEXP()
      ELSE IF (SRCTYP.EQ.2) THEN
        CALL SCNFOR()
      ELSE IF (SRCTYP.EQ.3) THEN
        CALL SCNDIP()
      ELSE IF (SRCTYP.EQ.4) THEN
        CALL SCNSTR()
      else if (srctyp.eq.5.or.srctyp.eq.6) then
        call scnten()
      else if (srctyp.eq.7) then
c >>> seismic moment source
        call scnmom()
      END IF
      END IF      
c       if (debug) then
c        write(6,*) 'after sources:'
c        write(6,*) 'pot=',(pot(jjj,1,1),jjj=1,6)
c       end if
C *** RECEIVERS IN TRANSVERSILY ISOTROPIC LAYERS
cvd$  select(concur)
      DO 40 M=1,MSUFT
cvd$  permutation(NRPNT)
cvd$  nodepchk
      DO 40 J=1,NUMTR(4)
       IRCV=NRPNT(J,4)
       LL=LAY(IRCV)      
       ZZ=Z(IRCV)        
        IF (LL.NE.1) THEN
         ERALFM=CEXPT(-ZZ*ALFA(LL))                 
         ERBETM=CEXPT(-ZZ*BETA(LL))            
         ERGAMM=CEXPT(-ZZ*GAMA(LL))            
        ELSE
         ERALFM=0E0
         ERBETM=0E0
         ERGAMM=0E0
        END IF
        IF (LL.NE.NUML) THEN
         ERALFA=CEXPT((ZZ-THICK(LL))*ALFA(LL))
         ERBETA=CEXPT((ZZ-THICK(LL))*BETA(LL))
         ERGAMA=CEXPT((ZZ-THICK(LL))*GAMA(LL))
        ELSE
         ERALFA=0E0
         ERBETA=0E0
         ERGAMA=0E0
        END IF
        pot(1,ircv,m)=SS(LL,1,M,IS)*ERALFM
        pot(2,ircv,m)=SS(LL,2,M,IS)*ERBETM
        pot(3,ircv,m)=SS(LL,3,M,IS)*ERGAMM
        pot(4,ircv,m)=SS(LL,4,M,IS)*ERALFA
        pot(5,ircv,m)=SS(LL,5,M,IS)*ERBETA
        pot(6,ircv,m)=SS(LL,6,M,IS)*ERGAMA
 40   continue
      if (debug) write(6,*) 'Exiting WFIELD'
c       if (debug) then
c        write(6,*) 'end of wfield:'
c        write(6,*) 'pot=',(pot(jjj,1,1),jjj=1,6)
c       end if
      RETURN           
      END              
      SUBROUTINE KERNEL(CKERN)        
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX CKERN(IR,MSUFT,ISROW,NPAR)
      COMPLEX ERALFA,ERBETA,ERALFM,ERBETM,ERGAMA,ERGAMM 
      COMPLEX CC,CC1,CC2,CC3,CC4,CC5,CC6
      if (debug) write(6,*) 'Entering KERNEL'
c >>> Loop over individual sources
c      DO 500 IS=1,ISROW
      is=1
c >>> compute wave field
      call wfield(is)
       if (debug) then
        write(6,*) 'after wfield:'
        write(6,*) 'pot(i,1,1)=',(pot(jjj,1,1),jjj=1,6)
       end if
      call vclr(ckern,1,2*ir*msuft*isrow*npar)
C
C *** RECEIVERS IN ISOVELOCITY FLUID LAYERS
cvd$  select(concur)
      DO 10 M=1,MSUFT
cvd$  permutation(NUMTR)
cvd$  nodepchk
      DO 10 J=1,NUMTR(1)
       IRCV=NRPNT(J,1)
       LL=LAY(IRCV)      
       CC1=pot(1,ircv,m)
       CC3=pot(4,ircv,m)
       CKERN(IRCV,M,IS,1)=-CON1(LL)*(CC1+CC3)
       CKERN(IRCV,M,IS,2)=ALFA(LL)*(-CC1+CC3)
       CKERN(IRCV,M,IS,3)=-WVNO*(CC1+CC3)
       CKERN(IRCV,M,IS,4)= WVNO*(CC1+CC3)
       CKERN(IRCV,M,IS,5)=CKERN(IRCV,M,IS,1)
       CKERN(IRCV,M,IS,6)=CKERN(IRCV,M,IS,1)
 10    CONTINUE
C
C     AIRY SOLUTION IMPLEMENTED 840907
C
C *** RECEIVERS IN non-ISOVELOCITY FLUID LAYERS
      DO 20 M=1,MSUFT
cvd$  permutation(NRPNT)
cvd$  cncall
cvd$  nodepchk
cvd$  select(concur)
      DO 20 J=1,NUMTR(2)
       IRCV=NRPNT(J,2)
       LL=LAY(IRCV)      
        CKERN(IRCV,M,IS,1)=-CON1(LL)*(pot(1,ircv,m)+pot(4,ircv,m))
        CKERN(IRCV,M,IS,2)=-ACO(LL)*(pot(2,ircv,m)+pot(5,ircv,m))
        CKERN(IRCV,M,IS,3)=-WVNO*(pot(1,ircv,m)+pot(4,ircv,m))          
        CKERN(IRCV,M,IS,4)= WVNO*(pot(1,ircv,m)+pot(4,ircv,m))          
        CKERN(IRCV,M,IS,5)=CKERN(IRCV,M,IS,1)
        CKERN(IRCV,M,IS,6)=CKERN(IRCV,M,IS,1)
 20   CONTINUE
C *** RECEIVERS IN SOLID LAYERS
cvd$  select(concur)
      DO 30 M=1,MSUFT
cvd$  permutation(NRPNT)
cvd$  nodepchk
      DO 30 J=1,NUMTR(3)
       IRCV=NRPNT(J,3)
       LL=LAY(IRCV)      
        CC1=pot(1,ircv,m)
        CC2=pot(2,ircv,m)
        CC5=pot(3,ircv,m)
        CC3=pot(4,ircv,m)
        CC4=pot(5,ircv,m)
        CC6=pot(6,ircv,m)
        CKERN(IRCV,M,IS,1)=CON2(LL)*(CC1+CC3)+CON4(LL)*(CC4-CC2)
        CKERN(IRCV,M,IS,2)=ALFA(LL)*(CC3-CC1)+WVNO*(CC2+CC4)
        CKERN(IRCV,M,IS,3)=-WVNO*(CC1+CC3)+BETA(LL)*(CC2-CC4)
     &                    +WVNO*(CC5+CC6)
        CKERN(IRCV,M,IS,4)= WVNO*(CC1+CC3)-BETA(LL)*(CC2-CC4)
     &                    +WVNO*(CC5+CC6)
        CKERN(IRCV,M,IS,5)=-CON5(LL)*(CC1+CC3)-CON4(LL)*(CC4-CC2)
        CKERN(IRCV,M,IS,6)=-CON6(LL)*(CC1+CC3)
c >>> sigma_rz +sigma_zt
        ckern(ircv,m,is,7)= con3(ll)*(cc1-cc3)-con2(ll)*(cc2+cc4)
     &                     -0.5*con4(ll)*(cc5-cc6)
c >>> sigma_rz -sigma_zt
        ckern(ircv,m,is,8)=-con3(ll)*(cc1-cc3)+con2(ll)*(cc2+cc4)
     &                     -0.5*con4(ll)*(cc5-cc6)
c       if (debug.and.ircv.eq.1) then
c        write(6,*) 'ln,m,is=',ll,m,is
c        write(6,*) 'pot=',(pot(jjj,ircv,m),jjj=1,6)
c        write(6,*) 'ckern=',(ckern(ircv,m,is,jjj),jjj=1,5)
c       end if
 30   CONTINUE
C *** RECEIVERS IN TRANSVERSILY ISOTROPIC LAYERS
cvd$  select(concur)
      DO 40 M=1,MSUFT
cvd$  permutation(NRPNT)
cvd$  nodepchk
      DO 40 J=1,NUMTR(4)
       IRCV=NRPNT(J,4)
       LL=LAY(IRCV)      
        CC1=pot(1,ircv,m)
        CC2=pot(2,ircv,m)
        CC5=pot(3,ircv,m)
        CC3=pot(4,ircv,m)
        CC4=pot(5,ircv,m)
        CC6=pot(6,ircv,m)
        CKERN(IRCV,M,IS,1)=CON1(LL)*
     &       (ANSTD(13,LL)*CC1+ANSTD(14,LL)*CC2+
     &        ANSTD(17,LL)*CC3+ANSTD(18,LL)*CC4)
        CKERN(IRCV,M,IS,2)=(ANSTD(7,LL)*CC1+ANSTD(8,LL)*CC2+
     &                      ANSTD(11,LL)*CC3+ANSTD(12,LL)*CC4)
        CKERN(IRCV,M,IS,3)=( ANSTD(5,LL)*CC1+ANSTD(6,LL)*CC2
     &                      +ANSTD(9,LL)*CC3+ANSTD(10,LL)*CC4
     &                      +ANSTD(24,LL)*CC5+ANSTD(23,LL)*CC6)
        CKERN(IRCV,M,IS,4)=-( ANSTD(5,LL)*CC1+ANSTD(6,LL)*CC2
     &                      +ANSTD(9,LL)*CC3+ANSTD(10,LL)*CC4)
     &                      +ANSTD(24,LL)*CC5+ANSTD(23,LL)*CC6
        CKERN(IRCV,M,IS,5)=CON1(LL)*
     &       (ANSTD(29,LL)*CC1+ANSTD(30,LL)*CC2+
     &        ANSTD(27,LL)*CC3+ANSTD(28,LL)*CC4)
c *** Bulk stress *********************
        CKERN(ircv,m,is,6)=CON1(LL)*
     &       (ANSTD(37,LL)*CC1+ANSTD(38,LL)*CC2+
     &        ANSTD(35,LL)*CC3+ANSTD(36,LL)*CC4)

 40   continue
      if (debug) then
       write(6,*) 'KERNEL: before 500, CPFAC,CWUFAC=',cpfac,cwufac
       write(6,*) 'ckern(1,1,1,i)='
       write(6,*) (ckern(1,1,1,jjj),jjj=1,5)       
       end if
      DO 500 M=1,MSUFT
      DO 500 J=1,IR
       CKERN(J,M,IS,1)=CKERN(J,M,IS,1)*CPFAC
       CKERN(J,M,IS,2)=CKERN(J,M,IS,2)*CWUFAC
       CKERN(J,M,IS,3)=CKERN(J,M,IS,3)*CWUFAC
       CKERN(J,M,IS,4)=CKERN(J,M,IS,4)*CWUFAC
       CKERN(J,M,IS,5)=CKERN(J,M,IS,5)*CPFAC
       CKERN(J,M,IS,6)=CKERN(J,M,IS,6)*CPFAC
       CKERN(J,M,IS,7)=CKERN(J,M,IS,7)*CPFAC
       CKERN(J,M,IS,8)=CKERN(J,M,IS,8)*CPFAC
 500  CONTINUE
      if (debug) write(6,*) 'Exiting KERNEL'
      RETURN           
      END              
      SUBROUTINE KERDEC(CKERN)        
C 
C     DETERMINES THE DECOMPOSED WAVEFIELD KERNELS.
C     ON EXIT, THE PARAMETERS ARE PLACED IN ARRAY CKERN(I,M,IS,J,K)
C     AS FOLLOWS:
C
C     CKERN(I,M,IS,1,K)	NORMAL STRESS Szz, RECEIVER I.
C     CKERN(I,M,IS,2,K) Wm               , RECEIVER I.
C     CKERN(I,M,IS,3,K)	Um+Vm            , RECEIVER I.
C     CKERN(I,M,IS,4,K)	Um-Vm            , RECEIVER I.
C     CKERN(I,M,IS,5,K)	Radial stress Srr, RECEIVER I.
C     CKERN(I,M,IS,5,K)	Bulk stress (Srr+Szz+Stt)/3 , RECEIVER I.
C
C     CKERN(I,M,IS,J,1)	TOTAL KERNEL
C     CKERN(I,M,IS,J,2)	DOWN-GOING COMPRESSIONAL WAVES ONLY
C     CKERN(I,M,IS,J,3) DOWN-GOING SV WAVES ONLY
C     CKERN(I,M,IS,J,4) DOWN-GOING SH WAVES ONLY
C     CKERN(I,M,IS,J,5)	UP-GOING COMPRESSIONAL WAVES ONLY
C     CKERN(I,M,IS,J,6)	UP-GOING SV WAVES ONLY
C     CKERN(I,M,IS,J,7)	UP-GOING SH WAVES ONLY
C 
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX CKERN(IR,MSUFT,ISROW,NPAR,7)
      COMPLEX ERALFA,ERBETA,ERALFM,ERBETM,ERGAMA,ERGAMM 
      COMPLEX CC,CC1,CC2,CC3,CC4,CC5,CC6

c      DO 500 IS=1,ISROW
      is=1
c >>> compute wave field
      call wfield(is)
       if (debug) then
        write(6,*) 'after wfield:'
        write(6,*) 'pot(i,1,1)=',(pot(jjj,1,1),jjj=1,6)
       end if
      call vclr(ckern,1,14*ir*msuft*isrow*npar)
C
C *** RECEIVERS IN ISOVELOCITY FLUID LAYERS
cvd$  select(concur)
      DO 10 M=1,MSUFT
cvd$  permutation(NUMTR)
cvd$  nodepchk
      DO 10 J=1,NUMTR(1)
       IRCV=NRPNT(J,1)
       LL=LAY(IRCV)      
       CC1=pot(1,ircv,m)
       CC3=pot(4,ircv,m)
        CKERN(IRCV,M,IS,1,2)=-CON1(LL)*CC1
        CKERN(IRCV,M,IS,1,4)=-CON1(LL)*CC3
        CKERN(IRCV,M,IS,2,2)=-ALFA(LL)*CC1
        CKERN(IRCV,M,IS,2,4)= ALFA(LL)*CC3
        CKERN(IRCV,M,IS,3,2)=-WVNO*CC1
        CKERN(IRCV,M,IS,3,4)=-WVNO*CC3
        do ipp=2,7
         ckern(ircv,m,is,4,ipp)=-ckern(ircv,m,is,3,ipp)
         ckern(ircv,m,is,5,ipp)=ckern(ircv,m,is,1,ipp)
         ckern(ircv,m,is,6,ipp)=ckern(ircv,m,is,1,ipp)
        end do
 10    CONTINUE
C
C     AIRY SOLUTION IMPLEMENTED 840907
C
C *** RECEIVERS IN non-ISOVELOCITY FLUID LAYERS
      DO 20 M=1,MSUFT
cvd$  permutation(NRPNT)
cvd$  cncall
cvd$  nodepchk
cvd$  select(concur)
      DO 20 J=1,NUMTR(2)
       IRCV=NRPNT(J,2)
       LL=LAY(IRCV)      
        cc1=pot(1,ircv,m)
        cc2=pot(2,ircv,m)
        cc3=pot(4,ircv,m)
        cc4=pot(5,ircv,m)
        CKERN(IRCV,M,IS,1,2)=-CON1(LL)*cc1
        CKERN(IRCV,M,IS,1,4)=-CON1(LL)*cc3
        CKERN(IRCV,M,IS,2,2)=-ACO(LL)*cc2
        CKERN(IRCV,M,IS,2,4)=-ACO(LL)*cc4
        CKERN(IRCV,M,IS,3,2)=-WVNO*cc1
        CKERN(IRCV,M,IS,3,4)=-WVNO*cc3
        do ipp=2,7
         ckern(ircv,m,is,4,ipp)=-ckern(ircv,m,is,3,ipp)
         ckern(ircv,m,is,5,ipp)=ckern(ircv,m,is,1,ipp)
         ckern(ircv,m,is,6,ipp)=ckern(ircv,m,is,1,ipp)
        end do
 20   CONTINUE
C *** RECEIVERS IN SOLID LAYERS
cvd$  select(concur)
      DO 30 M=1,MSUFT
cvd$  permutation(NRPNT)
cvd$  nodepchk
      DO 30 J=1,NUMTR(3)
       IRCV=NRPNT(J,3)
       LL=LAY(IRCV)      
        CC1=pot(1,ircv,m)
        CC2=pot(2,ircv,m)
        CC5=pot(3,ircv,m)
        CC3=pot(4,ircv,m)
        CC4=pot(5,ircv,m)
        CC6=pot(6,ircv,m)
c >>> normal stress
        CKERN(IRCV,M,IS,1,2)=CON2(LL)*CC1
        CKERN(IRCV,M,IS,1,5)=CON2(LL)*CC3
        CKERN(IRCV,M,IS,1,3)=-CON4(LL)*CC2
        CKERN(IRCV,M,IS,1,6)=CON4(LL)*CC4
c >>> Wm
        CKERN(IRCV,M,IS,2,2)=-ALFA(LL)*CC1
        CKERN(IRCV,M,IS,2,5)=ALFA(LL)*CC3
        CKERN(IRCV,M,IS,2,3)=WVNO*CC2
        CKERN(IRCV,M,IS,2,6)=WVNO*CC4
c >>> Um+Vm
        CKERN(IRCV,M,IS,3,2)=-WVNO*CC1
        CKERN(IRCV,M,IS,3,5)=-WVNO*CC3
        CKERN(IRCV,M,IS,3,3)=BETA(LL)*CC2
        CKERN(IRCV,M,IS,3,6)=-BETA(LL)*CC4
c     SH
        CKERN(IRCV,M,IS,3,4)=WVNO*CC5
        CKERN(IRCV,M,IS,3,7)=WVNO*CC6
c >>> Um-Vm
        CKERN(IRCV,M,IS,4,2)=WVNO*CC1
        CKERN(IRCV,M,IS,4,5)=WVNO*CC3
        CKERN(IRCV,M,IS,4,3)=-BETA(LL)*CC2
        CKERN(IRCV,M,IS,4,6)=BETA(LL)*CC4
c     SH
        CKERN(IRCV,M,IS,4,4)=WVNO*CC5
        CKERN(IRCV,M,IS,4,7)=WVNO*CC6
c >>> Srr
        CKERN(IRCV,M,IS,5,2)=-CON5(LL)*CC1
        ckern(ircv,m,is,5,5)=-con5(ll)*cc3
        ckern(ircv,m,is,5,3)=coN4(LL)*CC2
        ckern(ircv,m,is,5,6)=-con4(ll)*cc4
c >>> Bulk stress
        ckern(ircv,m,is,6,2)=-con6(ll)*cc1
        ckern(ircv,m,is,6,5)=-con6(ll)*cc3
c >>> sigma_rz +sigma_zt
        ckern(ircv,m,is,7,2)=   con3(ll)*cc1
        ckern(ircv,m,is,7,5)= - con3(ll)*cc3
        ckern(ircv,m,is,7,3)= - con2(ll)*cc2
        ckern(ircv,m,is,7,6)= - con2(ll)*cc4
        ckern(ircv,m,is,7,4)= - 0.5*con4(ll)*cc5
        ckern(ircv,m,is,7,7)=   0.5*con4(ll)*cc6
c >>> sigma_rz -sigma_zt
        ckern(ircv,m,is,7,2)= - con3(ll)*cc1
        ckern(ircv,m,is,7,5)=   con3(ll)*cc3
        ckern(ircv,m,is,7,3)=   con2(ll)*cc2
        ckern(ircv,m,is,7,6)=   con2(ll)*cc4
        ckern(ircv,m,is,7,4)= - 0.5*con4(ll)*cc5
        ckern(ircv,m,is,7,7)=   0.5*con4(ll)*cc6


 30   CONTINUE
C *** RECEIVERS IN TRANSVERSILY ISOTROPIC LAYERS
cvd$  select(concur)
      DO 40 M=1,MSUFT
cvd$  permutation(NRPNT)
cvd$  nodepchk
      DO 40 J=1,NUMTR(4)
       IRCV=NRPNT(J,4)
       LL=LAY(IRCV)      
        CC1=pot(1,ircv,m)
        CC2=pot(2,ircv,m)
        CC5=pot(3,ircv,m)
        CC3=pot(4,ircv,m)
        CC4=pot(5,ircv,m)
        CC6=pot(6,ircv,m)
        CKERN(IRCV,M,IS,1,2)=CON1(LL)*ANSTD(13,LL)*CC1
        CKERN(IRCV,M,IS,1,3)=CON1(LL)*ANSTD(14,LL)*CC2
        CKERN(IRCV,M,IS,1,5)=CON1(LL)*ANSTD(17,LL)*CC3
        CKERN(IRCV,M,IS,1,6)=CON1(LL)*ANSTD(18,LL)*CC4
        CKERN(IRCV,M,IS,2,2)=ANSTD(7,LL)*CC1
        CKERN(IRCV,M,IS,2,3)=ANSTD(8,LL)*CC2
        CKERN(IRCV,M,IS,2,5)=ANSTD(11,LL)*CC3
        CKERN(IRCV,M,IS,2,6)=ANSTD(12,LL)*CC4
c >>> Um+Vm
        CKERN(IRCV,M,IS,3,2)=ANSTD(5,LL)*CC1
        CKERN(IRCV,M,IS,3,3)=ANSTD(6,LL)*CC2
        CKERN(IRCV,M,IS,3,4)=ANSTD(24,LL)*CC5
        CKERN(IRCV,M,IS,3,5)=ANSTD(9,LL)*CC3
        CKERN(IRCV,M,IS,3,6)=ANSTD(10,LL)*CC4
        CKERN(IRCV,M,IS,3,7)=ANSTD(23,LL)*CC6
c >>> Um-Vm
        CKERN(IRCV,M,IS,4,2)=-ANSTD(5,LL)*CC1
        CKERN(IRCV,M,IS,4,3)=-ANSTD(6,LL)*CC2
        CKERN(IRCV,M,IS,4,4)=ANSTD(24,LL)*CC5
        CKERN(IRCV,M,IS,4,5)=-ANSTD(9,LL)*CC3
        CKERN(IRCV,M,IS,4,6)=-ANSTD(10,LL)*CC4
        CKERN(IRCV,M,IS,4,7)=ANSTD(23,LL)*CC6
c *** radial stress *********************
        CKERN(ircv,m,is,5,2)=CON1(LL)*ANSTD(29,LL)*CC1
        CKERN(ircv,m,is,5,3)=CON1(LL)*ANSTD(30,LL)*CC2
        CKERN(ircv,m,is,5,5)=CON1(LL)*ANSTD(27,LL)*CC3
        CKERN(ircv,m,is,5,6)=CON1(LL)*ANSTD(28,LL)*CC4
c *** Bulk stress *********************
        CKERN(ircv,m,is,6,2)=CON1(LL)*ANSTD(37,LL)*CC1
        CKERN(ircv,m,is,6,3)=CON1(LL)*ANSTD(38,LL)*CC2
        CKERN(ircv,m,is,6,5)=CON1(LL)*ANSTD(35,LL)*CC3
        CKERN(ircv,m,is,6,6)=CON1(LL)*ANSTD(36,LL)*CC4
 40   continue
c >>> Re-normalization
      do 500 ipp=1,7
      DO 500 M=1,MSUFT
      DO 500 J=1,IR
       CKERN(J,M,IS,1,ipp)=CKERN(J,M,IS,1,ipp)*CPFAC
       CKERN(J,M,IS,2,ipp)=CKERN(J,M,IS,2,ipp)*CWUFAC
       CKERN(J,M,IS,3,ipp)=CKERN(J,M,IS,3,ipp)*CWUFAC
       CKERN(J,M,IS,4,ipp)=CKERN(J,M,IS,4,ipp)*CWUFAC
       CKERN(J,M,IS,5,ipp)=CKERN(J,M,IS,5,ipp)*CPFAC
       CKERN(J,M,IS,6,ipp)=CKERN(J,M,IS,6,ipp)*CPFAC
       CKERN(J,M,IS,7,ipp)=CKERN(J,M,IS,7,ipp)*CPFAC
       CKERN(J,M,IS,8,ipp)=CKERN(J,M,IS,8,ipp)*CPFAC
 500  CONTINUE
C *** TOTAL FIELD
      DO 600 IPD=2,7
       DO 600 IPP=1,npar
        do 600 m=1,msuft
         DO 600 J=1,IR
          CKERN(J,M,IS,Ipp,1)=CKERN(J,m,is,ipp,1)
     &                       +CKERN(J,m,is,ipp,IPD)
 600   CONTINUE
      if (debug) write(6,*) 'Exiting KERDEC'
      return
      end
      SUBROUTINE LCNEXP()
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     LCNEXP : OASES VERSION
C             
C              MAY 15, 1988 MODIFIED BY JKIM
C              (1) SOURCES ALONG DEPTH TREATED SEPARATELY
C
C     SOURCE CONT. EXPLOSIVE SOURCE IN LIQUID MEDIUM
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX S,cc1,cc2
      if (debug) write(6,*) 'Entering LCNEXP'
C
      M=1
cvd$  novector
cvd$  concur
cvd$  nodepchk
      DO 50 J=1,NUMTR(1)
       IRCV=NRPNT(J,1)
       LN=LAY(IRCV)
       ZZ=Z(IRCV)
C *** SOURCE CONTRIBUTION
       IF (NOSOU(LN).GT.0) THEN
cvd$  novector
         DO 120 I=IFSOU(LN),ILSOU(LN)
          zdif=rdc(ircv)-sdc(i)
          is=nint(sign(1.0,zdif))
          iwav=nint(2.5-is*1.5)
          cc1=cphfac(i)*cexpt(-abs(zdif)*alfa(ln))*alfinv(ln)
          pot(iwav,ircv,m)=pot(iwav,ircv,m)+cc1
 120     CONTINUE
       END IF
 50   continue
      if (debug) write(6,*) 'Exiting INITS'
      RETURN
      END
      SUBROUTINE ACNEXP()
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     ACNEXP : OASES VERSION
C             
C              MAY 15, 1988 MODIFIED BY JKIM
C              (1) SOURCES ALONG DEPTH TREATED SEPARATELY
C
C     SOURCE CONT. EXPLOSIVE SOURCE IN INHOM. LIQUID MEDIUM
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX S,CC3
C
      M=1
cvd$  novector
cvd$  concur
cvd$  nodepchk
      DO 50 J=1,NUMTR(2)
       IRCV=NRPNT(J,2)
       LN=LAY(IRCV)
       ZZ=Z(IRCV)
C
C      SOURCES INCLUDED IN AIRY LAYERS 840214
C
        IF (NOSOU(LN).GT.0) THEN
cvd$  novector
         DO 5 I=IFSOU(LN),ILSOU(LN)
         IF (REAL(ACO(LN))*(rdc(ircv)-sdc(I)).LE.0.) THEN
          CC3=CEXPT(CSAIR3(I)-ZTAM(IRCV))
          pot(1,ircv,m)=pot(1,ircv,m)+AIRY(IRCV)*CSAIR1(I)*CC3
          pot(2,ircv,m)=pot(2,ircv,m)+AIRYD(IRCV)*CSAIR1(I)*CC3
         ELSE
          CC3=CEXPT(ZTAM(IRCV)-CSAIR3(I))
          pot(4,ircv,m)=pot(4,ircv,m)+BIRY(IRCV)*CSAIR2(I)*CC3
          pot(5,ircv,m)=pot(5,ircv,m)+BIRYD(IRCV)*CSAIR2(I)*CC3
         END IF
 5       CONTINUE
        END IF
 50   continue
      RETURN
      END
      SUBROUTINE SCNEXP()
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     SOLEXP : OASES VERSION
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
      COMPLEX S,CC1
C
      m=1
cvd$  novector
cvd$  concur
cvd$  nodepchk
      DO 50 J=1,NUMTR(3)
       IRCV=NRPNT(J,3)
       LN=LAY(IRCV)
       ZZ=Z(IRCV)
       IF (NOSOU(LN).GT.0) THEN
         DO 40 I=IFSOU(LN),ILSOU(LN)
          zdif=rdc(ircv)-sdc(i)
          is=nint(sign(1.0,zdif))
          iwav=nint(2.5-is*1.5)
          cc1=cphfac(i)*cexpt(-abs(zdif)*alfa(ln))*alfinv(ln)
          pot(iwav,ircv,m)=pot(iwav,ircv,m)+cc1
 40       continue
       END IF
 50   continue
      RETURN
      END
      SUBROUTINE SCNFOR()
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     SCNFOR : OASES VERSION
C
C     IMPLEMENTING POINT FORCE SOURCE IN SOLID MEDIUM, 120987 BY JKIM
C     MODIFIED BY FOR MOVING SOURCE MAY 15, 1988
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX S,CC1,CC2,CC3
C
      S=WVNO
cvd$  novector
cvd$  concur
cvd$  nodepchk
      DO 50 J=1,NUMTR(3)
       IRCV=NRPNT(J,3)
       LN=LAY(IRCV)
       ZZ=Z(IRCV)
       IF (NOSOU(LN).GT.0) THEN
        cc3=ak2(ln,2)/(s*beta(ln))      
cvd$  novector
        DO 40 I=IFSOU(LN),ILSOU(LN)
          zdif=rdc(ircv)-sdc(i)
          is=nint(sign(1.0,zdif))
          iwav1=nint(2.5-is*1.5)
          iwav2=iwav1+1
          iwav3=iwav1+2
          cc1=cphfac(i)*cexpt(-abs(rdc(ircv)-sdc(i))*alfa(ln))
          cc2=cphfac(i)*cexpt(-abs(rdc(ircv)-sdc(i))*beta(ln))
c >>> m=1
        m=1
        pot(iwav1,ircv,m)=pot(iwav1,ircv,m) + is* F3*cc1
        pot(iwav2,ircv,m)=pot(iwav2,ircv,m) + F3*cc2*s*betinv(ln)      
c >>> m=2
        m=2
        pot(iwav1,ircv,m)=pot(iwav1,ircv,m) + F1*cc1*s*alfinv(ln)
        pot(iwav2,ircv,m)=pot(iwav2,ircv,m) + is* F1*cc2
        pot(iwav3,ircv,m)=pot(iwav3,ircv,m) + F1*cc3*cc2      
c >>> m=3
        m=3
        pot(iwav1,ircv,m)=pot(iwav1,ircv,m) + F2*cc1*s*alfinv(ln)
        pot(iwav2,ircv,m)=pot(iwav2,ircv,m) + is* F2*cc2      
        pot(iwav3,ircv,m)=pot(iwav3,ircv,m) + F2*cc3*cc2      
 40     CONTINUE
       END IF
 50   CONTINUE
      RETURN
      END
      SUBROUTINE SCNDIP()
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     SCNDIP : OASES VERSION
C
C     IMPLEMENTING DIP-SLIP SOURCE IN SOLID MEDIUM, 120987 BY JKIM
C     MODIFIED BY FOR MOVING SOURCE MAY 15, 1988
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX S,CC1,CC2,CC3,CC4,cc5
C
      S=WVNO
cvd$  novector
cvd$  concur
cvd$  nodepchk
      DO 50 J=1,NUMTR(3)
       IRCV=NRPNT(J,3)
       LN=LAY(IRCV)
       ZZ=Z(IRCV)
       IF (NOSOU(LN).GT.0) THEN
       cc=rmom*cos(2.0*rdelta)
       cs=rmom*sin(2.0*rdelta)
       cc3=(2.0*alfa(ln)+s2*alfinv(ln))
       cc4=ak2(ln,2)/s
       cc5=2e0*s2-ak2(ln,2)
cvd$  novector
        DO 40 I=IFSOU(LN),ILSOU(LN)
          zdif=rdc(ircv)-sdc(i)
          is=nint(sign(1.0,zdif))
          iwav1=nint(2.5-is*1.5)
          iwav2=iwav1+1
          iwav3=iwav1+2
          cc1=cphfac(i)*cexpt(-abs(rdc(ircv)-sdc(i))*alfa(ln))
          cc2=cphfac(i)*cexpt(-abs(rdc(ircv)-sdc(i))*beta(ln))
c >>> m=1
        m=1
        pot(iwav1,ircv,m)=pot(iwav1,ircv,m) - 0.5*cs*cc3*cc1
        pot(iwav2,ircv,m)=pot(iwav2,ircv,m) - is* 1.5*cs* s *cc2      
c >>> m=2

c >>> m=3
        m=3
        pot(iwav1,ircv,m)=pot(iwav1,ircv,m) - is* 2.0*cc*s*cc1
        pot(iwav2,ircv,m)=pot(iwav2,ircv,m)-cc*cc5*betinv(ln)*cc2      
        pot(iwav3,ircv,m)=pot(iwav3,ircv,m) - is* cc*cc4*cc2
c >>> m=4
        m=4
        pot(iwav1,ircv,m)=pot(iwav1,ircv,m) - 0.5*cs*s2*alfinv(ln)*cc1
        pot(iwav2,ircv,m)=pot(iwav2,ircv,m) - is* 0.5*cs*s*cc2      
        pot(iwav3,ircv,m)=pot(iwav3,ircv,m) 
     &                 - 0.5*cs*ak2(ln,2)*betinv(ln)*cc2
c >>> m=5

 40     CONTINUE
       END IF
 50   CONTINUE
      RETURN
      END

      SUBROUTINE SCNSTR()
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     SCNSTR : OASES VERSION
C
C     STRIKE-SLIP SOURCE IN SOLID MEDIUM.
c     Sep 15, 1994: Changed sign of P-potential in strike slip sources
c                   Now consistent with jkim appendix. 
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX S,CC1,CC2,CC3,CC4,cc5
C
      S=WVNO
cvd$  novector
cvd$  concur
cvd$  nodepchk
      DO 50 J=1,NUMTR(3)
       IRCV=NRPNT(J,3)
       LN=LAY(IRCV)
       ZZ=Z(IRCV)
       IF (NOSOU(LN).GT.0) THEN
        cc=rmom*cos(rdelta)
        cs=rmom*sin(rdelta)
        cc4=ak2(ln,2)/s
        cc5=2e0*s2-ak2(ln,2)
cvd$  novector
        DO 40 I=IFSOU(LN),ILSOU(LN)
          zdif=rdc(ircv)-sdc(i)
          is=nint(sign(1.0,zdif))
          iwav1=nint(2.5-is*1.5)
          iwav2=iwav1+1
          iwav3=iwav1+2
          cc1=cphfac(i)*cexpt(-abs(rdc(ircv)-sdc(i))*alfa(ln))
          cc2=cphfac(i)*cexpt(-abs(rdc(ircv)-sdc(i))*beta(ln))
c >>> m=1
c >>> m=2
        m=2
        pot(iwav1,ircv,m)=pot(iwav1,ircv,m) + is* 2.0*cc*s*cc1
        pot(iwav2,ircv,m)=pot(iwav2,ircv,m) + cc*cc5*betinv(ln)*cc2      
        pot(iwav3,ircv,m)=pot(iwav3,ircv,m) + is* cc*cc4*cc2
c >>> m=3

c >>> m=4

c >>> m=5
        m=5
        pot(iwav1,ircv,m)=pot(iwav1,ircv,m) - cs*s2*alfinv(ln)*cc1
        pot(iwav2,ircv,m)=pot(iwav2,ircv,m) - is*cs*s*cc2      
        pot(iwav3,ircv,m)=pot(iwav3,ircv,m) 
     &                 - cs*ak2(ln,2)*betinv(ln)*cc2

 40     CONTINUE
       END IF
 50   CONTINUE
      RETURN
      END

      SUBROUTINE SCNten()
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     SCNten : OASES VERSION
C
C     IMPLEMENTING tensile crack SOURCE IN SOLID MEDIUM, 120987 BY JKIM
C     MODIFIED BY FOR MOVING SOURCE MAY 15, 1988
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX S,CC1,CC2,CC3,CC4,cc5
      complex ctmom(3)
      real cc(3),cs(3)
C
      S=WVNO
C
cvd$  novector
cvd$  concur
cvd$  nodepchk
        do k=1,3
         cc(k)=cos(2*tendel(k))
         cs(k)=sin(2*tendel(k))
        end do
      DO 50 J=1,NUMTR(3)
       IRCV=NRPNT(J,3)
       LN=LAY(IRCV)
       ZZ=Z(IRCV)
       IF (NOSOU(LN).GT.0) THEN
C
C     FOR TENSION CRACK, DEFAULT EQUIVALENT BODY FORCES ARE USED
C     WHILE, FOR SRCTYP=6, BODY FORCE COMPONENTS CAN BE SPECIFIED ACCORDINGLY
C
        IF(SRCTYP.EQ.5) THEN
         CTMOM(3)=1
         CTMOM(2)=ALAME(LN,1)/( ALAME(LN,1)+2.*ALAME(LN,2) )
         CTMOM(1)=CTMOM(2)
        ELSE
         do k=1,3
          ctmom(k)=tenmom(k)
         end do
        ENDIF
        cc3=s2-2.0*alfa(ln)*alfa(ln)
        cc4=s2+2.0*alfa(ln)*alfa(ln)
        cc5=beta(ln)*beta(ln)
cvd$  novector
         DO 40 I=IFSOU(LN),ILSOU(LN)
          zdif=rdc(ircv)-sdc(i)
          is=nint(sign(1.0,zdif))
          iwav1=nint(2.5-is*1.5)
          iwav2=iwav1+1
          iwav3=iwav1+2
          cc1=cphfac(i)*cexpt(-abs(rdc(ircv)-sdc(i))*alfa(ln))
          cc2=cphfac(i)*cexpt(-abs(rdc(ircv)-sdc(i))*beta(ln))
          do k=1,3
c >>> m=1
          m=1
          pot(iwav1,ircv,m)=pot(iwav1,ircv,m) - 
     &                ctmom(k)*.25*(cc3-cc(k)*cc4)*alfinv(ln)*cc1
          pot(iwav2,ircv,m)=pot(iwav2,ircv,m) + 
     &                      ctmom(k)*.25*(1.0+3.0*cc(k))*is*s*cc2      
c >>> m=2
c >>> m=3
          if (k.gt.1) then
           m=3
           pot(iwav1,ircv,m)=pot(iwav1,ircv,m) - 
     &                      ctmom(k)*is*s*cs(k)*cc1
           pot(iwav2,ircv,m)=pot(iwav2,ircv,m) - 
     &              ctmom(k)*0.5*(cc5+s2)*betinv(ln)*cs(k)*cc2
           pot(iwav3,ircv,m)=pot(iwav3,ircv,m) - 
     &                      ctmom(k)*0.5*is*(s2-cc5)*cs(k)*cc2/s
          end if
c >>> m=4
          rr1=1.0-cc(k)
          if (k.eq.1) rr1=-rr1
          m=4
          pot(iwav1,ircv,m)=pot(iwav1,ircv,m) 
     &                      - 0.25*s2*alfinv(ln)*rr1*cc1
          pot(iwav2,ircv,m)=pot(iwav2,ircv,m) - 0.25*is*s*rr1*cc2      
          pot(iwav3,ircv,m)=pot(iwav3,ircv,m) 
     &                      - 0.25*(s2-cc5)*betinv(ln)*rr1*cc2
c >>> m=5
          end do

 40      CONTINUE
       END IF
 50   CONTINUE
      RETURN
      END


      SUBROUTINE SCNMOM()
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     SCNMOM : OASES VERSION
C
C     SEISMIC MOMENT TENSOR SOURCE IN SOLID MEDIUM
C     HS SEP 13, 1994
c     Sep 15, 1994: Changed sign of P-potential in strike slip sources
c                   Now consistent with jkim appendix. 
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX S,CC1,CC2,CC3,CC4,cc5,cc6,cc7
      REAL tmom(3)
      real cc(3)
C
      S=WVNO
C
      CC(1)=-1.0
      CC(2)=-1.0
      CC(3)=1.0
      TMOM(1)=M11
      TMOM(2)=M22
      TMOM(3)=M33
cvd$  novector
cvd$  concur
cvd$  nodepchk
      DO 50 J=1,NUMTR(3)
       IRCV=NRPNT(J,3)
       LN=LAY(IRCV)
       ZZ=Z(IRCV)
       IF (NOSOU(LN).GT.0) THEN
        cc3=s2-2.0*alfa(ln)*alfa(ln)
        cc4=s2+2.0*alfa(ln)*alfa(ln)
        cc5=beta(ln)*beta(ln)
        cc6=ak2(ln,2)/s
        cc7=2e0*s2-ak2(ln,2)
cvd$  novector
         DO 40 I=IFSOU(LN),ILSOU(LN)
          zdif=rdc(ircv)-sdc(i)
          is=nint(sign(1.0,zdif))
          iwav1=nint(2.5-is*1.5)
          iwav2=iwav1+1
          iwav3=iwav1+2
          cc1=cphfac(i)*cexpt(-abs(rdc(ircv)-sdc(i))*alfa(ln))
          cc2=cphfac(i)*cexpt(-abs(rdc(ircv)-sdc(i))*beta(ln))
c >>> Normal moment tensor components
c >>> m=1
         do k=1,3
          m=1
          pot(iwav1,ircv,m)=pot(iwav1,ircv,m) - 
     &                  tmom(k)*.25*(cc3-cc(k)*cc4)*alfinv(ln)*cc1
          pot(iwav2,ircv,m)=pot(iwav2,ircv,m) + 
     &                  tmom(k)*.25*(1.0+3.0*cc(k))*is*s*cc2      
c >>> m=2
c >>> m=3
c >>> m=4
          if (k.lt.3) then
           rr1=1.0-cc(k)
           if (k.eq.1) rr1=-rr1
           m=4
           pot(iwav1,ircv,m)=pot(iwav1,ircv,m) - 
     &                  tmom(k)*0.25*s2*alfinv(ln)*rr1*cc1
           pot(iwav2,ircv,m)=pot(iwav2,ircv,m) - 
     &                  tmom(k)*0.25*is*s*rr1*cc2      
           pot(iwav3,ircv,m)=pot(iwav3,ircv,m) - 
     &                  tmom(k)*0.25*(s2-cc5)*betinv(ln)*rr1*cc2
          end if
c >>> m=5
         end do
c >>> moment components M12=M21 (strike slip, t=pi/2)
c >>> m=1
c >>> m=2
c >>> m=3
c >>> m=4
c >>> m=5
         m=5
         pot(iwav1,ircv,m)=pot(iwav1,ircv,m)  
     &                      - m12*s2*alfinv(ln)*cc1
         pot(iwav2,ircv,m)=pot(iwav2,ircv,m)  
     &                      - is*m12*s*cc2      
         pot(iwav3,ircv,m)=pot(iwav3,ircv,m)  
     &                      - m12*ak2(ln,2)*betinv(ln)*cc2
c >>> moment components M13=M31 (strike slip, t=0)
c >>> m=1
c >>> m=2
         m=2
         pot(iwav1,ircv,m)=pot(iwav1,ircv,m)  
     &                      + is*m13*2.0*s*cc1
         pot(iwav2,ircv,m)=pot(iwav2,ircv,m)  
     &                      + m13*cc7*betinv(ln)*cc2      
         pot(iwav3,ircv,m)=pot(iwav3,ircv,m)  
     &                      + is*m13*cc6*cc2
c >>> m=3
c >>> m=4
c >>> m=5
c >>> moment components M23=M32 (dip slip, t=pi/2)
c >>> m=1
c >>> m=2
c >>> m=3
        m=3
        pot(iwav1,ircv,m)=pot(iwav1,ircv,m)  
     &                      + is*m23*2.0*s*cc1
        pot(iwav2,ircv,m)=pot(iwav2,ircv,m)  
     &                      + m23*cc7*betinv(ln)*cc2      
        pot(iwav3,ircv,m)=pot(iwav3,ircv,m)  
     &                      + is*m23*cc6*cc2
c >>> m=4
c >>> m=5
 40      CONTINUE
       END IF
 50   CONTINUE
      RETURN
      END



CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     SOLVE : FIPM VERSION
C
C     MAY 14, 1988 MODIFIED BY JKIM
C 
C     (1) CBGEMR TAKES THE MULTIPLE R.H.S. OF DIM. MROW 
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      SUBROUTINE SOLVE
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INTEGER NEQMSUF,MROW,NEQMI
      REAL EPS
C
C     INITIALIZE WORK1,WORK2,RHS
C
      CALL CVFILL(CNUL,WORK2,2,NNB)
C
C     ALO TO GLOBAL
C
      CALL CVIMOV(ALO,INDA,1,WORK1,2,NNA)
C
C     STORE THEM IN BAND FORM
C
      CALL CVMOVI(WORK1,2,INDB,1,WORK2,NNA)
C
C     ESTABLISH RHS
C
      NEQMSUF=NEQ*MSUFT
      MROW=MSUFT*ISROW
      NEQMI=NEQ*MSUFT*ISROW
      CALL CVIMOV(R,INDR,1,RHS,2,NEQMI)
      EPS=0.
C
C     SOLVE
C
      CALL CBGEMR(WORK2,RHS,NEQ,NEQ,MROW,IBW,EPS)
      IERR=EPS
      IF (IERR.NE.0) RETURN
      CALL CVMOVI(RHS,2,INDS,1,SS,NEQMI)
c
c *** compute determinant inverse
C
      IF (DETERM) THEN
       DETMNT=1E0
       I1=IBW*NEQ+1
       DO 1000 I=I1,I1+NEQ-1
        DETMNT=DETMNT*ABS(WORK2(I))
 1000  CONTINUE
      END IF        
      RETURN
      END
      SUBROUTINE AXFOR(i,force)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     axfor : OASES VERSION
C
C     r.h.s. contribution from vertical force number i
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX S,CC1,CC2,force
c      if (debug) write(6,*) '>>> Entering SAXFOR <<<'
C
      m=1
      S=WVNO
      LN=LAYS(I)
      cc1=cphfac(i)*force
      cc2=cphfac(i)*force*s*betinv(ln)
      if (ln.gt.1) then
c >>> Upper interface potentials
         PUIN(1,LN-1,m)=PUIN(1,LN-1,m)-
     &           cc1*CEXPT(-ABS(V(LN,1)-SDC(I))*ALFA(LN))
         PUIN(2,LN-1,m)=PUIN(2,LN-1,m)+
     &           cc2*CEXPT(-ABS(V(LN,1)-SDC(I))*BETA(LN))
      end if
c >>> Lower interface potentials
      if (ln.lt.NUML) then
         POIN(1,LN,m)=POIN(1,LN,m)+
     &           cc1*CEXPT(-ABS(V(LN+1,1)-SDC(I))*ALFA(LN))
         POIN(2,LN,m)=POIN(2,LN,m)+
     &           cc2*CEXPT(-ABS(V(LN+1,1)-SDC(I))*BETA(LN))
      end if
      RETURN
      END
      SUBROUTINE AXMOM(i,vermom,hormom)
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
      COMPLEX S,CC1,CC2,CCC1,DDD1,vermom,hormom
c      if (debug) write(6,*) '>>> Entering AXMOM <<<'
C
      m=1
      S=WVNO
      LN=LAYS(I)
      CCC1=alfinv(ln)*(s2*(vermom-hormom)
     &                 -ak2(ln,1)*(0.75*vermom+0.5*hormom))
      DDD1=s*(hormom-vermom)
      cc1=cphfac(i)*CCC1
      cc2=cphfac(i)*DDD1
      if (ln.gt.1) then
c >>> Upper interface potentials
         PUIN(1,LN-1,m)=PUIN(1,LN-1,m)+
     &           cc1*CEXPT(-ABS(V(LN,1)-SDC(I))*ALFA(LN))
         PUIN(2,LN-1,m)=PUIN(2,LN-1,m)+
     &           cc2*CEXPT(-ABS(V(LN,1)-SDC(I))*BETA(LN))
      end if
c >>> Lower interface potentials
      if (ln.lt.NUML) then
         POIN(1,LN,m)=POIN(1,LN,m)+
     &           cc1*CEXPT(-ABS(V(LN+1,1)-SDC(I))*ALFA(LN))
         POIN(2,LN,m)=POIN(2,LN,m)-
     &           cc2*CEXPT(-ABS(V(LN+1,1)-SDC(I))*BETA(LN))
      end if
      RETURN
      END
      SUBROUTINE SAXFOR(i,force)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     Saxfor : OASES VERSION
C
C     Field contribution from vertical force number i
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnrd.f'
      COMPLEX S,CC1,CC2,force
c      if (debug) write(6,*) '>>> Entering SAXFOR <<<'
C
      m=1
      S=WVNO
      LN=LAYS(I)
      cc1=cphfac(i)*force
      cc2=cphfac(i)*force*s*betinv(ln)
      do 50 IRCV=IFRCV(LN),ILRCV(LN)
       ZZ=Z(IRCV)
         ISI=nint(SIGN(1.0,RDC(IRCV)-SDC(I)))
         iwavp=nint(2.5-isi*1.5)
         iwavs=iwavp+1
         POT(iwavp,ircv,m)=POT(iwavp,ircv,m)+
     &       isi*cc1*CEXPT(-ABS(RDC(IRCV)-SDC(I))*ALFA(LN))
         POT(iwavs,ircv,m)=POT(iwavs,ircv,m)+
     &           cc2*CEXPT(-ABS(RDC(IRCV)-SDC(I))*BETA(LN))
 50   CONTINUE
      RETURN
      END
      SUBROUTINE Saxmom(i,vermom,hormom)
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
      COMPLEX CCC1,DDD1
c      if (debug) write(6,*) '>>> Entering SAXMOM <<<'
C
      m=1
      S=WVNO
      ln=lays(i)
      CCC1=alfinv(ln)*(s2*(vermom-hormom)
     &                 -ak2(ln,1)*(0.75*vermom+0.5*hormom))
      DDD1=s*(hormom-vermom)
      cc1=cphfac(i)*CCC1
      cc2=cphfac(i)*DDD1
          do 50 IRCV=IFRCV(LN),ILRCV(LN)
           ZZ=Z(IRCV)
           ISI=nint(SIGN(1.0,RDC(IRCV)-SDC(I)))
           iwavp=nint(2.5-isi*1.5)
           iwavs=iwavp+1
           POT(iwavp,ircv,m)=POT(iwavp,ircv,m)+
     &           CC1*CEXPT(-ABS(RDC(IRCV)-SDC(I))*ALFA(LN))
           POT(iwavs,ircv,m)=POT(iwavs,ircv,m)-
     &       isi*cc2*CEXPT(-ABS(RDC(IRCV)-SDC(I))*BETA(LN))
 50       CONTINUE
c >>> end of routine SAXMOM <<<
      RETURN
      END


