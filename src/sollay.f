      SUBROUTINE SOLLAY     
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
C     SECOND ROW IS HORIZONTAL DISPLACEMENT           
        AUP(LN,2,1)=WVNO         
        AUP(LN,2,2)=-BETA(LN) 
        AUP(LN,2,3)=WVNO         
        AUP(LN,2,4)=BETA(LN)  
C     THIRD ROW IS NORMAL STRESS  
        AUP(LN,3,1)=-CON2(LN)
        AUP(LN,3,2)=CON4(LN)
        AUP(LN,3,3)=-CON2(LN)
        AUP(LN,3,4)=-CON4(LN)
C     LAST ROW IS SHEAR STRESS    
        AUP(LN,4,1)=-CON3(LN)
        AUP(LN,4,2)=CON2(LN)
        AUP(LN,4,3)=CON3(LN)
        AUP(LN,4,4)=CON2(LN)
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
C *** SOURCE TERMS
      if (srctyp.eq.98) then
        do jj=1,numts(3)
         i=nspnt(jj,3)
         call soldis(i,sval(i,1),sval(i,2))
        end do
      else if (srctyp.eq.99) then
       if (nstrf.eq.3.and.isprm(1).eq.10.and.isprm(2).eq.11.and.
     &     isprm(3).eq.12) then
        do 205 jj=1,numts(3)
         i=nspnt(jj,3)
         call axmom(i,sval(i,1)+sval(i,2),sval(i,1))
c          call axexpl(i,sval(i,1))
         aaa=abs(real(sval(i,3))) + abs(imag(sval(i,3)))
         if (aaa.gt.1E-20) call vfor(i,sval(i,3))
 205    continue
       else if (nstrf.eq.2.and.isprm(1).eq.10.and.isprm(2).eq.11) then
        do 206 jj=1,numts(3)
         i=nspnt(jj,3)
 206     call axmom(i,sval(i,1)+sval(i,2),sval(i,1))
       else if (nstrf.eq.2.and.isprm(1).eq.1.and.isprm(2).eq.5) then
        do 207 jj=1,numts(3)
         i=nspnt(jj,3)
 207     call axmom(i,pcorr*(sval(i,1)+sval(i,2)),pcorr*sval(i,2))
       else
        do 210 jj=1,numts(3)
         i=nspnt(jj,3)
        do 210 j=1,nstrf
         if (isprm(j).eq.10) then
          call axmom(i,sval(i,j),sval(i,j))
         else if (isprm(j).eq.11) then
          call axmom(i,sval(i,j),cnul)
         else if (isprm(j).eq.12) then
          call vfor(i,sval(i,j))
         else
          stop ' >>> SOLLAY: Unknown source type in TRF file <<<'
         end if
 210    continue
       end if
      else
cvd$  novector
CDIR$ NOVECTOR
cvd$  noconcur
      DO 40 J=1,NUMTS(3)
       I=NSPNT(J,3)
       IF (srctyp.eq.1) THEN
c        call axexpl(i,cmplx(4*pi,rnul))
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
 40   CONTINUE
      end if
c
c >>> Compute DGM right hand sides for all interfaces
c
cvd$r permutation(LAYT)
cvd$  nodepchk
C$DIR NO_RECURRENCE
CDIR$ IVDEP
      DO 50 j=1,NUMT(3)
       LN=LAYT(j,3)              
       if (nosou(ln).gt.0) then
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
 50   continue
      RETURN                
      END                   
