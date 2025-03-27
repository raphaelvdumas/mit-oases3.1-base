      SUBROUTINE SCNDIS(i,hordis,verdis)
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

       do IRCV=IFRCV(LN),ILRCV(LN)
        if (rdc(ircv).eq.sdc(i)) then
         is=1
         pot(iwp,ircv,is)=pot(iwp,ircv,is)+0.5*(cc1)
         pot(iws_b,ircv,is)=pot(iws_b,ircv,is)+0.5*(cc2)
         pot(iwp+2,ircv,is)=pot(iwp+2,ircv,is)+0.5*(cc1)
         pot(iws_a,ircv,is)=pot(iws_a,ircv,is)+0.5*(-cc2)
         is=2
         pot(iwp,ircv,is)=pot(iwp,ircv,is)+0.5*(cc3)
         pot(iws_b,ircv,is)=pot(iws_b,ircv,is)+0.5*(cc4)
         pot(iwp+2,ircv,is)=pot(iwp+2,ircv,is)+0.5*(-cc3)
         pot(iws_a,ircv,is)=pot(iws_a,ircv,is)+0.5*(cc4)
        else
         isi=nint(SIGN(1.0,RDC(IRCV)-SDC(I)))
         iwavp=iwp+1-isi
         if (laytyp(ln).eq.5) then
          iwavs=nint(5.5-0.5*isi)
         iwavs=3-isi
         covp=CEXPT(-ABS(RDC(IRCV)-SDC(I))*ALF)
         covs=CEXPT(-ABS(RDC(IRCV)-SDC(I))*BET)
         is=1
         POT(iwavp,ircv,is)=POT(iwavp,ircv,is)+ cc1*covp
         POT(iwavs,ircv,is)=POT(iwavs,ircv,is)+ isi*cc2*covs
         is=2
         POT(iwavp,ircv,is)=POT(iwavp,ircv,is)+isi*cc3*covp
         POT(iwavs,ircv,is)=POT(iwavs,ircv,is)+cc4*covs
        end if
       end do

      RETURN
      END
