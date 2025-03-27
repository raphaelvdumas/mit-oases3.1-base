      parameter (nsprm = 10)
      COMPLEX CPHFAC(NRD),CF(NRD)
      COMPLEX POT(NLEQ,Nrd,mmax)
      COMMON /RECDAT/ LAY(NRD),Z(NRD),RDC(NRD),SDC(NRD),CF,
     1                LAYS(NRD),ZUS(NRD),ZLS(NRD),cphfac,POT,
     2                RLIND(NRD)
      COMPLEX CSAIR1(NRD),CSAIR2(NRD),CSAIR3(NRD)
      COMPLEX ZETA(NRD),AIRY(NRD),BIRY(NRD),
     &        AIRYD(NRD),BIRYD(NRD),ZTAM(NRD)
      COMMON /AIRYSC/ CSAIR1,CSAIR2,CSAIR3,
     &                ZETA,AIRY,BIRY,AIRYD,BIRYD,ZTAM
      COMMON /RECLAY/ NUMTR(4),NRPNT(NRD,4)
      COMMON /SOULAY/ NUMTS(4),NSPNT(NRD,4)
      real sstren(nrd),sdelay(nrd)
      integer isprm(nsprm), nstrf
      complex sval(nrd,nsprm)
      common /souext/ sval,sstren,sdelay,isprm, nstrf
      complex eofst(nrd)
      common /tiltda/ ofstar(nrd),eofst


