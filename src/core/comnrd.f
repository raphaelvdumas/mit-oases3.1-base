c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      parameter (nsprm = 10)
      COMPLEX CPHFAC(NRD),CF(NRD)
      COMPLEX POT(NLEQ,Nrd,isrowmax)
      COMMON /RECDAT/ cphfac,CF,POT,Z(NRD),RDC(NRD),SDC(NRD),
     1                ZUS(NRD),ZLS(NRD),RLIND(NRD),
     2                LAY(NRD),LAYS(NRD)
      COMPLEX CSAIR1(NRD),CSAIR2(NRD),CSAIR3(NRD)
      COMPLEX ZETA(NRD),AIRY(NRD),BIRY(NRD),
     &        AIRYD(NRD),BIRYD(NRD),ZTAM(NRD)
      COMMON /AIRYSC/ CSAIR1,CSAIR2,CSAIR3,
     &                ZETA,AIRY,BIRY,AIRYD,BIRYD,ZTAM

      COMMON /RECLAY/ NUMTR(NLTYP),NRPNT(NRD,NLTYP)
      COMMON /SOULAY/ NUMTS(NLTYP),NSPNT(NRD,NLTYP),
     &                ivssym(isrowmax),ivspnt(nrd),nvsl(nrd)
      real sstren(nrd),sdelay(nrd)
      integer isprm(nsprm), nstrf
      complex sval(nrd,nsprm)
      common /souext/ sval,sstren,sdelay,isprm, nstrf
      complex eofst(nrd)
      common /tiltda/ ofstar(nrd),eofst
