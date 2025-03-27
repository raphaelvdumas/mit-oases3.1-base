C
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
C     COMMON BLOCKS OF ARRAYS THE SIZE OF WHICH DEPEND ON
C     THE MAXIMUM NUMBER OF LAYERS NLA.
C
C     LOCAL COEFFICIENT MATRICES AND RIGHT HAND SIDES
C
      COMPLEX ALO(NLA,nleq,nleq),AUP(NLA,nleq,nleq),
     &        R(NLA,nleq,isrowmax),SS(NLA,nleq,isrowmax)
      COMMON /FIELD/ ALO,AUP,R,SS
      COMPLEX ROIN(NLA,nleq,isrowmax),RUIN(NLA,nleq,isrowmax),
     &        poin(nleqh,nla,isrowmax),puin(nleqh,nla,isrowmax)
      COMMON /LRHS/ ROIN,RUIN,poin,puin
C
C     ENVIRONMENTAL PARAMETERS AND WAVENUMBERS
C
Cms  array GAMMA added to /LAYDAT/ to accommodate third wavenumber
Cms  for porous sediment layers.
      COMPLEX AK(NLA,2),AK2(NLA,2),ALAME(NLA,2)
      COMPLEX ALFA(NLA),BETA(NLA),gamma(nla)
      complex alfinv(nla),betinv(nla),gaminv(nla)
      REAL V(NLA,12),THICK(NLA)
      COMMON /LAYDAT/ AK,AK2,ALAME,ALFA,BETA,gamma,alfinv,betinv,
     &                gaminv,v,THICK
      COMMON /LAYTYP/ LAYTYP(NLA),NUMT(NLTYP),LAYT(NLA,NLTYP)
C
C     FUNCTIONS OF WAVENUMBERS
C
Cms  array EZGAMM and DELA, DELB, CON5 to CON11 added to /VARS5/ to
Cms  accommodate third wavenumber for porous sediment layers.
c
      COMPLEX EZALFA(NLA),EZBETA(NLA),EZALFM(NLA),EZBETM(NLA),
     &        ezgamm(nla)
      COMPLEX CON1(NLA),CON2(NLA),CON3(NLA),CON4(NLA),CON5(NLA),
     &        CON6(NLA),CON7(NLA),CON8(NLA),CON9(NLA),
     &        CON10(NLA),CON11(NLA),DELA(NLA),DELB(NLA)

      REAL RCON1(NLA)
      COMMON /VARS5/ CON1,CON2,CON3,CON4,CON5,CON6,CON7,CON8,CON9,
     &               CON10,CON11,DELA,DELB,EZALFA,EZBETA,
     &         EZALFM,EZBETM,ezgamm,RCON1,LSOLF,LSOLL,NSOL
C
C     MAPPING POINTERS AND GLOBAL SYSTEM SIZE PARAMETERS
C
      COMPLEX RHS(NLA3*isrowmax)    
      COMMON /EQVAR/ RHS,EPS,NEQ,IPS(NLA,NLEQ),ICP(NLA2),IRN(NLA1),
     &               IDP(NLA3),NNA,ISTART(NLA),IRST(NLA),ICST(NLA),
     &               NCL(NLA),NRI(NLA),IBW,IRHCOL,NNB
c
c     POINTER ARRAYS SHOULD BE REAL IF APAL64 VERSION OF CVIMOV
C     AND CVMOVI ARE USED ON THE FPS164
C
C     REAL INDA(NLA1),INDR(NLA3),INDS(NLA3),INDB(NLA1)
      INTEGER INDA(NLA1),INDR(NLA3),INDS(NLA3),INDB(NLA1)
      COMMON /COM_INDEX/ INDA,INDR,INDS,INDB
C
C     SOURCE AND RECEIVER PARAMETERS
C
      COMMON /LAYSOU/ NOSOU(NLA),IFSOU(NLA),ILSOU(NLA)
      COMMON /LAYRCV/ NORCV(NLA),IFRCV(NLA),ILRCV(NLA)
C
C     ROUGH SURFACE SCATTERING PARAMETERS
C
      real ROUGH(NLA),ROUGH2(NLA),clen(nla),amod(nla)
      COMMON /NKIRCH/ rough2,DLWNK(NLA),IMX(NLA)
      equivalence (rough(1),v(1,7)),(clen(1),v(1,8)),(amod(1),v(1,9))
C
C     TRANSVERSE ISOTROPY
C
      COMPLEX ASP(3,NLA),BSP(3,NLA)
      COMMON /TISO/ ASP,BSP,ARO(3,NLA),
     &              AH(3,NLA),NTISOL,NSL(NLA),LAYNTI(NLA) 
      COMPLEX ANSTD(NTIPAR,NLA)
      COMMON /ANSTD/ ANSTD
Cms
Cms  porous sediment (bubbly sediment not implemented as of 3/1/96)
Cms
      LOGICAL BUBBLES(NLA),fs_switch(nla)
      COMPLEX CM(NLA),BCC(NLA),CH(NLA),CD(NLA),CMU(NLA),CGAMMA(NLA),
     &        CK1(NLA),CK2(NLA),CKS(NLA),bfast(nla),bshear(nla)
      COMMON /BIOT/ CM,BCC,CH,CD,CMU,CGAMMA,CK1,CK2,CKS,bfast,bshear,
     &              BPROP(13,NLA),RHOF(NLA),RHO(NLA),GASPROP(9,NLA),
     &              NBL,LAYNB(NLA),BUBBLES,fs_switch
C
C     Airy function solution
C
      COMPLEX ACO(NLA),BCO(NLA),CCO(NLA),AISC(NLA),BISC(NLA)
      COMPLEX ZETAU(NLA),AIRYU(NLA),BIRYU(NLA),
     &        AIRYDU(NLA),BIRYDU(NLA),ZTAMU(NLA)
      COMPLEX ZETAL(NLA),AIRYL(NLA),BIRYL(NLA),
     &        AIRYDL(NLA),BIRYDL(NLA),ZTAML(NLA)
      equivalence (aco(1),con2(1)),
     &            (bco(1),con3(1)),
     &            (cco(1),con4(1)),
     &            (aisc(1),con5(1)),
     &            (bisc(1),con6(1)),
     &            (zetau(1),con7(1)),
     &            (ztamu(1),con8(1)),
     &            (zetal(1),con9(1)),
     &            (ztaml(1),con10(1)),
     &            (airyu(1),ezalfa(1)),
     &            (biryu(1),ezbeta(1)),
     &            (airyl(1),ezalfm(1)),
     &            (biryl(1),ezbetm(1)),
     &            (airydu(1),con11(1)),
     &            (birydu(1),ezgamm(1)),
     &            (airydl(1),dela(1)),
     &            (birydl(1),delb(1))
c      COMMON /AIRYCO/ ACO,BCO,CCO,AISC,BISC,
c     &        ZETAU,AIRYU,BIRYU,
c     &        AIRYDU,BIRYDU,ZTAMU,
c     &        ZETAL,AIRYL,BIRYL,
c     &        AIRYDL,BIRYDL,ZTAML

c
c     EOF layers. Flagged with cp = -4
c
      real dep_h(nla),dep_t(nla),dep_d(nla)
      integer first_sublay(nla),last_sublay(nla)
      integer eof_type(nla)
      logical eof_layer(nla)
      common /eof_layers/ del_h,del_t,del_d,eof_type,
     &                    first_sublay,last_sublay,eof_layer
c
c Flow of fluid layers
c
      real flow_vel(nla),flow_fac(nla)
      logical flow_flag(nla)
      common /flow/ flow_fac,flow_flag
      equivalence (flow_vel(1),v(1,9))
C
C     WORKING ARRAYS
C
      COMPLEX WORK1(IBSI),WORK2(IBSI)
      COMMON /WORK/ WORK1,WORK2
c >>> Arrays for RC tables
      real tthet(1000),rrefl(1000),pphi(1000)
      integer numref
      COMMON /REFLCC/ TTHET,RREFL,PPHI,NUMREF
c
c     Dispersive layers
c
      parameter (mndlt=10, mndfr=500)
      real cpdl(mndlt,mndfr),csdl(mndlt,mndfr),
     &     apdl(mndlt,mndfr),asdl(mndlt,mndfr)
      integer iddis(nla),idltyp(nla)
      logical disper(nla)
      common /dislpa/ cpdl,csdl,apdl,asdl,iddis,nactf,idltyp,
     &                idlmax,disper

