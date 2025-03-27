c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      DIMENSION SLS(4),NWS(3),SLD(4),NWD(3)
      DIMENSION CONMAX(3)
      DIMENSION SLEVF(NRD),SLEVDB(NRD)
      DIMENSION ZDN(NSMAX),XDN(NSMAX),YDN(NSMAX),DNLEVDB(NSMAX),
     -          DNLEV(NSMAX)
	logical dnmask(nsmax),dndone(nsmax),ldnlin(1000),
     &          dntlev(1000)
      COMMON /NOIPRM/ SNLEVDB,WNLEVDB,DPLEVDB,
     &                SLEVEL ,WNLEV  ,DPLEV  ,
     &                CMINN  ,CMAXN  , SLS ,
     &                CMINP  ,CMAXP  ,SLD , DPSD, CMIND  ,CMAXD  ,
     &                SLOW1D ,SLOW2D ,CONMAX , SLEVF  ,SLEVDB  ,
     &                ZDN    ,XDN    ,YDN    ,DNLEVDB ,DNLEV , 
     &                NDNS,NWVNON , NWS , NWVNOP , NWD , NWVNOD , 
     &                ICUT1D , ICUT2D ,
     &		      dntlev,dnmask,dndone,ldnlin
	
