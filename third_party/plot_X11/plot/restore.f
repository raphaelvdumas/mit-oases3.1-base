      SUBROUTINE RESTORE(CURVET)

      CHARACTER*2 FORMT,BDEV
      CHARACTER*3 DEV, SYMB(20), ADD, PLOP(21), CURVET, URC, ULC, NWR
      CHARACTER*11 FACTORTMP

      COMMON /SPOPT/ ISEG,ISNUM,ITRUNC

      include 'default.f'

      COMMON /SCALES/ YSCALETMP, DRRATIO, YOLDLEN,
     &  SCFAC, HSCFAC, IHSC, FACTOR, FACT, FACSCALE, SCALEFACT

      COMMON /SETPRM/ PLOP, ADD, BDEV, FACTORTMP,
     %  SYMB, DEV, FORMT, URC, ULC, NWR

C   
C
C
      KTYP=1
      IXAX=0
      IYAX=0
      ICHTYP=1
      ITFRAM=0
      IXCH=0
      IMARK=0
      ISYMB=0
      IDASH=0
      NOP=0
      KFORM=2
      NEGPOS=0
      NSPP=1
      KFILL=0
      NDEC=1
      IUNI=0
      ICON=0
      IHCPY=1
      ISEG=0
      ITAX=0
      ITRUNC=0
      DEV='PRX'
      FORMT='A3'
      SCFAC=1.0
      HSCFAC=1.0
      IHSC=0
      ICOLF=0
      ICOL=1
      CURVET='CNT'
      ADD='   '
      URC='   '
      ULC='   '
      NWR='   '
      fill=.false.
      ipos=1
      ineg=1

      RETURN
      END
