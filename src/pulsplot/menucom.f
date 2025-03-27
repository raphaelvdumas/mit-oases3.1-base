      parameter (menu_max=24)
      CHARACTER*68 MENTIT
      character*16 item(menu_max)
      character*12 field(menu_max)
      character*36 unit(menu_max)
      CHARACTER*48 FLDTEX(menu_max)
      CHARACTER FLDCHA(menu_max)
      common /mencha/ MENTIT,item,field,unit,fldtex,fldcha
      INTEGER FLDTYP(menu_max)
      common /menval/ FLTNUM(menu_max),nmline,FLDTYP,INTNUM(menu_max),
     &                INTMAX(menu_max),INTMIN(menu_max)
C *** FTYPE= 1: CHAR. STRING, 2: REAL NUMBER, 3: INTEGER NUMBER,
C ***        4: CHARACTER.

