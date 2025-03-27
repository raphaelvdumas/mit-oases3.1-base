      SUBROUTINE AXES( TXTSTR, IDIR, VSTEP, APOS, HEIGHT, AXWID)

      CHARACTER*80 TXTSTR(*)

      DATA IPLOT,ICOMP,ISUPF/1,6,0/
      DATA T1,T2/999.999,999.999/
      DATA LENGTH,XPOS,YPOS/-2,999.999,999.999/
      DATA MODE/1/

      HEIGHB=HEIGHT



C
C *********************************************************************
C
C    SUBROUTINE RAXAXL:
C
C ************************ THIS SUBROUTINE IS USED TO SET SCALE LAYOUT
C                          AND AXLE ATTRIBUTES
C
C     CALL RAXAXL (IAXIS, IACOL, ISTY, AXWID)
C
C     IAXIS   I   IN   Select scale type
C
C                    1 Linear scale
C                    2 Logarithmic scale
C
C     IACOL   I   IN   Axle color index
C     ISTY    I   IN   Axle linestyle
C     AXWID   R   IN   Axle width (millimeters)
C
C     Function
C
C     Sets the various options that determine the  plotted  axle  type  and
C     scale layout.
C
C     ISTY selects a linestyle for plotting the  axle.   See  GDASH  for  a
C     complete   description   and   illustration  of  the  various  styles
C     available.
C
C     This routine can accept "undefined" values.  (See RAXIS.)
C



C
C *********************************************************************
C
C    SUBROUTINE RAXBTI:
C ********************  THIS SUBROUTINE SPECIFIES THE METHOD OF CALCULA-
C                       TION USED WHEN DECIDING HOW, OR IN WHAT STYLE,
C                       TO PLOT TICKMARKS AND AXIS VALUES ON THE SCALE.
C
C    ITICK  : SELECT TICKMARK LAYOUT STYLE
C             (SEE AGL/AXES USER GUIDE PAGES 26 THROUGH 34)
C              
C    T1     : TICKMARK FIRST REFERENCE AXIS VALUE
C
C    T2     : TICKMARK SECOND REFERENCE AXIS VALUE              
C
C    VSTEP  : TICKMARK STEP VALUE
C
C *******************************************************************
C
C    SUBROUTINE RAXTEX:
C    ******************   THIS ROUTINE SPECIFIES THE AXIS-UNIT OR
C                         AXIS-TEXT, TOGETHER WITH ITS POSITION AND 
C                         SIZE.
C
C    ICOMP  :  SELECT EITHER AXIS-UNIT OR AXIS-TEXT
C               = 5 AXIS-UNIT
C               = 6 AXIS-TEXT
C
C    LENGTH :  LENGTH OF CHARACTER STRING
C               = -1 THEN THE IMPLICIT LENGTH OF TXTSTR IS USED
C               = -2 THEN THE TRAILING BLANKS IN TXTSTR WILL ALSO
C                    BE REMOVED
C               = -3 IMPLIES THAT THE FIRST CHARACTER IN TXTSTR
C                    IS TO BE USED AS THE DELIMITER
C               = -9 UNLESS AN ESCAPE CHARACTER TERMINATOR (SEE
C                    RTXESC) IS DETECTED EARLIER, A LENGTH OF
C                    256 WILL BE USED FOR THE TRANSLATION
C
C    TXTSTR :  TEXT FOR AXIS TEXT
C
C    XPOS   :  TEXT JUSTIFICATION POINT X-COORDINATE (USER COORDINATES)
C
C    YPOS   :  TEXT JUSTIFICATION POINT Y-COORDINATE (USER COORDINATES)
C
C    HEIGHT :  TEXT CHARACTER HEIGHT (MILLIMETERS)
C
C ********************************************************************* 
C
C SUBROUTINE RAXDIS :
C *****************   THIS SUBROUTINE SELECTS WHICH AXIS COMPONENTS
C                     ARE TO BE DISPLAYED OR SUPPRESSED.
C
C    ICOMP  :  SELECT COMPONENTS TO BE DISPLAYED
C               = 1 AXLE
C               = 2 TICKMARKS
C               = 3 MINOR TICKMARKS
C               = 4 AXIS-LABELS
C               = 5 AXIS-UNIT
C               = 6 AXIS-TEXT
C               = 7 TICKLINES
C               = 8 MINOR TICKLINES
C
C    IPLOT  :  SELECT PLOT STATUS
C               = 0 NOT PLOTTED
C               = 1 PLOTTED
C
C    ISUPF  :  SUPPRESSION FACTOR
C               = 0 NO SUPPRESSION
C               = 1 SUPPRESS FIRST TICKMARK, LABEL, OR TICKLINE
C               = 2 SUPPRESS LAST TICKMARK, LABEL, OR TICKLINE
C               = 3 SUPPRESS FIRST AND LAST TICKMARKS, LABELS, OR
C                   TICKLINE
C*********************************************************************
C
C SUBROUTINE RAXIS :
C ****************   THIS SUBROUTINE PLOTS A NUMERIC AXIS OF THE USER
C                    COORDINATE SYSTEM, WITH EITHER A CONTINUOS LINEAR 
C                    OR LOGARITHMIC SCALE.
C
C    IDIR    : PLOT AXIS
C               = 1 PLOT X-AXIS
C               = 2 PLOT Y-AXIS
C
C    APOS    : START VALUE FOR X- OR Y-AXES
C              THE ARGUMENT APOS DETERMINES WHERE THE AXIS WILL START.
C              NOTE THAT THIS IS THE Y-AXIS VALUE WHEN THE X-AXIS IS
C              PLOTTED, AND THE X-AXIS VALUE WHEN THE Y-AXIS IS PLOTTED.
C
C    HEIGHB  : HEIGHT OF AXIS-LABELS (MILLIMETERS)
C
C    MODE    : SELECT NORMAL OR REVERSED LABELLING MODE
C               = 1 NORMAL LABELLING MODE DISPLAYS TICKMARKS, AXIS-LABELS
C                   AXIS-UNIT, AND AXIS-TEXT BELOW THE X-AXIS LINE, AND TO
C                   THE LEFT OF Y-AXIS LINE.
C               = 2 REVERSE LABELLING MODE IS THE OPPOSITE, TICKMARKS, 
C                   AXIS-LABELS, ETC ARE PLOTTED ABOVE THE X-AXIS LINE
C
C ***********************************************************************
C        
C


C
C
C  SET AXLE ATTRIBUTES
C
      CALL RAXAXL( 1, 1, 0, AXWID )

C
C
C SET TICKMARK FEATURES FOR AXIS
C
      ITICK=9999
      CALL RAXBTI(ITICK,T1,T2,VSTEP)
C
C
C SET AXIS TEXT FEATURES
C
         CALL RAXTEX(ICOMP,LENGTH,TXTSTR,XPOS,YPOS,HEIGHT*1.333)
C
C DISPLAY AXIS COMPONENTS
         CALL RAXDIS(ICOMP,IPLOT,ISUPF)
C
C PLOT A USER-LABELLED AXIS COMPONENTS
         CALL RAXIS(IDIR,APOS,HEIGHB,MODE)
       
C RESET TO DEFAULT TICKMARKS FEATURES
C
       ITICK=0
       VSTEP=999.999
       CALL RAXBTI(ITICK,T1,T2,VSTEP)
C
C
       RETURN
       END
