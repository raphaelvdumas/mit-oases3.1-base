C
C     ***COMMON AREA HOLDING DATA RELEVENT TO MESSAGE OUTPUT
C     *** JOHN STEIERT     2-DEC-81
C
C     *** AMMENDED:
C
C          JS     20-FEB-84   INCLUDE INFORMATION FOR MULTIPLE CHARACTER SETS.
C          JS     23-Mar-84   Allow 9 shift chars instead of 6.
C          JS     28-Mar-84   New variables: STCLLN, STILLN, LINSPC
C          JS      2-Apr-84   New variables: XMGOFS, YMGOFS
C
C          MSGHGT        HEIGHT FOR MESSAGES BEING OUTPUT
C          MSGANG        ANGLE AT WHICH MESSAGES TO BE OUTPUT
C          MSGSTP        STEP LENGTH FOR A CHARACTER SEGMENT
C          CHALPH        CURRENT ALPHABET TYPE:    0 = UNDEFINED
C                                                  1 = SIMPLEX
C                                                  2 = COMPLEX
C                                                  3 = DUPLEX
C          LFONT(9)      FLAGS TO SHOW FONTS DEFINED
C          CHSHFT(9)     TABLE OF FONT SHIFT CHARACTERS
C          FONTNO(9)     TABLE OF FONTS USED:      0 = UNDEFINED
C                                                  1 = STANDARD
C                                                  2 = LOWER CASE STANDARD
C                                                  3 = ITALIC
C                                                  4 = LOWER CASE ITALIC
C                                                  5 = SPECIAL
C                                                  6 = MATHEMATIC
C                                                  7 = GREEK
C                                                  8 = LOWER CASE GREEK
C                                                  9 = SUBSCRIPT
C                                                  10= SUPERSCRIPT
C                                                  11= END OF SUBS OR SUPERS
C          STCLLN        Length of STORY lines in characters.
C          STILLN        Length of STORY lines in integers
C          LINSPC        Interline spacing for STORY.
C          XMGOFS        Message offset in X direction.
C          YMGOFS        Message offset in Y direction.
C
      REAL MSGHGT, MSGANG, MSGSTP
      INTEGER CHALPH, FONTNO(9)
      CHARACTER*1 CHSHFT(9)
      LOGICAL LFONT(9)
      INTEGER STCLLN, STILLN
      REAL LINSPC
      REAL XMGOFS, YMGOFS
      COMMON /MSG$DT/ MSGHGT, MSGANG, MSGSTP, CHALPH, LFONT, CHSHFT,
     $                FONTNO, STCLLN, STILLN, LINSPC, XMGOFS, YMGOFS
