C NAME QT4695
C PACK DRIVER
C COMP VAX/VMS
C VERS 861002
C HIST 5V1,5V2,5V4
C---------------------------------------------------------------------C 
C                                                                     C 
C                    U N I R A S   S O F T W A R E                    C 
C                                                                     C 
C     THE   CONTENTS   OF   THIS   DOCUMENT  ARE  PROPRIETARY  TO     C 
C                                                                     C 
C                         U N I R A S  A/S                            C 
C                                                                     C 
C     AND  ARE  NOT  TO  BE  DISCLOSED  TO  OTHERS  OR  USED  FOR     C 
C     PURPOSES  OTHER  THAN  DESCRIBED  IN  AGREEMENT  OR WRITTEN     C 
C     APPROVAL  OF  THE  OWNERS.                                      C 
C                                                                     C 
C---------------------------------------------------------------------C 
C
      SUBROUTINE QT4695
C
C FUNCTION:
C
C   DRIVER FOR TEKTRONIX 4695 INK JET HARDCOPY RUNNING TROUGH
C   TEKTRONIX 4105/4107/4109
C
C---------------------------------------------------------------C
C   DUMP PAGES IN THE MEMORY
C---------------------------------------------------------------C
C
      CALL GSYSDP
C
C---------------------------------------------------------------C
C    CALL S-DRIVER
C---------------------------------------------------------------C
      CALL ST4695(*1)
C
      RETURN
C
C *** ERROR EXIT
1     CALL RSYSPA('C-QT4695-USABORT')
      RETURN
      END
