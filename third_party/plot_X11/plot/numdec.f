      SUBROUTINE NUMDEC(A1,A2,A3,DIV,NDEC)
      DATA NDECMX/9/

      NDEC=-1

      IF(   ( MOD(A1*DIV,1.0).NE. 0.0 ) .OR.
     &      ( MOD(A2*DIV,1.0).NE. 0.0 ) .OR.
     &      ( MOD(A3*DIV,1.0).NE. 0.0 )    )   THEN


       VMAX= DIV * MAX( ABS(A1), ABS(A2) )
       VMIN= VMAX - ABS(A3*DIV)
       DO 2000   I=1,NDECMX
       R1=ANINT(VMIN*10.0**(NDEC+1))
       R2=ANINT(VMAX*10.0**(NDEC+1))
       IF(R1 .NE. R2)   GO TO 2000
       NDEC=NDEC+1
 1000  CONTINUE
 2000  CONTINUE
       IF(NDEC.GT.-1)   NDEC=NDEC+1

      END IF

      RETURN
      END
