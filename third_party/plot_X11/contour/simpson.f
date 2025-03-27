      SUBROUTINE SIMPSON(COEFF,DNORM,I2)
      DIMENSION COEFF(1)
C
      IF(I2.EQ.2)   THEN
       DNORM=2.0
       COEFF(1)=1.0
       COEFF(2)=1.0
      ELSE
       DNORM=3.
       I=1
       COEFF(I)=0.
       IF( MOD(I2,2).EQ.1)   THEN
C   SIMPSON'S RULE FOR THE FIRST 3 POINTS
        I=3
        COEFF(1)=1.0
        COEFF(2)=4.0
        COEFF(3)=1.0
        ELSE
C   SIMPSON'S SECOND RULE FOR THE FIRST 4 POINTS
        I=4
        COEFF(1)=3.0/8.0 * 3.0
        COEFF(2)=9.0/8.0 * 3.0
        COEFF(3)=9.0/8.0 * 3.0
        COEFF(4)=3.0/8.0 * 3.0
       END IF
      END IF
      IF(I2.GT.4)   THEN
C   COMPOSITE FORMULA BASED ON SIMPSON'S RULE (1/3)*(1,4,2,...4,1)  
C   FOR THE REMAINING SUBINTERVALS
      COEFF(I)=COEFF(I) + 1.0
      DO 1000   J=I+1,I2-1,2
      COEFF(J)=4.0
      COEFF(J+1)=2.0
 1000 CONTINUE
      COEFF(I2)=1.0
      END IF
C
      RETURN
      END
