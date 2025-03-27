      SUBROUTINE SMOOTH(Z,NX,NY,NSM)                                    
C     GIVEN ARRAY Z(I,J),I=1,NX , J=1,NY   AND NSM=0,1,2,3 ...          
C     LAPLACIAN SMOOTHING IS APPLIED TO Z NSM TIMES BY MEANS OF THE     
C     OPERATION Z=Z + .25*(AV(ZN,ZS,ZE,ZW)-Z) .                         
C     ZXX AND ZYY ARE ASSUMED TO BE ZERO AT EDGES                       
C     UNUSED POINTS IN Z SHOULD BE .GE. 10**35 .                        
C     OCEANOGRAPHY EMR   OCT/69   VERSION NO. 2  IMPROVED EDGING        
C                                                                       
      DIMENSION Z(NX,NY)
      DATA BIG,R/0.9E35,0.06250/
C
  200 FORMAT(1H ,' WARNING: SMOOTHING IS BEING APPLIED ',/,
     & '  NUMBER OF SMOOTHINGS = ',I3)
C
      IF(NSM .LE. 0)   RETURN
      WRITE(6,200) NSM
C
      DO 4000 IT=1,NSM                                                   
      ITODD=IT-(IT/2)*2                                                 
      DO 3000 II=1,NX
      I=ITODD*II+(1-ITODD)*(NX+1-II)
      IINTP=(I-1)*(NX-I)
      DO 2000 JJ=1,NY
      J=ITODD*JJ+(1-ITODD)*(NY+1-JJ)
      ZIJ=Z(I,J)
      IF(ZIJ .LT. BIG)    THEN
       IF(IINTP .GT. 0)   THEN
        DEL2X=Z(I-1,J)+Z(I+1,J)-ZIJ-ZIJ
       ELSE
        IF(I .EQ. 1)   THEN
         DEL2X= 2*(Z(I+1,J) - ZIJ)
        ELSE
         DEL2X= 2*(Z(I-1,J) - ZIJ)
        END IF
       END IF
C
       JINTP=(J-1)*(NY-J)
       IF(JINTP .GT. 0)   THEN
        DEL2Y=Z(I,J-1)+Z(I,J+1)-ZIJ-ZIJ
       ELSE
        IF(J .EQ. 1)   THEN
         DEL2Y= 2*(Z(I,J+1) - ZIJ)
        ELSE
         DEL2Y= 2*(Z(I,J-1) - ZIJ)
        END IF
       END IF
       IF(DEL2X .GE. BIG)   DEL2X=0.0
       IF(DEL2Y .GE. BIG)   DEL2Y=0.0
       Z(I,J)=ZIJ+(DEL2X+DEL2Y)*R
      END IF
 2000 CONTINUE
 3000 CONTINUE
 4000 CONTINUE
      RETURN
      END
