      SUBROUTINE DOUBLE(Z,NX,NY)                                        
C     THE ARRAY Z(I,J) I=1,NX J=1,NY  IS EXPANDED TO TWICE ITS SIZE ,   
C     THAT IS, Z(I,J) I=1,2*NX-1,2  J=1,2*NY-1,2.  THE SPACES ARE FILLED
C     USING CUBIC POLYNOMIAL INTERPOLATION.  UNDEFINED POINTS OF Z      
C     SHOULD BE SET TO 1.E35 .                                          
C     OCEANOGRAPHY EMR   DECEMBER 1969.                                 
C                                                                       
C                                                                       
      DIMENSION Z(57,57)
      DIMENSION  ZZ(4)                                                  
C                                                                       
      NXD = 2*NX-1                                                      
      NYD = 2*NY-1                                                      
      NXDM2 =NXD-2                                                      
      NYDM2 =NYD-2                                                      
      BIG =.9E35                                                        
      BIG2 = BIG*2.                                                     
C     SPREAD EXISTING POINTS                                            
C                                                                       
C***********************************************************************
C                                                                       
      DO 20 II = 1,NX                                                   
      I= 1+NX-II                                                        
      ID= I*2-1                                                         
      DO 20 JJ = 1,NY                                                   
      J= 1+NY-JJ                                                        
      JD = J*2-1                                                        
20    Z(ID,JD)=Z(I,J)                                                   
C                                                                       
C     FILL IN HALF-WAY POINTS OF X-LINES.                               
C***********************************************************************
C                                                                       
      DO 90 I=1,NXDM2,2                                                 
      DO 90 J=1,NYD,2                                                   
      Z(I+1,J) = 1.E35                                                  
      IF(Z(I,J)+Z(I+2,J)-BIG)30,90,90                                   
30    ZZ(2)=Z(I,J)                                                      
      ZZ(3)=Z(I+2,J)                                                    
      ZZ(1)=1.E35                                                       
      IF(I-1)50,50,40                                                   
40    ZZ(1)= Z(I-2,J)                                                   
50    ZZ(4)=1.E35                                                       
      IF(I-NXDM2)60,70,70                                               
60    ZZ(4)=Z(I+4,J)                                                    
70    IRETRN = 80                                                       
      GO TO 400                                                         
80    Z(I+1,J) = ZZHALF                                                 
90    CONTINUE                                                          
C                                                                       
C     FILL IN HALF-WAY POINTS OF Y-LINES                                
C***********************************************************************
C                                                                       
      DO 190 J = 1,NYDM2,2                                              
      DO 190 I = 1,NXD,2                                                
      Z(I,J+1)=1.E35                                                    
      IF(Z(I,J)+Z(I,J+2)-BIG)130,190,190                                
130   ZZ(2)= Z(I,J)                                                     
      ZZ(3)= Z(I,J+2)                                                   
      ZZ(1)=1.E35                                                       
      IF(J-1)150,150,140                                                
140   ZZ(1)= Z(I,J-2)                                                   
150   ZZ(4)= 1.E35                                                      
      IF(J-NYDM2)160,170,170                                            
160   ZZ(4) = Z(I,J+4)                                                  
170   IRETRN = 180                                                      
      GO TO 400                                                         
180   Z(I,J+1)= ZZHALF                                                  
190   CONTINUE                                                          
C                                                                       
C     FILL IN CENTER-POINT OF EACH SQUARE                               
C***********************************************************************
C                                                                       
      DO 300 I = 1,NXDM2,2                                              
      DO 300 J = 1,NYDM2,2                                              
      Z(I+1,J+1) = 1.E35                                                
      ZSUM = Z(I,J)+Z(I+2,J)+Z(I  ,J+2)+Z(I+2,J+2)                      
      IF(ZSUM-BIG)245,210,210                                           
210   IF(ZSUM-BIG2)220,300,300                                          
220   IF(Z(I,J)+Z(I+2,J+2)-BIG) 230,240,240                             
230   Z(I+1,J+1)  = (Z(I,J)+Z(I+2,J+2))*.5                              
      GO TO 300                                                         
240   Z(I+1,J+1)  = (Z(I+2,J)+Z(I,J+2))*.5                              
      GO TO 300                                                         
245   ZZ(2)=  Z(I,J+1)                                                  
      ZZ(3)=  Z(I+2,J+1)                                                
      ZZ(1)=1.E35                                                       
      IF(I-1) 260,260,250                                               
250   ZZ(1)= Z(I-2,J+1)                                                 
260   ZZ(4)=1.E35                                                       
      IF(I-NXDM2)270,280,280                                            
270   ZZ(4) =  Z(I+4,J+1)                                               
280   IRETRN=290                                                        
      GO TO 400                                                         
290   Z(I+1,J+1) = ZZHALF                                               
300   CONTINUE                                                          
      RETURN                                                            
C                                                                       
C     INTERPOLATION TO GET ZZ(2.5) .                                    
C***********************************************************************
C                                                                       
400   IF(ZZ(1)+ZZ(4)-BIG)420,430,430                                    
420   ZZHALF = .5625*(ZZ(2)+ZZ(3)) -.0625*(ZZ(1)+ZZ(4))                 
      GO TO 480                                                         
430   IF(ZZ(1)-BIG)440,450,450                                          
440   ZZHALF = .375*ZZ(3) +.75*ZZ(2) -.125*ZZ(1)                        
      GO TO 480                                                         
450   IF(ZZ(4)-BIG) 460,470,470                                         
460   ZZHALF=  .375*ZZ(2)+.75*ZZ(3)-.125*ZZ(4)                          
      GO TO 480                                                         
470   ZZHALF = .5*(ZZ(2)+ZZ(3))                                         
480   IF(IRETRN-180)80,180,290                                          
      END                                                               
