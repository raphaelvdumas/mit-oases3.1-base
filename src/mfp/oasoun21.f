
      SUBROUTINE PAREST (NZ,NX,NY,IFR,WFREQ,SLEVEL,
     -                   AMPMX,CRARAO,GAMPLT,GETFLG,
     -                   PUTFLG)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c

C  Parameter Estimation Routine
C     modifications by :
C      - Bruce H Pasewark   September 30, 1986
C        Subroutines GETXSM and PUTXSM added
c
c     HS  27-Aug-87  Multible constraint MLM added as MAXLIK=3
c
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
      INCLUDE 'srccoo.f'
      INCLUDE 'recarr.f'
      INCLUDE 'corrfn.f'
      INCLUDE 'corrfs.f'      
      INCLUDE 'pesbuf.f'
	
      LOGICAL CRARAO,GAMPLT
      LOGICAL GETFLG, PUTFLG, BLURR
      COMPLEX CC,CNORM
      COMPLEX CVV(3,3),CLAMB(3),CWORK(6)
      COMPLEX DIRIND

      integer nretry(4)

      REAL WXYZ(3,3)
      REAL ALAMB(3),CRA(100,4),AMP(NP),JXYZ(3,3),VV(3,3),EWORK(3)
      REAL FFS(2,NP)
      REAL ALAM(NPMAX)
      CHARACTER*10 TYP(3)
      CHARACTER*6 CRTYP(4)

      COMMON /SCRFIL/ INPFIL,IOUFIL
      COMMON /BEAMFM/ MAXLIK,NBEAMF
      COMMON /MCMLM/ MAXIN,MCMDIR,BLURR
      COMMON /reppnt/ indexo,numrep,npnt

      DIMENSION GAM(NP)

      EQUIVALENCE (AMP(1),CFFS(1)),(GAM(1),CFFS(NPHP1))
      EQUIVALENCE (LF,NUMFR),(NREC,ISPACE)
      EQUIVALENCE (CFFS(1),FFS(1,1))

      DATA TYP    /'PRESSURE  ','VERT.PART.','HOR.PART. '/
      DATA CRTYP  /'HYDROP','X-GEOP','Y-GEOP','Z-GEOP'/

 300  FORMAT(2(1X,G13.6,1X,G13.6,'i'))

      do 1 intry=1,4
  1   nretry(intry)=0

C     CHECK BUFFER SIZES

      IF (NRCV.GT.NRMAX) STOP '*** PAREST: TOO MANY RECEIVERS ***'

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     READ IN CORRELLATION MATRICES 
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      CALL RDBUF(32,CORRNS,2*NRCV*NRCV)

      IF (GETFLG)  THEN       
C      
C        *****  READ XSM FROM EXTERNAL FILE          
         CALL GETXSM (CORRFD,NRCV,IFR,IERR)          
C
C      STOCHASTIC BLURRING OF SIGNAL COVARIANCE
C
         IF (BLURR) THEN
           CALL SIGBLR
         END IF
c         call vsmul(CORRFD,1,SLEVEL,CORRFD,1,2*NRCV*NRCV)
C      
         IF (MAXLIK.EQ.0)         THEN               
C      
            IF (PUTFLG)  THEN 
C      
C              *****  CREATE SIMULATED XSM           
               CALL RDBUF(33,CFILE,2*NRCV)           
               CALL CVCONJ(CFILE,2,ARG,2,NRCV)       
               CALL CMMUL(CFILE,NRCV,1,ARG,NRCV,CORRFD)                     
C      
C              *****  MULTIPLY BY SOURCE LEVEL       
               CALL VSMUL(CORRFD,1,SLEVEL,CORRFD,1,2*NRCV*NRCV)             
C      
C              *****  ADD NOISE FOR LINEAR BEAMFORMER 
               CALL VADD(CORRFD,1,CORRNS,1,CORRFD,1,2*NRCV*NRCV)            
C      
C              *****  WRITE XSM TO EXTERNAL FILE     
               CALL PUTXSM (CORRFD,NRCV,IFR,IERR)    
C      
            END IF             
C      
         ELSE IF (MAXLIK.EQ.1.OR.MAXLIK.EQ.3)    THEN 
C      
C           ***** Invert XSM for MAXIMUM LIKELIHOOD BEAMFORMER              
C           CALL CMATIN (NRCV,CORRFD,CORRFD)         

 1234       CALL CMINV(NRCV,CORRFD,CNINV,IERR)      
C *** CHECK CONDITION NUMBER
           if (IERR.EQ.0) THEN
            CALL INFNRM(CORRFD,NRCV,ANRM)
            CALL INFNRM(CNINV,NRCV,BNRM)
           END IF
            WRITE(6,*) 'K= ',ANRM*BNRM
           IF (IERR.NE.0.OR.(ANRM*BNRM).GT.1E5) THEN
            WRITE(6,*) '****       COVARIANCE MATRIX IS SINGULAR'
            WRITE(6,*) '**** NOTE: THIS VERSION ADDS -20dB WHITE NOISE'
            WRITE(6,*) '****       AND TRIES AGAIN'
            diasum=0
            do 1235 idia=1,nrcv
              diasum=diasum+corrfd(idia+(idia-1)*nrcv)
 1235       continue
              wnadj=diasum/(nrcv*100E0)
            DO 1236 IDIA=1,NRCV
            CORRFD(IDIA+(IDIA-1)*NRCV)=CORRFD(IDIA+(IDIA-1)*NRCV)+WNADJ
 1236       CONTINUE
            GO TO 1234
           END IF
           CALL VMOV(CNINV,1,CORRFD,1,2*NRCV*NRCV)

C      
         ELSE IF (MAXLIK.EQ.2) THEN                  
C      
C           *****  Shade and invert XSM for TRUE MLM BEAMFORMER             
C           CALL VADD (CORRFD,1,CORRNS,1,CORRFD,1,2*NRCV*NRCV)              
            CALL CMATIN (NRCV,CORRNS,CORRNS)         
C
C        FOR STOCHASTIC MLM DETERMINE Q AND D MATRICES
C
         ELSE IF (MAXLIK.EQ.4) THEN
              CALL QDMAT
C      
         END IF               
C      
      ELSE                    

         CALL RDBUF(33,CFILE,2*NRCV)
         CALL CVCONJ(CFILE,2,ARG,2,NRCV)
         CALL CMMUL(CFILE,NRCV,1,ARG,NRCV,CORRFD)
C
C      STOCHASTIC BLURRING OF SIGNAL COVARIANCE
C
         IF (BLURR) THEN
           CALL SIGBLR
         END IF

C        *****  MULTIPLY BY SOURCE LEVEL
         CALL VSMUL(CORRFD,1,SLEVEL,CORRFD,1,2*NRCV*NRCV)

         IF (MAXLIK.EQ.0.OR.MAXLIK.EQ.4) THEN

C           *****  ADD NOISE FOR LINEAR BEAMFORMER
            CALL VADD(CORRFD,1,CORRNS,1,CORRFD,1,2*NRCV*NRCV)

            IF (PUTFLG) THEN  
               CALL PUTXSM (CORRFD,NRCV,IFR,IERR)    
            END IF      
C
C        FOR STOCHASTIC MLM DETERMINE Q AND D MATRICES
C
            IF (MAXLIK.EQ.4) THEN
              CALL QDMAT
            END IF      

         ELSE IF (MAXLIK.EQ.1.OR.MAXLIK.EQ.3) THEN

C           *****  MATRIX INVERSION FOR MAX. LIKELIHOOD BEAMFORMING
C                  BY MEANS OF WOODBURY IDENTITY

            CALL CMATIN(NRCV,CORRNS,CNINV)
            CALL CMMUL(CNINV,NRCV,NRCV,CFILE,1,CFILE(NRCV+1))
            CALL CMMUL(ARG,1,NRCV,CFILE(NRCV+1),1,CC)
            XX=1E0+SLEVEL*REAL(CC)
            XX=-1E0/XX
            IF (IPRINT.NE.0) WRITE(26,*) 'XX: ',XX
            CALL CMMUL(CORRFD,NRCV,NRCV,CNINV,NRCV,CFILE(NRCV+1))
            CALL VSMUL(CFILE(NRCV+1),1,XX,CFILE(NRCV+1),1,2*NRCV*NRCV)
            X2=1E0
            CALL VADD (X2,0,CFILE(NRCV+1),2*(NRCV+1),CFILE(NRCV+1),
     1                 2*(NRCV+1),NRCV)
            CALL CMMUL(CNINV,NRCV,NRCV,CFILE(NRCV+1),NRCV,CORRFD)

            IF (MAXLIK.EQ.1.AND.(CRARAO.OR.GAMPLT)) THEN
               IF (IPRINT.NE.0) THEN
                  WRITE(26,*) 'Kn-1:'
                  WRITE(26,300) 
     -                   ((CNINV(I1+(I2-1)*NRCV),I2=1,NRCV),I1=1,NRCV)
               END IF
               IF (GAMPLT) THEN
C                 *****  PREPARE SIGNAL VECTORS FOR SIMILARITY CALCULATION
C                 CALL CVCONJ(CFILE,2,ARG,2,NRCV)
                  CALL CMMUL(ARG,1,NRCV,CNINV,NRCV,PATKN)
                  CALL CMMUL(PATKN,1,NRCV,CFILE,1,CC)
                  SNAT=1E0/REAL(CC)
                  CALL CMMUL(ARG,1,NRCV,CFILE,1,CC)
                  PATNM=REAL(CC)
               END IF
            END IF

         ELSE IF (MAXLIK.EQ.2) THEN

            CALL VADD(CORRFD,1,CORRNS,1,CORRFD,1,2*NRCV*NRCV)
            CALL CMATIN(NRCV,CORRNS,CORRNS)

         END IF

      END IF                  
c
c      reset replica buffer
c
       indexo=0
       npnt=0
C      WRITE(6,*)
C      WRITE(6,*) 'AMBIGUITY FUNCTION'
C      WRITE(6,*)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     loop for all required   Z  levels      --- 50 ---
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
      DO 50 I=1,NZ
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      loop for all required   X  levels      --- 40 ---
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
       DO 40 J=1,NX
        CALL RDBUF(INPFIL,AMP,NY)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C       loop for all required   Y  levels      --- 30 ---
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        DO 30 K=1,NY
         IF (IPRINT.NE.0) THEN
            WRITE(26,*) ZSC(I),' M SOURCE DEPTH'
            WRITE(26,*) XSC(J),' M SOURCE X-COORDINATE'
            WRITE(26,*) YSC(K),' M SOURCE Y-COORDINATE'
         END IF
         IF (MAXLIK.NE.3) THEN 
            CALL RDBUF(31,CFILE,2*NRCV)
            CALL CVCONJ(CFILE,2,ARG,2,NRCV)
         END IF
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C     AMBIGUITY FUNCTIONS
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
       IF (MAXLIK.LT.3) THEN  
         CALL CMMUL(CFILE,1,NRCV,ARG,1,CC)
         DENOM=REAL(CC)
         PAHATNM=DENOM
         IF (MAXLIK.LT.2) THEN
            CALL CMMUL(CORRFD,NRCV,NRCV,CFILE,1,CBUF)
            CALL CMMUL(ARG,1,NRCV,CBUF,1,CC)
            IF (MAXLIK.EQ.0) THEN
               RR=REAL(CC)/DENOM
               RI=RIMAG(CC)/DENOM
            ELSE
               CC=DENOM/CC
               RR=REAL(CC)
               RI=RIMAG(CC)
            END IF
         ELSE
c
c    true maximum likelihood
c
            CALL CMMUL(CORRNS,NRCV,NRCV,CFILE,1,CBUF)
            CALL CMMUL(ARG,1,NRCV,CBUF,1,CC)
            XX=1E0/REAL(CC)
            CALL VSMUL(CBUF,1,XX,CFILE,1,2*NRCV)        
c
c    normalization
c  
            CALL CTMMUL(CFILE,NRCV,1,CFILE,1,CC)
            DENOM=1E0/REAL(CC)
            CALL CVCONJ(CFILE,2,ARG,2,NRCV)
            CALL CMMUL(CORRFD,NRCV,NRCV,CFILE,1,CBUF)
            CALL CMMUL(ARG,1,NRCV,CBUF,1,CC)
            RR=REAL(CC)*DENOM
            RI=RIMAG(CC)*DENOM
         END IF
       ELSE IF (MAXLIK.EQ.3) THEN
C
C      MAXLIK=3: MULTIBLE CONSTRAINT MLM
C
        MAXSAV=MAXIN
          CALL GETEMAT(NRCV,J,K,I,NX,NY,NZ,EMAT,UWGHT,NWGHT)
        IF (NWGHT.GT.NPMAX) THEN
         WRITE(6,*) '>SOMETHING IS SCREWED UP, NWGHT=',NWGHT,' >',
     1              NPMAX
         STOP
        END IF
        NWSAVE=NWGHT
 269       CALL CMMUL(CORRFD,NRCV,NRCV,EMAT,NWGHT,CBUF)
c    premultiply by E+
        CALL CTMMUL(EMAT,NRCV,NWGHT,CBUF,NWGHT,EBUF1)
c
c       find eigenvalues and vectors for checking only
c
        IF (IPRINT.GT.0) THEN
         CALL EIGEN(EBUF1,NWGHT,NWGHT,ALAM,EBUF2,IERR)
         IF (IERR.NE.0) THEN
           WRITE(6,*) '>>>> ERROR RETURN FROM EIGEN <<<<'
         ELSE
           DO 450 IIP=1,NWGHT
             CALL CTMMUL(UWGHT,NWGHT,1,EBUF2(1+(IIP-1)*NWGHT),1,CC)
             WRITE(27,*) J,K,I,ALAM(IIP),CC,REAL(CC*CONJG(CC))/ALAM(IIP)
 450       CONTINUE
         END IF
        END IF
c
c    invert result
c        call infnrm(cninv,nwght,anrm)
c        call vsmul(cninv,1,1E0/anrm,cninv,1,2*nwght*nwght)
        iwne=-5
        iwnesav=iwne
 268    CALL CMINV(NWGHT,EBUF1,EBUF2,IERR)
        IF (IERR.NE.0) THEN
         IF (IPRINT.GT.0) THEN
          WRITE(6,*) '>>>> WARNING: [E+ S-1 E] SINGULAR AT'
          WRITE(6,*) '>>>> IX,IY,IZ:',J,K,I
          write(26,*) '[E+ S-1 E]='
          do 270 k1=1,nwght
          do 270 k2=1,nwght
            write(6,*) k1,k2,EBUF1(k1+(k2-1)*nwght)
 270      continue
          WRITE(6,*) '>>>> RETRY WITH NWGHT=',NWGHT-1
         END IF
c    prune constraints
c          NWGHT=NWGHT-1
c          INW=MIN(4,NWSAVE-NWGHT)
c          NRETRY(INW)=NRETRY(INW)+1
c          GO TO 269
c    add white noise
         diasum=0e0
         do k1=1,nwght
           id=k1+(k1-1)*nwght
           diasum=diasum+ebuf1(id)*conjg(ebuf1(id))
         end do
         dadd=diasum/nwght*10**(iwne)
         do k1=1,nwght
           id=k1+(k1-1)*nwght
           ebuf1(id)=ebuf1(id) + dadd
         end do
         iwne=iwne+1
         INW=MIN(4,iwne-iwnesav)
         NRETRY(INW)=NRETRY(INW)+1
         go to 268
        END IF
c        call vsmul(emat,1,1E0/anrm,emat,1,2*nwght*nwght)
        if (DEBUG) then
          write(26,*) 'emat:'
          do 271 k1=1,nwght
          do 271 k2=1,nwght
            write(6,*) k1,k2,EBUF1(k1+(k2-1)*nwght)
 271      continue
        end if
        if (DEBUG) then
          write(26,*) 'emat-1:'
          do 272 k1=1,nwght
          do 272 k2=1,nwght
            write(6,*) k1,k2,EBUF2(k1+(k2-1)*nwght)
 272      continue
        end if
        CALL CMMUL(EBUF2,NWGHT,NWGHT,UWGHT,1,EBUF1)
        CALL CTMMUL(UWGHT,NWGHT,1,EBUF1,1,CC)
C ***  DIRECTIVITY INDEX
        CALL CMMUL(EMAT,NRCV,NWGHT,EBUF1,1,EBUF2)
        CALL CMMUL(CORRFD,NRCV,NRCV,EBUF2,1,EBUF1)
        CALL CTMMUL(EBUF1,NRCV,1,EMAT,1,DIRIND)
        DIRIND=(DIRIND*CONJG(DIRIND))*NRCV
        CALL CTMMUL(EBUF1,NRCV,1,EBUF1,1,CNORM)
        DIRIND=DIRIND/REAL(CNORM)
C
C    NORMALIZATION
C
        CALL CTMMUL(EMAT,NRCV,NWGHT,EMAT,NWGHT,EBUF1)
        CALL CMINV(NWGHT,EBUF1,EBUF2,IERR)
        IF (IERR.NE.0) THEN
         IF (IPRINT.GT.0) THEN
          WRITE(6,*) '>>>> WARNING: [E+ E] SINGULAR AT'
          WRITE(6,*) '>>>>          IX,IY,IZ:',J,K,I
          write(6,*) '[E+ E]='
          do 273 k1=1,nwght
          do 273 k2=1,nwght
            write(6,*) k1,k2,EBUF1(k1+(k2-1)*nwght)
 273      continue
          WRITE(6,*) '>>>>           RETRY WITH NWGHT=',NWGHT-1
         END IF
          NWGHT=NWGHT-1
          INW=MIN(4,NWSAVE-NWGHT)
          NRETRY(INW)=NRETRY(INW)+1
          GO TO 269
        END IF
C
C       DETERMINE CONDITION NUMBER
C
        CALL INFNRM(EBUF1,NWGHT,ANRM)
        CALL INFNRM(EBUF2,NWGHT,BNRM)
        IF (ANRM*BNRM.GT.1E6) THEN
         IF (IPRINT.GT.0) THEN
          WRITE(6,*) '>>>> WARNING: [E+ E] IS ILL-CONDITIONED'
          WRITE(6,*) '>>>>          FOR IX,IY,IZ=',J,K,I
          write(6,*) '>>>>          K=',anrm*bnrm
         END IF
         IF (nwght.GT.1) THEN
          IF (IPRINT.GT.0)
     &    WRITE(6,*) '>>>>          RETRY WITH NWGHT=',NWGHT-1
          NWGHT=NWGHT-1
          INW=MIN(4,NWSAVE-NWGHT)
          NRETRY(INW)=NRETRY(INW)+1
          GO TO 269
         ELSE
          STOP '>>>> ABORTING: NWGHT ALREADY 1'
         END IF
        END IF
        CALL CMMUL(EBUF2,NWGHT,NWGHT,UWGHT,1,EBUF1)
        CALL CTMMUL(UWGHT,NWGHT,1,EBUF1,1,CNORM)
        RR=REAL(CC)/REAL(CNORM)
C        RI=RIMAG(CC/CNORM)
        MAXIN=MAXSAV
        WRITE(52,*) j,k,i,
     &              10.0*LOG10(max(1E-30,REAL(DIRIND)))
       ELSE
C
C      STOCHASTIC REPLICA BEAMFORMING (MAXLIK=4)
C
         IF (IPRINT.NE.0) THEN
          write(6,*) '>> repeig, ix,iy,iz=',j,k,i
         end if
        CALL REPEIG(RR) 
       END IF
         IF (IPRINT.NE.0) THEN
            WRITE(26,*) AMP(K),' OLD AMP'
         END IF
         RRR=MAX(RR,1E-10)
         AMP(K)=AMP(K)+WFREQ*10.*LOG10(RRR)
         AMPMX=MAX(AMPMX,AMP(K))
         IF (IPRINT.NE.0) THEN
            WRITE(26,100) RR,RI,5E0*LOG10(RR*RR+RI*RI),'dB'
            WRITE(26,*) AMP(K),' NEW AMP'
         END IF
 100     FORMAT(1H ,2(1X,G13.6),1X,F8.1,1X,A2,3X,A10)
C
C       SIMILARITY FUNCTION
C
        IF (GAMPLT) THEN
          CALL CMMUL(CNINV,NRCV,NRCV,CFILE,1,CBUF)
          CALL CMMUL(ARG,1,NRCV,CBUF,1,CC)
          SNAHAT=1E0/REAL(CC)
          CALL CMMUL(PATKN,1,NRCV,CFILE,1,CC)
          RR=REAL(CC)
          RI=RIMAG(CC)
          GAM(K)=SNAHAT*SNAT*(RR*RR+RI*RI)
C          CALL CMMUL(CORRFD,NRCV,NRCV,CFILE,1,CBUF)
C          CALL CMMUL(ARG,1,NRCV,CBUF,1,CC)
C          RR=REAL(CC)
C          RI=RIMAG(CC)
C          GAHAH=SNAHAT*SNAHAT*(RR*RR+RI*RI)
C          WRITE(26,*) I,J,K,GAHAH
C
C      CALCULATE SIGNAL LEVEL NECESSARY TO SUPPRESS 10 DB.
C
          IF (GAM(K).LT.1E0) THEN
            SIGLN=MAX(1E-20,(PAHATNM*SNAHAT-0.1*PATNM*SNAT)
     &            /(0.1*PATNM*(1.0-GAM(K))))
            SIGLN=10.0*LOG10(SIGLN)
          ELSE
            SIGLN=999.999
          END IF
          GAM(K+NY)=SIGLN
          WRITE(26,1100) I,J,K,GAM(K),SIGLN
 1100     FORMAT(1H ,'IZ,IX,IY,GAMMA-SQUARED,SIGLEV: ',
     &           3I5,F10.3,F10.2,' DB')
        END IF
 30     CONTINUE
        IF (GAMPLT) THEN
          CALL WRBUF(51,GAM,2*NY)
        END IF
        CALL WRBUF(IOUFIL,AMP,NY)
        IF (CRARAO.AND.MAXLIK.EQ.1) THEN
           CALL WRBUF(50,CRA(1,1),NY)
           CALL WRBUF(50,CRA(1,2),NY)
           CALL WRBUF(50,CRA(1,3),NY)
           CALL WRBUF(50,CRA(1,4),NY)
        END IF
 40    CONTINUE
 50   CONTINUE
      IF (MAXLIK.EQ.3) THEN
        WRITE(6,*) 'NUMBER OF 1. MCM RETRYS:',NRETRY(1)
        WRITE(6,*) 'NUMBER OF 2. MCM RETRYS:',NRETRY(2)
        WRITE(6,*) 'NUMBER OF 3. MCM RETRYS:',NRETRY(3)
        WRITE(6,*) 'NUMBER OF 4. MCM RETRYS:',NRETRY(4)
      END IF
      RETURN
      END  
      subroutine getemat(nrcv,ix,iy,iz,nx,ny,nz,emat,u,npl)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      parameter (ef=1e0/2.71828182846)
      INCLUDE 'compar.f'
      COMPLEX ECENT(NRMAX)
      logical blurr
      dimension emat(1),u(1)
      COMMON /MCMLM/  MAXIN,MCMDIR,blurr
c
c     subroutine for retreiving the replica matrix emat(nrcv,np)
c     for multible constraint MLM with a gaussian weighting,
c     calculated here and returned in u(np). The number of grid points
c     away from center determined by MAXIN, set in GETOPT.
c     
c     H. Schmidt 870826
c
c     871103 HS changed to only 1D mcm
c
      MAXDIF=MAXIN**2
c      call rwdbuf(31)
      npl=1
      iaddr=1
      nzmax=iz
      nzmin=iz
      nxmax=ix
      nxmin=ix
      nymax=iy
      nymin=iy
      if (MOD(MCMDIR,2).EQ.1) then
        nzmax=min(nz,iz+maxin)
        nzmin=max(1,iz-maxin)
      END IF
      if (MCMDIR.EQ.2.OR.MCMDIR.EQ.3.OR.MCMDIR.GE.6) then
       nxmax=min(nx,ix+maxin)
       nxmin=max(1,ix-maxin)
      END IF
      if (MCMDIR.GE.4) then
        nymax=min(ny,iy+maxin)
        nymin=max(1,iy-maxin)
      end if
c
      do 10 jz=nzmin,nzmax
       idz=(iz-jz)**2
       do 10 jx=nxmin,nxmax
        idx=(ix-jx)**2
        do 10 jy=nymin,nymax
         idy=(iy-jy)**2
         idif=idz+idx+idy
         if (idif.le.maxdif) then
c get replica field
          iaddr=1+2*npl*nrcv
          call grepl(jx,jy,jz,nx,ny,nz,emat(iaddr),nrcv)
c center point placed in column 1       
          if (idif.eq.0) then
            fac=1E0/cvlen(emat(iaddr),2,nrcv)
            call vsmul(emat(iaddr),1,fac,ecent,1,2*nrcv)
            call vmov(emat(iaddr),1,emat,1,2*nrcv)
          else
            npl=npl+1
          end if
        end if
 10   continue
      do 20 i=1,npl
        iaddr=1+2*(i-1)*nrcv
        call vsmul(emat(iaddr),1,fac,emat(iaddr),1,2*nrcv)
c        CALL CTMMUL(ECENT,NRCV,1,EMAT(IADDR),1,U(2*(i-1)+1))
        CALL CTMMUL(Emat(iaddr),NRCV,1,ECENT,1,U(2*(i-1)+1))
  20  continue
      return
      end      
      function cvlen(c,i,n)
      dimension c(1)
      i1=1
      clen=0E0
      do 10 j=1,n
        i2=i1+1
        clen=clen+c(i1)*c(i1)+c(i2)*c(i2)
        i1=i1+i
 10   continue
      cvlen=sqrt(clen)
      return
      end
      function vlen(c,i,n)
      dimension c(1)
      i1=1
      clen=0E0
      do 10 j=1,n
        clen=clen+c(i1)*c(i1)
        i1=i1+i
 10   continue
      vlen=sqrt(clen)
      return
      end
       
        
      subroutine grepl(ix,iy,iz,nx,ny,nz,rep,nrcv)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
c
c     subroutine for reading in replica fields
c     using array CFILE as a cyclic buffer
c     HS 870901
c
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      complex rep(1)
      common /reppnt/ indexo,numrep,npnt
c      data indexo /0/
c      data npnt /0/

      if (indexo.eq.0) then
        numrep=isize/nrcv
        call rwdbuf(31)
      end if
      index=((iz-1)*nx+ix-1)*ny+iy
      if ((indexo-index).ge.numrep) then
        call rwdbuf(31)
        indexo=0
        npnt=0
      end if
      if (index.gt.indexo) then
       do 10 i=indexo+1,index
        npnt=npnt+1
        if (npnt.gt.numrep) then
          npnt=1
        end if
        iad=(npnt-1)*nrcv+1
        call rdbuf(31,cfile(iad),2*nrcv)
 10    continue            
       indexo=index    
      else
       iad=npnt+(index-indexo)
       if (iad.lt.1) iad=iad+numrep
       iad=(iad-1)*nrcv+1
      end if
      call vmov(cfile(iad),1,rep,1,2*nrcv)
      return
      end
      
      subroutine infnrm(a,n,anrm)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      complex a(n,n)
      anrm=0
      do 10 i=1,n
        sum=0
        do 5 j=1,n
        sum=sum+abs(a(i,j))
 5    continue
      anrm=max(anrm,sum)
 10   continue
      return
      end
      SUBROUTINE SIGBLR
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      INCLUDE 'compar.f'
      INCLUDE 'recarr.f'
      INCLUDE 'corrfs.f'
      INCLUDE 'corrfn.f'
      INCLUDE 'pesbuf.f'

      COMPLEX XKN(nrnr),XKS(nrnr)
      EQUIVALENCE (XKN(1),CORRFD(1))
      EQUIVALENCE (XKS(1),CORRNS(1))
      COMPLEX Q(nrnr)
      EQUIVALENCE (Q(1),CNINV(1))
      COMPLEX EGNVEC(nrnr),DSQQ(nrnr),DQ(nrnr)
      COMPLEX DSQQI(nrnr)
      EQUIVALENCE (EGNVEC(1),CRP(1))
      EQUIVALENCE (DSQQ(1),CRPX(1))
      EQUIVALENCE (DQ(1),CRPY(1))
      EQUIVALENCE (DSQQI(1),CRPZ(1))

      REAL*4 D(NRMAX),DSR(NRMAX)
      EQUIVALENCE (D(1),PATKN(1))
      EQUIVALENCE (DSR(1),EMAT(1))

      COMMON /KSFPAR/ TOLR,CHL,CHLSIG
      
C      DATA CHL /0.02/
C
C     STOCHASTIC BLURRING OF SIGNAL COVARIANCE MATRIX
C
      DO 10 I=1,NRCV
       DO 10 J=I+1,NRCV
        DX=RAN(I)-RAN(J)
        DY=TRAN(I)-TRAN(J)
        DZ=DEP(I)-DEP(J)
        DD=DX*DX+DY*DY+DZ*DZ
        XKN(I+(J-1)*NRCV)=XKN(I+(J-1)*NRCV)*EXP(-0.5*CHLSIG*DD)
        XKN(J+(I-1)*NRCV)=CONJG(XKN(I+(J-1)*NRCV))
 10   CONTINUE
      RETURN
      END
      SUBROUTINE REPEIG(AMBIG)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'recarr.f'
      INCLUDE 'corrfs.f'
      INCLUDE 'corrfn.f'
      INCLUDE 'pesbuf.f'

      COMPLEX XKN(nrnr),XKS(nrnr)
      EQUIVALENCE (XKN(1),CORRFD(1))
      EQUIVALENCE (XKS(1),CORRNS(1))
      COMPLEX Q(nrnr)
      EQUIVALENCE (Q(1),CNINV(1))
      COMPLEX EGNVEC(nrnr),DSQQ(nrnr),DQ(nrnr)
      COMPLEX DSQQI(nrnr)
      EQUIVALENCE (EGNVEC(1),CRP(1))
      EQUIVALENCE (DSQQ(1),CRPX(1))
      EQUIVALENCE (DQ(1),CRPY(1))
      EQUIVALENCE (DSQQI(1),CRPZ(1))

      REAL*4 D(NRMAX),DSR(NRMAX)
      EQUIVALENCE (D(1),PATKN(1))
      EQUIVALENCE (DSR(1),EMAT(1))

      COMPLEX CX(NRMAX),CXT(NRMAX)
      COMPLEX CZERO,CONE,CSUM

      REAL*4 EGNVAL(NRMAX)

      COMMON /KSFPAR/ TOLR,CHL,chlsig

C      DATA TOLR/.001/
      DATA LX /4/

      CZERO=CMPLX(0.,0.)
      CONE=CMPLX(1.,0.)
C***    FIND replica covariance matrix
c       Replica is in array CFILE, the conjugate in ARG.
c       Normalize replica
        FACTOR=1E0/CVLEN(CFILE,2,NRCV)
        CALL VSMUL(CFILE,1,FACTOR,CFILE,1,2*NRCV)
        CALL CVCONJ(CFILE,2,ARG,2,NRCV)
        CALL CMMUL(CFILE,NRCV,1,ARG,NRCV,XKS)
C       GAUSSIAN BLURRING OF REPLICA
        CALL REPBLR      
C***    FIND EIGENVECTORS OF SIGCV
      CALL EIGEN(XKS,NRCV,NRCV,EGNVAL,EGNVEC,IERR)
      IF (IERR.NE.0) THEN
        STOP '>>>> ERROR IN EIGEN. ABORTING <<<<'
      end if
C***     FIND EIGENVALUE DISTRUBUTION AND DETERMINE SIGNAL SPACE
C***     DIMENSION (EIGENVALUES ORDER LOWEST TO HIGHEST OUT OF EIGEN)
      NS=0
      RSUM=0.
      DO 65 I=NRCV,1,-1
   65 RSUM=RSUM+XKS(I+(I-1)*NRCV)
      DO 70 I=1,NRCV
      PRCNT=EGNVAL(I)/RSUM
      IF (PRCNT.GT.TOLR) THEN
        NS=NS+1
        if (iprint.gt.0) then
         WRITE(6,*) 'I,PRCNT=',I,PRCNT
        end if
      END IF
   70 CONTINUE
      if (iprint.gt.0) then
       write(6,*) 'ns=',ns
      end if
C****    USE NS TO NRCV FOR SIGNAL SPACE
C****    FIND EIGENVALUE PROJECTION ON XKN
C      DO 73 ID=1,NS
C      IX=NRCV-ID+1
C      CSUM=CZERO
C      DO 72 I=1,NRCV
C      DO 72 J=1,NRCV
C   72 CSUM=CSUM+CONJG(EGNVEC(I+(IX-1)*NRCV))*XKN(I+(J-1)*NRCV)*
C     &      EGNVEC(J+(IX-1)*NRCV)
C      EGNDSQ(NDPHI,ID)=10.*LOG10(REAL(CSUM))
C      IF(NDPHI.GT.1)
C     & CALL PXY(ANGLE,EGNDSQ(1,ID),NDPHI,PHIMIN,PHIMAX,YMIN,YMAX,ID)
C   73 CONTINUE     

C****    FIND DSR*CONJG(Q)*EGNVEC(I)
      DO 80 ID=1,NS
      IX=NRCV-ID+1
      DO 75 I=1,NRCV
      CSUM=CZERO
      DO 74 J=1,I
   74 CSUM=CSUM+CONJG(Q(J+(I-1)*NRCV))*EGNVEC(J+(IX-1)*NRCV)
   75 DQ(I+(ID-1)*NRCV)=DSR(I)*CSUM
   80 CONTINUE
      DO 90 I=1,NS
      DO 90 J=1,I
      CSUM=CZERO
      DO 85 K=1,NRCV
   85 CSUM=CSUM+CONJG(DQ(K+(I-1)*NRCV))*DQ(K+(J-1)*NRCV) 
      DSQQ(I+(J-1)*NRCV)=CSUM
      DSQQ(J+(I-1)*NRCV)=CONJG(CSUM)
   90 CONTINUE
C***     MULTIPLY BY EIGENVALUES OF XKS
      DO 100 I=1,NS
      IX=NRCV-I+1
      DO 99 J=1,NS
   99 DSQQ(I+(J-1)*NRCV)=EGNVAL(IX)*DSQQ(I+(J-1)*NRCV)
  100 CONTINUE
C***     LARGEST EIGENVALUE AND VECTOR BY POWER METHOD
      RSUM=SQRT(FLOAT(NS))
      DO 120 I=1,NS
  120 CX(I)=CONE/RSUM
      DO 140 L=1,LX
C***     MATRIX PRODUCT
      DO 125 I=1,NS
      CSUM=CZERO
      DO 124 J=1,NS
  124 CSUM=CSUM+DSQQ(I+(J-1)*NRCV)*CX(J)
  125 CXT(I)=CSUM
C***     NORMALIZATION AND EIGENVALUE
      RSUM=0.
      CSUM=CZERO
      DO 130 I=1,NS
      AR=REAL(CXT(I))
      AI=RIMAG(CXT(I))
      RSUM=RSUM+AR**2+AI**2
      CSUM=CSUM+CONJG(CX(I))*CXT(I)
  130 CONTINUE
      RSUM=SQRT(RSUM)
      DO 135 I=1,NS
  135 CX(I)=CXT(I)/RSUM
  140 CONTINUE
      AMBIG=1E0/(REAL(CSUM))
      RETURN
      END
      SUBROUTINE REPBLR
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      INCLUDE 'compar.f'
      INCLUDE 'recarr.f'
      INCLUDE 'corrfs.f'
      INCLUDE 'corrfn.f'
      INCLUDE 'pesbuf.f'

      COMPLEX XKN(nrnr),XKS(nrnr)
      EQUIVALENCE (XKN(1),CORRFD(1))
      EQUIVALENCE (XKS(1),CORRNS(1))
      COMPLEX Q(nrnr)
      EQUIVALENCE (Q(1),CNINV(1))
      COMPLEX EGNVEC(nrnr),DSQQ(nrnr),DQ(nrnr)
      COMPLEX DSQQI(nrnr)
      EQUIVALENCE (EGNVEC(1),CRP(1))
      EQUIVALENCE (DSQQ(1),CRPX(1))
      EQUIVALENCE (DQ(1),CRPY(1))
      EQUIVALENCE (DSQQI(1),CRPZ(1))

      REAL*4 D(NRMAX),DSR(NRMAX)
      EQUIVALENCE (D(1),PATKN(1))
      EQUIVALENCE (DSR(1),EMAT(1))

      COMMON /KSFPAR/ TOLR,CHL,chlsig
      
C      DATA CHL /0.02/
C
C     STOCHASTIC BLURRING OF REPLICA
C
      DO 10 I=1,NRCV
       DO 10 J=I+1,NRCV
        DX=RAN(I)-RAN(J)
        DY=TRAN(I)-TRAN(J)
        DZ=DEP(I)-DEP(J)
        DD=DX*DX+DY*DY+DZ*DZ
        XKS(I+(J-1)*NRCV)=XKS(I+(J-1)*NRCV)*EXP(-0.5*CHL*DD)
        XKS(J+(I-1)*NRCV)=CONJG(XKS(I+(J-1)*NRCV))
 10   CONTINUE
      RETURN
      END
C****    computes the eigenvalues and eigenvectors of a 
C****    Hermitian matrix using EISPACK routines htridi and htribk
      SUBROUTINE EIGEN(KX,N,NMAX,EGNVAL,EGNVEC,IERR)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      PARAMETER (NM=20)
      COMPLEX KX(NMAX,NMAX),EGNVEC(NMAX,NMAX)
      REAL*4 EGNVAL(NMAX)
C****    SETUP TO ACCOMODATE UP TO 20 X 20
C****    EISPACK ARRAYS
      REAL*8 AR(NM,NM),AI(NM,NM),D(NM),E(NM),E2(NM),TAU(2,NM)
      REAL*8 ZR(NM,NM),ZI(NM,NM)
      COMPLEX CSUM,CZERO,EIGENT(NM)
      CZERO=CMPLX(0.,0.)
C****    SEPARATE REAL AND IMAG PARTS AS REQUIRED BY EISPACK
C****    ROUTINES, CLEAR ARRAYS FIRST        
      DO 10 I=1,NM
      DO  9 J=1,NM
      AR(I,J)=0.
      AI(I,J)=0.
      ZR(I,J)=0.
      ZI(I,J)=0.
    9 CONTINUE
      D(I)=0.
      E(I)=0.
      E2(I)=0.
      TAU(1,I)=0.
      TAU(2,I)=0.
   10 CONTINUE
      DO 20 I=1,N
      DO 19 J=1,N
      AR(I,J)=REAL(KX(I,J))
      AI(I,J)=RIMAG(KX(I,J))
   19 CONTINUE
      ZR(I,I)=1.
   20 CONTINUE
C***     HTRIDI CONVERTS TO REAL, TRIDIAGONAL MATRIX
      CALL HTRIDI(NM,N,AR,AI,D,E,E2,TAU)
C***     TQL2 CALCULATES THE EIGENVALUES AND VECTORS OF THE
C***     REAL, TRIDIAGONAL MATRIX
      CALL TQL2(NM,N,D,E,ZR,IERR)
C***     HTRIBK CALCULATES EIGENVALUES ON ORIGINAL MATRIX BY 
C***     BACK SUBSTITUTION
      CALL HTRIBK(NM,N,AR,AI,TAU,N,ZR,ZI)
C***     CONVERT BACK TO COMPLEX FORM
      DO 30 I=1,N
      DO 30 J=1,N
      EGNVEC(I,J)=CMPLX(ZR(I,J),ZI(I,J))
   30 CONTINUE
C***     TRANSFER EIGENVALUES  
      DO 40 I=1,N
      EGNVAL(I)=D(I)
   40 CONTINUE
C***      CHECK ORTHOGONALITY OF EIGENVECTORS
c      DO 50 I=1,N
c      DO 50 J=I,N
c      CSUM=CZERO
c      DO 49 K=1,N
c   49 CSUM=CSUM+CONJG(EGNVEC(K,I))*EGNVEC(K,J)
c      WRITE(*,1001) I,J,CSUM
c 1001 FORMAT(2I4,2E14.5)
c   50 CONTINUE  
      RETURN
      END
C***************************************************************** 
      subroutine htridi(nm,n,ar,ai,d,e,e2,tau)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
c
      integer i,j,k,l,n,ii,nm,jp1
      double precision ar(nm,n),ai(nm,n),d(n),e(n),e2(n),tau(2,n)
      double precision f,g,h,fi,gi,hh,si,scale,pythag
c
c     this subroutine is a translation of a complex anlogue of
c     the algol procedure tred1, num. math. 11, 181-195(1968)
c     by martin, reinsch, and wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 212-226(1971).
c
c     this subroutine reduces a complex hermitian matrix
c     to a real symmetric tridiagonal matrix using
c     unitary similarity transformations.
c
c     on input
c
c        nm must be set to the row dimension of two-dimensional
c          array parameters as declared in the calling program
c          dimension statement.
c
c        n is the order of the matrix.
c
c        ar and ai contain the real and imaginary parts,
c          respectively, of the complex hermitian input matrix.
c          only the lower triangle of the matrix need be supplied.
c
c     on output
c
c        ar and ai contain information about the unitary trans-
c          formations used in the reduction in their full lower
c          triangles.  their strict upper triangles and the
c          diagonal of ar are unaltered.
c
c        d contains the diagonal elements of the the tridiagonal matrix.
c
c        e contains the subdiagonal elements of the tridiagonal
c          matrix in its last n-1 positions.  e(1) is set to zero.
c
c        e2 contains the squares of the corresponding elements of e.
c          e2 may coincide with e if the squares are not needed.
c
c        tau contains further information about the transformations.
c
c     calls pythag for  dsqrt(a*a + b*b) .
c
c     questions and comments should be directed to burton s. garbow,
c     mathematics and computer science div, argonne national laboratory
c
c     this version dated august 1983.
c
c     ------------------------------------------------------------------
c
      tau(1,n) = 1.0d0
      tau(2,n) = 0.0d0
c
      do 100 i = 1, n
  100 d(i) = ar(i,i)
c     .......... for i=n step -1 until 1 do -- ..........
      do 300 ii = 1, n
         i = n + 1 - ii
         l = i - 1
         h = 0.0d0
         scale = 0.0d0
         if (l .lt. 1) go to 130
c     .......... scale row (algol tol then not needed) ..........
         do 120 k = 1, l
  120    scale = scale + dabs(ar(i,k)) + dabs(ai(i,k))
c
         if (scale .ne. 0.0d0) go to 140
         tau(1,l) = 1.0d0
         tau(2,l) = 0.0d0
  130    e(i) = 0.0d0
         e2(i) = 0.0d0
         go to 290
c
  140    do 150 k = 1, l
            ar(i,k) = ar(i,k) / scale
            ai(i,k) = ai(i,k) / scale
            h = h + ar(i,k) * ar(i,k) + ai(i,k) * ai(i,k)
  150    continue
c
         e2(i) = scale * scale * h
         g = dsqrt(h)
         e(i) = scale * g
         f = pythag(ar(i,l),ai(i,l))
c     .......... form next diagonal element of matrix t ..........
         if (f .eq. 0.0d0) go to 160
         tau(1,l) = (ai(i,l) * tau(2,i) - ar(i,l) * tau(1,i)) / f
         si = (ar(i,l) * tau(2,i) + ai(i,l) * tau(1,i)) / f
         h = h + f * g
         g = 1.0d0 + g / f
         ar(i,l) = g * ar(i,l)
         ai(i,l) = g * ai(i,l)
         if (l .eq. 1) go to 270
         go to 170
  160    tau(1,l) = -tau(1,i)
         si = tau(2,i)
         ar(i,l) = g
  170    f = 0.0d0
c
         do 240 j = 1, l
            g = 0.0d0
            gi = 0.0d0
c     .......... form element of a*u ..........
            do 180 k = 1, j
               g = g + ar(j,k) * ar(i,k) + ai(j,k) * ai(i,k)
               gi = gi - ar(j,k) * ai(i,k) + ai(j,k) * ar(i,k)
  180       continue
c
            jp1 = j + 1
            if (l .lt. jp1) go to 220
c
            do 200 k = jp1, l
               g = g + ar(k,j) * ar(i,k) - ai(k,j) * ai(i,k)
               gi = gi - ar(k,j) * ai(i,k) - ai(k,j) * ar(i,k)
  200       continue
c     .......... form element of p ..........
  220       e(j) = g / h
            tau(2,j) = gi / h
            f = f + e(j) * ar(i,j) - tau(2,j) * ai(i,j)
  240    continue
c
         hh = f / (h + h)
c     .......... form reduced a ..........
         do 260 j = 1, l
            f = ar(i,j)
            g = e(j) - hh * f
            e(j) = g
            fi = -ai(i,j)
            gi = tau(2,j) - hh * fi
            tau(2,j) = -gi
c
            do 260 k = 1, j
               ar(j,k) = ar(j,k) - f * e(k) - g * ar(i,k)
     x                           + fi * tau(2,k) + gi * ai(i,k)
               ai(j,k) = ai(j,k) - f * tau(2,k) - g * ai(i,k)
     x                           - fi * e(k) - gi * ar(i,k)
  260    continue
c
  270    do 280 k = 1, l
            ar(i,k) = scale * ar(i,k)
            ai(i,k) = scale * ai(i,k)
  280    continue
c
         tau(2,l) = -si
  290    hh = d(i)
         d(i) = ar(i,i)
         ar(i,i) = hh
         ai(i,i) = scale * dsqrt(h)
  300 continue
c
      return
      end
C**********************************************************************
      subroutine tql2(nm,n,d,e,z,ierr)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
c
      integer i,j,k,l,m,n,ii,l1,l2,nm,mml,ierr
      double precision d(n),e(n),z(nm,n)
      double precision c,c2,c3,dl1,el1,f,g,h,p,r,s,s2,tst1,tst2,pythag
c
c     this subroutine is a translation of the algol procedure tql2,
c     num. math. 11, 293-306(1968) by bowdler, martin, reinsch, and
c     wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 227-240(1971).
c
c     this subroutine finds the eigenvalues and eigenvectors
c     of a symmetric tridiagonal matrix by the ql method.
c     the eigenvectors of a full symmetric matrix can also
c     be found if  tred2  has been used to reduce this
c     full matrix to tridiagonal form.
c
c     on input
c
c        nm must be set to the row dimension of two-dimensional
c          array parameters as declared in the calling program
c          dimension statement.
c
c        n is the order of the matrix.
c
c        d contains the diagonal elements of the input matrix.
c
c        e contains the subdiagonal elements of the input matrix
c          in its last n-1 positions.  e(1) is arbitrary.
c
c        z contains the transformation matrix produced in the
c          reduction by  tred2, if performed.  if the eigenvectors
c          of the tridiagonal matrix are desired, z must contain
c          the identity matrix.
c
c      on output
c
c        d contains the eigenvalues in ascending order.  if an
c          error exit is made, the eigenvalues are correct but
c          unordered for indices 1,2,...,ierr-1.
c
c        e has been destroyed.
c
c        z contains orthonormal eigenvectors of the symmetric
c          tridiagonal (or full) matrix.  if an error exit is made,
c          z contains the eigenvectors associated with the stored
c          eigenvalues.
c
c        ierr is set to
c          zero       for normal return,
c          j          if the j-th eigenvalue has not been
c                     determined after 30 iterations.
c
c     calls pythag for  dsqrt(a*a + b*b) .
c
c     questions and comments should be directed to burton s. garbow,
c     mathematics and computer science div, argonne national laboratory
c
c     this version dated august 1983.
c
c     ------------------------------------------------------------------
c
      ierr = 0
      if (n .eq. 1) go to 1001
c
      do 100 i = 2, n
  100 e(i-1) = e(i)
c
      f = 0.0d0
      tst1 = 0.0d0
      e(n) = 0.0d0
c
      do 240 l = 1, n
         j = 0
         h = dabs(d(l)) + dabs(e(l))
         if (tst1 .lt. h) tst1 = h
c     .......... look for small sub-diagonal element ..........
         do 110 m = l, n
            tst2 = tst1 + dabs(e(m))
            if (tst2 .eq. tst1) go to 120
c     .......... e(n) is always zero, so there is no exit
c                through the bottom of the loop ..........
  110    continue
c
  120    if (m .eq. l) go to 220
  130    if (j .eq. 30) go to 1000
         j = j + 1
c     .......... form shift ..........
         l1 = l + 1
         l2 = l1 + 1
         g = d(l)
         p = (d(l1) - g) / (2.0d0 * e(l))
         r = pythag(p,1.0d0)
         d(l) = e(l) / (p + dsign(r,p))
         d(l1) = e(l) * (p + dsign(r,p))
         dl1 = d(l1)
         h = g - d(l)
         if (l2 .gt. n) go to 145
c
         do 140 i = l2, n
  140    d(i) = d(i) - h
c
  145    f = f + h
c     .......... ql transformation ..........
         p = d(m)
         c = 1.0d0
         c2 = c
         el1 = e(l1)
         s = 0.0d0
         mml = m - l
c     .......... for i=m-1 step -1 until l do -- ..........
         do 200 ii = 1, mml
            c3 = c2
            c2 = c
            s2 = s
            i = m - ii
            g = c * e(i)
            h = c * p
            r = pythag(p,e(i))
            e(i+1) = s * r
            s = e(i) / r
            c = p / r
            p = c * d(i) - s * g
            d(i+1) = h + s * (c * g + s * d(i))
c     .......... form vector ..........
            do 180 k = 1, n
               h = z(k,i+1)
               z(k,i+1) = s * z(k,i) + c * h
               z(k,i) = c * z(k,i) - s * h
  180       continue
c
  200    continue
c
         p = -s * s2 * c3 * el1 * e(l) / dl1
         e(l) = s * p
         d(l) = c * p
         tst2 = tst1 + dabs(e(l))
         if (tst2 .gt. tst1) go to 130
  220    d(l) = d(l) + f
  240 continue
c     .......... order eigenvalues and eigenvectors ..........
      do 300 ii = 2, n
         i = ii - 1
         k = i
         p = d(i)
c
         do 260 j = ii, n
            if (d(j) .ge. p) go to 260
            k = j
            p = d(j)
  260    continue
c
         if (k .eq. i) go to 300
         d(k) = d(i)
         d(i) = p
c
         do 280 j = 1, n
            p = z(j,i)
            z(j,i) = z(j,k)
            z(j,k) = p
  280    continue
c
  300 continue
c
      go to 1001
c     .......... set error -- no convergence to an
c                eigenvalue after 30 iterations ..........
 1000 ierr = l
 1001 return
      end
C**********************************************************************
      subroutine htribk(nm,n,ar,ai,tau,m,zr,zi)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
c
      integer i,j,k,l,m,n,nm
      double precision ar(nm,n),ai(nm,n),tau(2,n),zr(nm,m),zi(nm,m)
      double precision h,s,si
c
c     this subroutine is a translation of a complex anlogue of
c     the algol procedure trbak1, num. math. 11, 181-195(1968)
c     by martin, reinsch, and wilkinson.
c     handbook for auto. comp., vol.ii-linear algebra, 212-226(1971).
c
c     this subroutine forms the eigenvectors of a complex hermitian
c     matrix by back transforming those of the corresponding
c     real symmetric tridiagonal matrix determined by  htridi.
c
c     on input
c
c        nm must be set to the row dimension of two-dimensional
c          array parameters as declared in the calling program
c          dimension statement.
c
c        n is the order of the matrix.
c
c        ar and ai contain information about the unitary trans-
c          formations used in the reduction by  htridi  in their
c          full lower triangles except for the diagonal of ar.
c
c        tau contains further information about the transformations.
c
c        m is the number of eigenvectors to be back transformed.
c
c        zr contains the eigenvectors to be back transformed
c          in its first m columns.
c
c     on output
c
c        zr and zi contain the real and imaginary parts,
c          respectively, of the transformed eigenvectors
c          in their first m columns.
c
c     note that the last component of each returned vector
c     is real and that vector euclidean norms are preserved.
c
c     questions and comments should be directed to burton s. garbow,
c     mathematics and computer science div, argonne national laboratory
c
c     this version dated august 1983.
c
c     ------------------------------------------------------------------
c
      if (m .eq. 0) go to 200
c     .......... transform the eigenvectors of the real symmetric
c                tridiagonal matrix to those of the hermitian
c                tridiagonal matrix. ..........
      do 50 k = 1, n
c
         do 50 j = 1, m
            zi(k,j) = -zr(k,j) * tau(2,k)
            zr(k,j) = zr(k,j) * tau(1,k)
   50 continue
c
      if (n .eq. 1) go to 200
c     .......... recover and apply the householder matrices ..........
      do 140 i = 2, n
         l = i - 1
         h = ai(i,i)
         if (h .eq. 0.0d0) go to 140
c
         do 130 j = 1, m
            s = 0.0d0
            si = 0.0d0
c
            do 110 k = 1, l
               s = s + ar(i,k) * zr(k,j) - ai(i,k) * zi(k,j)
               si = si + ar(i,k) * zi(k,j) + ai(i,k) * zr(k,j)
  110       continue
c     .......... double divisions avoid possible underflow ..........
            s = (s / h) / h
            si = (si / h) / h
c
            do 120 k = 1, l
               zr(k,j) = zr(k,j) - s * ar(i,k) - si * ai(i,k)
               zi(k,j) = zi(k,j) - si * ar(i,k) + s * ai(i,k)
  120       continue
c
  130    continue
c
  140 continue
c
  200 return
      end
C*********************************************************************
      DOUBLE PRECISION FUNCTION PYTHAG(A,B)
      DOUBLE PRECISION A,B
C
C     FINDS DSQRT(A**2+B**2) WITHOUT OVERFLOW OR DESTRUCTIVE UNDERFLOW
C
      DOUBLE PRECISION P,R,S,T,U
      P = DMAX1(DABS(A),DABS(B))
      IF (P .EQ. 0.0D0) GO TO 20
      R = (DMIN1(DABS(A),DABS(B))/P)**2
   10 CONTINUE
	 T = 4.0D0 + R
	 IF (T .EQ. 4.0D0) GO TO 20
	 S = R/T
	 U = 1.0D0 + 2.0D0*S
	 P = U*P
	 R = (S/U)**2 * R
      GO TO 10
   20 PYTHAG = P
      RETURN
      END
      SUBROUTINE QDMAT()
c      SUBROUTINE QDMAT(NA)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      INCLUDE 'compar.f'
      INCLUDE 'recarr.f'
      INCLUDE 'corrfs.f'
      INCLUDE 'corrfn.f'
      INCLUDE 'pesbuf.f'

      COMPLEX XKN(nrnr),XKS(nrnr)
      EQUIVALENCE (XKN(1),CORRFD(1))
      EQUIVALENCE (XKS(1),CORRNS(1))
      COMPLEX Q(nrnr)
      EQUIVALENCE (Q(1),CNINV(1))
      COMPLEX EGNVEC(nrnr),DSQQ(nrnr),DQ(nrnr)
      COMPLEX DSQQI(nrnr)
      EQUIVALENCE (EGNVEC(1),CRP(1))
      EQUIVALENCE (DSQQ(1),CRPX(1))
      EQUIVALENCE (DQ(1),CRPY(1))
      EQUIVALENCE (DSQQI(1),CRPZ(1))

      REAL*4 D(NRMAX),DSR(NRMAX)
      EQUIVALENCE (D(1),PATKN(1))
      EQUIVALENCE (DSR(1),EMAT(1))

      COMPLEX CZERO,CONE,CSUM

      DATA TOLR/.001/

      CZERO=CMPLX(0.,0.)
      CONE=CMPLX(1.,0.)
C***    FIND SQUARE ROOT OF FIELD COVARIANCE (LOWER/UPPER TRIANGLES)
      CALL QINV(XKN,Q,D,NRCV,NRCV,IER)
      IF(IER.NE.0) WRITE(6,1003) IER
 1003 FORMAT(' IER =',I5)
C***    FIND SQUARE ROOT OF D
      DO 60 I=1,NRCV
   60 DSR(I)=SQRT(D(I))
      RETURN
      END
C***     SUBROUTINE FOR FACTORING THE INNVERSE OF A POSITIVE 
C***     DEFINITE MATRIX
C***
C***     FACTORING IS Q*D*CONJGTR(Q)
C***      Q (COMPLEX) IS UPPER TRIANGULAR WITH 1'S ON THE MAIN DIAGONAL
C***      D (REAL*4) IS DIAGONAL (=PREDICTION ERROR AT EACH STAGE)
      SUBROUTINE QINV(K,Q,D,N,NA,IER)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      PARAMETER (NMAX=20) 
C***      K = INPUT MATRIX
C***      Q = FACTORED SQUARE ROOT
C***      D = DIAGONAL OF SQUARE ROOT
C***      N = DIMENSION OF MATRIX
C***      NA = FORTRAN DIMENSION STATEMENT FOR K,Q,D
C***      IER = ERROR ID (0 FOR NORMAL RETURN, NX FOR ZERO MAIN DIAGONAL,
C***            -NX FOR PRECISION AT NXth STEP
      COMPLEX K(NA,NA),Q(NA,NA)
      REAL*4 D(NA)
      COMPLEX QN(NMAX),QK(NMAX),CSUM,CZERO,CONE,TMP
C***
      CZERO=CMPLX(0.,0.)
      CONE=CMPLX(1.,0.)
      IER=0
C***     ZERO MATRICES AND CHECK DIAGONAL FOR POSITIVE VALUE
      DO 10 I=1,N
      D(I)=0.
      AR=REAL(K(I,I))
      AI=RIMAG(K(I,I))
      IF((AR.GT.0.).AND.(ABS(AI).LT.1.E-20)) GO TO 9
      IER=I
      RETURN
    9 CONTINUE  
      DO 10 J=1,N
      Q(I,J)=CZERO
   10 CONTINUE
C***     1sT STEP
      Q(1,1)=CONE
      D(1)=1./K(1,1)
      IF(N.EQ.1) RETURN
C***     MAIN LOOP
      DO 100 NX=2,N
      NX1=NX-1
      DO 30 I=1,NX1
      CSUM=CZERO
      DO 29 J=1,I
   29 CSUM=CSUM+CONJG(Q(J,I))*K(J,NX)
   30 QK(I)=D(I)*CSUM
      DO 40 I=1,NX1
      CSUM=CZERO
      DO 39 J=I,NX1
   39 CSUM=CSUM+Q(I,J)*QK(J)
   40 QN(I)=CSUM
C***     CREATE Q FOR NEXT STEP
      DO 50 I=1,NX1
   50 Q(I,NX)=-QN(I)
      Q(NX,NX)=CONE
C***     COMPUTE D(NX)
      CSUM=CZERO
      DO 60 J=1,NX1
   60 CSUM=CSUM+K(NX,J)*QN(J)
      AR=REAL(CSUM)
      DX=REAL(K(NX,NX))-AR
      IF(ABS(DX).GT.1.E-20) GO TO 65
      IER=-NX
      RETURN
   65 D(NX)=1./DX
  100 CONTINUE
      RETURN
      END



      SUBROUTINE AMBDRW(TITLE,NPX,NPY,NX,NY,XLEFT,XRIGHT 
     $,XSCALE,XINC,YUP,YDOWN,YSCALE,YINC,ZMIN            
     $,ZMAX,ZSTEP,FREQ,SD,RECUP,RECLO,X1,XL,PX,BOPT,IAXT)               
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      DIMENSION SECTOR(28),PX(1)                   
      CHARACTER*6 BOPT      
      CHARACTER*50 FILENM
      CHARACTER*4 TITLEX(20),TITLEY(20)
      character*(*) title    
      DATA X1PL,Y1PL/2.,2.0/,HGTPT,HGTC,LABPT,NDIV,      
     *NARC/0.1,0.14,-3,1,5/,LABC,LWGT/-1,-1/,NSM/0/       
      DATA DUMMY /0./
      DATA TITLEX /'RANG','E (K','M)  ',17*'    '/
      DATA TITLEY /'DEPT','H (M',')   ',17*'    '/
C          
C   FORMATS
 401  FORMAT(1H ,F15.4,3X,'  NUMBER OF DATA POINTS ALONG THE X AXIS')           
 402  FORMAT(1H ,F15.4,3X,'  NUMBER OF DATA POINTS ALONG THE Y AXIS')           
403   FORMAT(1H ,F15.4,3X,'  DIVX ' )                   
404   FORMAT(1H ,F15.4,3X,'  DIVY ' )                   
405   FORMAT(1H ,F15.4,3X,'  FLAGRC ' )                   
406   FORMAT(1H ,F15.4,3X,'  RDUP ' )                   
407   FORMAT(1H ,F15.4,3X,'  RDLO ' )         
408   FORMAT(1H ,F15.4,3X,'  SOURCE DEPTH (M) ' )        
 409  FORMAT(1H ,F15.4,3X,'  NUMBER OF GRID POINTS ALONG THE X AXIS ' )         
 410  FORMAT(1H ,F15.4,3X,'  NUMBER OF GRID POINTS ALONG THE Y AXIS ' )         
  411 FORMAT(1H ,F15.4,3X,'  FREQUENCY (HZ)' )           
  412 FORMAT(1H ,F15.4,3X,'  DUMMY ' )                   
  413 FORMAT(1H ,F15.4,3X,'  CAY ' )                   
  414 FORMAT(1H ,F15.4,3X,'  NRNG ' )                   
  415 FORMAT(1H ,F15.4,3X,'  ZMIN ' )                    
  416 FORMAT(1H ,F15.4,3X,'  ZMAX ' )                    
  417 FORMAT(1H ,F15.4,3X,'  ZINC ' )                    
  418 FORMAT(1H ,F15.4,3X,'  X ORIGIN OF PLOT IN INCHES ' )                     
  419 FORMAT(1H ,F15.4,3X,'  DUMMY ' )                   
  420 FORMAT(1H ,F15.4,3X,'  Y ORIGIN OF PLOT IN INCHES ' )                     
  421 FORMAT(1H ,F15.4,3X,'  NSM   ' )                   
  422 FORMAT(1H ,F15.4,3X,'  HGTPT ' )                   
  423 FORMAT(1H ,F15.4,3X,'  HGTC ' )                    
  424 FORMAT(1H ,F15.4,3X,'  LABPT ' )                   
  425 FORMAT(1H ,F15.4,3X,'  NDIV ' )                    
  426 FORMAT(1H ,F15.4,3X,'  NARC ' )                    
  427 FORMAT(1H ,F15.4,3X,'  LABC ' )                    
  428 FORMAT(1H ,F15.4,3X,'  LWGT ' )                    
  800 FORMAT('AMBDR,',A3,',FMT,REV')         
 801  FORMAT(A50)            
 849  format(a)
  850 FORMAT(20A4)                
  900 FORMAT(1X,F15.4,3X,'  XLEFT',/,F15.4,4X,'  XRIGHT',/,F15.4,3X,            
     *'   XSCALE',/,F15.4,4X,'  XINC')                   
  901 FORMAT(1X,F15.4,3X,'  YUP',/,F15.4,4X,'  YDOWN',/,F15.4,3X,               
     *'   YSCALE',/,F15.4,4X,'  YINC')                   
  950 FORMAT(1H ,F15.4,1X,'    RMIN',/,F15.4,2X,'    RMAX')                     
      WRITE(28,800) BOPT(2:4)         
      WRITE(28,849)TITLE          
      CALL VCLR(SECTOR,1,28)
      SECTOR(1)=NPX               
C          
C   SECTOR(4) IS A FLAG WHICH IS SET TO ZERO IN THE RANGE
C   DEPENDENT VERSION OF SNAP FOR ALL SECTORS EXCEPT THE LAST                   
C   ONE. HERE IS USED TO INDICATE THAT THIS IS THE LAST SECTOR                  
       SECTOR(4)=1.0              
      WRITE(29,444) (SECTOR(L),L=1,28)
 444  FORMAT(1H ,6G13.5)
      INQUIRE(UNIT=29,NAME=FILENM)
      WRITE(28,801) FILENM         
      IF (IAXT.EQ.1) THEN
        TITLEY(1)='Dept'
        TITLEY(2)='h (m'
        TITLEY(3)=')  '
        DIVY=1E0 
        IF (ABS(XL-X1).LT.1.0) THEN
          TITLEX(1)='Rang'
          TITLEX(2)='e (m'
          TITLEX(3)=')   '
          DIVX=1E3
        ELSE
          TITLEX(1)='Rang'
          TITLEX(2)='e (k'
          TITLEX(3)='m)  '
          DIVX=1E0
        END IF
      ELSE IF (IAXT.EQ.2) THEN
        IF (ABS(XL-X1).LT.1.0) THEN
          TITLEX(1)='Rang'
          TITLEX(2)='e (m'
          TITLEX(3)=')   '
          DIVX=1E3
        ELSE
          TITLEX(1)='Rang'
          TITLEX(2)='e (k'
          TITLEX(3)='m)  '
          DIVX=1E0
        END IF
        IF (ABS(YUP-YDOWN).LT.1.0) THEN
          TITLEY(1)='Rang'
          TITLEY(2)='e (m'
          TITLEY(3)=')   '
          DIVY=1E3
        ELSE
          TITLEY(1)='Rang'
          TITLEY(2)='e (k'
          TITLEY(3)='m)  '
          DIVY=1E0
        END IF
      ELSE IF (IAXT.EQ.3) THEN
        TITLEX(1)='Bear'
        TITLEX(2)='ing '
        TITLEX(3)='(dg)'
        divx=1E0
        TITLEY(1)='Pitc'
        TITLEY(2)='h (d'
        TITLEY(3)='g)  '
        DIVY=1E0
      ELSE
      END IF
C      DIVY=1E0
      CAY=5.
      NRNG=5
      FLAGRC=0.
      WRITE(28,850)TITLEX         
      R1=X1                 
      R2=XL
      WRITE(28,950)R1,R2          
      AX1=XLEFT
      AX2=XRIGHT
      AX3=XSCALE
      AX4=XINC
      WRITE(28,900)AX1,AX2,AX3,AX4
      WRITE(28,850)TITLEY         
      WRITE(28,901)YUP,YDOWN,YSCALE,YINC                 
      WRITE(28,401)FLOAT(NPX)
      WRITE(28,402)FLOAT(NPY)
      WRITE(28,403)DIVX          
      WRITE(28,404)DIVY          
      WRITE(28,405)FLAGRC          
      WRITE(28,406)RECUP          
      WRITE(28,407)RECLO                 
      WRITE(28,408)SD 
C   NUMBER OF GRID POINTS ALONG THE X AXIS               
      WRITE(28,409)FLOAT(NX)      
C   NUMBER OF GRID POINTS ALONG THE Y AXIS               
      WRITE(28,410)FLOAT(NY)      
      WRITE(28,411)FREQ  
      WRITE(28,412)DUMMY          
      WRITE(28,413)CAY          
      WRITE(28,414)FLOAT(NRNG)          
      WRITE(28,415)ZMIN           
      WRITE(28,416)ZMAX           
      WRITE(28,417)ZSTEP          
C X ORIGIN  OF PLOT IN INCHES     
      WRITE(28,418)X1PL           
      WRITE(28,419)DUMMY          
C Y ORIGIN  OF PLOT IN INCHES     
      WRITE(28,420)Y1PL           
      WRITE(28,421)FLOAT(NSM)            
      WRITE(28,422)HGTPT          
      WRITE(28,423)HGTC           
      WRITE(28,424)FLOAT(LABPT)   
      WRITE(28,425)FLOAT(NDIV)    
      WRITE(28,426)FLOAT(NARC)    
      WRITE(28,427)FLOAT(LABC)    
      WRITE(28,428)FLOAT(LWGT)    
      RETURN                      
      END  



      SUBROUTINE CRADRW(TITLE,NPX,NPY,NX,NY,XLEFT,XRIGHT 
     $,XSCALE,XINC,YUP,YDOWN,YSCALE,YINC,ZMIN            
     $,ZMAX,ZSTEP,FREQ,SD,RECUP,RECLO,X1,XL,PX,BOPT,IAXT)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      DIMENSION SECTOR(28),PX(1)                   
      CHARACTER*6 BOPT      
      CHARACTER*50 FILENM
      CHARACTER*4 TITLEX(20),TITLEY(20)
      character*(*) title    
      DATA X1PL,Y1PL/2.,2.0/,HGTPT,HGTC,LABPT,NDIV,      
     *NARC/0.1,0.14,-3,1,5/,LABC,LWGT/-1,-1/,NSM/0/       
      DATA DUMMY /0./
      DATA TITLEX /'Rang','e (k','m)  ',17*'    '/
      DATA TITLEY /'Dept','h (m',')   ',17*'    '/
C          
C   FORMATS
 401  FORMAT(1H ,F15.4,3X,'  NUMBER OF DATA POINTS ALONG THE X AXIS')           
 402  FORMAT(1H ,F15.4,3X,'  NUMBER OF DATA POINTS ALONG THE Y AXIS')           
403   FORMAT(1H ,F15.4,3X,'  DIVX ' )                   
404   FORMAT(1H ,F15.4,3X,'  DIVY ' )                   
405   FORMAT(1H ,F15.4,3X,'  FLAGRC ' )                   
406   FORMAT(1H ,F15.4,3X,'  RDUP ' )                   
407   FORMAT(1H ,F15.4,3X,'  RDLO ' )         
408   FORMAT(1H ,F15.4,3X,'  SOURCE DEPTH (M) ' )        
 409  FORMAT(1H ,F15.4,3X,'  NUMBER OF GRID POINTS ALONG THE X AXIS ' )         
 410  FORMAT(1H ,F15.4,3X,'  NUMBER OF GRID POINTS ALONG THE Y AXIS ' )         
  411 FORMAT(1H ,F15.4,3X,'  FREQUENCY (HZ)' )           
  412 FORMAT(1H ,F15.4,3X,'  DUMMY ' )                   
  413 FORMAT(1H ,F15.4,3X,'  CAY ' )                   
  414 FORMAT(1H ,F15.4,3X,'  NRNG ' )                   
  415 FORMAT(1H ,F15.4,3X,'  ZMIN ' )                    
  416 FORMAT(1H ,F15.4,3X,'  ZMAX ' )                    
  417 FORMAT(1H ,F15.4,3X,'  ZINC ' )                    
  418 FORMAT(1H ,F15.4,3X,'  X ORIGIN OF PLOT IN INCHES ' )                     
  419 FORMAT(1H ,F15.4,3X,'  DUMMY ' )                   
  420 FORMAT(1H ,F15.4,3X,'  Y ORIGIN OF PLOT IN INCHES ' )                     
  421 FORMAT(1H ,F15.4,3X,'  NSM   ' )                   
  422 FORMAT(1H ,F15.4,3X,'  HGTPT ' )                   
  423 FORMAT(1H ,F15.4,3X,'  HGTC ' )                    
  424 FORMAT(1H ,F15.4,3X,'  LABPT ' )                   
  425 FORMAT(1H ,F15.4,3X,'  NDIV ' )                    
  426 FORMAT(1H ,F15.4,3X,'  NARC ' )                    
  427 FORMAT(1H ,F15.4,3X,'  LABC ' )                    
  428 FORMAT(1H ,F15.4,3X,'  LWGT ' )                    
  800 FORMAT('CRADR,',A3,',FMT,REV')         
 801  FORMAT(A50)            
  850 FORMAT(20A4)       
 849  format(a)
  900 FORMAT(1X,F15.4,3X,'  XLEFT',/,F15.4,4X,'  XRIGHT',/,F15.4,3X,            
     *'   XSCALE',/,F15.4,4X,'  XINC')                   
  901 FORMAT(1X,F15.4,3X,'  YUP',/,F15.4,4X,'  YDOWN',/,F15.4,3X,               
     *'   YSCALE',/,F15.4,4X,'  YINC')                   
  950 FORMAT(1H ,F15.4,1X,'    RMIN',/,F15.4,2X,'    RMAX')                     
      WRITE(28,800) BOPT(2:4)         
      WRITE(28,849)TITLE          
      CALL VCLR(SECTOR,1,28)
      SECTOR(1)=NPX               
C          
C   SECTOR(4) IS A FLAG WHICH IS SET TO ZERO IN THE RANGE
C   DEPENDENT VERSION OF SNAP FOR ALL SECTORS EXCEPT THE LAST                   
C   ONE. HERE IS USED TO INDICATE THAT THIS IS THE LAST SECTOR                  
       SECTOR(4)=1.0              
      WRITE(29,444) (SECTOR(L),L=1,28)
 444  FORMAT(1H ,6G13.5)
      INQUIRE(UNIT=29,NAME=FILENM)
      WRITE(28,801) FILENM         
      IF (IAXT.EQ.1) THEN
        TITLEY(1)='Dept'
        TITLEY(2)='h (m'
        TITLEY(3)=')  '
        DIVY=1E0 
        IF (ABS(XL-X1).LT.1.0) THEN
          TITLEX(1)='Rang'
          TITLEX(2)='e (m'
          TITLEX(3)=')   '
          DIVX=1E0
        ELSE
          TITLEX(1)='Rang'
          TITLEX(2)='e (k'
          TITLEX(3)='m)  '
          DIVX=1E-3
        END IF
      ELSE IF (IAXT.EQ.2) THEN
        IF (ABS(XL-X1).LT.1.0) THEN
          TITLEX(1)='Rang'
          TITLEX(2)='e (m'
          TITLEX(3)=')   '
          DIVX=1E0
        ELSE
          TITLEX(1)='Rang'
          TITLEX(2)='e (k'
          TITLEX(3)='m)  '
          DIVX=1E-3
        END IF
        IF (ABS(YUP-YDOWN).LT.1.0) THEN
          TITLEY(1)='Rang'
          TITLEY(2)='e (m'
          TITLEY(3)=')   '
          DIVY=1E3
        ELSE
          TITLEY(1)='Rang'
          TITLEY(2)='e (k'
          TITLEY(3)='m)  '
          DIVY=1E0
        END IF
      ELSE IF (IAXT.EQ.3) THEN
        TITLEX(1)='Phi '
        TITLEX(2)='    '
        TITLEX(3)='    '
        divx=1E0
        TITLEY(1)='Thet'
        TITLEY(2)='a   '
        TITLEY(3)='    '
        DIVY=1E0
      ELSE
      END IF
C      DIVY=1E0
      CAY=5.
      NRNG=5
      FLAGRC=0.
      WRITE(28,850)TITLEX         
      R1=X1*1.0E3                 
      R2=XL*1.0E3                 
      WRITE(28,950)R1,R2          
      AX1=XLEFT*1.0E3             
      AX2=XRIGHT*1.0E3            
      AX3=XSCALE*1.0E3            
      AX4=XINC*1.0E3              
      WRITE(28,900)AX1,AX2,AX3,AX4
      WRITE(28,850)TITLEY         
      WRITE(28,901)YUP,YDOWN,YSCALE,YINC                 
      WRITE(28,401)FLOAT(NPX)
      WRITE(28,402)FLOAT(NPY)
      WRITE(28,403)DIVX          
      WRITE(28,404)DIVY          
      WRITE(28,405)FLAGRC          
      WRITE(28,406)RECUP          
      WRITE(28,407)RECLO                 
      WRITE(28,408)SD 
C   NUMBER OF GRID POINTS ALONG THE X AXIS               
      WRITE(28,409)FLOAT(NX)      
C   NUMBER OF GRID POINTS ALONG THE Y AXIS               
      WRITE(28,410)FLOAT(NY)      
      WRITE(28,411)FREQ  
      WRITE(28,412)DUMMY          
      WRITE(28,413)CAY          
      WRITE(28,414)FLOAT(NRNG)          
      WRITE(28,415)ZMIN           
      WRITE(28,416)ZMAX           
      WRITE(28,417)ZSTEP          
C X ORIGIN  OF PLOT IN INCHES     
      WRITE(28,418)X1PL           
      WRITE(28,419)DUMMY          
C Y ORIGIN  OF PLOT IN INCHES     
      WRITE(28,420)Y1PL           
      WRITE(28,421)FLOAT(NSM)            
      WRITE(28,422)HGTPT          
      WRITE(28,423)HGTC           
      WRITE(28,424)FLOAT(LABPT)   
      WRITE(28,425)FLOAT(NDIV)    
      WRITE(28,426)FLOAT(NARC)    
      WRITE(28,427)FLOAT(LABC)    
      WRITE(28,428)FLOAT(LWGT)    
      RETURN                      
      END  



        SUBROUTINE MATIN(NM,A,AR,IERR)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
        PARAMETER (NMAX=10)
        DIMENSION A(NM,NM),AR(NM,NM)
        DIMENSION BR(NMAX),CR(NMAX),
     1            IP(NMAX),IQ(NMAX)
C       DOUBLE PRECISION A,AR,CR,BR,ZR,PR
       IERR=0
       IF (NM.GT.NMAX) THEN
         IERR=2
         RETURN
       END IF
       DO 51 I=1,NM
        DO 51 J=1,NM
        AR(I,J)=A(I,J)
 51     CONTINUE
       DO 1 K=1,NM
       PR=0.0D0
       DO 100 I=K,NM
       DO 100 J=K,NM
       ZR=AR(I,J)
       IF(ZR*ZR-PR*PR)100,100,101
 101    PR=AR(I,J)
        IP(K)=I
       IQ(K)=J
 100   CONTINUE
       IF(ABS(PR).LT.1.0D-37)GO TO 35
       IPK=IP(K)
       IQK=IQ(K)
       IF(IPK-K)200,299,200
 200   DO 201 J=1,NM
       ZR=AR(IPK,J)
       AR(IPK,J)=AR(K,J)
       AR(K,J)=ZR
 201   CONTINUE
 299   CONTINUE
       IF(IQK-K)300,399,300
 300   DO 301 I=1,NM
       ZR=AR(I,IQK)
       AR(I,IQK)=AR(I,K)
       AR(I,K)=ZR
 301   CONTINUE
 399   CONTINUE
        ZR=1.0D0/(PR*PR)
       DO 400 J=1,NM
       IF(J-K)403,402,403
 402   BR(J)=PR*ZR
       CR(J)=1.0D0
       GO TO 404
 403   BR(J)=-(AR(K,J)*PR)*ZR
       CR(J)=AR(J,K)
 404   AR(K,J)=0.0D0
        AR(J,K)=0.0D0
  400  CONTINUE
       DO 405 I=1,NM
       DO 405 J=1,NM
       AR(I,J)=AR(I,J)+CR(I)*BR(J)
 405  CONTINUE
  1    CONTINUE
       K=NM
       DO 500 KM=1,NM
       IPK=IP(K)
       IQK=IQ(K)
       IF(IPK-K)501,502,501
 501   DO 503 I=1,NM
       ZR=AR(I,IPK)
       AR(I,IPK)=AR(I,K)
       AR(I,K)=ZR
 503   CONTINUE
 502   IF(IQK-K)504,500,504
 504   DO 506 J=1,NM
       ZR=AR(IQK,J)
       AR(IQK,J)=AR(K,J)
       AR(K,J)=ZR
 506   CONTINUE
 500   K=K-1
          RETURN
  35      IERR=1
          RETURN 
          END



      SUBROUTINE PLPWAB(X,ISTEP,LF,RMIN,RSTEP,TITLE,INR,PX,MODU,
     1      XLEN,YLEN,XLEFT,XRIGHT,XINC,
     2      YUP,YDOWN,YINC,FREQ,IAXT,SLEVDB,SNLEVDB,WNLEVDB)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      DIMENSION X(1)
      DIMENSION PX(MODU)
      CHARACTER*80 TITLE,TITLEX,TITLEY,HEADING
      CHARACTER*3 XTYP,YTYP
      CHARACTER*40 LAB
      CHARACTER*6 OPTION(2),OPT2(3)
      DATA OPTION /'PLWAF ','      '/
      DATA OPT2 /' BE   ',' ML   ',' TML  '/
      DATA XTYP,YTYP /'LIN','LIN'/
      OPTION(2)=OPT2(INR)
      I=INR
      IF (INR.EQ.0) I=1
      WRITE(19,777) OPTION
      HEADING='AMBIGUITY FUNCTION'
      NH=18
      WRITE(19,778) HEADING
      WRITE(19,778) TITLE
 777  FORMAT(1H ,2A6)
 778  FORMAT(1H ,A80)
 779  FORMAT(1H ,A40)
 780  FORMAT(1H ,A3)
      WRITE(LAB,801) FREQ
 801  FORMAT('FREQ:',F7.1,' Hz$')
 802  FORMAT('SL:  ',F7.1,' dB$')
 803  FORMAT('NL:  ',F7.1,' dB$')
 804  FORMAT('WN:  ',F7.1,' dB$')
      NLAB=4
      WRITE(19,6010) NLAB,'NUMBER OF LABELS'
      WRITE(19,779) LAB
      WRITE(LAB,802) SLEVDB
      WRITE(19,779) LAB
      WRITE(LAB,803) SNLEVDB
      WRITE(19,779) LAB
      WRITE(LAB,804) WNLEVDB
      WRITE(19,779) LAB
      WRITE(19,6020) XLEN,'LENGTH OF X-AXIS IN CM'
      WRITE(19,6020) YLEN,'LENGTH OF Y-AXIS IN CM'
      WRITE(19,6010) 0,'GRID TYPE. 0: NO GRID'
      WRITE(19,6030) XLEFT,'XLEFT'
      WRITE(19,6030) XRIGHT,'XRIGHT'
      WRITE(19,6030) XINC,'XINC'
      WRITE(19,6030) 1.,'XDIV'
      IF (IAXT.EQ.1) THEN
      TITLEX='Grazing angle (degrees)$'
      ELSE
      TITLEX='Bearing angle (degrees)$'
      end if
      WRITE(19,778) TITLEX
      WRITE(19,780) XTYP
      WRITE(19,6030) YDOWN,'YDOWN'
      WRITE(19,6030) YUP,'YUP'
      WRITE(19,6030) YINC,'YINC'
      WRITE(19,6030) 1.,'YDIV'
      TITLEY='Amplitude (dB)$'
      WRITE(19,778) TITLEY
      WRITE(19,780) YTYP
      WRITE(19,6010) 1,'NC'
      WRITE(19,6010) LF,'N'
      WRITE(19,6020) RMIN,'RMIN'
      WRITE(19,6030) RSTEP,'RSTEP'
      WRITE(19,6030) 0.,'YMIN'
      WRITE(19,6020) 0.,'DY'
 6010 FORMAT(1H ,I8,8X,A40)
 6020 FORMAT(1H ,F15.6,1X,A40)
 6030 FORMAT(1H ,G15.6,1X,A40)
      DO 1350 K=1,LF,MODU     
      I1=K      
      I2=I1+MODU-1    
      I2=MIN0(I2,LF)    
      JK=0
      DO 1300 J=I1,I2   
      JK=JK+1
      PX(JK)=X(1+(J-1)*ISTEP)       
 1300 CONTINUE  
      WRITE(20,444)(PX(J),J=1,JK)   
 444  FORMAT(1H ,6G13.5)
1350  CONTINUE  
      RETURN
      END







