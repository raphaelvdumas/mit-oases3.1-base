c TRFTOMAT: Fortran program that converts both ascii and binary trf files to
c Matlab files. Most of the info in the trf file turns up as 
c single precision float variables in the MAT file. (To do anything
c useful in Matlab with these variable, they need to be converted to
c double precision with the double() function.
c The actual data in the trf file becomes a six dimensional matrix
c in matlab with size: [2(for r/i),NOUT,IR,NPLOTS,MSUFT,MXTRF-LXTRF+1]

c Script readtrfmat.m is a utility that reads a mat file created by this
c program, does to single --> double conversion on all variables, and
c "squeezes" the data to the smallest dimesion possible

c Eddie Scheer, WHOI August,1999
      program trftomat
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'compul.f'

      LOGICAL BINFILE
      COMMON /FILFRM/ BINFILE
      character*80 filein,fileout

      real matdum
      dimension matdum(10000)
      character*80 cmatdum
      dimension cmatdum(80)
c     max expected dimension of matlab vars
      dimension idim(10)
c >>> input file names
      write(6,*) 'Input file?'
      read(5,'(A)') filein

      call matopen

C ***  OPEN TRF-FILE AND READ HEADING

      CALL rwHEAD(ISTAT,filein,fileout)

      call matcsetup(lenstr(PROGNM),PADFLAG
     &     ,REM,6,"PROGNM")
      call matcwrite(PROGNM,lenstr(PROGNM)
     &     ,PADFLAG,REM)
      matdum(1) = NOUT
      ndim = 2
      idim(1) = 1
      idim(2) = 1
      call matsetup(ndim,idim,PADFLAG,4,"NOUT")
      call matwrite(matdum,1,PADFLAG)
      do 2000 iii=1,12
         matdum(iii)=IPARM(iii)
 2000 continue
      idim(1) = 1
      idim(2) = 12
      call matsetup(ndim,idim,PADFLAG,5,"IPARM")
      call matwrite(matdum,12,PADFLAG)

      call matcsetup(lenstr(TITLE),PADFLAG
     &     ,REM,5,"TITLE")
      call matcwrite(TITLE,lenstr(TITLE),PADFLAG,REM)

      print *,'TITLE',title
      print *,'TITLE LENGTH',lenstr(title)
      print *,'SIGNN',title
      print *,'SIGNN LENGTH',lenstr(SIGNN)
      call matcsetup(lenstr(SIGNN),PADFLAG,REM,5,"SIGNN")
      call matcwrite(SIGNN,lenstr(SIGNN),PADFLAG,REM)
      matdum(1) = FCTRF
      idim(1) = 1
      idim(2) = 1
      call matsetup(ndim,idim,PADFLAG,5,"FCTRF")
      call matwrite(matdum,1,PADFLAG)
      matdum(1) = SD
      call matsetup(ndim,idim,PADFLAG,2,"SD")
      call matwrite(matdum,1,PADFLAG)
      matdum(1) = RD
      call matsetup(ndim,idim,PADFLAG,2,"RD")
      call matwrite(matdum,1,PADFLAG)
      matdum(1) = RDLOW
      call matsetup(ndim,idim,PADFLAG,5,"RDLOW")
      call matwrite(matdum,1,PADFLAG)
      matdum(1) = IR
      call matsetup(ndim,idim,PADFLAG,2,"IR")
      call matwrite(matdum,1,PADFLAG)
      do 2001 iii=1,IR
         matdum(iii)=RDC(iii)
 2001 continue
      idim(1) = 1
      idim(2) = IR
      call matsetup(ndim,idim,PADFLAG,3,"RDC")
      call matwrite(matdum,IR,PADFLAG)
      idim(1) = 1
      idim(2) = 1
      matdum(1) = R0
      call matsetup(ndim,idim,PADFLAG,2,"R0")
      call matwrite(matdum,1,PADFLAG)
      matdum(1) = RSPACE
      call matsetup(ndim,idim,PADFLAG,6,"RSPACE")
      call matwrite(matdum,1,PADFLAG)
      matdum(1) = NPLOTS
      call matsetup(ndim,idim,PADFLAG,6,"NPLOTS")
      call matwrite(matdum,1,PADFLAG)

      matdum(1) = NX
      call matsetup(ndim,idim,PADFLAG,2,"NX")
      call matwrite(matdum,1,PADFLAG)
      matdum(1) = LXTRF
      call matsetup(ndim,idim,PADFLAG,5,"LXTRF")
      call matwrite(matdum,1,PADFLAG)
      matdum(1) = MXTRF
      call matsetup(ndim,idim,PADFLAG,5,"MXTRF")
      call matwrite(matdum,1,PADFLAG)
      matdum(1) = DT
      call matsetup(ndim,idim,PADFLAG,2,"DT")
      call matwrite(matdum,1,PADFLAG)
      
      DF = 1/(DT*NX)
      matdum(1) = DF
      call matsetup(ndim,idim,PADFLAG,2,"DF")
      call matwrite(matdum,1,PADFLAG)

      do 2002 ii = LXTRF-1,MXTRF-1
         matdum(ii) = ii*DF
 2002 continue
      idim(1) = 1
      idim(2) = MXTRF-LXTRF+1

      call matsetup(ndim,idim,PADFLAG,4,"FREQ")
      call matwrite(matdum,MXTRF-LXTRF+1,PADFLAG)

      matdum(1) = ICDRIN
      idim(1) = 1
      idim(2) = 1

      call matsetup(ndim,idim,PADFLAG,6,"ICDRIN")
      call matwrite(matdum,1,PADFLAG)
      matdum(1) = OMEGIM
      call matsetup(ndim,idim,PADFLAG,6,"OMEGIM")
      call matwrite(matdum,1,PADFLAG)
      matdum(1) = MSUFT
      call matsetup(ndim,idim,PADFLAG,5,"MSUFT")
      call matwrite(matdum,1,PADFLAG)
      matdum(1) = ISROW
      call matsetup(ndim,idim,PADFLAG,5,"ISROW")
      call matwrite(matdum,1,PADFLAG)
      matdum(1) = INTTYP
      call matsetup(ndim,idim,PADFLAG,6,"INTTYP")
      call matwrite(matdum,1,PADFLAG)
      IF (NPLOTS.GT.1) THEN
         do 2006 iii=1,nplots
            RANGE=R0+(iii-1)*RSPACE
            RANGEM=RANGE*1000.0
            matdum(iii)=rangem
 2006    continue
      END IF
      idim(1) = nplots
      idim(2) = 1
      call matsetup(ndim,idim,PADFLAG,5,"RANGE")
      call matwrite(matdum,nplots,PADFLAG)

c      idim1 = MXTRF-LXTRF+1
c      idim2 = MSUFT
c      idim3 = NPLOTS
c      idim4 = IR
c      idim5 = NOUT

      
      ndim = 6
      idim(6) = MXTRF-LXTRF+1
      idim(5) = MSUFT
      idim(4) = NPLOTS
      idim(3) = IR
      idim(2) = NOUT
C idim(1)= 2 for real then imag
      idim(1) = 2

      call matsetup(ndim,idim,PADFLAG,4,"DATA")


      DO 35 K=LXTRF,MXTRF
         do 40 M=1,MSUFT
            DO 40 I=1,NPLOTS
               DO 50 J=1,IR
                  if (BINFILE) THEN
                     READ(15) (ARG(KK),FAC(KK),KK=1,NOUT)
                  else
                     READ(15,*) (ARG(KK),FAC(KK),KK=1,NOUT)
                  endif
                  do 2010 kk=1,nout
                     matdum(1) = ARG(KK)
                     matdum(2) = FAC(KK)
                     call matwrite(matdum,2,PADFLAG)
 2010             continue
 50            CONTINUE
 40         CONTINUE
 35      CONTINUE
         PRINT *,"GENERATED MAT FILE WITH 6-D DATA MATRIX"
         PRINT *,"SIZE: [2(for r/i),NOUT,IR,NPLOTS,MSUFT,MXTRF-LXTRF+1]"
         END
      SUBROUTINE RwHEAD(ISTAT,filein,fileout)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'compul.f'

      character*(*) filein,fileout
      LOGICAL BINFILE
      COMMON /FILFRM/ BINFILE

      CHARACTER*8 FILEID
      CHARACTER PARCHC(12)
      DATA PARCHC /'N','V','H','T',8*' '/
      ISTAT=0
      BINFILE=.FALSE.
c       CHECK IF ASCII FILE
      OPEN(15,FILE=FILEin,STATUS='OLD',FORM='FORMATTED',ERR=500)
      READ(15,20,ERR=500) FILEID
c >>> non-saclant format
      print *,'FILEID = ',fileid
      IF (FILEID.NE.'PULSETRF') THEN
         go to 500
      END IF
      write(6,*) 'ASCII File'
      READ(15,20) PROGNM
      READ(15,*) NOUT
      READ(15,*) (IPARM(J),J=1,NOUT)
      READ(15,20) TITLE
      READ(15,20) SIGNN
      READ(15,*) FCTRF
      READ(15,*) SD
      READ(15,*) RD,RDLOW,IR
      IF (IR.GT.0) THEN
         IF (IR.GT.NRD) THEN
            WRITE(6,*) '>>> TOO MANY RECEIVERS IN FILE <<<'
            ISTAT=3
            RETURN
         END IF
         IF (IR.GT.1) THEN
            RDSTEP=(RDLOW-RD)/FLOAT(IR-1)
         ELSE
            RDSTEP=1.0
         ENDIF
         DO 1105 L=1,IR
            RDC(L)=(L-1)*RDSTEP+RD
 1105    CONTINUE
      ELSE
         IR=ABS(IR)
         READ(15,*) (RDC(L),L=1,IR)
         rd=rdc(1)
         rdlow=rdc(ir)
      END IF

      READ(15,*) R0,RSPACE,NPLOTS
      READ(15,*)NX,LXTRF,MXTRF,DT

      READ(15,*) ICDRIN
      ICDR=ICDRIN
      READ(15,*) OMEGIM
C     *** READ HEADER EXTENSION
      READ(15,*) MSUFT
      READ(15,*) ISROW
      READ(15,*) INTTYP
        write(6,*) 'omegim=',omegim
        write(6,*) 'IR=    ',ir
        write(6,*) 'nplot= ',nplots
        write(6,*) 'nout=  ',nout
        write(6,*) 'msuft= ',msuft

      DO 2300 I=1,2
         READ(15,*) IDUMMY
 2300 CONTINUE
      DO 2400 I=1,5
         READ(15,*) DUMMY
 2400 CONTINUE
      go to 600

 500  CLOSE(15,STATUS='KEEP',ERR=501)

 501  BINFILE=.TRUE.
      OPEN(15,FILE=FILEin,STATUS='OLD',FORM='UNFORMATTED',ERR=995)

      READ(15) FILEID
      READ(15) PROGNM
        READ(15) NOUT
        READ(15) (IPARM(J),J=1,NOUT)
        READ(15) TITLE
        READ(15) SIGNN
        READ(15) FCTRF
        READ(15) SD
        READ(15) RD,RDLOW,IR
        IF (IR.GT.0) THEN
         IF (IR.GT.NRD) THEN
          WRITE(6,*) '>>> TOO MANY RECEIVERS IN FILE <<<'
          ISTAT=3
          RETURN
         END IF
         IF (IR.GT.1) THEN
          RDSTEP=(RDLOW-RD)/FLOAT(IR-1)
         ELSE
          RDSTEP=1.0
         ENDIF
         DO 5 L=1,IR
          RDC(L)=(L-1)*RDSTEP+RD
  5      CONTINUE
        ELSE
          READ(15) (RDC(L),L=1,(-IR))
        END IF

        READ(15) R0,RSPACE,NPLOTS
        READ(15)NX,LXTRF,MXTRF,DT
        READ(15) ICDRIN
        ICDR=ICDRIN
        READ(15) OMEGIM
C *** READ HEADER EXTENSION
        READ(15) MSUFT
        READ(15) ISROW
        READ(15) INTTYP
        DO 300 I=1,2
         READ(15) IDUMMY
 300     CONTINUE
        DO 400 I=1,5
         READ(15) DUMMY
 400     CONTINUE
        write(6,*) 'omegim=',omegim
        write(6,*) 'IR=    ',ir
        write(6,*) 'nplot= ',nplots
        write(6,*) 'nout=  ',nout
        write(6,*) 'msuft= ',msuft
 
c        OPEN(16,FILE=FILEout,STATUS='unknown',FORM='FORMATTED',ERR=995)
c        WRITE(16,20) FILEID
c        WRITE(16,20) PROGNM
c        WRITE(16,*) NOUT
c        WRITE(16,*) (IPARM(J),J=1,NOUT)
c        WRITE(16,20) TITLE
c        WRITE(16,20) SIGNN
c        WRITE(16,*) FCTRF
c        WRITE(16,*) SD
c        WRITE(16,*) RD,RDLOW,IR
        IF (IR.GT.0) THEN
         IF (IR.GT.NRD) THEN
          WRITE(6,*) '>>> TOO MANY RECEIVERS IN FILE <<<'
          ISTAT=3
          RETURN
         END IF
         IF (IR.GT.1) THEN
          RDSTEP=(RDLOW-RD)/FLOAT(IR-1)
         ELSE
          RDSTEP=1.0
         ENDIF
         DO 105 L=1,IR
          RDC(L)=(L-1)*RDSTEP+RD
105      CONTINUE
        ELSE
         IR=ABS(IR)
         WRITE(16,*) (RDC(L),L=1,IR)
        END IF

c        WRITE(16,*) R0,RSPACE,NPLOTS
c        WRITE(16,*)NX,LXTRF,MXTRF,DT
c        WRITE(16,*) ICDRIN
        ICDR=ICDRIN
c        WRITE(16,*) OMEGIM
C *** READ HEADER EXTENSION
c        WRITE(16,*) MSUFT
c        WRITE(16,*) ISROW
c        WRITE(16,*) INTTYP
        DO 1300 I=1,2
c         WRITE(16,*) IDUMMY
 1300     CONTINUE
        DO 1400 I=1,5
c         WRITE(16,*) DUMMY
 1400     CONTINUE
 20     FORMAT(1X,A)

 600    DO 30 JJ=1,NOUT
 30     PCHOICE(JJ)=PARCHC(IPARM(JJ))
        RETURN

 995    ISTAT=1
        RETURN
        
        END


