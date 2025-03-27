      program trftoascii
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'compul.f'

      LOGICAL BINFILE
      COMMON /FILFRM/ BINFILE
      character*80 filein,fileout
      character*4 numb
c >>> input file names
      write(6,*) 'Input file?'
      read(5,'(A)') filein
      write(6,*) 'Output file?'
      read(5,'(A)') fileout
      do 5 i=80,1,-1
       if (fileout(i:i).ne.' ') then
        lns=i
        go to 6
       end if
 5    continue
 6    continue
C ***  OPEN TRF-FILE AND READ HEADING
      numb='0000'
      CALL rwHEAD(ISTAT,filein,fileout(1:lns)//numb)
c
c >>> close header file
      close(16,status='keep')
        DO 35 K=LXTRF,MXTRF
         write(numb,'(i4.4)') k
         open(16,file=fileout(1:lns)//numb,status='unknown',
     &        form='formatted')
        do 40 M=1,MSUFT
        DO 40 I=1,NPLOTS
          DO 50 J=1,IR
            READ(15) (ARG(KK),FAC(KK),KK=1,NOUT)
            write(16,*) (ARG(KK),FAC(KK),KK=1,NOUT)
  50       CONTINUE
          
 40      CONTINUE
          close(16)
 35      CONTINUE
        CLOSE(15)
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
        BINFILE=.TRUE.
        OPEN(15,FILE=FILEin,STATUS='OLD',FORM='UNFORMATTED',ERR=995)

        READ(15) FILEID
        IF (FILEID.NE.'PULSETRF') THEN
         ISTAT=2
         RETURN
        END IF
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
 
        OPEN(16,FILE=FILEout,STATUS='unknown',FORM='FORMATTED',ERR=995)
        WRITE(16,20) FILEID
        WRITE(16,20) PROGNM
        WRITE(16,*) NOUT
        WRITE(16,*) (IPARM(J),J=1,NOUT)
        WRITE(16,20) TITLE
        WRITE(16,20) SIGNN
        WRITE(16,*) FCTRF
        WRITE(16,*) SD
        WRITE(16,*) RD,RDLOW,IR
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

        WRITE(16,*) R0,RSPACE,NPLOTS
        WRITE(16,*)NX,LXTRF,MXTRF,DT
        WRITE(16,*) ICDRIN
        ICDR=ICDRIN
        WRITE(16,*) OMEGIM
C *** READ HEADER EXTENSION
        WRITE(16,*) MSUFT
        WRITE(16,*) ISROW
        WRITE(16,*) INTTYP
        DO 1300 I=1,2
         WRITE(16,*) IDUMMY
 1300     CONTINUE
        DO 1400 I=1,5
         WRITE(16,*) DUMMY
 1400     CONTINUE
 20     FORMAT(1X,A)

 600    DO 30 JJ=1,NOUT
 30     PCHOICE(JJ)=PARCHC(IPARM(JJ))
        RETURN

 995    ISTAT=1
        RETURN
        
        END
