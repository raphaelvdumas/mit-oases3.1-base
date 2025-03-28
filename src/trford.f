      Program trf_conc
c
c     Concatenates   single freq trf files to a full trf file
c
c     Based on trf2bdr.f, command line argument
c     feature is added and several bugs are fixed.
c     09/10/97 by Austin J. Lee
c
      include 'compar.f'
      include 'comnp.f'
      include 'compul.f'
      character*80 filall,filsin,filbas,inarg,command,filgz,form
      logical newfile
      LOGICAL BINFILE,saclant
      COMMON /FILFRM/ BINFILE,saclant
c
      debug=.true.
      binfile=.true.
c
c >>> Usage trfapp trf-single-f trf-all
        iii = iargc()
        if (iii.eq.3) then
           call argcnv(3,filall,jja,'trf')
           call getarg(2,filbas)
           call getarg(1,inarg)
           read(inarg,*) NF
           if (NF.lt.0) then
            write(6,*) '>>> NF < 0'
            stop
           end if
        else
           write(6,*) 'usage: trford <NF>  <trf-bas> <trf-all>'
           stop
        end if

      write(6,*) 'Single order trf base file name:',filbas
      write(6,*) 'Total trf file:           ',filall

C ***  OPEN 1st TRF-FILE AND READ HEADer
      lenbas=lenstr(filbas)
      do ii=1,4
       write(form,'(a,i1,a,i1,a)') '(a,a,i',ii,'.',ii,',a)'
       write(filename,form) filbas(1:lenbas),'-',1,'.trf'
       write(6,*) filename
       filgz=filename(1:lenstr(filename))//'.gz'
       call system('gunzip '//filgz)
       CALL READHEAD(ISTAT)
       call system('gzip '//filename)
       if (istat.eq.0) go to 111
      end do
      stop 'ERROR reading compressed trf files'
 111  continue


       msave=nf

c >>> save file parameters
       nxsave=nx
       npsave=nplots
       irsave=ir
       nsave=nout
       dtsave=dt
       fsave=freq
       lxall=lxtrf
       mxall=mxtrf

       msuft=msave

      write(6,*)
      write(6,*) '>>> Target TRF file header write'
      write(6,*) '    TITLE =',title(1:54)
      write(6,*) '    FC =   ',fcall
      write(6,*) '    IR =   ', ir
      write(6,*) '    NX =   ', nxall
      write(6,*) '    LX =   ', lxall
      write(6,*) '    MX =   ', mxall
      write(6,*) '    DT = ', dtall
      write(6,*) '    NPLOT =', nplots
      write(6,*) '    Nout  =', nout
      write(6,*) '    MSUFT =', msuft
      write(6,*) '    SIGN  =', signn

        call trfsumhd(filall,title,rd,rdlow,r0,rspace,
     &                nxall,lxall,mxall,dtall,fcall,sd,iparm)
c >>> open buffer file
       open(30,status='scratch',form='unformatted')

       do ifr=lxall,mxall
        rewind(30)
        DO K=1,nf
         write(filename,form) filbas(1:lenbas),'-',k,'.trf'
         write(6,*) filename
         filgz=filename(1:lenstr(filename))//'.gz'
         if (ifr.eq.lxall) call system('gunzip '//filgz)
         CALL READHEAD(ISTAT)
         if (istat.ne.0) then
          write(6,*) 'Error reading file',filename
          stop
         end if

c >>>  Check for file consistency:
         if  (    (lxtrf.ne.lxall)
     &     .or.(mxtrf.ne.mxall)
     &     .or.(npsave.ne.nplots)
     &     .or.(irsave.ne.ir)
     &     .or.(nsave.ne.nout) 
     &     .or.(msuft.ne.1)  ) then
           close(91,status='delete')
          write(6,*) 'Inconsistent TRF files'
          stop
         else 
          write(6,*) 'freq,m=', ifr,k
         end if

         do jfr=lxtrf,ifr
          do I=1,NPLOTS
           do j J=1,IR
            if (jfr.eq.ifr) then
             read(15)  (ARG(KK),FAC(KK),KK=1,NOUT)
             write(30) (ARG(KK),FAC(KK),KK=1,NOUT)
            else
             read(15)  (ARG(KK),FAC(KK),KK=1,NOUT)   
            end if
           end do
          end do
         end do
         close(15)
         if (irf.eq.mxall) call system('gzip '//filename)
        end do
        rewind(30)
        do k=1,nf
          do I=1,NPLOTS
           do j J=1,IR
             read(30)  (ARG(KK),FAC(KK),KK=1,NOUT)
             write(16) (ARG(KK),FAC(KK),KK=1,NOUT)
           end do
          end do
         end do
       end do
       close(16)

       END

      SUBROUTINE TRFsumhd(filnam,TITLE,RD,RDLOW,R0,RSPACE,
     &                    NX,LX,MX,DT,freqs,SD,iparm)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'

      CHARACTER*(*) filnam
      CHARACTER*8 FILEID
      character*16 bufch
      CHARACTER*80 TITLE,filenm
      CHARACTER SIGNN
      INTEGER IPARM(12)
      LOGICAL BINFILE,saclant
      COMMON /FILFRM/ BINFILE,saclant
        luttrf=16
        OPEN(LUTTRF,FILE=filnam,
     &          STATUS='UNKNOWN',FORM='UNFORMATTED')
cunix        OPEN(LUTTRF,FILE=FILEnm(1:ii)//trfext,
cunix     &       STATUS='UNKNOWN',FORM='UNFORMATTED')
        FILEID='PULSETRF'
        WRITE(LUTTRF) FILEID
        WRITE(LUTTRF) PROGNM
        WRITE(LUTTRF) NOUT
        WRITE(LUTTRF) (IPARM(J),J=1,NOUT)
        WRITE(LUTTRF) TITLE
        SIGNN='+'
        WRITE(LUTTRF) SIGNN
        WRITE(LUTTRF) FREQS
        WRITE(LUTTRF) SD
        WRITE(LUTTRF) RD,RDLOW,IR
        IF (IR.LT.0) THEN
         WRITE(LUTTRF) (RDC(L),L=1,ABS(IR))
        END IF
        WRITE(LUTTRF) R0,RSPACE,NPLOTS

        WRITE(LUTTRF) NX,LX,MX,DT
        WRITE(LUTTRF) ICDR
        WRITE(LUTTRF) OMEGIM
C ***  EXTRA FIELDS ADDED 891211 HS
        WRITE(LUTTRF) MSUFT
        write(6,*) 'trfhead: msuft=',msuft
        WRITE(LUTTRF) ISROW
        write(LUTTRF) inttyp
        DO 300 I=1,2
         WRITE(LUTTRF) IDUMMY
 300    CONTINUE
        DO 400 I=1,5
         WRITE(LUTTRF) DUMMY
 400    CONTINUE
        RETURN
        
        END

      SUBROUTINE READHEAD(ISTAT)
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'compul.f'
      LOGICAL BINFILE,saclant
      COMMON /FILFRM/ BINFILE,saclant
      character*2 tspar(3),cdum
      CHARACTER*8 FILEID
      CHARACTER PARCHC(12)
      DATA PARCHC /'N','V','H','T','R','K','X','Y',1*' ','M','D','F'/
        ISTAT=0

        BINFILE=.FALSE.
        OPEN(15,FILE=FILENAME,STATUS='OLD',FORM='FORMATTED',ERR=500)
        READ(15,20,ERR=500) FILEID
c >>> non-saclant format
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
         DO 105 L=1,IR
          RDC(L)=(L-1)*RDSTEP+RD
105      CONTINUE
        ELSE
         IR=ABS(IR)
         READ(15,*) (RDC(L),L=1,IR)
         rd=rdc(1)
         rdlow=rdc(ir)
        END IF

        READ(15,*) R0,RSPACE,NPLOTS
        READ(15,*)NX,LXTRF,MXTRF,DT
C
C     CHECK THAT NX IS INTEGER POWER OF 2
C
        IPOW=0
 991    IPOW=IPOW+1
        II=2**IPOW
        IF (II.LT.NX) GO TO 991
        IF (II.NE.NX) THEN
         WRITE(6,*)
         WRITE(6,*) '>>>> NT MUST BE INTEGER POWER OF 2, IS',
     &             NX,' <<<<'      
         close(15,status='keep')
         istat=4
         return
        END IF

        READ(15,*) ICDRIN
        ICDR=ICDRIN
        READ(15,*) OMEGIM
C *** READ HEADER EXTENSION
        READ(15,*) MSUFT
        READ(15,*) ISROW
        READ(15,*) INTTYP
        DO 1300 I=1,2
         READ(15,*) IDUMMY
 1300     CONTINUE
        DO 1400 I=1,5
         READ(15,*) DUMMY
 1400     CONTINUE
 20     FORMAT(1X,A)
        go to 600
  500   CLOSE(15,STATUS='KEEP',ERR=501)
 501    BINFILE=.TRUE.
        OPEN(15,FILE=FILENAME,STATUS='OLD',FORM='UNFORMATTED',ERR=995)

        READ(15,ERR=700) FILEID
        IF (FILEID.NE.'PULSETRF') THEN
         ISTAT=2
         WRITE(6,*) 'File format incompatible'
         return
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
         IR=ABS(IR)
         READ(15) (RDC(L),L=1,IR)
        END IF

        READ(15) R0,RSPACE,NPLOTS
        READ(15)NX,LXTRF,MXTRF,DT
C
C     CHECK THAT NX IS INTEGER POWER OF 2
C
        IPOW=0
 992    IPOW=IPOW+1
        II=2**IPOW
        IF (II.LT.NX) GO TO 992
        IF (II.NE.NX) THEN
         WRITE(6,*)
         WRITE(6,*) '>>>> NT MUST BE INTEGER POWER OF 2, IS',
     &             NX,' <<<<'      
         close(15,status='keep')
         istat=4
         return
        END IF

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
        go to 600
c
c >>> the following is for SACLANT TRF format
c

 700    rewind(15)
        READ(15,ERR=995) FILEID(1:6)
        if (fileid(1:5).eq.'PAREQ') then
         prognm=fileid(1:5)
         saclant=.true.
         binfile=.true.
         write(6,*) 'SACLANT TRF format'
        else
         ISTAT=2
         WRITE(6,*) 'File format incompatible'
         return
        end if

        READ(15) (tspar(J),J=1,3)
c
c >>> find out how many parameters
        nout=0
        do j=1,3
         if (tspar(j).eq.' 1') then
          iparm(j)=j
          nout=nout+1
         end if
        end do
        READ(15) TITLE
        READ(15) cdum
        if (cdum(1:1).ne.' ') then
         SIGNN=cdum(1:1)
        else
         SIGNN=cdum(2:2)
        end if    
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
         DO L=1,IR
          RDC(L)=(L-1)*RDSTEP+RD
         end do
        ELSE
         IR=ABS(IR)
         READ(15) (RDC(L),L=1,IR)
        END IF
        READ(15) NX,LXTRF,MXTRF,DT,R0,RSPACE,NPLOTS
c >>> set parameters used by new versions but not in SACLANT version
        omegim=0e0
        msuft=1
        isrow=1
C
C     CHECK THAT NX IS INTEGER POWER OF 2
C
        IPOW=0
 993    IPOW=IPOW+1
        II=2**IPOW
        IF (II.LT.NX) GO TO 993
        IF (II.NE.NX) THEN
         WRITE(6,*)
         WRITE(6,*) '>>>> NT MUST BE INTEGER POWER OF 2, IS',
     &             NX,' <<<<'      
         close(15,status='keep')
         istat=4
         return
        END IF

        READ(15) ICDRIN
        ICDR=ICDRIN

        write(6,*) 'omegim=',omegim
        write(6,*) 'IR=    ',ir
        write(6,*) 'nplot= ',nplots
        write(6,*) 'nout=  ',nout
        do j=1,3
         write(6,'(1x,a4,i1,a2,a2)') 'Par:',j,' =',tspar(j)
        end do
        write(6,'(1x,a5,a1)') 'sign=',signn
 600    DO 30 JJ=1,NOUT
 30     PCHOICE(JJ)=PARCHC(IPARM(JJ))
        RETURN

 995    ISTAT=1
        RETURN
        
        END




      integer function lenstr(filnam)
      character*(*) filnam
      do ii=len(filnam),1,-1
       if (filnam(ii:ii).ne.' '.and.ichar(filnam(ii:ii)).ne.0) then
        lenstr=ii
        return
       end if
      end do
      lenstr=ii
      return
      end

