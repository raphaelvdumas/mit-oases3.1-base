      SUBROUTINE READTRF()
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'compul.f'

      LOGICAL BINFILE,saclant
      COMMON /FILFRM/ BINFILE,saclant

      COMPLEX CARR(NP3)
      EQUIVALENCE (CARR(1),CFF(1,1))

C *** STATEMENT FUNCTION FOR INDEXING CARR
      INDEXC(IND1,IND2,IND3,IND4)=IND1
     &       +IR*((IND2-1)+NPLOTS*((IND3-1)+NOUT*(IND4-1)))

C ***  OPEN TRF-FILE AND READ HEADING

      CALL READHEAD(ISTAT)

C ***  CHECK SIZE OF INPUT BUFFERS

        NELM=NP3
        IF ((IR*NPLOTS*NOUT*MSUFT).GT.NELM) THEN
         WRITE(6,*) '>>> READTRF: ARRAY CFF TOO SMALL <<<'
         CLOSE(15,STATUS='KEEP')
         PAUSE
         RETURN
        END IF
        NELM=ISIZE
        IF ((NPLOTS*NOUT*MSUFT).GT.NELM) THEN
         WRITE(6,*) '>>> READFMT: ARRAY CFILE TOO SMALL <<<'
         CLOSE(15,STATUS='KEEP')
         PAUSE
         RETURN
        END IF

        DLFRQP=1.0/REAL(DT*NX)
        DOMEGA=8.0*ATAN(1.0)*DLFRQP
        OPTION(1)=PROGNM
        LX=nint(FMIN/DLFRQP+1)
        LX=MAX(LX,LXTRF)
        MX=NINT(FMAX/DLFRQP+1)
        MX=MIN(MX,MXTRF)
        NUMFR=MX-LX+1
*     Open asynchronous scratch file
        NBLOCKS=0.8*MEMLFT()
        CALL OPNBUF(31,2*NOUT*NPLOTS*MSUFT,IR*NUMFR,NBLOCKS)

        if (saclant) then
c >>> SACLANT TRF format
         nf_fil=mxtrf-lxtrf+1
         if ((nf_fil*nplots*ir*nout).gt.np3) then
           write(6,*) '>>> READ_SACLANT: Too many values <<<'
           close(15,status='keep')
           pause
           return
         end if
         do l=1,nout
          do j=1,ir
           do i=1,nplots
            read(15) (carr(indexc(j,i,l,k)),k=1,nf_fil)
           end do
          end do
         end do
         do k=lxtrf,mxtrf
          ik=k-lxtrf+1
          if (k.ge.lx.and.k.le.mx) then
           do j=1,ir
            do l=1,nout
             do i=1,nplots
              IF (SIGNN.EQ.'-') THEN
               CFILE(I+(L-1)*NPLOTS)=
     &                        CARR(INDEXc(J,I,L,ik))
              ELSE
               CFILE(I+(L-1)*NPLOTS)=
     &                  conjg(CARR(INDEXc(J,I,L,ik)))
              END IF
             end do
            end do
            CALL WRBUF(31,CFILE,2*NOUT*NPLOTS)
           end do
          end if
         end do
        else
c >>>  New TRF format
         DO K=LXTRF,MXTRF
          do is=1,isrow
           do M=1,MSUFT
            DO I=1,NPLOTS
             DO J=1,IR
              IF (BINFILE) THEN
               READ(15) (ARG(KK),FAC(KK),KK=1,NOUT)
              ELSE 
               READ(15,*) (ARG(KK),FAC(KK),KK=1,NOUT)
              END IF
              if (is.eq.isact) then
               do KK=1,NOUT
                INDX1=INDEXC(J,I,KK,M)
                CARR(INDX1)=CMPLX(ARG(KK),FAC(KK))
               end do
              end if
             end do
            end do
           end do
          end do
          if (K.ge.LX.and.K.le.MX) then
           DO J=1,IR
            DO L=1,NOUT
             DO I=1,NPLOTS
              DO M=1,MSUFT
               IF (SIGNN.EQ.'-') THEN
                CFILE(I+((L-1)+NOUT*(M-1))*NPLOTS)=
     &                 CONJG(CARR(INDEXC(J,I,L,M)))
               ELSE
                CFILE(I+((L-1)+NOUT*(M-1))*NPLOTS)=
     &                       CARR(INDEXC(J,I,L,M))
               END IF
              end do
             end do
            end do
            CALL WRBUF(31,CFILE,2*NOUT*NPLOTS*MSUFT)
           end do
          end if
         end do
        end if
        CLOSE(15)
        CALL ENFBUF(31)

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



      SUBROUTINE ADDTRF()
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      INCLUDE 'compul.f'
      character*48  addfil1,addfil2,sumfil,svfile
      character*80 newtit
      LOGICAL BINFILE,saclant,fscal
      COMMON /FILFRM/ BINFILE,saclant
      write(6,*) 'TRF file 1:'
      read(5,'(a)') addfil1
      write(6,*) 'Multiplyer:'
      read(5,*) fmul1

      write(6,*) 'TRF file 2:'
      read(5,'(a)') addfil2
      write(6,*) 'Multiplyer:'
      read(5,*) fmul2

      write(6,*) 'Sum TRF file:'
      read(5,'(a)') sumfil

      write(6,*) 'Title for output file:'
      read(5,'(a)') newtit
      
      write(6,*) 'Time shift:'
      read(5,*) tshift
      write(6,*) 'Time shift:',tshift
      
      svfile=filename
C ***  OPEN 1st TRF-FILE AND READ HEADer
      filename=addfil1
      CALL READHEAD(ISTAT)
      if (istat.ne.0) then
       filename=svfile
       write(6,*) 'File 1 read error'
       return
      end if

C ***  CHECK SIZE OF INPUT BUFFERS

        NELM=NP3
        IF ((IR*NPLOTS*NOUT*MSUFT).GT.NELM) THEN
         WRITE(6,*) '>>> READTRF: ARRAY CFF TOO SMALL <<<'
         CLOSE(15,STATUS='KEEP')
         PAUSE
         RETURN
        END IF
        NELM=ISIZE
        IF ((NPLOTS*NOUT*MSUFT).GT.NELM) THEN
         WRITE(6,*) '>>> READFMT: ARRAY CFILE TOO SMALL <<<'
         CLOSE(15,STATUS='KEEP')
         PAUSE
         RETURN
        END IF

        DLFRQP=1.0/REAL(DT*NX)
        DOMEGA=8.0*ATAN(1.0)*DLFRQP
        OPTION(1)=PROGNM
        LX=nint(FMIN/DLFRQP+1)
        LX=MAX(LX,LXTRF)
        MX=NINT(FMAX/DLFRQP+1)
        MX=MIN(MX,MXTRF)
        NUMFR=MX-LX+1
        lx1=lxtrf
        mx1=mxtrf
        nx1=nx
        dt1=dt
        om1=omegim
c >>> open temporary file
        open(91,status='scratch',form='unformatted')
        lll=mxtrf-lxtrf+1
        do K=LXTRF,MXTRF
         do is=1,isrow
          do M=1,MSUFT
           do I=1,NPLOTS
            do J=1,IR
             IF (BINFILE) THEN
              READ(15) (ARG(KK),FAC(KK),KK=1,NOUT)
             ELSE 
              READ(15,*) (ARG(KK),FAC(KK),KK=1,NOUT)
             END IF
             if (signn.eq.'+') then
              write(91)  (ARG(KK),FAC(KK),KK=1,NOUT)
             else
              write(91)  (ARG(KK),-FAC(KK),KK=1,NOUT)
             end if
            end do
           end do
          end do
         end do
        end do
        CLOSE(15)
        rewind(91)
c >>> save file parameters
        nxsave=nx
        dtsave=dt
        dfsave=1e0/(nx*dt)
        msave=msuft
        msuft1=msuft
        npsave=nplots
        irsave=ir
        nsave=nout

C ***  OPEN 2nd TRF-FILE AND READ HEADer
      filename=addfil2
      CALL READHEAD(ISTAT)
      if (istat.ne.0) then
       filename=svfile
       oldfile=' '
       write(6,*) 'File 2 read error'
       close(91,status='delete')
       return
      end if
      msuft2=msuft
      lx2=lxtrf
      mx2=mxtrf
      dt2=dt
      nx2=nx
      om2=omegim
      df=1.0/(nx*dt)
      domega=df*2*pi
c >>> Check for file consistency:
      if  ( (abs(dfsave-df)/df.gt.1e-3)
     &     .or.(npsave.ne.nplots)
     &     .or.(irsave.ne.ir)
     &     .or.(nsave.ne.nout)    ) then
        close(91,status='delete')
        write(6,*) 'nx1,nx2=',nxsave,nx
        write(6,*) 'dt1,dt2=',dtsave,dt
        write(6,*) 'Inconsistent TRF files'
        filename=svfile
        oldfile=' '
        return
       end if
c >>> Make imaginary frequency consistent
      if (abs(om1).ge.abs(om2)) then
       omm=om1
      else
       omm=om2
      end if
      if (abs(omm).gt.0e0) then
       if (abs((om1-om2)/omm).gt.1e-3) then
        fscal=.true.
        write(6,*) 'Inconsistent imaginary frequency.'
        write(6,*) 'File 2 will be exponentiall scaled'
        write(6,*) 'om1,om2=',om1,om2
       else
        fscal=.false.
       end if
      else
       fscal=.false.
      end if
      if (fscal) then
       ntval=(nx+2)*isrow*msuft*nplots*ir
       if (ntval.gt.np) then
        write(6,*) '>>> TRFADD: trf files too large <<<'
        write(6,*) 'ntval,np=',ntval,np
        filename=svfile
        oldfile=' '
        return
       end if        
       if (signn.eq.'+') then 
        iftd=1
       else
        iftd=-1
       end if
c >>> open temporary file
       open(92,status='scratch',form='unformatted')
       lll=mxtrf-lxtrf+1
       nxh=nx/2
       nxhp1=nxh+1
       do kk=1,nout
        call vclr(cff(1,kk),1,ntval)
       end do
       do K=LXTRF,MXTRF
        do is=1,isrow
         do M=1,MSUFT
          do I=1,NPLOTS
           do J=1,IR
            IF (BINFILE) THEN
             READ(15) (ARG(KK),FAC(KK),KK=1,NOUT)
            ELSE 
             READ(15,*) (ARG(KK),FAC(KK),KK=1,NOUT)
            END IF
            index=k+nxhp1*((is-1)+isrow*((m-1)+msuft*
     &          ((i-1)+nplots*(j-1))))
            do kk=1,nout
              cff(index,kk)=cmplx(arg(kk),iftd*fac(kk))
            end do
           end do
          end do
         end do
        end do
       end do
c >>> Transform to time domain, apply exponential, and back to frequency
       do ii=1,nxh
        arg(ii)=(ii-1)*domega
       end do
       CALL VMUL(ARG,1,TSHIFT,0,ARG,1,nxh)
       CALL CVEXP(ARG,1,CBUF,2,nxh)
       write(6,*) 'dt,2*pi/(nx*domega)=',dt,2*pi/(nx*domega)
       do ii=1,nx
        arg(ii)=exp((-abs(om1)-(-abs(om2)))*(tshift+(ii-1)*dt))
       end do
       do is=1,isrow
        do M=1,MSUFT
         do I=1,NPLOTS
          do J=1,IR
           do kk=1,nout
            index=1+nxhp1*((is-1)+isrow*((m-1)+msuft*
     &          ((i-1)+nplots*(j-1))))
            CALL CVMUL(CBUF,2,CFF(index,kk),2,CFF(index,kk),2,nxh,1)
            call rfft(cff(index,kk),nx,-1)
            call vmul(cff(index,kk),1,arg,1,cff(index,kk),1,nx)
            call rfft(cff(index,kk),nx,1)
            call rfftsc(cff(index,kk),nx,1,1)
            CALL CVMUL(CBUF,2,CFF(index,kk),2,CFF(index,kk),2,nxh,-1)
           end do
          end do
         end do
        end do
       end do
c Now write to temporary file 
c Note full spectrum
       lx2=2
       mx2=nxh
       do K=lx2,mx2
        do is=1,isrow
         do M=1,MSUFT
          do I=1,NPLOTS
           do J=1,IR
            index=k+nxhp1*((is-1)+isrow*((m-1)+msuft*
     &          ((i-1)+nplots*(j-1))))
            write(92)  ( real(cff(index,KK)),
     &                   aimag(cff(index,KK)),KK=1,NOUT)
           end do
          end do
         end do
        end do
       end do
       CLOSE(15)
       rewind(92)
      else
c >>> here the old stuff starts
c >>> open temporary file
       open(92,status='scratch',form='unformatted')
       lll=mxtrf-lxtrf+1
       do K=LXTRF,MXTRF
        do is=1,isrow
         do M=1,MSUFT
          do I=1,NPLOTS
           do J=1,IR
            IF (BINFILE) THEN
             READ(15) (ARG(KK),FAC(KK),KK=1,NOUT)
            ELSE 
             READ(15,*) (ARG(KK),FAC(KK),KK=1,NOUT)
            END IF
            if (signn.eq.'+') then
             write(92)  (ARG(KK),FAC(KK),KK=1,NOUT)
            else
             write(92)  (ARG(KK),-FAC(KK),KK=1,NOUT)
            end if
           end do
          end do
         end do
        end do
       end do
       CLOSE(15)
       rewind(92)
      end if
c >>> Now make sum trf file
        binfile=.true.
c >>> Fourier coefficients of largest file
        msuft=max(msuft1,msuft2)
        lxtrf=min(lx1,lx2)
        mxtrf=max(mx1,mx2)
        dt=min(dt1,dt2)
        nx=max(nx1,nx2)
        signn='+'
        omegim=-abs(om1)
        call trfsumhd(sumfil,newtit,rd,rdlow,r0,rspace,
     &                nx,lxtrf,mxtrf,dt,fctrf,sd,iparm)
        numval=lll*msuft*nplots*ir
        lxtrf=min(lx1,lx2)
        mxtrf=max(mx1,mx2)
        do K=LXTRF,MXTRF
         do is=1,isrow
          do M=1,MSUFT
           do I=1,NPLOTS
            do J=1,IR
             if (msuft.le.msuft1.and.k.ge.lx1.and.k.le.mx1) then
              read(91)  (ARG(KK),FAC(KK),KK=1,NOUT)
             else
              call vclr(arg,1,nout)
              call vclr(fac,1,nout)
             end if
             if (msuft.le.msuft2.and.k.ge.lx2.and.k.le.mx2) then
              read(92)  (ARG(KK+nout),FAC(KK+nout),KK=1,NOUT)
             else
              call vclr(arg(1+nout),1,nout)
              call vclr(fac(1+nout),1,nout)
             end if
             do ii=1,nout
              arg(ii)=fmul1*arg(ii) + fmul2*arg(ii+nout)
              fac(ii)=fmul1*fac(ii) + fmul2*fac(ii+nout)
             end do
             write(15) (ARG(KK),FAC(KK),KK=1,NOUT)
            end do
           end do
          end do
         end do
        end do

        close(15)
        close(91)
        close(92)
        filename=sumfil
        RETURN
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
        luttrf=15
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


