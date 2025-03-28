      program addcov
c >>> program to add two covariance matrix files of xsm format
      include 'compar.f'
      include 'comnp.f'
      include 'corrfn.f'
      include 'corrfs.f'
      integer srtput,srtget
      COMMON /GETPUT/ SRTPUT, SRTGET
      complex buffer(np3)
      real conorm(nrnr)
      equivalence (conorm(1),cffs(1))
      equivalence (buffer(1),cff(1,1))
      character*20 infil1,infil2,outfil
      write(6,*) 'Input xsm-file no.1 ? (no extension)'
      read(5,'(a)') infil1
      write(6,*) 'Gain to be applied (dB)?'
      read(5,*) gain1
      gain1=10.0**(gain1/10.0)
      lin=lenstr(infil1)
      call system('cp '//infil1(1:lin)//'.xsm fort.15')
c >>> read header to initializa parameters
      call GETXSM (buffer,NRCV,-1,LERR)
      if (nfreq*nrcv*nrcv.gt.np3) stop 'Too many freq or rec'
      write(6,*) 'nrcv =',nrcv
      write(6,*) 'nfreq=',nfreq
      do ifr=1,nfreq
       iof=1+(ifr-1)*nrcv*nrcv
       call getxsm(buffer(iof),nrcv,ifr,lerr)
      end do
      close(15)
      srtget=0
c >>> second input file
      write(6,*) 'Input xsm-file no.2 ? (no extension)'
      read(5,'(a)') infil2
      write(6,*) 'Gain to be applied (dB)?'
      read(5,*) gain2
      gain2=10.0**(gain2/10.0)
      lin=lenstr(infil2)
      call system('cp '//infil2(1:lin)//'.xsm fort.15')
c >>> output files
      write(6,*) 'Output xsm-file ? (no extension)'
      read(5,'(a)') outfil
      lout=lenstr(outfil)
      open(24,file=outfil(1:lout)//'.cov',form='formatted',
     &        status='unknown')
      do ifr=1,nfreq
       iof=(ifr-1)*nrcv*nrcv
       call getxsm(corrfd,nrcv,ifr,lerr)
       do i=1,nrcv*nrcv
        corrfd(i)=corrfd(i)*gain2+buffer(iof+i)*gain1
       end do
       call putxsm(corrfd,nrcv,ifr,lerr)
C *** Normalized covariance
       do jrec=1,nrcv
        write(24,'(1H ,a,i6)') 'receiver:',jrec
        indexj=jrec+(jrec-1)*nrcv
        do irec=1,nrcv
         index=irec+(jrec-1)*nrcv
         indexi=irec+(irec-1)*nrcv
         conorm(index)= real(corrfd(index))
     &         /sqrt(real(corrfd(indexi))*real(corrfd(indexj)))
        end do
        index=1+(jrec-1)*nrcv
        i2=index+nrcv-1
        write(24,'(1H ,6f12.6)') (conorm(ii),ii=index,i2)
       end do
      end do
      close(15)
      close(16)
      call system('mv fort.16 '//outfil(1:lout)//'.xsm')
      call system('rm fort.15')
      end
      
