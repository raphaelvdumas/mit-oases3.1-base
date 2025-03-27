      program addcov
c >>> program to normalize covariance matrix files of xsm format
c     outpot written to .cov file
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
      write(6,*) 'Input xsm-file ? (no extension)'
      read(5,'(a)') infil1
      lin=lenstr(infil1)
      outfil=infil1
      lout=lin
      call system('cp '//infil1(1:lin)//'.xsm fort.15')
c >>> read header to initializa parameters
      call GETXSM (buffer,NRCV,-1,LERR)
      if (nfreq*nrcv*nrcv.gt.np3) stop 'Too many freq or rec'
      write(6,*) 'nrcv =',nrcv
      write(6,*) 'nfreq=',nfreq
      open(24,file=outfil(1:lout)//'.cov',form='formatted',
     &        status='unknown')
      do ifr=1,nfreq
       iof=(ifr-1)*nrcv*nrcv
       call getxsm(corrfd,nrcv,ifr,lerr)
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
      call system('rm fort.15')
      end
      
