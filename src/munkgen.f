      program envgen
c
c     Generates RDOAST environment files for sloping environments with 
c     a Munk profile in the water column
c
      character*80 filenm
      real v(8),munk
      do j=1,8
       v(j)=0e0
      end do
      write(6,*) 'Initial depth (m)?'
      read(5,*) d1
      write(6,*) 'Final depth (m)?'
      read(5,*) d2
      write(6,*) 'Max depth (m)?'
      read(5,*) dm
      
      write(6,*) 'Sector length (km)?'
      read(5,*) r12
      write(6,*) 'Munk profile parameters. Za,Ca,w?'
      read(5,*) za,ca,w
      write(6,*) 'Lower medium, Cp,Cs,Ap,As,Rho?'
      read(5,*) cp2,cs2,ap2,as2,rho2
      write(6,*) 'Number of sectors?'
      read(5,*) ns
      write(6,*) 'Number of layers?'
      read(5,*) nl
      write(6,*) 'Filename?'
      read(5,'(a)') filenm
      open(10,file=filenm,status='unknown',form='formatted')
      nsn=ns-1
      write(10,*) ns,ns-1
      sl=r12/nsn
      dd=(d2-d1)/nsn
      dl=dm/(nl-1)
      nlt=nl+2
      do i=1,ns
       if (i.eq.1.or.i.eq.ns) then
         slen=sl*0.5
       else
         slen=sl
       end if
       d = d1+(i-1)*dd
       n1=int(d/dl)+1
       n2=nl-n1
       do j=1,8
        v(j)=0e0
       end do
       write(10,*) nlt, slen
       write(10,'(1x,f8.3,2f10.2,3f7.3,2f5.1)') (v(j),j=1,8)
       v(4)=0e0
       v(5)=0e0
       v(6)=1e0
       do l=1,n1
        v(1)=(l-1)*dl
        v(2)=munk(v(1),za,ca,w)
        if (l.eq.n1) then
         v(3)=-munk(d,za,ca,w)
        else
         v(3)=-munk(l*dl,za,ca,w)
        end if
        write(6,*) l,v(2),v(3)
        write(10,'(1x,f8.3,2f10.2,3f7.3,2f5.1)') (v(j),j=1,8)
       end do
       v(1)=d
       v(2)=cp2
       v(3)=cs2
       v(4)=ap2
       v(5)=as2
       v(6)=rho2
       write(10,'(1x,f8.3,2f10.2,3f7.3,2f5.1)') (v(j),j=1,8) 
       do l=1,n2
        v(1)=(l-1+n1)*dl
        write(10,'(1x,f8.3,2f10.2,3f7.3,2f5.1)') (v(j),j=1,8)
       end do
      end do
      end
      real function munk(z,za,ca,w)
      parameter (eps=0.00737)
      zb=2*(z-w)/w
      munk = ca*(1E0+eps*(zb-1E0+exp(-zb)))
      return
      end

      
