      program tetime
      real secs(2)
      do i=1,1000
       tt=etime(secs)
       if (mod(i,100).eq.0) then
        write(6,*) i,tt,secs(1),secs(2)
       end if
      end do
      end
