      SUBROUTINE ADD(DEL,VUG,NNP,DEVICE,FAX,BWCOL)
      LOGICAL BATCH
      CHARACTER*3 DEVICE, VUG, DEL, FAX,BWCOL
      CHARACTER*8 HHMMSS
      CHARACTER*80 FILENM,FILEWK,COMFILE,TBUF
      CHARACTER*80 UNIINF,UNIWRK,UNIRST
      COMMON /FLSUNI/ UNIINF,UNIWRK,UNIRST
      common /unplct/ nrofpl
      CHARACTER*1 YN
      INTEGER TTYPE,GTXTTY,getpid
      CHARACTER*80 UNIPID
      character*12 psname
 101  format('eps.',A1,I5.5)
 102  format('ps.',A1,I5.5)
      if (device.eq.'LAS') then
       if (BWCOL.eq.'B/W') then
        write(psname,102) char(65+nrofpl),getpid()
        call system('mv POST '//psname)
        call system('lpr '//psname)
        call system('rm '//psname)
        nrofpl=nrofpl+1
c        write(6,*)
c        write(6,'(1H ,a,a)') 'Saved in Postscript file: ',psname
       else
c        call system('lpr -Plw4 POST')
        write(psname,102) char(65+nrofpl),getpid()
        call system('mv POST '//psname)
        nrofpl=nrofpl+1
        write(6,*)
        write(6,'(1H ,a,a)') 'Saved in Postscript file: ',psname
       end if
      else if (device.eq.'EPS') then
        write(psname,101) char(65+nrofpl),getpid()
        call system('mv POST '//psname)
        nrofpl=nrofpl+1
        write(6,*)
        write(6,'(1H ,a,a)') 'Saved in Postscript file: ',psname
      else if (device.eq.'PSL'.or.device.eq.'PSP') then
        write(psname,102) char(65+nrofpl),getpid()
        call system('mv POST '//psname)
        nrofpl=nrofpl+1
        write(6,*)
        write(6,'(1H ,a,a)') 'Saved in Postscript file: ',psname
      end if
      RETURN
      END

