      program pltsplit
      parameter (ntype=50)

      real    argnum
      logical argopt,icheck,ifile,ibbase,iparm,irxs,idirect,ihelp

      logical      blanks
      character*4  ddir(3),rext,fty
      character*5  pext
      character*60 fnm(ntype),sf
      character*40 bfile
      character*79 dummy
      character*40  base,ext2
      integer ll,nline,iperfile,ibase,npar,ir,nplt,nndir

      icount  = iargc()
      ddir(1) = '.for'
      ddir(2) = '.bac'
      ddir(3) = '.tot'
      ext2 = '.gem.plt'

      if (icount.ne.0.) then

c**   Command Line argument is very strict
c**   
c**   pltsplit -f filename -b ibase -p iparm -r ir -d nndir

         ihelp   = argopt('h')
         ifile   = argopt('f')
         ibbase  = argopt('b')
         iparm   = argopt('p')
         irxs    = argopt('r')
         idirect = argopt('d')
         icheck = ifile.and.ibbase.and.iparm.and.irxs.and.idirect
         if (.not.icheck.and..not.ihelp) stop 'Insufficient parameters'

         if (ihelp) then
      write (*,*) 'pltsplit -f fname -b ibase -p iparm -r ir -d nndir'
      write (*,*) 
      write (*,*) 'Order of input parameters is IMPORTANT'
      write (*,*) 'fname must not contain extensions'
      write (*,*) 'ibase = 1 -> CORE '
      write (*,*) '      = 2 -> RDOAST '
      write (*,*) '      = 3 -> OASES/SAFARI '
      write (*,*) '      = 4 -> BEM '
      write (*,*) 'iparm = Number of field parameters '
      write (*,*) 'ir    = Number of receivers'
      write (*,*) 'nndir = 0 -> BEM/C.O.R.E. backward field only'
      write (*,*) '      = 1 -> Forward field only'
      write (*,*) '      = 2 -> Forward and Backward field'
      write (*,*) '      = 3 -> Forward, Backward and Total field'
      stop
         else
            ibase = int(argnum(4))
            npar  = int(argnum(6))
            ir    = int(argnum(8))
            nndir = int(argnum(10))
            call getarg(2,base)
            if (ibase.eq.4) nndir = 1
         end if
      else
         print '(1X,A,$)', 'File to disect (w/o extension) : '
         read  (*,'(A15)') base
         write (*,*) 'Extension of base file '
         write (*,*) '[1] .gem.plt (default CORE)'
         write (*,*) '[2] .plt (RDOAST)'
         write (*,*) '[3] .plt (OASES/SAFARI)'
         write (*,*) '[4] .plt (BEM)'
         read  (*,*) ibase
         print '(1X,A,$)', 'Number of parameter : '
         read  (*,*) npar
         
         if (ibase.ne.3) then
            write (*,*) '*** Direction parameter ***'
            write (*,*) '[0] -> BEM/C.O.R.E. backward field only'
            write (*,*) '[1] -> Forward field only'
            write (*,*) '[2] -> Forward and Backward field'
            write (*,*) '[3] -> Forward, Backward and Total field'
            print '(1X,A,$)', 'Directions (0,1,2,3) ? : '
            read  (*,*) nndir
         else
c** SAFARI has only the forward direction
            nndir = 1
         end if
         print '(1X,A,$)', 'Number of receivers : '
         read  (*,*) ir

      end if

c**   NNDIR = 0 -> BEM/C.O.R.E. backward field only
c**   NNDIR = 1 -> Forward run only
c**   NNDIR = 2 -> Forward + Backward runs
c**   NNDIR = 3 -> Forward + Backward and Total field.

      if (ibase.eq.1) then

         write (*,*) 'Processing C.O.R.E file'
         fty  = '.gem'
         ext2 = '.gem.plt'

      else if (ibase.eq.2)  then

c**   Total field always included by default.

         if (nndir.eq.0) stop 'NNDIR=0 for C.O.R.E. ONLY!'
         write (*,*) 'Processing RDOAST type file!'
         fty  = '.rdo'
         ext2 = '.plt' 

      else if (ibase.eq.3) then

         if (nndir.eq.0) stop 'NNDIR=0 for C.O.R.E. ONLY!'
         write (*,*) 'Processing OASES type file!'
         fty  = '.saf'
         ext2 = '.plt' 

      else
         write (*,*) 'Processing BEM type file!'
         fty  = '.bem'
         ext2 = '.plt' 
      end if

c**   Set up a vector of filenames to store the split files

      ll    = lenstr(base)
      l4    = ll+4
      bfile = base(1:ll)//ext2
      sf = base(1:ll)//fty

      if (ibase.eq.1) then

c**   C.O.R.E. PLT structure

         if (nndir.eq.0) then

         do i=1,npar
            write (pext,'(A,I1)') '.par',i
            do ird=1,ir
               ifx = (i-1)*ir+ird
               write (rext,'(A,I1)') '.rx',ird
               fnm(ifx) = sf(1:l4)//pext//ddir(2)//rext//'.dat'
            end do
         end do

         else

         do i=1,npar
            write (pext,'(A,I1)') '.par',i
            do id=1,nndir
               do ird=1,ir
                  ifx = (i-1)*nndir*ir+(id-1)*ir+ird
                  write (rext,'(A,I1)') '.rx',ird
                  fnm(ifx) = sf(1:l4)//pext//ddir(id)//rext//'.dat'
               end do
            end do
         end do

         end if

      elseif (ibase.eq.2) then

c**   RDOAST type PLT structure

         do id=1,nndir
            do i=1,npar
               write (pext,'(A,I1)') '.par',i
               do ird=1,ir
                  ifx = (id-1)*npar*ir+(i-1)*ir+ird
                  write (rext,'(A,I1)') '.rx',ird
                  fnm(ifx) = sf(1:l4)//pext//ddir(id)//rext//'.dat'
               end do
            end do
         end do

      else if (ibase.eq.3) then

c**   SAFARI/OASES PLT structure

         if (nndir.ne.1) stop 'SAFARI can only do 1 direction'
         do ird=1,ir
            do i=1,npar
               write (pext,'(A,I1)') '.par',i
               ifx = i + (ird-1)*npar
               write (rext,'(A,I1)') '.rx',ird
               fnm(ifx) = sf(1:l4)//pext//rext//'.dat'
            end do
         end do

      else if (ibase.eq.4) then

c**   BEM type file - IDIR = forward or backward only

         if (nndir.gt.1) stop 'BEM can only do forward or backward'
         ixdd = nndir
         if (nndir.eq.0) ixdd = 2
         do i=1,npar
            write (pext,'(A,I1)') '.par',i
            do ird=1,ir
               ifx = (i-1)*nndir*ir+ird
               write (rext,'(A,I1)') '.rx',ird
               fnm(ifx) = sf(1:l4)//pext//ddir(ixdd)//rext//'.dat'
            end do
         end do
      end if

c**   Clean out the input file first. BEM generated output has
c**   blank lines inserted.

      open (1,file=bfile,status='old')
      open (2,file='split.tmp',status='unknown')
 2    read (1,'(A79)',end =999) dummy
      if (.not.blanks(dummy)) write(2,'(A79)') dummy
      goto 2
 999  close(1)
      close(2)

c**   Now that we have set up the vector of filenames in FNM(I)

      nline = 0
      bfile = 'split.tmp'
      open(1,file=bfile,status='old')
 5    read (1,'(A79)',end =99) dummy
      nline=nline+1
      goto 5
 99   close(1)
      write (*,*) 'Number of lines in base file = ',nline

c**   Now set up for the split. Note that RDOAST has the
c**   total field included by default.

      if (nndir.eq.0) nndir = 1
      nplt     = npar*ir*nndir
      iperfile = nline/nplt
      write (*,*) 'No. of lines per split file = ',iperfile
      open (1,file=bfile,status='old')
      do j=1,nplt
         write (*,110) fnm(j)
         open (2,file=fnm(j),status='unknown')
         do i=1,iperfile
            read (1,'(A79)') dummy
            write(2,'(A79)') dummy
         end do
         close(2)
      end do
      close(1,status='delete')

 100  format('**** Input file : ',A25,' ****')
 110  format('Writing : ',A40)
      stop
      end

c**   ---------------------------------------------------------
      function lenstr(c)
      integer       lenstr
      character*(*) c

      lenstr = len(c)
      if(lenstr.eq.0) return

      do i=lenstr,1,-1
         if(ichar(c(i:i)).ne.32) then
            lenstr = i 
            return
         end if
      end do
      return
      end
c**   ----------------------------------------------------------
      logical function blanks(msg)
      integer l1
      character*(*) msg

      blanks = .false.
      l1     = lenstr(msg)
      ifs    = index(msg,char(32))

c**   First handle leading spaces from option string.

      i = 1
      if (ifs.eq.1) then
 5       i=i+1
         if (ichar(msg(i:i)).eq.32) goto 5
         ifs = index(msg(i:l1),char(32))+i-1
      end if

c**   Next check for empty message string
      
      if (ifs.eq.l1) blanks = .true.
      return
      end
c**   ---------------------------------------------------------------

      FUNCTION ARGOPT(OPTION)
      LOGICAL       ARGOPT,CND1,CND2
      CHARACTER*(*) OPTION
      CHARACTER*40  DUMMY
      INTEGER       ILEN,OLEN,DLEN,LENSTR,IARGC
C      EXTERNAL      LENSTR,IARGC,GETARG
      ARGOPT = .FALSE.
      ILEN = IARGC()
      IF(ILEN.LT.1) RETURN
      OLEN = LENSTR(OPTION)
      IF(OLEN.EQ.0) RETURN
      DO 10 I=1,ILEN
         CALL GETARG(I,DUMMY)
         DLEN = LENSTR(DUMMY)
         CND1 = DUMMY(1:1).EQ.'-'
         CND2 = DUMMY(2:DLEN).EQ.OPTION(1:OLEN)
         ARGOPT = CND1.AND.CND2
         IF(ARGOPT) RETURN
 10   CONTINUE
      RETURN
      END
c** --------------------------------------------------------------
      FUNCTION ARGNUM(N)
      REAL ARGNUM
      INTEGER N
      DOUBLE PRECISION ATOF
      CHARACTER*99 S
      INTEGER I
      EXTERNAL ARGGET,ATOF
      CALL ARGGET(N,S,I)
      ARGNUM = ATOF(S)
      RETURN
      END
c** --------------------------------------------------------------
      SUBROUTINE ARGGET(N,ARG,LARG)
      INTEGER       N,LARG
      CHARACTER*(*) ARG
      INTEGER       IARGC,LENSTR
c      EXTERNAL      IARGC,GETARG,LENSTR
      LOGICAL       CND
      CND = (N.GT.IARGC()).OR.(N.LT.1)
      CALL GETARG(N,ARG)
      LARG = LENSTR(ARG)
      RETURN
      END
c** --------------------------------------------------------------



