      SUBROUTINE CALSVP(dlwran,nkr,dpth,nkz,gama,pow)

c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      PARAMETER (SQ2PI=2.506628,ONEO2PI=1.591549E-1)
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f' 
      INCLUDE 'comnrd.f'
      INCLUDE 'comvol.f'
      complex c_field(nnkr*2),cfield(nnkr*2),field_int
      complex c_dxfield(nnkr*2),cdxfield(nnkr*2),dxfield_int
      complex c_dzfield(nnkr*2),cdzfield(nnkr*2),dzfield_int
      real    field(nnkr*4),dxfield(nnkr*4),dzfield(nnkr*4)
      equivalence (c_field(1),field(1))
      equivalence (c_dxfield(1),dxfield(1))
      equivalence (c_dzfield(1),dzfield(1))
      real    ctempr,ctempi,temp(nnkr*4)
      real    dzfield_temp(nnkr*4)
      real    field_temp(nnkr*4),dxfield_temp(nnkr*4)
      real gama,pow

      COMPLEX FACSQ,cintpl

      COMPLEX k_r(np),k_z(np)
      COMPLEX ampu(np),ampd(np)
      COMPLEX airyid,biryid,airydid,birydid
      COMPLEX airyiu,biryiu,airydiu,birydiu
      COMPLEX expiu,expid
      COMPLEX ZETAI,ARYI,BRYI,ARYDI,BRYDI,ZTMI
      COMPLEX ZETAIU,ARYIU,BRYIU,ARYDIU,BRYDIU,ZTMIU,ZZTMIU(nnkr)
      COMPLEX ZETAIL,ARYIL,BRYIL,ARYDIL,BRYDIL,ZTMIL,ZZTMIL(nnkr)
      COMPLEX CC1,CC2,CC3,CC4,scatu,scatd
      COMPLEX cmplx_i,ex

      integer j

      logical nfmean

      if (nkr.gt.nnkr) then
       write(6,*) '>>> NKR too large in CALSVP<<<'
       write(*,*) "nkr=",nkr,"max=",nnkr
       stop
      end if

      if (nkz.gt.nnkz) then
       write(6,*) '>>> NKZ too large in CALSVP <<<'
       write(*,*) "nkz=",nkz,"max=",nnkz
       stop
      end if

      onodlw=1e0/dlwran
c      write(*,*) 'dlwran=',dlwran
      READ(46) FF1,NNN,nkmean,nfmean,dlmean
      if (abs(ff1-freq).gt.1e-3*freq) then
       write(6,*) '>>> ERROR: Frequency mismatch in rhs file <<<'
       stop
      end if
c
      if (nkmean.gt.nnkr) then
       write(6,*) '>>> NKMEAN too large in CALSVP<<<'
       write(*,*) "nkmean=",nkmean,"max=",nnkr
       stop
      end if

      if (nfmean) write(6,*) '>>> Mean negative spectrum included <<<'

      do nnk=1,nkmean
        READ(46) ln,k_r(nnk),k_z(nnk),ampd(nnk),ampu(nnk)
      end do

      dk_inc=real(k_r(2)-k_r(1))

c >>> re-scattering disabled 980402
c      if (.not.rescat) then
       DO 3 II=1,NUML
 3     ROUGH2(II)=0
c      end if

      DZ=DPTH/NKZ     

      nkr_i=nkr*2
      dlr=2.*pi/(real(dlwran)*nkr_i)
      dlr_i=2.0*pi/(dk_inc*nkr_i)

      write(*,*) 'dpth=',dpth,', nkz=',nkz
      write(*,*) 'dkr_i=',k_r(2)-k_r(1),'dkr_s=',dlwran

      cmplx_i=cmplx(0.0,1.0)

        do j=1,nkz
c         write(*,*) 'j=',j
         do nnk=1,nkr_i
          c_field(nnk)=0.0
          c_dxfield(nnk)=0.0
          c_dzfield(nnk)=0.0
         enddo
         if (laytyp(ln).eq.2) then
          do nnk=1,nkmean
            if (j.eq.1) then
             ZETAIU=CCO(LN)*K_R(NNK)*K_R(NNK)-BCO(LN)
             ZETAIL=CCO(LN)*K_R(NNK)*K_R(NNK)-(ACO(LN)*DPTH+BCO(LN))
             CALL SCAIRY(ZETAIU,ARYIU,BRYIU,ARYDIU,BRYDIU,ZTMIU)
             CALL SCAIRY(ZETAIL,ARYIL,BRYIL,ARYDIL,BRYDIL,ZTMIL)
             ZZTMIU(nnk)=ZTMIU
             ZZTMIL(nnk)=ZTMIL
            endif

            ZETAI=CCO(LN)*K_R(NNK)*K_R(NNK)-(ACO(LN)*(J-0.5)*DZ+BCO(LN))
            CALL SCAIRY(ZETAI,ARYI,BRYI,ARYDI,BRYDI,ZTMI)
           
            if (real(aco(ln)).lt.0) then
               AIRYID=ARYI*CEXP(ZZTMIU(nnk)-ZTMI)
               AIRYDID=ARYDI*CEXP(ZZTMIU(nnk)-ZTMI)
               BIRYIU=BRYI*CEXP(ZTMI-ZZTMIL(nnk))
               BIRYDIU=BRYDI*CEXP(ZTMI-ZZTMIL(nnk))

c                  write(*,*) 'building field for cylindrical source'

C     build up cylindrically propagating incident field

               c_field(nnk)=
     &           ( ampd(nnk)*AIRYID + ampu(nnk)*BIRYIU )

               if (gama.gt.1E-3) then
C     build up horizontal derivative of cylindrically propagate incident field

                c_dxfield(nnk)= - cmplx_I*k_r(nnk)*
     &           ( ampd(nnk)*AIRYID + ampu(nnk)*BIRYIU )

C     build up vertical derivative of cylindrically propagating incident field

                c_dzfield(nnk)=
     &           -aco(ln)*( ampd(nnk)*AIRYDID + ampu(nnk)*BIRYDIU )
               end if

            else

               BIRYID=BRYI*CEXP(ZTMI-ZZTMIU(nnk))
               BIRYDID=BRYDI*CEXP(ZTMI-ZZTMIU(nnk))
               AIRYIU=ARYI*CEXP(ZZTMIL(nnk)-ZTMI)
               AIRYDIU=ARYDI*CEXP(ZZTMIL(nnk)-ZTMI)


c                  write(*,*) 'building field for cylindrical source'

C     build up cylindrically propagating incident field

               c_field(nnk)=
     &           ( ampd(nnk)*BIRYID + ampu(nnk)*AIRYIU )

C     build up horizontal derivative of cylindrically propagate incident field

               if (gama.gt.1E-3) then
                c_dxfield(nnk)=- cmplx_i*k_r(nnk)*
     &           ( ampd(nnk)*BIRYID + ampu(nnk)*AIRYIU )

C     build up vertical derivative of cylindrically propagating incident field

                c_dzfield(nnk)=
     &          - aco(ln)*( ampd(nnk)*BIRYDID + ampu(nnk)*AIRYDIU )
               end if
            endif

          end do
         else
          do nnk=1,nkmean
           EXPID=CEXPT(-CMPLX_I*K_Z(NNK)*(J-0.5)*DZ)
           EXPIU=CEXPT(-CMPLX_I*K_Z(NNK)*(DPTH-(J-0.5)*DZ))

C     build up incident field

           c_field(nnk)= (ampd(nnk)*EXPID + ampu(nnk)*EXPIU)

C     build up horizontal derivative of incident field

           if (gama.gt.1E-3) then
            c_dxfield(nnk)= (-cmplx_i)*k_r(nnk)*
     c              ( ampd(nnk)*EXPID + ampu(nnk)*EXPIU )

C     build up vertical derivative of incident field

            c_dzfield(nnk)= cmplx_i*k_z(nnk)*
     c              ( -ampd(nnk)*EXPID + ampu(nnk)*EXPIU )

           end if
          end do
         end if

c Multiply by range exponential and dk

         do jj=1,nkr
          rangem=(jj-1)*dlr_i
          ex=cexp(-cmplx_i*real(k_r(1))*rangem)*dk_inc
C          ex=dk_inc
          c_field(jj)=ex*c_field(jj)
          c_dxfield(jj)=ex*c_dxfield(jj)
          c_dzfield(jj)=ex*c_dzfield(jj)
         end do

c >>> add negative spectrum

         if (nfmean) then
          do jj=2,nkr
           jn=nkr_i+2-jj
           c_field(jn)  =   c_field(jj)
           c_dxfield(jn)= - c_dxfield(jj)
           c_dzfield(jn)=   c_dzfield(jj) 
          end do
         end if
C     transform incident field at depth j to spatial domain

         CALL FOUR1(c_field,NKR_I,-1)

C     transform horizontal derivative of inc field at depth j to spatial domain

         if (gama.gt.1E-3) then

          CALL FOUR1(c_dxfield,NKR_I,-1)

C     transform vertical derivative of inc field at depth j to spatial domain

          CALL FOUR1(c_dzfield,NKR_I,-1)
         end if

C     interpolate incident field to spatial grid of roughness realization
c     Symmetric
c        write(6,*) '      Interpolation'
         do jj=1,nkr

            field_int=cintpl(c_field,0.,dk_inc*nkr_i*oneo2pi,nkr_i,
     c           (jj-1)*dlr)

            cfield(jj)=field_int

            if (jj.gt.1) then
C             cfield(nkr_i+2-jj)=field_int
               field_int=cintpl(c_field,0.,dk_inc*nkr_i*oneo2pi,nkr_i,
     c              1.0/(dk_inc*oneo2pi)-(jj-1)*dlr)
               cfield(nkr_i+2-jj)=field_int
            end if
         end do
C         cfield(nkr+1)=cfield(nkr)

         field_int=cintpl(c_field,0.,dk_inc*nkr_i*oneo2pi,nkr_i,
     c        (nkr)*dlr)

         cfield(nkr+1)=cmplx(2.*real(field_int),0.0)


        if (gama.gt.1E-3) then

C     interpolate horizontal derivative of incident field to spatial 
c     grid of roughness realization. Antisymmetric

         do jj=1,nkr
            
            dxfield_int=cintpl(c_dxfield,0.,dk_inc*nkr_i*oneo2pi,nkr_i,
     c           (jj-1)*dlr)

            cdxfield(jj)=dxfield_int

            if (jj.gt.1) then
C             cdxfield(nkr_i+2-jj)=-dxfield_int
               dxfield_int=cintpl(c_dxfield,0.,dk_inc*nkr_i*oneo2pi,
     c              nkr_i,1.0/(dk_inc*oneo2pi)-(jj-1)*dlr)
               cdxfield(nkr_i+2-jj)=dxfield_int
            end if
         end do
C         cdxfield(nkr+1)=0e0

         cdxfield(nkr+1)=cmplx(0.0,0.0)

C     interpolate vertical derivative of incident field to spatial grid 
c     of roughness realization. Symmetric.

         do jj=1,nkr

            dzfield_int=cintpl(c_dzfield,0.,dk_inc*nkr_i*oneo2pi,nkr_i,
     c           (jj-1)*dlr)
            cdzfield(jj)=dzfield_int

            if (jj.gt.1) then
C             cdzfield(nkr_i+2-jj)=dzfield_int
               dzfield_int=cintpl(c_dzfield,0.,dk_inc*nkr_i*oneo2pi,
     c              nkr_i,1.0/(dk_inc*oneo2pi)-(jj-1)*dlr)
               cdzfield(nkr_i+2-jj)=dzfield_int
            end if
         end do
C         cdzfield(nkr+1)=cdzfield(nkr)

         dzfield_int=cintpl(c_dzfield,0.,dk_inc*nkr_i*oneo2pi,nkr_i,
     c        (nkr)*dlr)

         cdzfield(nkr+1)=cmplx(2.*real(dzfield_int),0.0)

        end if

c     multiply roughness in spatial domain by incident field

         do jj=1,nkr_i
            c_field(jj)=ETA_x_Z((jJ-1)*NKZ+j)*
     c           cfield(jj)*dlr*oneo2pi

C            write(90,*) real(cfield(jj)),aimag(cfield(jj))
C            write(100,*) ETA_x_Z((jJ-1)*NKZ+j)

         enddo

c     transform product of incident field and roughness to q domain

         CALL FOUR1(c_field,NKR_I,1)

        if (gama.gt.1E-3) then

c     multiply horizontal derivative of roughness in spatial domain 
C     by horizontal derivative of incident field

         do jj=1,nkr_i
            c_dxfield(jj)=ETA_dx_Z((jJ-1)*NKZ+j)*
     c           cdxfield(jj)*dlr*oneo2pi

C            write(91,*) real(cdxfield(jj)),aimag(cdxfield(jj))
C            write(101,*) ETA_dx_Z((jJ-1)*NKZ+j)

         enddo

c     transform product of dx incident field and dx roughness back to q domain

         CALL FOUR1(c_dxfield,NKR_I,1)

c     multiply vertical derivative of roughness in spatial domain by 
C     vertical derivative of incident field

         do jj=1,nkr_i
            c_dzfield(jj)=eta_x_dz((jJ-1)*NKZ+j)*
     c           cdzfield(jj)*dlr*oneo2pi

C            write(92,*) real(cdzfield(jj)),aimag(cdzfield(jj))
C            write(102,*) ETA_x_dZ((jJ-1)*NKZ+j)

         enddo

c     transform product of incident field and roughness back to q domain

         CALL FOUR1(c_dzfield,NKR_I,1)

        end if

c     insert transformed product into peta_kx_z

            do jj=1,nkr_i

               pETA_KX_Z((jJ-1)*NKZ+j)=c_field(jj)

            enddo

c     insert sum of transformed products associated with derivatives 
C     into deta_kx_z

            do jj=1,nkr_i
               dETA_KX_Z((jJ-1)*NKZ+j)= ( c_dxfield(jj) +
     c              c_dzfield(jj) )
            enddo


        enddo
      
      write(6,*) '>>> CALSVP: Calling SOLSV <<<'

      call SOLSV(ln,dlwran,nkr,dpth,nkz,gama,pow)

      RETURN        
                   
      END           

      SUBROUTINE CALSVC(dlwran,nkr,dpth,nkz,gama,pow)

c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      PARAMETER (SQ2PI=2.506628,ONEO2PI=1.591549E-1)
      parameter (nwght=2048)
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f' 
      INCLUDE 'comnrd.f'
      INCLUDE 'comvol.f'
      INCLUDE 'combes.f'
      complex c_field(nnkr*2),cfield(nnkr*2),field_int
      complex c_dxfield(nnkr*2),cdxfield(nnkr*2),dxfield_int
      complex c_dzfield(nnkr*2),cdzfield(nnkr*2),dzfield_int
      real    field(nnkr*4),dxfield(nnkr*4),dzfield(nnkr*4)
      equivalence (c_field(1),field(1))
      equivalence (c_dxfield(1),dxfield(1))
      equivalence (c_dzfield(1),dzfield(1))
      real    ctempr,ctempi,temp(nnkr*4)
      real    dzfield_temp(nnkr*4)
      real    field_temp(nnkr*4),dxfield_temp(nnkr*4)
      complex c_fld_tmp(nnkr*2),c_dxfld_tmp(nnkr*2),c_dzfld_tmp(nnkr*2)
      equivalence (c_fld_tmp(1),field_temp(1))
      equivalence (c_dxfld_tmp(1),dxfield_temp(1))
      equivalence (c_dzfld_tmp(1),dzfield_temp(1))
      real wght(nwght)
      real gama,pow
      COMPLEX FACSQ,cintpl

      COMPLEX k_r(np),k_z(np)
      COMPLEX ampu(np),ampd(np)
      COMPLEX airyid,biryid,airydid,birydid
      COMPLEX airyiu,biryiu,airydiu,birydiu
      COMPLEX expiu,expid
      COMPLEX ZETAI,ARYI,BRYI,ARYDI,BRYDI,ZTMI
      COMPLEX ZETAIU,ARYIU,BRYIU,ARYDIU,BRYDIU,ZTMIU,ZZTMIU(nnkr)
      COMPLEX ZETAIL,ARYIL,BRYIL,ARYDIL,BRYDIL,ZTMIL,ZZTMIL(nnkr)
      COMPLEX CC1,CC2,CC3,CC4,scatu,scatd
      complex sqrtr(nnkr),sqrtk(nnkr)
      COMPLEX cmplx_i,cfk,cfr,wn,ex,ex1,ex2,extab(nnkr)
      real sqrtri(nnkr),sqrtq(nnkr)
      integer j

      logical nfmean

      if (nkr.gt.nnkr) then
       write(6,*) '>>> NKR too large in CALSVC<<<'
       write(*,*) "nkr=",nkr,"max=",nnkr
       stop
      end if

      if (nkz.gt.nnkz) then
       write(6,*) '>>> NKZ too large in CALSVC <<<'
       write(*,*) "nkz=",nkz,"max=",nnkz
       stop
      end if

c 
c >>> For full Bessel integration, prepare weight array.
c >>> First 50% full Bessel. Last 50% Hanning weighted with
c >>> asymptotic Hankel function
c
       dwght=(rkmax-brk_0)/(nwght-1)
       onodwg=1e0/dwght
       nwghth=nwght/2
       do i=1,nwghth
        wght(i)=1.0
        wght(i+nwghth)=0.5*( 1e0 + cos( ((i-1)*pi)/(nwghth-1) ) )
       end do

      onodlw=1e0/dlwran
c      write(*,*) 'dlwran=',dlwran
      READ(46) FF1,NNN,nkmean,nfmean,dlmean
      if (abs(ff1-freq).gt.1e-3*freq) then
       write(6,*) '>>> ERROR: Frequency mismatch in rhs file <<<'
       stop
      end if
c
      if (nkmean.gt.nnkr) then
       write(6,*) '>>> NKMEAN too large in CALSVC<<<'
       write(*,*) "nkmean=",nkmean,"max=",nnkr
       stop
      end if

      if (nfmean) write(6,*) '>>> Mean negative spectrum included <<<'

      do nnk=1,nkmean
        READ(46) ln,k_r(nnk),k_z(nnk),ampd(nnk),ampu(nnk)
      end do

      dk_inc=real(k_r(2)-k_r(1))

      write(*,*) 'dk_inc=',dk_inc,', dlmean=',dlmean ,', dlwvno=',dlwvno 

c >>> re-scattering disabled 980402
c      if (.not.rescat) then
       DO 3 II=1,NUML
 3     ROUGH2(II)=0
c      end if

      DZ=DPTH/NKZ     

      nkr_i=nkr*2
      dlr=2.*pi/(dlwran*nkr_i)
      dlr_i=2.0*pi/((dk_inc)*nkr_i)

      cmplx_i=cmplx(0.0,1.0)

c >>> make sqrt(range,wvno) table for full Bessel function compensation
       do i=1,nkr_i
        if (i.eq.1) then
         sqrtri(i)=1E0/sqrt(0.5*dlr_i)
        else
         sqrtri(i)=1E0/sqrt((i-1)*dlr_i)
        end if
       end do

       cfk=cexp(cmplx_i*(0.5+mbf_0)*pi*0.5)/sqrt(pi*2E0)*dk_inc
       do i=1,nkmean
         wn=k_r(1)+(i-1)*dk_inc
         sqrtk(i)=cfk*sqrt(wn)
       end do

       cfr=cexp(-cmplx_i*(0.5+mbf_0)*pi*0.5)/sqrt(pi*2E0)*dlr
       do i=1,nkr
        sqrtr(i)=cfr*sqrt((i-1)*dlr)
        if (i.gt.1) then
         sqrtr(nkr_i+2-i)=cmplx(real(sqrtr(i)),-aimag(sqrtr(i)))
        end if
        sqrtr(nkr+1)=sqrtr(nkr)
       end do

       do i=1,nkr
        if (i.eq.1) then
         sqrtq(i)=1E0/sqrt(0.5*dlwran)
        else
         sqrtq(i)=1E0/sqrt((i-1)*dlwran)
         sqrtq(nkr_i+2-i)=sqrtq(i)
        end if
       end do
       sqrtq(nkr+1)=sqrtq(nkr)

c >>> make exponential table for Hankel compensation
       d_kr=2*pi/nkr_i
       do i=1,nkr_i
        extab(i)=cexpt(cmplx_i*(i-1)*d_kr)
       end do      

      write(*,*) 'dpth=',dpth,', nkz=',nkz
      write(*,*) 'dkr_i=',k_r(2)-k_r(1),'dkr_s=',dlwran
      write(*,*) 'dlr=',dlr

        do j=1,nkz
c         write(*,*) 'j=',j
         if (mbf_0.eq.0) then
          do nnk=1,nkr_i
           c_field(nnk)=0.0
           c_dxfield(nnk)=0.0
           c_dzfield(nnk)=0.0
          enddo
          if (laytyp(ln).eq.2) then
           do nnk=1,nkmean
             if (j.eq.1) then
              ZETAIU=CCO(LN)*K_R(NNK)*K_R(NNK)-BCO(LN)
              ZETAIL=CCO(LN)*K_R(NNK)*K_R(NNK)-(ACO(LN)*DPTH+BCO(LN))
              CALL SCAIRY(ZETAIU,ARYIU,BRYIU,ARYDIU,BRYDIU,ZTMIU)
              CALL SCAIRY(ZETAIL,ARYIL,BRYIL,ARYDIL,BRYDIL,ZTMIL)
              ZZTMIU(nnk)=ZTMIU
              ZZTMIL(nnk)=ZTMIL
             endif

             ZETAI=CCO(LN)*K_R(NNK)*K_R(NNK)
     &                   -(ACO(LN)*(J-0.5)*DZ+BCO(LN))
             CALL SCAIRY(ZETAI,ARYI,BRYI,ARYDI,BRYDI,ZTMI)
           
             if (real(aco(ln)).lt.0) then
               AIRYID=ARYI*CEXP(ZZTMIU(nnk)-ZTMI)
               AIRYDID=ARYDI*CEXP(ZZTMIU(nnk)-ZTMI)
               BIRYIU=BRYI*CEXP(ZTMI-ZZTMIL(nnk))
               BIRYDIU=BRYDI*CEXP(ZTMI-ZZTMIL(nnk))

c                  write(*,*) 'building field for cylindrical source'

C     build up cylindrically propagating incident field

               c_field(nnk)=
     &           ( ampd(nnk)*AIRYID + ampu(nnk)*BIRYIU )

              if (gama.gt.1E-3) then
C     build up horizontal derivative of cylindrically propagate incident field

               c_dxfield(nnk)= - cmplx_I*k_r(nnk)*
     &           ( ampd(nnk)*AIRYID + ampu(nnk)*BIRYIU )

C     build up vertical derivative of cylindrically propagating incident field

               c_dzfield(nnk)=
     &           -aco(ln)*( ampd(nnk)*AIRYDID + ampu(nnk)*BIRYDIU )
              end if
             else

               BIRYID=BRYI*CEXP(ZTMI-ZZTMIU(nnk))
               BIRYDID=BRYDI*CEXP(ZTMI-ZZTMIU(nnk))
               AIRYIU=ARYI*CEXP(ZZTMIL(nnk)-ZTMI)
               AIRYDIU=ARYDI*CEXP(ZZTMIL(nnk)-ZTMI)

c                  write(*,*) 'building field for cylindrical source'

C     build up cylindrically propagating incident field

               c_field(nnk)=
     &           ( ampd(nnk)*BIRYID + ampu(nnk)*AIRYIU )

               if (gama.gt.1E-3) then
C     build up horizontal derivative of cylindrically propagate incident field

                c_dxfield(nnk)=- cmplx_i*k_r(nnk)*
     &           ( ampd(nnk)*BIRYID + ampu(nnk)*AIRYIU )

C     build up vertical derivative of cylindrically propagating incident field

                c_dzfield(nnk)=
     &          - aco(ln)*( ampd(nnk)*BIRYDID + ampu(nnk)*AIRYDIU )
               end if
            endif

           end do
          else
           do nnk=1,nkmean
            EXPID=CEXPT(-CMPLX_I*K_Z(NNK)*(J-0.5)*DZ)
            EXPIU=CEXPT(-CMPLX_I*K_Z(NNK)*(DPTH-(J-0.5)*DZ))

C     build up incident field

            c_field(nnk)= (ampd(nnk)*EXPID + ampu(nnk)*EXPIU)

            if (gama.gt.1E-3) then
C     build up horizontal derivative of incident field

             c_dxfield(nnk)= (-cmplx_i)*k_r(nnk)*
     c              ( ampd(nnk)*EXPID + ampu(nnk)*EXPIU )

C     build up vertical derivative of incident field

             c_dzfield(nnk)= cmplx_i*k_z(nnk)*
     c              ( -ampd(nnk)*EXPID + ampu(nnk)*EXPIU )
            end if
           end do
         end if

C     transform incident field at depth j to spatial domain
         
         call vmov(c_field,1,c_fld_tmp,1,2*nkr_i)
         call cvmul(c_field,2,sqrtk,2,c_field,2,nkmean,1)

         CALL FOUR1(c_field,nkr_i,-1)

C     transform horizontal derivative of inc field at depth j to spatial domain

         if (gama.gt.1E-3) then
          call vmov(c_dxfield,1,c_dxfld_tmp,1,2*nkr_i)
          call cvmul(c_dxfield,2,sqrtk,2,c_dxfield,2,nkmean,1)
 
          CALL FOUR1(c_dxfield,nkr_i,-1)
         
C     transform vertical derivative of inc field at depth j to spatial domain

          call vmov(c_dzfield,1,c_dzfld_tmp,1,2*nkr_i)
          call cvmul(c_dzfield,2,sqrtk,2,c_dzfield,2,nkmean,1)

          CALL FOUR1(c_dzfield,nkr_i,-1)
         end if
c >>> Multiply all by waveno offset exponential and 1/sqrt(r)

         do jr=1,nkr_i
          rangem=(jr-1)*dlr_i
          ex1=cexpt(-cmplx(0e0,1e0)*real(k_r(1))*rangem)
          c_field(jr)=sqrtri(jr)*ex1*c_field(jr)
          c_dxfield(jr)=sqrtri(jr)*ex1*c_dxfield(jr)
          c_dzfield(jr)=sqrtri(jr)*ex1*c_dzfield(jr)
         end do

c      write(6,*) ' Incident Hankel compensation '
c >>> Hankel compensation

         do jr=1,nkr_i
          rangem=(jr-1)*dlr_i
          indx_e=1
          if (jr.gt.1) then
           bkmx=rkmax/rangem
           ibkmx=int((bkmx-k_r(1))/dk_inc)+1
           ex1=cexpt(-cmplx(0e0,1e0)*real(k_r(1))*rangem)
           do jk=1,min(nkmean,ibkmx)
            wn=k_r(1)+(jk-1)*dk_inc
            rk=rangem*wn

            if (rk.lt.rkmax) then
             awg=RINTPL(wght,brk_0,onodwg,nwght,rk)
            else
             awg=0e0
            end if
            bes_J0= RINTPL(BF(1,1),brk_0,ONODRK,NRKMAX,rk)
            if (jk.eq.1) then
             ex=ex1*extab(1)
            else
             ex=ex1*extab(nkr_i+2-indx_e)
            end if
c            ex=cexpt(cmplx(0e0,-rk))
            c_field(jr)=c_field(jr) + c_fld_tmp(jk)*awg*
     &       ( wn*bes_J0*dk_inc - sqrtk(jk)*ex*sqrtri(jr))   
            if (gama.gt.1E-3) then
             bes_J1= RINTPL(BF(1,2),brk_0,ONODRK,NRKMAX,rk)
             c_dxfield(jr)=c_dxfield(jr) + c_dxfld_tmp(jk)*awg*
     &        (-ai*wn*bes_J1*dk_inc - sqrtk(jk)*ex*sqrtri(jr))   
             c_dzfield(jr)=c_dzfield(jr) + c_dzfld_tmp(jk)*awg*
     &       ( wn*bes_J0*dk_inc - sqrtk(jk)*ex*sqrtri(jr))   
            end if
           indx_e=indx_e+(jr-1)
           if (indx_e.gt.nkr_i) indx_e=indx_e-nkr_i

           end do

          else
           do jk=1,nkmean
            wn=k_r(1)+(jk-1)*dk_inc
            if (jk.eq.1) then
             c_field(jr)=0.5*wn*c_fld_tmp(jk)*dk_inc
             if (gama.gt.1E-3) then
              c_dxfield(jr)=0e0
              c_dzfield(jr)=0.5*wn*c_dzfld_tmp(jk)*dk_inc
             end if
            else
             c_field(jr)=c_field(jr) + wn*dk_inc*c_fld_tmp(jk)
             if (gama.gt.1E-3) then
              c_dzfield(jr)=c_dzfield(jr) + wn*dk_inc*c_dzfld_tmp(jk)
             end if
            end if
           end do
          end if
       end do

C     interpolate incident field to spatial grid of roughness realization
c     Symmetric
c        write(6,*) '      Interpolation'
         do jj=1,nkr

            field_int=cintpl(c_field,0.,dk_inc*nkr_i*oneo2pi,nkr_i,
     c           (jj-1)*dlr)

            cfield(jj)=field_int

            if (jj.gt.1) then
             cfield(nkr_i+2-jj)=field_int
            end if
         end do

         field_int=cintpl(c_field,0.,dk_inc*nkr_i*oneo2pi,nkr_i,
     c        (nkr)*dlr)

         cfield(nkr+1)=2.*field_int

         if (gama.gt.1E-3) then
C     interpolate horizontal derivative of incident field to spatial 
c     grid of roughness realization. Antisymmetric
 
          do jj=1,nkr
            
            dxfield_int=cintpl(c_dxfield,0.,dk_inc*nkr_i*oneo2pi,nkr_i,
     c           (jj-1)*dlr)

            cdxfield(jj)=dxfield_int

            if (jj.gt.1) then
             cdxfield(nkr_i+2-jj)=-dxfield_int
            end if
          end do

          cdxfield(nkr+1)=(0.0,0.0)

C     interpolate vertical derivative of incident field to spatial grid 
c     of roughness realization. Symmetric.

          do jj=1,nkr

            dzfield_int=cintpl(c_dzfield,0.,dk_inc*nkr_i*oneo2pi,nkr_i,
     c           (jj-1)*dlr)
            cdzfield(jj)=dzfield_int

            if (jj.gt.1) then
             cdzfield(nkr_i+2-jj)=dzfield_int
            end if
          end do

          dzfield_int=cintpl(c_dzfield,0.,dk_inc*nkr_i*oneo2pi,nkr_i,
     c        (nkr)*dlr)
          cdzfield(nkr+1)=2.*dzfield_int
         end if
c >>> write interpolated incident field to file 47
         write(47) (cfield(jj),jj=1,nkr_i),(cdxfield(jj),jj=1,nkr_i),
     &          (cdzfield(jj),jj=1,nkr_i) 
        else
c >>> write interpolated incident field to file 47
         read(47) (cfield(jj),jj=1,nkr_i),(cdxfield(jj),jj=1,nkr_i),
     &          (cdzfield(jj),jj=1,nkr_i) 
        end if
c     multiply roughness in spatial domain by incident field
c     and extract symmetric component for cylindrical geometry
c       write(6,*) '      Multiply by roughness'
            jj=1
            do jj=1,nkr_i
               c_field(jj)=ETA_x_Z((jJ-1)*NKZ+j)*
     c              cfield(jj)
C            write(90,*) real(cfield(jj)),aimag(cfield(jj))
C            write(100,*) ETA_x_Z((jJ-1)*NKZ+j)
            enddo


c         do jj=2,nkr
c            jn=nkr_i+2-jj
c           c_field(jj)=0.5*
c     &       ( ETA_x_Z((jJ-1)*NKZ+j)*cfield(jj) +
c     &         ETA_x_Z((jn-1)*NKZ+j)*cfield(jn) )

C            c_field(jj)=ETA_x_Z((jJ-1)*NKZ+j)*cfield(jj) 

c            c_field(jn)=c_field(jj)

C            write(90,*) real(cfield(jj)),aimag(cfield(jj))
C            write(100,*) ETA_x_Z((jJ-1)*NKZ+j)

c         enddo
         do jj=nkr+1,nkr_i
C            write(90,*) real(cfield(jj)),aimag(cfield(jj))
C            write(100,*) ETA_x_Z((jJ-1)*NKZ+j)
         enddo

           c_field(nkr+1)=ETA_x_Z(nkr*NKZ+j)*cfield(nkr+1)

c     transform product of incident field and roughness to q domain

c       write(6,*) '      Transform to q domain'

         call vmov(c_field,1,c_fld_tmp,1,2*nkr_i)
         call cvmul(c_field,2,sqrtr,2,c_field,2,nkr_i,1)

         CALL FOUR1(c_field,NKR_I,1)

         do jq=1,nkr_i
          c_field(jq)=sqrtq(jq)*c_field(jq)
         end do

         if (gama.gt.1E-3) then

c     multiply horizontal derivative of roughness in spatial domain 
C     by horizontal derivative of incident field
c     and extract symmetric component for cylindrical geometry
c       write(6,*) '      Multiply dx by roughness'

            jj=1
            do jj=1,nkr_i
               c_dxfield(jj)=ETA_dx_Z((jJ-1)*NKZ+j)*
     c              cdxfield(jj)
C            write(91,*) real(cdxfield(jj)),aimag(cdxfield(jj))
C            write(101,*) ETA_dx_Z((jJ-1)*NKZ+j)
            enddo


          c_dxfield(nkr+1)=ETA_dx_Z(nkr*NKZ+j)*cdxfield(nkr+1)

c     transform product of dx incident field and dx roughness back to q domain
c       write(6,*) '      Transform dx to q domain'

          call vmov(c_dxfield,1,c_dxfld_tmp,1,2*nkr_i)
          call cvmul(c_dxfield,2,sqrtr,2,c_dxfield,2,nkr_i,1)

          CALL FOUR1(c_dxfield,NKR_I,1)

          do jq=1,nkr_i
           c_dxfield(jq)=sqrtq(jq)*c_dxfield(jq)
          end do

c     multiply vertical derivative of roughness in spatial domain by 
C     vertical derivative of incident field
c     and extract symmetric component for cylindrical geometry
c       write(6,*) '      Multiply dz by roughness'

c            jj=1

          do jj=1,nkr_i
            c_dzfield(jj)=eta_x_dz((jJ-1)*NKZ+j)*
     c           cdzfield(jj)
C            write(92,*) real(cdzfield(jj)),aimag(cdzfield(jj))
C            write(102,*) ETA_x_dZ((jJ-1)*NKZ+j)
          enddo

          c_dzfield(nkr+1)=ETA_x_dZ(nkr*NKZ+j)*cdzfield(nkr+1)

c     transform product of incident field and roughness back to q domain
c       write(6,*) '      Transform dz to q domain'

          call vmov(c_dzfield,1,c_dzfld_tmp,1,2*nkr_i)
          call cvmul(c_dzfield,2,sqrtr,2,c_dzfield,2,nkr_i,1)

          CALL FOUR1(c_dzfield,NKR_I,1)

          do jq=1,nkr_i
           c_dzfield(jq)=sqrtq(jq)*c_dzfield(jq)
          end do
         end if
c >>> Hankel compensation
c       write(6,*) '      Hankel compensation'

         do jq=1,nkr
          wk=(jq-1)*dlwran
          indx_e=1
          if (jq.gt.1) then
           bkmx=rkmax/wk
           ibkmx=int((bkmx)/dlr)+1
           do jr=1,min(nkr,ibkmx)
            jrm=nkr_i+2-jr
            rangem=(jr-1)*dlr
            rk=rangem*wk
            if (rk.lt.brk_0) then
             c_field(jq)=0e0
             c_dxfield(jq)=0e0
             c_dzfield(jq)=0e0
            else
             if (rk.lt.rkmax) then
              awg=RINTPL(wght,brk_0,onodwg,nwght,rk)
              bes_J0= RINTPL(BF(1,1),brk_0,ONODRK,NRKMAX,rk)
             else
              awg=0e0
             end if

             ex1=extab(indx_e)
c            ex1=cexpt(cmplx(0E0,rk))
             if (jr.gt.1) then
               ex2=extab(nkr_i+2-indx_e)
               ex2=cmplx(real(ex1),-aimag(ex1))
c              ex2=cexpt(cmplx(0E0,-rk))
             else
              ex2=0e0
             end if 
             c_field(jq)=c_field(jq)+c_fld_tmp(jr)*awg*
     &           (rangem*bes_J0*dlr-(sqrtr(jr)*ex1+sqrtr(jrm)*ex2)*
     &           sqrtq(jq))
             if (gama.gt.1E-3) then
              c_dxfield(jq)=c_dxfield(jq)+c_dxfld_tmp(jr)*awg*
     &           (rangem*bes_J0*dlr-(sqrtr(jr)*ex1+sqrtr(jrm)*ex2)*
     &           sqrtq(jq))
              c_dzfield(jq)=c_dzfield(jq)+c_dzfld_tmp(jr)*awg*
     &           (rangem*bes_J0*dlr-(sqrtr(jr)*ex1+sqrtr(jrm)*ex2)*
     &           sqrtq(jq))
             end if
            end if

            indx_e=indx_e+(jq-1)
            if (indx_e.gt.nkr_i) indx_e=indx_e-nkr_i

           end do
          else if (mbf_0.eq.0) then
           do jr=1,nkr
            rangem=(jr-1)*dlr
            if (jr.eq.1) then
             c_field(jq)=0.5*rangem*c_fld_tmp(jr)*dlr
             if (gama.gt.1E-3) then
              c_dxfield(jq)=0.5*rangem*c_dxfld_tmp(jr)*dlr
              c_dzfield(jq)=0.5*rangem*c_dzfld_tmp(jr)*dlr
             end if
            else
             c_field(jq)=c_field(jq) + rangem*dlr*c_fld_tmp(jr)
             if (gama.gt.1E-3) then
              c_dxfield(jq)=c_dxfield(jq) + rangem*dlr*c_dxfld_tmp(jr)
              c_dzfield(jq)=c_dzfield(jq) + rangem*dlr*c_dzfld_tmp(jr)
             end if
            end if
           end do
          else
           c_field(jq)=0e0
           c_dxfield(jq)=0e0
           c_dzfield(jq)=0e0
          end if
         end do

c         write(6,*) 'Hankel done'

c     insert transformed product into peta_kx_z

            do jj=1,nkr_i

               pETA_KX_Z((jJ-1)*NKZ+j)=c_field(jj)
               if (freq.eq.freq1) then
                write(84,*) real(c_field(jj)),aimag(c_field(jj))
               end if
            enddo

c     insert sum of transformed products associated with derivatives 
C     into deta_kx_z

             do jj=1,nkr_i

               dETA_KX_Z((jJ-1)*NKZ+j)= ( c_dxfield(jj) +
     c              c_dzfield(jj) )

C               write(85,*) real(c_dxfield(jj)),aimag(c_dxfield(jj))
C               write(86,*) real(c_dzfield(jj)),aimag(c_dzfield(jj))

             enddo


        enddo
      

      write(6,*) '>>> CALSVC: Calling SOLSV <<<'
      call SOLSV(ln,dlwran,nkr,dpth,nkz,gama,pow)

      RETURN        
                   
      END           

      SUBROUTINE SOLSV(ln,dlwran,nkr,dpth,nkz,gama,pow)

c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      PARAMETER (SQ2PI=2.506628,ONEO2PI=1.591549E-1)
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f' 
      INCLUDE 'comnrd.f'
      INCLUDE 'comvol.f'

      real gama,pow,zss(nnkz)
      COMPLEX FACSQ

      COMPLEX airyss1(nnkz),biryss1(nnkz),ztamss(nnkz),cc(nnkz)
      COMPLEX airyss2(nnkz),biryss2(nnkz)
      COMPLEX q_z
      COMPLEX ZETASS,ARYSS,BRYSS,ARYDSS,BRYDSS,ZTMSS
      COMPLEX CC1,CC2,CC3,CC4,scatu,scatd
      COMPLEX cmplx_i

      integer j

      logical nfmean

      NS=ICUT2-ICUT1+1
C *** OPEN SCRATCH FILE FOR KERNELS
      IF (DECOMP) THEN
       NGFILS=5
       IF (ISIZE.LT.IR*5*npar) STOP '>>> PARAMETER ISIZE TOO SMALL <<<'
      ELSE
       NGFILS=1
       IF (ISIZE.LT.IR*npar) STOP '>>> PARAMETER ISIZE TOO SMALL <<<'
      END IF
      LUOFF=LUGRN-1
c *** determine buffer size
      NBLOCKS=MEMLFT()/NGFILS-1 
      DO 5 IFC=1,NGFILS
       CALL OPNBUF(LUOFF+IFC,2*IR,NS*NOUT,NBLOCKS)
 5    CONTINUE
C *** WAVENUMBER RAMP
      CALL VRAMP(WK0,DLWVNO,FAC,1,NWVNO)
      NGVALS=2*IR*NPAR*NGFILS
C *** WAVENUMBER LOOP
c      write(47,*) numfr
c      write(47,*) real(dsq),rimag(dsq)
c      write(47,*) wk0,offima,dlwvno
c      write(47,*) icut2-icut1+1
      p_conv=1e0/real(pcorr)

      dz=dpth/nkz     
      do j=1,nkz
       zss(j)=(j-0.5)*dz
      end do

      do II=ICUT1,ICUT2 
       WVNO=CMPLX(FAC(II),OFFIMA)

c >>> One set of sources
       nsrow=1
       CALL INITS
       CALL BUILD                 
C *** FILL IN SCATTERING RIGHT HAND SIDE
       CC1=CNUL
       CC2=CNUL
       CC3=CNUL
       CC4=CNUL
       q_z = -ai*alfa(ln)

       if (laytyp(ln).eq.2) then

        do j=1,nkz
c          write(6,*) 'SCAIRY, j=',j,zss(j)
          ZETASS=CCO(LN)*WVNO*WVNO-(ACO(LN)*(J-0.5)*DZ+BCO(LN))
          CALL SCAIRY(ZETASS,ARYSS,BRYSS,ARYDSS,BRYDSS,ZTMSS)

          if (real(aco(ln)).lt.0) then
             AIRYSS1(J)=ARYSS
             BIRYSS2(J)=BRYSS
             ztamss(j)=ztmss
          else
             BIRYSS1(J)=BRYSS
             AIRYSS2(J)=ARYSS
             ztamss(j)=ztmss
          endif

          CC(J)=2.0*SIGN(1.0,REAL(ACO(LN)))/((-ARYSS*BRYDSS+
     C         BRYSS*ARYDSS)*(-ACO(LN)))
        end do
       endif

c >>>  Virtual scattering sources        
       zr=0E0
c       write(6,*) 'scatu',ln
       cc1=scatu(wvno,q_z,dlwran,ak(ln,1),
     &     zss,airyss1,biryss1,ztamss,zr,ztamu(ln),
     &     cc,peta_kx_z,deta_kx_z,nkr,nkz,dpth,gama,pow,ln)
       zr=dpth
c       write(6,*) 'scatd',zr
       cc2=scatd(wvno,q_z,dlwran,ak(ln,1),
     &     zss,airyss2,biryss2,ztamss,zr,ztaml(ln),
     &     cc,peta_kx_z,deta_kx_z,nkr,nkz,dpth,gama,pow,ln)
         
C *** Upper interface

       if (laytyp(ln).ne.2) then
          
         R(ln-1,1,1)=R(ln-1,1,1)
     &        + DISNRM*CC1*alfa(ln)*pi
         R(ln-1,3,1)=R(ln-1,3,1)
     &        - STRNRM*con1(ln)*CC1*pi
         r(ln-1,6,1) = - r(ln-1,3,1)
         
C     *** Lower interface
         R(ln,1,1)=R(ln,1,1)
     &        + DISNRM*CC2*alfa(ln)*pi
         R(ln,3,1)=R(ln,3,1)
     &        + STRNRM*con1(ln)*CC2*pi
         r(ln,6,1) = - r(ln,3,1)
         
       else

         if (real(aco(ln)).lt.0) then

C     write(*,*) 'STRNRM=',strnrm,'DISNRM=',disnrm

            R(ln-1,1,1)=R(ln-1,1,1)
     &           - DISNRM*ACO(LN)*CC1*BIRYDU(ln)*pi
            R(ln-1,3,1)=R(ln-1,3,1)
     &           - STRNRM*CON1(LN)*CC1*BIRYU(ln)*pi
            r(ln-1,6,1) = - r(ln-1,3,1)
            
C     *** Lower interface
            
            R(ln,1,1)=R(ln,1,1)
     &           + DISNRM*ACO(LN)*CC2*AIRYDL(ln)*pi
            R(ln,3,1)=R(ln,3,1)
     &           + STRNRM*CON1(LN)*CC2*AIRYL(ln)*pi
            r(ln,6,1) = - r(ln,3,1)
            
         else
            
            R(ln-1,1,1)=R(ln-1,1,1)
     &           - DISNRM*ACO(LN)*CC1*AIRYDU(ln)*pi
            R(ln-1,3,1)=R(ln-1,3,1)
     &           - STRNRM*CON1(ln)*CC1*AIRYU(ln)*pi
            r(ln-1,6,1) = - r(ln-1,3,1)
            
C     *** Lower interface

            R(ln,1,1)=R(ln,1,1)
     &           + DISNRM*ACO(LN)*CC2*BIRYDL(ln)*pi
            R(ln,3,1)=R(ln,3,1)
     &           + STRNRM*CON1(ln)*CC2*BIRYL(ln)*pi
            r(ln,6,1) = - r(ln,3,1)
            
         endif
         
       endif

       CALL SOLVE    

       IF (II.LT.ICW1) THEN
        TFAC=0.5*(1E0+COS((II-ICW1)*PI/(ICUT1-ICW1-1)))
       ELSE IF (II.GT.ICW2) THEN
        TFAC=0.5*(1E0+COS((II-ICW2)*PI/(ICUT2-ICW2+1)))
       else
        tfac=1e0
       END IF

c       write(170,*) real(ss(ln-1,3,1)),aimag(ss(ln-1,3,1))

c       write(48,*) tfac*p_conv*real(ss(ln-1,3,1)),
c     &             tfac*p_conv*rimag(ss(ln-1,3,1))

c >>> Compute wavefield potentials
       call wfield()

c >>>  Virtual scattering source contributions        
       if (norcv(ln).gt.0) then
        do int=ifrcv(ln),ilrcv(ln)
         zr=z(int)
c >>>    Upgoing component
         cc1=scatu(wvno,q_z,dlwran,ak(ln,1),
     &     zss,airyss1,biryss1,ztamss,zr,ztam(int),
     &     cc,peta_kx_z,deta_kx_z,nkr,nkz,dpth,gama,pow,ln)
c >>>    Downgoing component
         cc2=scatd(wvno,q_z,dlwran,ak(ln,1),
     &     zss,airyss2,biryss2,ztamss,zr,ztam(int),
     &     cc,peta_kx_z,deta_kx_z,nkr,nkz,dpth,gama,pow,ln)
         
C *** 

         if (laytyp(ln).eq.1) then
          pot(1,int,1) =  pot(1,int,1) + cc2*pi          
          pot(3,int,1) =  pot(3,int,1) + cc1*pi          
         else
          if (real(aco(ln)).lt.0) then
           pot(1,int,1) =  pot(1,int,1) + cc2*AIRY(INT)*pi          
           pot(3,int,1) =  pot(3,int,1) + cc1*BIRY(INT)*pi          
           pot(2,int,1) =  pot(2,int,1) + cc2*AIRYD(INT)*pi          
           pot(4,int,1) =  pot(4,int,1) + cc1*BIRYD(INT)*pi          
          else
           pot(1,int,1) =  pot(1,int,1) + cc2*BIRY(INT)*pi          
           pot(3,int,1) =  pot(3,int,1) + cc1*AIRY(INT)*pi          
           pot(2,int,1) =  pot(2,int,1) + cc2*BIRYD(INT)*pi          
           pot(4,int,1) =  pot(4,int,1) + cc1*AIRYD(INT)*pi          
          endif
         endif
        end do
       end if
      
       IF (IERR.GT.0) RETURN

       IF (DECOMP) THEN
        CALL KERDEC(CFILEK)
       ELSE
        CALL KERNEL(CFILEK)
       END IF
c *** tapering

       IF (II.LT.ICW1) THEN
        CALL VSMUL(CFILEK,1,TFAC,CFILEK,1,NGVALS)
       ELSE IF (II.GT.ICW2) THEN
        CALL VSMUL(CFILEK,1,TFAC,CFILEK,1,NGVALS)
       END IF

c >>> write to tmp files
       do IFC=1,NGFILS
        do ipp=1,npar
         IF (IOUT(ipp).GT.0) THEN
          INDXCF=1+IR*(ipp-1+NPAR*(IFC-1))
          CALL WRBUF(LUOFF+IFC,CFILEK(INDXCF),2*IR)
         END IF
        end do
       end do
      end do

      do IFC=1,NGFILS
       CALL ENFBUF(LUOFF+IFC)
      end do
                   
      return
      end

      SUBROUTINE PV(NKR,RANGE,NKZ,DEPTH,LR,LZ,MODEL,SKEW,cylgeo,iseed)

C      IMPLICIT NONE
      include 'compar.f'
      include 'comvol.f'
      include 'combes.f'

      REAL RHOW,RHOB,RANGE,DEPTH,DR,DZ,DKR,DKZ,MODEL
      REAL SKEW,COS2,SIN2,LR,LRR,LZ,LZZ,LRZ,KR,KZ,FRAC,RMS
      INTEGER J,JJ,K,NKR,NKZ,NKZ_I,NKR_I,IDUM,IDUM2,NN(2),NDIM
      INTEGER INDEX_UPPER
      REAL PK,ran_1,ran_2
      REAL TEMP(nnkr*4)
      real field(nnkr*4),dxfield(nnkr*4),tempz(nnkz*4),dzfield(nnkz*4)
      complex c_field(nnkr*2),c_dxfield(nnkr*2),c_dzfield(nnkr*2)
      equivalence (c_field(1),field(1))
      equivalence (c_dxfield(1),dxfield(1))
      equivalence (c_dzfield(1),dzfield(1))

      real    field_temp(nnkr*4),dxfield_temp(nnkr*4)
      complex c_fld_tmp(nnkr*2),c_dxfld_tmp(nnkr*2)
      equivalence (c_fld_tmp(1),field_temp(1))
      equivalence (c_dxfld_tmp(1),dxfield_temp(1))

      real ononkr,ononkz
      logical cylgeo
      complex extab(nnkr),ex,ex1,ex2
      real sqrtr(nnkr),sqrtp(nnkr)
      complex cfk(nnkr)

C      PARAMETER (PI=3.141592653589793108624468950438)

      REAL RAN2

      REAL GASDEV

      parameter (nwght=2048)
      real wght(nwght)

C     FUNCTION TO RETURN RESONANT SPECTRUM FOR VOLUME SCATTER
C
C     KEVIN D. LEPAGE
C     SACLANTCEN
C     2/2/98
C
C     INPUTS
C
C     NWMAX  LENGTH OF WAVENUMBER INTEGRAL/SPATIAL REALIZATION OF SCATTERERS
C     RANGE  2PI/DK OR LENGTH OF SCATTERER REALIZATION IN METERS
C     DEPTH  DEPTH OF SCATTERER REALIZATION IN METERS
C     LR     RANGE CORRELATION LENGTH SCALE OF SCATTERERS IN METERS
C     LD     DEPTH CORRELATION LENGTH SCALE OF SCATTERERS IN METERS
C     MODEL  2<MODEL<3 FOR GOFF-JORDAN (2-D TO 3-D), NEGATIVE FOR GAUSSIAN
C     SKEW   ANGLE OF MAJOR AXIS AWAY FROM X AXIS (CW, DEGREES)
C
C     RETURNS A DEPTH TIMES HORIZONTAL WAVENUMBER VOLUME SCATTERER 
C     DISTRIBUTION.  ALSO SAVES THE SCATTERER REALIZATION TO THE FILE
C     "volume.out"

      AI=CMPLX(0.0,1.0)

C     COMPUTE SPATIAL SAMPLING INTERVALS

      if (nkr.gt.nnkr) then
         write(6,*) '>>> NKR too large in PV<<<'
         write(*,*) "nkr=",nkr,"max=",nnkr
         stop
      end if

      if (nkz.gt.nnkz) then
         write(6,*) '>>> NKZ too large in PV<<<'
         write(*,*) "nkz=",nkz,"max=",nnkz
         stop
      end if


      DR=RANGE/NKR
         
      DKR=2.0*PI/RANGE

      DZ=DEPTH/NKZ

      DKZ=2.0*PI/DEPTH

      write(6,*) '>>> PV: Volume scattering layer <<<'
      write(6,*) '>>> Patch diameter: ', range,' m'
      write(6,*) '>>> Samples:        ', nkr,' m'
      write(6,*) '>>> Delta r:        ', dr,' m'
      write(6,*) '>>> Patch depth:    ', depth,' m'
      write(6,*) '>>> Samples:        ', nkz,' m'
      write(6,*) '>>> Delta z:        ', dz,' m'
      
C     CREATE REALIZATION OF SOUND SPEED PERTURBATIONS

C     FIRST SET SOME SPECTRAL PARAMETERS

      COS2=COS(SKEW/180.0*PI)**2.0
      
      SIN2=SIN(SKEW/180.0*PI)**2.0

      LRR=LR**2.0*COS2+LZ**2.0*SIN2
      
      LZZ=LZ**2.0*COS2+LR**2.0*SIN2
         
      LRZ=2.0*COS(SKEW/180.0*PI)*SIN(SKEW/180.0*PI)*(LR*LR-LZ*LZ)

C     CHECK FOR TYPE OF SPECTRUM
         
C     FIRST SET SOME DUMMY VARIABLES FOR THE RANDOM NUMBER GENERATOR GASDEV

c      IDUM=-7
c      IDUM2=-9
      idum=iseed
      idum2=idum-2   
      NKR_I=2*NKR

      PPOW=0.0

      DO 100 J=1,NKR_I
         
         DO 90 K=1,NKZ

            IF (J.LE.NKR_I/2) THEN

               KR=(J-1)*DKR
               
            ELSE

               KR=(J-1-NKR_I)*DKR
               
            ENDIF
               
            IF (K.LE.NKZ/2) THEN

               KZ=(K-1)*DKZ
               
            ELSE

               KZ=(K-1-NKZ)*DKZ

            ENDIF

            IF (MODEL.GT.0) THEN
      
               FRAC=MODEL
             
               if (cylgeo) then
        
               frc=exp(gammln((3.0-FRAC)+1.0))/
     C              exp(gammln((3.0-FRAC)-0.5))

               PK=SQRT(LRR*LZ*PI**(-1.5)*frc*(1.0+(KR*KR*LRR+KR*KZ*LRZ
     C              +KZ*KZ*LZZ))**(-(3.0-FRAC)-1.0))

               else

               frc=exp(gammln((3.0-FRAC)+1.0))/
     C              exp(gammln(3.0-FRAC))

               PK=SQRT(LR*LZ/PI*frc*(1.0+(KR*KR*LRR+KR*KZ*LRZ
     C              +KZ*KZ*LZZ))**(-(3.0-FRAC)-1.0))

               endif
                  
            ELSE
                     
               if (cylgeo) then

               PK=SQRT(sqrt(1/2.0/pi)**3.0*lrr*lz*
     C              EXP(-(KR*KR*LRR+KR*KZ*LRZ+KZ*KZ*LZZ)/2.0))

               else

               PK=SQRT(lr*lz/2./pi*
     C              EXP(-(KR*KR*LRR+KR*KZ*LRZ+KZ*KZ*LZZ)/2.0))
                   
               endif
  
            ENDIF
               
            ran_1=GASDEV(IDUM)
               
            ran_2=GASDEV(IDUM2)

            if (cylgeo) then

               if (j.le.nkr) then

                  PPOW=PPOW+KR*PK**2.0*dkr*dkz*2.*pi

               endif

            else

               PPOW=PPOW+PK**2.0*dkr*dkz

            endif

            TEMP_KX_KZ((J-1)*NKZ*2+(K-1)*2+1)=PK*ran_1
            
            TEMP_KX_KZ((J-1)*NKZ*2+(K-1)*2+2)=PK*ran_2
               

 90      CONTINUE
            
 100  CONTINUE

      write(*,*) 'pow=',ppow,'dkz=',dkz

C???      CALL FOURN(TEMP_KX_KZ,NN,NDIM,-1)

      write(*,*) 'calling 2-D fft code'

C     CALL THE 2-D FFT CODE FROM NUMERICAL RECIPIES

C     FIRST SOME CONSTANTS

      NN(1)=NKZ
      NN(2)=NKR_I
      
      NDIM=2
         
      CALL FOURN(TEMP_KX_KZ,NN,NDIM,-1)

      if (cylgeo) then

      DO J=1,NKR_I

         DO K=1,NKZ

               ETA_X_Z((J-1)*NKZ+K)=TEMP_KX_KZ((J-1)*NKZ*2+(K-1)*2+1)
     C           *sqrt(dkr*dkz)/sqrt(2*pi)

         ENDDO

      ENDDO

      else

C     FILL THE REAL PART OF DATA FROM ETA INTO A 2-D ARRAY OF SSP PERTURBATIONS
      
C     NORMALIZE THE ROUGHNESS

      RMS=0.0

      DO 120 J=1,NKR_I

         DO 110 K=1,NKZ

            RMS=RMS+TEMP_KX_KZ((J-1)*NKZ*2+(K-1)*2+1)**2.0

 110     CONTINUE

 120  CONTINUE

      RMS=RMS/(NKR_I*NKZ)

      RMS=SQRT(RMS)

      RMS=1.0
         
      write(*,*) 'NKR=',NKR,', NKR_I=',NKR_I,
     &     ', NKZ=',NKZ,', NKZ_I=',NKZ_I, 'RMS=',RMS

      DO 140 J=1,NKR_I

         DO 130 K=1,NKZ

               ETA_X_Z((J-1)*NKZ+K)=TEMP_KX_KZ((J-1)*NKZ*2+(K-1)*2+1)
     C           *sqrt(dkr*dkz)
     &              /RMS

 130     CONTINUE
         
 140  CONTINUE

      endif

c      write(6,*) 'loop 140 done'

C     if cylindrical geometry prepare first and second order 
C     bessel functions of first kind

      if (cylgeo) then
c 
c >>> For full Bessel integration, prepare weight array.
c >>> First 50% full Bessel. Last 50% Hanning weighted with
c >>> asymptotic Hankel function
c
       write(6,*) 'prepbf. m=',mbf_0
         bwkmax=(10*pi+mbf_0)/range
         brk_0=max(0e0,mbf_0-10.0*pi)
         CALL PREPBF(range,bwkmax)
         
c       write(6,*) 'weights'
         dwght=(rkmax-brk_0)/(nwght-1)
         onodwg=1e0/dwght
         nwghth=nwght/2
         do i=1,nwghth
            wght(i)=1.0
            wght(i+nwghth)=0.5*( 1e0 + cos( ((i-1)*pi)/(nwghth-1) ) )
         end do
         
c     >>> make sqrt(range,wvno) table for full Bessel function compensation
c     write(6,*) 'sqrtr'
         do i=1,nkr
            if (i.eq.1) then
               sqrtr(i)=1E0/sqrt(0.5*dr)
            else
               sqrtr(i)=1E0/sqrt((i-1)*dr)
               sqrtr(nkr_i+2-i)=sqrtr(i)
            end if
            sqrtr(nkr+1)=sqrtr(nkr)
         end do
         
c     write(6,*) 'sqrtr'
         rfk=sqrt(2e0*pi)
         onorfk=1e0/rfk
         do i=1,nkr
            wn=(i-1)*dkr
            if (i.eq.1) then
               cfk(i)=1.0/rfk
               sqrtp(i)=sqrt(wn)
            else
               cfk(i)=cexp(AI*(0.5+mbf_0)*pi*0.5)/rfk
               sqrtp(i)=sqrt(wn)
               cfk(nkr_i+2-i)=cexp(-AI*(0.5+mbf_0)*pi*0.5)/rfk
               sqrtp(nkr_i+2-i)=sqrtp(i)
            end if
            sqrtp(nkr+1)=sqrtp(nkr)
            cfk(nkr+1)=1.0/rfk
         end do
         
c     >>> make exponential table for Hankel compensation
         d_kr=2*pi/nkr_i
         do i=1,nkr_i
            extab(i)=cexpt(ai*(i-1)*d_kr)
         end do      
         
C     get horizontal derivative of the roughness

         do j=1,nkz
            
            write(*,*) 'cylindrical, j=',j,' out of ',nkz
            
c     Symmetric roughness in cylindrical geometry
            
            call vclr(c_field,1,4*nkr)
            c_field(1)=eta_x_z(j)*cmplx(1.0,0.0)
            do jj=2,nkr
               jn=nkr_i+2-jj
               c_field(jj)=0.5*(eta_x_z((jJ-1)*NKZ+j) + 
     &              eta_x_z((jn-1)*NKZ+j) )*cmplx(1.0,0.0)
            enddo
            call vmov(c_field(2),2,c_field(nkr_i),-2,nkr)
            c_field(nkr+1)=c_field(nkr)*cmplx(1.0,0.0)
         
c     transform to kx
c     write(6,*) 'four1 to wn'

            call four1(c_field,nkr_i,1)
c     
c     >>> Integration constants consistent with Hankel transform (dr/sqrt(2pi))
            
            call vsmul(c_field,1,dr/rfk,c_field,1,2*nkr_i)
            
c     Horizontal derivative of roughness
            
            do jj=1,NKR_I/2

               Dxfield((Jj-1)*2+1)=field((jJ-1)*2+2)*(jj-1)*dkr
               Dxfield((Jj-1)*2+2)=-field((jJ-1)*2+1)*(jj-1)*dkr
               
            enddo
            
            do jj=NKR_I/2+1,nkr_i

               Dxfield((Jj-1)*2+1)=field((jJ-1)*2+2)*(jj-1-nkr_i)*dkr
               Dxfield((Jj-1)*2+2)=-field((jJ-1)*2+1)*(jj-1-nkr_i)*dkr
               
            enddo

            
C     transform roughness and its horizontal derivative at depth j back to 
c     spatial domain using Hankel transform
            
c         write(6,*) 'four1 to r'
            
            call vmov(c_field,1,c_fld_tmp,1,2*nkr_i)
C            call cvmul(c_field,2,rfk*cfk,0,c_field,2,nkr_i,1)
            call cvmul(c_field,2,cfk,2,c_field,2,nkr_i,1)
      call cvmul(c_field,2,rfk*cmplx(1.0,0.),0,c_field,2,nkr_i,1)
            
            CALL FOUR1(c_field,nkr_i,-1)
            
            do jr=1,nkr_i
               c_field(jr)=sqrtr(jr)*c_field(jr)
            end do

C     transform horizontal derivative of inc field at depth j to spatial domain
            
            call vmov(c_dxfield,1,c_dxfld_tmp,1,2*nkr_i)
C            call cvmul(c_dxfield,2,rfk*cfk,0,c_dxfield,2,nkr_i,1)
            call cvmul(c_dxfield,2,cfk,2,c_dxfield,2,nkr_i,1)
      call cvmul(c_dxfield,2,rfk*cmplx(1.0,0.),0,c_dxfield,2,nkr_i,1)
         
            CALL FOUR1(c_dxfield,nkr_i,-1)

            do jr=1,nkr_i
               c_dxfield(jr)=sqrtr(jr)*c_dxfield(jr)
            end do

c            write(6,*) j,c_field(2),c_dxfield(2)

c >>> Hankel compensation

            if (1.eq.1) then

            do jr=1,nkr
               rangem=(jr-1)*dr
               indx_e=1
               if (jr.gt.1) then
                  bkmx=rkmax/rangem
                  ibkmx=int(bkmx/dkr)+1
                  do jk=1,min(nkr,ibkmx)
C                  do jk=1,nkr
                     wn=(jk-1)*dkr
                     jkn=nkr_i+2-jk
                     rk=rangem*wn
                     if (rk.lt.brk_0) then
                      awg=1.0
                      c_field(jr)=0e0
                      c_dxfield(jr)=0e0
                     else 
                      if (rk.lt.rkmax) then
                       awg=RINTPL(wght,brk_0,onodwg,nwght,rk)
                       bes_J0= RINTPL(BF(1,1),brk_0,ONODRK,NRKMAX,rk)
                       if (mbf_0.eq.0.or.rk.lt.1e-7) then
                        bes_J1=RINTPL(BF(1,2),brk_0,ONODRK,NRKMAX,rk)
                       else
                        bes_J1=RINTPL(BF(1,2),brk_0,ONODRK,NRKMAX,rk)
     &                       - (mbf_0/rk)*bes_J0
                       end if
                      else
                        awg=0e0
                      end if

                      if (jk.eq.1) then
                        ex1=extab(1)
                        ex2=0e0
                      else
                        ex1=extab(nkr_i+2-indx_e)
                        ex2=cmplx(real(ex1),-aimag(ex1))
                      end if

                      c_field(jr)=c_field(jr) + c_fld_tmp(jk)*awg*rfk*
     &           (sqrtp(jk)*bes_J0-(cfk(jk)*ex1+cfk(jkn)*ex2)*sqrtr(jr))   
                      c_dxfield(jr)=c_dxfield(jr)
     &                              + c_dxfld_tmp(jk)*awg*rfk
     &      *(-ai*sqrtp(jk)*bes_J1-(cfk(jk)*ex1-cfk(jkn)*ex2)*sqrtr(jr))   
                     end if 

                     indx_e=indx_e+(jr-1)
                     if (indx_e.gt.nkr_i) indx_e=indx_e-nkr_i
                     
                  end do
                  
               else if (mbf_0.eq.0) then
                  do jk=1,nkr
                     wn=(jk-1)*dkr
                     if (jk.eq.1) then
                        c_field(jr)=0.5*sqrtp(jk)*c_fld_tmp(jk)*rfk
                        c_dxfield(jr)=0e0
                     else
                c_field(jr)=c_field(jr)+sqrtp(jk)*c_fld_tmp(jk)*rfk
                     end if
                  end do
               else
                c_field(jr)=0e0
                c_dxfield(jr)=0e0
               end if
            end do
         endif

            do jj=1,nkr
               jn=nkr_i+2-jj
               eta_x_z((jJ-1)*NKZ+j)=field((jj-1)*2+1)*dkr
               eta_dx_z((jJ-1)*NKZ+j)=dxfield((jj-1)*2+1)*dkr
               if (jj.gt.1) then
                  eta_x_z((jn-1)*NKZ+j)=eta_x_z((jj-1)*nkz+j)
                  eta_dx_z((jn-1)*NKZ+j)=-eta_dx_z((jj-1)*nkz+j)
               end if
            enddo
c               write(6,*) eta_x_z(j+nkz),eta_dx_z(j+nkz)
            eta_x_z(nkr*nkz+j)=field(nkr*2+1)*dkr
            eta_dx_z(nkr*nkz+j)=0e0
            
         end do
         
      else
c >>> Plane geometry
         do j=1,nkz

         write(*,*) 'Plane, j=',j,' out of ',nkz

         do jj=1,nkr_i

            field((jj-1)*2+1)=real(eta_x_z((jJ-1)*NKZ+j))
            field((jj-1)*2+2)=0.0
            
         enddo
         
c     transform to kx

         call four1(field,nkr_i,1)

C     transform horizontal derivative of roughness at depth j back to 
c     spatial domain

            do jj=1,NKR_I/2

               Dxfield((Jj-1)*2+1)=field((jJ-1)*2+2)*(jj-1)*dkr
               Dxfield((Jj-1)*2+2)=-field((jJ-1)*2+1)*(jj-1)*dkr
            
            enddo

            do jj=NKR_I/2+1,nkr_i

               Dxfield((Jj-1)*2+1)=field((jJ-1)*2+2)*(jj-1-nkr_i)*dkr
               Dxfield((Jj-1)*2+2)=-field((jJ-1)*2+1)*(jj-1-nkr_i)*dkr
               
            enddo

C     transfrom back to space

            call four1(dxfield,nkr_i,-1)

C     fill in eta_dx_z
            
            ononkr=1e0/nkr_i
            
c     write(6,*) 'four1, nkr, ononkr=', nkr_i, ononkr

            do jj=1,nkr_i

               eta_dx_z((jJ-1)*NKZ+j)=dxfield((jj-1)*2+1)*ononkr
               
C     write(82,*)  eta_dx_z((jJ-1)*NKZ+j)
            
            enddo

       end do
      end if
      
C      do j=1,nkz

C         do jj=1,nkr
C            jn=nkr_i+2-jj
C            eta_x_z((jJ-1)*NKZ+j)=0.0
C            eta_dx_z((jJ-1)*NKZ+j)=0.0

C            if (jj.gt.1) then
C               eta_x_z((jn-1)*NKZ+j)=0.0
C               eta_dx_z((jn-1)*NKZ+j)=0.0
C            end if

C            if (j.eq.NKZ/2) then
                  
C               if (jj.eq.2) then
               
C                  eta_x_z((jJ-1)*NKZ+j)=1.0
C                  eta_x_z((jn-1)*NKZ+j)=1.0
C                  write(*,*) 'j=',j,', jj=',jj
                  
C               endif
               
C            endif

C         enddo
C         eta_x_z(nkr*nkz+j)=0.0
C         eta_dx_z(nkr*nkz+j)=0.0
         
C      end do
      
c     get vertical derivative of roughness

      do jj=1,nkr_i

         do j=1,nkz

            tempz((j-1)*2+1)=real(eta_x_z((jJ-1)*NKZ+j))
            tempz((j-1)*2+2)=0.0
            
         enddo

c     transform to kz

         call four1(tempz,nkz,1)

c     write(6,*) 'four1, nkz=', nkz

         do j=1,nkz/2

            dzfield((j-1)*2+1)=tempz((j-1)*2+2)*(j-1)*dkz
            dzfield((j-1)*2+2)=-tempz((j-1)*2+1)*(j-1)*dkz
            
            dzfield((nkz-j)*2+1)=-tempz((nkz-j)*2+2)*j*dkz
            dzfield((nkz-j)*2+2)=tempz((nkz-j)*2+1)*j*dkz
            
         enddo
         
C     transfrom back to space
         
         call four1(dzfield,nkz,-1)

c     write(6,*) 'four1, nkz=', nkz

C     fill in eta_x_dz
         
         ononkz=1e0/nkz

c     write(6,*) 'four1, nkz, ononkz=', nkz, ononkz

         do j=1,nkz

C            eta_x_dz((jJ-1)*NKZ+j)=dzfield((nkz-j)*2+1)*ononkz

            eta_x_dz((jJ-1)*NKZ+j)=dzfield((j-1)*2+1)*ononkz

            write(80,*)  eta_x_z((jJ-1)*NKZ+j)

C            write(81,*)  eta_x_dr((jJ-1)*NKZ+j)

C            write(82,*)  eta_x_dz((jJ-1)*NKZ+j)

         enddo

      enddo
      close(80,status='keep')

      END


      COMPLEX FUNCTION SCATU(Q,QBZ,DK,KB,zss,
     C     AIRYSS,BIRYSS,ztamss,zr,ztamr,C_C,
     C     ETA_KX_Z,DETA_KX_Z,NKR,NKZ,DEPTH,gama,POW,LN)

      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'

      COMPLEX Q,QWZ,KWZ,I,SCATT,CCT
      COMPLEX KW,KB,QBZ,AMPU,AMPD,D_SCATT,dzoiqz
      COMPLEX D_CCT
      COMPLEX EXPS
      COMPLEX C_C(NKZ)
      COMPLEX AIRYSS(NKZ),BiRYSS(NKZ),ztamss(nkz),ztamr
      REAL DZ,DKZ,DEPTH,KZ_RESU,GAMA,zr,zss(nkz)
      REAL KR_RES,KZ_RES,POW,DK,RCONJ,ZCONJ,ZUCONJ
      INTEGER NKR,NKZ,BIN,BINZ,J,NKZR,NKZ_I,QI,KI,MECH,D_FLAG,BINZU
      COMPLEX ETA_KX_Z(NKZ*NKR*2),DETA_KX_Z(NKZ*NKR*2)
      COMPLEX COND,CONU,CONDZ,CONUZ,cc
      INTEGER LN,q_bin

C     FUNCTION TO RETURN UPGOING WAVE AMPLITUDE FOR VOLUME SCATTER
C
C     KEVIN D. LEPAGE
C     SACLANTCEN
C     7/3/99

c      write(6,*) 'entering SCATU'
c      write(6,*) 'zr=',zr
c      write(6,*) 'ztamr=',ztamr
c      write(6,*) 'ln=',ln

C     DETERMINE THE VERTICAL WAVENUMBER INCREMENT

      DKZ=2.0*PI/DEPTH

      DZ=DEPTH/NKZ     

C     INTERPOLATED INDICIES

      NKZ_I=NKZ

C     FIND THE RELEVANT HORIZONTAL WAVENUMBER BIN

      q_bin=((q+dk/10.)/dk)+1


      if (q_bin.lt.1) then

C         write(*,*) 'warning: qbin=',q_bin

         BIN=nkr*2+q_bin

      else
         
         bin=q_bin

      endif

C      write(6,*) 'q=',q,'dk=',dk,'bin=',bin

c >>> 1/4pi for the Green's function applied here HS 991211

      ono4pi=1e0/(4.0*pi)

      IF (LAYTYP(LN).EQ.1) THEN

         SCATT=CMPLX(0.0,0.0)
      
         D_SCATT=CMPLX(0.0,0.0)

C     DO THE DEPTH INTEGRAL

c >>> Depth-dependent Green's function for point source

         dzoiqz=ono4pi*dz/(ai*qbz)

         do J=1,NKZ
          if (zr.lt.zss(j)) then
            cc=ai*(zr-zss(j))*qbz
            SCATT=SCATT+ETA_KX_Z(J+(bin-1)*nkz_i)*
     c           cexpt(cc)*dzoiqz
            D_SCATT=D_SCATT+(deta_kx_z(j+(bin-1)*nkz_i))*
     c           cexpt(cc)*dzoiqz
          end if
         end do

      ELSEIF (LAYTYP(LN).EQ.2) THEN
         
C     WRITE(*,*) 'VOLUME SCATTERING FROM AIRY LAYER'
         
C     DO THE DEPTH INTEGRAL

        SCATT=CMPLX(0.0,0.0)
         
        D_SCATT=CMPLX(0.0,0.0)
        
        IF (REAL(ACO(LN)).LT.0) THEN
            
         do J=1,NKZ
          if (zr.lt.zss(j)) then
            
            CCT=ono4pi*C_C(J)*AIRYSS(J)*cexpt(ztamr-ztamss(j))
            SCATT=SCATT+ETA_KX_Z(J+(bin-1)*nkz_i)*CCT*DZ

            D_CCT=ono4pi*C_C(J)*AIRYSS(J)*cexpt(ztamr-ztamss(j))
            D_SCATT=D_SCATT+(dETA_KX_Z(J+(bin-1)*nkz_i))*D_CCT*DZ
          end if
         end do            
         
        ELSE
         
         do J=1,NKZ
          if (zr.lt.zss(j)) then
            CCT=ono4pi*C_C(J)*BIRYSS(J)*cexpt(ztamss(j)-ztamr)
            SCATT=SCATT+ETA_KX_Z(J+(bin-1)*nkz_i)*CCT*DZ

            D_CCT=ono4pi*C_C(J)*BIRYSS(J)*cexpt(ztamss(j)-ztamr)
            D_SCATT=D_SCATT+(dETA_KX_Z(J+(bin-1)*nkz_i))*D_CCT*DZ
          end if
         end do            

        ENDIF
         
      ENDIF
      
      SCATU=(2.0*KB*KB*(SCATT)+GAMA*2.0*D_SCATT)*POW
         
      RETURN 
      
      END

      COMPLEX FUNCTION SCATD(Q,QBZ,DK,KB,zss,
     C     AIRYSS,BIRYSS,ztamss,zr,ztamr,C_C,
     C     ETA_KX_Z,dETA_KX_Z,NKR,NKZ,DEPTH,gama,POW,LN)

      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'

      COMPLEX Q,QWZ,KWZ,SCATT,CCT
      COMPLEX KW,KB,QBZ,AMPU,AMPD,D_SCATT,dzoiqz
      COMPLEX D_CCT
      COMPLEX C_C(NKZ)
      COMPLEX AIRYSS(NKZ),BiRYSS(NKZ),ztamss(nkz),ztamr
      REAL DZ,DKZ,DEPTH,KZ_RESU,GAMA,zr,zss(nkz)
      REAL KR_RES,KZ_RES,POW,DK,RCONJ,ZCONJ,ZUCONJ
      INTEGER NKR,NKZ,BIN,BINZ,J,NKZR,NKZ_I,QI,KI,MECH,D_FLAG,BINZU
      COMPLEX ETA_KX_Z(NKZ*NKR*2),dETA_KX_Z(NKZ*NKR*2)
      COMPLEX COND,CONU,CONDZ,CONUZ
      INTEGER LN,q_bin

C     FUNCTION TO RETURN UPGOING WAVE AMPLITUDE FOR VOLUME SCATTER
C
C     KEVIN D. LEPAGE
C     SACLANTCEN
C     7/3/99

C     DETERMINE THE VERTICAL WAVENUMBER INCREMENT

      DKZ=2.0*PI/DEPTH

      DZ=DEPTH/NKZ     

C     INTERPOLATED INDICIES

      NKZ_I=NKZ

C     FIND THE RELEVANT HORIZONTAL WAVENUMBER BIN

      q_bin=((q+dk/10.)/dk)+1

      if (q_bin.lt.1) then

         BIN=nkr*2+q_bin

      else
         
         bin=q_bin

      endif

c >>> 1/(4 pi) for the Green's function applied here HS 991211
      ono4pi=1e0/(4.0*pi)

      IF (LAYTYP(LN).EQ.1) THEN

         SCATT=CMPLX(0.0,0.0)
      
         D_SCATT=CMPLX(0.0,0.0)

C     DO THE DEPTH INTEGRAL

         dzoiqz=ono4pi*dz/(ai*qbz)

         do J=1,NKZ
          if (zr.ge.zss(j)) then
            SCATT=SCATT+ETA_KX_Z(J+(bin-1)*nkz_i)*
     &           cexpt(ai*(zss(j)-zr)*qbz)*dzoiqz
            D_SCATT=D_SCATT+(dETA_KX_Z(J+(bin-1)*nkz_i))*
     &           cexpt(ai*(zss(j)-zr)*qbz)*dzoiqz
          end if
         end do

      ELSEIF (LAYTYP(LN).EQ.2) THEN
         
C     WRITE(*,*) 'VOLUME SCATTERING FROM AIRY LAYER'
         
C     DO THE DEPTH INTEGRAL
         
        SCATT=CMPLX(0.0,0.0)
         
        D_SCATT=CMPLX(0.0,0.0)
         
        IF (REAL(ACO(LN)).LT.0) THEN

         do J=1,NKZ
          if (zr.ge.zss(j)) then
            CCT=ono4pi*C_C(J)*BIRYSS(J)*cexpt(ztamss(j)-ztamr)
            SCATT=SCATT+ETA_KX_Z(J+(bin-1)*nkz_i)*CCT*DZ

            D_CCT=ono4pi*C_C(J)*BIRYSS(J)*cexpt(ztamss(j)-ztamr)
            D_SCATT=D_SCATT+(dETA_KX_Z(J+(bin-1)*nkz_i))*D_CCT*DZ
          end if            
         end do         

        ELSE

         do J=1,NKZ
          if (zr.ge.zss(j)) then
            CCT=ono4pi*C_C(J)*AIRYSS(J)*cexpt(ztamr-ztamss(j))
            SCATT=SCATT+ETA_KX_Z(J+(bin-1)*nkz_i)*CCT*DZ

            D_CCT=ono4pi*C_C(J)*AIRYSS(J)*cexpt(ztamr-ztamss(j))
            D_SCATT=D_SCATT+(dETA_KX_Z(J+(bin-1)*nkz_i))*D_CCT*DZ
          end if            
         end do
        ENDIF
         
      ENDIF
      
      SCATD=(2.0*KB*KB*(SCATT)+GAMA*2.0*D_SCATT)*POW
         
      RETURN 
      
      END

      SUBROUTINE four1(data,nn,isign)
      INTEGER isign,nn
      REAL data(2*nn)
      INTEGER i,istep,j,m,mmax,n
      REAL tempi,tempr
      DOUBLE PRECISION theta,wi,wpi,wpr,wr,wtemp
      n=2*nn
      j=1
      do 11 i=1,n,2
        if(j.gt.i)then
          tempr=data(j)
          tempi=data(j+1)
          data(j)=data(i)
          data(j+1)=data(i+1)
          data(i)=tempr
          data(i+1)=tempi
        endif
        m=n/2
1       if ((m.ge.2).and.(j.gt.m)) then
          j=j-m
          m=m/2
        goto 1
        endif
        j=j+m
11    continue
      mmax=2
2     if (n.gt.mmax) then
        istep=2*mmax
        theta=6.28318530717959d0/(isign*mmax)
        wpr=-2.d0*sin(0.5d0*theta)**2
        wpi=sin(theta)
        wr=1.d0
        wi=0.d0
        do 13 m=1,mmax,2
          do 12 i=m,n,istep
            j=i+mmax
            tempr=sngl(wr)*data(j)-sngl(wi)*data(j+1)
            tempi=sngl(wr)*data(j+1)+sngl(wi)*data(j)
            data(j)=data(i)-tempr
            data(j+1)=data(i+1)-tempi
            data(i)=data(i)+tempr
            data(i+1)=data(i+1)+tempi
12        continue
          wtemp=wr
          wr=wr*wpr-wi*wpi+wr
          wi=wi*wpr+wtemp*wpi+wi
13      continue
        mmax=istep
      goto 2
      endif
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software 0H21.

      SUBROUTINE fourn(data,nn,ndim,isign)
      INTEGER isign,ndim,nn(ndim)
      REAL data(*)
      INTEGER i1,i2,i2rev,i3,i3rev,ibit,idim,ifp1,ifp2,ip1,ip2,ip3,k1,
     *k2,n,nprev,nrem,ntot
      REAL tempi,tempr
      DOUBLE PRECISION theta,wi,wpi,wpr,wr,wtemp
      ntot=1
      do 11 idim=1,ndim
        ntot=ntot*nn(idim)
11    continue
      nprev=1
      do 18 idim=1,ndim
        n=nn(idim)
        nrem=ntot/(n*nprev)
        ip1=2*nprev
        ip2=ip1*n
        ip3=ip2*nrem
        i2rev=1
        do 14 i2=1,ip2,ip1
          if(i2.lt.i2rev)then
            do 13 i1=i2,i2+ip1-2,2
              do 12 i3=i1,ip3,ip2
                i3rev=i2rev+i3-i2
                tempr=data(i3)
                tempi=data(i3+1)
                data(i3)=data(i3rev)
                data(i3+1)=data(i3rev+1)
                data(i3rev)=tempr
                data(i3rev+1)=tempi
12            continue
13          continue
          endif
          ibit=ip2/2
1         if ((ibit.ge.ip1).and.(i2rev.gt.ibit)) then
            i2rev=i2rev-ibit
            ibit=ibit/2
          goto 1
          endif
          i2rev=i2rev+ibit
14      continue
        ifp1=ip1
2       if(ifp1.lt.ip2)then
          ifp2=2*ifp1
          theta=isign*6.28318530717959d0/(ifp2/ip1)
          wpr=-2.d0*sin(0.5d0*theta)**2
          wpi=sin(theta)
          wr=1.d0
          wi=0.d0
          do 17 i3=1,ifp1,ip1
            do 16 i1=i3,i3+ip1-2,2
              do 15 i2=i1,ip3,ifp2
                k1=i2
                k2=k1+ifp1
                tempr=sngl(wr)*data(k2)-sngl(wi)*data(k2+1)
                tempi=sngl(wr)*data(k2+1)+sngl(wi)*data(k2)
                data(k2)=data(k1)-tempr
                data(k2+1)=data(k1+1)-tempi
                data(k1)=data(k1)+tempr
                data(k1+1)=data(k1+1)+tempi
15            continue
16          continue
            wtemp=wr
            wr=wr*wpr-wi*wpi+wr
            wi=wi*wpr+wtemp*wpi+wi
17        continue
          ifp1=ifp2
        goto 2
        endif
        nprev=n*nprev
18    continue
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software 0H21.

      FUNCTION gasdev(idum)
      INTEGER idum
      REAL gasdev
CU    USES ran1
      INTEGER iset
      REAL fac,gset,rsq,v1,v2,ran1
      SAVE iset,gset
      DATA iset/0/
      if (iset.eq.0) then
1       v1=2.*ran1(idum)-1.
        v2=2.*ran1(idum)-1.
        rsq=v1**2+v2**2
        if(rsq.ge.1..or.rsq.eq.0.)goto 1
        fac=sqrt(-2.*log(rsq)/rsq)
        gset=v1*fac
        gasdev=v2*fac
        iset=1
      else
        gasdev=gset
        iset=0
      endif
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software 0H21.

      FUNCTION ran1(idum)
      INTEGER idum,IA,IM,IQ,IR,NTAB,NDIV
      REAL ran1,AM,EPS,RNMX
      PARAMETER (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836,
     *NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=1.2e-7,RNMX=1.-EPS)
      INTEGER j,k,iv(NTAB),iy
      SAVE iv,iy
      DATA iv /NTAB*0/, iy /0/
      if (idum.le.0.or.iy.eq.0) then
        idum=max(-idum,1)
        do 11 j=NTAB+8,1,-1
          k=idum/IQ
          idum=IA*(idum-k*IQ)-IR*k
          if (idum.lt.0) idum=idum+IM
          if (j.le.NTAB) iv(j)=idum
11      continue
        iy=iv(1)
      endif
      k=idum/IQ
      idum=IA*(idum-k*IQ)-IR*k
      if (idum.lt.0) idum=idum+IM
      j=1+iy/NDIV
      iy=iv(j)
      iv(j)=idum
      ran1=min(AM*iy,RNMX)
      return
      END
C  (C) Copr. 1986-92 Numerical Recipes Software 0H21.

      FUNCTION gammln(xx)
      REAL gammln,xx
      INTEGER j
      DOUBLE PRECISION ser,stp,tmp,x,y,cof(6)
      SAVE cof,stp
      DATA cof,stp/76.18009172947146d0,-86.50532032941677d0,
     *24.01409824083091d0,-1.231739572450155d0,.1208650973866179d-2,
     *-.5395239384953d-5,2.5066282746310005d0/
      x=xx
      y=x
      tmp=x+5.5d0
      tmp=(x+0.5d0)*log(tmp)-tmp
      ser=1.000000000190015d0
      do 11 j=1,6
        y=y+1.d0
        ser=ser+cof(j)/y
11    continue
      gammln=tmp+log(stp*ser/x)
      return
      END
