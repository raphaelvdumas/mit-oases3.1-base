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

      if (nkmean.gt.nnkr) then
       write(6,*) '>>> NKMEAN too large in CALSVC<<<'
       write(*,*) "nkmean=",nkmean,"max=",nnkr
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
       dwght=rkmax/(nwght-1)
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

       cfk=cexp(cmplx_i*pi*0.25)/sqrt(pi*2E0)*dk_inc
       do i=1,nkmean
         wn=k_r(1)+(i-1)*dk_inc
         sqrtk(i)=cfk*sqrt(wn)
       end do

       cfr=cexp(-cmplx_i*pi*0.25)/sqrt(pi*2E0)*dlr
       do i=1,nkr
        sqrtr(i)=cfr*sqrt((i-1)*dlr)
        if (i.gt.1) then
         sqrtr(nkr_i+2-i)=sqrtr(i)
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

C     build up horizontal derivative of cylindrically propagate incident field

               c_dxfield(nnk)= - cmplx_I*k_r(nnk)*
     &           ( ampd(nnk)*AIRYID + ampu(nnk)*BIRYIU )

C     build up vertical derivative of cylindrically propagating incident field

               c_dzfield(nnk)=
     &           -aco(ln)*( ampd(nnk)*AIRYDID + ampu(nnk)*BIRYDIU )


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

               c_dxfield(nnk)=- cmplx_i*k_r(nnk)*
     &           ( ampd(nnk)*BIRYID + ampu(nnk)*AIRYIU )

C     build up vertical derivative of cylindrically propagating incident field

               c_dzfield(nnk)=
     &          - aco(ln)*( ampd(nnk)*BIRYDID + ampu(nnk)*AIRYDIU )

            endif

          end do
         else
          do nnk=1,nkmean
           EXPID=CEXPT(-CMPLX_I*K_Z(NNK)*(J-0.5)*DZ)
           EXPIU=CEXPT(-CMPLX_I*K_Z(NNK)*(DPTH-(J-0.5)*DZ))

C     build up incident field

           c_field(nnk)= (ampd(nnk)*EXPID + ampu(nnk)*EXPIU)

C     build up horizontal derivative of incident field

           c_dxfield(nnk)= (-cmplx_i)*k_r(nnk)*
     c              ( ampd(nnk)*EXPID + ampu(nnk)*EXPIU )

C     build up vertical derivative of incident field

           c_dzfield(nnk)= cmplx_i*k_z(nnk)*
     c              ( -ampd(nnk)*EXPID + ampu(nnk)*EXPIU )

          end do
         end if

C     transform incident field at depth j to spatial domain
         
         call vmov(c_field,1,c_fld_tmp,1,2*nkr_i)
         call cvmul(c_field,2,sqrtk,2,c_field,2,nkmean,1)

         CALL FOUR1(c_field,nkr_i,-1)

C     transform horizontal derivative of inc field at depth j to spatial domain

         call vmov(c_dxfield,1,c_dxfld_tmp,1,2*nkr_i)
         call cvmul(c_dxfield,2,sqrtk,2,c_dxfield,2,nkmean,1)

         CALL FOUR1(c_dxfield,nkr_i,-1)

C     transform vertical derivative of inc field at depth j to spatial domain

         call vmov(c_dzfield,1,c_dzfld_tmp,1,2*nkr_i)
         call cvmul(c_dzfield,2,sqrtk,2,c_dzfield,2,nkmean,1)

         CALL FOUR1(c_dzfield,nkr_i,-1)

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
             awg=RINTPL(wght,0E0,onodwg,nwght,rk)
            else
             awg=0e0
            end if
            bes_J0= RINTPL(BF(1,1),0E0,ONODRK,NRKMAX,rk)
            bes_J1= RINTPL(BF(1,2),0E0,ONODRK,NRKMAX,rk)
            if (jk.eq.1) then
             ex=ex1*extab(1)
            else
             ex=ex1*extab(nkr_i+2-indx_e)
            end if
c            ex=cexpt(cmplx(0e0,-rk))
            c_field(jr)=c_field(jr) + c_fld_tmp(jk)*awg*
     &       ( wn*bes_J0*dk_inc - sqrtk(jk)*ex*sqrtri(jr))   
            c_dxfield(jr)=c_dxfield(jr) + c_dxfld_tmp(jk)*awg*
     &        (-ai*wn*bes_J1*dk_inc - sqrtk(jk)*ex*sqrtri(jr))   
            c_dzfield(jr)=c_dzfield(jr) + c_dzfld_tmp(jk)*awg*
     &       ( wn*bes_J0*dk_inc - sqrtk(jk)*ex*sqrtri(jr))   

           indx_e=indx_e+(jr-1)
           if (indx_e.gt.nkr_i) indx_e=indx_e-nkr_i

           end do

          else
           do jk=1,nkmean
            wn=k_r(1)+(jk-1)*dk_inc
            if (jk.eq.1) then
             c_field(jr)=0.5*wn*c_fld_tmp(jk)*dk_inc
             c_dxfield(jr)=0e0
             c_dzfield(jr)=0.5*wn*c_dzfld_tmp(jk)*dk_inc
            else
             c_field(jr)=c_field(jr) + wn*dk_inc*c_fld_tmp(jk)
             c_dzfield(jr)=c_dzfield(jr) + wn*dk_inc*c_dzfld_tmp(jk)
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
         cfield(nkr+1)=cfield(nkr)

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
         cdxfield(nkr+1)=cdxfield(nkr)

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
         cdzfield(nkr+1)=cdzfield(nkr)

c     multiply roughness in spatial domain by incident field
c     and extract symmetric component for cylindrical geometry
c       write(6,*) '      Multiply by roughness'
            jj=1
            c_field(jj)=ETA_x_Z((jJ-1)*NKZ+j)*
     c           cfield(jj)
         do jj=2,nkr
            jn=nkr_i+2-jj
            c_field(jj)=0.5*
     &       ( ETA_x_Z((jJ-1)*NKZ+j)*cfield(jj) +
     &         ETA_x_Z((jn-1)*NKZ+j)*cfield(jn) )
            c_field(jn)=c_field(jj)
         enddo
         c_field(nkr+1)=c_field(nkr)

c     transform product of incident field and roughness to q domain

c       write(6,*) '      Transform to q domain'

         call vmov(c_field,1,c_fld_tmp,1,2*nkr_i)
         call cvmul(c_field,2,sqrtr,2,c_field,2,nkr_i,1)

         CALL FOUR1(c_field,NKR_I,1)

         do jq=1,nkr_i
          c_field(jq)=sqrtq(jq)*c_field(jq)
         end do

c     multiply horizontal derivative of roughness in spatial domain 
C     by horizontal derivative of incident field
c     and extract symmetric component for cylindrical geometry
c       write(6,*) '      Multiply dx by roughness'

            jj=1
            c_dxfield(jj)=ETA_dx_Z((jJ-1)*NKZ+j)*
     c           cdxfield(jj)
         do jj=2,nkr
            jn=nkr_i+2-jj
            c_dxfield(jj)=0.5*
     &       ( ETA_dx_Z((jJ-1)*NKZ+j)*cdxfield(jj) +
     &         ETA_dx_Z((jn-1)*NKZ+j)*cdxfield(jn) )
            c_dxfield(jn)=c_dxfield(jj)
         end do
         c_dxfield(nkr+1)=c_dxfield(nkr)

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

            jj=1
            c_dzfield(jj)=eta_x_dz((jJ-1)*NKZ+j)*
     c           cdzfield(jj)
         do jj=2,nkr
            jn=nkr_i+2-jj
            c_dzfield(jj)=0.5*
     &       ( eta_x_dz((jJ-1)*NKZ+j)*cdzfield(jj) +
     &         eta_x_dz((jn-1)*NKZ+j)*cdzfield(jn) )
            c_dzfield(jn)=c_dzfield(jj)
         end do
         c_dzfield(nkr+1)=c_dzfield(nkr)


         do jj=1,nkr_i

            c_dzfield(jj)=eta_x_dz((jj-1)*nkz+j)*
     c           cdzfield(jj)

         enddo

c     transform product of incident field and roughness back to q domain
c       write(6,*) '      Transform dz to q domain'

         call vmov(c_dzfield,1,c_dzfld_tmp,1,2*nkr_i)
         call cvmul(c_dzfield,2,sqrtr,2,c_dzfield,2,nkr_i,1)

         CALL FOUR1(c_dzfield,NKR_I,1)

         do jq=1,nkr_i
          c_dzfield(jq)=sqrtq(jq)*c_dzfield(jq)
         end do

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
            if (rk.lt.rkmax) then
             awg=RINTPL(wght,0E0,onodwg,nwght,rk)
            else
             awg=0e0
            end if
            bes_J0= RINTPL(BF(1,1),0E0,ONODRK,NRKMAX,rk)
            ex1=extab(indx_e)
c            ex1=cexpt(cmplx(0E0,rk))
            if (jr.gt.1) then
              ex2=extab(nkr_i+2-indx_e)
c             ex2=cexpt(cmplx(0E0,-rk))
            else
             ex2=0e0
            end if 
            c_field(jq)=c_field(jq) + c_fld_tmp(jr)*awg*
     &        ( rangem*bes_J0*dlr - sqrtr(jr)*(ex1+ex2)*sqrtq(jq))   
            c_dxfield(jq)=c_dxfield(jq) + c_dxfld_tmp(jr)*awg*
     &        ( rangem*bes_J0*dlr - sqrtr(jr)*(ex1+ex2)*sqrtq(jq))   
            c_dzfield(jq)=c_dzfield(jq) + c_dzfld_tmp(jr)*awg*
     &        ( rangem*bes_J0*dlr - sqrtr(jr)*(ex1+ex2)*sqrtq(jq))   

            indx_e=indx_e+(jq-1)
            if (indx_e.gt.nkr_i) indx_e=indx_e-nkr_i

           end do
          else
           do jr=1,nkr
            rangem=(jr-1)*dlr
            if (jr.eq.1) then
             c_field(jq)=0.5*rangem*c_fld_tmp(jr)*dlr
             c_dxfield(jq)=0.5*rangem*c_dxfld_tmp(jr)*dlr
             c_dzfield(jq)=0.5*rangem*c_dzfld_tmp(jr)*dlr
            else
             c_field(jq)=c_field(jq) + rangem*dlr*c_fld_tmp(jr)
             c_dxfield(jq)=c_dxfield(jq) + rangem*dlr*c_dxfld_tmp(jr)
             c_dzfield(jq)=c_dzfield(jq) + rangem*dlr*c_dzfld_tmp(jr)
            end if
           end do
          end if
         end do
c         write(6,*) 'Hankel done'

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
      

      write(6,*) '>>> CALSVC: Calling SOLSV <<<'
      call SOLSV(ln,dlmean,dlwran,nkr,dpth,nkz,gama,pow)

      RETURN        
                   
      END           

