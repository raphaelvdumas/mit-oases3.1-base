      SUBROUTINE INENVI
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
C
C     SUBROUTINE FOR READING IN ENVIRONMENTAL DATA
C
Cms
Cms **** Subroutine modified and updated to include porous sediment
Cms **** (BIOT) layers  4/26/96
Cms
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comti.f'
      INCLUDE 'comnrd.f'

      integer ladd, lcnt
      real arctic_tlc
C           
C ********************FORMATS*******************               
C           
C           
 350  FORMAT(1H0,'     DEPTH        ALPHA       BETA      ATTENA       AT        
     1TENB         RHO      ROUGHNESS')
 351  FORMAT(1H ,3F12.5,2F12.8,2F12.5)              
C           
C           
C **********************************************               
C           
C     DEFAULT: ISOTROPIC LAYERS
C
      NTISOL=0
      idlmax=0
C           
C     NUML IS THE NUMBER OF LAYERS INCLUDING THE HALF SPACES.  
C     NUMI IS THE NUMBER OF INTERFACES        
      DO 1 I=1,NLTYP
 1    NUMT(I)=0
      READ(1,*) NUML       
      IF (NUML.GT.NLA) THEN
      WRITE(6,*) '*** TOO MANY LAYERS ***'
      STOP
      END IF
      NUMI=NUML-1
      WRITE (6,350)
C
C     READ IN ENVIRONMENTAL DATA
C
      ladd = 0
      DO 110 M=1,NUML
       lcnt= M + ladd            
       READ(1,*) (V(lcnt,N),N=1,6),ROUGH(lcnt)
c >>> check for layer sequence consistency
       if (lcnt.le.2) then
        v(1,1)=v(2,1)
       else if (v(lcnt,1).lt.v(lcnt-1,1)) then
        write(6,*) '>>> Layer sequence error, layer:', m
        stop
       end if
c >>> check for flow
       if (abs(v(lcnt,5)+888.888).lt. 1e-3) then
        flow_flag(lcnt)=.true.
        flow_vel(lcnt)=v(lcnt,5)
        v(lcnt,5)=0
       else
        flow_flag(lcnt)=.false.
       end if
c
      disper(lcnt)=.false.
      eof_layer(m)=.false.

      if (rough(lcnt).lt.-1e-10) then
C     BACKSPACE AND READ ALSO CORRELLATION LENGTH
       BACKSPACE(1)
       READ(1,*) (V(lcnt,N),N=1,6),ROUGH(lcnt),CLEN(lcnt)
c >>> Volume scattering
       if (clen(lcnt).lt.-1e-10) then
        nvol=6
        backspace(1)
        READ(1,*) (V(lcnt,N),N=1,6),(fac(j+(lcnt-1)*nvol),j=1,nvol) 
        WRITE(6,351) (V(lcnt,N),N=1,6)
        write(6,*) '>>> Volume scattering layer <<<'
        write(6,'(a,f6.1,a)') '>>> L_z  = ',-fac(1+(lcnt-1)*nvol),' m'
        write(6,'(a,f6.1,a)') '>>> L_x  = ',-fac(2+(lcnt-1)*nvol),' m'
        write(6,'(a,f6.1,a)') '>>> Skew = ', fac(3+(lcnt-1)*nvol),' deg'
        write(6,'(a,f6.1,a)') '>>> Exp  = ', fac(4+(lcnt-1)*nvol),'  '
        write(6,'(a,f6.1,a)') '>>> dc/c = ', fac(5+(lcnt-1)*nvol),'  '
        write(6,'(a,f6.1,a)') '>>> Gamm = ', fac(6+(lcnt-1)*nvol),'  '
        amod(lcnt)=fac(4+(lcnt-1)*nvol)
       else
        nvol=3
        backspace(1)
        READ(1,*) (V(lcnt,N),N=1,6),(fac(j+(lcnt-1)*nvol),j=1,nvol) 
        WRITE(6,351) (V(lcnt,N),N=1,6),ROUGH(lcnt)
        write(6,*) '>>> Rough Interface <<<'
        write(6,*) '>>> Rms = ',abs(rough(lcnt))
        WRITE(6,*) '>>> L_x = ',CLEN(lcnt)
        write(6,*) '>>> Exp = ',fac(3+(lcnt-1)*nvol)
        amod(lcnt)=fac(3+(lcnt-1)*nvol)
       end if
      else
       clen(lcnt)=0e0
       if (goff) then
        amod(lcnt)=2.5
       else
        amod(lcnt)=-1.0
       end if
       WRITE(6,351) (V(lcnt,N),N=1,6),ROUGH(lcnt)
      end if

      IF (CLEN(lcnt).EQ.0.0) THEN
C     NON-KIRCHHOFF SCATTERING
         CLEN(lcnt)=1E6
      END IF
      if (flow_flag(lcnt)) then
       read(1,*) flow_vel(lcnt)
       write(6,*) '>>> Flow speed:',flow_vel(lcnt),' m/s'
      end if

C     TYPE OF LAYER
c >>>
c >>> Cp = 0 ; Cs = 0
c >>> Vacuum
c >>> 
      IF (ABS(V(lcnt,2)).LT.1E-10) THEN
        LAYTYP(lcnt)=-1
c >>>
c >>> Cp > 0 ; Cs = 0
c >>> Homogeneous fluid.
c >>> 
      ELSE IF (V(lcnt,2).GT.0E0.AND.ABS(V(lcnt,3)).LT.1E-10) THEN
c >>>  Iso-velocity fluid
c >>>  Check for auto-continuous SVP
       if (lcnt.GT.2) then
         if (abs(V(lcnt-1,3)+999.999).lt.1E-3) then
          if (abs(v(lcnt-1,2)-v(lcnt,2)).gt.1E-3) then
           V(lcnt-1,3)=-V(lcnt,2)
          else
c >>>      change layer m-1 to iso
           V(lcnt-1,3)=0e0
           laytyp(lcnt-1)=1
           layt(numt(2),2)=0
           NUMT(2)=NUMT(2)-1
           NUMT(1)=numt(1)+1
           layt(numt(1),1)=lcnt-1
           write(6,'(1H ,a,i4,a)') '>>> Note: Layer',m-1,
     &                          ' is treated as iso-velocity'

          end if
         end if
        end if
        LAYTYP(lcnt)=1
        NUMT(1)=NUMT(1)+1
        LAYT(NUMT(1),1)=lcnt
c >>>
c >>> Cp > 0 ; Cs < 0
c >>> n^2 linear layer (Airy)
c >>> Cs = -999.999 forces continuity of Cp at lower interface
c >>> 
      ELSE IF (V(lcnt,2).GT.0E0.AND.V(lcnt,3).LT.0) THEN
c >>> Check for auto-continuous SVP
       if (lcnt.GT.2) then
         if (abs(V(lcnt-1,3)+999.999).lt.1E-3) then
          if (abs(v(lcnt-1,2)-v(lcnt,2)).gt.1E-3) then
           V(lcnt-1,3)=-V(lcnt,2)
          else
c >>>      change layer m-1 to iso
           V(lcnt-1,3)=0e0
           laytyp(lcnt-1)=1
           layt(numt(2),2)=0
           NUMT(2)=NUMT(2)-1
           NUMT(1)=numt(1)+1
           layt(numt(1),1)=lcnt-1
           write(6,'(1H ,a,i4,a)') '>>> Note: Layer',m-1,
     &                          ' is treated as iso-velocity'

          end if
         end if
       end if
c >>> check if iso velocity
       if (abs(v(lcnt,2)+v(lcnt,3)).lt.1e-2) then
        v(lcnt,3)=0e0
        LAYTYP(lcnt)=1
        NUMT(1)=NUMT(1)+1
        LAYT(NUMT(1),1)=lcnt
        write(6,'(1H ,a,i4,a)') '>>> Note: Layer',m,
     &                          ' is treated as iso-velocity'
       else
        LAYTYP(lcnt)=2
        NUMT(2)=NUMT(2)+1
        LAYT(NUMT(2),2)=lcnt
       end if
c >>>
c >>> Cp > 0 ; Cs > 0
c >>> Isotropic elastic medium
c >>> 
      ELSE IF (V(lcnt,2).GT.0E0.AND.V(lcnt,3).GT.0) THEN
        LAYTYP(lcnt)=3
        NUMT(3)=NUMT(3)+1
        LAYT(NUMT(3),3)=lcnt
C
C     CHECK WHETHER PARAMETERS ARE PHYSICALLY MEANINGFUL
C
        GAMMA2=(V(lcnt,3)/V(lcnt,2))**2
        IF (GAMMA2.GT.0.75) THEN
          WRITE(6,*) '>>>>>WARNING: UNPHYSICAL SPEED RATIO, Cs/Cp>0.75'
        END IF
        IF ((GAMMA2*V(lcnt,5)).GT.(0.75*V(lcnt,4))) THEN
          WRITE(6,*) 
     &  '>>>>>WARNING: UNPHYSICAL ATTENUATION, (As/Ap)*(Cs/Cp)**2>0.75'
        END IF
      else if (nint(v(lcnt,2)) .eq. -4) then 
c >>>
c >>> Cp =-4 
c >>> Arctic EOF SVP
c >>>
       if (m .eq. numl) stop 
     &   '>>> Arctic EOF layer must be intermediate <<<'
       write(6,'(1H ,a,i4,a)') '>>> Note: Layer',m,
     &                          ' is arctic eof layer'
       nsub = nint(v(lcnt,3));
       dep_ptb = v(lcnt,1)
       dch = v(lcnt,4)
       dct = v(lcnt,5)
       dcd = v(lcnt,6)

       read(1,*) dep_next
       backspace(1)
       del_ptb = (dep_next-dep_ptb)/nsub 
       eof_layer(m) = .true.
       eof_type(m) = 4
       first_sublay(m) = lcnt
       last_sublay(m) = lcnt+nsub-1 
       dep_h(m) = dch
       dep_t(m) = dct
       dep_d(m) = dcd
       do isub = lcnt, lcnt+nsub-1
         v(isub,1) = dep_ptb+(isub-lcnt)*del_ptb
         v(isub,2) = arctic_svp(v(isub,1),dch,dct,dcd)
         v(isub,3) =-arctic_svp(v(isub,1)+del_ptb,dch,dct,dcd)
         v(isub,4) = 0
         v(isub,5) = 0
         v(isub,6) = 1.0
         v(isub,7) = 0
         LAYTYP(isub)=2
         NUMT(2)=NUMT(2)+1
         LAYT(NUMT(2),2)=isub
         write(6,*) (v(isub,ii),ii=1,6)
       end do
       ladd = ladd + nsub - 1

      else if (nint(v(lcnt,2)).eq.-3.and.v(lcnt,3).ge.0E0) then
c >>>
c >>> Cp =-3 ; Cs > 0
c >>> dispersive elastic medium
c >>>
       if (v(lcnt,3).gt.0E0) then
        LAYTYP(lcnt)=3
        disper(lcnt)=.true.
        NUMT(3)=NUMT(3)+1
        LAYT(NUMT(3),3)=lcnt
       else
        Laytyp(lcnt)=1
        disper(lcnt)=.true.
        NUMT(1)=NUMT(1)+1
        LAYT(NUMT(1),1)=lcnt
       end if
c >>>  read type of dispersive layer
        read(1,*) idltyp(lcnt)
        idlmax=max(idlmax,idltyp(lcnt))
        if (idltyp(lcnt).lt.1) stop 
     &        '>>> ERROR in dispers. layer type <<<'
        if (idltyp(lcnt).gt.mndlt) 
     &   stop '>>> Too many dispersive layer types <<<'

Cms
Cms  If layer is porous sediment, then input data has negative values
Cms  for both compression and shear velocities.  Only the layer depth
Cms  i.e., V(lcnt,1), is used in subsequent calculations and must
Cms  therefore be entered correctly.
Cms
c >>>
c >>> Cp < 0 ; Cs < 0
c >>> 
      ELSE IF (V(lcnt,2).LT.0E0.AND.V(lcnt,3).LT.0E0) THEN
        LAYTYP(lcnt)=5
        NUMT(5)=NUMT(5)+1
        LAYT(NUMT(5),5)=lcnt
        NBL=NBL+1
        LAYNB(NBL)=lcnt
        WRITE(6,*) 'LAYER',M,' IS POROUS SEDIMENT AT ',
     &             'DEPTH: ',V(lcnt,1),' M'
Cms
Cms  Next input line contains 13 layer data entries:
Cms  fluid density (g/cm^3),fluid bulk modulus (Pa), viscosity (kg/m-s),
Cms  grain density (g/cm^3), grain bulk modulus (Pa),
Cms  porosity, permeability (m^2), pore size factor (m),
Cms  frame shear modulus (Pa),frame bulk modulus (Pa),
Cms  shear attenuation (dB/wvlen), bulk attenuation (dB/wvlen)
Cms  virtual mass parameter (nominally 1.25).
C

        READ(1,*) (BPROP(NN,lcnt),NN=1,13)

Cms
Cms     RHOF~BPROP(1,NB),XKF~2,ETA~3,RHOS~4,XKR~5,B~6,
Cms     PERM~7,AA~8,XMU~9,XKB~10,DMU~11,DKB~12,VMP~13
Cms
        WRITE(6,599) (BPROP(NN,lcnt),NN=1,13)
 599    FORMAT('   FLUID DENSITY = ',F5.3,'   BULK MOD = ',E9.3,
     &  '   VISCOSITY = ',E9.3,/,'   GRAIN DENSITY = ',F5.3,
     &  '   BULK MOD = ',E9.3,/,'   POROSITY = ',E9.3,
     &  '   PERMEABILITY = ',E9.3,'   PORE SIZE FACTOR = ',E9.3,/,
     &  '   FRAME SHEAR MOD = ',E9.3,'   BULK MOD = ',E9.3,/,
     &  '   SHEAR ATTEN = ',F5.3,'   BULK ATTEN = ',F5.3,/,
     &  '   VIRTUAL MASS = ',F5.3,/)
Cms
Cms  Replace possibly negative velocities and density with realistic
Cms  values to avoid possible difficulties in other sections of the
Cms  program where these values are consulted.
Cms
c        V(lcnt,6) = BPROP(1,lcnt)
c        V(lcnt,2) = SQRT(BPROP(10,lcnt)*1.E-3/BPROP(1,lcnt))
c        V(lcnt,3) = SQRT(BPROP(9,lcnt)*1.E-3/BPROP(1,lcnt))

Cmss
Cmss ** Modified 5/8/96 to calculate frequency independent parameters
Cmss ** only once and to compute zero frequency limit wave speeds for
Cmss ** the 'Z' option.
Cmss
      Call blprp(lcnt)
      write(6,598) bfast(lcnt), bshear(lcnt), rho(lcnt)
  598 format('   ZERO FREQ LIMIT COMPLEX WAVESPEEDS AND DENSITY',/
     &'      FAST:',2f9.3,'     SHEAR:',2f9.3,'     DENSITY:',f9.3)
Cms
Cms  Replace possibly negative velocities and density with realistic
Cms  values to avoid possible difficulties in other sections of the
Cms  program where these values are consulted.
Cms
        V(lcnt,6) = rho(lcnt)*1e-3
        V(lcnt,2) = bfast(lcnt)
        V(lcnt,3) = bshear(lcnt)

Cms
Cms  transversely isotropic layer - modified to require V(lcnt,3) >= 0
Cms
c >>>
c >>> Cp < 0 ; Cs >= 0
c >>> 
      ELSE IF (V(lcnt,2).LT.0E0.AND.V(lcnt,3).GE.0E0) THEN

        LAYTYP(lcnt)=4
        NUMT(4)=NUMT(4)+1
        LAYT(NUMT(4),4)=lcnt
        NTISOL=NTISOL+1
        LAYNTI(NTISOL)=lcnt
        WRITE(6,*) 'LAYER',lcnt,' IS TRANSV. ISOTROPIC'
        WRITE(6,*) 'DEPTH: ',V(lcnt,1),' M'
c *** Thinly layered medium
        IF (nint(V(lcnt,2)).eq.-1) THEN
         READ(1,*) NSL(lcnt)
         AHSUM=0E0
         write(6,*) 'SPECIFIED AS THINLY LAYERED MEDIUM:'
         WRITE(6,*) 'LAYERS:',NSL(lcnt)
         WRITE(6,352)
 352  FORMAT(1H ,8X,'   ALPHA         BETA         ATTNA        ATTNB'
     &       ,'        RO       FRACTION')       
         AMEAN=0E0
         BMEAN=0E0
         DO 109 NNN=1,NSL(lcnt)
          READ(1,*) RASP,RBSP,ATTA,ATTB,
     &              ARO(NNN,lcnt),AH(NNN,lcnt)
          AMEAN=AMEAN+RASP
          BMEAN=BMEAN+RBSP
          WRITE(6,353) RASP,RBSP,ATTA,ATTB,ARO(NNN,lcnt),AH(NNN,lcnt)
 353      FORMAT((/1H ,8X,2(3X,F9.2),2(3X,F9.6),2(3X,F9.3)))
C
C       CONVERT ATTENUATIONS TO IMAGINARY PART OF VELOCITY
C
          CIMA=RASP*ATTA/54.57512
          CIMB=RBSP*ATTB/54.57512
          ASP(NNN,lcnt)=CMPLX(RASP,CIMA)
          BSP(NNN,lcnt)=CMPLX(RBSP,CIMB)
          AHSUM=AHSUM+AH(NNN,lcnt)
 109     CONTINUE
c ***  convert densities to kg/m^3
         DO 212 N=1,NSL(lcnt)
 212     ARO(N,lcnt)=1E3*ARO(N,lcnt)
         V(lcnt,2)=AMEAN/NSL(lcnt)
         V(lcnt,3)=BMEAN/NSL(lcnt)
         IF (ABS(AHSUM-1E0).GT.1E-4) 
     &     STOP '*** SUM OF FRACTIONS IS NOT 1.0 ***'
         CALL FINELAY(lcnt)
       ELSE
        READ(1,*) RR,RI
        C11=CMPLX(RR,RI)
        READ(1,*) RR,RI
        C13=CMPLX(RR,RI)
        READ(1,*) RR,RI
        C33=CMPLX(RR,RI)
        READ(1,*) RR,RI
        C44=CMPLX(RR,RI)
        READ(1,*) RR,RI
        C66=CMPLX(RR,RI)
        READ(1,*) RH
c *** convert density to kg/m^3
        RH=RH*1E3
       END IF
       SLNORM(lcnt)=SQRT(REAL(C44)/RH)
       V(lcnt,3)=SLNORM(lcnt)
       V(lcnt,2)=SQRT(REAL(C11)/RH)
       V(lcnt,6)=RH*1E-3
       CALL DETABC()
       CALL VMOV(A,1,ELAG(1,lcnt),1,24)
       WRITE(6,*) 'C11=',C11
       WRITE(6,*) 'C13=',C13
       WRITE(6,*) 'C33=',C33
       WRITE(6,*) 'C44=',C44
       WRITE(6,*) 'C66=',C66
       WRITE(6,*) 'RHO=',V(lcnt,6)
      ELSE
        STOP '*** UNKNOWN LAYER TYPE ***'
      END IF
 110  CONTINUE
      NUML = NUML + ladd
      NUMI = NUML - 1

      ROUGH(1)=ROUGH(2)
      DO 111 M=1,NUML
      ROUGH2(M)=ROUGH(M)**2
 111  CONTINUE   
      DO 1111 M=2,NUML
      IF (ROUGH2(M).GT.1E-10) THEN
        IF (LAYTYP(M-1).EQ.2.OR.LAYTYP(M).EQ.2) THEN
          WRITE(6,*) '*** SURFACE ROUGHNESS NOT ALLOWED BETWEEN    ***'
          WRITE(6,*) '*** LAYERS WITH SOUND SPEED GRADIENT. INSERT ***'
          WRITE(6,*) '*** A DUMMY ISOVELOCITY LAYER. THIS RUN      ***'
          WRITE(6,*) '*** MUST BE FOR VOLUME SCATTERING ONLY       ***'
C          STOP '>>>> EXECUTION TERMINATED <<<<'
        ELSE IF (LAYTYP(M-1).GE.4.OR.LAYTYP(M).GE.4) THEN
          WRITE(6,*) '*** NON-KIRCHHOFF SCATTERING NOT INCLUDED FOR ***'
          WRITE(6,*) '*** POROUS SEDIMENT OR ANISOTROPIC LAYERS.    ***'
          WRITE(6,*) '*** KIRCHHOFF APPROXIMATION WILL BE APPLIED.  ***'
          CLEN(M)=1E6
        ELSE
        END IF
      END IF
 1111 CONTINUE
C
C     THE DENSITIES ARE CONVERTED FROM G/CM**3 TO KG/M**3      
C           
      DO 112 M=1,NUML
        V(M,6)=V(M,6)*1E3      
 112  CONTINUE
      RETURN
      END

      subroutine eof_svp(lay_num,dh,dt,dd)
       INCLUDE 'compar.f'
       INCLUDE 'comnla.f'
       real dh,dt,dd
       real arctic_svp
       do lcnt=first_sublay(lay_num),last_sublay(lay_num)
          if (eof_type(lay_num) .eq. 4) then
            v(lcnt,2) = arctic_svp(v(lcnt,1),dh,dt,dd)
            v(lcnt,3) = -arctic_svp(v(lcnt+1,1),dh,dt,dd)
          end if
       end do
       return
       end
            
c           Arctic profile layer
            
c*****************************************************
c****    Arctic Perturbation  ************************
c*****************************************************
c****   Arctic profile w/ nominal 
c****   halocline @ 50 m, thermocline @ 250 m, to hydrostatic @ 1000 m
c****   -the svp is continuous and cz at the break points are set w/
c****   surface: 1440 m/s, halocline: 1442 m/s, thermocline: 1460 m/s, 
c****   at 1000 m: 1478 m/s, bottom at 5000 m w/ .0167 /s gradient to it
c****   Emperical fit from data: halocline sin**2, thermocline sin, rest linear
c*****************************************************
	real function arctic_svp(z,dh,dt,dd)
        real z,dh,dt,dd
	parameter (zh = 50.,zt = 250.,zd = 1000.,zb = 5000.)
	parameter (cs = 1440.,ch = 1442.,ct = 1462.,cd = 1480.,cb = 5000.) 
c	parameter (dch = 8., dct = 20., dcd = .024, dcb = .0167)
	parameter (dch = 8., dct = 20., dcb = .0167)
	data pi/3.1415962/,pi2/1.5707963/,pi4/0.7853981/
	data c0/1440./
c*****************************************************
c***      layers
c	dh = zh + dzh
c	dt = zt + dzt
c	dd = zd + dzd
c***      thicknesses
	xdh = dh
	xdt = dt - dh
	xdd = dd - dt
        dcd = (cd-ct)/(dd-dt)
c***      sound speeds
	if (z .lt. 0.) then
           cz = 0
c***        halocline
	else if (z .lt. dh) then                   
	   cz = cs + dch*(sin(pi4*z/xdh))**4
c***        thermocline
	else if (z .lt. dt) then                   
	   cz = ch + dct*sin(pi2*(z-dh)/xdt)
c***        deep
	else if (z .lt. dd) then
	   cz = ct + dcd*(z-dt)
c***        hydrostatic gradient
	else if (z. lt. zb) then          
	   cz = cd + dcb*(z-dd)
	else 
	   stop 'Max depth exceeded'
	end if
        arctic_ptb = cz
        return
	end





      subroutine dltable(lx,mx,dlfr)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
c
c >>> subroutine for setting up parameter tables for dispersive
c     media
c
      include 'compar.f'
      include 'comnla.f'
      include 'comnp.f'
      character*80 filenm
      dimension x(5,mndfr)
      equivalence (x(1,1),cff(1,1))
      if (mx.gt.mndfr) stop '>>> Too many frequencies <<<'
      inquire(unit=1,name=filenm)
      ll=indexs(filenm,'.')
      open(unit=3,file=filenm(1:ll)//'dis',status='old',err=5000)
      do 20 i=1,idlmax
       write(6,*) 'Dispersive medium:',i
       read(3,*) nf
       do 10 j=1,nf

        read(3,*) (x(k,j),k=1,5)
 10     continue
       do 8 j=lx,mx
        fr=(j-1)*dlfr
        do 6 k=2,nf
         if (x(1,k).gt.fr.or.k.eq.nf) then
          xx=(fr-x(1,k-1))/(x(1,k)-x(1,k-1))
          cpdl(i,j)=x(2,k-1)+xx*(x(2,k)-x(2,k-1))
          csdl(i,j)=x(3,k-1)+xx*(x(3,k)-x(3,k-1))
          apdl(i,j)=x(4,k-1)+xx*(x(4,k)-x(4,k-1))
          asdl(i,j)=x(5,k-1)+xx*(x(5,k)-x(5,k-1))
          write(6,*) fr,cpdl(i,j),csdl(i,j),apdl(i,j),asdl(i,j)
          go to 8
         end if
 6      continue
 8     continue
 20   continue
      do 30 i=1,numl
       if (disper(i)) then
        ity=idltyp(i)
        v(i,2)=cpdl(ity,lx)
        v(i,3)=csdl(ity,lx)
        v(i,4)=apdl(ity,lx)
        v(i,5)=asdl(ity,lx)
        write(6,*) i,(lx-1)*dlfr,v(i,2),v(i,3),v(i,4),v(i,5)
       end if
 30    continue
      close(unit=3)
      return
 5000     stop '>>> Dispersion file not found <<<'
      end

      subroutine get_disper(lfr)
      include 'compar.f'
      include 'comnla.f'
      include 'comnp.f'
      do 30 i=1,numl
       if (disper(i)) then
        ity=idltyp(i)
        v(i,2)=cpdl(ity,lfr)
        v(i,3)=csdl(ity,lfr)
        v(i,4)=apdl(ity,lfr)
        v(i,5)=asdl(ity,lfr)
        write(6,*) i,lfr,v(i,2),v(i,3),v(i,4),v(i,5),v(i,6)
       end if
 30    continue
      return
      end
 
      subroutine blprp(nb)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
Cmss **** Subroutine to compute frequency independent properties
Cmss **** associated with a porous sediment layer - added 5/8/96
Cmss
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'

      COMPLEX CC1,CD1,CKB,XKF
      XKF = CMPLX(BPROP(2,NB),0E0)
      XKR = BPROP(5,NB)
      B = BPROP(6,NB)
      RHOF(NB) = BPROP(1,NB)*1E3
      RHOS = BPROP(4,NB)*1E3
      RHO(NB) = (1. - B)*RHOS + B*RHOF(NB)
      CMU(NB) = BPROP(9,NB)*(1. + AI*BPROP(11,NB)/8.685889/PI)
      CKB = BPROP(10,NB)*(1. + AI*BPROP(12,NB)/8.685889/PI)
      CD1 = CKB + CMU(NB)/.75
      CC1 = 1. - CKB/XKR
      CM(NB) = XKR/(CC1 + B*(XKR/XKF - 1.))
      CH(NB) = CC1*CC1*CM(NB) + CD1
      CD(NB) = CD1*CM(NB)
      BCC(NB) = CC1*CM(NB)
      bfast(nb) = csqrt(ch(nb) / rho(nb))
      bshear(nb) = csqrt(cmu(nb) / rho(nb))
      end

      SUBROUTINE BLPROP(NB)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
Cms
Cms  **** Subroutine to compute frequency dependent properties
Cms  **** associated with a porous sediment layer - added 8/27/92
Cms
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'

      COMPLEX CF,CC1,CD1,CALPHA,CKB,CB1,CG1,XKF,cg2
      XKF = CMPLX(BPROP(2,NB),0E0)
      XKR = BPROP(5,NB)
      ETA = BPROP(3,NB)
      PERM = BPROP(7,NB)
      AA = BPROP(8,NB)
      B = BPROP(6,NB)
      RHOF(NB) = BPROP(1,NB)*1E3
      RHOS = BPROP(4,NB)*1E3
      RHO(NB) = (1. - B)*RHOS + B*RHOF(NB)
      XM = BPROP(13,NB)*RHOF(NB)/B
      CMU(NB) = BPROP(9,NB)*(1. + AI*BPROP(11,NB)/8.685889/PI)
      CKB = BPROP(10,NB)*(1. + AI*BPROP(12,NB)/8.685889/PI)
c >>> if AA negative, fix F to 1 for compatibility with Collins' formulation.
      if (aa.lt.0) then
       cf=1E0
      else
       X = AA*SQRT(DSQ*RHOF(NB)/ETA)
       IF(X.GT.119.) THEN
         CF = (1.,1.)*X/4./SQRT(2.)
       ELSE
         CF = CMPLX(BERP0(X),BEIP0(X))/CMPLX(BER0(X),BEI0(X))
         CF = X*CF/(4.*(1. + 2.*AI*CF/X))
       ENDIF
      end if
      CALPHA = AI*CF*ETA/PERM/DSQ
      CGAMMA(NB) = RHOF(NB)/(XM - CALPHA)
      CD1 = CKB + CMU(NB)/.75
      CC1 = 1. - CKB/XKR
      CM(NB) = XKR/(CC1 + B*(XKR/XKF - 1.))
      CH(NB) = CC1*CC1*CM(NB) + CD1
      CD(NB) = CD1*CM(NB)
      BCC(NB) = CC1*CM(NB)
      CB1 = (CM(NB)*RHO(NB) + CH(NB)*XM - 2.*BCC(NB)*RHOF(NB)
     &         - CH(NB)*CALPHA)*CSQ/(2.*CD(NB))
      CG1 = (XM*RHO(NB) - RHOF(NB)**2 - RHO(NB)*CALPHA)
     &         *CSQ*CSQ/CD(NB)
      cg2 = SQRT(CB1*CB1 - CG1)
      CK1(NB) = CB1 - cg2
      CK2(NB) = CB1 + cg2
Cms....Modification due to B. Gurevich (7/19/94, implemented 2/7/96)
Cms....to avoid loss of accuracy in the smaller root in some cases. 
      if(cabs(ck1(nb)).gt.cabs(ck2(nb))) then
c        ck2(nb) = cg1/ck1(nb)
c        fs_switch(nb) = .true.
Chs 970697 alfa always fast wave, beta slow.
        ck2(nb)=ck1(nb)
        ck1(nb) = cg1/ck2(nb)
        fs_switch(nb) = .false.
      else
        ck1(nb) = cg1/ck2(nb)
        fs_switch(nb) = .false.
      endif
      CKS(NB) = (RHO(NB) - CGAMMA(NB)*RHOF(NB))*CSQ/CMU(NB)
      RETURN
      END
ccc
c     Note:  machine dependent variable XINF within
c
c     The code below is an old version of the IMSL routines for
c     the Kelvin functions ber0, bei0, and their derivatives.
c     These functions are needed for the Biot sediment calculations.
c     One final note:  there is a machine dependent variable,
c     XINF.  You need to set it to the maximum value for a REAL*4
c     on your machine.  The two values here are for Mac/FX8(default)
c     and VAX. There are only two places you'll need to change it.
c
ccc
      real function ber0(x)
	  real x,ber,bei,xker,xkei
	  integer ier
	  call mmkel0(x,ber,bei,xker,xkei,ier)
	  ber0=ber
	  return
	  end
c
      real function bei0(x)
	  real x,ber,bei,xker,xkei
	  integer ier
	  call mmkel0(x,ber,bei,xker,xkei,ier)
	  bei0=bei
	  return
	  end
c
	  real function berp0(x)
	  real x,berp,beip,xkerp,xkeip
	  integer ier
	  call mmkeld(x,berp,beip,xkerp,xkeip,ier)
	  berp0=berp
	  return
	  end
c
      real function beip0(x)
	  real x,berp,beip,xkerp,xkeip
	  integer ier
	  call mmkeld(x,berp,beip,xkerp,xkeip,ier)
	  beip0=beip
	  return
	  end
c
	  SUBROUTINE MMKEL0 (X,BER,BEI,XKER,XKEI,IER)
C
      DIMENSION          C1(9),C2(9),C3(9),C4(9),E1(9),E2(9)
      REAL               C1,C2,C3,C4,E1,E2,PIO8,RT2,XINF,
     *                   PI,EUL,TEN,ZERO,HALF,ONE,ARG,BER,BEI,B1,B2,B3,
     *                   B4,CON,DC,DCM,DE,DS,DSM,DSQ,PIO2,R1,R2,S,SM,T,
     *                   TM,TWOPI,X,XKER,XKEI,Z,ZI,ZIM,ZSQ,Z4,ZMAX
C
C                                  COEFFICIENTS FOR EVALUATION OF
C                                  BER-SUB-ZERO(X) FOR X GREATER THAN
C                                  0. AND LESS THAN OR EQUAL TO 10.
C
      DATA               C1(1)/5.16070465E-5/,C1(2)/-4.8987125727E-3/
      DATA               C1(3)/.25977730007/,C1(4)/-7.2422567278207/
      DATA               C1(5)/93.859669297173/
      DATA               C1(6)/-470.95027958900/
      DATA               C1(7)/678.16840276631/
      DATA               C1(8)/-156.24999999957/
      DATA               C1(9)/.9999999999974/
C
C                                  COEFFICIENTS FOR EVALUATION OF
C                                  BEI-SUB-ZERO(X) FOR X GREATER THAN 0.
C                                  AND X LESS THAN OR EQUAL TO 10.
C
      DATA               C2(1)/4.4913000E-6/,C2(2)/-5.444243175E-4/
      DATA               C2(3)/3.84288282734E-2/
      DATA               C2(4)/-1.4963342749742/
      DATA               C2(5)/28.969033878650/
      DATA               C2(6)/-240.28075494426/
      DATA               C2(7)/678.16840277698/
      DATA               C2(8)/-434.02777777775/
      DATA               C2(9)/25.000000000000/
C
C                                  COEFFICIENTS FOR EVALUATION OF
C                                  KEI-SUB-ZERO(X) FOR X GREATER THAN
C                                  OR EQUAL TO ZERO AND X LESS THAN OR
C                                  EQUAL TO 10.
C
      DATA               C3(1)/1.54363047E-5/,C3(2)/-1.8064777860E-3/
      DATA               C3(3)/.1222087382192/
      DATA               C3(4)/-4.5187459132639/
      DATA               C3(5)/81.952477160620/
      DATA               C3(6)/-623.01367174052/
      DATA               C3(7)/1548.4845196731/
      DATA               C3(8)/-795.71759259249/
      DATA               C3(9)/24.999999999999/
C
C                                  COEFFICIENTS FOR EVALUATION OF
C                                  KER-SUB-ZERO(X) FOR X GREATER THAN OR
C                                  EQUAL TO ZERO AND X LESS THAN OR
C                                  EQUAL TO TEN
C
      DATA               C4(1)/1.2161109E-6/,C4(2)/-1.797627986E-4/
      DATA               C4(3)/1.59380149705E-2/
      DATA               C4(4)/-.8061529027876/
      DATA               C4(5)/21.212345166023/
      DATA               C4(6)/-255.09717427105/
      DATA               C4(7)/1153.8281852815/
      DATA               C4(8)/-1412.8508391204/
      DATA               C4(9)/234.375/
C
C                                  COEFFICIENTS FOR EVALUATION OF
C                                  AUXILIARY FUNCTIONS FOR X GREATER
C                                  THAN 10.
C
      DATA               E1(1)/4.92E-8/,E1(2)/1.452E-7/,E1(3)/1.35E-8/
      DATA               E1(4)/-1.6192E-6/,E1(5)/-1.12207E-5/
      DATA               E1(6)/-5.17869E-5/,E1(7)/7.0E-10/
      DATA               E1(8)/8.8388346E-3/,E1(9)/1.0/
      DATA               E2(1)/-2.43E-8/,E2(2)/7.5E-8/,E2(3)/5.929E-7/
      DATA               E2(4)/1.6431E-6/,E2(5)/-7.2E-9/
      DATA               E2(6)/-5.18006E-5/,E2(7)/-7.031241E-4/
      DATA               E2(8)/-8.8388340E-3/,E2(9)/0.0/
C
C                                  MISCELLANEOUS CONSTANTS
C
      DATA               PIO2/1.5707963267949/
      DATA               TWOPI/6.2831853071796/
      DATA               PIO8/.39269908169872/
      DATA               RT2/.70710678118655/
      DATA               XINF/0.339E39/             !Mac/FX8
C     DATA               XINF/1.698E38/             !VAX
      DATA               PI/3.1415926535898/
      DATA               EUL/.57721566490153/
      DATA               TEN/10./,ZERO/0./,HALF/.5/,ONE/1./
      DATA               ZMAX/119./
C                                  FIRST EXECUTABLE STATEMENT
      IER = 0
      Z = ABS(X)
      IF (Z .GT. TEN) GO TO 15
      IF (Z .EQ. ZERO) GO TO 10
C                                  CALCULATION OF FUNCTIONS FOR ABS(X)
C                                  LESS THAN 10.
      Z = Z/TEN
      ZSQ = Z*Z
      Z4 = ZSQ*ZSQ
      B1 = C1(1)
      B2 = C2(1)
      B3 = C3(1)
      B4 = C4(1)
      DO 5 I = 2,9
         B1 = B1*Z4+C1(I)
         B2 = B2*Z4+C2(I)
         B3 = B3*Z4+C3(I)
         B4 = B4*Z4+C4(I)
    5 CONTINUE
      BER = B1
      BEI = ZSQ*B2
      IF (X .LT. ZERO) GO TO 30
      R1 = ZSQ*B3
      R2 = Z4*B4
      CON = (ALOG(X*HALF)+EUL)
      XKEI = -PIO2*HALF*BER+(R1-BEI*CON)
      XKER = PIO2*HALF*BEI-(R2+BER*CON)
      GO TO 9005
C                                  X EQUAL 0. DEFAULT TO PROPER VALUES
   10 BER = ONE
      BEI = ZERO
      XKEI = -HALF*PIO2
      XKER = XINF
      GO TO 9005
C                                  X GREATER THAN 10. CALCULATE
C                                  AUXILIARY FUNCTIONS
   15 IF (Z .GT. ZMAX) GO TO 25
      ZI = TEN/Z
      ZIM = -ZI
      S = E1(1)
      SM = S
      T = E2(1)
      TM = T
      DO 20 I = 2,9
         S = S*ZI+E1(I)
         T = T*ZI+E2(I)
         SM = SM*ZIM+E1(I)
         TM = TM*ZIM+E2(I)
   20 CONTINUE
      ARG = Z*RT2
      DS = SIN(ARG-PIO8)
      DC = COS(ARG-PIO8)
      DSM = SIN(ARG+PIO8)
      DCM = COS(ARG+PIO8)
      DE = EXP(ARG)
      DSQ = SQRT(TWOPI*Z)
C                                  CALCULATE THE DESIRED FUNCTIONS
      BER = DE*(S*DC-T*DS)/DSQ
      BEI = DE*(T*DC+S*DS)/DSQ
      IF (X .LT. ZERO) GO TO 30
      XKEI = PI*(TM*DCM-SM*DSM)/(DE*DSQ)
      XKER = PI*(SM*DCM+TM*DSM)/(DE*DSQ)
      GO TO 9005
C                                  Z TOO LARGE.
   25 BER = ZERO
      BEI = ZERO
      IER = 129
      IF (X .LT. ZERO) GO TO 35
      XKEI = ZERO
      XKER = ZERO
      GO TO 9000
C                                  X LESS THAN 0. DEFAULT TO PROPER
C                                  VALUES
   30 IER = 34
   35 XKEI = XINF
      XKER = XINF
      IF (IER .EQ. 0) GO TO 9005
 9000 CONTINUE
C      CALL UERTST (IER,6HMMKEL0)
 9005 RETURN
      END
ccc
ccc
ccc
      SUBROUTINE MMKELD (X,BERP,BEIP,XKERP,XKEIP,IER)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
C
      DIMENSION          D1(9),D2(9),D3(9),D4(9),E3(9),E4(9)
      REAL               ARG,BEI,BEIP,BER,BERP,B1,B2,B3,B4,CON,DC,DCM,
     *                   DE,DS,DSM,DSQ,D1,D2,D3,D4,EUL,E3,E4,PI,PIO2,
     *                   PIO8,RT2,R1P,R2P,TWOPI,U,UM,V,VM,X,XINF,XKEI,
     *                   XKEIP,XKER,XKERP,Z,ZI,ZIM,ZSQ,Z3,Z4,ZMAX
      REAL               TEN,ZERO,HALF
      DATA               TEN/10./,ZERO/0./,HALF/.5/
C
C                                  COEFFICIENTS FOR EVALUATION OF BERP-
C                                  SUB-ZERO(X) FOR X GREATER THAN 0. AND
C                                  LESS THAN OR EQUAL TO 10.
C
      DATA               D1(1)/-1.2506046E-6/,D1(2)/1.701453451E-4/
      DATA               D1(3)/-1.37246036190E-2/
      DATA               D1(4)/.6234726348243/
      DATA               D1(5)/-14.484516949840/
      DATA               D1(6)/150.17547184323/
      DATA               D1(7)/-565.14033564795/
      DATA               D1(8)/542.53472222221/
      DATA               D1(9)/-62.500000000000/
C
C                                  COEFFICIENTS FOR EVALUATION OF BEIP-
C                                  SUB-ZERO(X) FOR X GREATER THAN 0.
C                                  AND LESS THAN OR EQUAL TO 10.
C
      DATA               D2(1)/1.52269884E-5/,D2(2)/-1.6331100837E-3/
      DATA               D2(3)/9.99147064932E-2/
      DATA               D2(4)/-3.2919352108579/
      DATA               D2(5)/52.144260897591/
      DATA               D2(6)/-336.39305690237/
      DATA               D2(7)/678.16840277475/
      DATA               D2(8)/-260.41666666655/
      DATA               D2(9)/4.9999999999993/
C
C                                  COEFFICIENTS FOR EVALUTION OF KEIP-
C                                  SUB-ZERO(X) FOR X GREATER THAN 0.
C                                  AND LESS THAN OR EQUAL TO 10.
C
      DATA               D3(1)/5.23294314E-5/
      DATA               D3(2)/-5.4188558408E-3/
      DATA               D3(3)/.3177418434686/
      DATA               D3(4)/-9.9412403209725/
      DATA               D3(5)/147.51445859133/
      DATA               D3(6)/-872.21914036725/
      DATA               D3(7)/1548.4845196652/
      DATA               D3(8)/-477.43055555515/
      DATA               D3(9)/4.9999999999975/
C
C                                  COEFFICIENTS FOR EVALUATION OF KERP-
C                                  SUB-ZERO(X) FOR X GREATER THAN OR
C                                  EQUAL TO 0. AND LESS THAN OR EQUAL
C                                  TO 10.
C
      DATA               D4(1)/4.3682053E-6/,D4(2)/-5.752042283E-4/
      DATA               D4(3)/4.46263862145E-2/
      DATA               D4(4)/-1.9347669229237/
      DATA               D4(5)/42.424690313109/
      DATA               D4(6)/-408.15547882926/
      DATA               D4(7)/1384.5938223372/
      DATA               D4(8)/-1130.2806712963/
      DATA               D4(9)/93.750000000000/
C
C
C                                  COEFFICIENTS FOR EVALUTION OF
C                                  AUXILIARY FUNCTIONS FOR X GREATER
C                                  THAN 10.
C
      DATA               E3(1)/-5.63E-8/,E3(2)/-1.671E-7/
      DATA               E3(3)/-1.47E-8/,E3(4)/1.9780E-6/
      DATA               E3(5)/1.44255E-5/,E3(6)/7.25024E-5/
      DATA               E3(7)/-8.0E-10/,E3(8)/-2.65165040E-2/
      DATA               E3(9)/1.0/
      DATA               E4(1)/-2.69E-8/,E4(2)/-8.83E-8/
      DATA               E4(3)/-6.992E-7/,E4(4)/-2.0042E-6/
      DATA               E4(5)/7.9E-9/,E4(6)/7.25179E-5/
      DATA               E4(7)/1.1718740E-3/,E4(8)/2.65165034E-2/
      DATA               E4(9)/0.0/
C
C                                  MISCELLANEOUS CONSTANTS
C
      DATA               PIO2/1.5707963267949/
      DATA               TWOPI/6.2831853071796/
      DATA               PIO8/.39269908169872/
      DATA               RT2/.70710678118655/
      DATA               XINF/0.339E39/              !Mac/FX8
C     DATA               XINF/1.698E38/              !VAX
      DATA               PI/3.1415926535898/
      DATA               EUL/.57721566490153/
      DATA               ZMAX/119./
C                                  FIRST EXECUTABLE STATEMENT
      IER = 0
      CALL MMKEL0(X,BER,BEI,XKER,XKEI,IER)
      Z = ABS(X)
      IF (Z .GT. TEN) GO TO 15
      IF (Z .EQ. ZERO) GO TO 10
C                                  CALCULATION OF FUNCTIONS FOR ABS(X)
C                                  LESS THAN 10.
      Z = Z/TEN
      ZSQ = Z*Z
      Z3 = ZSQ*Z
      Z4 = ZSQ*ZSQ
      B1 = D1(1)
      B2 = D2(1)
      B3 = D3(1)
      B4 = D4(1)
      DO 5 I = 2,9
         B1 = B1*Z4+D1(I)
         B2 = B2*Z4+D2(I)
         B3 = B3*Z4+D3(I)
         B4 = B4*Z4+D4(I)
    5 CONTINUE
      BERP = B1*Z3
      BEIP = Z*B2
      IF ( X .LT. ZERO) GO TO 30
      R1P = Z*B3
      R2P = Z3*B4
      CON = (ALOG(X*HALF) + EUL)
      V = ABS(X)
      XKEIP = -PIO2*HALF*BERP+(R1P-BEIP*CON-BEI/V)
      XKERP = PIO2*HALF*BEIP-(R2P+BERP*CON+BER/V)
      GO TO 9005
C                                  X EQUAL TO 0. DEFAULT TO PROPER
C                                  VALUES
   10 BERP = ZERO
      BEIP = ZERO
      XKEIP = ZERO
      XKERP = -XINF
      GO TO 9005
C                                  X GREATER THAN 10. CALCULATE
C                                  AUXILIARY FUNCTIONS
   15 IF (Z .GT. ZMAX) GO TO 25
      ZI = TEN/Z
      ZIM = -ZI
      U = E3(1)
      UM = U
      V = E4(1)
      VM = V
      DO 20 I = 2,9
         U = U*ZI+E3(I)
         V = V*ZI+E4(I)
         UM = UM*ZIM+E3(I)
         VM = VM*ZIM+E4(I)
   20 CONTINUE
      ARG = Z*RT2
      DS = SIN(ARG-PIO8)
      DC = COS(ARG-PIO8)
      DSM = SIN(ARG+PIO8)
      DCM = COS(ARG+PIO8)
      DE = EXP(ARG)
      DSQ = SQRT(TWOPI*Z)
C                                  CALCULATE THE DESIRED FUNCTIONS
      BERP = DE*(U*DCM-V*DSM)/DSQ
      BEIP = DE*(V*DCM+U*DSM)/DSQ
      IF (X .LT. ZERO) GO TO 30
      XKEIP = -PI*(VM*DC-UM*DS)/(DE*DSQ)
      XKERP = -PI*(UM*DC+VM*DS)/(DE*DSQ)
      GO TO 9005
C                                  Z TOO LARGE.
   25 BERP = ZERO
      BEIP = ZERO
      IER = 129
      IF (X .LT. ZERO) GO TO 35
      XKEIP = ZERO
      XKERP = ZERO
      GO TO 9000
C                                  X LESS THAN 0. DEFAULT TO PROPER
C                                  VALUES
   30 IER = 34
      BERP = -BERP
      BEIP = -BEIP
   35 XKERP = XINF
      XKEIP = XINF
      IF (IER .EQ. 0) GO TO 9005
 9000 CONTINUE
C      CALL UERTST(IER,6HMMKELD)
 9005 RETURN
      END
      SUBROUTINE UERTST (IER,NAME)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
C
C     	Replacemant for subroutine furnished for IMSL library routines on Cyber
C
      INTEGER		IER
      CHARACTER		NAME*6
      WRITE(2,10) IER, NAME
 10   FORMAT(' *** ERROR NUMBER ',I3,'  CALLED FROM ',A6 )
      RETURN
      END

      SUBROUTINE INSRC(SD)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      INCLUDE 'compar.f'
      INCLUDE 'comnrd.f'
      INCLUDE 'comnla.f'
C *** DEFAULTS
      ISROW=1
      ISINC=0
      SRCTYP=1
      LS=1
      DELTA=1.
      THETA=0.
      FOCDEP=0.
      LTYP=1      
      if (PROGNM(1:5).EQ.'OASP3') THEN
c *** source type read in for 3d version
      READ(1,*) SRCTYP
      BACKSPACE(1)
      IF(SRCTYP.EQ.1) THEN
        READ(1,*) SRCTYP
      ELSEIF(SRCTYP.EQ.2) THEN
        READ(1,*) SRCTYP,RFOR,HANGLE,VANGLE
        HANGLE=HANGLE*PI/180.
        VANGLE=VANGLE*PI/180.
        SINH=SIN(HANGLE)
        SINV=SIN(VANGLE)
        COSH=COS(HANGLE)
        COSV=COS(VANGLE)
        IF(ABS(SINH).LE.0.0001) SINH=0.
        IF(ABS(SINV).LE.0.0001) SINV=0.
        IF(ABS(COSH).LE.0.0001) COSH=0.
        IF(ABS(COSV).LE.0.0001) COSV=0.
        F1=RFOR*COSV*COSH
        F2=RFOR*COSV*SINH
        F3=RFOR*SINV
        write(6,*) 'f1 f2 f3',f1,f2,f3
      ELSEIF(SRCTYP.EQ.3) THEN
        READ(1,*) SRCTYP,RMOM,ADELTA
        RDELTA=ADELTA*PI/180.
      ELSEIF(SRCTYP.EQ.4) THEN
        READ(1,*) SRCTYP,RMOM,ADELTA
        RDELTA=ADELTA*PI/180.
      ELSEIF(SRCTYP.EQ.5 .OR. SRCTYP.EQ.6) THEN
        READ(1,*) SRCTYP,TENMOM(1),TENMOM(2),TENMOM(3),ATENDEL(3)
        TENDEL(3)=ATENDEL(3)*PI/180.
        ATENDEL(2)=ATENDEL(3)+90.
        TENDEL(2)=ATENDEL(2)*PI/180.
        ATENDEL(1)=90.
        TENDEL(1)=ATENDEL(1)*PI/180.
      ENDIF
      ELSE
C *** Source type = 1 for 2d version
       if (trfsou) then
        srctyp=99
        write(6,*)
        write(6,'(a)') ' Source type: 99'
        write(6,'(a)') ' trf-source array'
       else if (mom_sou) then
        srctyp=5
       else if (dip_sou) then
        srctyp=4
       else if (hor_for) then
        srctyp=3
       else if (ver_for.or.shear) then
        srctyp=2
       else
        SRCTYP=1
       end if
       write(6,*) 'SRCTYP=',srctyp
      END IF
c *** source data
      if (lina.eq.1.and.extlar) then
       call opfilr(2,ierr)
       if (ierr.ne.0) stop '>>> Source file not found <<<'
       read(1,*) sd
       read(2,*) ls
       write(6,*)
       write(6,'(a)') 'Externally specified source array:'
       write(6,*)
       write(6,'(a)') '    Depth       Delay        Strength'
       do 700 is=1,ls
        read(2,*) sdc(is),sdelay(is),sstren(is)
        write(6,'(3(1x,f10.3))') sdc(is),sdelay(is),sstren(is)
 700   continue
      else IF (LINA.EQ.1.and.(.not.trfsou)) THEN
       if (dip_sou) then
        READ(1,*) SD,LS,DELTA,THETA,LTYP,FOCDEP,dip_angle
        rdelta=dip_angle*pi/180.0
        write(6,*) 'Dip-slip source. Dip angle:',dip_angle
       else
        READ(1,*) SD,LS,DELTA,THETA,LTYP,FOCDEP
       end if
       IF (LS.GT.NRD) THEN
        WRITE(6,*) '*** TOO MANY SOURCES ***'
        STOP
       END IF
       THETA=THETA*PI/180.
       IF (LTYP.LT.1.OR.LTYP.GT.5) LTYP=1
       DELTA=ABS(DELTA)
       CALL LINARR(SD)
       WRITE(6,901) SD,LS,DELTA,THETA*180./PI,LTYP
 901   FORMAT(/1H ,'VERTICAL SOURCE ARRAY:',
     1      /1H ,'MEAN DEPTH:          ',F7.1,' M',
     2      /1H ,'NUMBER OF SOURCES:   ',I5,
     3      /1H ,'SOURCE SPACING:      ',F7.1,' M',
     4      /1H ,'MAIN LOBE ANGLE:     ',F7.1,' DEG',
     5      /1H ,'ARRAY TYPE:          ',I5)
       IF (LTYP.EQ.3.OR.LTYP.EQ.5) WRITE(6,951) FOCDEP
 951   FORMAT(1H ,'FOCAL DEPTH:         ',F7.1,' M')
      ELSE
       if (dip_sou) then
        read(1,*) SD, dip_angle
        rdelta=dip_angle*pi/180.0
        write(6,*) 'Dip-slip source. Dip angle:',dip_angle
       else
        write(6,*) 'reading sd'
        READ(1,*) SD
        write(6,*) 'SD= ', sd    
       end if
       SDC(1)=SD
      END IF

C
C     DETERMINATION OF SOURCE LAYERS
C
      WRITE(6,908)
 908  FORMAT(/1H ,'SOURCE DATA:',//1H ,'  N0. ','   DEPTH  ',
     1       'LAYER','      ZU        ZL')
      DO 906 I=1,LS
 906  CALL SOURCE(V,NUML,SDC(I),LAYS(I),ZUS(I),ZLS(I))
      WRITE(6,907) 1,SDC(1),LAYS(1),ZUS(1),ZLS(1)
      IF (LS.GT.1) THEN
       WRITE(6,907) LS,SDC(LS),LAYS(LS),ZUS(LS),ZLS(LS)
      END IF
 907  FORMAT(1H ,I6,F10.1,I6,2F10.1)
      RETURN
      END
      SUBROUTINE INREC(RD,RDLOW,idinc)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      INCLUDE 'compar.f'
      INCLUDE 'comnrd.f'
      INCLUDE 'comnla.f'
      ztilt=0.0
      atilt=0.0
      if (PROGNM(1:5).EQ.'OASTL') THEN
       READ(1,*) RD,RDLOW,IR,IDINC
      ELSE if (tilt) then
       read(1,*) rd,rdlow,ir,ztilt,dtilt
       write(6,*)
       write(6,'(1x,a,f10.2,a)') 'Array tilt: ',dtilt,' deg'
       write(6,'(1x,a,f10.2,a)') 'Ref. depth: ',ztilt,' m'      
       write(6,*)
       atilt=dtilt*atan(1e0)/45.
      else
       READ(1,*) RD,RDLOW,IR
      END IF
      IF (IR.GT.1) THEN
       RDSTEP=(RDLOW-RD)/FLOAT(IR-1)
      ELSE
       RDSTEP=1.
      END IF
      if (iabs(ir).gt.nrd) stop '>>> Too many receiver depths <<<'
      WRITE(6,918)
 918  FORMAT(/1H ,'RECEIVER DATA:',//1H ,'  NO. ','      DEPTH  ',
     1       'LAYER','           Z          DR')
      IF (IR.GT.0) THEN
        DO 920 JJ=1,IR
        rtmp=(JJ-1)*RDSTEP+RD
        rdc(jj)=ztilt+(rtmp-ztilt)*cos(atilt)
        ofstar(jj)=(rtmp-ztilt)*sin(atilt)
        CALL RECEIV(V,NUML,RDC(JJ),LAY(JJ),Z(JJ))
        WRITE(6,907) JJ,RDC(JJ),LAY(JJ),Z(JJ),ofstar(jj)
 920    CONTINUE
      ELSE
        IR=-IR
        READ(1,*) (RDC(JJ),jj=1,ir)
        DO 921 JJ=1,IR
c        READ(1,*) RDC(JJ)
        rtmp=rdc(jj)
        rdc(jj)=ztilt+(rtmp-ztilt)*cos(atilt)
        ofstar(jj)=(rtmp-ztilt)*sin(atilt)
        CALL RECEIV(V,NUML,RDC(JJ),LAY(JJ),Z(JJ))
        WRITE(6,907) JJ,RDC(JJ),LAY(JJ),Z(JJ),ofstar(jj)
 921    CONTINUE
      END IF
 907  FORMAT(1H ,I6,F10.1,I6,3F10.1)
      return
      end
      SUBROUTINE RECEIV(VL,NUMLL,RD,LR,Z)                    
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      V(1,1)=V(2,1)                   
      DO 10 I=2,NUML                  
      IF (V(I,1)-RD) 10,10,20         
 10   CONTINUE   
      LR=NUML    
      Z=RD-V(NUML,1)                  
      RETURN     
 20   LR=I-1     
      Z=RD-V(LR,1)                    
      RETURN     
      END        
      SUBROUTINE SOURCE(VL,NUMLL,SD,LSL,ZUP,ZLO)              
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      DO 10 I=2,NUML                  
      IF (V(I,1)-SD) 10,10,20         
 10   CONTINUE   
      LSL=NUML    
Cms *** insert error message if source is in porous sediment
c      IF(LAYTYP(LSL).EQ.5) THEN
c        WRITE(6,900) SD
c        STOP
c      ENDIF
      ZUP=SD-V(NUML,1)                
      ZLO=-ZUP     
      RETURN     
 20   LSL=I-1     
Cms *** insert error message if source is in porous sediment
c      IF(LAYTYP(LSL).EQ.5) THEN
c        WRITE(6,900) SD
c        STOP
c      ENDIF
      ZUP=SD-V(LSL,1)                  
      ZLO=V(I,1)-SD                   
      RETURN     
c 900  FORMAT(' **** SOURCE AT DEPTH ',F7.1,' M      *****'
c     &       ' **** IN POROUS SEDIMENT - NOT ALLOWED *****')
      END        
      SUBROUTINE PINIT1         
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
Cms
Cms ** modified and updated to include porous sediment layers - 4/26/96
Cms
C     INITIALIZATION OF VARIABLES               
C               
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
c
      IERR=0
      IOERR=0
      IFN=0
      PI=4.0*ATAN(1E0)
      CNUL=CMPLX(0.,0.)         
C
C     LAYER CLASSIFICATION:
C
C     LAYTYP=1:   ISOVELOCITY FLUID: V(I,3) = 0
C
C     LAYTYP=2:   1/C**2 LINEAR:     V(I,3) < 0 
C               -V(I,3) = VEL. AT LOWER BOUNDARY
C
C     LAYTYP=3:   ISOVELOCITY SOLID: V(I,3) > 0
C
Cms   LAYTYP=4:   TRANSVERSE ISOTROPIC SOLID: V(I,3) >= 0
Cms               and V(I,2) < 0
Cms
Cms   LAYTYP=5:   Biot model porous sediment:
Cms               V(I,2) < 0 and V(I,3) < 0
      LSOLF=NUML+1
      LSOLL=0
C
C     UPPER AND LOWER HALF SPACES MUST BE ISOVELOCITY
C
      IF (LAYTYP(1).EQ.2) THEN
        LAYTYP(1)=1
        V(1,3)=0
      END IF
      IF (LAYTYP(NUML).EQ.2) THEN
        LAYTYP(NUML)=1
        V(NUML,3)=0
      END IF
      DO 10 I=1,NUML
C
C     FOR SMALL GRADIENTS LAYER TREATED AS ISOVELOCITY
C
      IF (laytyp(i).eq.2.and.abs(v(i,2)+v(i,3)).LT.1E-3) then
       v(i,3)=0E0
       laytyp(i)=1
      end if

      IF (LSOLL.EQ.0.AND.LAYTYP(I).EQ.3) THEN
       LSOLF=I
      END IF
      IF (LAYTYP(I).EQ.3) THEN
       LSOLL=I
      END IF
 10   CONTINUE
C
C     NUMBER OF SOLID LAYERS
C
      NSOL=LSOLL-LSOLF+1
      do I=1,NUML            
       do J=1,nleq               
        IPS(I,J)=0
        do is=1,nsrow
         SS(I,J,is)=CNUL              
         R(I,J,is)=CNUL
        end do               
        do K=1,nleq               
         AUP(I,J,K)=CNUL           
         ALO(I,J,K)=CNUL
        end do
       end do
      end do           

      NEQ=0     
      IF (LAYTYP(1).LT.0) GO TO 100            
      IF (LAYTYP(1).EQ.3.OR.LAYTYP(1).EQ.4) GO TO 50             
Cms  First layer is porous sediment
      IF (LAYTYP(1).EQ.5) THEN
        IPS(1,3)=1
        IPS(1,4)=2
        IPS(1,6)=3
        NEQ = 3
        GO TO 100
      ENDIF
C     LIQUID HALF SPACE         
      IPS(1,3)=1
      NEQ=1     
      GO TO 100 
C     SOLID HALF SPACE          
 50   IPS(1,3)=1
      IPS(1,4)=2
      NEQ=2     
 100  IF (NUMI.EQ.1) GO TO 200  
      DO 190 I=2,NUMI           
Cms  porous sediment layer - note ordering!
       IF (LAYTYP(I).EQ.5) THEN
        IPS(I,1) = NEQ + 1
        IPS(I,2) = NEQ + 2
        IPS(I,5) = NEQ + 3
        IPS(I,3) = NEQ + 4
        IPS(I,4) = NEQ + 5
        IPS(I,6) = NEQ + 6
        NEQ = NEQ + 6
        GOTO 190
       ENDIF

      IF (LAYTYP(I).EQ.3.OR.LAYTYP(I).EQ.4) GO TO 150            
C     LIQUID LAYER              
      IPS(I,1)=NEQ+1            
      IPS(I,3)=NEQ+2            
      NEQ=NEQ+2 
      GO TO 190 
 150  CONTINUE  
C     SOLID LAYER               
      DO 160 J=1,4              
 160  IPS(I,J)=NEQ+J            
      NEQ=NEQ+4 
 190  CONTINUE  
 200  IF (LAYTYP(NUML).LT.0) GO TO 300         

Cms  last layer is porous sediment
      IF (LAYTYP(NUML).EQ.5) THEN
        IPS(NUML,1) = NEQ + 1
        IPS(NUML,2) = NEQ + 2
        IPS(NUML,5) = NEQ + 3
        NEQ = NEQ + 3
        GO TO 300
      ENDIF

      IF (LAYTYP(NUML).EQ.3.OR.LAYTYP(NUML).EQ.4) GO TO 250         
      IPS(NUML,1)=NEQ+1         
      NEQ=NEQ+1 
      GO TO 300 
 250  IPS(NUML,1)=NEQ+1         
      IPS(NUML,2)=NEQ+2         
      NEQ=NEQ+2 
 300  CALL DETBW
      IBW=MIN0(NEQ-1,IBW)
      CALL DETPNT
      RCC=1.0/V(LAYS((LS-1)/2+1),6)
      CALL VSMUL(V(1,6),1,RCC,RCON1,1,NUML)

      DO 340 I=1,LS
 340  RLIND(I)=FLOAT(LAYS(I)*2-1)
C
C     DETERMINE SOURCE AND RECEIVER POINTERS 
C
      DO 350 I=1,NUML
        NOSOU(I)=0
        IFSOU(I)=NRD
        ILSOU(I)=1
        NORCV(I)=0
        IFRCV(I)=NRD
        ILRCV(I)=1
 350  CONTINUE
      do 355 I=1,nltyp
        NUMTS(I)=0
        NUMTR(I)=0
 355  CONTINUE
      DO 360 I=1,LS
        LL=LAYS(I)
        NOSOU(LL)=NOSOU(LL)+1
        IFSOU(LL)=MIN(IFSOU(LL),I)
        ILSOU(LL)=MAX(ILSOU(LL),I)
        IF (NOSOU(LL).NE.(ILSOU(LL)-IFSOU(LL)+1)) THEN
          WRITE(6,*) '*** PINIT1: Source no.',I,' out of order ***'
          STOP
        END IF
        LT=LAYTYP(LL)
        NUMTS(LT)=NUMTS(LT)+1
        NSPNT(NUMTS(LT),LT)=I
c        if (debug) write(6,*) 'pinit1: lt,nspnt=',lt,nspnt(numtr(lt),lt)
 360  CONTINUE
c        if (debug) write(6,*) 'pinit1: numts=',(numts(jj),jj=1,4)

      DO 365 I=1,IR
        LL=LAY(I)
        NORCV(LL)=NORCV(LL)+1
        IFRCV(LL)=MIN(IFRCV(LL),I)
        ILRCV(LL)=MAX(ILRCV(LL),I)
        IF (NORCV(LL).NE.(ILRCV(LL)-IFRCV(LL)+1)) THEN
          WRITE(6,*) '*** PINIT1: Receiver no.',I,' out of order ***'
          STOP
        END IF
        LT=LAYTYP(LL)
        NUMTR(LT)=NUMTR(LT)+1
        NRPNT(NUMTR(LT),LT)=I
c        if (debug) write(6,*) 'pinit1: lt,nrpnt=',lt,nrpnt(numtr(lt),lt)
 365  CONTINUE
c        if (debug) write(6,*) 'pinit1: numtr=',(numtr(jj),jj=1,4)
      RETURN    
      END       
      SUBROUTINE PINIT2         
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
Cms
Cms ** modified and updated to include porous sediment layers - 7/6/92
Cms
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
Cms
Cms  Tempory names for complex wavespeeds in porous sediment layers
      COMPLEX BIOSP1,BIOSP2,BIOSP3
      if (debug) write(6,*) '>>> Entering PINIT2 <<<'
C
C     AIRY FUNCTION SOLUTION ADDED 840907
C     SPEED AT LOWER INTERFACE OF LAYER I IS
C     GIVEN BY -V(I,3) (REPLACING SHEAR).
C
C     COMPLEX CONTOUR INTEGRATION ADDED AS OPTION 'J'  850321
C
      if (debug) write(6,*) 'ls,v,dsq',ls,V(LAYS((LS-1)/2+1),2),dsq
      OFFIMA=FREQ*OFFDB/(8.68588964*V(LAYS((LS-1)/2+1),2))
C
      DISNRM=1E0/DSQ
      akmax=0
      DO 10 I=1,NUML
c >>> check for dispersion
       if (disper(i)) then
        ity=idltyp(i)
        v(i,2)=cpdl(ity,nactf)
        v(i,3)=csdl(ity,nactf)
        v(i,4)=apdl(ity,nactf)
        v(i,5)=asdl(ity,nactf)
        write(6,*) (v(i,jj),jj=1,6)
       end if
      THICK(I)=0E0            
      IF (I.GT.1.AND.I.LT.NUML) THICK(I)=V(I+1,1)-V(I,1)        

      if (laytyp(i).eq.5) then
       akmax=max(akmax,real(dsq/v(i,3)))
      end if

      IF (LAYTYP(I).LT.0.OR.LAYTYP(I).GE.4) GO TO 10             

      IF (LAYTYP(I).LE.2.AND.V(I,4).LE.0) THEN
      FK=FREQ/1000.
      VV=V(I,2)*FK*1E-6*(.007+.264/(2.89+FK**2))
      ELSE
      VV=V(I,4)
      END IF
      VI4=FREQ*VV/(8.68588964*V(I,2))           
      AK(I,1)=DSQ/V(I,2)+CMPLX(0E0,-VI4)            
      AK2(I,1)=AK(I,1)*AK(I,1)  
      ALAME(I,1)=CSQ*V(I,6)/AK2(I,1)            
C
C     AIRY FUNCTION SOLUTION ADDED 840907
C     SPEED AT LOWER INTERFACE OF LAYER I IS
C     GIVEN BY -V(I,3) (REPLACING SHEAR).
C
      IF (LAYTYP(I).EQ.2) THEN
       DELTAK=RIMAG(AK(I,1))/REAL(AK(I,1))
       AKU=REAL(AK(I,1))
       AKL=-DSQ/V(I,3)
       AKU2=AKU*AKU*(1E0-DELTAK*DELTAK)
       AKL2=AKL*AKL*(1E0-DELTAK*DELTAK)
       GRAD=(AKL2-AKU2)/THICK(I)
       AA=SIGN(ABS(GRAD)**0.333333333333333,GRAD)
       ACO(I)=AA*CMPLX(1E0,DELTAK*0.666666666666667)
       AAM2=1E0/(AA*AA)
       CCO(I)=AAM2*CMPLX(1E0,-DELTAK*1.33333333333333)
       BCO(I)=CCO(I)*AKU2*CMPLX(1E0,2*DELTAK)
C      WRITE(6,876) I,ACO(I),BCO(I),CCO(I)
C876   FORMAT(1H ,I3,3(/1H ,2G20.8))
      END IF
      IF (laytyp(i).eq.3) then             
       VI5=FREQ*V(I,5)/(8.68588964*V(I,3))           
       AK(I,2)=DSQ/V(I,3)+CMPLX(0E0,-VI5)            
       AK2(I,2)=AK(I,2)*AK(I,2)  
       ALAME(I,2)=CSQ*V(I,6)/AK2(I,2)            
       ALAME(I,1)=ALAME(I,1)-2*ALAME(I,2)        
      end if
      akmax=max(akmax,abs(ak(i,2)),abs(ak(i,1)))
      if (debug) write(6,*) 'disnrm=',disnrm
 10   CONTINUE  
      disnrm=1e0/akmax
C
C     DETERMINE INTEGRATION SAMPLING FOR N-K SCATTERING
      DO 12 I=2,NUML
      if (rough2(i).gt.1e-6) then
       IF (LAYTYP(I-1).LE.0) THEN
         DLWNK(I)=REAL(AK(I,1))/NSIP
       ELSE IF (LAYTYP(I).LE.0) THEN
         DLWNK(I)=REAL(AK(I-1,1))/NSIP
       ELSE IF (LAYTYP(I).NE.4.AND.LAYTYP(I-1).NE.4) THEN
        IF (LAYS((LS-1)/2+1).LT.I) THEN
         DLWNK(I)=REAL(AK(I-1,1))/NSIP
        ELSE
         DLWNK(I)=REAL(AK(I,1))/NSIP
        END IF
       ELSE
         DLWNK(I)=1E10
       END IF
c >>> find width of significant roughness spectrum
       ref=1e-3*p(0E0,i)
       imx(i)=0
 110   imx(i)=imx(i)+1
       rr=p(imx(i)*dlwnk(i),i)
       if (rr.ge.ref) go to 110
       rkmx=imx(i)*dlwnk(i)
c      if (goff) then
c       IMX(I)=16E0/(CLEN(I)*DLWNK(I))
c      else
c       IMX(I)=8E0/(CLEN(I)*DLWNK(I))
c      end if
       IF (IMX(I).GE.1.AND.IMX(I).LT.IMXMIN) THEN
         IMX(I)=IMXMIN
c         dlwnk(i)=rkmx/imx(i)
         DLWNK(I)=8E0/(abs(CLEN(I))*IMX(I))
       END IF
      write(6,*) '>>> Roughness spectrum sampling parameters <<<'
      WRITE(6,*) 'Interface:',i
      write(6,*) 'dq=   ',DLWNK(I)
      write(6,*) 'n_min=',IMXMIN
      write(6,*) 'n_q=  ',IMX(I)
      write(6,*) 'k_max=',rkmx
      write(6,*) 'k_L=  ',2*pi/clen(i)
      end if
 12   CONTINUE
      PCORR=CSQ*V(LAYS((LS-1)/2+1),6)         
      if (debug) write(6,*) ls,lays(1),pcorr
      PCORR=1E0/PCORR
      if (debug) write(6,*) ls,lays(1),pcorr
      DO 13 I=1,NUMT(1)
        LL=LAYT(I,1)
        CON1(LL)=PCORR*V(LL,6)*CSQ
 13   CONTINUE
      DO 14 I=1,NUMT(2)
        LL=LAYT(I,2)
        CON1(LL)=PCORR*V(LL,6)*CSQ
 14   CONTINUE
      DO 15 I=1,NUMT(3)
        LL=LAYT(I,3)
        CON1(LL)=PCORR*ALAME(LL,2)
        con6(ll)=pcorr*ak2(ll,1)*(alame(ll,1)+0.6666667*alame(ll,2))
 15     CONTINUE
      IF (NTISOL.GT.0) THEN
c *** changed to new ti subroutines
c        CALL RWDBUF(41)
        DO 20 I=1,NUMT(4)
        LL=LAYT(I,4)
c        RCON1(LL)=DSQ
c        CON1(LL)=-AI*CSQ*PCORR
         CON1(LL)=PCORR
 20     CONTINUE
      END IF
Cms
Cms  Compute frequency dependent properties for
Cms  porous sediment layers,
Cms
      IF(NBL.GT.0) THEN
        DO 25 J=1,NBL
Cms  then set scaling factor for porous sediment layers
          LL=LAYT(J,5)
          CALL BLPROP(LL)
          CON1(LL)=PCORR
Cms  and write complex wave-speeds to output except in OASP.
          IF(PROGNM(1:4).EQ.'OASP') GOTO 25
          WRITE(6,996)
          BIOSP1 = REAL(DSQ)/SQRT(CK1(LL))
          BIOSP2 = REAL(DSQ)/SQRT(CK2(LL))
          IF(REAL(BIOSP1).LT.REAL(BIOSP2)) THEN
            BIOSP3=BIOSP1
            BIOSP1=BIOSP2
            BIOSP2=BIOSP3
          ENDIF
          BIOSP3 = REAL(DSQ)/SQRT(CKS(LL))
          WRITE(6,995) LL, BIOSP1, BIOSP2, BIOSP3
 25     CONTINUE
 996  FORMAT(/,' COMPLEX WAVESPEEDS IN POROUS SEDIMENT LAYERS',//,
     & ' LAY',T16,'FAST SPEED',T37,'SLOW SPEED',T57,'SHEAR SPEED')
 995  FORMAT(1X,I3,3(3X,2F9.3))
      ENDIF
c
c *** integration constants
c
      IF (ICDR.EQ.0) THEN
       FNIFAC =  SQRT( 2.0/PI ) * 0.5    
      ELSE
       FNIFAC=1E0
      END IF
      FNI5=DLWVNO*FNIFAC
c
c *** source phases
c
      if (trfsou) then
       do is=1,ls
        cphfac(is)=cmplx(1e0,0e0)
        call rdbuf(81,cfile,2*nstrf)
        do isnm=1,nstrf
c Note normalization is done below here it should be seismic moment
c         sval(is,isnm)=cfile(isnm)/(4E0*PI*v(lays(is),6)*csq)
         sval(is,isnm)=cfile(isnm)
        end do
c        if (debug) then
c         write(6,'(1x,i3,1x,2(e12.4))') is,sval(is,1)
c        end if
       end do
      else if (extlar) then
       do 700 is=1,ls
        cphfac(is)=sstren(is)*exp(-ai*dsq*sdelay(is))
 700    continue
      else
       if (debug) write(6,*) 'entering PHASES'
       CALL PHASES()
       if (debug) write(6,*) 'exiting PHASES'
      end if
C
C *** Source normalizations
c
      if (debug) write(6,*) 'Source normalization'
      DO 900 IS=1,LS
      LL=LAYS(IS)
      IF (LAYTYP(LL).LE.2) then
c      IF (LAYTYP(LL).LE.2.or.
c     &    (laytyp(ll).eq.3.and.v(ll,3).lt.100.0)) THEN
C ***  FLUID SOURCES NORMALIZED TO YIELD P=1 AT R=1 m.
c       if (laytyp(ll).eq.3) then
c        write(6,*) '>>> WARNING: Source normalization assumes fluid.'
c       end if
       IF (ICDR.EQ.1) THEN
        IF (NWVNO.GT.1) THEN
         cphfac(is)=cphfac(is)*SQRT(FREQ/V(LAYs(is),2))
     &                         /(v(lays(is),6)*csq)
        else
         cphfac(is)=freq*cphfac(is)/(v(lays(is),6)*csq)
        END IF
       ELSE
         cphfac(is)=cphfac(is)/(v(lays(is),6)*csq)
       END IF
      ELSE
C *** SEISMIC SOURCES NORMALIZED TO UNIT seismic moment OR
C     UNIT FORCE.
       if (debug) write(6,*) 'is,csq,v(is,6)',is,csq,v(lays(is),6)
       cphfac(is)=cphfac(is)/(4E0*PI*v(lays(is),6)*csq)
       if (debug) write(6,*) 'is,cphfac=',is,cphfac(is)
c       write(6,*) 'Source moment:',is,20.0*
c     &  alog10(real(alame(lays(is),2))/real(v(lays(is),6)*dsq))
      END IF
 900  continue
C
C *** PRESSURE AND PARTICLE VELOCITY CONVERSION FACTORS
C
      CPFAC=1E0/PCORR
      CWUFAC=AI*DSQ
      if (debug) write(6,*) '>>> exiting pinit2 <<<'
      RETURN    
      END       
      SUBROUTINE LINARR(SD)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      INCLUDE 'compar.f'
      INCLUDE 'comnrd.f'
C
      DO 10 I=1,LS  
 10   SDC(I)=SD+(I-1-(LS-1)/2.0)*ABS(DELTA)  
      if (debug) write(6,*) '>>> Exiting PINIT2 <<<'
      RETURN        
      END           
      SUBROUTINE PHASES
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      INCLUDE 'compar.f'
      INCLUDE 'comnrd.f'
      INCLUDE 'comnla.f'
      PI=4.*ATAN(1.)
      VMEAN=0       
      IF (srctyp.eq.2.AND.V(LAYS((LS-1)/2+1),3).GT.1E-10) THEN
        INDV=3
      ELSE 
        INDV=2
      END IF

      DO 10 I=1,LS  
 10      vmean=vmean+speed(indv-1,sdc(i))/ls
c 10   VMEAN=VMEAN+V(LAYS(I),INDV)/LS       
      ANG=2*PI*FREQ*ABS(DELTA)*SIN(THETA)/VMEAN                  
      HMEAN=.5*(SDC(LS)+SDC(1))
      ALEN=ABS(SDC(LS)-SDC(1))
      IF (LS.EQ.1) GO TO 15
      GO TO (15,25,35,45,55),LTYP                
 15   DO 20 I=1,LS  
 20   CPHFAC(I)=EXP(CMPLX(0.,-(I-1-(LS-1)/2.0)*ANG))/LS       
      RETURN        
 25   DO 30 I=1,LS  
      CPHFAC(I)=EXP(CMPLX(0.,-(I-1-(LS-1)/2.0)*ANG))       
      FAC=1.0-COS((I-1)*2.0*PI/FLOAT(LS-1))                 
      CPHFAC(I)=CPHFAC(I)*FAC/LS           
 30   CONTINUE      
      RETURN        
 35   CONTINUE
      IF (ABS(THETA).GT.1E-8) THEN
      HH=FOCDEP-HMEAN
      DM=HH/SIN(THETA)
      R2=(HH/TAN(THETA))**2
      ELSE
      DM=FOCDEP
      R2=DM*DM
      END IF
      ALS=1E0/LS
      DO 40 I=1,LS
      IF (ABS(THETA).GT.1E-8) THEN
      H=FOCDEP-SDC(I)
      ELSE
      H=HMEAN-SDC(I)
      END IF
      D=SQRT(H*H+R2)
      ANG=(D-DM)*2*PI*FREQ/VMEAN
      CPHFAC(I)=EXP(CMPLX(0.,ANG))
      FAC=1.0-COS((I-1)*2.0*PI/FLOAT(LS-1))
      CPHFAC(I)=CPHFAC(I)*FAC*ALS
 40   CONTINUE
      RETURN
 45   CONTINUE
      FSUM=0.
      DO 48 I=1,LS  
      CPHFAC(I)=EXP(CMPLX(0.,-(I-1-(LS-1)/2.0)*ANG))       
      FAC=EXP(-(4.*(SDC(I)-HMEAN)/ALEN)**2)
      FSUM=FSUM+FAC
      CPHFAC(I)=CPHFAC(I)*FAC           
 48   CONTINUE      
      DO 49 I=1,LS
 49   CPHFAC(I)=CPHFAC(I)/FSUM
      RETURN        
 55   CONTINUE
      IF (ABS(THETA).GT.1E-8) THEN
      HH=FOCDEP-HMEAN
      DM=HH/SIN(THETA)
      R2=(HH/TAN(THETA))**2
      ELSE
      DM=FOCDEP
      R2=DM*DM
      END IF
      FSUM=0.
      DO 58 I=1,LS
      IF (ABS(THETA).GT.1E-8) THEN
      H=FOCDEP-SDC(I)
      ELSE
      H=HMEAN-SDC(I)
      END IF
      D=SQRT(H*H+R2)
      ANG=(D-DM)*2*PI*FREQ/VMEAN
      CPHFAC(I)=EXP(CMPLX(0.,ANG))
      FAC=EXP(-(4.*(SDC(I)-HMEAN)/ALEN)**2)
      FSUM=FSUM+FAC
      CPHFAC(I)=CPHFAC(I)*FAC
 58   CONTINUE
      DO 59 I=1,LS
 59   CPHFAC(I)=CPHFAC(I)/FSUM
      RETURN
      END           
      real function speed(i,zd)
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
      call receiv(v,numl,zd,ll,zz)
      if (laytyp(ll).ne.2) then
       speed = v(ll,i+1)
      else if (i.eq.2) then
       speed = 0e0
      else
       b=1e0/(v(ll,2)**2)
       a=(1e0/(v(ll,3)**2)-b)/(v(ll+1,1)-v(ll,1))
       speed=sqrt(1E0/(a*zz+b))
      end if
      return
      end

      SUBROUTINE CALINT    
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
      COMPLEX FACSQ
c >>> One set of simultaneous sources
      nsrow=1
c >>>
      NS=ICUT2-ICUT1+1
C *** OPEN SCRATCH FILE FOR KERNELS
      IF (DECOMP) THEN
       NGFILS=5
       IF (ISIZE.LT.IR*25) STOP '>>> PARAMETER ISIZE TOO SMALL <<<'
      ELSE
       NGFILS=1
       IF (ISIZE.LT.IR*5) STOP '>>> PARAMETER ISIZE TOO SMALL <<<'
      END IF
      LUOFF=LUGRN-1
c *** determine buffer size from remaining amount of memory
      NBLOCKS=MEMLFT()/NGFILS-1 
      DO 5 IFC=1,NGFILS
       CALL OPNBUF(LUOFF+IFC,2*IR,NS*NOUT,NBLOCKS)
 5    CONTINUE
      IF (SCTOUT) THEN
C *** OPEN FILE AND WRITE HEADER
       WRITE(45) FREQ,LAYS(1),nwvno,nflag,fni5
       WRITE(46) FREQ,LAYS(1),nwvno,nflag,fni5
      END IF
C *** WAVENUMBER RAMP
      CALL VRAMP(WK0,DLWVNO,FAC,1,NWVNO)
C *** WAVENUMBER LOOP
      DO 20 II=ICUT1,ICUT2         
      WVNO=CMPLX(FAC(II),OFFIMA)
      IF (ICDR.EQ.0) THEN
        FACSQ=SQRT(WVNO)
      END IF
c >>> One set of sources
      nsrow=1
      CALL INITS
      CALL BUILD                  
      CALL SOLVE    
C *** DETERMINANT INVERSE
      IF (DETERM) ARG(II)=DETMNT
      IF (IERR.GT.0) RETURN
C      IF (DEBUG) WRITE(6,*) 'CALLING KERNEL'
      IF (DECOMP) THEN
       CALL KERDEC(CFILE)
      ELSE
       CALL KERNEL(CFILE)
      END IF
      DO 10 IFC=1,NGFILS
      IF (IOUT(1).GT.0) THEN
       INDXCF=1+IR*3*(IFC-1)
        IF (ICDR.EQ.0) THEN
          CALL CVMUL(CFILE(INDXCF),2,FACSQ,0,CFILE(INDXCF),2,IR,1)
        END IF
        CALL WRBUF(LUOFF+IFC,CFILE(INDXCF),2*IR)
      END IF
      IF (IOUT(2).GT.0) THEN
       INDXCF=1+IR*(1+3*(IFC-1))
        IF (ICDR.EQ.0) THEN
          CALL CVMUL(CFILE(INDXCF),2,FACSQ,0,CFILE(INDXCF),2,IR,1)
        END IF
        CALL WRBUF(LUOFF+IFC,CFILE(INDXCF),2*IR)
      END IF
      IF (IOUT(3).GT.0) THEN
       INDXCF=1+IR*(2+3*(IFC-1))
        IF (ICDR.EQ.0) THEN
          CALL CVMUL(CFILE(INDXCF),2,FACSQ,0,CFILE(INDXCF),2,IR,1)
        END IF
        CALL WRBUF(LUOFF+IFC,CFILE(INDXCF),2*IR)
      END IF 
 10   CONTINUE
C
C *** OUTPUT ROUGH SURFACE DISCONTINUITIES FOR CALCULATING SCATTERED
C     FIELD
C
      IF (SCTOUT) CALL SCTRHS(CMPLX(1E0,0E0))
 20   CONTINUE     
      DO 30 IFC=1,NGFILS
      CALL ENFBUF(LUOFF+IFC)
 30   CONTINUE
C                   
      RETURN        
C                   
      END           
      SUBROUTINE CALIN3    
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
c >>> Kernel extrapolation introduced 950927
      parameter (fac_ex=2.0)    
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
      COMPLEX FACSQ,CC
      complex pot_save(nleq,nrd,isrowmax)
      real aslope(nleq,nrd,isrowmax),pslope(nleq,nrd,isrowmax)
      logical negsp
c >>> One set of simultaneous sources
      nsrow=1
c >>>>>>>>>>>>>>>>>>
      NS=ICUT2-ICUT1+1
C *** OPEN SCRATCH FILE FOR KERNELS
      IF (DECOMP) THEN
       NGFILS=5
       IF (ISIZE.LT.IR*5*NPAR*nsrow) 
     &      STOP '>>> PARAMETER ISIZE TOO SMALL <<<'
      ELSE
       NGFILS=1
       IF (ISIZE.LT.IR*NPAR*nsrow) 
     &      STOP '>>> PARAMETER ISIZE TOO SMALL <<<'
      END IF
      LUOFF=LUGRN-1
c *** determine buffer size from remaining amount of memory
      NBLOCKS=MEMLFT()/NGFILS-1
      NWSTEP=1
      NKFIL=NS

      DO 5 IFC=1,NGFILS
       CALL OPNBUF(LUOFF+IFC,2*IR*nsrow,NKFIL*NOUT,NBLOCKS)
 5    CONTINUE
      
      IF (SCTOUT) THEN
C *** OPEN FILE AND WRITE HEADER
       WRITE(45) FREQ,LAYS(1),nwvno,nflag,fni5
       WRITE(46) FREQ,LAYS(1),nwvno,nflag,fni5
      END IF
c >>> potentials at patch interface
      if (outpot) then
c >> this was the old stuff:
c       call opfilw(46,ioerr)
c       write(46,*) v(numl-1,2),v(numl-1,6)
c       write(46,*) (v(numl,j),j=2,6)
c >>> patch range set to max range
c       write(46,*) rpat,numl
c       write(46,*) freq
c       write(46,*) ns,wk0+(icut1-1)*dlwvno,dlwvno,omegim
c
c >>> 970912   Jaiyong Lee's format
c
C
C *** OASK module : Header parameters
C
       WRITE(60) FREQ,NWVNO,DLWVNO,omegim
      end if
C *** WAVENUMBER RAMP
      CALL VRAMP(WK0,DLWVNO,FAC,1,NWVNO)
      NGVALS=2*IR*nsrow*NPAR*NGFILS
        if (doppler) fsave=freq
c >>> set up for extrapolation
      ic_ext = fac_ex *icw2
      iiwmax=100
C *** WAVENUMBER LOOP
      DO 20 II=ICUT1,ICUT2,NWSTEP         
      WVNO=CMPLX(FAC(II),OFFIMA)
c
c >>> doppler
c
      if (doppler) then
       freq=fsave+real(wvno)*vrec/(2*pi)
       dsq=2*pi*freq+cmplx(0e0,omegim)
       rdsq=dsq
       csq=dsq*dsq
       if (prognm(1:4).EQ.'OASP') then
        fshft=fsave-real(wvno)*(vsou-vrec)/(2*pi)
        posneg=sign(1.0,fshft)
        fshft=abs(fshft)
        indf=fshft/dlfreq+1
        if (indf.ge.1) then
         fdf=fshft-(indf-1)*dlfreq
         amp1=abs(cffs(indf))
         pha1=atan2z(rimag(cffs(indf)),real(cffs(indf)))
         amp2=abs(cffs(indf+1))
         pha2=atan2z(rimag(cffs(indf+1)),real(cffs(indf+1)))
c >>> phase unwrapping
         if (abs(pha2-pha1).gt.pi) then
          if (pha2.gt.pha1) then
           pha2=pha2-2e0*pi
          else
           pha1=pha1-2e0*pi
          end if
         end if
         ampf=amp1+fdf*(amp2-amp1)/dlfreq
         phaf=pha1+fdf*(pha2-pha1)/dlfreq
         cc=cmplx(ampf*cos(phaf),posneg*ampf*sin(phaf))
        else
         cc=cffs(1)
        end if
       else
        cc=1E0
       end if
       call pinit2
       do is=1,ls
        cphfac(is)=cphfac(is)*cc
       end do
      end if
             
      CALL INITS

       if (extrap.and.ii.gt.ic_ext) then
c >>> extrapolate
        iidiff=ii-ic_ext+1
        if (ii-ic_ext.lt.iiwmax) then
         etfac=0.5*(1E0+COS((II-ic_ext)*PI/iiwmax))
        else
         etfac=0e0
        end if
        do is=1,nsrow
         do jdp=1,nrd
          do iwav=1,4
           ang=iidiff*pslope(iwav,jdp,is)
           val=iidiff*aslope(iwav,jdp,is)
           if (val.gt.0) then
            val=val*etfac
           end if
           pot(iwav,jdp,is)=pot_save(iwav,jdp,is)*cexpt(cmplx(val,ang))
          end do
         end do
        end do
       else
        CALL BUILD                  
        CALL SOLVE
        IF (IERR.GT.0) RETURN
c >>> Compute wavefield potentials
        call wfield()
       end if

c >>>  check for extrapolation

       if (extrap) then
        if (ii.eq.ic_ext-1)then
         do is=1,nsrow
          do jdp=1,ir
           do iwav=1,4
            pot_save(iwav,jdp,is)=pot(iwav,jdp,is)
           end do
          end do
         end do
        else if (ii.eq.ic_ext) then
         write(6,*) 'Starting extrapolation, iw=',ii
         do is=1,nsrow
          do ijp=1,ir
           do iwav=1,4
            if (abs(pot_save(iwav,jdp,is)).ne.0e0) then
             cc= pot(iwav,jdp,is)/pot_save(iwav,jdp,is)
            else
             cc=1e0
            end if
            aslope(iwav,jdp,is)=alog(abs(cc))
            valmax=max(valmax,aslope(iwav,jdp,is))
            pslope(iwav,jdp,is)=atan2z(rimag(cc),real(cc))
           end do
          end do
         end do
         iiwmax=40.0/valmax
        end if
       end if



C *** DETERMINANT INVERSE
      IF (DETERM) ARG(II)=DETMNT
      IF (IERR.GT.0) RETURN

      IF (DECOMP) THEN
       CALL KERDEC(CFILEK)
      ELSE
       CALL KERNEL(CFILEK(1))
       if (debug) write(6,*) '>> CALIN3: exiting kernel'
      END IF

c *** tapering
      IF (II.LT.ICW1) THEN
       TFAC=0.5*(1E0+COS((II-ICW1)*PI/(ICUT1-ICW1-1)))
       CALL VSMUL(CFILEK(1),1,TFAC,CFILEK(1),1,NGVALS)
      ELSE IF (II.GT.ICW2) THEN
       TFAC=0.5*(1E0+COS((II-ICW2)*PI/(ICUT2-ICW2+1)))
       CALL VSMUL(CFILEK(1),1,TFAC,CFILEK(1),1,NGVALS)
      else
       tfac=1e0
      END IF

      DO 10 IFC=1,NGFILS
       do 10 ipp=1,npar
       IF (IOUT(ipp).GT.0) THEN
        INDXCF=1+IR*nsrow*(ipp-1+NPAR*(IFC-1))
        CALL WRBUF(LUOFF+IFC,CFILEK(INDXCF),2*IR*nsrow)
       END IF
 10   CONTINUE
C
C *** OUTPUT ROUGH SURFACE DISCONTINUITIES FOR CALCULATING SCATTERED
C     FIELD
C
      IF (SCTOUT) CALL SCTRHS(CMPLX(tfac,0E0))
      if (outpot) then
c >>> This is the old Henry Fan format
c       cc=fni5*sqrt(fac(ii))*tfac*exp(-ai*wvno*rpat)/sqrt(rpat)
c       write(46,'(2G15.6)') cc*ss(numl-1,1,1),cc*ss(numl-1,3,1),
c     &             cc*ss(numl,1,1),cmplx(0e0,0e0),cc*ai*ss(numl,2,1)
C
C *** OASK module : dump ss vectors
C
       write(60) wvno,
     &     (tfac*ss(inpatch,jj,1),jj=1,4),
     &     (tfac*ss(inpatch1,jj,1),jj=1,4)
      end if
 20   CONTINUE     
      if (doppler) freq=fsave
c
c *** Endfile on grf files
      DO 30 IFC=1,NGFILS
       CALL ENFBUF(LUOFF+IFC)
 30   CONTINUE
      RETURN        
      END           
      SUBROUTINE CHKSOL
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      INCLUDE 'compar.f'

          IF (IERR.NE.0) THEN
C            CALL PREQV(NUML,NUMI)
            WRITE(6,9987) IERR
9987        FORMAT(//1H ,'**** EXECUTION TERMINATED ***',
     -            //1H ,'**** ERROR NUMBER : ',I3)
            STOP
         END IF
         IF (IOERR.NE.0) THEN
            WRITE(6,9887) IOERR
9887        FORMAT(//1H ,'**** EXECUTION TERMINATED ****',
     -            //1H ,'**** IO- ERROR NUMBER : ',I3)
            STOP
         END IF
       RETURN
       END
      SUBROUTINE GETKNL(NREC,M,IS)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
      IF (DEBUG) WRITE(6,*) 'ENTER GETKNL'
C
C    READ KERNELS FROM SCRATCH FILE
C
      IF (DEBUG) WRITE(6,*) 'RWDBUF'
      CALL RWDBUF(LUTGRN)
      IF (DEBUG) WRITE(6,*) 'EXIT RWDBUF'
      NN=2*NP
      DO 2 I=1,npar
       IF (IOUT(I).GT.0) THEN
        DO 1 JR=1,ICUT1-1
 1      CFF(JR,I)=CNUL
        DO 11 JR=ICUT2+1,NWVNO
 11     CFF(JR,I)=CNUL
       END IF
 2    CONTINUE
      LREC=2*IR*MSUFT*ISROW
      INDX=NREC+((IS-1)*MSUFT+(M-1))*IR
      DO 4 JR=ICUT1,ICUT2
      DO 3 I=1,npar
      IF (IOUT(I).GT.0) THEN
       CALL RDBUF(LUTGRN,CFILE,LREC)
       CFF(JR,I)=CFILE(INDX)
      END IF
 3    CONTINUE
 4    CONTINUE
C
C
      RETURN
      END
      SUBROUTINE SCTRHS(cfac)              
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
C
C     WRITES SCATTERING RIGHT HAND SIDES TO FILE 45
c
      PARAMETER (SQ2PI=2.506628,ONEO2PI=1.591549E-1)
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
C
C     LOCAL COEFFICIENT MATRICES RE-DEFINED
C
      COMPLEX DBDZ(4),BLC(4)
      COMPLEX S,cfac
c >>> Remove normalizations
c     Stress equations should be divided by pcorr to yield true stresses
c      disnmi=1e0/disnrm
c      strnmi=1e0/strnrm
      pcoi=1e0/pcorr
      S=WVNO
      do is=1,nsrow
       do J=1,4
        do I=1,NUMI
         ROIN(I,J,is) = CMPLX(0E0,0E0)
         RUIN(I,J,is) = CMPLX(0E0,0E0)
        end do
       end do
      end do

       IF (NUMT(1).GT.0) CALL LIQLAY
       IF (NUMT(2).GT.0) CALL AIRLAY
       IF (NUMT(3).GT.0) CALL SOLLAY
       IF (NUMT(4).GT.0) CALL TISOLL
      DO 10 IN=1,NUMI
      IN1=IN+1      
      IF (ROUGH2(IN1).LT.1E-10) GO TO 10

C     HOMEGENEOUS PART
      is=1
      DO 252 JJ=1,4
      DBDZ(JJ)   =   -(-ALFA(IN))*ALO(IN,JJ,1)*SS(IN,1,is)
     1              -(-BETA(IN))*ALO(IN,JJ,2)*SS(IN,2,is)
     2              -ALFA(IN)*ALO(IN,JJ,3)*SS(IN,3,is)
     3              -BETA(IN)*ALO(IN,JJ,4)*SS(IN,4,is)
      DBDZ(JJ)   = DBDZ(JJ) - (-ALFA(IN1))*AUP(IN1,JJ,1)*SS(IN1,1,is)
     2           - (-BETA(IN1))*AUP(IN1,JJ,2)*SS(IN1,2,is)
     3           - ALFA(IN1)*AUP(IN1,JJ,3)*SS(IN1,3,is)
     4           - BETA(IN1)*AUP(IN1,JJ,4)*SS(IN1,4,is)
 252  CONTINUE

C     SOURCE TERMS SIMMILARLY (ONLY PROP TOWARDS INTERFACE)

      DO 256 JJ=1,4
      DBDZ(JJ) = DBDZ(JJ) + (-ALFA(IN))*ROIN(IN,JJ,is)
      DBDZ(JJ) = DBDZ(JJ) + ALFA(IN1)*RUIN(IN,JJ,is)
 256  CONTINUE     
c
c >>> remove stress normalization
c
      do jj=3,4
       dbdz(jj)=dbdz(jj)*pcoi
      end do
C
C     ROTATION TERMS
C
        DO 2502 JJ=1,4
 2502     BLC(JJ)=CMPLX(0E0,0E0)
        DO 2504 JJ=1,4
          BLC(1)= BLC(1)-(-AI*ALO(IN,2,JJ))*SS(IN,JJ,is)
          BLC(2)= BLC(2)-(-AI*ALO(IN,1,JJ))*SS(IN,JJ,is)
          BLC(3)= BLC(3)-(-2E0*AI*ALO(IN,4,JJ))*SS(IN,JJ,is)
          BLC(1)= BLC(1)-(-AI*AUP(IN1,2,JJ))*SS(IN1,JJ,is)
          BLC(2)= BLC(2)-(-AI*AUP(IN1,1,JJ))*SS(IN1,JJ,is)
          BLC(3)= BLC(3)-(-2E0*AI*AUP(IN1,4,JJ))*SS(IN1,JJ,is)
 2504   CONTINUE

C     rotation for shear stress

        IF (LAYTYP(IN).EQ.3) THEN 
         BLC(4)=-2E0*(-AI)*con1(IN)*SS(IN,1,is)*
     &          (-ALFA(IN)*ALO(IN,1,1)-WVNO*ALO(IN,2,1))
         BLC(4)=BLC(4)-2E0*(-AI)*con1(in)*SS(IN,2,is)*
     &          (-BETA(IN)*ALO(IN,1,2)-WVNO*ALO(IN,2,2))
         BLC(4)=BLC(4)-2E0*(-AI)*CON1(IN)*SS(IN,3,is)*
     &          (ALFA(IN)*ALO(IN,1,3)-WVNO*ALO(IN,2,3))
         BLC(4)=BLC(4)-2E0*(-AI)*CON1(IN)*SS(IN,4,is)*
     &          (BETA(IN)*ALO(IN,1,4)-WVNO*ALO(IN,2,4))
        END IF
        IF (LAYTYP(IN1).EQ.3) THEN
         BLC(4)=BLC(4)-2E0*(-AI)*CON1(IN1)*SS(IN1,1,is)*
     &          (-ALFA(IN1)*AUP(IN1,1,1)-WVNO*AUP(IN1,2,1))
         BLC(4)=BLC(4)-2E0*(-AI)*CON1(IN1)*SS(IN1,2,is)*
     &          (-BETA(IN1)*AUP(IN1,1,2)-WVNO*AUP(IN1,2,2))
         BLC(4)=BLC(4)-2E0*(-AI)*CON1(IN1)*SS(IN1,3,is)*
     &          (ALFA(IN1)*AUP(IN1,1,3)-WVNO*AUP(IN1,2,3))
         BLC(4)=BLC(4)-2E0*(-AI)*CON1(IN1)*SS(IN1,4,is)*
     &          (BETA(IN1)*AUP(IN1,1,4)-WVNO*AUP(IN1,2,4))
        END IF
C       SOURCE TERMS

         BLC(1)=BLC(1)+(-AI*ROIN(IN,2,is))
         BLC(2)=BLC(2)+(-AI*ROIN(IN,1,is))
         BLC(3)=BLC(3)+(-2E0*AI*ROIN(IN,4,is))
        IF (LAYTYP(IN).EQ.3) THEN
         BLC(4)=BLC(4)+2*(-AI)*CON1(IN)*
     &          (-ALFA(IN)*ROIN(IN,1,is)-WVNO*ROIN(IN,2,is))
        END IF
         BLC(1)=BLC(1)+(-AI*RUIN(IN,2,is))
         BLC(2)=BLC(2)+(-AI*RUIN(IN,1,is))
         BLC(3)=BLC(3)+(-2*AI*RUIN(IN,4,is))
        IF (LAYTYP(IN1).EQ.3) THEN
         BLC(4)=BLC(4)+2*(-AI)*CON1(IN1)*
     &          (ALFA(IN1)*RUIN(IN,1,is)-WVNO*RUIN(IN,2,is))
        END IF
c
c >>> remove stress normalization 
c
      do jj=3,4
       blc(jj)=blc(jj)*pcoi
      end do
       WRITE(45) WVNO,IN1,(DBDZ(JJ)*cfac,JJ=1,4),(BLC(JJ)*cfac,JJ=1,4)
       write(46) in1,wvno,-ai*alfa(in1),
     &           cfac*ss(in1,1,1),cfac*ss(in1,3,1)
 10   CONTINUE
      RETURN        
      END           

      SUBROUTINE DETPNT      
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
Cms
Cms ** modified to account for porous sediment layers - 7/3/92
Cms
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
C
C     DETERMINE NUMBER OF ROWS FOR EACH INTERFACE
C
      ICNT=1
      DO 510 IN=1,NUMI      
      IN1=IN+1              
Cms
Cms  Add code to treat interface with porous sediment
Cms  on either side.
Cms
        IF(LAYTYP(IN).EQ.5) THEN
          IF(LAYTYP(IN1).LE.0) NRI(IN)=3
          IF(LAYTYP(IN1).EQ.1) NRI(IN)=4
          IF(LAYTYP(IN1).EQ.2) NRI(IN)=4
          IF(LAYTYP(IN1).EQ.3) NRI(IN)=5
          IF(LAYTYP(IN1).EQ.4) NRI(IN)=5
          IF(LAYTYP(IN1).EQ.5) NRI(IN)=6
          GO TO 500
        ELSEIF(LAYTYP(IN1).EQ.5) THEN
          IF(LAYTYP(IN).LE.0) NRI(IN)=3
          IF(LAYTYP(IN).EQ.1) NRI(IN)=4
          IF(LAYTYP(IN).EQ.2) NRI(IN)=4
          IF(LAYTYP(IN).EQ.3) NRI(IN)=5
          IF(LAYTYP(IN).EQ.4) NRI(IN)=5
          GO TO 500
        ENDIF
Cms
      IF (V(IN,2).GT.1E-10) GO TO 100                 
C 
C     VACUUM UPPER HALFSPACE
C     **********************
      IF (V(IN1,3).GT.1E-10) GO TO 50                 
C     NEXT LAYER OR HALFSPACE IS LIQUID               
C     ---------------------------------               
      NRI(IN)=1
      GO TO 500             
 50   CONTINUE              
C     NEXT LAYER OR HALFSPACE IS SOLID                
C     --------------------------------                
      NRI(IN)=2
      GO TO 500             
 100  IF (V(IN,3).GT.1E-10) GO TO 200                 
C 
C     LAYER OR HALFSPACE OVER INTERFACE IS LIQUID     
C     *******************************************     
      IF (V(IN1,2).GT.1E-10) GO TO 125                
C     LOWER HALFSPACE IS VACUUM   
C     -------------------------   
      NRI(IN)=1
      GO TO 500             
 125  IF (V(IN1,3).GT.1E-10) GO TO 150                
C     NEXT LAYER OR HALFSPACE IS LIQUID               
C     ---------------------------------               
      NRI(IN)=2
      GO TO 500             
C 
C     NEXT LAYER OR HALFSPACE IS SOLID                
C     --------------------------------                
 150  CONTINUE              
      NRI(IN)=3
      GO TO 500             
 200  CONTINUE              
C     LAYER OR HALFSPACE OVER INTERFACE IS SOLID      
C     *******************************************     
C     LOWER HALFSPACE VACUUM
C     ----------------------
      IF (V(IN1,2).GT.1E-10) GO TO 225                
      NRI(IN)=2
      GO TO 500             
 225  IF (V(IN1,3).GT.1E-10) GO TO 250                
C     NEXT LAYER OR HALFSPACE IS LIQUID               
C     ---------------------------------               
      NRI(IN)=3
      GO TO 500             
 250  CONTINUE              
C     NEXT LAYER OR HALFSPACE IS SOLID                
C     --------------------------------                
      NRI(IN)=4
 500  IRST(IN)=ICNT
      ICNT=ICNT+NRI(IN)
 510  CONTINUE              
      NRI(NUML)=0
Cms
Cms  Determine Number of columns associated with each layer
Cms
      ICNT=1
      DO 10 IL=1,NUML
Cms  Add code to include porous sediment layer
      IF(LAYTYP(IL).EQ.5) THEN
        NCL(IL)=3
        GO TO 8
      ENDIF
Cms  **************
Cms  ** No changes made beyond this point in this subroutine
Cms  **************
      NCL(IL)=0
      IF (V(IL,2).LT.1E-10) GO TO 10
      IF (V(IL,3).GE.1E-10) GO TO 5
      NCL(IL)=1
      GO TO 8
 5    NCL(IL)=2
 8    IF (IL.NE.1.AND.IL.NE.NUML) NCL(IL)=2*NCL(IL)
      ICST(IL)=ICNT
      ICNT=ICNT+NCL(IL)
 10   CONTINUE
C
C     DETERMINE FIRST POINTER FOR EACH LAYER
C
      ISTART(1)=1
      ISTART(2)=ISTART(1)+NRI(1)*NCL(1)
      IF (NUML.LE.2) GO TO 20
      DO 19 IL=3,NUML
 19   ISTART(IL)=ISTART(IL-1)+(NRI(IL-2)+NRI(IL-1))*NCL(IL-1)
 20   CONTINUE
C
C     DETERMINE POINTERS
C
      IF (NCL(1).EQ.0) GO TO 30
      DO 25 J=1,NCL(1)
      DO 22 L=1,NRI(1)
      IRN(ISTART(1)+(J-1)*NRI(1)+L-1)=IRST(1)+L-1
 22   CONTINUE
      ICP(ICST(1)+J-1)=ISTART(1)+(J-1)*NRI(1)
      IDP(ICST(1)+J-1)=ISTART(1)+(J-1)*(NRI(1)+1)
 25   CONTINUE
 30   DO 40 IL=2,NUML
      INA=IL-1
      INB=IL
      IF (NCL(IL).EQ.0) GO TO 40
      DO 35 J=1,NCL(IL)
      NR=NRI(INA)+NRI(INB)
      DO 32 L=1,NR
      IRN(ISTART(IL)+(J-1)*NR+L-1)=IRST(INA)+L-1
 32   CONTINUE
      ICP(ICST(IL)+J-1)=ISTART(IL)+(J-1)*NR
      IDP(ICST(IL)+J-1)=ISTART(IL)+(ICST(IL)-IRST(INA))+(J-1)*(NR+1)
 35   CONTINUE
 40   CONTINUE
C
C     DETERMINE NUMBER OF SPARSE ELEMENTS
C
      NNA=ISTART(NUML)+NRI(NUMI)*NCL(NUML)-1
      ICP(NEQ+1)=NNA+1
      DO 710 I=1,NEQ
      DO 705 J=ICP(I),ICP(I+1)-1
      IIRR=IRN(J)
      IICC=I-IIRR+IBW+1
      IICC=2*(IIRR+(IICC-1)*NEQ)-1
      INDB(J)=IICC
 705  CONTINUE
 710  CONTINUE
      IRHCOL=3*IBW+2
      NNB=(IRHCOL-1)*NEQ
      CALL DETIND
      RETURN                
      END                   
      SUBROUTINE DETIND              
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
Cms
Cms ** modified to include porous sediment layers - 7/7/92
Cms
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
Cms
Cms ## DATA statement added to permit easy reording of porous sediment
Cms    degrees of freedom
C
        INTEGER IS(6)
        DATA IS/1,2,5,3,4,6/
Cms
      DO 10 IN=1,NUMI
      IN1=IN+1      
Cms
Cms  treat cases involving porous sediment upper layer first
Cms  *******************************************************
        IF(LAYTYP(IN).EQ.5) THEN
Cms ** next layer is vacuum
Cms    --------------------
          IF(LAYTYP(IN1).LE.0) THEN
            I = IPS(IN,3)
            J = IPS(IN,4)
            K = IPS(IN,6)
            INDR(I) = INDX2(IN,3)
            INDR(J) = INDX2(IN,4)
            INDR(K) = INDX2(IN,6)
            IF(IN.EQ.1) THEN
              J1 = 4
              IST = ISTART(1)
              INC = NRI(1)
            ELSE
              J1 = 1
              IST = ISTART(IN) + NRI(IN-1)
              INC = NRI(IN-1) + NRI(IN)
            ENDIF
            DO 310 J = J1,6
              INDA(IST)   = INDXLO(IN,3,IS(J))
              INDA(IST+1) = INDXLO(IN,4,IS(J))
              INDA(IST+2) = INDXLO(IN,6,IS(J))
              IST = IST + INC
 310      CONTINUE
            GO TO 10
Cms ** next layer is liquid
Cms    --------------------
C
Cms  Corrected error in next line replacing IN with IN1 - 4/10/96
Cms
          ELSEIF(LAYTYP(IN1).EQ.1.OR.LAYTYP(IN1).EQ.2) THEN
            I = IPS(IN,3)
            J = IPS(IN,4)
            K = IPS(IN,6)
            L = IPS(IN1,1)
            INDR(I) = INDX2(IN,1)
            INDR(J) = INDX2(IN,3)
            INDR(K) = INDX2(IN,4)
            INDR(L) = INDX2(IN,6)
            IF(IN.EQ.1) THEN
              J1 = 4
              IST = ISTART(1)
              INC = NRI(1)
            ELSE
              J1 = 1
              IST = ISTART(IN) + NRI(IN-1)
              INC = NRI(IN-1) + NRI(IN)
            ENDIF
            DO 320 J = J1,6
              INDA(IST)   = INDXLO(IN,1,IS(J))
              INDA(IST+1) = INDXLO(IN,3,IS(J))
              INDA(IST+2) = INDXLO(IN,4,IS(J))
              INDA(IST+3) = INDXLO(IN,6,IS(J))
              IST = IST + INC
 320      CONTINUE
          IST = ISTART(IN1)
          IF(IN.EQ.NUMI) THEN
            J2 = 1
            INC = NRI(IN)
          ELSE
            J2 = 3
            INC = NRI(IN) + NRI(IN1)
          ENDIF
          DO 325 J = 1,J2,2
            INDA(IST)   = INDXUP(IN1,1,J)
            INDA(IST+1) = INDXUP(IN1,3,J)
            INDA(IST+2) = INDXUP(IN1,4,J)
            INDA(IST+3) = INDXUP(IN1,6,J)
            IST = IST + INC
 325      CONTINUE
          GO TO 10
Cms ** next layer is solid
Cms    -------------------
        ELSEIF(LAYTYP(IN1).EQ.3.OR.LAYTYP(IN1).EQ.4) THEN
            I = IPS(IN,3)
            J = IPS(IN,4)
            K = IPS(IN,6)
            L = IPS(IN1,1)
            M = IPS(IN1,2)
            INDR(I) = INDX2(IN,1)
            INDR(J) = INDX2(IN,2)
            INDR(K) = INDX2(IN,3)
            INDR(L) = INDX2(IN,4)
            INDR(M) = INDX2(IN,5)
            IF(IN.EQ.1) THEN
              J1 = 4
              IST = ISTART(1)
              INC = NRI(1)
            ELSE
              J1 = 1
              IST = ISTART(IN) + NRI(IN-1)
              INC = NRI(IN-1) + NRI(IN)
            ENDIF
            DO 330 J = J1,6
              INDA(IST)   = INDXLO(IN,1,IS(J))
              INDA(IST+1) = INDXLO(IN,2,IS(J))
              INDA(IST+2) = INDXLO(IN,3,IS(J))
              INDA(IST+3) = INDXLO(IN,4,IS(J))
              INDA(IST+4) = INDXLO(IN,5,IS(J))
              IST = IST + INC
 330      CONTINUE
          IST = ISTART(IN1)
          IF(IN.EQ.NUMI) THEN
            J2 = 2
            INC = NRI(IN)
          ELSE
            J2 = 4
            INC = NRI(IN) + NRI(IN1)
          ENDIF
          DO 335 J = 1,J2
            INDA(IST)   = INDXUP(IN1,1,J)
            INDA(IST+1) = INDXUP(IN1,2,J)
            INDA(IST+2) = INDXUP(IN1,3,J)
            INDA(IST+3) = INDXUP(IN1,4,J)
            INDA(IST+4) = INDXUP(IN1,5,J)
            IST = IST + INC
 335      CONTINUE
          GO TO 10
        ELSEIF(LAYTYP(IN1).EQ.5) THEN
Cms ** next layer is porous sediment
            I = IPS(IN,3)
            J = IPS(IN,4)
            K = IPS(IN,6)
            L = IPS(IN1,1)
            M = IPS(IN1,2)
            N = IPS(IN1,5)
            INDR(I) = INDX2(IN,1)
            INDR(J) = INDX2(IN,2)
            INDR(K) = INDX2(IN,3)
            INDR(L) = INDX2(IN,4)
            INDR(M) = INDX2(IN,5)
            INDR(N) = INDX2(IN,6)
            IF(IN.EQ.1) THEN
              J1 = 4
              IST = ISTART(1)
              INC = NRI(1)
            ELSE
              J1 = 1
              IST = ISTART(IN) + NRI(IN-1)
              INC = NRI(IN-1) + NRI(IN)
            ENDIF
            DO 340 J = J1,6
              INDA(IST)   = INDXLO(IN,1,IS(J))
              INDA(IST+1) = INDXLO(IN,2,IS(J))
              INDA(IST+2) = INDXLO(IN,3,IS(J))
              INDA(IST+3) = INDXLO(IN,4,IS(J))
              INDA(IST+4) = INDXLO(IN,5,IS(J))
              INDA(IST+5) = INDXLO(IN,6,IS(J))
              IST = IST + INC
 340      CONTINUE
          IST = ISTART(IN1)
          IF(IN.EQ.NUMI) THEN
            J2 = 3
            INC = NRI(IN)
          ELSE
            J2 = 6
            INC = NRI(IN) + NRI(IN1)
          ENDIF
          DO 345 J = 1,J2
            INDA(IST)   = INDXUP(IN1,1,IS(J))
            INDA(IST+1) = INDXUP(IN1,2,IS(J))
            INDA(IST+2) = INDXUP(IN1,3,IS(J))
            INDA(IST+3) = INDXUP(IN1,4,IS(J))
            INDA(IST+4) = INDXUP(IN1,5,IS(J))
            INDA(IST+5) = INDXUP(IN1,6,IS(J))
            IST = IST + INC
 345      CONTINUE
          GO TO 10
        ENDIF
      ENDIF
Cms
Cms ** upper layer is not porous sediment **
Cms
      IF (V(IN,2).GT.1E-10) GO TO 100   
C                   
C     VACUUM UPPER HALFSPACE            
C     **********************            
Cms   next layer is porous sediment
Cms   -----------------------------
        IF(LAYTYP(IN1).EQ.5) THEN
          I = IPS(IN1,1)
          J = IPS(IN1,2)
          K = IPS(IN1,5)
          INDR(I) = INDX2(IN,3)
          INDR(J) = INDX2(IN,4)
          INDR(K) = INDX2(IN,6)
          IST = ISTART(IN1)
          IF(IN.GE.NUMI) THEN
            INC = NRI(IN)
            J2 = 3
          ELSE
            INC = NRI(IN) + NRI(IN1)
            J2 = 6
          ENDIF
          DO 90 J=1,J2
            INDA(IST)   = INDXUP(IN1,3,IS(J))
            INDA(IST+1) = INDXUP(IN1,4,IS(J))
            INDA(IST+2) = INDXUP(IN1,6,IS(J))
            IST = IST + INC
  90      CONTINUE
          GO TO 10
        ENDIF
      IF (V(IN1,3).GT.1E-10) GO TO 50   
C     NEXT LAYER OR HALFSPACE IS LIQUID 
C     --------------------------------- 
      I=IPS(IN1,1)  
      J=IPS(IN1,3)  
      INDR(I)=INDX2(IN,3)
      IST=ISTART(IN1)
      INDA(IST)=INDXUP(IN1,3,1)
      IF (IN.GE.NUMI) GO TO 10
      IST=IST+NRI(IN)+NRI(IN1)
      INDA(IST)=INDXUP(IN1,3,3)
      GO TO 10        
 50   CONTINUE      
C     NEXT LAYER OR HALFSPACE IS SOLID  
C     --------------------------------  
      I=IPS(IN1,1)  
      J=IPS(IN1,2)  
      INDR(I)=INDX2(IN,3)
      INDR(J)=INDX2(IN,4)
      IST=ISTART(IN1)
      IF (IN.GE.NUMI) THEN
      INC=NRI(IN)
      J2=2
      ELSE
      INC=NRI(IN)+NRI(IN1)
      J2=4
      END IF
      DO 60 J=1,J2
      DO 55 I=0,1
 55   INDA(IST+I)=INDXUP(IN1,3+I,J)
 60   IST=IST+INC
      GO TO 10        
 100  IF (V(IN,3).GT.1E-10) GO TO 200   
C                   
C     LAYER OR HALFSPACE OVER INTERFACE IS LIQUID           
C     *******************************************           
Cms   next layer is porous sediment
Cms   -----------------------------
        IF(LAYTYP(IN1).EQ.5) THEN
          I = IPS(IN,3)
          J = IPS(IN1,1)
          K = IPS(IN1,2)
          L = IPS(IN1,5)
          INDR(I) = INDX2(IN,1)
          INDR(J) = INDX2(IN,3)
          INDR(K) = INDX2(IN,4)
          INDR(L) = INDX2(IN,6)
          IF(IN.EQ.1) THEN
          J1 = 3
          IST = ISTART(1)
          INC = NRI(1)
        ELSE
          J1 = 1
          IST = ISTART(IN) + NRI(IN-1)
          INC = NRI(IN-1) + NRI(IN)
        END IF
        DO 190 J=J1,3,2
         INDA(IST)   = INDXLO(IN,1,J)
         INDA(IST+1) = INDXLO(IN,3,J)
         INDA(IST+2) = INDXLO(IN,4,J)
         INDA(IST+3) = INDXLO(IN,6,J)
         IST=IST+INC
 190    CONTINUE
        IST = ISTART(IN1)
        IF(IN.GE.NUMI) THEN
          INC = NRI(IN)
          J2 = 3
        ELSE
          INC = NRI(IN) + NRI(IN1)
          J2 = 6
        ENDIF
        DO 195 J=1,J2
          INDA(IST)   = INDXUP(IN1,1,IS(J))
          INDA(IST+1) = INDXUP(IN1,3,IS(J))
          INDA(IST+2) = INDXUP(IN1,4,IS(J))
          INDA(IST+3) = INDXUP(IN1,6,IS(J))
          IST = IST + INC
 195    CONTINUE
        GO TO 10
      ENDIF
      IF (V(IN1,2).GT.1E-10) GO TO 125  
C     LOWER HALFSPACE IS VACUUM         
C     -------------------------         
      I=IPS(IN,3)   
      INDR(I)=INDX2(IN,3)
      IF (IN.EQ.1) THEN
      J1=3
      IST=ISTART(1)
      INC=NRI(1)
      ELSE
      J1=1
      IST=ISTART(IN)+NRI(IN-1)
      INC=NRI(IN-1)+NRI(IN)
      END IF
      DO 110 J=J1,3,2
      INDA(IST)=INDXLO(IN,3,J)
 110  IST=IST+INC
      GO TO 10        
 125  IF (V(IN1,3).GT.1E-10) GO TO 150  
C     NEXT LAYER OR HALFSPACE IS LIQUID 
C     --------------------------------- 
      I=IPS(IN,3)   
      J=IPS(IN1,1)  
      INDR(I)=INDX2(IN,1)
      INDR(J)=INDX2(IN,3)
      IF (IN.EQ.1) THEN
      J1=3
      IST=ISTART(1)
      INC=NRI(1)
      ELSE
      J1=1
      IST=ISTART(IN)+NRI(IN-1)
      INC=NRI(IN-1)+NRI(IN)
      END IF
      DO 130 J=J1,3,2
      INDA(IST)=INDXLO(IN,1,J)
      INDA(IST+1)=INDXLO(IN,3,J)
 130  IST=IST+INC
      IF (IN.GE.NUMI) THEN
      J2=1
      IST=ISTART(IN1)
      INC=NRI(IN)
      ELSE
      J2=3
      IST=ISTART(IN1)
      INC=NRI(IN)+NRI(IN1)
      END IF
      DO 140 J=1,J2,2
      INDA(IST)=INDXUP(IN1,1,J)
      INDA(IST+1)=INDXUP(IN1,3,J)
 140  IST=IST+INC
      GO TO 10        
C                   
C     NEXT LAYER OR HALFSPACE IS SOLID  
C     --------------------------------  
 150  CONTINUE      
      I=IPS(IN,3)   
      J=IPS(IN1,1)  
      K=IPS(IN1,2)  
      INDR(I)=INDX2(IN,3)
      INDR(J)=INDX2(IN,1)
      INDR(K)=INDX2(IN,4)
      IF (IN.EQ.1) THEN
      J1=3
      IST=ISTART(1)
      INC=NRI(1)
      ELSE
      J1=1
      IST=ISTART(IN)+NRI(IN-1)
      INC=NRI(IN-1)+NRI(IN)
      END IF
      DO 170 J=J1,3,2
      INDA(IST)=INDXLO(IN,3,J)
      INDA(IST+1)=INDXLO(IN,1,J)
      INDA(IST+2)=INDXLO(IN,4,J)
 170  IST=IST+INC
      IF (IN.GE.NUMI) THEN
      J2=2
      IST=ISTART(IN1)
      INC=NRI(IN)
      ELSE
      J2=4
      IST=ISTART(IN1)
      INC=NRI(IN)+NRI(IN1)
      END IF
      DO 180 J=1,J2
      INDA(IST)=INDXUP(IN1,3,J)
      INDA(IST+1)=INDXUP(IN1,1,J)
      INDA(IST+2)=INDXUP(IN1,4,J)
 180  IST=IST+INC
      GO TO 10        
 200  CONTINUE      
C     LAYER OR HALFSPACE OVER INTERFACE IS SOLID            
C     *******************************************           
Cms   next layer is porous sediment
Cms   -----------------------------
        IF(LAYTYP(IN1).EQ.5) THEN
          I = IPS(IN,3)
          J = IPS(IN,4)
          K = IPS(IN1,1)
          L = IPS(IN1,2)
          M = IPS(IN1,5)
          INDR(I) = INDX2(IN,1)
          INDR(J) = INDX2(IN,2)
          INDR(K) = INDX2(IN,3)
          INDR(L) = INDX2(IN,4)
          INDR(M) = INDX2(IN,5)
          IF(IN.EQ.1) THEN
          J1 = 3
          IST = ISTART(1)
          INC = NRI(1)
        ELSE
          J1 = 1
          IST = ISTART(IN) + NRI(IN-1)
          INC = NRI(IN-1) + NRI(IN)
        END IF
        DO 290 J=J1,4
         INDA(IST)   = INDXLO(IN,1,J)
         INDA(IST+1) = INDXLO(IN,2,J)
         INDA(IST+2) = INDXLO(IN,3,J)
         INDA(IST+3) = INDXLO(IN,4,J)
         INDA(IST+4) = INDXLO(IN,5,J)
         IST=IST+INC
 290    CONTINUE
          IST = ISTART(IN1)
          IF(IN.GE.NUMI) THEN
            INC = NRI(IN)
            J2 = 3
          ELSE
            INC = NRI(IN) + NRI(IN1)
            J2 = 6
          ENDIF
          DO 295 J=1,J2
            INDA(IST)   = INDXUP(IN1,1,IS(J))
            INDA(IST+1) = INDXUP(IN1,2,IS(J))
            INDA(IST+2) = INDXUP(IN1,3,IS(J))
            INDA(IST+3) = INDXUP(IN1,4,IS(J))
            INDA(IST+4) = INDXUP(IN1,5,IS(J))
            IST = IST + INC
 295    CONTINUE
          GO TO 10
        ENDIF
C     LOWER HALFSPACE VACUUM            
C     ----------------------            
      IF (V(IN1,2).GT.1E-10) GO TO 225  
      I=IPS(IN,3)   
      J=IPS(IN,4)   
      INDR(I)=INDX2(IN,3)
      INDR(J)=INDX2(IN,4)
      IF (IN.LE.1) THEN
      J1=3
      IST=ISTART(1)
      INC=NRI(1)
      ELSE
      J1=1
      IST=ISTART(IN)+NRI(IN-1)
      INC=NRI(IN-1)+NRI(IN)
      END IF
      DO 220 J=J1,4
      INDA(IST)=INDXLO(IN,3,J)
      INDA(IST+1)=INDXLO(IN,4,J)
 220  IST=IST+INC
      GO TO 10        
 225  IF (V(IN1,3).GT.1E-10) GO TO 250  
C     NEXT LAYER OR HALFSPACE IS LIQUID 
C     --------------------------------- 
      I=IPS(IN,3)   
      J=IPS(IN,4)   
      K=IPS(IN1,1)  
      INDR(I)=INDX2(IN,1)
      INDR(J)=INDX2(IN,3)
      INDR(K)=INDX2(IN,4)
      IF (IN.LE.1) THEN
      J1=3
      IST=ISTART(1)
      INC=NRI(1)
      ELSE
      J1=1
      IST=ISTART(IN)+NRI(IN-1)
      INC=NRI(IN-1)+NRI(IN)
      END IF
      DO 230 J=J1,4
      INDA(IST)=INDXLO(IN,1,J)
      INDA(IST+1)=INDXLO(IN,3,J)
      INDA(IST+2)=INDXLO(IN,4,J)
 230  IST=IST+INC
      IF (IN.GE.NUMI) THEN
      J2=1
      IST=ISTART(IN1)
      INC=NRI(IN)
      ELSE
      J2=3
      IST=ISTART(IN1)
      INC=NRI(IN)+NRI(IN1)
      END IF
      DO 240 J=1,J2,2
      INDA(IST)=INDXUP(IN1,1,J)
      INDA(IST+1)=INDXUP(IN1,3,J)
      INDA(IST+2)=INDXUP(IN1,4,J)
 240  IST=IST+INC
      GO TO 10        
 250  CONTINUE      
C     NEXT LAYER OR HALFSPACE IS SOLID  
C     --------------------------------  
      I=IPS(IN,3)   
      J=IPS(IN,4)   
      K=IPS(IN1,1)  
      L=IPS(IN1,2)  
      INDR(I)=INDX2(IN,1)
      INDR(J)=INDX2(IN,2)
      INDR(K)=INDX2(IN,3)
      INDR(L)=INDX2(IN,4)
      IF (IN.LE.1) THEN
      J1=3
      IST=ISTART(1)
      INC=NRI(1)
      ELSE
      J1=1
      IST=ISTART(IN)+NRI(IN-1)
      INC=NRI(IN-1)+NRI(IN)
      END IF
      DO 260 J=J1,4
      DO 255 I=0,3
 255  INDA(IST+I)=INDXLO(IN,I+1,J)
 260  IST=IST+INC
      IST=ISTART(IN1)
      IF (IN.GE.NUMI) THEN
      J2=2
      INC=NRI(IN)
      ELSE
      J2=4
      INC=NRI(IN)+NRI(IN1)
      END IF
      DO 280 J=1,J2
      DO 275 I=0,3
 275  INDA(IST+I)=INDXUP(IN1,I+1,J)
 280  IST=IST+INC
 10   CONTINUE
Cms ** Changed following index from 4 to NLEQ
      DO 20 I=1,NUML
      DO 20 J=1,NLEQ
 20   IF (IPS(I,J).GT.0) INDS(IPS(I,J))=INDX2(I,J)
      RETURN        
      END           
C                   
      INTEGER FUNCTION INDX2(I,J)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
Cms
Cms  this function calculates the one-dimensional vector index which
Cms  corresponds to the two-dimensional indices in  R, SS, ROIN, RUIN
Cms  and IPS.
Cms
      INCLUDE 'compar.f'
      II=I+(J-1)*NLA
      INDX2=2*II-1
      RETURN
      END
      INTEGER FUNCTION INDXLO(I,J,K)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
Cms ** modified to allow NLEQ degrees of freedom per layer - 7/6/92
Cms
Cms  this function calculates the one-dimensional vector index which
Cms  corresponds to the three-dimensional indices in  ALO
Cms
      INCLUDE 'compar.f'
      II=I+(J-1)*NLA+(K-1)*NLA*NLEQ
      INDXLO=2*II-1
      RETURN
      END
      INTEGER FUNCTION INDXUP(I,J,K)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
Cms ** modified to allow NLEQ degrees of freedom per layer - 7/6/92
Cms
Cms  this function calculates the one-dimensional vector index which
Cms  corresponds to the three-dimensional indices in  AUP assuming AUP
Cms  is appended to ALO.
Cms
      INCLUDE 'compar.f'
      II=I+(J-1)*NLA+(K-1)*NLA*NLEQ+NLA*NLEQ*NLEQ
      INDXUP=2*II-1
      RETURN
      END





      SUBROUTINE HERMIT(I,FF,NP,LS,ICUT1,ICUT2,DLWVNO,PI,WK0,
     *IPLOT1,IPLOT2)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      DIMENSION FF(2,1)
      IPLOT1=ICUT1                
      NCUT=min(100,NINT((ICUT2-ICUT1+1)*.1))
      IF(ICUT1.LE.2)   GO TO 4000       
      IPL1=MAX0(2,ICUT1-NCUT)           
      IPL2=ICUT1-1  
C                   
C   A AND B ARE THE LOWEST AND THE HIGHEST WAVE NUMBERS OF INTEREST             
      A=(WK0+(IPL1-1)*DLWVNO)     
      B=(WK0+(ICUT1-1)*DLWVNO)    
      FA=0.0        
      FB=FF(I,ICUT1)
      SIGNFB=SIGN(1.0,FB)               
      F1A=0.0       
      F1B=(FF(I,ICUT1+1)-FF(I,ICUT1))/DLWVNO         
C   F1B IS NOT ALLOWED TO BE NEGATIVE WHEN FB IS POSITIVE, AND VICEVERSA        
      IF(SIGNFB.GT.0.0)   F1B=MAX(F1B,0.)                 
      IF(SIGNFB.LT.0.0)   F1B=MIN(F1B,0.)                 
C                   
C   THE FOLLOWING STATEMENT CHECKS THE CONDITION THAT THE HERMITE               
C   FUNCTION DOES NOT CHANGE SIGN IN THE INTERVAL A-B       
      IF(((3.0*FB*SIGNFB)/(B-A)).GE.F1B*SIGNFB)   GO TO 1000
C   WE INCREASE A TO SATISFY THE ABOVE CONDITION            
      A=B-3*FB/F1B  
      IPL1=1.5+(A-WK0)/DLWVNO    
 1000 CONTINUE      
C *** changed 89/10/3 HS
      C0=0
      C1=0
      C2=FB
      C3=(F1B*(B-A)-2E0*FB)                 
C **********************
      DO 3000   II=IPL1,IPL2            
      X = (WK0 + FLOAT(II-1) * DLWVNO)               
C   IN THIS CASE P1 IS ALWAYS ZERO (FA=0,F1A=0)             
C     P1=((X-B)/(B-A))**2*((1.0+2.0*(X-A)/(B-A))*FA+(X-A)*F1A)             
C      P2=((X-A)/(B-A))**2*((1.0-2.0*(X-B)/(B-A))*FB+(X-B)*F1B)            
C     FF(I,II)=P1+P2
      XB=(X-B)/(B-A)
      XA=(X-A)/(B-A)
      P2=XA*XA*(C2+C3*XB)
      FF(I,II)=P2   
 3000 CONTINUE      
       IPLOT1=IPL1  
C                   
C                   
 4000 CONTINUE      
      IPLOT2=ICUT2  
      IF(ICUT2.EQ.LS)   RETURN          
      IPL1=ICUT2+1  
      IPL2=MIN0(LS,ICUT2+NCUT)          
C                   
C   A AND B ARE THE LOWEST AND THE HIGHEST WAVE NUMBERS OF INTEREST             
      A = (WK0 + FLOAT(ICUT2-1) * DLWVNO) 
      B = (WK0+(IPL2-1)*DLWVNO)     
      FA=FF(I,ICUT2)
      SIGNFA=SIGN(1.0,FA)               
      FB=0.0        
      F1A=(FF(I,ICUT2)-FF(I,ICUT2-1))/DLWVNO         
C   F1A IS NOT ALLOWED TO BE POSITIVE   
      IF(SIGNFA.GT.0.0)F1A=MIN(F1A,0.)
      IF(SIGNFA.LT.0.0)F1A=MAX(F1A,0.)
      F1B=0.0       
C                   
C   THE FOLLOWING STATEMENT CHECKS THE CONDITION THAT THE HERMITE               
C   FUNCTION DOES NOT BECOME NEGATIVE IN THE INTERVAL A-B   
      IF(((3.0*FA*SIGNFA)/(B-A)).GE.-F1A*SIGNFA)   GO TO 5000                   
C   WE DECREASE B TO SATISFY THE ABOVE CONDITION            
      B=(-3.0*FA)/F1A+A                 
      IPL2=1+INT((B-WK0)/DLWVNO)
 5000 CONTINUE      
C                   
C *** changed 89/10/3 HS
      C0=FA
      C1=F1A*(B-A)
      C2=(-F1A*(B-A)-FA)
      C3=(F1A*(B-A)+2E0*FA)                 
C **********************
      DO 7000   II=IPL1,IPL2            
      X = (WK0 + FLOAT(II-1) * DLWVNO)
C      P1=((X-B)/(B-A))**2*((1.0+2.0*(X-A)/(B-A))*FA+(X-A)*F1A)    
C   IN THIS CASE P2 IS ALWAYS ZERO (FB=0,F1B=0)             
C     P2=((X-A)/(B-A))**2*((1.0-2.0*(X-B)/(B-A))*FB+(X-B)*F1B) 
C     FF(I,II)=P1+P2
      XB=(X-B)/(B-A)
      XA=(X-A)/(B-A)
      P1=C0+XA*(C1+XA*(C2+C3*XB))
      FF(I,II)=P1   
 7000 CONTINUE      
      IPLOT2=IPL2   
C     WRITE(6,100) ICUT1,ICUT2,IPLOT1,IPLOT2                
C     WRITE(6,300)  
      RETURN        
      END           
      SUBROUTINE CHERMIT(CFF,LS,ICUT1,ICUT2,DLWVNO,WK0,   
     *IPLOT1,IPLOT2)
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
C
C *** COMPLEX HERMITE EXTRAPOLATOR
C     HS 89/10/3
      COMPLEX CFF(LS)
      IPLOT1=ICUT1                
      NCUT=min(500,NINT(LS*.1))
      IF(ICUT1.LE.2)   GO TO 4000       
      IPL1=MAX0(2,ICUT1-NCUT)           
      IPL2=ICUT1-1  
C                   
C   A AND B ARE THE LOWEST AND THE HIGHEST WAVE NUMBERS OF INTEREST             
      A=(WK0+(IPL1-1)*DLWVNO)     
      B=(WK0+(ICUT1-1)*DLWVNO)    
      FA=0.0        
      FB=ABS(CFF(ICUT1))
      F1A=0.0       
      F1B=(ABS(CFF(ICUT1+1))-ABS(CFF(ICUT1)))/DLWVNO         
C *** PHASES
      IF (FB.GT.1E-35) THEN
      RR=REAL(CFF(ICUT1))
      RI=RIMAG(CFF(ICUT1))
      PB=ATAN2Z(RI,RR)
      RR=REAL(CFF(ICUT1+1))
      RI=RIMAG(CFF(ICUT1+1))
      PB2=ATAN2Z(RI,RR)
      DP=PB2-PB
c *** f1b must be positive
      F1B=MAX(0E0,F1B)
C                   
C   THE FOLLOWING STATEMENT CHECKS THE CONDITION THAT THE HERMITE               
C   FUNCTION DOES NOT CHANGE SIGN IN THE INTERVAL A-B       
      IF(((3.0*FB)/(B-A)).GE.F1B)   GO TO 1000
C   WE INCREASE A TO SATISFY THE ABOVE CONDITION            
      A=B-3*FB/F1B  
      IPL1=1.5+(A-WK0)/DLWVNO    
 1000 CONTINUE      
C *** changed 89/10/3 HS
      C0=0
      C1=0
      C2=FB
      C3=(F1B*(B-A)-2E0*FB)                 
C **********************
      DO 3000   II=IPL1,IPL2            
      X = (WK0 + FLOAT(II-1) * DLWVNO)               
C   IN THIS CASE P1 IS ALWAYS ZERO (FA=0,F1A=0)             
C     P1=((X-B)/(B-A))**2*((1.0+2.0*(X-A)/(B-A))*FA+(X-A)*F1A)             
C      P2=((X-A)/(B-A))**2*((1.0-2.0*(X-B)/(B-A))*FB+(X-B)*F1B)            
C     FF(I,II)=P1+P2
      XB=(X-B)/(B-A)
      XA=(X-A)/(B-A)
      P2=XA*XA*(C2+C3*XB)
      CFF(II)=P2*CMPLX(COS(PB+(II-ICUT1)*DP),SIN(PB+(II-ICUT1)*DP))   
 3000 CONTINUE      
       IPLOT1=IPL1  
      END IF
C                   
C                   
 4000 CONTINUE      
      IPLOT2=ICUT2  
      IF(ICUT2.EQ.LS)   RETURN          
      IPL1=ICUT2+1  
      IPL2=MIN0(LS,ICUT2+NCUT)          
C                   
C   A AND B ARE THE LOWEST AND THE HIGHEST WAVE NUMBERS OF INTEREST             
      A = (WK0 + FLOAT(ICUT2-1) * DLWVNO) 
      B = (WK0+(IPL2-1)*DLWVNO)     
      FA=abs(CFF(ICUT2))
      F1A=(ABS(CFF(ICUT2))-ABS(CFF(ICUT2-1)))/DLWVNO         
      FB=0.0        
      F1B=0
C *** PHASES
      IF (FA.GT.1E-35) THEN
      RR=REAL(CFF(ICUT2))
      RI=RIMAG(CFF(ICUT2))
      PA=ATAN2Z(RI,RR)
      RR=REAL(CFF(ICUT2-1))
      RI=RIMAG(CFF(ICUT2-1))
      PA2=ATAN2Z(RI,RR)
      DP=PA-PA2
C *** F1A MUST BE NEGATIVE
      F1A=MIN(F1A,0E0)
C                   
C   THE FOLLOWING STATEMENT CHECKS THE CONDITION THAT THE HERMITE               
C   FUNCTION DOES NOT BECOME NEGATIVE IN THE INTERVAL A-B   
      IF(((3.0*FA)/(B-A)).GE.-F1A)   GO TO 5000                   
C   WE DECREASE B TO SATISFY THE ABOVE CONDITION            
      B=(-3.0*FA)/F1A+A                 
      IPL2=1+INT((B-WK0)/DLWVNO)
 5000 CONTINUE      
C                   
C *** changed 89/10/3 HS
      C0=FA
      C1=F1A*(B-A)
      C2=(-F1A*(B-A)-FA)
      C3=(F1A*(B-A)+2E0*FA)                 
C **********************
      DO 7000   II=IPL1,IPL2            
      X = (WK0 + FLOAT(II-1) * DLWVNO)
C      P1=((X-B)/(B-A))**2*((1.0+2.0*(X-A)/(B-A))*FA+(X-A)*F1A)    
C   IN THIS CASE P2 IS ALWAYS ZERO (FB=0,F1B=0)             
C     P2=((X-A)/(B-A))**2*((1.0-2.0*(X-B)/(B-A))*FB+(X-B)*F1B) 
C     FF(I,II)=P1+P2
      XB=(X-B)/(B-A)
      XA=(X-A)/(B-A)
      P1=C0+XA*(C1+XA*(C2+C3*XB))
      CFF(II)=P1*CMPLX(COS(PA+(II-ICUT2)*DP),SIN(PA+(II-ICUT2)*DP))      
 7000 CONTINUE      
      IPLOT2=IPL2   
      END IF
C     WRITE(6,100) ICUT1,ICUT2,IPLOT1,IPLOT2                
C     WRITE(6,300)  
      RETURN        
      END           

      SUBROUTINE DETBW                  
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
Cms
Cms  ** modified to account for porous sediment layers - 7/3/92
Cms
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      IBW=0         
      DO 500 IN=1,NUMI                  
      IN1=IN+1      
Cms
Cms  Calculate "bandwidth" at porous sediment layer interfaces
Cms
        IF(LAYTYP(IN).EQ.5) THEN
Cms ** Layer above interface is a porous sediment
          IF(IN.EQ.1) THEN
Cms ** it is the first layer
            IF(NUMI.EQ.1) THEN
Cms ** and there is only one interface
              IF(LAYTYP(IN1).LE.0) IBW = MAX0(IBW,2)
              IF(LAYTYP(IN1).EQ.1.OR.LAYTYP(IN1).EQ.2) IBW = MAX0(IBW,3)
              IF(LAYTYP(IN1).EQ.3.OR.LAYTYP(IN1).EQ.4) IBW = MAX0(IBW,4)
              IF(LAYTYP(IN1).EQ.5) IBW = MAX0(IBW,5)
              GO TO 500
            ELSE
              IF(LAYTYP(IN1).LE.0) IBW = MAX0(IBW,2)
              IF(LAYTYP(IN1).EQ.1.OR.LAYTYP(IN1).EQ.2) IBW = MAX0(IBW,4)
              IF(LAYTYP(IN1).EQ.3.OR.LAYTYP(IN1).EQ.4) IBW = MAX0(IBW,6)
              IF(LAYTYP(IN1).EQ.5) IBW = MAX0(IBW,8)
              GO TO 500
            ENDIF
          ELSE
            IF(LAYTYP(IN1).LE.0) IBW = MAX0(IBW,5)
            IF(LAYTYP(IN1).EQ.1.OR.LAYTYP(IN1).EQ.2) IBW = MAX0(IBW,6)
            IF(LAYTYP(IN1).EQ.3.OR.LAYTYP(IN1).EQ.4) IBW = MAX0(IBW,7)
            IF(LAYTYP(IN1).EQ.5) IBW = MAX0(IBW,8)
            GO TO 500
          ENDIF
        ELSEIF(LAYTYP(IN1).EQ.5) THEN
Cms ** Layer below interface is a porous sediment (one above is not)
          IF(IN1.EQ.NUML) THEN
Cms ** and it is the last layer
            IF(NUMI.EQ.1) THEN
Cms ** and there is only one interface
              IF(LAYTYP(IN).LE.0) IBW = MAX0(IBW,2)
              IF(LAYTYP(IN).EQ.1.OR.LAYTYP(IN).EQ.2) IBW = MAX0(IBW,3)
              IF(LAYTYP(IN).EQ.3.OR.LAYTYP(IN).EQ.4) IBW = MAX0(IBW,4)
              GO TO 500
            ELSE
              IF(LAYTYP(IN).LE.0) IBW = MAX0(IBW,2)
              IF(LAYTYP(IN).EQ.1.OR.LAYTYP(IN).EQ.2) IBW = MAX0(IBW,4)
              IF(LAYTYP(IN).EQ.3.OR.LAYTYP(IN).EQ.4) IBW = MAX0(IBW,6)
              GO TO 500
            ENDIF
          ELSE
            IF(LAYTYP(IN).LE.0) IBW = MAX0(IBW,5)
            IF(LAYTYP(IN).EQ.1.OR.LAYTYP(IN).EQ.2) IBW = MAX0(IBW,6)
            IF(LAYTYP(IN).EQ.3.OR.LAYTYP(IN).EQ.4) IBW = MAX0(IBW,7)
            GO TO 500
          ENDIF
        ENDIF
Cms
Cms  other cases
Cms
      IF (V(IN,2).GT.1E-10) GO TO 100   
C                   
C     VACUUM UPPER HALFSPACE            
C     **********************            
      IF (V(IN1,3).GT.1E-10) GO TO 50   
C     NEXT LAYER OR HALFSPACE IS LIQUID 
C     --------------------------------- 
      I=IPS(IN1,1)  
      J=IPS(IN1,3)  
      IF (IN1.LT.NUML) IBW=MAX0(IBW,IABS(I-J))              
      GO TO 500     
 50   CONTINUE      
C     NEXT LAYER OR HALFSPACE IS SOLID  
C     --------------------------------  
      I=IPS(IN1,1)  
      J=IPS(IN1,2)  
      IBW=MAX0(IBW,IABS(I-J))           
      IF (IN1.GE.NUML) GO TO 500        
      K=IPS(IN1,3)  
      L=IPS(IN1,4)  
      IBW=MAX0(IBW,IABS(I-K),IABS(I-L),IABS(J-K),IABS(J-L)) 
      GO TO 500     
 100  IF (V(IN,3).GT.1E-10) GO TO 200   
C                   
C     LAYER OR HALFSPACE OVER INTERFACE IS LIQUID           
C     *******************************************           
      IF (V(IN1,2).GT.1E-10) GO TO 125  
C     LOWER HALFSPACE IS VACUUM         
C     -------------------------         
      I=IPS(IN,3)   
      J=IPS(IN,1)   
      IF (IN.GT.1) IBW=MAX0(IBW,IABS(I-J))                  
      GO TO 500     
 125  IF (V(IN1,3).GT.1E-10) GO TO 150  
C     NEXT LAYER OR HALFSPACE IS LIQUID 
C     --------------------------------- 
      I=IPS(IN,3)   
      J=IPS(IN1,1)  
      IBW=MAX0(IBW,IABS(I-J))           
      IF (IN.GE.NUMI) GO TO 130         
      K=IPS(IN1,3)  
      IBW=MAX0(IBW,IABS(I-K),IABS(J-K)) 
 130  IF (IN.LE.1) GO TO 500            
      K=IPS(IN,1)   
      IBW=MAX0(IBW,IABS(I-K),IABS(J-K)) 
      GO TO 500     
C                   
C     NEXT LAYER OR HALFSPACE IS SOLID  
C     --------------------------------  
 150  CONTINUE      
      I=IPS(IN,3)   
      J=IPS(IN1,1)  
      K=IPS(IN1,2)  
      IBW=MAX0(IBW,IABS(I-J),IABS(I-K),IABS(J-K))           
      IF (IN.GE.NUMI) GO TO 180         
      M=IPS(IN1,3)  
      L=IPS(IN1,4)  
      IBW=MAX0(IBW,IABS(I-M),IABS(I-L),IABS(J-M),IABS(J-L), 
     1   IABS(K-M),IABS(K-L))           
 180  IF (IN.LE.1) GO TO 500            
      M=IPS(IN,1)   
      IBW=MAX0(IBW,IABS(I-M),IABS(J-M),IABS(K-M))           
      GO TO 500     
 200  CONTINUE      
C     LAYER OR HALFSPACE OVER INTERFACE IS SOLID            
C     *******************************************           
C     LOWER HALFSPACE VACUUM            
C     ----------------------            
      IF (V(IN1,2).GT.1E-10) GO TO 225  
      I=IPS(IN,3)   
      J=IPS(IN,4)   
      IBW=MAX0(IBW,IABS(I-J))           
      IF (IN.LE.1) GO TO 500            
      K=IPS(IN,1)   
      L=IPS(IN,2)   
      IBW=MAX0(IBW,IABS(I-K),IABS(I-L),IABS(J-K),IABS(J-L)) 
      GO TO 500     
 225  IF (V(IN1,3).GT.1E-10) GO TO 250  
C     NEXT LAYER OR HALFSPACE IS LIQUID 
C     --------------------------------- 
      I=IPS(IN,3)   
      J=IPS(IN,4)   
      K=IPS(IN1,1)  
      IBW=MAX0(IBW,IABS(I-J),IABS(I-K),IABS(J-K))           
      IF (IN.GE.NUMI) GO TO 230         
      M=IPS(IN1,3)  
      IBW=MAX0(IBW,IABS(I-M),IABS(J-M),IABS(K-M))           
 230  IF (IN.LE.1) GO TO 500            
      M=IPS(IN,1)   
      N=IPS(IN,2)   
      IBW=MAX0(IBW,IABS(I-M),IABS(I-N),IABS(J-M),IABS(J-N), 
     1         IABS(K-M),IABS(K-N))     
      GO TO 500     
 250  CONTINUE      
C     NEXT LAYER OR HALFSPACE IS SOLID  
C     --------------------------------  
      I=IPS(IN,3)   
      J=IPS(IN,4)   
      K=IPS(IN1,1)  
      L=IPS(IN1,2)  
      IBW=MAX0(IBW,IABS(I-J),IABS(I-K),IABS(I-L),IABS(J-K), 
     1         IABS(J-L),IABS(K-L))     
      IF (IN.GE.NUMI) GO TO 280         
      M=IPS(IN1,3)  
      N=IPS(IN1,4)  
      IBW=MAX0(IBW,IABS(I-M),IABS(I-N),IABS(J-M),IABS(J-N), 
     1         IABS(K-M),IABS(K-N),IABS(L-M),IABS(L-N))     
 280  IF (IN.LE.1) GO TO 500            
      M=IPS(IN,1)   
      N=IPS(IN,2)   
      IBW=MAX0(IBW,IABS(I-M),IABS(I-N),IABS(J-M),IABS(J-N), 
     1         IABS(K-M),IABS(K-N),IABS(L-M),IABS(L-N))     
 500  CONTINUE      
      RETURN        
      END           

      SUBROUTINE FLLNEG
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
      INCLUDE 'compar.f'
      INCLUDE 'comnp.f'
C
      DO 10 I=1,3
      IF (IOUT(I).NE.1) GO TO 10
      IF (I.LT.3) THEN
      CALL CVMOV(CFF(ICUT1,I),2,CFF(ICUT1-1,I),-2,NWVNO/2)
      ELSE
      CALL CVNEG(CFF(ICUT1,I),2,CFF(ICUT1-1,I),-2,NWVNO/2)
      END IF
 10   CONTINUE
      RETURN
      END

      real function p(eta,ind)
      PARAMETER (PIP=3.1415927,TWOPI=2*3.1415927,SQ2PI=2.506628,
     &           twoopi=2.0/3.1415927 )
C
C     function returning roughness spectrum
c     eta:   wavenumber
c     ind:   interface number (equal to layer below)
C
      include 'compar.f'
      include 'comnla.f'
      real kl,kl2,kz,model
      if (goff.and.amod(ind).gt.0.0) then
c >>> Goff-Jordan spectrum
c       p=PIP*abs(clen(ind))*((eta*clen(ind))**2+1)**(-1.5)
               model=amod(ind)
               if (icdr.eq.0) then
c >>> Cylindrical geometry
c >>> 2-D spectral model (Turgut, JASA 102(2), 833-852, 1997, eq.(9)
        
                frc=exp(gammln(model-0.5))/
     C              exp(gammln(model-1.5))

                P= twopi*(clen(ind)**2)/PI*frc*
     &             (1.0+(eta*clen(ind))**2)**(-model+0.5)

               else
c >>> 1-D spectral model (Turgut, JASA 102(2), 833-852, 1997, eq.(10)

                frc=exp(gammln(model-1.0))/
     C              exp(gammln(model-1.5))
c                write(6,*) 'fc=',frc/sqrt(pip)
                P= twopi*clen(ind)/sqrt(PI)*frc*
     &             (1.0+(eta*clen(ind))**2)**(-model+1.0)

               endif

      else if (pierson) then
       kl=TWOPI/abs(clen(ind))
       kl2=kl*kl
       aeta=abs(eta)
       eta2=eta*eta
       if (aeta.lt.kl*1E-3) then
        p=twopi/(1.5*pip*kl)
       else if (aeta.lt.0.5*kl) then
        p=twopi*kl2/(2*aeta**3)*
     &     twoopi*( asin(aeta/kl)-aeta*sqrt(kl2-eta2)/kl2 )
       else if (aeta.lt.kl) then
        p=twopi*kl2/(2*aeta**3)*
     &     ( 1E0-twoopi*(aeta*sqrt(kl2-eta2)/kl2 + acos(aeta/kl)) )
       else
        p=twopi*kl2/(2*aeta**3)
       end if
      else
c >>> Gaussian spectrum
       P=SQ2PI*abs(CLEN(IND))*EXP(-0.5*CLEN(IND)*CLEN(IND)
     1           *ETA*ETA)
      end if
      return
      end

      subroutine settrc()
c ********************************************************
c *                       OASES                          *
c *  Ocean Acoustic and Seismic Exploration Synthetics   *
c *                   Copyright (C)                      *
c *                  Henrik Schmidt                      *
c *       Massachusetts Institute of Technology          *
c *               Cambridge, MA 02139                    *
c ********************************************************
c
c >>> sets up stuff for tabulated surface reflection coefficient
      INCLUDE 'compar.f'
      INCLUDE 'comnla.f'
      INCLUDE 'comnp.f'
      INCLUDE 'comnrd.f'
      character*80 trcfil,brcfil
c >>>     
C
c >>> upper halfspace must be specified to vacuum
c
         if (laytyp(1).ne.-1) then
          stop '>>> Upper halfspace must be vacuum for tabular RC <<<'
         end if
         if (laytyp(2).ne.1.or.nosou(2).gt.0) then
          stop '>>> Uppermost layer must be source-free fluid <<<'
         end if
         if (bottomrc) then
          write(6,*) '>>> Currently tabulated RC only allowed for <<<'
          write(6,*) '>>> surface or bottom, not both.            <<<'
         end if
         CALL OPFILR(23,IOER)
         IF (IOER.NE.0) STOP '>>>> ERROR: .trc FILE NOT FOUND <<<<'
         inquire(unit=23,name=trcfil)
         WRITE( 6,'(2(1h ,a))') 
     &      '>>> Using tabulated top reflection coefficient',TRCFIL
         READ( 23, * ) frq1,frq2,nfr,islw
         if (abs(frq1-freq1).gt.1e-6.or.
     &       abs(frq2-freq2).gt.1e-6.or.
     &       nfr.ne.nfreq) then
          write(6,'(1h ,a,a)') 
     &     '>>> Frequency mismatch in tabulated RC file ',trcfil(1:30)
          stop
         end if
         slowrc=(islw.eq.1)
         if (slowrc) then
           write(6,'(a)')'>>> RC tabulated vs slowness. <<<'
         else
           write(6,'(a)')'>>> RC tabulated vs grazing angle. <<<'
         end if
      return
      entry setbrc()
c >>> sets up stuff for tabulated bottom reflection coefficient
c >>> lower halfspace must be specified to vacuum
c
         if (laytyp(numl).ne.-1) then
          stop '>>> Lower halfspace must be vacuum for tabular RC <<<'
         end if
         if (laytyp(numl-1).ne.1.or.nosou(numl-1).gt.0) then
          stop '>>> Lowermost layer must be source-free fluid <<<'
         end if
         CALL OPFILR(23,IOER)
         IF (IOER.NE.0) STOP '>>>> ERROR: .trc FILE NOT FOUND <<<<' 
         inquire(unit=23,name=brcfil)
         WRITE( 6, * ) 
     &      'Using tabulated bottom reflection coefficient',BRCFIL
         READ( 23, * ) frq1,frq2,nfr,islw
         if (abs(frq1-freq1).gt.1e-6.or.
     &       abs(frq2-freq2).gt.1e-6.or.
     &       nfr.ne.nfreq) then
          write(6,'(1h ,a,a)')
     &     '>>> Frequency mismatch in tabulated RC file ',brcfil(1:30)
          stop
         end if
         slowrc=(islw.eq.1)
         if (slowrc) then
           write(6,'(a)')'>>> RC tabulated vs slowness. <<<'
         else
           write(6,'(a)')'>>> RC tabulated vs grazing angle. <<<'
         end if
      return
c >>>>>>>>>>>>>
      entry gettrc()
c >>> Read Surface reflection coefficient table
         READ( 23, * ) frr, numref
         if (abs(frr-freq).gt.1e-6) then
          write(6,'(1h ,a)') 
     &     '>>> Frequency mismatch in tabulated RC file '
          write(6,*) '>>> In file:',frr,' <<<'
          stop
         end if
         read(23,*) ( TTHET( I ), RREFL( I ), PPHI( I ), I = 1, NUMREF )
         WRITE( 6, * ) 'Read top reflection coefficient'
c         WRITE( 6, * ) 
c     &      NUMREF, ( TTHET( I ), RREFL( I ), PPHI( I ), I = 1, NUMREF )
          DO I = 1, NUMREF
            PPHI( I ) = PI / 180. * PPHI( I )
            if (slowrc) then
             tthet(i)= 1e-3*tthet(i)
            else
             TTHET( I ) = PI / 180. * TTHET( I )
            end if
          end do
         if (tthet(numref).lt.tthet(1)) then
c >>>    reverse table sequence
          write(6,*) '>>> Table sequence reversed <<<' 
          call vmov(tthet(1),1,arg(numref),-1,numref)
          call vmov(arg(1),1,tthet(1),1,numref)
          call vmov(rrefl(1),1,arg(numref),-1,numref)
          call vmov(arg(1),1,rrefl(1),1,numref)
          call vmov(pphi(1),1,arg(numref),-1,numref)
          call vmov(arg(1),1,pphi(1),1,numref)
         end if
      return
c >>>>>>>>>>>>>
      entry getbrc()
c >>> Read Bottom reflection coefficient table
         READ( 23, * ) frr,numref
         if (abs(frr-freq).gt.1e-6) then
          write(6,'(1h ,a)') 
     &     '>>> Frequency mismatch in tabulated RC file '
          write(6,*) '>>> In file:',frr,' <<<'
          stop
         end if
         read(23,*) ( TTHET( I ), RREFL( I ), PPHI( I ), I = 1, NUMREF )
         WRITE( 6, * ) 'Read bottom reflection coefficient'
c         WRITE( 6, * ) 
c     &      NUMREF, ( TTHET( I ), RREFL( I ), PPHI( I ), I = 1, NUMREF )
          DO  I = 1, NUMREF
            PPHI( I ) = PI / 180. * PPHI( I )
            if (slowrc) then
             tthet(i)= 1e-3*tthet(i)
            else
             TTHET( I ) = PI / 180. * TTHET( I )
            end if
          end do
         if (tthet(numref).lt.tthet(1)) then
c >>>    reverse table sequence
          write(6,*) '>>> Table sequence reversed <<<' 
          call vmov(tthet(1),1,arg(numref),-1,numref)
          call vmov(arg(1),1,tthet(1),1,numref)
          call vmov(rrefl(1),1,arg(numref),-1,numref)
          call vmov(arg(1),1,rrefl(1),1,numref)
          call vmov(pphi(1),1,arg(numref),-1,numref)
          call vmov(arg(1),1,pphi(1),1,numref)
         end if
      return
      end 
