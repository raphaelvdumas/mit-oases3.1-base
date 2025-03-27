%        %          %          %          %          $          %          %         %
%%%%%%%%%%%%%%% Grid for CRB output %%%%%%%%%%%%%%%%%%%
%%%%%%       Get input file template 
%%%%%%       Scanning grid
clear
rmax        = 200.;
nr          = 8;
dr          = rmax/nr;
rgrid       = linspace(dr,rmax,nr);
zmax        = 200.;
nz          = 8;
dz          = zmax/nz;
zgrid       = linspace(0.,zmax,nz);
crbhlc      = zeros(nz,nr);
crbpyn      = zeros(nz,nr);
pltout      = zeros(1,6);
xi          = zeros(1,1);
!rm crbout
fout        = fopen('crbout','w')
              fprintf(fout,'%5.0f\n',nr)
              fprintf(fout,'%5.0f\n',nz)
              fclose(fout)
foutx       = fopen('crboutx','w')
              fprintf(fout,'%5.0f\n',nr)
              fprintf(fout,'%5.0f\n',nz)
eval(['!sed "s/100 100 0/ 25  25 0/"<abb_template.dat >output.dat']);
myformat    = '%5.0f %5.0f %12.8f\n'
for ndr=1:nr
   rng      = rgrid(ndr); 
   for ndz=1:nz
       depth = zgrid(ndz);
       if(depth < .1)
           depth = 5.
       end
       [rng depth]
       outfile = ['abb_cr_',int2str(rng),'_',int2str(depth),'.dat']
       newdata = [int2str(rng),' ',int2str(depth),' 0']
       rngchr  = int2str(rng);
       depthchr = int2str(depth);
       eval(['!sed "s/xxx/',num2str(rng),'/" < abb_template.dat  > dum1.dat']);
       eval(['!sed "s/yyy/',num2str(depth),'/" < dum1.dat > dum2.dat']);
       eval(['!cp dum2.dat abb_cr_',int2str(rng),'_',int2str(depth)]);
       eval(['!oasi dum2']);
       fcrb = fopen('dum2.plt','r')
       for ix=1:6
	  xi=fscanf(fcrb,'%f',1);
	  info = fgetl(fcrb);
       end
       info = fgetl(fcrb);
       for ix=1:6
	  xi=fscanf(fcrb,'%f',1)
          info = fgetl(fcrb);
	  pltout(ix) = xi;
       end
       crb1=abs(pltout(5)-pltout(4));
       crb2=abs(pltout(3)-pltout(2));
       crbhlc(ndz,ndr) = crb1
       crbpyn(ndz,ndr) = crb2
       fout = fopen('crbout','a')
       x1 = [ndz ndr crb1]
       x2 = [ndz ndr crb2]
       fout = fopen('crbout','a')
       fprintf(fout,myformat,x1);
       fprintf(fout,myformat,x2);
       fclose(fout)
    end
end
fprintf(foutx,'%12.8f\n',crbhlc); 
fprintf(foutx,'%12.8f\n',crbpyn);
fclose(foutx) 
