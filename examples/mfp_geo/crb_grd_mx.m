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
rng         = zeros(1,nr);
fout        = fopen('crbout','w');
              fprintf(fout,'%f /n',nr);
              fprintf(fout,'%f10.4 /n',rmax);
              fprintf(fout,'%f /n',nz);
              fprintf(fout,'%f10.4 /n',zmax);
              fclose(fout);
for ndz=1:nz
   depth = zgrid(ndz);
   if(depth < .1)
       depth = 5.
   end
   for ndr=1:nr-1
       rngx = rgrid(ndr);
       [ndr  rng(ndr) depth]
       dbuf = ['buf',int2str(ndr),'.dat'];
       dname = ['dumx_',int2str(ndz),'_',int2str(ndr)];
       eval(['!sed -e "s/xxx/',num2str(rngx),'/g"  abb_template.dat  >' dbuf]);
       eval(['!sed -e "s/yyy/',num2str(depth),'/g"  ' dbuf ' > ' dname '.dat']);
       eval(['!oasi ' dname '> & /dev/null &']) 
   end
   for ndr=nr:nr
       rngx  = rgrid(ndr);
       [ndr rng(ndr) depth]
       dbuf = ['buf',int2str(ndr),'.dat'];
       dname = ['dumx_',int2str(ndz),'_',int2str(ndr)];
       eval(['!sed -e "s/xxx/',num2str(rngx),'/g"  abb_template.dat  >' dbuf]);
       eval(['!sed -e "s/yyy/',num2str(depth),'/g" ' dbuf ' > ' dname '.dat']);
       eval(['!oasi ' dname])
    end
end

