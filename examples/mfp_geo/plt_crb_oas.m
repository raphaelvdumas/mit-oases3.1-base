%%%%%%%%%%%%%%    plot crb - oasis output %%%%%%%%%%%%%%%%%%%%
clear
fcrb        = fopen('crbout','r')
nr          = fscanf(fcrb,'%f',1)
info        = fgetl(fcrb);
nz          = fscanf(fcrb,'%f',1)
info        = fgetl(fcrb);
crbhal      = zeros(nz,nr);
crbpyn      = zeros(nz,nr);
xraxis      = 25.*linspace(1,nr,nr);
xzaxis      = 25.*linspace(0,nz,nz);
for nrx = 1:nr
   for nzx = 1:nz
      xi = fscanf(fcrb,'%f %f %f',3)
      yi = fscanf(fcrb,'%f %f %f',3)
      crbhal(nrx,nzx) = xi(3);
      crbpyn(nrx,nzx) = yi(3);
   end
end
crbhal      = 10.*log10(crbhal);
crbpyn      = 10.*log10(crbpyn);
halmin      = -40.
halmax      = -20.
pynmin      = 0.
pynmax      = 10.
lvlshal     = linspace(halmin,halmax,11)
lvlspyn     = linspace(pynmin,pynmax,11)
figure
clratoc;
pcolor(xraxis,xzaxis,crbhal);
caxis([halmin,halmax]);
shading flat
hold on
cx          = contour(xraxis,xzaxis,crbhal,lvlshal,'k');
clabel(cx,lvlshal);
colorbar
title(' halocline error ');
ylabel(' Depth (m)   ')
xlabel(' Range (km)  ')
figure
clratoc;
pcolor(xraxis,xzaxis,crbpyn);
caxis([pynmin,pynmax]);
shading flat
hold on
cx          = contour(xraxis,xzaxis,crbpyn,lvlspyn,'k');
clabel(cx,lvlspyn);
colorbar
title(' pycncoline error ');
ylabel(' Depth (m)   ')
xlabel(' Range (km)  ')
ncls        = fclose(fcrb)

