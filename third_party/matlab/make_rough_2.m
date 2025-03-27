%function[rough]=make_rough_2(nfft,R,corr,skew,model,dist,out)
%
% function to generate 2-D roughness realizations or power spectra
%
% Kevin D. LePage
% SACLANTCEN
% 12/2/97
%
% INPUTS
%
% nfft		vector of desired patch size ([nfftx nffty])
% R		vector of desired size of patch ([X Y])
% corr		vector of corr length scale of patch ([lx ly])
% skew		skew angle of lx away from x (degrees)
% model         4,3,2 or -1 (3-D, 2.5-D 2-D and Gaussian 2-D)
% dist		amplitude distribution ('gauss' or 'expon')
% out		1 for realization, 2 for power spectrum, 3 for spatial filter
%
% OUTPUTS
%
% rough		desired roughness product (nffty x nfftx)

function[rough]=make_rough_2(nfft,R,corr,skew,model,dist,out)

frac=model/2+1;

if nargin<6

out=1;

end

cos2=cos(skew/180*pi)^2;

sin2=sin(skew/180*pi)^2;

ellx=corr(1)^2*cos2+corr(2)^2*sin2;

elly=corr(2)^2*cos2+corr(1)^2*sin2;

ellxy=2*sqrt(cos2*sin2)*(corr(1)^2-corr(2)^2);

X=R(1);

Y=R(2);

nfftx=nfft(1);

nffty=nfft(2);

skew=skew*pi/180;

% get the range discretization

drx=X/nfftx;

dry=Y/nffty;

% get the range matrix

rx=[0:drx:X-drx];

ry=[0:dry:Y-dry];

length(rx);

% get the spatial filter function to convolve with the scatterer 
% distribution

dkx=2*pi/X;

dky=2*pi/Y;

kkx=ones(nffty,1)*[-pi/drx:2*pi/X:pi*(nfftx-1)/drx/nfftx];

kky=[-pi/dry:2*pi/Y:pi*(nffty-1)/dry/nffty]'*ones(1,nfftx);

if model<=0

pk=exp(-(kkx.^2*ellx+kkx.*kky*ellxy+kky.^2*elly)/2);

if out==1 

if dist=='gauss'

fprintf('distribution is Gaussian\n')

rough=fft2(fftshift(pk.^(.5).*(randn(size(pk))+i*randn(size(pk)))));

elseif dist=='expon'

fprintf('distribution is exponential\n')

field=(-log(rand(size(pk)))/sqrt(2).*sign(rand(size(pk))-.5));

rough_shape=fftshift(fft2(fftshift(pk.^(.5))));

[i_ind,j_ind]=find(rough_shape<1e-2);

i_ind=min(find(diff(i_ind)>1));

j_ind=min(find(diff(j_ind)>1));

rough_shape=rough_shape(floor(size(pk,1)/2)-i_ind:floor(size(pk,1)/2)+i_ind,floor(size(pk,2)/2)-j_ind:floor(size(pk,2)/2)+j_ind);

rough=conv2(field,rough_shape,'same');

%rough=fft2(fftshift(pk.^(.5).*(-log(rand(size(pk)))/sqrt(2).*sign(rand(size(pk))-.5)-i*log(rand(size(pk)))/sqrt(2).*sign(rand(size(pk))-.5))));

end

rough=real(rough)/(mean(mean(real(rough).^2))).^(.5);

elseif out==2

rough=pk;

rough=rough./sum(sum(rough))/dkx/dky;

else

rough=fft2(fftshift(pk.^(.5)));

rough=real(rough)/max(max(real(rough)));

end

else

nu=3-frac;

pk=(1+(kkx.^2*ellx+kkx.*kky*ellxy+kky.^2*elly)).^(-nu-.5);

if out==1

if dist=='gauss'

fprintf('distribution is Gaussian\n')

rough=fft2(fftshift(pk.^(.5).*(randn(size(pk))+i*randn(size(pk)))));

elseif dist=='expon'

fprintf('distribution is exponential\n')

field=(-log(rand(size(pk)))/sqrt(2).*sign(rand(size(pk))-.5));

rough_shape=fftshift(fft2(fftshift(pk.^(.5))));

[i_ind,j_ind]=find(rough_shape<1e-2);

i_ind=min(find(diff(i_ind)>1));

j_ind=min(find(diff(j_ind)>1));

rough_shape=rough_shape(floor(size(pk,1)/2)-i_ind:floor(size(pk,1)/2)+i_ind,floor(size(pk,2)/2)-j_ind:floor(size(pk,2)/2)+j_ind);

rough=conv2(field,rough_shape,'same');

%rough=fft2(fftshift(pk.^(.5).*(-log(rand(size(pk)))/sqrt(2).*sign(rand(size(pk))-.5)-i*log(rand(size(pk)))/sqrt(2).*sign(rand(size(pk))-.5))));

end

rough=real(rough)/(mean(mean(real(rough).^2))).^(.5);

elseif out==2

rough=pk;

rough=rough./sum(sum(rough))/dkx/dky;

else

rough=fft2(fftshift(pk.^(.5)));

rough=real(rough)/max(max(real(rough)));

end

end



