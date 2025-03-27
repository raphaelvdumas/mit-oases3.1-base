function[rough]=make_rough(nfft,l,corr,model,out)
%
% makes one or two dimensional rougness realizations of unity variance
%
% function[rough]=make_rough(nfft,l,corr,model,out)
% nfft = size of spatial sequence
% l = length of spatial sequence (if matrix, two-D spec or rough output)
% corr = correlation length of roughness
% model = goff4 - 2 D (fractal dimension) Goff Jordan
% model = goff3 - 2.5 D (fractal dimension) Goff Jordan
% model = goff2 - 3 D (fractal dimension) Goff Jordan
% model = gauss - 2 D (fractal dimension) Gaussian
% out = 1 if roughness realization is required
% out = 2 if roughness power spectrum is required
% out = 3 if mean of roughness only is required
randn('seed',sum(100*clock))
junk=randn(1,nfft);     
junk2=randn(1,nfft);     

%nfft
%l
%corr
%model
%out

%fprintf('in make_rough, nfft=%d, l=%f, corr=%f, model=%s, out=%d\n',nfft,l,corr,model,out)

%pause(4)

if(length(l)<size(l,1)*size(l,2))

nfftx=size(l,1);
nffty=size(l,2);

end

wave_vec=[-nfft/2:nfft/2-1]*2*pi/l;

% change corr to scale as a correlation length correctly with wavenumbers

%corr=corr*2*pi;

% Goff Jordan, fractal dimension 2

if model == 'goff4'

%plot(wave_vec,dba(sqrt(corr/sqrt(pi/2)*2*pi/l)*sqrt(((wave_vec*corr).^(2)+1).^(-2)).*(junk+i*junk2)))

if(out==3)

rough=sum(real(sqrt(((wave_vec*corr).^(2)+1).^(-2)).*(junk+i*junk2)));

rough=rough*sqrt(corr/sqrt(pi/2)*2*pi/l);

else

rough=real(fft(fftshift(sqrt(((wave_vec*corr).^(2)+1).^(-2)).*(junk+i*junk2))));

rough=rough*sqrt(corr/sqrt(pi/2)*2*pi/l);

spec=fftshift((((wave_vec*corr).^(2)+1).^(-2)))*(corr/sqrt(pi/2)*2*pi/l);

end

% Goff Jordan, fractal dimension 2.5

elseif model == 'goff3'

%%plot(wave_vec,dba(sqrt(corr/sqrt(pi)*2*pi/l)*sqrt(((wave_vec*corr).^(2)+1).^(-1.5)).*(junk+i*junk2)))

if out==3

rough=sum(real(sqrt(((wave_vec*corr).^(2)+1).^(-1.5)).*(junk+i*junk2)));

rough=rough*sqrt(corr/sqrt(pi)*2*pi/l);

else

rough=real(fft(fftshift(sqrt(((wave_vec*corr).^(2)+1).^(-1.5)).*(junk+i*junk2))));

rough=rough*sqrt(corr/sqrt(pi)*2*pi/l);

spec=fftshift((((wave_vec*corr).^(2)+1).^(-1.5)))*(corr/sqrt(pi)*2*pi/l);

end

% Goff Jordan, fractal dimension 3

elseif model == 'goff2'

%plot(wave_vec,dba(sqrt(corr/sqrt(2*pi)*2*pi/l)*sqrt(((wave_vec*corr).^(2)+1).^(-1)).*(junk+i*junk2)))

if out==3

rough=sum(real(sqrt(((wave_vec*corr).^(2)+1).^(-1)).*(junk+i*junk2)));

rough=rough*sqrt(corr/sqrt(2*pi)*2*pi/l);

else

rough=real(fft(fftshift(sqrt(((wave_vec*corr).^(2)+1).^(-1)).*(junk+i*junk2))));

rough=rough*sqrt(corr/sqrt(2*pi)*2*pi/l);

spec=fftshift((((wave_vec*corr).^(2)+1).^(-1)))*(corr/sqrt(2*pi)*2*pi/l);

end

% Gaussian

elseif model == 'gauss'

%plot(wave_vec,dba(sqrt(corr/sqrt(2*pi)*2*pi/l)*(exp(-(wave_vec*corr/1.414).^(2))).^(.5).*(junk+i*junk2)))

%grid

if out==3

rough=sum(real(exp(-(wave_vec*corr/1.414).^(2))).^(.5).*(junk+i*junk2));

rough=rough*sqrt(corr/sqrt(2*pi)*2*pi/l);

else

rough=real(fft(fftshift((exp(-(wave_vec*corr/1.414).^(2))).^(.5).*(junk+i*junk2))));

rough=rough*sqrt(corr/sqrt(2*pi)*2*pi/l);

spec=fftshift((exp(-(wave_vec*corr/1.414).^(2))))*(corr/sqrt(2*pi)*2*pi/l);

end

end

if out==2,
rough=spec;
end




