nt=2048
dt=0.0004
f0=320,0
wim=4.775419

nk=2048
dkr=2.0483052e-3
cw=1510.0

rmax=pi/dkr

zmax=ceil(rmax*0.5/100.)*100.
nz=101
dz=zmax/(nz-1)


tmax=min(nt*dt,rmax/cw)
nframes=60
nfrm=20

nsteps=(nframes-1)/nfrm+1;

load -ascii vol.wns

wave_freq=vol(:,1)+i*vol(:,2);

wave_freq=reshape(wave_freq,nk,length(wave_freq)/nk).';

wkmax=((nk-1.0)/2.0)*dkr
wkmin=-wkmax

fs=1/dt;

delt=tmax/nframes
tstep=delt*fs

dw=fs/nt*2*pi

w=((floor(f0*2*pi/dw)-1)*dw+[1:size(wave_freq,1)]'*dw-i*wim);

w(1)

k=w/cw;

kz=(((k.^2*ones(1,nk)-(ones(size(wave_freq,1),1)*[wkmin:dkr:wkmax]).^2)').^(.5)).';

kz=real(kz)-i*abs(imag(kz));

for j=1:size(wave_freq,1)

wave_freq(j,:)=fftshift(wave_freq(j,:));

kz(j,:)=fftshift(kz(j,:));

end

zz=[0:dz:zmax]';
zzp=[-zmax:dz:0];

for ll=1:nsteps

tt=fftshift([(ll-1)*nfrm:(ll-1)*nfrm+(nfrm-1)])*delt;

time=(exp(i*w*tt).*(exp(-(w-mean(w)).^2/((max(w)-min(w))/8)^2)*ones(1,length(tt)))).';

if(ll==1)

field=zeros(length(zz),nk*length(tt));

size(field)

end

for l=1:length(zz)

fprintf('\rl=%d out of %d',l,length(zz))

field(l,:)=reshape(abs(fftshift(fft((time*(exp(-i*kz*zz(l)).*wave_freq/nk)).').')),1,nk*length(tt));

end

ttt=fftshift(tt);

for j=1:length(tt)

wavei(flipud(dba(field(:,j:length(tt):size(field,2)))),[-pi/dkr:pi/dkr],zzp,-40,20);eval(['text(rmax*0.7,-zmax*0.85,''' num2str(ttt(j)) ' sec'')']);eval(['a=text(rmax*0.7,-zmax*0.925,''' num2str(ttt(j)) ' sec'');']);set(a,'Color',[1 1 1]);drawnow
xlabel('Range (m)')
ylabel('Depth(m)')


if j==1&ll==1

mov=moviein(length(tt)*6);

end

mov(:,j+(ll-1)*length(tt))=getframe;

end

end


