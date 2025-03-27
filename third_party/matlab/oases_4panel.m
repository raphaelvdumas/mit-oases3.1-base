fid=fopen('fort.47')

num_freq=str2num(fscanf(fid,'%s',1));
wo=str2num(fscanf(fid,'%s',1));
woi=str2num(fscanf(fid,'%s',1));
cap_k_neg=str2num(fscanf(fid,'%s',1));
cap_k_i=str2num(fscanf(fid,'%s',1));
dkr=str2num(fscanf(fid,'%s',1));
num=str2num(fscanf(fid,'%s',1));
fscanf(fid,'%s',1);
dw=str2num(fscanf(fid,'%s',1))-wo
fs=dw*num/2/pi
fclose(fid)

load -ascii fort.48

wave_freq=fort(:,1)+i*fort(:,2);

wave_freq=wave_freq(1:floor(length(wave_freq)/num)*num,:);

wave_freq=reshape(wave_freq,num,length(wave_freq)/num).';

w=(wo+[0:size(wave_freq,1)-1]'*dw-i*woi);

w(1)/2/pi

k=w/1500;

kz=(((k.^2*ones(1,num)-(ones(size(wave_freq,1),1)*[-num/2+1:num/2]*dkr).^2)').^(.5)).';

kz=real(kz)-i*abs(imag(kz));

fac=input('input desired delta c/co');

wave_freq=wave_freq*fac;

for j=1:size(wave_freq,1)

wave_freq(j,:)=fftshift(wave_freq(j,:));

kz(j,:)=fftshift(kz(j,:));

end

zz=[0:2:332]';

tt=[.15 .3 .45 .6];

time=(exp(i*w*fftshift(tt)).*(exp(-(w-mean(w)).^2/((max(w)-min(w))/8)^2)*ones(1,length(tt)))).';

field=zeros(length(zz),num*length(tt));

for l=1:length(zz)

fprintf('\rl=%d out of %d',l,length(zz))

field(l,:)=reshape(abs(fftshift(fft((time*(exp(-i*kz*zz(l)).*wave_freq/num)).').')*dkr),1,num*length(tt));

end

for j=1:length(tt)

bla=dba(field(:,j:length(tt):size(field,2)));

max_bla=max(max(bla));

%max_bla=40;

min_bla=max_bla-45;

subplot(length(tt),1,j)

imagesc([-1 1]*pi/dkr,zz,flipud(bla),[min_bla max_bla]);eval(['text(0,100,''' num2str(tt(j)) ' sec'')']);eval(['a=text(0,50,''' num2str(tt(j)) ' sec'');']);set(a,'Color',[1 1 1]);axis([-pi/dkr pi/dkr min(zz) max(zz)]);drawnow
hold on
a=plot(0,272,'r*');set(a,'MarkerSize',12);set(a,'LineWidth',2)
hold off
colorbar

%axis('equal')

end







