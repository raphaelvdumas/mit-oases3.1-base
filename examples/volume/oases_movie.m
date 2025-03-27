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

%nnum=input('available number of frequencies =%d, how many do you want?',floor(length(wave_freq)/num))

wave_freq=wave_freq(1:floor(length(wave_freq)/num)*num,:);

wave_freq=reshape(wave_freq,num,length(wave_freq)/num).';

w=(wo+[0:size(wave_freq,1)-1]'*dw-i*woi);

w(1)/2/pi

k=w/1500;

kz=(((k.^2*ones(1,num)-(ones(size(wave_freq,1),1)*[-num/2+1:num/2]*dkr).^2)').^(.5)).';

kz=real(kz)-i*abs(imag(kz));

for j=1:size(wave_freq,1)

wave_freq(j,:)=fftshift(wave_freq(j,:));

kz(j,:)=fftshift(kz(j,:));

end

zz=[0:0.2:33.2]';

for ll=1:2

tt=fftshift([(ll-1)*20:(ll-1)*20+19])/fs*50;

time=(exp(i*w*tt).*(exp(-(w-mean(w)).^2/((max(w)-min(w))/8)^2)*ones(1,length(tt)))).';

if(ll==1)

field=zeros(length(zz),num*length(tt));

size(field)

end

for l=1:length(zz)

fprintf('\rl=%d out of %d',l,length(zz))

field(l,:)=reshape(abs(fftshift(fft((time*(exp(-i*kz*zz(l)).*wave_freq/num)).').')),1,num*length(tt));

end

ttt=fftshift(tt);

for j=1:length(tt)

bla=dba(field(:,j:length(tt):size(field,2)));

max_bla=max(max(bla));

%max_bla=40;

min_bla=max_bla-40;

wavei(bla,[-1 1]*pi/dkr,zz,min_bla,max_bla);eval(['text(0,100,''' num2str(ttt(j)) ' sec'')']);eval(['a=text(0,50,''' num2str(ttt(j)) ' sec'');']);set(a,'Color',[0 0 0]);axis([-pi/dkr pi/dkr min(zz) max(zz)]);drawnow

if j==1&ll==1

mov=moviein(length(tt)*2,gcf);

end

mov(:,j+(ll-1)*length(tt))=getframe(gcf);

end

end







