load -ascii wave_freq.out

wave_freq=wave_freq(1:2:length(wave_freq))+i*wave_freq(2:2:length(wave_freq));

wave_freq=reshape(wave_freq,2048,length(wave_freq)/2048).';

fs=8192/2;

dkr=2*pi/2000;

w=(500+[1:size(wave_freq,1)]'*.5+i*.35)*2*pi;

k=w/1500;

kz=(((k.^2*ones(1,2048)-(ones(size(wave_freq,1),1)*[-1024:1023]*dkr).^2)').^(.5)).';

kz=real(kz)+i*abs(imag(kz));

for j=1:size(wave_freq,1)

wave_freq(j,:)=fftshift(wave_freq(j,:));

kz(j,:)=fftshift(kz(j,:));

end

zz=[0:4:400]';

tt=fftshift([1:5])/fs*500;

time=(exp(-i*w*tt).*(exp(-(w-mean(w)).^2/((max(w)-min(w))/8)^2)*ones(1,length(tt)))).';

field=zeros(length(zz),2048*length(tt));

for l=1:length(zz)

fprintf('\rl=%d out of %d',l,length(zz))

field(l,:)=reshape(abs(fftshift(ifft((time*(exp(i*kz*zz(l)).*wave_freq)).').')),1,2048*length(tt));

end

ttt=fftshift(tt);

for j=1:length(tt)-1

subplot(5,1,j+1)

wavei(flipud(dba(field(:,j:length(tt):size(field,2)))),[-1000:1000],zz,55,115);eval(['text(0,100,''' num2str(ttt(j)) ' sec'')']);eval(['a=text(0,50,''' num2str(ttt(j)) ' sec'');']);set(a,'Color',[0 0 0]);drawnow

end

subplot(5,1,1)

wavei(flipud(dba(field(:,5:length(tt):size(field,2)))),[-1000:1000],zz,55,115);eval(['text(0,100,''' num2str(ttt(5)) ' sec'')']);eval(['a=text(0,50,''' num2str(ttt(5)) ' sec'');']);set(a,'Color',[0 0 0]);drawnow

end

orient('tall')


