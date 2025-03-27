%[freq_beam]=abel_k_w(fname,chan,ping,z,kz,co,shading,freqs)
%
% Kevin D. LePage
% 5/21/98
% SACLANTCEN
%
% INPUTS
%
% fname		filename		
% chan		chan vector (assumed to overlap number of cards if
%               larger than noCard)
% ping          ping vector
% z		element locations (m)
% kz		desired wavenumbers
% co		sound speed (m/s) (default 1500 m/s)
% shading	spatial shading (default hanning(length(z)))
% freqs         frequencies at which beamforming is desired (default all)
%
% OUTPUTS
%
% freq_beam	frequency-beam output of beamformer (plot using 20log10(abs))

function[freq_beam]=abel_k_w(fname,chan,ping,z,kz,co,shading,freqs)

if nargin<6

co=1500;

end

if nargin<7

shading=hanning(length(z));

end

for j=1:length(ping)

%fprintf('j=%d out of %d\n',j,length(ping))

[data,fs]=abel_reader(fname,chan,ping(j));

data=detrend(data');

% condition data

temp=abs(data);

vec=mean(mean(temp));

for ll=1:size(data,2)

spike_ind=find(temp(:,ll)>100*vec);

spike_ind=ones(150,1)*spike_ind(:)'+[-75:74]'*ones(1,length(spike_ind));

data(spike_ind(:),ll)=zeros(length(spike_ind(:)),1);

end

clear temp;

data=fft(data);

if j==1

num_l=size(data,1);

num_kz=length(kz);

% define frequency vector

if nargin<8

freq=[0:fs/num_l:fs/num_l*(num_l/2-1)];

iind=[1:num_l/2];

else

for k=1:length(freqs)

[val,ind]=min(abs(freqs(k)-[0:fs/num_l:fs/num_l*(num_l/2-1)]));

freq(k)=(ind-1)/num_l*fs;

iind(k)=ind;

end

end

freq_beam=zeros(num_kz,length(freq)*length(ping));

% end of if loop about first ping

end

for k=1:length(freq)

fprintf('\rj=%d out of %d, freq=%d out of %d',j,length(ping),k,length(freq))

% build steering vectors

steer=(ones(num_kz,1)*shading(:)').*exp(-i*kz(:)*z(:)');

freq_beam(:,(j-1)*length(freq)+k)=steer*data(iind(k),:).';

end

clear data

% end of loop over pings

end





