%function[t_beam_avg]=kevin_stacker(fname,num_avg,R,T,dt,bw,beam,cmin,cmax)
%
% function to load fname.trf files in , turn them into time series
% beamform them, take their complex envelope, take the manitude, and average
%
% Kevin D. LePage
% SACLANTCEN
% 250800
%
% INPUTS
%
% fname		file name with or without .trf extension
% num_avg	number of averages (assumes file name extensions fname_01.trf)
% R		desired range (km)
% T		desired time vector limits for Fourier synthesis (2 arg)
% dt		delta time (sec)
% bw		desired bandwidth (hz)
% beam		desired beams in degrees
%
% OUTPUTS
%
% t_beam_avg	average beam time series

function[t_beam_avg]=kevin_stacker(fname,num_avg,R,T,dt,bw,beam,cmin,cmax)

if fname(length(fname)-3:length(fname))~='.trf'

fname=[fname '.trf']

end

fs=1/dt;

t=T(1):dt:T(2);

t_beam_avg=zeros(2^ceil(log2(length(t))),length(beam));

out=0;

for j=1:num_avg

junk=num2str(j);

fname_int=[fname(1:length(fname)-4) '_' junk '.trf']; 

out_old=out;

[out,sd,z,range,f,fo,omegim]=trf_reader_oases(fname_int);

omegim

if j==1

range_ind=find(range==R);

ts_out_temp=zeros(length(z),length(t));

else

figure(3)

plot(out(:,:,1)-out_old(:,:,1))

drawnow

end

[ts_out]=trf_time_series(out,z,range,f,fo,omegim,t,bw);

ts_out_temp(:,:)=real(ts_out(:,:,range_ind));

[t_beam]=beamform(ts_out_temp',z,fs,beam,1500);

t_beam_avg=t_beam_avg+abs(hilbert(t_beam))/(num_avg*2*pi*bw*sqrt(2*pi));

figure(4)

tim=[T(1):1/fs:T(2)];

tit= [ fname ' - ' num2str(j) ' ensemples']
wavei(dba(t_beam_avg(1:length(tim),:)'),T,beam,cmin-(num_avg-j)*3,cmax-(num_avg-j)*3)
%wavei(dba(t_beam_avg'),[T(1) T(2)*2^ceil(log2(length(t)))/length(t)],beam,cmin-(num_avg-j)*3,cmax-(num_avg-j)*3)

title(tit);
xlabel('Time (s)');
ylabel('Beam (deg)');
drawnow

end





