%[low,high,center,number]=third_octave(f_low,f_high)
%
% function to return the low limits, upper limits and center 
% frequencies of the third octave bands whos center frequencies
% lie between f_low and f_high Hertz
%
% Kevin D. LePage
% SACLANTCEN
% 17/4/98
%
% INPUTS:
%
% flow	lower frequency limit of band of interest
% fhigh upper frequency limit of band of interest
%
% OUTPUTS:
%
% low	vector of relevant lower octave band limits (Hz)
% high	vector of relevant upper octave band limits (Hz)
% centervector of relevant octave band center frequencies (Hz)
% numbervector of octave band numbers (12:45 defined here)
function[low,high,center,number]=third_octave(f_low,f_high)

% first define the octave info

low_all=[9 11 14 18 22.4 28 35.5 45 56 71 90 112 140 180 224 280 355 450 560 710 900 1120 1400 1800 2240 2800 3550 4500 5600 7100 9000 11200 14000 18000 22400 28000 35500 45500 56000 71000]';

high_all=[low_all(2:40);89000];

center_all=[10 12.5 16 20 25 31.5 40 50 63 80 100 125 160 200 250 315 400 500 630 800 1000 1250 1600 2000 2500 3150 4000 5000 6300 8000 10000 12500 16000 20000 25000 31500 40000 50000 63000 80000]';

number_all=[10:49]';

if (f_low<10)

f_low_low=10^(ceil(log10(f_low)));

num_3rd_oct_low=10*floor(1-log10(f_low_low));

vec=floor(num_3rd_oct_low/10);

resid=num_3rd_oct_low-vec*10;

values=[1 1.25 1.6 2 2.5 3.15 4 5 6.3 8]';

low=reshape(values*(10.^[-vec+1:0]),10*vec,1);

low=[values(10-resid+1:10)*10^(-vec);low];

center_all=[low;center_all]; 

low_all=[low*2^(-1/6);low_all];

high_all=[low*2^(1/6);high_all];

end

if (f_high>31500)

f_high_high=10^(ceil(log10(f_high)));

num_3rd_oct_high=10*floor(-log10(31500)+log10(f_high_high));

vec=floor(num_3rd_oct_high/10);

resid=num_3rd_oct_high-vec*10;

values=[1 1.25 1.6 2 2.5 3.15 4 5 6.3 8]';

high=reshape(values*(10.^[5:5+vec-1]),10*vec,1);

high=[high;values(1:resid)*10^(5+vec)];

center_all=[center_all;high]; 

low_all=[low_all;high*2^(-1/6)];

high_all=[high_all;high*2^(1/6)];

end

ind=find((center_all>f_low)&(center_all<f_high));

center=center_all(ind);

low=low_all(ind);

high=high_all(ind);





