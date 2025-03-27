%[data,fs]=abel_reader(fname,chan,ping)
%
% Kevin D. LePage
% SACLANTCEN
% 5/20/98
%
% INPUTS
%
% fname		filename
% chan		chan vector (assumed to overlap number of cards if
%               larger than noCard)
% ping		ping vector
%
% OUTPUTS
%
%
function[data,fs]=abel_reader(fname,chan,ping)

fid=fopen(fname,'r','b');

%[a,file_size]=fread(fid);

%fclose(fid);

fid=fopen(fname,'r','b');

%fid=fopen(fname,'r');

    id=fread(fid,12);

% Cruise info 
    cname=setstr(fread(fid,64))';   % Cruise name 
    sic=setstr(fread(fid,64))';     % Scientist in charge 
    fdesc=setstr(fread(fid,256))';  % Description of file, e.g. track number 
    site=setstr(fread(fid,64))';    % Experiment site  
    fname=setstr(fread(fid,64))';   % File name 
    pdesc=setstr(fread(fid,256))';  % System
%
% Acquisition data 
    sfreq=fread(fid,1,'float'); % Sampling frequency
    fs=sfreq;
    fprintf('sample frequency is %d\n',fs)
    delay=fread(fid,1,'int');   % Delay in points 
    llength=fread(fid,1,'int'); % Acquisition length (in points pr. channel) 
    fprintf('length of each ping is %d\n',llength)
    pings=fread(fid,1,'int');   % Number of pings (Might be less)
    fprintf('maximum number of pings is %d\n',pings)
    samp=fread(fid,1,'int');    % 1 = Normal sampling, 2 = Quadrature sampling 
    fprintf('sampling is %d, (1 normal, 2 quadrature)\n',samp)
    noBits=fread(fid,1,'int');
    noCard=fread(fid,1,'int');
    totChan=fread(fid,1,'int');
    fprintf('total number of channels is %d\n',totChan)
    noChan=fread(fid,16,'int');

%
% Topas transmitter data

    level=fread(fid,1,'short');
    interval=fread(fid,1,'short');
    pulsform=fread(fid,1,'short');
    HRP_comp=fread(fid,1,'short');
    sec_freq=fread(fid,1,'short');
    chirp_start=fread(fid,1,'short');
    chirp_stop=fread(fid,1,'short');
    chirp_length=fread(fid,1,'short');
    chirp_type=fread(fid,1,'short');
    beam_dir=fread(fid,1,'short');
    scan_sec=fread(fid,1,'short');
    scan_step=fread(fid,1,'short');
    beam_pat=fread(fid,1,'short');
    top_dummy=fread(fid,3,'short');
%
% Swath transmitter data

    gain_st=fread(fid,1,'int');
    gain_sl=fread(fid,1,'int');
    pulse_corr=fread(fid,1,'int');

%
% Other transmitter data

    trans=fread(fid,64,'int');

% Processing data 

    process=fread(fid,1,'int');
    prodesc=setstr(fread(fid,256))';  % Verbal description of post processing 

for j=1:totChan

    no(j)=fread(fid,1,'int');
    freq(j)=fread(fid,1,'float');
    gain(j)=fread(fid,1,'float');
    sens(j)=fread(fid,1,'float');
    hydtype(j)=fread(fid,1,'int');
    range(j)=fread(fid,1,'float');
    desc=setstr(fread(fid,256))';

end

%endy=(file_size-12-1424-328-280*totChan)/(368+noBits/8*totChan*llength);

data=zeros(length(chan),length(ping)*llength);

for j=1:length(ping)

fseek(fid,12+1424+280*totChan+(ping(j)-1)*(368+round(noBits/8)*totChan*llength),-1);

%
% read ping header

    year=fread(fid,1,'short');
    month=fread(fid,1,'short');
    day=fread(fid,1,'short');
    hour=fread(fid,1,'short');
    min=fread(fid,1,'short');
    sec=fread(fid,1,'short');
    msec=fread(fid,1,'short');

    dgps=fread(fid,1,'short');
    lat_north=fread(fid,1,'double');
    lon_east=fread(fid,1,'double');
    zone_lon=fread(fid,1,'double');
    zone_no=fread(fid,1,'int');
    heading=fread(fid,1,'float');
    speed=fread(fid,1,'float');
    water_depth=fread(fid,1,'float');

    f1_head=fread(fid,1,'float');
    f1_xpos=fread(fid,1,'float');
    f1_ypos=fread(fid,1,'float');

    f2_head=fread(fid,1,'float');
    f2_xpos=fread(fid,1,'float');
    f2_ypos=fread(fid,1,'float');

    tx_heave=fread(fid,1,'float');
    tx_roll=fread(fid,1,'float');
    tx_pitch=fread(fid,1,'float');

    rx_heave=fread(fid,1,'float');
    rx_roll=fread(fid,1,'float');
    rx_pitch=fread(fid,1,'float');

    angle=fread(fid,1,'float');
    vari=fread(fid,64,'float');

    other=fread(fid,4);

    if j==1

    temp=zeros(noChan(1)*noCard,llength);

    end

    for jj=1:noCard

    temp((jj-1)*noChan(1)+1:jj*noChan(1),:)=reshape(fread(fid,llength*noChan(1),'short'),noChan(1),llength);

    end

    if (max(diff(gain(chan)))>0)

    data(:,(j-1)*llength+1:j*llength)=temp(chan,:)./(10.^(gain(chan)'/20)*ones(1,llength));

    else

    data(:,(j-1)*llength+1:j*llength)=temp(chan,:)/10^(gain(1)/20);

    end

end

fseek(fid,12+1424+280*totChan+(100)*(368+round(noBits/8)*totChan*llength),-1);

%
% adamtail

    check=fread(fid,1,'short');
    fprintf('NOTE check=%d, should be = -9510 (=HHHHDADA in hex, which is "chksum")\n',check)
    suc=fread(fid,1,'short');
    fprintf('NOTE suc=%d, should be = 0\n',suc)
    vari=fread(fid,1,'int');
    desc=setstr(fread(fid,256))';
    desc_file=setstr(fread(fid,64))';

fclose(fid);












