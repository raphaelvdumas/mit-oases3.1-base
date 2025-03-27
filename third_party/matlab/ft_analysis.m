From - Wed Apr  7 11:03:29 1999
Received: from frosty3.whoi.edu (escheer@frosty3.whoi.edu [128.128.92.17])
	by keel.mit.edu (8.8.8/8.8.8) with ESMTP id KAA26904
	for <henrik@keel.mit.edu>; Wed, 7 Apr 1999 10:22:39 -0400 (EDT)
Received: (from escheer@localhost)
	by frosty3.whoi.edu (8.8.7/8.8.7) id KAA02631;
	Wed, 7 Apr 1999 10:21:26 -0400
Date: Wed, 7 Apr 1999 10:21:26 -0400
From: edward k scheer <escheer@frosty3.whoi.edu>
Message-Id: <199904071421.KAA02631@frosty3.whoi.edu>
To: henrik@keel.mit.edu
Subject: script to plot spectrum of tr1.mat data...
Cc: escheer@frosty3.whoi.edu
X-Mozilla-Status: 8001


I plotted an output on lp1 for you.

Here's the script with notes:

%EKS SCRIPT TO DO SPECTRUM ON DATA IN HS

load tr1.mat

%diff(tim2) gets me a vector of first order differences in the time vector.
% They should be constant. Mean of this is lazy way to get that constant.

deltat = mean(diff(tim2))

fs = 1/deltat;

%tmin is starting time
tmin = tim2(1);

%Here's the format of the specgram function:
%[B,F,T] = SPECGRAM(A,NFFT,Fs,WINDOW,NOVERLAP)

%Note that the closer NOVERLAP is to the window length (while still
%remaining smaller), the finer the time spacing will be on the
%output spectrum, and the larger the dimensions of the output.

[S,F,T] = specgram(tr1,256,fs,256,240);

%I'M ADDING TMIN TO T TO GET CORRECT START TIME....

imagesc(T+tmin,F,10*log10(abs(S)))

%IMAGESC TURNS Y AXIS UPSIDE DOWN. THIS TURNS IT BACK...

set(gca,'ydir','normal')

colorbar
xlabel('Time(secs)')
ylabel('Freq(Hz)')
title('Specgram for Henrik Schmidt')
%print -dpsc -Plp1



