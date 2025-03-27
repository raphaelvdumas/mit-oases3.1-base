From - Wed Aug 25 09:36:17 1999
Received: from polaris.shore.net (polaris.shore.net [207.244.124.105])
	by keel.mit.edu (8.8.8/8.8.8) with ESMTP id QAA02348
	for <henrik@keel.mit.edu>; Tue, 24 Aug 1999 16:30:11 -0400 (EDT)
Received: from gniqncy-s08-130.port.shore.net (shore.net) [209.192.240.130] 
	by polaris.shore.net with esmtp (Exim)
	for henrik@keel.mit.edu
	id 11JNCs-0003cz-00; Tue, 24 Aug 1999 16:30:11 -0400
Message-ID: <37C2F2AF.A68E502D@shore.net>
Date: Tue, 24 Aug 1999 15:29:51 -0400
From: Satu Schmidt <satu@shore.net>
X-Mailer: Mozilla 4.61 [en] (Win98; I)
X-Accept-Language: en
MIME-Version: 1.0
To: "Schmidt, Henrik" <henrik@keel.mit.edu>
Subject: matlab
Content-Type: multipart/mixed;
 boundary="------------0C3BFF10044C61762E760EF7"
X-Mozilla-Status: 8001

This is a multi-part message in MIME format.
--------------0C3BFF10044C61762E760EF7
Content-Type: text/plain; charset=us-ascii
Content-Transfer-Encoding: 7bit



--------------0C3BFF10044C61762E760EF7
Content-Type: text/plain; charset=us-ascii;
 name="pp_read.m"
Content-Transfer-Encoding: 7bit
Content-Disposition: inline;
 filename="pp_read.m"

%[ts,t,z,r,b,tit,parameter,planes,traces,samples,fs]=pp_read('file')
%
% function to read ascii files output by pp
%
% Kevin D. LePage
% SACLANTCEN
% 14/1/98
%
% INPUTS	
%
% file 		is the name of the Matlab 5 file (.mat)
%
% OUTPUTS
%
% ts		time series (length(t) x (planes x traces))
% t		start times (
% z		depths (vector of corresponding depths)
% r		range (vector of corrsponding ranges)
% b		bearing (vector of corresponding bearings)
% tit 		character string
% parameter	character string (N is normal stress)
% planes	number (for different bearings, for instance)
% traces	number (for depth or range stacks, for instance)
% samples	number (N)
% fs		sample frequency (Hz)

function[ts,t,z,r,b,tit,parameter,planes,traces,samples,fs]=pp_read(file)

%len_name=length(fname);

load(file);
ts=double(DDDD);
t=double(TSHF);
z=double(ZZZZ);
r=double(RRRR);
b=double(AAAA);
fs=1/double(DTDT(1));
samples=size(ts,1);
traces=size(ts,2);
planes=1;
parameter='X';
tit='NO TITLE';



--------------0C3BFF10044C61762E760EF7--


