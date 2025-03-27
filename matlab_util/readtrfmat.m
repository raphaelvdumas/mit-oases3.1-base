% Script readtrfmat.m is a utility that reads a mat file translated
% from an ascii or binary trf (transfer function file in the OASES
% package) by program trftomat.f. 
% This script does to single --> double conversion on all variables, and
% "squeezes" the 6 dimensional data matrix
% to the smallest dimesion possible

% Eddie Scheer, WHOI August,1999


fname = input('E mat file generated from trf by trftomat.f (in quotes): ');
eval(['load ',fname]);
DT = double(DT);
DF = double(DF);
FREQ = double(FREQ);
FCTRF = double(FCTRF);
ICDRIN = double(ICDRIN);
INTTYP = double(INTTYP);
IPARM = double(IPARM);
IR = double(IR);
ISROW = double(ISROW);
LXTRF = double(LXTRF);
MSUFT = double(MSUFT);
MXTRF = double(MXTRF);
NOUT = double(NOUT);
NPLOTS = double(NPLOTS);
NX = double(NX);
OMEGIM = double(OMEGIM);
PROGNM = double(PROGNM);
R0 = double(R0);
RANGE = double(RANGE);
RD = double(RD);
RDC = double(RDC);
RDLOW = double(RDLOW);
RSPACE = double(RSPACE);
SD = double(SD);

CDATA = double(DATA(1,:,:,:,:,:))+sqrt(-1)*double(DATA(2,:,:,:,:,:));
clear DATA
CDATA = squeeze(CDATA);
disp('The resultant output Complex Data variable, CDATA, has dimensions: ')
disp(size(CDATA))
