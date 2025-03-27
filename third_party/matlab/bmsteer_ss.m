function [bms]=bmsteer_ss

% This provides the steer angles for the MF array
% for the Sicily SCARAB Experiment
% They were chosen to give 3 dB overlap at 3600 Hz
% for Hanning shading
% The convention here is that negative angles are down
bmm=[180 155.285 143.845 134.946 127.265 120.305 113.809 107.626 101.646 95.792];
bm1=-(bmm-90)
bms=[ bm1 0.001 -fliplr(bm1)];
