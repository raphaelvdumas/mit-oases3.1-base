clf reset , hold on , grid on

%%%%%%%%%%%%%%%%%%%%%%%%
% Plot experimental data
%%%%%%%%%%%%%%%%%%%%%%%%

load xfcn_r4_60cm.dat
x4 = xfcn_r4_60cm;
load xfcn_r5_60cm.dat
x5 = xfcn_r5_60cm;
load xfcn_r8_60cm.dat
x8 = xfcn_r8_60cm;

% 4 kHz Ricker
i1 = find(x4(:,2) > -900);
plot(x4(i1,1)/1000 , x4(i1,2) , 'rx')
i1 = find(x4(:,3) > -900);
plot(x4(i1,1)/1000 , x4(i1,3) , 'bx')
i1 = find(x4(:,4) > -900);
plot(x4(i1,1)/1000 , x4(i1,4) , 'gx')
i1 = find(x4(:,5) > -900);
plot(x4(i1,1)/1000 , x4(i1,5) , 'mx')

% 5 kHz Ricker
i1 = find(x5(:,2) > -900);
plot(x5(i1,1)/1000 , x5(i1,2) , 'ro')
i1 = find(x5(:,3) > -900);
plot(x5(i1,1)/1000 , x5(i1,3) , 'bo')
i1 = find(x5(:,4) > -900);
plot(x5(i1,1)/1000 , x5(i1,4) , 'go')
i1 = find(x5(:,5) > -900);
plot(x5(i1,1)/1000 , x5(i1,5) , 'mo')

% 8 kHz Ricker
i1 = find(x8(:,2) > -900);
plot(x8(i1,1)/1000 , x8(i1,2) , 'r+')
i1 = find(x8(:,3) > -900);
plot(x8(i1,1)/1000 , x8(i1,3) , 'b+')
i1 = find(x8(:,4) > -900);
plot(x8(i1,1)/1000 , x8(i1,4) , 'g+')
i1 = find(x8(:,5) > -900);
plot(x8(i1,1)/1000 , x8(i1,5) , 'm+')

title('MCG1-97: 30 cm hydrophone')
xlabel('Frequency (kHz)')
ylabel('Frequency Response (dB)')
axis([0 15 -30 10])
set(gca , 'Box' , 'on')
