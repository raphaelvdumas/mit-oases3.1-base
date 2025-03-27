 AAAA = double(AAAA);
 DDDD = double(DDDD);
 DTDT = double(DTDT);
 RRRR = double(RRRR);
 TSHF = double(TSHF);
 ZZZZ = double(ZZZZ);
 for i =1:length(TSHF)
     T(i,:) = [TSHF(i):DTDT(i):TSHF(i)+(-1+size(DDDD,1))*double(DTDT(i))];
 end
 dt = DTDT(1);
 baset = [0:dt:(-1+size(DDDD,1))*dt];
 plotmatrix2(T(1,:),DDDD,'PP\_MATFILE');
% imagesc(baset,ZZZZ,DDDD)
 grid on
