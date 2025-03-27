load wg_av_8
ib=imresize(mat2gray(dba(image1),[-140 -80]),[256 256]);
i=ib(size(ib,1):-1:1,:);
figure(1)
imshow(i)
h=fspecial('gaussian',16,4);
ii=filter2(h,i);
figure(2)
imshow(ii)
s=qtdecomp(ii,0.01);
figure(3)
x=mat2gray(s,[0 .5]);
imshow(x)
bw=bwperim(x,8);
figure(4)
imshow(bw)
