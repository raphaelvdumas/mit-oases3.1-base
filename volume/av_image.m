cmin=-140
cmax=-80

n=1
[image1,tim,beam] = tbp('wg_1');
image= image1.*image1;
image1=sqrt(image/n);
[a,b]= plimage(image1,tim,beam,cmin,cmax,'n=1')

n=n+1
[image1,tim,beam] = tbp('wg_2');
image= image + image1.*image1;
image1=sqrt(image/n);
[a,b]= plimage(image1,tim,beam,cmin,cmax,'n=2')

n=n+1
[image1,tim,beam] = tbp('wg_3');
image= image + image1.*image1;
image1=sqrt(image/n);
[a,b]= plimage(image1,tim,beam,cmin,cmax,'n=3')

n=n+1
[image1,tim,beam] = tbp('wg_4');
image= image + image1.*image1;
image1=sqrt(image/n);
[a,b]= plimage(image1,tim,beam,cmin,cmax,'n=4')

n=n+1
[image1,tim,beam] = tbp('wg_5');
image= image + image1.*image1;
image1=sqrt(image/n);
[a,b]= plimage(image1,tim,beam,cmin,cmax,'n=5')

n=n+1
[image1,tim,beam] = tbp('wg_6');
image= image + image1.*image1;
image1=sqrt(image/n);
[a,b]= plimage(image1,tim,beam,cmin,cmax,'n=6')

n=n+1
[image1,tim,beam] = tbp('wg_7');
image= image + image1.*image1;
image1=sqrt(image/n);
[a,b]= plimage(image1,tim,beam,cmin,cmax,'n=7')

n=n+1
[image1,tim,beam] = tbp('wg_8');
image= image + image1.*image1;
image1=sqrt(image/n);
[a,b]= plimage(image1,tim,beam,cmin,cmax,'n=8')

save image image1 tim beam






