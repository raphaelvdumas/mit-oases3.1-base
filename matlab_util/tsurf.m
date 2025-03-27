clear
load -ascii fort.49
nz=fort(1)
nt=fort(2)
clear fort
load -ascii fort.51
x=reshape(fort(:,1),nt,nz);
y=-reshape(fort(:,2),nt,nz);
z=-reshape(fort(:,3),nt,nz);
surfl(x,y,z);
axis('equal')
shading('interp')
% colormap('bone')
colormap('copper')
xlabel('x')
