clear
load -ascii fort.50
hold off
r=fort(:,6);
z=fort(:,5);
r=[r' -flipud(r)' r(1)]';
z=[z' flipud(z)' z(1)]';
plot(r,-z)
hold on
rs=fort(:,4);                 
zs=fort(:,3);
plot(rs,-zs,'ro')
plot(-rs,-zs,'ro')
dr=fort(:,8);
dz=fort(:,7);
dr=[dr' -flipud(dr)']';
dz=[dz' flipud(dz)']';
ns=length(dr)
for i=1:ns
x(1)=r(i);          
y(1)=z(i);          
x(2)=x(1)+dr(i)*0.1;
y(2)=y(1)+dz(i)*0.1;
plot(x,-y,'g')   
end
axis('equal');
hold off


