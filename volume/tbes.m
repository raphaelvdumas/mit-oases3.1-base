m=0;
hf=figure(1);
hold off
x=[0:0.5:100];
y=[1:0.5:100];
ai=sqrt(-1); 
ff=sqrt(1./(2*pi*y)) .* real(exp(ai*(y-(m+0.5)*pi*0.5) )+exp(-ai*(y-(m+0.5)*pi*0.5)) ) ;
b=besselj(m,x);
h=semilogy(x,abs(b),'k');
set(h,'LineWidth',2);
hold on;
h=semilogy(y,abs(ff),'k.');
set(h,'MarkerSize',15);
hx=xlabel('kr');
set(h,'LineWidth',2);
set(hx,'FontSize',14);
hy=ylabel(['J_' num2str(m) '(kr)']);
set(hy,'FontSize',14);
d=b(3:length(b))-ff;
h= semilogy(y,abs(d),'k--');
set(h,'LineWidth',2);
set(gca,'FontSize',14);
grid on;
legend('Exact','Assymptotic','Error');
print -deps2 besj0.eps;

