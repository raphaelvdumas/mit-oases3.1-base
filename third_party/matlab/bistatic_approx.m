for r=10:10:20000,
a=R^2+r^2;
right(r/10,:)=sqrt(a-2*r*R*cos(thet));
approx1(r/10,:)=(sqrt(a)-r*R*cos(thet)/sqrt(a)-(a)^(-3/2)*(r*R*cos(thet)).^2/2);
approx2(r/10,:)=(sqrt(a)-r*R*cos(thet)/sqrt(a));fprintf('\rr=%f',r);
end

% plot stuff

pcolor(x,y,(approx1+r'*ones(1,360)-(right+r'*ones(1,360))));
shading('flat');
colormap('jet');
axis('equal');
colorbar
