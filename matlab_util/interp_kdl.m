function[yi]=interp_kdl(x,y,xi,n,ni);

junk=-x(:)*ones(1,length(xi))+ones(length(x),1)*xi(:)';

%junk(find(junk<=0))=5000*ones(size(find(junk<=0)));

[dy,index]=min(abs(junk)); 

delta=diff(x);
delta_y=diff(y);
yi=y(index)+dy'.*delta_y(index)./delta(index)';

