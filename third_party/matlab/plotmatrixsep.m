function plotmatrixsep(x,sep,meanflag,clipflag)
% Script to plot columns of matrix offset by sep
nargin;
[m,n] = size(x);
% generate temp matrix with just means removed...
for iii=1:n
     mmmean = mean(x(:,iii));
     if meanflag==1
       temporary(:,iii) = x(:,iii)-mmmean;
     else
       temporary(:,iii) = x(:,iii);
     end

     if clipflag
       for jjj=1:m
	 if meanflag==1
           if abs(temporary(jjj,iii))>.49*sep
             temporary(jjj,iii) = sign(temporary(jjj,iii))*.49*sep;
           end
         else
           if abs(temporary(jjj,iii)-mmmean)>.49*sep
             temporary(jjj,iii) = ...
	mmmean+sign(temporary(jjj,iii)-mmmean)*.49*sep;
           end
         end
       end
     end
end
for iii=1:n
	temporary(:,iii) = temporary(:,iii) + (iii-1)*sep;
end
plot(temporary);
grid on
clear temporary;
