function[mov]=make_mov(reverb,T)

j=1,
mat=reshape(reverb(:,j),sqrt(size(reverb,1)),sqrt(size(reverb,1)));
mat=mat.*conj(mat)./(diag(mat)*diag(mat)');
imagesc(real(mat));
eval(['title(''' num2str(T(j)) ' sec'')']);
colormap('jet');
colorbar;
drawnow;

mov=moviein(size(reverb,2));

for j=1:size(reverb,2),
mat=reshape(reverb(:,j),sqrt(size(reverb,1)),sqrt(size(reverb,1)));
mat=mat.*conj(mat)./(diag(mat)*diag(mat)');
imagesc([2:2:196],[2:2:196],real(mat));
%eval(['title(''' num2str(T(j)) ' sec'')']);
eval(['h=text(150,20,''' num2str(T(j)) ' sec'');']);
%set(h,'Color',[0 0 0])
set(h,'FontWeight','bold')  
eval(['h=text(150,27,''' num2str(T(j)) ' sec'');']);
set(h,'Color',[0 0 0])
set(h,'FontWeight','bold')  
colormap('jet');
colorbar;
drawnow;
mov(:,j)=getframe;
end



