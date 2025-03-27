Z=exp(i*w*(r^2+(z-50).^2).^(.5)/c)-exp(i*w*(r^2+(z+50).^2).^(.5)/c); 
Z=[real(Z.') imag(Z.')];
save -ascii wave.in Z2


fclose(fid)       
fid=fopen('tl.grid')            
out2=fread(3,'float');     
wavei(-flipud(reshape(out2(4:251003),251,1000)),[0 1],[0 1],-120,-0) 



wavec(-flipud(reshape(out2(4:251003),251,1000)),[0 5000],[-1000 0],-140,-20)
title('Collins 50 m self-starter source')
xlabel('Range (m)')
ylabel('Depth (m)')
title('Collins 50 m self-starter source (50 Hz)')
print -depsc collins_50_50

wavec(-flipud(reshape(out3(4:251003),251,1000))-d),[200 5200],[-1000 0],-140,-20)
title('Test 50 m source (50 Hz)')                
xlabel('Range (m)')
ylabel('Depth (m)')
print -depsc test_50_50   