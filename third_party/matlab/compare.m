figure(1)
subplot(221)
imagesc(real(one));colormap('jet');colorbar
title('real one')
subplot(222)
imagesc(imag(one));colormap('jet');colorbar
title('imag one')
subplot(223)
imagesc(real(one-one_f.'));colormap('jet');colorbar
title('-real one_f')
subplot(224)
imagesc(imag(one-one_f.'));colormap('jet');colorbar
title('-imag one_f')

figure(2)
subplot(221)
imagesc(real(two));colormap('jet');colorbar
title('real two')
subplot(222)
imagesc(imag(two));colormap('jet');colorbar
title('imag two')
subplot(223)
imagesc(real(two-two_f.'));colormap('jet');colorbar
title('-real two_f')
subplot(224)
imagesc(imag(two-two_f.'));colormap('jet');colorbar
title('-imag two_f')

figure(3)
subplot(221)
imagesc(real(three));colormap('jet');colorbar
title('real three')
subplot(222)
imagesc(imag(three));colormap('jet');colorbar
title('imag three')
subplot(223)
imagesc(real(three-three_f.'));colormap('jet');colorbar
title('-real three_f')
subplot(224)
imagesc(imag(three-three_f.'));colormap('jet');colorbar
title('-imag three_f')

figure(4)
subplot(221)
imagesc(real(four));colormap('jet');colorbar
title('real four')
subplot(222)
imagesc(imag(four));colormap('jet');colorbar
title('imag four')
subplot(223)
imagesc(real(four-four_f.'));colormap('jet');colorbar
title('-real four_f')
subplot(224)
imagesc(imag(four-four_f.'));colormap('jet');colorbar
title('-imag four_f')

figure(5)
subplot(221)
imagesc(real(five));colormap('jet');colorbar
title('real five')
subplot(222)
imagesc(imag(five));colormap('jet');colorbar
title('imag five')
subplot(223)
imagesc(real(five-five_f.'));colormap('jet');colorbar
title('-real five_f')
subplot(224)
imagesc(imag(five-five_f.'));colormap('jet');colorbar
title('-imag five_f')

figure(6)
subplot(211)
imagesc(real(six));colormap('jet');colorbar
title('real six')
subplot(212)
imagesc(real(six-six_f'));colormap('jet');colorbar
title('-real six_f')


