load -ascii one.out
one=one(1:2:400^2*2)+i*one(2:2:400^2*2);
one=reshape(one,400,400);
imagesc(real(one));colormap('jet');drawnow
one_f=one;
clear one

load -ascii two.out
two=two(1:2:400^2*2)+i*two(2:2:400^2*2);
two=reshape(two,400,400);
imagesc(real(two));colormap('jet');drawnow
two_f=two;
clear two

load -ascii three.out
three=three(1:2:400^2*2)+i*three(2:2:400^2*2);
three=reshape(three,400,400);
imagesc(real(three));colormap('jet');drawnow
three_f=three;
clear three

load -ascii four.out
four=four(1:2:400^2*2)+i*four(2:2:400^2*2);
four=reshape(four,400,400);
imagesc(real(four));colormap('jet');drawnow
four_f=four;
clear four

load -ascii five.out
five=five(1:2:400^2*2)+i*five(2:2:400^2*2);
five=reshape(five,400,400);
imagesc(real(five));colormap('jet');drawnow
five_f=five;
clear five

load -ascii six.out
six=six(1:400^2);
six=reshape(six,400,400);
imagesc(real(six));colormap('jet');drawnow
six_f=six;
clear six

