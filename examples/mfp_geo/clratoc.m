%% clratoc.m:  m-file which sets up the standard ATOC colormap

brt=1.0; sat=1.0;
cmap=[];
for ind=0:63
    hue = 240.0-270.0*ind/64;
    [red,green,blue] = hsb( hue, sat, brt);
    cmap = [cmap; [red, green, blue]];
end
colormap(cmap);

clear brt sat ind hue red green blue
