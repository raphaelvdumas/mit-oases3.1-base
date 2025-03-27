function [red,green,blue] = hsb( hue, sat, brt)

   if sat == 0.0
     red = brt;
     green = brt;
     blue = brt;
     return
   end

   while hue >= 360.
     hue = hue - 360.;
   end
   while hue < 0.0
     hue = hue + 360.;
   end

   hue = hue/60.;
   i = floor( hue);
   f = hue - i;
   p = brt*(1.0 - sat);
   q = brt*(1.0 -sat*f);
   t = brt*(1.0 - sat*(1.0 -f));
   if i == 0.0
     red = brt;
     green = t;
     blue = p;
   elseif i == 1.0
     red = q;
     green = brt;
     blue = p;
   elseif i == 2.0
     red = p;
     green = brt;
     blue = t;
   elseif i == 3.0
     red = p;
     green = q;
     blue = brt;
   elseif i == 4.0
     red = t;
     green = p;
     blue = brt;
   elseif i == 5.0
     red = brt;
     green = p;
     blue = q;
   end
