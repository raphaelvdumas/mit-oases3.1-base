#include "cmap.h"

/* Color Maps based on Matlab's color map functions and other routine */
/* hsv, gray, hot, cool, bone, copper, pink, jet, astro, heat         */

void colorscale_hsv(c,r,g,b)
     int c;
     int *r, *g, *b;
{
  switch(c) {
  case  0: *r=255; *g=  0; *b=  0; break;
  case  1: *r=255; *g= 48; *b=  0; break;
  case  2: *r=255; *g= 96; *b=  0; break;
  case  3: *r=255; *g=143; *b=  0; break;
  case  4: *r=255; *g=191; *b=  0; break;
  case  5: *r=255; *g=239; *b=  0; break;
  case  6: *r=223; *g=255; *b=  0; break;
  case  7: *r=175; *g=255; *b=  0; break;
  case  8: *r=128; *g=255; *b=  0; break;
  case  9: *r= 80; *g=255; *b=  0; break;
  case 10: *r= 32; *g=255; *b=  0; break;
  case 11: *r=  0; *g=255; *b= 16; break;
  case 12: *r=  0; *g=255; *b= 64; break;
  case 13: *r=  0; *g=255; *b=112; break;
  case 14: *r=  0; *g=255; *b=159; break;
  case 15: *r=  0; *g=255; *b=207; break;
  case 16: *r=  0; *g=255; *b=255; break;
  case 17: *r=  0; *g=207; *b=255; break;
  case 18: *r=  0; *g=159; *b=255; break;
  case 19: *r=  0; *g=112; *b=255; break;
  case 20: *r=  0; *g= 64; *b=255; break;
  case 21: *r=  0; *g= 16; *b=255; break;
  case 22: *r= 32; *g=  0; *b=255; break;
  case 23: *r= 80; *g=  0; *b=255; break;
  case 24: *r=128; *g=  0; *b=255; break;
  case 25: *r=175; *g=  0; *b=255; break;
  case 26: *r=223; *g=  0; *b=255; break;
  case 27: *r=255; *g=  0; *b=239; break;
  case 28: *r=255; *g=  0; *b=191; break;
  case 29: *r=255; *g=  0; *b=143; break;
  case 30: *r=255; *g=  0; *b= 96; break;
  case 31: *r=255; *g=  0; *b= 48; break;
  default: *r=255; *g=255; *b=255; break;
  }
}


void colorscale_gray(c,r,g,b)
     int c;
     int *r, *g, *b;
{
  switch(c) {
  case  0: *r=  0; *g=  0; *b=  0; break;
  case  1: *r=  8; *g=  8; *b=  8; break;
  case  2: *r= 16; *g= 16; *b= 16; break;
  case  3: *r= 25; *g= 25; *b= 25; break;
  case  4: *r= 33; *g= 33; *b= 33; break;
  case  5: *r= 41; *g= 41; *b= 41; break;
  case  6: *r= 49; *g= 49; *b= 49; break;
  case  7: *r= 58; *g= 58; *b= 58; break;
  case  8: *r= 66; *g= 66; *b= 66; break;
  case  9: *r= 74; *g= 74; *b= 74; break;
  case 10: *r= 82; *g= 82; *b= 82; break;
  case 11: *r= 90; *g= 90; *b= 90; break;
  case 12: *r= 99; *g= 99; *b= 99; break;
  case 13: *r=107; *g=107; *b=107; break;
  case 14: *r=115; *g=115; *b=115; break;
  case 15: *r=123; *g=123; *b=123; break;
  case 16: *r=132; *g=132; *b=132; break;
  case 17: *r=140; *g=140; *b=140; break;
  case 18: *r=148; *g=148; *b=148; break;
  case 19: *r=156; *g=156; *b=156; break;
  case 20: *r=165; *g=165; *b=165; break;
  case 21: *r=173; *g=173; *b=173; break;
  case 22: *r=181; *g=181; *b=181; break;
  case 23: *r=189; *g=189; *b=189; break;
  case 24: *r=197; *g=197; *b=197; break;
  case 25: *r=206; *g=206; *b=206; break;
  case 26: *r=214; *g=214; *b=214; break;
  case 27: *r=222; *g=222; *b=222; break;
  case 28: *r=230; *g=230; *b=230; break;
  case 29: *r=239; *g=239; *b=239; break;
  case 30: *r=247; *g=247; *b=247; break;
  case 31: *r=255; *g=255; *b=255; break;
  default: *r=255; *g=255; *b=255; break;
  }
}
         

void colorscale_hot(c,r,g,b)
     int c;
     int *r, *g, *b;
{
  switch(c) {
  case  0: *r= 21; *g=  0; *b=  0; break;
  case  1: *r= 43; *g=  0; *b=  0; break;
  case  2: *r= 64; *g=  0; *b=  0; break;
  case  3: *r= 85; *g=  0; *b=  0; break;
  case  4: *r=106; *g=  0; *b=  0; break;
  case  5: *r=128; *g=  0; *b=  0; break;
  case  6: *r=149; *g=  0; *b=  0; break;
  case  7: *r=170; *g=  0; *b=  0; break;
  case  8: *r=191; *g=  0; *b=  0; break;
  case  9: *r=213; *g=  0; *b=  0; break;
  case 10: *r=234; *g=  0; *b=  0; break;
  case 11: *r=255; *g=  0; *b=  0; break;
  case 12: *r=255; *g= 21; *b=  0; break;
  case 13: *r=255; *g= 43; *b=  0; break;
  case 14: *r=255; *g= 64; *b=  0; break;
  case 15: *r=255; *g= 85; *b=  0; break;
  case 16: *r=255; *g=106; *b=  0; break;
  case 17: *r=255; *g=128; *b=  0; break;
  case 18: *r=255; *g=149; *b=  0; break;
  case 19: *r=255; *g=170; *b=  0; break;
  case 20: *r=255; *g=191; *b=  0; break;
  case 21: *r=255; *g=213; *b=  0; break;
  case 22: *r=255; *g=234; *b=  0; break;
  case 23: *r=255; *g=255; *b=  0; break;
  case 24: *r=255; *g=255; *b= 32; break;
  case 25: *r=255; *g=255; *b= 64; break;
  case 26: *r=255; *g=255; *b= 96; break;
  case 27: *r=255; *g=255; *b=128; break;
  case 28: *r=255; *g=255; *b=159; break;
  case 29: *r=255; *g=255; *b=191; break;
  case 30: *r=255; *g=255; *b=223; break;
  case 31: *r=255; *g=255; *b=255; break;
  default: *r=255; *g=255; *b=255; break;
  }
}
         

void colorscale_cool(c,r,g,b)
     int c;
     int *r, *g, *b;
{
  switch(c) {
  case  0: *r=  0; *g=255; *b=255; break;
  case  1: *r=  8; *g=247; *b=255; break;
  case  2: *r= 16; *g=239; *b=255; break;
  case  3: *r= 25; *g=230; *b=255; break;
  case  4: *r= 33; *g=222; *b=255; break;
  case  5: *r= 41; *g=214; *b=255; break;
  case  6: *r= 49; *g=206; *b=255; break;
  case  7: *r= 58; *g=197; *b=255; break;
  case  8: *r= 66; *g=189; *b=255; break;
  case  9: *r= 74; *g=181; *b=255; break;
  case 10: *r= 82; *g=173; *b=255; break;
  case 11: *r= 90; *g=165; *b=255; break;
  case 12: *r= 99; *g=156; *b=255; break;
  case 13: *r=107; *g=148; *b=255; break;
  case 14: *r=115; *g=140; *b=255; break;
  case 15: *r=123; *g=132; *b=255; break;
  case 16: *r=132; *g=123; *b=255; break;
  case 17: *r=140; *g=115; *b=255; break;
  case 18: *r=148; *g=107; *b=255; break;
  case 19: *r=156; *g= 99; *b=255; break;
  case 20: *r=165; *g= 90; *b=255; break;
  case 21: *r=173; *g= 82; *b=255; break;
  case 22: *r=181; *g= 74; *b=255; break;
  case 23: *r=189; *g= 66; *b=255; break;
  case 24: *r=197; *g= 58; *b=255; break;
  case 25: *r=206; *g= 49; *b=255; break;
  case 26: *r=214; *g= 41; *b=255; break;
  case 27: *r=222; *g= 33; *b=255; break;
  case 28: *r=230; *g= 25; *b=255; break;
  case 29: *r=239; *g= 16; *b=255; break;
  case 30: *r=247; *g=  8; *b=255; break;
  case 31: *r=255; *g=  0; *b=255; break;
  default: *r=255; *g=255; *b=255; break;
  }
} 


void colorscale_bone(c,r,g,b)
     int c;
     int *r, *g, *b;
{
  switch(c) {
  case  0: *r=  0; *g=  0; *b=  3; break;
  case  1: *r=  7; *g=  7; *b= 13; break;
  case  2: *r= 14; *g= 14; *b= 22; break;
  case  3: *r= 22; *g= 22; *b= 32; break;
  case  4: *r= 29; *g= 29; *b= 42; break;
  case  5: *r= 36; *g= 36; *b= 52; break;
  case  6: *r= 43; *g= 43; *b= 62; break;
  case  7: *r= 50; *g= 50; *b= 72; break;
  case  8: *r= 58; *g= 58; *b= 81; break;
  case  9: *r= 65; *g= 65; *b= 91; break;
  case 10: *r= 72; *g= 72; *b=101; break;
  case 11: *r= 79; *g= 79; *b=111; break;
  case 12: *r= 86; *g= 89; *b=118; break;
  case 13: *r= 94; *g= 99; *b=125; break;
  case 14: *r=101; *g=109; *b=133; break;
  case 15: *r=108; *g=119; *b=140; break;
  case 16: *r=115; *g=128; *b=147; break;
  case 17: *r=122; *g=138; *b=154; break;
  case 18: *r=130; *g=148; *b=161; break;
  case 19: *r=137; *g=158; *b=169; break;
  case 20: *r=144; *g=168; *b=176; break;
  case 21: *r=151; *g=178; *b=183; break;
  case 22: *r=158; *g=188; *b=190; break;
  case 23: *r=166; *g=197; *b=197; break;
  case 24: *r=177; *g=205; *b=205; break;
  case 25: *r=188; *g=212; *b=212; break;
  case 26: *r=199; *g=219; *b=219; break;
  case 27: *r=210; *g=226; *b=226; break;
  case 28: *r=221; *g=233; *b=233; break;
  case 29: *r=233; *g=241; *b=241; break;
  case 30: *r=244; *g=248; *b=248; break;
  case 31: *r=255; *g=255; *b=255; break;
  default: *r=255; *g=255; *b=255; break;
  }
}
         

void colorscale_copper(c,r,g,b)
     int c;
     int *r, *g, *b;
{
  switch(c) {
  case  0: *r=  0; *g=  0; *b=  0; break;
  case  1: *r= 10; *g=  6; *b=  4; break;
  case  2: *r= 21; *g= 13; *b=  8; break;
  case  3: *r= 31; *g= 19; *b= 12; break;
  case  4: *r= 41; *g= 26; *b= 16; break;
  case  5: *r= 51; *g= 32; *b= 20; break;
  case  6: *r= 62; *g= 39; *b= 25; break;
  case  7: *r= 72; *g= 45; *b= 29; break;
  case  8: *r= 82; *g= 51; *b= 33; break;
  case  9: *r= 93; *g= 58; *b= 37; break;
  case 10: *r=103; *g= 64; *b= 41; break;
  case 11: *r=113; *g= 71; *b= 45; break;
  case 12: *r=123; *g= 77; *b= 49; break;
  case 13: *r=134; *g= 84; *b= 53; break;
  case 14: *r=144; *g= 90; *b= 57; break;
  case 15: *r=154; *g= 96; *b= 61; break;
  case 16: *r=165; *g=103; *b= 65; break;
  case 17: *r=175; *g=109; *b= 70; break;
  case 18: *r=185; *g=116; *b= 74; break;
  case 19: *r=195; *g=122; *b= 78; break;
  case 20: *r=206; *g=129; *b= 82; break;
  case 21: *r=216; *g=135; *b= 86; break;
  case 22: *r=226; *g=141; *b= 90; break;
  case 23: *r=236; *g=148; *b= 94; break;
  case 24: *r=247; *g=154; *b= 98; break;
  case 25: *r=255; *g=161; *b=102; break;
  case 26: *r=255; *g=167; *b=106; break;
  case 27: *r=255; *g=174; *b=110; break;
  case 28: *r=255; *g=180; *b=115; break;
  case 29: *r=255; *g=186; *b=119; break;
  case 30: *r=255; *g=193; *b=123; break;
  case 31: *r=255; *g=199; *b=127; break;
  default: *r=255; *g=255; *b=255; break;
  }
}


void colorscale_pink(c,r,g,b)
     int c;
     int *r, *g, *b;
{
  switch(c) {
  case  0: *r= 43; *g=  0; *b=  0; break;
  case  1: *r= 71; *g= 37; *b= 37; break;
  case  2: *r= 91; *g= 53; *b= 53; break;
  case  3: *r=107; *g= 65; *b= 65; break;
  case  4: *r=121; *g= 75; *b= 75; break;
  case  5: *r=134; *g= 84; *b= 84; break;
  case  6: *r=145; *g= 92; *b= 92; break;
  case  7: *r=156; *g= 99; *b= 99; break;
  case  8: *r=166; *g=106; *b=106; break;
  case  9: *r=175; *g=112; *b=112; break;
  case 10: *r=184; *g=118; *b=118; break;
  case 11: *r=193; *g=124; *b=124; break;
  case 12: *r=196; *g=136; *b=130; break;
  case 13: *r=200; *g=148; *b=135; break;
  case 14: *r=203; *g=158; *b=140; break;
  case 15: *r=207; *g=168; *b=145; break;
  case 16: *r=210; *g=177; *b=150; break;
  case 17: *r=213; *g=186; *b=154; break;
  case 18: *r=216; *g=194; *b=159; break;
  case 19: *r=220; *g=203; *b=163; break;
  case 20: *r=223; *g=210; *b=167; break;
  case 21: *r=226; *g=218; *b=171; break;
  case 22: *r=229; *g=225; *b=175; break;
  case 23: *r=232; *g=232; *b=179; break;
  case 24: *r=235; *g=235; *b=190; break;
  case 25: *r=238; *g=238; *b=201; break;
  case 26: *r=241; *g=241; *b=211; break;
  case 27: *r=244; *g=244; *b=220; break;
  case 28: *r=247; *g=247; *b=230; break;
  case 29: *r=249; *g=249; *b=238; break;
  case 30: *r=252; *g=252; *b=247; break;
  case 31: *r=255; *g=255; *b=255; break;
  default: *r=255; *g=255; *b=255; break;
  }
}
         

void colorscale_jet(c,r,g,b)
     int c;
     int *r, *g, *b;
{
  switch(c) {
  case  0: *r=  0; *g=  0; *b=159; break;
  case  1: *r=  0; *g=  0; *b=191; break;
  case  2: *r=  0; *g=  0; *b=223; break;
  case  3: *r=  0; *g=  0; *b=255; break;
  case  4: *r=  0; *g= 32; *b=255; break;
  case  5: *r=  0; *g= 64; *b=255; break;
  case  6: *r=  0; *g= 96; *b=255; break;
  case  7: *r=  0; *g=128; *b=255; break;
  case  8: *r=  0; *g=159; *b=255; break;
  case  9: *r=  0; *g=191; *b=255; break;
  case 10: *r=  0; *g=223; *b=255; break;
  case 11: *r=  0; *g=255; *b=255; break;
  case 12: *r= 32; *g=255; *b=255; break;
  case 13: *r= 64; *g=255; *b=223; break;
  case 14: *r= 96; *g=255; *b=191; break;
  case 15: *r=128; *g=255; *b=159; break;
  case 16: *r=159; *g=255; *b=128; break;
  case 17: *r=191; *g=255; *b= 96; break;
  case 18: *r=223; *g=255; *b= 64; break;
  case 19: *r=255; *g=255; *b= 32; break;
  case 20: *r=255; *g=255; *b=  0; break;
  case 21: *r=255; *g=223; *b=  0; break;
  case 22: *r=255; *g=191; *b=  0; break;
  case 23: *r=255; *g=159; *b=  0; break;
  case 24: *r=255; *g=128; *b=  0; break;
  case 25: *r=255; *g= 96; *b=  0; break;
  case 26: *r=255; *g= 64; *b=  0; break;
  case 27: *r=255; *g= 32; *b=  0; break;
  case 28: *r=255; *g=  0; *b=  0; break;
  case 29: *r=223; *g=  0; *b=  0; break;
  case 30: *r=191; *g=  0; *b=  0; break;
  case 31: *r=159; *g=  0; *b=  0; break;
  default: *r=255; *g=255; *b=255; break;
  }
}


void colorscale_astro(c,r,g,b)
     int c;
     int *r, *g, *b;
{
  switch(c) {
  case  0:*r=255; *g=255; *b=255;break;
  case  1:*r=246; *g=246; *b=248;break;
  case  2:*r=238; *g=238; *b=241;break;
  case  3:*r=230; *g=230; *b=234;break;
  case  4:*r=222; *g=222; *b=227;break;
  case  5:*r=213; *g=213; *b=220;break;
  case  6:*r=205; *g=205; *b=213;break;
  case  7:*r=197; *g=197; *b=207;break;
  case  8:*r=189; *g=189; *b=200;break;
  case  9:*r=180; *g=180; *b=193;break;
  case 10:*r=172; *g=172; *b=186;break;
  case 11:*r=164; *g=164; *b=179;break;
  case 12:*r=156; *g=156; *b=172;break;
  case 13:*r=148; *g=148; *b=165;break;
  case 14:*r=139; *g=139; *b=159;break;
  case 15:*r=131; *g=131; *b=152;break;
  case 16:*r=123; *g=123; *b=145;break;
  case 17:*r=115; *g=115; *b=138;break;
  case 18:*r=106; *g=106; *b=131;break;
  case 19:*r= 98; *g= 98; *b=124;break;
  case 20:*r= 90; *g= 90; *b=117;break;
  case 21:*r= 82; *g= 82; *b=111;break;
  case 22:*r= 74; *g= 74; *b=104;break;
  case 23:*r= 65; *g= 65; *b= 97;break;
  case 24:*r= 57; *g= 57; *b= 90;break;
  case 25:*r= 49; *g= 49; *b= 83;break;
  case 26:*r= 41; *g= 41; *b= 76;break;
  case 27:*r= 32; *g= 32; *b= 69;break;
  case 28:*r= 24; *g= 24; *b= 63;break;
  case 29:*r= 16; *g= 16; *b= 56;break;
  case 30:*r=  8; *g=  8; *b= 49;break;
  case 31:*r=  0; *g=  0; *b= 42;break;
  default:*r=255; *g=255; *b=255;break;
  }
}


void colorscale_heat(c,r,g,b)
     int c;
     int *r, *g, *b;
{
  switch(c) {
  case  0: *r= 75; *g=  0; *b=  0; break;
  case  1: *r= 87; *g=  0; *b=  0; break;
  case  2: *r= 99; *g=  0; *b=  0; break;
  case  3: *r=111; *g=  0; *b=  0; break;
  case  4: *r=123; *g=  0; *b=  0; break;
  case  5: *r=135; *g=  0; *b=  0; break;
  case  6: *r=147; *g=  0; *b=  0; break;
  case  7: *r=159; *g=  0; *b=  0; break;
  case  8: *r=171; *g= 15; *b=  0; break;
  case  9: *r=183; *g= 31; *b=  0; break;
  case 10: *r=195; *g= 47; *b=  0; break;
  case 11: *r=207; *g= 63; *b=  0; break;
  case 12: *r=219; *g= 79; *b=  0; break;
  case 13: *r=231; *g= 95; *b=  0; break;
  case 14: *r=243; *g=111; *b=  0; break;
  case 15: *r=255; *g=127; *b=  0; break;
  case 16: *r=255; *g=143; *b= 15; break;
  case 17: *r=255; *g=159; *b= 31; break;
  case 18: *r=255; *g=175; *b= 47; break;
  case 19: *r=255; *g=191; *b= 63; break;
  case 20: *r=255; *g=207; *b= 79; break;
  case 21: *r=255; *g=223; *b= 95; break;
  case 22: *r=255; *g=239; *b=111; break;
  case 23: *r=255; *g=255; *b=127; break;
  case 24: *r=255; *g=255; *b=143; break;
  case 25: *r=255; *g=255; *b=159; break;
  case 26: *r=255; *g=255; *b=175; break;
  case 27: *r=255; *g=255; *b=191; break;
  case 28: *r=255; *g=255; *b=207; break;
  case 29: *r=255; *g=255; *b=223; break;
  case 30: *r=255; *g=255; *b=239; break;
  case 31: *r=255; *g=255; *b=255; break;
  default: *r=255; *g=255; *b=255; break; 
  }
}


void colorscale_srb(c,r,g,b)
     int c;
     int *r, *g, *b;
{
  switch(c) {
  case  0: *r=  0; *g=  0; *b=255; break;
  case  1: *r=  0; *g= 33; *b=255; break;
  case  2: *r=  0; *g= 66; *b=255; break;
  case  3: *r=  0; *g= 99; *b=255; break;
  case  4: *r=  0; *g=133; *b=255; break;
  case  5: *r=  0; *g=165; *b=255; break;
  case  6: *r=  0; *g=199; *b=255; break;
  case  7: *r=  0; *g=232; *b=255; break;
  case  8: *r=  0; *g=255; *b=245; break;
  case  9: *r=  0; *g=255; *b=212; break;
  case 10: *r=  0; *g=255; *b=179; break;
  case 11: *r=  0; *g=255; *b=146; break;
  case 12: *r=  0; *g=255; *b=112; break;
  case 13: *r=  0; *g=255; *b= 80; break;
  case 14: *r=  0; *g=255; *b= 46; break;
  case 15: *r=  0; *g=255; *b= 13; break;
  case 16: *r= 20; *g=255; *b=  0; break;
  case 17: *r= 53; *g=255; *b=  0; break;
  case 18: *r= 87; *g=255; *b=  0; break;
  case 19: *r=119; *g=255; *b=  0; break;
  case 20: *r=153; *g=255; *b=  0; break;
  case 21: *r=186; *g=255; *b=  0; break;
  case 22: *r=219; *g=255; *b=  0; break;
  case 23: *r=252; *g=255; *b=  0; break;
  case 24: *r=255; *g=227; *b=  0; break;
  case 25: *r=255; *g=195; *b=  0; break;
  case 26: *r=255; *g=163; *b=  0; break;
  case 27: *r=255; *g=132; *b=  0; break;
  case 28: *r=255; *g=101; *b=  0; break;
  case 29: *r=255; *g= 69; *b=  0; break;
  case 30: *r=255; *g= 38; *b=  0; break;
  case 31: *r=255; *g=  7; *b=  0; break;
  default: *r=255; *g=255; *b=255; break;
  }
}
         

void colorscale_lrb(c,r,g,b)
     int c;
     int *r, *g, *b;
{
  switch(c) {
  case  0: *r=  0; *g=  0; *b=255; break;
  case  1: *r=  0; *g= 49; *b=255; break;
  case  2: *r=  0; *g= 98; *b=255; break;
  case  3: *r=  0; *g=147; *b=255; break;
  case  4: *r=  0; *g=196; *b=255; break;
  case  5: *r=  0; *g=245; *b=255; break;
  case  6: *r=  0; *g=255; *b=216; break;
  case  7: *r=  0; *g=255; *b=167; break;
  case  8: *r=  0; *g=255; *b=117; break;
  case  9: *r=  0; *g=255; *b= 68; break;
  case 10: *r=  0; *g=255; *b= 19; break;
  case 11: *r= 30; *g=255; *b=  0; break;
  case 12: *r= 78; *g=255; *b=  0; break;
  case 13: *r=127; *g=255; *b=  0; break;
  case 14: *r=176; *g=255; *b=  0; break;
  case 15: *r=225; *g=255; *b=  0; break;
  case 16: *r=255; *g=236; *b=  0; break;
  case 17: *r=255; *g=187; *b=  0; break;
  case 18: *r=255; *g=138; *b=  0; break;
  case 19: *r=255; *g= 89; *b=  0; break;
  case 20: *r=255; *g= 40; *b=  0; break;
  case 21: *r=255; *g=  0; *b=  9; break;
  case 22: *r=255; *g=  0; *b= 58; break;
  case 23: *r=255; *g=  0; *b=107; break;
  case 24: *r=255; *g=  0; *b=157; break;
  case 25: *r=255; *g=  0; *b=206; break;
  case 26: *r=255; *g=  0; *b=255; break;
  case 27: *r=255; *g= 49; *b=255; break;
  case 28: *r=255; *g= 98; *b=255; break;
  case 29: *r=255; *g=147; *b=255; break;
  case 30: *r=255; *g=196; *b=255; break;
  case 31: *r=255; *g=245; *b=255; break;
  default: *r=255; *g=255; *b=255; break;
  }
}




