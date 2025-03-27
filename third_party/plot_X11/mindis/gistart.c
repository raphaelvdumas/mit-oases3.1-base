#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <stdio.h>
#include <ctype.h>

/* the bitmap file "theIcon" is a file made from the X11
** program bitmap, which is a general-purpose bitmap editor.
** You can use the bitmap file as presented, or create your
** own. */

#include "theIcon"
#define      BORDER_WIDTH     10
#define      MAX_NUM_COLORS 8 	
#define      ProgName "MINDIS PLOT PACKAGE" 

/*           Programwide Globals                             */

Display      *theDisplay;  /* Which Display                  */
int          theScreen;    /* Which screen on the theDisplay */
int          theDepth;     /* Number of color planes         */
int          xl = 0;       /* remember last position         */
int          yl = 0;       /* remember last position         */
int          chtl;         /* remember last fontheight       */

unsigned long 	xcolors[MAX_NUM_COLORS];   /*Available colors*/
unsigned long   theBlackPixel;           /*System black color*/
unsigned long   theWhitePixel;           /*System white color*/
Window   theWindow;
GC       theGC;
XFontStruct *fontStruct;
char        *fontname;

/* gistart_() - Initalization of X window*/

void gistart_(x,y,width,height)

int          *x,*y;          /* Where the window should go  */
int          *width,*height; /* Size of the window          */

{
/*      printf("gistart");*/
/* 1) Establish a connection to the X server. The connection
**    is asked for on the local server for local theDisplay. */

   theDisplay = XOpenDisplay(NULL); 

/*  Check if the connection was made   */

   if (theDisplay == NULL)
      {
      fprintf(stderr,
"ERROR: Cannot establish a connection to the X server%s\n",
             XDisplayName(NULL));
      exit(1);
      }

/* 2) Check for the default screen and color plane depth.
**    If theDepth == 1, then we have a monochrome system  */

   theScreen = DefaultScreen(theDisplay);
   theDepth  = DefaultDepth(theDisplay,theScreen);
   theBlackPixel = BlackPixel(theDisplay,theScreen);
   theWhitePixel = WhitePixel(theDisplay,theScreen);

/* 3) Get some info on the theDisplay and print it.          */

/*   getXInfo();*/

/* 4) Open a window on the theDisplay.                       */

   theWindow = openWindow(*x,*y,            /* x,y           */
                          *width,*height,   /* width,height  */
                          0,                /* not pop-up    */
                          &theGC);        /* Graphics Context*/

/* 5) Select font for text.                                  */

   chtl = 0;
/*
   fontname = "*-*-medium-r-normal--8-*-*-*-c-*";
   initFont();
*/

} /* function gistart_() */

/* getXInfo prints information to the starting terminal
** (usually an xterm) about the current X Window theDisplay
** and screen. */

getXInfo()
{
/* You will find many funny values when you check the X
** information below. It should be something to the tune of:
** MIT X Consortium (vendor)
** 3 (vendor release)
** X11 R3 (or R2, the Version and Revision numbers)
** But, on most implementations, this information does not
** seem to jibe.
** On the generic MIT X Consortium release for X11R3, you
** might see X11R0, with a vendor release # of 3.  */

   printf("%s version %d of the X Window System, X%d R%d\n",
          ServerVendor(theDisplay),VendorRelease(theDisplay),
          ProtocolVersion(theDisplay),
          ProtocolRevision(theDisplay));
/* Now, print some more detailed information
** about the theDisplay. */

   if (theDepth == 1)
      {
      printf("Color plane depth...........%d (Monochrome)\n",
             theDepth);
      }
   else
      {
      printf("Color plane depth...........%d\n",theDepth);
      }
   printf("Display Width...............%d\n",
          DisplayWidth(theDisplay,theScreen));
   printf("Display Height..............%d\n",
          DisplayHeight(theDisplay,theScreen));
   printf("The theDisplay %s\n",DisplayString(theDisplay));
}    /* function getXInfo    */

/* openWindow
**
** This function takes a x, y pixel location for the upper
** left corner of the window, as well as a width and height
** for the for the size of the window. It opens a window on
** the X theDisplay, and returns the window id. It is also pas-
** sed a flag that specifies whether the window is to be a
** pop-up window (such as a menu) or not. This initial ver-
** sion does not implement that feature.
** The GC theNewGC is a graphics context that is created for
** the window, and returned to the caller. The GC is necessary
** for drawing into the window.
**
** 1) Set up the attributes desired for the window.
** 2) Open a window on the theDisplay.
** 3) Set up an Icon for the window.
** 4) Send "Hints" to the Window Manager.
** 5) Now tell the window manager about the size and loca-
**    tion of the window.
** 6) Create a graphics context for the window.
** 7) Ask X to place the window visibly on the screen.
** 8) Flush out all the queued up X requests to the X
**    server.
**
** Note: With this program, you can either be friendly or
** unfriendly to the window manager, if one is running.
** See the section on override_redirect below. If you set
** override_redirect to True, you are being unfriendly to
** the window manager.
*/

openWindow(x,y,width,height,flag,theNewGC)

int          x,y;          /* Where the window should go  */
int          width,height; /* Size of the window          */
int          flag;         /* if > 0 then the window is
                              a pop up                    */
GC           *theNewGC;    /* Returned Graphics Context   */

{                          /* Function openWindow         */
   XSetWindowAttributes  theWindowAttributes;
   XSizeHints            theSizeHints;
   unsigned  long        theWindowMask;
   Window                theNewWindow;
   Pixmap                theIconPixmap;
   XWMHints              theWMHints;

/* 1) Set up the attributes desired for the window.
** Note that window managers may deny us some of these
** resources. Note also that setting override_redirect
** to True will tell the window manager to leave our
** window alone. Try this program by setting
** override_redirect to False.
*/

/* theWindowAttributes.override_redirect = True; */
/* JooThiam Goh 31 March 1994 - Added event_mask and backing_store
/* attributes so that overlapping windows will not kill MINDIS window */

   theWindowAttributes.border_pixel = theBlackPixel;
   theWindowAttributes.background_pixel = theWhitePixel;
   theWindowAttributes.override_redirect = False; 
   theWindowAttributes.event_mask = ExposureMask | KeyPressMask |
      StructureNotifyMask | ButtonPressMask;
   theWindowAttributes.backing_store = Always; 
   theWindowMask = CWBackingStore | CWBackPixel | CWEventMask |
       CWBorderPixel | CWOverrideRedirect;

/*
** 2) Open a window on the theDisplay.
*/

   theNewWindow =XCreateWindow(theDisplay,
        RootWindow(theDisplay,theScreen),
        x,y,width,height,BORDER_WIDTH,theDepth,
        InputOutput,CopyFromParent,theWindowMask,
        &theWindowAttributes);

/* 3) Set up an icon for the window. Each window should
**    also register an icon with the window manager.
**    This icon is used if the window is shrunk down to
**    an iconic form by the user (through interaction with
**    the window manager). */

/* JooThiam Goh 8 Jan 94. The icon is changed to the Mensetmanus
   bitmap supplied with the standard X-distribution */

   theIconPixmap = XCreateBitmapFromData(theDisplay,
                   theNewWindow,theIcon_bits,
                   theIcon_width,theIcon_height);

/* 4) Send "Hints" to the Window Manager. Before this
**    window will appear on the theDisplay, an X window
**    manager may intercept the call and place the window
**    where it wants to. This next section tells the win-
**    dow manager "hints" as to where the window should go.
*/
   theWMHints.icon_pixmap   = theIconPixmap;
   theWMHints.initial_state = NormalState;
   theWMHints.flags         = IconPixmapHint|StateHint;

   XSetWMHints(theDisplay,theNewWindow,&theWMHints);
/*
** 5) Now tell the window manager about the size and loca-
**    tion of the window
*/

   theSizeHints.flags   = PPosition|PSize;
   theSizeHints.x       = x;
   theSizeHints.y       = y;
   theSizeHints.width   = width;
   theSizeHints.height  = height;
   XSetNormalHints(theDisplay,theNewWindow,&theSizeHints);

/* 6) Create a graphics context for the window.   */

   if (createGC(theNewWindow,theNewGC) == 0)
      {
      XDestroyWindow(theDisplay,theNewWindow);
      return((Window) 0);
      }
 
/*
** 7) Ask X to place the window visibly on the screen.
**    Up to now, the window has been created but has not
**    appeared on the screen. Mapping the window places
**    it visibly on the screen.
*/
   XMapWindow(theDisplay,theNewWindow);

/* Added the name "MINDIS PLOT PACKAGE" to the window - 
** JooThiam Goh Jan 8 1994 */

   XStoreName(theDisplay,theNewWindow,ProgName);
/*
**
** 8) Determine the type of theDisplay and then assciate fipplot color
**    scheme with the available colors
*/
   map_colors();
/*
** 9) Flush out all the queued up X requests to the X
**    server.
*/
/* JooThiam Goh 8 Jan 1994 - sleep(2) is necessary for slower or remote
   workstations to avoid missing drawing the axes */

   sleep(2);
   XFlush(theDisplay);
/*
** 10) Return the window ID, which is needed to specify
**    which window to draw to.
*/

   return(theNewWindow);

}                          /* End of function openWindow  */



/* createGC creates a graphics context for the given window.
** A graphics context is necessary to draw into the window.
**
** Returns 0 if there was an error, 1 if all is OK.       */

createGC(theNewWindow,theNewGC)

Window     theNewWindow;
GC         *theNewGC;

{                          /* function createGC           */
   XGCValues      theGCValues;
   *theNewGC = XCreateGC(theDisplay,theNewWindow,
                         (unsigned long) 0,&theGCValues);
   if (*theNewGC == 0)     /* unable to create a GC       */
      {
      return(0);            /* Error  */
      }
   else
      {
   /* Set foregound and background defaults for the new GC*/
      XSetForeground(theDisplay,*theNewGC,theBlackPixel);
      XSetBackground(theDisplay,*theNewGC,theWhitePixel);
      return(1);            /* OK  */
      }
} /* function createGC           */

/* initFont()
** Initializes a font for drawing text in for a given gra-
** phics context. If you just use this font for a given GC,
** then you no longer have to worry about the font.       */

initFont()

{        /* function initFont   */

/*   XFontStruct *fntStrct; */
   fontStruct = XLoadQueryFont(theDisplay,fontname);
   if (fontStruct != 0)
      {
      XSetFont(theDisplay,theGC,fontStruct->fid);
      }
   else
      {
      printf("Could not load Font\n");
      }

/*   fntStrct = fontStruct; */
/*    return(fntStrct); */
}        /* function initFont   */

/* quitx_() - closes the connection to the X server        */

void quitx_()

{     /* Function quitx              */
/*   sleep(3);*/
   XDestroyWindow(theDisplay,theWindow);
/*
   XFreeFont(theDisplay,fontStruct);
*/
   XCloseDisplay(theDisplay);
}     /* End of function quitx       */
/* drawLine
** Draws a line from (x1,y1) to (x2,y2) in the window
** theWindow, using the graphics context theGC.           */

void drawline_(xt,yt,act)
   int        *xt,*yt;        /* Ending location          */
   int        *act;   /* 2 => draw, 3 => move */
{  /* function drawLine   */
   if (*act == 2)
     {
     XDrawLine(theDisplay,theWindow,theGC,xl,yl,*xt,*yt);
     XFlush(theDisplay);
      }
   xl = *xt;    /* Remeber last location        */
   yl = *yt;

}  /* function drawLine   */
/* write one character on screen */
void wrchar_(x,y,a,cheight)
   int        *x,*y;  /*lower left location of characterbox*/
   char       *a;   /*character to be written on the screen*/
   int        *cheight; /*characterbox height*/
{  /* function wrchar_ */
/*   int i;*/
/*      int        h;*/
/*       h = fontStruct->ascent;*//*+ fontStruct->descent;*/
/*       printf("characterbox height as realized = %d\n",h);*/
/*  Select font for text.                 */
   if (*cheight != chtl)
    {
/*    printf("chtl = %d\n",chtl);
    printf("cheight = %d\n",*cheight);*/
    chtl = *cheight;
    if (chtl<=8)
     fontname = "*-fixed-medium-r-normal--8-*-*-*-c-*";
    else {
     if (chtl==9)
      fontname = "*-fixed-medium-r-normal--9-*-*-*-c-*";
     else {
      if ((chtl>=10)&&(chtl<=12))
       fontname = "*-fixed-medium-r-normal--10-*-*-*-c-*";
      else {
       if ((chtl==13)||(chtl==14))
        fontname = "*-fixed-medium-r-normal--13-*-*-*-c-*";
       else {
        if ((chtl>=15)&&(chtl<=19))
         fontname = "*-fixed-medium-r-normal--15-*-*-*-c-*";
        else {
         if ((chtl>=20)&&(chtl<=23))
          fontname = "*-fixed-medium-r-normal--20-*-*-*-c-*";
         else
          fontname = "*-fixed-medium-r-normal--24-*-*-*-c-*";
             }
            }
           }
          }
         }
/*    printf("fontname = %s\n",fontname);*/
    initFont();
    }

/*"variable";*/
   XDrawImageString(theDisplay,theWindow,theGC,*x,*y,a,1);
/* Send output tothe screen */
   XFlush(theDisplay);
}  /* function wrchar_ */

/*This program will establish the forground color of the theDisplay
*/

void lxcol_(ipen)
int *ipen;
{
XSetForeground(theDisplay,theGC , xcolors[*ipen]);
}

map_colors()
{
	int depth; /*depth of screen*/
	XVisualInfo visual_info,*visual_list;
	Visual *def_visual;
	XColor	ncolors;
	Colormap def_colormap;
	int i=6,step,num;
	char	grayscale[7];

/* The following color scheme is used by the fipplot program */
	static char *color_scheme[] = { "White","Red","Green","Blue","Cyan",
					"Magenta","Yellow","Black"}; 
/* The color scheme below looks better for a white background screen. */
/*	static char *color_scheme[] = { "White","Red","Blue","Black","Cyan",
					"Magenta","Green","Yellow"};*/
	static char *visuals[] = { "StaticGray","GrayScale","StaticColor","PseudoColor",
					"TrueColor","DirectColor"};

	depth = DefaultDepth(theDisplay,theScreen);
	def_colormap = DefaultColormap(theDisplay, theScreen);
	def_visual = DefaultVisual(theDisplay,theScreen);
	theBlackPixel = BlackPixel(theDisplay,theScreen);
	theWhitePixel = WhitePixel(theDisplay,theScreen);

/*
* 	If depth == 1 the screen is only one plane deep thus the screen must 
*	be black and white.  The current version of fipplot only uses eight colors. 
*	There for we will make xcolor(0) = white and xcolor(1-7) = black.
*
*/
	if (depth == 1) {
		xcolors[0] = theWhitePixel;
		for(i=0;i<=MAX_NUM_COLORS;i++)
			xcolors[i] = theBlackPixel;
		return;
  		}

/*
*	By using the XGetVisualInfo command we will be able to determine the number
*	of visuals which match the screen.  If the screen is DirectColor then num
*	will be 6.  Therefore we could use a DirectColor visual or any other visual.
*	If num == 2 then the visual used must be either GrayScale or StaticGray.
*
*/

	visual_info.depth = depth;
	visual_info.screen = theScreen;
	visual_list = XGetVisualInfo (theDisplay,VisualScreenMask|VisualDepthMask,
			 &visual_info,&num );

/*
*	The XMatchVisualInfo command will find the highest appropriate visual which
*	matches the screen.  Some grayscale monitors will match with PseudoColor if
*	the monitor tries to map colors to the nearest grayscale.  Then next if {}
*	will test for this and allow only a grayscale colormap.
*
*/

	while (!XMatchVisualInfo(theDisplay,theScreen,depth,--i,&visual_info));


/*
*	If i <= 2 which means the visual is either GrayScale or StaticGray or
*	if num = 1 which is the number of matches returned from the XGetVisualInfo 
*	command then we will set up a grayscale colormap.  There are some X terms
*	which are 4 plane gray scale which return a PseudoColor visual match from 
*	the XMatchVisualInfo command but return num = 1 from the XGetVisualInfo 
*	command.  If there is only 1 match to the XGetVisualInfo command then the 
*	visual must be StaticGray or GrayScale.
*
*/
	if ((i <= 2)|(num==1)){ /*classes 0 & 1 are "StaticGray" and "GrayScale" */
		/* setting up the max number of gray scale colors */
		printf (" The visual is .... GrayScale \n");
		xcolors[MAX_NUM_COLORS] = theBlackPixel;
	     	for (i=0;i<MAX_NUM_COLORS-1;i++){
			step = 90/(MAX_NUM_COLORS -1); 
			sprintf(grayscale,"Gray%d",100-(i*step));
		     	if(!XAllocNamedColor(theDisplay,def_colormap, grayscale,
				   &ncolors,&ncolors))
		   		printf(" Error allocating RGB for %s \n",grayscale);
			xcolors[i] = ncolors.pixel;
			}
	     return;
	     }

	/* setting up the rainbow of colors will be slightly different 
	   The following will keep the color scheme of fipplot in tack */

	printf (" The visual is %s \n", visuals[visual_info.class]);
	xcolors[0] = theWhitePixel;
	xcolors[MAX_NUM_COLORS-1] = theBlackPixel;
	for (i=1;i<MAX_NUM_COLORS-1;i++){
	     	if(!XAllocNamedColor(theDisplay,def_colormap, color_scheme[i],
				   &ncolors,&ncolors))
		   	printf(" Error allocating colors in colormap \n");
	    	xcolors[i] = ncolors.pixel;	
		}
	}
