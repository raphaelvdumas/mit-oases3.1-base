#!/bin/sh

# 
# This script tests the PLOTMTV program, and also serves to display some
# of its capabilities.
#

# program location
PLOTMTV="../Bin/plotmtv"

# Use this to test color postscript
#PLOTMTV="../Bin/plotmtv -scale 0.5 -colorps"

# Use this to test 3D postscript and the scale factor
#PLOTMTV="../Bin/plotmtv -3D -scale 0.8"

# Use this to generate lots of postscript files (without prompting)
#PLOTMTV="../Bin/plotmtv -noxplot -print"

# do each test-case one-by-one, prompting for input
ALLPLOT=0

#
# Continue/quit
# This is a variant of "do_continue" which does not have the skip option.
#
continue_intro()
{
	INPUT=

	# Prompt for and read input
	echo -n "Press 'q' to quit, or any other character to continue :" 
	read INPUT
	if [ ! "$INPUT" ] ; then
		INPUT="help"
	fi

	# Branch according to character
	if [ $INPUT = "q" ] ; then
		echo ""
		echo "Done!"
		exit
	fi
}

#
# Run an input file
#
run_script()
{
	echo "% $PLOTMTV $DATAFILE"
	do_continue
	if [ $SKIP -eq 0 ]; then
		$PLOTMTV $DATAFILE
		echo ""
	fi
}

#
# Continue/quit
#
do_continue()
{
	INPUT=
	SKIP=0

	# Check ALLPLOT option
	if [ $ALLPLOT -eq 1 ] ; then
		echo ""
		return
	fi

	# Prompt for and read input
	echo -n "Press 'q' to quit, 'n' to skip, any other character to continue :" 
	read INPUT
	if [ ! "$INPUT" ] ; then
		INPUT="help"
	fi

	# Branch according to character
	if [ $INPUT = "n" ] ; then
		echo ""
		SKIP=1

	elif [ $INPUT = "q" ] ; then
		echo ""
		echo "Done!"
		exit
	fi
}

#
# Run an input file
#
run_script()
{
	echo "% $PLOTMTV $DATAFILE"
	do_continue
	if [ $SKIP -eq 0 ]; then
		$PLOTMTV $DATAFILE
		echo ""
	fi
}

#
# Run an input file
#
run_mult_script()
{
	echo "% $PLOTMTV $DATAFILE"
	do_continue
	if [ $SKIP -eq 0 ]; then
		$PLOTMTV -l -geom 1140x850+0+0 -comb $DATAFILE
		echo ""
	fi
}

#
# Run an input file
#
run_3d_script()
{
	echo "% $PLOTMTV -3d $DATAFILE"
	do_continue
	if [ $SKIP -eq 0 ]; then
		$PLOTMTV -3d $DATAFILE
		echo ""
	fi
}

#
# Run an input file
#
run_all_script()
{
	echo "% $PLOTMTV -plotall $DATAFILE"
	do_continue
	if [ $SKIP -eq 0 ]; then
		$PLOTMTV -plotall $DATAFILE
		echo ""
	fi
}

#
# Run an input file using a different colormap
#
run_clr_script()
{
	echo "% setenv MTV_WRB_COLORMAP=on"
	echo "% $PLOTMTV $DATAFILE"
	do_continue
	if [ $SKIP -eq 0 ]; then
		MTV_WRB_COLORMAP=on $PLOTMTV $DATAFILE
		echo ""
	fi
}

# Scan arguments
for i
do case $i in
	-all)	echo "Doing all plots..."
		ALLPLOT=1;;
	esac
done

#
# Introduction
#
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo "PLOTMTV has a limited but functional Graphical User Interface."
echo ""
echo "The PLOTMTV window consists of several buttons at the top of the"
echo "window, and 2 arrow buttons on the lower left corner of the window."
echo "The arrow buttons appear only when there is more than 1 plot to be"
echo "drawn, and these arrow buttons are used to page between plots."
echo ""
echo "Keyboard buttons can also be used to page between plots:"
echo "         'n'  goes to the next plot"
echo "         'p'  goes to the previous plot"
echo ""
echo ""
echo ""
echo ""
continue_intro
echo ""
echo ""
echo ""
echo "The GUI functionality is as follows:"
echo ""
echo "   Zoom"
echo "      A single button-click inside the plot zooms in and centers"
echo "         around the clicked location."
echo "      Drawing a rectangle using the mouse causes the plot to be"
echo "         redrawn to that boundary."
echo "      Clicking on the FULL ZOOM button redraws the plot with the"
echo "         original (full) boundaries."
echo "      Clicking on UNZOOM zooms out, while ZOOM zooms in."
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
continue_intro
echo ""
echo ""
echo ""
echo "   Misc"
echo "      The log/linear buttons redraw the plot in linear or "
echo "         logarithmic scales."
echo "      Clicking on the 2D/3D button toggles between a 2D and 3D plot."
echo "      In 3D, pressing the following keyboard keys have these actions:"
echo "         'h'  rotates the view right by 5 degrees"
echo "         'j'  rotates the view down by 5 degrees"
echo "         'k'  rotates the view up by 5 degrees"
echo "         'l'  rotates the view left by 5 degrees"
echo "         'a'  rotates the view right by 90 degrees"
echo "         's'  rotates the view down by 90 degrees"
echo "         'd'  rotates the view up by 90 degrees"
echo "         'f'  rotates the view left by 90 degrees"
echo "         'o'  returns the plot to the original view vector"
echo "         'x'  shows the plot projected on the y-z plane (x=const)"
echo "         'y'  shows the plot projected on the x-z plane (y=const)"
echo "         'z'  shows the plot projected on the x-y plane (z=const)"
echo ""
echo "   Quit"
echo "      To quit the X plot, press 'q'/'Q' in the window, or click on the"
echo "      QUIT button."
echo ""
continue_intro

#
# Basic Line/fill types
#

echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo "PLOTMTV has lots of different linetypes, colors, and fills"
echo ""

# Test fill colors
echo "Testing fill colors..."
DATAFILE=test_fillclrs.mtv
run_script

echo ""
echo ""
echo ""

# Test line-types 
echo "Testing line types..."
DATAFILE=test_lines.mtv
run_script

echo ""
echo ""
echo ""

# Test markers-types 
echo "Testing marker types..."
DATAFILE=test_markers.mtv
run_script

echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo "PLOTMTV also allows you to place text and other annotations"
echo "in the plot"
echo ""

# Test annotations
echo "Testing annotations..."
DATAFILE=test_annot.mtv
run_script

#
# Test 2D Curves
#
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo "This next example shows a few simple curves in 2D"
echo ""
echo "Testing 2D curves..."
DATAFILE=test_curve2D.mtv
run_script

#
# Test 3D curves
#
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo "It is also possible to draw curves in 3D"
echo ""
echo "Testing 3D curves..."
DATAFILE=test_curve3D.mtv
run_3d_script

echo ""
echo ""
echo ""
echo "Testing 3D pyramid (with hiddenlines)..."
DATAFILE=test_pyramid.mtv
run_3d_script

#
# Test contours
#

echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo "This example demonstrates contours."
echo "The contour steps are selected automatically to be rounded values."
echo ""

# colored contours with fills
echo "Testing contours..."
DATAFILE='test_ctrelb.mtv test_ctrelb2.mtv test_ctrelb3.mtv'
run_script


echo ""
echo ""
echo ""
echo "PLOTMTV can also draw multiple plots in the same screen."
echo "In the following example, the 3 contour plots from the"
echo "previous example are plotted on the same window."
echo ""
echo "The window will come up with 3 subwindows, each containing a plot."
echo "The topmost subwindow is highlighted, meaning that all the buttons"
echo "at the top of the window affect this subwindow.  To shift the focus"
echo "either click the mouse on the subwindow, or press the SPACE bar while"
echo "placing the mouse in the subwindow."
echo ""
# Multiple plots
echo "Testing multiple-plot contours..."
DATAFILE='test_ctrelb.mtv test_ctrelb2.mtv test_ctrelb3.mtv'
run_mult_script
echo ""
echo ""
echo ""
echo "The following two examples illustrate the use of different"
echo "colormaps in drawing filled contours."
echo ""

# colored contours
echo "Testing default contour fill color map..."
DATAFILE=test_ctrcolors.mtv
run_script

echo ""
echo ""
echo ""

# colored contours (different color map)
echo "Testing alternate contour fill color map..."
DATAFILE=test_ctrcolors.mtv
run_clr_script

echo ""
echo ""
echo ""
echo ""
echo "The 'test_ctrs.mtv' file contains several simple contour datasets."
echo "This example shows the various things one can do with contours."
echo "Contours can be drawn with lines, or filled with colors, or plotted"
echo "as a surface mesh.  The values of the contours may also be specified"
echo "in the data file."
echo ""

# various contour options
echo "Testing various contour options..."
DATAFILE=test_ctrs.mtv
run_mult_script

echo ""
echo ""
echo ""
echo ""
echo "This example takes in random data, triangulates the data, and"
echo "plots the contours."
echo ""

# triangular contours
echo "Testing contours based on triangular/random data..."
DATAFILE=test_trictr.mtv
run_script

#
# Different formats
#

echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo "A common data format is the column format, where data is arranged"
echo "in columns - one column corresponds to the x-ordinate, while the"
echo "others are the various y-ordinates.  This example shows the use"
echo "of the column format."
echo ""

# Test column format
echo "Testing column format..."
DATAFILE=test_column.mtv
run_all_script

echo ""
echo ""
echo ""
echo ""
echo "PLOTMTV can also plot 4D data, that is, data as a function of x,y,z"
echo ""

# Test grid format
echo "Testing grid4D format..."
DATAFILE=test_grid4D.mtv
run_3d_script

echo ""
echo ""
echo ""
echo ""
echo "This program can even do vectors!"
echo ""

# Test vector format
echo "Testing vector format..."
DATAFILE=test_vector.mtv
run_script

echo ""
echo ""
echo ""
echo ""
echo "Barcharts were recently added in..."
echo ""

# Test barchart format
echo "Testing barchart format..."
DATAFILE=test_bar.mtv
run_script

echo ""
echo ""
echo ""
echo ""
echo "PLOTMTV can also plot data in the form of histograms"
echo ""

# Test histogram format
echo "Testing histogram format..."
DATAFILE=test_histogram.mtv
run_script

echo ""
echo ""
echo ""
echo ""
echo "Probability plots are used frequently in statistics"
echo ""

# Test probability format
echo "Testing probability format..."
DATAFILE=test_prob.mtv
run_script


#
# Misc Options
#

echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo ""
echo "These next plots demonstrate some useful plotting options..."
echo ""
echo "PLOTMTV even has splines! Yow!"
echo ""

# Test splines
echo "Testing splines..."
DATAFILE=test_spln.mtv
run_script

echo ""
echo ""
echo ""
echo ""
echo "Sometimes it is necessary to plot the absolute value of the data"
echo "This example demonstrates this."
echo ""

# Test absolute
echo "Testing plotting on absolute-valued scales..."
DATAFILE=test_abs.mtv
run_script

echo ""
echo ""
echo ""

# Test log scale
echo "Testing log interpolation..."
DATAFILE=test_log.mtv
run_script

echo ""
echo ""
echo ""

# Testing small numbers
echo "Testing small numbers..."
DATAFILE=test_smallnum.mtv
run_script

echo ""
echo ""
echo ""

# Testing exponents     
echo "Testing exponential notation on plot axes..."
DATAFILE=test_exp.mtv
run_script

echo ""
echo ""
echo ""

# Testing axis flipping
echo "Testing axis flipping..."
DATAFILE=test_axisflip.mtv
run_script

echo ""
echo ""
echo ""

# Testing axis labels
echo "Testing axis labels..."
DATAFILE=test_axislabel.mtv
run_script
