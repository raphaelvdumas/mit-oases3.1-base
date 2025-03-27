#! /usr/bin/env python3
#
# Author: Gaute Hope <gaute.hope@nersc.no> / 2017-01-11
#

import os, sys
import numpy as np

from ..utils.wtimer import Timer

def read_plt (file, debug = False):
  """

  Reads an OASES plt and plp file created for MULPLT from the MINDIS
  graphics package.

  Args:

    file            name of .plt and .plp file pair to read

  Returns:

    plots           list of plot dictionaries

  """

  plots_to_read = None # currently disabled, performance might not be an issue here

  if file[-4:] == '.plp' or file[-4:] == '.plt':
    file = file[:-4]

  plp_u = file + '.plp'
  plt_u = file + '.plt'

  assert os.path.exists (plp_u), "%s does not exist." % plp_u
  assert os.path.exists (plt_u), "%s does not exist." % plt_u


  # state:
  # 0 = waiting for first header
  # 1 = header and setup
  # 2 = labels
  # 3 = first block of opts ( x axis stuff )
  # 4 = second block of opts ( y axis stuff )
  # 5 = curves
  state = 0;

  plots = []
  Np    = -1

  print ('reading: %s..' % plp_u)
  with Timer () as tt:
    with open (plp_u, 'r') as plp_f:

      l = plp_f.readline ()

      while l != '':
        if state == 0:
          if 'MODU' in l:
            # first line, skip
            pass

          elif 'PLTEND' in l:
            break # we are finished

          else:
            # set up new plot
            Np += 1
            plots.append (dict())

            # reset internal states
            s1_n = 1
            s2_n = 1
            s3_n = 1
            s4_n = 1
            s5_n = 1

            nopts = l.split (',')
            plots[Np]['name'] = nopts[0].strip ()

            state = 1

        elif state == 1:
          # titles
          if s1_n == 1:
            plots[Np]['title'] = l.strip ()

          elif s1_n == 2:
            plots[Np]['datatitle'] = l.strip ()
            state += 1

          s1_n += 1

        elif state == 2:
          # labels
          if s2_n == 1:
            ls = l.strip ().split (' ')
            plots[Np]['labelsn'] = int (ls[0])
            plots[Np]['labels']  = []
            nl = 0

            s2_n += 1

            if plots[Np]['labelsn'] < 1:
              state += 1

          elif s2_n == 2:
            # read labels
            k = l.find ('$')
            assert k >= 0, 'label not terminated'
            plots[Np]['labels'].append (l[:k].strip ())

            nl += 1
            if nl >= plots[Np]['labelsn']:
              state += 1

        elif state == 3:
          # first block of opts
          if s3_n == 1:
            ls = float(l.strip ().split (' ')[0])
            plots[Np]['xlen'] = ls

          elif s3_n == 2:
            ls = float(l.strip ().split (' ')[0])
            plots[Np]['ylen'] = ls

          elif s3_n == 3:
            ls = float(l.strip ().split (' ')[0])
            plots[Np]['grid_type'] = ls

          elif s3_n == 4:
            ls = float(l.strip ().split (' ')[0])
            plots[Np]['xleft'] = ls

          elif s3_n == 5:
            ls = float(l.strip ().split (' ')[0])
            plots[Np]['xright'] = ls

          elif s3_n == 6:
            ls = float(l.strip ().split (' ')[0])
            plots[Np]['xinc'] = ls

          elif s3_n == 7:
            ls = float(l.strip ().split (' ')[0])
            plots[Np]['xdiv'] = ls

          elif s3_n == 8:
            # x name
            k = l.find('$')
            assert k > 0, 'x name not terminated'
            l = l[0:k].strip ()

            plots[Np]['xname'] = l

          elif s3_n == 9:
            # x scale
            l = l.strip ()
            plots[Np]['xscale'] = l

            state += 1

          s3_n += 1

        elif state == 4:
          # y axis
          if s4_n == 1:
            ls = float(l.strip ().split (' ')[0])
            plots[Np]['ydown'] = ls

          elif s4_n == 2:
            ls = float(l.strip ().split (' ')[0])
            plots[Np]['yup'] = ls

          elif s4_n == 3:
            ls = float(l.strip ().split (' ')[0])
            plots[Np]['yinc'] = ls

          elif s4_n == 4:
            ls = float(l.strip ().split (' ')[0])
            plots[Np]['ydiv'] = ls

          elif s4_n == 5:
            # y name
            k = l.find ('$')
            assert k > 0, 'y name not terminated'
            l = l[0:k].strip ()
            plots[Np]['yname'] = l

          elif s4_n == 6:
            # y scale
            plots[Np]['yscale'] = l.strip ()

            state = state + 1;

          s4_n = s4_n + 1;

        elif state == 5:
          # curves
          if s5_n == 1:
            ls = int(l.strip ().split (' ')[0])
            plots[Np]['nc'] = ls
            plots[Np]['curves'] = []

            s5_n_i = 1;
            nc     = 0;

            assert plots[Np]['nc'] > 0, 'no curves specified'

            s5_n = s5_n + 1;

          elif s5_n == 2:
            if s5_n_i == 1:
              ls = int(l.strip ().split (' ')[0])
              plots[Np]['curves'].append ({})
              plots[Np]['curves'][nc]['N'] = ls

            elif s5_n_i == 2:
              ls = l.strip ().split (' ')
              plots[Np]['curves'][nc]['xoff'] = float (ls[0])

            elif s5_n_i == 3:
              ls = l.strip ().split (' ')
              plots[Np]['curves'][nc]['dx'] = float(ls[0])

            elif s5_n_i == 4:
              ls = l.strip ().split (' ')
              plots[Np]['curves'][nc]['yoff'] = float(ls[0])

            elif s5_n_i == 5:
              ls = l.strip ().split (' ')
              plots[Np]['curves'][nc]['dy'] = float(ls[0])

              nc = nc + 1;
              if nc >= plots[Np]['nc']:
                # done with all curves for plot, plot is finished
                state = 0;
              else:
                # more curves to go
                s5_n_i = 0;

            s5_n_i = s5_n_i + 1;

        l = plp_f.readline ()

    Np += 1

    with open (plt_u, 'r') as plt_f:
      read      = 0
      skipped   = 0

      debug_every = 1

      for i in range (0, Np):

        nc = plots[i]['nc']
        # the data block contains the data points for the curves without any
        # spaces.
        #
        # each data block is separated by a blank line.
        #

        for c in range (0, nc):
          N  = plots[i]['curves'][c]['N']
          dx = plots[i]['curves'][c]['dx']
          dy = plots[i]['curves'][c]['dy']

          assert N > 0, 'negative N values not supported, should be plotted as markers'

          ## From OASES Version 3.1 manual p. 167
          #
          # "The parameter NC specifies the number of curves to be plotted. For
          # each curve a sub-block of 5 records has to be specified. The first
          # parameter N indicates the number of points in the curve. If N is
          # negative, no curve will be plotted; instead a marker will be plotted
          # at the position of each data point. The parameter XMIN is the
          # x-coordinate (range) of the first data point, whereas DX is the
          # equidistant spacing. If DX had been specified as 0, then
          # the N x-coordinates of the data points would be read from the plt
          # file. In that case XMIN would be interpreted as an x-offset to be
          # applied to the curve. The same rules apply to YMIN and DY .
          # In the above example the y-values will therefore be read from
          # the PLT file, and no y-offset will be applied. The offsets are
          # mainly used for the stacked time series plots If both DX and DY
          # are specified as 0, then mplot will first read all N x-values and
          # then all y-values."
          #
          # However, there is no XMIN or YMIN parameter, I have assumed XLEFT
          # and YDOWN should be used here, these are used to define the
          # starting point of the argument vector. Also, X and Y offset (specfied
          # for each curve) + XLEFT (or YDOWN) is assumed to be the starting
          # point of the first element in the argument vector.
          #
          ##

          if (plots_to_read is not None) and (i not in plots_to_read):
            if (debug and skipped % debug_every == 0):
              print ('skipped data for (plot no: %d of %d): %s, %s, %s..' % (i, Np, plots[i]['name'], plots[i]['title'], plots[i]['datatitle']))

            skipped += 1

            number_s = 15 # each number-line is 15 bytes

            if dx == 0:
              plt_f.seek (number_s * N, 1) # we are now positioned after the last number

            if dy == 0:
              plt_f.seek (number_s * N, 1) # we are now positioned after the last number

            plots[i]['curves'][c]['x'] = np.nan
            plots[i]['curves'][c]['y'] = np.nan

          else:
            # read..

            if (debug and read % debug_every == 0):
              print ('reading data for (plot no: %d of %d): %s, %s, %s..' % (i+1, Np, plots[i]['name'], plots[i]['title'], plots[i]['datatitle']))

            read += 1

            plots[i]['curves'][c]['x'] = np.full ((N,), np.nan)
            plots[i]['curves'][c]['y'] = np.full ((N,), np.nan)

            if dx == 0:
              plots[i]['curves'][c]['x'] = np.fromfile (plt_f, float, N, ' ')
              plt_f.readable () # skip empty line
              plots[i]['curves'][c]['x_generated'] = False

            else:
              # generate x scale
              assert 'LIN' in plots[i]['xscale'], "we only support generating linear scales"
              plots[i]['curves'][c]['x'] = np.linspace (plots[i]['xleft'], plots[i]['xright'], N)
              plots[i]['curves'][c]['x_generated'] = True

            if dy == 0:
              plots[i]['curves'][c]['y'] = np.fromfile (plt_f, float, N, ' ')
              plt_f.readable () # skip empty line
              plots[i]['curves'][c]['y_generated'] = False

            else:
              # generate y scale
              assert 'LIN' in plots[i]['yscale'], "we only support generating linear scales"
              plots[i]['curves'][c]['y'] = np.linspace (plots[i]['ydown'], plots[i]['yup'], N)
              plots[i]['curves'][c]['y_generated'] = True


  print ("read %d plots (%d skipped) in %s" % (read, skipped, tt.pp_duration ()))


  return plots


