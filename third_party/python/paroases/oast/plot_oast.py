#! /usr/bin/env python3
#
# Author: Gaute Hope <gaute.hope@nersc.no> / 2015
#

import os, sys
import argparse
import numpy as np
import scipy as sc
import scipy.io as scio
from ..utils.wtimer import *

import gc

import matplotlib
# matplotlib.use ('Agg')
import matplotlib.pyplot as plt
from matplotlib import gridspec, cm, colors

class PlotOast:
  vmin = -100
  vmax = -30
  dpi  = 300
  mat  = None
  npz  = None
  rd   = False

  def __init__ (self, _name, plot, save, npz, verbose = False):
    self.name = _name
    self.plot = plot
    self.save = save
    self.npz  = npz
    self.verbose = verbose

  def load (self):
    if self.verbose:
      print ("loading: %s" % self.name)

    if self.name[-4:] == '.mat':
      self.mat = scio.loadmat (self.name)

      self.tloss = self.mat['tloss']
      self.oast  = self.mat['oast']
      self.x     = self.oast['x'][0][0][0]
      self.plts  = self.oast['plts'][0][0][0]

      self.frequencies = self.oast['frequencies'][0][0][0]
      self.receiver_depths = self.oast['receiver_depths'][0][0][0]

      self.depth_range = (len(self.frequencies) == 1)
      self.freq_range = (len(self.receiver_depths) == 1)

      self.source_depth = self.oast['source_depth'][0][0][0][0]
      self.title = self.oast['title'][0][0][0]
      self.layers = self.oast['layers'][0][0]

    elif self.name[-4:] == '.npz':
      self.npz = np.load (self.name)

      self.tloss = self.npz['tloss']
      self.x     = self.npz['x']
      self.plts  = self.npz['plts']

      self.frequencies = self.npz['frequencies']
      self.receiver_depths = self.npz['receiver_depths']

      self.depth_range = (len(self.frequencies) == 1)
      self.freq_range = (len(self.receiver_depths) == 1)

      self.source_depth = self.npz['source_depth']
      self.title        = self.npz['title']
      self.layers       = self.npz['layers']

    elif self.name[-4:] == '.dat':
      # read .plp, .plt and .dat
      from paroases  import mulplt
      from paroases  import oast
      oast = oast.OastJob (self.name, True)

      if self.npz:
        p = self.name[:-4] + '.npz'
        oast.save (p)

      self.tloss = oast.tloss
      self.x     = oast.x
      self.plts  = oast.plts

      self.frequencies     = oast.frequencies
      self.receiver_depths = oast.receiver_depths

      self.depth_range = (len(self.frequencies)     == 1)
      self.freq_range  = (len(self.receiver_depths) == 1)

      self.source_depth = oast.source_depth
      self.title        = oast.title
      self.layers       = oast.layers
      self.rd           = oast.rd
      if self.rd:
        self.sector_segments = oast.sector_segments

    else:
      raise ValueError ("only .dat, .mat or .npz files supported")

  def clear (self):
    if self.mat: del self.mat
    if self.npz: del self.npz
    del self.tloss
    gc.collect ()

  def make_plot (self):
    fig = plt.figure (figsize = (20, 6), dpi = self.dpi)
    if not self.rd:
      gs = gridspec.GridSpec (1, 2, width_ratios = [1, 6])
      ax = plt.subplot (gs[0])

      # plot velocity profile
      vel = self.layers
      # depth = vel[:, 0]
      # pvel  = vel[:, 1]
      # svel  = vel[:, 2]
      depth = []
      pvel  = []
      svel  = []

      for vi,v in enumerate(vel):
        d = v[0]
        p = v[1]
        s = v[2]

        if (vi > 0):
          if svel[-1] < 0:
            # linear layer, add new interface
            depth.append (d)
            pvel.append (np.abs(svel[-1]))
            svel.append (0)
          else:
            depth.append (d)
            pvel.append (pvel[-1])
            svel.append (0)

        depth.append (d)
        pvel.append (p)
        svel.append (s)


      # extend last layer to maximum receiver depth
      depth.append (d)
      pvel.append (p)
      svel.append (s)
      depth[-1] = max(self.receiver_depths)

      plt.plot (pvel, depth)
      # ax.set_xlim (1430, 1480)
      # _b = np.linspace (1430, 1480, 6)
      # ax.set_xticks (_b)
      # _x = ["" for xx in _b]
      # _x[0] = str(_b[0])
      # _x[-1] = str(_b[-1])
      # ax.set_xticklabels (_x)

      # ax.set_xticks ([1410, 1530])
      # ax.set_xlim (1410, 3600)
      ymin, ymax = ax.get_ylim ()
      ax.set_ylim (0, ymax)
      ax.invert_yaxis ()
      ax.grid ()

      # plt.title ('Velocity')
      plt.ylabel ('Depth [m]')
      plt.xlabel ('Speed [m/s]')

      ax = plt.subplot (gs[1])

    else:
      ax = fig.gca ()

    if self.depth_range:
      if self.verbose:
        print ("plotoast: depth-range")

      extent = [min(self.x), max(self.x), max(self.receiver_depths), min(self.receiver_depths)]
      h = ax.imshow (-self.tloss, aspect = 'auto', norm = colors.Normalize(), vmin = self.vmin, vmax = self.vmax, extent = extent)
      cb = plt.colorbar (h)
      cb.set_label ('dB')

      # mark source
      plt.plot (0, self.source_depth, 'rp')

      plt.title ('%s - %.1f Hz - (SD: %.1f m) ($\sigma_{zz}$, dB/1Pa)' % (self.title, self.frequencies, self.source_depth))
      plt.xlabel ('Range [km]')

      ax.set_xlim (min(self.x), max(self.x))
      ax.set_ylim (max(self.receiver_depths), min(self.receiver_depths))
      # ax.set_ylim (4500, min(self.receiver_depths))

      # update limits on velocity plot
      if not self.rd:
        ax = plt.subplot (gs[0])
        ax.set_ylim (0, max(self.receiver_depths))
        # ax.set_ylim (0, 4500)
        ax.invert_yaxis ()

    else:
      if self.verbose:
        print ("plotoast: freq-range")

      extent = [min(self.x), max(self.x), max(self.frequencies), min(self.frequencies)]
      h = ax.imshow (-self.tloss, aspect = 'auto', norm = colors.Normalize(), vmin = self.vmin, vmax = self.vmax, extent = extent)
      cb = plt.colorbar (h)
      cb.set_label ('dB')

      plt.title ('%s - receiver depth: %.1f - (source depth: %.1f m) (normal stress, dB/1Pa)' % (self.title, self.receiver_depths, self.source_depth))
      plt.xlabel ('Range [km]')
      plt.ylabel ('Frequencies [Hz]')

      ax.set_xlim (min(self.x), max(self.x))
      ax.set_ylim (max(self.frequencies), min(self.frequencies))
      ax.invert_yaxis ()


    plt.gcf().subplots_adjust(bottom=0.15)
    plt.tight_layout ()
    # gs.tight_layout (fig, w_pad = 0.1)


  def save_plot (self):
    uri = self.name.replace ('.mat', '')
    uri = self.name.replace ('.npz', '')
    uri = self.name.replace ('.dat', '')
    uri = uri + '_tloss.png'
    if self.verbose:
      print ("plotoast: saving..: %s" % (uri))
    plt.savefig (uri)

  def show_plot (self):
    if self.verbose:
      print ("plotoast: showing..")
    plt.show (True)

  def main (self):
    with Wtimer() as t:
      self.load ()
      self.make_plot ()

      self.clear ()

      if self.save:
        self.save_plot ()


    print ('total time: (%0.2f s)' % t.interval)

    if self.plot:
      self.show_plot ()


