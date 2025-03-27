#! /usr/bin/env python3
#
# Author: Gaute Hope <gaute.hope@nersc.no> / 2017-02-27
#

from ..mulplt import *

class OastJob:
  file = None

  def __init__ (self, fname = None, read = False):
    if fname is not None:
      self.read_dat (fname)

    if read and fname is not None:
      self.read_plt ()

  def read_dat (self, f):
    if not os.path.exists (f):
      raise ValueError ("oast dat file: %s does not exist." % f)

    self.file = f

    print ("oast: reading %s.." % self.file)

    with open (self.file, 'r') as fd:
      ll = fd.readlines ()

    def strip (l):
      e = l.find ('#')
      if e != -1:
        l = l[:e]
      return l.strip ()

    def strip_split (l):
      return strip(l).split ()

    self.title = strip(ll[0])
    self.opts  = strip(ll[1])

    self.frequencies = [float (_f) for _f in strip_split(ll[2])]
    self.frequencies = np.linspace (self.frequencies[0], self.frequencies[1], int(self.frequencies[2]))

    # try to detect if this is range-dependent
    self.rd = False
    _fnd = False
    _p   = None
    for _si, _sectors in enumerate(ll[3:]):
      # find first non-empty line after the first layer number or sector number line
      _sectors = strip(_sectors)
      if len(_sectors) == 0:
        continue
      else:
        if not _fnd:
          _fnd = True
          _p   = _sectors
          continue
        # print (_sectors)
        _sectors = strip_split(_sectors)
        # print (_sectors)

        # should either be exactly 2 (layers, range) or be more for a layer line
        if len(_sectors) == 2:
          print ("mode: range-dependent")
          self.rd = True
          _sectors = strip_split (_p)
          self.sectors = int(_sectors[0])
          j = 3 + _si
        else:
          self.rd = False
        break

    if not self.rd:
      print ("mode: range-independent")
      Nl = int (ll[3].strip ())
      self.layers = []
      for k in range (Nl):
        self.layers.append (strip(ll[4 + k]))
      self.layers = [[float (_f) for _f in l.split ()] for l in self.layers]

      j = 4 + Nl

    else:
      # loop over sectors
      self.sector_segments = []
      self.layers = None
      for s in range(self.sectors):
        layers = []
        nl     = 0
        # find start of next sector
        while len(ll[j].strip ()) == 0:
          j += 1

        _nl = strip_split(ll[j])
        nl = int(_nl[0])

        j += 1
        for k in range (nl):
          layers.append (strip(ll[j + k]))

        layers = [[float (_f) for _f in l.split ()] for l in layers]

        self.sector_segments.append ((nl, layers))
        j += nl

      # skip empty blocks
      while len(ll[j].strip ()) == 0:
        j += 1


    self.source_depth = float (strip(ll[j]));  j += 1

    # receiver array depths
    if self.rd:
      rr = [float (_f) for _f in strip_split(ll[j])]; j += 1
      self.receiver_array_depths = np.linspace (rr[0], rr[1], int(rr[2]))

    # receivers
    rr = [float (_f) for _f in strip_split(ll[j])]; j += 1
    self.receiver_depths = np.linspace (rr[0], rr[1], int(rr[2]))

    # Cmin and Cmax
    self.clim = [float (_f) for _f in strip_split(ll[j])]; j += 1

    # Wavenumber integration
    wn = [int(_f) for _f in strip_split(ll[j])]; j += 1
    self.nw  = wn[0]
    self.ic1 = wn[1]
    self.ic2 = wn[2]

    j += 1 # empty line (?)

    # range axis
    # self.range = np.arange (*[float (_f) for _f in fd.readline ().split ()])

    # tloss axis
    # ..

    self.velocity_profile_plotted = ('Z' in self.opts)
    self.depth_range        = (len(self.frequencies) == 1)
    self.frequency_range    = (len(self.receiver_depths) == 1)

    assert (self.frequency_range or self.depth_range), "only either frequency-range or depth-range plots supported"


  def read_plt (self):
    """
    Reads output from OAST .plp and .plt files and returns
    a list of plot dictionaries and a image of the transmission
    loss.

    Returns:
      tloss   Image of transmission loss
      plts    List of plot dictionaries
    """
    assert self.file is not None, "no file set"

    self.plts = read_plt (self.file[:-4] + '.plp')

    # start of transmission loss plots: integrands come first in pairs of integrand
    # transmission loss curves.
    start_tloss = 1

    if self.velocity_profile_plotted:
      start_tloss += 1

    plts = self.plts[start_tloss::2] # skip integrands and vel profile
    Np   = len (plts)

    # make transmission loss image
    if self.depth_range:
      x  = plts[0]['curves'][0]['x']
      nx = len(x)
      self.x = x

      tloss = np.full ((Np, nx), np.nan)
      for i, p in enumerate (plts):
        tloss[i, :] = p['curves'][0]['y']

    elif self.frequency_range:
      x  = plts[-1]['curves'][0]['x'] # last one is the longest
      nx = len (x)
      self.x = x

      tloss = np.full ((Np, nx), np.nan)
      for i, p in enumerate (plts):
        tloss[i, :] = np.interp (x, p['curves'][0]['x'], p['curves'][0]['y'])

    self.tloss = tloss

    return (tloss, self.plts)

  def save (self, fname):
    """
    Saves this class to a npz file
    """
    print ("oast: saving to: %s.." % fname)
    np.savez (fname, tloss = self.tloss, plts = self.plts, title = self.title, x = self.x, frequencies = self.frequencies, receiver_depths = self.receiver_depths, source_depth = self.source_depth, layers = self.layers)









