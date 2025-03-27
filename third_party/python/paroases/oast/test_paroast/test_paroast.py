#! /usr/bin/env python3
#
# Author: Gaute Hope <gaute.hope@nersc.no> / 2016-11-15
#

import unittest as ut
import numpy as np
import scipy as sc
import scipy.io
import os, sys, shutil, subprocess

root = os.path.abspath (os.path.join(os.path.dirname (__file__), '../../..'))
sys.path.insert (0, root)

import matplotlib.pyplot as plt

import numpy.testing

from paroases.oast.paroast      import ParOast
from paroases.oast.ploat_oast   import PlotOast

class BaseTestCases:
  class CompareSequentialParallel (ut.TestCase):
    name = None
    dir  = None

    def setUp (self):
      # run parallel and sequential jobs

      assert self.dir is not None, "dir not set"
      assert self.name is not None, "name not set"

      self.seq = os.path.join (self.dir, 'sequential')
      self.par = os.path.join (self.dir, 'parallel')

      if not os.path.exists (os.path.join(self.seq, self.name + '.plt')):
        if not os.path.exists (self.seq): os.makedirs (self.seq)
        shutil.copy (os.path.join (self.dir, self.name + '.dat'), os.path.join (self.seq, self.name + '.dat'))

        print ("running oast sequential..")
        subprocess.check_call ('time oast ' + self.name + ' 2>&1 | tee sequential.log', cwd = self.seq, shell = True)
        print ("running oast sequential.. done.")

      if not os.path.exists (os.path.join(self.seq, self.name + '.npz')):
        print ("processing oast sequential..")

        # older version based on matlab
        if not os.path.exists (os.path.join(self.seq, self.name + '.mat')):
          subprocess.check_call ('time process_oast_mat.sh ' + self.name + ' 2>&1 | tee -a sequential.log', cwd = self.seq, shell = True)

        pth = os.path.join (self.seq, self.name + '.dat')
        p = PlotOast (pth, False, True, True, False)
        p.main ()

        print ("processing oast sequential.. done.")

      if not os.path.exists (os.path.join (self.par, self.name + '.plt')):
        if not os.path.exists (self.par): os.makedirs (self.par)
        shutil.copy (os.path.join (self.dir, self.name + '.dat'), os.path.join (self.par, self.name + '.dat'))

        print ("running oast parallel..")
        subprocess.check_call ('time paroast ' + self.name + '.dat | tee parallel.log', cwd = self.par, shell = True)
        print ("running oast parallel.. done.")

      if not os.path.exists (os.path.join(self.par, self.name + '.npz')):
        print ("processing oast parallel..")

        # older version based on matlab
        if not os.path.exists (os.path.join(self.par, self.name + '.mat')):
          subprocess.check_call ('time process_oast_mat.sh ' + self.name + ' 2>&1 | tee -a parallel.log', cwd = self.par, shell = True)

        pth = os.path.join (self.par, self.name + '.dat')
        p = PlotOast (pth, False, True, True, False)
        p.main ()
        print ("processing oast parallel.. done.")


    def tearDown (self):
      pass

    def test_runs_completed (self):
      self.assertTrue (os.path.exists (os.path.join (self.seq, self.name + '.plt')))
      self.assertTrue (os.path.exists (os.path.join (self.par, self.name + '.plt')))

    def load (self, path):
      npz   = np.load (path)
      tloss = npz['tloss']
      x     = npz['x']
      plts  = npz['plts']

      frequencies = npz['frequencies']
      receiver_depths = npz['receiver_depths']

      depth_range = (len(frequencies) == 1)
      freq_range = (len(receiver_depths) == 1)

      source_depth = npz['source_depth']
      title = npz['title']

      return npz, tloss, x, plts, source_depth, receiver_depths, frequencies, title


    def test_load_sequential (self):
      npz, tloss, x, plts, source_depth, receiver_depths, frequencies, title = self.load (os.path.join (self.seq, self.name + '.npz'))

    def test_load_parallel (self):
      npz, tloss, x, plts, source_depth, receiver_depths, frequencies, title = self.load (os.path.join (self.par, self.name + '.npz'))

    def test_compare_parameters (self):
      snpz, stloss, sx, splts, ssource_depth, sreceiver_depths, sfrequencies, stitle = self.load (os.path.join (self.seq, self.name + '.npz'))

      pnpz, ptloss, px, pplts, psource_depth, preceiver_depths, pfrequencies, ptitle = self.load (os.path.join (self.par, self.name + '.npz'))

      np.testing.assert_array_equal (px, sx)
      np.testing.assert_array_equal (psource_depth, ssource_depth)
      np.testing.assert_array_equal (preceiver_depths, sreceiver_depths)
      np.testing.assert_array_equal (pfrequencies, sfrequencies)
      np.testing.assert_array_equal (ptitle, stitle)

    def test_compare_tloss (self):
      snpz, stloss, sx, splts, ssource_depth, sreceiver_depths, sfrequencies, stitle = self.load (os.path.join (self.seq, self.name + '.npz'))

      pnpz, ptloss, px, pplts, psource_depth, preceiver_depths, pfrequencies, ptitle = self.load (os.path.join (self.par, self.name + '.npz'))

      np.testing.assert_equal (ptloss, stloss)

    def test_read_plt (self):
      """
      tests plt.read_plt vs .mat file
      """

      from paroases      import mulplt
      from paroases      import oast

      # load mat
      path = os.path.join (self.dir, 'sequential', self.name + '.mat')

      if not os.path.exists (path):
        self.skipTest ("Skipping .mat vs .npz test since there is not .mat file")

      mat  = sc.io.loadmat (path)

      # loat plp and plt
      path = os.path.join (self.dir, 'sequential', self.name + '.dat')
      oast = oast.OastJob (path, True)

      # save npz, is done in setup
      # path = os.path.join (self.dir, 'sequential', self.name + '.npz')
      # oast.save (path)

      mloss = mat['tloss'].squeeze ()
      oloss = oast.tloss.squeeze ()

      np.testing.assert_equal (mloss, oloss)


class TestAirWater (BaseTestCases.CompareSequentialParallel):
  def __init__ (self, *args):
    self.dir    = 'test_air_water'
    self.name   = 'oasttest'

    super (TestAirWater, self).__init__ (*args)


class TestSimple (BaseTestCases.CompareSequentialParallel):
  def __init__ (self, *args):
    self.dir    = 'test_simple'
    self.name   = 'oasttest'

    super (TestSimple, self).__init__ (*args)

class TestUnderIce (BaseTestCases.CompareSequentialParallel):
  def __init__ (self, *args):
    self.dir    = 'test_underice'
    self.name   = 'test'

    super (TestUnderIce, self).__init__ (*args)


if __name__ == '__main__':
  ut.main ()

