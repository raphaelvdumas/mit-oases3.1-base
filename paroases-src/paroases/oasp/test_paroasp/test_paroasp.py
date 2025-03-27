#! /usr/bin/env python3
#
# Author: Gaute Hope <gaute.hope@nersc.no> / 2016-11-15
#

import unittest as ut
import numpy as np
import scipy as sc
import scipy.signal
from scipy.signal import hilbert
import os, sys, shutil, subprocess

root = os.path.abspath (os.path.join(os.path.dirname (__file__), '../../..'))
sys.path.insert (0, root)

import matplotlib.pyplot as plt

import numpy.testing

from paroases.oasp.paroasp import ParOasp
from paroases.oasp import oasp

class BaseTestCases:
  class ParallelJob (ut.TestCase):
    name = None
    dir  = None
    exe  = 'oasp'
    flags = ''

    def setUp (self):
      # run parallel jobs
      super (BaseTestCases.ParallelJob, self).setUp ()

      assert self.dir is not None, "dir not set"
      assert self.name is not None, "name not set"

      self.par = os.path.join (self.dir, 'parallel')

      if not os.path.exists (os.path.join (self.par, self.name + '.npz')) or not os.path.exists (os.path.join (self.par, self.name + '.trf')):
        if not os.path.exists (self.par): os.makedirs (self.par)
        shutil.copy (os.path.join (self.dir, self.name + '.dat'), os.path.join (self.par, self.name + '.dat'))

        print ("running oasp parallel..")
        subprocess.check_call ('time paroasp ' + self.name + '.dat --npz ' + self.flags + ' | tee parallel.log', cwd = self.par, shell = True)
        print ("running oasp parallel.. done.")

        print ("creating .trf as well for paroasp..")
        subprocess.check_call ('time paroasp ' + self.name + '.dat --trf -t ' + self.flags + ' | tee parallel_trf.log', cwd = self.par, shell = True)
        print ("trf.. done.")


    def tearDown (self):
      pass

    def test_parallel_completed (self):
      self.assertTrue (os.path.exists (os.path.join (self.par, self.name + '.npz')))

  class SequentialJob (ut.TestCase):
    name = None
    dir  = None
    exe  = 'oasp'
    flags = ''

    def setUp (self):
      # run sequential jobs
      super (BaseTestCases.SequentialJob, self).setUp ()

      assert self.dir is not None, "dir not set"
      assert self.name is not None, "name not set"

      self.seq = os.path.join (self.dir, 'sequential')

      if not os.path.exists (os.path.join(self.seq, self.name + '.trf')):
        if not os.path.exists (self.seq): os.makedirs (self.seq)
        shutil.copy (os.path.join (self.dir, self.name + '.dat'), os.path.join (self.seq, self.name + '.dat'))

        print ("running oasp sequential..")
        subprocess.check_call ('time %s ' % self.exe + self.name + ' 2>&1 | tee sequential.log', cwd = self.seq, shell = True)
        print ("running oasp sequential.. done.")

    def tearDown (self):
      pass

    def test_sequential_completed (self):
      self.assertTrue (os.path.exists (os.path.join (self.seq, self.name + '.trf')))

  class CompareSequentialParallel (ParallelJob, SequentialJob):
    def setUp (self):
      super (BaseTestCases.CompareSequentialParallel, self).setUp ()


    def test_load_sequential (self):
      trf, sd, z, rr, f, fc, omegaim, nfft, dt, nout, header, icdr, msuft, isrow, inttyp, num_z = oasp.read_trf (os.path.join (self.seq, self.name + '.trf'), False)

    def test_load_parallel (self):
      T = np.load (os.path.join (self.par, self.name + '.npz'))

      trf = T['trf']
      f   = T['f']
      sd  = T['sd']
      z   = T['z']
      rr  = T['range']
      fc  = T['fc']
      omegim = T['omegim']
      nfft = T['nfft']
      dt  = T['dt']

    def test_compare_trf (self):
      s_trf, s_sd, s_z, s_rr, s_f, s_fc, s_omegaim, s_nfft, s_dt, s_nout, s_header, s_icdr, s_msuft, m_isrow, m_inttyp, s_num_z = oasp.read_trf (os.path.join (self.seq, self.name + '.trf'), False)

      T = np.load (os.path.join (self.par, self.name + '.npz'))

      p_trf = T['trf']
      p_f   = T['f']
      p_sd  = T['sd']
      p_z   = T['z']
      p_rr  = T['range']
      p_fc  = T['fc']
      p_omegim = T['omegim']
      p_nfft = T['nfft']
      p_dt  = T['dt']
      p_nout = T['nout']

      np.testing.assert_equal (s_sd, p_sd)
      np.testing.assert_equal (s_z, p_z)
      np.testing.assert_equal (s_rr, p_rr)
      # np.testing.assert_equal (s_fc, p_fc) # this will not match, since it is changed in by paroasp
      np.testing.assert_equal (s_omegaim, p_omegim)
      np.testing.assert_equal (s_nfft, p_nfft)
      np.testing.assert_equal (s_dt, p_dt)

      # frequency array
      np.testing.assert_equal (p_f, s_f)

      # trfs
      # print (p_trf.shape)
      # print (s_trf.shape)

      np.testing.assert_equal (p_trf.shape, s_trf.shape)

      D = np.abs (p_trf - s_trf)

      nf = len (p_f)
      nr = p_trf.shape[1]
      nd = p_trf.shape[0]

      NN  = nf * nr * nd
      SSD = np.sum (np.abs(p_trf - s_trf)**2)
      MSE = SSD / NN

      self.assertEqual (MSE, 0, "Mean Square Error betweeen transfer functions too great")

      SS = np.sum(np.abs(p_trf))
      self.assertGreater (SS, .01, "Tranfer function has very low amplitude")

      # np.testing.assert_almost_equal (p_trf, s_trf, decimal = 6)
      np.testing.assert_equal (p_trf[:], s_trf[:]) # THE BIG TEST!

    def test_compare_trftoascii (self):
      """
      Test whether output from trftoascii matches for both versions
      """

      import pexpect

      # make ascii trf for sequential
      if not os.path.exists (os.path.join (self.seq, self.name + ".ascii")):
        t = pexpect.spawn('trftoascii', cwd = self.seq)
        t.expect ('Input file?')
        t.sendline (self.name + '.trf')
        t.expect ('Output file?')
        t.sendline (self.name + '.ascii')
        t.expect ([pexpect.EOF, pexpect.TIMEOUT], timeout = None)
        t.close ()

      # aaand parallel..
      if not os.path.exists (os.path.join (self.par, self.name + ".ascii")):
        t = pexpect.spawn('trftoascii', cwd = self.par)
        t.expect ('Input file?')
        t.sendline (self.name + '.trf')
        t.expect ('Output file?')
        t.sendline (self.name + '.ascii')
        t.expect ([pexpect.EOF, pexpect.TIMEOUT], timeout = None)
        t.close ()

      # now test, everything should be identical except the center frequency
      with open (os.path.join (self.seq, self.name + '.ascii')) as fd:
        seq = fd.readlines()

      with open (os.path.join (self.par, self.name + '.ascii')) as fd:
        par = fd.readlines ()

      # test except fc and custom depths
      self.assertEqual (seq[:8], par[:8])
      self.assertEqual (seq[9:16], par[9:16])
      self.assertEqual (seq[24:], par[24:])


    def test_make_full_fft (self):
      """
      This function does not test paroasp or oasp.
      """
      Fs = 1000.
      dt = 1. / Fs
      N = 2**16

      t = np.arange (0, N) / Fs

      s = np.sin (400 * 2 * np.pi * t)

      S  = np.fft.fftshift(np.fft.fft (s))
      fs = np.fft.fftshift(np.fft.fftfreq (N, dt))

      tf = fs[np.logical_and (fs>350, fs<450)]
      ts = S[np.logical_and (fs>350, fs<450)]


      # plt.figure ()
      # plt.plot (fs, np.abs(S))

      mf, mS = oasp.make_full_fft (Fs, tf, ts, N)

      # plt.plot (mf, np.abs(mS))

      # plt.show (True)

      self.assertEqual (mS.shape, S.shape)
      np.testing.assert_array_almost_equal (fs, mf)

      np.testing.assert_allclose (np.abs(S), np.abs(mS), atol = 5)


    def test_prop_signal (self):
      """
      This function does not test paroasp or oasp.
      """
      Fs = 1000.
      dt = np.float32(1. / Fs)
      N = 2**13

      t = np.arange (0, N) / Fs

      s = np.sin (400 * 2 * np.pi * t)

      # S  = np.fft.fftshift(np.fft.fft (s))
      # fs = np.float32(np.fft.fftshift(np.fft.fftfreq (N, dt)))


      # mf, mS = oasp.make_full_fft (Fs, tf, ts, N)

      ## make unity transfer function
      T = np.ones ((N,), dtype = np.complex)

      r = oasp.prop_signal (T, s)

      np.testing.assert_allclose (r, s, atol = 1.e-14)

    def test_prop_signal2 (self):
      """
      This function does not test paroasp or oasp.
      """
      Fs = 1000.
      dt = np.float32(1. / Fs)
      N = 2**13

      t = np.arange (0, N) / Fs

      s = np.sin (200 * 2 * np.pi * t)

      # S  = np.fft.fftshift(np.fft.fft (s))

      # tf = fs[np.logical_and (fs>350, fs<450)]
      # ts = S[np.logical_and (fs>350, fs<450)]

      # mf, mS = oasp.make_full_fft (Fs, tf, ts, N)

      ## make unity, transfer function
      fs = np.float32(np.fft.rfftfreq (N, dt))
      T  = np.ones ((len(fs),), dtype = np.complex)

      r = oasp.prop_signal2 (fs, T, dt, N, s)

      # reduce precision
      S = np.complex64(np.fft.rfft (np.float32(s)))
      s = np.fft.irfft (S)

      np.testing.assert_allclose (r, s, atol = 1e-14)


    def test_time_domain (self):
      s_trf, s_sd, s_z, s_rr, s_f, s_fc, s_omegaim, s_nfft, s_dt, s_nout, s_header, s_icdr, s_msuft, m_isrow, m_inttyp, s_num_z = oasp.read_trf (os.path.join (self.seq, self.name + '.trf'), False)

      T = np.load (os.path.join (self.par, self.name + '.npz'))

      p_trf = T['trf']
      p_f   = T['f']
      p_sd  = T['sd']
      p_z   = T['z']
      p_rr  = T['range']
      p_fc  = T['fc']
      p_omegim = T['omegim']
      p_nfft = int(T['nfft'])
      p_dt  = T['dt']

      ## make a test signal and propagate it through the transfer functions, then compare the
      ## time domain signal

      maxt = p_dt * p_nfft
      t = np.arange (0, p_nfft) * p_dt

      fc = (np.max(p_f) + np.min(p_f)) / 2
      # f_s = np.min (p_f)
      f_s = np.min(p_f) + (np.max (p_f) - np.min (p_f)) * .33
      # print ("f_s =", f_s)

      t1 = 0.
      t2 = t1 + min(10., .2 * maxt)
      t1s = int(t1 / p_dt)
      tt = np.arange (t1, t2, p_dt)

      assert t2 <= 0.2 * maxt, "source signal should have good margin on maxt"

      ss = np.sin (f_s * 2 * np.pi * tt) * np.hamming (len(tt))
      s  = np.zeros ((p_nfft,))
      s[t1s:t1s + len(ss)] = ss


      # plt.figure ()

      # source
      # plt.plot (t, s)
      # plt.xlim ([0, .02])
      # plt.show (True)

      Fs = 1. / p_dt

      no = p_trf.shape[0]
      nd = p_trf.shape[1]
      nr = p_trf.shape[2]

      for o in range (no):
        for d in range (nd):
          for r in range (nr):
            for m in range (s_msuft.squeeze()):
              print ("testing, range: %d, depth: %d, nopt: %d, msuft: %d" % (r, d, o, m))

              ## Use prop_signal
              pfreqs, pH = oasp.make_full_fft (Fs, p_f, p_trf[o,d,r,m,:].squeeze (), p_nfft)
              sfreqs, sH = oasp.make_full_fft (Fs, s_f, s_trf[o,d,r,m,:].squeeze (), s_nfft)

              # plt.plot (sfreqs, np.abs(sH))
              # plt.plot (pfreqs, np.abs(pH))
              # plt.show (True)

              np.testing.assert_equal (pfreqs, sfreqs)
              # trf is tested in separate test method

              r1 = oasp.prop_signal (pH, s)
              r2 = oasp.prop_signal (sH, s)

              ## Use prop_signal2
              r1_ = oasp.prop_signal2 (p_f, p_trf[o,d,r,m,:].squeeze (), p_dt, p_nfft, s)
              r2_ = oasp.prop_signal2 (s_f, s_trf[o,d,r,m,:].squeeze (), s_dt, s_nfft, s)

              # plt.plot (t, r1)
              # plt.plot (t, r2, 'g--')
              # plt.show ()

              with self.subTest (i=r+d*nr):
                np.testing.assert_equal (r1, r2)
                np.testing.assert_equal (r1_, r2_)
                np.testing.assert_allclose (r1_, r1, atol = 1e-10)
                np.testing.assert_allclose (r2_, r2, atol = 1e-10)

              # analytical signal
              r1a = hilbert (r1)
              r2a = hilbert (r2)

              # envelope
              np.testing.assert_equal (np.abs(r1a), np.abs(r2a))

class TestAirWater (BaseTestCases.CompareSequentialParallel):
  def __init__ (self, *args):
    self.dir    = 'test_air_water'
    self.name   = 'test'

    super (TestAirWater, self).__init__ (*args)

class TestSimple (BaseTestCases.CompareSequentialParallel):
  def __init__ (self, *args):
    self.dir    = 'test_simple'
    self.name   = 'test'

    super (TestSimple, self).__init__ (*args)

class TestSimpleVH (BaseTestCases.CompareSequentialParallel):
  def __init__ (self, *args):
    self.dir    = 'test_simple_vh'
    self.name   = 'test'

    super (TestSimpleVH, self).__init__ (*args)

class TestRdGx (BaseTestCases.CompareSequentialParallel):
  def __init__ (self, *args):
    self.dir    = 'test_rd_gx'
    self.name   = 'gx'
    self.exe    = 'rdoasp'
    self.flags  = '-2 -w 134'

    super (TestRdGx, self).__init__ (*args)

class TestRdSimple (BaseTestCases.CompareSequentialParallel):
  def __init__ (self, *args):
    self.dir    = 'test_rd_simple'
    self.name   = 'test'
    self.exe    = 'rdoasp'
    self.flags  = '-2'

    super (TestRdSimple, self).__init__ (*args)

class Test3dOasp (BaseTestCases.CompareSequentialParallel):
  def __init__ (self, *args):
    self.dir = 'test_3d'
    self.name = 'saffipp2f'
    self.exe  = 'oasp3'
    self.flags = '-3'

    super (Test3dOasp, self).__init__ (*args)

class TestFreqBins (BaseTestCases.CompareSequentialParallel):
  def __init__ (self, *args):
    self.dir    = 'test_freqbins'
    self.name   = 'test_freqbins'

    # this test case produces f2 to f1 differences less than df. it depends
    # on the number of workers used.
    self.flags  = '-w 2000'

    super (TestFreqBins, self).__init__ (*args)

if __name__ == '__main__':
  ut.main ()

