#! /usr/bin/env python3
#
# Author: Gaute Hope <gaute.hope@nersc.no> / 2015-08-07

import numpy as np
from scipy.io import FortranFile
import struct

def write_trf (fname, trf, sd, z, rr, f, fc, omegaim, nfft, dt, nout, header, icdr, msuft, isrow, inttyp, num_z, debug = False):
  """
  Write OASES trf file

  input:
    trf     -   list of raw transfer functions that should be combined
  """

  print ("writing: %s" % fname)

  with FortranFile (fname, 'w') as fd:
    for h in header:
      fd.write_record (h)

    fd.write_record (fc)
    fd.write_record (sd)

    def _write_record (s):
      # buggy scipy write_record assumes all elements in array to be same type
      import collections

      hdt = fd._header_dtype
      if isinstance (s, np.ndarray) or not isinstance (s, collections.Iterable):
        s  = np.array(s, order = 'F')
        nb = np.array([s.nbytes], dtype = hdt)

        nb.tofile (fd._fp)
        s.tofile (fd._fp)
        nb.tofile (fd._fp)

      else:
        s  = [np.array(e, order = 'F') for e in s]
        nb = np.array([np.sum ((e.nbytes) for e in s)], dtype = hdt)

        nb.tofile (fd._fp)

        for e in s:
          e.tofile (fd._fp)

        nb.tofile (fd._fp)

    if num_z > 0:
      _write_record ([np.float32(z[0]), np.float32(z[-1]), np.int32(len(z))])
    else:
      _write_record ([np.float32(0), np.float32(0), num_z])
      _write_record (z)

    _write_record ((np.float32(rr[0]), np.float32(rr[1] - rr[0]), np.int32(len(rr))))

    if debug:
      print (z)
      print (rr)

    df = np.float32(1. / np.float32(dt * nfft))
    b  = f / df
    bin_low  = np.int32(np.rint(b[0] + 1))
    bin_high = np.int32(np.rint(b[-1] + 1))

    _write_record ([np.int32(nfft), bin_low, bin_high, np.float32(dt)])

    fd.write_record (icdr)
    fd.write_record (omegaim)
    fd.write_record (msuft)
    fd.write_record (isrow)
    fd.write_record (inttyp)

    fd.write_record (np.int32(nout[0])) # hack inherited from oases to support matlab
                                        # script.
    fd.write_record (np.int32(nout[0]))

    for _i in range(5):
      fd.write_record (np.float32(0.))

    trf = trf.T

    if debug:
      print ("trf:", trf.shape)

    _msuft = int(msuft.squeeze ())
    _nout  = int(nout.squeeze ())

    # write trf in one go to avoid time consuming loop
    sz = np.uint32(_nout * 2 * 4) # size of nout array, one real and imaginary part, 4 bytes each
    sz = np.float32(struct.unpack ('f', struct.pack ('I', sz))[0])

    t = np.empty ((trf.shape[0], _msuft, len (rr), len (z), _nout * 2 + 2), dtype = np.float32)
    t[:,:,:,:,0]  = sz # size
    t[:,:,:,:,-1] = sz # size
    t[:,:,:,:,1:-1:2] = np.real (trf)
    t[:,:,:,:,2:-1:2] = np.imag (trf)
    t.tofile (fd._fp)

    # for _if in range(trf.shape[0]):
    #   for _im in range(msuft.squeeze()):
    #     for _ir in range(len(rr)):
    #       for _iz in range(len(z)):
    #         # fd.write_record (_trf[_if, _ir, _iz, :])
    #         # print (_trf[_if, _ir, _iz, :].squeeze())
    #         res = trf[_if, _im, _ir, _iz, :]
    #         res = np.array([[np.real(r), np.imag(r)] for r in res], dtype = 'float32')
    #         res = np.ravel(res)
    #         _write_record (res)


def read_trf (fname, debug = False):
  """
  Read transfer function file produced by OASP

  fname     -   (string) .trf file produced by OASP
  debug     -   (bool)   print debug output (default: False)

  returns:

  trf       -   complex array with three dimensions [depth, receiver, frequency]
  sd        -   source depth in meters
  z         -   receivers in meters
  rr        -   range of receivers in km
  f         -   frequencies
  fc        -   center frequency - do not trust, only specified for historical reasons
  omegaim   -   imaginary part of radian frequency
  nfft      -   length of transfer functions / number of frequencies
  dt        -   sample interval in seconds
  nout      -   number of output parameters
  header    -   dump of first part of header
  """

  with FortranFile (fname, 'r') as fd:

  # with open (fname, 'rb') as fd:
    # junk = np.fromfile (fd, 'str', -1)
    # ported from read_trf.m

    fileid = fd.read_record ('c')
    prognm = fd.read_record ('c')
    nout   = fd.read_ints ()
    iparms = fd.read_ints ()
    title  = fd.read_record ('c')
    if debug:
      print ("title=", "".join((c.decode() for c in title)))

    sign = fd.read_record('c')
    assert sign == b'+' or sign == b'-'

    header = [ fileid, prognm, nout, iparms, title, sign]

    if debug:
      print ("sign =", sign)

    fc = fd.read_reals ('float32')

    if debug:
      print ("center frequency:", fc)

    sd = fd.read_reals ('float32')
    if debug:
      print ("sd:", sd)

    z1, z2, num_z = fd.read_record ('f4,f4,i4')[0]

    if num_z < 0:
      z = fd.read_reals ('float32')
      # d = np.fromfile (fd, 'float32', num_z + 2)
    else:
      z = np.linspace (z1, z2, num_z)

    if debug:
      print ("numz:", num_z)
      print ("z:", z)

    r1, dr, nr = fd.read_record ('f4,f4,i4')[0]

    if debug:
      print ("r1=", r1, "dr=", dr, "nr=", nr)

    rr = np.linspace (r1, r1 + dr*(nr-1), nr)
    if debug:
      print ("range:", rr)

    nfft,bin_low,bin_high,dt = fd.read_record('i4,i4,i4,f4')[0]

    if debug:
      print ("nfft:", nfft)

    df = np.float32(1. / np.float32(dt * nfft))
    f  = np.float32(df * (np.arange (bin_low, bin_high + 1, dtype = np.float32) - 1))

    nf = len (f)

    if debug:
      print ("NF:", nf)
      print ("F:", f)

    icdr    = fd.read_ints ()
    omegaim = fd.read_reals ('float32')
    msuft   = fd.read_ints ()
    isrow   = fd.read_ints ()
    inttyp  = fd.read_ints ()

    if nout == 0:
      nout = 1

    if debug:
      print ("nout=", nout)

    fd.read_ints ()
    fd.read_ints ()

    for i in range(5):
      fd.read_reals ('float32')

    _msuft = int(msuft.squeeze ())
    _nout  = int(nout.squeeze ())

    # we read the full transfer function in one go to save time and avoid a costly loop
    t = np.fromfile (fd._fp, 'float32', nf * _msuft * nr * np.abs (num_z) * (_nout * 2 + 2))
    t = t.reshape (nf, _msuft, nr, np.abs(num_z), _nout * 2 + 2)
    t = t[:,:,:,:,1:-1]                       # skip sizes (dtype of size header is 32 bit)
    t = t[:,:,:,:,::2] + 1j * t[:,:,:,:,1::2] # make into complex from real and imaginary part
    trf = t.T

    # loop matching the writing routine in OASES, this is time consuming
    # trf = np.empty ((nf, msuft.squeeze(), nr, np.abs(num_z), nout[0]), dtype = 'complex64')
    # for _if in range(nf):
    #   for _im in range(msuft.squeeze()):
    #     for _ir in range(nr):
    #       for _iz in range(np.abs(num_z)):
    #         res = fd.read_reals('float32')
    #         trf[_if, _im, _ir, _iz, :] = res[::2] + 1j * res[1::2]

    if debug:
      print ("trf shape:", trf.shape)

    return (trf, sd, z, rr, f, fc, omegaim, nfft, dt, nout, header, icdr, msuft, isrow, inttyp, num_z)

def make_full_fft (Fs, f, F, N):
  """
  Make a full version of tapered FFT assuming the FFT is for a real signal

  input arguments:
  Fs    -   sample rate in Hz
  f     -   array of frequencies specified in F
  F     -   complex frequency response for frequency in f (tapered FFT)
  N     -   number of frequencies, should be equal to len (f) and len (F)

  returns:
  freqs -   full array of negative and positive frequencies
  H     -   full complex frequency response for negative and positive frequencies
  """
  N = int (N)
  assert np.log2 (N).is_integer (), "N is not a power of 2"

  assert F.ndim <= 2, "F.ndim > 2"

  if F.ndim > 1:
    assert F.shape[1] == 1, "second dimenion not length 1"

  H = np.zeros ((int(N),), dtype = np.complex)

  freqs = np.float32(np.float32(Fs) * np.float32(np.arange (- N/2, N/2 ) / N))

  fmin = f[0]
  fmax = f[-1]

  assert len(freqs) == N, 'frequency array not correct length'

  # print (fmin, fmax)
  # print ("Nyquist:", Fs / 2)
  # print (freqs)

  sf = np.argmin (np.abs (freqs - fmin))
  ef = np.argmin (np.abs (freqs - fmax))

  assert (ef - sf + 1) == len (F), "frequency windows do not match"

  c = int(N / 2) # N is power of 2
  nsf = c - (sf - c)
  nef = c - (ef - c)

  H[sf:ef + 1]   = F
  H[nef:nsf + 1] = np.conjugate (F)[::-1]

  return freqs, H


def prop_signal (T, s):
  """
  Propagate signal (time domain) using FFT through _full_ transfer function T

  input arguments:
  T     -   full, complex, transfer function
  s     -   source signal in time domain

  the source signal, s, must be the same length as T.

  returns:
  r     -   a real time domain signal of the same length as s and T.
  """
  assert len(T) == len (s), "length of transfer function must match length of time domain signal"

  N = len(s)
  assert np.log2(N).is_integer (), "N is not power of 2"

  S = np.fft.fftshift(np.fft.fft (s))

  R = S * T
  R = np.fft.ifftshift (R)
  r = np.fft.ifft (R)
  r = np.real (r)

  return r

def prop_signal2 (f, T, dt, nfft, s):
  """
  Propagate signal through _tapered_ transfer function

  input arguments:
  f     -   frequency argument to transfer function as returned by OASES
  T     -   tapered, complex, transfer function
  dt    -   sample rate (as returned by OASES, must also have been used to generate source signal)
  nfft  -   length of full transfer function
  s     -   source signal in time domain

  returns:
  r     -   a real time domain signal of the same length as s.
  """

  assert len(f) == len (T)
  assert len(s) == nfft
  assert np.log2(nfft).is_integer (), "nfft is not a power of 2"

  # these guys should come directly from OASES
  assert dt.dtype == np.float32
  # assert f.dtype  == np.float32

  s  = np.float32(s)
  S  = np.complex64(np.fft.rfft (s))
  fS = np.float32(np.fft.rfftfreq (nfft, dt))

  # OASES transfer function has been defined for these frequencies:
  df = np.float32 ( 1. / np.float32(dt * nfft))
  b  = f / df

  # these are OASES bins, except not offset +1 from fS
  bin_low  = np.int32(np.rint(b[0]))
  bin_high = np.int32(np.rint(b[-1]))

  tS  = S[bin_low:bin_high + 1]
  Rt  = tS * T

  R = np.zeros (S.shape, dtype = S.dtype)
  R[bin_low:bin_high+1] = Rt

  r = np.real(np.fft.irfft (R))

  return r


""" Reads trf and saves it as .mat """
if __name__ == '__main__':
  import sys
  import scipy.io
  import scipy as sc

  fname = sys.argv[1]

  trf, sd, z, rr, f, fc, omeagaim, nfft, dt = read_trf (fname, True)

  fname = fname[:-4] + ".mat"

  print ("saving to:", fname)
  sc.io.savemat (fname, { 'f' : f, 'trf' : trf})

