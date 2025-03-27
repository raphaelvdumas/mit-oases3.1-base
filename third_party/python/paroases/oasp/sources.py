"""
Defines source pulses as described in the OASES and SAFARI manual.
"""

import numpy as np
import scipy as sc

def safari_2 (fc, dt):
  """
  SAFARI pulse no. 2

  inpurt arguments:

  fc    -   center frequency
  dt    -   sample rate

  output:

  t     -   time
  r     -   source pulse for `t`
  """

  A =  [ 0.48829,
        -0.14128,
         0.01168 ]
  T  = 1.55 / fc
  tt = np.arange (0, T, dt)
  s  = np.sum ((a * (i+1)**2 * np.cos (2 * np.pi * (i+1) * tt / T) for i,a in enumerate(A)))

  return (tt, s)

def safari_5 (fc, dt):
  """
  Seismic pulse (SAFARI no. 5)

  f(t) = sin (w_c * t) - 1/2 * sin(2* w_c * t), 0 <= t <= T = 1/f_c

  input arguments:

  fc    -   center frequency
  dt    -   sample rate


  output:

  t     -   time
  r     -   source pulse for `t`
  """

  wc = fc * 2 * np.pi
  tt = np.arange (0, 1/fc, dt)
  s  = np.sin (wc * tt) - 1./2. * np.sin (2 * wc * tt)


  return (tt, s)


def berlage (A, n, alpha, f0, phi0, tt):
  """
  Berlage wavelet

  Example:

  >>> A       = - 100000.
  >>> n       = 2
  >>> alpha   = 30
  >>> f0      = 20
  >>> phi0    = np.pi / 2

  >>> tt = np.arange (-5, 10, .01)
  >>> s  = berlage (A, n, alpha, f0, phi0, tt)
  >>> s  = np.hamming (len(s)) * s
  """

  # emulating heaviside
  w = np.zeros (tt.shape)
  tth = tt[np.where(tt>=0)]

  w[np.where(tt>=0)] = A * tth**n * np.exp (-alpha * tth) * np.cos (2 * np.pi * f0 * tth + phi0)

  w[np.where(tt==0)] = .5 * w[np.where(tt==0)]

  return w


def ricker (fc, t):
  """
  Ricker wavelet with center frequency fc at t.

  Example:

  >>> t = np.arange (-10, 10, .001)
  >>> s = ricker (fc, t)
  """

  return (1 - 2*np.pi*np.pi*fc*fc*t*t) * np.exp (-np.pi*np.pi*fc*fc*t*t);

def chirp (f1, f2, t):
  """
  Linear sweep from f1 to f2 at t.

  Example:

  >>> f1 = 480.
  >>> f2 = 520.
  >>> t = np.arange (0, 10, .001)
  >>> s = ricker (f1, f2, t)

  """

  ff = np.linspace (f1, f2, len(t))
  s  = np.cos (ff * 2 * np.pi * t)

  return s

