#! /usr/bin/env python3
#
# A timer class that can be used with the 'with' statement.
#
# Author: Gaute Hope <gaute.hope@nersc.no> / 2014-03-18
#

import time
from math import floor

class Wtimer:
  start = None
  end   = None

  printit = False
  endl    = True
  desc    = None

  def __init__ (self, printit = None, endl = True, desc = None):
    self.printit = printit
    self.endl    = endl
    self.desc    = desc

    if self.desc is not None and self.printit is None:
      self.printit = True

    if self.printit is None:
      self.printit = False

    if self.printit and self.desc and self.endl:
      print (self.desc + "..", end = '', flush = True);


  def tic (self):
    self.__enter__ ()
    return self

  def toc (self):
    self.__exit__ ()
    return self

  def __enter__ (self):
    self.start = time.perf_counter ()

    return self

  def __exit__ (self, *args):
    self.end = time.perf_counter ()

    if self.printit:
      if self.endl:
        if self.desc is not None:
          print ("done, duration: " + self.pp_duration () + ".")
        else:
          print ("duration: " + self.pp_duration () + ".")
      else:
        if self.desc is not None:
          print (self.desc + ": duration: " + self.pp_duration () + "." , end = '', flush = True)
        else:
          print ("duration: " + self.pp_duration () + "." , end = '', flush = True)

  @property
  def interval (self):
    if self.end is None:
      end = time.perf_counter ()
    else:
      end = self.end
    i = end - self.start

    return i


  def pp_duration (self, d = None):
    if d is None:
      d = self.interval

    dys = floor (d / (24 * 60 * 60))
    d   = d - (dys * 24 * 60 * 60)

    h = floor (d / (60 * 60))
    d = d - (h * 60 * 60)

    m = floor (d / 60)
    d = d - (m * 60)

    s = d

    o = ''
    above = False
    if dys > 0:
      o = '%dd-' % dys
      above = True

    if above or h > 0:
      o = o + '%02dh:' % h
      above = True

    if above or m > 0:
      o = o + '%02dm:' % m
      above = True

    o = o + '%06.3fs' % s

    return o

Timer = Wtimer

if __name__ == '__main__':
  print ("testing Wtimer")

  with Wtimer (False) as t:
    time.sleep (1)

  print ("duration: %f" % t.interval)
  print ("pp_duration: " + t.pp_duration ())

  t.end = 12313123123
  print ("pp_duration: " + t.pp_duration ())



