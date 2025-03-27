#! /usr/bin/env python3
#
# Author: Gaute Hope <gaute.hope@nersc.no> / 2017-02-02
#
# Count occurences in files and output progress

import time
import os

from threading import Thread

try:
  from tqdm import tqdm
  _has_tqdm = True
except ImportError:
  _has_tqdm = False

class ProgressCounter:
  delay = .1

  files = None
  key   = None
  total = 0
  current = 0

  # file mtimes
  mtimes = None
  pos    = None

  # current counts
  counts = None

  # thread
  thread = None
  run    = False

  # progress bar
  pbar   = None

  has_tqdm  = _has_tqdm
  available = has_tqdm

  def __init__ (self, key, total, files):
    self.files = files
    self.key   = key
    self.total = total

    self.mtimes = [0 for f in self.files]
    self.pos    = [0 for f in self.files]
    self.counts = [0 for f in self.files]

  def counter (self):
    while self.run:
      for fe,f in enumerate (self.files):
        if os.path.exists (f):
          st = os.stat (f).st_mtime
          if st != self.mtimes[fe]:
            self.mtimes[fe] = st

            diff = self.check (fe)
            if diff > 0:
              self.counts[fe] += diff
              self.current    += diff
              self.update (diff)

      time.sleep (self.delay)

  def check (self, fe):
    f = self.files[fe]

    count = 0

    with open (f, 'r') as fd:
      fd.seek (self.pos[fe])

      for l in fd:
        if self.key in l:
          count += 1

      self.pos[fe] = fd.tell ()

    return count

  def update (self, count):
    if self.has_tqdm and self.pbar is not None:
      if count > 0:
        self.pbar.update (count)

  def stop (self):
    if self.has_tqdm:
      self.run = False
      self.thread.join ()
      d = self.total - self.current
      if d > 0:
        self.pbar.update (self.total - self.current)
      self.pbar.close ()

  def start (self):
    if self.has_tqdm:
      # set up progress bar
      self.pbar = tqdm (total = self.total, leave = True)

      # start thread
      self.run = True
      self.thread = Thread (target = self.counter)
      self.thread.start ()



