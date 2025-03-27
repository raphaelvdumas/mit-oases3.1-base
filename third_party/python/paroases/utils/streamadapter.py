#! /usr/bin/env python3
#
# Author: Gaute Hope <gaute.hope@nersc.no> / 2016-11-11
#
# An adapter for writing to several streams at the same time.

import sys

class StreamAdapter:
  def __init__ (self, streams = []):
    self.streams = streams

  def flush (self, *args):
    for s in self.streams:
      s.flush (*args)

  def write (self, *args):
    for s in self.streams:
      s.write (*args)

  def close (self):
    del self.streams

  def has_stdout (self):
    if sys.stdout in self.streams:
      return True

  def has_stderr (self):
    if sys.stderr in self.streams:
      return True

if __name__ == '__main__':
  print ("testing streamadapter: test.log and stdout")
  import sys

  with open ('test.log', 'w') as fd:
    ss = StreamAdapter ([sys.stdout, fd])


    print ("testing..", file = ss)

