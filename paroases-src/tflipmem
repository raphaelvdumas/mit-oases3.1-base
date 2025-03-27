#! /usr/bin/env python
#
# Author: Gaute Hope <gaute.hope@nersc.no> / 2017-07-28
#
# Flip memory model of array to Fortran or C order using memory mapped files.

import numpy as np
import os, sys
import argparse
from   wtimer import *


parser = argparse.ArgumentParser ( description = 'Flip mem' )
parser.add_argument ('infile', metavar = 'infile', type = str, nargs = 1,
                         help = '.npy file to be flipped')

parser.add_argument ('outfile', metavar = 'outfile', type = str, nargs = 1,
                         help = 'store output file in this file')

parser.add_argument ('-C', dest = 'C', action='store_true', default = False,
                         help = 'store result in C order')

parser.add_argument ('-F', dest = 'F', action='store_true', default = False,
                         help = 'store result in Fortran order')


args = parser.parse_args (sys.argv[1:])

c_order = args.C
f_order = args.F

inf  = args.infile[0]
outf = args.outfile[0]

if not os.path.exists (inf):
  raise ValueError ('input file does not exist')

tin = np.load (inf, mmap_mode = 'r')
print ("file .........:", inf)
print ("output .......:", outf)
print ("input shape ..:", tin.shape)
print ("input order ..:", 'C' if tin.flags['C_CONTIGUOUS'] else 'F')

if os.path.exists (outf):
  raise ValueError ('output file exists')

if not (c_order or f_order) or c_order and f_order:
  raise ValueError ('(only) one of -C or -F must be given')

neworder      = 'C' if c_order else 'F'
fortran_order = f_order
print ("new order ....:", neworder)

if (c_order and tin.flags['C_CONTIGUOUS']) or (f_order and tin.flags['F_CONTIGUOUS']):
  print ('input file is already in correct order')
  sys.exit (0)

# https://docs.scipy.org/doc/numpy/reference/generated/numpy.memmap.html
# https://stackoverflow.com/questions/4335289/how-can-i-create-a-numpy-npy-file-in-place-on-disk
tout = np.lib.format.open_memmap (outf, 'w+', dtype = tin.dtype, shape = tin.shape, fortran_order = fortran_order)

with Timer (desc = 'writing %s (order: %s) [est. size: %.1f mb]' % (outf, neworder, tin.nbytes / 1024**2)) as t:
  tout[:] = tin[:]
  tout.flush ()


