#! /usr/bin/env python3
#
# Author: Gaute Hope <gaute.hope@nersc.no> / 2016-05-02
#

import os, sys
import argparse
import subprocess
from   concurrent.futures   import ThreadPoolExecutor
import tempfile

from ..oasp                   import oasp
from ..utils.progresscounter  import *
from ..utils.wtimer           import *
from ..utils.streamadapter    import StreamAdapter

import numpy as np
import scipy as sc
import scipy.io

class ParOasp:
  # show less per-job output when number of processes exceed this
  JOB_DEBUG_LIMIT = 32

  def __init__ (self, mpi = False):
    self.mpi      = mpi
    self.progbar  = True


  def main (self, _args, wd = os.getcwd (), log = None):
    mt = Timer ().tic ()

    self.wd     = wd
    self.out    = os.path.join (self.wd, 'out')

    parser = argparse.ArgumentParser ( description = 'Parallel OASP' )
    parser.add_argument ('file', metavar = 'file', type = str, nargs = 1,
                         help = 'OASP .dat file to be split up')

    parser.add_argument ('-d', '--debug',  action='store_true', default = False,
                         help = 'turn on more debug output')

    parser.add_argument ('-w', '--workers', type = int, default = 8, help = 'number of workers (default: 8, use 1 to disable multiprocessing)')

    parser.add_argument ('-l', '--list', action='store_true', default=False,
                        help = 'list jobs and status')

    parser.add_argument ('-g', '--generate',  action='store_true', default = False,
                        help = 'generate job files (default: yes)')

    parser.add_argument ('-r', '--run',  action='store_true', default = False,
        help = 'run job files (use same number of workers as during generation) (default: yes)')


    parser.add_argument ('-t', '--transfer',  action='store_true', default = False,
        help = 'make transfer function (use same number of workers as during run) (default: yes)')

    parser.add_argument ('-3', '--3d', dest = 'oasp3d', action = 'store_true', default = False,
        help = 'Input file is for OASP3D')

    parser.add_argument ('-2', '--2d', dest = 'rdoasp', action = 'store_true', default = False,
        help = 'Input file is for RDOASP')

    parser.add_argument ('--trf', action = 'store_true', default = False,
        help = 'store transfer function in .trf file (default)')

    parser.add_argument ('--mat', action = 'store_true', default = False,
        help = 'store transfer function in .mat file')

    parser.add_argument ('--npz', action = 'store_true', default = False,
        help = 'store transfer function in .npz file')

    parser.add_argument ('--npy', action = 'store_true', default = False,
        help = 'store transfer function in .npy file with metadata in .npz file (allows directly MemoryMapping output which is useful for gigantic TRFs)')

    parser.add_argument ('--split-transfer', type = int, default = 1,
        help = 'split large transfer functions (across depth) (only for --npz)')

    parser.add_argument ('--log', type = str, default = None,
        help = 'log file (default: job.log)')

    parser.add_argument ('--progbar', action = 'store_const',
        default = 'unknown', const = 'yes', dest = 'progbar',
        help = 'force enable progress bar (default: on when stderr is tty)')

    parser.add_argument ('--no-progbar', action = 'store_const',
        const = 'no', dest = 'progbar',
        help = 'disable progress bar')

    parser.add_argument ('--unbuffer', action = 'store_true', default = False,
        help = 'Unbuffer output from OASES')

    parser.epilog = "If neither -g, -l, -r or -t are specified, -grt will be specified by default."
    args = parser.parse_args (_args)

    self.file       = args.file[0]
    self.debug      = args.debug
    self.list       = args.list
    self.generate   = args.generate
    self.run        = args.run
    self.transfer   = args.transfer
    self.split_transfer = args.split_transfer
    self.oasp3d     = args.oasp3d
    self.rd         = args.rdoasp

    self.progbar    = args.progbar != 'no'
    self.unbuffer   = args.unbuffer

    if log is not None:
      self.log = log

      # disable progressbar if a single log file not including sys.stdout is specified
      if self.log is not sys.stdout and args.progbar != 'yes':
        self.progbar = False

    else:
      if args.log is not None:
        self.logf = open (args.log, 'w')
      else:
        self.logf = open(self.file[:-4] + ".log", 'w')

      self.log = StreamAdapter ([sys.stdout, self.logf])

    if not sys.stderr.isatty () and args.progbar != 'yes':
      self.progbar = False

    if self.rd and self.oasp3d:
      print ("parsoasp: cannot both specify -2 and -3", file = self.log)
      sys.exit (1)

    if not ProgressCounter.available:
      self.progbar = False

    if not os.path.exists (self.file):
      print ("paroasp: file: %s does not exist" % self.file, file = self.log)
      sys.exit (1)

    if not self.generate and not self.list and not self.run and not self.transfer:
      self.generate = True
      self.run      = True
      self.transfer = True

    self.transfer_type = 'trf'

    if not self.transfer and (args.mat or args.npz or args.trf or args.npy):
      print ("paroasp: --trf, --mat or --npz requires -t.", file = self.log)
      sys.exit (1)

    frmt = np.array([args.mat, args.trf, args.npz, args.npy])

    if np.sum (frmt) > 1:
      print ("paroasp: only one of --trf, --mat, --npz and --npy can be specified.", file = self.log)
      sys.exit (1)

    elif args.trf:
      self.transfer_type = 'trf'

    elif args.mat:
      self.transfer_type = 'mat'

    elif args.npz:
      self.transfer_type = 'npz'

    elif args.npy:
      self.transfer_type = 'npy'

    if self.split_transfer != 1 and self.transfer_type != 'npz':
      print ("paroasp: --split-transfer can only be used with --npz", file = self.log)
      sys.exit (1)

    if self.mpi:
      from mpi4py import MPI
      self.comm = MPI.COMM_WORLD
      rank = self.comm.Get_rank ()
      size = self.comm.Get_size ()

      self.workers = size
      print ("paroasp: running on mpi. cpu width:", self.workers, file = self.log)

    else:
      self.workers = args.workers


    self.jobs = []
    self.jobs_made = False

    self.read_job ()


    if self.list:
      self.do_list ()

    if self.generate:
      self.do_generate ()

    if self.run:
      self.do_run ()

    if self.transfer:
      self.do_transfer ()

    print ("paroasp: done: total duration %s." % mt.toc ().pp_duration (), file = self.log)

  def read_job (self):
    with open (self.file, 'r') as fd:
      self.template = fd.readlines ()

    # WARNING: We look for the last non-empty line and assume it is the frequency line,
    #          other stuff below it is not supported.
    self.template = [t.strip () for t in self.template if len(t.strip()) > 0]

    if self.debug:
      print ("read job: " + '\n'.join(self.template), file = self.log)
    else:
      print ("read job: " + self.template[0], file = self.log)

    self.opts = self.template[1]
    self.freq_line = self.template[2]

    if '|' in self.opts:
      print ("ERROR: '|' should not be specified in the input file, it will automatically be added.")
      sys.exit (1)

    self.opts += ' |'

    _cmin_l = -3
    _wvn_l  = -2
    _freq_l = -1

    def strip (l):
      e = l.find ('#')
      if e != -1:
        l = l[:e]
      return l.strip ()

    def strip_split (l):
      return strip(l).split ()

    # cmin cmax line
    cpp = strip_split (self.template[_cmin_l])
    self.cmin = np.float32(cpp[0])
    self.cmax = np.float32(cpp[1])

    # wavenumber line
    wvn = strip_split (self.template[_wvn_l])
    self.nwvno = int (wvn[0])

    # frequency / time line
    self.frequencies = strip_split (self.template[_freq_l])
    self.frequencies = [f for f in self.frequencies if len(f.strip()) > 0]

    self.nt  = int(self.frequencies[0])
    self.fr1 = float(self.frequencies[1])
    self.fr2 = float(self.frequencies[2])
    self.dt  = np.float32(self.frequencies[3])
    self.r1  = self.frequencies[4]
    self.dr  = self.frequencies[5]
    self.nr  = self.frequencies[6]

    print (file = self.log)
    print ("Frequency and range sampling:", file = self.log)
    print ("-----------------------------", file = self.log)
    print ("nt, time samples ....:", self.nt, file = self.log)
    print ("f_1 .................:", self.fr1, file = self.log)
    print ("f_2 .................:", self.fr2, file = self.log)
    print ("dt ..................:", self.dt, file = self.log)
    print ("r1 ..................:", self.r1, file = self.log)
    print ("dr ..................:", self.dr, file = self.log)
    print ("nr ..................:", self.nr, file = self.log)
    print ("cmin ................:", self.cmin, file = self.log)
    print ("cmax ................:", self.cmax, file = self.log)
    print ("range dependent .....:", self.rd, file = self.log)
    print ("oasp 3d .............:", self.oasp3d, file = self.log)

    # Fortran OASES generally uses 32 bit floats. Reducing the python floats to 32 bit
    # results in the exact same numbers as OASES uses. Make sure that when writing
    # decimals through .DAT files, a high enough precission is used so that it reduces
    # to the same 32-bit float.

    # unoasp22: 196
    assert (self.dt < 2.5 / self.fr2), 'dt must be less than (2.5 / fr2)'
    assert np.log2(self.nt).is_integer(), 'nt must be a power of 2'

    # oases: src/unoasp22.f:238
    self.df = np.float32(1. / (self.dt * self.nt))
    print ("df ..................: %.50f" % self.df, file = self.log)

    # use oases way of calculating frequency samples
    self.mx = np.rint (self.fr2 / self.df) + 1
    lx      = np.rint (self.fr1 / self.df) + 1

    print ("lx ..................:", lx, file = self.log)
    print ("mx ..................:", self.mx, file = self.log)


    self.lxp1 = int (max (2, lx))

    self.freq0 = np.float32(self.df * (self.lxp1 - 1))
    self.freq1 = np.float32(self.df * (self.mx - 1))

    self.fs = self.df * (np.arange (self.lxp1, self.mx + 1, dtype = np.float32) - 1)
    print (self.fs)

    print ("lxp1 ................:", self.lxp1, file = self.log)
    print ("f_1 (calculated).....: %.33f" % self.freq0, file = self.log)
    print ("f_2 (calculated).....: %.33f" % self.freq1, file = self.log)

    # this stuff is hard to make sense of in oases: rather making sure that the condition does not happen

    # lx = int (max (lx, 1))
    # mx = int (min (mx, self.nt / 2))
    assert lx >= 1, 'first frequency too low'
    assert self.mx <= (self.nt / 2), 'mx must be less than (nt / 2): dt too high compared to max frequency (sample rate must satisify Nyquist criteria).'
    assert (self.fr2 - self.fr1) > self.df, 'frequency range less than frequency delta (1 / (dt * nt))'

    # adjusting unused frequency line to give a hint to the job about what
    # the maximum frequency.
    f = self.freq_line.split ()
    self.fc = np.float32(f[0])
    f[0] = '%.18f' % self.freq1
    self.freq_line = ' '.join (f)

    self.wk0    = 2 * np.float32(np.pi) * self.freq1 / self.cmax
    self.wkmax  = 2 * np.float32(np.pi) * self.freq1 / self.cmin
    self.dlwvno = np.float32(self.wkmax - self.wk0)  / np.float32(self.nwvno - 1)

    print ("f_c .................:", self.fc, file = self.log)
    print ("wk0 .................:", self.wk0, file = self.log)
    print ("wkmax ...............:", self.wkmax, file = self.log)
    print ("nwvno ...............:", self.nwvno, file = self.log)
    print ("dlwvno ..............: %.33f" % self.dlwvno, file = self.log)

    # self.fs = np.arange (self.fr1, self.fr2 + self.df, self.df)

    self.nf = len (self.fs)
    assert self.nf == (self.mx - self.lxp1 + 1), 'frequency numbers do not match'

    print ("nf ..................:", self.nf, file = self.log)

    print (file = self.log)
    print (file = self.log)

  def make_jobs (self):
    if self.jobs_made:
      return

    freqs_per_worker = np.floor(self.nf / self.workers)
    remainder = np.remainder (self.nf, self.workers)
    print ("frequencies per worker:", int(freqs_per_worker), ", rem: ", remainder, file = self.log)

    kf = self.lxp1 - 1
    for w in range(self.workers):
      nf = freqs_per_worker
      if remainder > 0:
        nf += 1
        remainder -= 1

      f1  = np.float32 ( self.df * kf )

      # OASES adds one frequency, so if we want to support the case of just computing
      # one frequency for each thread we have to treat it specially by setting the upper
      # frequency equal to the first.

      if nf == 1:
        f2 = f1
      else:
        f2  = np.float32 ( self.df * (kf + nf - 1))

      if nf > 1 and (f2 - f1) < self.df:
        # the difference between start and end frequency sometimes ends up in less than
        # 'df' inside oasp, in this case oasp sets the upper frequency number to the lower. by
        # increasing f2 so that it rounds to the same number, but the difference is greater than df
        # this is avoided.
        f2 += np.float32(self.df * .25)

      _nf = int((f2 - f1) / self.df  + 1)

      # check that f2 and f1 back calculation to frequency number rounds off to greater difference
      # than df
      _mx = np.rint (f2 / self.df) +1
      _lx = np.rint (f1 / self.df) +1

      assert _lx >= 2
      assert _mx <= (self.nt / 2)

      assert nf == 1 or (f2-f1) >= self.df
      assert (f2-f1) < ((nf+1)*self.df)

      kf = kf + nf

      self.jobs.append ([f1, f2])
      print ("job", w, ", f1: %.18f" % f1, ", f2: %.18f" % f2, ", nf =", _nf , file = self.log)

    assert kf == self.mx, 'did not reach last frequency'
    assert remainder == 0, 'left-over jobs'

    self.jobs_made = True
    print (file = self.log)
    print (file = self.log)

  def do_list (self):
    self.make_jobs ()
    pass

  def write_job (self, job, no):

    fname = os.path.basename(self.file)[:-4]
    d = os.path.join (self.out, '%d' % no)
    fname = os.path.join (d, fname + '_%d.dat' % no)

    if self.debug:
      print ("writing job no:", no, ", freqs:", job, ", fname:", os.path.basename (fname), file = self.log)

    if not os.path.exists (d):
      os.makedirs (d)

    with open (fname, 'w') as fd:
      fd.write (self.template[0] + '\n')    # title
      fd.write (self.opts + '\n')           # opts with '|' option
      fd.write (self.freq_line + '\n')      # frequency with FREQM for the '|' option

      for t in self.template[3:-1]:
        fd.write ("%s\n" % t)

      # write frequency line
      # e.g.: 870.00640869140625
      fd.write ("%d %.35f %.35f %.35f %s %s %s\n" % (self.nt, job[0], job[1], self.dt, self.r1, self.dr, self.nr))

      fd.write ("\n\n\n") # three newlines, TODO: these probably mean something.


  def do_generate (self):
    if not os.path.exists (self.out):
      os.makedirs (self.out)

    self.make_jobs ()

    for e,j in enumerate(self.jobs):
      self.write_job (j, e)

    print (file = self.log)
    print (file = self.log)

  def do_run (self):
    # run jobs
    print ('=> launching jobs (max workers =', self.workers, ')', file = self.log)
    self.job_futures = []

    if self.progbar:
      progc = ProgressCounter ('FREQ. NO.', self.nf, [ os.path.join (self.out, str(d), self.file[:-4] + "_" + str(d) + ".log") for d in range (self.workers)])
      progc.start ()

    result = True
    with Wtimer() as t:
      if self.mpi:
        # send full job data to workers
        for i in range (self.workers - 1):
          job_data = [self.out, i, self.file, self.progbar, self.rd, self.oasp3d, self.unbuffer]

          self.comm.send (job_data, dest = i+1)

        if not self.progbar:
          print ('=> all jobs launched, waiting.. ', file = self.log) # not quite true

        # run last job in this thread
        result = self.job_run (self.workers - 1) and result

        # wait for jobs to finish
        for i in range (self.workers -1):
          result = self.comm.recv (source = i+1) and result

      else:
        with ThreadPoolExecutor (max_workers = self.workers) as executor:
          for n,j in enumerate(self.jobs):

            self.job_futures.append (executor.submit (self.job_run, n))

        # catch exceptions
        for j in self.job_futures:
          result = j.result () and result

    if self.progbar:
      progc.stop ()

    if result:
      print ("=> all jobs done, duration: %s." % t.pp_duration (), file = self.log)
    else:
      print ("=> one or more jobs failed! duration: %s." % t.pp_duration (), file = self.log)

    print (file = self.log)
    print (file = self.log)

  def job_run (self, no):
    path = os.path.join (self.out, '%d' % no)
    fname = os.path.basename(self.file)[:-4]
    name  = fname + '_%d' % no
    fname = os.path.join (path, name + '.dat')
    log_file = os.path.join (path, name + '.log')

    if not self.progbar and self.workers <= self.JOB_DEBUG_LIMIT:
      print ('+> running: oasp job: %d..' % no, file = self.log)

    with Wtimer() as t:
      # it appears that OASES only outputs to stdout, but maybe fortran
      # outputs something more?

      with open (log_file, 'w') as logf:
        # set up environment for this job and launch oasp2 directly
        jenv = os.environ.copy ()
        jenv['FOR001'] = name + '.dat'
        jenv['FOR002'] = name + '.src'
        jenv['FOR019'] = name + '.plp'
        jenv['FOR020'] = name + '.plt'
        jenv['FOR022'] = name + '.bot'
        jenv['FOR028'] = name + '.cdr'
        jenv['FOR029'] = name + '.bdr'
        jenv['FOR045'] = name + '.rhs'
        jenv['FOR046'] = name + '.vol'

        ## make sure TMPDIR is set in PBS job script
        if os.environ.get ('TMPDIR') is not None:
          jenv['TMPDIR'] = os.environ['TMPDIR']

        ## don't buffer stdin/stdout:
        # if self.progbar:
        if self.unbuffer:
          jenv['GFORTRAN_UNBUFFERED_PRECONNECTED'] = 'y'

        if self.oasp3d:
          _exec = 'oasp3d'
        elif self.rd:
          _exec = 'rdoasp2'
        else:
          _exec = 'oasp2'

        p = subprocess.Popen ('time %s' % _exec,
            env = jenv,
            cwd = path,
            stdout = logf,
            stderr = subprocess.STDOUT,
            shell = True,
            bufsize = 0
            )

        p.wait ()

    if not self.progbar and self.workers <= self.JOB_DEBUG_LIMIT:
      print ('= job done (%s) (duration: %s).' % (name, t.pp_duration()), file = self.log)

    return (p.returncode == 0)

  def do_transfer (self):
    print ("=> making transfer function..", file = self.log)
    self.make_jobs ()

    T = None
    fstart = 0

    F = np.empty((0,), dtype = np.float32)

    with Timer () as t:

      with Timer () as t_r:
        freqs_per_worker = np.floor(self.nf / self.workers)
        remainder = np.remainder (self.nf, self.workers)

        for no,j in enumerate(self.jobs):
          path = os.path.join (self.out, '%d' % no)
          fname = os.path.basename(self.file)[:-4]
          name  = fname + '_%d' % no
          fname = os.path.join (path, name + '.trf')

          if self.debug:
            ofname = fname
          else:
            ofname = os.path.basename (fname)

          print ("=> reading job:", no, ", file:", ofname, "..", end = '', flush = True, file = self.log)

          if not os.path.exists (fname):
            print ("file does not exist.", file = self.log)
            exit (1)

          trf, sd, z, rr, f, fc, omegaim, nfft, dt, nout, header, icdr, msuft, isrow, inttyp, num_z = oasp.read_trf (fname, self.debug)

          if T is None:
            if self.transfer_type == 'npy':
              # directly memorymap final result to npy file, metadata will be stored in .npz file below
              fname = self.file[:-4] + ".npy"
              T = np.lib.format.open_memmap (fname, mode = 'w+', dtype = 'complex64', fortran_order = True, shape = (int(nout[0]), int(len(z)), int(len(rr)), int(msuft.squeeze()), int(self.nf)))
            else:
              try:
                T = np.empty ((nout[0], len(z), len(rr), msuft.squeeze(), self.nf), dtype = 'complex64', order = 'F')

              except MemoryError:
                # use memmapped tempfile
                # TODO: use open_memmap to store directly to npy file, so that we only need to write
                # metadata afterwards.
                print ("!! MemoryError: trying to memorymap final transfer function (order F).", file = self.log, flush = True)

                _T_tmpfile = tempfile.TemporaryFile () # is cleaned up on GC automatically
                T = np.memmap (_T_tmpfile, mode = 'w+', shape = (nout[0], len(z), len(rr), msuft.squeeze(), self.nf), dtype = 'complex64', order = 'F')

            # print ("Full T:", T.shape, file = self.log)

          print ("read %d frequencies." % len(f), file = self.log)

          expected_nf = freqs_per_worker
          if remainder > 0:
            expected_nf += 1
            remainder   -= 1

          assert len (f) == expected_nf, "wrong number of frequencies in sub-job, did you specify the correct number of jobs (-w)?"
          assert (f[0] == j[0] and f[-1] == j[1]) or (f[0] == j[0] and f[-1] == j[1] - np.float32(.25 * self.df)), "incorrect frequency limits in sub-job"

          T[:, :, :, :, fstart:(fstart+trf.shape[4])] = trf

          fstart = fstart + len(f)

          F = np.concatenate ((F,f))

          del trf, f

      print ("*= read all transfer functions, duration: ", t_r.pp_duration (), file = self.log)

      with Timer () as t_w:
        print ("*= storing transfer function.. [", self.transfer_type, "]: T size ~= %.2f mb" % (T.nbytes / 1024 / 1024), file = self.log, flush = True)

        if self.transfer_type == 'trf':
          fname = self.file[:-4] + ".trf"
          oasp.write_trf (fname, T, sd, z, rr, F, self.fc, omegaim, nfft, dt, nout, header, icdr, msuft, isrow, inttyp, num_z, self.debug)

        elif self.transfer_type == 'mat':

          fname = self.file[:-4] + ".mat"
          sc.io.savemat (fname, { 'trf' : T, 'f' : F, 'sd' : sd, 'z' : z, 'range' : rr, 'fc' : fc, 'omegim' : omegaim, 'nfft' : nfft, 'dt' : dt, 'nout' : nout })

        elif self.transfer_type == 'npy':
          # transfer function was stored in memmap above, now flushing, and storing metadata
          T.flush ()
          del T
          fname = self.file[:-4] + ".npz"
          np.savez (fname, f = F, sd = sd, z = z, range = rr, fc = fc, omegim = omegaim, nfft = nfft, dt = dt, nout = nout)

        elif self.transfer_type == 'npz':

          if self.split_transfer == 1:
            fname = self.file[:-4] + ".npz"
            np.savez (fname, trf = T, f = F, sd = sd, z = z, range = rr, fc = fc, omegim = omegaim, nfft = nfft, dt = dt, nout = nout)
          else:
            # split across depth
            n = int(np.trunc(len(z) / self.split_transfer))
            for i in range(self.split_transfer):
              fname = self.file[:-4] + "_%d.npz" % i

              z0 = i * n
              z1 = (i+1) * n + 1
              if i == (self.split_transfer-1):
                z1 = len(z)

              print ("*= storing split transfer function: %d / %d: %s" % (i+1, self.split_transfer, fname), file = self.log, flush = True)

              np.savez (fname, trf = T[z0:z1,:,:], f = F, sd = sd, z = z[z0:z1], range = rr, fc = fc, omegim = omegaim, nfft = nfft, dt = dt, nout = nout)

        print ("*= wrote transfer function, duration :", t_w.pp_duration (), file = self.log)


    if self.split_transfer == 1:
      print ("*= transfer function saved:", fname, ", duration: %s." % t.pp_duration (), file = self.log)
    else:
      print ("*= transfer function saved, duration: %s." % t.pp_duration (), file = self.log)


