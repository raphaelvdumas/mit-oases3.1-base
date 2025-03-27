#! /usr/bin/env python3
#
# Author: Gaute Hope <gaute.hope@nersc.no> / 2016-05-20
#

import os, sys
import argparse
import subprocess
from   concurrent.futures import ThreadPoolExecutor

from ..utils.wtimer import *
from ..utils.progresscounter  import *
from ..utils.streamadapter import StreamAdapter

import numpy as np
import scipy as sc
import scipy.io
import io
import shutil

class ParOast:
  # show less per-job output when number of processes exceed this
  JOB_DEBUG_LIMIT = 32

  def __init__ (self, mpi = False):
    self.mpi = mpi
    self.progbar = True

  def main (self, _args, wd = os.getcwd (), log = None):
    mt = Timer ().tic ()

    self.wd = wd
    self.out = os.path.join (self.wd, 'out')
    self.log = log

    parser = argparse.ArgumentParser ( description = 'Parallel OAST' )
    parser.add_argument ('file', metavar = 'file', type = str, nargs = 1,
                         help = 'OAST .dat file to be split up')
    parser.add_argument ('-d', '--debug',  action='store_true', default = False,
                         help = 'turn on more debug output')
    parser.add_argument ('-w', '--workers', type = int, default = 8, help = 'number of workers (default: 8, use 1 to disable multiprocessing)')

    parser.add_argument ('-l', '--list', action='store_true', default=False,
                        help = 'list jobs and status')

    parser.add_argument ('-g', '--generate',  action='store_true', default = False,
                        help = 'generate job files (default: yes)')

    parser.add_argument ('-r', '--run',  action='store_true', default = False,
        help = 'run job files (use same number of workers as during generation) (default: yes)')


    parser.add_argument ('-j', '--join-plots',  action='store_true', default = False,
        help = 'join plots (use same number of workers as during run) (default: yes)')

    parser.add_argument ('-m', '--memory', action='store_true', default = False,
        help = 'Use memory when possible to avoid double access to filesystem')

    parser.add_argument ('--log', type = str, default = None,
        help = 'log file (default: job.log)')

    parser.add_argument ('--progbar', action = 'store_const',
        default = 'unknown', const = 'yes', dest = 'progbar',
        help = 'force enable progress bar (default: on when stderr is tty)')

    parser.add_argument ('--no-progbar', action = 'store_const',
        const = 'no', dest = 'progbar',
        help = 'disable progress bar')

    parser.epilog = "If neither -g, -r, -l or -j are specified -grj will be specified by default."

    args = parser.parse_args (_args)

    self.file       = args.file[0]
    self.debug      = args.debug
    self.list       = args.list
    self.generate   = args.generate
    self.run        = args.run
    self.progbar    = args.progbar != 'no'

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

    if not ProgressCounter.available:
      self.progbar = False

    print ("paroast:", self.file, file = self.log)

    if not os.path.exists (self.file):
      print ("paroast: file: %s does not exist" % self.file, file = self.log)
      sys.exit (1)

    if self.mpi:
      from mpi4py import MPI
      self.comm = MPI.COMM_WORLD
      rank = self.comm.Get_rank ()
      size = self.comm.Get_size ()

      self.workers = size
      print ("paroast: running on mpi. cpu width:", self.workers, file = self.log)

    else:
      self.workers    = args.workers

    self.join_plots = args.join_plots
    self.memory     = args.memory

    self.jobs = []
    self.jobs_made = False

    self.read_job ()

    if not self.generate and not self.list and not self.run and not self.join_plots:
      self.generate = True
      self.run      = True
      self.join_plots = True

    if self.list:
      self.do_list ()

    if self.generate:
      self.do_generate ()

    if self.run:
      self.do_run ()

    if self.join_plots:
      self.do_join_plots ()

    print ("paroast: done: total duration %s." % mt.toc().pp_duration (), file = self.log)

  def read_job (self):
    with open (self.file, 'r') as fd:
      self.template = fd.readlines ()

    self.template = [t.strip () for t in self.template]

    if self.debug:
      print ("read job: " + '\n'.join(self.template), file = self.log)
    else:
      print ("read job: " + self.template[0], file = self.log)

    if 'Z' in self.template[1]:
      print ("paroast does not support oast runs with the Z option (plotting of the velocity profile)", file = self.log)
      exit (1)

    nl = int (self.template[3])

    self.receiver_line = 5 + nl
    self.receivers = self.template[self.receiver_line].split (' ')

    self.r0 = np.float32(self.receivers[0])
    self.r1 = np.float32(self.receivers[1])
    self.nr = int(self.receivers[2])
    self.dr = np.float32(np.float32((self.r1 - self.r0)) / (self.nr - 1))

    print (file = self.log)
    print ("Receiver sampling:", file = self.log)
    print ("-----------------------------", file = self.log)
    print ("r0 ........: %.25f" % self.r0, file = self.log)
    print ("r1 ........: %.25f" % self.r1, file = self.log)
    print ("nr ........:", self.nr, file = self.log)
    print ("dr ........: %.25f" % self.dr, file = self.log)

    if self.workers > self.nr:
      print ("*= there are more workers than receivers, limiting workers to number of receivers.", file = self.log)
      self.workers = self.nr

    print (file = self.log)
    print (file = self.log)

  def make_jobs (self):
    if self.jobs_made:
      return

    rcvrs = self.r0 + np.arange (0, self.nr, dtype = np.float32) * self.dr

    receivers_per_worker = np.floor(self.nr / self.workers)
    remainder = np.remainder (self.nr, self.workers)
    print ("receivers per worker:", receivers_per_worker, ", rem: ", remainder, file = self.log)

    ir = 0

    for w, rs in enumerate(np.array_split(rcvrs, self.workers)):
      r0 = rs[0]
      r1 = rs[-1]

      print ("job", w, ", r0:", r0, ", r1:", r1, file = self.log)

      self.jobs.append (rs)

    self.jobs_made = True
    print (file = self.log)
    print (file = self.log)

  def do_list (self):
    self.make_jobs ()

  def write_job (self, job, no):

    fname = os.path.basename(self.file)[:-4]
    d = os.path.join (self.out, '%d' % no)
    fname = os.path.join (d, fname + '_%d.dat' % no)

    print ("writing job no:", no, ", fname:", os.path.basename(fname), file = self.log)

    if not os.path.exists (d):
      os.makedirs (d)

    temp = self.template.copy ()
    temp[self.receiver_line] = "0 0 -%d 1" % (len(job))

    rcvrs = " ".join(["%.25f" % j for j in job])
    temp.insert (self.receiver_line + 1, rcvrs)

    with open (fname, 'w') as fd:
      for t in temp:
        fd.write ("%s\n" % t)


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
      progc = ProgressCounter ('RECEIVER NO.', self.nr, [ os.path.join (self.out, str(d), self.file[:-4] + "_" + str(d) + ".log") for d in range (self.workers)])
      progc.start ()

    with Wtimer() as t:
      if self.mpi:
        # send full job data to workers
        for i in range (self.workers - 1):
          job_data = [self.out, i, self.file, self.progbar]

          self.comm.send (job_data, dest = i+1)

        # finally, run last job in this master thread
        self.job_run (self.workers - 1)

        # wait for jobs from workers to finish
        for i in range (self.workers - 1):
          res = self.comm.recv (source = i+1)

      else:
        with ThreadPoolExecutor (max_workers = self.workers) as executor:
          for n,j in enumerate(self.jobs):
            self.job_futures.append (executor.submit (self.job_run, n))

        # catch exceptions
        for j in self.job_futures:
          j.result ()

    if self.progbar:
      progc.stop ()

    print ("=> all jobs done, duration: %s." % t.pp_duration (), file = self.log)
    print (file = self.log)
    print (file = self.log)

  def job_run (self, no):

    path = os.path.join (self.out, '%d' % no)
    fname = os.path.basename(self.file)[:-4]
    name  = fname + '_%d' % no
    fname = os.path.join (path, name + '.dat')
    log_file = os.path.join (path, name + '.log')

    if not self.progbar and self.workers <= self.JOB_DEBUG_LIMIT:
      print ('* running: oast %s' % name, file = self.log)

    with Wtimer() as t:
      # it appears that OASES only outputs to stdout, but maybe fortran
      # outputs something more?

      with open (log_file, 'w') as logf:
        # set up environment for this job and launch oast2 directly
        jenv = os.environ.copy ()
        jenv['FOR001'] = name + '.dat'
        jenv['FOR002'] = name + '.src'
        jenv['FOR019'] = name + '.plp'
        jenv['FOR020'] = name + '.plt'
        jenv['FOR028'] = name + '.cdr'
        jenv['FOR029'] = name + '.bdr'
        jenv['FOR045'] = name + '.rhs'
        jenv['FOR046'] = name + '.vol'

        ## make sure TMPDIR is set in PBS job script
        if os.environ.get ('TMPDIR') is not None:
          jenv['TMPDIR'] = os.environ['TMPDIR']

        # don't buffer stdin/stdout
        # jenv['GFORTRAN_UNBUFFERED_PRECONNECTED'] = 'y'

        p = subprocess.Popen ('time oast2',
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

  def do_join_plots (self):
    print ("=> joining plots..", file = self.log)
    self.make_jobs ()

    fname = os.path.basename(self.file)[:-4]

    plp_fname = os.path.join (self.wd, fname + '.plp')
    plt_fname = os.path.join (self.wd, fname + '.plt')

    print (" writing: %s and %s.." % (os.path.basename(plp_fname), os.path.basename(plt_fname)), file = self.log)
    with Wtimer () as jt:

      if self.memory:
        plp = io.StringIO ()
        plt = io.StringIO ()
      else:
        plp = open (plp_fname, 'w')
        plt = open (plt_fname, 'w')

      first = True

      for no,j in enumerate(self.jobs):
        with Timer () as jjt:
          path = os.path.join (self.out, '%d' % no)
          name  = fname + '_%d' % no

          m_plp_fname = os.path.join (path, name + '.plp')
          m_plt_fname = os.path.join (path, name + '.plt')

          print ("=> reading job:", no, ", files:", os.path.basename(m_plp_fname), "and", os.path.basename(m_plt_fname), end = ("" if (not self.debug) else None), flush = True, file = self.log)

          if not os.path.exists (m_plp_fname) or not os.path.exists (m_plt_fname):
            print (" files do not exist.", file = self.log)
            exit (1)

          # concatenate output from all files onto combined output files
          with Wtimer() as t:
            with open (m_plp_fname, 'r') as m_plp:
              if not first:
                # skip header
                m_plp.readline ()

              for l in m_plp.readlines ():
                if not 'PLTEND' in l:
                  plp.write (l)
            if self.debug:
              print ("D: Write PLP: %s" % t.pp_duration (), file = self.log)

          with Wtimer() as t:
            with open (m_plt_fname, 'r') as m_plt:
              shutil.copyfileobj (m_plt, plt, -1)

            if self.debug:
              print ("D: Write PLT: %s" % t.pp_duration (), file = self.log)

          first = False

          if not self.debug:
            print (" (duration: %s)" % jjt.pp_duration (), file = self.log)
          else:
            print ("D: total duration: %s" % jjt.pp_duration (), file = self.log)

      # write end
      plp.write (" OASTL PLTEND")

      if self.memory:
        with Timer() as wtt:
          print ("*= writing buffered output to disk.. ", end = "", flush = True, file = self.log)

          with open (plp_fname, 'w') as plpf:
            plp.seek (0)
            shutil.copyfileobj (plp, plpf)
            plp.close () # frees StringIO

          with open (plt_fname, 'w') as pltf:
            plt.seek (0)
            shutil.copyfileobj (plt, pltf)
            plt.close () # frees StringIO

        print ("(duration: %s)" % wtt.pp_duration (), file = self.log)

      else:
        plp.close ()
        plt.close ()


    print ("*= saved result to: %s and %s.. (duration: %s)" % (os.path.basename (plp_fname), os.path.basename(plt_fname), jt.pp_duration ()), file = self.log)



if __name__ == '__main__':
  p = ParOast ()

  p.main (sys.argv[1:])


