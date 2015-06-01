from __future__ import print_function
import os, os.path, glob, datetime, shutil, sys
import subprocess as sp
import Tkinter as tk
import tkMessageBox as tkMB
import utils as U

#----------------------------------------------------------------------
#
#  UTILITIES
#

def job_status(jd):
    # Determine the current status of a job given the path to its job
    # directory.

    # If the job directory does not exist, no status can be
    # determined.
    if not os.path.exists(jd): return None

    # If there are not even any namelist files, the job has not yet
    # been configured.
    if not os.path.exists(os.path.join(jd, 'data_genie')): return 'UNCONFIGURED'

    # If there are namelists but no status file written by GENIE, then
    # the job has been configured but not yet run, and is runnable.
    if not os.path.exists(os.path.join(jd, 'status')): return 'RUNNABLE'

    # Otherwise, GENIE will have recorded the job status in the status
    # file.
    with open(os.path.join(jd, 'status')) as fp:
        return fp.readline().strip().split()[0]


status_images = { }
def job_status_img(s):
    # Icon images for different job statuses.
    global status_images
    if not s in status_images:
        p = os.path.join(U.cgenie_root, 'tools', 'images',
                         'status-' + s + '.gif')
        status_images[s] = tk.PhotoImage(file=p)
    return status_images[s]


#----------------------------------------------------------------------
#
#  MAIN JOB CLASS
#

# Class to record information about individual jobs.

class Job:
    def __init__(self, jobdir=None, folder=None):
        # Basic configuration information.
        self.base_config = None
        self.user_config = None
        self.full_config = None
        self.mods = ''
        self.runlen = None
        self.t100 = None
        self.restart = None

        # "Run segments" are used to record different configuration
        # settings used for different periods in a job's run.  For
        # example, if you run a job for a while, pause it, modify some
        # configuration settings then restart the job, there are two
        # run segments, one recording the configuration before the
        # pause and the other (the current segment) recording the
        # configuration after the pause and restart.
        self.segments = []

        # Here, base_jobdir is the base directory for the hierarchy of
        # jobs in which this job lives (this will normally be
        # ~/cgenie-jobs or something like that, but there is the
        # possibility of having multiple job folders to allow for
        # shared libraries of jobs); jobdir is the full path to the
        # job directory; and jobid is the path to the job directory
        # relative to the base job directory, i.e. something that can
        # be used to uniquely identify jobs within a job tree.
        if not jobdir:
            self.jobid = None
            self.base_jobdir = None
            self.jobdir = None
            self.status = None
        else:
            self.base_jobdir = folder.base_path
            self.jobid = os.path.relpath(jobdir, folder.base_path)
            self.jobdir = jobdir
            self.set_status()
            try:
                # Read configuration information for job as written by
                # the new-job script.
                with open(os.path.join(self.jobdir, 'config', 'config')) as fp:
                    for line in fp:
                        ss = line.split(':')
                        k = ss[0].strip()
                        v = ':'.join(ss[1:]).strip()
                        if k == 't100':
                            self.t100 = True if v == 'True' else False
                        elif k == 'run_length':
                            self.runlen = None if v == '?' else int(v)
                        elif k == 'full_config': self.full_config = v
                        elif k == 'base_config': self.base_config = v
                        elif k == 'user_config': self.user_config = v
                        elif k == 'restart': self.restart = v
                modfile = os.path.join(self.jobdir, 'config', 'config_mods')
                if os.path.exists(modfile):
                    with open(modfile) as fp: self.mods = fp.read()
                self.read_segments()
            except Exception as e:
                # Shouldn't get here...
                print('Exception 1:', e)


    def read_segments(self):
        # Read run segment limits information for a job.

        # If it exists, the seglist file in the job's config directory
        # contains simple "<id> <start-step> <end-step>" tuples, one
        # per line, recording the timestep limits for the run segments
        # for the job.
        self.segments = []
        cfgdir = os.path.join(self.jobdir, 'config')
        segfile = os.path.join(cfgdir, 'seglist')
        if os.path.exists(segfile):
            with open(segfile) as fp:
                for l in fp:
                    l = l.split()
                    self.segments.append((int(l[1]), int(l[2])))


    # Convert various bits of job information to printable strings.
    def jobdir_str(self): return self.jobdir if self.jobdir else 'n/a'
    def status_str(self): return self.status if self.status else 'n/a'
    def runlen_str(self): return str(self.runlen) if self.runlen else 'n/a'
    def t100_str(self): return str(self.t100) if self.t100 != None else 'n/a'
    def config_type(self): return 'full' if self.full_config else 'base+user'


    # Make a printable representation of a job, mostly for debugging.
    def __str__(self):
        res = '{ '
        res += 'jobid:' + str(self.jobid) + ' '
        res += 'jobdir:' + str(self.base_jobdir) + ' '
        res += 'dir:' + str(self.jobdir) + ' '
        res += 'base_config:' + str(self.base_config) + ' '
        res += 'user_config:' + str(self.user_config) + ' '
        res += 'full_config:' + str(self.full_config) + ' '
        res += 'restart:' + str(self.restart) + ' '
        res += 'mods:' + str(self.mods) + ' '
        res += 'runlen:' + str(self.runlen) + ' '
        res += 't100:' + str(self.t100) + ' '
        res += 'status:' + str(self.status) + ' '
        res += '}'
        return res


    def write_config(self):
        # Write configuration information for a job.

        # Determine the job's current status.
        self.set_status()

        # Paused or completed jobs may require the creation of a new
        # run segment.
        if self.status == 'PAUSED' or self.status == 'COMPLETE':
            cfgdir = os.path.join(self.jobdir, 'config')
            segdir = os.path.join(cfgdir, 'segments')
            segfile = os.path.join(cfgdir, 'seglist')
            if not os.path.exists(segdir): os.mkdir(segdir)

            # Determine segment information for new segment.
            save_seg = 1
            startk = 1
            endk = int(self.status_params()[1])
            if os.path.exists(segfile):
                with open(segfile) as fp:
                    l = fp.readlines()[-1].split()
                    save_seg = int(l[0]) + 1
                    startk = int(l[2])

            # Make a new run segment if necessary to record the old
            # configuration information.  All of the model
            # configuration files are copied over into a numbered
            # segment directory to allow the GUI to display the
            # history of changes to the job configuration.
            if startk != endk:
                with open(segfile, 'a') as fp:
                    print(save_seg, startk, endk, file=fp)
                self.segments.append((startk, endk))
                segdir = os.path.join(segdir, str(save_seg))
                os.mkdir(segdir)
                for f in ('config', 'base_config', 'user_config',
                          'full_config', 'config_mods'):
                    p = os.path.join(cfgdir, f)
                    if os.path.exists(p): shutil.copy(p, segdir)
        try:
            # Write a configuration file.
            with open(os.path.join(self.jobdir, 'config', 'config'), 'w') as fp:
                if self.base_config:
                    print('base_config_dir:',
                          os.path.join(U.cgenie_data, 'base-configs'), file=fp)
                    print('base_config:', self.base_config, file=fp)
                if self.user_config:
                    print('user_config_dir:',
                          os.path.join(U.cgenie_data, 'user-configs'), file=fp)
                    print('user_config:', self.user_config, file=fp)
                if self.full_config:
                    print('full_config_dir:',
                          os.path.join(U.cgenie_data, 'full-configs'), file=fp)
                    print('full_config:', self.full_config, file=fp)
                if self.restart:
                    print('restart:', self.restart, file=fp)
                modfile = os.path.join(self.jobdir, 'config', 'config_mods')
                if self.mods:
                    with open(modfile, 'w') as mfp: print(self.mods, file=mfp)
                else:
                    if os.path.exists(modfile): os.remove(modfile)
                print('config_date:', str(datetime.datetime.today()), file=fp)
                print('run_length:', self.runlen, file=fp)
                print('t100:', self.t100, file=fp)
        except Exception as e:
            # Shouldn't get here...
            print('Exception 2:', e)


    def gen_namelists(self):
        # Generate GENIE namelists by running new-job script.

        # Location of the new-job script.
        new_job = os.path.join(U.cgenie_root, 'tools', 'new-job.py')

        # Create command line for running new-job.
        cmd = [new_job, '--gui']
        if self.base_config: cmd += ['-b', self.base_config]
        if self.user_config: cmd += ['-u', self.user_config]
        if self.full_config: cmd += ['-c', self.full_config]
        if self.restart: cmd += ['-r', self.restart]
        if self.mods:
            modfile = os.path.join(self.jobdir, 'config', 'config_mods')
            with open(modfile, 'w') as fp: print(self.mods, file=fp)
            cmd += ['-m', modfile]
        cmd += ['-j', self.base_jobdir]
        if self.t100: cmd += ['--t100']
        cmd += [self.jobid, str(self.runlen)]

        # Make sure new-job is run as a Python script!
        cmd = [sys.executable] + cmd
        try:
            with open(os.devnull, 'w') as sink:
                res = sp.check_output(cmd, stderr=sink).strip()
        except Exception as e:
            res = 'ERR:Failed to run new-job script'
        if res != 'OK': tkMB.showerror('Error', res[4:])


    def check_output_files(self):
        # Check for output files that could be plotted.  This is
        # needed because it's not immediately clear from the job
        # configuration what files might be available for plotting,
        # and it's easier just to look and see what GENIE produces and
        # to use these to set up the plotting GUI.

        # At the moment, only BIOGEM time series files are considered
        # for plotting.
        od = os.path.join(self.jobdir, 'output')
        chkds = [os.path.join(od, 'biogem', 'biogem_series_*.res')]

        # Both the file name (key) and the full path to the data file
        # (value) are recorded in a dictionary.
        res = { }
        for g in chkds:
            for f in glob.glob(g): res[os.path.basename(f)] = f
        return res


    def set_status(self, runlen_increased=False):
        # Determine the current status of a job.

        # Most of the time the job status is determined directly by
        # the job_status function.
        self.status = job_status(self.jobdir)

        # The only exception to this is if the job is complete but we
        # are increasing the run length.  In that case, the job needs
        # to be marked as paused at its current time.
        if runlen_increased and self.status == 'COMPLETE':
            sout = ' PAUSED ' + ' '.join(self.status_params()[1:])
            with open(os.path.join(self.jobdir, 'status'), 'w') as fp:
                print(sout, file=fp)
            self.status = 'PAUSED'


    def status_img(self):
        # Return icon image for job status.
        return job_status_img(self.status)


    def pct_done(self):
        # Calculate percentage complete for running jobs for status
        # display.  The current timestep and total number of timesteps
        # are recorded in the GENIE status file for running and paused
        # jobs and these are used to calculate the percentage done.
        if not os.path.exists(os.path.join(self.jobdir, 'status')):
            return None
        with open(os.path.join(self.jobdir, 'status')) as fp:
            ss = fp.readline().strip().split()
            if ss[0] != 'RUNNING' and ss[0] != 'PAUSED': return None
            return 100 * float(ss[1]) / float(ss[2])


    def status_params(self):
        # Return all status parameters written in GENIE status file.
        # For running, paused and complete jobs, the current and total
        # timesteps as well as the "GENIE clock" value are written
        # into the status file to assist in restarting the model after
        # pauses.
        if not os.path.exists(os.path.join(self.jobdir, 'status')):
            return None
        with open(os.path.join(self.jobdir, 'status')) as fp:
            ss = fp.readline().strip().split()
            return ss


    def segment_strs(self):
        # Generate string representations of the job run segments for
        # selection in the setup panel GUI.

        # Read the segment information.
        self.read_segments()

        if len(self.segments) == 0:
            # Default case.
            res = ('1: 1-END',)
        else:
            # Otherwise generate a tuple of strings of the form "<id>:
            # <start>-<end>" recording the timestep ranges for the
            # different run segments.
            res = []
            iseg = 1
            for kstart, kend in self.segments:
                res.append(str(iseg) + ': ' + str(kstart) + '-' + str(kend))
                iseg += 1
            res.append(str(iseg) + ': ' + str(kend + 1) + '-END')
            res.reverse()
            res = tuple(res)
        return res


    def read_segment(self, iseg):
        # Read the configuration information for a given run segment.
        # Here, the configuration information stored for a run segment
        # is read into a dictionary for use in the setup panel GUI to
        # display the history of configuration changes for a job.
        segdir = os.path.join(self.jobdir, 'config', 'segments', str(iseg))
        if not os.path.exists(segdir): return None
        res = { }
        with open(os.path.join(segdir, 'config')) as fp:
            for line in fp:
                ss = line.split(':')
                k = ss[0].strip()
                v = ':'.join(ss[1:]).strip()
                if k == 't100':
                    res['t100'] = True if v == 'True' else False
                elif k == 'run_length':
                    res['runlen'] = None if v == '?' else int(v)
                elif k == 'full_config': res['full_config'] = v
                elif k == 'base_config': res['base_config'] = v
                elif k == 'user_config': res['user_config'] = v
                elif k == 'restart': res['restart'] = v
        modfile = os.path.join(segdir, 'config_mods')
        if os.path.exists(modfile):
            with open(modfile) as fp: res['mods'] = fp.read()
        return res
