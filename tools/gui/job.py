from __future__ import print_function
import os, os.path, glob, datetime, shutil, sys
import subprocess as sp
import Tkinter as tk
import tkMessageBox as tkMB
import utils as U

def job_status(jd):
    if not os.path.exists(jd): return None
    if not os.path.exists(os.path.join(jd, 'data_genie')): return 'UNCONFIGURED'
    if not os.path.exists(os.path.join(jd, 'status')): return 'RUNNABLE'
    with open(os.path.join(jd, 'status')) as fp:
        return fp.readline().strip().split()[0]

status_images = { }
def job_status_img(s):
    global status_images
    if not s in status_images:
        p = os.path.join(U.cgenie_root, 'tools', 'images',
                         'status-' + s + '.gif')
        status_images[s] = tk.PhotoImage(file=p)
    return status_images[s]

class Job:
    def __init__(self, jobdir=None, folder=None):
        self.runlen = None
        self.t100 = None
        self.base_config = None
        self.user_config = None
        self.full_config = None
        self.mods = ''
        self.segments = []
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
                modfile = os.path.join(self.jobdir, 'config', 'config_mods')
                if os.path.exists(modfile):
                    with open(modfile) as fp: self.mods = fp.read()
                cfgdir = os.path.join(self.jobdir, 'config')
                segfile = os.path.join(cfgdir, 'seglist')
                ### ===> TODO: read all segment data here
                if os.path.exists(segfile):
                    with open(segfile) as fp:
                        for l in fp:
                            l = l.split()
                            self.segments.append((int(l[1]), int(l[2])))
            except Exception as e:
                ### ===> TODO: better exception handling
                print('Exception 1:', e)

    def jobdir_str(self): return self.jobdir if self.jobdir else 'n/a'
    def status_str(self): return self.status if self.status else 'n/a'
    def runlen_str(self): return str(self.runlen) if self.runlen else 'n/a'
    def t100_str(self): return str(self.t100) if self.t100 != None else 'n/a'
    def config_type(self): return 'full' if self.full_config else 'base+user'

    def __str__(self):
        res = '{ '
        res += 'jobid:' + str(self.jobid) + ' '
        res += 'jobdir:' + str(self.base_jobdir) + ' '
        res += 'dir:' + str(self.jobdir) + ' '
        res += 'base_config:' + str(self.base_config) + ' '
        res += 'user_config:' + str(self.user_config) + ' '
        res += 'full_config:' + str(self.full_config) + ' '
        res += 'mods:' + str(self.mods) + ' '
        res += 'runlen:' + str(self.runlen) + ' '
        res += 't100:' + str(self.t100) + ' '
        res += 'status:' + str(self.status) + ' '
        res += '}'
        return res

    def write_config(self):
        self.set_status()
        if self.status == 'PAUSED' or self.status == 'COMPLETE':
            cfgdir = os.path.join(self.jobdir, 'config')
            segdir = os.path.join(cfgdir, 'segments')
            segfile = os.path.join(cfgdir, 'seglist')
            if not os.path.exists(segdir): os.mkdir(segdir)
            save_seg = 1
            startk = 1
            endk = self.status_params()[1]
            if os.path.exists(segfile):
                with open(segfile) as fp:
                    l = fp.readlines()[-1].split()
                    save_seg = int(l[0]) + 1
                    startk = int(l[2])
            with open(segfile, 'a') as fp:
                print(save_seg, startk, endk, file=fp)
            segdir = os.path.join(segdir, str(save_seg))
            os.mkdir(segdir)
            for f in ('base_config', 'user_config',
                      'full_config', 'config_mods'):
                p = os.path.join(cfgdir, f)
                if os.path.exists(p): shutil.copy(p, segdir)
            ### ===> TODO: update segment information member variables
        try:
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
                modfile = os.path.join(self.jobdir, 'config', 'config_mods')
                if self.mods:
                    with open(modfile, 'w') as mfp: print(self.mods, file=mfp)
                else:
                    if os.path.exists(modfile): os.remove(modfile)
                print('config_date:', str(datetime.datetime.today()), file=fp)
                print('run_length:', self.runlen, file=fp)
                print('t100:', self.t100, file=fp)
        except Exception as e:
            ### ===> TODO: better exception handling
            print('Exception 2:', e)

    def gen_namelists(self):
        new_job = os.path.join(U.cgenie_root, 'tools', 'new-job.py')
        cmd = [new_job, '--gui']
        if self.base_config: cmd += ['-b', self.base_config]
        if self.user_config: cmd += ['-u', self.user_config]
        if self.full_config: cmd += ['-c', self.full_config]
        if self.mods:
            modfile = os.path.join(self.jobdir, 'config', 'config_mods')
            with open(modfile, 'w') as fp: print(self.mods, file=fp)
            cmd += ['-m', modfile]
        cmd += ['-j', self.base_jobdir]
        if self.t100: cmd += ['--t100']
        cmd += [self.jobid, str(self.runlen)]
        cmd = [sys.executable] + cmd
        try:
            with open(os.devnull, 'w') as sink:
                res = sp.check_output(cmd, stderr=sink).strip()
        except Exception as e:
            res = 'ERR:Failed to run new-job script'
        if res != 'OK': tkMB.showerror('Error', res[4:])

    def check_output_files(self):
        od = os.path.join(self.jobdir, 'output')
        chkds = [os.path.join(od, 'biogem', 'biogem_series_*.res')]
        res = { }
        for g in chkds:
            for f in glob.glob(g): res[os.path.basename(f)] = f
        return res

    def set_status(self):
        self.status = job_status(self.jobdir)

    def status_img(self):
        return job_status_img(self.status)

    def pct_done(self):
        if not os.path.exists(os.path.join(self.jobdir, 'status')):
            return None
        with open(os.path.join(self.jobdir, 'status')) as fp:
            ss = fp.readline().strip().split()
            if ss[0] != 'RUNNING' and ss[0] != 'PAUSED': return None
            return 100 * float(ss[1]) / float(ss[2])

    def status_params(self):
        if not os.path.exists(os.path.join(self.jobdir, 'status')):
            return None
        with open(os.path.join(self.jobdir, 'status')) as fp:
            ss = fp.readline().strip().split()
            return ss
