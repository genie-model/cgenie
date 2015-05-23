#!/usr/bin/env python2

from __future__ import print_function
import os, os.path, shutil, re, datetime, glob
import subprocess as sp
import platform as plat
import Tkinter as tk
import tkSimpleDialog as tkSD
import tkMessageBox as tkMB
import tkFont
import ttk

import utils as U
import gui_utils as G


# GENIE configuration

if not U.read_cgenie_config():
    sys.exit('GENIE not set up: run the setup-cgenie script!')


# Platform setup, including any runtime environment variables needed
# to control model output buffering.

platform = U.discover_platform()
execfile(os.path.join(U.cgenie_root, 'platforms', platform))
if 'runtime_env' in locals():
    for k, v in locals()['runtime_env'].iteritems(): os.environ[k] = v


#----------------------------------------------------------------------

class JobFolder:
    """Job folder management"""
    def __init__(self, path, name, tree):
        self.path = path
        self.name = name
        self.tree = tree
        self.item = None
        self.folders = { path: 1 }
        self.status = { }

    def possible_folders(self):
        fs = self.folders.keys()
        fs.sort()
        return fs

    def scan(self, select):
        self.item = self.tree.insert('', 'end', self.path,
                                     text=self.name, open=True)
        for p, type in G.walk_jobs(self.path):
            if type == 'JOB':
                self.add_job(os.path.relpath(p, self.path))
            else:
                self.add_folder(os.path.relpath(p, self.path))
        self.sort_children(self.item)
        if select: self.tree.selection_set(self.item)

    def add_job(self, jfull, sort=False):
        ds, j = G.job_split(jfull)
        p = self.path
        for f in ds:
            parent = p
            p = os.path.join(p, f)
            if not self.tree.exists(p):
                self.folders[p] = 1
                self.tree.insert(parent, 'end', p, text=f,
                                 image=G.status_img('FOLDER'))
        jpath = os.path.join(self.path, jfull)
        self.status[jfull] = G.job_status(jpath)
        self.tree.insert(p, 'end', jpath, text=j,
                         image=G.status_img(self.status[jfull]))
        if sort: self.sort_children(self.item)

    def add_folder(self, ffull, sort=False):
        ds, j = G.job_split(os.path.join(ffull, 'DUMMY'))
        p = self.path
        for f in ds:
            parent = p
            p = os.path.join(p, f)
            if not self.tree.exists(p):
                self.folders[p] = 1
                self.tree.insert(parent, 'end', p, text=f,
                                 image=G.status_img('FOLDER'))
        if sort: self.sort_children(self.item)

    def delete(self, p):
        self.tree.delete(p)
        if p in self.folders: del self.folders[p]
        if p in self.status: del self.status[p]

    def move(self, fr, to):
        os.rename(fr, to)
        is_folder = fr in self.folders
        to = os.path.relpath(to, self.path)
        self.tree.delete(fr)
        if is_folder:
            self.add_folder(to, True)
        else:
            self.add_job(to, True)

    def clone(self, fr, to):
        shutil.copytree(fr, to)
        to = os.path.relpath(to, self.path)
        self.add_job(to, True)

    def sort_children(self, f):
        def chcmp(x, y):
            fx = G.is_folder(self.tree, x)
            fy = G.is_folder(self.tree, y)
            if fx == fy: return cmp(x, y)
            else:
                if fx: return -1
                else: return 1
        cs = list(self.tree.get_children(f))
        cs.sort(chcmp)
        self.tree.set_children(f, *cs)
        for c in cs: self.sort_children(c)


#----------------------------------------------------------------------

class Job:
    def __init__(self, jobid=None, folder=None):
        self.runlen = None
        self.t100 = None
        self.base_config = None
        self.user_config = None
        self.full_config = None
        self.mods = None
        self.modules = None
        if not jobid:
            self.jobid = None
            self.jobdir = None
            self.dir = None
            self.status = None
        else:
            self.jobdir = folder.path
            self.jobid = os.path.relpath(jobid, folder.path)
            self.dir = jobid
            self.status = G.job_status(self.dir)
            try:
                with open(os.path.join(self.dir, 'config', 'config')) as fp:
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
            except Exception as e:
                ### ===> TODO: better exception handling
                print('Exception:', e)

    def dir_str(self): return self.dir if self.dir else 'n/a'
    def status_str(self): return self.status if self.status else 'n/a'
    def runlen_str(self): return str(self.runlen) if self.runlen else 'n/a'
    def t100_str(self): return str(self.t100) if self.t100 != None else 'n/a'
    def config_type(self): return 'full' if self.full_config else 'base+user'

    def __str__(self):
        res = '{ '
        res += 'jobid:' + str(self.jobid) + ' '
        res += 'jobdir:' + str(self.jobdir) + ' '
        res += 'dir:' + str(self.dir) + ' '
        res += 'base_config:' + str(self.base_config) + ' '
        res += 'user_config:' + str(self.user_config) + ' '
        res += 'full_config:' + str(self.full_config) + ' '
        res += 'mods:' + str(self.mods) + ' '
        res += 'modules:' + str(self.modules) + ' '
        res += 'runlen:' + str(self.runlen) + ' '
        res += 't100:' + str(self.t100) + ' '
        res += 'status:' + str(self.status) + ' '
        res += '}'
        return res

    def write_config(self):
        try:
            with open(os.path.join(self.dir, 'config', 'config'), 'w') as fp:
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
                print('config_date:', str(datetime.datetime.today()), file=fp)
                print('run_length:', self.runlen, file=fp)
                print('t100:', self.t100, file=fp)
        except Exception as e:
            ### ===> TODO: better exception handling
            print('Exception:', e)

    def gen_namelists(self):
        new_job = os.path.join(U.cgenie_root, 'tools', 'new-job.py')
        cmd = [new_job, '--gui']
        if self.base_config: cmd += ['-b', self.base_config]
        if self.user_config: cmd += ['-u', self.user_config]
        if self.full_config: cmd += ['-c', self.full_config]
        cmd += ['-j', self.jobdir]
        if self.t100: cmd += ['--t100']
        cmd += [self.jobid, str(self.runlen)]
        if plat.system() == 'Windows': cmd = ['python'] + cmd
        try:
            with open(os.devnull, 'w') as sink:
                res = sp.check_output(cmd, stderr=sink).strip()
        except Exception as e:
            print(e)
            res = 'ERR:Failed to run new-job script'
        if res != 'OK': tkMB.showerror('Error', res[4:])


#----------------------------------------------------------------------

class Panel(ttk.Frame):
    def __init__(self, notebook, type, title):
        self.stmp = ttk.Style()
        self.stmp.configure('Tmp.TFrame', background='red')
        ttk.Frame.__init__(self, notebook)
        self.view_type = type
        self.job = None
        self.grid(column=0, row=0, padx=5, pady=5, sticky=tk.N+tk.S+tk.E+tk.W)
        notebook.add(self, text=title)

    def set_job(self, job):
        self.job = job
        self.update()


class StatusPanel(Panel):
    def __init__(self, notebook, app):
        """Initial creation of status panel"""

        Panel.__init__(self, notebook, 'status', 'Status')

        lab = ttk.Label(self, text='Job path:', font=app.bold_font)
        lab.grid(column=0, row=0, pady=5, padx=5, sticky=tk.W)
        self.job_path = ttk.Label(self, font=app.bold_font)
        self.job_path.grid(column=1, row=0, pady=5, sticky=tk.W)

        lab = ttk.Label(self, text='Job status:')
        lab.grid(column=0, row=1, pady=5, padx=5, sticky=tk.W)
        self.job_status = ttk.Label(self)
        self.job_status.grid(column=1, row=1, pady=5, sticky=tk.W)

        lab = ttk.Label(self, text='Run length:')
        lab.grid(column=0, row=2, pady=5, padx=5, sticky=tk.W)
        self.runlen = ttk.Label(self)
        self.runlen.grid(column=1, row=2, pady=5, sticky=tk.W)

        lab = ttk.Label(self, text='T100:')
        lab.grid(column=0, row=3, pady=5, padx=5, sticky=tk.W)
        self.t100 = ttk.Label(self)
        self.t100.grid(column=1, row=3, pady=5, sticky=tk.W)

        lab = ttk.Label(self, text='Modules:')
        lab.grid(column=0, row=4, pady=5, padx=5, sticky=tk.W)
        self.modules = ttk.Label(self)
        self.modules.grid(column=1, row=4, pady=5, sticky=tk.W)

        self.update()

    def update(self):
        """Setting status panel fields"""

        if not self.job:
            self.job_path.configure(text='')
            self.job_status.configure(text='')
            self.runlen.configure(text='')
            self.t100.configure(text='')
        else:
            self.job_path.configure(text=self.job.dir_str())
            s = self.job.status_str()
            if s == 'RUNNING':
                s += ' (' + format(G.job_pct(self.job.dir), '.2f') + '%)'
            self.job_status.configure(text=s)
            self.runlen.configure(text=self.job.runlen_str())
            self.t100.configure(text=self.job.t100_str())
            ### ===> TODO: get and display list of modules


### ===> TODO: also need to handle "full config" cases.
### ===> TODO: also need to handle restart setup.
class SetupPanel(Panel):
    def __init__(self, notebook, app):
        """Initial creation of setup panel"""

        Panel.__init__(self, notebook, 'setup', 'Setup')

        lab = ttk.Label(self, text='Job path:', font=app.bold_font)
        lab.grid(column=0, row=0, pady=5, padx=5, sticky=tk.W)
        self.job_path = ttk.Label(self, font=app.bold_font)
        self.job_path.grid(column=1, row=0, pady=5, sticky=tk.W)

        lab = ttk.Label(self, text='Base config:')
        lab.grid(column=0, row=1, pady=5, padx=5, sticky=tk.W)
        self.base_config = ttk.Combobox(self, values=app.base_configs, width=80)
        self.base_config.bind('<<ComboboxSelected>>', self.state_change)
        self.base_config.state(['readonly'])
        self.base_config.grid(column=1, row=1, pady=5, sticky=tk.W)

        lab = ttk.Label(self, text='User config:')
        lab.grid(column=0, row=2, pady=5, padx=5, sticky=tk.W)
        self.user_config = ttk.Combobox(self, values=app.user_configs, width=80)
        self.user_config.bind('<<ComboboxSelected>>', self.state_change)
        self.user_config.state(['readonly'])
        self.user_config.grid(column=1, row=2, pady=5, sticky=tk.W)

        lab = ttk.Label(self, text='Modifications:')
        lab.grid(column=0, row=3, pady=5, padx=5, sticky=tk.W+tk.N)
        self.mods_frame = ttk.Frame(self)
        self.mods_frame.grid(column=1, row=3, pady=5, sticky=tk.W)
        self.mods = tk.Text(self.mods_frame, width=80, height=20,
                            font=app.normal_font)
        self.mods.bind('<<Modified>>', self.state_change)
        self.mods_scroll = ttk.Scrollbar(self.mods_frame,
                                         command=self.mods.yview)
        self.mods['yscrollcommand'] = self.mods_scroll.set
        self.mods.grid(column=0, row=0, sticky=tk.W)
        self.mods_scroll.grid(column=1, row=0, sticky=tk.N+tk.S)

        lab = ttk.Label(self, text='Run length:')
        lab.grid(column=0, row=4, pady=5, padx=5, sticky=tk.W)
        self.check = self.register(self.check_runlen)
        self.runlen_var = tk.StringVar()
        self.runlen = ttk.Entry(self, width=20, validate='all',
                                textvariable=self.runlen_var,
                                validatecommand=(self.check, '%P'))
        self.runlen.grid(column=1, row=4, pady=5, sticky=tk.W)
        self.runlen_var.trace('w', self.state_change)

        lab = ttk.Label(self, text='T100:')
        lab.grid(column=0, row=5, pady=5, padx=5, sticky=tk.W)
        self.t100_var = tk.IntVar()
        self.t100 = ttk.Checkbutton(self, variable=self.t100_var,
                                    command=self.state_change)
        self.t100.grid(column=1, row=5, pady=5, sticky=tk.W)

        self.but_frame = ttk.Frame(self)
        self.but_frame.grid(column=1, row=6, pady=5, sticky=tk.W)
        self.save_button = ttk.Button(self.but_frame, text="Save changes",
                                      command=self.save_changes)
        self.revert_button = ttk.Button(self.but_frame, text="Revert changes",
                                        command=self.revert_changes)
        self.save_button.grid(column=0, row=0)
        self.revert_button.grid(column=1, row=0, padx=5)

        # ===> TODO: add warning for changes to configuration files

        self.edited = False
        self.complete = False

        self.update()

    def check_runlen(self, s):
        try:
            v = s.strip()
            if not v: return True
            return int(v) > 0
        except:
            return False

    def set_button_state(self):
        if self.edited and self.complete:
            self.save_button.state(['!disabled'])
        else:
            self.save_button.state(['disabled'])
        if self.edited:
            self.revert_button.state(['!disabled'])
        else:
            self.revert_button.state(['disabled'])

    def set_state(self):
        if not self.job:
            self.complete = False
            self.edited = False
        else:
            self.complete = self.base_config.get() and self.user_config.get()
            self.edited = False
            if (self.base_config.get() and
                self.base_config.get() != self.job.base_config):
                self.edited = True
            if (self.user_config.get() and
                self.user_config.get() != self.job.user_config):
                self.edited = True
            mods_tmp = self.mods.get('1.0', 'end').rstrip()
            if (mods_tmp and mods_tmp != self.job.mods):
                self.edited = True
            ### ===> TODO: fix this
            if (self.runlen.get() and
                int(self.runlen.get()) != self.job.runlen):
                self.edited = True
            if (self.t100_var.get() != self.job.t100):
                self.edited = True

    def state_change(self, event=None, dummy1=None, dummy2=None):
        self.set_state()
        self.set_button_state()

    def save_changes(self):
        self.job.base_config = self.base_config.get()
        self.job.user_config = self.user_config.get()
        self.job.mods = self.mods.get('1.0', 'end').rstrip()
        self.job.runlen = int(self.runlen_var.get())
        self.job.t100 = True if self.t100_var.get() else False
        self.job.write_config()
        self.job.gen_namelists()
        self.job.status = G.job_status(self.job.dir)
        print(self.job.dir, self.job.status)
        app.tree.item(self.job.dir, image=G.status_img(self.job.status))
        for p in app.panels.itervalues():
            if p != self: p.update()
        self.set_state()
        self.set_button_state()

    def revert_changes(self):
        self.base_config.set(self.job.base_config if self.job.base_config
                             else '')
        self.user_config.set(self.job.user_config if self.job.user_config
                             else '')
        self.mods.delete('1.0', 'end')
        if self.job.mods: self.mods.insert('end', self.job.mods)
        self.runlen.delete(0, 'end')
        if self.job.runlen != None:
            self.runlen.insert('end', str(self.job.runlen))
        self.t100_var.set(bool(self.job.t100))
        self.state_change(None)

    def update(self):
        """Setting setup panel fields"""

        if not self.job:
            self.set_button_state()
            return
        self.job_path.configure(text=self.job.dir_str())
        if self.job.base_config:
            self.base_config.set(self.job.base_config
                                 if self.job.base_config != '?' else '')
        else:
            self.base_config.set('')
        if self.job.user_config:
            self.user_config.set(self.job.user_config
                                 if self.job.user_config != '?' else '')
        else:
            self.user_config.set('')
        self.mods.delete('1.0', 'end')
        if self.job.mods: self.mods.insert('end', self.job.mods)
        self.runlen.delete(0, 'end')
        if self.job.runlen != None:
            self.runlen.insert('end', str(self.job.runlen))
        self.t100_var.set(bool(self.job.t100))
        self.set_state()
        self.set_button_state()


class NamelistPanel(Panel):
    def __init__(self, notebook, app):
        Panel.__init__(self, notebook, 'namelists', 'Namelists')

        self.sel_frame = ttk.Frame(self)
        lab = ttk.Label(self.sel_frame, text='Namelist:')

        nls = ()
        self.namelists = { }
        self.nl_var = tk.StringVar()
        self.nl_sel = ttk.OptionMenu(self.sel_frame, self.nl_var, None, *nls,
                                     command=self.selection_changed)
        if self.job: self.nl_var.set(nls[0])

        self.out = tk.Text(self, font=app.mono_font,
                           state=tk.DISABLED, wrap=tk.NONE)
        self.out_scroll = ttk.Scrollbar(self, command=self.out.yview)
        self.out['yscrollcommand'] = self.out_scroll.set

        self.columnconfigure(0, weight=1)
        self.rowconfigure(0, weight=0)
        self.rowconfigure(1, weight=1)
        self.sel_frame.grid(column=0, row=0, sticky=tk.W, pady=5)
        lab.grid(column=0, row=0, padx=5, pady=5, sticky=tk.W)
        self.nl_sel.grid(column=1, row=0, stick=tk.W)
        self.out.grid(column=0, row=1, sticky=tk.E+tk.W+tk.N+tk.S)
        self.out_scroll.grid(column=1, row=1, sticky=tk.N+tk.S)

    def selection_changed(self, event):
        self.set_namelist_text()

    def set_namelist_text(self):
        self.out['state'] = tk.NORMAL
        self.out.delete('1.0', 'end')
        self.out.insert('end', self.namelists[self.nl_var.get()])
        self.out['state'] = tk.DISABLED

    def update(self):
        nls = ()
        self.namelists = { }
        if self.job:
            nls = []
            for nl in glob.iglob(os.path.join(self.job.dir, 'data_*')):
                nlname = os.path.basename(nl)[5:]
                nls.append(nlname)
                with open(nl) as fp: self.namelists[nlname] = fp.read()
            nls.sort()
            nls = tuple(nls)
            self.nl_sel.set_menu(nls[0], *nls)
            self.set_namelist_text()
        else:
            self.nl_sel.set_menu(None, *nls)


class OutputPanel(Panel):
    def __init__(self, notebook, app):
        Panel.__init__(self, notebook, 'output', 'Output')

        self.app = app
        self.tailer = None
        self.tailer_job = None
        self.output_text = ''

        self.out = tk.Text(self, font=app.mono_font, state=tk.DISABLED)
        self.out_scroll = ttk.Scrollbar(self, command=self.out.yview)
        self.out['yscrollcommand'] = self.out_scroll.set

        self.columnconfigure(0, weight=1)
        self.rowconfigure(0, weight=1)
        self.out.grid(column=0, row=0, sticky=tk.E+tk.W+tk.N+tk.S)
        self.out_scroll.grid(column=1, row=0, sticky=tk.N+tk.S)

    def set_output_text(self):
        self.out['state'] = tk.NORMAL
        self.out.delete('1.0', 'end')
        self.out.insert('end', self.output_text)
        self.out['state'] = tk.DISABLED

    def add_output_text(self, t):
        self.output_text += t
        atend = self.out_scroll.get()[1] == 1.0
        self.out['state'] = tk.NORMAL
        self.out.insert('end', t)
        self.out['state'] = tk.DISABLED
        if atend: self.out.see('end')

    def update(self):
        if self.tailer and self.tailer_job != self.job:
            self.tailer.stop()
            self.output_text = ''
            self.set_output_text()
        if self.job != self.tailer_job:
            if self.job:
                self.tailer_job = self.job
                self.tailer = G.Tailer(app,
                                       os.path.join(self.job.dir, 'run.log'))
                self.tailer.start(self.add_output_text)
            else:
                self.tailer_job = None
                self.tailer = None


class PlotPanel(Panel):
    def __init__(self, notebook, app):
        Panel.__init__(self, notebook, 'plot1', '+')
        ttk.Label(self, text='View: plot').grid(column=0, row=0)

    def update(self):
        pass


#----------------------------------------------------------------------

class MoveRenameDialog(tkSD.Dialog):
    def __init__(self, full_path, is_folder, folders, parent=None):
        if not parent: parent = tk._default_root
        self.orig_folder, self.orig_name = os.path.split(full_path)
        self.is_folder = is_folder
        self.new_folder = None
        self.new_name = None
        self.folder_changed = False
        self.name_changed = False
        self.folders = folders
        self.result = False
        tkSD.Dialog.__init__(self, parent, 'Move/rename job')

    def destroy(self):
        tkSD.Dialog.destroy(self)

    def body(self, master):
        lab = ttk.Label(master, text='Folder:')
        lab.grid(column=0, row=0, pady=5, padx=5, sticky=tk.W)
        self.folder = ttk.Combobox(master, values=self.folders, width=50)
        self.folder.state(['readonly'])
        self.folder.grid(column=1, row=0, pady=5, sticky=tk.W)
        self.folder.set(self.orig_folder)

        lab = ttk.Label(master, text='Name:')
        lab.grid(column=0, row=1, pady=5, padx=5, sticky=tk.W)
        self.name = ttk.Entry(master, width=50)
        self.name.grid(column=1, row=1, pady=5, sticky=tk.W)
        self.name.insert(0, self.orig_name)

        return self.name

    def validate(self):
        if len(self.name.get()) == 0:
            tkMB.showwarning('Illegal value',
                             "New name can't be empty!",
                             parent=self)
            return 0
        if self.is_folder and self.folder.get().startswith(self.orig_folder):
            tkMB.showwarning('Illegal move',
                             "Can't move a folder into one " +
                             "of its own descendants!",
                             parent=self)
            return 0
        return 1

    def apply(self):
        self.new_folder = self.folder.get()
        self.new_name = self.name.get()
        if self.new_folder != self.orig_folder: self.folder_changed = True
        if self.new_name != self.orig_name: self.name_changed = True
        self.result = self.folder_changed or self.name_changed


#----------------------------------------------------------------------

class Application(ttk.Frame):
    def __init__(self, master=None):
        self.root = root
        self.normal_font = ttk.Style().lookup('TEntry', 'font')
        self.mono_font = tkFont.Font(family='DejaVu Sans Mono', size=10)
        self.bold_font = tkFont.Font(family='Helvetica', weight='bold')
        self.big_font = tkFont.Font(family='Helvetica', size=16, weight='bold')
        self.jobid = None
        self.job = Job()
        ttk.Frame.__init__(self, master)
        self.grid(sticky=tk.N+tk.E+tk.S+tk.W)
        self.find_configs()
        self.create_widgets()
        self.job_folder = JobFolder(U.cgenie_jobs, 'My Jobs', self.tree)
        self.job_folder.scan(True)


    def new_job(self):
        """Callback for new job button press"""

        # Get folder location for new job.
        loc = self.tree.selection()[0]
        while not G.is_folder(self.tree, loc):
            loc = self.tree.parent(loc)

        # Get new job name and check.
        job_name = tkSD.askstring("New job", "Name for new job")
        if not job_name: return
        jobdir = os.path.join(loc, job_name)
        jobid = os.path.relpath(jobdir, self.job_folder.path)
        if os.path.exists(jobdir):
            tkMB.showerror('Error', job_name + ' already exists!')
            return

        # Create job folder.
        try:
            os.mkdir(jobdir)
            os.mkdir(os.path.join(jobdir, 'config'))
        except Exception as e:
            tkMB.showerror('Error', "Couldn't create directory: " + jobdir)
            return

        # Write initial job configuration file.
        with open(os.path.join(jobdir, 'config', 'config'), 'w') as fp:
            print('base_config: ?', file=fp)
            print('user_config: ?', file=fp)
            print('run_length: ?', file=fp)
            print('t100: ?', file=fp)

        # Add job entry to tree and select.
        self.job_folder.add_job(jobid, True)
        self.tree.see(jobdir)
        self.tree.selection_set(jobdir)


    def new_folder(self):
        """Callback for new folder button press"""

        # Get folder location for new folder.
        loc = self.tree.selection()[0]
        while len(self.tree.get_children(loc)) == 0:
            loc = self.tree.parent(loc)

        # Get new folder name and check.
        folder_name = tkSD.askstring("New folder", "Name for new folder")
        if not folder_name: return
        folder = os.path.join(loc, folder_name)
        p = os.path.join(U.cgenie_jobs, folder)
        if os.path.exists(p):
            tkMB.showerror('Error', folder_name + ' already exists!')
            return

        # Create new folder.
        try:
            os.mkdir(p)
        except Exception as e:
            tkMB.showerror('Error', "Couldn't create directory: " + p)
            return

        # Add folder entry to tree and select.
        self.job_folder.add_folder(os.relpath(folder, U.cgenie_jobs), True)
        self.tree.selection_set(p)


    def move_rename(self):
        full_path = self.tree.selection()[0]
        is_folder = G.is_folder(self.tree, full_path)
        d = MoveRenameDialog(full_path, is_folder,
                             self.job_folder.possible_folders())
        if d.result:
            new_full_path = os.path.join(d.new_folder, d.new_name)
            try:
                self.job_folder.move(full_path, new_full_path)
                self.tree.see(new_full_path)
                self.tree.selection_set(new_full_path)
            except Exception as e:
                print(e)
                tkMB.showwarning('Move/rename failed', 'Oops', parent=self)


    def delete_job(self):
        """Delete a job or a folder from tree (and on disk)"""

        # Determine whether a single job or a folder is selected.
        p = self.tree.selection()[0]
        if G.is_folder(self.tree, p):
            msg = 'Are you sure you want to delete this folder?\n\n'
            msg += 'This will delete all jobs beneath the folder!\n\n'
            msg += 'This action is IRREVERSIBLE!'
        else:
            msg = 'Are you sure you want to delete this job?\n\n'
            msg += 'This action is IRREVERSIBLE!'

        # Confirmation dialog -- single job or folder.
        chk = tkMB.askokcancel('Confirm deletion', msg)
        if not chk: return

        # Find adjacent item ID for post-delete selection.
        post = self.tree.next(p)
        if not post: post = self.tree.prev(p)

        # Recursively delete.
        try:
            shutil.rmtree(p)
        except Exception as e:
            tkMB.showerror('Error', "Couldn't delete directory: " + p)
            return

        # Delete from tree and select.
        self.tree.selection_set(post)
        self.job_folder.delete(p)


    def clone_job(self):
        p = self.tree.selection()[0]
        pnew = p + '-CLONE'
        i = 1
        while os.path.exists(pnew):
            i += 1
            pnew = p + '-CLONE' + str(i)
        self.job_folder.clone(p, pnew)
        self.tree.see(pnew)
        self.tree.selection_set(pnew)


    def clear_job(self):
        p = self.tree.selection()[0]
        msg = 'Are you sure you want to clear\n'
        msg += 'all output data for this job?\n\n'
        msg += 'This action is IRREVERSIBLE!'

        chk = tkMB.askokcancel('Confirm deletion', msg)
        if not chk: return

        if os.path.exists(os.path.join(p, 'status')):
            os.remove(os.path.join(p, 'status'))
        for d, ds, fs in os.walk(os.path.join(p, 'output')):
            for f in fs: os.remove(os.path.join(d, f))
        self.update_job_data()


    def archive_job(self):
        print('archive_job...')


    def run_job(self):
        # Check for existence of genie-ship.exe executable.
        exe = os.path.join(U.cgenie_jobs, 'MODELS', U.cgenie_version,
                           platform, 'ship', 'genie.exe')
        runexe = os.path.join(self.job.dir, 'genie-ship.exe')
        if not os.path.exists(exe):
            tkMB.showerror('Error', 'GENIE executable missing!')
            return
        shutil.copy(exe, runexe)

        # Start executable with stdout and stderr directed to run.log
        # in job directory.
        with open(os.path.join(self.job.dir, 'run.log'), 'w') as fp:
            try:
                sp.Popen(runexe, cwd=self.job.dir, stdout=fp, stderr=sp.STDOUT)
            except Exception as e:
                tkMB.showerror('Error', 'Failed to start GENIE executable!')


    def pause_job(self):
        print('pause_job...')
        with open(os.path.join(self.job.dir, 'command'), 'w') as fp:
            print('PAUSE', file=fp)


    # Buttons that change state depending on the state of the
    # currently selected job.
    switchable_buttons = ['move_rename', 'delete_job', 'clear_job',
                          'clone_job', 'archive_job', 'run_job', 'pause_job']

    # Enabled buttons for different states of selected job.
    state_buttons = { 'UNCONFIGURED': ['move_rename', 'clear_job', 'delete_job',
                                       'clone_job'],
                      'RUNNABLE': ['move_rename', 'clear_job', 'delete_job',
                                   'clone_job', 'run_job'],
                      'RUNNING': ['pause_job'],
                      'PAUSED': ['move_rename', 'clear_job', 'delete_job',
                                 'clone_job', 'archive_job', 'run_job'],
                      'COMPLETE': ['move_rename', 'clear_job', 'delete_job',
                                   'clone_job', 'archive_job'],
                      'ERRORED': ['move_rename', 'clear_job', 'delete_job',
                                  'clone_job'] }


    def item_selected(self, event=None):
        """Callback for item selection in job tree"""

        sel = self.tree.selection()[0]
        if len(self.tree.get_children(sel)) != 0 or G.is_folder(self.tree, sel):
            self.select_job(None)
        else:
            self.select_job(sel)
        self.set_job_buttons()


    def set_job_buttons(self):
        sel = self.tree.selection()[0]
        if self.job == None:
            for k, v in self.tool_buttons.iteritems():
                if k in self.switchable_buttons:
                    if ((k == 'move_rename' or k == 'delete_job')
                        and self.tree.parent(sel) != ''):
                        v.state(['!disabled'])
                    else:
                        v.state(['disabled'])
        else:
            on_buttons = self.state_buttons[G.job_status(sel)]
            for k, v in self.tool_buttons.iteritems():
                if k in self.switchable_buttons:
                    if k in on_buttons:
                        v.state(['!disabled'])
                    else:
                        v.state(['disabled'])


    def select_job(self, jobid):
        """Select a job and set up information tracking"""

        if jobid:
            self.job = Job(jobid, self.job_folder)
        else:
            self.job = None

        ### ===> TODO: need to track model output if it's running to
        ###      add to output panel (using same threading approach as
        ###      in go.py).
        for p in self.panels.itervalues(): p.set_job(self.job)

    def update_job_data(self):
        s = None
        if self.job and self.job.dir:
            s = self.job.status
            self.job.status = G.job_status(self.job.dir)
        self.panels['status'].update()
        self.panels['output'].update()
        ### ===> TODO: update plot panels
        if self.job and self.job.dir and s != self.job.status:
            self.tree.item(self.job.dir, image=G.status_img(self.job.status))
        self.set_job_buttons()
        self.after(500, self.update_job_data)

    def create_widgets(self):
        """UI layout"""

        self.pane = ttk.PanedWindow(self, orient=tk.HORIZONTAL)

        self.tree = ttk.Treeview(self.pane, selectmode='browse')
        self.tree.bind('<<TreeviewSelect>>', self.item_selected)
        self.pane.add(self.tree)

        self.main_frame = ttk.Frame(self.pane)
        self.pane.add(self.main_frame)

        self.toolbar = ttk.Frame(self.main_frame)
        self.tool_buttons = { }
        tool_info = [['new_job',     'New job'],
                     ['new_folder',  'New folder'],
                     ['move_rename', 'Move/rename job or folder'],
                     ['delete_job',  'Delete job'],
                     ['clear_job',   'Clear job output'],
                     ['clone_job',   'Clone job'],
                     ['archive_job', 'Archive job'],
                     ['spacer', ''],
                     ['run_job',     'Run job'],
                     ['pause_job',   'Pause job']]
        for t in tool_info:
            if t[0] == 'spacer':
                f = ttk.Frame(self.toolbar, height=16)
                f.pack()
            else:
                img = G.file_img(t[0])
                b = ttk.Button(self.toolbar, image=img,
                               command=getattr(self, t[0]))
                b.image = img
                b.pack()
                G.ToolTip(b, t[1])
                self.tool_buttons[t[0]] = b

        # Set up default notebook panels.
        self.notebook = ttk.Notebook(self.main_frame)
        self.panels = { }
        self.panels['status'] = StatusPanel(self.notebook, self)
        self.panels['setup'] = SetupPanel(self.notebook, self)
        self.panels['namelists'] = NamelistPanel(self.notebook, self)
        self.panels['output'] = OutputPanel(self.notebook, self)
        self.panels['plot1'] = PlotPanel(self.notebook, self)

        # Enable window resizing and place widgets.
        top = self.winfo_toplevel()
        top.rowconfigure(0, weight=1)
        top.columnconfigure(0, weight=1)
        self.columnconfigure(0, weight=1)
        self.rowconfigure(0, weight=1)
        self.pane.columnconfigure(0, weight=1)
        self.pane.rowconfigure(0, weight=1)
        self.main_frame.columnconfigure(0, weight=0)
        self.main_frame.columnconfigure(1, weight=1)
        self.main_frame.rowconfigure(0, weight=1)
        self.pane.grid(column=0, row=0, sticky=tk.N+tk.E+tk.S+tk.W)
        self.toolbar.grid(column=0, row=0, sticky=tk.N+tk.E+tk.S+tk.W)
        self.notebook.grid(column=1, row=0, sticky=tk.N+tk.E+tk.S+tk.W)

        self.menu = tk.Menu(top)
        top['menu'] = self.menu
        self.file_menu = tk.Menu(self.menu, tearoff=0)
        self.menu.add_cascade(label='File', menu=self.file_menu)
        self.file_menu.add_command(label='Quit', command=self.quit)
        self.help_menu = tk.Menu(self.menu, tearoff=0)
        self.menu.add_cascade(label='Help', menu=self.help_menu)
        self.help_menu.add_command(label='About')

        self.after(500, self.update_job_data)

    def find_configs(self):
        """Find all base and user configuration files"""

        bs = os.listdir(os.path.join(U.cgenie_data, 'base-configs'))
        bs = filter(lambda s: s.endswith('.config'), bs)
        self.base_configs = map(lambda s: s.rpartition('.')[0], bs)
        self.base_configs.sort()
        us = []
        udir = os.path.join(U.cgenie_data, 'user-configs')
        for d, ds, fs in os.walk(udir):
            for f in fs:
                us.append(os.path.relpath(os.path.join(d, f), udir))
        self.user_configs = us
        self.user_configs.sort()


#----------------------------------------------------------------------

root = tk.Tk()
app = Application(root)
app.master.title("cGENIE GUI")
app.master.geometry("1024x768")
app.mainloop()
