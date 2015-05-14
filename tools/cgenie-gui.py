#!/usr/bin/env python2

from __future__ import print_function
import os, os.path, shutil
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


class JobFolder:
    """Job folder management"""
    def __init__(self, path, name, tree):
        self.path = path
        self.name = name
        self.tree = tree
        self.item = None
        self.status = { }

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
                self.tree.insert(parent, 'end', p, text=f,
                                 image=G.status_img('FOLDER'))
        if sort: self.sort_children(self.item)

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
            self.dir = None
            self.status = None
        else:
            self.jobid = os.path.relpath(jobid, folder.path)
            self.dir = jobid
            self.status = G.job_status(self.dir)
            try:
                with open(os.path.join(self.dir, 'config', 'config')) as fp:
                    for line in fp:
                        k, v = map(lambda s: s.strip(), line.strip().split(':'))
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

        if not self.job: return
        self.job_path.configure(text=self.job.dir_str())
        self.job_status.configure(text=self.job.status_str())
        self.runlen.configure(text=self.job.runlen_str())
        self.t100.configure(text=self.job.t100_str())


### ===> TODO: also need to handle "full config" cases.
### ===> TODO: need to sort out job config/config contents conventions
###      for consistency between GUI jobs, new-job jobs and test jobs.
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
        self.base_config.grid(column=1, row=1, pady=5, sticky=tk.W)

        lab = ttk.Label(self, text='User config:')
        lab.grid(column=0, row=2, pady=5, padx=5, sticky=tk.W)
        self.user_config = ttk.Combobox(self, values=app.user_configs, width=80)
        self.user_config.grid(column=1, row=2, pady=5, sticky=tk.W)

        lab = ttk.Label(self, text='Modifications:')
        lab.grid(column=0, row=3, pady=5, padx=5, sticky=tk.W+tk.N)
        self.mods_frame = ttk.Frame(self)
        self.mods_frame.grid(column=1, row=3, pady=5, sticky=tk.W)
        self.mods = tk.Text(self.mods_frame, width=80, height=20,
                            font=app.normal_font)
        self.mods_scroll = ttk.Scrollbar(self.mods_frame,
                                         command=self.mods.yview)
        self.mods['yscrollcommand'] = self.mods_scroll.set
        self.mods.grid(column=0, row=0, sticky=tk.W)
        self.mods_scroll.grid(column=1, row=0, sticky=tk.N+tk.S)

        self.but_frame = ttk.Frame(self)
        self.but_frame.grid(column=1, row=4, pady=5, sticky=tk.W)
        self.save_button = ttk.Button(self.but_frame, text="Save changes")
        self.revert_button = ttk.Button(self.but_frame, text="Revert changes")
        self.save_button.grid(column=0, row=0)
        self.revert_button.grid(column=1, row=0, padx=5)

        self.update()

    def update(self):
        """Setting setup panel fields"""

        if not self.job: return
        self.job_path.configure(text=self.job.dir_str())
        if self.job.base_config:
            self.base_config.set(self.job.base_config)
        else:
            self.base_config.set('')
        if self.job.user_config:
            self.user_config.set(self.job.user_config)
        else:
            self.user_config.set('')


class NamelistPanel(Panel):
    def __init__(self, notebook, app):
        Panel.__init__(self, notebook, 'namelists', 'Namelists')

        self.sel_frame = ttk.Frame(self)
        lab = ttk.Label(self.sel_frame, text='Namelist:')
        # ===> TODO: should set this from the keys of
        #      self.job.namelists
        nls = ('genie', 'GEM', 'BIOGEM')
        self.nl_var = tk.StringVar()
        self.nl_sel = ttk.OptionMenu(self.sel_frame, self.nl_var, None, *nls,
                                     command=self.selection_changed)
        self.nl_var.set(nls[0])

        self.out = tk.Text(self, font=app.normal_font,
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
        print(self.nl_var.get())
        self.set_namelist_text()

    def set_namelist_text(self):
        self.out['state'] = tk.DISABLED
        # ===> TODO: set text here from
        #      self.job.namelists[self.nl_var.get()]
        self.out['state'] = tk.NORMAL
        pass

    def update(self):
        # ===> TODO: update the list of possible namelists here from
        #            the keys of self.job.namelists
        pass


class OutputPanel(Panel):
    def __init__(self, notebook, app):
        Panel.__init__(self, notebook, 'output', 'Output')

        self.out = tk.Text(self, font=app.normal_font)
        self.out_scroll = ttk.Scrollbar(self, command=self.out.yview)
        self.out['yscrollcommand'] = self.out_scroll.set
        self.columnconfigure(0, weight=1)
        self.rowconfigure(0, weight=1)
        self.out.grid(column=0, row=0, sticky=tk.E+tk.W+tk.N+tk.S)
        self.out_scroll.grid(column=1, row=0, sticky=tk.N+tk.S)

    def update(self):
        pass


class PlotPanel(Panel):
    def __init__(self, notebook, app):
        Panel.__init__(self, notebook, 'plot1', '+')
        ttk.Label(self, text='View: plot').grid(column=0, row=0)

    def update(self):
        pass



class Application(ttk.Frame):
    def __init__(self, master=None):
        self.root = root
        self.normal_font = ttk.Style().lookup('TEntry', 'font')
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
        self.job_folder.add_folder(folder, True)
        self.tree.selection_set(p)


    def move_rename(self):
        print('move_rename...')


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

        # Find adjacent item ID for post-delete selection.
        post = self.tree.next(p)
        if not post: post = self.tree.prev(p)
        print('post:', post)

        # Recursively delete.
        try:
            shutil.rmtree(p)
        except Exception as e:
            tkMB.showerror('Error', "Couldn't delete directory: " + p)
            return

        # Delete from tree and select.
        self.tree.selection_set(post)
        self.tree.delete(p)


    def clone_job(self):
        print('clone_job...')

    def archive_job(self):
        print('archive_job...')

    def run_job(self):
        print('run_job...')

    def pause_job(self):
        print('pause_job...')


    # Buttons that change state depending on the state of the
    # currently selected job.
    switchable_buttons = ['move_rename', 'delete_job', 'clone_job',
                          'archive_job', 'run_job', 'pause_job']

    # Enabled buttons for different states of selected job.
    state_buttons = { 'UNCONFIGURED': ['move_rename', 'delete_job',
                                       'clone_job'],
                      'RUNNABLE': ['move_rename', 'delete_job',
                                   'clone_job', 'run_job'],
                      'RUNNING': ['pause_job'],
                      'PAUSED': ['move_rename', 'delete_job',
                                 'clone_job', 'archive_job', 'run_job'],
                      'COMPLETE': ['move_rename', 'delete_job',
                                   'clone_job', 'archive_job'],
                      'ERRORED': ['move_rename', 'delete_job',
                                  'clone_job'] }


    def item_selected(self, event):
        """Callback for item selection in job tree"""

        sel = self.tree.selection()[0]
        if len(self.tree.get_children(sel)) != 0:
            self.select_job(None)
            for k, v in self.tool_buttons.iteritems():
                if k in self.switchable_buttons:
                    if ((k == 'move_rename' or k == 'delete_job')
                        and self.tree.parent(sel) != ''):
                        v.state(['!disabled'])
                    else:
                        v.state(['disabled'])
        else:
            self.select_job(sel)
            on_buttons = self.state_buttons[G.job_status(sel)]
            for k, v in self.tool_buttons.iteritems():
                if k in self.switchable_buttons:
                    if k in on_buttons:
                        v.state(['!disabled'])
                    else:
                        v.state(['disabled'])


    def select_job(self, jobid):
        """Select a job and set up information tracking"""

        self.job = Job(jobid, self.job_folder)
        ### ===> TODO: need to track model output if it's running to
        ###      add to output panel (using same threading approach as
        ###      in go.py).
        for p in self.panels.itervalues(): p.set_job(self.job)


    def create_widgets(self):
        """UI layout"""

        self.tree = ttk.Treeview(self, selectmode='browse')
        self.tree.bind('<<TreeviewSelect>>', self.item_selected)

        self.toolbar = ttk.Frame(self)
        self.tool_buttons = { }
        tool_info = [['new_job',     'New job'],
                     ['new_folder',  'New folder'],
                     ['move_rename', 'Move/rename job or folder'],
                     ['delete_job',  'Delete job'],
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
        self.notebook = ttk.Notebook(self)
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
        self.columnconfigure(0, weight=0)
        self.columnconfigure(1, weight=0)
        self.columnconfigure(2, weight=1)
        self.rowconfigure(0, weight=1)
        self.tree.grid(column=0, row=0, sticky=tk.N+tk.E+tk.S+tk.W)
        self.toolbar.grid(column=1, row=0, sticky=tk.N+tk.E+tk.S+tk.W)
        self.notebook.grid(column=2, row=0, sticky=tk.N+tk.E+tk.S+tk.W)

        self.menu = tk.Menu(top)
        top['menu'] = self.menu
        self.file_menu = tk.Menu(self.menu, tearoff=0)
        self.menu.add_cascade(label='File', menu=self.file_menu)
        self.file_menu.add_command(label='Quit', command=self.quit)
        self.help_menu = tk.Menu(self.menu, tearoff=0)
        self.menu.add_cascade(label='Help', menu=self.help_menu)
        self.help_menu.add_command(label='About')


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


root = tk.Tk()
app = Application(root)
app.master.title("cGENIE GUI")
app.master.geometry("1024x768")
app.mainloop()
