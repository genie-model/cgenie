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


class Application(tk.Frame):
    def __init__(self, master=None):
        self.root = root
        self.bold_font = tkFont.Font(family='Helvetica', weight='bold')
        self.big_font = tkFont.Font(family='Helvetica', size=16, weight='bold')
        self.selected_jobid = None
        self.job_dir = None
        self.job_status = None
        self.job_modules = None
        self.job_runlen = None
        self.job_t100 = None
        tk.Frame.__init__(self, master)
        self.grid(column=0, row=0)
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

        if not jobid:
            self.selected_jobid = None
            self.job_dir = None
            self.job_status = None
            self.job_modules = None
            self.job_runlen = None
            self.job_t100 = None
        else:
            self.job_id = jobid
            self.job_dir = os.path.join(self.job_folder, self.job_id)
            self.job_status = G.job_status(self.job_id)
            ### ===> [ TODO
            self.job_modules = None
            self.job_runlen = None
            self.job_t100 = None
            ### ===> ]
            ### ALSO: need to track model output if it's running to
            ### add to output panel (using same threading approach as
            ### in go.py).
        self.update_panels()


    def update_panels(self):
        """Update all job information panels"""

        self.update_status_panel()
        self.update_config_panel()
        self.update_output_panel()
        self.update_plot_panels()


    def create_widgets(self):
        """UI layout"""

        self.tree = ttk.Treeview(self, selectmode='browse')
        self.tree.bind('<<TreeviewSelect>>', self.item_selected)
        self.tree.pack()
        self.pack()

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
        view_info = [['status',   'Status',        self.create_status_panel],
                     ['config',   'Configuration', self.create_config_panel],
                     ['output',   'Output',        self.create_output_panel],
                     ['add_plot', '+',             self.create_plot_panel]]
        for v in view_info:
            panel = ttk.Frame(self.notebook)
            panel.view_type = v[0]
            (v[2])(panel)
            self.notebook.add(panel, text=v[1])

        # Enable window resizing and place widgets.
        self.winfo_toplevel().rowconfigure(0, weight=1)
        self.winfo_toplevel().columnconfigure(0, weight=1)
        self.grid(sticky=tk.N+tk.E+tk.S+tk.W)
        self.columnconfigure(0, weight=0)
        self.columnconfigure(1, weight=0)
        self.columnconfigure(2, weight=1)
        self.rowconfigure(0, weight=1)
        self.tree.grid(column=0, row=0, sticky=tk.N+tk.S+tk.W)
        self.toolbar.grid(column=1, row=0, sticky=tk.N+tk.S+tk.W)
        self.notebook.grid(column=2, row=0, columnspan=3,
                           sticky=tk.N+tk.E+tk.S+tk.W)

        top = self.winfo_toplevel()
        self.menu = tk.Menu(top)
        top['menu'] = self.menu
        self.file_menu = tk.Menu(self.menu, tearoff=0)
        self.menu.add_cascade(label='File', menu=self.file_menu)
        self.file_menu.add_command(label='Quit', command=self.quit)
        self.help_menu = tk.Menu(self.menu, tearoff=0)
        self.menu.add_cascade(label='Help', menu=self.help_menu)
        self.help_menu.add_command(label='About')


    def create_status_panel(self, panel):
        """Initial creation of status panel"""

        lab = ttk.Label(panel, text='Job path:', font=self.bold_font)
        lab.grid(column=0, row=0, pady=5, padx=5, sticky=tk.W)
        self.status_job_path = ttk.Label(panel, font=self.bold_font)
        self.status_job_path.grid(column=1, row=0, pady=5, sticky=tk.W)

        lab = ttk.Label(panel, text='Job status:')
        lab.grid(column=0, row=1, pady=5, padx=5, sticky=tk.W)
        self.status_job_status = ttk.Label(panel)
        self.status_job_status.grid(column=1, row=1, pady=5, sticky=tk.W)

        lab = ttk.Label(panel, text='Run length:')
        lab.grid(column=0, row=2, pady=5, padx=5, sticky=tk.W)
        self.status_runlen = ttk.Label(panel)
        self.status_runlen.grid(column=1, row=2, pady=5, sticky=tk.W)

        lab = ttk.Label(panel, text='T100:')
        lab.grid(column=0, row=3, pady=5, padx=5, sticky=tk.W)
        self.status_t100 = ttk.Label(panel)
        self.status_t100.grid(column=1, row=3, pady=5, sticky=tk.W)

        lab = ttk.Label(panel, text='Modules:')
        lab.grid(column=0, row=4, pady=5, padx=5, sticky=tk.W)
        self.status_modules = ttk.Label(panel)
        self.status_modules.grid(column=1, row=4, pady=5, sticky=tk.W)

        self.update_status_panel()


    def update_status_panel(self):
        """Setting status panel fields"""

        jd = self.job_dir if self.job_dir else 'n/a'
        self.status_job_path.configure(text=jd)
        js = self.job_status if self.job_status else 'n/a'
        self.status_job_status.configure(text=js)
        jl = str(self.job_runlen) if self.job_runlen else 'n/a'
        self.status_runlen.configure(text=jl)
        j100 = str(self.job_t100) if self.job_t100 != None else 'n/a'
        self.status_t100.configure(text=j100)


    def create_config_panel(self, panel):
        ttk.Label(panel, text='View: config').grid(column=0, row=0)

    def update_config_panel(self):
        pass

    def create_output_panel(self, panel):
        ttk.Label(panel, text='View: output').grid(column=0, row=0)

    def update_output_panel(self):
        pass

    def create_plot_panel(self, panel):
        ttk.Label(panel, text='View: plot').grid(column=0, row=0)

    def update_plot_panels(self):
        pass


root = tk.Tk()
app = Application(root)
app.master.title("cGENIE GUI")
app.master.geometry("1024x768")
app.mainloop()
