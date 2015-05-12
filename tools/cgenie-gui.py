#!/usr/bin/env python2

from __future__ import print_function
import os, os.path
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
        fimg = str(G.status_img('FOLDER'))
        def chcmp(x, y):
            fx = str(self.tree.item(x, 'image')[0]) == fimg
            fy = str(self.tree.item(y, 'image')[0]) == fimg
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
        tk.Frame.__init__(self, master)
        self.grid(column=0, row=0)
        self.createWidgets()
        self.job_folders = []

    # Multiple job folders not yet used.
    def clear_job_folders(self):
        for f in self.job_folders:
            if f.item: self.tree.delete(f.item)
        self.job_folders = []

    def add_job_folder(self, path, name, select):
        f = JobFolder(path, name, self.tree)
        self.job_folders.append(f)
        f.scan(select)


    def new_job(self):
        # Get folder location for new job.
        loc = self.tree.focus()
        fimg = str(G.status_img('FOLDER'))
        while str(self.tree.item(loc, 'image')[0]) != fimg:
            loc = self.tree.parent(loc)

        # Get new job name and check.
        job_name = tkSD.askstring("New job", "Name for new job")
        if not job_name: return
        jobdir = os.path.join(loc, job_name)
        jobid = os.path.relpath(jobdir, self.job_folders[0].path)
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
        self.job_folders[0].add_job(jobid, True)
        self.tree.see(jobdir)
        self.tree.selection_set(jobdir)


    def new_folder(self):
        # Get folder location for new folder.
        loc = self.tree.focus()
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
        self.job_folders[0].add_folder(folder, True)
        self.tree.selection_set(p)


    def move_rename(self):
        print('move_rename...')

    def delete_job(self):
        print('delete_job...')
        # Determine whether a single job or a folder is selected.
        # Confirmation dialog -- single job or folder.
        # Find adjacent item ID for post-delete selection.
        # Recursively delete.
        # Rescan and select.

    def clone_job(self):
        print('clone_job...')

    def archive_job(self):
        print('archive_job...')

    def run_job(self):
        print('run_job...')

    def pause_job(self):
        print('pause_job...')

    switchable_buttons = ['move_rename', 'delete_job', 'clone_job',
                          'archive_job', 'run_job', 'pause_job']

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
        sel = self.tree.focus()
        if len(self.tree.get_children(sel)) != 0:
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

    def createWidgets(self):
        self.tree = ttk.Treeview(self)
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
        view_info = [['status', 'Status'],
                     ['config', 'Configuration'],
                     ['output', 'Output'],
                     ['add_plot', '+']]
        for v in view_info:
            tmp = ttk.Frame(self.notebook)
            tmp.view_type = v[0]
            ttk.Label(tmp, text='View: ' + v[0]).grid(column=0, row=0)
            self.notebook.add(tmp, text=v[1])

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

root = tk.Tk()
app = Application(root)
app.master.title("cGENIE GUI")
app.master.geometry("1024x768")
app.add_job_folder(U.cgenie_jobs, 'My Jobs', True)
app.mainloop()
