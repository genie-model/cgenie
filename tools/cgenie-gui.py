#!/usr/bin/env python2

from __future__ import print_function
import os, os.path
import Tkinter as tk
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
        for d, ds, fs in os.walk(self.path):
            if not os.path.exists(os.path.join(d, 'data_genie')): continue
            self.add_job(os.path.relpath(d, self.path))
        self.sort_children(self.item)
        if select: self.tree.selection_set(self.item)

    def add_job(self, jfull):
        ds, j = G.job_split(jfull)
        folder = self.item
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

    def sort_children(self, f):
        def chcmp(x, y):
            fx = self.tree.get_children(x) != ()
            fy = self.tree.get_children(y) != ()
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

    def clearJobFolders(self):
        for f in self.job_folders:
            if f.item: self.tree.delete(f.item)
        self.job_folders = []

    def addJobFolder(self, path, name, select):
        f = JobFolder(path, name, self.tree)
        self.job_folders.append(f)
        f.scan(select)

    def new_job(self):
        print('new_job...')

    def new_folder(self):
        print('new_folder...')

    def move_rename(self):
        print('move_rename...')

    def delete_job(self):
        print('delete_job...')

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

    state_buttons = { 'FOLDER': ['move_rename', 'delete_job'],
                      'UNCONFIGURED': ['move_rename', 'delete_job',
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
            status = 'FOLDER'
        else:
            status = G.job_status(sel)
        on_buttons = self.state_buttons[status]
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
app.addJobFolder(U.cgenie_jobs, 'My Jobs', True)
app.mainloop()
