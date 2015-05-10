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

    def scan(self):
        self.item = self.tree.insert('', 'end', self.path,
                                     text=self.name, open=True)
        for d, ds, fs in os.walk(self.path):
            if not os.path.exists(os.path.join(d, 'data_genie')): continue
            self.add_job(os.path.relpath(d, self.path))
        self.sort_children(self.item)

    def add_job(self, jfull):
        ds, j = G.job_split(jfull)
        folder = self.item
        p = self.path
        for f in ds:
            parent = p
            p = os.path.join(p, f)
            if not self.tree.exists(p):
                self.tree.insert(parent, 'end', p, text=f)
        self.tree.insert(p, 'end', os.path.join(p, j), text=j)

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

    def addJobFolder(self, path, name):
        f = JobFolder(path, name, self.tree)
        self.job_folders.append(f)
        f.scan()

    def createWidgets(self):
        self.tree = ttk.Treeview(self)
        self.tree.pack()
        self.pack()

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
        self.columnconfigure(1, weight=1)
        self.rowconfigure(0, weight=1)
        self.tree.grid(column=0, row=0, sticky=tk.N+tk.S+tk.W)
        self.notebook.grid(column=1, row=0, columnspan=3,
                           sticky=tk.N+tk.E+tk.S+tk.W)

        top = self.winfo_toplevel()
        self.menu = tk.Menu(top)
        top['menu'] = self.menu
        self.file_menu = tk.Menu(self.menu)
        self.menu.add_cascade(label='File', menu=self.file_menu)
        self.file_menu.add_command(label='Quit', command=self.quit)
        self.help_menu = tk.Menu(self.menu)
        self.menu.add_cascade(label='Help', menu=self.help_menu)
        self.help_menu.add_command(label='About')

root = tk.Tk()
app = Application(root)
app.master.title("cGENIE GUI")
app.master.geometry("1024x768")
app.addJobFolder(U.cgenie_jobs, 'My Jobs')
app.mainloop()
