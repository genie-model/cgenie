import os, os.path
import Tkinter as tk
from job import *
import utils as U

def walk_jobs(p, basedir=None):
    if not basedir: basedir = p
    model_dir = os.path.join(basedir, 'MODELS')
    es = os.listdir(p)
    for e in os.listdir(p):
        f = os.path.join(p, e)
        if f.startswith(model_dir): continue
        if os.path.exists(os.path.join(f, 'config', 'config')):
            yield (f, 'JOB')
        elif os.path.isdir(f):
            if not os.listdir(f):
                yield (f, 'FOLDER')
            else:
                for sube in walk_jobs(f, basedir):
                    yield sube


def job_split(jfull):
    d, j = os.path.split(jfull)
    ds = []
    while d:
        d, d1 = os.path.split(d)
        ds.append(d1)
    ds.reverse()
    return (ds, j)


folder_image = None
def folder_img():
    global folder_image
    if not folder_image:
        folder_image = tk.PhotoImage(file=os.path.join(U.cgenie_root,
                                                       'tools', 'images',
                                                       'status-FOLDER.gif'))
    return folder_image


class JobFolder:
    """Job folder management"""
    def __init__(self, path, name, tree, app):
        self.base_path = path
        self.name = name
        self.tree = tree
        self.selected = None
        self.folders = { path: 1 }
        self.status = { }
        self.app = app
        self.selected = self.tree.insert('', 'end', self.base_path,
                                         text=self.name, open=True)
        for p, type in walk_jobs(self.base_path):
            if type == 'JOB':
                self.add_job(os.path.relpath(p, self.base_path))
            else:
                self.add_folder(os.path.relpath(p, self.base_path))
        self.sort_children(self.selected)
        self.tree.selection_set(self.selected)
        self.app.after(500, self.set_statuses)


    def possible_folders(self):
        fs = self.folders.keys()
        fs.sort()
        return fs


    def add_job(self, jfull, sort=False):
        ds, j = job_split(jfull)
        p = self.base_path
        for f in ds:
            parent = p
            p = os.path.join(p, f)
            if not self.tree.exists(p):
                self.folders[p] = 1
                self.tree.insert(parent, 'end', p, text=f, image=folder_img())
        jpath = os.path.join(self.base_path, jfull)
        job = Job(jpath, self)
        self.status[jfull] = job.status
        self.tree.insert(p, 'end', jpath, text=j, image=job.status_img())
        if sort: self.sort_children(self.selected)


    def add_folder(self, ffull, sort=False):
        ds, j = job_split(os.path.join(ffull, 'DUMMY'))
        p = self.base_path
        for f in ds:
            parent = p
            p = os.path.join(p, f)
            if not self.tree.exists(p):
                self.folders[p] = 1
                self.tree.insert(parent, 'end', p, text=f, image=folder_img())
        if sort: self.sort_children(self.selected)

    def is_folder(self, p):
        return p in self.folders

    def delete(self, p):
        self.tree.delete(p)
        if self.is_folder(p):
            ds = []
            for f in self.folders.keys():
                if f.startswith(p): ds.append(f)
            for d in ds: del self.folders[d]
        if p in self.status: del self.status[p]


    def move(self, fr, to):
        os.rename(fr, to)
        isf = fr in self.folders
        to = os.path.relpath(to, self.base_path)
        self.delete(fr)
        if isf:
            self.add_folder(to, True)
        else:
            self.add_job(to, True)


    def clone(self, fr, to):
        shutil.copytree(fr, to)
        to = os.path.relpath(to, self.base_path)
        self.add_job(to, True)


    def sort_children(self, f):
        def chcmp(x, y):
            if self.is_folder(x) == self.is_folder(y): return cmp(x, y)
            else:
                if self.is_folder(x): return -1
                else: return 1
        cs = list(self.tree.get_children(f))
        cs.sort(chcmp)
        self.tree.set_children(f, *cs)
        for c in cs: self.sort_children(c)

    def set_statuses(self):
        dels = []
        for p, s in self.status.iteritems():
            pfull = os.path.join(self.base_path, p)
            schk = job_status(pfull)
            if not schk:
                dels.append(p)
            elif schk != s:
                self.status[p] = schk
                self.tree.item(pfull, image=job_status_img(schk))
        for p in dels: del self.status[p]
        self.app.after(500, self.set_statuses)
