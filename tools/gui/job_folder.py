import os, os.path, shutil
import Tkinter as tk
from job import *
import utils as U

#----------------------------------------------------------------------
#
#  UTILITIES
#

def walk_jobs(p, basedir=None):
    # Iterator to walk through the hierarchy of job and normal folders
    # under a given base job directory, yielding paths and information
    # about whether entries are folders or individual jobs.

    # Base case for recursion.
    if not basedir: basedir = p

    # Skip directory containing model executables and repositories.
    model_dir = os.path.join(basedir, 'MODELS')

    # Loop over entries in the current directory, recursing into
    # sub-directories.
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
    # Split a full job path into directories and job name.
    d, j = os.path.split(jfull)
    ds = []
    while d:
        d, d1 = os.path.split(d)
        ds.append(d1)
    ds.reverse()
    return (ds, j)


folder_image = None
def folder_img():
    # Return icon image to use for folders in job tree view.
    global folder_image
    if not folder_image:
        folder_image = tk.PhotoImage(file=os.path.join(U.cgenie_root,
                                                       'tools', 'images',
                                                       'status-FOLDER.gif'))
    return folder_image


#----------------------------------------------------------------------
#
#  MAIN JOB FOLDER CLASS
#

# Job folder management: abstraction for the folder hierarchy under
# the cgenie-jobs directory.

class JobFolder:
    def __init__(self, path, name, tree, app):
        self.app = app          # Application object used for timers.
        self.base_path = path   # The base directory for this job folder.
        self.name = name        # The name used for the base directory
                                # in the job tree.
        self.tree = tree        # The job tree widget.
        self.selected = None    # The currently selected item in the
                                # job tree.

        # Dictionaries to record which entries in the job tree are
        # folders and the current status of jobs in the job tree.
        self.folders = { path: 1 }
        self.status = { }

        # Add root entry to job tree and mark it as the current
        # selection.  Note that entries are inserted with IDs that are
        # the full paths to the job directory for jobs.
        self.selected = self.tree.insert('', 'end', self.base_path,
                                         text=self.name, open=True)

        # Walk hierarchy of jobs and folders under base directory and
        # insert into the job tree.
        for p, type in walk_jobs(self.base_path):
            if type == 'JOB':
                self.add_job(os.path.relpath(p, self.base_path))
            else:
                self.add_folder(os.path.relpath(p, self.base_path))

        # Set current selection in job tree.
        self.sort_children(self.selected)
        self.tree.selection_set(self.selected)

        # This timer updates the status icons of jobs displayed in the
        # job tree, so that, for instance, jobs icons switch from
        # "running" to "complete" as they finish.
        self.app.after(500, self.set_statuses)


    def possible_folders(self):
        # Return list of folders in the job tree (used as possible
        # destinations for moving entries).
        fs = self.folders.keys()
        fs.sort()
        return fs


    def add_job(self, jfull, sort=False):
        # Add a job to the job tree.

        # Determine the containing folders for the job.
        ds, j = job_split(jfull)

        # Recursively insert folder entries into the tree (recording
        # their presence in the folders dictionary).
        p = self.base_path
        for f in ds:
            parent = p
            p = os.path.join(p, f)
            if not self.tree.exists(p):
                self.folders[p] = 1
                self.tree.insert(parent, 'end', p, text=f, image=folder_img())

        # Create a job, determine and store its current status and
        # create a suitable entry in the job tree.
        jpath = os.path.join(self.base_path, jfull)
        job = Job(jpath, self)
        self.status[jfull] = job.status
        self.tree.insert(p, 'end', jpath, text=j, image=job.status_img())
        if sort: self.sort_children(self.selected)


    def add_folder(self, ffull, sort=False):
        # Add a folder to the job tree.

        # Determine the containing folders for the new folder.
        ds, j = job_split(os.path.join(ffull, 'DUMMY'))

        # Recursively insert folder entries into the tree (recording
        # their presence in the folders dictionary).
        p = self.base_path
        for f in ds:
            parent = p
            p = os.path.join(p, f)
            if not self.tree.exists(p):
                self.folders[p] = 1
                self.tree.insert(parent, 'end', p, text=f, image=folder_img())
        if sort: self.sort_children(self.selected)


    def is_folder(self, p):
        # Flag whether a given entry is a folder or not.
        return p in self.folders


    def delete(self, p):
        # Delete an entry from the job tree.

        self.tree.delete(p)

        # For folders, we also need to delete any child folders.
        if self.is_folder(p):
            ds = []
            for f in self.folders.keys():
                if f.startswith(p): ds.append(f)
            for d in ds: del self.folders[d]
        if p in self.status: del self.status[p]


    def move(self, fr, to):
        # Move an entry in the job tree and on disk.

        # Move on disk.
        os.rename(fr, to)
        isf = fr in self.folders
        to = os.path.relpath(to, self.base_path)

        # Move in the tree by deleting and re-adding.
        self.delete(fr)
        if isf:
            self.add_folder(to, True)
        else:
            self.add_job(to, True)


    def clone(self, fr, to):
        # Clone a job entry in the job tree and on disk.

        # Clone on disk.
        shutil.copytree(fr, to)

        # Clone in the tree.
        to = os.path.relpath(to, self.base_path)
        self.add_job(to, True)


    def sort_children(self, f):
        # Sort the children of a given entry in the job tree.  Folders
        # sort before jobs, and otherwise entries sort alphabetically.

        # Comparison function based on folder/job determination and
        # name.
        def chcmp(x, y):
            if self.is_folder(x) == self.is_folder(y): return cmp(x, y)
            else:
                if self.is_folder(x): return -1
                else: return 1

        # Get and sort children of the required entry and replace with
        # the sorted version.
        cs = list(self.tree.get_children(f))
        cs.sort(chcmp)
        self.tree.set_children(f, *cs)

        # Recursively sort descendants.
        for c in cs: self.sort_children(c)


    def set_statuses(self):
        # Timer-driven routine to maintain job status icons in job
        # tree.

        # Some entries may have been deleted since last time this was
        # called, so record them for deletion afterwards -- you
        # mustn't try to modify a collection while you're iterating
        # over it...
        dels = []

        # Iterate over statuses.
        for p, s in self.status.iteritems():
            pfull = os.path.join(self.base_path, p)
            schk = job_status(pfull)

            if not schk:
                # If the item no longer exists, record that it needs
                # to be deleted.
                dels.append(p)
            elif schk != s:
                # Otherwise, update its status and icon in the job
                # tree.
                self.status[p] = schk
                self.tree.item(pfull, image=job_status_img(schk))

        # Delete missing entries from the status dictionary.
        for p in dels: del self.status[p]

        # And repeat...
        self.app.after(500, self.set_statuses)


    def find_restart_jobs(self):
        # Find all jobs that are suitable for use as restart jobs,
        # i.e. all completed jobs.
        res = []
        for p, v in self.status.iteritems():
            if v == 'COMPLETE': res.append(p)

        # We add a "<None>" entry for the case where no restart is
        # used.
        res.append('<None>')
        res.sort()
        return res
