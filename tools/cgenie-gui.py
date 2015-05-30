from __future__ import print_function
import os, os.path, shutil, re, glob
import subprocess as sp
import Tkinter as tk
import tkSimpleDialog as tkSD
import tkMessageBox as tkMB
import tkFont
import ttk

import utils as U

from gui.tooltip import *
from gui.job_folder import *
from gui.job import *
from gui.panels import *
from gui.dialogs import *


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

class Application(ttk.Frame):
    def __init__(self, master=None):
        self.root = root
        self.aft_c2id = { }
        self.aft_id2c = { }
        self.aft_n = 0
        ### ===> TODO: Sort out fonts
        self.normal_font = ttk.Style().lookup('TEntry', 'font')
        self.mono_font = tkFont.Font(family='liberation mono', size=10)
        self.bold_font = tkFont.Font(family='droid sans', weight='bold')
        self.big_font = tkFont.Font(family='droid sans', size=16, weight='bold')
        self.jobid = None
        self.job = Job()
        ttk.Frame.__init__(self, master)
        self.grid(sticky=tk.N+tk.E+tk.S+tk.W)
        self.find_configs()
        self.create_widgets()
        self.job_folder = JobFolder(U.cgenie_jobs, 'My Jobs', self.tree, self)


    #------------------------------------------------------------------
    #
    #  AFTER TIMER HANDLING
    #
    # This is a little nasty: we use a lot of "after" timers, and
    # Tkinter doesn't seem to have a built-in way to clean them all up
    # before exit.  If you don't clean them up, the application hangs
    # on exit with a bunch of error messages.  So, we override the
    # after handling here to make sure everything does get cleaned up
    # before exit.

    def after(self, ms, func=None, *args):
        id = ttk.Frame.after(self, ms, self.trigger, self.aft_n, func, *args)
        self.aft_c2id[self.aft_n] = id
        self.aft_id2c[id] = self.aft_n
        self.aft_n += 1
        return id

    def after_cancel(self, id):
        del self.aft_c2id[self.aft_id2c[id]]
        del self.aft_id2c[id]
        ttk.Frame.after_cancel(self, id)

    def trigger(self, c, func, *args):
        del self.aft_id2c[self.aft_c2id[c]]
        del self.aft_c2id[c]
        func(*args)

    def quit(self):
        for id in self.aft_id2c.keys(): ttk.Frame.after_cancel(self, id)
        ttk.Frame.quit(self)


    #------------------------------------------------------------------
    #
    #  MAIN BUTTON CALLBACKS
    #

    def new_job(self):
        """Callback for new job button press"""

        # Get folder location for new job.
        loc = self.tree.selection()[0]
        while not self.job_folder.is_folder(loc):
            loc = self.tree.parent(loc)

        # Get new job name and check.
        job_name = tkSD.askstring("New job", "Name for new job")
        if not job_name: return
        jobdir = os.path.join(loc, job_name)
        jobid = os.path.relpath(jobdir, self.job_folder.base_path)
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
        is_folder = self.job_folder.is_folder(full_path)
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
        if self.job_folder.is_folder(p):
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
        if os.path.exists(os.path.join(p, 'command')):
            os.remove(os.path.join(p, 'command'))
        if os.path.exists(os.path.join(p, 'run.log')):
            os.remove(os.path.join(p, 'run.log'))
        for d, ds, fs in os.walk(os.path.join(p, 'output')):
            for f in fs: os.remove(os.path.join(d, f))
        if os.path.exists(os.path.join(p, 'config', 'seglist')):
            os.remove(os.path.join(p, 'config', 'seglist'))
        if os.path.exists(os.path.join(p, 'config', 'segments')):
            shutil.rmtree(os.path.join(p, 'config', 'segments'))
        for f in glob.iglob(os.path.join(p, 'gui_restart_*.nc')):
            os.remove(f)
        self.panels['output'].clear()
        self.panels['plots'].clear()
        self.update_job_data()


    def run_job(self):
        # Check for existence of genie-ship.exe executable.
        exe = os.path.join(U.cgenie_jobs, 'MODELS', U.cgenie_version,
                           platform, 'ship', 'genie.exe')
        runexe = os.path.join(self.job.jobdir, 'genie-ship.exe')
        if not os.path.exists(exe):
            tkMB.showerror('Error', 'GENIE executable missing!')
            return
        shutil.copy(exe, runexe)

        # Set up GUI_RESTART command file if this is a restart after a
        # pause.
        command = os.path.join(self.job.jobdir, 'command')
        if os.path.exists(command): os.remove(command)
        jpath = os.path.relpath(self.job.jobdir, self.job_folder.base_path)
        if self.job_folder.status[jpath] == 'PAUSED':
            st, koverall, dum, genie_clock = self.job.status_params()
            with open(command, 'w') as fp:
                print('GUI_RESTART', koverall, genie_clock, file=fp)

        # Start executable with stdout and stderr directed to run.log
        # in job directory.
        with open(os.path.join(self.job.jobdir, 'run.log'), 'a') as fp:
            try:
                sp.Popen(runexe, cwd=self.job.jobdir,
                         stdout=fp, stderr=sp.STDOUT)
            except Exception as e:
                tkMB.showerror('Error', 'Failed to start GENIE executable!')


    def pause_job(self):
        with open(os.path.join(self.job.jobdir, 'command'), 'w') as fp:
            print('PAUSE', file=fp)


    # Buttons that change state depending on the state of the
    # currently selected job.
    switchable_buttons = ['move_rename', 'delete_job', 'clear_job',
                          'clone_job', 'run_job', 'pause_job']

    # Enabled buttons for different states of selected job.
    state_buttons = { 'UNCONFIGURED': ['move_rename', 'clear_job', 'delete_job',
                                       'clone_job'],
                      'RUNNABLE': ['move_rename', 'clear_job', 'delete_job',
                                   'clone_job', 'run_job'],
                      'RUNNING': ['pause_job'],
                      'PAUSED': ['move_rename', 'clear_job', 'delete_job',
                                 'clone_job', 'run_job'],
                      'COMPLETE': ['move_rename', 'clear_job', 'delete_job',
                                   'clone_job'],
                      'ERRORED': ['move_rename', 'clear_job', 'delete_job',
                                  'clone_job'] }


    def item_selected(self, event=None):
        """Callback for item selection in job tree"""

        sel = self.tree.selection()[0]
        if (len(self.tree.get_children(sel)) != 0 or
            self.job_folder.is_folder(sel)):
            self.select_job(None)
        else:
            self.select_job(sel)
        self.set_job_buttons()


    def set_job_buttons(self):
        sel = self.tree.selection()[0]
        if self.job == None:
            for k, v in self.tool_buttons.iteritems():
                if k in self.switchable_buttons:
                    e = ((k == 'move_rename' or k == 'delete_job')
                         and self.tree.parent(sel) != '')
                    v.state(['!disabled' if e else 'disabled'])
                    self.job_menu.entryconfig(self.menu_items[k],
                                              state=tk.NORMAL if e
                                              else tk.DISABLED)
        else:
            on_buttons = self.state_buttons[self.job.status]
            for k, v in self.tool_buttons.iteritems():
                if k in self.switchable_buttons:
                    e = k in on_buttons
                    v.state(['!disabled' if e else 'disabled'])
                    self.job_menu.entryconfig(self.menu_items[k],
                                              state=tk.NORMAL if e
                                              else tk.DISABLED)


    def select_job(self, jobid):
        """Select a job and set up information tracking"""

        if jobid:
            self.job = Job(jobid, self.job_folder)
        else:
            self.job = None
        for p in self.panels.itervalues(): p.set_job(self.job)


    def update_job_data(self):
        s = None
        if self.job and self.job.jobdir:
            s = self.job.status
            self.job.set_status()
        self.panels['status'].update()
        self.panels['output'].update()
        ### ===> TODO: update plot panels
        if self.job and self.job.jobdir and s != self.job.status:
            self.tree.item(self.job.jobdir, image=self.job.status_img())
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
        tool_info = [['new_job',     'New job', True],
                     ['new_folder',  'New folder', True],
                     ['move_rename', 'Move/rename job', True],
                     ['delete_job',  'Delete job', True],
                     ['clear_job',   'Clear job output', True],
                     ['clone_job',   'Clone job', True],
                     ['spacer', '', False],
                     ['run_job',     'Run job', False],
                     ['pause_job',   'Pause job', False]]
        for t, title, dia in tool_info:
            if t == 'spacer':
                f = ttk.Frame(self.toolbar, height=16)
                f.pack()
            else:
                img = tk.PhotoImage(file=os.path.join(U.cgenie_root, 'tools',
                                                      'images', t + '.gif'))
                b = ttk.Button(self.toolbar, image=img,
                               command=getattr(self, t))
                b.image = img
                b.pack()
                ToolTip(b, title)
                self.tool_buttons[t] = b

        # Set up default notebook panels.
        self.notebook = ttk.Notebook(self.main_frame)
        self.panels = { }
        self.panels['status'] = StatusPanel(self.notebook, self)
        self.panels['setup'] = SetupPanel(self.notebook, self)
        self.panels['namelists'] = NamelistPanel(self.notebook, self)
        self.panels['output'] = OutputPanel(self.notebook, self)
        self.panels['plots'] = PlotPanel(self.notebook, self)

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
        self.menu_items = { }
        top['menu'] = self.menu
        self.job_menu = tk.Menu(self.menu, tearoff=0)
        self.menu.add_cascade(label='Job', menu=self.job_menu)
        it = 0
        for t, title, dia in tool_info:
            if t == 'spacer':
                self.job_menu.add_separator()
            else:
                if dia: title += '...'
                c = getattr(self, t)
                self.job_menu.add_command(label=title, command=c)
                self.menu_items[t] = it
            it += 1
        self.job_menu.add_separator()
        self.job_menu.add_command(label='Quit', command=self.quit)
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
root.protocol("WM_DELETE_WINDOW", app.quit)
app.mainloop()
