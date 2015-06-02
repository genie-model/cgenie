from __future__ import print_function
import os, os.path, shutil, re, glob
import platform as plat
import subprocess as sp
import Tkinter as tk
import tkSimpleDialog as tkSD
import tkMessageBox as tkMB
import tkFont
import ttk

# General GENIE utilities.
import utils as U

# Most of the GUI code is in these modules...
from gui.tooltip import *
from gui.filetreeview import *
from gui.job_folder import *
from gui.job import *
from gui.panels import *
from gui.dialogs import *
from gui.after import *
from gui.util import *


#----------------------------------------------------------------------

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
#
#  MAIN TKINTER APPLICATION CLASS
#

class Application(ttk.Frame, AfterHandler):
    def __init__(self, master=None):
        ttk.Frame.__init__(self, master)
        AfterHandler.__init__(self)
        self.reapable = set()

        # Set up monospaced and bold fonts.
        self.normal_font = tkFont.nametofont('TkDefaultFont')
        self.mono_font = tkFont.nametofont('TkFixedFont')
        self.bold_font = self.normal_font.copy()
        sz = self.normal_font.cget('size')
        self.bold_font.configure(weight='bold', size=int(1.5*sz))

        # Initialise job member and find all available model
        # configurations.
        self.job = Job()
        self.restart_jobs = []
        self.find_configs()

        # Set up UI: this creates the main window (with a menu bar,
        # the "job tree", a button toolbar and a notebook for the main
        # part of the window) plus all the "panels" that appear in the
        # notebook part of the UI.
        self.grid(sticky=tk.N+tk.E+tk.S+tk.W)
        self.create_widgets()

        # Creating this JobFolder object populates the job tree in the
        # UI.
        self.job_folder = JobFolder(U.cgenie_jobs, 'My Jobs', self.tree, self)

        # Find jobs suitable for use as restarts and update the setup
        # panel to use them.
        self.restart_jobs = self.job_folder.find_restart_jobs()
        self.panels['setup'].update()


    #------------------------------------------------------------------
    #
    #  MAIN BUTTON CALLBACKS
    #

    def new_job(self):
        """Create a new job (button press callback)"""

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

        # Write initial placeholder job configuration file.
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
        """Create a new folder (button press callback)"""

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
        """Move or rename a folder or job (button press callback)"""

        # Find current full path to the selected element in the job
        # tree and determine whether it's a job or a folder.
        full_path = self.tree.selection()[0]
        is_folder = self.job_folder.is_folder(full_path)

        # Run the move/rename dialog and act on the result.
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
        """Delete a job or a folder from tree and on disk (button press
           callback)
        """

        # Determine whether a job or a folder is selected.
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
        """Clone a job (button press callback)"""

        # Determine a suitable name for the new job -- the clone has a
        # name derived from the original job, and the user can rename
        # it as they like after cloning.
        p = self.tree.selection()[0]
        pnew = p + '-CLONE'
        i = 1
        while os.path.exists(pnew):
            i += 1
            pnew = p + '-CLONE' + str(i)

        # Clone the job on disk and in the job tree, make it visible
        # and select it.
        self.job_folder.clone(p, pnew)
        self.tree.see(pnew)
        self.tree.selection_set(pnew)


    def clear_job(self):
        """Clear job data (button press callback)"""

        # Confirmation dialog.
        p = self.tree.selection()[0]
        msg = 'Are you sure you want to clear\n'
        msg += 'all output data for this job?\n\n'
        msg += 'This action is IRREVERSIBLE!'
        chk = tkMB.askokcancel('Confirm deletion', msg)
        if not chk: return

        # Clean everything up: status, command and log files, model
        # output, "run segment" configuration storage and GUI restart
        # files.
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

        # Update record of job data and reflect the changes in the
        # panel views.
        self.update_job_data()
        for p in self.panels.itervalues(): p.clear()


    def run_job(self):
        """Run a job (button press callback)"""

        # Check for existence of genie-ship.exe executable and build
        # if necessary.
        exe = os.path.join(U.cgenie_jobs, 'MODELS', U.cgenie_version,
                           platform, 'ship', 'genie.exe')
        runexe = os.path.join(self.job.jobdir, 'genie-ship.exe')
        if not os.path.exists(exe):
            d = BuildExecutableDialog(self, self.job.jobdir)
            if not d.result: return
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
        # in job directory.  Add the resulting process to the
        # "reapable" set.  We check this list periodically and reap
        # (i.e. wait for) any processes that have finished.  On Linux,
        # you need to do this to make sure that you don't end up with
        # a load of defunct "zombie" processes hanging around, waiting
        # for their parent to notice that they've finished.  (This is
        # only really a problem because the existence of a zombie
        # process still marks the executable that the process was
        # running as being in use, which means you can't overwrite it.
        # If you run a job to completion from the GUI, then extend its
        # run length and try to continue it, this means that you can't
        # update the GENIE executable, as we do above in the line that
        # says "shutil.copy(exe, runexe)".  If you try to do that, you
        # get an error saying something like "Text file busy".  The
        # solution is to explicitly clean up these child processes
        # when they finish.  If the GUI exits before the model
        # processes that it starts, those processes become orphaned,
        # which is fine, since the kernel reparents them to the init
        # process and they can happily continue running.)
        with open(os.path.join(self.job.jobdir, 'run.log'), 'a') as fp:
            try:
                pipe = sp.Popen(runexe, cwd=self.job.jobdir,
                                stdout=fp, stderr=sp.STDOUT)
                self.reapable.add(pipe)
            except Exception as e:
                tkMB.showerror('Error', 'Failed to start GENIE executable!')


    def pause_job(self):
        """Pause a running job (button press callback)"""

        # Just write a command file to let the model know that it
        # should pause.
        with open(os.path.join(self.job.jobdir, 'command'), 'w') as fp:
            print('PAUSE', file=fp)



    #----------------------------------------------------------------------
    #
    #  JOB TREE MANAGEMENT
    #

    def item_selected(self, event=None):
        """Callback for item selection in job tree"""

        # If we have a real job selected in the tree, make a Job
        # object for it and recalculate the possible restart jobs for
        # the setup panel.
        jobid = self.tree.selection()[0]
        if (len(self.tree.get_children(jobid)) != 0 or
            self.job_folder.is_folder(jobid)):
            self.job = None
        else:
            self.job = Job(jobid, self.job_folder)
            self.restart_jobs = self.job_folder.find_restart_jobs()

        # Let all the panels know we have a new selected job.
        for p in self.panels.itervalues(): p.set_job(self.job)

        # Set the action button states depending on the job status.
        self.set_job_buttons()


    # Buttons that change state depending on the state of the
    # currently selected job.  Some buttons (add job, add folder,
    # etc.) are always enabled.
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

    def set_job_buttons(self):
        """Enable or disable action buttons depending on the state of the
           selected job.
        """

        sel = self.tree.selection()[0]
        if self.job == None:
            # A folder is selected: for folders other than the
            # top-level "My Jobs" folder, we can move/rename or delete
            # the folder.
            for k, v in self.tool_buttons.iteritems():
                if k in self.switchable_buttons:
                    e = ((k == 'move_rename' or k == 'delete_job')
                         and self.tree.parent(sel) != '')
                    enable(v, e)
        else:
            # A job is selected: the actions that are enabled depend
            # on the job status.
            on_buttons = self.state_buttons[self.job.status]
            for k, v in self.tool_buttons.iteritems():
                if k in self.switchable_buttons:
                    e = k in on_buttons
                    enable(v, e)


    def set_menu_state(self):
        """Enable or disable menu items depending on the state of the selected
           job.
        """

        sel = self.tree.selection()[0]
        if self.job == None:
            # A folder is selected: for folders other than the
            # top-level "My Jobs" folder, we can move/rename or delete
            # the folder.
            for k, v in self.tool_buttons.iteritems():
                if k in self.switchable_buttons:
                    e = ((k == 'move_rename' or k == 'delete_job')
                         and self.tree.parent(sel) != '')
                    self.job_menu.entryconfig(self.menu_items[k],
                                              state=tk.NORMAL if e
                                              else tk.DISABLED)
        else:
            # A job is selected: the actions that are enabled depend
            # on the job status.
            on_buttons = self.state_buttons[self.job.status]
            for k, v in self.tool_buttons.iteritems():
                if k in self.switchable_buttons:
                    e = k in on_buttons
                    self.job_menu.entryconfig(self.menu_items[k],
                                              state=tk.NORMAL if e
                                              else tk.DISABLED)


    def update_job_data(self):
        """Runs on a timer to update job data in panels, maintain action
           button states and clean up child processes.
        """

        if self.job: self.job.set_status()
        self.panels['status'].update()
        self.panels['output'].update()
        self.set_job_buttons()
        self.reap()
        self.after(500, self.update_job_data)


    #----------------------------------------------------------------------
    #
    #  UI SETUP
    #

    def create_widgets(self):
        """UI layout"""

        # The main window is paned, so you can drag the divider
        # between the job tree and the main notebook part of the
        # window.
        self.pane = ttk.PanedWindow(self, orient=tk.HORIZONTAL)

        # The job tree is populated by the creation of the JobFolder
        # object in the application constructor.  Here we just set it
        # up as an empty tree and put it into the paned view.
        self.tree = FileTreeview(self.pane, selectmode='browse')
        self.tree.bind('<<TreeviewSelect>>', self.item_selected)
        self.pane.add(self.tree)

        # The right-hand part of the main window has the button
        # toolbar and the panel notebook, so we make a frame to
        # contain them.
        self.main_frame = ttk.Frame(self.pane)
        self.pane.add(self.main_frame)

        # Create toolbar buttons -- the names of the tools here
        # (e.g. "new_job") refer both to the methods that are called
        # when the buttons are pressed and to the image files that are
        # used to make the buttons (which live in
        # <cgenie_root>/tools/images).  Each of the buttons has
        # floating tooltip help, implemented using a helper class.
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

        # Create a main menu using mostly the same items as appear in
        # the toolbar.  Having a menu as well as a toolbar means that
        # it's possible to add less frequently used actions to the
        # menu so that the main part of the GUI doesn't get too
        # cluttered.
        self.menu = tk.Menu(top)
        self.menu_items = { }
        top['menu'] = self.menu
        self.job_menu = tk.Menu(self.menu, tearoff=0,
                                postcommand=self.set_menu_state)
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

        # Start the background update timer.
        self.after(500, self.update_job_data)


    #----------------------------------------------------------------------
    #
    #  UTILITY FUNCTIONS
    #

    def find_configs(self):
        """Find all base and user configuration files"""

        # Base configuration files -- all in one directory.
        bs = os.listdir(os.path.join(U.cgenie_data, 'base-configs'))
        bs = filter(lambda s: s.endswith('.config'), bs)
        self.base_configs = map(lambda s: s.rpartition('.')[0], bs)
        self.base_configs.sort()

        # User configuration files -- need to walk the directory
        # hierarchy here.
        us = []
        udir = os.path.join(U.cgenie_data, 'user-configs')
        for d, ds, fs in os.walk(udir):
            for f in fs:
                us.append(os.path.relpath(os.path.join(d, f), udir))
        self.user_configs = us
        self.user_configs.sort()


    def reap(self):
        """Reap child processes"""

        # Check the status of all child processes that have been
        # recorded as reapable, reap those that have finished (which
        # is indicated by the Popen.poll method returning something
        # other than None) by waiting for them (which does nothing
        # here except make sure they're removed from the kernel's
        # process table), then removing them from our "reapable" set.
        reaped = set()
        for ch in self.reapable:
            if ch.poll() != None:
                ch.wait()
                reaped.add(ch)
        self.reapable -= reaped


#----------------------------------------------------------------------
#
#  MAIN PROGRAM
#

# The "main program" here just initialises the Tkinter toolkit,
# creates the main Application object, sets a couple of top-level
# properties (window title and geometry and a window manager protocol
# handler to deal with window close events), then fires off the
# Tkinter event loop.  Everything else that happens is driven by
# callbacks from menu or button actions or other GUI component
# interactions.

root = tk.Tk()
app = Application(root)
app.master.title("cGENIE GUI")
app.master.geometry("1024x768")
root.protocol("WM_DELETE_WINDOW", app.quit)
app.mainloop()
