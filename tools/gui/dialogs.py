import os, os.path, sys
import subprocess as sp
import Tkinter as tk
import tkMessageBox as tkMB
import ttk

import utils as U

from tailer import *
from util import *

# Fixed version of base dialog class from tkSimpleDialog.

class SimpleDialog(tk.Toplevel):
    def __init__(self, parent, title = None):
        tk.Toplevel.__init__(self, parent)
        self.withdraw() # remain invisible for now
        # If the master is not viewable, don't
        # make the child transient, or else it
        # would be opened withdrawn
        if parent.winfo_viewable(): self.transient(parent)

        if title: self.title(title)
        self.parent = parent
        self.result = None
        body = ttk.Frame(self)
        self.initial_focus = self.body(body)
        body.grid(row=0, column=0, sticky=tk.N+tk.E+tk.S+tk.W)
        box = self.buttonbox()
        box.grid(row=1, column=0, sticky=tk.E+tk.S+tk.W)
        top = self.winfo_toplevel()
        top.rowconfigure(0, weight=1)
        top.rowconfigure(1, weight=0)
        top.columnconfigure(0, weight=1)

        if not self.initial_focus: self.initial_focus = self
        self.protocol("WM_DELETE_WINDOW", self.cancel)
        if self.parent is not None:
            self.geometry("+%d+%d" % (parent.winfo_rootx()+50,
                                      parent.winfo_rooty()+50))
        self.deiconify() # become visibile now
        self.initial_focus.focus_set()

        # wait for window to appear on screen before calling grab_set
        self.wait_visibility()
        self.grab_set()
        self.wait_window(self)

    def destroy(self):
        self.initial_focus = None
        tk.Toplevel.destroy(self)

    def body(self, master):
        pass

    def buttonbox(self):
        box = ttk.Frame(self)
        self.cancel_button = ttk.Button(box, text="Cancel",
                                        width=10, command=self.cancel)
        self.cancel_button.pack(side=tk.RIGHT, padx=5, pady=5)
        self.ok_button = ttk.Button(box, text="OK", width=10,
                                    command=self.ok, default=tk.ACTIVE)
        self.ok_button.pack(side=tk.RIGHT, padx=5, pady=5)
        self.bind("<Return>", self.ok)
        self.bind("<Escape>", self.cancel)
        return box

    def ok(self, event=None):
        if not self.validate():
            self.initial_focus.focus_set() # put focus back
            return

        self.withdraw()
        self.update_idletasks()

        try:
            self.apply()
        finally:
            self.cancel()

    def cancel(self, event=None):
        if self.parent is not None:
            self.parent.focus_set()
        self.destroy()

    def validate(self):
        return 1

    def apply(self):
        pass


class MoveRenameDialog(SimpleDialog):
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
        SimpleDialog.__init__(self, parent, 'Move/rename job')


    def body(self, master):
        lab = ttk.Label(master, text='Folder:')
        lab.grid(column=0, row=0, pady=5, padx=5, sticky=tk.W)
        self.folder = ttk.Combobox(master, values=self.folders, width=50)
        self.folder.state(['readonly'])
        self.folder.grid(column=1, row=0, pady=5, sticky=tk.W+tk.E)
        self.folder.set(self.orig_folder)

        lab = ttk.Label(master, text='Name:')
        lab.grid(column=0, row=1, pady=5, padx=5, sticky=tk.W+tk.E)
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



class BuildExecutableDialog(SimpleDialog):
    def __init__(self, app, dir, parent=None):
        if not parent: parent = tk._default_root
        self.app = app
        self.dir = dir
        self.state = 'PENDING'
        self.result = False
        self.tailer = None
        self.pipe = None
        SimpleDialog.__init__(self, parent, 'Build model executable')


    def destroy(self):
        if self.state == 'RUNNING':
            if self.tailer: self.tailer.stop()
            self.tailer = None
            self.message('KILLING EXECUTABLE BUILD')
            self.pipe.terminate()
            self.pipe.wait()
        SimpleDialog.destroy(self)


    def body(self, master):
        msg = 'GENIE executable needs to be rebuilt\n\n'
        msg += 'Please wait...'
        lab = ttk.Label(master, text=msg,
                        font=self.app.bold_font)
        lab.grid(column=0, row=0, pady=5, padx=5, sticky=tk.W+tk.N)

        self.out_frame = ttk.Frame(master)
        self.out = tk.Text(self.out_frame, width=80,
                           font=self.app.mono_font, state=tk.DISABLED)
        self.out_scroll = ttk.Scrollbar(self.out_frame, command=self.out.yview)
        self.out['yscrollcommand'] = self.out_scroll.set
        master.rowconfigure(0, weight=0)
        master.rowconfigure(1, weight=1)
        master.columnconfigure(0, weight=1)
        self.out_frame.rowconfigure(0, weight=1)
        self.out_frame.columnconfigure(0, weight=1)
        self.out_frame.columnconfigure(1, weight=0)
        self.out_frame.grid(column=0, row=1, padx=5, sticky=tk.N+tk.E+tk.S+tk.W)
        self.out.rowconfigure(0, weight=1)
        self.out.columnconfigure(0, weight=1)
        self.out.grid(column=0, row=0, sticky=tk.N+tk.E+tk.S+tk.W, pady=5)
        self.out_scroll.grid(column=1, row=0, sticky=tk.N+tk.S, pady=5)

        self.message('STARTING EXECUTABLE BUILD')
        go = os.path.join(U.cgenie_root, 'tools', 'go.py')
        cmd = [sys.executable, '-u', go, 'build']
        model_config = U.ModelConfig('ship', self.dir)
        model_dir = model_config.directory()
        log = os.path.join(model_dir, 'build.log')
        if os.path.exists(log): os.remove(log)
        with open(os.devnull, 'w') as sink:
            try:
                self.pipe = sp.Popen(cmd, cwd=self.dir,
                                     stdout=sink, stderr=sink)
                self.state = 'RUNNING'
                self.tailer = Tailer(self.app, log)
                self.tailer.start(self.add_output)
            except Exception as e:
                self.message('FAILED TO START BUILD:\n\n' + str(e) + '\n')
                self.state = 'FAILED'

        return self.out


    def buttonbox(self):
        box = SimpleDialog.buttonbox(self)
        enable(self.ok_button, False)
        return box


    def message(self, s):
        self.out['state'] = tk.NORMAL
        self.out.insert(tk.END, 79 * '*' + '\n')
        self.out.insert(tk.END, '\n')
        self.out.insert(tk.END, '    ' + s + '\n')
        self.out.insert(tk.END, '\n')
        self.out.insert(tk.END, 79 * '*' + '\n')
        self.out.insert(tk.END, '\n')
        self.out['state'] = tk.DISABLED
        self.out.see(tk.END)


    def add_output(self, t):
        if self.pipe.poll() != None:
            self.state = 'COMPLETE'
            self.message('BUILD COMPLETE')
            enable(self.ok_button, True)
        self.out['state'] = tk.NORMAL
        atend = self.out_scroll.get()[1] == 1.0
        self.out.insert('end', t)
        self.out['state'] = tk.DISABLED
        if atend: self.out.see('end')


    def validate(self):
        if self.state != 'COMPLETE' and self.state != 'FAILED':
            tkMB.showwarning('Building',
                             "Build is not yet complete!",
                             parent=self)
            return 0
        return 1
