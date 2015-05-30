import os.path
import Tkinter as tk
import tkSimpleDialog as tkSD
import tkMessageBox as tkMB
import ttk

class MoveRenameDialog(tkSD.Dialog):
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
        tkSD.Dialog.__init__(self, parent, 'Move/rename job')

    def destroy(self):
        tkSD.Dialog.destroy(self)

    def body(self, master):
        lab = ttk.Label(master, text='Folder:')
        lab.grid(column=0, row=0, pady=5, padx=5, sticky=tk.W)
        self.folder = ttk.Combobox(master, values=self.folders, width=50)
        self.folder.state(['readonly'])
        self.folder.grid(column=1, row=0, pady=5, sticky=tk.W)
        self.folder.set(self.orig_folder)

        lab = ttk.Label(master, text='Name:')
        lab.grid(column=0, row=1, pady=5, padx=5, sticky=tk.W)
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
