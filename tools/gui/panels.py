from __future__ import print_function
import os.path, glob
import Tkinter as tk
import ttk
import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg

from gui.tailer import *
from gui.tsfile import *
from gui.util import *

#----------------------------------------------------------------------

class Panel(ttk.Frame):
    def __init__(self, notebook, app, type, title):
        self.app = app
        ttk.Frame.__init__(self, notebook)
        self.view_type = type
        self.job = None
        self.grid(column=0, row=0, padx=5, pady=5, sticky=tk.N+tk.S+tk.E+tk.W)
        notebook.add(self, text=title)

    def set_job(self, job):
        self.job = job
        self.update()

    def label(self, t, row, column=0, font=None):
        lab = ttk.Label(self, text=t, font=font)
        lab.grid(column=column, row=row, pady=5, padx=5, sticky=tk.W)
        return lab


class StatusPanel(Panel):
    def __init__(self, notebook, app):
        """Initial creation of status panel"""

        Panel.__init__(self, notebook, app, 'status', 'Status')

        self.label('Job path:', 0, font=self.app.bold_font)
        self.job_path = ttk.Label(self, font=self.app.bold_font)
        self.job_path.grid(column=1, row=0, pady=5, sticky=tk.W)

        self.label('Job status:', 1)
        self.job_status = ttk.Label(self)
        self.job_status.grid(column=1, row=1, pady=5, sticky=tk.W)

        self.label('Run length:', 2)
        self.runlen = ttk.Label(self)
        self.runlen.grid(column=1, row=2, pady=5, sticky=tk.W)

        self.label('T100:', 3)
        self.t100 = ttk.Label(self)
        self.t100.grid(column=1, row=3, pady=5, sticky=tk.W)

        self.update()

    def update(self):
        """Setting status panel fields"""

        if not self.job:
            self.job_path.configure(text='')
            self.job_status.configure(text='')
            self.runlen.configure(text='')
            self.t100.configure(text='')
        else:
            self.job_path.configure(text=self.job.jobdir_str())
            s = self.job.status_str()
            if s == 'RUNNING':
                s += ' (' + format(self.job.pct_done(), '.2f') + '%)'
            self.job_status.configure(text=s)
            self.runlen.configure(text=self.job.runlen_str())
            self.t100.configure(text=self.job.t100_str())


class SetupPanel(Panel):
    def __init__(self, notebook, app):
        """Initial creation of setup panel"""

        Panel.__init__(self, notebook, app, 'setup', 'Setup')

        self.label('Job path:', 0, font=self.app.bold_font)
        self.job_path = ttk.Label(self, font=self.app.bold_font)
        self.job_path.grid(column=1, row=0, pady=5, sticky=tk.W)

        self.label('Run segment:', 1)
        ### ===> TODO: Set up segments from job data
        self.segments = ('1: 1-END [CURRENT]',)
        self.segment_var = tk.StringVar()
        self.segment_sel = ttk.OptionMenu(self, self.segment_var,
                                          None, *self.segments,
                                          command=self.segment_changed)
        self.segment_sel.grid(column=1, row=1, pady=5, sticky=tk.W)
        self.segment_var.set(self.segments[0])
        enable(self.segment_sel, len(self.segments) > 1)

        self.label('Base config:', 2)
        self.base_config = ttk.Combobox(self, values=self.app.base_configs,
                                        width=80)
        self.base_config.bind('<<ComboboxSelected>>', self.state_change)
        self.base_config.state(['readonly'])
        self.base_config.grid(column=1, row=2, pady=5, sticky=tk.W)

        self.label('User config:', 3)
        self.user_config = ttk.Combobox(self, values=self.app.user_configs,
                                        width=80)
        self.user_config.bind('<<ComboboxSelected>>', self.state_change)
        self.user_config.state(['readonly'])
        self.user_config.grid(column=1, row=3, pady=5, sticky=tk.W)

        self.label('Modifications:', 4)
        self.mods_frame = ttk.Frame(self)
        self.mods_frame.grid(column=1, row=4, pady=5, sticky=tk.W)
        self.mods = tk.Text(self.mods_frame, width=80, height=20,
                            font=self.app.normal_font)
        self.mods.bind('<<Modified>>', self.state_change)
        self.mods_scroll = ttk.Scrollbar(self.mods_frame,
                                         command=self.mods.yview)
        self.mods['yscrollcommand'] = self.mods_scroll.set
        self.mods.grid(column=0, row=0, sticky=tk.W)
        self.mods_scroll.grid(column=1, row=0, sticky=tk.N+tk.S)

        self.label('Run length:', 5)
        self.check = self.register(self.check_runlen)
        self.runlen_var = tk.StringVar()
        self.runlen = ttk.Entry(self, width=20, validate='all',
                                textvariable=self.runlen_var,
                                validatecommand=(self.check, '%P'))
        self.runlen.grid(column=1, row=5, pady=5, sticky=tk.W)
        self.runlen_var.trace('w', self.state_change)

        self.label('T100:', 6)
        self.t100_var = tk.IntVar()
        self.t100 = ttk.Checkbutton(self, variable=self.t100_var,
                                    command=self.state_change)
        self.t100.grid(column=1, row=6, pady=5, sticky=tk.W)

        self.label('Restart from:', 7)
        self.restart = ttk.Combobox(self, values=self.app.restart_jobs,
                                    width=80)
        self.restart.bind('<<ComboboxSelected>>', self.state_change)
        self.restart.state(['readonly'])
        self.restart.grid(column=1, row=7, pady=5, sticky=tk.W)

        self.but_frame = ttk.Frame(self)
        self.but_frame.grid(column=1, row=8, pady=5, sticky=tk.W)
        self.save_button = ttk.Button(self.but_frame, text="Save changes",
                                      command=self.save_changes)
        self.revert_button = ttk.Button(self.but_frame, text="Revert changes",
                                        command=self.revert_changes)
        self.save_button.grid(column=0, row=0)
        self.revert_button.grid(column=1, row=0, padx=5)

        self.edited = False
        self.complete = False

        self.update()

    def check_runlen(self, s):
        try:
            v = s.strip()
            if not v: return True
            return int(v) > 0
        except:
            return False

    def set_button_state(self):
        enable(self.save_button, self.edited and self.complete)
        enable(self.revert_button, self.edited)

    def set_state(self):
        self.complete = False
        self.edited = False
        if self.job:
            self.complete = (self.base_config.get() and
                             self.user_config.get() and
                             self.runlen.get())
            if (self.base_config.get() and
                self.base_config.get() != self.job.base_config):
                self.edited = True
            if (self.user_config.get() and
                self.user_config.get() != self.job.user_config):
                self.edited = True
            if (self.mods.get('1.0', 'end').rstrip() != self.job.mods.rstrip()):
                self.edited = True
            if (self.runlen.get() and
                int(self.runlen.get()) != self.job.runlen):
                self.edited = True
            if (self.t100_var.get() != self.job.t100):
                self.edited = True

    def state_change(self, event=None, dummy1=None, dummy2=None):
        self.set_state()
        self.set_button_state()
        self.mods.edit_modified(False)

    def segment_changed(self, event):
        ### ===> TODO: save current segment values and switch display
        ###      to segment values if not current segment and disable
        ###      editing; if current segment, restore values and
        ###      button state from saved state.
        print('segment_changed')

    def save_changes(self):
        self.job.base_config = self.base_config.get()
        self.job.user_config = self.user_config.get()
        self.job.mods = self.mods.get('1.0', 'end').rstrip()
        new_runlen = int(self.runlen_var.get())
        runlen_increased = (self.job.runlen != None and
                            new_runlen > self.job.runlen)
        self.job.runlen = new_runlen
        self.job.t100 = True if self.t100_var.get() else False
        r = self.restart.get()
        if r == '<None>': r = None
        self.job.restart = r
        self.job.write_config()
        self.job.gen_namelists()
        self.job.set_status(runlen_increased)
        self.app.tree.item(self.job.jobdir, image=self.job.status_img())
        for p in self.app.panels.itervalues():
            if p != self: p.update()
        self.set_state()
        self.set_button_state()

    def revert_changes(self):
        self.base_config.set(self.job.base_config if self.job.base_config
                             else '')
        self.user_config.set(self.job.user_config if self.job.user_config
                             else '')
        self.mods.delete('1.0', 'end')
        if self.job.mods: self.mods.insert('end', self.job.mods)
        self.runlen.delete(0, 'end')
        if self.job.runlen != None:
            self.runlen.insert('end', str(self.job.runlen))
        self.t100_var.set(bool(self.job.t100))
        self.restart.set(self.job.restart if self.job.restart else '<None>')
        self.state_change(None)

    def update(self):
        """Setting setup panel fields"""

        self.restart.configure(values=self.app.restart_jobs)
        self.base_config.set('')
        self.user_config.set('')
        self.restart.set('<None>')
        self.mods.delete('1.0', 'end')
        self.runlen.delete(0, 'end')
        self.t100_var.set(False)
        self.segments = ('1: 1-END [CURRENT]',)
        self.segment_sel.set_menu(self.segments[0], *self.segments)
        enable(self.segment_sel, len(self.segments) > 1)
        self.set_button_state()
        if not self.job: return
        self.job_path.configure(text=self.job.jobdir_str())
        if self.job.base_config:
            self.base_config.set(self.job.base_config
                                 if self.job.base_config != '?' else '')
        else:
            self.base_config.set('')
        if self.job.user_config:
            self.user_config.set(self.job.user_config
                                 if self.job.user_config != '?' else '')
        else:
            self.user_config.set('')
        self.restart.set(self.job.restart if self.job.restart else '<None>')
        self.mods.delete('1.0', 'end')
        if self.job.mods: self.mods.insert('end', self.job.mods)
        self.runlen.delete(0, 'end')
        if self.job.runlen != None:
            self.runlen.insert('end', str(self.job.runlen))
        self.t100_var.set(bool(self.job.t100))
        if not self.job.segments:
            self.segments = ('1: 1-END [CURRENT]',)
        else:
            self.segments = []
            i = 1
            for kstart, kend in self.job.segments:
                self.segments.append(str(i) + ': ' +
                                     str(kstart) + '-' + str(kend))
                i += 1
            self.segments.append(str(i) + ': ' + str(kend+1) + '-END')
            self.segments.reverse()
            self.segments = tuple(self.segments)
        self.segment_sel.set_menu(self.segments[0], *self.segments)
        self.set_state()
        self.set_button_state()


class NamelistPanel(Panel):
    def __init__(self, notebook, app):
        Panel.__init__(self, notebook, app, 'namelists', 'Namelists')

        self.sel_frame = ttk.Frame(self)
        lab = ttk.Label(self.sel_frame, text='Namelist:')

        nls = ()
        self.namelists = { }
        self.nl_var = tk.StringVar()
        self.nl_sel = ttk.OptionMenu(self.sel_frame, self.nl_var, None, *nls,
                                     command=self.set_namelist_text)

        self.out = tk.Text(self, font=self.app.mono_font,
                           state=tk.DISABLED, wrap=tk.NONE)
        self.out_scroll = ttk.Scrollbar(self, command=self.out.yview)
        self.out['yscrollcommand'] = self.out_scroll.set

        self.columnconfigure(0, weight=1)
        self.rowconfigure(0, weight=0)
        self.rowconfigure(1, weight=1)
        self.sel_frame.grid(column=0, row=0, sticky=tk.W, pady=5)
        lab.grid(column=0, row=0, padx=5, pady=5, sticky=tk.W)
        self.nl_sel.grid(column=1, row=0, stick=tk.W)
        self.out.grid(column=0, row=1, sticky=tk.E+tk.W+tk.N+tk.S)
        self.out_scroll.grid(column=1, row=1, sticky=tk.N+tk.S)

    def set_namelist_text(self, event=None):
        self.out['state'] = tk.NORMAL
        self.out.delete('1.0', 'end')
        if self.nl_var.get():
            self.out.insert('end', self.namelists[self.nl_var.get()])
        self.out['state'] = tk.DISABLED

    def update(self):
        nls = ()
        self.namelists = { }
        if self.job:
            nls = []
            for nl in glob.iglob(os.path.join(self.job.jobdir, 'data_*')):
                nlname = os.path.basename(nl)[5:]
                nls.append(nlname)
                with open(nl) as fp: self.namelists[nlname] = fp.read()
            nls.sort()
            nls = tuple(nls)
            self.nl_sel.set_menu(None if not nls else nls[0], *nls)
            self.nl_var.set(nls[0] if nls else '')
            enable(self.nl_sel, True if nls else False)
            self.set_namelist_text()
        else:
            self.nl_sel.set_menu(None, *nls)
            self.nl_var.set('')
            enable(self.nl_sel, False)
            self.set_namelist_text()


class OutputPanel(Panel):
    def __init__(self, notebook, app):
        Panel.__init__(self, notebook, app, 'output', 'Output')

        self.tailer = None
        self.tailer_job = None
        self.output_text = ''

        self.out = tk.Text(self, font=self.app.mono_font, state=tk.DISABLED)
        self.out_scroll = ttk.Scrollbar(self, command=self.out.yview)
        self.out['yscrollcommand'] = self.out_scroll.set

        self.columnconfigure(0, weight=1)
        self.rowconfigure(0, weight=1)
        self.out.grid(column=0, row=0, sticky=tk.E+tk.W+tk.N+tk.S)
        self.out_scroll.grid(column=1, row=0, sticky=tk.N+tk.S)

    def add_output_text(self, t, clear=False):
        self.out['state'] = tk.NORMAL
        if clear:
            self.output_text = t
            self.out.delete('1.0', 'end')
        else:
            self.output_text += t
        atend = self.out_scroll.get()[1] == 1.0
        self.out.insert('end', t)
        self.out['state'] = tk.DISABLED
        if atend or clear: self.out.see('end')

    def clear(self):
        if self.tailer: self.tailer.stop()
        self.tailer = None
        self.tailer_job = None
        self.add_output_text('', clear=True)

    def update(self):
        if not self.job: self.clear()
        else:
            log = os.path.join(self.job.jobdir, 'run.log')
            if not os.path.exists(log): self.add_output_text('', clear=True)
            if self.tailer and self.tailer_job != self.job: self.clear()
            if not self.tailer:
                self.tailer_job = self.job
                self.tailer = Tailer(self.app, log)
                self.tailer.start(self.add_output_text)


class PlotPanel(Panel):
    def __init__(self, notebook, app):
        Panel.__init__(self, notebook, app, 'plots', 'Plots')

        self.plot_job = None

        self.fig = plt.figure(figsize=(5,4), dpi=100)
        self.ax = self.fig.add_subplot(111)
        self.plot = None

        self.choice_frame = ttk.Frame(self)
        lab = ttk.Label(self.choice_frame, text='Data file:')
        lab.pack(side=tk.LEFT, padx=5)
        self.files = ()
        self.file_var = tk.StringVar()
        self.file_sel = ttk.OptionMenu(self.choice_frame, self.file_var,
                                       None, *self.files,
                                       command=self.file_changed)
        enable(self.file_sel, False)
        self.file_sel.pack(side=tk.LEFT, padx=5)
        lab = ttk.Label(self.choice_frame, text='')
        lab.pack(side=tk.LEFT, padx=5)
        lab = ttk.Label(self.choice_frame, text='Variable:')
        lab.pack(side=tk.LEFT, padx=5)
        self.vars = ()
        self.var_var = tk.StringVar()
        self.var_sel = ttk.OptionMenu(self.choice_frame, self.var_var,
                                      None, *self.vars,
                                      command=self.var_changed)
        enable(self.var_sel, False)
        self.var_sel.pack(side=tk.LEFT, padx=5)
        self.canvas = FigureCanvasTkAgg(self.fig, master=self)
        self.choice_frame.pack(side=tk.TOP, pady=10, anchor=tk.NW)
        self.canvas.get_tk_widget().pack(side=tk.TOP, fill=tk.BOTH, expand=1)

    def clear(self):
        self.plot_job = None
        self.files = ()
        self.vars = ()
        self.file_sel.set_menu(None, *self.files)
        self.file_var.set('')
        enable(self.file_sel, False)
        self.var_sel.set_menu(None, *self.vars)
        self.var_var.set('')
        enable(self.var_sel, False)
        self.output_files = { }
        self.ax.clear()
        self.canvas.draw()
        self.after(0, self.check_job_files)

    def update(self):
        if self.job != self.plot_job:
            self.clear()
            self.plot_job = self.job
            if self.job: self.after(0, self.check_job_files)

    def check_job_files(self):
        if self.job and not self.files:
            self.output_files = self.job.check_output_files()
            self.files = self.output_files.keys()
            if self.files:
                self.files.sort()
                self.file_sel.set_menu(self.files[0], *self.files)
                self.file_var.set(self.files[0])
                enable(self.file_sel, True)
                self.file_changed()
            self.after(500, self.check_job_files)

    def file_changed(self, event=None):
        if self.file_var.get():
            tsp = self.output_files[self.file_var.get()]
            self.vars = ()
            self.var_sel.set_menu(None, *self.vars)
            self.var_var.set('')
            enable(self.var_sel, False)
            self.ts_file = TimeSeriesFile(self.app, tsp, self.data_update)
        else:
            self.ts_file = None

    def data_update(self, tnew, dnew):
        if self.vars == ():
            self.vars = self.ts_file.vars
            self.var_sel.set_menu(None, *self.vars)
            enable(self.var_sel, True)
            self.ax.clear()
            if len(self.vars) >= 1:
                self.var_var.set(self.vars[0])
                self.var_changed()
            else:
                self.canvas.draw()
        else:
            self.plot.set_xdata(self.ts_file.time)
            self.plot.set_ydata(self.ts_file.data[self.var_var.get()])
            self.ax.relim()
            self.ax.autoscale_view()
            self.canvas.draw()

    def var_changed(self, event=None):
        self.ax.clear()
        t = self.ts_file.time
        d = self.ts_file.data[self.var_var.get()]
        self.plot, = self.ax.plot(t, d)
        self.ax.set_xlabel('Time (yr)')
        self.ax.set_ylabel(self.var_var.get())
        self.canvas.draw()
