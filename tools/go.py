#!/usr/bin/env python2

from __future__ import print_function
import os, os.path, sys, shutil, argparse, glob
import subprocess as sp
import platform as plat
try:
    import Queue as qu
    import threading as thr
    import Tkinter as tk
    import tkFont
    import tkMessageBox
    import ttk
    gui_available = True
except:
    gui_available = False

import utils as U


if gui_available:
    # GUI CODE

    def iter_except(function, exception):
        """Works like builtin 2-argument `iter()`, but stops on `exception`."""
        try:
            while True:
                yield function()
        except exception:
            return

    def kill_after(r, p, countdown):
        if p.poll() is None:
            countdown -= 1
            if countdown < 0:
                p.kill()
            else:
                r.after(1000, kill_after, r, p, countdown)
                return
        p.wait()

    class Application(tk.Frame):
        def clear(self):
            self.stop()
            self.text.delete('1.0', tk.END)

        def clean(self):
            self.clear()
            clean(False)

        def cleaner(self):
            self.clear()
            clean(True)

        def build(self):
            self.clear()
            self.buttons_active(False)
            build(lambda: self.buttons_active(True))

        def run(self):
            self.clear()
            self.buttons_active(False)
            def c():
                run(lambda: self.buttons_active(True))
            build(c)

        def stop(self):
            if self.proc:
                p = self.proc
                self.proc = None
                self.cont = None
                self.contrest = None
                p.terminate()
                kill_after(self.root, p, countdown=5)
                message('CANCELLED')
                self.buttons_active(True)

        def done(self):
            self.stop()
            self.quit()

        def message(self, s):
            self.stop()
            self.text.insert(tk.END, 79 * '*' + '\n')
            self.text.insert(tk.END, '\n')
            self.text.insert(tk.END, '    ' + s + '\n')
            self.text.insert(tk.END, '\n')
            self.text.insert(tk.END, 79 * '*' + '\n')
            self.text.insert(tk.END, '\n')
            self.text.see(tk.END)

        def line(self, s):
            self.text.insert(tk.END, s + '\n')
            self.text.see(tk.END)

        def createWidgets(self):
            self.job_label_font = tkFont.Font(weight='bold')
            job = os.path.relpath(os.curdir, U.cgenie_jobs)
            self.job_label = ttk.Label(self, text='Job: ' + job,
                                       font=self.job_label_font)
            self.button_frame = ttk.Frame(self)
            self.content_frame = ttk.Frame(self)
            self.action_buttons = [ ]
            action_button_defs = [['Run',     self.run],
                                  ['Build',   self.build],
                                  ['Clean',   self.clean],
                                  ['Cleaner', self.cleaner]]
            for r in range(len(action_button_defs)):
                lab, act = action_button_defs[r]
                self.action_buttons.append(ttk.Button(self.button_frame,
                                                      text=lab, command=act))
            self.quit_button = ttk.Button(self.button_frame,
                                          text='Quit', command=self.done)
            self.cancel_button = ttk.Button(self.button_frame, text='Cancel',
                                            command=self.stop)
            self.text = tk.Text(self.content_frame)
            self.text_scroll = ttk.Scrollbar(self.content_frame,
                                             command=self.text.yview)
            self.text['yscrollcommand'] = self.text_scroll.set

            self.winfo_toplevel().rowconfigure(0, weight=1)
            self.winfo_toplevel().columnconfigure(0, weight=1)
            self.grid(sticky=tk.N+tk.E+tk.S+tk.W)
            self.columnconfigure(0, weight=0)
            self.columnconfigure(1, weight=1)
            self.rowconfigure(0, weight=0)
            self.rowconfigure(1, weight=1)
            self.job_label.grid(column=0, row=0, columnspan=2, sticky=tk.W,
                                padx=10, pady=5)
            self.button_frame.grid(column=0, row=1, sticky=tk.N+tk.W, pady=5)
            self.content_frame.rowconfigure(0, weight=1)
            self.content_frame.columnconfigure(0, weight=1)
            self.content_frame.grid(column=1, row=1,
                                    sticky=tk.N+tk.E+tk.S+tk.W, padx=5)
            self.text.grid(column=0, row=0, sticky=tk.N+tk.E+tk.S+tk.W, pady=5)
            self.text.rowconfigure(0, weight=1)
            self.text.columnconfigure(0, weight=1)
            self.text_scroll.grid(column=1, row=0, sticky=tk.N+tk.S, pady=5)
            for r in range(len(action_button_defs)):
                self.action_buttons[r].grid(column=0, row=r,
                                            sticky=tk.E+tk.W, padx=5)
            self.quit_button.grid(column=0, row=len(action_button_defs),
                                  sticky=tk.E+tk.W, padx=5)
            self.cancel_button.grid(column=0, row=len(action_button_defs)+1,
                                    sticky=tk.E+tk.W+tk.S, padx=5, pady=10)

        def buttons_active(self, state):
            s = ['disabled'] if not state else ['!disabled']
            for b in self.action_buttons: b.state(s)

        def __init__(self, root=None):
            self.root = root
            self.proc = None
            tk.Frame.__init__(self, root)
            self.createWidgets()

        # Manage subsidiary process.
        def manage(self, cmd, logfp, cont, *rest):
            self.cont = cont
            self.contrest = rest
            self.proc = sp.Popen(cmd, stdout=sp.PIPE, stderr=sp.STDOUT)
            q = qu.Queue()
            t = thr.Thread(target=self.reader_thread, args=[q, logfp]).start()
            self.update(q)

        # Background thread: reads from subsidiary process.
        def reader_thread(self, q, logfp):
            for line in iter(self.proc.stdout.readline, ''):
                q.put(line)
                logfp.write(line)
            q.put(None)

        # Runs in GUI thread...
        def update(self, q):
            for line in iter_except(q.get_nowait, qu.Empty):
                if line is None:
                    status = self.proc.wait() if self.proc else -1
                    self.proc = None
                    if self.cont:
                        cont = self.cont
                        self.cont = None
                        contrest = self.contrest if self.contrest else None
                        self.contrest = None
                        if contrest:
                            cont(status, *contrest)
                        else:
                            cont(status)
                    return
                else:
                    self.text.insert(tk.END, line)
                    self.text.see(tk.END)
            self.root.after(40, self.update, q)


# This script can run as a GUI or command line tool.

gui = True if len(sys.argv) == 1 else False
if gui and not gui_available:
    sys.exit('GUI operation is not available!')


# GENIE configuration.

if not U.read_cgenie_config():
    if gui:
        tk.Tk().withdraw()
        tkMessageBox.showerror('GENIE not set up',
                               'GENIE not set up: run the setup-cgenie script!')
        sys.exit()
    else:
        sys.exit('GENIE not set up: run the setup-cgenie script!')
scons = os.path.join(U.cgenie_root, 'tools', 'scons', 'scons.py')

def console_message(s):
    print(79 * '*')
    print('')
    print('    ' + s)
    print('')
    print(79 * '*')
    print('')

def console_line(s):
    print(s)

def console_manage(cmd, logfp, cont, *rest):
    runner = sp.Popen(cmd, stdout=sp.PIPE, stderr=sp.STDOUT)
    while True:
        line = runner.stdout.readline()
        if not line: break
        logfp.write(line)
        print(line, end='')
    cont(runner.wait(), *rest)


# Command line arguments.

def usage():
    print("""
Usage: go <command>

Commands:
  clean                                 Clean results
  cleaner                               Clean results and model build
  build [<build-type>] [--no-progress]  Build model
  run [<build-type>] [--no-progress]    Build and run model
  set-platform <platform>               Set explicit build platform
  clear-platform                        Clear explicit build platform
""")
    sys.exit()

build_type = 'ship'
progress = True
if not gui:
    if len(sys.argv) < 2: usage()
    action = sys.argv[1]
    if action in ['clean', 'cleaner', 'clear-platform']:
        if len(sys.argv) != 2: usage()
    elif action == 'set-platform':
        if len(sys.argv) != 3: usage()
        platform = sys.argv[2]
    elif action in ['build', 'run']:
        if len(sys.argv) == 3:
            if sys.argv[2] == '--no-progress': progress = False
            else:                              build_type = sys.argv[2]
        elif len(sys.argv) == 4:
            build_type = sys.argv[2]
            if sys.argv[3] == '--no-progress': progress = False
            else:                              usage()
        elif len(sys.argv) != 2: usage()
        if build_type and build_type not in U.build_types:
            sys.exit('Unrecognised build type: "', build_type, '"')
    else: usage()


# Model configuration for job.

model_config = U.ModelConfig(build_type)
model_dir = model_config.directory()
exe_name = 'genie-' + build_type + '.exe' if build_type else 'genie.exe'


# Clean up output directories for this job and (optionally) build
# directories for model setup for this job.

def clean(clean_model):
    message('CLEANING MODEL RESULTS' +
            (' AND BUILD' if clean_model else '') + '...')
    if clean_model:
        model_config.clean()
        for exe in glob.iglob('genie-*.exe'): os.remove(exe)
        if os.path.exists('build.log'): os.remove('build.log')
    if os.path.exists('run.log'): os.remove('run.log')
    for d, ds, fs in os.walk('output'):
        for f in fs: os.remove(os.path.join(d, f))


# Build model.

def build(cont):
    model_config.setup()
    model_dir = model_config.directory()
    with open(os.devnull, 'w') as sink:
        cmd = [scons, '-q', '-C', model_dir]
        if plat.system() == 'Windows': cmd = ['python'] + cmd
        need_build = sp.call(cmd, stdout=sink, stderr=sink)
    if not need_build:
        message('Build is up to date')
        shutil.copy(os.path.join(model_dir, 'genie.exe'),
                    os.path.join(os.curdir, exe_name))
        if cont: cont()
        else: return
    message('BUILDING: ' + model_config.display_model_version)
    logfp = open(os.path.join(model_dir, 'build.log'), 'w')
    rev = 'rev=' + model_config.display_model_version
    cmd = [scons, '-C', model_dir, rev]
    if plat.system() == 'Windows': cmd = ['python'] + cmd
    cmd.append('progress=' + ('1' if progress else '0'))
    manage(cmd, logfp, build2, cont)

def build2(result, cont):
    shutil.copy(os.path.join(model_dir, 'build.log'), os.curdir)
    if result == 0:
        line('')
        message('Build OK')
        shutil.copy(os.path.join(model_dir, 'genie.exe'),
                    os.path.join(os.curdir, exe_name))
        if cont: cont()
    else:
        message('BUILD FAILED: see build.log for details')


# Run model.

def run(cont=None):
    message('RUNNING: ' + model_config.display_model_version)
    logfp = open('run.log', 'w')
    manage(os.path.join('.', exe_name), logfp, run2, cont)

def run2(result, cont):
    if result == 0:
        message('Run OK!')
    else:
        message('RUN FAILED: see run.log for details')
    if cont: cont()


# Actions: platform management, clean, build or run.

pfile = os.path.join('config', 'platform-name')
if gui:
    root = tk.Tk()
    app = Application(root)
    message = app.message
    line = app.line
    manage = app.manage
    root.mainloop()
else:
    message = console_message
    line = console_line
    manage = console_manage
    if   action == 'clear-platform':
        if os.path.exists(pfile): os.remove(pfile)
    elif action == 'set-platform':
        with open(pfile, 'w') as ofp: print(platform, file=ofp)
    elif action == 'clean':
        clean(False)
    elif action == 'cleaner':
        clean(True)
    elif action == 'build':
        build(None)
    elif action == 'run':
        build(run)
    else:
        usage()
