
import os, os.path, sys, shutil, argparse, glob
import subprocess as sp
import platform as plat
try:
    import queue as qu
    import threading as thr
    import tkinter as tk
    from tkinter import font as tkFont
    from tkinter import messagebox as tkMessageBox
    from tkinter import ttk
    gui_available = True
except ImportError:
    gui_available = False

if gui_available:
    print("gui")
else:
    print("no-gui")

