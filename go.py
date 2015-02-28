#!/usr/bin/env python2

from __future__ import print_function
import os, os.path, sys, shutil, argparse
import subprocess as sp

import utils as U


# GENIE configuration.

if not U.read_cgenie_config():
    sys.exit("GENIE not set up: run the setup.py script!")


# Command line arguments.

def usage():
    print("""
Usage: go.py <command>

Commands:
  clean                     Clean results and model build
  build                     Build model
  run                       Build and run model
  set-platform <platform>   Set explicit build platform
  clear-platform            Clear explicit build platform
""")
    sys.exit()

if len(sys.argv) == 2:
    if sys.argv[1] in ['clean', 'build', 'run', 'clear-platform']:
        action = sys.argv[1]
    else: usage()
elif len(sys.argv) == 3:
    if sys.argv[1] == 'set-platform':
        action = sys.argv[1]
        platform = sys.argv[2]
    else: usage()
else: usage()


def message(s):
    print(79 * '*')
    print('')
    print('    ' + s)
    print('')
    print(79 * '*')
    print('')


# Model configuration for job.

model_config = U.ModelConfig()
model_dir = model_config.directory()


# Clean up output directories for this job and build directories for
# model setup for this job.

def clean():
    message('CLEANING...')
    model_config.clean()
    if os.path.exists('run.log'): os.remove('run.log')
    for d, ds, fs in os.walk('output'):
        for f in fs: os.remove(os.path.join(d, f))


# Build model.

def build():
    if model_config.model_version != 'DEVELOPMENT': model_config.setup_repo()
    model_config.setup()
    model_dir = model_config.directory()
    with open(os.devnull, 'w') as sink:
        need_build = sp.call(['scons', '-q', '-C', model_dir],
                             stdout=sink, stderr=sink)
    if not need_build:
        message('Build is up to date')
        return True
    message('BUILDING...')
    with open('build.log', 'w') as logfp:
        result = sp.call(['scons', '-C', model_dir],
                         stdout=logfp, stderr=sp.STDOUT)
        if result == 0:
            message('Build OK')
            return True
        else:
            message('BUILD FAILED: see build.log for details')
            return False


# Build model.

def run():
    message('RUNNING...')
    model_dir = model_config.directory()
    with open('run.log', 'w') as logfp:
        genie = sp.Popen(os.path.join(model_dir, 'genie.exe'),
                         stdout=sp.PIPE, stderr=sp.STDOUT)
        while True:
            line = genie.stdout.readline()
            if not line: break
            logfp.write(line)
            print(line, end='')
        result = genie.wait()
        if result == 0:
            message('Run OK!')
        else:
            message('RUN FAILED: see run.log for details')


# Actions: platform management, clean, build or run.

pfile = os.path.join('config', 'platform-name')
if   action == 'clear-platform':
    if os.path.exists(pfile): os.remove(pfile)
elif action == 'set-platform':
    with open(pfile, 'w') as ofp: print(platform, file=ofp)
elif action == 'clean':
    clean()
elif action == 'build':
    build()
elif action == 'run':
    if build():
        run()
    else:
        message('RUN CANCELLED')
else:
    sys.exit("Action must be one of clean, build or run")
