#!/usr/bin/env python2

from __future__ import print_function
import os, os.path, sys, shutil
import subprocess as sp
import argparse


# Command line arguments.

parser = argparse.ArgumentParser(description='Build and run GENIE jobs')
parser.add_argument('action', help='Command action (required)',
                    choices=['clean', 'build', 'run'])
parser.add_argument('-q', '--quiet', action='store_true',
                    help='Suppress output')
args = parser.parse_args()
action = args.action
quiet = args.quiet

def message(s):
    print(79 * '*')
    print('')
    print('    ' + s)
    print('')
    print(79 * '*')
    print('')


# Clean up build and output directories.

def clean():
    message('CLEANING...')
    if os.path.exists('build'): shutil.rmtree('build')
    if os.path.exists('build.log'): os.remove('build.log')
    if os.path.exists('run.log'): os.remove('run.log')
    for d, ds, fs in os.walk('output'):
        for f in fs: os.remove(os.path.join(d, f))


# Build model.

def build():
    with open(os.devnull, 'w') as sink:
        need_build = sp.call(['scons', '-q'], stdout=sink, stderr=sink)
    if not need_build:
        message('Build is up to date')
        return True
    message('BUILDING...')
    with open('build.log', 'w') as logfp:
        result = sp.call('scons', stdout=logfp, stderr=sp.STDOUT)
        if result == 0:
            message('Build OK')
            return True
        else:
            message('BUILD FAILED: see build.log for details')
            return False


# Build model.

def run():
    message('RUNNING...')
    with open('run.log', 'w') as logfp:
        genie = sp.Popen(os.path.join('.', 'genie.exe'),
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


# Actions: clean, build or run.

if   action == 'clean':
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
