#!/usr/bin/env python2

from __future__ import print_function
import os
import subprocess as sp
import json
import utils


# Data and test GitHub repositories.

datarepo = 'git@github.com:ian-ross/cgenie-data'
testrepo = 'git@github.com:ian-ross/cgenie-test'


# Input helpers.

def ask(prompt, default, options=None):
    while True:
        res = raw_input(prompt + ' [' + default + ']: ') or default
        if not options or res in options:
            return res
        else:
            print('Input must be one of:', ' '.join(options))

def yesno(prompt, default):
    opts = 'Yn' if default else 'yN'
    return raw_input(prompt + ' [' + opts + ']: ') or default


# Get options from user.

versions = utils.available_versions()
default_version = versions[-1]
config = utils.read_cgenie_config()
if config:
    print('Already set up...')
else:
    root = ask('Root directory', os.path.expanduser('~/cgenie'))
    base = os.path.abspath(os.path.join(root, os.pardir))
    data = ask('Data directory', os.path.join(base, 'cgenie-data'))
    test = ask('Test directory', os.path.join(base, 'cgenie-test'))
    jobs = ask('Jobs directory', os.path.join(base, 'cgenie-jobs'))
    vers = ask('Default version', default_version, versions)
    with open(utils.genie_cfgfile, 'w') as fp:
        print('cgenie_root: ' + root, file=fp)
        print('cgenie_data: ' + data, file=fp)
        print('cgenie_test: ' + test, file=fp)
        print('cgenie_jobs: ' + jobs, file=fp)
        print('cgenie_version: ' + vers, file=fp)


# Download data and test repositories if required.

download_data = False
download_test = False
if not os.path.exists(data):
    download_data = yesno('Data directory does not exist. Download?', True)
if not os.path.exists(test):
    download_test = yesno('Test directory does not exist. Download?', True)

if download_data:
    print('Downloading cgenie-data repository...')
    if sp.call(['git', 'clone', datarepo, data]) != 0:
        print('FAILED TO CLONE cgenie-data REPOSITORY!')
if download_test:
    print('Downloading cgenie-test repository...')
    if sp.call(['git', 'clone', testrepo, test]) != 0:
        print('FAILED TO CLONE cgenie-test REPOSITORY!')
