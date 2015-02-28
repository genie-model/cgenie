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
        res = raw_input(prompt + " [" + default + "]: ") or default
        if not options or res in options:
            return res
        else:
            print('Input must be one of:', ' '.join(options))

def yesno(prompt, default):
    opts = 'Yn' if default else 'yN'
    return raw_input(prompt + " [" + opts + "]: ") or default


# Determine list of available model versions.

versions = ['DEVELOPMENT'] + sp.check_output(['git', 'tag', '-l']).splitlines()
default_version = versions[-1]


# Get options from user.

config = utils.read_cgenie_config()
if config:
    print("Already set up...")
else:
    root = ask("Root directory", os.path.expanduser("~/cgenie"))
    data = ask("Data directory", os.path.expanduser("~/cgenie-data"))
    test = ask("Test directory", os.path.expanduser("~/cgenie-test"))
    jobs = ask("Jobs directory", os.path.expanduser("~/cgenie-jobs"))
    vers = ask("Default version", default_version, versions)
    config = { 'cgenie_root': root, 'cgenie_data': data,
               'cgenie_test': test, 'cgenie_jobs': jobs,
               'cgenie_version': vers }
    with open(utils.genie_cfgfile, 'w') as f:
        json.dump(config, f)


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
