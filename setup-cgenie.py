#!/usr/bin/env python2

import os
import subprocess as sp
import json
import utils

datarepo = 'git@github.com:ian-ross/cgenie-data'
testrepo = 'git@github.com:ian-ross/cgenie-test'

def ask(prompt, default):
    return raw_input(prompt + " [" + default + "]: ") or default

def yesno(prompt, default):
    opts = 'Yn' if default else 'yN'
    return raw_input(prompt + " [" + opts + "]: ") or default

config = utils.read_cgenie_config()
if config:
    print("Already set up...")
else:
    root = ask("Root directory", os.path.expanduser("~/cgenie"))
    data = ask("Data directory", os.path.expanduser("~/cgenie-data"))
    test = ask("Test directory", os.path.expanduser("~/cgenie-test"))
    jobs = ask("Jobs directory", os.path.expanduser("~/cgenie-jobs"))
    vers = ask("Default version", "DEVELOPMENT")
    config = { 'cgenie_root': root, 'cgenie_data': data,
               'cgenie_test': test, 'cgenie_jobs': jobs,
               'cgenie_version': vers }
    with open(utils.genie_cfgfile, 'w') as f:
        json.dump(config, f)

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
