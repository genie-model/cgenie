#!/usr/bin/env python2

import os
import json
import utils

def ask_dir(prompt, default):
    return raw_input(prompt + " [" + default + "]: ") or default

config = utils.read_cgenie_config()
if config:
    print("Already set up...")
else:
    root = ask_dir("Root directory", os.path.expanduser("~/cgenie"))
    data = ask_dir("Data directory", os.path.expanduser("~/cgenie-data"))
    test = ask_dir("Test directory", os.path.expanduser("~/cgenie-test"))
    jobs = ask_dir("Jobs directory", os.path.expanduser("~/cgenie-jobs"))
    config = { 'cgenie_root': root, 'cgenie_data': data,
               'cgenie_test': test, 'cgenie_jobs': jobs }
    with open(utils.genie_cfgfile, 'w') as f:
        json.dump(config, f)
