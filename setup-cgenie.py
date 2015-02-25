#!/usr/bin/env python2

import os
import json
import utils

def ask(prompt, default):
    return raw_input(prompt + " [" + default + "]: ") or default

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
