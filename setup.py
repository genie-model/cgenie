#!/usr/bin/env python2

import json
import errno
import os

cfg = os.path.expanduser("~/.cgenierc")

# Read cGENIE configuration.
def read_cgenie_config():
    try:
        with open(cfg) as fp:
            return json.load(fp)
    except IOError as e:
        if e.errno == errno.ENOENT:
            return None
        raise
    else:
        return None

def ask_dir(prompt, default):
    return raw_input(prompt + " [" + default + "]: ") or default

config = read_cgenie_config()
if config:
    print("Already set up...")
else:
    root = ask_dir("Root directory", os.path.expanduser("~/cgenie"))
    data = ask_dir("Data directory", os.path.expanduser("~/cgenie-data"))
    test = ask_dir("Test directory", os.path.expanduser("~/cgenie-test"))
    jobs = ask_dir("Jobs directory", os.path.expanduser("~/cgenie-jobs"))
    config = { 'cgenie_root': root, 'cgenie_data': data,
               'cgenie_test': test, 'cgenie_jobs': jobs }
    with open(cfg, 'w') as f:
        json.dump(config, f)
