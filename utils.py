#!/usr/bin/env python2

import json, csv
import errno, os, sys
import re

genie_cfgfile = os.path.expanduser("~/.cgenierc")

# Read cGENIE configuration.
def read_cgenie_config():
    try:
        with open(genie_cfgfile) as fp: return json.load(fp)
    except IOError as e:
        if e.errno == errno.ENOENT: return None
        raise
    else: return None


# Read and parse a GENIE configuration file.
def read_config(f, msg):
    try:
        res = { }
        with (open(f)) as fp:
            for line in fp:
                m = re.search('([a-zA-Z0-9_]+)=(.*)', line)
                if m:
                    k = m.group(1)
                    v = m.group(2)
                    if (v[0] == '"' and v[-1] == '"' or
                        v[0] == "'" and v[-1] == "'"):
                        v = v[1:-1]
                    res[k] = v
            return res
    except IOError as e:
        if e.errno == errno.ENOENT: sys.exit(msg + ' not found: ' + f)
        else: raise

# Merge module flags from base and user configurations.
def merge_flags(*dicts):
    res = { }
    for d in dicts:
        for k in d.keys():
            if d[k].lower() == '.true.': v = 1
            else: v = 0
            res[k] = v
    return res

flagname_to_mod = { }

def load_module_info():
    with open('module-info.csv') as fp:
        for row in csv.reader(fp, skipinitialspace=True):
            if row[0] == '#': next
            flagname_to_mod['ma_flag_' + row[1]] = row[0]

def module_from_flagname(flagname):
    if not flagname_to_mod: load_module_info()
    return flagname_to_mod[flagname]
