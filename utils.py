#!/usr/bin/env python2

from __future__ import print_function
import json, csv
import errno, os, sys
import re


# Read cGENIE configuration.

genie_cfgfile = os.path.expanduser("~/.cgenierc")

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


# Module information: mappings between module names, flag names and
# namelist names.

srcdir = None

def set_src_dir(d):
    global srcdir
    srcdir = d

module_info = { }
flagname_to_mod = { }

def load_module_info():
    try:
        with open(os.path.join(srcdir, 'module-info.csv')) as fp:
            for row in csv.reader(fp, skipinitialspace=True):
                if row[0] == '#': next
                flag = ('ma_flag_' + row[1]) if row[1] != 'NONE' else row[1]
                module_info[row[0]] = { 'flag_name': flag, 'prefix': row[2],
                                        'nml_file': row[3], 'nml_name': row[4] }
                flagname_to_mod[flag] = row[0]
        for k, v in module_info.iteritems():
            excs = { }
            try:
                with open(os.path.join(srcdir, k, k + '-exceptions.csv')) as fp:
                    for row in csv.reader(fp): excs[row[0]] = row[1]
            except IOError as e:
                if e.errno != errno.ENOENT: raise
            module_info[k]['exceptions'] = excs
    except:
        if not srcdir:
            sys.exit("Internal error: source directory not set!")
        else:
            sys.exit("Couldn't open module info file " +
                     os.path.join(srcdir, 'module-info.csv'))

def module_from_flagname(flagname):
    if not flagname_to_mod: load_module_info()
    return flagname_to_mod[flagname]

def lookup_module(modname):
    if not module_info: load_module_info()
    return module_info[modname]



class Namelist:
    """Fortran namelists"""
    def __init__(self, fp):
        self.entries = { }
        self.name = ''
        mode = 'start'
        for line in fp:
            line = line.strip()
            if mode == 'start':
                if line.startswith('&'):
                    self.name = line[1:].strip()
                    mode = 'main'
            else:
                if line.startswith('&'): mode = 'done'
                else:
                    if line.endswith(','): line = line[:-1]
                    kv = line.split('=')
                    self.entries[kv[0]] = kv[1].strip('"\'')

    def formatValue(self, v):
        if v == '.true.' or v == '.TRUE.': return '.TRUE.'
        if v == '.false.' or v == '.FALSE.': return '.FALSE.'
        if re.match('[+-]?(\d+(\.\d*)?|\.\d+)([eE][+-]?\d+)?', v): return v
        return '"' + v + '"'

    def write(self, fp):
        print('&' + self.name, file=fp)
        for k, v in self.entries.iteritems():
            print(k + '=' + self.formatValue(v) + ',', file=fp)
        print('&END', file=fp)

    def merge(self, prefix, excs, *maps):
        """Merge configuration data into default namelist.  Deals with
           stripping model-dependent prefix, exceptions to common naming
           conventions and parameter arrays."""
        plen = len(prefix) + 1
        for m in maps:
            for k in m.keys():
                rk = k[plen:]
                if (k in excs):
                    rk = excs[k]
                else:
                    s = re.search('_(\d+)$', rk)
                    if s:
                        rk = rk.rstrip('_0123456789') + '(' + s.group(1) + ')'
                if (rk in self.entries): self.entries[rk] = m[k]
