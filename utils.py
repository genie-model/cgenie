#!/usr/bin/env python2

from __future__ import print_function
import json, errno, os, sys, platform


# Read cGENIE configuration.

genie_cfgfile = os.path.expanduser("~/.cgenierc")

def read_cgenie_config():
    global cgenie_root, cgenie_data, cgenie_jobs, cgenie_version
    try:
        with open(genie_cfgfile) as fp:
            config = json.load(fp)
            cgenie_root = config['cgenie_root']
            cgenie_data = config['cgenie_data']
            cgenie_jobs = config['cgenie_jobs']
            cgenie_version = config['cgenie_version']
            return True
    except IOError as e:
        if e.errno == errno.ENOENT: return False
        raise
    else: return False



# Discover build platform.

def discover_platform():
    def exists(p):
        return os.path.exists(os.path.join(cgenie_root, 'platforms', p))

    def discover():
        host = platform.node()
        os = platform.system().upper()
        if exists(host): return host
        if exists(os): return os
        sys.exit('Cannot find suitable build platform!')

    pfile = os.path.join('config', 'platform-name')
    if os.path.exists(pfile):
        with open(pfile) as fp:
            p = fp.readline().strip()
    else:
        p = discover()
    if exists(p): return p
    else: sys.exit('Build platform "' + p + '" not known!')
