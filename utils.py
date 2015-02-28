#!/usr/bin/env python2

from __future__ import print_function
import json, errno, os, sys, shutil, platform, string
import re, hashlib, glob


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


# Recognised build types.

build_types = ['normal', 'debug', 'ship', 'profile', 'bounds']


# Model configuration information: model configuration is based on
# model version, build platform, build type (debug, optimised, etc.)
# and a hash derived from the job configuration (basically ensuring
# that model builds for different coordinate definitions, tracer
# counts, etc. are correctly segregated).

class ModelConfig:
    # Assumes current working directory is the job directory.
    def __init__(self, build_type):
        if not build_type: build_type = 'normal'
        with open(os.path.join('config', 'model-version')) as fp:
            self.model_version = fp.readline().strip()
        self.platform = discover_platform()
        self.build_type = build_type
        with open(os.path.join('config', 'job.py')) as fp:
            cs = re.search('coordvars\s*=\s*{([^}]+)}', fp.read()).group(1)
            cs = filter(lambda c: not str.isspace(c), cs).split(',')
            cs = ','.join(sorted(cs))
            self.job_hash = hashlib.sha1(cs).hexdigest()

    # Determine the model directory for this configuration: these all
    # live under cgenie_jobs/MODELS.
    def directory(self):
        return os.path.join(cgenie_jobs, 'MODELS',
                            self.model_version, self.platform,
                            self.job_hash, self.build_type)

    # Clean out model builds for a given model configuration -- this
    # removes all build types for this model configuration, just to
    # avoid surprises.
    def clean(self):
        # Go up one level to catch all build type directories.
        hashd = os.path.abspath(os.path.join(self.directory(), os.pardir))
        for d in glob.iglob(os.path.join(hashd, '*')):
            shutil.rmtree(d)
        os.removedirs(hashd)

    # Set up repository clone for building model at explicitly
    # selected version tags.
    def setup_repo(self):
        if self.model_version != 'DEVELOPMENT':
            sys.exit("Not set up for using specific model versions yet!")

    # Set up model build directory.
    def setup(self):
        d = self.directory()
        if not os.path.exists(d): os.makedirs(d)
        vfile = os.path.join(d, 'version.py')
        if not os.path.exists(vfile):
            if self.model_version == 'DEVELOPMENT':
                scons_dir = cgenie_root
            else:
                sys.exit('NOT YET IMPLEMENTED!!!')
            scons_srcdir = os.path.join(scons_dir, 'src')
            with open(vfile, 'w') as fp:
                print('# Model source directory', file=fp)
                print("srcdir = '" + scons_srcdir + "'\n", file=fp)
                print('# Build type', file=fp)
                print("build_type = '" + self.build_type + "'\n", file=fp)
        jfile = os.path.join(d, 'job.py')
        if not os.path.exists(jfile):
            shutil.copy(os.path.join('config', 'job.py'), jfile)
        sfile = os.path.join(d, 'SConstruct')
        if not os.path.exists(sfile):
            shutil.copy(os.path.join(scons_dir, 'SConstruct'), sfile)
