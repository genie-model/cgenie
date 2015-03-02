#!/usr/bin/env python2

from __future__ import print_function
import errno, os, sys, shutil, platform, string
import re, hashlib, glob
import subprocess as sp


# Read cGENIE configuration.

genie_cfgfile = os.path.expanduser("~/.cgenierc")

def read_cgenie_config():
    global cgenie_root, cgenie_data, cgenie_jobs, cgenie_version
    try:
        with open(genie_cfgfile) as fp:
            for line in fp:
                k, v = line.strip().split(':')
                v = v.strip()
                if   k == 'cgenie_root':    cgenie_root = v
                elif k == 'cgenie_data':    cgenie_data = v
                elif k == 'cgenie_jobs':    cgenie_jobs = v
                elif k == 'cgenie_version': cgenie_version = v
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
        if os.path.exists(hashd): os.removedirs(hashd)

    # Set up model build directory.
    def setup(self):
        d = self.directory()
        if not os.path.exists(d): os.makedirs(d)
        vfile = os.path.join(d, 'version.py')
        if not os.path.exists(vfile):
            if self.model_version == 'DEVELOPMENT':
                scons_dir = cgenie_root
            else:
                scons_dir = os.path.join(cgenie_jobs, 'MODELS', 'REPOS',
                                         self.model_version)
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


# Determine list of available model versions.

def available_versions():
    return ['DEVELOPMENT'] + sp.check_output(['git', 'tag', '-l']).splitlines()


# Set up repository clone for building model at explicitly
# selected version tags.

def setup_version_repo(ver):
    if ver == 'DEVELOPMENT': return
    if ver not in available_versions():
        sys.exit('Invalid model version "' + ver + '"')
    dst = os.path.join(cgenie_jobs, 'MODELS', 'REPOS', ver)
    if os.path.exists(dst): return dst
    with open(os.devnull, 'w') as sink:
        if sp.call(['git', 'clone', '-l', '--single-branch', '--branch', ver,
                    os.curdir, dst], stdout=sink, stderr=sink) != 0:
            sys.exit('Failed to set up repository clone for version "' +
                     ver + '"')
    with open(os.path.join(dst, 'repo-version'), 'w') as ofp:
        print(ver, file=ofp)
    return dst
