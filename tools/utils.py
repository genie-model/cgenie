from __future__ import print_function
import errno, os, sys, shutil, platform, string
import re, hashlib, glob
import subprocess as sp


# Read cGENIE configuration.

genie_cfgfile = os.path.expanduser(os.path.join('~', '.cgenierc'))

def read_cgenie_config():
    global cgenie_root, cgenie_data, cgenie_test, cgenie_jobs, cgenie_version
    try:
        with open(genie_cfgfile) as fp:
            for line in fp:
                fs = line.strip().split(':')
                k = fs[0]
                v = ':'.join(fs[1:]).strip()
                if   k == 'cgenie_root':    cgenie_root = v
                elif k == 'cgenie_data':    cgenie_data = v
                elif k == 'cgenie_test':    cgenie_test = v
                elif k == 'cgenie_jobs':    cgenie_jobs = v
                elif k == 'cgenie_version': cgenie_version = v
            return True
    except IOError as e:
        if e.errno == errno.ENOENT: return False
        raise
    else: return False


# 
# could be: subprocess.Popen(cmd) instead

def fixe(file):
    cmd = [os.path.join(cgenie_root, 'tools', 'fix-exceptions.py'), file]
    #os.system(cmd)
    sp.Popen(cmd)


# Discover build platform.

def discover_platform():
    def exists(p):
        return os.path.exists(os.path.join(cgenie_root, 'platforms', p))

    def discover():
        host = platform.node()
        os = platform.system().upper()
        plat = platform.platform().split('-')[0].upper()
        if exists(host): return host
        if exists(os): return os
        if exists(plat): return plat
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

build_types = ['normal', 'debug', 'ship', 'profile', 'bounds', 'coverage']


# Model configuration information: model configuration is based on
# model version, build platform, build type (debug, optimised, etc.)
# and a hash derived from the job configuration (basically ensuring
# that model builds for different coordinate definitions, tracer
# counts, etc. are correctly segregated).

class ModelConfig:
    # Assumes current working directory is the job directory.
    def __init__(self, build_type, dir=None):
        if not build_type: build_type = 'normal'
        vfile = os.path.join('config', 'model-version')
        if dir: vfile = os.path.join(dir, vfile)
        with open(vfile) as fp:
            self.model_version = fp.readline().strip()
            self.display_model_version = self.model_version
            if self.model_version.startswith('DEVELOPMENT:'):
                self.display_model_version = self.model_version.split(':')[1]
                self.model_version = 'DEVELOPMENT'
        self.platform = discover_platform()
        self.build_type = build_type

    # Determine the model directory for this configuration: these all
    # live under cgenie_jobs/MODELS.
    def directory(self):
        return os.path.join(cgenie_jobs, 'MODELS',
                            self.model_version, self.platform, self.build_type)

    # Clean out model builds for a given model configuration -- this
    # removes all build types for this model configuration, just to
    # avoid surprises.
    def clean(self):
        # Go up one level to catch all build type directories.
        platd = os.path.abspath(os.path.join(self.directory(), os.pardir))
        for d in glob.iglob(os.path.join(platd, '*')):
            shutil.rmtree(d)
        if os.path.exists(platd): os.removedirs(platd)

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
            scons_srcdir = scons_srcdir.replace('\\', '\\\\')
            scriptdir = os.path.join(cgenie_root, 'tools')
            scriptdir = scriptdir.replace('\\', '\\\\')
            with open(vfile, 'w') as fp:
                print('# Model source directory', file=fp)
                print("srcdir = '" + scons_srcdir + "'\n", file=fp)
                print('# Model script directory', file=fp)
                print("scriptdir = '" + scriptdir + "'\n", file=fp)
                print('# Build type', file=fp)
                print("build_type = '" + self.build_type + "'\n", file=fp)
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
