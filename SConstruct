from __future__ import print_function
import os, os.path, sys, glob
import platform as P

# Set up model source and tools directory (the "srcdir='src'" thing is
# there to allow us to run SCons in the root cgenie directory for
# development purposes -- normal job builds will pick up an explicit
# job configuration and model source directory file in their build
# directory).

if os.path.exists('version.py'):
    execfile('version.py')
    sys.path.append(scriptdir)
else:
    srcdir = 'src'
    build_type = 'normal'
    sys.path.append('tools')

import utils as U


# Read GENIE configuration.

if not U.read_cgenie_config():
    sys.exit("GENIE not set up: run the setup.py script!")


# Load platform configuration.

platform = U.discover_platform()
print('Using platform "' + platform + '"')
execfile(os.path.join(U.cgenie_root, 'platforms', platform))


# Set up model job configuration (the "dummy-job.py" thing is there to
# allow us to run SCons in the root cgenie directory for development
# purposes -- normal job builds will pick up an explicit job
# configuration and model source directory file in their build
# directory).

if os.path.exists('job.py'):
    execfile('job.py')
else:
    execfile(os.path.join(U.cgenie_root, 'src', 'dummy-job.py'))


# NetCDF paths.

netcdfinc = os.path.join(netcdf['base'], 'include')
if not os.path.exists(netcdfinc): netcdfinc = netcdf['base']
netcdflib = os.path.join(netcdf['base'], 'lib')
if not os.path.exists(netcdflib): netcdflib = netcdf['base']


# GENIE modules to build.

modules = Split("""atchem
                   biogem
                   embm
                   ents
                   gemlite
                   goldstein
                   goldsteinseaice
                   rokgem
                   sedgem""")
utils = Split('common utils wrappers')


# Subdirectories for compilation.

subdirs = modules + utils


# F90 module search paths.

modpath = map(lambda d: os.path.join('#/build', d), subdirs)


# Set up coordinate definitions.

defs = [ ]
for d in coordvars:
    if coordvars[d]:
        defs.append(f90['define'] + d + '=' + str(coordvars[d]))


# Set up model version marker.

rev = ARGUMENTS['rev'] if 'rev' in ARGUMENTS else 'UNKNOWN'
defs.append(f90['define'] + "REV=" + rev)


# Set up SCons environment: Fortran compiler definitions taken from
# platform configuration.  Some of these things are Windows-specific:
# the Microsoft linker uses the directory given in the TMP environment
# variable for temporary files, and if that's not defined, tries to
# put them in a read-only system directory; SCons uses the HOST_ARCH
# environment setting to decide whether to use 32- or 64-bit tools
# from Visual Studio.

envcopy = { }
envcopy['PATH'] = os.environ['PATH']
if 'TMP' in os.environ: envcopy['TMP'] = os.environ['TMP']
baselinkflags = []
if 'baselinkflags' in f90:
    baselinkflags = f90['baselinkflags']
extraf90flags = []
if build_type in f90: extraf90flags = f90[build_type]
extralinkflags = []
if build_type + '_link' in f90: extralinkflags = f90[build_type + '_link']
extraf90libpaths = []
if 'libpath' in f90: extraf90libpaths = f90['libpath']

target_vs_arch = 'linux'
if 'TARGET_VS_ARCH' in os.environ:
    target_vs_arch = os.environ['TARGET_VS_ARCH']

env = Environment(ENV = envcopy,
                  TOOLS = ['default', f90['compiler']],
                  HOST_ARCH = target_vs_arch,
                  F90FLAGS = f90['baseflags'] + extraf90flags + defs,
                  LINKFLAGS = baselinkflags + extralinkflags,
                  F90PATH = [netcdfinc] + modpath,
                  FORTRANMODDIRPREFIX = f90['module_dir'],
                  FORTRANMODDIR = '${TARGET.dir}',
                  LIBPATH = [netcdflib] + extraf90libpaths,
                  LIBS = netcdf['libs'])


# Set up prompt progress reporting.

if 'progress' not in ARGUMENTS or ARGUMENTS['progress'] == '1':
    if P.system() == 'Windows':
        screen = open('CON:', 'w')
    else:
        screen = open('/dev/tty', 'w')
    def progress_function(node):
        node = str(node)
        if node.endswith('.f90'):
            print(os.path.relpath(node, srcdir), file=screen)
    Progress(progress_function)


# Build!

Export('env', 'subdirs')
SConscript(os.path.join(srcdir, 'SConscript'),
           variant_dir='#build', duplicate=0)
Install('.', 'build/genie.exe')
