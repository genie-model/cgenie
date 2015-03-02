from __future__ import print_function
import os, os.path, sys
import platform as P

# Set up model source and scripts directory (the "srcdir='src'" thing
# is there to allow us to run SCons in the root cgenie directory for
# development purposes -- normal job builds will pick up an explicit
# job configuration and model source directory file in their build
# directory).

if os.path.exists('version.py'):
    execfile('version.py')
    sys.path.append(scriptdir)
else:
    srcdir = 'src'
    build_type = 'normal'
    sys.path.append('scripts')

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
netcdflib = os.path.join(netcdf['base'], 'lib')


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

coorddefs = [ ]
for d in coordvars:
    if coordvars[d]:
        coorddefs.append(f90['define'] + d + '=' + str(coordvars[d]))


# Set up SCons environment: Fortran compiler definitions take from
# platform configuration.

extraf90flags = []
extralinkflags = []
if build_type in f90: extraf90flags = f90[build_type]
if build_type + '_link' in f90: extralinkflags = f90[build_type + '_link']

env = Environment(FORTRAN = f90['compiler'], F90 = f90['compiler'],
                  LINK = f90['compiler'],
                  F90FLAGS = f90['baseflags'] + extraf90flags + coorddefs,
                  LINKFLAGS = extralinkflags,
                  F90PATH = [netcdfinc] + modpath,
                  FORTRANMODDIRPREFIX = f90['module_dir'],
                  FORTRANMODDIR = '${TARGET.dir}',
                  LIBPATH = [netcdflib],
                  LIBS = netcdf['libs'])


# Set up prompt progress reporting.

if P.system == 'Windows':
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
