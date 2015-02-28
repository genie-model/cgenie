import os, os.path, sys
import utils as U

if not U.read_cgenie_config():
    sys.exit("GENIE not set up: run the setup.py script!")

# Load platform configuration.
platform = U.discover_platform()
print('Using platform "' + platform + '"')
execfile(os.path.join(U.cgenie_root, 'platforms', platform))

# Load job configuration.
execfile(os.path.join('config', 'job.py'))

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
env = Environment(FORTRAN = f90['compiler'],
                  LINK = f90['compiler'],
                  F90FLAGS=f90['baseflags'] + coorddefs,
                  F90PATH = [netcdfinc] + modpath,
                  FORTRANMODDIRPREFIX = f90['module_dir'],
                  FORTRANMODDIR = '${TARGET.dir}',
                  LIBPATH = [netcdflib],
                  LIBS = netcdf['libs'])

# Build!
Export('env', 'subdirs')
SConscript(os.path.join(srcdir, 'SConscript'),
           variant_dir='#build', duplicate=0)
Install('.', 'build/genie.exe')
