import os, os.path

# Load platform and job configuration.
execfile(os.path.join('config', 'platform.py'))
execfile(os.path.join('config', 'job.py'))

# NetCDF paths.
netcdfinc = os.path.join(netcdf, 'include')
netcdflib = os.path.join(netcdf, 'lib')

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
        coorddefs.append(f90define + d + '=' + str(coordvars[d]))

# Set up SCons environment: Fortran compiler definitions take from
# platform configuration.
env = Environment(FORTRAN = f90,
                  LINK = f90,
                  F90FLAGS=f90flags + coorddefs,
                  F90PATH = [netcdfinc] + modpath,
                  FORTRANMODDIRPREFIX = f90moddir,
                  FORTRANMODDIR = '${TARGET.dir}',
                  LIBPATH = [netcdflib],
                  LIBS = netcdflibs)

# Build!
Export('env', 'subdirs')
SConscript(os.path.join(srcdir, 'SConscript'),
           variant_dir='#build', duplicate=0)
Install('.', 'build/genie.exe')
