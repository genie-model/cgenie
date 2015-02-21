import os, os.path

#----------------------------------------------------------------------

srcdir = 'src'

netcdf = '/usr/local/netcdf-cxx-4.1.3/'
netcdflibs = ['netcdf', 'netcdff']

f90 = 'gfortran'
f90flags = ['-O2', '-O3', '-funroll-loops', '-msse', '-fno-automatic',
            '-x', 'f95-cpp-input', '-ffree-line-length-none',
            '-fdefault-real-8', '-fimplicit-none']
f90moddir = '-J'

nlons = 36
nlats = 36
nlevs = 16
ntracers = 14

#----------------------------------------------------------------------

netcdfinc = os.path.join(netcdf, 'include')
netcdflib = os.path.join(netcdf, 'lib')

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

subdirs = modules + utils
modpath = map(lambda d: os.path.join('#/build', d), subdirs)

coordvars = { 'GENIENX':          nlons,
              'GENIENY':          nlats,
              'GOLDSTEINNLONS':   nlons,
              'GOLDSTEINNLATS':   nlats,
              'GOLDSTEINNLEVS':   nlevs,
              'GOLDSTEINNTRACS' : ntracers }
coorddefs = [ ]
for d in coordvars:
    coorddefs.append('-D' + d + '=' + str(coordvars[d]))

env = Environment(FORTRAN = f90,
                  LINK = f90,
                  F90FLAGS=f90flags + coorddefs,
                  F90PATH = [netcdfinc] + modpath,
                  FORTRANMODDIRPREFIX = f90moddir,
                  FORTRANMODDIR = '${TARGET.dir}',
                  LIBPATH = [netcdflib],
                  LIBS = netcdflibs)

Export('env', 'subdirs')
SConscript(os.path.join(srcdir, 'SConscript'),
           variant_dir='#build', duplicate=0)
Install('.', 'build/genie.exe')
