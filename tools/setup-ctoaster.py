import os, sys, glob, shutil
import subprocess as sp
import json
import tempfile
import platform as plat
import utils as U


# Data and test GitHub repositories.

datarepo = 'https://github.com/derpycode/ctoaster.cupcake-data'
testrepo = 'https://github.com/derpycode/ctoaster.cupcake-test'


# Input helpers.

def ask(prompt, default, options=None):
    # Ensure default is a string, assuming UTF-8 encoding; adjust encoding as necessary.
    if isinstance(default, bytes):
        default = default.decode('utf-8')
    while True:
        res = input(f'{prompt} [{default}]: ') or default
        if not options or res in options:
            return res
        else:
            print('Input must be one of:', ' '.join(options))

def yesno(prompt, default):
    opts = 'Yn' if default else 'yN'
    return input(f'{prompt} [{opts}]: ') or default

# Get options from user.

versions = U.available_versions()
default_version = versions[-1]
config = U.read_ctoaster_config()
if config:
    print('Already set up...')
else:
    root = ask('Root directory', os.path.expanduser('~/ctoaster.cupcake'))
    base = os.path.abspath(os.path.join(root, os.pardir))
    data = ask('Data directory', os.path.join(base, 'ctoaster.cupcake-data'))
    test = ask('Test directory', os.path.join(base, 'ctoaster.cupcake-test'))
    jobs = ask('Jobs directory', os.path.join(base, 'ctoaster.cupcake-jobs'))
    vers = ask('Default version', default_version, str(versions))
    with open(U.ctoaster_cfgfile, 'w') as fp:
        print(f'ctoaster_root: {root}', file=fp)
        print(f'ctoaster_data: {data}', file=fp)
        print(f'ctoaster_test: {test}', file=fp)
        print(f'ctoaster_jobs: {jobs}', file=fp)
        print(f'ctoaster_version: {vers}', file=fp)

    try:
        if not os.path.exists(jobs): os.mkdir(jobs)
    except IOError:
        print('Could not create jobs directory!')


# Download data and test repositories if required.

download_data = False
download_test = False
if not os.path.exists(data):
    download_data = yesno('Data directory does not exist. Download?', True)
if not os.path.exists(test):
    download_test = yesno('Test directory does not exist. Download?', True)

if download_data:
    print('Downloading ctoaster.cupcake-data repository...')
    if sp.call(['git', 'clone', datarepo, data]) != 0:
        print('FAILED TO CLONE ctoaster.cupcake-data REPOSITORY!')
if download_test:
    print('Downloading ctoaster.cupcake-test repository...')
    if sp.call(['git', 'clone', testrepo, test]) != 0:
        print('FAILED TO CLONE ctoaster.cupcake-test REPOSITORY!')


# Test setup.

def setup_error(msg):
    print('\n' + 79 * '*' + '\n\n' + f'   {msg}\n')
    print('    IN THIS SITUATION, cTOASTER IS UNLIKELY TO WORK!')
    print('    CONTACT: andy@seao2.org\n')
    print(79 * '*')
    sys.exit(1)


# Test build platform discovery.

print('\nChecking build platform...')
if not U.read_ctoaster_config():
    sys.exit('Internal error: cTOASTER set up failed!')

platform = U.discover_platform()
if platform in ['LINUX', 'WINDOWS']:
    print(f'  Using default {platform.capitalize()} platform')
else:
    print(f'  Using platform "{platform}"')
try:
    exec(open(os.path.join(U.ctoaster_root, 'platforms', platform)).read())
except Exception as e:
    setup_error(f'PLATFORM SETUP FAILED!{e}')

print(f'    Fortran compiler:      {f90["compiler"]}')
print(f'    NetCDF base directory: {" ".join(netcdf["base"])}')

## Test Git version.
#
#if platform == 'LINUX':
#    print('\nTesting Git version...')
#
#    gitversion = sp.check_output(['git', '--version']).strip().split()[2]
#    gitversion = map(int, gitversion.split('.'))
#    if gitversion[0] < 2 or gitversion[1] < 3:
#        setup_error('GIT VERSION IS TOO OLD!')
#    else:
#        print('  Git version OK')

# Test Python setup.

print('\nTesting Python setup...')

# Make temporary directory.

tmpdir = tempfile.mkdtemp(dir=os.curdir)
import atexit
def remove_tmpdir():
    shutil.rmtree(tmpdir, ignore_errors=True)
atexit.register(remove_tmpdir)

# Write test Python script and helper shell script.

with open(os.path.join(tmpdir, 'pytest.py'), 'w') as pyfp:
    print("""
import os, os.path, sys, shutil, argparse, glob
import subprocess as sp
import platform as plat
try:
    import queue as qu
    import threading as thr
    import tkinter as tk
    from tkinter import font as tkFont
    from tkinter import messagebox as tkMessageBox
    from tkinter import ttk
    gui_available = True
except ImportError:
    gui_available = False

if gui_available:
    print("gui")
else:
    print("no-gui")
""", file=pyfp)

# Run test Python script: either fails completely (bad) or prints
# "gui" or "no-gui".

try:
    completed_process = sp.run([sys.executable, os.path.join(tmpdir, 'pytest.py')],
                               stdout=sp.PIPE,
                               stderr=sp.PIPE,
                               check=True,
                               encoding='utf-8')
    pyres = completed_process.stdout
    print('  Python install OK')
    if pyres.strip() == 'gui':
        print('  Python GUI available')
    else:
        print('  Python GUI not available')
except:
    setup_error('BAD PYTHON INSTALLATION!')


# Basic Fortran setup.

print('\nTesting Fortran setup...')

# Write basic test Fortran file.

with open(os.path.join(tmpdir, 'f90test.f90'), 'w') as f90fp:
    print("""
PROGRAM f90test
  IMPLICIT NONE
    PRINT *, 'OK'
END PROGRAM f90test
""", file=f90fp)

# Write test SCons file.

with open(os.path.join(tmpdir, 'SConstruct'), 'w') as sconsfp:
    sconsfp.write("""
import os
""")
    sconsfp.write(f'f90 = {f90}\n')
    sconsfp.write(f'netcdf = {netcdf}\n')
    sconsfp.write("""
envcopy = { }
envcopy['PATH'] = os.environ['PATH']
if 'TMP' in os.environ: envcopy['TMP'] = os.environ['TMP']
baselinkflags = []
if 'baselinkflags' in f90:
    baselinkflags = f90['baselinkflags']
extraf90libpaths = []
if 'libpath' in f90: extraf90libpaths = extraf90libpaths.append(f90['libpath'])
target_vs_arch = 'linux'
if 'TARGET_VS_ARCH' in os.environ:
    target_vs_arch = os.environ['TARGET_VS_ARCH']
env = Environment(ENV = envcopy,
                  TOOLS = ['default', f90['compiler']],
                  HOST_ARCH = target_vs_arch,
                  F90FLAGS = f90['baseflags'],
                  LINKFLAGS = baselinkflags,
                  FORTRANMODDIRPREFIX = f90['module_dir'],
                  FORTRANMODDIR = '${TARGET.dir}',
                  LIBPATH = extraf90libpaths)

if 'ld_library_path' in f90:
    env['ENV']['LD_LIBRARY_PATH'] = f90['ld_library_path']

env.Program('f90test.exe', ['f90test.f90'])
""")

# Run SCons in test directory.

###scons = os.path.join(U.ctoaster_root, 'tools', 'scons', 'scons.py')
scons = 'scons'
try:
    cmd = [scons, '-C', tmpdir]
    if plat.system() == 'Windows':
        cmd = ['python'] + cmd
    result = sp.run(cmd, stdout=sp.DEVNULL, stderr=sp.DEVNULL)
    if result.returncode == 0:
        print('  Basic executable build OK')
    else:
        setup_error('BASIC FORTRAN BUILD FAILED! [1.1]')
except Exception as e:
    setup_error(f'BASIC FORTRAN BUILD FAILED! [1.2] {e}')

# Check for existence of executable, run executable and check result.

if os.path.exists(os.path.join(tmpdir, 'f90test.exe')):
    print('  Basic executable output:')
    try:
        fres = sp.check_output([os.path.join(tmpdir, 'f90test.exe')]).decode('utf8')
        print(fres)
        if fres.strip() == 'OK':
            print('  Basic executable run OK')
        else:
            setup_error('BASIC FORTRAN BUILD FAILED! [1.3]')
    except Exception as e:
        setup_error('BASIC FORTRAN BUILD FAILED! [1.4]')
else:
    setup_error('BASIC FORTRAN BUILD FAILED! [1.5]')


# Fortran NetCDF setup.

# Write Fortran NetCDF test file.

with open(os.path.join(tmpdir, 'f90nctest.f90'), 'w') as f90fp:
    print("""
PROGRAM nf90test
  USE netcdf
  IMPLICIT NONE
  INTEGER :: ncido, dimido, varido
  INTEGER :: ncidi, dimidi, varidi
  REAL, DIMENSION(10) :: valso = (/ 1, 4, 9, 16, 25, 36, 49, 64, 81, 100 /)
  INTEGER :: lenchk
  REAL, DIMENSION(10) :: valsi
  CALL chk(NF90_CREATE('tmp.nc', NF90_CLOBBER, ncido))
  CALL chk(NF90_DEF_DIM(ncido, 'x', 10, dimido))
  CALL chk(NF90_DEF_VAR(ncido, 'v', NF90_DOUBLE, (/ dimido /), varido))
  CALL chk(NF90_ENDDEF(ncido))
  CALL chk(NF90_PUT_VAR(ncido, varido, valso))
  CALL chk(NF90_CLOSE(ncido))
  CALL chk(NF90_OPEN('tmp.nc', NF90_NOWRITE, ncidi))
  CALL chk(NF90_INQ_DIMID(ncidi, 'x', dimidi))
  CALL chk(NF90_INQUIRE_DIMENSION(ncidi, dimidi, LEN=lenchk))
  IF (lenchk /= 10) THEN
     PRINT *, 'Dimension length mismatch'
     STOP 1
  END IF
  CALL chk(NF90_INQ_VARID(ncidi, 'v', varidi))
  CALL chk(NF90_GET_VAR(ncidi, varidi, valsi))
  IF (ANY(valsi /= valso)) THEN
     PRINT *, 'Value mismatch'
     STOP 1
  ELSE
    PRINT *, 'OK'
  END IF
CONTAINS
  SUBROUTINE chk(status)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: status
    IF (status /= NF90_NOERR) THEN
       PRINT *, 'NetCDF error: ', NF90_STRERROR(status)
       STOP 1
    END IF
  END SUBROUTINE chk
END PROGRAM nf90test
""", file=f90fp)

# Write test SCons file.

with open(os.path.join(tmpdir, 'SConstruct'), 'w') as sconsfp:
    sconsfp.write("""
import os
""")
    sconsfp.write(f'f90 = {f90}\n')
    sconsfp.write(f'netcdf = {netcdf}\n')
    print("""
envcopy = { }
envcopy['PATH'] = os.environ['PATH']
if 'TMP' in os.environ: envcopy['TMP'] = os.environ['TMP']
baselinkflags = []
if 'baselinkflags' in f90:
    baselinkflags = f90['baselinkflags']
extraf90libpaths = []
if 'libpath' in f90: extraf90libpaths.append(f90['libpath'])
target_vs_arch = 'linux'
if 'TARGET_VS_ARCH' in os.environ:
    target_vs_arch = os.environ['TARGET_VS_ARCH']
netcdfinc = os.path.join(str(netcdf['base']), 'include')
if not os.path.exists(netcdfinc): netcdfinc = netcdf['base']
netcdflib = os.path.join(str(netcdf['base']), 'lib')
if not os.path.exists(netcdflib): netcdflib = netcdf['base']
env = Environment(ENV = envcopy,
                  TOOLS = ['default', f90['compiler']],
                  HOST_ARCH = target_vs_arch,
                  F90FLAGS = f90['baseflags'],
                  LINKFLAGS = baselinkflags,
                  F90PATH = [netcdfinc],
                  FORTRANMODDIRPREFIX = f90['module_dir'],
                  FORTRANMODDIR = '${TARGET.dir}',
                  LIBPATH = [netcdflib] + extraf90libpaths,
                  LIBS = netcdf['libs'])

if 'ld_library_path' in f90:
    env['ENV']['LD_LIBRARY_PATH'] = f90['ld_library_path']

env.Program('f90nctest.exe', ['f90nctest.f90'])
""", file=sconsfp)

# Run SCons in test directory.

try:
    cmd = [scons, '-C', tmpdir]
    if plat.system() == 'Windows':
        cmd = ['python'] + cmd
    result = sp.run(cmd, stdout=sp.DEVNULL, stderr=sp.DEVNULL)
    if result.returncode == 0:
        print('  NetCDF executable build OK')
    else:
        setup_error('NETCDF FORTRAN BUILD FAILED! [2.1]')
except Exception as e:
    setup_error(f'NETCDF FORTRAN BUILD FAILED! [2.2] {e}')

# Check for existence of executable, run executable and check result.

if os.path.exists(os.path.join(tmpdir, 'f90nctest.exe')):
    try:
        os.chdir(tmpdir)
        fres = sp.check_output([os.path.join(os.curdir, 'f90nctest.exe')]).decode('utf8')
        os.chdir(os.pardir)
        if fres.strip() == 'OK':
            print('  NetCDF executable run OK')
        else:
            setup_error('NETCDF FORTRAN BUILD FAILED! [2.3]')
    except Exception as e:
        setup_error('NETCDF FORTRAN BUILD FAILED! [2.4]')
else:
    setup_error('NETCDF FORTRAN BUILD FAILED! [2.5]')


# Final message.

print('\nEverything seems to be OK!')
print('\nCheck that the compiler and NetCDF settings ' +
      'above are what you expect.')
print('\n')
