#!/usr/bin/env python2

from __future__ import print_function
import os, os.path, sys, errno, shutil
import argparse

import utils

# GENIE configuration

config = utils.read_cgenie_config()
if not config: sys.exit("GENIE not set up: run the setup.py script!")

cgenie_root = config['cgenie_root']
cgenie_data = config['cgenie_data']
cgenie_jobs = config['cgenie_jobs']
cgenie_version = config['cgenie_version']


# Command line arguments.

parser = argparse.ArgumentParser(description='Configure GENIE jobs')
parser.add_argument('job_name', help='Job name')
parser.add_argument('-O', '--overwrite', action='store_true',
                    help='Overwrite existing job of given name')
parser.add_argument('-b', '--base-config', required=True,
                    help='Base configuration name')
parser.add_argument('-u', '--user-config', required=True,
                    help='User configuration name')
parser.add_argument('-j', '--job-dir', default=cgenie_jobs,
                    help='Specify alternative destination directory for jobs')
parser.add_argument('-l', '--run-length', type=int, required=True,
                    help='Job run length (years)')
args = parser.parse_args()
job_name = args.job_name
base_config = args.base_config
user_config = args.user_config
run_length = args.run_length
overwrite = args.overwrite
print("   Job name: ", job_name)
print("Base config: ", base_config)
print("User config: ", user_config)
print(" Run length: ", run_length)
print("  Overwrite: ", overwrite)


# Read and parse configuration files.

base_config = os.path.join(cgenie_data, 'base-configs', base_config + '.config')
base = utils.read_config(base_config, 'Base configuration')
user_config = os.path.join(cgenie_data, 'user-configs', user_config)
user = utils.read_config(user_config, 'User configuration')


# Set up source directory.

srcdir = 'src'
utils.set_src_dir(srcdir)


# Determine modules used in job.

base_mod_opts = filter(lambda x: x[0:8] == 'ma_flag_', base.keys())
base_mod_flags = { k: base[k] for k in base_mod_opts }
user_mod_opts = filter(lambda x: x[0:8] == 'ma_flag_', user.keys())
user_mod_flags = { k: base[k] for k in user_mod_opts }
merged_mod_flags = utils.merge_flags(base_mod_flags, user_mod_flags)
mod_flags = filter(lambda k: merged_mod_flags[k], merged_mod_flags.keys())
modules = map(utils.module_from_flagname, mod_flags)


# Set up job directory and per-module sub-directories.

job_dir = os.path.join(cgenie_jobs, job_name)
if overwrite: shutil.rmtree(job_dir, ignore_errors=True)
try: os.mkdir(job_dir)
except OSError as e: sys.exit("Can't create job directory: " + job_dir)
for m in modules:
    os.mkdir(os.path.join(job_dir, m))


# Write configuration information to job directory.

job_cfg_dir = os.path.join(job_dir, 'config')
os.mkdir(job_cfg_dir)
shutil.copyfile(base_config, os.path.join(job_cfg_dir, 'base_config'))
shutil.copyfile(user_config, os.path.join(job_cfg_dir, 'user_config'))
with open(os.path.join(job_cfg_dir, 'config'), 'w') as fp:
    print('base_config: ' + base_config, file=fp)
    print('user_config: ' + user_config, file=fp)


# Create platform.py SCons file for job.
###===> DO PLATFORM-SPECIFIC SETUP HERE.

shutil.copy(os.path.join('config', 'platform.py'), job_cfg_dir)


# Create job.py SCons file for job.

###===> CURRENTLY ONLY "DEVELOPMENT": NEED TO DO SOMETHING ABOUT
###     GETTING SPECIFIED MODEL VERSION FOR JOB
if cgenie_version != 'DEVELOPMENT':
    sys.exit("Not set up for using specific model versions yet!")
scons_srcdir = os.path.join(cgenie_root, 'src')
with open(os.path.join(job_cfg_dir, 'job.py'), 'w') as fp:
    print('# Model source directory', file=fp)
    print("srcdir = '" + scons_srcdir + "'\n", file=fp)
    ###===> ALL BODGED: NEED TO GET COORDINATE DEFINITIONS FROM
    ###     NAMELISTS
    print("# Dimension sizes.", file=fp)
    print("nlons = 36", file=fp)
    print("nlats = 36", file=fp)
    print("nlevs = 16", file=fp)
    print("ntracers = 14\n", file=fp)
    print("# Coordinate definitions.", file=fp)
    print("coordvars = { 'GENIENX':          nlons,", file=fp)
    print("              'GENIENY':          nlats,", file=fp)
    print("              'GOLDSTEINNLONS':   nlons,", file=fp)
    print("              'GOLDSTEINNLATS':   nlats,", file=fp)
    print("              'GOLDSTEINNLEVS':   nlevs,", file=fp)
    print("              'GOLDSTEINNTRACS' : ntracers }", file=fp)


# Create SConstruct file for job.

shutil.copy('SConstruct', job_dir)


# Namelist construction.

for m in modules + ['main', 'gem']:
    minfo = utils.lookup_module(m)
    if minfo['flag_name'] == 'NONE':
        nmlin = os.path.join(srcdir, m + '-defaults.nml')
    else:
        nmlin = os.path.join(srcdir, m, m + '-defaults.nml')
    nmlout = os.path.join(job_dir, 'data_' + minfo['nml_file'])
    with open(nmlin) as fp:
        nml = utils.Namelist(fp)
        nml.merge(minfo['prefix'], minfo['exceptions'], base, user)
        with open(nmlout, 'w') as ofp: nml.write(ofp)
