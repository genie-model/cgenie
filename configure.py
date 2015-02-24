#!/usr/bin/env python2

from __future__ import print_function
import os
import os.path
import sys
import errno
import shutil
import utils

# GENIE configuration

config = utils.read_cgenie_config()
if not config: sys.exit("GENIE not set up: run the setup.py script!")

cgenie_root = config['cgenie_root']
cgenie_data = config['cgenie_data']
cgenie_jobs = config['cgenie_jobs']


# Command line arguments: <job-name> <base-config> <user-config> <run-length>
# test-job cgenie.eb_go_gs_ac_bg.worjh2.BASEFe EXAMPLE.worjh2.PO4Fe.SPIN 1000

if len(sys.argv) != 5:
    sys.exit("Usage: configure.py <job-name> <base-config> "
             "<user-config> <run-length>")
job_name = sys.argv[1]
base_config = sys.argv[2]
user_config = sys.argv[3]
run_length = sys.argv[4]
print("   Job name: " + job_name)
print("Base config: " + base_config)
print("User config: " + user_config)
print(" Run length: " + run_length)


# Read and parse configuration files.

base_config = os.path.join(cgenie_data, 'base-configs', base_config + '.config')
base = utils.read_config(base_config, 'Base configuration')
user_config = os.path.join(cgenie_data, 'user-configs', user_config)
user = utils.read_config(user_config, 'User configuration')


# Determine modules used in job.

base_mod_opts = filter(lambda x: x[0:8] == 'ma_flag_', base.keys())
base_mod_flags = { k: base[k] for k in base_mod_opts }
user_mod_opts = filter(lambda x: x[0:8] == 'ma_flag_', user.keys())
user_mod_flags = { k: base[k] for k in user_mod_opts }
merged_mod_flags = utils.merge_flags(base_mod_flags, user_mod_flags)
mod_flags = filter(lambda k: merged_mod_flags[k], merged_mod_flags.keys())
modules = map(utils.module_from_flagname, mod_flags)
print(modules)

# Set up job directory and per-module sub-directories.

job_dir = os.path.join(cgenie_jobs, job_name)
### TEMPORARY:
os.system('/bin/rm -fr ' + job_dir)
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


# Create SConstruct file for job.
###===> CURRENTLY ONLY "DEVELOPMENT": NEED TO DO SOMETHING ABOUT
###     GETTING SPECIFIED MODEL VERSION FOR JOB


# Per-module namelist substitution.


# Main model namelist substitution.
