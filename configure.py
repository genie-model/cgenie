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
parser.add_argument('-b', '--base-config',
                    help='Base configuration name')
parser.add_argument('-u', '--user-config',
                    help='User configuration name')
parser.add_argument('-c', '--config',
                    help='Consolidated configuration name')
parser.add_argument('-r', '--restart',
                    help='Restart name')
parser.add_argument('-j', '--job-dir', default=cgenie_jobs,
                    help='Specify alternative destination directory for jobs')
parser.add_argument('-l', '--run-length', type=int, required=True,
                    help='Job run length (years)')
args = parser.parse_args()
job_name = args.job_name
overwrite = args.overwrite
base_config = args.base_config
user_config = args.user_config
full_config = args.config
restart = args.restart
job_dir_base = args.job_dir
run_length = args.run_length
print("   Job name: ", job_name)
print("Base config: ", base_config)
print("User config: ", user_config)
print("Full config: ", full_config)
print(" Run length: ", run_length)
print("  Overwrite: ", overwrite)


# Check configuration file options.

base_and_user_config = base_config and user_config
if not base_and_user_config and not full_config:
    sys.exit("Either base and user or full configuration must be specified")
if base_and_user_config and full_config:
    sys.exit("Only one of base and user or full configuration may be specified")


# Read and parse configuration files.

if (base_and_user_config):
    base_config = os.path.join(cgenie_data, 'base-configs',
                               base_config + '.config')
    base = utils.read_config(base_config, 'Base configuration')
    user_config = os.path.join(cgenie_data, 'user-configs', user_config)
    user = utils.read_config(user_config, 'User configuration')
    configs = [base, user]
else:
    full_config = os.path.join(cgenie_data, 'full-configs',
                               full_config + '.config')
    full = utils.read_config(full_config, 'Full configuration')
    configs = [full]


# Set up source and per-module input data directories.

srcdir = 'src'
datadir = 'data'
utils.set_dirs(srcdir, datadir)


# Determine modules used in job.

def extract_mod_opts(c):
    return filter(lambda x: x[0:8] == 'ma_flag_', c.keys())
mod_opts = map(extract_mod_opts, configs)
def extract_mod_flags(c, os):
    return { k: c[k] for k in os }
mod_flags = map(extract_mod_flags, configs, mod_opts)
merged_mod_flags = utils.merge_flags(mod_flags)
mod_flags = filter(lambda k: merged_mod_flags[k], merged_mod_flags.keys())
modules = map(utils.module_from_flagname, mod_flags)


# Set up job directory and per-module sub-directories.

job_dir = os.path.join(job_dir_base, job_name)
if overwrite: shutil.rmtree(job_dir, ignore_errors=True)
try: os.mkdir(job_dir)
except OSError as e: sys.exit("Can't create job directory: " + job_dir)
for m in modules:
    os.makedirs(os.path.join(job_dir, 'input', m))
    os.makedirs(os.path.join(job_dir, 'output', m))
    if restart: os.makedirs(os.path.join(job_dir, 'restart', m))
os.makedirs(os.path.join(job_dir, 'input', 'main'))
os.makedirs(os.path.join(job_dir, 'output', 'main'))
if restart: os.makedirs(os.path.join(job_dir, 'restart', 'main'))


# Write configuration information to job directory.

job_cfg_dir = os.path.join(job_dir, 'config')
os.mkdir(job_cfg_dir)
with open(os.path.join(job_cfg_dir, 'config'), 'w') as fp:
    if base_config:
        shutil.copyfile(base_config, os.path.join(job_cfg_dir, 'base_config'))
        print('base_config: ' + base_config, file=fp)
    if user_config:
        shutil.copyfile(user_config, os.path.join(job_cfg_dir, 'user_config'))
        print('user_config: ' + user_config, file=fp)
    if full_config:
        shutil.copyfile(full_config, os.path.join(job_cfg_dir, 'full_config'))
        print('full_config: ' + full_config, file=fp)


# Create platform.py SCons file for job.
###===> DO PLATFORM-SPECIFIC SETUP HERE.

shutil.copy(os.path.join('config', 'platform.py'), job_cfg_dir)


# Extract coordinate definitions from configuration.

defines = utils.extract_defines(configs)
maxdeflen = max(map(len, defines.keys()))
deflines = map(lambda d: ("'" + d + "':").ljust(maxdeflen + 4) +
               str(defines[d]), defines.keys())
deflines[0] = 'coordvars = { ' + deflines[0]
for i in range(1, len(deflines)):
    deflines[i] = '              ' + deflines[i]
for i in range(len(deflines)-1):
    deflines[i] += ','
deflines[-1] += ' }'


# Set up timestepping and restart options: this is only done if we
# have a base+user configuration (i.e. the normal case); some test
# configurations already include timestepping options.

if not full_config:
    tsopts = utils.timestepping_options(run_length, defines, t100=False)
    rstopts = utils.restart_options(restart)
    configs = [base, tsopts, user]


# Create job.py SCons file for job.

###===> CURRENTLY ONLY "DEVELOPMENT": NEED TO DO SOMETHING ABOUT
###     GETTING SPECIFIED MODEL VERSION FOR JOB
if cgenie_version != 'DEVELOPMENT':
    sys.exit("Not set up for using specific model versions yet!")
scons_srcdir = os.path.join(cgenie_root, 'src')
with open(os.path.join(job_cfg_dir, 'job.py'), 'w') as fp:
    print('# Model source directory', file=fp)
    print("srcdir = '" + scons_srcdir + "'\n", file=fp)
    print("# Coordinate definitions.", file=fp)
    for l in deflines: print(l, file=fp)


# Create SConstruct file and "go" script for job.

shutil.copy('SConstruct', job_dir)
shutil.copy('go.py', job_dir)


# Set up per-module extra data files (these are files that don't
# appear in any configuration information...).

extra_data_files = { }
if 'embm' in modules and 'ents' in modules:
    extra_data_files['embm'] = ['inv_linterp_matrix.dat',
                                'NCEP_airt_monthly.dat',
                                'NCEP_pptn_monthly.dat',
                                'NCEP_RH_monthly.dat',
                                'atm_albedo_monthly.dat',
                                'uvic_windx.silo',
                                'uvic_windy.silo',
                                'monthly_windspd.silo']
if 'ents' in modules:
    extra_data_files['ents'] = ['ents_config.par', 'sealevel_config.par']


# Construct namelists and copy data files.

for m in modules + ['main', 'gem']:
    minfo = utils.lookup_module(m)
    if minfo['flag_name'] == 'NONE':
        nmlin = os.path.join(srcdir, m + '-defaults.nml')
    else:
        nmlin = os.path.join(srcdir, m, m + '-defaults.nml')
    nmlout = os.path.join(job_dir, 'data_' + minfo['nml_file'])
    with open(nmlin) as fp:
        nml = utils.Namelist(fp)
        nml.merge(minfo['prefix'], minfo['exceptions'], configs)
        with open(nmlout, 'w') as ofp: nml.write(ofp)
        utils.copy_data_files(m, nml, os.path.join(job_dir, 'input', m),
                              extra_data_files.get(m))


# Extra data files for main program.

jobmaindatadir = os.path.join(job_dir, 'input', 'main')
srcmaindatadir = os.path.join(cgenie_root, 'data', 'main')
for s in ['atm', 'ocn', 'sed']:
    shutil.copy(os.path.join(srcmaindatadir, 'tracer_define.' + s),
                jobmaindatadir)
