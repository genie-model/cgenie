#!/usr/bin/env python2

from __future__ import print_function
import os, os.path, sys, errno, shutil
import optparse
import subprocess as sp

import utils as U
import config_utils as C


# GENIE configuration

if not U.read_cgenie_config():
    sys.exit('GENIE not set up: run the setup-cgenie script!')


# Command line arguments.

parser = optparse.OptionParser(usage='new-job [options] job-name run-length',
                               description='Configure GENIE jobs')
parser.add_option('-O', '--overwrite',     help='Overwrite existing job',
                  action='store_true')
parser.add_option('-b', '--base-config',   help='Base configuration name')
parser.add_option('-u', '--user-config',   help='User configuration name')
parser.add_option('-c', '--config',        help='Full configuration name')
parser.add_option('-r', '--restart',       help='Restart name')
parser.add_option('--old-restart',         help='Restart from old cGENIE job',
                  action='store_true')
parser.add_option('--t100',                help='Use "T100" timestepping',
                  action='store_true')
parser.add_option('-j', '--job-dir',       help='Alternative job directory',
                  default=U.cgenie_jobs)
parser.add_option('-v', '--model-version', help='Model version to use',
                  default = U.cgenie_version)
opts, args = parser.parse_args()
if len(args) != 2:
    parser.print_help()
    sys.exit()
job_name = args[0]
run_length = int(args[1])
overwrite = opts.overwrite
base_config = opts.base_config
user_config = opts.user_config
full_config = opts.config
restart = opts.restart
old_restart = True if opts.old_restart else False
t100 = True if opts.t100 else False
job_dir_base = opts.job_dir
model_version = opts.model_version
if model_version not in U.available_versions():
    sys.exit('Model version "' + model_version + '" does not exist')


# If a specific model version is requested, set up a repository clone
# on the appropriate branch and run the configuration script at that
# version.

repo_version = 'DEVELOPMENT'
if (os.path.exists('repo-version')):
    with open('repo-version') as fp:
        repo_version = fp.readline().strip()
if model_version != repo_version:
    repodir = U.setup_version_repo(model_version)
    os.chdir(repodir)
    os.execv(sys.executable,
             [os.path.join(os.curdir, 'tools', 'new-job.py')] + sys.argv)


# All set up.  Off we go...

print("   Job name: ", job_name)
print("Base config: ", base_config)
print("User config: ", user_config)
print("Full config: ", full_config)
print(" Run length: ", run_length)
print("  Overwrite: ", overwrite)
print("      Model: ", model_version)


# Check configuration file options.

base_and_user_config = base_config and user_config
if not base_and_user_config and not full_config:
    sys.exit("Either base and user or full configuration must be specified")
if base_and_user_config and full_config:
    sys.exit("Only one of base and user or full configuration may be specified")


# Check for existence of any restart job.

if restart:
    if old_restart:
        restart_path = os.path.join(os.path.expanduser('~/cgenie_output'),
                                    restart)
    elif os.path.exists(restart):
        restart_path = restart
    else:
        restart_path = os.path.join(job_dir_base, restart, 'output')
    if not os.path.exists(restart_path):
        if old_restart:
            sys.exit('Old cGENIE restart job "' + restart + '" does not exist')
        else:
            sys.exit('Restart job "' + restart + '" does not exist')


# Read and parse configuration files.

if (base_and_user_config):
    if not os.path.exists(base_config):
        base_config = os.path.join(U.cgenie_data, 'base-configs',
                                   base_config + '.config')
    base = C.read_config(base_config, 'Base configuration')
    if not os.path.exists(user_config):
        user_config = os.path.join(U.cgenie_data, 'user-configs', user_config)
    user = C.read_config(user_config, 'User configuration')
    configs = [base, user]
else:
    if not os.path.exists(full_config):
        full_config = os.path.join(U.cgenie_data, 'full-configs',
                                   full_config + '.config')
    full = C.read_config(full_config, 'Full configuration')
    configs = [full]


# Set up source and per-module input data directories.

srcdir = 'src'
datadir = 'data'
C.set_dirs(srcdir, datadir)


# Determine modules used in job.

def extract_mod_opts(c):
    return [x for x in c.keys() if x.startswith('ma_flag_')]
mod_opts = map(extract_mod_opts, configs)
def extract_mod_flags(c, os):
    return { k: c[k] for k in os }
mod_flags = map(extract_mod_flags, configs, mod_opts)
merged_mod_flags = C.merge_flags(mod_flags)
mod_flags = [k for k in merged_mod_flags.keys() if merged_mod_flags[k]]
modules = map(C.module_from_flagname, mod_flags)


# Set up job directory and per-module sub-directories.

job_dir = os.path.join(job_dir_base, job_name)
if overwrite: shutil.rmtree(job_dir, ignore_errors=True)
try: os.makedirs(job_dir)
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
    print('run_length:', run_length, file=fp)
    print('t100:', t100, file=fp)


# Extract coordinate definitions from configuration.

defines = C.extract_defines(configs)
maxdeflen = max(map(len, defines.keys()))
deflines = [("'" + d + "':").ljust(maxdeflen + 4) + str(defines[d])
            for d in defines.keys()]
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
    tsopts = C.timestepping_options(run_length, defines, t100=t100)
    rstopts = C.restart_options(restart)
    configs = [base, tsopts, rstopts, user]


# Create job.py SCons file for job.

with open(os.path.join(job_cfg_dir, 'job.py'), 'w') as fp:
    print("# Coordinate definitions.", file=fp)
    for l in deflines: print(l, file=fp)


# Create model version file for build.

with open(os.path.join(job_cfg_dir, 'model-version'), 'w') as fp:
    if model_version == 'DEVELOPMENT':
        try:
            with open(os.devnull, 'w') as sink:
                rev = sp.check_output(['git', 'describe',
                                       '--tags', 'HEAD'], stderr=sink).strip()
            print('DEVELOPMENT:' + rev, file=fp)
        except:
            print('DEVELOPMENT:UNKNOWN', file=fp)
    else:
        print(model_version, file=fp)


# Create "go" script for job.

shutil.copy(os.path.join(U.cgenie_root, 'tools', 'go'), job_dir)
shutil.copy(os.path.join(U.cgenie_root, 'tools', 'go.bat'), job_dir)


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

if 'sedgem' in modules:
    extra_data_files['sedgem'] = ['lookup_calcite_4.dat', 'lookup_opal_5.dat']


# Construct namelists and copy data files.

configs.append(C.make_coordinates(defines))
for m in modules + ['main', 'gem']:
    minfo = C.lookup_module(m)
    if minfo['flag_name'] == 'NONE':
        nmlin = os.path.join(srcdir, m + '-defaults.nml')
    else:
        nmlin = os.path.join(srcdir, m, m + '-defaults.nml')
    nmlout = os.path.join(job_dir, 'data_' + minfo['nml_file'])
    with open(nmlin) as fp:
        nml = C.Namelist(fp)
        nml.merge(minfo['prefix'], minfo['exceptions'], configs)
        with open(nmlout, 'w') as ofp: nml.write(ofp)
        C.copy_data_files(m, nml, os.path.join(job_dir, 'input', m),
                          extra_data_files.get(m))
        if restart:
            C.copy_restart_files(m, nml, os.path.join(job_dir, 'restart', m),
                                 restart_path)



# Extra data files for main program.

jobmaindatadir = os.path.join(job_dir, 'input', 'main')
srcmaindatadir = os.path.join(U.cgenie_root, 'data', 'main')
for s in ['atm', 'ocn', 'sed']:
    shutil.copy(os.path.join(srcmaindatadir, 'tracer_define.' + s),
                jobmaindatadir)
