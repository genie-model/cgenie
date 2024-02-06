from __future__ import print_function
import os, os.path, sys, errno, shutil, datetime
import optparse
import subprocess as sp

import utils as U
import config_utils as C


# GENIE configuration

if not U.read_ctoaster_config():
    sys.exit('GENIE not set up: run the setup-ctoaster script!')


# Command line arguments.

parser = optparse.OptionParser(usage='new-job [options] job-name run-length',
                               description='Configure GENIE jobs')
parser.add_option('-O', '--overwrite',     help='Overwrite existing job',
                  action='store_true')
parser.add_option('-b', '--base-config',   help='Base configuration name')
parser.add_option('-u', '--user-config',   help='User configuration name')
parser.add_option('-m', '--config-mods',   help='Configuration mods filename')
parser.add_option('-c', '--config',        help='Full configuration name')
parser.add_option('-r', '--restart',       help='Restart name')
parser.add_option('--old-restart',         help='Restart from old ctoaster job',
                  action='store_true')
parser.add_option('--t100',                help='Use "T100" timestepping',
                  action='store_true')
parser.add_option('-t', '--test-job',      help='Set up from test')
parser.add_option('-j', '--job-dir',       help='Alternative job directory',
                  default=U.ctoaster_jobs)
parser.add_option('-v', '--model-version', help='Model version to use',
                  default = U.ctoaster_version)
parser.add_option('-g', '--gui', action='store_true',
                  help=optparse.SUPPRESS_HELP)
opts, args = parser.parse_args()
if not opts.test_job and len(args) != 2 or opts.test_job and len(args) != 0:
    parser.print_help()
    sys.exit()
if len(args) == 2:
    job_name = args[0]
    run_length = int(args[1])
else:
    job_name = opts.test_job
running_from_gui = opts.gui
overwrite = opts.overwrite
base_config = opts.base_config
user_config = opts.user_config
config_mods = opts.config_mods
full_config = opts.config
restart = opts.restart
test_job = opts.test_job
old_restart = True if opts.old_restart else False
t100 = True if opts.t100 else False
job_dir_base = opts.job_dir
model_version = opts.model_version
if model_version not in U.available_versions():
    sys.exit('Model version "' + model_version + '" does not exist')


def error_exit(msg):
    if running_from_gui:
        sys.exit('ERR:' + msg)
    else:
        sys.exit(msg)

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


# Check configuration file options.

base_and_user_config = base_config and user_config
if not base_and_user_config and not full_config and not test_job:
    error_exit('Either base and user, full configuration ' +
               'or test must be specified')
if not base_and_user_config and config_mods:
    error_exit('Configuration mods can only be specified if ' +
               'using base and user configuration')
nset = 0
if base_and_user_config: nset += 1
if full_config:          nset += 1
if test_job:             nset += 1
if nset > 1:
    error_exit('Only one of base and user, full configuration ' +
               'or test may be specified')


# 

if test_job:
    test_dir = os.path.join(U.ctoaster_test, test_job)
    with open(os.path.join(test_dir, 'test_info')) as fp:
        for line in fp:
            ss = line.split(':')
            k = ss[0].strip()
            v = ':'.join(ss[1:]).strip()
            if k == 'restart_from': restart = v
            elif k == 'run_length': run_length = int(v)
            elif k == 't100': t100 = True if v == 'True' else False


# Check for existence of any restart job.

if restart:
    if old_restart:
        restart_path = os.path.join(os.path.expanduser('~/ctoaster_output'),
                                    restart)
    elif os.path.exists(restart):
        restart_path = restart
    else:
        restart_path = os.path.join(job_dir_base, restart, 'output')
    if not os.path.exists(restart_path):
        if old_restart:
            error_exit('Old ctoaster restart job "' + restart +
                       '" does not exist')
        else:
            error_exit('Restart job "' + restart + '" does not exist')


# All set up.  Off we go...

if not running_from_gui:
    print('   Job name:', job_name + (' [TEST]' if test_job else ''))
    if base_and_user_config:
        print('Base config:', base_config)
        print('User config:', user_config)
    if config_mods: print('Config mods:', config_mods)
    if full_config: print('Full config:', full_config)
    if not test_job: print(' Run length:', run_length)
    print('  Overwrite:', overwrite)
    print('      Model:', model_version)


# Read and parse configuration files.

if (base_and_user_config):
    if not os.path.exists(base_config):
        base_config_dir = os.path.join(U.ctoaster_data, 'base-configs')
        base_config_path = os.path.join(base_config_dir,
                                        base_config + '.config')
    else:
        base_config_dir = os.getcwd()
        base_config_path = base_config
    base = C.read_config(base_config_path, 'Base configuration')
    if not os.path.exists(user_config):
        user_config_dir = os.path.join(U.ctoaster_data, 'user-configs')
        user_config_path = os.path.join(user_config_dir, user_config)
    else:
        user_config_dir = os.getcwd()
        user_config_path = user_config
    user = C.read_config(user_config_path, 'User configuration')
    configs = [base, user]
    if config_mods:
        mods = C.read_config(config_mods, 'Configuration modifications')
        configs.append(mods)
elif full_config:
    if not os.path.exists(full_config):
        full_config_dir = os.path.join(U.ctoaster_data, 'full-configs')
        full_config_path = os.path.join(full_config_dir,
                                        full_config + '.config')
    else:
        full_config_dir = os.getcwd()
        full_config_path = full_config
    full = C.read_config(full_config_path, 'Full configuration')
    configs = [full]
else:
    # Test job -- read base_config, user_config and full_config files
    # as they exist.
    if os.path.exists(os.path.join(test_dir, 'full_config')):
        full = C.read_config(os.path.join(test_dir, 'full_config'),
                             'Full configuration')
        configs = [full]
    else:
        base = C.read_config(os.path.join(test_dir, 'base_config'),
                             'Base configuration')
        user = C.read_config(os.path.join(test_dir, 'user_config'),
                             'User configuration')
        configs = [base, user]


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
modules = list(map(C.module_from_flagname, mod_flags))


# Set up job directory and per-module sub-directories.

def safe_mkdir(p):
    if not os.path.exists(p): os.makedirs(p)

job_dir = os.path.join(job_dir_base, job_name)
if not running_from_gui:
    if overwrite: shutil.rmtree(job_dir, ignore_errors=True)
    try: safe_mkdir(job_dir)
    except OSError as e: error_exit("Can't create job directory: " + job_dir)
try:
    for m in modules:
        safe_mkdir(os.path.join(job_dir, 'input', m))
        safe_mkdir(os.path.join(job_dir, 'output', m))
        if restart: safe_mkdir(os.path.join(job_dir, 'restart', m))
    safe_mkdir(os.path.join(job_dir, 'input', 'main'))
    safe_mkdir(os.path.join(job_dir, 'output', 'main'))
    if restart: safe_mkdir(os.path.join(job_dir, 'restart', 'main'))
except Exception as e:
    with open('/dev/tty', 'w') as fp:
        print(e, file=fp)


# Write configuration information to job directory.

cfg_dir = os.path.join(job_dir, 'config')
if not running_from_gui:
    os.mkdir(cfg_dir)
    if not test_job:
        with open(os.path.join(cfg_dir, 'config'), 'w') as fp:
            if base_config:
                print('base_config_dir:', base_config_dir, file=fp)
                print('base_config:', base_config, file=fp)
            if user_config:
                print('user_config_dir:', user_config_dir, file=fp)
                print('user_config:', user_config, file=fp)
            if full_config:
                print('full_config_dir:', full_config_dir, file=fp)
                print('full_config:', full_config, file=fp)
            if config_mods:
                print('config_mods:', config_mods, file=fp)
            print('config_date:', str(datetime.datetime.today()), file=fp)
            print('run_length:', run_length, file=fp)
            print('t100:', t100, file=fp)
            if restart: print('restart:', restart, file=fp)

if test_job:
    shutil.copyfile(os.path.join(test_dir, 'test_info'),
                    os.path.join(cfg_dir, 'config'))
    if os.path.exists(os.path.join(test_dir, 'base_config')):
        shutil.copyfile(os.path.join(test_dir, 'base_config'),
                        os.path.join(cfg_dir, 'base_config'))
    if os.path.exists(os.path.join(test_dir, 'user_config')):
        shutil.copyfile(os.path.join(test_dir, 'user_config'),
                        os.path.join(cfg_dir, 'user_config'))
    if os.path.exists(os.path.join(test_dir, 'full_config')):
        shutil.copyfile(os.path.join(test_dir, 'full_config'),
                        os.path.join(cfg_dir, 'full_config'))
else:
    if base_config:
        shutil.copyfile(base_config_path, os.path.join(cfg_dir, 'base_config'))
    if user_config:
        shutil.copyfile(user_config_path, os.path.join(cfg_dir, 'user_config'))
    if full_config:
        shutil.copyfile(full_config_path, os.path.join(cfg_dir, 'full_config'))
    if config_mods and not running_from_gui:
        shutil.copyfile(config_mods, os.path.join(cfg_dir, 'config_mods'))


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

if len(configs) > 1:
    tsopts = C.timestepping_options(run_length, defines, t100=t100,
                                    quiet=running_from_gui)
    rstopts = C.restart_options(restart)
    configs = [configs[0], tsopts, rstopts] + configs[1:]


# Create model version file for build.

with open(os.path.join(cfg_dir, 'model-version'), 'w') as fp:
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

shutil.copy(os.path.join(U.ctoaster_root, 'tools', 'go'), job_dir)
shutil.copy(os.path.join(U.ctoaster_root, 'tools', 'go.bat'), job_dir)


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
        nml.merge(minfo['prefix'], configs)
        with open(nmlout, 'w') as ofp: nml.write(ofp)
        C.copy_data_files(m, nml, os.path.join(job_dir, 'input', m),
                          extra_data_files.get(m))
        if restart:
            C.copy_restart_files(m, nml, os.path.join(job_dir, 'restart', m),
                                 restart_path)


# Extra data files for main program.

jobmaindatadir = os.path.join(job_dir, 'input', 'main')
srcmaindatadir = os.path.join(U.ctoaster_root, 'data', 'main')
for s in ['atm', 'ocn', 'sed']:
    shutil.copy(os.path.join(srcmaindatadir, 'tracer_define.' + s),
                jobmaindatadir)

if running_from_gui: print('OK')
