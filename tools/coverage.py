import os, os.path, sys, errno, shutil, glob, re
import argparse 
import subprocess as sp
import datetime as dt
import ctypes as ct
import platform as plat

import utils as U
import config_utils as C


# GENIE configuration

if not U.read_ctoaster_config():
    sys.exit('GENIE not set up: run the setup-ctoaster script!')
scons = [os.path.join(U.ctoaster_root, 'tools', 'scons', 'scons.py')]
if plat.system() == 'Windows': scons = ['python'] + scons
test_version = U.ctoaster_version


#----------------------------------------------------------------------
#
#   RUN COVERAGE TESTS
#

# Run a single coverage test job.

def do_run(t, rdir, logfp):
    os.chdir(U.ctoaster_root)
    print(f'Running test "{t}"')
    print(f'Running test "{t}"', file=logfp)

    t = t.replace('\\', '\\\\')

    test_dir = os.path.join(U.ctoaster_test, t)
    if plat.system() == 'Windows':
        cmd = ['cmd', '/c', os.path.join(os.curdir, 'new-job.bat')]
    else:
        cmd = [os.path.join(os.curdir, 'new-job')]

    # Read test information file.
    config = { }
    with open(os.path.join(test_dir, 'test_info')) as fp:
        for line in fp:
            k, v = line.strip().split(':')
            config[k.strip()] = v.strip()
            have_full = os.path.exists(os.path.join(test_dir, 'full_config'))
            have_base = os.path.exists(os.path.join(test_dir, 'base_config'))
            have_user = os.path.exists(os.path.join(test_dir, 'user_config'))

    # Set up configuration file options for "new-job".
    if have_full:
        cmd += ['-c', os.path.join(test_dir, 'full_config')]
    elif have_base and have_user:
        cmd += ['-b', os.path.join(test_dir, 'base_config')]
        cmd += ['-u', os.path.join(test_dir, 'user_config')]
    else:
        sys.exit('Test "' + t + '" configured incorrectly!')

    # Set up other options for "new-job".
    cmd += ['-j', rdir]
    if 't100' in config and config['t100'] == 'True':
        cmd += ['--t100']
    if test_version != U.ctoaster_version:
        cmd += ['-v', test_version]
    if os.path.exists(os.path.join(test_dir, 'restart')):
        cmd += ['-r', os.path.join(test_dir, 'restart')]
    elif 'restart_from' in config:
        rjobdir = config['restart_from']
        if plat.system() == 'Windows': rjobdir = rjobdir.replace('/', '\\')
        cmd += ['-r', rjobdir]
    cmd += [t, config['run_length']]

    # Do job configuration, copying restart files if necessary.
    print('  Configuring job...')
    print('  Configuring job...', file=logfp)
    logfp.flush()
    if sp.run(cmd, stdout=logfp, stderr=logfp).returncode != 0:
        sys.exit('Failed to configure test job')

    # Build and run job.
    os.chdir(os.path.join(rdir, t))
    print('  Building and running job...')
    print('  Building and running job...', file=logfp)
    logfp.flush()
    if plat.system() == 'Windows':
        go = ['cmd', '/c', os.path.join(os.curdir, 'go.bat')]
    else:
        go = [os.path.join(os.curdir, 'go')]
    cmd = go + ['run', 'coverage', '--no-progress']
    if sp.run(cmd, stdout=logfp, stderr=logfp).returncode != 0:
        sys.exit('Failed to build and run test job')


# Calculate transitive closure of restart dependency graph.

def restart_map(tests):
    res = { }
    check = set(tests)
    while len(check) != 0:
        for t in check:
            r = None
            ifile = os.path.join(U.ctoaster_test, t, 'test_info')
            if not os.path.exists(ifile):
                sys.exit('Test "' + t + '" does not exist')
            with open(ifile) as fp:
                for line in fp:
                    if line.startswith('restart_from'):
                        r = line.split(':')[1].strip()
                        if plat.system() == 'Windows':
                            r = r.replace('/', '\\')
            res[t] = r
        check = set(res.values()) - set(res.keys()) - set([None])
    return res


# Topological sort of restart dependency graph.

def topological_sort(g):
    res = []
    while len(g) > 0:
        tails = [k for k in g.keys() if not g[k]]
        res = res + tails
        for t in tails: g.pop(t)
        for k in g.keys():
            if g[k] in tails: g[k] = None
    return res


# Run a list of coverage tests.

def run_coverage(tests):
    # Set up coverage jobs directory.
    label = dt.datetime.today().strftime('%Y%m%d-%H%M%S')
    rdir = os.path.join(U.ctoaster_jobs, 'coverage-' + label)
    print(f'Coverage output in {rdir}\n')

    os.makedirs(rdir)

    # Deal with "ALL" case.
    if tests == ['ALL']:
        tests = [os.path.relpath(p, U.ctoaster_test)
                 for p in glob.glob(os.path.join(U.ctoaster_test, '*'))
                 if os.path.isdir(p)]

    # Determine leaf tests.
    ltests = []
    for tin in tests:
        for d, ds, fs in os.walk(os.path.join(U.ctoaster_test, tin)):
            if os.path.exists(os.path.join(d, 'test_info')):
                ltests.append(os.path.relpath(d, U.ctoaster_test))

    # Determine restart prerequisites for tests in list.
    restarts = restart_map(ltests)

    # Determine suitable execution order for tests from topological
    # sort of restart dependency graph.
    rtests = topological_sort(restarts)
    if not rtests: sys.exit('No tests specified!')
    # Clear coverage builds, run all jobs and collect gcov data.
    clear_gcov()
    with open(os.path.join(rdir, 'coverage.log'), 'w') as logfp:
        for t in rtests: do_run(t, rdir, logfp)
        collect_gcov(rdir, logfp)
        merge_gcov(rdir, logfp)


# Remove all pre-existing coverage builds.

def clear_gcov():
    if os.path.exists(os.path.join(U.ctoaster_jobs, 'MODELS')):
        os.chdir(os.path.join(U.ctoaster_jobs, 'MODELS'))
        for cov in glob.iglob('*/*/*/coverage'):
            shutil.rmtree(cov, ignore_errors=True)
            parent_dir = os.path.abspath(os.path.join(cov, os.pardir))
            if not os.listdir(parent_dir):
                os.removedirs(parent_dir)



# Collect coverage results.

def collect_gcov(rdir, logfp):
    os.makedirs(os.path.join(rdir, 'gcov-results'))

    # Walk all "coverage" directories under ~/ctoaster-jobs/MODELS
    os.chdir(os.path.join(U.ctoaster_jobs, 'MODELS'))
    covs = glob.glob('*/*/*/coverage')
    icov = 0
    for cov in covs:
        icov += 1
        os.chdir(os.path.join(U.ctoaster_jobs, 'MODELS', cov, 'build'))

        # Run gcov on all object files.
        objs = glob.glob('**/*.o', recursive=True)
        for o in objs:
            ps = o.split('/')
            if len(ps) == 2:
                d = ps[0]
                f = ps[1]
            else:
                d = None
                f = ps[0]
            if d: os.chdir(d)
            try:
                sp.run(['gcov', f], check=True, stdout=logfp, stderr=logfp)
            except sp.CalledProcessError:
                sys.exit(f'Failed processing {o} in {cov}')

            gc = f"{os.path.splitext(f)[0]}.f90.gcov"
            gcto = f"{gc}-{icov}"
            if d: gcto = f"{d}-{gcto}"
            gcto = os.path.join(rdir, 'gcov-results', gcto)
            if os.path.exists(gc): shutil.move(gc, gcto)
            if d: os.chdir(os.pardir)



# Merge coverage results.

def count_max(c1, c2):
    if c1.strip() == '-':                       return c1
    elif c1.strip() == '#####':                 return c2
    elif c2.strip() == '#####':                 return c1
    elif float(c1.strip()) > float(c2.strip()): return c1
    else:                                       return c2

def merge_gcov(rdir, logfp):
    os.chdir(os.path.join(rdir, 'gcov-results'))
    for gc in glob.iglob('*.f90.gcov-1'):
        f90 = gc.replace('.gcov-1', '')
        lines = []
        counts = []
        first = True
        for f in glob.iglob(f90 + '.gcov-*'):
            i = 0
            with open(f) as fpin:
                for l in fpin:
                    c, v = l.split(':', 1)
                    if first:
                        lines.append(v.rstrip())
                        counts.append(c)
                    else:
                        counts[i] = count_max(counts[i], c)
                    i += 1
            first = False
        with open(f90 + '.GCOV', 'w') as fpout:
            for count, line in zip(counts, lines):
                print(f"{count}:{line}", file=fpout)
    for gc in glob.iglob('*.f90.gcov-*'): os.remove(gc)


# Command line arguments.

def usage():
    print("""
Usage: coverage [-v <version>] <test-name>...
""")
    sys.exit()

# Setup argparse for command line arguments
parser = argparse.ArgumentParser(description='Run coverage on specified tests.')
parser.add_argument('-v', '--version', help='Specify model version', default=None)
parser.add_argument('tests', nargs='+', help='Test names or "ALL"')

args = parser.parse_args()

# Validate model version if specified
if args.version and args.version not in U.available_versions():
    sys.exit(f'Model version "{args.version}" does not exist')

# Validate tests argument
if 'ALL' in args.tests and len(args.tests) > 1:
    sys.exit('Must specify either "ALL" or a list of tests, not both')

# Run coverage with parsed arguments, without passing test_version
run_coverage(args.tests)

