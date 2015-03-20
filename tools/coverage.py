#!/usr/bin/env python2

from __future__ import print_function
import os, os.path, sys, errno, shutil, glob, re
import optparse
import subprocess as sp
import datetime as dt
import ctypes as ct
import platform as plat

import utils as U
import config_utils as C


# GENIE configuration

if not U.read_cgenie_config():
    sys.exit('GENIE not set up: run the setup-cgenie script!')
scons = [os.path.join(U.cgenie_root, 'tools', 'scons', 'scons.py')]
if plat.system() == 'Windows': scons = ['python'] + scons
test_version = U.cgenie_version


#----------------------------------------------------------------------
#
#   RUN COVERAGE TESTS
#

# Run a single coverage test job.

def do_run(t, rdir, logfp):
    os.chdir(U.cgenie_root)
    print('Running test "' + t + '"')
    print('Running test "' + t + '"', file=logfp)
    t = t.replace('\\', '\\\\')

    test_dir = os.path.join(U.cgenie_test, t)
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
    if test_version != U.cgenie_version:
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
    if sp.check_call(cmd, stdout=logfp, stderr=logfp) != 0:
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
    if sp.check_call(cmd, stdout=logfp, stderr=logfp) != 0:
        sys.exit('Failed to build and run test job')


# Calculate transitive closure of restart dependency graph.

def restart_map(tests):
    res = { }
    check = set(tests)
    while len(check) != 0:
        for t in check:
            r = None
            ifile = os.path.join(U.cgenie_test, t, 'test_info')
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
    rdir = os.path.join(U.cgenie_jobs, 'coverage-' + label)
    print('Coverage output in ' + rdir + '\n')
    os.makedirs(rdir)

    # Deal with "ALL" case.
    if tests == ['ALL']:
        tests = [os.path.relpath(p, U.cgenie_test)
                 for p in glob.glob(os.path.join(U.cgenie_test, '*'))
                 if os.path.isdir(p)]

    # Determine leaf tests.
    ltests = []
    for tin in tests:
        for d, ds, fs in os.walk(os.path.join(U.cgenie_test, tin)):
            if os.path.exists(os.path.join(d, 'test_info')):
                ltests.append(os.path.relpath(d, U.cgenie_test))

    # Determine restart prerequisites for tests in list.
    restarts = restart_map(ltests)

    # Determine suitable execution order for tests from topological
    # sort of restart dependency graph.
    rtests = topological_sort(restarts)
    if rtests == []: sys.exit('No tests specified!')

    # Clear coverage builds, run all jobs and collect gcov data.
    clear_gcov()
    with open(os.path.join(rdir, 'coverage.log'), 'w') as logfp:
        for t in rtests: do_run(t, rdir, logfp)
        collect_gcov(rdir, logfp)
        merge_gcov(rdir, logfp)


# Remove all pre-existing coverage builds.

def clear_gcov():
    if os.path.exists(os.path.join(U.cgenie_jobs, 'MODELS')):
        os.chdir(os.path.join(U.cgenie_jobs, 'MODELS'))
        for cov in glob.iglob('*/*/*/coverage'):
            shutil.rmtree(cov, ignore_errors=True)
            if glob.glob(os.path.join(cov, os.pardir, '*')) == []:
                os.removedirs(os.path.abspath(os.path.join(cov, os.pardir)))


# Collect coverage results.

def collect_gcov(rdir, logfp):
    os.makedirs(os.path.join(rdir, 'gcov-results'))

    # Walk all "coverage" directories under ~/cgenie-jobs/MODELS
    os.chdir(os.path.join(U.cgenie_jobs, 'MODELS'))
    covs = glob.glob('*/*/*/coverage')
    icov = 0
    for cov in covs:
        icov += 1
        os.chdir(os.path.join(U.cgenie_jobs, 'MODELS', cov, 'build'))

        # Run gcov on all object files.
        objs = glob.glob('*.o') + glob.glob('*/*.o')
        for o in objs:
            ps = o.split('/')
            if len(ps) == 2:
                d = ps[0]
                f = ps[1]
            else:
                d = None
                f = ps[0]
            if d: os.chdir(d)
            if sp.check_call(['gcov', f], stdout=logfp, stderr=logfp) != 0:
                sys.exit('Failed processing ' + o + ' in ' + cov)
            gc = os.path.splitext(f)[0] + '.f90.gcov'
            gcto = os.path.join(rdir, 'gcov-results', gc + '-' + str(icov))
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
        print('f90:', gc)
        for f in glob.iglob(f90 + '.gcov-*'):
            print('  f:', f)
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
            for i in range(len(lines)):
                print(counts[i] + ':' + lines[i], file=fpout)


# Command line arguments.

def usage():
    print("""
Usage: coverage [-v <version>] <test-name>...
""")
    sys.exit()

if len(sys.argv) < 2: usage()
if sys.argv[1] == '-v':
    if len(sys.argv) < 3: usage()
    test_version = sys.argv[2]
    if test_version not in U.available_versions():
        sys.exit('Model version "' + test_version + '" does not exist')
    tests = sys.argv[3:]
else:
    tests = sys.argv[1:]
if 'ALL' in tests and len(tests) > 1:
    sys.exit('Must specify either "ALL" or a list of tests, not both')
run_coverage(tests)
