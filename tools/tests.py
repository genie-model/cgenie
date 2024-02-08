import os, sys, errno, shutil, glob, re
import argparse  # Replacing optparse for Python 3
import subprocess as sp
import datetime as dt
import ctypes as ct
import platform as plat

import utils as U
import config_utils as C

# GENIE configuration
if not U.read_cgenie_config():
    sys.exit('GENIE not set up: run the setup-cgenie script!')

# Adjusting scons command for cross-platform compatibility
scons_command = 'scons'
if plat.system() == 'Windows':
    scons_command = ['python', os.path.join(U.cgenie_root, 'tools', 'scons', 'scons.py')]
else:
    scons_command = [scons_command]

nccompare = os.path.join(U.cgenie_root, 'build', 'nccompare.exe')
test_version = U.cgenie_version



#----------------------------------------------------------------------
#
#   LIST ALL EXISTING TESTS
#

def list(list_base):
    tests = []
    for d, ds, fs in os.walk(U.cgenie_test):
        if os.path.exists(os.path.join(d, 'test_info')):
            tests.append(os.path.relpath(d, U.cgenie_test))
    for t in sorted(tests):
        if not list_base or list_base and t.startswith(list_base):
            print(t)


#----------------------------------------------------------------------
#
#   ADD A TEST
#

def biogemish_defaults(subd):
    ncs = glob.glob(os.path.join(subd, '*.nc'))
    ncs = [f for f in ncs if f.find('restart') < 0]
    ress = glob.glob(os.path.join(subd, '*.res'))
    return ncs + ress

def nc_defaults(subd):
    ncs = glob.glob(os.path.join(subd, '*.nc'))
    return [f for f in ncs if f.find('restart') < 0]

select_defaults = { 'biogem': biogemish_defaults,
                    'rokgem': biogemish_defaults,
                    'sedgem': biogemish_defaults }

def add_test(test_job, test_name, restart):
    def has_job_output(jdir):
        for d, ds, fs in os.walk(os.path.join(jdir, 'output')):
            if fs != []: return True
        return False

    # Check for existence of required jobs, tests and directories.
    job_dir = os.path.join(U.cgenie_jobs, test_job)
    if not has_job_output(job_dir):
        sys.exit('Need to run job "' + test_job +
                 '" before adding it as a test')
    test_dir = os.path.join(U.cgenie_test, test_name)
    if not os.path.exists(job_dir):
        sys.exit('Job "' + test_job + '" does not exist')
    if os.path.exists(test_dir): sys.exit('Test already exists!')
    if restart:
        restart_test_dir = os.path.join(U.cgenie_test, restart)
        if not os.path.exists(restart_test_dir):
            sys.exit('Restart test "' + restart + '" does not exist')

    # Set up test directory and copy configuration files.
    os.makedirs(test_dir)
    shutil.copy(os.path.join(job_dir, 'config', 'config'),
                os.path.join(test_dir, 'test_info'))
    if restart:
        with open(os.path.join(test_dir, 'test_info'), 'a') as fp:
            print(f'restart_from: {restart}', file=fp)
    for c in ['full_config', 'base_config', 'user_config']:
        if os.path.exists(os.path.join(job_dir, 'config', c)):
            shutil.copy(os.path.join(job_dir, 'config', c), test_dir)

    # Ask user which output files to use for comparison and copy them
    # to the test "knowngood" directory.
    print('Selecting output files for test comparison:')
    srcdir = os.path.join(job_dir, 'output')
    dstdir = os.path.join(test_dir, 'knowngood')
    os.chdir(srcdir)
    for subd in glob.iglob('*'):
        fs = []
        if subd in select_defaults:
            fs = select_defaults[subd](subd)
        else:
            fs = nc_defaults(subd)
        print(f'  {subd} defaults: {"NONE" if not fs else " ".join(map(os.path.basename, fs))}')
        accept_defaults = raw_input('    Accept defaults? [Yn]: ').strip()
        if accept_defaults != '' and accept_defaults.lower() != 'y':
            fs = []
            for f in glob.iglob(os.path.join(subd, '*')):
                yn = raw_input('    ' + f + ' [yN]: ').strip()
                if yn.lower() == 'y': fs.append(f)
        for f in fs:
            src = os.path.join(srcdir, f)
            dst = os.path.join(dstdir, f)
            if not os.path.exists(os.path.dirname(dst)):
                os.makedirs(os.path.dirname(dst))
            shutil.copyfile(src, dst)


    # Copy restart files if they exist and we aren't restarting from
    # another test.
    if not restart and os.path.exists(os.path.join(job_dir, 'restart')):
        shutil.copytree(os.path.join(job_dir, 'restart'),
                        os.path.join(test_dir, 'restart'))


#----------------------------------------------------------------------
#
#   RUN TESTS
#

# Comparison tolerances used for comparing floating point numbers.

abstol = 6.0E-15
reltol = 35


# Make sure that the nccompare tool is available.
def ensure_nccompare():
    if os.path.exists(nccompare): return
    cmd = ['scons', '-C', U.cgenie_root, os.path.join('build', 'nccompare.exe')]
    result = sp.run(cmd, stdout=sp.DEVNULL, stderr=sp.DEVNULL, text=True)
    if result.returncode != 0:
        sys.exit('Could not build nccompare.exe program')



# Compare NetCDF files.
def compare_nc(f1, f2, logfp):
    cmd = [nccompare, '-v', '-a', str(abstol), '-r', str(reltol), f1, f2]
    result = sp.run(cmd, stdout=logfp, stderr=logfp, text=True)
    return result.returncode



# "Dawson" float comparison.

def float_compare(x, y):
    def transfer(x):
        cx = ct.cast(ct.pointer(ct.c_float(x)), ct.POINTER(ct.c_int32))
        return cx.contents.value
    return abs((0x80000000 - transfer(x)) - (0x80000000 - transfer(y)))


# Compare ASCII files.

fp_re_str = '[+-]?(\d+(\.\d*)?|\.\d+)([eE][+-]?\d+)?'
fpline_re_str = '^(' + fp_re_str + ')((\s*,\s*|\s+)' + fp_re_str + ')*$'
fpline_re = re.compile(fpline_re_str)

def compare_ascii(f1, f2, logfp):
    with open(f1, encoding='utf-8') as fp1, open(f2, encoding='utf-8') as fp2:
        for l1, l2 in zip(fp1, fp2):
            l1, l2 = l1.strip(), l2.strip()
            if l1 == l2:
                continue
            m1, m2 = fpline_re.match(l1), fpline_re.match(l2)
            if not m1 or not m2 or len(m1.groups()) != len(m2.groups()):
                break
            xs1, xs2 = map(float, re.split(r',|\s+', l1)), map(float, re.split(r',|\s+', l2))
            max_absdiff = max(abs(x - y) for x, y in zip(xs1, xs2))
            max_reldiff = max(float_compare(x, y) for x, y in zip(xs1, xs2))
            if max_absdiff > abstol and max_reldiff >= reltol:
                break
        else:
            # Check for file length differences after loop completion
            if not all((line == '' for line in fp1)) or not all((line == '' for line in fp2)):
                print(f'Files {f1} and {f2} differ in length', file=logfp)
                return True
            return False
        print(f'Files {f1} and {f2} are different', file=logfp)
        return True



# Compare files: might be NetCDF, might be ASCII.

def file_compare(f1, f2, logfp):
    if not os.path.exists(f1):
        print(f'File missing: {f1}')
        print(f'File missing: {f1}', file=logfp)
        return True
    if not os.path.exists(f2):
        print(f'File missing: {f2}')
        print(f'File missing: {f2}', file=logfp)
        return True
    with open(f1, encoding="latin-1") as tstfp: chk = tstfp.read(4)
    if chk == 'CDF\x01':
        return compare_nc(f1, f2, logfp)
    else:
        return compare_ascii(f1, f2, logfp)


# Run a single test job and do results comparison: note that this uses
# *exactly* the same mechanisms that one would use to run these jobs
# by hand!

def do_run(t, rdir, logfp, i, n):
    os.chdir(U.cgenie_root)
    print('Running test "' + t + '" [' + str(i) + '/' + str(n) + ']')
    print('Running test "' + t + '"', file=logfp)
    t = t.replace('\\', '\\\\')

    test_dir = os.path.join(U.cgenie_test, t)
    if plat.system() == 'Windows':
        cmd = ['cmd', '/c', os.path.join(os.curdir, 'new-job.bat')]
    else:
        cmd = [os.path.join(os.curdir, 'new-job')]

    # Set up test job configuration option for "new-job".
    cmd += ['-t', t]

    # Set up other options for "new-job".
    cmd += ['-j', rdir]

    # Do job configuration, copying restart files if necessary.
    print('  Configuring job...')
    print('  Configuring job...', file=logfp)
    logfp.flush()
    result = sp.run(cmd, stdout=logfp, stderr=logfp, text=True)
    if result.returncode != 0:
        sys.exit('Failed to configure test job')


    ###print(cmd)

    # Build and run job.
    os.chdir(os.path.join(rdir, t))
    print('  Building and running job...')
    print('  Building and running job...', file=logfp)
    logfp.flush()
    if plat.system() == 'Windows':
        go = ['cmd', '/c', os.path.join(os.curdir, 'go.bat')]
    else:
        go = [os.path.join(os.curdir, 'go')]
    cmd = go + ['run', '--no-progress']
    result = sp.run(cmd, stdout=logfp, stderr=logfp, text=True)
    if result.returncode != 0:
        sys.exit('Failed to build and run test job')


    # Compare results, walking over all known good files in the test
    # directory.
    print('  Checking results...')
    print('  Checking results...', file=logfp)
    logfp.flush()
    kg = os.path.join(test_dir, 'knowngood')
    passed = True
    for d, ds, fs in os.walk(kg):
        for f in fs:
            fullf = os.path.join(d, f)
            relname = os.path.relpath(fullf, kg)
            testf = os.path.join(rdir, t, 'output', relname)
            if file_compare(fullf, testf, logfp):
                passed = False
                print(f'    FAILED: {relname}')
                print(f'    FAILED: {relname}', file=logfp)
            else:
                print(f'    OK: {relname}')
                print(f'    OK: {relname}', file=logfp)
    return passed


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


# Run a list of tests.

def run_tests(tests):
    ensure_nccompare()

    # Set up test jobs directory.
    label = dt.datetime.today().strftime('%Y%m%d-%H%M%S')
    rdir = os.path.join(U.cgenie_jobs, 'test-' + label)
    print('Test output in ' + rdir + '\n')
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

    summ = { }
    with open(os.path.join(rdir, 'test.log'), 'w') as logfp:
        i = 1
        for t in rtests:
            summ[t] = do_run(t, rdir, logfp, i, len(rtests))
            i += 1
    if len(summ.keys()) == 0:
        print('NO TESTS RUN')
    else:
        fmtlen = max(map(len, summ.keys())) + 3
        print('\nSUMMARY:\n')
        with open(os.path.join(rdir, 'summary.txt'), 'w') as sumfp:
            for t in sorted(summ.keys()):
                msg = t.ljust(fmtlen) + ('OK' if summ[t] else 'FAILED')
                print(msg)
                print(msg, file=sumfp)


# Command line arguments.

def usage():
    print("""
Usage: tests <command>

Commands:
  list [<base>]                      List available tests
  run [-v <version>] <test-name>...  Run test or group of tests
  add <job>                          Add pre-existing job as test
  add <test-name>=<job>              Add job as test with given name
        [-r <test>]                  Restart from a pre-existing test
""")
    sys.exit()

if len(sys.argv) < 2: usage()
action = sys.argv[1]
if action == 'list':
    list_base = None
    if len(sys.argv) == 3: list_base = sys.argv[2]
    elif len(sys.argv) != 2: usage()
    list(list_base)
elif action == 'add':
    if len(sys.argv) < 3: usage()
    job = sys.argv[2]
    if '=' in job:
        name, job = job.split('=')
    else:
        name = job
    restart = None
    if len(sys.argv) == 5 and sys.argv[3] == '-r':
        restart = sys.argv[4]
    elif len(sys.argv) != 3: usage()
    add_test(job, name, restart)
elif action == 'run':
    if len(sys.argv) < 3: usage()
    if sys.argv[2] == '-v':
        if len(sys.argv) < 4: usage()
        test_version = sys.argv[3]
        if test_version not in U.available_versions():
            sys.exit('Model version "' + test_version + '" does not exist')
        tests = sys.argv[4:]
    else:
        tests = sys.argv[2:]
    if 'ALL' in tests and len(tests) > 1:
        sys.exit('Must specify either "ALL" or a list of tests, not both')
    run_tests(tests)
else: usage()