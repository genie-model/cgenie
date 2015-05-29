from __future__ import print_function
import json, csv
import errno, os, sys, shutil, glob
import re

import utils as U


# Regex for matching floating point values.

fp_re = '[+-]?(\d+(\.\d*)?|\.\d+)([eE][+-]?\d+)?'


# Read and parse a GENIE configuration file.

def read_config(f, msg):
    # Clean string quotes and comments from parameter value.
    def clean(s):
        if   (s[0] == '"'): return s[1:].partition('"')[0]
        elif (s[0] == "'"): return s[1:].partition("'")[0]
        else:               return s.partition('#')[0].strip()
    try:
        res = { }
        with (open(f)) as fp:
            for line in fp:
                if re.match('^\s*#', line): continue
                m = re.search('([a-zA-Z0-9_]+)=(.*)', line)
                if m: res[m.group(1)] = clean(m.group(2).strip())
            return res
    except IOError as e:
        if e.errno == errno.ENOENT: sys.exit(msg + ' not found: ' + f)
        else: raise


# Merge module flags from base and user configurations.

def merge_flags(dicts):
    res = { }
    for d in dicts:
        for k in d.keys():
            if d[k].lower() == '.true.': v = 1
            else: v = 0
            res[k] = v
    return res


# Module information: mappings between module names, flag names and
# namelist names.

srcdir = None
datadir = None

def set_dirs(src, data):
    global srcdir, datadir
    srcdir = src
    datadir = data

module_info = { }
flagname_to_mod = { }

def load_module_info():
    try:
        with open(os.path.join(srcdir, 'module-info.csv')) as fp:
            for row in csv.reader(fp, skipinitialspace=True):
                if row[0] == '#': next
                flag = ('ma_flag_' + row[1]) if row[1] != 'NONE' else row[1]
                module_info[row[0]] = { 'flag_name': flag, 'prefix': row[2],
                                        'nml_file': row[3], 'nml_name': row[4] }
                flagname_to_mod[flag] = row[0]
    except:
        if not srcdir:
            sys.exit("Internal error: source directory not set!")
        else:
            sys.exit("Couldn't open module info file " +
                     os.path.join(srcdir, 'module-info.csv'))

def module_from_flagname(flagname):
    if not flagname_to_mod: load_module_info()
    return flagname_to_mod[flagname]

def lookup_module(modname):
    if not module_info: load_module_info()
    return module_info[modname]


def extract_defines(maps):
    res = { }
    for m in maps:
        for k, v in m.iteritems():
            if v.startswith('$(DEFINE)'):
                ckv = v[9:].split('=')
                res[ckv[0]] = int(ckv[1])
    return res


def make_coordinates(defs):
    res = { }
    for k, v in defs.iteritems():
        res['ma_dim_' + k] = v
    return res


def timestepping_options(runlen, coords, t100, quiet=False):
    lons = coords['GOLDSTEINNLONS']
    lats = coords['GOLDSTEINNLATS']
    levs = coords['GOLDSTEINNLEVS']

    # Define relative biogeochem time-stepping.

    dbiotbl = [
    #                 t96            t100
    #    lons levs   ntstp dbiostp   ntstp dbiostp
        [ 36,  16,     96,    2,      100,    2 ],
        [ 36,   8,     96,    4,      100,    5 ],
        [ 18,  16,     48,    1,       50,    2 ],
        [ 18,   8,     48,    2,       50,    5 ],
        [ 36,  32,     96,    1,      100,    1 ] ]
    #    ANY-OTHER     96     1       100     1
    nsteps = 100 if t100 else 96
    dbio = 1
    for chk in dbiotbl:
        if lons == chk[0] and levs == chk[1]:
            nsteps = chk[4] if t100 else chk[2]
            dbio = chk[5] if t100 else chk[3]
    if not quiet:
        print("Setting time-stepping [GOLDSTEIN, BIOGEM:GOLDSTEIN]: ",
              nsteps, dbio)

    # Define primary model time step.
    dstp = 3600.0 * 24.0 * 365.25 / 5.0 / nsteps

    res = { }
    # Primary model time step.
    res['ma_genie_timestep'] = dstp

    # Relative time-stepping.
    for k in ['ma_ksic_loop', 'ma_kocn_loop', 'ma_klnd_loop']: res[k] = 5
    for k in ['ma_conv_kocn_katchem', 'ma_conv_kocn_kbiogem',
              'ma_conv_kocn_krokgem']: res[k] = dbio
    for k in ['ma_conv_kocn_ksedgem', 'ma_kgemlite']: res[k] = nsteps

    # BIOGEM run length and SEDGEM sediment age.
    for k in ['bg_par_misc_t_runtime', 'sg_par_misc_t_runtime']: res[k] = runlen

    # Overall GENIE run length.
    for k in ['ma_koverall_total', 'ma_dt_write']: res[k] = runlen * 5 * nsteps

    # npstp: 'Health check' frequency (*)
    # iwstp: Climate model component restart frequency.
    # itstp: 'Time series' frequency (*)
    # ianav: 'Average' frequency (*)
    # nyear: Climate components time-steps per year.
    #     (*) A '+1' in effect disables this feature
    ###===> WHAT DOES "a '+1' in effect disables this feature" MEAN?
    ps = ['ea', 'go', 'gs', 'ents']
    for p in ps: res[p + '_npstp'] = runlen * nsteps
    for p in ps: res[p + '_iwstp'] = runlen * nsteps
    for p in ps: res[p + '_itstp'] = runlen * nsteps + 1
    for p in ps: res[p + '_ianav'] = runlen * nsteps + 1
    for k in ['ea_nyear', 'go_nyear', 'gs_nyear']: res[k] = nsteps

    return res


def restart_options(restart):
    res = { }

    # Set climate model re-start file details.

    # Set default flags.
    # Set NetCDF restart saving flag.
    for k in ['ea_netout', 'go_netout', 'gs_netout']: res[k] = 'n'
    # Set ASCII restart output flag.
    for k in ['ea_ascout', 'go_ascout', 'gs_ascout']: res[k] = 'y'
    # Set ASCII restart number (i.e., output file string).
    for k in ['ea_lout', 'go_lout', 'gs_lout', 'ents_out_name']: res[k] = 'rst'
    res['ents_restart_file'] = 'rst.sland'

    # Configure use of restart.
    # -----------------------------
    # Set continuing/new run flags
    # => set restart input flags
    # => disable netCDF restart input flag
    # => set restart input number
    # => copy restart files to data directory
    if restart:
        for p in ['ea', 'go', 'gs', 'ents']:
            res[p + '_ans'] = 'c'
            res[p + '_netin'] = 'n'
        for p in ['ac', 'bg', 'sg', 'rg']:
            res[p + '_ctrl_continuing'] = '.TRUE.'
        for k in ['ea_lin', 'go_lin', 'gs_lin']: res[k] = 'rst.1'
        res['ea_rstdir_name'] = 'restart/embm'
        res['go_rstdir_name'] = 'restart/goldstein'
        res['gs_rstdir_name'] = 'restart/goldsteinseaice'
        res['ents_outdir_name'] = 'output/ents'
        res['ents_dirnetout'] = 'restart/ents'
        res['ents_rstdir_name'] = 'restart/ents'
        res['ac_par_rstdir_name'] = 'restart/atchem'
        res['bg_par_rstdir_name'] = 'restart/biogem'
        res['sg_par_rstdir_name'] = 'restart/sedgem'
        res['rg_par_rstdir_name'] = 'restart/rokgem'
    else:
        for p in ['ea', 'go', 'gs', 'ents']: res[p + '_ans'] = 'n'
        for p in ['ac', 'bg', 'sg', 'rg']:
            res[p + '_ctrl_continuing'] = '.FALSE.'

    # Set NetCDF format biogeochem restart files.
    for p in ['ac', 'bg', 'sg']: res[p + '_ctrl_ncrst'] = '.TRUE.'

    # Over-ride defaults.
    res['bg_ctrl_force_oldformat'] = '.FALSE.'

    return res


def is_bool(x):
    return str(x).lower() == '.true.' or str(x).lower() == '.false.'


class Namelist:
    """Fortran namelists"""
    def __init__(self, fp):
        self.entries = { }
        self.name = ''
        mode = 'start'
        for line in fp:
            line = line.strip()
            if mode == 'start':
                if line.startswith('&'):
                    self.name = line[1:].strip()
                    mode = 'main'
            else:
                if line.startswith('&'): mode = 'done'
                else:
                    if line.endswith(','): line = line[:-1]
                    kv = line.split('=')
                    self.entries[kv[0]] = kv[1].strip('"\'')

    def formatValue(self, v):
        if v == '.true.' or v == '.TRUE.': return '.TRUE.'
        if v == '.false.' or v == '.FALSE.': return '.FALSE.'
        if re.match('^' + fp_re + '$', v): return v
        return '"' + v + '"'

    def write(self, fp):
        print('&' + self.name, file=fp)
        for k in sorted(self.entries.keys()):
            v = self.entries[k]
            print(' ' + k + '=' + self.formatValue(str(v)) + ',', file=fp)
        print('&END', file=fp)

    def merge(self, prefix, maps):
        """Merge configuration data into default namelist.  Deals with
           stripping model-dependent prefix and parameter arrays."""
        plen = len(prefix)
        for m in maps:
            for k in m.keys():
                if k[0:plen] != prefix: continue
                rk = k[plen+1:]
                s = re.search('_(\d+)$', rk)
                if s: rk = rk.rstrip('_0123456789') + '(' + s.group(1) + ')'
                if (rk in self.entries):
                    current = self.entries[rk]
                    new = m[k]
                    # Make sure that boolean values from default
                    # namelists don't get overwritten by integer flag
                    # values.
                    self.entries[rk] = new
                    # if (is_bool(current) and not is_bool(new)):
                    #     if re.match('\d+', new):
                    #         if int(new) == 0: self.entries[rk] = '.FALSE.'
                    #         else: self.entries[rk] = '.TRUE.'
                    #     else: self.entries[rk] = '.FALSE.'
                    # else: self.entries[rk] = new


# Model data file setup.  This is kind of horrible.  We really want to
# pick up all the data files used in a job to put them into the job
# directory so that jobs can be self-contained and reproducible.
# However, it's often the case that what appears in the namelists and
# configuration files *isn't* the complete filename of whatever data
# files are used, but just a *part* of the filename.  In order to make
# sure that we get all the data files that we need (plus possibly some
# extra unused ones, but that's pretty unavoidable), for each namelist
# we do the following:
#
# 1. Extract all non-numeric, non-boolean parameters (i.e. all the
#    character string parameters).
# 2. Filter out some obvious non-file values ("n", "y", the module
#    name).
# 3. For each remaining parameter value, look in the module data
#    directory for a file with that name.  If there's an exact match,
#    copy that file to the relevant job data directory.
# 4. For parameters for which there is exact file match, copy all
#    files whose names contain the parameter value to the relevant job
#    data directory.  Doing this means that we'll almost certainly
#    pick up some unused files, but we should get everything that we
#    need.

def copy_data_files(m, nml, outdir, extras):
    # Extract and filter parameter values.
    def check_data_item(s):
        if not isinstance(s, str): return False
        if s.lower() in ['.true.', '.false.', 'n', 'y', m]: return False
        if re.match(fp_re, s): return False
        if s == 'input/' + m: return False
        for t in ['output/', 'restart/', '/']:
            if s.startswith(t): return False
        return True
    cands = [os.path.basename(f)
             for f in nml.entries.values() if check_data_item(f)]

    # Add per-module 'specials'.
    if extras: cands += extras

    # Look for exact file matches in module data directory.
    checkdir = os.path.join(U.cgenie_root, 'data', m)
    def exact(f):
        try:
            shutil.copy(os.path.join(checkdir, f), outdir)
            return True
        except: return False
    cands = [f for f in cands if not exact(f)]

    # Look for exact file matches in forcings directory.
    checkdir = os.path.join(U.cgenie_data, 'forcings')
    def forcing(f):
        try:
            shutil.copytree(os.path.join(checkdir, f), os.path.join(outdir, f))
            return True
        except: return False
    cands = [f for f in cands if not forcing(f)]

    # Look for partial matches.
    checkdir = os.path.join(U.cgenie_root, 'data', m)
    def partial(f):
        ret = False
        try:
            for match in glob.iglob(os.path.join(checkdir, '*' + f + '*')):
                shutil.copy(match, outdir)
                ret = True
            return ret
        except: return ret
    cands = [f for f in cands if not partial(f)]


# Copy restart files: if restarting from an old cGENIE job, assume
# that the job is in ~/cgenie_output.

def copy_restart_files(m, nml, outdir, restart_path):
    indir = os.path.join(restart_path, m)
    fs = glob.glob(os.path.join(indir, '*rst*'))
    fs += glob.glob(os.path.join(indir, '*restart*'))
    if os.path.exists(os.path.join(indir, 'sedcore.nc')):
        fs += [os.path.join(indir, 'sedcore.nc')]
    for f in fs: shutil.copy(f, outdir)
