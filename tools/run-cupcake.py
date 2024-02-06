from __future__ import print_function
import os, os.path, sys
import subprocess as sp

import utils as U


# cTOASTER configuration

if not U.read_ctoaster_config():
    sys.exit("cTOASTER not set up: run the setup.py script!")


# Command line arguments.

def usage():
    print("""
Usage: run-cupcake <base-config> <config-dir> <run-id> <run-length> [<restart>]
""")
    sys.exit()

if len(sys.argv) != 5 and len(sys.argv) != 6: usage()
base_config = sys.argv[1]
config_dir = sys.argv[2]
run_id = sys.argv[3]
run_length = sys.argv[4]
restart = None
if len(sys.argv) == 6: restart = sys.argv[5]


# Configure job.

os.chdir(U.ctoaster_root)
user_config = os.path.join(config_dir, run_id)
cmd = [os.path.join(os.curdir, 'new-job'),
       '-O',
       '-b', base_config,
       '-u', user_config,
       run_id, run_length]
if restart: cmd += ['-r', restart]

print('Configuring job...')
print('')
if sp.check_call(cmd) != 0: sys.exit('Failed to configure job')


# Build and run job.

os.chdir(os.path.join(U.ctoaster_jobs, run_id))
print('')
print('')
print('Building and running job...')
print('')
cmd = [os.path.join(os.curdir, 'go'), 'run']
if sp.check_call(cmd) != 0: sys.exit('Failed to build and run job')

print('RUN COMPLETE!')
