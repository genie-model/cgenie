import os
import subprocess as sp
import argparse
import utils as U

# cTOASTER configuration
if not U.read_ctoaster_config():
    sys.exit("cTOASTER not set up: run the setup.py script!")

# Setup command line arguments using argparse
parser = argparse.ArgumentParser(description='Run a cupcake job with the specified configuration.')
parser.add_argument('base_config', help='Base configuration name')
parser.add_argument('config_dir', help='Directory containing the configuration')
parser.add_argument('run_id', help='Unique identifier for the run')
parser.add_argument('run_length', help='Length of the run')
parser.add_argument('restart', nargs='?', default=None, help='Optional restart file')

args = parser.parse_args()

# Configure job
os.chdir(U.ctoaster_root)
user_config = os.path.join(args.config_dir, args.run_id)
cmd = [
    os.path.join(os.curdir, 'new-job'), '-O',
    '-b', args.base_config,
    '-u', user_config,
    args.run_id, args.run_length
]
if args.restart:
    cmd += ['-r', args.restart]

print('Configuring job...\n')
if sp.check_call(cmd) != 0:
    sys.exit('Failed to configure job')

# Build and run job
os.chdir(os.path.join(U.ctoaster_jobs, args.run_id))
print('\nBuilding and running job...\n')
cmd = [os.path.join(os.curdir, 'go'), 'run']
if sp.check_call(cmd) != 0:
    sys.exit('Failed to build and run job')

print('RUN COMPLETE!')
