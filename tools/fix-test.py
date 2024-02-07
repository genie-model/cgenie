#!/usr/bin/env python2

from __future__ import print_function
import sys, os.path, datetime

if len(sys.argv) != 2: sys.exit('Usage: fix-test.py <infile>')

with open(sys.argv[1]) as fp:
    for line in fp:
        ss = line.split(':')
        k = ss[0].strip()
        v = ':'.join(ss[1:]).strip()
        if k == 'full_config':
            print('full_config_dir: /home/iross/ctoaster-data/full-configs')
            print('full_config:', os.path.basename(v).replace('.config', ''))
        elif k == 'base_config':
            print('base_config_dir: /home/iross/ctoaster-data/base-configs')
            print('base_config:', os.path.basename(v).replace('.config', ''))
        elif k == 'user_config':
            print('user_config_dir: /home/iross/ctoaster-data/user-configs')
            print('user_config:', os.path.basename(v).replace('.config', ''))
        else:
            print(k + ': ' + v)
    print('config_date:', str(datetime.datetime.today()))
