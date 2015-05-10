#!/usr/bin/env python2

from __future__ import print_function
import os, os.path

import utils as U


def job_split(jfull):
    d, j = os.path.split(jfull)
    ds = []
    while d:
        d, d1 = os.path.split(d)
        ds.append(d1)
    ds.reverse()
    return (ds, j)
