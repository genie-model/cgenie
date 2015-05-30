import os.path
from tailer import *

class TimeSeriesFile:
    def __init__(self, app, p, cb):
        self.app = app
        self.time = []
        self.data = { }
        self.vars = ()
        self.cb = cb
        self.tailer = None
        if os.path.exists(p):
            self.time = []
            self.data = { }
            self.vars = ()
            self.tailer = Tailer(app, p)
            self.tailer.start(self.add_output)

    def add_output(self, t):
        tnew = []
        dnew = []
        for l in t.splitlines():
            if self.vars == ():
                header = l.strip().lstrip('%').strip()
                header = map(lambda s: s.strip(), header.split(' / '))
                self.vars = tuple(header[1:])
                for v in self.vars: self.data[v] = []
            else:
                l = l.strip().split()
                self.time.append(l[0])
                for i in range(1, len(l)):
                    self.data[self.vars[i-1]].append(l[i])
                tnew.append(l[0])
                dnew.append(l[1:])
        self.cb(tnew, dnew)
