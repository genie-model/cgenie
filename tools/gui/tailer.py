import os, os.path

class Tailer:
    def __init__(self, app, fname):
        self.app = app
        self.fname = fname
        self.pos = 0
        self.fp = None
        self.after_id = None
        self.cb = None

    def start(self, cb):
        self.cb = cb
        if not self.after_id: self.after_id = self.app.after(0, self.read)

    def stop(self):
        if self.after_id: self.app.after_cancel(self.after_id)
        self.after_id = None

    def read(self):
        if not self.fp and os.path.exists(self.fname):
            self.fp = open(self.fname)
            self.pos = 0
        if self.fp:
            self.fp.seek(0, os.SEEK_END)
            self.size = self.fp.tell()
            if self.size > self.pos:
                self.fp.seek(self.pos, os.SEEK_SET)
                new = self.fp.read(self.size - self.pos)
                self.pos = self.size
                self.cb(new)
        self.after_id = self.app.after(500, self.read)
