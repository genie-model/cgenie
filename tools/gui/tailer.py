import os, os.path

# Class for "following" files, i.e. capturing changes in a file as a
# process writes output into it.  Used for capturing GENIE model and
# build process output for display in the GUI.

class Tailer:
    def __init__(self, app, fname):
        # Create a tailer.  We need a Tkinter application object to
        # use for timers and a filename to follow.
        self.app = app          # Tkinter application.
        self.fname = fname      # Filename.
        self.pos = 0            # Current position in file.
        self.fp = None          # Current file handle.
        self.after_id = None    # ID of current "after" timer.
        self.cb = None          # Callback for reporting file changes.

    def start(self, cb):
        # Start a tailer running, calling a given callback when new
        # data is written to the tailed file.
        self.cb = cb
        if not self.after_id: self.after_id = self.app.after(0, self.read)

    def stop(self):
        # Stop a running tailer.
        if self.after_id: self.app.after_cancel(self.after_id)
        self.after_id = None

    def read(self):
        # Attempt to read new data from the tailed file and report to
        # the callback.
        if not self.fp and os.path.exists(self.fname):
            # If the file current is not open, but it exists, open it,
            # starting from the beginning.
            self.fp = open(self.fname)
            self.pos = 0
        if self.fp:
            # Determine the current size of the file.
            self.fp.seek(0, os.SEEK_END)
            self.size = self.fp.tell()
            if self.size > self.pos:
                # If there is new content in the file since the last
                # time we checked, read the new data and pass it to
                # the callback.
                self.fp.seek(self.pos, os.SEEK_SET)
                new = self.fp.read(self.size - self.pos)
                self.pos = self.size
                self.cb(new)

        # Schedule the next read.
        self.after_id = self.app.after(500, self.read)
