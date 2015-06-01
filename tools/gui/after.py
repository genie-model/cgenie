import ttk

#  AFTER TIMER HANDLING
#
# This is a little nasty: we use a lot of "after" timers, and Tkinter
# doesn't seem to have a built-in way to clean them all up before
# exit.  If you don't clean them up, the application hangs on exit
# with a bunch of error messages.  So, we override the after handling
# here to make sure everything does get cleaned up before exit.  This
# class is used as a mixin to the main application class.

class AfterHandler:
    def __init__(self):
        self.aft_c2id = { }
        self.aft_id2c = { }
        self.aft_n = 0

    def after(self, ms, func=None, *args):
        id = ttk.Frame.after(self, ms, self.trigger, self.aft_n, func, *args)
        self.aft_c2id[self.aft_n] = id
        self.aft_id2c[id] = self.aft_n
        self.aft_n += 1
        return id

    def after_cancel(self, id):
        del self.aft_c2id[self.aft_id2c[id]]
        del self.aft_id2c[id]
        ttk.Frame.after_cancel(self, id)

    def trigger(self, c, func, *args):
        del self.aft_id2c[self.aft_c2id[c]]
        del self.aft_c2id[c]
        func(*args)

    def quit(self):
        for id in self.aft_id2c.keys(): ttk.Frame.after_cancel(self, id)
        ttk.Frame.quit(self)
