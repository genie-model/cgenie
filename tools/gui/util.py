def enable(w, on):
    # Enable or disable Tkinter widgets.
    w.state(['!disabled' if on else 'disabled'])
