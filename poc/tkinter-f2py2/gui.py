# import the python libraries that we'll need
import Tkinter as tk
import math
import time
from pandas import DataFrame
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
# import the shared object created using f2py
import cupcake

def close_window():
    root.destroy()

# create a window with Tkinter
root = tk.Tk()

# call the Fortran init routine
cupcake.initialise('runtime_params.nml')
offset_step = (2 * math.pi) / cupcake.runtime_parameters.nx

# start assembling the graph we'll plot in the GUI
# We'll use Python's Matplotlib to create the plot
# and tkinter to display it. 
figure = plt.Figure(figsize=(5,4), dpi=100)
ax = figure.add_subplot(111)
root.title('cupcake GUI')
line = FigureCanvasTkAgg(figure, root)
line.show()
line.get_tk_widget().pack(side=tk.LEFT, fill=tk.BOTH)
button = tk.Button (text = "exit", command = close_window)
button.pack(side=tk.BOTTOM)

# The main timestep loop
for ii in range (0, cupcake.runtime_parameters.koverall):
    print "loop iteration: ", ii
    cupcake.timestep(ii * offset_step)
    # create dataframe (using pandas) for subsequent use by Matplotlib
    data = {'xvals': cupcake.global_variables.xvals,
        'yvals': cupcake.global_variables.yvals}
    df = DataFrame(data, columns = ['xvals','yvals'])
    # plot a figure from the dataframe and put into a tkinter window
    df = df[['xvals','yvals']].groupby('xvals').sum()
    ax.clear()
    df.plot(kind='line', legend=True, ax=ax, color='r',marker='o', fontsize=10)
    line.get_tk_widget().pack(side=tk.LEFT, fill=tk.BOTH)
    line.draw()
    time.sleep(3)

cupcake.finalise()

# An infinite loop
root.mainloop()
