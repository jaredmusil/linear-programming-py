# app.py by jared musil
# various tools for use within the iowa water science center
# tested with Python 3.3 and Tkinter 8.5

__version__ = '0.6'

import os
import tkinter as tk
import tkinter.ttk as ttk

import matplotlib
matplotlib.use('TkAgg')

from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
import numpy as np
import matplotlib.pyplot as plt


class Application:
    def __init__(self, root):
        self.root = root
        self.root.title('Simplex Solver')
        self.root.iconbitmap(os.path.dirname(os.path.abspath(__file__)) + '\\logo.ico')

        # variables
        self.appRoot = os.path.dirname(os.path.abspath(__file__))
        self.widthProcessBtn  = 130
        self.widthProcessIcon = 27
        # images
        self.imgAdd     = tk.PhotoImage(data='R0lGODlhEAAQALMKAABy/5bG4ZDG5pLI247E9JXH2ZbF4ZDH5o7B/5LI3P///wAAAAAAAAAAAAAAAAAAACH5BAEAAAoALAAAAAAQABAAAAQ0UMlJq71Ygg3yBEHQeQookmWAjBa3EYTLpkJtH+FsDjyf5BRZoSDDAAwnkmlmBCo5qGg0AgA7')
        self.imgRemove  = tk.PhotoImage(data='R0lGODlhEAAQAMIGAABy/5DG5pDH5pbG4ZLI25LI3P///////yH5BAEKAAcALAAAAAAQABAAAAMheLrc/jDKSesDOGvMwAhgKAwD0JFEmhakuWywa810bdMJADs=')
        self.imgRun     = tk.PhotoImage(data='R0lGODlhEAAQAMZSADNIYzNIZDRJYzVKZTdMZjhNZzlNaDlOaDxQazxRaz5SbD5SbUJWb0RYcipbvUdadEpdd0teeExeeE5helBifFFkfVRmf1RngFlrhFtuhlxuh2BxiWFzi2R1jWV3jmZ4kGd5kEN+9Gt9k21/lm+Al1GD5XOEmneHnn+PpF6T/NTe9dXf9tbh9tji99rj9tzk99zl993m997n99/n+OHn+OLp+OPp+ePr+eXr+eXr+ubs+ebs+ubt+uju+eju+urv+uvv+uvw+uzx++3y++7y++7z++/z+/Dz/PD0/PH1/PL2/PP2/fT3/fX3/fX4/ff5/fn7/vv8/v///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////yH5BAEKAH8ALAAAAAAQABAAAAeXgH+Cg4SFhCWHiYaLi4iDjoKQkYqTkSkpIQ4ll5mbmA6RmaGgfyWig6SCqX+pJyYkIiAdGxoYFxUTEA8NfyNSv8DBv0wKfx5RKFAfTxRNCUoARwBDBX8ZTkxLSUhGREJAPz48OQF/FkUcQRE9BjoANgAzADAAfxI7ODc1NDIxLy4tWKxQUa8BgwUIDhAYIACAw4f1GC0KBAA7')
        self.imgOpen    = tk.PhotoImage(data='R0lGODlhEAAQAOMJAICAAJ+fP///n19fP5+ff9+/P7+fP//fn19fX////////////////////////////yH5BAEKAA8ALAAAAAAQABAAAARE8MlJq70WaIAnOsJxIB0AnsemBRMQvudLSiZspy087Hw/1IdBYUgcGkiuYLF4pCmXxtkDMDBYr1fpg4Doer+dsHgsjgAAOw==')
        self.imgOkay    = tk.PhotoImage(data='R0lGODlhEAAQAIABAB4eHQAAACH5BAEAAAEALAAAAAAQABAAAAIjjI+pG8AK3DtRzmotwlk3TkUhtlUfWXJmoq6QeqGx99DTVAAAOw==')
        self.imgReset   = tk.PhotoImage(data='R0lGODlhEAAQAOMJAAAAALAwF39ZBo1jBpVyHdGUDNusN+K7Uu7WgP///////////////////////////yH5BAEKAA8ALAAAAAAQABAAAARF8ElAq6XyAYK6/wIwIUZhnmYpakgRXEDQrsCcZXVB2zerj79ebhe8DSszDM5WaxWBxJWRp5FaZM+p55DFoQoDqRDWK5cjADs=')
        self.imgDelete  = tk.PhotoImage(data='R0lGODlhEAAQAOcAAP///8opAJMAAPb4+v/M/4SQnfr7/Pj5+/z9/f7+/vT2+Ss2QvH1+P9MDP8zAP8pAOnu9O3y9uXs8u/z9+vw9f+DIJSfq/9sFHJ+ivr7/ff5+4+PyGBpc/n7/GNsd1dfaIGOmkpUX3aBjX6KlvP2+e7y9/n6+3B7h2Zveuzx9oSRnfH09v3+/2hyfVBZYW14hJKRy/7///j6/I6Ox//+/mBoc52evoSRnrjBx2t1gHuHk3mEkOXr8dPY3oiUn4mVoerv9Ss3QlRcZoCKlvD1+Kissejt84iUof///wAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAP///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////yH5BAEKAH8ALAAAAAAQABAAAAjRAH/cKECwoAofFv4oVAgCgMOHDmEASLhwBIAEGDMCmOGQ4h8dABCIHAlgw0OKOwAYWMkyhg0XQj4MUSgCwIGbOE2wWNEDxxGFGAAMGEp0qAYZADAoPAGAhIKnTzM4lApA4QsARBhoZdCBRoAADr/+yQGgxISzEwAEIEDgK9sALQCkiEA3AgABFSqwzSsABQAgFAJT0HD3guELAv54AGAEgmMIA9Q2mNwgwB8OAHhI2CxBLYEHDhw8aFsD4kMBoAUIEC0gRJAFsGEXuSvAoeo/AQEAOw==')
        self.imgOutput  = tk.PhotoImage(data='R0lGODlhEAAQAKUqAABCZQFGagBHbgBLcwBMcwFMdAFQeT9DRgBSewJSeAJXgQBZhANagklNTwJdiQBfiwNijwNmlAVsmwdwoV5hZAl0pmlsb25xc06LrVOOsGCdvKnV+KfY+rPa+Krf+b3f98Lh+crl+dHo+tfq+9zs++Hw/OXy/Ofz/e32/fD4/f///////////////////////////////////////////////////////////////////////////////////////yH5BAEKAD8ALAAAAAAQABAAAAaNwJ/wV6kMf5ekcjhJTY7QoQQlklivP4vW8oucQKCTWByBPkqdtDr9+FHelN9iQ6/TF9EfgsNB+P8/DYINQwQeBHl5AgJHB46PQhGSDJKVlJM/EBomJhoQJhCfn5wQPw4kDqcOI6msqw4JPwohCrO2t7RCARghIRgBGb0ZAQYfBkMAAwMAPwAFy83QiYlBADs=')
        self.imgRecover = tk.PhotoImage(data='R0lGODlhEAAQALMAAP9jAP8AAM4AAL0AAP///87OznNzcwAAAP///wAAAAAAAAAAAAAAAAAAAAAAAAAAACH5BAEAAAgALAAAAAAQABAAAARaEEl5qp0YVQI6IFV2EGRgBmRxTGMqDC9JGKuW0pas2kRBYy1fBRAQ1IBE44E4OLKYw6KTkow2Mxpoi+QM/ragXKp2KMg8n96PYigUTgH3mnUwwGA4LMUSwkQAADs=')
        # colors
        self.ncsRed    = '#C40233'
        self.ncsBlue   = '#0087BD'
        self.ncsGreen  = '#009F6B'
        self.ncsYellow = '#FFD300'
        # styles
        style = ttk.Style()
        style.configure('bgRed.TLabel', background=self.ncsRed)
        style.configure('bgGreen.TLabel', background=self.ncsGreen)
        style.configure('bgYellow.TLabel', background=self.ncsYellow)

        # construct gui
        self.init_widgets()

    def init_widgets(self):
        # root window resizing
        self.root.grid_rowconfigure(0, weight=1)
        self.root.grid_columnconfigure(0, weight=1)

        # add tabs
        self.nb = ttk.Notebook(root)
        self.nb.grid(row=0, column=0, sticky=tk.NSEW)
        self.tab_variables()
        self.tab_formula()
        self.tab_standard()
        self.tab_agumented()
        
        # add plot
        self.frame_plot()
    

    def tab_variables(self):
        self.tab_variables = tk.Frame()
        tab = self.tab_variables
        self.nb.add(tab, text='Variables')

        # tab variables
        tab.label1 = tk.StringVar()
        tab.variable1 = tk.StringVar()

        # tab window resizing
        #tab.grid_rowconfigure(0, weight=1)
        #tab.grid_rowconfigure(1, weight=1)
        
        # frames
        tab.addRemove = ttk.Frame(tab)
        tab.inputs    = ttk.Frame(tab)
        tab.addRemove.grid( row=0, column=0, sticky=tk.NSEW, padx=5, pady=5)
        tab.inputs.grid(    row=1, column=0, sticky=tk.NSEW, padx=(0,5), pady=5)

        # frame widgets
        tab.addRemove.addBtn    = ttk.Button(tab.addRemove, command=lambda:self.add_variable(tab), image=self.imgAdd)
        tab.addRemove.removeBtn = ttk.Button(tab.addRemove, command=lambda:self.remove_variable(tab), image=self.imgRemove)
        tab.inputs.label1Label    = ttk.Label(tab.inputs, text='Label')
        tab.inputs.variable1Label = ttk.Label(tab.inputs, text='Var')
        tab.inputs.label1Entry    = ttk.Entry(tab.inputs, textvariable='label1')
        tab.inputs.variable1Entry = ttk.Entry(tab.inputs, textvariable='variable1')

        # frame widget placement
        tab.addRemove.addBtn.grid(    row=0, column=0, sticky=tk.NSEW)
        tab.addRemove.removeBtn.grid( row=0, column=1, sticky=tk.NSEW)
        
        tab.inputs.label1Label.grid(    row=0, column=0, sticky=tk.NSEW)
        tab.inputs.variable1Label.grid( row=0, column=1, sticky=tk.NSEW)
        tab.inputs.label1Entry.grid(    row=1, column=0, sticky=tk.NSEW)
        tab.inputs.variable1Entry.grid( row=1, column=1, sticky=tk.NSEW)

        # frame widget window resizing
        tab.addRemove.grid_columnconfigure(0, weight=1)
        tab.addRemove.grid_rowconfigure(0, weight=1)
        
        # - strech process frame columns to be the same width as other tabs
        ##tab.label.grid_columnconfigure(0, weight=0, minsize=self.widthProcessBtn)
        ##tab.label.grid_columnconfigure(1, weight=0, minsize=self.widthProcessIcon)

    def tab_formula(self):
        self.tab_formula = tk.Frame()
        tab = self.tab_formula
        self.nb.add(tab, text='Formula')

        # tab window resizing
        # - the process frame can only resize vertically
        # - the log frame can resize in both directions
        tab.grid_rowconfigure(1, weight=1)
        tab.grid_columnconfigure(0, weight=0)
        tab.grid_columnconfigure(1, weight=1)

        # tab variables
        tab.var1Num  = tk.StringVar()
        tab.var1val  = tk.StringVar()
        tab.var2Num  = tk.StringVar()
        tab.var2val  = tk.StringVar()
        tab.varObjective = tk.StringVar()

        # frames
        tab.predicate  = ttk.Frame(tab)
        tab.variable   = ttk.Frame(tab)
        tab.constraint = ttk.Frame(tab)
        tab.predicate.grid(  row=0, column=0, sticky=tk.NSEW)
        tab.variable.grid(   row=0, column=1, sticky=tk.NSEW)
        tab.constraint.grid( row=0, column=2, sticky=tk.NSEW)

        # frame widgets
        #tab.predicate.objectiveLabel  = ttk.Label(tab.predicate, text='Max/Min')
        tab.predicate.objectiveLabel  = ttk.OptionMenu(tab.predicate, tab.varObjective, 'Maximize', 'Minimize')
        tab.predicate.subjectToLabel  = ttk.Label(tab.predicate, text='Subject To:')
        tab.variable.maxSignLabel  = ttk.Label(tab.variable, text='+/-')
        tab.variable.maxNumEntry   = ttk.Entry(tab.variable, textvariable='#')
        tab.variable.maxvalLabel   = ttk.Label(tab.variable, textvariable='x1')
        tab.variable.var1SignLabel = ttk.Label(tab.variable, text='+/-')
        tab.variable.var1NumEntry  = ttk.Entry(tab.variable, textvariable='#')
        tab.variable.var1ValLabel  = ttk.Label(tab.variable, textvariable='x1')
        tab.constraint.var1Sign       = ttk.Label(tab.constraint, text='<=')
        tab.constraint.var1Constraint = ttk.Label(tab.constraint, text='0')

        # frame widget placement
        tab.predicate.objectiveLabel.grid( row=0, column=0, sticky=tk.EW)
        tab.predicate.subjectToLabel.grid( row=1, column=0, sticky=tk.EW)
        tab.variable.maxSignLabel.grid(  row=0, column=0, sticky=tk.EW)
        tab.variable.maxNumEntry.grid(   row=0, column=1, sticky=tk.EW)
        tab.variable.maxvalLabel.grid(   row=0, column=2, sticky=tk.EW)
        tab.variable.var1SignLabel.grid( row=1, column=0, sticky=tk.EW)
        tab.variable.var1NumEntry.grid(  row=1, column=1, sticky=tk.EW)
        tab.variable.var1ValLabel.grid(  row=1, column=2, sticky=tk.EW)
        tab.constraint.var1Sign.grid(       row=1, column=0, sticky=tk.EW)
        tab.constraint.var1Constraint.grid( row=1, column=1, sticky=tk.EW)

        #frame widget event binding
        #tab.process.runoffBtn.bind("<Return>",self.add_runoff(tab))

        # frame widget window resizing
        # - the text widget will fill its containter in both directions
        ##tab.log.grid_columnconfigure(0, weight=1)
        ##tab.log.grid_rowconfigure(0, weight=1)
        # - push 'recover input' button to bottom of process frame using spacer
        ##tab.process.grid_rowconfigure(5, weight=1)
        # - strech process frame columns to be the same width as other tabs
        ##tab.process.grid_columnconfigure(0, weight=0, minsize=self.widthProcessBtn)
        ##tab.process.grid_columnconfigure(1, weight=0, minsize=self.widthProcessIcon)

    def tab_standard(self):
        self.tab_standard = tk.Frame()
        tab = self.tab_standard
        self.nb.add(tab, text='Standard')

        # tab variables
        tab.label1 = tk.StringVar()
        tab.variable1 = tk.StringVar()

        # tab window resizing

        # frames
        tab.addRemove = ttk.Frame(tab)
        tab.inputs    = ttk.Frame(tab)
        tab.addRemove.grid( row=0, column=0, sticky=tk.NSEW, padx=5, pady=5)
        tab.inputs.grid(    row=1, column=0, sticky=tk.NSEW, padx=(0,5), pady=5)

        # frame widgets
        tab.addRemove.addBtn    = ttk.Button(tab.addRemove, command=lambda:self.open_directory(tab, 'changeME'), image=self.imgAdd)
        tab.addRemove.removeBtn = ttk.Button(tab.addRemove, command=lambda:self.open_directory(tab, 'changeME'), image=self.imgRemove)
        tab.inputs.label1Label    = ttk.Label(tab.inputs, text='Label')
        tab.inputs.variable1Label = ttk.Label(tab.inputs, text='Var')
        tab.inputs.label1Entry    = ttk.Entry(tab.inputs, textvariable='label1')
        tab.inputs.variable1Entry = ttk.Entry(tab.inputs, textvariable='variable1')

        # frame widget placement
        tab.addRemove.addBtn.grid(    row=0, column=0, sticky=tk.NSEW)
        tab.addRemove.removeBtn.grid( row=0, column=1, sticky=tk.NSEW)
        tab.inputs.label1Label.grid(    row=0, column=0, sticky=tk.NSEW)
        tab.inputs.variable1Label.grid( row=0, column=1, sticky=tk.NSEW)
        tab.inputs.label1Entry.grid(    row=1, column=0, sticky=tk.NSEW)
        tab.inputs.variable1Entry.grid( row=1, column=1, sticky=tk.NSEW)

    def tab_agumented(self):
        self.tab_agumented = tk.Frame()
        tab = self.tab_agumented
        self.nb.add(tab, text='Agumented')

        #tab variables
        #tab.formula = tk.StringVar()
        #tab.formula.set(r'\frac{a}{b}') #<-- Will change to input from variables tab data

        # frames
        tab.formulaPlot = ttk.Frame(tab)
        tab.formulaPlot.grid(row=0, column=0, sticky=tk.NSEW)
        
        # frame widgets
        tab.figure = plt.Figure(figsize=(6, 1), dpi=100)
        
        #subplot = tab.figure.add_subplot(111)
        #subplot = plt.axes([0, 0, 0.1, 0.2]) #left,bottom,width,height
        #tab.figure.set_xticks([]) #No tick marks
        #tab.figure.set_yticks([]) #No tick marks
        tab.figure.text(0.3, 0.4, '$%s$' %r'\frac{a}{b}', size=40) #Place text in mpl frame
        
        # frame widget placement
        canvas = FigureCanvasTkAgg(tab.figure, master=tab.formulaPlot)
        canvas.show()
        canvas.get_tk_widget().grid(row=0, column=0, sticky=tk.NSEW)
        
        
    def frame_plot(self):
        self.frame_plot = tk.Frame()
        
        # data
        x = np.arange(0, 2*np.pi, 0.01)
        y = np.sin(x)
        
        # plot setup
        figure = plt.Figure(figsize=(6, 3), dpi=100)
        subplot = figure.add_subplot(111)
        subplot.plot(x, y)
        
        # add to gui
        canvas = FigureCanvasTkAgg(figure, master=root)
        canvas.get_tk_widget().grid(row=1, column=0)

# -----------------------------------------------------------------------------

    def add_variable(self, tab):
        # add variable label
        tab.process.drainage.label = ttk.Label(tab.process.drainage, text='')
        tab.process.drainage.label.grid(row=0, column=0, columnspan=2, sticky=tk.EW)

        # add new label inputs
        entry = ttk.Entry(tab.process.drainage)
        label = ttk.Label(tab.process.drainage, anchor=tk.CENTER)
        # offset the grid by 1 because of the label
        entry.grid(row=1, column=0, sticky=tk.EW)
        label.grid(row=1, column=1, sticky=tk.EW)


# -----------------------------------------------------------------------------

if __name__ == '__main__':
    root = tk.Tk()
    Application(root)
    root.mainloop()