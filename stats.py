# -*- coding: utf-8 -*-

#Automated file creation with the results of an analysis about delivery dates where the user imports any csv and gets results in a txt.

import os 
import pandas as pd
from tkinter import filedialog 

pd.set_option('display.max_rows', 100000)
pd.set_option('display.expand_frame_repr', False)

fname = filedialog.askopenfilename(title="Select file: ")

with open(os.path.dirname(fname) + '/'+'statistics.txt', 'w') as fw: 
    csv_f = pd.read_csv(fname, sep=",",   #read csv
                  encoding = 'utf-8',parse_dates=['del_day'], #delivery date
                  usecols=['del_day','user','code_paper','number_of_files'],
                  dayfirst=True)
    csv_f = csv_f[csv_f['code_paper'].notnull()]


    fi = csv_f.groupby(['del_day','user']).agg({'del_day' : 'count','number_of_files':'sum'}).sort_index(level=[0,1], ascending=[False,True]))
    fw.write(str(fi))
