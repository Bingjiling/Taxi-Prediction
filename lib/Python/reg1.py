# -*- coding: utf-8 -*-
"""
Created on Sat Jun 27 11:16:59 2015

@author: Xinghai
"""

import numpy as np
import pandas as pd

def main():
    df = pd.read_csv('new3.csv',sep=',', index_col=0)
    df.plot(kind = "scatter", x = 'past', y = 'diff')    
    df['past'].plot(kind = 'hist',alpha=0.5, bins=50 )
    df['diff'].plot(kind = 'hist',alpha=0.5, bins=50 )
    
    
    
main()