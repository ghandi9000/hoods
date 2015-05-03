# dists.py --- 
# Filename: dists.py
# Description: 
# Author: Noah Peart
# Created: Sun May  3 02:34:46 2015 (-0400)
# Last-Updated: Sun May  3 03:41:12 2015 (-0400)
#           By: Noah Peart
# 

from IPython.core.pylabtools import figsize
from matplotlib import pyplot as plt
import numpy as np

dat = np.loadtxt("/home/noah/work/hoods/bayes/p4.csv", delimiter=",", 
                 skiprows=1)

ba = dat[:,0]
growth = dat[:,1]
year = dat[:,2]

plt.plot(ba, growth, marker='.', linestyle='')
plt.show()

plt.hist(growth, histtype='stepfilled')
plt.show()

plt.hist(ba, histtype='stepfilled')
plt.show()

ba = pm.Exponential('ba', 1, value=ba, observed=True)
growth = pm.Normal('growth', 1, 1, value=growth, observed=True)

