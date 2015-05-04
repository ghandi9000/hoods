# dists.py --- 
# Filename: dists.py
# Description: 
# Author: Noah Peart
# Created: Sun May  3 02:34:46 2015 (-0400)
# Last-Updated: Sun May  3 22:39:37 2015 (-0400)
#           By: Noah Peart
# 

from IPython.core.pylabtools import figsize
from matplotlib import pyplot as plt
import numpy as np
import pymc as pm

dat = np.loadtxt("c://home//work//hoods//bayes//p4.csv", delimiter=",", 
                 skiprows=1)

ba = dat[:,0]
growth = dat[:,1]
year = dat[:,2]
n = len(ba)

## Priors
# plt.plot(ba, growth, marker='.', linestyle='')
# plt.show()

# plt.hist(growth, histtype='stepfilled')
# plt.show()

# plt.hist(ba, histtype='stepfilled')
# plt.show()

# ba = pm.Exponential('ba', 1.0/ba.mean(), value=ba, observed=True)

def model(x, g, p0):
    a = pm.Normal('a', mu=p0['a'], tau=1.0/p0['a'])
    b = pm.Normal('b', mu=p0['b'], tau=1.0/p0['b'])
    # intercept = pm.Uniform('intercept', 0, 1, value=0.01)
    
    obs_tau = pm.Gamma('obs_tau', alpha=0.1, beta=3)
    
    @pm.deterministic
    def power(x=x, a=a, b=b):
        return a*x**b
    y = pm.Normal('y', mu=power, tau=obs_tau, value=g, observed=True)
    return locals()
    
p0 = {
    'a' : 0.005,
    'b' : 0.3,
}

def model2(x, g, p2):
    a = pm.Normal('a', mu=p2['a'], tau=1.0 / p2['a'])
    intercept = pm.Normal('intercept', p2['intercept'], 1/p2['intercept'])
    obs_tau = pm.Gamma('obs_tau', alpha=0.1, beta=3)
    
    @pm.deterministic
    def line(x=x, a=a):
        return a*x
    y = pm.Normal('y', mu=line, tau=obs_tau, value=g, observed=True)
    return locals()
    
p2 = {
    'a' : 7.933e-03,
    'intercept' : 3.129e-04,
}

mcmc1 = pm.MCMC(model2(ba, growth, p2))
mcmc1.sample(30000, 10000, 1)

a_samples = mcmc1.trace('a')[:]
int_samples = mcmc1.trace('intercept')[:]
y_min = mcmc1.stats()['line']['quantiles'][2.5]
y_max = mcmc1.stats()['line']['quantiles'][97.5]
y_fit = mcmc1.stats()['line']['mean']
plt.plot(ba, growth, '.', label='Data')
plt.plot(ba, y_fit + int_samples.mean(), linestyle='', marker='o', label='Fitted')
# plt.fill_between(ba, y_min, y_max, color='0.5', alpha=0.5)
plt.legend()

## Posterior Distributions
plt.figure()
ax = plt.subplot(211)
ax.set_autoscaley_on(False)
plt.hist(a_samples, bins=300, histtype='stepfilled', label='posterior of $a$', color="#A60628", normed=True)
plt.legend()

ax = plt.subplot(212)
ax.set_autoscaley_on(False)
plt.hist(int_samples, bins=300, histtype='stepfilled', label='posterior of $intercept$', color="#7468A6", 
         normed=True)

plt.legend()

pm.Matplot.plot(mcmc1)
################################################################################
##
##                                   Power
##
################################################################################
mcmc = pm.MCMC(model(ba, growth, p0))
mcmc.sample(40000, 10000, 1)

a_samples = mcmc.trace('a')[:]
b_samples = mcmc.trace('b')[:]
obs_tau_samples = mcmc.trace('obs_tau')[:]
# intercept_samples = mcmc.trace('intercept')[:]

## Plotting fit
y_min = mcmc.stats()['power']['quantiles'][2.5]
y_max = mcmc.stats()['power']['quantiles'][97.5]
y_fit = mcmc.stats()['power']['mean']
plt.plot(ba, growth, '.', label='Data')
plt.plot(ba, y_fit, linestyle='', marker='o', label='Fitted')
# plt.fill_between(ba, y_min, y_max, color='0.5', alpha=0.5)
plt.legend()

## Posterior Distributions
plt.figure()
ax = plt.subplot(311)
ax.set_autoscaley_on(False)
plt.hist(a_samples, histtype='stepfilled', label='posterior of $a$', color="#A60628", normed=True)

ax = plt.subplot(312)
ax.set_autoscaley_on(False)
plt.hist(b_samples, histtype='stepfilled', label='posterior of $b$', color="#7468A6", normed=True)

ax = plt.subplot(313)
ax.set_autoscaley_on(False)
plt.hist(obs_tau_samples, histtype='stepfilled', label='posterior of $obs_\tau$', color="#FF9900", normed=True)

pm.Matplot.plot(mcmc)

# import matplotlib.pyplot as plt; plt.ion()
# import pymc
# x = np.arange(5,400,10)*1e3

# # Parameters for gaussian
# amp_true = 0.2
# size_true = 1.8
# ps_true = 0.1

# # Gaussian function
# gauss = lambda x,amp,size,ps: amp*np.exp(-1*(np.pi**2/(3600.*180.)*size*x)**2/(4.*np.log(2.)))+ps
# f_true = gauss(x=x,amp=amp_true, size=size_true, ps=ps_true )

# # add noise to the data points
# noise = np.random.normal(size=len(x)) * .02 
# f = f_true + noise 
# f_error = np.ones_like(f_true)*0.05*f.max()

# # define the model/function to be fitted.
# def model(x, f): 
#     amp = pymc.Uniform('amp', 0.05, 0.4, value= 0.15)
#     size = pymc.Uniform('size', 0.5, 2.5, value= 1.0)
#     ps = pymc.Normal('ps', 0.13, 40, value=0.15)

#     @pymc.deterministic(plot=False)
#     def gauss(x=x, amp=amp, size=size, ps=ps):
#         e = -1*(np.pi**2*size*x/(3600.*180.))**2/(4.*np.log(2.))
#         return amp*np.exp(e)+ps
#     y = pymc.Normal('y', mu=gauss, tau=1.0/f_error**2, value=f, observed=True)
#     return locals()

# MDL = pymc.MCMC(model(x,f))
# MDL.sample(1e4)

# # extract and plot results
# y_min = MDL.stats()['gauss']['quantiles'][2.5]
# y_max = MDL.stats()['gauss']['quantiles'][97.5]
# y_fit = MDL.stats()['gauss']['mean']
# plt.plot(x,f_true,'b', marker='None', ls='-', lw=1, label='True')
# plt.errorbar(x,f,yerr=f_error, color='r', marker='.', ls='None', label='Observed')
# plt.plot(x,y_fit,'k', marker='+', ls='None', ms=5, mew=2, label='Fit')
# plt.fill_between(x, y_min, y_max, color='0.5', alpha=0.5)
# plt.legend()
