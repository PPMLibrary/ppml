#!/usr/bin/env python
import matplotlib.pyplot as plt

def plot_tot(ival):
    etot = []
    epot = []
    ekin = []
    f = open('energy.dat')
    f.readline()
    i = 0
    for l in f:
        r = l.strip().split()
        if (i % ival == 0):
            etot.append(float(r[1]))
            epot.append(float(r[3]))
            #ekin.append(float(r[3]))
        i = i + 1
    f.close()
    t = [i*ival for i in range(len(epot))]
    plt.plot(t,etot,'b',label='$E_{tot}$')
    plt.plot(t,epot,'r',label='$E_{pot}$')
    plt.legend(loc=3)
    plt.xlabel('timestep')
    plt.ylabel('$E(r)$')
    plt.ylim([-6.0e-9,-3.0e-9])
    plt.show()

plot_tot(10)
