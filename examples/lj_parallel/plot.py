#!/usr/bin/env python
import matplotlib.pyplot as plt

def plot_tot(ival):
    etot = []
    epot = []
    ekin = []
    #dist = []
    f = open('energy.dat')
    f.readline()
    f.readline()
    i = 0
    for l in f:
        r = l.strip().split()
        if (i % ival == 0):
            etot.append(float(r[1]))
            ekin.append(float(r[2]))
            epot.append(float(r[3]))
            #dist.append(float(r[4]))
        i = i + 1
    f.close()
    t = [i*ival for i in range(len(epot))]

    fig = plt.figure()
    ax1 = fig.add_subplot(111)

    ax1.plot(t,etot,'b',label='$E_{tot}$')
    ax1.plot(t,epot,'r',label='$E_{pot}$')
    
    #ax2 = ax1.twinx()
    #ax2.plot(t,dist,'g',label='$\lambda$')
    
    
    ax1.legend(loc=3)
    #ax2.legend(loc=0)
    ax1.set_xlabel('timestep')
    ax1.set_ylabel('$E(r)$')
    #ax2.set_ylabel('displacement since nlist')
    #ax1.set_ylim([-1.0e-5,-1.0e-6])
    plt.show()

plot_tot(10)
