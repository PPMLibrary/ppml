#!/usr/bin/env python

import matplotlib.pyplot as plt
import sys


filename = sys.argv[1]

f = open(filename)
header = f.readline()

header = header.strip().split('\t')
pdata = []
for h in header:
  pdata.append([])

for l in f:
  r = l.strip().split('\t')
  for i in range(len(r)):
    pdata[i].append(float(r[i]))

for i in range(1,len(pdata)):
  plt.plot(pdata[0],pdata[i],label=header[i])

plt.legend(loc=2)
plt.show()
