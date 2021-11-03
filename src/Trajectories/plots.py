import numpy as np
import matplotlib.pyplot as plt

with open('5.dat','r') as f: # change number infront of .dat to see a specific sailor's path
    lines=f.readlines()
    x = [int(line.split()[0]) for line in lines]
    y = [int(line.split()[1]) for line in lines]

fig=plt.figure()
ax =fig.add_subplot(1,1,1)
majorticks= np.arange(-50,50,10) # ticks look nice for the smaller walks
minorticks= np.arange(-50,50,1)

ax.set_xlim([-50,50]) # change these to fit the case. the usual saw fits nicely in a -30,30, but a large nrw can take thousands. pyplot doesnt autoscale nicely at for small walks
ax.set_ylim([-50,50]) # better be too large than small size pyplot can be zoomed

ax.set_xticks(majorticks)
ax.set_xticks(minorticks,minor=True)

ax.set_yticks(majorticks)
ax.set_yticks(minorticks,minor=True)
ax.grid(which='both')

plt.xlabel('x')
plt.ylabel('y')

ax.set_title('Example of random walk')
ax.grid(which='minor', alpha=0.2) #lower alpha=lower transparency
ax.grid(which='major', alpha=0.5)

ax.plot(x,y)
plt.show()
