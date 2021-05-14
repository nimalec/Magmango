from spin_axes import SpinVectors 
import numpy as np 
spins = SpinVectors(npoints=100, angle_range=[[0, np.pi/4],[0, np.pi/4]]) 
print(spins._spin_axes.T[1]) 
