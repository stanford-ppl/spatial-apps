import numpy as np
import matplotlib.pyplot as plt
from numpy import genfromtxt


'''
f-kernel-d_cell4_bw-1500-100.csv
f-kernel-d_cell4_fw-1500-100.csv
f-kernel-first_cell_bw-300-100.csv
f-kernel-first_cell_fw-300-100.csv
f-kernel-second_cell_bw-900-100.csv
f-kernel-second_cell_fw-900-100.csv
i-bias-d_cell4_bw-1500-100.csv
i-bias-d_cell4_fw-1500-100.csv
i-bias-first_cell_bw-300-100.csv
i-bias-first_cell_fw-300-100.csv
i-bias-second_cell_bw-900-100.csv
i-bias-second_cell_fw-900-100.csv
i-kernel-d_cell4_bw-1500-100.csv
i-kernel-d_cell4_fw-1500-100.csv
i-kernel-first_cell_bw-300-100.csv
i-kernel-first_cell_fw-300-100.csv
i-kernel-second_cell_bw-900-100.csv
i-kernel-second_cell_fw-900-100.csv
j-bias-d_cell4_bw-1500-100.csv
j-bias-d_cell4_fw-1500-100.csv
j-bias-first_cell_bw-300-100.csv
j-bias-first_cell_fw-300-100.csv
j-bias-second_cell_bw-900-100.csv
j-bias-second_cell_fw-900-100.csv
j-kernel-d_cell4_bw-1500-100.csv
j-kernel-d_cell4_fw-1500-100.csv
j-kernel-first_cell_bw-300-100.csv
j-kernel-first_cell_fw-300-100.csv
j-kernel-second_cell_bw-900-100.csv
j-kernel-second_cell_fw-900-100.csv
o-kernel-d_cell4_bw-1500-100.csv
o-kernel-d_cell4_fw-1500-100.csv
o-kernel-first_cell_bw-300-100.csv
o-kernel-first_cell_fw-300-100.csv
o-kernel-second_cell_bw-900-100.csv
o-kernel-second_cell_fw-900-100.csv
'''

'''
# make these smaller to increase the resolution
dx, dy = 0.15, 0.05

# generate 2 2d grids for the x & y bounds
y, x = np.mgrid[slice(-3, 3 + dy, dy),
                slice(-3, 3 + dx, dx)]
z = (1 - x / 2. + x ** 5 + y ** 3) * np.exp(-x ** 2 - y ** 2)
# x and y are bounds, so z should be the value *inside* those bounds.
# Therefore, remove the last value from the z array.
z = z[:-1, :-1]
z_min, z_max = -np.abs(z).max(), np.abs(z).max()


plt.subplot(2, 2, 1)
plt.pcolor(x, y, z, cmap='RdBu', vmin=z_min, vmax=z_max)
plt.title('pcolor')
# set the limits of the plot to the limits of the data
plt.axis([x.min(), x.max(), y.min(), y.max()])
plt.colorbar()


plt.subplot(2, 2, 2)
plt.pcolormesh(x, y, z, cmap='RdBu', vmin=z_min, vmax=z_max)
plt.title('pcolormesh')
# set the limits of the plot to the limits of the data
plt.axis([x.min(), x.max(), y.min(), y.max()])
plt.colorbar()


plt.subplot(2, 2, 3)
plt.imshow(z, cmap='RdBu', vmin=z_min, vmax=z_max,
           extent=[x.min(), x.max(), y.min(), y.max()],
           interpolation='nearest', origin='lower')
plt.title('image (nearest)')
plt.colorbar()


ax = plt.subplot(2, 2, 4)
ax.pcolorfast(x, y, z, cmap='RdBu', vmin=z_min, vmax=z_max)
plt.title('pcolorfast')
plt.colorbar()

plt.subplots_adjust(wspace=0.5, hspace=0.5)

plt.show()
Keywords: python, matplotlib, pylab, example, codex (see Search examples)

© Copyright 2002 - 2012 John Hunter, Darren Dale, Eric Firing, Michael Droettboom and the Matplotlib development team; 2012 - 2016 The Matplotlib development team. Last updated on May 10, 2017. Created using Sphinx 1.5.5.
'''

csvs = ['f-kernel-first_cell_fw-300-100.csv', \
        'i-kernel-first_cell_fw-300-100.csv', \
        'j-kernel-first_cell_fw-300-100.csv', \
        'o-kernel-first_cell_fw-300-100.csv']

matrices = [genfromtxt('../bi-att-flow/'+file_name, delimiter=',') for file_name in csvs]
plt.figure()
for idx, matrix in enumerate(matrices):
  print(matrix.shape)
  plt.imshow(matrix)
  plt.colorbar()

plt.show()
