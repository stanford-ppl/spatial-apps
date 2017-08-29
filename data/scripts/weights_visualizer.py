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

def draw_kernel(csvs, name):
    matrices = [ genfromtxt('../bi-att-flow/'+file_name, delimiter=',') for file_name in csvs ]
    cat_matrix = np.concatenate(matrices, axis=1)
    plt.figure()
    plt.imshow(cat_matrix)
    plt.colorbar()
    plt.savefig(name+'.pdf')

csvs_first_cell_fw  =  ['f-kernel-first_cell_fw-300-100.csv', \
                        'i-kernel-first_cell_fw-300-100.csv', \
                        'j-kernel-first_cell_fw-300-100.csv', \
                        'o-kernel-first_cell_fw-300-100.csv']

csvs_first_cell_bw  =  ['f-kernel-first_cell_bw-300-100.csv', \
                        'i-kernel-first_cell_bw-300-100.csv', \
                        'j-kernel-first_cell_bw-300-100.csv', \
                        'o-kernel-first_cell_bw-300-100.csv']

csvs_second_cell_fw  = ['f-kernel-second_cell_fw-900-100.csv', \
                        'i-kernel-second_cell_fw-900-100.csv', \
                        'j-kernel-second_cell_fw-900-100.csv', \
                        'o-kernel-second_cell_fw-900-100.csv']

csvs_second_cell_bw  = ['f-kernel-second_cell_bw-900-100.csv', \
                        'i-kernel-second_cell_bw-900-100.csv', \
                        'j-kernel-second_cell_bw-900-100.csv', \
                        'o-kernel-second_cell_bw-900-100.csv']

csvs_d_cell4_fw  = ['f-kernel-d_cell4_fw-1500-100.csv', \
                        'i-kernel-d_cell4_fw-1500-100.csv', \
                        'j-kernel-d_cell4_fw-1500-100.csv', \
                        'o-kernel-d_cell4_fw-1500-100.csv']

csvs_d_cell4_bw = ['f-kernel-d_cell4_bw-1500-100.csv', \
                        'i-kernel-d_cell4_bw-1500-100.csv', \
                        'j-kernel-d_cell4_bw-1500-100.csv', \
                        'o-kernel-d_cell4_bw-1500-100.csv']

pairs = [(csvs_first_cell_bw, 'csvs_first_cell_bw'), \
         (csvs_first_cell_fw, 'csvs_first_cell_fw'), \
         (csvs_second_cell_fw, 'csvs_second_cell_fw'), \
         (csvs_second_cell_bw, 'csvs_second_cell_bw'), \
         (csvs_d_cell4_fw, 'csvs_d_cell4_fw'), \
         (csvs_d_cell4_bw, 'csvs_d_cell4_bw')]

for pair in pairs:
    cell, cell_name = pair
    draw_kernel(cell, cell_name)