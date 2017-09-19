import numpy as np
import math
from np import tanh


def sigmoid(x):
  return 1 / (1 + math.exp(-x))


fileDir = '/home/tianzhao/spatial-lang/apps/LSTM-internals/'
IOs = fileDir + 'IOs/'
weights = fileDir + 'weights/'

input_f = IOs + 'input-0.csv'
kernel_f = weights + 'kernel-d_cell4_fw-1500-400.csv'
bias_f = weights + 'bias-first_cell_fw-400.csv'

kernel = np.genfromtxt(kernel_f, delimiter=',') # (1500, 400)
bias = np.genfromtxt(bias_f, delimiter=',') # (400, )
in_mat = np.genfromtxt(input_f, delimiter=',') # (9660, 1500)

JX = 161

for idx in range(161):
    concat = in_mat.dot(kernel) + bias # 9600, 400
    i, j, f, o = 