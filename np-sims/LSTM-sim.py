import numpy as np
import code
import pickle
from numpy import tanh


def get_mat(fn):
    pkl_file = open(fn, 'rb')
    data = pickle.load(pkl_file)
    return data

def sigmoid(x):
  return 1. / (1. + np.exp(-x))


JX = 161
M = 1
N = 60
dco = 1400
d = 100
forget_bias = 1.0

fileDir = '/home/tianzhao/spatial-lang/apps/LSTM-internals/'
IOs = fileDir + 'IOs/'
weights = fileDir + 'weights/'

input_f = IOs + 'input-0.csv'
# input_pkl = IOs + 'input-0.pkl'
kernel_f = weights + 'kernel-d_cell4_fw-1500-400.csv'
bias_f = weights + 'bias-first_cell_fw-400.csv'
seq_f = IOs + 'seq_len-0.pkl'
sim_result_f = 'sim-state'

# original input size: (60, 1, 161, 1400)
kernel = np.genfromtxt(kernel_f, delimiter=',') # (dco+d, 4*d)
bias = np.genfromtxt(bias_f, delimiter=',') # (4*d, )
in_mat = np.genfromtxt(input_f, delimiter=',') # (N*M*JX, dco)
in_mat = in_mat.reshape(N, M, JX, -1).squeeze()
print('input shape is', in_mat.shape)

h = np.zeros((N, d))
c = np.zeros((N, d))
state = np.concatenate([c, h], 1)


for idx in range(161):
    print(idx)
    input = in_mat[:, idx, :] # (N, dco)
    c, h = np.split(state, 2, axis=1)
    concat = np.concatenate([input, h], axis=1).dot(kernel) + bias # (N, dco)
    i, j, f, o = np.split(concat, 4, axis=1) # (N, d)
    new_c = np.multiply(c, sigmoid(f + forget_bias)) + np.multiply(sigmoid(i), tanh(j)) # (N, d)
    new_h = np.multiply(tanh(new_c), sigmoid(o)) # (N, d)
    state = np.concatenate([new_c, new_h], 1) # (N, 2d)

np.savetxt(sim_result_f +'.csv', state, delimiter=',', newline='\n', fmt='%.9e')
