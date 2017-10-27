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

fileDir = '/home/tianzhao/spatial-lang/apps/parameters/'
IOs = fileDir + 'IOs/'
weights = fileDir + 'weights/'

input_f = IOs + 'input-0.csv'
# input_pkl = IOs + 'input-0.pkl'
kernel_f = weights + 'kernel-d_cell4_fw-1500-400.csv'
bias_f = weights + 'bias-first_cell_fw-400.csv'
seq_f = IOs + 'seq_len-0.pkl'
sim_result_f = 'sim-state'
acc_mem = 'acc_c'
acc_hidden = 'acc_h'

# original input size: (60, 1, 161, 1400)
kernel = np.genfromtxt(kernel_f, delimiter=',') # (dco+d, 4*d)
bias = np.genfromtxt(bias_f, delimiter=',') # (4*d, )
in_mat = np.genfromtxt(input_f, delimiter=',') # (N*M*JX, dco)
in_mat = in_mat.reshape(N, M, JX, -1).squeeze()
print('input shape is', in_mat.shape)

h = np.zeros((N, d))
c = np.zeros((N, d))
state = np.concatenate([c, h], 1)

re_c = None
re_h = None

for idx in range(161):
    print(idx)
    input = in_mat[:, idx, :] # (N, dco)
    c, h = np.split(state, 2, axis=1)
    concat = np.concatenate([input, h], axis=1).dot(kernel) + bias # (N, dco)
    i, j, f, o = np.split(concat, 4, axis=1) # (N, d)
    new_c = np.multiply(c, sigmoid(f + forget_bias)) + np.multiply(sigmoid(i), tanh(j)) # (N, d)
    new_h = np.multiply(tanh(new_c), sigmoid(o)) # (N, d)
    state = np.concatenate([new_c, new_h], 1) # (N, 2d)
    if re_c is None:
        print('init')
        re_c = np.copy(new_c).reshape((N, 1, d))
    else:
        re_c = np.append(re_c, np.copy(new_c).reshape((N, 1, d)), axis=1)

    if re_h is None:
        re_h = np.copy(new_h).reshape((N, 1, d))
    else:
        re_h = np.append(re_h, np.copy(new_h).reshape((N, 1, d)), axis=1)

print(re_c.shape)
print(re_h.shape)
re_c_flatten = re_c.reshape((-1, d))
re_h_flatten = re_h.reshape((-1, d))
np.savetxt(sim_result_f +'.csv', state, delimiter=',', newline='\n', fmt='%.9e')
np.savetxt(acc_mem + '.csv', re_c_flatten, delimiter=',', newline='\n', fmt='%.9e')
np.savetxt(acc_hidden + '.csv', re_h_flatten, delimiter=',', newline='\n', fmt='%.9e')