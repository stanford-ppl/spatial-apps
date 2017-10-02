from lib import save_csv, get_csv
import code
import numpy as np
from numpy import tanh


N = 60
JX = 161
dco = 1400
d = 100
forget_bias = 1.0

def sigmoid(x):
  return 1. / (1. + np.exp(-x))

fileDir = '/home/tianzhao/spatial-lang/apps/LSTM-internals/'
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


np.random.seed(42)

a_total = N * JX * dco
# a = np.linspace(0, 0.2, num=a_total).reshape((N,JX,dco))
a = np.genfromtxt(input_f, delimiter=',').reshape((N,JX,dco)) # (N*M*JX, dco)
# a = np.random.uniform(0, 0.2, a_total).reshape((N,JX,dco))
save_csv(a.flatten(), 'a')

# hidden size should be: (N, JX, d)
h_total = N*JX*d 
hidden = np.linspace(0, 0.1, num=h_total).reshape((N,JX,d))
# hidden = np.random.uniform(0, 0.1, h_total).reshape((N,JX,d))
save_csv(hidden.flatten(), 'hidden')

k_total = (dco+d)*4*d
# kernel = np.linspace(0, 0.5, num=k_total).reshape((dco+d, 4*d))
kernel = np.genfromtxt(kernel_f, delimiter=',').reshape((dco+d, 4*d)) # (dco+d, 4*d)
# kernel = np.random.uniform(0, 0.5, k_total).reshape((dco+d, 4*d))
save_csv(kernel.flatten(), 'kernel')

b_total = 4*d
# bias = np.linspace(0, 0.3, num=b_total).reshape((4*d))
bias = np.genfromtxt(bias_f, delimiter=',').reshape((4*d)) # (4*d, )
# bias = np.random.uniform(0, 0.3, b_total).reshape((4*d))
save_csv(bias.flatten(), 'bias')

print(a.shape)
print(hidden.shape)
print(kernel.shape)
print(bias.shape)
concat = np.concatenate([a[:,0,:], hidden[:,0,:]], axis=1).dot(kernel) + bias
i, j, f, o = np.split(concat, 4, axis=1)
# test_result = np.concatenate([i, j, f + forget_bias, o], axis=1)
test_result = np.concatenate([sigmoid(i), tanh(j), sigmoid(f + forget_bias), sigmoid(o)], axis=1)
print(test_result.shape)
code.interact(local=locals())

dram = np.delete(get_csv('DRAM3Test_result_bias.csv'), 0).reshape(N, 4*d)
tr_splits = np.split(test_result, 4, axis=1)
dram_splits = np.split(dram, 4, axis=1)
tr_i, tr_j, tr_f, tr_o = tr_splits 
dram_i, dram_j, dram_f, dram_o = dram_splits
print('relative error in i:', np.amax(np.abs(tr_i - dram_i) / np.abs(tr_i)))
print('relative error in j:', np.amax(np.abs(tr_j - dram_j) / np.abs(tr_j)))
print('relative error in f:', np.amax(np.abs(tr_f - dram_f) / np.abs(tr_f)))
print('relative error in o:', np.amax(np.abs(tr_o - dram_o) / np.abs(tr_o)))
# # code.interact(local=locals())
