from lib import save_csv, get_csv
import code
import numpy as np
from numpy import tanh


N = 32
JX = 10
dco = 100
d = 20
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


# np.random.seed(42)

a_total = N * JX * dco
# a = np.linspace(0, 0.2, num=a_total).reshape((N,JX,dco))
# a = np.genfromtxt(input_f, delimiter=',').reshape((N,JX,dco)) # (N*M*JX, dco)
a = np.random.normal(-1, 1, a_total).reshape((N,JX,dco))
save_csv(a.flatten(), 'a')

code.interact(local=locals())

# hidden size should be: (N, JX, d)
h_total = N*JX*d 
hidden = np.linspace(0, 0.001, num=h_total).reshape((N,JX,d))
# hidden = np.random.uniform(0, 0.1, h_total).reshape((N,JX,d))
save_csv(hidden.flatten(), 'hidden')

# memory ize should be: (N, JX, d)
c_total = N*JX*d 
memory = np.linspace(0, 0.001, num=c_total).reshape((N,JX,d))
# memory = np.random.uniform(0, 0.001, h_total).reshape((N,JX,d))
save_csv(memory.flatten(), 'memory')

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
sig_i = sigmoid(i)
tanh_f = tanh(f)
sig_f = sigmoid(f + forget_bias)
sig_o = sigmoid(o)
# test_result = np.concatenate([sigmoid(i), tanh(j), sigmoid(f + forget_bias), sigmoid(o)], axis=1)
# print(test_result.shape)

new_c = (np.multiply(memory[:,0,:], sig_f) + np.multiply(sig_i, tanh_f)) 
new_h = np.multiply(tanh(new_c), sig_o)

code.interact(local=locals())

dram_c = np.delete(get_csv('mem_re.csv'), 0).reshape(N, JX, d)[:, 0, :]
dram_h = np.delete(get_csv('hidden_re.csv'), 0).reshape(N, JX, d)[:, 0, :]
# tr_splits = np.split(test_result, 4, axis=1)
# dram_splits = np.split(dram, 4, axis=1)
# tr_i, tr_j, tr_f, tr_o = tr_splits 
# dram_i, dram_j, dram_f, dram_o = dram_splits
# print('relative error in i:', np.amax(np.abs(tr_i - dram_i) / np.abs(tr_i)))
# print('relative error in j:', np.amax(np.abs(tr_j - dram_j) / np.abs(tr_j)))
# print('relative error in f:', np.amax(np.abs(tr_f - dram_f) / np.abs(tr_f)))
# print('relative error in f:', np.amax(np.abs(tr_f - dram_f) / np.abs(tr_f)))

print('relative error in mem:',    np.amax(np.abs(new_c - dram_c) / np.abs(new_c)))
print('relative error in hidden:', np.amax(np.abs(new_h - dram_h) / np.abs(new_h)))
# code.interact(local=locals())
