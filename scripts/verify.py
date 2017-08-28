import numpy as np
import code
from numpy import genfromtxt
from scipy.special import expit as sigmoid
from numpy import tanh, matmul, multiply


D_h = 64
d = 64
N = 32
project_dir = '/Users/tianzhao/Developers/spatial/spatial-lang/' 
data_64_64 = project_dir + 'apps/data/bi-att-flow/64_by_64_eles.csv'
data_64_32 = project_dir + 'apps/data/bi-att-flow/64_by_32_eles.csv'
results_dir = project_dir + 'apps/results/LSTM_Forward_Single/'

W_i = genfromtxt(data_64_64, delimiter=',')
U_i = genfromtxt(data_64_64, delimiter=',') 
W_f = genfromtxt(data_64_64, delimiter=',')
U_f = genfromtxt(data_64_64, delimiter=',') 
W_o = genfromtxt(data_64_64, delimiter=',') 
U_o = genfromtxt(data_64_64, delimiter=',') 
W_c = genfromtxt(data_64_64, delimiter=',') 
U_c = genfromtxt(data_64_64, delimiter=',') 
x_t = genfromtxt(data_64_32, delimiter=',') 
h_t_1 = genfromtxt(data_64_32, delimiter=',') 
W_c_t_1 = genfromtxt(data_64_32, delimiter=',') 

i = sigmoid(matmul(U_i, h_t_1) + matmul(W_i, x_t))
c = tanh(matmul(U_c, h_t_1) + matmul(W_c, x_t))
f = sigmoid(matmul(U_f, h_t_1) + matmul(W_f, x_t))
o = sigmoid(matmul(U_o, h_t_1) + matmul(W_o, x_t))

ci = multiply(c, i)
ctf = multiply(W_c_t_1, f)

ct = ci + ctf

ht = multiply(o, tanh(ct))

np.savetxt(results_dir + 'ct_ref.csv', ct, delimiter=',')
np.savetxt(results_dir + 'ht_ref.csv', ht, delimiter=',')

print('new hidden state = ')
print(ht)
print('new memory state = ')
print(ct)