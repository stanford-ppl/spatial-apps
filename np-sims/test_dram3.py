from lib import save_csv, get_csv
import code
import numpy as np
from numpy import tanh


N = 3
JX = 16
dco = 32
d = 8 
forget_bias = 1.0

def sigmoid(x):
  return 1. / (1. + np.exp(-x))

a_total = N * JX * dco
a = np.linspace(0, 0.2, num=a_total).reshape((N,JX,dco))

save_csv(a.flatten(), 'a')
# hidden size should be: (N, JX, d)
h_total = N * JX * d
hidden = np.linspace(0, 0.1, num=h_total).reshape((N,JX,d))
save_csv(hidden.flatten(), 'hidden')

k_total = (dco+d)*4*d
kernel = np.linspace(0, 0.5, num=k_total).reshape((dco+d, 4*d))
save_csv(kernel.flatten(), 'kernel')

b_total = 4*d
bias = np.linspace(0, 0.3, num=b_total).reshape((4*d))
# bias = np.zeros((b_total)).reshape((4*d))
save_csv(bias.flatten(), 'bias')

concat = np.concatenate([a[:,0,:], hidden[:,0,:]], axis=1).dot(kernel) + bias
# print('result shape:', concat.shape)
# print(np.array2string(concat.flatten()))
i, j, f, o = np.split(concat, 4, axis=1)
test_result = np.concatenate([sigmoid(i), tanh(j), sigmoid(f + forget_bias), sigmoid(o)], axis=1)
# test_result = np.concatenate([i, j, f + forget_bias, o], axis=1)
print('test_result = ', test_result)

dram = np.delete(get_csv('DRAM3Test_result_bias.csv'), 0).reshape(N, 4*d)

code.interact(local=locals())