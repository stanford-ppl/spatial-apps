from lib import save_csv, get_csv
import code
import numpy as np


N = 3
JX = 16
dco = 32
d = 8 

a_total = N * JX * dco
a = np.linspace(0, a_total-1, num=a_total).reshape((N,JX,dco))

save_csv(a.flatten(), 'a')
# hidden size should be: (N, JX, d)
h_total = N * JX * d
hidden = np.linspace(0, h_total-1, num=h_total).reshape((N,JX,d))
save_csv(hidden.flatten(), 'hidden')

k_total = (dco+d)*4*d
kernel = np.linspace(0, k_total-1, num=k_total).reshape((dco+d, 4*d))
save_csv(kernel.flatten(), 'kernel')

b_total = 4*d
bias = np.linspace(0, b_total-1, num=b_total).reshape((4*d))
# bias = np.zeros((b_total)).reshape((4*d))
save_csv(bias.flatten(), 'bias')

# result = np.concatenate([a[:,0,:], hidden[:,0,:]], axis=1).dot(kernel) + bias
result = np.concatenate([a[:,0,:], hidden[:,0,:]], axis=1).dot(kernel) + bias
print('result shape:', result.shape)
print(np.array2string(result.flatten()))

dram = np.delete(get_csv('DRAM3Test_result_bias.csv'), 0).reshape((N, 4*d))

code.interact(local=locals())
