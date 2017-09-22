from lib import save_csv
import numpy as np


N = 3
JX = 4
dco = 2
d = 4 

a_total = N * JX * dco
a = np.linspace(0, a_total-1, num=a_total).reshape((N,JX,dco))

save_csv(a.flatten(), 'a_3_4_2')
# hidden size should be: (N, JX, d)
h_total = N * JX * d
hidden = np.linspace(0, h_total-1, num=h_total).reshape((N,JX,d))
save_csv(hidden.flatten(), 'hidden_3_4_4')

kernel = np.ones((dco+d, 4*d)) * 2
save_csv(kernel.flatten(), 'kernel_6_16')
bias = np.ones((4*d)) * 3
save_csv(bias.flatten(), 'bias_16')

result = np.concatenate([a[:,0,:], hidden[:,0,:]], axis=1).dot(kernel) + bias
print('result shape:', result.shape)