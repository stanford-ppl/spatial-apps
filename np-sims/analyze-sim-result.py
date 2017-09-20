import numpy as np
import code


JX = 161
M = 1
N = 60
dco = 1400
d = 100

fileDir = '/home/tianzhao/spatial-lang/apps/LSTM-internals/'
IOs = fileDir + 'IOs/'
weights = fileDir + 'weights/'
output_f = IOs + 'output-0.csv'

output = np.genfromtxt(output_f, delimiter=',').reshape((N, JX, 2*d))
output_last = output[:,-1,:]
output_fw, output_bw = np.split(output_last, 2, axis=1)

sim_output_f = './sim-state.csv' 
sim_ch = np.genfromtxt(sim_output_f, delimiter=',')
c, h = np.split(sim_ch, 2, axis=1)

code.interact(local=locals())