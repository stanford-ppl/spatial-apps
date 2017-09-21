import numpy as np
import pickle
import code
import tensorflow

def get_dat(fn):
    pkl_file = open(fn, 'rb')
    data = pickle.load(pkl_file)
    return data

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

# sim_output_f = './sim-state.csv' 
# sim_ch = np.genfromtxt(sim_output_f, delimiter=',')
# c, h = np.split(sim_ch, 2, axis=1)

c = np.genfromtxt('acc_c.csv', delimiter=',').reshape((N, JX, d))
h = np.genfromtxt('acc_h.csv', delimiter=',').reshape((N, JX, d))

fw_states_f = IOs + 'fw_s20.pkl'
states = get_dat(fw_states_f)
code.interact(local=locals())