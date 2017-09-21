import numpy as np
import pickle

def get_mat(fn):
    pkl_file = open(fn, 'rb')
    data = pickle.load(pkl_file)
    return data

def get_csv(fn):
    return np.genfromtxt(fn, delimiter=',')

def save_mat(mat, fn):
    tmp_pkl = open(fn+'.pkl', 'wb')    
    pickle.dump(mat, tmp_pkl)

def save_csv(mat, fn):
    np.savetxt(fn+'.csv', mat, delimiter=',', newline='\n', fmt='%.9e')
