import numpy as np
import pickle

def get_mat(fn):
    pkl_file = open(fn, 'rb')
    data = pickle.load(pkl_file)
    return data

def get_csv(fn):
    return np.genfromtxt(fn, delimiter=',')
