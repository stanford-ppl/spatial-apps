import numpy as np


def fake_data_gen(n_rows, n_cols, name):
  mat = np.ones((n_rows, n_cols))
  np.savetxt(name, mat, delimiter=',')

if __name__ == '__main__':
  fake_data_gen(64, 64, '64_by_64_eles.csv')
  fake_data_gen(64, 32, '64_by_32_eles.csv')
