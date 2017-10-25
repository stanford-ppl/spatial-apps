import numpy as np
import lib
from lib import get_csv, get_mat
import sys


mat = get_csv(sys.argv[1])
print("max is: ", np.amax(mat))
print("min is: ", np.amin(mat))
