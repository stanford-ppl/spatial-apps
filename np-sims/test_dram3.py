from lib import save_csv
import numpy as np

a = np.linspace(0, 23, num=24).reshape((3,4,2))
print(a.shape)

save_csv(np.linspace(0, 23, num=24), 'a_3_4_2')