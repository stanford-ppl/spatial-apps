from lib import save_csv
import code
import numpy as np


project_dir = '/home/tianzhao/spatial-lang/apps/src/'

def sigmoid(arr):
    return 1. / (1. + np.exp(-arr))

def gen_fn(hi, totalSize, fn, fnF):
    spacing = hi / float(totalSize)
    re = []
    for i in range(0, totalSize):
        re.append(i * spacing)
    space = np.array(re)
    result = fn(space)
    save_csv(result, project_dir+fnF+'_'+str(totalSize)+'_'+str(hi)+'_'+str(np.log2(spacing)))
    return result


halfSigLUT = gen_fn(16, 512, sigmoid, 'sigmoid_') # this should cover 1024 points though we ask for 512
# tval = 6.3125
# looked_up_result = 1 - LUT[abs(tval) * 2**6]
# looked_up_result = halfSigLUT[abs(tval) * 2**6]
# print(looked_up_result, sigmoid(tval))

# tanh
halfTanhLUT = gen_fn(16, 512, np.tanh, 'tanh')
tval = -6.3125
looked_up_result = -halfTanhLUT[abs(tval) * 2**5]
print(looked_up_result, np.tanh(tval))