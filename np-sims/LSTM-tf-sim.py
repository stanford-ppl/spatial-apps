import tensorflow as tf
import numpy as np
import code
from tensorflow.contrib.rnn import BasicLSTMCell
from tensorflow.python.ops.rnn import dynamic_rnn


JX = 161
M = 1
N = 60
dco = 1400
d = 100


fileDir = '/home/tianzhao/spatial-lang/apps/LSTM-internals/'
IOs = fileDir + 'IOs/'
weights = fileDir + 'weights/'

input_f = IOs + 'input-0.csv'
# input_pkl = IOs + 'input-0.pkl'
kernel_f = weights + 'kernel-d_cell4_fw-1500-400.csv'
bias_f = weights + 'bias-first_cell_fw-400.csv'
seq_f = IOs + 'seq_len-0.csv'
sim_result_f = 'sim-state'
acc_mem = 'acc_c'
acc_hidden = 'acc_h'


kernel = np.genfromtxt(kernel_f, delimiter=',', dtype='float32') # (dco+d, 4*d)
bias = np.genfromtxt(bias_f, delimiter=',', dtype='float32') # (4*d, )
seq_len = np.genfromtxt(seq_f, delimiter=',', dtype='int')
in_mat = np.genfromtxt(input_f, delimiter=',', dtype='float32') # (N*M*JX, dco)
input_batch = in_mat.reshape(N, M, JX, -1).squeeze()


# only simulate one link for now
inputs = tf.placeholder(tf.float32, shape=(N, JX, dco))
cell_fw = BasicLSTMCell(d, state_is_tuple=True, \
                        kernel_initializer=lambda x, dtype, partition_info=None: tf.constant(kernel, dtype), \
                        bias_initializer=lambda x, dtype, partition_info=None: tf.constant(bias, dtype))
test_rnn = dynamic_rnn(cell_fw, inputs, sequence_length=seq_len, dtype='float')

init_op = tf.initialize_all_variables()
sess = tf.Session()
sess.run(init_op)
result = sess.run(test_rnn, feed_dict={inputs: input_batch})
code.interact(local=locals())