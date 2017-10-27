import tensorflow as tf
import numpy as np
import code
from lib import get_mat, get_csv, save_mat, save_csv
from tensorflow.contrib.rnn import BasicLSTMCell
from tensorflow.python.ops.rnn import dynamic_rnn


JX = 161
M = 1
N = 60
dco = 1400
d = 100


fileDir = '/home/tianzhao/spatial-lang/apps/parameters/'
IOs = fileDir + 'IOs/'
weights = fileDir + 'weights/'

input_f = IOs + 'input-0.csv'
input_pkl = IOs + 'input-0.pkl'
kernel_f = weights + 'kernel-d_cell4_fw-1500-400.csv'
bias_f = weights + 'bias-first_cell_fw-400.csv'
seq_f = IOs + 'seq_len-0.csv'
sim_result_f = 'sim-state'
acc_mem = 'acc_c'
acc_hidden = 'acc_h'


kernel = np.genfromtxt(kernel_f, delimiter=',', dtype='float32') # (dco+d, 4*d)
bias = np.genfromtxt(bias_f, delimiter=',', dtype='float32') # (4*d, )
seq_len = np.genfromtxt(seq_f, delimiter=',', dtype='int')
# in_mat = np.genfromtxt(input_f, delimiter=',', dtype='float32') # (N*M*JX, dco)
in_mat = get_mat(input_pkl)
input_batch = in_mat.reshape(N, M, JX, -1).squeeze()


# only simulate one link for now
inputs = tf.placeholder(tf.float32, shape=(N, JX, dco))
seq_lens = tf.placeholder(tf.int32, shape=(N))
cell_fw = BasicLSTMCell(d, state_is_tuple=True, \
                        kernel_initializer=lambda x, dtype, partition_info=None: tf.constant(kernel, dtype), \
                        bias_initializer=lambda x, dtype, partition_info=None: tf.constant(bias, dtype))
test_rnn_seq = dynamic_rnn(cell_fw, inputs, sequence_length=seq_lens, dtype='float')
test_rnn_noseq = dynamic_rnn(cell_fw, inputs, sequence_length=None, dtype='float')

init_op = tf.global_variables_initializer()
sess = tf.Session()
sess.run(init_op)
result = sess.run(test_rnn_seq, feed_dict={inputs: input_batch, seq_lens: seq_len})
save_mat(result, 'test_rnn_seq')


init_op_1 = tf.global_variables_initializer()
sess_1 = tf.Session()
sess_1.run(init_op_1)
result_1 = sess_1.run(test_rnn_noseq, feed_dict={inputs: input_batch, seq_lens: seq_len})
save_mat(result_1, 'test_rnn_noseq')
