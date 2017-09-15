'''
LSTM cell implementation in tf
'''
    sigmoid = math_ops.sigmoid
    # Parameters of gates are concatenated into one multiply for efficiency.
    if self._state_is_tuple:
      c, h = state
    else:
      c, h = array_ops.split(value=state, num_or_size_splits=2, axis=1)

    concat = _linear([inputs, h], 4 * self._num_units, True)

    #   i = input_gate, j = new_input, f = forget_gate, o = output_gate
    i, j, f, o = array_ops.split(value=concat, num_or_size_splits=4, axis=1)

    new_c = (
        c * sigmoid(f + self._forget_bias) + sigmoid(i) * self._activation(j))
    new_h = self._activation(new_c) * sigmoid(o)

    if self._state_is_tuple:
      new_state = LSTMStateTuple(new_c, new_h)
    else:
      new_state = array_ops.concat([new_c, new_h], 1)
    return new_h, new_state

'''
Parameter needed: 
'''
  d = config.hidden_size = 100 # TODO: retrain with 128
  # input size: (60, 1, 161, 1400)
  # output size: (60, 1, 161, 200)
  num_units = d * 2

'''
TODO: what's the max size of hidden and bias matrices I can store in BRAM? 
TODO: MS guys are right in the sense that LSTM is actually mem bound. O(mem) ~= O(compute)
'''
