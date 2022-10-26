
# Small script for generating test IDX dataset files

import idx2numpy
import numpy as np

xs = np.array([0.1,3,-2,-3.2]).astype('float32')

f_write = open('dataset.idx', 'wb')
idx2numpy.convert_to_file(f_write, xs)
