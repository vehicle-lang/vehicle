
# Small script for generating test IDX dataset files

import idx2numpy
import numpy as np

f_read = open('./examples/mnist-robustness/10k-test-labels.idx', 'rb')
dataset = idx2numpy.convert_from_file(f_read)

shrunkDataset = dataset[:5]

f_write = open('./examples/mnist-robustness/test-labels-5.idx', 'wb')
idx2numpy.convert_to_file(f_write, shrunkDataset)
