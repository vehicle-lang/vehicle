# Small script for generating test IDX dataset files

import idx2numpy
import numpy as np

size = 2

f_read = open("./examples/mnist-robustness/t10k-labels.idx", "rb")
dataset = idx2numpy.convert_from_file(f_read)
# dataset = dataset.astype("float64") / 255.0

shrunkDataset = dataset[:size]

f_write = open(f"./examples/mnist-robustness/t{size}-labels.idx", "wb")
idx2numpy.convert_to_file(f_write, shrunkDataset)
