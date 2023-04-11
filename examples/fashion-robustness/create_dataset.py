from keras.datasets import fashion_mnist
import idx2numpy


(X_train, y_train), (X_test, y_test) = fashion_mnist.load_data()
X_train = X_train / 255.0
X_test = X_test /255.0

idx2numpy.convert_to_file('examples/fashion-robustness/10k-images.idx', X_test)
idx2numpy.convert_to_file('examples/fashion-robustness/10k-labels.idx', y_test)

idx2numpy.convert_to_file('examples/fashion-robustness/1k-images.idx', X_test[:1000])
idx2numpy.convert_to_file('examples/fashion-robustness/1k-labels.idx', y_test[:1000])

idx2numpy.convert_to_file('examples/fashion-robustness/1-images.idx', X_test[:1])
idx2numpy.convert_to_file('examples/fashion-robustness/1-labels.idx', y_test[:1])
