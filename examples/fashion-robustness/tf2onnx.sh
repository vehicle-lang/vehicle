#!/bin/zsh

python -m tf2onnx.convert --saved-model ./path/to/tf/model/name_of_tf_model_1 --output ./path/to/onnx/model/name_of_onnx_model_1.onnx
python -m tf2onnx.convert --saved-model ./path/to/tf/model/name_of_tf_model_2 --output ./path/to/onnx/model/name_of_onnx_model_2.onnx
python -m tf2onnx.convert --saved-model ./path/to/tf/model/name_of_tf_model_3 --output ./path/to/onnx/model/name_of_onnx_model_3.onnx
python -m tf2onnx.convert --saved-model ./path/to/tf/model/name_of_tf_model_4 --output ./path/to/onnx/model/name_of_onnx_model_4.onnx