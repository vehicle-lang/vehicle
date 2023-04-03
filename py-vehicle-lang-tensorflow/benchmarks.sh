#!/bin/sh

echo "epochs,DL,ratio,accuracy" > classification-data.csv
echo "epochs,DL,ratio,delta,satisfaction" > constraint-data.csv

for EPOCHS in 20
do
    export EPOCHV=$EPOCHS
    for RATIO in 100 95 90 85 80 75 70 65 60 55 50 45 40 35 30 25 20 15 10 5 0
    do
        export RATIOV=$RATIO
        for DL in LossFunction-DL2 LossFunction-Godel LossFunction-Lukasiewicz LossFunction-Product
        do
            export DLV=$DL
            poetry run python3 tests/test_mnist_custom_loss.py
        done
    done
done
