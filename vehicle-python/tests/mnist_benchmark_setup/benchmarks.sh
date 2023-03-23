echo "epochs,DL,ratio,accuracy" > classification-data.csv
echo "epochs,DL,ratio,delta,satisfaction" > constraint-data.csv

export EPOCHV=10

# for EPOCHS in 20
# do
# export EPOCHV=$EPOCHS
for DL in LossFunction-Godel LossFunction-Lukasiewicz LossFunction-Product LossFunction-DL2
do
    export DLV=$DL
    # for RATIO in 100 95 90 85 80 75 70 65 60 55 50 45 40 35 30 25 20 15 10 5 0
    for RATIO in 100 90 80 70 60 50 40 30 20 10 0
    do
        export RATIOV=$RATIO
        poetry run python3 test_mnist_custom_loss_experiment_setup.py
    done
done