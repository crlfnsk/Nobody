#!/bin/bash

N=10000
dt=0.01
stepsize=10

filename="data_d/dt$dt"
mkdir data_d
mkdir $filename


v_all=("0.3333499996" "0.2108290513" "0.1490786519" "0.1054145257" "0.03333499996")
ecc_all=("00" "60" "80" "90" "99")

for i in {0..4}; do
    echo "$N
$dt
$stepsize
2
0.5      0.00 0.00  0.0     -0.1490786519  0.0  0.0
0.5      0.00 9.00  0.0      0.1490786519  0.0  0.0
" > input_file.inp
    ./nbody < input_file.inp > out
    rm movie/*.png
    ./plot_frames.sh
    ./make_movie.sh
    mv out $filename/${ecc_all[$i]}
    mv energy.txt $filename/${ecc_all[$i]}.txt
    mv out.mp4 $filename/${ecc_all[$i]}.mp4
done

