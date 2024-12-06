#!/bin/bash

N=10000
dt=0.001
stepsize=10
framestep=50

filename="data_d/dt$dt"
mkdir data_d
mkdir $filename

for i in {0..4}; do
    echo "$N
$dt
$stepsize
2
1.0         0.00 0.00  0.0      0.0  0.0  0.0
0.0001      0.00 9.00  0.0     ${v_all[$i]}  0.0  0.0
" > input_file.inp
    ./nbody < input_file.inp > out
    rm movie/*.png
    ./plot_frames.sh $framestep
    ./make_movie.sh
    mv out $filename/${ecc_all[$i]}
    mv energy.txt $filename/${ecc_all[$i]}.txt
    mv out.mp4 $filename/${ecc_all[$i]}.mp4
done

rm movie/*.png
./nbody < input_file.inp > out

gnuplot << EOF
set term png font "/usr/share/fonts/truetype/freefont/FreeSans.ttf" 20 size 1440,480
set out 'error.png'
unset key
plot "energy.txt"
EOF

#./plot_frames.sh $framestep
#./make_movie.sh