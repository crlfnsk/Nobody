#!/bin/bash

time=50
dt=0.0001
stepsize=$(echo "scale=4; 0.1 / $dt" | bc)
stepsize=${stepsize%.*} 
#stepsize=1
framestep=3

dir="data_5.1.2.d"
subdir="data_5.1.2.d/dt$dt"
mkdir $dir
mkdir $subdir

v_all=("1.054145257" "0.6666999992" "0.4714280904" "0.3333499996" "0.1054145257")
ecc_all=("00" "60" "80" "90" "99")

for i in {0..4}; do
    echo "$time
$dt
$stepsize
2
1.0         0.00 0.00  0.0      0.0  0.0  0.0
0.0001      0.00 0.90  0.0      ${v_all[$i]}  0.0  0.0
" > input_file.inp
    ./nbody < input_file.inp > out

    outfile=$subdir"/error_${ecc_all[$i]}.png"
gnuplot << EOF
set term png font "/usr/share/fonts/truetype/freefont/FreeSans.ttf" 20 size 1440,480
set out '$outfile'
unset key
plot "energy.txt"
EOF
    mv energy.txt $subdir/${ecc_all[$i]}.txt
    
    rm movie/*.png
    ./plot_frames.sh $framestep
    ./make_movie.sh
    mv out $subdir/${ecc_all[$i]}
    mv out.mp4 $subdir/${ecc_all[$i]}.mp4
done

