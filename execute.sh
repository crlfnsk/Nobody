#!/bin/bash

time=50
dt=0.001
stepsize=50
framestep=3

echo "$time
$dt
$stepsize
2
1.0         0.00 0.00  0.0      0.0  0.0  0.0
0.0001      0.00 0.90  0.0      1.054145257  0.0  0.0
" > input_file.inp

./nbody < input_file.inp > out

subdir="data_d/dt$dt"

mkdir $subdir
outfile=$subdir"/error_$dt.png"

gnuplot << EOF
set term png font "/usr/share/fonts/truetype/freefont/FreeSans.ttf" 20 size 1440,480
set out '$outfile'
unset key
plot "energy.txt"
EOF

mv energy.txt $subdir"/error$dt.txt"

rm movie/*.png
./plot_frames.sh $framestep
mv out $subdir"/out$dt.txt"
./make_movie.sh

mv out.mp4 $subdir/error_$dt.mp4