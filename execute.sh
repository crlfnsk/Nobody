#!/bin/bash

time=20
dt=0.0001
stepsize=300
framestep=3

dir="bin"

echo "$time
$dt
$stepsize
3
1.0  -2.00 0.00  0.0    0.3672   0.5635  0.0
1.0   0.00 0.00  0.0   -0.3672  -0.5635  0.0
1.0   2.00 0.00  0.0    0.3672   0.5635  0.0
" > $dir/input_file.inp

cd bin

./nbody < input_file.inp > out

outfile="error.png"

gnuplot << EOF
set term png font "/usr/share/fonts/truetype/freefont/FreeSans.ttf" 20 size 1440,480
set out '$outfile'
unset key
plot "energy.txt"
EOF

mkdir movie

./plot_frames.sh $framestep
mv out out.txt

./make_movie.sh
mv out.mp4 $subdir/out.mp4