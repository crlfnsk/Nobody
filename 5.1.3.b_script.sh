#!/bin/bash

time=10
#time=100
#dt=0.0001
#stepsize=1
#framestep=3

dir="data_5.1.3.b"
mkdir $dir

dt_all=("1" "0.1" "0.01" "0.001" "0.0001" "0.00001")
stepsize_all=("1" "1" "3" "30" "300" "3000")
#stepsize_all=("1" "3" "30" "300" "3000" "30000")
framestep_all=("1" "3" "3" "3" "3" "3")


#0..5

for i in {0..5}; do
    subdir="data_5.1.3.b/dt${dt_all[$i]}"
    mkdir $subdir
    echo "$time
${dt_all[$i]}
${stepsize_all[$i]}
3
1.0         0.00 0.00  0.0                                                                  0.5  -0.8660254037844386467637231707529361834714026269051903140279034897  0.0
1.0         1.00 0.00  0.0                                                                  0.5  +0.8660254037844386467637231707529361834714026269051903140279034897  0.0
1.0         0.50 0.8660254037844386467637231707529361834714026269051903140279034897  0.0   -1.0  0.0  0.0
" > input_file.inp
    ./nbody < input_file.inp > out

    outfile=$subdir"/error.png"
gnuplot << EOF
set term png font "/usr/share/fonts/truetype/freefont/FreeSans.ttf" 20 size 1440,480
set out '$outfile'
unset key
plot "energy.txt"
EOF
    mv input_file.inp $subdir/input_file.inp
    mv energy.txt $subdir/energy.txt
    rm movie/*.png
    ./plot_frames_circle.sh ${framestep_all[$i]} ${dt_all[$i]}
    ./make_movie.sh
    mv out $subdir/out
    mv out.mp4 $subdir/out.mp4
done

