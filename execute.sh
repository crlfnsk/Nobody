#!/bin/bash

rm movie/*.png
./nbody < input_file.inp > out
./plot_frames.sh
./make_movie.sh
