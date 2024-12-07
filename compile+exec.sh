#!/bin/bash

#rm bin/movie/*.png
cd src
make all

cd ..
make install

cd bin
./nbody < input_file.inp > out
./plot_frames.sh
./make_movie.sh
