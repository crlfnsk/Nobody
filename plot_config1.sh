#!/bin/bash
#
# shell-script to plot several snapshots from a single data file
# the data is in a file called 'out' and has the format 
#
#     time    x1  y1  z1     x2  y2  z2     x3  y3  z3
#
# settings allow you to change the name of the input file and also
# skip lines for plotting (e.g. every=10 plots only one in ten lines)
# the resulting plots are in PNG-format and store in numbered files
# a movie can be created with the make_movie.sh script 

# settings
infile='out'
every=$1
dt=$2

# loop over lines 
i=0     # file number counter

cat << EOF > tmp.awk
{
 if (NR%$every==0) print 
}
EOF

awk -f tmp.awk $infile |\
while read line
do
    # Adding the timestamp to each plot
    step=$(echo "$line" | awk '{print $1}')
    timestamp=$(echo "scale=4; $step * $dt" | bc)

    # set name of output png file 
    outfile=`printf "movie/threebody_%5.5i.png" $i`
    echo generating $outfile


    echo $line > tmp.dat

# calling gnuplot to make plot
# use lots of settings in gnuplot including multiplot 
# make sure FreeSans.ttf is in the stated dir or change accordingly
gnuplot << EOF
set term png font "/usr/share/fonts/truetype/freefont/FreeSans.ttf" 20 size 720, 480
set out '$outfile'

unset key
set border 31 lw 3

set label 1 "t=$timestamp"
set label 1 at graph 0.05, 0.9

set origin 0.0 , 0.0

set xlabel 'x'
set ylabel 'y'

set xrange [-3.0 to 3.0]
set yrange [-2.0 to 2.0]

set xtics 0.5
set ytics 0.5

plot 'tmp.dat' using 2:3 with points pt 7 ps 3,\
     'tmp.dat' using 5:6 with points pt 7 ps 3,\
     'tmp.dat' using 8:9 with points pt 7 ps 3

EOF

((i++))
done

rm tmp.dat tmp.awk