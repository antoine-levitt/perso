#!/bin/bash
echo -n 'plot ' > gnuplot_commands
for i in $(find . -type d -maxdepth 1 -not -name . -name 'lambda*' | sort -k 2 -g -t '-') ; do
    echo -n "\"$i/profil_v_0.5.dat\"  using 1:$1 title \"$i\" with linespoints," >> gnuplot_commands
done
sed -i 's/,$//' gnuplot_commands
echo "" >> gnuplot_commands
echo "replot" >> gnuplot_commands
echo "pause -1" >> gnuplot_commands
gnuplot gnuplot_commands
