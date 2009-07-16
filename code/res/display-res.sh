#!/bin/bash
echo -n 'plot ' > gnuplot_commands
for i in $(find . -type d -maxdepth 1 -not -name .) ; do
    echo -n "\"$i/profil_v_0.5.dat\"  using 1:$1 title \"$i\" with lines," >> gnuplot_commands
done
sed -i 's/,$//' gnuplot_commands
echo "" >> gnuplot_commands
echo "set xrange [7:9]" >> gnuplot_commands
echo "replot" >> gnuplot_commands
gnuplot -persist gnuplot_commands
