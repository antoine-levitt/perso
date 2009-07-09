#!/bin/bash
echo 'set multiplot' > gnuplot_commands
echo -n 'plot ' >> gnuplot_commands
for i in $(find . -type d -maxdepth 1 -not -name .) ; do
    echo -n "\"$i/profil_v_0.5.dat\"  using 1:$1 title \"$i\" with lines," >> gnuplot_commands
done
sed -i 's/,$//' gnuplot_commands
gnuplot -persist gnuplot_commands
