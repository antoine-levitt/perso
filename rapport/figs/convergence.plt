set terminal postscript eps color
set output 'convergence.eps'
set title "Convergence of Newton method"
set xlabel "n"
set ylabel "Error (log scale)"
set logscale y
     set size 1.0, .8
plot "convergence.txt" using 1:2 with linespoints title "Residual", "convergence.txt" using 1:3 with linespoints title "Increment"
!epstopdf --outfile=convergence.pdf convergence.eps
