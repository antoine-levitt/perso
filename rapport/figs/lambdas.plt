set terminal postscript eps color
set output 'lambdas.eps'
set size 1.0, .8
set title "Pressure at mid-width for different lambdas (zoom)"
set xlabel "y"
set ylabel "Pressure"
set xrange [2.35:2.65]
plot "lambdas/3.dat" using 1:7 with linespoints title "1e-3",\
     "lambdas/4.dat" using 1:7 with linespoints title "1e-4",\
"lambdas/5.dat" using 1:7 with linespoints title "1e-5",\
"lambdas/6.dat" using 1:7 with linespoints title "1e-6",\
"lambdas/8.dat" using 1:7 with linespoints title "1e-8"
#"lambdas/12.dat" using 1:7 with linespoints title "1e-12"
!epstopdf --outfile=lambdas.pdf lambdas.eps
