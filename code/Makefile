simu: nvstks_05_05_31.F90
	g95 nvstks_05_05_31.F90 -o simu -cpp -g
run: simu
	cat donnees.dat | GFORTRAN_UNBUFFERED_ALL=y G95_UNBUFFERED_ALL=y ./simu | tee out.txt
