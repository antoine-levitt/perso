simu: nvstks_05_05_31.f90
	g95 nvstks_05_05_31.f90 -o simu -cpp
run: simu
	cat donnees.dat | G95_UNBUFFERED_ALL=y ./simu | tee out.txt
