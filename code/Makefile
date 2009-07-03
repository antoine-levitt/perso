simu: nvstks_05_05_31.f90
	g95 nvstks_05_05_31.f90 -o simu -O3
run: simu

	cat donnees.dat | G95_UNBUFFERED_ALL=y ./simu | tee out.txt
