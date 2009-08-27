CFLAGS_COMMON=#-march=native -mtune=native -mfpmath=sse
CFLAGS_OPTIM=-O3 -ffast-math -funroll-loops -fomit-frame-pointer -funsafe-math-optimizations -fgcse-lm -fgcse-sm -fsched-spec-load $(CFLAGS_COMMON)
CFLAGS_DEBUG=-g $(CFLAGS_COMMON)
simu: nvstks_05_05_31.F90
	gfortran nvstks_05_05_31.F90 -o simu $(CFLAGS_OPTIM)
run: simu donnees.dat srectangle.dat
	cat donnees.dat | GFORTRAN_UNBUFFERED_ALL=y G95_UNBUFFERED_ALL=y ./simu | tee out.txt
