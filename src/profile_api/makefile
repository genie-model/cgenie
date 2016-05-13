all: api_fortran_itt.o

clean:
	-rm api_fortran.o

api_fortran_itt.o: api_fortran_itt.c
	gfortran -c api_fortran_itt.c -I/cm/shared/apps/intel-cluster-studio/vtune_amplifier_xe_2015/include
