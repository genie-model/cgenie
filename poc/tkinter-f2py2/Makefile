# A simple makefile using f2py

all: cupcake.so

cupcake.so: model.f90
	f2py -c -m cupcake $^

.PHONY:		all clean spotless

clean: spotless
spotless:
		\rm -f *.so
