#
# Makefile for nbody: contains dependencies for nbody program
#
#    make [nbody]          : compile main program
#    make clean            : remove temporary files
#    
#
.SUFFIXES: .f90

F90      := gfortran
F90FLAGS := -O3 -Wall

SRCDIR   := src

SOURCE=parameters.f90 nbody_particles.f90 nbody_io.f90 nbody_integrator.f90
OBJECTS=$(SOURCE:.f90=.o)

all: nbody

nbody: $(OBJECTS)
	$(F90) $(F90FLAGS) $(OBJECTS) -o $@ 

nbody_particles.o: parameters.o nbody_particles.f90

nbody_io.o: parameters.o nbody_particles.o nbody_io.f90

nbody_integrator.o:  parameters.o nbody_particles.o nbody_io.o nbody_integrator.f90

%.o: %.f90
	$(F90) $(F90FLAGS) -c $<

clean:
	rm -f *.o *.mod *~
