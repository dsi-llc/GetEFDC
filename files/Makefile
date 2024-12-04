# Select the fortran compiler to use
FC=/opt/intel/oneapi/mpi/latest/bin/mpiifx

# Compiler options
#FFLAGS=-g -traceback -fpp -DENABLE_MPI -D_MPI -nofixed -qopenmp -DNCOUT
FFLAGS=-traceback -qopenmp -pthread -O3 -fpp -DENABLE_MPI -D_MPI -DLINUX -real-size 32 -fpe0 -fp-model=source -fp-speculation=safe -multiple-processes=12
# Compilation options for debug mode
#FFLAGS=-g -traceback -qopenmp -debug all -check all -fpp -DENABLE_MPI -D_MPI -DDEBUGGING -fpic -real-size 64 -fpe0 -fp-speculation=safe -multiple-processes=8

# Setup the OpenMP compiler options
OMPFLAGS=#-qopenmp
FPPFLAGS= 

# Preprocessor

# Libs
LIBS=

PROGRAM = getefdc
PROGEXT = .x

PROJHOME= ..

# Suffix rules
.SUFFIXES: .f90 .F90 .for .FOR .o

INCLUD   =   
CDFDIR   =   
CDFLIBS  =  

# Include dependency list created by makedepf90
#include .depend 


# list directories containing other files
SRCS=*.f90 */*.f90 
	  
OBJS= 

	  
OBJSLINK=

  	  	 	  	  	 	 	  	 							 
MODS    = src/globalvars.o src/tecmod.o src/infomod.o src/xyijconv.o src/efdcpromod.o src/geteeoutmod.o src/gethfreqout.o


MODSLINK  =globalvars.o tecmod.o infomod.o xyijconv.o efdcpromod.o geteeoutmod.o gethfreqout.o

MAIN    = src/getefdc.f90


EXEPROG = $(PROGRAM)$(PROGEXT)


###### FOR LINUX #######################################################

$(EXEPROG) : $(OBJS) $(MODS) $(MAIN) 
  
	$(FC) $(FFLAGS) $(CDFDIR) $(CDFLIBS) $(INCLUD) -o  $@  $(OBJSLINK) $(MODSLINK) $(MAIN)


# Recompile only source file of change #################################

.f90.o: 
	$(FC) $(FFLAGS) $(INCLUD) -c $<

.F90.o: 
	$(FC) $(FFLAGS) $(INCLUD) -c $<

.for.o: 
	$(FC) $(FFLAGS) $(INCLUD) -c $<

.FOR.o: 
	$(FC) $(FFLAGS) $(INCLUD) -c $<

### Special dependencies ###############################################

$(OBJS)              : $(MODS)
$(MAIN)              : $(OBJS)
 
clean:
	rm -f *.o *.mod

# Make dependency list
#depend .depend :
#	makedepf90 -W -DENABLE_MPI -o aaefdc $(SRCS) > .depend

