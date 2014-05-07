# Make and run the tests
FC = gfortran
FFLAGS = -O0 -g -fcheck=all -Wall

B = ../bin
M = ../mod
O = ../obj

PROGS = $B/test_save

MODS = $O/f90rad3.o

OBJS = $O/test_save

default: $(PROGS)

$B/test_save: $(MODS) $O/test_save.o
	$(FC) $(FFLAGS) -J$M -o $@ $^

$O/%.o: %.f90
	$(FC) $(FFLAGS) -c -J$M -o $O/$*.o $*.f90

$(PROGS): $(MODS)

.PHONY: clean

clean:
	/bin/rm -f $O/*.o $M/*.mod