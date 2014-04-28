FC = gfortran
FFLAGS = -O2 -g -fcheck=all -Wall

B = bin
M = mod
O = obj

PROGS = $B/rad3dump \
        $B/rad3info

MODS = $O/f90rad3.o

OBJS = $O/rad3dump.o \
       $O/rad3info.o

default: $(PROGS)

$B/rad3dump: $(MODS) $O/rad3dump.o
	$(FC) $(FFLAGS) -J$M -o $@ $^

$B/rad3info: $(MODS) $O/rad3info.o
	$(FC) $(FFLAGS) -J$M -o $@ $^

$O/%.o: %.f90
	$(FC) $(FFLAGS) -c -J$M -o $O/$*.o $*.f90

$(PROGS): $(MODS)

.PHONY: clean

clean:
	/bin/rm -f $O/*.o $M/*.mod