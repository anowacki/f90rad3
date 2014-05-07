FC = gfortran
FFLAGS = -O2 -g -fcheck=all -Wall

B = bin
M = mod
O = obj

LIBS = -L/opt/local/lib -lnetcdff

INCS = -I/opt/local/include

PROGS = $B/rad3dump \
        $B/rad3info \
		$B/rad3toncf

MODS = $O/f90rad3.o

OBJS = $O/rad3dump.o \
       $O/rad3info.o \
	   $O/rad3toncf.o

default: $(PROGS)

$B/rad3dump: $(MODS) $O/rad3dump.o
	$(FC) $(FFLAGS) -J$M $(LIBS) $(INCS) -o $@ $^

$B/rad3info: $(MODS) $O/rad3info.o
	$(FC) $(FFLAGS) -J$M $(LIBS) $(INCS) -o $@ $^

$B/rad3toncf: $(MODS) $O/rad3toncf.o
	$(FC) $(FFLAGS) -J$M $(LIBS) $(INCS) -o $@ $^

$O/%.o: %.f90
	$(FC) $(FFLAGS) -c -J$M $(INCS) -o $O/$*.o $*.f90

$(PROGS): $(MODS)

.PHONY: clean

clean:
	/bin/rm -f $O/*.o $M/*.mod