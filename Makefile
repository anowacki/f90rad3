FC = gfortran
FFLAGS = -O2 -g -fcheck=all -Wall

B = bin
M = mod
O = obj
S = src

LIBS = -L/opt/local/lib -lnetcdff

INCS = -I/opt/local/include

PROGS = $B/rad3dump \
        $B/rad3info \
        $B/rad3slice \
        $B/rad3toncf \
        $B/rad3voltoncf

TEST_PROGS = $B/test_save

MODS = $O/f90rad3.o

OBJS = $O/rad3dump.o \
       $O/rad3info.o \
       $O/rad3slice.o \
       $O/rad3toncf.o \
       $O/rad3voltoncf.o

default: $(PROGS)

test: $(TEST_PROGS)

# Build programs
$B/%: $(MODS) $O/%.o
	$(FC) $(FFLAGS) -J$M $(LIBS) $(INCS) -o $@ $^

# Build object files
$O/%.o: src/%.f90
	$(FC) $(FFLAGS) -c -J$M $(INCS) -o $O/$*.o src/$*.f90

$O/%.o: test/%.f90
	$(FC) $(FFLAGS) -c -J$M $(INCS) -o $O/$*.o test/$*.f90
	
# Dependencies
$(PROGS): $(MODS)
$(TEST_PROGS): $(MODS)

$(OBJS): $(MODS)

.PHONY: clean

clean:
	/bin/rm -f $O/*.o $M/*.mod