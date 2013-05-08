VPATH=spmat
.SUFFIXES: .f90
.PHONY: clean
FC=gfortran
FCFLAG=

OBJECT =\
	matcrs.o\
	op3pt.o\
	op9pt.o\
	op27pt.o\
	matgen.o

TARGET = matgen.exe

$(TARGET): $(OBJECT)
	$(FC) -o $@ $(OBJECT) $(FCFLAG)

.f.o:
	$(FC) -c $<

.f90.o:
	$(FC) -c $<

clean:
	rm -f *.exe *.o *.mod *~
