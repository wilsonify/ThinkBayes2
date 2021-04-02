# program name
# set MODULE from cl; for example:
# MODULE=hello_world make -f makefile
FC := /usr/bin/gfortran # /usr/bin/f77
FCFLAGS = # -fdefault-real-8 -fbacktrace -fno-align-commons -fbounds-check
FFFLAGS =
.PHONY: paths all clean test

$(MODULE).out: $(MODULE).o
	$(FC) $(FFFLAGS) $^ -o $@

$(MODULE).o: $(MODULE).f
	$(FC) -c $(FCFLAGS) $^

all: $(MODULE).out

clean:
	@echo Cleaning object files
	@rm -rf *.o
	@echo Cleaning binary files
	@rm -rf *.out