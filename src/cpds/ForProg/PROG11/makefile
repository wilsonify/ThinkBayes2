MODULE=prog11
FC := /usr/bin/gfortran
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
	@echo Cleaning binary files
	@rm -rf *.1