CC = gcc
FC = gfortran
LD = gcc
FFLAGS = -Og -g -fPIC
LDFLAGS = -lgfortran -L. -lfortran_interface

EXAMPLE_EXE = c_example f_example
INTERFACE_LIB = libfortran_interface.so

.PHONY: all

all: examples interface

interface: $(INTERFACE_LIB)
examples: $(EXAMPLE_EXE)


c_example: c_socket_example.c
	@echo "Building example of C socket client"
	@$(CC) $(FFLAGS) -o $@ $^

$(INTERFACE_LIB) : fortran_socket_interface.c
	@echo "Building Fortran-C interface library"
	@$(CC) $(FFLAGS) -shared -o $@ $^

f_example: fortran_socket_example.f90 | interface
	@echo "Building example for the F-python interface"
	@$(FC) $(FFLAGS) $(LDFLAGS) -o $@ $^


fortran_interface.so: fortran_socket_interface.c
	@echo "Building the fortran_interface library"


clean:
	@echo "cleaning ..."
	@rm $(EXAMPLE_EXE) $(INTERFACE_LIB) 


