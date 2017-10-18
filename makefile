CC = gcc
FC = gfortran
LD = gcc
FFLAGS = -Og -g -fPIC
LIBPATH="lib"
LDFLAGS = -lgfortran -L$(LIBPATH) -lfortran_interface -Wl,-rpath,$(LIBPATH)

EXAMPLE_EXE = c_example_client f_example_client py_example_server
INTERFACE_LIB = lib/libfortran_interface.so

.PHONY: all

all: examples interface

interface: $(INTERFACE_LIB)
examples: $(EXAMPLE_EXE)

VPATH += examples src


$(INTERFACE_LIB) : fortran_socket_interface.c
	@mkdir -p lib
	@echo "Building Fortran-C interface library"
	@$(CC) $(FFLAGS) -shared -o $@ $^


py_example_server: python_example.py
	@echo "Creating link tp python socket server"
	@ln -s $^ $@
	@chmod +x $@
c_example_client: c_socket_example.c
	@echo "Building example of C socket client"
	@$(CC) $(FFLAGS) -o $@ $^
f_example_client: fortran_socket_example.f90 | interface
	@echo "Building example for the F-python interface"
	@$(FC) $(FFLAGS) $(LDFLAGS) -o $@ $^


fortran_interface.so: fortran_socket_interface.c
	@echo "Building the fortran_interface library"


clean:
	@echo "cleaning ..."
	@rm $(EXAMPLE_EXE) $(INTERFACE_LIB)


