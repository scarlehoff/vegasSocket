EXECUTABLE = run_example

CORE = integrand.f90 main.f90
MODULES = Vegas.f90

OBJDIR = obj
MODDIR = mod
OBJFILES = $(addprefix $(OBJDIR)/, $(patsubst %.f90,%.o, $(CORE)))
MODFILES = $(addprefix $(OBJDIR)/, $(patsubst %.f90,%.o, $(MODULES)))

ifeq ($(FC), ifort)
	LD = ifort
	FFLAGS = -g -qopenmp -fpp -D USE_IFORT -module $(MODDIR)
else
	FC = gfortran
	LD = gfortran
	FFLAGS = -Wall -Og -ggdb -cpp -fopenmp -fbounds-check -J $(MODDIR)
endif

ifeq ($(mode), sockets)
	ifeq ($(FC), ifort)
		LIBPATH = ../lib
		LDFLAGS = -Wl,-rpath,$(LIBPATH)
		FFLAGS += -L$(LIBPATH) -lfortran_interface -D USE_SOCKETS
	else
		LIBPATH = ../lib
		LDFLAGS = -lgfortran -Wl,-rpath,$(LIBPATH)
		FFLAGS += -L$(LIBPATH) -lfortran_interface -D USE_SOCKETS
	endif
endif

.PHONY: all clean dirs

$(OBJDIR)/%.o: %.f90 | dirs
	@echo "Building: $<"
	@$(FC) $(FFLAGS) -c $< -o $@

$(OBJDIR)/%.o: %.f90 | $(MODFILES) dirs
	@echo "Building: $<"
	@$(FC) $(FFLAGS) -c $< -o $@

all: $(MODFILES) $(OBJFILES)
	@echo "Linking into: $(EXECUTABLE)"
	@$(LD) -o $(EXECUTABLE) $^ $(FFLAGS) $(LDFLAGS)

dirs: $(MODDIR) $(OBJDIR)

$(MODDIR) $(OBJDIR):
	@mkdir -p $@

clean:
	@echo "cleaning obj and mod folders"
	@rm -f $(OBJDIR)/* $(MODDIR)/*
