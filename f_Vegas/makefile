FC = gfortran
LD = gfortran
EXECUTABLE = run_example

CORE = integrand.f90 main.f90
MODULES = Vegas.f90

OBJDIR = obj
MODDIR = mod
OBJFILES = $(addprefix $(OBJDIR)/, $(patsubst %.f90,%.o, $(CORE)))
MODFILES = $(addprefix $(MODDIR)/, $(patsubst %.f90,%.o, $(MODULES)))

FFLAGS = -Wall -Og -g -cpp -fno-automatic -fbounds-check -J $(OBJDIR)

ifeq ($(mode), sockets)
	LIBPATH = ../lib
	LDFLAGS = -lgfortran -Wl,-rpath,$(LIBPATH)
	FFLAGS += -L$(LIBPATH) -lfortran_interface -D USE_SOCKETS
endif

.PHONY: all clean dirs

$(MODDIR)/%.o: %.f90 | dirs
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
