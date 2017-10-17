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
LDFLAGS = -lgfortran

.PHONY: all clean dirs

$(MODDIR)/%.o: %.f90 | dirs
	@echo "Building: $<"
	@$(FC) $(FFLAGS) -c $< -o $@

$(OBJDIR)/%.o: %.f90 | $(MODFILES) dirs
	@echo "Building: $<"
	@$(FC) $(FFLAGS) -c $< -o $@

all: $(MODFILES) $(OBJFILES)
	@echo "Linking into: $(EXECUTABLE)"
	@$(LD) -o $(EXECUTABLE) $^ $(LDFLAGS)

dirs: $(MODDIR) $(OBJDIR)

$(MODDIR) $(OBJDIR):
	@mkdir -p $@

clean:
	@echo "cleaning obj and mod folders"
	@rm -f $(OBJDIR)/* $(MODDIR)/*
