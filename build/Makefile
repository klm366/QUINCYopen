#!> makefile
#!>
#!> QUINCY standalone
#!>
#!> ---------------------------------------
#!> Copyright (C) 2013-2024, MPI-M, MPI-BGC
#!>
#!> Contact: https://doi.org/10.17871/quincy-model-2019
#!> Authors: AUTHORS.md
#!> See LICENSES/ for license information
#!> SPDX-License-Identifier: BSD-3-Clause
#!> ---------------------------------------
#!>
#!>#### makefile for compiling quincy
#!>

#
# main folder is main-repo-dir/build
# with Makefile location: main-repo-dir/build/Makefile
# compiler specific folders will be created during build process
#
#---------------------------------------------------------------------
# include compiler specific options
include ./make_compile-options.incl
#---------------------------------------------------------------------
# COMP = x86_64-intel or x86_64-gfortran
# CDIR = ../COMP
CDIR = $(addprefix ../,$(COMP))
# LIBDIR main-repo-dir/COMP/build/lib
LIBDIR = $(addsuffix /build/lib,$(CDIR))
# MODDIR main-repo-dir/COMP/build/mod
MODDIR = $(addsuffix /build/mod,$(CDIR))
# OBJDIR = main-repo-dir/COMP/build/mod
OBJDIR = $(MODDIR)
# BDIR = main-repo-dir/COMP/bin
BDIR = $(addsuffix /bin,$(CDIR))
# $(strip string)  Removes leading and trailing whitespace from string and replaces each internal sequence of one or more whitespace characters with a single space.
BINDIR = $(strip $(BDIR))
# if not present, create following folder plus parent folders
$(shell [ ! -d $(MODDIR) ] && mkdir -p $(MODDIR))
$(shell [ ! -d $(LIBDIR) ] && mkdir -p $(LIBDIR))
$(shell [ ! -d $(BINDIR) ] && mkdir -p $(BINDIR))
#---------------------------------------------------------------------
#-
LIBRARY = $(LIBDIR)/libslm.a
#---------------------------------------------------------------------
RM      = rm -f
STRIP   = strip
SIZE    = size
AR      = ar -r
#---------------------------------------------------------------------
# The VPATH is a list of directories to be searched for missing source files.
# The list can be separated by spaces or colons - the best approach is probably to use spaces as this works across all platforms.
VPATH = src_dsl4jsb/quincy_standalone \
	src_dsl4jsb/atmland \
	 src_dsl4jsb/icon_adapter \
	 src_dsl4jsb/base \
	 src_dsl4jsb/drivers \
	 src_dsl4jsb/shared \
	 src_dsl4jsb/q_agriculture \
	 src_dsl4jsb/q_assimilation \
	 src_dsl4jsb/radiation \
	 src_dsl4jsb/turbulence \
	 src_dsl4jsb/hydrology \
	 src_dsl4jsb/soil_snow_energy \
	 src_dsl4jsb/srf_energy_bal \
	 src_dsl4jsb/q_simple_harvest \
	 src_dsl4jsb/q_soil_phys \
	 src_dsl4jsb/q_soil_biogeochemistry \
	 src_dsl4jsb/q_phenology \
	 src_dsl4jsb/q_vegetation \
	 src_dsl4jsb/dist_fire \
	 src_dsl4jsb/dist_insect \
	 src_dsl4jsb/plant_hydraulics\
	 $(OBJDIR)
# 
MODS = $(shell find src_dsl4jsb/quincy_standalone/ -name '*.f90' -print)
MODS += $(shell find src_dsl4jsb/atmland/ -name '*.f90' -print)
MODS += $(shell find src_dsl4jsb/icon_adapter/ -name '*.f90' -print)
MODS += $(shell find src_dsl4jsb/base/ -name '*.f90' -print)
MODS += $(shell find src_dsl4jsb/drivers/ -name 'mo*.f90' -print)
MODS += $(shell find src_dsl4jsb/shared/ -name '*.f90' -print)
MODS += $(shell find src_dsl4jsb/q_agriculture/ -name '*.f90' -print)
MODS += $(shell find src_dsl4jsb/q_assimilation/ -name '*.f90' -print)
MODS += $(shell find src_dsl4jsb/radiation/ -name '*.f90' -print)
MODS += $(shell find src_dsl4jsb/turbulence/ -name '*.f90' -print)
MODS += $(shell find src_dsl4jsb/hydrology/ -name '*.f90' -print)
MODS += $(shell find src_dsl4jsb/soil_snow_energy/ -name '*.f90' -print)
MODS += $(shell find src_dsl4jsb/srf_energy_bal/ -name '*.f90' -print)
MODS += $(shell find src_dsl4jsb/q_simple_harvest/ -name '*.f90' -print)
MODS += $(shell find src_dsl4jsb/q_soil_phys/ -name '*.f90' -print)
MODS += $(shell find src_dsl4jsb/q_soil_biogeochemistry/ -name '*.f90' -print)
MODS += $(shell find src_dsl4jsb/q_phenology/ -name '*.f90' -print)
MODS += $(shell find src_dsl4jsb/q_vegetation/ -name '*.f90' -print)
MODS += $(shell find src_dsl4jsb/dist_fire/ -name '*.f90' -print)
MODS += $(shell find src_dsl4jsb/dist_insect/ -name '*.f90' -print)
MODS += $(shell find src_dsl4jsb/plant_hydraulics/ -name '*.f90' -print)

# 'notdir' takes away the path from the file name leaving just the file name (so /x/y/foo.a becomes foo.a)
OBJFILES = $(notdir $(MODS))
OBJSMODS = $(OBJFILES:.f90=.o)
.SUFFIXES: .o .f90


# main target of make; removes all main-repo-dir/build/*.o files after build process has finished successfully
all: $(BINDIR)/land.x
	@rm -f ./*.o

# rules for creating the land.x binary
$(BINDIR)/land.x: $(LIBRARY) quincy_qs_model_driver.f90
	$(F_C) $(F_O) src_dsl4jsb/drivers/quincy_qs_model_driver.f90
	$(F_L) -o $(BINDIR)/land.x quincy_qs_model_driver.o $(LIBRARY)
	@$(RM) quincy_qs_model_driver.o
	@echo -e "\n  land.x binary was built successfully"

# create libslm.a from .o files
$(LIBRARY): $(OBJSMODS)
	@cp $(OBJDIR)/*.o ./
	$(AR) $@ $(OBJSMODS)

# removes all .o .mod files and the libslm.a
clean:
	$(RM) $(LIBRARY) $(MODDIR)/*.mod $(OBJDIR)/*.o ./*.o
	@echo -e "\n  all *.o and *.mod files and the libslm.a were removed"

help:
	@echo "## Commands available with make ##"
	@echo " "
	@echo " help:              this help" 
	@echo " all (or nothing):  stand alone model (land.x binary including all SLM models)" 
	@echo " clean:             clean up library files" 

# create .o files from .f90 files and copy to OBJDIR which is in the VPATH so that only changed/new .f90 files have to be compiled
%.o: %.f90
	$(F_C) $(F_O) $<
	@cp $*.o $(OBJDIR)

#
# All dependencies
#
include ../src/icon_adapter/Make.inc.deps
include ../src/base/Make.inc.deps
include ../src/atmland/Make.inc.deps
include ../src/shared/Make.inc.deps
include ../src/q_agriculture/Make.inc.deps
include ../src/q_assimilation/Make.inc.deps
include ../src/radiation/Make.inc.deps
include ../src/turbulence/Make.inc.deps
include ../src/hydrology/Make.inc.deps
include ../src/soil_snow_energy/Make.inc.deps
include ../src/srf_energy_bal/Make.inc.deps
include ../src/q_simple_harvest/Make.inc.deps
include ../src/q_soil_phys/Make.inc.deps
include ../src/q_soil_biogeochemistry/Make.inc.deps
include ../src/q_phenology/Make.inc.deps
include ../src/q_vegetation/Make.inc.deps
include ../src/drivers/Make.inc.deps
include ../src/quincy_standalone/Make.inc.deps
include ../src/dist_fire/Make.inc.deps
include ../src/dist_insect/Make.inc.deps
include ../src/plant_hydraulics/Make.inc.deps
