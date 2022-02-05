# Copyright : 2022, Fran Martinez Fadrique <Fran.Martinez.Fadrique@gmail.com>
# Project   : Atlantis
# Author    : Fran Martinez Fadrique
# Language  : make
# Synopsis  : Makefile for FTL library build

# License   : This file is part of the Fortran Template Library (FTL).
#
#             FTL is free software: you can redistribute it and/or modify
#             it under the terms of the GNU Lesser General Public License as
#             published by the Free Software Foundation, either version 3 of
#             the License, or (at your option) any later version.
#
#             FTL is distributed in the hope that it will be useful,
#             but WITHOUT ANY WARRANTY; without even the implied warranty of
#             MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
#             See the GNU Lesser General Public License for more details.
#
#             You should have received a copy of the GNU Lesser General Public
#             License along with FTL.  
#             If not, see <http://www.gnu.org/licenses/>.

# Master configuration key through the compiler command
F90COMP = gfortran
F90COMP = /home/fran/gcc/gcc-10/bin/gfortran

# Reference directories
OBJDIR := Debug
MODDIR := Debug
LIBDIR := Debug

# XFunit library
LIBS = $(LIBDIR)/libftl.a

# Option selection for Intel Fortran (ifort)
ifeq ($(findstring ifort, $(F90COMP)), ifort)
OPTIONS = -c -fpp -g -O0 -stand f18 -traceback -check bounds -assume byterecl -I $(MODDIR) -module $(MODDIR) -o $@ $<
AR = ar
endif

# Option selection for GNU Fortran (gfortran)
ifeq ($(findstring gfortran, $(F90COMP)), gfortran)
OPTIONS = -c -cpp -g -O0 -std=f2008 -fbacktrace -fbounds-check -I $(MODDIR) -J $(MODDIR) -o $@ $<
AR = ar
endif

# Automatic generation of list of source files and targets
SRCS_F90 := $(wildcard *.f90)
OBJECTS_F90 := $(SRCS_F90:%.f90=$(OBJDIR)/%.o)

# Main make target (recursive in subdirectories)
all: $(LIBS)
	@cd utest; $(MAKE)

# Target to build the XFunit library only
libs: $(LIBS)

# Build library from objects
$(LIBS): $(OBJECTS_F90)
	@echo "(AR)" $(LIBS)
	@$(AR) -r $@ $(OBJECTS_F90)
    
# Build objects from sources
$(OBJDIR)/%.o: %.f90
	@if [ ! -d "$(OBJDIR)" ]; then mkdir -p $(OBJDIR); fi
	@echo "(FC)" $@
	@$(F90COMP) $(OPTIONS)
	
# Target for unit test execution (recursive in subdirectories)
units:
	@$(MAKE); cd utest; $(MAKE) units

# Target for directory clean up (recursive in subdirectories)
clean:
	@echo "(RM)" $(OBJDIR)"/*.o"
	@rm -f $(OBJDIR)/*.o
	@echo "(RM)" $(MODDIR)"/*.mod"
	@rm -f $(MODDIR)/*.mod
	@cd utest; $(MAKE) clean

# Test target
test:
	echo $(OPTIONS)
	echo $(findstring gfortran, $(F90COMP))

# Include dependencies for individual sources/objects for compilation order
include makefile.deps
