# Platform specific settings
#-------------------------------------------------------------------------------

# Make
#-----
# Make command
MAKE=make

# Fortran
#--------
# Compiler command
FC=ifort
# Precision flags (passed to all compilation commands)
FCFLAGS_PREC=-O2 -fp-model precise
# Flag used to set OpenMP (passed to all compilation commands)
FCFLAGS_OPENMP=-openmp
# Flag used to unset OpenMP (passed to all compilation commands)
FCFLAGS_NOOPENMP=
# Any other flags (to be passed to all compilation commands)
FCFLAGS_EXTRA=-standard-semantics -assume nostd_mod_proc_name -std03
# Flag used to set PIC (Position-independent-code; required by dynamic lib 
# and so will only be passed to compile objects destined for the dynamic lib)
FCFLAGS_PIC=-fPIC
# Flags used to toggle the building of a dynamic (shared) library
FCFLAGS_SHARED=-shared
# Flags used for compiling a dynamically linked test executable; in some cases
# control of this is argument order dependent - for these cases the first 
# variable will be inserted before the link commands and the second will be
# inserted afterwards
FCFLAGS_DYNAMIC=
FCFLAGS_DYNAMIC_TRAIL=-Wl,-rpath=${LIBDIR_OUT}/lib
# Flags used for compiling a statically linked test executable (following the
# same rules as the dynamic equivalents - see above comment)
FCFLAGS_STATIC=-Bstatic
FCFLAGS_STATIC_TRAIL=

# C
#--
# Compiler command
CC=icc
# Precision flags (passed to all compilation commands)
CCFLAGS_PREC=
# Flag used to set OpenMP (passed to all compilation commands)
CCFLAGS_OPENMP=-qopenmp
# Flag used to unset OpenMP (passed to all compilation commands)
CCFLAGS_NOOPENMP=
# Any other flags (to be passed to all compilation commands)
CCFLAGS_EXTRA=
# Flag used to set PIC (Position-independent-code; required by dynamic lib 
# and so will only be passed to compile objects destined for the dynamic lib)
CCFLAGS_PIC=-fPIC

# Archiver
#---------
# Archiver command
AR=ar -rc

# Set the name of this platform; this will be included as the name of the 
# top-level directory in the build
PLATFORM=nci-x86-ifort-icc

# Proceed to include the rest of the common makefile
include Makefile
