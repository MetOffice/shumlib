# Platform specific settings
#-------------------------------------------------------------------------------

# Make
#-----
# Make command
MAKE=make

# Fortran
#--------
# Compiler command
FC=ftn
# Precision flags (passed to all compilation commands)
FCFLAGS_PREC=-fp-model precise
# Flag used to set OpenMP (passed to all compilation commands)
FCFLAGS_OPENMP=-openmp
# Flag used to unset OpenMP (passed to all compilation commands)
FCFLAGS_NOOPENMP=
# Any other flags (to be passed to all compliation commands)
FCFLAGS_EXTRA=-standard-semantics -assume nostd_mod_proc_name -std03 -msse4.2
# Flag used to set PIC (Position-independent-code; required by dynamic lib 
# and so will only be passed to compile objects destined for the dynamic lib)
FCFLAGS_PIC=-fPIC
# Flags used to toggle the building of a dynamic (shared) library
FCFLAGS_SHARED=-shared
# Flags used for compiling a dynamically linked test executable; in some cases
# control of this is argument order dependent - for these cases the first 
# variable will be inserted before the link commands and the second will be
# inserted afterwards
FCFLAGS_DYNAMIC=-dynamic
FCFLAGS_DYNAMIC_TRAIL=-Wl,-rpath=${LIBDIR_OUT}/lib
# Flags used for compiling a statically linked test executable (following the
# same rules as the dynamic equivalents - see above comment)
FCFLAGS_STATIC=
FCFLAGS_STATIC_TRAIL=

# C
#--
# Compiler command
CC=cc
# Precision flags (passed to all compilation commands)
CCFLAGS_PREC=
# Flag used to set OpenMP (passed to all compilation commands)
CCFLAGS_OPENMP=-qopenmp
# Flag used to unset OpenMP (passed to all compilation commands)
CCFLAGS_NOOPENMP=-diag-disable 3180
# Any other flags (to be passed to all compilation commands)
CCFLAGS_EXTRA=-std=c99 -w3 -Werror-all -no-inline-max-size -msse4.2
# Flag used to set PIC (Position-independent-code; required by dynamic lib 
# and so will only be passed to compile objects destined for the dynamic lib)
CCFLAGS_PIC=-fPIC

# Archiver
#---------
# Archiver command
AR=ar -rc

# Set the name of this platform; this will be included as the name of the 
# top-level directory in the build
PLATFORM=ncas-xc30-ifort-icc

# Proceed to include the rest of the common makefile
include Makefile