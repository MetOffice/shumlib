# Platform specific settings
#-------------------------------------------------------------------------------

# Make
#-----
# Make command
MAKE=gmake

# Fortran
#--------
# Compiler command
FC=xlf2003_r
# Precision flags (passed to all compilation commands)
FCFLAGS_PREC=
# Flag used to set OpenMP (passed to all compilation commands)
FCFLAGS_OPENMP=-qsmp=omp
# Flag used to unset OpenMP (passed to all compilation commands)
FCFLAGS_NOOPENMP=
# Any other flags (to be passed to all compilation commands)
FCFLAGS_EXTRA=
# Flag used to set PIC (Position-independent-code; required by dynamic lib 
# and so will only be passed to compile objects destined for the dynamic lib)
FCFLAGS_PIC=-qmkshrobj
# Flags used to toggle the building of a dynamic (shared) library
FCFLAGS_SHARED=-G
# Flags used for compiling a dynamically linked test executable; in some cases
# control of this is argument order dependent - for these cases the first 
# variable will be inserted before the link commands and the second will be
# inserted afterwards
FCFLAGS_DYNAMIC=-brtl
FCFLAGS_DYNAMIC_TRAIL=
# Flags used for compiling a statically linked test executable (following the
# same rules as the dynamic equivalents - see above comment)
FCFLAGS_STATIC=
FCFLAGS_STATIC_TRAIL=

# C
#--
# Compiler command
CC=xlc_r
# Precision flags (passed to all compilation commands)
CCFLAGS_PREC=
# Flag used to set OpenMP (passed to all compilation commands)
CCFLAGS_OPENMP=-qsmp=omp
# Flag used to unset OpenMP (passed to all compilation commands)
CCFLAGS_NOOPENMP=
# Any other flags (to be passed to all compilation commands)
CCFLAGS_EXTRA=
# Flag used to set PIC (Position-independent-code; required by dynamic lib 
# and so will only be passed to compile objects destined for the dynamic lib)
CCFLAGS_PIC=-qmkshrobj

# Archiver
#---------
# Archiver command
AR=ar -rc

# Set the name of this platform; this will be included as the name of the 
# top-level directory in the build
PLATFORM=niwa-pwr6-xlf-xlc

# Proceed to include the rest of the common makefile
include Makefile