# Platform specific settings
#-------------------------------------------------------------------------------

# Fortran
#--------
# Compiler command
FC=ftn
# Precision flags (passed to all compilation commands)
FCFLAGS_PREC=-G2 -O2 -Ovector1 -hfp0 -hflex_mp=strict -h omp
# Flag used to set PIC (Position-independent-code; required by dynamic lib 
# and so will only be passed to compile objects destined for the dynamic lib)
FCFLAGS_PIC=-h pic
# Flags used to toggle the building of a dynamic (shared) library
FCFLAGS_SHARED=-shared -L${CRAYLIBS_X86_64} -lomp -lpgas-dmapp
# Flags used for compiling a dynamically linked test executable; in some cases
# control of this is argument order dependent - for these cases the first 
# variable will be inserted before the link commands and the second will be
# inserted afterwards
FCFLAGS_DYNAMIC=-dynamic
FCFLAGS_DYNAMIC_TRAIL=
# Flags used for compiling a statically linked test executable (following the
# same rules as the dynamic equivalents - see above comment)
FCFLAGS_STATIC=
FCFLAGS_STATIC_TRAIL=

# C
#--
# Compiler command
CC=cc
# Precision flags (passed to all compilation commands)
CCFLAGS_PREC=-G2 -O3 -h c99 -h omp
# Flag used to set PIC (Position-independent-code; required by dynamic lib 
# and so will only be passed to compile objects destined for the dynamic lib)
CCFLAGS_PIC=-h pic

# Archiver
#---------
# Archiver command
AR=ar -rc

# Set the name of this platform; this will be included as the name of the 
# top-level directory in the build
PLATFORM=meto-xc40-crayftn-craycc

# Proceed to include the rest of the common makefile
include Makefile