# Platform specific settings
#-------------------------------------------------------------------------------

# Make
#-----
# Make command
MAKE=make

# Fortran
#--------
FPP=cpp
# Any flags required to make the preprocessor function correctly
FPPFLAGS_BASE=-C -P -undef -nostdinc
# Any other flags (to be passed to all preprocessing commands)
FPPFLAGS_EXTRA=-Wall -Wtraditional -Werror -fdiagnostics-show-option
# IEEE Arithmetic
SHUM_HAS_IEEE_ARITHMETIC ?= false
ifeq (${SHUM_HAS_IEEE_ARITHMETIC}, true)
FPPFLAGS_IEEE=-DHAS_IEEE_ARITHMETIC
else ifeq (${SHUM_HAS_IEEE_ARITHMETIC}, false)
FPPFLAGS_IEEE=
endif
SHUM_EVAL_NAN_BY_BITS ?= true
ifeq (${SHUM_EVAL_NAN_BY_BITS}, true)
FPPFLAGS_ENBB=-DEVAL_NAN_BY_BITS
else ifeq (${SHUM_EVAL_NAN_BY_BITS}, false)
FPPFLAGS_ENBB=
endif
SHUM_EVAL_DENORMAL_BY_BITS ?= true
ifeq (${SHUM_EVAL_DENORMAL_BY_BITS}, true)
FPPFLAGS_EDBB=-DEVAL_DENORMAL_BY_BITS
else ifeq (${SHUM_EVAL_DENORMAL_BY_BITS}, false)
FPPFLAGS_EDBB=
endif
# Combine the preprocessor flags
FPPFLAGS=${FPPFLAGS_BASE} ${FPPFLAGS_IEEE} ${FPPFLAGS_ENBB} ${FPPFLAGS_EDBB} ${FPPFLAGS_EXTRA}
# Compiler command
FC=ftn
# Precision flags (passed to all compilation commands)
FCFLAGS_PREC=
# Flag used to set OpenMP (passed to all compilation commands)
FCFLAGS_OPENMP=-h omp
# Flag used to unset OpenMP (passed to all compilation commands)
FCFLAGS_NOOPENMP=-h noomp
# Any other flags (to be passed to all compilation commands)
FCFLAGS_EXTRA=-O2 -Ovector1 -hfp0 -hflex_mp=strict -hipa1 -hnopgas_runtime -hnocaf -herror_on_warning
# Flag used to set PIC (Position-independent-code; required by dynamic lib
# and so will only be passed to compile objects destined for the dynamic lib)
FCFLAGS_PIC=-h pic
# Flags used to toggle the building of a dynamic (shared) library
FCFLAGS_SHARED=-shared -L${CRAYLIBS_X86_64} -lomp
ifdef SHUM_OPENMP
ifeq (${SHUM_OPENMP}, false)
FCFLAGS_SHARED=-shared
endif
endif
# Flags used for compiling a dynamically linked test executable; in some cases
# control of this is argument order dependent - for these cases the first
# variable will be inserted before the link commands and the second will be
# inserted afterwards
FCFLAGS_DYNAMIC=-dynamic
FCFLAGS_DYNAMIC_TRAIL=-Wl,-rpath=${LIBDIR_OUT}/lib
# Flags used for compiling a statically linked test executable (following the
# same rules as the dynamic equivalents - see above comment)
FCFLAGS_STATIC=-static
FCFLAGS_STATIC_TRAIL=

# C
#--
# Compiler command
CC=cc
# Precision flags (passed to all compilation commands)
CCFLAGS_PREC=
# Flag used to set OpenMP (passed to all compilation commands)
SHUM_USE_C_OPENMP_VIA_THREAD_UTILS ?= false
CCFLAGS_OPENMP=-h omp
# Flag used to unset OpenMP (passed to all compilation commands)
CCFLAGS_NOOPENMP=-h noomp
# Any other flags (to be passed to all compilation commands)
CCFLAGS_EXTRA=-O3 -h c99 -hconform -hstdc -hnotolerant -hnognu -hnopgas_runtime -herror_on_warning
# Flag used to set PIC (Position-independent-code; required by dynamic lib
# and so will only be passed to compile objects destined for the dynamic lib)
CCFLAGS_PIC=-h pic

# Archiver
#---------
# Archiver command
AR=ar -rc

# Set the name of this platform; this will be included as the name of the
# top-level directory in the build
PLATFORM=niwa-xc50-crayftn-craycc

# Proceed to include the rest of the common makefile
include Makefile
