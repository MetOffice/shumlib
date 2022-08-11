# Platform specific settings
#-------------------------------------------------------------------------------

# Make
#-----
# Make command
MAKE=make

# Note that on the EX1A only Dynamic linking is supported
SHUM_BUILD_STATIC=false

# To prevent spiral search being compiled with a multithreaded
# library it can't use, set openmp off for Cray EX1A builds
SHUM_OPENMP=false

# Fortran
#--------
FPP=cpp
# Any flags required to make the preprocessor function correctly
FPPFLAGS_BASE=-C -P -undef -nostdinc
# Any other flags (to be passed to all preprocessing commands)
FPPFLAGS_EXTRA=-Wall -Wtraditional -Werror -fdiagnostics-show-option
# IEEE Arithmetic
SHUM_HAS_IEEE_ARITHMETIC ?= true
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
FCFLAGS_EXTRA ?= -O2 -Ovector1 -hfp0 -hflex_mp=strict -hipa1 -hnopgas_runtime  \
                 -hnocaf -herror_on_warning -M E287,E5001
# Flags to set paths for the dynamic library builds
LDFLAGS= -Wl,-rpath,/opt/cray/pe/gcc-libs
# Flag used to set PIC (Position-independent-code; required by dynamic lib
# and so will only be passed to compile objects destined for the dynamic lib)
FCFLAGS_PIC=-h pic ${LDFLAGS}
# Flags used to toggle the building of a dynamic (shared) library
FCFLAGS_SHARED=-shared -L${CRAYLIBS_X86_64} -lmodules

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
SHUM_USE_C_OPENMP_VIA_THREAD_UTILS ?= false
ifeq (${SHUM_USE_C_OPENMP_VIA_THREAD_UTILS}, true)
CCFLAGS_OPENMP=-fopenmp -DSHUM_USE_C_OPENMP_VIA_THREAD_UTILS=shum_use_c_openmp_via_thread_utils
else ifeq (${SHUM_USE_C_OPENMP_VIA_THREAD_UTILS}, false)
CCFLAGS_OPENMP=-fopenmp
endif
# Flag used to unset OpenMP (passed to all compilation commands)
ifeq (${SHUM_USE_C_OPENMP_VIA_THREAD_UTILS}, true)
CCFLAGS_NOOPENMP=-DSHUM_USE_C_OPENMP_VIA_THREAD_UTILS=shum_use_c_openmp_via_thread_utils -D_OPENMP
else ifeq (${SHUM_USE_C_OPENMP_VIA_THREAD_UTILS}, false)
CCFLAGS_NOOPENMP=
endif

# Any other flags (to be passed to all compilation commands)
CCFLAGS_EXTRA=-std=c99 -Weverything -Wno-vla -Wno-padded  \
              -Wno-missing-noreturn -pedantic  \
              -fdiagnostics-show-option

# Flag used to set PIC (Position-independent-code; required by dynamic lib
# and so will only be passed to compile objects destined for the dynamic lib)
CCFLAGS_PIC=-fPIC

# Archiver
#---------
# Archiver command
AR=ar -rc

# Set the name of this platform; this will be included as the name of the
# top-level directory in the build
PLATFORM=meto-ex1a-crayftn-craycc

# Proceed to include the rest of the common makefile
include Makefile
