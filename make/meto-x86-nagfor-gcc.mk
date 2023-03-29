# Platform specific settings
#-------------------------------------------------------------------------------

# SHUM_OPENMP is tested in this file, but is not set by default until the main
# makefile is included. We therefore need to set a default here.
SHUM_OPENMP ?= true

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
FC=nagfor
# Precision flags (passed to all compilation commands)
FCFLAGS_PREC=
# Flag used to set OpenMP (passed to all compilation commands)
FCFLAGS_OPENMP=
# Flag used to unset OpenMP (passed to all compilation commands)
FCFLAGS_NOOPENMP=
# Any other flags (to be passed to all compilation commands)
FCFLAGS_EXTRA=-C=alias -C=array -C=bits -C=calls -C=dangling -C=do -C=present -C=pointer -C=recursion -ieee=full
# Flag used to set PIC (Position-independent-code; required by dynamic lib
# and so will only be passed to compile objects destined for the dynamic lib)
FCFLAGS_PIC=-pic
# Flags used to toggle the building of a dynamic (shared) library
FCFLAGS_SHARED=-Wl,-shared
# Flags used for compiling a dynamically linked test executable; in some cases
# control of this is argument order dependent - for these cases the first
# variable will be inserted before the link commands and the second will be
# inserted afterwards
FCFLAGS_DYNAMIC=
ifeq (${SHUM_OPENMP}, true)
FCFLAGS_DYNAMIC_TRAIL=-lgomp -Wl,-Wl,,-rpath=${LIBDIR_OUT}/lib
else ifeq (${SHUM_OPENMP}, false)
FCFLAGS_DYNAMIC_TRAIL=-Wl,-Wl,,-rpath=${LIBDIR_OUT}/lib
endif
# Flags used for compiling a statically linked test executable (following the
# same rules as the dynamic equivalents - see above comment)
FCFLAGS_STATIC=-Bstatic
ifeq (${SHUM_OPENMP}, true)
FCFLAGS_STATIC_TRAIL=-Bdynamic -lgomp -Wl,-Wl,,-rpath=${LIBDIR_OUT}/lib
else ifeq (${SHUM_OPENMP}, false)
FCFLAGS_STATIC_TRAIL=-Bdynamic -Wl,-Wl,,-rpath=${LIBDIR_OUT}/lib
endif

# C
#--
# Compiler command
CC=gcc
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
CCFLAGS_NOOPENMP=-Wno-unknown-pragmas -DSHUM_USE_C_OPENMP_VIA_THREAD_UTILS=shum_use_c_openmp_via_thread_utils -D_OPENMP
else ifeq (${SHUM_USE_C_OPENMP_VIA_THREAD_UTILS}, false)
CCFLAGS_NOOPENMP=-Wno-unknown-pragmas
endif

# Any other flags (to be passed to all compilation commands)
CCFLAGS_EXTRA=-std=c99 -Wall -Wextra -Werror -Wformat=2 -Winit-self -Wfloat-equal   \
              -Wpointer-arith -Wbad-function-cast -Wcast-qual -Wcast-align          \
              -Wconversion -Wlogical-op -Wstrict-prototypes -Wmissing-declarations  \
              -Wredundant-decls -Wnested-externs -Woverlength-strings               \
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
PLATFORM=meto-x86-nagfor-gcc

# Proceed to include the rest of the common makefile
include Makefile
