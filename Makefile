# Main makefile storing common options for building all libraries, by triggering
# individual makefiles from each library directory
#--------------------------------------------------------------------------------

# The intention is that the user points "make" at one of the platform specific
# files, which include this file at the end; here we try to ensure that this
# has happened (if it hasn't, various important variables will not be set)
ifndef PLATFORM
$(error Platform file not loaded, re-run as "make -f" providing a file from \
the "make" subdirectory as an argument)
endif

# Setup destination directory - this can be overidden by the user if they wish
# to install directly to a different location
LIBDIR_ROOT ?= ${PWD}/build
LIBDIR_OUT ?= ${LIBDIR_ROOT}/${PLATFORM}
export

# Setup the flags which will be passed to all compilations - add the openMP
# flags based on the setting below (defaults to true)
SHUM_OPENMP ?= true
ifeq (${SHUM_OPENMP}, true)
FCFLAGS=${FCFLAGS_PREC} ${FCFLAGS_OPENMP} ${FCFLAGS_EXTRA}
CCFLAGS=${CCFLAGS_PREC} ${CCFLAGS_OPENMP} ${CCFLAGS_EXTRA}
else ifeq (${SHUM_OPENMP}, false)
FCFLAGS=${FCFLAGS_PREC} ${FCFLAGS_EXTRA}
CCFLAGS=${CCFLAGS_PREC} ${CCFLAGS_EXTRA}
else
$(error If specified in the environment SHUM_OPENMP environment variable must \
be set to either "true" or "false")
endif

# Default target - build all available libraries
#--------------------------------------------------------------------------------
.PHONY: default 
default: libs

# Output directories
#--------------------------------------------------------------------------------
OUTDIRS=${LIBDIR_OUT}/lib ${LIBDIR_OUT}/include 
${OUTDIRS}:
	mkdir -p ${LIBDIR_OUT}/lib
	mkdir -p ${LIBDIR_OUT}/include

# Precision Bomb (a C-header which will protect against compilation on
# platforms where the assumptions about precision made in the libraries is
# invalid)
#--------------------------------------------------------------------------------
PBOMB_DIR=${PWD}/pbomb

# Libraries
#--------------------------------------------------------------------------------

# String conv
#------------
STR_CONV=shum_string_conv
${STR_CONV}: ${OUTDIRS}
	${MAKE} -C ${STR_CONV}/src

# Byte-swapping
#--------------
BSWAP=shum_byteswap
${BSWAP}: ${STR_CONV} ${OUTDIRS} 
	${MAKE} -C ${BSWAP}/src
${BSWAP}_tests: fruit ${BSWAP}
	${MAKE} -C ${BSWAP}/test

# Data conv
#----------
DATA_CONV=shum_data_conv
${DATA_CONV}: ${STR_CONV} ${OUTDIRS}
	${MAKE} -C ${DATA_CONV}/src

# WGDOS packing
#--------------
PACK=shum_wgdos_packing
${PACK}: ${STR_CONV} ${OUTDIRS}
	${MAKE} -C ${PACK}/src
${PACK}_tests: fruit ${PACK}
	${MAKE} -C ${PACK}/test

ALL_LIBS=${BSWAP} ${STR_CONV} ${DATA_CONV} ${PACK}
.PHONY: libs ${ALL_LIBS} $(addsuffix, _test, ${ALL_LIBS})

# Add a target which points to all libraries
libs: ${ALL_LIBS}

# FRUIT testing control
#--------------------------------------------------------------------------------
# We want the "test" target to turn on building of the tests. It is actually 
# the variable "FRUIT_TESTS" which controls this (since a make target cannot
# act as a switch in this way)
.PHONY: test

# Names of libraries which are currently compiled in output directory
LIBFILES := $(wildcard ${LIBDIR_OUT}/lib/*.so)
LIBNAMES := $(filter-out fruit, $(patsubst lib%.so, %, $(notdir $(LIBFILES))))

# Names of libraries which defined FRUIT tests
FRUITFILES := $(wildcard ${PWD}/shum_*/test/fruit_test_*.f90)
FRUITNAMES := $(patsubst fruit_test_%.f90, %, $(notdir $(FRUITFILES)))

# Intersection of the above - i.e. names of libraries which are both compiled
# and have tests available (so we can compile only tests that actually exist)
FRUITTESTS := $(foreach libname, ${LIBNAMES}, $(filter ${libname}, ${FRUITNAMES}))

# Since building the tests is invalid unless some libraries have been built, 
# don't try to proceed if none exist
OUTDIR_TESTS=${LIBDIR_OUT}/tests

ifneq ($(filter test, $(MAKECMDGOALS)),)
ifndef LIBNAMES
test:
$(error Unable to build tests - at least one library must have been built \
before calling with the "test" target)
else
${OUTDIR_TESTS}:
	mkdir -p ${LIBDIR_OUT}/tests

test: ${OUTDIR_TESTS} $(addsuffix _tests, ${FRUITTESTS})
	${MAKE} -f ${FRUIT}/Makefile-driver
	${LIBDIR_OUT}/tests/fruit_tests_static.exe
	${LIBDIR_OUT}/tests/fruit_tests_dynamic.exe
endif
endif

# The FRUIT source itself
#------------------------
FRUIT=fruit
.PHONY: fruit
fruit: ${OUTDIRS}
	${MAKE} -C ${FRUIT}

# Cleanup targets
#--------------------------------------------------------------------------------
.PHONY: clean clean-temp clean-build 
clean-temp:
	${MAKE} -C ${BSWAP}/src clean
	${MAKE} -C ${BSWAP}/test clean
	${MAKE} -C ${STR_CONV}/src clean
	${MAKE} -C ${DATA_CONV}/src clean
	${MAKE} -C ${PACK}/src clean
	${MAKE} -C ${PACK}/test clean
	${MAKE} -C ${FRUIT} clean
	${MAKE} -f ${FRUIT}/Makefile-driver clean

clean-build: 
	rm -rf ${OUTDIRS} ${OUTDIR_TESTS}
	rm -rf ${LIBDIR_OUT}
	rmdir ${LIBDIR_ROOT} || :

clean: clean-temp clean-build 




