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
# Drhook dummy
#-------------
HOOK=shum_drhook_dummy
${HOOK}: ${OUTDIRS}
	make -C ${HOOK}/src

# String conv
#------------
STR_CONV=shum_string_conv
${STR_CONV}: ${OUTDIRS}
	make -C ${STR_CONV}/src

# Byte-swapping
#--------------
BSWAP=shum_byteswap
${BSWAP}: ${STR_CONV} ${OUTDIRS} 
	make -C ${BSWAP}/src
${BSWAP}_tests: fruit ${BSWAP}
	make -C ${BSWAP}/test

# Data conv
#----------
DATA_CONV=shum_data_conv
${DATA_CONV}: ${STR_CONV} ${OUTDIRS}
	make -C ${DATA_CONV}/src

# WGDOS packing
#--------------
PACK=shum_wgdos_packing
${PACK}: ${STR_CONV} ${HOOK} ${DATA_CONV} ${OUTDIRS}
	make -C ${PACK}/src
${PACK}_tests: fruit ${PACK}
	make -C ${PACK}/test

ALL_LIBS=${BSWAP} ${HOOK} ${STR_CONV} ${DATA_CONV} ${PACK}
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
	make -f ${FRUIT}/Makefile-driver
	${LIBDIR_OUT}/tests/fruit_tests_static.exe
	${LIBDIR_OUT}/tests/fruit_tests_dynamic.exe
endif
endif

# The FRUIT source itself
#------------------------
FRUIT=fruit
.PHONY: fruit
fruit: ${OUTDIRS}
	make -C ${FRUIT}

# Cleanup targets
#--------------------------------------------------------------------------------
.PHONY: clean clean-temp clean-build 
clean-temp:
	make -C ${BSWAP}/src clean
	make -C ${BSWAP}/test clean
	make -C ${HOOK}/src clean
	make -C ${STR_CONV}/src clean
	make -C ${DATA_CONV}/src clean
	make -C ${PACK}/src clean
	make -C ${PACK}/test clean
	make -C ${FRUIT} clean
	make -f ${FRUIT}/Makefile-driver clean

clean-build: 
	rm -rf ${OUTDIRS} ${OUTDIR_TESTS}
	rm -rf ${LIBDIR_OUT}
	rm -rf ${LIBDIR_ROOT}

clean: clean-temp clean-build 




