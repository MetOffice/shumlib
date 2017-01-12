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

# Default target - clean everything first (no incremental builds; the library
# builds fairly fast anyway so this avoids complications) then build libraries
#--------------------------------------------------------------------------------
.PHONY: default 
default: clean libs

# Output directories
#--------------------------------------------------------------------------------
.PHONY: dirs
dirs:
	mkdir -p ${LIBDIR_OUT}/lib
	mkdir -p ${LIBDIR_OUT}/include
	mkdir -p ${LIBDIR_OUT}/tests

# Libraries
#--------------------------------------------------------------------------------
.PHONY: bswap hook str_conv data_conv pack

# Byte-swapping
#--------------
BSWAP=shum_byteswap
bswap: dirs
	make -C ${BSWAP}/src

# Drhook dummy
#-------------
HOOK=shum_drhook_dummy
hook: dirs
	make -C ${HOOK}/src

# String conv
#------------
STR_CONV=shum_string_conv
str_conv: dirs
	make -C ${STR_CONV}/src

# Data conv
#----------
DATA_CONV=shum_data_conv
data_conv: str_conv dirs
	make -C ${DATA_CONV}/src

# WGDOS packing
#--------------
PACK=shum_wgdos_packing
pack: str_conv hook data_conv dirs
	make -C ${PACK}/src

# Add a target which points to all libraries
.PHONY: libs
libs: bswap hook str_conv data_conv pack

# Cleanup targets
#--------------------------------------------------------------------------------
.PHONY: clean clean-temp clean-build
clean-temp:
	make -C ${BSWAP}/src clean
	make -C ${HOOK}/src clean
	make -C ${STR_CONV}/src clean
	make -C ${DATA_CONV}/src clean
	make -C ${PACK}/src clean

clean-build: 
	rm -rf ${LIBDIR_OUT}/lib ${LIBDIR_OUT}/include ${LIBDIR_OUT}/tests
	rmdir ${LIBDIR_OUT} || :
	rmdir ${LIBDIR_ROOT} || :

clean: clean-temp clean-build




