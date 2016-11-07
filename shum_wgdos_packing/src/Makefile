FC=ifort
CC=gcc

FCFLAGS=-i8 -r8

STRING_CONV_LIB=shum_string_conv
STRING_CONV_DIR=../../${STRING_CONV_LIB}/src

DATA_CONV_LIB=shum_data_conv
DATA_CONV_DIR=../../${DATA_CONV_LIB}/src

HOOK_LIB=shum_drhook_dummy
HOOK_DIR=../../${HOOK_LIB}/src

all: libshum_wgdos_packing.so libshum_wgdos_packing.a

c_shum_read_wgdos_header.o: c_shum_read_wgdos_header.c 
	${CC} -fPIC -c -I${DATA_CONV_DIR} $<

f_shum_read_wgdos_header.o: f_shum_read_wgdos_header.F90 c_shum_read_wgdos_header.o
	${FC} ${FCFLAGS} -c -fPIC -I${STRING_CONV_DIR} $<

f_shum_wgdos_packing.o: f_shum_wgdos_packing.F90
	${FC} ${FCFLAGS} -c -fPIC -I${HOOK_DIR} $<

c_shum_wgdos_packing.o: c_shum_wgdos_packing.F90 f_shum_wgdos_packing.o
	${FC} ${FCFLAGS} -c -fPIC -I${STRING_CONV_DIR} $<

libshum_wgdos_packing.so: \
	c_shum_read_wgdos_header.o \
	f_shum_read_wgdos_header.o \
	f_shum_wgdos_packing.o \
	c_shum_wgdos_packing.o
	${FC} -shared ${FCFLAGS} -fPIC *.o -o $@

libshum_wgdos_packing.a: \
	c_shum_read_wgdos_header.o \
	f_shum_read_wgdos_header.o \
	f_shum_wgdos_packing.o \
	c_shum_wgdos_packing.o 

	ar -rc $@ *.o

.PHONY: clean

clean: 
	rm -f *.o *.mod *.so *.a
