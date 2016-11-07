FC=ifort
CC=gcc

FCFLAGS=-i8 -r8

STRING_CONV_LIB=shum_string_conv
STRING_CONV_DIR=../../${STRING_CONV_LIB}/src

all: libshum_data_conv.so libshum_data_conv.a

c_shum_data_conv.o: c_shum_data_conv.c c_shum_data_conv.h 
	${CC} -fPIC -c $<

f_shum_data_conv.o: f_shum_data_conv.F90
	${FC} ${FCFLAGS} -fPIC -c -I${STRING_CONV_DIR} $<

libshum_data_conv.so: c_shum_data_conv.o f_shum_data_conv.o 
	${FC} -shared ${FCFLAGS} -fPIC $^ -o $@

libshum_data_conv.a: c_shum_data_conv.o f_shum_data_conv.o
	ar -rc $@ $^

.PHONY: clean

clean: 
	rm -f *.o *.mod *.so *.a
