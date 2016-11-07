FC=ifort
CC=gcc

FCFLAGS=-i8 -r8

all: libshum_string_conv.so libshum_string_conv.a

f_shum_string_conv.o: f_shum_string_conv.F90
	${FC} ${FCFLAGS} -fPIC -c $<

libshum_string_conv.so: f_shum_string_conv.o
	${FC} -shared ${FCFLAGS} -fPIC $< -o $@

libshum_string_conv.a: f_shum_string_conv.o
	ar -rc $@ $<

.PHONY: clean

clean: 
	rm -f *.o *.mod *.so *.a
