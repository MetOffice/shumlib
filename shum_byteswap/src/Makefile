FC=ifort
CC=gcc

FCFLAGS=-i8 -r8

all: libshum_byteswap.so libshum_byteswap.a

c_shum_byteswap.o: c_shum_byteswap.c c_shum_byteswap.h c_shum_byteswap_opt.h
	${CC} -fPIC -c $<

f_shum_byteswap.o: f_shum_byteswap.F90
	${FC} ${FCFLAGS} -fPIC -c $<

libshum_byteswap.so: c_shum_byteswap.o f_shum_byteswap.o
	${FC} -shared ${FCFLAGS} -fPIC $^ -o $@

libshum_byteswap.a: c_shum_byteswap.o f_shum_byteswap.o
	ar -rc $@ $^

.PHONY: clean

clean: 
	rm -f *.o *.mod *.so *.a
