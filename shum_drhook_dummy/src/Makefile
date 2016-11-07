FC=ifort
CC=gcc

FCFLAGS=-i8 -r8

all: libshum_drhook_dummy.so libshum_drhook_dummy.a

parkind1.o: parkind1.F90
	${FC} ${FCFLAGS} -fPIC -c $<

yomhook.o: yomhook.F90 parkind1.o
	${FC} ${FCFLAGS} -fPIC -c $<

drhook_control_mod.o: drhook_control_mod.F90 yomhook.o
	${FC} ${FCFLAGS} -fPIC -c $<

libshum_drhook_dummy.so: parkind1.o yomhook.o drhook_control_mod.o
	${FC} -shared ${FCFLAGS} -fPIC $^ -o $@

libshum_drhook_dummy.a: parkind1.o yomhook.o drhook_control_mod.o
	ar -rc $@ $^

.PHONY: clean

clean: 
	rm -f *.o *.mod *.so *.a
