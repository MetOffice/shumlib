all: libshum_byteswap.so libshum_byteswap.a copy test

c_shum_byteswap.o: c_shum_byteswap.c c_shum_byteswap.h c_shum_byteswap_opt.h
	${CC} ${CCFLAGS_PREC} -c $<

c_shum_byteswap_PIC.o: c_shum_byteswap.c c_shum_byteswap.h c_shum_byteswap_opt.h
	${CC} ${CCFLAGS_PREC} ${CCFLAGS_PIC} -c $< -o $@

f_shum_byteswap.o: f_shum_byteswap.F90
	${FC} ${FCFLAGS_PREC} -c $<

f_shum_byteswap_PIC.o: f_shum_byteswap.F90
	${FC} ${FCFLAGS_PREC} ${FCFLAGS_PIC} -c $< -o $@

libshum_byteswap.so: c_shum_byteswap_PIC.o f_shum_byteswap_PIC.o
	${FC} ${FCFLAGS_SHARED} ${FCFLAGS_PREC} ${FCFLAGS_PIC} $^ -o $@

libshum_byteswap.a: c_shum_byteswap.o f_shum_byteswap.o
	${AR} $@ $^

${LIBDIR_OUT}/tests/test_byteswap_static.exe: test_byteswap.F90 copy 
	${FC} ${FCFLAGS_STATIC} ${FCFLAGS_PREC} $< \
	-I${LIBDIR_OUT}/include -L${LIBDIR_OUT}/lib -lshum_byteswap -o $@ \
	${FCFLAGS_STATIC_TRAIL}

${LIBDIR_OUT}/tests/test_byteswap_dynamic.exe: test_byteswap.F90 copy 
	${FC} ${FCFLAGS_DYNAMIC} ${FCFLAGS_PREC} ${FCFLAGS_PIC} $< \
	-I${LIBDIR_OUT}/include -L${LIBDIR_OUT}/lib -lshum_byteswap \
	-Wl,-rpath=${LIBDIR_OUT}/lib -o $@ ${FCFLAGS_DYNAMIC_TRAIL}

test: ${LIBDIR_OUT}/tests/test_byteswap_static.exe \
      ${LIBDIR_OUT}/tests/test_byteswap_dynamic.exe

.PHONY: clean copy test

copy: libshum_byteswap.so libshum_byteswap.a
	mv *.so *.a ${LIBDIR_OUT}/lib
	cp *.h *.mod ${LIBDIR_OUT}/include

clean: 
	rm -f *.o *.mod *.so *.a
