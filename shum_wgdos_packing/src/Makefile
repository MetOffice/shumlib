all: libshum_wgdos_packing.so libshum_wgdos_packing.a copy test

c_shum_read_wgdos_header.o: c_shum_read_wgdos_header.c 
	${CC} ${CCFLAGS_PREC} -c -I${LIBDIR_OUT}/include $<

c_shum_read_wgdos_header_PIC.o: c_shum_read_wgdos_header.c 
	${CC} ${CCFLAGS_PREC} ${CCFLAGS_PIC} -c -I${LIBDIR_OUT}/include $< -o $@

f_shum_read_wgdos_header.o: f_shum_read_wgdos_header.F90 c_shum_read_wgdos_header.o
	${FC} ${FCFLAGS_PREC} -c -I${LIBDIR_OUT}/include $<

f_shum_read_wgdos_header_PIC.o: f_shum_read_wgdos_header.F90 c_shum_read_wgdos_header_PIC.o
	${FC} ${FCFLAGS_PREC} ${FCFLAGS_PIC} -c -I${LIBDIR_OUT}/include $< -o $@

f_shum_wgdos_packing.o: f_shum_wgdos_packing.F90
	${FC} ${FCFLAGS_PREC} -c -I${LIBDIR_OUT}/include $<

f_shum_wgdos_packing_PIC.o: f_shum_wgdos_packing.F90
	${FC} ${FCFLAGS_PREC} ${FCFLAGS_PIC} -c -I${LIBDIR_OUT}/include $< -o $@

c_shum_wgdos_packing.o: c_shum_wgdos_packing.F90 f_shum_wgdos_packing.o
	${FC} ${FCFLAGS_PREC} -c -I${LIBDIR_OUT}/include $<

c_shum_wgdos_packing_PIC.o: c_shum_wgdos_packing.F90 f_shum_wgdos_packing_PIC.o
	${FC} ${FCFLAGS_PREC} ${FCFLAGS_PIC} -c -I${LIBDIR_OUT}/include $< -o $@

libshum_wgdos_packing.so: \
	c_shum_read_wgdos_header_PIC.o \
	f_shum_read_wgdos_header_PIC.o \
	f_shum_wgdos_packing_PIC.o \
	c_shum_wgdos_packing_PIC.o
	${FC} ${FCFLAGS_SHARED} ${FCFLAGS_PREC} ${FCFLAGS_PIC} $^ -o $@

libshum_wgdos_packing.a: \
	c_shum_read_wgdos_header.o \
	f_shum_read_wgdos_header.o \
	f_shum_wgdos_packing.o \
	c_shum_wgdos_packing.o 

	${AR} $@ $^

${LIBDIR_OUT}/tests/test_wgdos_packing_static.exe: test_packing.F90 copy
	${FC} ${FCFLAGS_STATIC} ${FCFLAGS_PREC} $< \
	-I${LIBDIR_OUT}/include -L${LIBDIR_OUT}/lib \
	-lshum_wgdos_packing -lshum_data_conv -lshum_string_conv \
	-lshum_drhook_dummy -o $@ ${FCFLAGS_STATIC_TRAIL}

${LIBDIR_OUT}/tests/test_wgdos_packing_dynamic.exe: test_packing.F90 copy
	${FC} ${FCFLAGS_DYNAMIC} ${FCFLAGS_PREC} ${FCFLAGS_PIC} $< \
	-I${LIBDIR_OUT}/include -L${LIBDIR_OUT}/lib -Wl,-rpath=${LIBDIR_OUT}/lib \
	-lshum_wgdos_packing -lshum_data_conv -lshum_string_conv \
	-lshum_drhook_dummy -o $@ ${FCFLAGS_DYNAMIC_TRAIL}

test: ${LIBDIR_OUT}/tests/test_wgdos_packing_static.exe \
      ${LIBDIR_OUT}/tests/test_wgdos_packing_dynamic.exe

.PHONY: clean copy test

copy: libshum_wgdos_packing.so libshum_wgdos_packing.a
	mv *.so *.a ${LIBDIR_OUT}/lib
	cp *.h *.mod ${LIBDIR_OUT}/include || :

clean: 
	rm -f *.o *.mod *.so *.a
