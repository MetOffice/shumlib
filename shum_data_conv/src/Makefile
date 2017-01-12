all: libshum_data_conv.so libshum_data_conv.a copy

c_shum_data_conv.o: c_shum_data_conv.c c_shum_data_conv.h 
	${CC} ${CCFLAGS_PREC} -c $<

c_shum_data_conv_PIC.o: c_shum_data_conv.c c_shum_data_conv.h 
	${CC} ${CCFLAGS_PREC} ${CCFLAGS_PIC} -c $< -o $@

f_shum_data_conv.o: f_shum_data_conv.F90
	${FC} ${FCFLAGS_PREC} -c -I${LIBDIR_OUT}/include $<

f_shum_data_conv_PIC.o: f_shum_data_conv.F90
	${FC} ${FCFLAGS_PREC} ${FCFLAGS_PIC} -c -I${LIBDIR_OUT}/include $< -o $@

libshum_data_conv.so: c_shum_data_conv_PIC.o f_shum_data_conv_PIC.o 
	${FC} ${FCFLAGS_SHARED} ${FCFLAGS_PREC} ${FCFLAGS_PIC} $^ -o $@

libshum_data_conv.a: c_shum_data_conv.o f_shum_data_conv.o
	${AR} $@ $^

.PHONY: clean copy

copy:
	mv *.so *.a ${LIBDIR_OUT}/lib
	cp *.h *.mod ${LIBDIR_OUT}/include || :

clean: 
	rm -f *.o *.mod *.so *.a
