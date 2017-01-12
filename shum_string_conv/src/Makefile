all: libshum_string_conv.so libshum_string_conv.a copy

f_shum_string_conv.o: f_shum_string_conv.F90
	${FC} ${FCFLAGS_PREC} -c $<

f_shum_string_conv_PIC.o: f_shum_string_conv.F90
	${FC} ${FCFLAGS_PREC} ${FCFLAGS_PIC} -c $< -o $@

libshum_string_conv.so: f_shum_string_conv_PIC.o
	${FC} ${FCFLAGS_SHARED} ${FCFLAGS_PREC} ${FCFLAGS_PIC} $< -o $@

libshum_string_conv.a: f_shum_string_conv.o
	${AR} $@ $<

.PHONY: clean copy

copy:
	mv *.so *.a ${LIBDIR_OUT}/lib
	cp *.h *.mod ${LIBDIR_OUT}/include || :

clean: 
	rm -f *.o *.mod *.so *.a
