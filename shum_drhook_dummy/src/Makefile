all: libshum_drhook_dummy.so libshum_drhook_dummy.a copy

parkind1.o: parkind1.F90
	${FC} ${FCFLAGS_PREC} -c $<

parkind1_PIC.o: parkind1.F90
	${FC} ${FCFLAGS_PREC} ${FCFLAGS_PIC} -c $< -o $@

yomhook.o: yomhook.F90 parkind1.o
	${FC} ${FCFLAGS_PREC} -c $<

yomhook_PIC.o: yomhook.F90 parkind1.o
	${FC} ${FCFLAGS_PREC} ${FCFLAGS_PIC} -c $< -o $@

libshum_drhook_dummy.so: parkind1_PIC.o yomhook_PIC.o 
	${FC} ${FCFLAGS_SHARED} ${FCFLAGS_PREC} ${FCFLAGS_PIC} $^ -o $@

libshum_drhook_dummy.a: parkind1.o yomhook.o 
	${AR} $@ $^

.PHONY: clean copy

copy:
	mv *.so *.a ${LIBDIR_OUT}/lib
	cp *.h *.mod ${LIBDIR_OUT}/include || :

clean: 
	rm -f *.o *.mod *.so *.a
