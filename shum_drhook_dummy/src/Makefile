# Makefile for library; builds both a static (.a) and dynamic (.so) version
# for maximum flexibility. The required files are copied to the final directory
#------------------------------------------------------------------------------
LIBTARGETS= \
 ${LIBDIR_OUT}/lib/libshum_drhook_dummy.so \
 ${LIBDIR_OUT}/lib/libshum_drhook_dummy.a

${LIBTARGETS}: libshum_drhook_dummy.so libshum_drhook_dummy.a
	cp $^ ${LIBDIR_OUT}/lib
	cp *.mod ${LIBDIR_OUT}/include

# Static library - uses archiver to create library
#-------------------------------------------------------------------------------
parkind1.o: parkind1.F90
	${FC} ${FCFLAGS_PREC} -c $<

yomhook.o: yomhook.F90 parkind1.o
	${FC} ${FCFLAGS_PREC} -c $<

libshum_drhook_dummy.a: parkind1.o yomhook.o 
	${AR} $@ $^

# Dynamic library - uses compiler to create shared library - the object files
# are suffixed to keep them separate to the above (since the dynamic library
# required position-independent-code flags whilst the static version does not)
#-------------------------------------------------------------------------------
parkind1_PIC.o: parkind1.F90
	${FC} ${FCFLAGS_PREC} ${FCFLAGS_PIC} -c $< -o $@

yomhook_PIC.o: yomhook.F90 parkind1.o
	${FC} ${FCFLAGS_PREC} ${FCFLAGS_PIC} -c $< -o $@

libshum_drhook_dummy.so: parkind1_PIC.o yomhook_PIC.o 
	${FC} ${FCFLAGS_SHARED} ${FCFLAGS_PREC} ${FCFLAGS_PIC} $^ -o $@

# Cleanup
#-------------------------------------------------------------------------------
.PHONY: clean
clean: 
	rm -f *.o *.mod *.so *.a
