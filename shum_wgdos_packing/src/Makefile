# Makefile for library; builds both a static (.a) and dynamic (.so) version
# for maximum flexibility. The required files are copied to the final directory
#------------------------------------------------------------------------------
LIBTARGETS= \
 ${LIBDIR_OUT}/lib/libshum_wgdos_packing.so \
 ${LIBDIR_OUT}/lib/libshum_wgdos_packing.a

${LIBTARGETS}: libshum_wgdos_packing.so libshum_wgdos_packing.a
	cp $^ ${LIBDIR_OUT}/lib
	cp *.h *.mod ${LIBDIR_OUT}/include

# Static library - uses archiver to create library
#-------------------------------------------------------------------------------
f_shum_wgdos_packing.o: f_shum_wgdos_packing.f90
	${FC} ${FCFLAGS_PREC} -c -I${LIBDIR_OUT}/include $<

c_shum_wgdos_packing.o: c_shum_wgdos_packing.f90 f_shum_wgdos_packing.o
	${FC} ${FCFLAGS_PREC} -c -I${LIBDIR_OUT}/include $<

libshum_wgdos_packing.a: \
	f_shum_wgdos_packing.o \
	c_shum_wgdos_packing.o 
	${AR} $@ $^

# Dynamic library - uses compiler to create shared library - the object files
# are suffixed to keep them separate to the above (since the dynamic library
# required position-independent-code flags whilst the static version does not)
#-------------------------------------------------------------------------------
f_shum_wgdos_packing_PIC.o: f_shum_wgdos_packing.f90
	${FC} ${FCFLAGS_PREC} ${FCFLAGS_PIC} -c -I${LIBDIR_OUT}/include $< -o $@

c_shum_wgdos_packing_PIC.o: c_shum_wgdos_packing.f90 f_shum_wgdos_packing_PIC.o
	${FC} ${FCFLAGS_PREC} ${FCFLAGS_PIC} -c -I${LIBDIR_OUT}/include $< -o $@

libshum_wgdos_packing.so: \
	f_shum_wgdos_packing_PIC.o \
	c_shum_wgdos_packing_PIC.o
	${FC} ${FCFLAGS_SHARED} ${FCFLAGS_PREC} ${FCFLAGS_PIC} $^ -o $@

# Cleanup
#-------------------------------------------------------------------------------
.PHONY: clean
clean: 
	rm -f *.o *.mod *.so *.a
