# Makefile for library; builds both a static (.a) and dynamic (.so) version
# for maximum flexibility. The required files are copied to the final directory
#------------------------------------------------------------------------------
LIBTARGETS= \
 ${LIBDIR_OUT}/lib/libshum_data_conv.so \
 ${LIBDIR_OUT}/lib/libshum_data_conv.a

${LIBTARGETS}: libshum_data_conv.so libshum_data_conv.a
	cp $^ ${LIBDIR_OUT}/lib
	cp *.h *.mod ${LIBDIR_OUT}/include

# Static library - uses archiver to create library
#-------------------------------------------------------------------------------
c_shum_data_conv.o: c_shum_data_conv.c c_shum_data_conv.h 
	${CC} ${CCFLAGS_PREC} -c -I${PBOMB_DIR} $<

f_shum_data_conv.o: f_shum_data_conv.f90
	${FC} ${FCFLAGS_PREC} -c -I${LIBDIR_OUT}/include $<

libshum_data_conv.a: c_shum_data_conv.o f_shum_data_conv.o
	${AR} $@ $^

# Dynamic library - uses compiler to create shared library - the object files
# are suffixed to keep them separate to the above (since the dynamic library
# required position-independent-code flags whilst the static version does not)
#-------------------------------------------------------------------------------
c_shum_data_conv_PIC.o: c_shum_data_conv.c c_shum_data_conv.h 
	${CC} ${CCFLAGS_PREC} ${CCFLAGS_PIC} -c -I${PBOMB_DIR} $< -o $@

f_shum_data_conv_PIC.o: f_shum_data_conv.f90
	${FC} ${FCFLAGS_PREC} ${FCFLAGS_PIC} -c -I${LIBDIR_OUT}/include $< -o $@

libshum_data_conv.so: c_shum_data_conv_PIC.o f_shum_data_conv_PIC.o 
	${FC} ${FCFLAGS_SHARED} ${FCFLAGS_PREC} ${FCFLAGS_PIC} $^ -o $@

# Cleanup
#-------------------------------------------------------------------------------
.PHONY: clean
clean: 
	rm -f *.o *.mod *.so *.a
