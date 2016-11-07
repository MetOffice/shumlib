FC=ifort
CC=gcc

FCFLAGS=-i8 -r8

all: test_byteswap.exe test_packing.exe

# LIBS
#--------------------------------------------------------------------------------
BSWAP_LIB=shum_byteswap
BSWAP_DIR=${BSWAP_LIB}/src

${BSWAP_DIR}/${BSWAP_LIB}.so:
	make -C ${BSWAP_DIR}

STR_CONV_LIB=shum_string_conv
STR_CONV_DIR=${STR_CONV_LIB}/src


${STR_CONV_DIR}/${STR_CONV_LIB}.so:
	make -C ${STR_CONV_DIR}

DATA_CONV_LIB=shum_data_conv
DATA_CONV_DIR=${DATA_CONV_LIB}/src


${DATA_CONV_DIR}/${DATA_CONV_LIB}.so: ${STR_CONV_DIR}/${STR_CONV_LIB}.so
	make -C ${DATA_CONV_DIR}

PACK_LIB=shum_wgdos_packing
PACK_DIR=${PACK_LIB}/src

${PACK_DIR}/${PACK_LIB}.so: \
	${STR_CONV_DIR}/${STR_CONV_LIB}.so \
	${DATA_CONV_DIR}/${DATA_CONV_LIB}.so
	make -C ${PACK_DIR}

HOOK_LIB=shum_drhook_dummy
HOOK_DIR=${HOOK_LIB}/src/

${HOOK_DIR}/${HOOK_LIB}.so: 
	make -C ${HOOK_DIR}

# BYTE SWAPPING TEST
#--------------------------------------------------------------------------------
test_byteswap.exe: test_byteswap.o
	${FC} ${FCFLAGS} \
		-L${BSWAP_DIR} -l${BSWAP_LIB} -Wl,-rpath ${BSWAP_DIR} $< -o $@

test_byteswap.o: test_byteswap.F90 ${BSWAP_DIR}/${BSWAP_LIB}.so
	${FC} -c ${FCFLAGS} -fPIC -I${BSWAP_DIR} $<

# PACKING TEST
#--------------------------------------------------------------------------------
test_packing.exe: test_packing.o
	${FC} ${FCFLAGS}  \
		-L${PACK_DIR} -l${PACK_LIB} -Wl,-rpath ${PACK_DIR} \
		-L${STR_CONV_DIR}  -l${STR_CONV_LIB} -Wl,-rpath ${STR_CONV_DIR}  \
		-L${HOOK_DIR} -l${HOOK_LIB} -Wl,-rpath ${HOOK_DIR} \
		-L${DATA_CONV_DIR} -l${DATA_CONV_LIB} -Wl,-rpath ${DATA_CONV_DIR} $< -o $@

test_packing.o: test_packing.F90 \
                ${STR_CONV_DIR}/${STR_CONV_LIB}.so \
                ${DATA_CONV_DIR}/${DATA_CONV_LIB}.so \
                ${HOOK_DIR}/${HOOK_LIB}.so \
                ${PACK_DIR}/${PACK_LIB}.so
	${FC} -c ${FCFLAGS} -fPIC -I${PACK_DIR} $<


.PHONY: clean all

clean:
	rm -f *.o *.exe
	make -C ${BSWAP_DIR} clean
	make -C ${STR_CONV_DIR} clean
	make -C ${DATA_CONV_DIR} clean
	make -C ${PACK_DIR} clean
	make -C ${HOOK_DIR} clean

