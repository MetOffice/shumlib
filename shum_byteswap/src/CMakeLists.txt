# ------------------------------------------------------------------------------
#  (c) Crown copyright Met Office. All rights reserved.
#  The file LICENCE, distributed with this code, contains details of the terms
#  under which the code may be used.
# ------------------------------------------------------------------------------
target_sources(shum
  PRIVATE
  c_shum_byteswap.c
  f_shum_byteswap.f90)

target_sources(shum
  PUBLIC
  FILE_SET byteswap_headers
  TYPE HEADERS
  FILES
  c_shum_byteswap.h
  c_shum_byteswap_opt.h)

add_library(shum_byteswap)
target_sources(shum_byteswap
  PRIVATE
  c_shum_byteswap.c
  f_shum_byteswap.f90
  ${PROJECT_SOURCE_DIR}/common/src/shumlib_version.c
  ${PROJECT_BINARY_DIR}/f_shum_byteswap_version_mod.f90)

target_compile_definitions(shum_byteswap PUBLIC ${SHUM_DEFINES})

target_include_directories(shum_byteswap
  PUBLIC
  ${CMAKE_Fortran_MODULE_DIRECTORY}
  ${PROJECT_SOURCE_DIR}/common/src)

target_link_libraries(shum_byteswap PUBLIC shum_string_conv)
