# ------------------------------------------------------------------------------
#  (c) Crown copyright Met Office. All rights reserved.
#  The file LICENCE, distributed with this code, contains details of the terms
#  under which the code may be used.
# ------------------------------------------------------------------------------
target_sources(shum
  PRIVATE
  c_shum_data_conv.c
  f_shum_data_conv.f90)

target_sources(shum
  PUBLIC
  FILE_SET data_conv_headers
  TYPE HEADERS
  FILES
  c_shum_data_conv.h)

add_library(shum_data_conv)
target_sources(shum_data_conv
  PRIVATE
  c_shum_data_conv.c
  f_shum_data_conv.f90
  ${PROJECT_SOURCE_DIR}/common/src/shumlib_version.c
  ${PROJECT_BINARY_DIR}/f_shum_data_conv_version_mod.f90)

target_compile_definitions(shum_data_conv PUBLIC ${SHUM_DEFINES})

target_include_directories(shum_data_conv
  PUBLIC
  ${CMAKE_Fortran_MODULE_DIRECTORY}
  ${PROJECT_SOURCE_DIR}/common/src)
