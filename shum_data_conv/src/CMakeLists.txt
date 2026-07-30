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

set_target_properties(shum_data_conv PROPERTIES
  Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules_shum_data_conv)

target_include_directories(shum_data_conv
  PUBLIC
  ${PROJECT_BINARY_DIR}/modules_shum_data_conv
  ${PROJECT_SOURCE_DIR}/common/src)

target_link_libraries(shum_data_conv PUBLIC shum_string_conv)
