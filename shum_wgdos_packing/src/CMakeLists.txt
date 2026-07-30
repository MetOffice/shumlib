# ------------------------------------------------------------------------------
#  (c) Crown copyright Met Office. All rights reserved.
#  The file LICENCE, distributed with this code, contains details of the terms
#  under which the code may be used.
# ------------------------------------------------------------------------------
target_sources(shum
  PRIVATE
  c_shum_wgdos_packing.f90
  f_shum_wgdos_packing.f90)

target_sources(shum
  PUBLIC
  FILE_SET wgdos_packing_headers
  TYPE HEADERS
  FILES
  c_shum_wgdos_packing.h)

add_library(shum_wgdos_packing)
target_sources(shum_wgdos_packing
  PRIVATE
  c_shum_wgdos_packing.f90
  f_shum_wgdos_packing.f90
  ${PROJECT_SOURCE_DIR}/common/src/shumlib_version.c
  ${PROJECT_BINARY_DIR}/f_shum_wgdos_packing_version_mod.f90)

target_compile_definitions(shum_wgdos_packing PUBLIC ${SHUM_DEFINES})

set_target_properties(shum_wgdos_packing PROPERTIES
  Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules_shum_wgdos_packing)

target_include_directories(shum_wgdos_packing
  PUBLIC
  ${PROJECT_BINARY_DIR}/modules_shum_wgdos_packing
  ${PROJECT_SOURCE_DIR}/common/src)

target_link_libraries(shum_wgdos_packing PUBLIC shum_constants shum_string_conv)
