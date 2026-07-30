# ------------------------------------------------------------------------------
#  (c) Crown copyright Met Office. All rights reserved.
#  The file LICENCE, distributed with this code, contains details of the terms
#  under which the code may be used.
# ------------------------------------------------------------------------------
target_sources(shum
  PRIVATE
  f_shum_spiral_search.f90
  c_shum_spiral_search.f90)

target_sources(shum
  PUBLIC
  FILE_SET spiral_search_headers
  TYPE HEADERS
  FILES
  c_shum_spiral_search.h)

add_library(shum_spiral_search)
target_sources(shum_spiral_search
  PRIVATE
  f_shum_spiral_search.f90
  c_shum_spiral_search.f90
  ${PROJECT_SOURCE_DIR}/common/src/shumlib_version.c
  ${PROJECT_BINARY_DIR}/f_shum_spiral_search_version_mod.f90)

target_compile_definitions(shum_spiral_search PUBLIC ${SHUM_DEFINES})

set_target_properties(shum_spiral_search PROPERTIES
  Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules_shum_spiral_search)

target_include_directories(shum_spiral_search
  PUBLIC
  ${PROJECT_BINARY_DIR}/modules_shum_spiral_search
  ${PROJECT_SOURCE_DIR}/common/src)

target_link_libraries(shum_spiral_search PUBLIC shum_constants shum_string_conv)
