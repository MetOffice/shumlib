# ------------------------------------------------------------------------------
#  (c) Crown copyright Met Office. All rights reserved.
#  The file LICENCE, distributed with this code, contains details of the terms
#  under which the code may be used.
# ------------------------------------------------------------------------------
target_sources(shum
  PRIVATE
  f_shum_string_conv.f90)

add_library(shum_string_conv)
target_sources(shum_string_conv
  PRIVATE
  f_shum_string_conv.f90
  ${PROJECT_SOURCE_DIR}/common/src/shumlib_version.c
  ${PROJECT_BINARY_DIR}/f_shum_string_conv_version_mod.f90)

target_compile_definitions(shum_string_conv PUBLIC ${SHUM_DEFINES})

set_target_properties(shum_string_conv PROPERTIES
  Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules_shum_string_conv)

target_include_directories(shum_string_conv
  PUBLIC
  ${PROJECT_BINARY_DIR}/modules_shum_string_conv
  ${PROJECT_SOURCE_DIR}/common/src)
