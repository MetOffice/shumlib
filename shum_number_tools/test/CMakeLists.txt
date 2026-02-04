# ------------------------------------------------------------------------------
#  (c) Crown copyright Met Office. All rights reserved.
#  The file LICENCE, distributed with this code, contains details of the terms
#  under which the code may be used.
# ------------------------------------------------------------------------------
target_sources(shumlib-tests PRIVATE
  c_fruit_test_shum_number_tools.c
  fruit_test_shum_number_tools.F90)

set_source_files_properties(
  fruit_test_shum_number_tools.F90
  PROPERTIES Fortran_PREPROCESS ON)

target_include_directories(shumlib-tests
    PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR})
