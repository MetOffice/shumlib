# ------------------------------------------------------------------------------
#  (c) Crown copyright Met Office. All rights reserved.
#  The file LICENCE, distributed with this code, contains details of the terms
#  under which the code may be used.
# ------------------------------------------------------------------------------
target_sources(shum
  PRIVATE
  f_shum_is_inf.F90
  f_shum_is_nan.F90
  f_shum_is_denormal.F90)

set_source_files_properties(
  f_shum_is_inf.F90
  f_shum_is_nan.F90
  f_shum_is_denormal.F90
  PROPERTIES Fortran_PREPROCESS ON)
