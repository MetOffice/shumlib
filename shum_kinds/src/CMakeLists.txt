# ------------------------------------------------------------------------------
#  (c) Crown copyright Met Office. All rights reserved.
#  The file LICENCE, distributed with this code, contains details of the terms
#  under which the code may be used.
# ------------------------------------------------------------------------------
target_sources(shum
  PRIVATE
  f_shum_kinds.F90)

set_source_files_properties(
  f_shum_kinds.F90
  PROPERTIES Fortran_PREPROCESS ON)
