# ------------------------------------------------------------------------------
#  (c) Crown copyright Met Office. All rights reserved.
#  The file LICENCE, distributed with this code, contains details of the terms
#  under which the code may be used.
# ------------------------------------------------------------------------------
target_sources(shum
  PRIVATE
  f_shum_fieldsfile.f90
  f_shum_stashmaster.f90
  f_shum_fixed_length_header_indices.f90
  f_shum_lookup_indices.f90)
