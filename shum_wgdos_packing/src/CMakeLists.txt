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
