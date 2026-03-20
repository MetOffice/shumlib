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
