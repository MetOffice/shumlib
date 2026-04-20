# ------------------------------------------------------------------------------
#  (c) Crown copyright Met Office. All rights reserved.
#  The file LICENCE, distributed with this code, contains details of the terms
#  under which the code may be used.
# ------------------------------------------------------------------------------
target_sources(shum
  PRIVATE
  f_shum_thread_utils.f90)

target_sources(shum
  PUBLIC
  FILE_SET thread_utils_headers
  TYPE HEADERS
  FILES
  c_shum_thread_utils.h)
