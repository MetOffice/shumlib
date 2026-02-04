# ------------------------------------------------------------------------------
#  (c) Crown copyright Met Office. All rights reserved.
#  The file LICENCE, distributed with this code, contains details of the terms
#  under which the code may be used.
# ------------------------------------------------------------------------------
target_sources(shum
  PRIVATE
  c_shum_byteswap.c
  f_shum_byteswap.f90)

target_sources(shum
  PUBLIC
  FILE_SET byteswap_headers
  TYPE HEADERS
  FILES
  c_shum_byteswap.h
  c_shum_byteswap_opt.h)
