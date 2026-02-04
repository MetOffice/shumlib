# ------------------------------------------------------------------------------
#  (c) Crown copyright Met Office. All rights reserved.
#  The file LICENCE, distributed with this code, contains details of the terms
#  under which the code may be used.
# ------------------------------------------------------------------------------
target_sources(shumlib-tests PRIVATE
  fruit_test_shum_thread_utils.f90
  c_fruit_test_shum_thread_utils.c)

target_include_directories(shumlib-tests
  PUBLIC
  ${CMAKE_CURRENT_SOURCE_DIR})
