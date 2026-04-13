# ------------------------------------------------------------------------------
#  (c) Crown copyright Met Office. All rights reserved.
#  The file LICENCE, distributed with this code, contains details of the terms
#  under which the code may be used.
# ------------------------------------------------------------------------------

# Setup fruit unit testing for shumlib
#
# Take the accumluted list of shumlib subdirectories and use it to
# generate the top-level driver for the regression suite.
#
macro(setup_shum_fruit)

  message(STATUS "Creating shumlib regression tests")

  list(LENGTH CMAKE_SHUM_SUBLIBS fruit_count)
  if(${fruit_count} EQUAL 0)
    message(FATAL_ERROR "No regression tests set")
  endif()
  unset(fruit_count)

  set(SHUM_FRUIT_USE "! Use shumlib modules")
  set(SHUM_FRUIT_CALLS "! Call shumlib unit tests")

  foreach(SHUM_LIBNAME IN LISTS CMAKE_SHUM_SUBLIBS)
    # Build the variables that pull in each of the test modules ready
    # to process the driver template

    if(EXISTS ${SHUM_LIBNAME}/test/CMakeLists.txt)
      message(VERBOSE "Adding ${SHUM_LIBNAME} unit tests")

      set(SHUM_FRUIT_USE "${SHUM_FRUIT_USE}\nUSE fruit_test_${SHUM_LIBNAME}_mod")
      set(SHUM_FRUIT_CALLS "${SHUM_FRUIT_CALLS}\nCALL fruit_test_${SHUM_LIBNAME}")

      # FIXME: deal with relative path
      add_subdirectory(${SHUM_LIBNAME}/test)
    endif()

  endforeach()

  configure_file(fruit/fruit_driver.f90.in
    fruit_driver.f90)

  target_sources(shumlib-tests PRIVATE
      "${CMAKE_CURRENT_BINARY_DIR}/fruit_driver.f90")

endmacro()
