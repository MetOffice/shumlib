# ------------------------------------------------------------------------------
#  (c) Crown copyright Met Office. All rights reserved.
#  The file LICENCE, distributed with this code, contains details of the terms
#  under which the code may be used.
# ------------------------------------------------------------------------------
#
# This is the top-level CMake build file for the Shared Unified Model
# Library
#
cmake_minimum_required(VERSION 3.20 FATAL_ERROR)

# Shumlib version numbers are the 4-digit year followed by 2-digit
# month and an additional digit signifying the release count within
# that specified month.  This should be specified in major.minor.patch
# format used by CMake and then recombined to allow it to be passed to
# shumlib via a pre-processor definition
project(shumlib LANGUAGES C Fortran
  VERSION 2026.01.1
  DESCRIPTION "Met Office Shared Unifed Model Library"
  HOMEPAGE_URL "https://github.com/MetOffice/shumlib")

string(CONCAT SHUMLIB_VERSION
  ${PROJECT_VERSION_MAJOR}
  ${PROJECT_VERSION_MINOR}
  ${PROJECT_VERSION_PATCH})

set(CMAKE_C_STANDARD 99)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)

# Include helper and setup routines
include(GNUInstallDirs)
include(cmake/PreventInSourceBuilds.cmake)
include(cmake/ShumAddSublibrary.cmake)
include(cmake/ShumFruit.cmake)
include(cmake/ShumOptions.cmake)
include(cmake/ShumVersions.cmake)


# Build rules for shumlib
add_library(shum)

add_shum_sublibraries(shum
  TARGETS
  shum_byteswap
  shum_constants
  shum_data_conv
  shum_fieldsfile
  shum_fieldsfile_class
  shum_horizontal_field_interp
  shum_kinds
  shum_latlon_eq_grids
  shum_number_tools
  shum_spiral_search
  shum_string_conv
  shum_thread_utils
  shum_wgdos_packing)

configure_shum_versions()

target_compile_definitions(shum
  PUBLIC
  ${SHUM_DEFINES})

# Install shumlib and the C header files for the sub-libraries that
# have them
install(TARGETS shum
  FILE_SET byteswap_headers
  FILE_SET data_conv_headers
  FILE_SET spiral_search_headers
  FILE_SET thread_utils_headers
  FILE_SET wgdos_packing_headers
)

# Install the Fortran module files
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})

# Unit tests and framework
if(BUILD_TESTS)
  enable_testing()
  add_library(fruit)

  # Set a relative RPATH to ensure that the unit tests continue to
  # work even after being installed
  list(APPEND CMAKE_INSTALL_RPATH "$ORIGIN")
  list(APPEND CMAKE_INSTALL_RPATH "$ORIGIN/../${CMAKE_INSTALL_LIBDIR}")
  add_executable(shumlib-tests)

  target_compile_definitions(shumlib-tests
    PUBLIC
    ${SHUM_DEFINES})

  target_include_directories(shumlib-tests
    PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
  target_link_libraries(shumlib-tests PUBLIC fruit shum)

  add_subdirectory(fruit)
  setup_shum_fruit()

  install(TARGETS fruit)
  install(TARGETS shumlib-tests)

  add_test(NAME shumlib-tests
    COMMAND $<TARGET_FILE:shumlib-tests>)

  # Always set the shumlib directory to /tmp
  set_property(TEST shumlib-tests
    APPEND PROPERTY
    ENVIRONMENT SHUM_TMPDIR=/tmp)

endif()
