# ------------------------------------------------------------------------------
#  (c) Crown copyright Met Office. All rights reserved.
#  The file LICENCE, distributed with this code, contains details of the terms
#  under which the code may be used.
# ------------------------------------------------------------------------------

# Library type options
option(BUILD_SHARED_LIBS "Build using shared libraries" ON)

# Data options
option(IEEE_ARITHMETIC "Use Fortran intrinsic IEEE features" OFF)
option(NAN_BY_BITS "Check NaNs by bitwise inspection" OFF)
option(DENORMAL_BY_BITS "Check denormals by bitwise inspect" OFF)

# Build options
option(BUILD_OPENMP "Build with OpenMP parallelisation" ON)
option(BUILD_FTHREADS "Build with Fortran OpenMP everywhere" OFF)
option(BUILD_TESTS "Build fruit unit tests" ON)

# Build a list of preprocessor settings based on options
set(SHUM_DEFINES "SHUMLIB_VERSION=${SHUMLIB_VERSION}")
list(APPEND SHUM_DEFINES "SHUMLIB_CMAKE=1")

# Install options
option(ENABLE_PKGCONFIG "Install a pkg-config description" ON)

if(IEEE_ARITHMETIC)
  message(VERBOSE "Enabling shumlib IEEE arithmetic")
  list(APPEND SHUM_DEFINES "HAS_IEEE_ARITHMETIC")
endif()

if(NAN_BY_BITS)
  message(VERBOSE "Enabling shumlib evaluate NaNs by bits")
  list(APPEND SHUM_DEFINES "EVAL_NAN_BY_BITS")
endif()

if(DENORMAL_BY_BITS)
  message(VERBOSE "Enabling shumlib evaluate denormals by bits")
  list(APPEND SHUM_DEFINES "EVAL_DENORMAL_BY_BITS")
endif()

if(BUILD_OPENMP)
  # FIXME: this probably needs newer version of cmake on the Cray
  # UPDATE: Doesn't work with cmake 3.31.9
  # A little hack to check supported OMP spec on Cray
  # ---
  if(CMAKE_Fortran_COMPILER_ID MATCHES "Cray" OR CMAKE_C_COMPILER_ID MATCHES "Cray")
    # Cray's OpenMP is compiler-managed (no separate runtime library).
    # CMake's FindOpenMP fails on Cray because it can't resolve LIB_NAMES.
    # Verify the minimum required OpenMP spec date manually instead.
    # See OpenMP Spec dates <https://www.openmp.org/specifications/>
    include(CheckFortranSourceRuns)
    set(CMAKE_REQUIRED_FLAGS "-homp")
    check_fortran_source_runs(
      "program check\n  if (_OPENMP < 200805) stop 1\nend program\n"
      CRAY_OMP_MEETS_MINIMUM SRC_EXT F90
    )
    unset(CMAKE_REQUIRED_FLAGS)
    if(NOT CRAY_OMP_MEETS_MINIMUM)
      message(FATAL_ERROR "shumlib requires OpenMP >= 3.0 (date 200805); Cray compiler does not meet this.")
    endif()

    # Create the standard imported targets so downstream consumers work normally
    if(NOT TARGET OpenMP::OpenMP_Fortran)
      add_library(OpenMP::OpenMP_Fortran INTERFACE IMPORTED)
      set_target_properties(OpenMP::OpenMP_Fortran PROPERTIES
        INTERFACE_COMPILE_OPTIONS "-homp"
        INTERFACE_LINK_OPTIONS   "-homp")
    endif()
    if(NOT TARGET OpenMP::OpenMP_C)
      add_library(OpenMP::OpenMP_C INTERFACE IMPORTED)
      set_target_properties(OpenMP::OpenMP_C PROPERTIES
        INTERFACE_COMPILE_OPTIONS "-fopenmp"
        INTERFACE_LINK_OPTIONS   "-fopenmp")
    endif()
    set(OpenMP_FOUND TRUE)
    set(OpenMP_Fortran_FOUND TRUE)
    set(OpenMP_C_FOUND TRUE)
  else()
    find_package(OpenMP 3.0 REQUIRED)
  endif()

  if(BUILD_FTHREADS)
    message(VERBOSE "Using shumlib with Fortran OpenMP threading")
    list(APPEND SHUM_DEFINES
      SHUM_USE_C_OPENMP_VIA_THREAD_UTILS="shum_use_c_openmp_via_thread_util")
  endif()

endif()
