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
  find_package(OpenMP 3.0 REQUIRED)

  if(BUILD_FTHREADS)
    message(VERBOSE "Using shumlib with Fortran OpenMP threading")
    list(APPEND SHUM_DEFINES
      SHUM_USE_C_OPENMP_VIA_THREAD_UTILS="shum_use_c_openmp_via_thread_util")
  endif()

endif()
