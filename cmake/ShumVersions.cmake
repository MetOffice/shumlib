# ------------------------------------------------------------------------------
#  (c) Crown copyright Met Office. All rights reserved.
#  The file LICENCE, distributed with this code, contains details of the terms
#  under which the code may be used.
# ------------------------------------------------------------------------------

# Create shumlib version files from templates
#
# Replace the previous versioning which used preprocessor macros and
# pure Makefiles with templating of the module file.
macro(configure_shum_versions)

  message(STATUS "Creating shumlib version functions")
  
  foreach(SHUM_LIBNAME IN LISTS CMAKE_SHUM_SUBLIBS)
    message(VERBOSE "Versioning ${SHUM_LIBNAME}")
    configure_file(common/src/f_version_mod.f90.in
      "f_${SHUM_LIBNAME}_version_mod.f90")
    
    target_sources(shum PRIVATE
      "${CMAKE_CURRENT_BINARY_DIR}/f_${SHUM_LIBNAME}_version_mod.f90")
    
  endforeach()

  # FIXME: remove hardwiring?
  target_include_directories(shum
    PUBLIC
    common/src)

  target_sources(shum PRIVATE
    common/src/shumlib_version.c)

  unset(SHUM_LIBNAME)
  unset(SHUM_VERSION_DEFINES)

endmacro()
