# Add shumlib sublibraries
#
# Each section of shumlib is in its own subdiretory with its own
# CMakeLists.txt file in its src/ directory.  Add each src directory
# and add the name of the sublibrary to a list for subsequnt use with
# the regression testing framework.
macro(add_shum_sublibraries libname)

  set(multiValueArgs TARGETS)
  cmake_parse_arguments(arg_sublibs
    "" "" "${multiValueArgs}"
    ${ARGN})

  set(CMAKE_SHUM_SUBLIBS "")

  message(STATUS "Adding shublib sub-libraries to ${libname}")

  foreach(sublib IN LISTS arg_sublibs_TARGETS)
    if(EXISTS ${sublib}/src/CMakeLists.txt)
      message(VERBOSE "Including ${sublib}")
      add_subdirectory(${sublib}/src)
      list(APPEND CMAKE_SHUM_SUBLIBS ${sublib})
    else()
      message(FATAL_ERROR "Unaable to add ${sublib}")
    endif()
    
  endforeach()
  
endmacro()
