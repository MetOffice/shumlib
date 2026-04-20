# ------------------------------------------------------------------------------
#  (c) Crown copyright Met Office. All rights reserved.
#  The file LICENCE, distributed with this code, contains details of the terms
#  under which the code may be used.
# ------------------------------------------------------------------------------

if(ENABLE_PKGCONFIG)
  # Create and install a pkg-config file
  #
  # This will only work correctly if the --install-prefix argument is
  # used when CMake is first run and the files are generated.  It will
  # break if a different --prefix location is passed to cmake --install
  #
  configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cmake/${PROJECT_NAME}.pc.in
    ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}.pc
    @ONLY)
  
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}.pc
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig)
endif()
