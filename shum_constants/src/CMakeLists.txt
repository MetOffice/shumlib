# ------------------------------------------------------------------------------
#  (c) Crown copyright Met Office. All rights reserved.
#  The file LICENCE, distributed with this code, contains details of the terms
#  under which the code may be used.
# ------------------------------------------------------------------------------
target_sources(shum
  PRIVATE
  f_shum_chemistry_constants_mod.f90
  f_shum_conversions_mod.f90
  f_shum_planet_earth_constants_mod.f90
  f_shum_ztables.f90
  f_shum_rel_mol_mass_mod.f90
  f_shum_water_constants_mod.f90)

add_library(shum_constants)
target_sources(shum_constants
  PRIVATE
  f_shum_chemistry_constants_mod.f90
  f_shum_conversions_mod.f90
  f_shum_planet_earth_constants_mod.f90
  f_shum_ztables.f90
  f_shum_rel_mol_mass_mod.f90
  f_shum_water_constants_mod.f90
  ${PROJECT_SOURCE_DIR}/common/src/shumlib_version.c
  ${PROJECT_BINARY_DIR}/f_shum_constants_version_mod.f90)

target_compile_definitions(shum_constants PUBLIC ${SHUM_DEFINES})

target_include_directories(shum_constants
  PUBLIC
  ${CMAKE_Fortran_MODULE_DIRECTORY}
  ${PROJECT_SOURCE_DIR}/common/src)
