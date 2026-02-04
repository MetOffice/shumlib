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
