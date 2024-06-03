# Platform specific settings
#-------------------------------------------------------------------------------

# Note that the below are overrides for the main ifort config, due to changes
# in some of the supported flags

# Also note that it's possible this config will work for versions earlier
# than ifort 12 - this was just the earliest version available for testing

# At ifort 15 the option to enable OpenMP was changed to -qopenmp
FCFLAGS_OPENMP=-openmp

# Pickup the remaining config details from the more recent config
include make/meto-x86-ifort15+-gcc.mk
