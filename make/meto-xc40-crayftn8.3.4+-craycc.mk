# Platform specific settings
#-------------------------------------------------------------------------------

# Note that the below are overrides for the main cce config, due to changes
# in some of the supported flags

# Also note that it's possible this config will work for versions earlier
# than CCE 8.3.4 - this was just the earliest version available for testing

# At CCE 8.4.0 a new "-herror_on_warning" flag was added for crayftn, but
# won't work at earlier versions (so this line omits it)
FCFLAGS_EXTRA=-O2 -Ovector1 -hfp0 -hflex_mp=strict -hipa1 -hnopgas_runtime     \
              -hnocaf -M E287,E5001

# Pickup the remaining config details from the more recent config
include make/meto-xc40-crayftn8.4.0+-craycc.mk
