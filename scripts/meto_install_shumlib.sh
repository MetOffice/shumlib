#!/bin/bash --login
# *********************************COPYRIGHT************************************
# (C) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file LICENCE.txt
# which you should have received as part of this distribution.
# *********************************COPYRIGHT************************************
#
# This file is part of the UM Shared Library project.
#
# The UM Shared Library is free software: you can redistribute it
# and/or modify it under the terms of the Modified BSD License, as
# published by the Open Source Initiative.
#
# The UM Shared Library is distributed in the hope that it will be
# useful, but WITHOUT ANY WARRANTY; without even the implied warranty
# of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# Modified BSD License for more details.
#
# You should have received a copy of the Modified BSD License
# along with the UM Shared Library.
# If not, see <http://opensource.org/licenses/BSD-3-Clause>.
#*******************************************************************************
#
# Script to assist with installing shumlib under the many platform/compiler
# combinations available at the Met Office...
#
# USAGE:  (Note - must be run from the toplevel Shumlib directory!)
#   scripts/meto_install_shumlib.sh [azspice|ex1a]
#
# This script was used to install shumlib version 2025.10.1
# and was intended for use with the UM at UM 14.0
#

set -eu

RUN_SUCCESS=0

# set up no IEEE list
NO_IEEE_LIST=${NO_IEEE_LIST:-"azspice_gnu_12.2"}

# Ensure directory is correct
CANONICAL_DIR=$(dirname "$0")
CANONICAL_DIR=$(readlink -f "$CANONICAL_DIR/..")
cd "$CANONICAL_DIR"

# Take the platform name as an argument
# (purely so we can maintain one script rather than 2)
PLATFORM=${1:-}

if [ -z "${PLATFORM}" ] ; then
    echo "Please provide platform or specific build as argument"
    echo "e.g. azspice, ex1a or grep this file for THIS to see the "
    echo "other options for specific builds"
    exit 1
fi

# Try and detect if the script is being run from the right place
if [ ! -f Makefile ] || ! ls -d shum_* > /dev/null 2>&1 ; then
    echo "Cannot find expected files... Please note that this script "
    echo "must be run from toplevel Shumlib directory"
    exit 1
fi

# Get the list of all possible libraries to build (this is used by some of the
# tasks to exclude certain libraries)
LIB_DIRS=$(find shum_* -maxdepth 0 -type d -print0 | xargs -0)

# Destination for the build (can be overidden, otherwise defaults to a
# "build" directory in the working copy - like the Makefile would)
BUILD_DESTINATION=${BUILD_DESTINATION:-$PWD/_build}

# This list dictates which threading variants of Shumlib will be installed
# (all versions defined will always be built + tested, but only those set
# by this list will end up in the BUILD_DESTINATION directory)
INSTALL_LIBS=${INSTALL_LIBS:-"openmp no-openmp thread_utils serial_thread_utils"}

# Function for testing membership of a list
function contains {
    local elt
    for elt in $2 ; do
        [[ "$elt" == "$1" ]] && return 0;
    done
    return 1
}

# Functions which build given libraries and tests for those libraries
function build_test_clean {
    local config=$1
    shift
    if [[ " $NO_IEEE_LIST " =~ [[:space:]]"$THIS"[[:space:]] ]] ; then
      export SHUM_HAS_IEEE_ARITHMETIC="false"
    else
      unset SHUM_HAS_IEEE_ARITHMETIC
    fi
    echo "make -f make/$config.mk clean-temp"
    make -f "make/$config.mk" clean-temp
    echo "make -f make/$config.mk $*"
    make -f "make/$config.mk" "$@"
    echo "make -f make/$config.mk test"
    make -f "make/$config.mk" test
    echo "make -f make/$config.mk clean-temp"
    make -f "make/$config.mk" clean-temp
}

# Function which executes the above function several times - once for each of
# the possible OpenMP state and Thread_Utils state combinations. Note that
# the build will only be copied to the destination install directory if the
# variable LIBDIR_OUT is set - this is done selectively depending on which
# builds are required to be installed (according to the INSTALL_LIBS variable)
function build_openmp_onoff {
    local config=$1
    local dir=$2
    shift 2
    # Copy source to temporary build directory and switch there
    TEMP_BUILD_DIR=${SHUM_TMPDIR:-$(mktemp -d)}
    mkdir -p "$TEMP_BUILD_DIR"
    cp -r ./* "$TEMP_BUILD_DIR"
    cd "$TEMP_BUILD_DIR"

    echo "Build Dir is: $TEMP_BUILD_DIR"

    # No-OpenMP
    unset LIBDIR_OUT
    if contains "no-openmp" "$INSTALL_LIBS" ; then
        export LIBDIR_OUT=$dir/no-openmp
    fi
    export SHUM_OPENMP=false
    export SHUM_USE_C_OPENMP_VIA_THREAD_UTILS=false
    build_test_clean "$config" "$@"

    # OpenMP
    unset LIBDIR_OUT
    if contains "openmp" "$INSTALL_LIBS" ; then
        export LIBDIR_OUT=$dir/openmp
    fi
    export SHUM_OPENMP=true
    export SHUM_USE_C_OPENMP_VIA_THREAD_UTILS=false
    build_test_clean "$config" "$@"

    # Thread Utils + OpenMP
    unset LIBDIR_OUT
    if contains "thread_utils" "$INSTALL_LIBS" ; then
        export LIBDIR_OUT=$dir/thread_utils
    fi
    export SHUM_OPENMP=true
    export SHUM_USE_C_OPENMP_VIA_THREAD_UTILS=true
    build_test_clean "$config" "$@"

    # Thread Utils, No-OpenMP
    unset LIBDIR_OUT
    if contains "serial_thread_utils" "$INSTALL_LIBS" ; then
        export LIBDIR_OUT=$dir/serial_thread_utils
    fi
    export SHUM_OPENMP=false
    export SHUM_USE_C_OPENMP_VIA_THREAD_UTILS=true
    build_test_clean "$config" "$@"

    # Unset threading - not practically a useful install; more of a test
    # of the Makefiles; this checks to see what happens if the environment
    # variables that control the build threading are not set
    unset LIBDIR_OUT
    if contains "unset_threading" "$INSTALL_LIBS" ; then
        export LIBDIR_OUT=$dir/unset_threading
    fi
    unset SHUM_OPENMP
    unset SHUM_USE_C_OPENMP_VIA_THREAD_UTILS
    build_test_clean "$config" "$@"

    # Test if $SHUM_TMPDIR is unset. If it is not set, we are using the mktmp
    # directory, which must be cleaned up again.
    if [ -z "${SHUM_TMPDIR+x}" ]; then
      # Tidy up the temporary directory
      rm -rf "$TEMP_BUILD_DIR"
    fi

}

# AZ SPICE GNU 12.2
THIS="azspice_gnu_12.2"
if [ "$PLATFORM" == "azspice" ] || [ "$PLATFORM" == $THIS ] ; then

  if [ -z "${SPACKDIR}" ] ; then
    echo "Please set the SPACKDIR environment variable for loading"
    echo "the relevant modules"
    exit 1
  fi

  module use $SPACKDIR/spack/modules/linux-rhel9-zen2
  module load gcc/12.2.0-gcc-12.2.0-elnqzkg
  module load mpich/4.2.3-gcc-12.2.0-vqupwpn
  unset SHUM_TMPDIR
  CONFIG=meto-azspice-gfortran-gcc
  LIBDIR=$BUILD_DESTINATION/azspice-gfortran-$(gfortran -dumpversion)-gcc-$(gcc -dumpversion)
  build_openmp_onoff $CONFIG "$LIBDIR" all_libs
  if [ $? -ne 0 ] ; then
      >&2 echo "Error compiling for $THIS"
      exit 1
  fi
  RUN_SUCCESS=1
fi

# Crayftn/CrayCC 15.0.0
THIS="ex1a_cray_15.0.0"
if [ "$PLATFORM" == "ex1a" ] || [ "$PLATFORM" == $THIS ] ; then

    unset SHUM_TMPDIR
    # If it is not the case that $CYLC_TASK_WORK_PATH is unset, use it to
    # define $SHUM_TMPDIR
    if [ ! -z "${CYLC_TASK_WORK_PATH+x}" ]; then
      export SHUM_TMPDIR=$CYLC_TASK_WORK_PATH/meto-ex1a-crayftn-15.0.0-craycc-15.0.0
    fi

    (
    module switch PrgEnv-cray PrgEnv-cray/8.4.0
    module load cpe/23.05
    module swap cce cce/15.0.0
    module load craype-x86-milan

    CONFIG=meto-ex1a-crayftn12.0.1+-craycc
    LIBDIR=$BUILD_DESTINATION/meto-ex1a-crayftn-15.0.0-craycc-15.0.0
    read -ra TEST_LIBS < <(sed -e "s/\bshum_fieldsfile_class\b//g" \
                               -e "s/\bshum_fieldsfile\b//g" \
                               <<< "$LIB_DIRS")
    build_openmp_onoff $CONFIG "$LIBDIR" "${TEST_LIBS[@]}"
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
    RUN_SUCCESS=1
fi

# Gfortran 12.2.0
THIS="ex1a_gnu_12.2.0"
if [ "$PLATFORM" == "ex1a" ] || [ "$PLATFORM" == $THIS ] ; then

    unset SHUM_TMPDIR
    # If it is not the case that $CYLC_TASK_WORK_PATH is unset, use it to
    # define $SHUM_TMPDIR
    if [ ! -z "${CYLC_TASK_WORK_PATH+x}" ]; then
      export SHUM_TMPDIR=$CYLC_TASK_WORK_PATH/meto-ex1a-gfortran-12.2.0-gcc-12.2.0
    fi

    (
    module switch PrgEnv-cray PrgEnv-gnu/8.4.0
    module load cpe/23.05
    module swap gcc gcc/12.2.0
    module load craype-x86-milan

    CONFIG=meto-ex1a-gfortran-gcc
    LIBDIR=$BUILD_DESTINATION/meto-ex1a-gfortran-12.2.0-gcc-12.2.0
    build_openmp_onoff $CONFIG "$LIBDIR" all_libs
    )
    if [ $? -ne 0 ] ; then
        >&2 echo "Error compiling for $THIS"
        exit 1
    fi
    RUN_SUCCESS=1
fi

if [[ "$RUN_SUCCESS" -eq 0 ]] ; then
  echo "Did not succesfully run." | tee >(cat >&2)
  echo "(Is '${PLATFORM}' a valid platform argument?)" | tee >(cat >&2)
  exit 2
fi
