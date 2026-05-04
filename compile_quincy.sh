#!/bin/bash

#!> Compile script for MPI-BGC
#!>
#!> QUINCY standalone
#!>
#!> ---------------------------------------
#!> Copyright (C) 2013-2026, MPI-M, MPI-BGC
#!>
#!> Contact: https://doi.org/10.17871/quincy-model-2019
#!> Authors: AUTHORS.md
#!> See LICENSES/ for license information
#!> SPDX-License-Identifier: BSD-3-Clause
#!> ---------------------------------------
#!>
#!>#### compile quincy code at MPI-BGC
#!>

# run this script by executing: ./compile_quincy.sh [SETUP]
# 
# this script compiles quincy according to the specified SETUP
#   run './compile_quincy.sh -h' to get a list of compile setups available
#
# in addition it collects information on the current git status and stores it alongside the binary into the files:
#     ${binary_git_info_file}
#     ${binary_git_info_file_short}
#


#--- specify setup here or pass as argument
compile_setup="$1"

#--- compile setups available (long and short versions are working):
## keep the previously selected one
current_setups="update (up)"
## with ascii txt output
current_setups="${current_setups}, g-debug (gd), g-debug-scrutinise (gds), g-standard (gs), g-perform (gp)"
current_setups="${current_setups}, i-debug (id), i-standard (is), i-perform (ip)"
## with netcdf4 output
current_setups="${current_setups}, g-debug-netcdf (gdn), g-debug-scrutinise-netcdf (gdsn), g-standard-netcdf (gsn), g-perform-netcdf (gpn)"
#current_setups="${current_setups}, i-debug-netcdf (idn), i-standard-netcdf (isn), i-perform-netcdf (ipn)"

#--- specification of default settings
default_compile_setup="update"      # compile setup used if no argument is provided to the compile script
fallback_compile_setup="g-debug"    # use fallback if "update" is selected but no compile setup has been selected previously

#--- main variables, directories and output files
## values are defined here (other scripts, called in the compile script, might expect values exactly as defined here)
cd "$( dirname "$0" )"                              # change into the directory where this script is located, and
compile_script_location=$(pwd)                      #   save this dir into a variable
currentDate=$(date '+%Y-%m-%d %H:%M:%S')            # current date of executing this compile script
username=$(whoami)                                  # user who is executing
buildDir="build/"                                   # build dir with Makefile
dsl4jsbSrcDir="src_dsl4jsb/"                        # directory in $buildDir with src files that the "python for dsl4jsb script" was applied to
compileOptionsInclFile="make_compile-options.incl"  # contains all compile options, included by the Makefile
binary_git_info_file="binary_git_info.txt"          # extensive information about the git status the binary was compiled with
binary_git_info_file_short="binary_sinfo.txt"       # short     information about the git status and compile options the binary was compiled with
## values are defined below (i.e., values defined here will be overwritten)
compiler="gfortran"                                 # compiler used to compile the code [gfortran,ifort]
flagNetcdf="FALSE"                                  # use netcdf for compilation so that, e.g., netcdf model output is possible [FALSE,TRUE]
flagUpdateCompileSetup="FALSE"                      # compile with compile_setup "update (up)", i.e., compile only code changes
flagMpiBgcSlurmCluster="FALSE"                      # executing this script at a MPI-BGC slurm-cluster node [FALSE,TRUE]
binaryBuildDir="x86_64-gfortran/"                   # build dir with info files, binary, lib & mod files; compiler specific

#--- source modules
## a bash script providing the function 'create_git_binary_info_files'
source ./build/compile_hlp/create_git_binary_info_files.sh
## the 'compile_options.incl' is sourced below

#--- help
if [[ ${compile_setup} == "-h" ]] || [[ ${compile_setup} == "--help" ]]; then
  echo " "
  echo " You get this help with either argument: '-h' and '--help'."
  echo " "
  echo " Call this script with one argument to set the compiler setup."
  echo " Example: './compile_quincy.sh gd'  for the gfortran compiler with debug options."
  echo " Example: './compile_quincy.sh gdn' for the gfortran compiler with debug options and netcdf simulation-output."
  echo " "
  echo " Compiler setups available:"
  echo " "${current_setups}
  echo " "
  echo " In case you do not give an argument the default is used: "${default_compile_setup}
  echo " "
  exit 0
fi

#--- change into build/ dir with Makefile
## test existence of the build dir
if [ ! -d ${buildDir} ]; then
  echo "EXIT! build directory not found -- please check your quincy clone."
  exit 1
fi
## change into the build dir
cd ${buildDir}

#--- select compile options
## use default compiler setup if none is selected, i.e., no argument is provided to the compile script
## or remove all previously created temporary files, if "update" is not selected
if [[ ${compile_setup} == "" ]]; then
  compile_setup=${default_compile_setup}
fi
## use fallback compiler setup, if "update" is selected but no compile setup has been selected previously
if [[ ${compile_setup} == "update" ]] || [[ ${compile_setup} == "up" ]]; then
  # check if the "compile options file" is available
  if [[ -e ${compileOptionsInclFile} ]]; then 
    # if available: everything is already set-up ... keeping files files from previous compilations
    echo "  Update - compiling only code changes and dependencies"
    flagUpdateCompileSetup="TRUE"
  else
    # if file was not yet created, use fallback compile setup
    echo "  no compiler settings selected"
    echo "  select fallback compiler setup: "${fallback_compile_setup}
    compile_setup=${fallback_compile_setup}
    flagUpdateCompileSetup="FALSE"
  fi
else
  # if compile_setup is not "update", remove all files from previous compilations
  # needs the "compile options file" to be present, even if empty
  touch ${compileOptionsInclFile}
  make clean
  # remove "compile options file"
  rm -f ${compileOptionsInclFile}
  # remove the directory with 'dsl4jsb'-processed files
  rm -rf ${dsl4jsbSrcDir}
  echo "  removed "${dsl4jsbSrcDir}
  # sync filesystem
  sync
  flagUpdateCompileSetup="FALSE"
fi

#--- run the dsl4jsb script
echo ""
echo "  ## start the dsl4jsb script"
# create new directory for processed files, if not already present
if [[ ! -d ${dsl4jsbSrcDir} ]]; then 
  mkdir ${dsl4jsbSrcDir}
fi
python3 ../scripts/dsl4jsb/dsl4jsb.py -k -d ../src -t ${dsl4jsbSrcDir}
# get exit code of the dsl4jsb.py script (0 == all good)
exitCodeDSLpy=$?

# exit compile_quincy.sh if dsl4jsb.py exited with an error
if [ ${exitCodeDSLpy} == 0 ]; then
  echo "  ## dsl4jsb script done"
else
  echo "  ## the dsl4jsb script did exit with an ERROR"
  echo "  EXIT! compile script"
  echo " "
  exit 1
fi

#--- create the "build/make_compile-options.incl", used by the Makefile, accordingly to the compile_setup
source ./../build/compile_hlp/compile_options.incl

#--- select and load the correct compiler (& netcdf4) software modules
echo " "
## gfortran / ifort
if [[ ${compiler} == "gfortran" ]]; then
  compilerModule="gnu12"
elif [[ ${compiler} == "ifort" ]]; then
  compilerModule="intel"
fi
## netcdf4 output
if [[ ${flagNetcdf} == "TRUE" ]]; then
  compileAddModulesNetcdf="openmpi4 phdf5 netcdf"
  echo "  Model output will be provided as netcdf4 files."
else
  echo "  Model output will be provided as ascii files."
fi
## load modules (only at MPI-BGC slurm-cluster nodes)
## test if this script is executed on a node of the MPI-BGC slurm-cluster (test if hostname contains "node-r")
hostname | grep -i "node-r" > /dev/null 2>&1
exitCode="$?"
if [[ $exitCode -ne 0 ]]; then
  echo "-----"
  echo " You are running this script outside the MPI-BGC HPC. The modules for the compiler are not loaded."
  echo "-----"
  flagMpiBgcSlurmCluster="FALSE"
else
  flagMpiBgcSlurmCluster="TRUE"
fi
## load the modules if working on the MPI-BGC slurm-cluster
if [[ ${flagMpiBgcSlurmCluster} == "TRUE" ]]; then
  # rm all modules, just for safety
  module -q purge
  # compiler
  module -q load ${compilerModule}
  echo "  "${compilerModule}" module is loaded"
  # model output as netcdf4 files
  if [[ ${flagNetcdf} == "TRUE" ]]; then
    module -q load ${compileAddModulesNetcdf}
    echo "  "${compileAddModulesNetcdf}" modules are loaded"
  fi
fi
## if outside the MPI-BGC slurm-cluster:
##  no attempt to load any software modules if outside MPI-BGC slurm-cluster
##  test for the selected compiler
##  no test for netcdf4 !
if [[ ${flagMpiBgcSlurmCluster} == "FALSE" ]]; then
  # pass the compiler selected for MPI-BGC to the "outside MPI-BGC compiler"
  compilerOutsideMpiBgcSlurmCluster=${compiler}
  # test availability of selected compiler
  ${compilerOutsideMpiBgcSlurmCluster} --version  > /dev/null 2>&1
  exitCode="$?"
  if [[ $exitCode -ne 0 ]]; then
    echo " "
    echo "EXIT! The selected compiler "${compilerOutsideMpiBgcSlurmCluster}" seems to be NOT available on your system."
    exit 1
  fi
fi
echo " "

#--- make
## compile the code ('time' is just measuring the time and writes it to stdout)
time make -j 4
exitCode="$?"
echo " "

#--- feedback about compiling the code
echo " "
if [[ $exitCode -eq 0 ]]; then
  echo "  the QUINCY binary was compiled with the compiler setup: "${compile_setup}" & compiler: "${compiler}
else
  echo "  some went wrong with compiling the QUINCY binary"
  echo "  compiler setup: "${compile_setup}" & compiler: "${compiler}
  echo "EXIT!"
  exit 1
fi
echo " "

#--- collect current git and binary info
create_git_binary_info_files ${username} "${currentDate}" ${compiler} ${compile_script_location} ${buildDir} ${binaryBuildDir} ${flagNetcdf} ${binary_git_info_file} ${binary_git_info_file_short}

# Fine #
