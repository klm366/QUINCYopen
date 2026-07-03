#!/bin/bash

##
## used by the compile_quincy.sh
##
## gathers information about the git status and options used to compile the binary
## saves this information in two files used by, e.g., postprocessing scripts
##
## does only a simple check about the function arguments provided 
## 

echo ""
echo "  ## sourcing the contrib/compile_hlp/create_git_binary_info_files.sh"
echo ""

# function --------------------------------------------------------------------------------------------------
function create_git_binary_info_files {

#--- function arguments
username="$1"
currentDate="$2"
compiler="$3"
compile_script_location="$4"
buildDir="$5"
binaryBuildDir="$6"
flagNetcdf="$7"
binary_git_info_file="$8"
binary_git_info_file_short="$9"

#--- brief check of arguments
## number of arguments
numArgsExpected=9
numArgsProvided="$#"
if [[ ${numArgsExpected} -ne ${numArgsProvided} ]]; then
  echo "  the number of arguments provided to the function 'create_git_binary_info_files'"
  echo "    does not match the number of arguments expected"
  echo "EXIT!"
  exit 1
fi
## build dir and binaryBuild dir exist
if [ ! -d ${compile_script_location}/${buildDir} ]; then
  echo "EXIT! build directory (with Makefile) not found -- please check your quincy clone."
  exit 1
fi
if [ ! -d ${compile_script_location}/${binaryBuildDir} ]; then
  echo "EXIT! build directory (with binary) not found -- please check your quincy clone."
  exit 1
fi

#--- cd into the dir the compile script is located (usually the main dir of the git repo)
cd ${compile_script_location}

#--- check if this dir is part of a git repro (exit status == 0)
git log > /dev/null 2>&1
exitCode=$?

#--- collect info from git
if [[ ${exitCode} -eq 0 ]]; then 
  ## get the first 8 digits of the git commit sha and the commit header
  gitCommitNum=$(git rev-parse --short=8 HEAD)
  gitCommitHead=$(git log -1 --pretty=short | tail -n 1)
  ## console output
  echo ""
  echo "  The git commit-number of your local git repo and branch which you just compiled is: "${gitCommitNum}
  echo "  (short commit info:"${gitCommitHead}")"
  ## extended info saved into file 'binary_git_info_file'
  echo "Compiled on ${currentDate} by User: ${username}"   > ${buildDir}${binary_git_info_file}
  echo "from commit ${gitCommitNum}"                      >> ${buildDir}${binary_git_info_file}
  echo "$(git status | head -n 1)"                        >> ${buildDir}${binary_git_info_file}
  echo ""                                                 >> ${buildDir}${binary_git_info_file}
  echo "Commit header:"                                   >> ${buildDir}${binary_git_info_file}
  echo ${gitCommitNum}${gitCommitHead}                    >> ${buildDir}${binary_git_info_file}
  echo ""                                                 >> ${buildDir}${binary_git_info_file}
  echo "Changed files (if any):"                          >> ${buildDir}${binary_git_info_file}
  echo "$(git status -s)"                                 >> ${buildDir}${binary_git_info_file}
  echo ""                                                 >> ${buildDir}${binary_git_info_file}
  echo ""                                                 >> ${buildDir}${binary_git_info_file}
  echo "Detailed commit message:"                         >> ${buildDir}${binary_git_info_file}
  echo "$(git show -s $gitCommitNum)"                     >> ${buildDir}${binary_git_info_file}
  echo " "                                                >> ${buildDir}${binary_git_info_file}
  echo " "                                                >> ${buildDir}${binary_git_info_file}
  echo "========================================="        >> ${buildDir}${binary_git_info_file}
  echo " "                                                >> ${buildDir}${binary_git_info_file}
  echo "Detailed status:"                                 >> ${buildDir}${binary_git_info_file}
  echo "$(git status)"                                    >> ${buildDir}${binary_git_info_file}
  echo " "                                                >> ${buildDir}${binary_git_info_file}
  echo " "                                                >> ${buildDir}${binary_git_info_file}
  echo "========================================="        >> ${buildDir}${binary_git_info_file}
  echo " "                                                >> ${buildDir}${binary_git_info_file}
  echo "Diff:"                                            >> ${buildDir}${binary_git_info_file}
  echo "$(git diff)"                                      >> ${buildDir}${binary_git_info_file}
  ## short info for postprocessing scripts saved into file 'binary_git_info_file_short'
  gitBranchName=$(git rev-parse --abbrev-ref HEAD)
  gitShortStatusFirstLetters=$(git status -s | head -n 1 | cut -c 1-2)
  if [[ $gitShortStatusFirstLetters == "M" ]]; then
    gitStatus="pending_commit"
  elif [[ $gitShortStatusFirstLetters == " M" ]]; then
    gitStatus="modified"
  else
    gitStatus="unchanged"
  fi
  echo "commit branch status:"                           > ${buildDir}${binary_git_info_file_short}
  echo "${gitCommitNum} ${gitBranchName} ${gitStatus}"  >> ${buildDir}${binary_git_info_file_short}
  echo "compiled with netcdf-libraries:"                >> ${buildDir}${binary_git_info_file_short}
  echo ${flagNetcdf}                                    >> ${buildDir}${binary_git_info_file_short}
  echo "compiler:"                                      >> ${buildDir}${binary_git_info_file_short}
  echo ${compiler}                                      >> ${buildDir}${binary_git_info_file_short}
  ## move the two info files to the build dir with binary and lib & mod files
  mv  ${buildDir}${binary_git_info_file}        ${binaryBuildDir}${binary_git_info_file}
  mv  ${buildDir}${binary_git_info_file_short}  ${binaryBuildDir}${binary_git_info_file_short}
else
  ## if there is no git information, i.e., no git repo
  echo "The commit number of your local git repo which you just attempted to compile could not be identified, please check!"
  echo "EXIT!"
  exit 1
fi

#--- return value of normal exit
return 0

}
