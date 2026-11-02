
# Release notes for 202Y.MM-X--QSn
### scientific changes


### model infrastructure


### scripting environment


### changes only relevant for ICON-Land



# Release notes for 2026.10
### scientific changes
* /

### model infrastructure
* Fixed a internal cray compiler error by refactoring the use of the canopy_conductance_scheme namelist parameter:
it is now an integer from an enumator instead of a string.
* Replace INT by NINT to properly determine the nearest integer preventing runtime errors
* Use `mo_util_string` directly instead of the `mo_jsb_utils_iface` adapter module (removed), syncronize with an ICON-Land change
* Fix unrealistic soil temperature changes during water freezing or ice melting periods, syncronize with ICON-Land
* Improve inline code documentation of the SSE modules, syncronize with ICON-Land
* Implement new working-precision kind mo_kind::lp, syncronize with ICON-Land
* Fix `is_newyear` not returning TRUE at the first timestep of the transient simulation after spinup, and make
`is_newyear`/`is_newday` robust against round-off in the accumulated `time_day` (QS time adapter and driver only)

### scripting environment
* Update the prepare_sim_quincy.sh for Trinity College Dublin (TCD) after changes of soil physics processes

### meta data
* Added AGENTS.md file to provide some persistent memory describing the model for AI coding agents.

### changes only relevant for ICON-Land
* Reduce the number of aggregated variables (ASSIMI, VEG) in canopy model-mode


# Release notes for 2026.04--QS1
### scientific changes
* Remove the SPQ_ process: soil physics quincy, a rather simple version of the jsbach physics processes

### model infrastructure
* Improve inline code documentation - syncronize with ICON-Land
* Cleanup model infrastructure: remove and refactor all code related to operator overload procedures of `t_jsb_pool` and rename to `t_lnd_bgcm`
* Add new LOGICAL option `l_exclude_weathering`, used to disable P weathering in SB_ process during model spin-up

### changes only relevant for ICON-Land
* Enable output for additional P and C isotope variables in ICON-Land
* Remove all `__NO_QUINCY__` cpp flags to reduce code complexity in ICON-Land, not relevant for QS


# Release notes for 2026.04
### model infrastructure
* Change default configuration from SPQ_ process to jsbach-physics processes, modify and rename usecases accordingly
* Syncronize against ICON-Land: add new routine to read soil data bc files
* Move the directory `./contrib/compile_hlp/` into the `./build/` directory
* Set the default number of soil layers to 5 (quincy_nsoil_e, quincy_nsoil_w) for only QUINCY standalone (differs from ICON-Land default)
* Disable `finish()` for failed checks of consistency between start/end year in forcing file and namelist, if output is ascii txt

### scripting environment
* Move `data/basic_output_variables.txt` to `contrib/netcdf_output/basic_output_variables.txt`, rename associated config variable to `netcdf_output_var_list`


# Release notes for 2025.10-2--QS3
### scientific changes
* improvement of temperture acclimation on BNF and N uptake to avoid cold system dying
* bugfix in the calculation of nutrient vertical transport with jsbach physics
* bugfix in the calculation of relative plant available water in the presence of supercooled water
* bugfix in the calculation of fractional water leaching in the presence of ice 
* bugfix in the calculation of asymbiotic N fixation and litter decomposition in simple soil-model (SSM), Carbon-only configuration
* add interactive nitrogen and phosphorus flags in soil biogeochemistry
* remove the code specific to the SPP 1685 project
* refactor calc_langmuir_kinetics subroutine in SB process to avoid devision by zero

### changes only relevant for ICON-Land
* copied changes in shared files resulting in ICON-Land from a global diagnostics refactoring
* copied changes in shared files resulting in ICON-Land from updates in code not used with QS (tmx, jsbach-only HYDRO parts, l_snow switch)

### model infrastructure
* refactoring to simplify the spinup accelerator, suppress carbon-conservation test after accelerator execution
* Split-off mo_quincy_output_class parts only required in QS (and only when using netcdf)
* Clean up use of mo util: remove duplicated routines and better adhere to the adaptor concept
* Change default configuration from SPQ_ process to jsbach-physics processes, modify and rename usecases accordingly

### scripting environment
* Add new documentation `code_development_guidelines.md` providing a brief overview about how to contribute to QS code development
* bugfix model-data comparison scripts for postprocessing fluxnet and cruncep simulations (correction of model output filenames)
* improve and correct postprocessing, comparison against reference runs, add test for simulation errors (`ERROR.txt`)


# Release notes for 2025.10-2--QS2
### scientific changes
* /

### model infrastructure
* minor adaptations due to changes in modules shared with IQJ - resulting from enabling anthropogenic land use change (ALCC) with IQJ
* update netcdf output for runs with jsb physics
* update the time range of the BSD-3C licence Copyright to 2026
* refactor vegetation memory structure (add seed-bed bgcm) to reduce code complexity and facilitate land-use change calculations
* rename binary and namelist to match the model name: quincy standalone (QS)

### scripting environment
* fix and update scripts used at Trinity College Dublin with missing flags and options
* change default compile settings of long-running reference runs from "debug -O0" to "standard -O2"
* update and improve gitlab templates for merge requests and issues


# Release notes for 2025.10-1--QS1
### scientific changes
* enable running the Jena-Soil-Model (JSM) in ICON-Land
* improve tuning of vmax of vegetation Phosphorous uptake `vmax_uptake_p` lctlib parameter
* change the lctlib parameter of the vmax of vegetation Nitrogen uptake `vmax_uptake_n` to have more reasonable global Carbon and Nitrogen budgets
* improve the calculation of plant water stress with frozen soil
* update of default 13C and 14C values to 1850 values reported by Graven, H (2025)  doi: `10.25981/ESGF.input4MIPs.CMIP7/2583902`

### model infrastructure
* re-write QS ascii text output module adding selectable groups of output files
* improve namelist names, applying uniform ICON-Land names of namelists (lnd_[process]_nml) across QS and IQ
* correction of timing and counting of running the SB_ slow pool spinup accelerator
* correction of aggregation of area-dependend variables in ICON-Land
* add missing and correct wrongly calculated output variables in ICON-Land to write-out all required output variables for diagnostics and global budget calculations

### scripting environment
* improve script for creating QS releases: `create_qs_release.sh`
* add new gitlab templates for merge requests and issues
* modify usecase 36, add new u37,38,39, medium-long test simulations with SPQ_, jsbach soil-physics, simple soil model and jsm, land model-model
* add a new site list TIBETP for the Tibetan Plateau, with transient and static forcing, and daily time-steps, 9 sites, permafrost-affected and mountainous
* modify `prepare_sim_quincy.sh` runscript, add new slurm partition (MPI-BGC, Jena), increase max runtime of jobs to minimize the risk of jobs being finished at 'maxtime' due to high runtime at high i/o load at the HPC
* update scripts for creating and preprocessing drylands site-set forcing with the new data directly from Fluxnet  


# QS version consistent with icon-2025.10 release
* tag 2025-10-icon-model-release
  * https://gitlab.dkrz.de/quincy-community/quincy-model-developers/qs-development/-/tags/2025-10-icon-model-release

