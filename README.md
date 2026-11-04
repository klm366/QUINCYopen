# QUINCY standalone

* QUINCY: QUantifying Interactions between terrestrial Nutrient CYcles and the climate system

* The QUINCY terrestrial ecosystem model is:
  * published via https://doi.org/10.17871/quincy-model-2019
  * developed at https://gitlab.dkrz.de/quincy-community/quincy-model-developers/qs-development

* QUINCY has been developed at the MPI for Biogeochemistry as an alternative, more comprehensive representation of biogeochemical processes for ICON-Land.
  * It is currently developed to run:
    * standalone, for individual sites driven by surface meteorology, and
    * coupled, integrated in ICON-Land as land-surface scheme of the global climate model ICON (https://www.icon-model.org).

* This QUINCY model version is running standlone driven by external forcing at site-level.

* Model documentation is available at:
  * https://jsbach.gitlab-pages.dkrz.de/jsbach
  * https://docs.icon-model.org/2026.10/documentation/land/land.html#quincy

* Please use HTTPS for cloning the repository
* Compile the QUINCY model with `./compile_quincy.sh`
* Software requirements for compiling: `make`, `cpp` (C preprocessor), `gfortran` (GNU Fortran compiler), `python`

* Feed providing information about new releases: https://git.bgc-jena.mpg.de/quincy-model/qs-open-access.atom
* For inquiries and bug reports please write to: `qs-open-access >at< bgc-jena.mpg.de`
