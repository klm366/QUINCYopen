!>
!!  @par Copyright
!!    QUINCY terrestrial biosphere model with coupled carbon, nitrogen, and phosphorus cycles
!!    Copyright (C) 2019 Max Planck Institute for Biogeochemistry (MPI-BGC), Jena, Germany 
!!    Contributors: Caldararu S., Engel J., Kern M., Pallandt M., Schnur R., Thum T., Yu L., Zaehle S.
!!
!!    This file is part of the QUINCY model.
!!    The QUINCY model is free software: you can redistribute it and/or modify
!!    it under the terms of the GNU General Public License version 3,
!!    as published by the Free Software Foundation.
!!
!!    The QUINCY model is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!!    GNU General Public License for more details.
!!
!!    You should have received a copy of the GNU General Public License
!!    along with the QUINCY model.  If not, see <https://www.gnu.org/licenses/>.
!! 
!! @brief dummy forcing routine for generic forcing 
!!
!! @author
!!  Sönke Zaehle, MPI-BGC Jena 
!!
!! @par Revision History
!! First version by Sönke Zaehle (2016-01-27)
!! 2nd version Jan Engel (20160825) moved memory from SLM to JSBACH4 style
!! added daily reading / WG: Julia Nabel (20220523): WG based on jsbach3/4 WG with some ideas from OCN WG
!!
MODULE mo_qs_atmland_forcing

  USE mo_kind,                  ONLY: wp
  USE mo_util,                  ONLY: int2string
  USE mo_jsb_control,           ONLY: debug_on
  USE mo_exception,             ONLY: message, finish, message_text
  USE mo_jsb4_forcing,          ONLY: forcing_options
  USE mo_jsb_math_constants,    ONLY: eps4, eps8, one_day, one_year, dtime, pi, deg2rad

  USE mo_lnd_bgcm_idx
  USE mo_lnd_bgcm_store,          ONLY: t_lnd_bgcm_store
  USE mo_lnd_bgcm_store_class,    ONLY: VEG_BGCM_POOL_ID, VEG_BGCM_LITTERFALL_ID

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: update_forcing, open_forcing_input_file, close_forcing_input_file

  CHARACTER(len=*), PARAMETER :: modname = 'mo_qs_atmland_forcing'

CONTAINS

  ! ======================================================================================================= !
  !>update forcing variables per timestep & read forcing data per timestep from file climate.dat
  !> 
  !> called in the time loop in plant_model_driver subsequently to advancing the timer
  !> 
  !> read one line from file per timestep \n
  !> if file ends, start again at top of file with reading lines \n
  !> parameter units are re-scaled accordingly to the needs of SLM \n
  !> the two header-lines may be neglected \n
  !> write column headers of output files for fluxes \n
  !>
  !
  ! @NOTE: the .dat forcing files unfortunately usually have "swvis_srf_down" instead of "sw_srf_down"
  !        in the according column header, although they do carry "sw_srf_down"
  !
  SUBROUTINE update_forcing(  tile               , &  ! inout
                              ics                , &  ! in
                              ice                , &  ! in
                              nc                 , &  ! in
                              iblk               , &  ! in
                              timestep           , &  ! in
                              time               , &  ! in
                              timestep_prop_day  , &  ! in
                                  t_air              , &  ! out
                                  q_air              , &  ! out
                                  rain               , &  ! out
                                  snow               , &  ! out
                                  wind_air           , &  ! out
                                  wind_10m           , &  ! out
                                  lw_srf_down        , &  ! out
                                  swvis_srf_down     , &  ! out
                                  swnir_srf_down     , &  ! out
                                  swpar_srf_down     , &  ! out
                                  fract_par_diffuse  , &  ! out
                                  press_srf          , &  ! out
                                  drag_srf           , &  ! out
                                  t_acoef            , &  ! out
                                  t_bcoef            , &  ! out
                                  q_acoef            , &  ! out
                                  q_bcoef            , &  ! out
                                  pch                , &  ! out
                                  cos_zenith_angle   , &  ! out
                                  co2_mixing_ratio     , &! out
                                  co2_mixing_ratio_C13 , &! out
                                  co2_mixing_ratio_C14 , &! out
                                  ga                   , &! out
                                  nhx_deposition     , &  ! out
                                  noy_deposition     , &  ! out
                                  nhx_n15_deposition , &  ! out
                                  noy_n15_deposition , &  ! out
                                  p_deposition       , &  ! out
                                  ! For lakes:
                                  albedo_lwtr        , &  ! out
                                  drag_wtr           , &  ! out
                                  drag_ice           , &  ! out
                                  t_acoef_wtr        , &  ! out
                                  t_bcoef_wtr        , &  ! out
                                  q_acoef_wtr        , &  ! out
                                  q_bcoef_wtr        , &  ! out
                                  t_acoef_ice        , &  ! out
                                  t_bcoef_ice        , &  ! out
                                  q_acoef_ice        , &  ! out
                                  q_bcoef_ice          &  ! out
                                  &  )

    USE mo_jsb_class,              ONLY: Get_model
    USE mo_quincy_model_config,    ONLY: QLAND, QPLANT, QSOIL, QCANOPY, Q_TEST_CANOPY, Q_TEST_RADIATION
    USE mo_jsb_process_class,      ONLY: A2L_, SPQ_, VEG_
    USE mo_jsb_grid_class,         ONLY: t_jsb_grid, t_jsb_vgrid
    USE mo_jsb_grid,               ONLY: Get_Grid, Get_vgrid
    USE mo_jsb_tile_class,         ONLY: t_jsb_tile_abstract
    USE mo_jsb_model_class,        ONLY: t_jsb_model
    USE mo_jsb_physical_constants, ONLY: molar_mass_N, molar_mass_P, grav
    USE mo_isotope_util,           ONLY: calc_mixing_ratio_C13C12, calc_mixing_ratio_C14C, calc_delta_C13, calc_delta_C14, &
      &                                  calc_mixing_ratio_N15N14
    USE mo_qs_atmland_util,        ONLY: update_orbital_parameters, calc_solar_angle, calc_fract_par_diffuse
    USE mo_atmland_constants,      ONLY: def_co2_deltaC13, def_co2_deltaC14, &
      &                                  frac_vis_swtot_srf, frac_par_swvis_srf
    USE mo_qs_atmland_constants,   ONLY: declination
    ! ----------------------------------------------------------------------------------------------------- !
    dsl4jsb_Use_memory(A2L_)
    dsl4jsb_Use_memory(SPQ_)
    dsl4jsb_Use_memory(VEG_)
    ! ----------------------------------------------------------------------------------------------------- !
    CLASS(t_jsb_tile_abstract), INTENT(inout) :: tile                           !< one tile with data structure for one lct
    INTEGER                   , INTENT(in)    :: ics, ice, nc, iblk             !< info on block, chunk and soil layer number
    INTEGER                   , INTENT(in)    :: timestep                       !< timestep; first timestep of static simulation and simulation after transient spinup is 1
    REAL(wp)                  , INTENT(in)    :: time                           !< current time in days (not timestep !)
    REAL(wp)                  , INTENT(in)    :: timestep_prop_day              !< proportion of one day that is simulated with one timestep (dtime/one_day)
    REAL(wp), OPTIONAL        , INTENT(out)   :: &                 !! these are all variables from a2l memory; with assumed shape VAR(1:nc)
                                      t_air(:)              , &    !< ..
                                      q_air(:)              , &    !< ..
                                      rain(:)               , &    !< ..
                                      snow(:)               , &    !< ..
                                      wind_air(:)           , &    !< ..
                                      wind_10m(:)           , &    !< ..
                                      lw_srf_down(:)        , &    !< ..
                                      swvis_srf_down(:)     , &    !< ..
                                      swnir_srf_down(:)     , &    !< ..
                                      swpar_srf_down(:)     , &    !< ..
                                      fract_par_diffuse(:)  , &    !< ..
                                      press_srf(:)          , &    !< ..
                                      drag_srf(:)           , &    !< ..
                                      t_acoef(:)            , &    !< ..
                                      t_bcoef(:)            , &    !< ..
                                      q_acoef(:)            , &    !< ..
                                      q_bcoef(:)            , &    !< ..
                                      pch(:)                , &    !< ..
                                      cos_zenith_angle(:)   , &    !< ..
                                      co2_mixing_ratio(:)     , &  !< ..
                                      co2_mixing_ratio_C13(:) , &  !< ..
                                      co2_mixing_ratio_C14(:) , &  !< ..
                                      ga(:)                   , &  !< ..
                                      nhx_deposition(:)     , &    !< ..
                                      noy_deposition(:)     , &    !< ..
                                      nhx_n15_deposition(:) , &    !< ..
                                      noy_n15_deposition(:) , &    !< ..
                                      p_deposition(:)       , &    !< ..
                                      ! For lakes:
                                      albedo_lwtr(:)        , &    !< ..
                                      drag_wtr(:)           , &    !< ..
                                      drag_ice(:)           , &    !< ..
                                      t_acoef_wtr(:)        , &    !< ..
                                      t_bcoef_wtr(:)        , &    !< ..
                                      q_acoef_wtr(:)        , &    !< ..
                                      q_bcoef_wtr(:)        , &    !< ..
                                      t_acoef_ice(:)        , &    !< ..
                                      t_bcoef_ice(:)        , &    !< ..
                                      q_acoef_ice(:)        , &    !< ..
                                      q_bcoef_ice(:)               !< ..
    ! ----------------------------------------------------------------------------------------------------- !
    TYPE(t_jsb_model),      POINTER  :: model                  !< the model structure
    TYPE(t_lnd_bgcm_store), POINTER  :: bgcm_store             !< the bgcm store of this tile
    TYPE(t_jsb_grid),       POINTER  :: hgrid                  !< horizontal grid
    TYPE(t_jsb_vgrid),      POINTER  :: vgrid_soil_sb          !< Vertical grid
    INTEGER                          :: nsoil_sb               !< number of soil layers as used/defined by the SB_ process
    ! ----------------------------------------------------------------------------------------------------- !
    ! READ() input variables for: land plant canopy and soil (these variable names can differ from column names in forcing files)
    REAL(wp) ::      sw_srf_down_r,      lw_srf_down_r, t_air_r,    q_air_r,    &
                     press_srf_r,        rain_r,        snow_r,     wind_air_r, &
                     co2_mixing_ratio_r, co2_dC13_r,    co2_DC14_r,             &
                     nhx_deposition_r,   noy_deposition_r, &   ! variables reading nhx_srf_down & noy_srf_down from the climate forcing file (maybe rename there)
                     nhx_dn15_deposition_r, noy_dn15_deposition_r, & 
                     p_deposition_r, f_n_demand_r, f_p_demand_r 
    ! READ() input variables for: soil
    REAL(wp) ::      leaf_carbon_r, leaf_nitrogen_r, leaf_phosphorus_r    , &
                     leaf_dC13_r, leaf_DC14_r, leaf_dN15_r  , &
                     fine_root_carbon_r, fine_root_nitrogen_r, fine_root_phosphorus_r   , &
                     fine_root_dC13_r, fine_root_DC14_r, fine_root_dN15_r , &
                     coarse_root_carbon_r, coarse_root_nitrogen_r, coarse_root_phosphorus_r   , &
                     coarse_root_dC13_r, coarse_root_DC14_r, coarse_root_dN15_r , &
                     sap_wood_carbon_r, sap_wood_nitrogen_r, sap_wood_phosphorus_r    , &
                     sap_wood_dC13_r, sap_wood_DC14_r, sap_wood_dN15_r  , &
                     heart_wood_carbon_r, heart_wood_nitrogen_r, heart_wood_phosphorus_r    , &
                     heart_wood_dC13_r, heart_wood_DC14_r, heart_wood_dN15_r  , &
                     labile_carbon_r, labile_nitrogen_r, labile_phosphorus_r    , &
                     labile_dC13_r, labile_DC14_r, labile_dN15_r  , &
                     reserve_carbon_r, reserve_nitrogen_r, reserve_phosphorus_r   , &
                     reserve_dC13_r, reserve_DC14_r, reserve_dN15_r , &
                     fruit_carbon_r, fruit_nitrogen_r, fruit_phosphorus_r   , &
                     fruit_dC13_r, fruit_DC14_r, fruit_dN15_r , &
                     seed_bed_carbon_r, seed_bed_nitrogen_r, seed_bed_phosphorus_r  , &
                     seed_bed_dC13_r, seed_bed_DC14_r, seed_bed_dN15_r
    REAL(wp) ::      veg_pool_fine_root_carbon_r
    REAL(wp), ALLOCATABLE, DIMENSION(:) :: & 
                     t_soil_sl_r, &
                     w_soil_sl_r, &
                     percolation_sl_r, &
                     root_fraction_sl_r
    ! other local variables
    INTEGER                     :: stat                           !< status of I/O (https://software.intel.com/en-us/node/691976)
    INTEGER                     :: lun                            !< logical unit number for file input stream
    REAL(wp), DIMENSION(nc)     :: sw_toa_down                    !< potential radiation at the top of the atmosphere
    REAL(wp), DIMENSION(nc)     :: hlp1                           !< dummy variable 
    REAL(wp)                    :: year                           !< dummy variable
    REAL(wp)                    :: hour                           !< dummy variable
    REAL(wp)                    :: time_hlp                       !< dummy variable for soil forcing
    REAL(wp), SAVE              :: day                            !< read input dummy variable - save value
    REAL(wp)                    :: doy_old                        !< check for new year (column 2 in input file would change from 365 -> 1)
    REAL(wp)                    :: doy_new                        !< check for new year (column 2 in input file would change from 365 -> 1)
    REAL(wp)                    :: lon, lat                       !< lon and lat - required to calculate timestep from daily forcing
    INTEGER                     :: nlevel_veg   = -1              !< number of levels at the Z-Axis (canopy layer)
    INTEGER                     :: isoil, ic                      !< looping over soil layers or chunk
    LOGICAL, DIMENSION(nc)      :: l_timestep_first_year_passed   !< is TRUE for the timestep after the 1st simulation year has passed
    CHARACTER(len=*), PARAMETER :: routine = TRIM(modname)//':update_forcing'
    ! ----------------------------------------------------------------------------------------------------- !
    dsl4jsb_Def_mt2L2D :: veg_pool_mt
    dsl4jsb_Def_mt2L2D :: veg_litterfall_mt
    ! ----------------------------------------------------------------------------------------------------- !
    dsl4jsb_Def_memory(SPQ_)
    dsl4jsb_Def_memory(VEG_)
    dsl4jsb_Def_memory(A2L_)
    ! ----------------------------------------------------------------------------------------------------- !
    ! VEG_ 2D
    dsl4jsb_Real2D_onChunk      :: f_n_demand
    dsl4jsb_Real2D_onChunk      :: f_p_demand
    ! VEG_ 3D
    dsl4jsb_Real3D_onChunk      :: root_fraction_sl
    ! SPQ_
    dsl4jsb_Real3D_onChunk      :: t_soil_sl
    dsl4jsb_Real3D_onChunk      :: w_soil_sl
    dsl4jsb_Real3D_onChunk      :: percolation_sl
    ! A2L_
    dsl4jsb_Real2D_onChunk      :: nhx_deposition_acc
    dsl4jsb_Real2D_onChunk      :: nhy_deposition_acc
    dsl4jsb_Real2D_onChunk      :: p_deposition_acc
    dsl4jsb_Real2D_onChunk      :: co2_mixing_ratio_acc
    dsl4jsb_Real2D_onChunk      :: co2_dC13_acc
    dsl4jsb_Real2D_onChunk      :: co2_DC14_acc
    dsl4jsb_Real2D_onChunk      :: const_input_timestep_counter
    dsl4jsb_Real2D_onChunk      :: nhx_deposition_const
    dsl4jsb_Real2D_onChunk      :: nhy_deposition_const
    dsl4jsb_Real2D_onChunk      :: p_deposition_const
    dsl4jsb_Real2D_onChunk      :: co2_mixing_ratio_const
    dsl4jsb_Real2D_onChunk      :: co2_dC13_const
    dsl4jsb_Real2D_onChunk      :: co2_DC14_const
    ! ----------------------------------------------------------------------------------------------------- !
    IF (debug_on() .AND. iblk == 1) CALL message(TRIM(routine), 'Starting on tile '//TRIM(tile%name)//' ...')
    ! ----------------------------------------------------------------------------------------------------- !
    model         => Get_model(tile%owner_model_id)
    hgrid         => Get_grid(model%grid_id)
    vgrid_soil_sb => Get_vgrid('soil_layer_sb')
    nsoil_sb      =  vgrid_soil_sb%n_levels
    ! ----------------------------------------------------------------------------------------------------- !
    dsl4jsb_Get_memory(SPQ_)
    dsl4jsb_Get_memory(VEG_)
    dsl4jsb_Get_memory(A2L_)
    ! ----------------------------------------------------------------------------------------------------- !
    ALLOCATE(t_soil_sl_r(nsoil_sb))
    ALLOCATE(w_soil_sl_r(nsoil_sb))
    ALLOCATE(percolation_sl_r(nsoil_sb))
    ALLOCATE(root_fraction_sl_r(nsoil_sb))
    ! ----------------------------------------------------------------------------------------------------- !
    ! get bgcm matrices are needed only for the QSOIL model, hence all code is called below
    ! ----------------------------------------------------------------------------------------------------- !
    dsl4jsb_Get_var2D_onChunk(VEG_,  f_n_demand)
    dsl4jsb_Get_var2D_onChunk(VEG_,  f_p_demand)
    dsl4jsb_Get_var3D_onChunk(VEG_,  root_fraction_sl)
    ! ---------------------------
    dsl4jsb_Get_var3D_onChunk(SPQ_, t_soil_sl)
    dsl4jsb_Get_var3D_onChunk(SPQ_, w_soil_sl)
    dsl4jsb_Get_var3D_onChunk(SPQ_, percolation_sl)
    ! A2L_
    dsl4jsb_Get_var2D_onChunk(A2L_, nhx_deposition_acc)
    dsl4jsb_Get_var2D_onChunk(A2L_, nhy_deposition_acc)
    dsl4jsb_Get_var2D_onChunk(A2L_, p_deposition_acc)
    dsl4jsb_Get_var2D_onChunk(A2L_, co2_mixing_ratio_acc)
    dsl4jsb_Get_var2D_onChunk(A2L_, co2_dC13_acc)
    dsl4jsb_Get_var2D_onChunk(A2L_, co2_DC14_acc)
    dsl4jsb_Get_var2D_onChunk(A2L_, const_input_timestep_counter)
    dsl4jsb_Get_var2D_onChunk(A2L_, nhx_deposition_const)
    dsl4jsb_Get_var2D_onChunk(A2L_, nhy_deposition_const)
    dsl4jsb_Get_var2D_onChunk(A2L_, p_deposition_const)
    dsl4jsb_Get_var2D_onChunk(A2L_, co2_mixing_ratio_const)
    dsl4jsb_Get_var2D_onChunk(A2L_, co2_dC13_const)
    dsl4jsb_Get_var2D_onChunk(A2L_, co2_DC14_const)
    ! ----------------------------------------------------------------------------------------------------- !

    doy_old = 0.0_wp  ! doy in the previous timestep, used for detecting a new year

    ! give A2L_ variables a value to avoid compiler warning #6843: A dummy argument with an explicit INTENT(OUT) declaration is not given an explicit value
    ! these variables are declared in quincy_qs_model_driver as: REAL(wp), ALLOCATABLE, DIMENSION(:)
    ! and allocated using 'nc'   ALLOCATE(t_air(nc)) -> i.e. no ics:ice,iblk here !
    t_air(:)           = 0.0_wp
    q_air(:)           = 0.0_wp
    rain(:)            = 0.0_wp
    snow(:)            = 0.0_wp
    wind_air(:)        = 0.0_wp
    wind_10m(:)        = 0.0_wp
    lw_srf_down(:)     = 0.0_wp
    swvis_srf_down(:)  = 0.0_wp
    swnir_srf_down(:)  = 0.0_wp
    swpar_srf_down(:)  = 0.0_wp
    fract_par_diffuse(:) = 0.0_wp
    press_srf(:)         = 0.0_wp
    drag_srf(:)          = 1000.0_wp
    t_acoef(:)           = 0.0_wp
    t_bcoef(:)           = 0.0_wp
    q_acoef(:)           = 0.0_wp
    q_bcoef(:)           = 0.0_wp
    pch(:)               = 0.0_wp
    cos_zenith_angle(:)      = 0.0_wp
    co2_mixing_ratio(:)      = 0.0_wp
    co2_mixing_ratio_C13(:)  = 0.0_wp
    co2_mixing_ratio_C14(:)  = 0.0_wp
    ga(:)                    = 0.0_wp
    nhx_deposition(:)        = 0.0_wp
    noy_deposition(:)        = 0.0_wp
    nhx_n15_deposition(:)    = 0.0_wp
    noy_n15_deposition(:)    = 0.0_wp
    p_deposition(:)          = 0.0_wp
    ! For lakes:
    albedo_lwtr(:)     = 0.0_wp
    drag_wtr(:)        = 0.0_wp
    drag_ice(:)        = 0.0_wp
    t_acoef_wtr(:)     = 0.0_wp
    t_bcoef_wtr(:)     = 0.0_wp
    q_acoef_wtr(:)     = 0.0_wp
    q_bcoef_wtr(:)     = 0.0_wp
    t_acoef_ice(:)     = 0.0_wp
    t_bcoef_ice(:)     = 0.0_wp
    q_acoef_ice(:)     = 0.0_wp
    q_bcoef_ice(:)     = 0.0_wp


    !! #############################################################################################
    !! LAND PLANT CANOPY
    !! #############################################################################################
    SELECT CASE(model%config%qmodel_id)
    CASE(QLAND, QPLANT, QCANOPY)

      !> 1.0 orbital parameters
      !>
      ! "declination" is a parameter from mo_atmland_constants and is calculated here as output paramater, though, not used here in this module
      CALL update_orbital_parameters(time, declination, sw_toa_down(:))  ! mo_atmland_util

      !> 2.0 read forcing data
      !>
      ! read line by line from file until file end and start again at beginning of file \n
      ! if model%config%flag_climate_forcing_one_year = TRUE then 
      !        read only the data of the 1st year of climate forcing and start again at beginning of file
      lun=20

      ! day of the year: previous time step
      doy_old = day

      ! moved up from below because it is required to get timestep from daily forcing if demanded
      cos_zenith_angle(:)     = calc_solar_angle(hgrid%lat(1,1), hgrid%lon(1,1), time)      ! @TODO  make working globally

      ! see above the comment "make working globally" ...
      ! ... following that comment here also hgrid%lon(1,1) and hgrid%lat(1,1) is used.
      ! ... However, reading of forcing does not work globally anyway currently!
      ! ... in case this is changed and the code above gets adapted for cos_zenith_angle it might also be adapted here:
      lon = hgrid%lon(1,1)
      lat = hgrid%lat(1,1)

      CALL read_or_calculate_forcing(lun, lon, lat, cos_zenith_angle(1),                            &
                        time, timestep_prop_day, stat, year, day, hour,                             &
                        sw_srf_down_r,      lw_srf_down_r, t_air_r,    q_air_r,                     &
                        press_srf_r,        rain_r,        snow_r,     wind_air_r,                  &
                        co2_mixing_ratio_r, co2_dC13_r,    co2_DC14_r,                              &
                        nhx_deposition_r,   noy_deposition_r, p_deposition_r)

      ! day of the year: current time step
      doy_new = day 

      ! Check if the end of the file is reached (IS_IOSTAT_END(stat) == Test for end-of-file value)
      IF(IS_IOSTAT_END(stat)) THEN 
        ! Assert: the last year in the forcing file should equal the forcing file last year as specified in the namelist
        IF(forcing_options%forcing_year /= model%config%forcing_file_last_yr) THEN
          WRITE(message_text,*) 'Last year read from the forcing file ('//TRIM(int2string(forcing_options%forcing_year)) &
            &                   // ') not equal to forcing last year as specified in the namelist ('                     &
            &                   // TRIM(int2string(model%config%forcing_file_last_yr))//'). Please check!'
          CALL finish(TRIM(modname),message_text)
        ENDIF
      ENDIF

      ! if this flag is set TRUE, check whether the current time step is a new year, if so 
      ! set stat = -1 to indicate "end of file" which invokes the re-reading of the climate forcing input file at line 1
      IF(model%config%flag_climate_forcing_one_year) THEN
        IF( ABS(doy_old - doy_new - 364.0_wp) < eps8 ) THEN
          stat = -1
        END IF
      END IF

      ! if transient forcing_mode is enabled, the climate forcing file may be re-read at line 1 when:
      !  a) the spinup forcing period ends and restarts at start-year, and 
      !  b) the spinup has finished
      IF(model%config%l_transient_spinup_rewind_forcing) THEN
        stat = -1
      END IF

      ! if end of file (IS_IOSTAT_END(stat) == Test for end-of-file value)
      IF(IS_IOSTAT_END(stat)) THEN 

        ! start reading from line one & read header into dummy
        REWIND(lun)
        CALL read_forcing_header(lun, model%config%qmodel_id)
        CALL read_or_calculate_forcing(lun, lon, lat, cos_zenith_angle(1),                            &
                          time, timestep_prop_day, stat, year, day, hour,                             &
                          sw_srf_down_r,      lw_srf_down_r, t_air_r,    q_air_r,                     &
                          press_srf_r,        rain_r,        snow_r,     wind_air_r,                  &
                          co2_mixing_ratio_r, co2_dC13_r,    co2_DC14_r,                              &
                          nhx_deposition_r,   noy_deposition_r, p_deposition_r)
      END IF

      !> 2.1 increase the counter counting the 1st year of the simulation (used for constant N/P/CO2 schemes)
      !>  and set 'l_timestep_first_year_passed' TRUE only at the timestep after the 1st simulation year has passed
      !>
      l_timestep_first_year_passed(:) = .FALSE.
      DO ic = 1, nc
        ! only if the counter variable has a positive value
        ! it is set to '-1' at the timestep after the 1st year of the simulation has passed
        IF (const_input_timestep_counter(ic) > -eps4) THEN
          const_input_timestep_counter(ic) = const_input_timestep_counter(ic) + 1.0_wp
          ! at the timestep after the 1st simulation year has passed
          IF (const_input_timestep_counter(ic) > (one_year * one_day / dtime)) THEN
            const_input_timestep_counter(ic) = -1._wp
            l_timestep_first_year_passed(ic) = .TRUE.
          END IF
        END IF
      END DO

      !> 2.2 accumulate forcing variables for which a "constant" scheme exists
      !>  and calc the "constant" value at the timestep after the 1st year of the simulation has passed
      DO ic = 1, nc
        ! only if the counter variable has a positive value
        ! it is set to '-1' at the timestep after the 1st year of the simulation has passed
        IF (const_input_timestep_counter(ic) > -eps4) THEN
          co2_mixing_ratio_acc(ic)    = co2_mixing_ratio_acc(ic) + co2_mixing_ratio_r
          co2_dC13_acc(ic)            = co2_dC13_acc(ic)         + co2_dC13_r
          co2_DC14_acc(ic)            = co2_DC14_acc(ic)         + co2_DC14_r
          nhx_deposition_acc(ic)      = nhx_deposition_acc(ic)   + nhx_deposition_r
          nhy_deposition_acc(ic)      = nhy_deposition_acc(ic)   + noy_deposition_r
          p_deposition_acc(ic)        = p_deposition_acc(ic)     + p_deposition_r
        END IF
        ! at the timestep after the 1st simulation year has passed
        IF (l_timestep_first_year_passed(ic)) THEN
          co2_mixing_ratio_const(ic)  = co2_mixing_ratio_acc(ic) / (one_year * one_day / dtime)
          co2_dC13_const(ic)          = co2_dC13_acc(ic)         / (one_year * one_day / dtime)
          co2_DC14_const(ic)          = co2_DC14_acc(ic)         / (one_year * one_day / dtime)
          nhx_deposition_const(ic)    = nhx_deposition_acc(ic)   / (one_year * one_day / dtime)
          nhy_deposition_const(ic)    = nhy_deposition_acc(ic)   / (one_year * one_day / dtime)
          p_deposition_const(ic)      = p_deposition_acc(ic)     / (one_year * one_day / dtime)
        END IF
      END DO

      !> 3.0 update forcing variables from the above 'read data'
      !>
      t_air(:)                = t_air_r
      q_air(:)                = q_air_r
      rain(:)                 = rain_r
      snow(:)                 = snow_r
      wind_air(:)             = wind_air_r
      wind_10m(:)             = wind_air_r
      press_srf(:)            = press_srf_r
      lw_srf_down(:)          = lw_srf_down_r
      swnir_srf_down(:)       = (1._wp - frac_vis_swtot_srf ) * sw_srf_down_r 
      swvis_srf_down(:)       = frac_vis_swtot_srf * sw_srf_down_r 
      swpar_srf_down(:)       = frac_par_swvis_srf * swvis_srf_down(:)
      fract_par_diffuse(:)    = calc_fract_par_diffuse(swnir_srf_down(:) + swvis_srf_down(:), sw_toa_down(:), cos_zenith_angle(:))
      ! elemental function: calc_fract_par_diffuse()
      
      !> 3.1 update CO2 & dC13 & DC14 forcing data
      !>
      DO ic = 1, nc
        ! CO2
        ! if constant CO2 forcing is enabled and the first simulation year has passed
        IF (forcing_options%flag_forcing_co2_const .AND. const_input_timestep_counter(ic) < -eps4) THEN
          co2_mixing_ratio(ic)   = co2_mixing_ratio_const(ic)
        ELSE
          co2_mixing_ratio(ic)   = co2_mixing_ratio_r
        END IF
        ! dC13
        ! if dC13 forcing input is used
        IF (forcing_options%flag_read_dC13) THEN
          ! if constant CO2 forcing is enabled and the first simulation year has passed
          IF (forcing_options%flag_forcing_co2_const .AND. const_input_timestep_counter(ic) < -eps4) THEN
            hlp1(ic)             = calc_mixing_ratio_C13C12(co2_dC13_const(ic))
          ELSE
            hlp1(ic)             = calc_mixing_ratio_C13C12(co2_dC13_r)
          END IF
        ELSE
          hlp1(ic)               = calc_mixing_ratio_C13C12(def_co2_deltaC13)
        END IF
        co2_mixing_ratio_C13(ic) = co2_mixing_ratio(ic) / (1._wp + 1._wp / hlp1(ic))
        ! DC14
        ! if DC14 forcing input is used
        IF (forcing_options%flag_read_DC14) THEN
          ! if constant CO2 forcing is enabled and the first simulation year has passed
          IF (forcing_options%flag_forcing_co2_const .AND. const_input_timestep_counter(ic) < -eps4) THEN
            hlp1(ic)             = calc_mixing_ratio_C14C(co2_dC13_const(ic), co2_DC14_const(ic))
          ELSE
            hlp1(ic)             = calc_mixing_ratio_C14C(co2_dC13_r, co2_DC14_r)
          END IF
        ELSE
          hlp1(ic)               = calc_mixing_ratio_C14C(def_co2_deltaC13, def_co2_deltaC14)
        END IF
        co2_mixing_ratio_C14(ic) = hlp1(ic) * co2_mixing_ratio(ic)
      END DO

      !> 3.2 update N deposition forcing data
      !>
      SELECT CASE (TRIM(forcing_options%n_deposition_scheme))
      CASE ("none")
        nhx_deposition(:)   = 0.0_wp
        noy_deposition(:)   = 0.0_wp
      CASE ("constant")
        DO ic = 1, nc
          ! if the first simulation year has passed
          IF (const_input_timestep_counter(ic) < -eps4) THEN
            nhx_deposition(ic)  = nhx_deposition_const(ic) / molar_mass_N * 1000.0_wp / one_day    ! mg/m2/day -> mumol/m2/sec
            noy_deposition(ic)  = nhy_deposition_const(ic) / molar_mass_N * 1000.0_wp / one_day    ! mg/m2/day -> mumol/m2/sec
          ELSE
            nhx_deposition(ic)  = nhx_deposition_r         / molar_mass_N * 1000.0_wp / one_day    ! mg/m2/day -> mumol/m2/sec
            noy_deposition(ic)  = noy_deposition_r         / molar_mass_N * 1000.0_wp / one_day    ! mg/m2/day -> mumol/m2/sec
          END IF
        END DO
      CASE ("dynamic")
        nhx_deposition(:)   = nhx_deposition_r / molar_mass_N * 1000.0_wp / one_day    ! mg/m2/day -> mumol/m2/sec
        noy_deposition(:)   = noy_deposition_r / molar_mass_N * 1000.0_wp / one_day    ! mg/m2/day -> mumol/m2/sec
      ENDSELECT
      nhx_n15_deposition(:) = nhx_deposition(:) / ( 1._wp + 1._wp / calc_mixing_ratio_N15N14(0.0_wp)) 
      noy_n15_deposition(:) = noy_deposition(:) / ( 1._wp + 1._wp / calc_mixing_ratio_N15N14(0.0_wp)) 

      !> 3.3 update P deposition forcing data
      !>
      SELECT CASE (TRIM(forcing_options%p_deposition_scheme))
      CASE ("none")
        p_deposition(:) = 0.0_wp
      CASE ("constant")
        DO ic = 1, nc
          ! if the first simulation year has passed
          IF (const_input_timestep_counter(ic) < -eps4) THEN
            p_deposition(ic)  = p_deposition_const(ic) / molar_mass_P * 1000.0_wp / one_day    ! mg/m2/day -> mumol/m2/sec
          ELSE
            p_deposition(ic)  = p_deposition_r         / molar_mass_P * 1000.0_wp / one_day    ! mg/m2/day -> mumol/m2/sec
          END IF
        END DO
      CASE ("dynamic")
        p_deposition(:) = p_deposition_r / molar_mass_P * 1000.0_wp / one_day      ! mg/m2/day -> mumol/m2/sec
      ENDSELECT

      !> 4.0 feedback about using constant forcing values
      !>
      DO ic = 1, nc
        IF (l_timestep_first_year_passed(ic)) THEN
          print*, "  "
          print*, " the 1st simulation year has passed (ic:", ic, ")"
          !@TODO: This message is displayed, even if there are no constant forcing values at all!
          print*, " constant forcing values are used now for: "
          IF (forcing_options%flag_forcing_co2_const) THEN
            print*, "  co2_mixing_ratio", co2_mixing_ratio(ic)
            IF (forcing_options%flag_read_dC13) THEN
              print*, "  co2_mixing_ratio_C13", co2_mixing_ratio_C13(ic)
            END IF
            IF (forcing_options%flag_read_DC14) THEN
              print*, "  co2_mixing_ratio_C14", co2_mixing_ratio_C14(ic)
            END IF
          END IF
          IF (TRIM(forcing_options%n_deposition_scheme) == "constant") THEN
            print*, "  nhx_deposition", nhx_deposition(ic)
            print*, "  noy_deposition", noy_deposition(ic)
          END IF
          IF (TRIM(forcing_options%p_deposition_scheme) == "constant") THEN
            print*, "  p_deposition", p_deposition(ic)
          END IF
          print*, "  "
        END IF
      END DO

      !! For lakes
      ! currently no values read from file or calculated, just zet to zero
      ! albedo_lwtr(:)     =         albedo_lwtr(:)    
      ! drag_wtr(:)        =         drag_wtr(:)       
      ! drag_ice(:)        =         drag_ice(:)       
      ! t_acoef_wtr(:)     =         t_acoef_wtr(:)    
      ! t_bcoef_wtr(:)     =         t_bcoef_wtr(:)    
      ! q_acoef_wtr(:)     =         q_acoef_wtr(:)    
      ! q_bcoef_wtr(:)     =         q_bcoef_wtr(:)    
      ! t_acoef_ice(:)     =         t_acoef_ice(:)    
      ! t_bcoef_ice(:)     =         t_bcoef_ice(:)    
      ! q_acoef_ice(:)     =         q_acoef_ice(:)    
      ! q_bcoef_ice(:)     =         q_bcoef_ice(:)    

    !! #############################################################################################
    !! SOIL
    !! #############################################################################################
    CASE(QSOIL)

      !>0.9 get bgcm matrices
      !>
      bgcm_store => tile%bgcm_store
      dsl4jsb_Get_mt2L2D(VEG_BGCM_POOL_ID, veg_pool_mt)
      dsl4jsb_Get_mt2L2D(VEG_BGCM_LITTERFALL_ID, veg_litterfall_mt)

      !>1.0 read data
      !>
      ! the read statement, by default a list of values in a line is read and then advanced to the beginning of the next line
      lun=20
      READ(lun,*, IOSTAT=stat) time_hlp, & 
                       leaf_carbon_r, leaf_nitrogen_r, leaf_phosphorus_r    , &
                       leaf_dC13_r, leaf_DC14_r, leaf_dN15_r  , &
                       fine_root_carbon_r, fine_root_nitrogen_r, fine_root_phosphorus_r   , &
                       fine_root_dC13_r, fine_root_DC14_r, fine_root_dN15_r , &
                       coarse_root_carbon_r, coarse_root_nitrogen_r, coarse_root_phosphorus_r   , &
                       coarse_root_dC13_r, coarse_root_DC14_r, coarse_root_dN15_r , &
                       sap_wood_carbon_r, sap_wood_nitrogen_r, sap_wood_phosphorus_r    , &
                       sap_wood_dC13_r, sap_wood_DC14_r, sap_wood_dN15_r  , &
                       heart_wood_carbon_r, heart_wood_nitrogen_r, heart_wood_phosphorus_r    , &
                       heart_wood_dC13_r, heart_wood_DC14_r, heart_wood_dN15_r  , &
                       labile_carbon_r, labile_nitrogen_r, labile_phosphorus_r    , &
                       labile_dC13_r, labile_DC14_r, labile_dN15_r  , &
                       reserve_carbon_r, reserve_nitrogen_r, reserve_phosphorus_r   , &
                       reserve_dC13_r, reserve_DC14_r, reserve_dN15_r , &
                       fruit_carbon_r, fruit_nitrogen_r, fruit_phosphorus_r   , &
                       fruit_dC13_r, fruit_DC14_r, fruit_dN15_r , &
                       seed_bed_carbon_r, seed_bed_nitrogen_r, seed_bed_phosphorus_r  , &
                       seed_bed_dC13_r, seed_bed_DC14_r, seed_bed_dN15_r

      ! if transient forcing_mode is enabled, the climate forcing file may be re-read at line 1 when:
      ! a) the spinup forcing period ends and restarts at start-year, and b) the spinup has finished
      IF(model%config%l_transient_spinup_rewind_forcing) THEN
        stat = -1
      END IF

      ! set stat = -1 to indicate "end of file" which invokes the re-reading of the climate forcing input file at line 1
      IF(IS_IOSTAT_END(stat)) THEN 
        ! start reading from line one & read header into dummy
        REWIND(lun)
        CALL read_forcing_header(lun, model%config%qmodel_id)
        READ(lun,*) time_hlp, &
                       leaf_carbon_r, leaf_nitrogen_r, leaf_phosphorus_r    , &
                       leaf_dC13_r, leaf_DC14_r, leaf_dN15_r  , &
                       fine_root_carbon_r, fine_root_nitrogen_r, fine_root_phosphorus_r   , &
                       fine_root_dC13_r, fine_root_DC14_r, fine_root_dN15_r , &
                       coarse_root_carbon_r, coarse_root_nitrogen_r, coarse_root_phosphorus_r   , &
                       coarse_root_dC13_r, coarse_root_DC14_r, coarse_root_dN15_r , &
                       sap_wood_carbon_r, sap_wood_nitrogen_r, sap_wood_phosphorus_r    , &
                       sap_wood_dC13_r, sap_wood_DC14_r, sap_wood_dN15_r  , &
                       heart_wood_carbon_r, heart_wood_nitrogen_r, heart_wood_phosphorus_r    , &
                       heart_wood_dC13_r, heart_wood_DC14_r, heart_wood_dN15_r  , &
                       labile_carbon_r, labile_nitrogen_r, labile_phosphorus_r    , &
                       labile_dC13_r, labile_DC14_r, labile_dN15_r  , &
                       reserve_carbon_r, reserve_nitrogen_r, reserve_phosphorus_r   , &
                       reserve_dC13_r, reserve_DC14_r, reserve_dN15_r , &
                       fruit_carbon_r, fruit_nitrogen_r, fruit_phosphorus_r   , &
                       fruit_dC13_r, fruit_DC14_r, fruit_dN15_r , &
                       seed_bed_carbon_r, seed_bed_nitrogen_r, seed_bed_phosphorus_r  , &
                       seed_bed_dC13_r, seed_bed_DC14_r, seed_bed_dN15_r
      END IF

      lun=21
      READ(lun,*, IOSTAT=stat) time_hlp, & 
                       t_soil_sl_r(1:nsoil_sb), w_soil_sl_r(1:nsoil_sb), &
                       percolation_sl_r(1:nsoil_sb), root_fraction_sl_r(1:nsoil_sb), &
                       veg_pool_fine_root_carbon_r, &
                       nhx_deposition_r, noy_deposition_r, &
                       nhx_dn15_deposition_r, noy_dn15_deposition_r, &
                       p_deposition_r, &
                       f_n_demand_r, f_p_demand_r

      ! if transient forcing_mode is enabled, the climate forcing file may be re-read at line 1 when:
      ! a) the spinup forcing period ends and restarts at start-year, and b) the spinup has finished
      IF(model%config%l_transient_spinup_rewind_forcing) THEN
        stat = -1
      END IF

      IF(IS_IOSTAT_END(stat)) THEN 
        ! start reading from line one & read header into dummy
        REWIND(lun)
        CALL read_forcing_header(lun, model%config%qmodel_id)
        READ(lun,*) time_hlp, &
                       t_soil_sl_r(1:nsoil_sb), w_soil_sl_r(1:nsoil_sb), &
                       percolation_sl_r(1:nsoil_sb), root_fraction_sl_r(1:nsoil_sb), &
                       veg_pool_fine_root_carbon_r, &
                       nhx_deposition_r, noy_deposition_r, &
                       nhx_dn15_deposition_r, noy_dn15_deposition_r, &
                       p_deposition_r, &
                       f_n_demand_r, f_p_demand_r
      END IF

      !>2.0 pass values read from file (i.e. scalar values) to ics-ice
      !>
      veg_litterfall_mt(ix_leaf, ixC, :)     = leaf_carbon_r / 1000._wp
      veg_litterfall_mt(ix_leaf, ixN, :)   = leaf_nitrogen_r / 1000000._wp
      veg_litterfall_mt(ix_leaf, ixP, :) = leaf_phosphorus_r / 1000000._wp
      veg_litterfall_mt(ix_leaf, ixC13, :)   = veg_litterfall_mt(ix_leaf, ixC, :) / ( 1._wp + 1._wp / &
                                            calc_mixing_ratio_C13C12(leaf_dC13_r))
      veg_litterfall_mt(ix_leaf, ixC14, :)   = veg_litterfall_mt(ix_leaf, ixC, :) * &
                                            calc_mixing_ratio_C14C(leaf_dC13_r,leaf_DC14_r)
      veg_litterfall_mt(ix_leaf, ixN15, :) = veg_litterfall_mt(ix_leaf, ixN, :) / ( 1._wp + 1._wp / &
                                            calc_mixing_ratio_N15N14(leaf_dN15_r))

      veg_litterfall_mt(ix_fine_root, ixC, :)     = fine_root_carbon_r / 1000._wp
      veg_litterfall_mt(ix_fine_root, ixN, :)   = fine_root_nitrogen_r / 1000000._wp
      veg_litterfall_mt(ix_fine_root, ixP, :) = fine_root_phosphorus_r / 1000000._wp
      veg_litterfall_mt(ix_fine_root, ixC13, :)   = veg_litterfall_mt(ix_fine_root, ixC, :) / ( 1._wp + 1._wp / &
                                                 calc_mixing_ratio_C13C12(fine_root_dC13_r))
      veg_litterfall_mt(ix_fine_root, ixC14, :)   = veg_litterfall_mt(ix_fine_root, ixC, :) * &
                                                 calc_mixing_ratio_C14C(fine_root_dC13_r,fine_root_DC14_r)
      veg_litterfall_mt(ix_fine_root, ixN15, :) = veg_litterfall_mt(ix_fine_root, ixN, :) / ( 1._wp + 1._wp / &
                                                 calc_mixing_ratio_N15N14(fine_root_dN15_r))

      veg_litterfall_mt(ix_coarse_root, ixC, :)     = coarse_root_carbon_r / 1000._wp
      veg_litterfall_mt(ix_coarse_root, ixN, :)   = coarse_root_nitrogen_r / 1000000._wp
      veg_litterfall_mt(ix_coarse_root, ixP, :) = coarse_root_phosphorus_r / 1000000._wp
      veg_litterfall_mt(ix_coarse_root, ixC13, :)   = veg_litterfall_mt(ix_coarse_root, ixC, :) / ( 1._wp + 1._wp / &
                                                   calc_mixing_ratio_C13C12(coarse_root_dC13_r))
      veg_litterfall_mt(ix_coarse_root, ixC14, :)   = veg_litterfall_mt(ix_coarse_root, ixC, :) * &
                                                   calc_mixing_ratio_C14C(coarse_root_dC13_r,coarse_root_DC14_r)
      veg_litterfall_mt(ix_coarse_root, ixN15, :) = veg_litterfall_mt(ix_coarse_root, ixN, :) / ( 1._wp + 1._wp / &
                                                   calc_mixing_ratio_N15N14(coarse_root_dN15_r))

      veg_litterfall_mt(ix_sap_wood, ixC, :)        = sap_wood_carbon_r / 1000._wp
      veg_litterfall_mt(ix_sap_wood, ixN, :)      = sap_wood_nitrogen_r / 1000000._wp
      veg_litterfall_mt(ix_sap_wood, ixP, :)    = sap_wood_phosphorus_r / 1000000._wp
      veg_litterfall_mt(ix_sap_wood, ixC13, :)      = veg_litterfall_mt(ix_sap_wood, ixC, :) / ( 1._wp + 1._wp / &
                                                   calc_mixing_ratio_C13C12(sap_wood_dC13_r))
      veg_litterfall_mt(ix_sap_wood, ixC14, :)      = veg_litterfall_mt(ix_sap_wood, ixC, :) * &
                                                   calc_mixing_ratio_C14C(sap_wood_dC13_r,sap_wood_DC14_r)
      veg_litterfall_mt(ix_sap_wood, ixN15, :)    = veg_litterfall_mt(ix_sap_wood, ixN, :) / ( 1._wp + 1._wp / &
                                                   calc_mixing_ratio_N15N14(sap_wood_dN15_r))

      veg_litterfall_mt(ix_heart_wood, ixC, :)      = heart_wood_carbon_r / 1000._wp
      veg_litterfall_mt(ix_heart_wood, ixN, :)    = heart_wood_nitrogen_r / 1000000._wp
      veg_litterfall_mt(ix_heart_wood, ixP, :)  = heart_wood_phosphorus_r / 1000000._wp
      veg_litterfall_mt(ix_heart_wood, ixC13, :)    = veg_litterfall_mt(ix_heart_wood, ixC, :) / ( 1._wp + 1._wp / &
                                                   calc_mixing_ratio_C13C12(heart_wood_dC13_r))
      veg_litterfall_mt(ix_heart_wood, ixC14, :)    = veg_litterfall_mt(ix_heart_wood, ixC, :) * &
                                                   calc_mixing_ratio_C14C(heart_wood_dC13_r,heart_wood_DC14_r)
      veg_litterfall_mt(ix_heart_wood, ixN15, :)  = veg_litterfall_mt(ix_heart_wood, ixN, :) / ( 1._wp + 1._wp / &
                                                   calc_mixing_ratio_N15N14(heart_wood_dN15_r))

      veg_litterfall_mt(ix_labile, ixC, :)       = labile_carbon_r / 1000._wp
      veg_litterfall_mt(ix_labile, ixN, :)     = labile_nitrogen_r / 1000000._wp
      veg_litterfall_mt(ix_labile, ixP, :)   = labile_phosphorus_r / 1000000._wp
      veg_litterfall_mt(ix_labile, ixC13, :)     = veg_litterfall_mt(ix_labile, ixC, :) / ( 1._wp + 1._wp / &
                                                calc_mixing_ratio_C13C12(labile_dC13_r))
      veg_litterfall_mt(ix_labile, ixC14, :)     = veg_litterfall_mt(ix_labile, ixC, :) * &
                                                calc_mixing_ratio_C14C(labile_dC13_r,labile_DC14_r)
      veg_litterfall_mt(ix_labile, ixN15, :)   = veg_litterfall_mt(ix_labile, ixN, :) / ( 1._wp + 1._wp / &
                                                calc_mixing_ratio_N15N14(labile_dN15_r))

      veg_litterfall_mt(ix_reserve, ixC, :)      = reserve_carbon_r / 1000._wp
      veg_litterfall_mt(ix_reserve, ixN, :)    = reserve_nitrogen_r / 1000000._wp
      veg_litterfall_mt(ix_reserve, ixP, :)  = reserve_phosphorus_r / 1000000._wp
      veg_litterfall_mt(ix_reserve, ixC13, :)    = veg_litterfall_mt(ix_reserve, ixC, :) / ( 1._wp + 1._wp / &
                                                calc_mixing_ratio_C13C12(reserve_dC13_r))
      veg_litterfall_mt(ix_reserve, ixC14, :)    = veg_litterfall_mt(ix_reserve, ixC, :) * &
                                                calc_mixing_ratio_C14C(reserve_dC13_r,reserve_DC14_r)
      veg_litterfall_mt(ix_reserve, ixN15, :)  = veg_litterfall_mt(ix_reserve, ixN, :) / ( 1._wp + 1._wp / &
                                                calc_mixing_ratio_N15N14(reserve_dN15_r))

      veg_litterfall_mt(ix_fruit, ixC, :)        = fruit_carbon_r / 1000._wp
      veg_litterfall_mt(ix_fruit, ixN, :)      = fruit_nitrogen_r / 1000000._wp
      veg_litterfall_mt(ix_fruit, ixP, :)    = fruit_phosphorus_r / 1000000._wp
      veg_litterfall_mt(ix_fruit, ixC13, :)      = veg_litterfall_mt(ix_fruit, ixC, :) / ( 1._wp + 1._wp / &
                                                calc_mixing_ratio_C13C12(fruit_dC13_r))
      veg_litterfall_mt(ix_fruit, ixC14, :)      = veg_litterfall_mt(ix_fruit, ixC, :) * &
                                                calc_mixing_ratio_C14C(fruit_dC13_r,fruit_DC14_r)
      veg_litterfall_mt(ix_fruit, ixN15, :)    = veg_litterfall_mt(ix_fruit, ixN, :) / ( 1._wp + 1._wp / &
                                                calc_mixing_ratio_N15N14(fruit_dN15_r))

      veg_litterfall_mt(ix_seed_bed, ixC, :)     = seed_bed_carbon_r / 1000._wp
      veg_litterfall_mt(ix_seed_bed, ixN, :)   = seed_bed_nitrogen_r / 1000000._wp
      veg_litterfall_mt(ix_seed_bed, ixP, :) = seed_bed_phosphorus_r / 1000000._wp
      veg_litterfall_mt(ix_seed_bed, ixC13, :)   = veg_litterfall_mt(ix_seed_bed, ixC, :) / ( 1._wp + 1._wp / &
                                                calc_mixing_ratio_C13C12(seed_bed_dC13_r))
      veg_litterfall_mt(ix_seed_bed, ixC14, :)   = veg_litterfall_mt(ix_seed_bed, ixC, :) * &
                                                calc_mixing_ratio_C14C(seed_bed_dC13_r,seed_bed_DC14_r)
      veg_litterfall_mt(ix_seed_bed, ixN15, :) = veg_litterfall_mt(ix_seed_bed, ixN, :) / ( 1._wp + 1._wp / &
                                                calc_mixing_ratio_N15N14(seed_bed_dN15_r))

      DO isoil = 1,nsoil_sb
        t_soil_sl(:,isoil)                  = t_soil_sl_r(isoil)
        w_soil_sl(:,isoil)                  = w_soil_sl_r(isoil)
        percolation_sl(:,isoil)             = percolation_sl_r(isoil)
        root_fraction_sl(:,isoil)           = root_fraction_sl_r(isoil)
      ENDDO
      veg_pool_mt(ix_fine_root, ixC, :)     = veg_pool_fine_root_carbon_r

      nhx_deposition(:)     = nhx_deposition_r
      noy_deposition(:)     = noy_deposition_r
      nhx_n15_deposition(:) = nhx_deposition(:) / ( 1._wp + 1._wp / calc_mixing_ratio_N15N14(nhx_dn15_deposition_r))
      nhx_n15_deposition(:) = noy_deposition(:) / ( 1._wp + 1._wp / calc_mixing_ratio_N15N14(noy_dn15_deposition_r))
      p_deposition(:)       = p_deposition_r
      f_n_demand(:)         = f_n_demand_r
      f_p_demand(:)         = f_p_demand_r

    END SELECT  ! land plant canopy vs soil

    ! Deallocate local allocatable variables
    DEALLOCATE(t_soil_sl_r)
    DEALLOCATE(w_soil_sl_r)
    DEALLOCATE(percolation_sl_r)
    DEALLOCATE(root_fraction_sl_r)

  END SUBROUTINE update_forcing


  !-----------------------------------------------------------------------------------------------------
  !> open the file test_bed/plant/climate.dat, only IF model option is set to READ
  !! 
  !! called in the driver
  !! 
  !! note: lun is set to 20 for land plant canopy input; and 20/21 for soil input
  !-----------------------------------------------------------------------------------------------------
  SUBROUTINE open_forcing_input_file(qmodel_id)

    USE mo_quincy_model_config,   ONLY: QLAND, QPLANT, QSOIL, QCANOPY, Q_TEST_CANOPY, Q_TEST_RADIATION

    IMPLICIT NONE
    ! ---------------------------
    ! 0.1 InOut
    INTEGER, INTENT(IN) :: qmodel_id
    ! ---------------------------
    ! 0.2 Local
    LOGICAL         :: file_exists
    INTEGER         :: lun 
    CHARACTER(100)  :: fname
    CHARACTER(len=*), PARAMETER :: routine = TRIM(modname)//':open_forcing_input_file'

    SELECT CASE(qmodel_id)
      CASE(QLAND, QPLANT, QCANOPY)
        !> 1.1 open file for READ
        !! 
        lun   = 20
        fname = 'climate.dat'
        ! check if file exists
        INQUIRE(FILE=fname, EXIST=file_exists)
        ! open if fname == True
        IF(file_exists) THEN
          OPEN (UNIT=lun, FILE=fname, ACTION="read", STATUS="old") ! status=old: file exists already
        !> 1.2 read the two header lines into dummy variables
        !! to advance to line 3
          CALL read_forcing_header(lun, qmodel_id)
        ELSE
          CALL finish(TRIM(routine), 'climate forcing: climate.dat could not be opened, file does not exist')
        END IF

      CASE(QSOIL)
        !> 2.1 open file for READ
        !! 
        lun   = 20
        fname = 'soil_forcing_litterfall.dat'
        ! check if file exists
        INQUIRE(FILE=fname, EXIST=file_exists)
        ! open if fname == True
        IF(file_exists) THEN
          OPEN (UNIT=lun, FILE=fname, ACTION="read", STATUS="old") ! status=old: file exists already
        !> 2.2 read the header line into dummy variable
        !! to advance to line 2
          CALL read_forcing_header(lun, qmodel_id)
        ELSE
          CALL finish(TRIM(routine), 'soil forcing: soil_forcing_litterfall.dat could not be opened, file does not exist')
        END IF

        !> 3.1 open file for READ
        !! 
        lun   = 21
        fname = 'soil_forcing_soilphysics.dat'
        ! check if file exists
        INQUIRE(FILE=fname, EXIST=file_exists)
        ! open if fname == True
        IF(file_exists) THEN
          OPEN (UNIT=lun, FILE=fname, ACTION="read", STATUS="old") ! status=old: file exists already
        !> 3.2 read the header line into dummy variable
        !! to advance to line 2
          CALL read_forcing_header(lun, qmodel_id)
        ELSE
          CALL finish(TRIM(routine), 'soil forcing: soil_forcing_soilphysics.dat could not be opened, file does not exist')
        END IF

      CASE DEFAULT
        CALL finish(TRIM(routine), 'climate forcing: either land, plant, canopy or soil should lead you here, but others not')
    END SELECT

  END SUBROUTINE open_forcing_input_file


  !-----------------------------------------------------------------------------------------------------
  !> closes the file test_bed/plant/climate.dat, only IF model option is set to READ
  !! 
  !! called in the driver
  !-----------------------------------------------------------------------------------------------------
  SUBROUTINE close_forcing_input_file(qmodel_id)

    USE mo_quincy_model_config,   ONLY: QLAND, QPLANT, QSOIL, QCANOPY, Q_TEST_CANOPY, Q_TEST_RADIATION

    IMPLICIT NONE
    ! ---------------------------
    ! 0.1 InOut
    INTEGER, INTENT(IN) :: qmodel_id
    ! ---------------------------
    ! 0.2 Local
    CHARACTER(len=*), PARAMETER :: routine = TRIM(modname)//':close_forcing_input_file'

    SELECT CASE(qmodel_id)
      CASE(QLAND, QPLANT, QCANOPY)
        CLOSE(20)    ! climate

      CASE(QSOIL)
        CLOSE(20)   ! litterfall
        CLOSE(21)   ! soilphysics
    END SELECT

  END SUBROUTINE close_forcing_input_file


  !-----------------------------------------------------------------------------------------------------
  !> read only the header into a dummy variable to advanve the READ to the 3rd line in the file
  !! 
  !! consider this a helper subroutine to the read_forcing subroutine
  !-----------------------------------------------------------------------------------------------------
  SUBROUTINE read_forcing_header(lun, qmodel_id)

    USE mo_quincy_model_config,   ONLY: QLAND, QPLANT, QSOIL, QCANOPY, Q_TEST_CANOPY, Q_TEST_RADIATION

    IMPLICIT NONE
    ! ---------------------------
    ! 0.1 InOut
    INTEGER             ,INTENT(in)     :: lun
    INTEGER             ,INTENT(in)     :: qmodel_id
    ! ---------------------------
    ! 0.2 Local
    CHARACTER(200)      :: header_dummy1 = "header"
    CHARACTER(500)      :: header_dummy2 = "header_long"
    CHARACTER(len=*), PARAMETER :: routine = TRIM(modname)//':read_forcing_header'

    SELECT CASE(qmodel_id)
      CASE(QLAND, QPLANT, QCANOPY)
      !> 1.0 read 2 header lines of one file
      !! lun = 9
        READ(lun,'(a)') header_dummy1   ! header line 1
        READ(lun,'(a)') header_dummy1   ! header line 2

      CASE(QSOIL)
      !> 2.0 read 1 header line of two files
      !! lun = 9 or 10
        READ(lun,'(a)') header_dummy2   ! header line 1
    END SELECT

  END SUBROUTINE read_forcing_header

  ! ====================================================================================================== !
  !
  !> read or calculate forcing depending on forcing options and time
  !> - in case of timestep forcing the forcing is always read
  !> - in case of daily forcing the forcing is either read (upon a new day) or determined from the daily value
  !> - this routine is called a 2nd time in case 'IS_IOSTAT_END(stat) == TRUE', i.e., 
  !>   the attempt to read beyond the last line of the forcing file
  !
  ! Note: the .dat forcing files unfortunately usually have "swvis_srf_down" instead of "sw_srf_down"
  !       in the according column header, although they do carry "sw_srf_down"
  SUBROUTINE read_or_calculate_forcing( lun, &
                                        lon, &
                                        lat, &
                                        cos_zenith, &
                                        time, &
                                        timestep_prop_day, &
                                        stat, year, day, hour, &
                                        sw_srf_down_r, lw_srf_down_r, &
                                        t_air_r, q_air_r, &
                                        press_srf_r, &
                                        rain_r, snow_r, &
                                        wind_air_r, &
                                        co2_mixing_ratio_r, &
                                        co2_dC13_r, &
                                        co2_DC14_r, &
                                        nhx_deposition_r, noy_deposition_r, p_deposition_r)

    USE mo_atmland_util,          ONLY: calc_spec_humidity_sat

    ! -------------------------------------------------------------------------------------------------- !
    INTEGER, INTENT(IN)    :: lun                 !< logical unit number for file input stream
    REAL(wp), INTENT(IN)   :: lon                 !< lon of this site 
    REAL(wp), INTENT(IN)   :: lat                 !< lat of this site
    REAL(wp), INTENT(IN)   :: cos_zenith          !< cos_zenith of this site
    REAL(wp), INTENT(IN)   :: time                !< real time in days (called 'time_day' in calling routine!)
    REAL(wp), INTENT(IN)   :: timestep_prop_day   !< proportion of a day that is simulated with one timestep
    INTEGER,  INTENT(OUT)  :: stat                !< status of I/O calls of this routine
    REAL(wp), INTENT(OUT)  :: year, day, hour, &
                              sw_srf_down_r, lw_srf_down_r, &
                              t_air_r, q_air_r, &
                              press_srf_r, &
                              rain_r, snow_r, &
                              wind_air_r, &
                              co2_mixing_ratio_r, &
                              co2_dC13_r, &
                              co2_DC14_r, &
                              nhx_deposition_r, noy_deposition_r, p_deposition_r
    ! -------------------------------------------------------------------------------------------------- !
    CHARACTER(len=*), PARAMETER :: routine = TRIM(modname)//':read_or_calculate_forcing'

    REAL(wp) :: cos_zenith_angle_for_sw_calculation !< in contrast to cos_zenith (IN), this one is in [0,1]
    REAL(wp) :: sat_spec_humidity                   !< potentially reduces q_air
    REAL(wp) :: fract_timestep                      !< the current time step expressed as the fraction of the current day
    REAL(wp) :: precip_r                            !< if precip is read instead of snow and rain or/and when reading daily
    LOGICAL  :: l_read_forcing                      !< if forcing is read (depends on if timestep or daily and/or new day)

    REAL(wp) :: interp_qair, interp_press, interp_precip, interp_wind
    ! -------------------------------------------------------------------------------------------------- !
                      
    l_read_forcing = .TRUE.
    IF (forcing_options%is_daily_forcing) THEN
      ! In case of daily forcing, forcing only needs to be read if its a new day
      l_read_forcing = .FALSE.
      ! i.e. if this is the first timestep of this day
      IF ( ABS(time - timestep_prop_day - REAL(FLOOR(time), wp)) < eps4) THEN
        l_read_forcing = .TRUE.
      ENDIF
    ENDIF

    IF(l_read_forcing) THEN
      ! the read statement, by default a list of values in a line, is read and then advanced to the beginning of the next line
      IF (forcing_options%is_daily_forcing) THEN
        ! Save the forcing read on the previous day
        forcing_options%yDay%sw             = forcing_options%current%sw
        forcing_options%yDay%lw             = forcing_options%current%lw
        forcing_options%yDay%tmin           = forcing_options%current%tmin
        forcing_options%yDay%tmax           = forcing_options%current%tmax
        forcing_options%yDay%qair           = forcing_options%current%qair
        forcing_options%yDay%rain           = forcing_options%current%rain
        forcing_options%yDay%snow           = forcing_options%current%snow
        forcing_options%yDay%precipitation  = forcing_options%current%precipitation
        forcing_options%yDay%wind           = forcing_options%current%wind
        forcing_options%yDay%pressure       = forcing_options%current%pressure

        ! And last timesteps tair (required for t_air interpolation between midnight and dawn)
        forcing_options%yDay%tair = forcing_options%last_timestep_tair

        ! Note: the .dat forcing files unfortunately usually have "swvis_srf_down" instead of "sw_srf_down"
        !       in the according column header, although they do carry "sw_srf_down"
        IF(forcing_options%read_precipitation) THEN
          ! read precipitation instead of rain and snow
          READ(lun,*, IOSTAT = stat) year, day, hour,                                                       &
            & forcing_options%current%sw, forcing_options%current%lw, forcing_options%current%tmin,         &
            & forcing_options%current%tmax, forcing_options%current%qair, forcing_options%current%pressure, &
            & forcing_options%current%precipitation, forcing_options%current%wind,                          &
            & forcing_options%co2_mixing_ratio, forcing_options%co2_dC13, forcing_options%co2_dC14,         &
            & forcing_options%nhx_srf_down, forcing_options%noy_srf_down, forcing_options%p_srf_down
        ELSE
          ! read rain and snow instead of precipitation
          READ(lun,*, IOSTAT = stat) year, day, hour,                                                       &
            & forcing_options%current%sw, forcing_options%current%lw, forcing_options%current%tmin,         &
            & forcing_options%current%tmax, forcing_options%current%qair, forcing_options%current%pressure, &
            & forcing_options%current%rain, forcing_options%current%snow, forcing_options%current%wind,     &
            & forcing_options%co2_mixing_ratio, forcing_options%co2_dC13, forcing_options%co2_dC14,         &
            & forcing_options%nhx_srf_down, forcing_options%noy_srf_down, forcing_options%p_srf_down

          forcing_options%current%precipitation = forcing_options%current%snow + forcing_options%current%rain
        ENDIF
      ELSE
        IF(forcing_options%read_precipitation) THEN
          CALL finish(TRIM(routine), 'reading precip instead of rain and snow is only implemented with daily')
        ENDIF

        ! When reading timestep - i.e. non-daily - forcing: tair is expected instead of tmin and tmax 
        READ(lun,*, IOSTAT = stat) year, day, hour,                                                       &
          & forcing_options%current%sw, forcing_options%current%lw, forcing_options%current%tair,         &
          & forcing_options%current%qair, forcing_options%current%pressure, forcing_options%current%rain, &
          & forcing_options%current%snow, forcing_options%current%wind,                                   &
          & forcing_options%co2_mixing_ratio, forcing_options%co2_dC13, forcing_options%co2_dC14,         &
          & forcing_options%nhx_srf_down, forcing_options%noy_srf_down, forcing_options%p_srf_down
      ENDIF

      ! save forcing timing data read from dat file
      ! only if reading from file was successful (e.g., stat would be -1 if reading beyond the last line of the forcing file)
      IF (stat == 0) THEN
        forcing_options%forcing_year  = NINT(year)
        forcing_options%forcing_doy   = NINT(day)
        forcing_options%forcing_hour  = hour
      END IF
    ENDIF

    ! load current forcing (either timestep or day) into the forcing variables
    wind_air_r          = forcing_options%current%wind
    co2_mixing_ratio_r  = forcing_options%co2_mixing_ratio
    co2_dC13_r          = forcing_options%co2_dC13
    co2_DC14_r          = forcing_options%co2_dC14
    nhx_deposition_r    = forcing_options%nhx_srf_down
    noy_deposition_r    = forcing_options%noy_srf_down
    p_deposition_r      = forcing_options%p_srf_down

    ! some forcing input units need re-scaling for calculation within QUINCY
    !@todo: why not manipulating the input to have the units expected in QUINCY?
    press_srf_r = forcing_options%current%pressure * 100._wp       ! hPa -> Pa ?
    q_air_r     = forcing_options%current%qair / 1000._wp          ! g/g -> g/kg ?
    IF(.NOT. forcing_options%read_precipitation) THEN
      rain_r    = forcing_options%current%rain / one_day           ! mm/day -> mm/s
      snow_r    = forcing_options%current%snow / one_day           ! mm/day -> mm/s
      precip_r  = rain_r + snow_r  
    ELSE
      precip_r  = forcing_options%current%precipitation / one_day  ! mm/day -> mm/s
    ENDIF

    ! derive timestep forcing from daily forcing if required 
    IF(forcing_options%is_daily_forcing) THEN
      ! calculate fraction of current day 
      ! (-eps4 to avoid rounding to next integer for last timestep -> if fract_timestep ~ 1.0 )
      fract_timestep = MIN(1.0_wp, time - REAL(FLOOR(time - eps4), wp))

      IF (forcing_options%yDay%precipitation .NE. -999.0_wp) THEN
        ! Interpolate daily values to avoid jumps at midnight
        ! -> solution from OCN -- here applied for an only partially overlapping set of forcing variables
        interp_qair   = (1.0_wp - fract_timestep) * (forcing_options%yDay%qair / 1000._wp) + fract_timestep * q_air_r
        interp_precip = (1.0_wp - fract_timestep) * (forcing_options%yDay%precipitation / one_day) + fract_timestep * precip_r
        interp_press  = (1.0_wp - fract_timestep) * (forcing_options%yDay%pressure * 100._wp) + fract_timestep * press_srf_r
        interp_wind   = (1.0_wp - fract_timestep) * forcing_options%yDay%wind + fract_timestep * wind_air_r
      ELSE
        ! initialisation -- first timestep...
        interp_qair   = q_air_r
        interp_precip = precip_r
        interp_press  = press_srf_r
        interp_wind   = wind_air_r
      ENDIF

      ! compute temperature
      CALL instantly_from_daily_temp_2(day, fract_timestep, lon, lat, forcing_options%current%tmin, &
        & forcing_options%current%tmax, forcing_options%yDay%tair, t_air_r)

      ! Store timestep tair required for interpolation inbetween days
      forcing_options%last_timestep_tair = t_air_r

      ! reduce qair if required
      sat_spec_humidity = calc_spec_humidity_sat(t_air_r, interp_press)
      q_air_r           = MIN(interp_qair, sat_spec_humidity)

      ! compute sw
      !@Note: in ICON-Land/JSB cos_zenith_angle is calculated from the host model (ICON/Echam) in the routine
      !       solar_parameters of the module mo_radiation_parameters: 
      !       there, cos_mu0 (cosine of the solar zenith angle) is [0,1] (thus set to zero where <= 0.0_wp)
      cos_zenith_angle_for_sw_calculation = cos_zenith
      IF(cos_zenith <= 0.0_wp) THEN
        cos_zenith_angle_for_sw_calculation = 0.0_wp
      ENDIF
      CALL instantly_from_daily_sw(day, lat, cos_zenith_angle_for_sw_calculation, forcing_options%current%sw, sw_srf_down_r)

      ! compute lw
      CALL longwave_from_daily_longwave(forcing_options%current%lw, forcing_options%current%tmin, &
        &                               forcing_options%current%tmax, t_air_r, lw_srf_down_r)

      ! compute snow and rain
      CALL snow_and_rain_from_precip(interp_precip, t_air_r, rain_r, snow_r)

      ! use interpolated pressure value and wind
      press_srf_r = interp_press
      wind_air_r  = interp_wind 
    ELSE
      t_air_r       = forcing_options%current%tair
      sw_srf_down_r = forcing_options%current%sw
      lw_srf_down_r = forcing_options%current%lw
    ENDIF

  END SUBROUTINE read_or_calculate_forcing


  ! ====================================================================================================== !
  !
  !> Estimate timestep temperature from daily min- and max temperature
  !>
  !> - copied from jsb4 (including old and new version)
  !> "
  !> In contrast to Marko's subroutine daytemp() (from which this code is derived) the routine
  !> correctly accounts for local time, i.e. temperatures are correct at all longitudes!
  !>
  !> More precisely the temperature development is modelled as follows:
  !> Let: T_min = T_mean - 0.5*T_range,  and: T_max = T_mean + 0.5*T_range. Then for a
  !>     (i) normal day (more than 4, but less than 20 hours of daylight):
  !>         --> during daylight: temperature behaves like a positive sine-wave, such that T_max is reached
  !>             at 14:00 and T_min at sunrise.
  !>         --> during night: starting at sunset the temperature drops linearly until T_min at sunrise.
  !>    (ii) short day (less than 4 hours of daylight):
  !>         temperature is set to T_mean at all time steps, i.e. temperature is assumed to be constant.
  !>   (iii) long day (more than 20 hours of daylight):
  !>         temperature is modelled as in the daylight-case of (i), 
  !>         but for the whole day, even during night hours.
  !>
  !> REMARKS:
  !>      -- The time computations are based on the Julian/Gregorian calender. Hence there is a slight
  !>         mismatch between the correct begin of a year, as defined by the orbit around the sun, and the
  !>         begin of the year according to the calender. The mismatch is about 3 days in 10000 years.
  !>         This affects the computation of daylength in the current routine, and thus the "phase" of the
  !>         temperature curve, i.e. that e.g. the time when the temperature starts rising in the morning,
  !>         may be somewhat shifted.
  !> "
  !> ... with minor changes in passed arguments, some subsequent reformatting, few variable name changes and changes in comments
  !> +++ added last days last temperature value for interpolation before sunrise to avoid discontinuities at midnight
  SUBROUTINE instantly_from_daily_temp_2(day, fract_timestep, lon, lat, dtemp_min, dtemp_max, yDay_last_tair, temp)
    ! -------------------------------------------------------------------------------------------------- !
    USE mo_qs_atmland_constants,     ONLY: declination

    IMPLICIT NONE

    REAL(wp), PARAMETER :: rat1 = 14._wp / 24._wp  ! day fraction at which maximum temperature shall occur
    REAL(wp), PARAMETER :: rat2 = 4._wp / 24._wp   ! minimum length of normal day (as fraction of day)
    REAL(wp), PARAMETER :: rat3 = 20._wp / 24._wp  ! maximum length of normal day (as fraction of day)
    REAL(wp), PARAMETER :: rat4 = 2._wp / 24._wp   ! day fraction of dawn times (before sunrise + after sunset), used to set ...
                                                   ! ... temperature of a normal day to T_mean at sunrise-rat4/2 and sunset+rat4/2
    ! -------------------------------------------------------------------------------------------------- !
    REAL(wp), INTENT (IN)  :: day              !< current day (in year; i.e. [1.0,365.0]
    REAL(wp), INTENT (IN)  :: fract_timestep   !< the current time step expressed as the fraction of the current day
    REAL(wp), INTENT (IN)  :: lon, lat         !< required for solar zenith angle calculation
    REAL(wp), INTENT (IN)  :: dtemp_min !< minimal temperature at the considered day
    REAL(wp), INTENT (IN)  :: dtemp_max !< maximal range at that day
    REAL(wp), INTENT (IN)  :: yDay_last_tair
      !< last temperature at previous day (required for interpolation before sunrise to avoid discontinuity)
    REAL(wp), INTENT (OUT) :: temp      !< estimated temperature for this timestep
    ! -------------------------------------------------------------------------------------------------- !
    REAL(wp) :: pio180             !< conversion factor from degrees to radians ("pi over 180")
    REAL(wp) :: fract_localtime    !< local time at a particular grid point expressed as the fraction of ...
                                   !< ... the current day since midnight (mod 1)
    REAL(wp) :: fract_UTC_offset   !< offset between UTC and local time (depends on longitude)
    REAL(wp) :: cpds, spds         !< cosine and sine contributions to zenith angle
    REAL(wp) :: fract_daylight     !< fraction of day between sunrise and sunset
    REAL(wp) :: fract_since_sunset !< time between current time step and sunset expressed as fraction of day
    REAL(wp) :: fract_sunrise      !< time of sunrise expressed as fraction of day since midnight
    REAL(wp) :: fract_sunset       !< time of sunset expressed as fraction of day since midnight
    REAL(wp) :: temp_sunset        !< temperature at sunset
    REAL(wp) :: dtemp              !< mean day temperature @todo: code has been replaced: "dusk day temp."?
    REAL(wp) :: dtran              !< day temperature range: maximum - minimum
    REAL(wp) :: sd                 !< modulation factor for temperature
    REAL(wp) :: arg
    REAL(wp) :: coslat, sinlat, yearday
    ! -------------------------------------------------------------------------------------------------- !
    yearday = INT(day)

    pio180 = pi / 180._wp

    coslat = COS(deg2rad * lat)
    sinlat = SIN(deg2rad * lat)

    temp = 0._wp

    ! JN-NOTE: In quincy standalone the current site-level forcing seems to be such that it ALWAYS assumes
    ! local time and not UTC! Therefore, fract_UTC_offset is not calculated as described below, but set to 1
    fract_UTC_offset = 1.0_wp
    ! ++ IF forcing and output SHOULD be UTC and not local time, fract_UTC_offset would need to be calculated:
    ! (+ but note: cos_zenith_angle for sw calculation would also need to be calculated differently?)
    ! fract_UTC_offset = lon/360._wp   ! difference between local time and UTC as fraction of a day

    ! Note: lons in jsbach4 run from 0 to 360, but lons in quincy run from 0-180/-180-0
    ! however, this does not to influence the calculation of fract_localtime due to the MOD formula
    fract_localtime = MOD(fract_timestep + fract_UTC_offset + 2._wp, 1._wp) ! local time as fraction of the local day (modulo 1)

    spds = sinlat*SIN(declination)  ! sine contribution to solar zenith angle
    cpds = coslat*COS(declination)  ! cosine contribution to solar zenith angle
    !@todo: ask TR about this?
    arg = -spds / cpds                  ! faulty?: for cpds=0 arg gets infinite
    IF (arg > 1._wp) THEN               ! polar night:
      fract_daylight = 0._wp            ! ... day length is zero
    ELSEIF (arg < -1._wp) THEN          ! polar day:
      fract_daylight = 1._wp            ! ... day length is the whole day
    ELSE                                ! normal day / night:
      fract_daylight = ACOS(arg) / pi   ! ... day length expressed as fraction of the whole day
    ENDIF

    !! BLARPP empirical correction to account for the fact that the scheme does actually not
    !! considered dtemp=0.5*(max+min). The night-day turn temperature is adjusted such that the mean
    !! temperature is within 0.05 K of the 0.5*(tmax+tmin) temperature
    IF (fract_daylight >= rat2 ) THEN ! -------- normal day ---------------------------------------------
      IF(fract_daylight < 0.36_wp)THEN
        arg = -8.0_wp * (fract_daylight - 0.36_wp) ** 2.0_wp + 0.685_wp
      ELSE
        arg = -0.8_wp * (fract_daylight - 0.36_wp) ** 2.0_wp + 0.685_wp
      ENDIF
    ELSE
      arg = 0.5_wp
    ENDIF

    dtemp = arg * dtemp_max + (1._wp - arg) * dtemp_min ! The dusk day temperature
    !dtemp = 0.5_wp*(dtemp_max+dtemp_min) ! The mean day temperature
    dtran = dtemp_max - dtemp_min        ! temperature range
    IF (fract_daylight >= rat2 .AND. fract_daylight <= rat3) THEN   ! -------- normal day ---------------------------------------------
      fract_sunrise = 0.5_wp - fract_daylight / 2._wp               ! time of sunrise expressed as fraction of day since midnight
      fract_sunset = fract_sunrise + fract_daylight                 ! time of sunset expressed as fraction of day since midnight
      IF (fract_localtime > fract_sunrise .AND. fract_localtime <= fract_sunset) THEN   ! For time steps at daylight ...
        sd = COS(pi * (fract_localtime - rat1) / (fract_daylight / 2._wp + rat4))       ! ... modulation factor for day-temperature such that
                                                                                        ! .. temperature is at maximum at rat1 (2 pm) and at average at
                                                                                        ! .. sunrise-rat4/2 and sunset+rat4/2
        IF (sd >= 0) THEN
          temp = dtemp + sd * (dtemp_max - dtemp)                 ! temperature modulated by temperature range
        ELSE
          temp = dtemp + sd * (dtemp - dtemp_min)                 ! temperature modulated by temperature range
        ENDIF
        !temp = dtemp + sd*dtran/2._wp                        ! temperature modulated by temperature range
      ELSE                                                    ! For time steps at night ...
        sd = COS(pi * (fract_sunset - rat1) / (fract_daylight / 2._wp + rat4)) ! modulation factor for temperature at sunset
        IF (sd >= 0) THEN
          temp_sunset = dtemp + sd * (dtemp_max - dtemp)                       ! temperature modulated by temperature range
        ELSE
          temp_sunset = dtemp + sd * (dtemp - dtemp_min)                       ! temperature modulated by temperature range
        ENDIF
        !temp_sunset = dtemp + dtran/2._wp*sd                                   ! temperature at sunset
        fract_since_sunset = MOD(fract_localtime - fract_sunset + 1._wp, 1._wp) ! day fraction since sunset

        !NEW:
        IF( (fract_localtime > eps8) &
            & .AND. (fract_localtime < fract_sunrise) .AND. (yDay_last_tair .NE. -999.0_wp)) THEN
          ! Night but before sunrise -> needs to account for prev. day temperature
          ! - here: interpolate between this days min and yesterdays last timestep tair
          temp = dtemp_min + (yDay_last_tair - dtemp_min) * ((fract_sunrise - fract_localtime) / fract_sunrise)
              ! interpolation of temperature such that temperature is equal to last tair of yesterday
              ! ... temperature at midnight and goes toward this days minimum temperature at sunrise.

        ELSE
          ! Night after sunset can keep linear interpolation as used in jsb3&4 since we only know the previous
          ! day temperatures, but not the next day temperatures 
          temp = dtemp_min + (temp_sunset - dtemp_min) &
            & * (1._wp - fract_since_sunset / (1._wp - fract_daylight)) ! linear interpolation
                                                                        ! ... of temperature such that temperature is equal to the mean
                                                                        ! ... temperature at sunset and drops as if the minimum
                                                                        ! ... temperature should be met at sunrise.
        ENDIF

        !BEFORE (JSB3&4) - for all .NOT. daylight time steps:
        !temp = dtemp_min+(temp_sunset-dtemp_min) &
        !  & *(1._wp-fract_since_sunset/(1._wp-fract_daylight))! linear interpolation
                                                            ! ... of temperature such that temperature is equal to the
                                                            ! ... temperature at sunset and drops until the minimum ...
                                                            ! ... temperature is met at sunrise.
      ENDIF
    ELSEIF (fract_daylight > rat3) THEN ! --- long day (close to polar day) ---------------------------------------------------
      ! case is now obsolete  @todo: if so, why is this then still in?
      sd = COS(pi * (fract_localtime - rat1) / (fract_daylight / 2._wp + rat4)) ! modulation factor for day-temperature such that ...
                                                                    ! ... temperature is at maximum at rat1 (2 pm) and at average
                                                                    ! ... at sunrise-rat4/2 and sunset+rat4/2
      temp = dtemp + dtran / 2._wp * sd                             ! temperature modulated by temperature range
    ELSE ! ------------------------------- short day (close to polar night) ---------------------------------------------------
      temp = dtemp                                          ! temperature is set to mean temperture at all time steps
    ENDIF

  END SUBROUTINE instantly_from_daily_temp_2


  ! ====================================================================================================== !
  !
  !> Returns a calculated timestep sw from daily avg sw
  !>
  !> extracted from jsb4 out of the shortwave_from_direct_shortwave routine
  !> ... with adaptations
  SUBROUTINE instantly_from_daily_sw(day, lat, cos_zenith, rad_sw_in, rad_sw)
    ! -------------------------------------------------------------------------------------------------- !
    REAL(wp), INTENT(IN)  :: day          !< current day (in year; i.e. [1.0,365.0]
    REAL(wp), INTENT(IN)  :: lat          !< lat of this site
    REAL(wp), INTENT(IN)  :: cos_zenith   !< cosine of zenith angle
    REAL(wp), INTENT(IN)  :: rad_sw_in    !< "Rtot": either timestep or daily average total solar shortwave radiation :
                                          !< ... direct+diffuse, 280-3000 nm, (i.e. PAR+NIR) that actually arrives at the bottom [W/m^2]
    REAL(wp), INTENT(OUT) :: rad_sw       !< timestep sw
    ! -------------------------------------------------------------------------------------------------- !
    INTEGER  :: day_of_year
    REAL(wp) :: delta, day_angle, u, v, hh, sinehh, rad_sw_in_corr, K
    ! -------------------------------------------------------------------------------------------------- !
    day_of_year = INT(day)

    K = 13750.98708_wp
    day_angle = 2._wp * pi * (day_of_year + 10._wp) / 365._wp !! ATTENTION: this may be incorrect for paleo situations
    ! today's sum of top of the atmosphere radiation
    delta = -23.4_wp * pi / 180._wp * COS(day_angle)
    u = SIN(deg2rad * lat) * SIN(delta)
    v = COS(deg2rad * lat) * COS(delta)
    IF (u .GE. v) THEN
       hh = pi
    ELSEIF (u .LE. (0._wp - v)) THEN
       hh = 0.0_wp
    ELSE
       hh = ACOS(-u / v)
    ENDIF
    sinehh = SIN(hh)

    !estimate of noon radiation giving rise to the recorded radiation sum
    rad_sw_in_corr = 0._wp
    IF(u * hh + v * sinehh .GT. 1e-5_wp) THEN
       rad_sw_in_corr = rad_sw_in / (2._wp * (u * hh + v * sinehh) * K) * 86400._wp
    ENDIF
    rad_sw = rad_sw_in_corr * cos_zenith

  END SUBROUTINE instantly_from_daily_sw

  ! ====================================================================================================== !
  !
  !> Computes longwave from observed longwave and diural course associated with air temperature
  !>
  !> copied from jsb4
  !> ... with adaptations
  !>
  !>"
  !> === longwave_from_daily_longwave() ====================================================================================
  !>
  !> Downward long wave radiation flux "R_d" [W/m^2] is according to [1],[2] computed by
  !> (1) R_d = r_cloud * epsA * sigma * T^4,
  !> where "T" is the air temperature [K] (in the bottom layer?), sigma = 5.6703e-8 W/(m^2 K^4) is the Stefan-Boltzmann constant,
  !> The mean daily longwave is known from observations, thus we are concerned only with the diurnal course associated with T
  !> [1] W. Knorr, "Satellite remote sensing and modelling of the global CO2 exchange of land vegetation", Examensarbeit 49,
  !> (Max Planck Institut fuer Meteorologie, Hamburg, 1998).
  !> [2] W. Brutsaert, "Evaporation into the Atmosphere", (Reidel, Dordrecht, 1982), pp. 299.
  !>
  SUBROUTINE longwave_from_daily_longwave(longwave_down_daily, Tmin, Tmax, air_temp, timestep_longwave)
    ! -------------------------------------------------------------------------------------------------- !
    REAL(wp),INTENT(IN) :: longwave_down_daily          !< daily average downward longwave radiation [W/m^2]
    ! Note: in jsb all temperature input is in °C but converted in the routine
    REAL(wp),INTENT(IN) :: Tmin                         !< daily mimimum temperature [K]
    REAL(wp),INTENT(IN) :: Tmax                         !< daily maximum temperature [K]
    REAL(wp),INTENT(IN) :: air_temp                     !< air temperature at bottom [K]
    REAL(wp),INTENT(OUT):: timestep_longwave            !< timestep longwave downward radiation [W/m^2]
    ! -------------------------------------------------------------------------------------------------- !
    REAL(wp) :: dtmp    ! daily average temperature
    ! -------------------------------------------------------------------------------------------------- !

    dtmp = 0.5_wp * (Tmax + Tmin)
    timestep_longwave = longwave_down_daily * (air_temp ** 4.0_wp) / (dtmp ** 4.0_wp)      ! lw-radiation flux Eq. (1)

  END SUBROUTINE longwave_from_daily_longwave

  ! ====================================================================================================== !
  !>
  !> Returns rain and snow for this timestep from daily avg precipitation and timestep temperature
  !>
  !> Here we follow OCN, since JSBACH uses daily average temperature and constant snow and rain
  !> instead of timestep temperature
  !>
  !> From OCN:
  !>"
  !>!---- for rain / snow partitioning, make it all rain if
  !>!---- ta > 2.5 C and all snow if ta <= 2.5 C
  !>!-----
  !>!---- reference:
  !>!-----
  !>!---- Auer, A. H., 1974: The rain versus snow threshold temperatures,
  !>!---- Weatherwise, 27, 67.
  !>!-----
  !>" 
  SUBROUTINE snow_and_rain_from_precip(precip_r, t_air_r, rain_r, snow_r)

    USE mo_jsb_physical_constants,    ONLY: rain_as_snow_temp_thresh
    ! -------------------------------------------------------------------------------------------------- !
    REAL(wp),INTENT(IN) :: precip_r          !< daily average precipitation [mm/s]
    REAL(wp),INTENT(IN) :: t_air_r           !< timestep temperature [K]
    REAL(wp),INTENT(OUT):: rain_r            !< timestep rain [mm/s]
    REAL(wp),INTENT(OUT):: snow_r            !< timestep snow [mm/s]
    ! -------------------------------------------------------------------------------------------------- !

    IF(t_air_r .GT. rain_as_snow_temp_thresh) THEN
      rain_r = precip_r
      snow_r = 0.0_wp
    ELSE 
      snow_r = precip_r
      rain_r = 0.0_wp
    ENDIF

  END SUBROUTINE snow_and_rain_from_precip

END MODULE mo_qs_atmland_forcing
