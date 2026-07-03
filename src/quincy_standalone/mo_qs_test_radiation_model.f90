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
!! @brief
!!  routine for running the test_radiation_model \n
!!  run QUINCY for one time step
!!
!! @author
!!  Sönke Zaehle, MPI-BGC Jena
!!  Jan Engel, MPI-BGC Jena: copied from mo_qs_test_canopy_model
!!
!! @par Revision History
!!  created from mo_qs_test_canopy_model to call the new routine calc_radiative_transfer
!!
MODULE mo_qs_test_radiation_model

  USE mo_kind,                    ONLY: wp

  USE mo_lnd_bgcm_idx
  USE mo_lnd_bgcm_store,          ONLY: t_lnd_bgcm_store
  USE mo_lnd_bgcm_store_class,    ONLY: VEG_BGCM_POOL_ID

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: run_test_radiation_model

  CHARACTER(len=*), PARAMETER :: modname = 'mo_qs_test_radiation_model'

CONTAINS

  ! ======================================================================================================= !
  !>Run the test_radiation model from the land model interface
  !>
  !>  run one timestep
  !> 
  !>  process inits: assimi_init, rad_init, veg_init
  !>
  SUBROUTINE run_test_radiation_model(tile, options)
    USE mo_jsb_class,                     ONLY: Get_model
    USE mo_jsb_tile_class,                ONLY: t_jsb_tile_abstract
    USE mo_jsb_task_class,                ONLY: t_jsb_task_options
    USE mo_jsb_model_class,               ONLY: t_jsb_model
    USE mo_jsb_lctlib_class,              ONLY: t_lctlib_element
    USE mo_jsb_process_class,             ONLY: ASSIMI_, RAD_, PHENO_, VEG_, SPQ_
    USE mo_jsb_grid_class,                ONLY: t_jsb_vgrid
    USE mo_jsb_grid,                      ONLY: Get_vgrid
    USE mo_atmland_constants,             ONLY: frac_vis_swtot_srf, frac_par_swvis_srf, eps_vpd, &
      &                                         standard_press_srf, def_co2_mixing_ratio
    USE mo_atmland_util,                  ONLY: calc_spec_humidity_sat
    USE mo_jsb_physical_constants,        ONLY: molar_mass_N, Tzero, grav
    USE mo_veg_canopy,                    ONLY: calc_canopy_layers
    USE mo_q_rad_scope,                   ONLY: calc_radiative_transfer
    USE mo_atmland_interface,             ONLY: update_a2l
    ! ----------------------------------------------------------------------------------------------------- !
    dsl4jsb_Use_config(ASSIMI_)    
    dsl4jsb_Use_memory(RAD_)
    dsl4jsb_Use_memory(PHENO_)
    dsl4jsb_Use_memory(VEG_)
    dsl4jsb_Use_memory(SPQ_)
    ! ----------------------------------------------------------------------------------------------------- !
    CLASS(t_jsb_tile_abstract), INTENT(inout)     :: tile         !< one tile with data structure for one lct
    TYPE(t_jsb_task_options),   INTENT(in)        :: options      !< model options
    ! ----------------------------------------------------------------------------------------------------- !
    TYPE(t_jsb_model),      POINTER  :: model             !< the model
    TYPE(t_lnd_bgcm_store), POINTER  :: bgcm_store        !< the bgcm store of this tile
    TYPE(t_lctlib_element), POINTER  :: lctlib           !< land-cover-type library - parameter across pft's
    TYPE(t_jsb_vgrid),      POINTER  :: vgrid_canopy     !< Vertical grid
    TYPE(t_jsb_vgrid),      POINTER  :: vgrid_spectral   !< Vertical grid used to display the sprectal bands of light
    INTEGER                          :: ncanopy          !< number of canopy layers
    INTEGER                          :: nspec            !< number of spectral bands
    REAL(wp), DIMENSION(options%nc)  :: &
      &                                 t_air            , &
      &                                 press_srf        , &
      &                                 q_air            , &
      &                                 co2_mixing_ratio , &
      &                                 rain             , &
      &                                 snow             , &
      &                                 swvis_srf_down   , &
      &                                 swnir_srf_down   , &
      &                                 swpar_srf_down   , &
      &                                 fract_par_diffuse, &
      &                                 wind_air       , &
      &                                 wind_10m       , &
      &                                 lw_srf_down    , &
      &                                 drag_srf       , &
      &                                 t_acoef        , &
      &                                 t_bcoef        , &
      &                                 q_acoef        , &
      &                                 q_bcoef        , &
      &                                 pch            , &
      &                                 cos_zenith_angle, &
      &                                 ga             !, &
      ! &                                 latitude       , &
      ! &                                 longitude      , &
      ! &                                 vpd
    REAL(wp), DIMENSION(options%nc)  :: hlp1
    INTEGER             :: i, lun           !< looping, logical unit number
    INTEGER             :: ref_pft          !< PFT identifier
    INTEGER, PARAMETER  :: npft  = 8        !< number of PFTs simulated by the test_canopy model
    ! parameters (specific to the test_canopy model)
    REAL(wp),SAVE :: ref_t_air = 25.0_wp              ! temperature in deg C
    REAL(wp),SAVE :: ref_vpd = 1000._wp               ! vpd in Pa
    REAL(wp),SAVE :: ref_swdown = 652.173913_wp       ! PAR in W m-2
    REAL(wp),SAVE :: ref_fdiffuse = 0.2_wp            ! diffuse fraction
    REAL(wp),SAVE :: ref_cos_angle = 0.8_wp           ! cosine of zenith angle
    REAL(wp),SAVE :: ref_co2_mixing_ratio = 380.0_wp  ! co2 in ppm
    REAL(wp),SAVE :: ref_wind = 1.0_wp                ! wind in m/s
    REAL(wp),SAVE :: ref_ga = 0.4_wp                  ! aerodynamic conductance in m/s
    REAL(wp),SAVE :: ref_theta = 1._wp                ! soil water availability in fraction of AWC
    REAL(wp),SAVE :: ref_lai = 3._wp                  ! LAI
    REAL(wp),SAVE :: ref_nleaf = 2.0_wp               ! N leaf per unit leaf area in gN/m2
    REAL(wp)                :: lctlib_g1              !< set to g1_medlyn or g1_bberry depending on canopy_conductance_scheme
    REAL(wp)                :: alpha                  !< implicitness factor
    INTEGER                 :: ipoi, ichunk           ! number of the chunk: ipoi 1:nc / ichunk ics:ice
    INTEGER                 :: iblk, ics, ice, nc
    CHARACTER(len=*), PARAMETER :: routine = TRIM(modname)//':run_test_radiation_model'
    ! ----------------------------------------------------------------------------------------------------- !
    dsl4jsb_Def_mt2L2D :: veg_pool_mt
    ! ----------------------------------------------------------------------------------------------------- !
    ! Declare process configuration and memory Pointers
    dsl4jsb_Def_config(ASSIMI_)
    dsl4jsb_Def_memory(RAD_)
    dsl4jsb_Def_memory(PHENO_)
    dsl4jsb_Def_memory(VEG_)
    dsl4jsb_Def_memory(SPQ_)
    ! ----------------------------------------------------------------------------------------------------- !
    ! RAD_
    dsl4jsb_Real3D_onChunk      :: ppfd_sunlit_tfrac_mavg_cl
    dsl4jsb_Real3D_onChunk      :: ppfd_shaded_tfrac_mavg_cl
    dsl4jsb_Real3D_onChunk      :: canopy_reflectance_sp
    ! SPQ_
    dsl4jsb_Real2D_onChunk      :: w_soil_root_theta 
    ! PHENO_
    dsl4jsb_Real2D_onChunk      :: lai 
    dsl4jsb_Real3D_onChunk      :: lai_cl
    dsl4jsb_Real3D_onChunk      :: cumm_lai_cl
    ! VEG_
    dsl4jsb_Real2D_onChunk      :: t_jmax_opt_mavg
    dsl4jsb_Real3D_onChunk      :: leaf_nitrogen_cl
    dsl4jsb_Real3D_onChunk      :: fn_rub_cl
    dsl4jsb_Real3D_onChunk      :: fn_et_cl
    dsl4jsb_Real3D_onChunk      :: fn_pepc_cl
    dsl4jsb_Real3D_onChunk      :: fn_chl_cl
    dsl4jsb_Real3D_onChunk      :: fn_oth_cl
    dsl4jsb_Real3D_onChunk      :: fleaf_sunlit_tfrac_mavg_cl
    ! ----------------------------------------------------------------------------------------------------- !
    iblk    = options%iblk
    ics     = options%ics
    ice     = options%ice
    nc      = options%nc
    alpha   = options%alpha
    ! ----------------------------------------------------------------------------------------------------- !
    ! if pftid is set in the namelist use this pftid
    IF(tile%lcts(1)%lib_id>0)THEN
      ref_pft = tile%lcts(1)%lib_id
    ! else use pftid 1
    ELSE
      ref_pft = 0
      tile%lcts(1)%lib_id = 1
    ENDIF
    model           => Get_model(tile%owner_model_id)
    lctlib          => model%lctlib(tile%lcts(1)%lib_id)
    vgrid_canopy    => Get_vgrid('canopy_layer')
    vgrid_spectral  => Get_vgrid('spectral_bands')
    ncanopy         =  vgrid_canopy%n_levels
    nspec           =  vgrid_spectral%n_levels
    ! ----------------------------------------------------------------------------------------------------- !
    dsl4jsb_Get_config(ASSIMI_)
    dsl4jsb_Get_memory(RAD_)
    dsl4jsb_Get_memory(PHENO_)
    dsl4jsb_Get_memory(VEG_)
    dsl4jsb_Get_memory(SPQ_)
    ! ----------------------------------------------------------------------------------------------------- !
    bgcm_store => tile%bgcm_store
    dsl4jsb_Get_mt2L2D(VEG_BGCM_POOL_ID, veg_pool_mt)
    ! ----------------------------------------------------------------------------------------------------- !
    dsl4jsb_Get_var3D_onChunk(RAD_, ppfd_sunlit_tfrac_mavg_cl)      ! in
    dsl4jsb_Get_var3D_onChunk(RAD_, ppfd_shaded_tfrac_mavg_cl)      ! in
    dsl4jsb_Get_var3D_onChunk(RAD_, canopy_reflectance_sp)
    ! ---------------------------
    dsl4jsb_Get_var2D_onChunk(SPQ_, w_soil_root_theta)
    ! ---------------------------
    dsl4jsb_Get_var2D_onChunk(PHENO_, lai)
    dsl4jsb_Get_var3D_onChunk(PHENO_, lai_cl)
    dsl4jsb_Get_var3D_onChunk(PHENO_, cumm_lai_cl)
    ! ---------------------------
    dsl4jsb_Get_var2D_onChunk(VEG_, t_jmax_opt_mavg)
    dsl4jsb_Get_var3D_onChunk(VEG_, leaf_nitrogen_cl)
    dsl4jsb_Get_var3D_onChunk(VEG_, fn_rub_cl)
    dsl4jsb_Get_var3D_onChunk(VEG_, fn_et_cl)
    dsl4jsb_Get_var3D_onChunk(VEG_, fn_pepc_cl)
    dsl4jsb_Get_var3D_onChunk(VEG_, fn_chl_cl)
    dsl4jsb_Get_var3D_onChunk(VEG_, fn_oth_cl)
    dsl4jsb_Get_var3D_onChunk(VEG_, fleaf_sunlit_tfrac_mavg_cl)     ! in
    ! ----------------------------------------------------------------------------------------------------- !

    !>0.9 define chunk for output: ipoi 1:nc / ichunk ics:ice
    !>
    ipoi   = 1
    ichunk = ics+ipoi-1

    !>1.0 Init
    !>

    !>  1.1 set g1 according to canopy_conductance_scheme
    !>
    SELECT CASE(TRIM(dsl4jsb_Config(ASSIMI_)%canopy_conductance_scheme))
      CASE ("medlyn")
        lctlib_g1 = lctlib%g1_medlyn
      CASE ("ballberry")
        lctlib_g1 = lctlib%g1_bberry
    END SELECT

    !>  1.2 Initalisation of met-forcing which is assumed constant
    !>
    t_air(:)            = ref_t_air + TZero
    press_srf(:)        = standard_press_srf
    q_air(:)            = calc_spec_humidity_sat(t_air(:),press_srf(:)) - ref_vpd / press_srf(:) * eps_vpd
    co2_mixing_ratio(:) = def_co2_mixing_ratio   ! def_co2_mixing_ratio is used instead of ref_co2_mixing_ratio
    rain(:)             = 0.0_wp
    snow(:)             = 0.0_wp
    swpar_srf_down(:)   = ref_swdown
    swvis_srf_down(:)   = swpar_srf_down(:) / frac_par_swvis_srf
    swnir_srf_down(:)   = swvis_srf_down(:) * (1._wp - frac_vis_swtot_srf) / frac_vis_swtot_srf
    fract_par_diffuse(:)= ref_fdiffuse
    wind_air(:)         = ref_wind
    wind_10m(:)         = wind_air(:)
    lw_srf_down(:)      = 0.0_wp
    ga(:)               = ref_ga 
    drag_srf(:)         = ga(:) / wind_air(:) * alpha * grav * model%options%dtime 
    t_acoef(:)          = 0.0_wp
    t_bcoef(:)          = 0.0_wp
    q_acoef(:)          = 0.0_wp
    q_bcoef(:)          = 0.0_wp
    pch(:)              = 0.0_wp
    cos_zenith_angle(:) = ref_cos_angle
    !latitude                  = 50.0_wp
    !longitude                 = 0.0_wp


    CALL update_a2l( &
           tile                                            , &
           options                                         , &
           t_air                   = t_air(:)              , &
           q_air                   = q_air(:)              , &
           wind_air                = wind_air(:)           , &
           wind_10m                = wind_10m(:)           , &
           swvis_srf_down          = swvis_srf_down(:)     , &
           swnir_srf_down          = swnir_srf_down(:)     , &
           swpar_srf_down          = swpar_srf_down(:)     , &
           fract_par_diffuse       = fract_par_diffuse(:)  , &
           press_srf               = press_srf(:)          , &
           drag_srf                = drag_srf(:)           , &
           cos_zenith_angle        = cos_zenith_angle(:)   , &
           co2_mixing_ratio        = co2_mixing_ratio(:)   , &
           ga                      = ga(:)                 )

    !> 1.2 Initialisation of soil and vegetation (so far relevant)
    !!
    !!
    ! soil water & lai 
    w_soil_root_theta(:) = ref_theta
    lai(:)   = ref_lai
    ! leaf N & P
    IF(ref_pft==0) THEN
      veg_pool_mt(ix_leaf, ixN, :) = ref_nleaf / molar_mass_N * lai(:)
    ELSE
      veg_pool_mt(ix_leaf, ixN, :) = lai(:) / lctlib%sla / lctlib%cn_leaf
      veg_pool_mt(ix_leaf, ixP, :) = veg_pool_mt(ix_leaf, ixN, :) / lctlib%np_leaf
    ENDIF
    ! canopy
    CALL calc_canopy_layers(nc                                                      , & ! in
                            ncanopy                                                 , &
                            vgrid_canopy%dz(:)                                      , &
                            vgrid_canopy%lbounds(:)                                 , &
                            vgrid_canopy%ubounds(:)                                 , &
                            lctlib%ps_pathway                                       , &
                            lctlib%k0_fn_struc                                      , &
                            lctlib%fn_oth_min                                       , &
                            lctlib%sla                                              , &
                            lctlib%np_leaf                                          , &
                            lctlib%gmin                                             , &
                            lctlib%g0                                               , &
                            lctlib_g1                                               , &
                            lctlib%t_jmax_omega                                     , &
                            dsl4jsb_Config(ASSIMI_)%flag_optimal_Nfraction          , &
                            TRIM(dsl4jsb_Config(ASSIMI_)%canopy_conductance_scheme) , &
                            veg_pool_mt(ix_leaf, ixN, :)                            , &
                            lai(:)                                                  , & ! in
                            ppfd_sunlit_tfrac_mavg_cl(:,:), &                           ! inout
                            ppfd_shaded_tfrac_mavg_cl(:,:), & 
                            fleaf_sunlit_tfrac_mavg_cl(:,:), &
                            fn_rub_cl(:,:), &
                            fn_et_cl(:,:), &
                            fn_pepc_cl(:,:), &
                            fn_chl_cl(:,:), &
                            fn_oth_cl(:,:), &                                           ! inout
                            lai_cl(:,:), &                                              ! out
                            cumm_lai_cl(:,:), &
                            leaf_nitrogen_cl(:,:), &                                    ! out
                            inquire_n_fractions = .TRUE.)                               ! optional in
    ! t_jmax_opt_mavg
    t_jmax_opt_mavg(:) = 20._wp+0.35_wp*(t_air(:) - Tzero)


    !>2.0 radiative transfer calculations
    !>
    CALL calc_radiative_transfer( &
      &                          nc                                              , & ! in
      &                          ncanopy                                         , &
      &                          nspec                                           , &
      &                          t_air(:)                                        , &
      &                          canopy_reflectance_sp(:,:)                        ) ! inout

  END SUBROUTINE run_test_radiation_model

END MODULE mo_qs_test_radiation_model
