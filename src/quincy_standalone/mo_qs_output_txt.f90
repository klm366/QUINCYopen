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
!! @brief functions that define and create QUINCY model output
!!
!! @author
!!  Jan Engel, MPI-BGC Jena
!!
!! @par Revision History
!! First version by Jan Engel (2016-03-14)
!! Second version by Jan Engel (20170630): remove ALLOCATE (only one tile is used), and init *_acc VAR and pools within its routines
!! 3rd version: rename from mo_shared_output to mo_qs_output_txt by Jan Engel (20190114)
!! 
!! @par Three Main Sections
!! section I Pools 
!! section II Fluxes 
!! section III Soil forcing
!! 
MODULE mo_qs_output_txt

  USE mo_kind,                  ONLY: wp
  USE mo_jsb_control,           ONLY: debug_on
  USE mo_exception,             ONLY: message_text, message, finish
  USE mo_jsb_lctlib_class,      ONLY: t_lctlib_element
  USE mo_qs_forcing,            ONLY: forcing_options
  USE mo_quincy_model_config,   ONLY: QLAND, QPLANT, QSOIL, QCANOPY, Q_TEST_CANOPY, Q_TEST_RADIATION

  USE mo_lnd_bgcm_idx
  USE mo_lnd_bgcm_store,          ONLY: t_lnd_bgcm_store
  USE mo_lnd_bgcm_store_class,    ONLY: VEG_BGCM_POOL_ID, VEG_BGCM_TOTAL_BIO_ID, VEG_BGCM_GROWTH_ID, &
    &                                   VEG_BGCM_LITTERFALL_ID, VEG_BGCM_FRAC_ALLOC_ID,              &
    &                                   VEG_BGCM_EXUDATION_ID, VEG_BGCM_ESTABLISHMENT_ID, VEG_BGCM_RESERVE_USE_ID
  USE mo_lnd_bgcm_store_class,    ONLY: SB_BGCM_POOL_ID, SB_BGCM_DELTA_POOLS_ID, SB_BGCM_FORMATION_ID, SB_BGCM_LOSS_ID, &
    &                                   SB_BGCM_TRANSPORT_ID, SB_BGCM_MYCO_EXPORT_ID

  dsl4jsb_Use_config(SB_)

  IMPLICIT NONE

  PUBLIC  ! all is public

  !-----------------------------------------------------------------------------------------------------
  !> Define local variables
  !! 
  !! 
  !-----------------------------------------------------------------------------------------------------
  INTEGER, PARAMETER, PRIVATE   :: ic = 1                     ! the point in ics:ice of which data will be written to file; per default = 1
  ! LUN - logical unit number used for output files (ICON also has some defined for err & out)
  INTEGER, PARAMETER            :: lun_sim_runtime_estimate   = 90
  INTEGER, PRIVATE, PARAMETER   :: lun_ascii_pools_start      = 100
  INTEGER, PRIVATE, PARAMETER   :: lun_ascii_pools_max        = 129
  INTEGER, PRIVATE, PARAMETER   :: lun_ascii_fluxes_start     = 130
  INTEGER, PRIVATE, PARAMETER   :: lun_ascii_fluxes_max       = 159
  INTEGER, PRIVATE, PARAMETER   :: lun_ascii_soil_forc_start  = 160
  INTEGER, PRIVATE, PARAMETER   :: lun_ascii_soil_forc_max    = 169

  CHARACTER(len=*), PARAMETER, PRIVATE :: modname = 'mo_qs_output_txt'

CONTAINS

  !-----------------------------------------------------------------------------------------------------!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! section I Pools                                                                                   !!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !-----------------------------------------------------------------------------------------------------!


  !-----------------------------------------------------------------------------------------------------
  !> Pools: open files and write column headers
  !! 
  !! open output files, define file names for pools, write column header names
  !! 
  !! lun: start at 100 with a current max of 129 as flux outputs starts with lun 130
  !-----------------------------------------------------------------------------------------------------
  SUBROUTINE open_pool_output_files(tile, iblk, ics, ice, nc, output_interval_pool, spinup_id)

    USE mo_jsb_class,             ONLY: Get_model
    USE mo_jsb_tile_class,        ONLY: t_jsb_tile_abstract
    USE mo_jsb_model_class,       ONLY: t_jsb_model
    
    IMPLICIT NONE
    ! ---------------------------
    ! 0.1 InOut
    CLASS(t_jsb_tile_abstract), INTENT(in)      :: tile           !< one tile with data structure for one lct
    INTEGER,INTENT(in)                          :: iblk, &        !< iBlock, start point of chunk, end point of chunk, number of points in the chunk
                                                   ics, &
                                                   ice, &
                                                   nc
    CHARACTER(len=*), INTENT(in)                :: output_interval_pool
    CHARACTER(len=*), INTENT(in)                :: spinup_id
    ! ---------------------------
    ! 0.2 Local
    TYPE(t_jsb_model),      POINTER       :: model
    INTEGER                               :: lun                  ! logical unit number
    CHARACTER(100)                        :: fname                ! filename
    CHARACTER(len=*), PARAMETER :: routine = TRIM(modname)//':open_pool_output_files'
    ! ---------------------------
    ! 0.5 Get Memory
    model  => Get_model(tile%owner_model_id)


    !>1.0 init
    !>
    lun = lun_ascii_pools_start

    !>2.0 define open() statements for file output
    !> 
    SELECT CASE(model%config%qmodel_id)
    CASE(QLAND, QPLANT)
      ! vegpoolC
      fname = 'vegpoolC'//'_'//TRIM(spinup_id)//TRIM(output_interval_pool)//'.txt'
      OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
      lun=lun+1
      ! vegpooldC13DC14
      fname = 'vegpooldC13DC14'//'_'//TRIM(spinup_id)//TRIM(output_interval_pool)//'.txt'
      OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
      lun=lun+1
      ! vegpoolN
      fname = 'vegpoolN'//'_'//TRIM(spinup_id)//TRIM(output_interval_pool)//'.txt'
      OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
      lun=lun+1
      ! vegpoolP
      fname = 'vegpoolP'//'_'//TRIM(spinup_id)//TRIM(output_interval_pool)//'.txt'
      OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
      lun=lun+1
      ! vegpooldN15
      fname = 'vegpooldN15'//'_'//TRIM(spinup_id)//TRIM(output_interval_pool)//'.txt'
      OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
      lun=lun+1
      ! soilpoolPhysics
      fname = 'soilpoolPhysics'//'_'//TRIM(spinup_id)//TRIM(output_interval_pool)//'.txt'
      OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
      lun=lun+1
      ! soilpoolPhysics_lay
      fname = 'soilpoolPhysics_lay'//'_'//TRIM(spinup_id)//TRIM(output_interval_pool)//'.txt'
      OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
      lun=lun+1
      ! soilpoolInOrg
      fname = 'soilpoolInOrg'//'_'//TRIM(spinup_id)//TRIM(output_interval_pool)//'.txt'
      OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
      lun=lun+1
      ! ecopoolCNP
      fname = 'ecopoolCNP'//'_'//TRIM(spinup_id)//TRIM(output_interval_pool)//'.txt'
      OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
      lun=lun+1
      ! soilpooldC13DC14dN15
      fname = 'soilpooldC13DC14dN15'//'_'//TRIM(spinup_id)//TRIM(output_interval_pool)//'.txt'
      OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
      lun=lun+1
      ! veg_diagnostics
      fname = 'veg_diagnostics'//'_'//TRIM(spinup_id)//TRIM(output_interval_pool)//'.txt'
      OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
      lun=lun+1
    END SELECT

    ! land & plant & canopy
    SELECT CASE(model%config%qmodel_id)
      CASE(QLAND, QPLANT, QCANOPY)
        ! veg_canopy
        fname = 'veg_canopy'//'_'//TRIM(spinup_id)//TRIM(output_interval_pool)//'.txt'
        OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
        lun=lun+1
    END SELECT

    ! land & plant & soil
    SELECT CASE(model%config%qmodel_id)
    CASE(QLAND, QPLANT, QSOIL)
      ! soilpoolOrg
      fname = 'soilpoolOrg'//'_'//TRIM(spinup_id)//TRIM(output_interval_pool)//'.txt'
      OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
      lun=lun+1
    END SELECT
    
    ! land & soil
    SELECT CASE(model%config%qmodel_id)
    CASE(QLAND, QSOIL)
      ! soilpoolCOrg_lay
      fname = 'soilpoolCOrg_lay'//'_'//TRIM(spinup_id)//TRIM(output_interval_pool)//'.txt'
      OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
      lun=lun+1
      ! soilpoolNOrg_lay
      fname = 'soilpoolNOrg_lay'//'_'//TRIM(spinup_id)//TRIM(output_interval_pool)//'.txt'
      OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
      lun=lun+1
      ! soilpoolPOrg_lay
      fname = 'soilpoolPOrg_lay'//'_'//TRIM(spinup_id)//TRIM(output_interval_pool)//'.txt'
      OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
      lun=lun+1
      ! soilpoolInNOrg_lay
      fname = 'soilpoolInNOrg_lay'//'_'//TRIM(spinup_id)//TRIM(output_interval_pool)//'.txt'
      OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
      lun=lun+1
      ! soilpoolInPOrg_lay
      fname = 'soilpoolInPOrg_lay'//'_'//TRIM(spinup_id)//TRIM(output_interval_pool)//'.txt'
      OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
      lun=lun+1
      ! soilpooldC13org_lay
      fname = 'soilpooldC13org_lay'//'_'//TRIM(spinup_id)//TRIM(output_interval_pool)//'.txt'
      OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
      lun=lun+1
      ! soilpoolDC14org_lay
      fname = 'soilpoolDC14org_lay'//'_'//TRIM(spinup_id)//TRIM(output_interval_pool)//'.txt'
      OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
      lun=lun+1
      ! soilpooldN15org_lay
      fname = 'soilpooldN15org_lay'//'_'//TRIM(spinup_id)//TRIM(output_interval_pool)//'.txt'
      OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
      lun=lun+1
      ! soil_diagnostics_microbial_lay
      fname = 'soil_diagnostics_microbial_lay'//'_'//TRIM(spinup_id)//TRIM(output_interval_pool)//'.txt'
      OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
      lun=lun+1
      ! soil_diagnostics_sorption_lay
      fname = 'soil_diagnostics_sorption_lay'//'_'//TRIM(spinup_id)//TRIM(output_interval_pool)//'.txt'
      OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
      lun=lun+1
    END SELECT


    !> 2.1  test if lun for pools is too big
    !! 
    IF (lun > lun_ascii_pools_max) THEN
      WRITE(message_text,'(a)') 'lun of pools is too big, reaching values that are reserved for lun of flux output'
      CALL finish(TRIM(routine),message_text)
    END IF


    !> 3.0 write column headers into output files
    !! 
    !! 
    CALL init_output_writing_pool(model%config%qmodel_id)

  END SUBROUTINE open_pool_output_files


  !-----------------------------------------------------------------------------------------------------
  !> Pools: write column headers of output files
  !! 
  !! 
  !-----------------------------------------------------------------------------------------------------
  SUBROUTINE init_output_writing_pool(qmodel_id)

    USE mo_jsb_grid_class,        ONLY: t_jsb_vgrid
    USE mo_jsb_grid,              ONLY: Get_vgrid

    IMPLICIT NONE
    ! ---------------------------
    ! 0.1 InOut
    INTEGER  ,INTENT(in) :: qmodel_id
    ! ---------------------------
    ! 0.2 Local
    TYPE(t_jsb_vgrid),POINTER :: vgrid_soil_sb,   &           ! Vertical grid
                                 vgrid_snow_e
    INTEGER                   :: nsoil_sb                     ! number of soil layers as used/defined by the SB_ process
    INTEGER                   :: lun,isoil
    CHARACTER(2)              :: layerstr
    CHARACTER(450)            :: p1str,p2str,p3str,p4str,p5str,p6str,p7str,p8str,p9str,p10str, &
                                 p11str,p12str,p13str,p14str,p15str,p16str,p17str,p18str,p19str, &
                                 p20str
    ! ---------------------------
    ! 0.5 Get Memory
    vgrid_soil_sb => Get_vgrid('soil_layer_sb')
    nsoil_sb      =  vgrid_soil_sb%n_levels

    !>1.0 write headers
    !>
    lun = lun_ascii_pools_start
    !
    SELECT CASE(qmodel_id)
    CASE(QLAND, QPLANT)
      ! vegpoolC  [mol C / m2]
      WRITE(lun,'(2a)') 'Time Leaf_C FineRoot_C SapWood_C HeartWood_C CoarseRoot_C ', & 
                       'Fruit_C Labile_C Reserve_C SeedBank_C Total_C Total_C13 Total_C14'
      lun=lun+1
      ! vegpooldC13DC14
      WRITE(lun,'(4a)') 'Time Leaf_dC13 FineRoot_dC13 SapWood_dC13 HeartWood_dC13 CoarseRoot_dC13 ', & 
                       'Fruit_dC13 Labile_dC13 Reserve_dC13 SeedBank_dC13 Leaf_DC14 FineRoot_DC14 ', &
                       'SapWood_DC14 HeartWood_DC14 CoarseRoot_DC14 Fruit_DC14 Labile_DC14 ', &
                       'Reserve_DC14 SeedBank_DC14'
      lun=lun+1
      ! vegpoolN  [mol N / m2]
      WRITE(lun,'(2a)') 'Time Leaf_N FineRoot_N SapWood_N HeartWood_N CoarseRoot_N ', & 
                       'Fruit_N Labile_N Reserve_N SeedBank_N Total_N'
      lun=lun+1
      ! vegpoolP  [mol P / m2]
      WRITE(lun,'(2a)') 'Time Leaf_P FineRoot_P SapWood_P HeartWood_P CoarseRoot_P ', & 
                       'Fruit_P Labile_P Reserve_P SeedBank_P Total_P'
      lun=lun+1
      ! vegpooldN15
      WRITE(lun,'(2a)') 'Time Leaf_dN15 FineRoot_dN15 SapWood_dN15 HeartWood_dN15 CoarseRoot_dN15 ', & 
                       'Fruit_dN15 Labile_dN15 Reserve_dN15 SeedBank_dN15 Total_dN15'
      lun=lun+1
      ! soilpoolPhysics  [m, m, m, na, na]
      WRITE(lun,'(a)') 'Time SnowHeight RootZoneSoilMoisture RootZoneSoilWaterPotential'

      lun=lun+1
      p1str=''
      p2str=''
      DO isoil = 1,nsoil_sb
         IF(isoil<10) THEN 
           WRITE(layerstr,'(I1)') isoil
         ELSE
           WRITE(layerstr,'(I2)') isoil
         ENDIF
         WRITE(p1str,'(a)') TRIM(p1str)//' WaterContentSoil_L'//TRIM(layerstr)
         WRITE(p2str,'(a)') TRIM(p2str)//' TemperatureSoil_L'//TRIM(layerstr)
      ENDDO
      ! soilpoolPhysics_lay  [m, K]( output per soil_layer for two SPQ_ variables )
      WRITE(lun,'(3a)') 'Time ',TRIM(p1str), TRIM(p2str)
      lun=lun+1
      ! soilpoolInOrg [mol / m2]
      WRITE(lun,'(a)') 'Time MineralAssocNH4 MineralNH4 MineralNO3 MineralP NOY N2O N2'
      lun=lun+1
      ! ecopoolCNP [mol / m2]
      WRITE(lun,'(a)') 'Time TotalSoilOrgC TotalSoilOrgN TotalSoilInOrgN EcoC EcoN EcoP EcoN15'
      lun=lun+1
      ! soilpooldC13DC14dN15
      WRITE(lun,'(4a)') 'Time MetLitterdC13 StrLitterdC13 WodLitterdC13 FastdC13 SlowdC13 MycdC13 MetLitterDC14 StrLitterDC14',& 
                        ' WodLitterDC14 FastDC14 SlowDC14 MycDC14',&
                        ' MetLitterdN15 StrLitterdN15 WodLitterN15', & 
                        ' FastdN15 SlowdN15 MineralAssocNH4_dN15 MineralNH4_dN15 MineralNO3_dN15'
      lun=lun+1
      ! veg_diagnostics [???]

      WRITE(lun,'(6a)') 'Time Height Diameter LAI ind_density LeafM2RootM', &
                        ' growing_season Ndemand Pdemand sinklim', &
                        ' growthReqN_uptMavg growthReqP_uptMavg', &
                        ' BetaSoilGS BetaSoilPS',&
                        ' beta_exudation_c beta_exudation_n beta_exudation_p', &
                        ' beta_exudation beta_exudation_mavg'
      lun=lun+1
    END SELECT


    !! land & plant & canopy
    !! 
    SELECT CASE(qmodel_id)
      CASE(QLAND, QPLANT, QCANOPY)
        ! veg_canopy [???]
        WRITE(lun,'(a)') 'Time CanopyLayer f_et f_rub f_chl f_pepc f_oth leaf_nitrogen jmax vcmax chlfl_yield'
        lun=lun+1
    END SELECT

    !! land & plant & soil
    !! (simple_1d, jsm)
    SELECT CASE(qmodel_id)
    CASE(QLAND, QPLANT, QSOIL)
      ! soilpoolOrg [mol / m2]
      WRITE(lun,'(7a)') &
        & 'Time ', &
        & 'poolC_dom poolC_domAs poolC_solLitter poolC_polLitter poolC_wooLitter ', &
        & 'poolC_fungi poolC_myco poolC_mic poolC_res poolC_resAs ', &
        & 'poolN_dom poolN_domAs poolN_solLitter poolN_polLitter poolN_wooLitter ', &
        & 'poolN_fungi poolN_myco poolN_mic poolN_res poolN_resAs ', &
        & 'poolP_dom poolP_domAs poolP_solLitter poolP_polLitter poolP_wooLitter ', &
        & 'poolP_fungi poolP_myco poolP_mic poolP_res poolP_resAs'
      lun=lun+1
    END SELECT

    !! land & soil
    !! detailed data per layer only for simple_1d and jsm
    SELECT CASE(qmodel_id)
    CASE(QLAND, QSOIL)
      ! soilpoolCOrg_lay [mol C / m3]
      p1str=''
      p2str=''
      p3str=''
      p4str=''
      p5str=''
      p6str=''
      p7str=''
      p8str=''
      p9str=''
      p10str=''
      DO isoil = 1,nsoil_sb
        IF(isoil<10) THEN 
          WRITE(layerstr,'(I1)') isoil
        ELSE
          WRITE(layerstr,'(I2)') isoil
        ENDIF
        WRITE(p1str,'(a)') TRIM(p1str)//' SoluLit_L'//TRIM(layerstr)
        WRITE(p2str,'(a)') TRIM(p2str)//' PolyLit_L'//TRIM(layerstr)
        WRITE(p3str,'(a)') TRIM(p3str)//' WodLit_L'//TRIM(layerstr)
        WRITE(p4str,'(a)') TRIM(p4str)//' DOM_L'//TRIM(layerstr)
        WRITE(p5str,'(a)') TRIM(p5str)//' aDOM_L'//TRIM(layerstr)
        WRITE(p6str,'(a)') TRIM(p6str)//' Fungi_L'//TRIM(layerstr)
        WRITE(p7str,'(a)') TRIM(p7str)//' Myc_L'//TRIM(layerstr)
        WRITE(p8str,'(a)') TRIM(p8str)//' Mic_L'//TRIM(layerstr)
        WRITE(p9str,'(a)') TRIM(p9str)//' Res_L'//TRIM(layerstr)
        WRITE(p10str,'(a)') TRIM(p10str)//' aRes_L'//TRIM(layerstr)
      ENDDO
      WRITE(lun,'(11a)') 'Time ',TRIM(p1str),TRIM(p2str),TRIM(p3str),TRIM(p4str),TRIM(p5str),TRIM(p6str),TRIM(p7str),&
        TRIM(p8str),TRIM(p9str),TRIM(p10str)
      lun=lun+1
      
      !soilpoolNOrg_lay [mol N / m3]
      WRITE(lun,'(11a)') 'Time ',TRIM(p1str),TRIM(p2str),TRIM(p3str),TRIM(p4str),TRIM(p5str),TRIM(p6str),TRIM(p7str),&
        TRIM(p8str),TRIM(p9str),TRIM(p10str)
      lun=lun+1
      
      !soilpoolPOrg_lay [mol P / m3]
      WRITE(lun,'(11a)') 'Time ',TRIM(p1str),TRIM(p2str),TRIM(p3str),TRIM(p4str),TRIM(p5str),TRIM(p6str),TRIM(p7str),&
        TRIM(p8str),TRIM(p9str),TRIM(p10str)
      lun=lun+1
      
      !soilpoolInNOrg_lay [mol N / m3]
      p1str=''
      p2str=''
      p3str=''
      p4str=''
      p5str=''
      p6str=''
      p7str=''
      p8str=''
      p9str=''
      DO isoil = 1,nsoil_sb
        IF(isoil<10) THEN 
          WRITE(layerstr,'(I1)') isoil
        ELSE
          WRITE(layerstr,'(I2)') isoil
        END IF
        WRITE(p1str,'(a)') TRIM(p1str)//' NH4assoc_L'//TRIM(layerstr)
        WRITE(p2str,'(a)') TRIM(p2str)//' NH4solute_L'//TRIM(layerstr)
        WRITE(p3str,'(a)') TRIM(p3str)//' NO3solute_L'//TRIM(layerstr)
        WRITE(p4str,'(a)') TRIM(p4str)//' NOY_L'//TRIM(layerstr)
        WRITE(p5str,'(a)') TRIM(p5str)//' N2O_L'//TRIM(layerstr)
        WRITE(p6str,'(a)') TRIM(p6str)//' N2_L'//TRIM(layerstr)
        WRITE(p7str,'(a)') TRIM(p7str)//' dN15_NH4assoc_L'//TRIM(layerstr)
        WRITE(p8str,'(a)') TRIM(p8str)//' dN15_NH4solute_L'//TRIM(layerstr)
        WRITE(p9str,'(a)') TRIM(p9str)//' dN15_NO3solute_L'//TRIM(layerstr)
      END DO
      WRITE(lun,'(10a)') 'Time ',TRIM(p1str),TRIM(p2str),TRIM(p3str),TRIM(p4str),TRIM(p5str),TRIM(p6str), &
         TRIM(p7str),TRIM(p8str),TRIM(p9str)
      lun=lun+1
      
      !soilpoolInPOrg_lay [mol P / m3]
      p1str=''
      p2str=''
      p3str=''
      p4str=''
      p5str=''
      DO isoil = 1,nsoil_sb
        IF(isoil<10) THEN 
          WRITE(layerstr,'(I1)') isoil
        ELSE
          WRITE(layerstr,'(I2)') isoil
        ENDIF
        WRITE(p1str,'(a)') TRIM(p1str)//' PO4assoc_fast_L'//TRIM(layerstr)
        WRITE(p2str,'(a)') TRIM(p2str)//' PO4assoc_slow_L'//TRIM(layerstr)
        WRITE(p3str,'(a)') TRIM(p3str)//' PO4assoc_occl_L'//TRIM(layerstr)
        WRITE(p4str,'(a)') TRIM(p4str)//' PO4solute_L'//TRIM(layerstr)
        WRITE(p5str,'(a)') TRIM(p5str)//' PO4primary_L'//TRIM(layerstr)
      ENDDO
      WRITE(lun,'(6a)') 'Time ',TRIM(p1str),TRIM(p2str),TRIM(p3str),TRIM(p4str),TRIM(p5str)
      lun=lun+1
      
      ! dC13 DC14 dN15
      p1str=''
      p2str=''
      p3str=''
      p4str=''
      p5str=''
      p6str=''
      p7str=''
      p8str=''
      p9str=''
      p10str=''
      DO isoil = 1,nsoil_sb
        IF(isoil<10) THEN 
          WRITE(layerstr,'(I1)') isoil
        ELSE
          WRITE(layerstr,'(I2)') isoil
        ENDIF
        WRITE(p1str,'(a)') TRIM(p1str)//' SoluLit_L'//TRIM(layerstr)
        WRITE(p2str,'(a)') TRIM(p2str)//' PolyLit_L'//TRIM(layerstr)
        WRITE(p3str,'(a)') TRIM(p3str)//' WodLit_L'//TRIM(layerstr)
        WRITE(p4str,'(a)') TRIM(p4str)//' DOM_L'//TRIM(layerstr)
        WRITE(p5str,'(a)') TRIM(p5str)//' aDOM_L'//TRIM(layerstr)
        WRITE(p6str,'(a)') TRIM(p6str)//' Fungi_L'//TRIM(layerstr)
        WRITE(p7str,'(a)') TRIM(p7str)//' Myc_L'//TRIM(layerstr)
        WRITE(p8str,'(a)') TRIM(p8str)//' Mic_L'//TRIM(layerstr)
        WRITE(p9str,'(a)') TRIM(p9str)//' Res_L'//TRIM(layerstr)
        WRITE(p10str,'(a)') TRIM(p10str)//' aRes_L'//TRIM(layerstr)
      ENDDO
      ! soilpooldC13org_lay            
      WRITE(lun,'(11a)') 'Time ',TRIM(p1str),TRIM(p2str),TRIM(p3str),TRIM(p4str),TRIM(p5str),TRIM(p6str),TRIM(p7str),&
        TRIM(p8str),TRIM(p9str),TRIM(p10str)
      lun=lun+1          
      ! soilpoolDC14org_lay       
      WRITE(lun,'(11a)') 'Time ',TRIM(p1str),TRIM(p2str),TRIM(p3str),TRIM(p4str),TRIM(p5str),TRIM(p6str),TRIM(p7str),&
        TRIM(p8str),TRIM(p9str),TRIM(p10str)
      lun=lun+1      
      ! soilpooldN15org_lay       
      WRITE(lun,'(11a)') 'Time ',TRIM(p1str),TRIM(p2str),TRIM(p3str),TRIM(p4str),TRIM(p5str),TRIM(p6str),TRIM(p7str),&
        TRIM(p8str),TRIM(p9str),TRIM(p10str)
      lun=lun+1 
      
      ! soil_diagnostics_microbial_lay [mol C / m3, kg/m3, na, mol P / m3, mol P / m3, mol C / m3]
      p1str=''
      p2str=''
      p3str=''
      p4str=''
      p5str=''
      p6str=''
      p7str=''
      p8str=''
      p9str=''
      p10str=''
      p11str=''
      p12str=''
      p13str=''
      p14str=''
      p15str=''
      p16str=''
      p17str=''
      p18str=''
      p19str=''
      p20str=''

      DO isoil = 1,nsoil_sb
        IF(isoil<10) THEN 
          WRITE(layerstr,'(I1)') isoil
        ELSE
          WRITE(layerstr,'(I2)') isoil
        ENDIF
        WRITE(p1str,'(a)') TRIM(p1str)//' CUEmic_L'//TRIM(layerstr)
        WRITE(p2str,'(a)') TRIM(p2str)//' NUEmic_L'//TRIM(layerstr)
        WRITE(p3str,'(a)') TRIM(p3str)//' PUEmic_L'//TRIM(layerstr)
        WRITE(p4str,'(a)') TRIM(p4str)//' RootC_L'//TRIM(layerstr)
        WRITE(p5str,'(a)') TRIM(p5str)//' EnzPoly_L'//TRIM(layerstr) 
        WRITE(p6str,'(a)') TRIM(p6str)//' EnzRes_L'//TRIM(layerstr) 
        WRITE(p7str,'(a)') TRIM(p7str)//' EnzPolyC_L'//TRIM(layerstr) 
        WRITE(p8str,'(a)') TRIM(p8str)//' EnzPolyN_L'//TRIM(layerstr) 
        WRITE(p9str,'(a)') TRIM(p9str)//' EnzPolyP_L'//TRIM(layerstr) 
        WRITE(p10str,'(a)') TRIM(p10str)//' EnzPolyC_mavg_L'//TRIM(layerstr) 
        WRITE(p11str,'(a)') TRIM(p11str)//' EnzPolyN_mavg_L'//TRIM(layerstr) 
        WRITE(p12str,'(a)') TRIM(p12str)//' EnzPolyP_mavg_L'//TRIM(layerstr)
        WRITE(p13str,'(a)') TRIM(p13str)//' DOM_CN_L'//TRIM(layerstr)
        WRITE(p14str,'(a)') TRIM(p14str)//' DOM_CP_L'//TRIM(layerstr)
        WRITE(p15str,'(a)') TRIM(p15str)//' DOM_CN_mavg_L'//TRIM(layerstr)
        WRITE(p16str,'(a)') TRIM(p16str)//' DOM_CP_mavg_L'//TRIM(layerstr)
        WRITE(p17str,'(a)') TRIM(p17str)//' enzyme_frac_AP_L'//TRIM(layerstr)
        WRITE(p18str,'(a)') TRIM(p18str)//' lim_stat_diagn_L'//TRIM(layerstr)
        WRITE(p19str,'(a)') TRIM(p19str)//' fact_n_status_L'//TRIM(layerstr)
        WRITE(p20str,'(a)') TRIM(p20str)//' fact_p_status_L'//TRIM(layerstr)
      ENDDO

      WRITE(lun,'(21a)') 'Time ',TRIM(p1str),TRIM(p2str),TRIM(p3str),TRIM(p4str),TRIM(p5str),TRIM(p6str),TRIM(p7str),&
        TRIM(p8str),TRIM(p9str),TRIM(p10str),TRIM(p11str),TRIM(p12str),TRIM(p13str),TRIM(p14str),TRIM(p15str),TRIM(p16str),&
        TRIM(p17str),TRIM(p18str),TRIM(p19str),TRIM(p20str)

      lun=lun+1    

      ! soil_diagnostics_sorption_lay
      p1str=''
      p2str=''
      p3str=''
      p4str=''
      p5str=''
      p6str=''
      p7str=''
      p8str=''
      p9str=''
      DO isoil = 1,nsoil_sb
        IF(isoil<10) THEN 
          WRITE(layerstr,'(I1)') isoil
        ELSE
          WRITE(layerstr,'(I2)') isoil
        ENDIF
        WRITE(p1str,'(a)') TRIM(p1str)//' QmaxOrg_L'//TRIM(layerstr)
        WRITE(p2str,'(a)') TRIM(p2str)//' Rho_L'//TRIM(layerstr)
        WRITE(p3str,'(a)') TRIM(p3str)//' RhoCor_L'//TRIM(layerstr)
        WRITE(p4str,'(a)') TRIM(p4str)//' QmaxPo4_L'//TRIM(layerstr)
        WRITE(p5str,'(a)') TRIM(p5str)//' KmPo4_L'//TRIM(layerstr)
        WRITE(p9str,'(a)') TRIM(p9str)//' MinVol_L'//TRIM(layerstr)
      ENDDO
      WRITE(lun,'(10a)') 'Time ',TRIM(p1str),TRIM(p2str),TRIM(p3str),TRIM(p4str),TRIM(p5str),TRIM(p6str),TRIM(p7str),&
        TRIM(p8str),TRIM(p9str)
      lun=lun+1    
    END SELECT

  END SUBROUTINE init_output_writing_pool

  ! ======================================================================================================= !
  !>Pools: write data to files in a certain interval
  !> 
  SUBROUTINE write_output_pool(tile, options, time_d)

    USE mo_jsb_class,             ONLY: Get_model
    USE mo_jsb_tile_class,        ONLY: t_jsb_tile_abstract
    USE mo_jsb_task_class,        ONLY: t_jsb_task_options
    USE mo_jsb_model_class,       ONLY: t_jsb_model
    USE mo_veg_constants,         ONLY: itree
    USE mo_jsb_math_constants,    ONLY: eps8
    USE mo_isotope_util,          ONLY: calc_delta_C13,calc_delta_C14,calc_delta_N15
    USE mo_jsb_process_class,     ONLY: A2L_, L2A_, SEB_, TURB_, SPQ_, HYDRO_, VEG_, HD_, Q_RAD_, Q_ASSIMI_, Q_PHENO_, SB_
    USE mo_jsb_process_class,     ONLY: VEG_, Q_ASSIMI_, Q_PHENO_, SB_, SPQ_
    USE mo_jsb_grid_class,        ONLY: t_jsb_vgrid
    USE mo_jsb_grid,              ONLY: Get_vgrid
    ! ----------------------------------------------------------------------------------------------------- !
    dsl4jsb_Use_config(SB_)
    dsl4jsb_Use_memory(Q_ASSIMI_)
    dsl4jsb_Use_memory(VEG_)
    dsl4jsb_Use_memory(Q_PHENO_)
    dsl4jsb_Use_memory(SPQ_)
    dsl4jsb_Use_memory(SB_)
    ! ----------------------------------------------------------------------------------------------------- !
    CLASS(t_jsb_tile_abstract), INTENT(in) :: tile         !< one tile with data structure for one lct
    TYPE(t_jsb_task_options),   INTENT(in) :: options      !< model options
    REAL(wp),                   INTENT(in) :: time_d       !< time in days
    ! ----------------------------------------------------------------------------------------------------- !
    TYPE(t_jsb_model),      POINTER      :: model                     !< the model
    TYPE(t_lnd_bgcm_store), POINTER      :: bgcm_store                !< the bgcm store of this tile
    TYPE(t_lctlib_element), POINTER      :: lctlib                    !< land-cover-type library - parameter across pft's
    TYPE(t_jsb_vgrid),      POINTER      :: vgrid_canopy              !< Vertical grid
    TYPE(t_jsb_vgrid),      POINTER      :: vgrid_soil_sb             !< Vertical grid
    INTEGER                              :: ncanopy                   !< number of canopy layers
    INTEGER                              :: nsoil_sb                  !< number of soil layers as used/defined by the SB_ process
    LOGICAL                              :: l_land_plant_canopy_out,& !< write land/plant/canopy output T/F
                                            l_soil_output             !< write soil       output T/F
    INTEGER                              :: lun                       ! logical unit number
    INTEGER                              :: icanopy                   ! loop over ncanopy
    REAL(wp)                             :: leaf_mass_to_root_mass    ! veg_diagnostics output
    REAL(wp)                             :: dC13leaf,dC13fine_root, & ! 
                                            dC13sap_wood,dC13heart_wood,dC13coarse_root, & ! delta C13 in pools 
                                            dC13fruit,dC13labile,dC13reserve,dC13seed_bed
    REAL(wp),ALLOCATABLE, DIMENSION(:,:) :: dC13soluable_litter,dC13polymeric_litter,dC13woody_litter, & 
                                            dC13dom,dC13dom_assoc,dC13fungi,dC13mycorrhiza,dC13microbial, & 
                                            dC13residue,dC13residue_assoc
    REAL(wp),ALLOCATABLE, DIMENSION(:,:) :: hlp1_2d
    INTEGER                              :: iblk, ics, ice, nc, ic, is
    CHARACTER(len=*), PARAMETER :: routine = TRIM(modname)//':write_output_pool'
    ! ----------------------------------------------------------------------------------------------------- !
    dsl4jsb_Def_mt2L2D :: veg_pool_mt
    dsl4jsb_Def_mt1L2D :: veg_total_biomass_mt
    dsl4jsb_Def_mt2L3D :: sb_pool_mt
    ! ----------------------------------------------------------------------------------------------------- !
    dsl4jsb_Def_config(SB_)
    dsl4jsb_Def_memory(Q_ASSIMI_)
    dsl4jsb_Def_memory(VEG_)
    dsl4jsb_Def_memory(Q_PHENO_)
    dsl4jsb_Def_memory(SPQ_)
    dsl4jsb_Def_memory(SB_)
    ! ----------------------------------------------------------------------------------------------------- !
    ! Q_PHENO_
    dsl4jsb_Real2D_onChunk      :: growing_season
    ! Q_ASSIMI_ 2D
    dsl4jsb_Real2D_onChunk      :: beta_soil_gs
    dsl4jsb_Real2D_onChunk      :: beta_soil_ps
    ! Q_ASSIMI_ 2D
    dsl4jsb_Real3D_onChunk      :: jmax_cl
    dsl4jsb_Real3D_onChunk      :: vcmax_cl
    dsl4jsb_Real3D_onChunk      :: chlfl_yield_cl 
    ! SPQ_ 2D
    dsl4jsb_Real2D_onChunk      :: snow_height
    dsl4jsb_Real2D_onChunk      :: root_depth
    dsl4jsb_Real2D_onChunk      :: w_soil_root 
    dsl4jsb_Real2D_onChunk      :: w_soil_root_pot
    dsl4jsb_Real2D_onChunk      :: num_sl_above_bedrock
    ! SPQ_ 3D
    dsl4jsb_Real3D_onChunk      :: soil_depth_sl
    dsl4jsb_Real3D_onChunk      :: t_soil_sl
    dsl4jsb_Real3D_onChunk      :: w_soil_sl
    dsl4jsb_Real3D_onChunk      :: bulk_dens_sl
    dsl4jsb_Real3D_onChunk      :: volume_min_sl
    dsl4jsb_Real3D_onChunk      :: percolation_sl
    ! VEG_ 2D
    dsl4jsb_Real2D_onChunk      :: height
    dsl4jsb_Real2D_onChunk      :: lai
    dsl4jsb_Real2D_onChunk      :: diameter
    dsl4jsb_Real2D_onChunk      :: dens_ind
    dsl4jsb_Real2D_onChunk      :: beta_sinklim_ps_tacclim_mavg
    dsl4jsb_Real2D_onChunk      :: demand_uptake_n_tuptake_mavg
    dsl4jsb_Real2D_onChunk      :: demand_uptake_p_tuptake_mavg
    dsl4jsb_Real2D_onChunk      :: growth_req_n_tuptake_mavg
    dsl4jsb_Real2D_onChunk      :: growth_req_p_tuptake_mavg
        !kschufft
    dsl4jsb_Real2D_onChunk      :: beta_exudation_c
    dsl4jsb_Real2D_onChunk      :: beta_exudation_n
    dsl4jsb_Real2D_onChunk      :: beta_exudation_p
    dsl4jsb_Real2D_onChunk      :: beta_exudation
    dsl4jsb_Real2D_onChunk      :: beta_exudation_mavg
    ! VEG_ 3D
    dsl4jsb_Real3D_onChunk      :: fn_et_cl
    dsl4jsb_Real3D_onChunk      :: fn_rub_cl
    dsl4jsb_Real3D_onChunk      :: fn_chl_cl
    dsl4jsb_Real3D_onChunk      :: fn_pepc_cl
    dsl4jsb_Real3D_onChunk      :: fn_oth_cl
    dsl4jsb_Real3D_onChunk      :: leaf_nitrogen_cl
    dsl4jsb_Real3D_onChunk      :: root_fraction_sl
    ! SB_ 3D
    dsl4jsb_Real3D_onChunk      :: nh4_solute
    dsl4jsb_Real3D_onChunk      :: nh4_n15_solute
    dsl4jsb_Real3D_onChunk      :: no3_solute
    dsl4jsb_Real3D_onChunk      :: no3_n15_solute
    dsl4jsb_Real3D_onChunk      :: po4_solute
    dsl4jsb_Real3D_onChunk      :: noy
    dsl4jsb_Real3D_onChunk      :: noy_n15
    dsl4jsb_Real3D_onChunk      :: n2o
    dsl4jsb_Real3D_onChunk      :: n2o_n15
    dsl4jsb_Real3D_onChunk      :: n2
    dsl4jsb_Real3D_onChunk      :: n2_n15
    dsl4jsb_Real3D_onChunk      :: nh4_assoc
    dsl4jsb_Real3D_onChunk      :: nh4_n15_assoc
    dsl4jsb_Real3D_onChunk      :: po4_assoc_fast
    dsl4jsb_Real3D_onChunk      :: po4_assoc_slow
    dsl4jsb_Real3D_onChunk      :: po4_occluded
    dsl4jsb_Real3D_onChunk      :: po4_primary
    dsl4jsb_Real3D_onChunk      :: qmax_org
    dsl4jsb_Real3D_onChunk      :: qmax_po4
    dsl4jsb_Real3D_onChunk      :: km_adsorpt_po4_sl
    dsl4jsb_Real3D_onChunk      :: bulk_dens_corr_sl
    dsl4jsb_Real3D_onChunk      :: microbial_cue_eff_tmic_mavg 
    dsl4jsb_Real3D_onChunk      :: microbial_nue_eff_tmic_mavg
    dsl4jsb_Real3D_onChunk      :: microbial_pue_eff_tmic_mavg
    dsl4jsb_Real3D_onChunk      :: dom_cn 
    dsl4jsb_Real3D_onChunk      :: dom_cp
    dsl4jsb_Real3D_onChunk      :: dom_cn_mavg 
    dsl4jsb_Real3D_onChunk      :: dom_cp_mavg 
    dsl4jsb_Real3D_onChunk      :: enzyme_frac_AP
    dsl4jsb_Real3D_onChunk      :: enzyme_frac_poly
    dsl4jsb_Real3D_onChunk      :: enzyme_frac_residue
    dsl4jsb_Real3D_onChunk      :: enzyme_frac_poly_c
    dsl4jsb_Real3D_onChunk      :: enzyme_frac_poly_n
    dsl4jsb_Real3D_onChunk      :: enzyme_frac_poly_p
    dsl4jsb_Real3D_onChunk      :: enzyme_frac_poly_c_mavg
    dsl4jsb_Real3D_onChunk      :: enzyme_frac_poly_n_mavg
    dsl4jsb_Real3D_onChunk      :: enzyme_frac_poly_p_mavg
    dsl4jsb_Real3D_onChunk      :: limitation_status_diagnostics
    dsl4jsb_Real3D_onChunk      :: fact_n_status_mic_c_growth
    dsl4jsb_Real3D_onChunk      :: fact_p_status_mic_c_growth
    ! ----------------------------------------------------------------------------------------------------- !
    iblk      = options%iblk
    ics       = options%ics
    ice       = options%ice
    nc        = options%nc
    ! ----------------------------------------------------------------------------------------------------- !
    IF (debug_on() .AND. iblk == 1) CALL message(TRIM(routine), 'Starting on tile '//TRIM(tile%name)//' ...')
    ! ----------------------------------------------------------------------------------------------------- !
    model         => Get_model(tile%owner_model_id)
    lctlib        => model%lctlib(tile%lcts(1)%lib_id)
    vgrid_canopy  => Get_vgrid('q_canopy_layer')
    ncanopy       =  vgrid_canopy%n_levels
    vgrid_soil_sb => Get_vgrid('soil_layer_sb')
    nsoil_sb      =  vgrid_soil_sb%n_levels
    ! ----------------------------------------------------------------------------------------------------- !
    dsl4jsb_Get_config(SB_)
    dsl4jsb_Get_memory(Q_ASSIMI_)
    dsl4jsb_Get_memory(VEG_)
    dsl4jsb_Get_memory(Q_PHENO_)
    dsl4jsb_Get_memory(SPQ_)
    dsl4jsb_Get_memory(SB_)
    ! ----------------------------------------------------------------------------------------------------- !
    ALLOCATE(dC13soluable_litter(nc, nsoil_sb))
    ALLOCATE(dC13polymeric_litter(nc, nsoil_sb))
    ALLOCATE(dC13woody_litter(nc, nsoil_sb))
    ALLOCATE(dC13dom(nc, nsoil_sb))
    ALLOCATE(dC13dom_assoc(nc, nsoil_sb))
    ALLOCATE(dC13fungi(nc, nsoil_sb))
    ALLOCATE(dC13mycorrhiza(nc, nsoil_sb))
    ALLOCATE(dC13microbial(nc, nsoil_sb))
    ALLOCATE(dC13residue(nc, nsoil_sb))
    ALLOCATE(dC13residue_assoc(nc, nsoil_sb))
    ALLOCATE(hlp1_2d(nc, nsoil_sb))
    ! ----------------------------------------------------------------------------------------------------- !
    bgcm_store => tile%bgcm_store
    dsl4jsb_Get_mt2L2D(VEG_BGCM_POOL_ID, veg_pool_mt)
    dsl4jsb_Get_mt1L2D(VEG_BGCM_TOTAL_BIO_ID, veg_total_biomass_mt)
    dsl4jsb_Get_mt2L3D(SB_BGCM_POOL_ID, sb_pool_mt)
    ! ----------------------------------------------------------------------------------------------------- !
    dsl4jsb_Get_var2D_onChunk(Q_PHENO_, growing_season)
    ! ---------------------------
    dsl4jsb_Get_var2D_onChunk(Q_ASSIMI_, beta_soil_gs)
    dsl4jsb_Get_var2D_onChunk(Q_ASSIMI_, beta_soil_ps)
    dsl4jsb_Get_var3D_onChunk(Q_ASSIMI_, jmax_cl)
    dsl4jsb_Get_var3D_onChunk(Q_ASSIMI_, vcmax_cl)
    dsl4jsb_Get_var3D_onChunk(Q_ASSIMI_, chlfl_yield_cl)
    ! ---------------------------
    dsl4jsb_Get_var2D_onChunk(SPQ_, root_depth)
    dsl4jsb_Get_var2D_onChunk(SPQ_, snow_height)
    dsl4jsb_Get_var2D_onChunk(SPQ_, w_soil_root)
    dsl4jsb_Get_var2D_onChunk(SPQ_, w_soil_root_pot)
    dsl4jsb_Get_var2D_onChunk(SPQ_, num_sl_above_bedrock)
    ! SPQ_ 3D
    dsl4jsb_Get_var3D_onChunk(SPQ_, soil_depth_sl)
    dsl4jsb_Get_var3D_onChunk(SPQ_, t_soil_sl)
    dsl4jsb_Get_var3D_onChunk(SPQ_, w_soil_sl)
    dsl4jsb_Get_var3D_onChunk(SPQ_, bulk_dens_sl)
    dsl4jsb_Get_var3D_onChunk(SPQ_, volume_min_sl)
    dsl4jsb_Get_var3D_onChunk(SPQ_, percolation_sl)
    ! ---------------------------
    dsl4jsb_Get_var2D_onChunk(VEG_, height)
    dsl4jsb_Get_var2D_onChunk(VEG_, lai)
    dsl4jsb_Get_var2D_onChunk(VEG_, diameter)
    dsl4jsb_Get_var2D_onChunk(VEG_, dens_ind)
    dsl4jsb_Get_var2D_onChunk(VEG_, beta_sinklim_ps_tacclim_mavg)
    dsl4jsb_Get_var2D_onChunk(VEG_, demand_uptake_n_tuptake_mavg)
    dsl4jsb_Get_var2D_onChunk(VEG_, demand_uptake_p_tuptake_mavg)
    dsl4jsb_Get_var2D_onChunk(VEG_, growth_req_n_tuptake_mavg)
    dsl4jsb_Get_var2D_onChunk(VEG_, growth_req_p_tuptake_mavg)
    dsl4jsb_Get_var3D_onChunk(VEG_, fn_et_cl)
    dsl4jsb_Get_var3D_onChunk(VEG_, fn_rub_cl)
    dsl4jsb_Get_var3D_onChunk(VEG_, fn_chl_cl)
    dsl4jsb_Get_var3D_onChunk(VEG_, fn_pepc_cl)
    dsl4jsb_Get_var3D_onChunk(VEG_, fn_oth_cl)
    dsl4jsb_Get_var3D_onChunk(VEG_, leaf_nitrogen_cl)
    dsl4jsb_Get_var3D_onChunk(VEG_, root_fraction_sl)
        !kschufft
    dsl4jsb_Get_var2D_onChunk(VEG_, beta_exudation_c)
    dsl4jsb_Get_var2D_onChunk(VEG_, beta_exudation_n)
    dsl4jsb_Get_var2D_onChunk(VEG_, beta_exudation_p)
    dsl4jsb_Get_var2D_onChunk(VEG_, beta_exudation)
    dsl4jsb_Get_var2D_onChunk(VEG_, beta_exudation_mavg)
    ! ---------------------------
    dsl4jsb_Get_var3D_onChunk(SB_,  nh4_solute)
    dsl4jsb_Get_var3D_onChunk(SB_,  nh4_n15_solute)
    dsl4jsb_Get_var3D_onChunk(SB_,  no3_solute)
    dsl4jsb_Get_var3D_onChunk(SB_,  no3_n15_solute)
    dsl4jsb_Get_var3D_onChunk(SB_,  po4_solute)
    dsl4jsb_Get_var3D_onChunk(SB_,  noy)
    dsl4jsb_Get_var3D_onChunk(SB_,  noy_n15)
    dsl4jsb_Get_var3D_onChunk(SB_,  n2o)
    dsl4jsb_Get_var3D_onChunk(SB_,  n2o_n15)
    dsl4jsb_Get_var3D_onChunk(SB_,  n2)
    dsl4jsb_Get_var3D_onChunk(SB_,  n2_n15)
    dsl4jsb_Get_var3D_onChunk(SB_,  nh4_assoc)
    dsl4jsb_Get_var3D_onChunk(SB_,  nh4_n15_assoc)
    dsl4jsb_Get_var3D_onChunk(SB_,  po4_assoc_fast)
    dsl4jsb_Get_var3D_onChunk(SB_,  po4_assoc_slow)
    dsl4jsb_Get_var3D_onChunk(SB_,  po4_occluded)
    dsl4jsb_Get_var3D_onChunk(SB_,  po4_primary)
    dsl4jsb_Get_var3D_onChunk(SB_,  qmax_org)
    dsl4jsb_Get_var3D_onChunk(SB_,  qmax_po4)
    dsl4jsb_Get_var3D_onChunk(SB_,  km_adsorpt_po4_sl)
    dsl4jsb_Get_var3D_onChunk(SB_,  bulk_dens_corr_sl)
    dsl4jsb_Get_var3D_onChunk(SB_,  microbial_cue_eff_tmic_mavg)
    dsl4jsb_Get_var3D_onChunk(SB_,  microbial_nue_eff_tmic_mavg)
    dsl4jsb_Get_var3D_onChunk(SB_,  microbial_pue_eff_tmic_mavg)
    dsl4jsb_Get_var3D_onChunk(SB_,  dom_cn)
    dsl4jsb_Get_var3D_onChunk(SB_,  dom_cp)
    dsl4jsb_Get_var3D_onChunk(SB_,  dom_cn_mavg)
    dsl4jsb_Get_var3D_onChunk(SB_,  dom_cp_mavg)
    dsl4jsb_Get_var3D_onChunk(SB_,  enzyme_frac_AP)
    dsl4jsb_Get_var3D_onChunk(SB_,  enzyme_frac_poly)
    dsl4jsb_Get_var3D_onChunk(SB_,  enzyme_frac_residue)
    dsl4jsb_Get_var3D_onChunk(SB_,  enzyme_frac_poly_c)
    dsl4jsb_Get_var3D_onChunk(SB_,  enzyme_frac_poly_n)
    dsl4jsb_Get_var3D_onChunk(SB_,  enzyme_frac_poly_p)
    dsl4jsb_Get_var3D_onChunk(SB_,  enzyme_frac_poly_c_mavg)
    dsl4jsb_Get_var3D_onChunk(SB_,  enzyme_frac_poly_n_mavg)
    dsl4jsb_Get_var3D_onChunk(SB_,  enzyme_frac_poly_p_mavg)
    dsl4jsb_Get_var3D_onChunk(SB_,  limitation_status_diagnostics)
    dsl4jsb_Get_var3D_onChunk(SB_,  fact_n_status_mic_c_growth)
    dsl4jsb_Get_var3D_onChunk(SB_,  fact_p_status_mic_c_growth)
    ! ----------------------------------------------------------------------------------------------------- !

    !> 0.8 the point in ics:ice of which data will be written to file; per default = 1
    !>
    !> is actually defined and declared globally in his module above, but ifort complains anyway
    !>
    ic = 1

    !>0.9 init local variables
    !>
    lun = lun_ascii_pools_start

    !>1.0 test if anything would be written out at this timestep
    !> 
    ! set to FALSE as default
    l_land_plant_canopy_out = .FALSE.
    l_soil_output           = .FALSE.
    ! set TRUE according to the model that is used
    SELECT CASE(model%config%qmodel_id)
      CASE(QLAND, QPLANT, QCANOPY)
        l_land_plant_canopy_out = .TRUE.
      CASE(QSOIL)
        l_soil_output           = .TRUE.
    END SELECT


    !>2.0 output
    !>
    ! land/plant
    ! land/plant/canopy
    ! land/soil
    SELECT CASE(model%config%qmodel_id)
      CASE(QLAND,QPLANT)
        ! output T/F
        IF (l_land_plant_canopy_out) THEN 
              ! vegpoolC
              write(lun,'(13F12.6)') time_d,veg_pool_mt(ix_leaf, ixC, ic),&
                veg_pool_mt(ix_fine_root, ixC, ic),veg_pool_mt(ix_sap_wood, ixC, ic),&
                veg_pool_mt(ix_heart_wood, ixC, ic),veg_pool_mt(ix_coarse_root, ixC, ic),&
                veg_pool_mt(ix_fruit, ixC, ic),veg_pool_mt(ix_labile, ixC, ic),&
                veg_pool_mt(ix_reserve, ixC, ic),veg_pool_mt(ix_seed_bed, ixC, ic),&
                veg_total_biomass_mt(ixC, ic),veg_total_biomass_mt(ixC13, ic),veg_total_biomass_mt(ixC14, ic)
       
              dC13leaf        = calc_delta_C13(veg_pool_mt(ix_leaf, ixC, ic),veg_pool_mt(ix_leaf, ixC13, ic))
              dC13fine_root   = calc_delta_C13(veg_pool_mt(ix_fine_root, ixC, ic),veg_pool_mt(ix_fine_root, ixC13, ic))
              dC13sap_wood    = calc_delta_C13(veg_pool_mt(ix_sap_wood, ixC, ic),veg_pool_mt(ix_sap_wood, ixC13, ic))
              dC13heart_wood  = calc_delta_C13(veg_pool_mt(ix_heart_wood, ixC, ic),veg_pool_mt(ix_heart_wood, ixC13, ic))
              dC13coarse_root = calc_delta_C13(veg_pool_mt(ix_coarse_root, ixC, ic),veg_pool_mt(ix_coarse_root, ixC13, ic))
              dC13fruit       = calc_delta_C13(veg_pool_mt(ix_fruit, ixC, ic),veg_pool_mt(ix_fruit, ixC13, ic))
              dC13labile      = calc_delta_C13(veg_pool_mt(ix_labile, ixC, ic),veg_pool_mt(ix_labile, ixC13, ic))
              dC13reserve     = calc_delta_C13(veg_pool_mt(ix_reserve, ixC, ic),veg_pool_mt(ix_reserve, ixC13, ic))
              dC13seed_bed    = calc_delta_C13(veg_pool_mt(ix_seed_bed, ixC, ic),veg_pool_mt(ix_seed_bed, ixC13, ic))
              lun=lun+1   ! increase lun by 1 after each write statement
              ! vegpooldC13DC14
              write(lun,'(19F18.6)') time_d,dC13leaf,dC13fine_root,dC13sap_wood,dC13heart_wood,&
                dC13coarse_root,dC13fruit,dC13labile,dC13reserve,dC13seed_bed, & 
                calc_delta_C14(veg_pool_mt(ix_leaf, ixC, ic),veg_pool_mt(ix_leaf, ixC14, ic),dC13leaf),&
                calc_delta_C14(veg_pool_mt(ix_fine_root, ixC, ic),veg_pool_mt(ix_fine_root, ixC14, ic),dC13fine_root),&
                calc_delta_C14(veg_pool_mt(ix_sap_wood, ixC, ic),veg_pool_mt(ix_sap_wood, ixC14, ic),dC13sap_wood),&
                calc_delta_C14(veg_pool_mt(ix_heart_wood, ixC, ic),veg_pool_mt(ix_heart_wood, ixC14, ic),dC13heart_wood),&
                calc_delta_C14(veg_pool_mt(ix_coarse_root, ixC, ic),veg_pool_mt(ix_coarse_root, ixC14, ic),dC13coarse_root),&
                calc_delta_C14(veg_pool_mt(ix_fruit, ixC, ic),veg_pool_mt(ix_fruit, ixC14, ic),dC13fruit),&
                calc_delta_C14(veg_pool_mt(ix_labile, ixC, ic),veg_pool_mt(ix_labile, ixC14, ic),dC13labile),&
                calc_delta_C14(veg_pool_mt(ix_reserve, ixC, ic),veg_pool_mt(ix_reserve, ixC14, ic),dC13reserve),&
                calc_delta_C14(veg_pool_mt(ix_seed_bed, ixC, ic),veg_pool_mt(ix_seed_bed, ixC14, ic),dC13seed_bed)
              lun=lun+1
              ! vegpoolN
              write(lun,'(12F12.6)') time_d,veg_pool_mt(ix_leaf, ixN, ic),&
                veg_pool_mt(ix_fine_root, ixN, ic),veg_pool_mt(ix_sap_wood, ixN, ic),&
                veg_pool_mt(ix_heart_wood, ixN, ic),veg_pool_mt(ix_coarse_root, ixN, ic),&
                veg_pool_mt(ix_fruit, ixN, ic),veg_pool_mt(ix_labile, ixN, ic),&
                veg_pool_mt(ix_reserve, ixN, ic),veg_pool_mt(ix_seed_bed, ixN, ic),&
                veg_total_biomass_mt(ixN, ic)
              lun=lun+1
              ! vegpoolP
              write(lun,'(11F12.6)') time_d,veg_pool_mt(ix_leaf, ixP, ic),&
                veg_pool_mt(ix_fine_root, ixP, ic),veg_pool_mt(ix_sap_wood, ixP, ic),&
                veg_pool_mt(ix_heart_wood, ixP, ic),veg_pool_mt(ix_coarse_root, ixP, ic),&
                veg_pool_mt(ix_fruit, ixP, ic),veg_pool_mt(ix_labile, ixP, ic),&
                veg_pool_mt(ix_reserve, ixP, ic),veg_pool_mt(ix_seed_bed, ixP, ic),& 
                veg_total_biomass_mt(ixP, ic)
              lun=lun+1
              ! vegpooldN15
                write(lun,'(12F14.6)') time_d, &
                calc_delta_N15(veg_pool_mt(ix_leaf, ixN, ic),veg_pool_mt(ix_leaf, ixN15, ic)), &
                calc_delta_N15(veg_pool_mt(ix_fine_root, ixN, ic),veg_pool_mt(ix_fine_root, ixN15, ic)), &
                calc_delta_N15(veg_pool_mt(ix_sap_wood, ixN, ic),veg_pool_mt(ix_sap_wood, ixN15, ic)), &
                calc_delta_N15(veg_pool_mt(ix_heart_wood, ixN, ic),veg_pool_mt(ix_heart_wood, ixN15, ic)), &
                calc_delta_N15(veg_pool_mt(ix_coarse_root, ixN, ic),veg_pool_mt(ix_coarse_root, ixN15, ic)), &
                calc_delta_N15(veg_pool_mt(ix_fruit, ixN, ic),veg_pool_mt(ix_fruit, ixN15, ic)), &
                calc_delta_N15(veg_pool_mt(ix_labile, ixN, ic),veg_pool_mt(ix_labile, ixN15, ic)), &
                calc_delta_N15(veg_pool_mt(ix_reserve, ixN, ic),veg_pool_mt(ix_reserve, ixN15, ic)), &
                calc_delta_N15(veg_pool_mt(ix_seed_bed, ixN, ic),veg_pool_mt(ix_seed_bed, ixN15, ic)), &
                calc_delta_N15(veg_total_biomass_mt(ixN, ic),veg_total_biomass_mt(ixN15, ic))
              lun=lun+1
              ! soilpoolPhysics
              write(lun,'(20F16.6)') time_d, &
                                     snow_height(ic), w_soil_root(ic),  w_soil_root_pot(ic)
              lun=lun+1
              ! soilpoolPhysics_lay (max of 20 soil layers)
              write(lun,'(41F16.6)') time_d, w_soil_sl(ic,:), t_soil_sl(ic,:)
              lun=lun+1
              ! soilpoolInOrg
              write(lun,'(30F16.6)') time_d, &
                                     SUM(nh4_assoc(ic,:)*soil_depth_sl(ic,:))                       , & 
                                     SUM(nh4_solute(ic,:)*soil_depth_sl(ic,:))                       , & 
                                     SUM(no3_solute(ic,:)*soil_depth_sl(ic,:))                       , & 
                                     SUM(po4_solute(ic,:)*soil_depth_sl(ic,:))                       , &
                                     SUM(noy(ic,:)*soil_depth_sl(ic,:))                              , &
                                     SUM(n2o(ic,:)*soil_depth_sl(ic,:))                              , &
                                     SUM(n2(ic,:)*soil_depth_sl(ic,:))  
              lun=lun+1
              ! ecopoolCNP
              write(lun,'(30F16.6)') time_d, &
                                     SUM(sb_pool_mt(ix_soluable_litter, ixC, ic, :)*soil_depth_sl(ic,:))     + & 
                                      SUM(sb_pool_mt(ix_polymeric_litter, ixC, ic, :)*soil_depth_sl(ic,:))     + & 
                                      SUM(sb_pool_mt(ix_woody_litter, ixC, ic, :)*soil_depth_sl(ic,:))         + &
                                      SUM(sb_pool_mt(ix_microbial, ixC, ic, :)*soil_depth_sl(ic,:))            + &
                                      SUM(sb_pool_mt(ix_residue, ixC, ic, :)*soil_depth_sl(ic,:))              + & 
                                      SUM(sb_pool_mt(ix_mycorrhiza, ixC, ic, :)*soil_depth_sl(ic,:))           , &
                                     SUM(sb_pool_mt(ix_soluable_litter, ixN, ic, :)*soil_depth_sl(ic,:))   + & 
                                      SUM(sb_pool_mt(ix_polymeric_litter, ixN, ic, :)*soil_depth_sl(ic,:))   + & 
                                      SUM(sb_pool_mt(ix_woody_litter, ixN, ic, :)*soil_depth_sl(ic,:))       + &
                                      SUM(sb_pool_mt(ix_microbial, ixN, ic, :)*soil_depth_sl(ic,:))          + &
                                      SUM(sb_pool_mt(ix_residue, ixN, ic, :)*soil_depth_sl(ic,:))            + & 
                                      SUM(sb_pool_mt(ix_mycorrhiza, ixN, ic, :)*soil_depth_sl(ic,:))         , &
                                     SUM(nh4_solute(ic,:)*soil_depth_sl(ic,:))                                + & 
                                     SUM(nh4_assoc(ic,:)*soil_depth_sl(ic,:))                                + & 
                                      SUM(no3_solute(ic,:)*soil_depth_sl(ic,:))                                + & 
                                      SUM(noy(ic,:)*soil_depth_sl(ic,:))                                       + &
                                      SUM(n2o(ic,:)*soil_depth_sl(ic,:))                                       + &
                                      SUM(n2(ic,:)*soil_depth_sl(ic,:))                                        , &     
                                     SUM(sb_pool_mt(ix_soluable_litter, ixC, ic, :)*soil_depth_sl(ic,:))     + & 
                                      SUM(sb_pool_mt(ix_polymeric_litter, ixC, ic, :)*soil_depth_sl(ic,:))     + & 
                                      SUM(sb_pool_mt(ix_woody_litter, ixC, ic, :)*soil_depth_sl(ic,:))         + &
                                      SUM(sb_pool_mt(ix_microbial, ixC, ic, :)*soil_depth_sl(ic,:))            + &
                                      SUM(sb_pool_mt(ix_residue, ixC, ic, :)*soil_depth_sl(ic,:))              + & 
                                      SUM(sb_pool_mt(ix_mycorrhiza, ixC, ic, :)*soil_depth_sl(ic,:))           + &
                                      veg_total_biomass_mt(ixC, ic)                                         , &
                                     SUM(sb_pool_mt(ix_soluable_litter, ixN, ic, :)*soil_depth_sl(ic,:))   + & 
                                      SUM(sb_pool_mt(ix_polymeric_litter, ixN, ic, :)*soil_depth_sl(ic,:))   + & 
                                      SUM(sb_pool_mt(ix_woody_litter, ixN, ic, :)*soil_depth_sl(ic,:))       + &
                                      SUM(sb_pool_mt(ix_microbial, ixN, ic, :)*soil_depth_sl(ic,:))          + &
                                      SUM(sb_pool_mt(ix_residue, ixN, ic, :)*soil_depth_sl(ic,:))            + & 
                                      SUM(sb_pool_mt(ix_mycorrhiza, ixN, ic, :)*soil_depth_sl(ic,:))         + &
                                      SUM(nh4_solute(ic,:)*soil_depth_sl(ic,:))                                + & 
                                      SUM(nh4_assoc(ic,:)*soil_depth_sl(ic,:))                                + & 
                                      SUM(no3_solute(ic,:)*soil_depth_sl(ic,:))                                + &
                                      SUM(noy(ic,:)*soil_depth_sl(ic,:))                                       + &
                                      SUM(n2o(ic,:)*soil_depth_sl(ic,:))                                       + &
                                      SUM(n2(ic,:)*soil_depth_sl(ic,:))                                        + &     
                                      veg_total_biomass_mt(ixN, ic)                                       , &
                                     SUM(sb_pool_mt(ix_soluable_litter, ixP, ic, :)*soil_depth_sl(ic,:))   + & 
                                      SUM(sb_pool_mt(ix_polymeric_litter, ixP, ic, :)*soil_depth_sl(ic,:))   + & 
                                      SUM(sb_pool_mt(ix_woody_litter, ixP, ic, :)*soil_depth_sl(ic,:))       + &
                                      SUM(sb_pool_mt(ix_microbial, ixP, ic, :)*soil_depth_sl(ic,:))          + &
                                      SUM(sb_pool_mt(ix_residue, ixP, ic, :)*soil_depth_sl(ic,:))            + & 
                                      SUM(sb_pool_mt(ix_mycorrhiza, ixP, ic, :)*soil_depth_sl(ic,:))         + &
                                      SUM(po4_solute(ic,:)*soil_depth_sl(ic,:))                            + & 
                                      SUM(po4_assoc_fast(ic,:)*soil_depth_sl(ic,:))                        + &
                                      SUM(po4_assoc_slow(ic,:)*soil_depth_sl(ic,:))                        + &
                                      SUM(po4_occluded(ic,:)*soil_depth_sl(ic,:))                          + &
                                      SUM(po4_primary(ic,:)*soil_depth_sl(ic,:))                           + &
                                      veg_total_biomass_mt(ixP, ic)                                     , &
                                     SUM(sb_pool_mt(ix_soluable_litter, ixN15, ic, :)*soil_depth_sl(ic,:))   + & 
                                      SUM(sb_pool_mt(ix_polymeric_litter, ixN15, ic, :)*soil_depth_sl(ic,:))   + & 
                                      SUM(sb_pool_mt(ix_woody_litter, ixN15, ic, :)*soil_depth_sl(ic,:))       + &
                                      SUM(sb_pool_mt(ix_microbial, ixN15, ic, :)*soil_depth_sl(ic,:))          + &
                                      SUM(sb_pool_mt(ix_residue, ixN15, ic, :)*soil_depth_sl(ic,:))            + & 
                                      SUM(sb_pool_mt(ix_mycorrhiza, ixN15, ic, :)*soil_depth_sl(ic,:))         + &
                                      SUM(nh4_n15_assoc(ic,:)*soil_depth_sl(ic,:))                                + & 
                                      SUM(nh4_n15_solute(ic,:)*soil_depth_sl(ic,:))                                + & 
                                      SUM(no3_n15_solute(ic,:)*soil_depth_sl(ic,:))                                + &
                                      SUM(noy_n15(ic,:)*soil_depth_sl(ic,:))                                       + &
                                      SUM(n2o_n15(ic,:)*soil_depth_sl(ic,:))                                       + &
                                      SUM(n2_n15(ic,:)*soil_depth_sl(ic,:))                                        + &     
                                      veg_total_biomass_mt(ixN15, ic)
              lun=lun+1 
              ! soilpooldC13DC14dN15
              dC13soluable_litter(ic,1)  = calc_delta_C13(SUM(sb_pool_mt(ix_soluable_litter, ixC, ic, :)*&
                                              soil_depth_sl(ic,:)), &
                                              SUM(sb_pool_mt(ix_soluable_litter, ixC13, ic, :)*soil_depth_sl(ic,:)))
              dC13polymeric_litter(ic,1) = calc_delta_C13(SUM(sb_pool_mt(ix_polymeric_litter, ixC, ic, :)*&
                                              soil_depth_sl(ic,:)), &
                                              SUM(sb_pool_mt(ix_polymeric_litter, ixC13, ic, :)*soil_depth_sl(ic,:)))
              dC13woody_litter(ic,1)     = calc_delta_C13(SUM(sb_pool_mt(ix_woody_litter, ixC, ic, :)*&
                                              soil_depth_sl(ic,:)), &
                                              SUM(sb_pool_mt(ix_woody_litter, ixC13, ic, :)*soil_depth_sl(ic,:)))
              dC13microbial(ic,1)        = calc_delta_C13(SUM(sb_pool_mt(ix_microbial, ixC, ic, :)*&
                                              soil_depth_sl(ic,:)), &
                                              SUM(sb_pool_mt(ix_microbial, ixC13, ic, :)*soil_depth_sl(ic,:)))
              dC13residue(ic,1)          = calc_delta_C13(SUM(sb_pool_mt(ix_residue, ixC, ic, :)*&
                                              soil_depth_sl(ic,:)), &
                                              SUM(sb_pool_mt(ix_residue, ixC13, ic, :)*soil_depth_sl(ic,:)))
              dC13mycorrhiza(ic,1)       = calc_delta_C13(SUM(sb_pool_mt(ix_mycorrhiza, ixC, ic, :)*&
                                              soil_depth_sl(ic,:)), &
                                              SUM(sb_pool_mt(ix_mycorrhiza, ixC13, ic, :)*soil_depth_sl(ic,:)))
              write(lun,'(21F24.6)') time_d, &
                dC13soluable_litter(ic,1), &
                dC13polymeric_litter(ic,1), &
                dC13woody_litter(ic,1), &
                dC13microbial(ic,1), &
                dC13residue(ic,1), & 
                dC13mycorrhiza(ic,1), &
                calc_delta_C14(SUM(sb_pool_mt(ix_soluable_litter, ixC, ic, :)*soil_depth_sl(ic,:)), &
                               SUM(sb_pool_mt(ix_soluable_litter, ixC14, ic, :)*soil_depth_sl(ic,:)), &
                                    dC13soluable_litter(ic,1)) , &
                calc_delta_C14(SUM(sb_pool_mt(ix_polymeric_litter, ixC, ic, :)*soil_depth_sl(ic,:)), &
                               SUM(sb_pool_mt(ix_polymeric_litter, ixC14, ic, :)*soil_depth_sl(ic,:)), &
                                    dC13polymeric_litter(ic,1)), &
                calc_delta_C14(SUM(sb_pool_mt(ix_woody_litter, ixC, ic, :)*soil_depth_sl(ic,:)), &
                               SUM(sb_pool_mt(ix_woody_litter, ixC14, ic, :)*soil_depth_sl(ic,:)), &
                                    dC13woody_litter(ic,1))        , &
                calc_delta_C14(SUM(sb_pool_mt(ix_microbial, ixC, ic, :)*soil_depth_sl(ic,:)), &
                               SUM(sb_pool_mt(ix_microbial, ixC14, ic, :)*soil_depth_sl(ic,:)), dC13microbial(ic,1)), &
                calc_delta_C14(SUM(sb_pool_mt(ix_residue, ixC, ic, :)*soil_depth_sl(ic,:)), &
                               SUM(sb_pool_mt(ix_residue, ixC14, ic, :)*soil_depth_sl(ic,:)), dC13residue(ic,1)), &
                calc_delta_C14(SUM(sb_pool_mt(ix_mycorrhiza, ixC, ic, :)*soil_depth_sl(ic,:)), &
                               SUM(sb_pool_mt(ix_mycorrhiza, ixC14, ic, :)*soil_depth_sl(ic,:)),dC13mycorrhiza(ic,1)), &
                calc_delta_N15(SUM(sb_pool_mt(ix_soluable_litter, ixN, ic, :)*soil_depth_sl(ic,:)),&
                               SUM(sb_pool_mt(ix_soluable_litter, ixN15, ic, :)*soil_depth_sl(ic,:))), &
                calc_delta_N15(SUM(sb_pool_mt(ix_polymeric_litter, ixN, ic, :)*soil_depth_sl(ic,:)),&
                               SUM(sb_pool_mt(ix_polymeric_litter, ixN15, ic, :)*soil_depth_sl(ic,:))), &
                calc_delta_N15(SUM(sb_pool_mt(ix_woody_litter, ixN, ic, :)*soil_depth_sl(ic,:)), & 
                               SUM(sb_pool_mt(ix_woody_litter, ixN15, ic, :)*soil_depth_sl(ic,:))), &
                calc_delta_N15(SUM(sb_pool_mt(ix_microbial, ixN, ic, :)*soil_depth_sl(ic,:)), &
                               SUM(sb_pool_mt(ix_microbial, ixN15, ic, :)*soil_depth_sl(ic,:))), &
                calc_delta_N15(SUM(sb_pool_mt(ix_residue, ixN, ic, :)*soil_depth_sl(ic,:)), &
                               SUM(sb_pool_mt(ix_residue, ixN15, ic, :)*soil_depth_sl(ic,:))), &
                calc_delta_N15(SUM(nh4_assoc(ic,:)*soil_depth_sl(ic,:)), &
                               SUM(nh4_n15_assoc(ic,:)*soil_depth_sl(ic,:))), &
                calc_delta_N15(SUM(nh4_solute(ic,:)*soil_depth_sl(ic,:)), &
                               SUM(nh4_n15_solute(ic,:)*soil_depth_sl(ic,:))), &
                calc_delta_N15(SUM(no3_solute(ic,:)*soil_depth_sl(ic,:)), &
                               SUM(no3_n15_solute(ic,:)*soil_depth_sl(ic,:)))
              lun=lun+1
              ! veg_diagnostics
              IF (veg_pool_mt(ix_fine_root, ixC, ic) > eps8) THEN
                 leaf_mass_to_root_mass = veg_pool_mt(ix_leaf, ixC, ic) / veg_pool_mt(ix_fine_root, ixC, ic)
              ELSE
                 leaf_mass_to_root_mass = 0._wp
              ENDIF

              write(lun,'(20F24.8)') time_d, &
                height(ic), diameter(ic), lai(ic), dens_ind(ic), leaf_mass_to_root_mass, &
                growing_season(ic), demand_uptake_n_tuptake_mavg(ic),  demand_uptake_p_tuptake_mavg(ic), &
                beta_sinklim_ps_tacclim_mavg(ic), &
                growth_req_n_tuptake_mavg(ic), growth_req_p_tuptake_mavg(ic), &
                beta_soil_gs(ic), beta_soil_ps(ic),&
                beta_exudation_c(ic), beta_exudation_n(ic), beta_exudation_p(ic), beta_exudation(ic), &
                beta_exudation_mavg(ic)
              lun=lun+1
           END IF  ! write output T/F (l_land_plant_canopy_out)
    END SELECT  ! select land/plant & canopy


    !! land & plant & canopy
    !! 
    SELECT CASE(model%config%qmodel_id)
      CASE(QLAND, QPLANT, QCANOPY)
        ! output T/F
        IF (l_land_plant_canopy_out) THEN 
          ! veg_canopy 
          DO icanopy = 1,ncanopy
            write(lun,'(11F12.6)') time_d, REAL(icanopy), fn_et_cl(ic,icanopy), &
                                  fn_rub_cl(ic,icanopy), fn_chl_cl(ic,icanopy),&
                                  fn_pepc_cl(ic,icanopy), fn_oth_cl(ic,icanopy),leaf_nitrogen_cl(ic,icanopy), &
                                  jmax_cl(ic,icanopy), vcmax_cl(ic,icanopy), chlfl_yield_cl(ic,icanopy)
          END DO
          lun=lun+1
        ENDIF
    END SELECT  ! select land/plant & canopy


    !! land & plant & soil
    !! 
    SELECT CASE(model%config%qmodel_id)
    CASE(QLAND, QPLANT, QSOIL)
      ! output T/F
      IF (l_land_plant_canopy_out .OR. l_soil_output) THEN 
        ! soilpoolOrg
        write(lun,'(31F24.8)') time_d, &
          ! C
          & SUM(sb_pool_mt(ix_dom, ixC, ic, :) * soil_depth_sl(ic,:)), &
          & SUM(sb_pool_mt(ix_dom_assoc, ixC, ic, :) * soil_depth_sl(ic,:)), &
          & SUM(sb_pool_mt(ix_soluable_litter, ixC, ic, :) * soil_depth_sl(ic,:)), &
          & SUM(sb_pool_mt(ix_polymeric_litter, ixC, ic, :) * soil_depth_sl(ic,:)), &
          & SUM(sb_pool_mt(ix_woody_litter, ixC, ic, :) * soil_depth_sl(ic,:)), &
          & SUM(sb_pool_mt(ix_fungi, ixC, ic, :) * soil_depth_sl(ic,:)), &
          & SUM(sb_pool_mt(ix_mycorrhiza, ixC, ic, :) * soil_depth_sl(ic,:)), &
          & SUM(sb_pool_mt(ix_microbial, ixC, ic, :) * soil_depth_sl(ic,:)), &
          & SUM(sb_pool_mt(ix_residue, ixC, ic, :) * soil_depth_sl(ic,:)), &
          & SUM(sb_pool_mt(ix_residue_assoc, ixC, ic, :) * soil_depth_sl(ic,:)), &
          ! N
          & SUM(sb_pool_mt(ix_dom, ixN, ic, :) * soil_depth_sl(ic,:)), &
          & SUM(sb_pool_mt(ix_dom_assoc, ixN, ic, :) * soil_depth_sl(ic,:)), &
          & SUM(sb_pool_mt(ix_soluable_litter, ixN, ic, :) * soil_depth_sl(ic,:)), &
          & SUM(sb_pool_mt(ix_polymeric_litter, ixN, ic, :) * soil_depth_sl(ic,:)), &
          & SUM(sb_pool_mt(ix_woody_litter, ixN, ic, :) * soil_depth_sl(ic,:)), &
          & SUM(sb_pool_mt(ix_fungi, ixN, ic, :) * soil_depth_sl(ic,:)), &
          & SUM(sb_pool_mt(ix_mycorrhiza, ixN, ic, :) * soil_depth_sl(ic,:)), &
          & SUM(sb_pool_mt(ix_microbial, ixN, ic, :) * soil_depth_sl(ic,:)), &
          & SUM(sb_pool_mt(ix_residue, ixN, ic, :) * soil_depth_sl(ic,:)), &
          & SUM(sb_pool_mt(ix_residue_assoc, ixN, ic, :) * soil_depth_sl(ic,:)), &
          ! P
          & SUM(sb_pool_mt(ix_dom, ixP, ic, :) * soil_depth_sl(ic,:)), &
          & SUM(sb_pool_mt(ix_dom_assoc, ixP, ic, :) * soil_depth_sl(ic,:)), &
          & SUM(sb_pool_mt(ix_soluable_litter, ixP, ic, :) * soil_depth_sl(ic,:)), &
          & SUM(sb_pool_mt(ix_polymeric_litter, ixP, ic, :) * soil_depth_sl(ic,:)), &
          & SUM(sb_pool_mt(ix_woody_litter, ixP, ic, :) * soil_depth_sl(ic,:)), &
          & SUM(sb_pool_mt(ix_fungi, ixP, ic, :) * soil_depth_sl(ic,:)), &
          & SUM(sb_pool_mt(ix_mycorrhiza, ixP, ic, :) * soil_depth_sl(ic,:)), &
          & SUM(sb_pool_mt(ix_microbial, ixP, ic, :) * soil_depth_sl(ic,:)), &
          & SUM(sb_pool_mt(ix_residue, ixP, ic, :) * soil_depth_sl(ic,:)), &
          & SUM(sb_pool_mt(ix_residue_assoc, ixP, ic, :) * soil_depth_sl(ic,:))
        lun=lun+1
      ENDIF
    END SELECT  ! select land/plant/soil


    !! land & soil
    !! detailed data per layer only for simple_1d and jsm (max of 20 soil layers)
    SELECT CASE(model%config%qmodel_id)
    CASE(QLAND, QSOIL)
      ! output T/F
      IF (l_land_plant_canopy_out .OR. l_soil_output) THEN 
        ! soilpoolCOrg_lay
        write(lun,'(201F16.6)') time_d,sb_pool_mt(ix_soluable_litter, ixC, ic, :),&
            sb_pool_mt(ix_polymeric_litter, ixC, ic, :),sb_pool_mt(ix_woody_litter, ixC, ic, :),&
            sb_pool_mt(ix_dom, ixC, ic, :),sb_pool_mt(ix_dom_assoc, ixC, ic, :),&
            sb_pool_mt(ix_fungi, ixC, ic, :),sb_pool_mt(ix_mycorrhiza, ixC, ic, :),&
            sb_pool_mt(ix_microbial, ixC, ic, :),sb_pool_mt(ix_residue, ixC, ic, :),&
            sb_pool_mt(ix_residue_assoc, ixC, ic, :)
        lun=lun+1
        ! soilpoolNOrg_lay
        write(lun,'(201F16.6)') time_d,sb_pool_mt(ix_soluable_litter, ixN, ic, :),&
            sb_pool_mt(ix_polymeric_litter, ixN, ic, :),sb_pool_mt(ix_woody_litter, ixN, ic, :),&
            sb_pool_mt(ix_dom, ixN, ic, :),sb_pool_mt(ix_dom_assoc, ixN, ic, :),&
            sb_pool_mt(ix_fungi, ixN, ic, :),sb_pool_mt(ix_mycorrhiza, ixN, ic, :),&
            sb_pool_mt(ix_microbial, ixN, ic, :),sb_pool_mt(ix_residue, ixN, ic, :),&
            sb_pool_mt(ix_residue_assoc, ixN, ic, :)
        lun=lun+1
        ! soilpoolPOrg_lay
        write(lun,'(201F16.6)') time_d,sb_pool_mt(ix_soluable_litter, ixP, ic, :),&
            sb_pool_mt(ix_polymeric_litter, ixP, ic, :),sb_pool_mt(ix_woody_litter, ixP, ic, :),&
            sb_pool_mt(ix_dom, ixP, ic, :),sb_pool_mt(ix_dom_assoc, ixP, ic, :),&
            sb_pool_mt(ix_fungi, ixP, ic, :),sb_pool_mt(ix_mycorrhiza, ixP, ic, :),&
            sb_pool_mt(ix_microbial, ixP, ic, :),sb_pool_mt(ix_residue, ixP, ic, :),&
            sb_pool_mt(ix_residue_assoc, ixP, ic, :)
        lun=lun+1
        ! soilpoolInNOrg_lay
        write(lun,'(181F18.6)') time_d, &
          nh4_assoc(ic,:), nh4_solute(ic,:),no3_solute(ic,:)  , &
          noy(ic,:), n2o(ic,:), n2(ic,:), &
          calc_delta_N15(nh4_assoc(ic,:) ,nh4_n15_assoc(ic,:))  , &
          calc_delta_N15(nh4_solute(ic,:),nh4_n15_solute(ic,:)) , &
          calc_delta_N15(no3_solute(ic,:),no3_n15_solute(ic,:))
        lun=lun+1
        ! soilpoolInPOrg_lay
        write(lun,'(101F16.6)') time_d, &
          po4_assoc_fast(ic,:), po4_assoc_slow(ic,:), po4_occluded(ic,:), po4_solute(ic,:), po4_primary(ic,:)
        lun=lun+1
        ! soilpooldC13org_lay
        dC13soluable_litter(ic,:)   = calc_delta_C13(sb_pool_mt(ix_soluable_litter, ixC, ic, :), &
                                                    sb_pool_mt(ix_soluable_litter, ixC13, ic, :))
        dC13polymeric_litter(ic,:)  = calc_delta_C13(sb_pool_mt(ix_polymeric_litter, ixC, ic, :), &
                                                    sb_pool_mt(ix_polymeric_litter, ixC13, ic, :))
        dC13woody_litter(ic,:)      = calc_delta_C13(sb_pool_mt(ix_woody_litter, ixC, ic, :), &
                                                    sb_pool_mt(ix_woody_litter, ixC13, ic, :))
        dC13dom(ic,:)               = calc_delta_C13(sb_pool_mt(ix_dom, ixC, ic, :), &
                                                    sb_pool_mt(ix_dom, ixC13, ic, :))
        dC13dom_assoc(ic,:)         = calc_delta_C13(sb_pool_mt(ix_dom_assoc, ixC, ic, :), &
                                                    sb_pool_mt(ix_dom_assoc, ixC13, ic, :))
        dC13fungi(ic,:)             = calc_delta_C13(sb_pool_mt(ix_fungi, ixC, ic, :), &
                                                    sb_pool_mt(ix_fungi, ixC13, ic, :))
        dC13mycorrhiza(ic,:)        = calc_delta_C13(sb_pool_mt(ix_mycorrhiza, ixC, ic, :), &
                                                    sb_pool_mt(ix_mycorrhiza, ixC13, ic, :))
        dC13microbial(ic,:)         = calc_delta_C13(sb_pool_mt(ix_microbial, ixC, ic, :), &
                                                    sb_pool_mt(ix_microbial, ixC13, ic, :))
        dC13residue(ic,:)           = calc_delta_C13(sb_pool_mt(ix_residue, ixC, ic, :), &
                                                    sb_pool_mt(ix_residue, ixC13, ic, :))
        dC13residue_assoc(ic,:)     = calc_delta_C13(sb_pool_mt(ix_residue_assoc, ixC, ic, :), &
                                                    sb_pool_mt(ix_residue_assoc, ixC13, ic, :))
        write(lun,'(201F16.6)') time_d, &
                                dC13soluable_litter(ic,:), dC13polymeric_litter(ic,:), &
                                dC13woody_litter(ic,:),    dC13dom(ic,:), &
                                dC13dom_assoc(ic,:),       dC13fungi(ic,:), &
                                dC13mycorrhiza(ic,:),      dC13microbial(ic,:), &
                                dC13residue(ic,:),         dC13residue_assoc(ic,:)
        lun=lun+1
        ! soilpoolDC14org_lay
        write(lun,'(201F16.6)') time_d,&
          calc_delta_C14(sb_pool_mt(ix_soluable_litter, ixC, ic, :), &
                          sb_pool_mt(ix_soluable_litter, ixC14, ic, :),dC13soluable_litter(ic,:)),&
          calc_delta_C14(sb_pool_mt(ix_polymeric_litter, ixC, ic, :), &
                          sb_pool_mt(ix_polymeric_litter, ixC14, ic, :),dC13polymeric_litter(ic,:)),&
          calc_delta_C14(sb_pool_mt(ix_woody_litter, ixC, ic, :), &
                          sb_pool_mt(ix_woody_litter, ixC14, ic, :),dC13woody_litter(ic,:)),&
          calc_delta_C14(sb_pool_mt(ix_dom, ixC, ic, :), &
                          sb_pool_mt(ix_dom, ixC14, ic, :),dC13dom(ic,:)),&
          calc_delta_C14(sb_pool_mt(ix_dom_assoc, ixC, ic, :), &
                          sb_pool_mt(ix_dom_assoc, ixC14, ic, :),dC13dom_assoc(ic,:)),&
          calc_delta_C14(sb_pool_mt(ix_fungi, ixC, ic, :), &
                          sb_pool_mt(ix_fungi, ixC14, ic, :),dC13fungi(ic,:)),&
          calc_delta_C14(sb_pool_mt(ix_mycorrhiza, ixC, ic, :), &
                          sb_pool_mt(ix_mycorrhiza, ixC14, ic, :),dC13mycorrhiza(ic,:)),&
          calc_delta_C14(sb_pool_mt(ix_microbial, ixC, ic, :), &
                          sb_pool_mt(ix_microbial, ixC14, ic, :),dC13microbial(ic,:)),&
          calc_delta_C14(sb_pool_mt(ix_residue, ixC, ic, :), &
                          sb_pool_mt(ix_residue, ixC14, ic, :),dC13residue(ic,:)),&
          calc_delta_C14(sb_pool_mt(ix_residue_assoc, ixC, ic, :), &
                          sb_pool_mt(ix_residue_assoc, ixC14, ic, :),dC13residue_assoc(ic,:))
        lun=lun+1
        ! soilpooldN15org_lay
        write(lun,'(201F20.6)') time_d, &
          calc_delta_N15(sb_pool_mt(ix_soluable_litter, ixN, ic, :), &
                          sb_pool_mt(ix_soluable_litter, ixN15, ic, :)), &
          calc_delta_N15(sb_pool_mt(ix_polymeric_litter, ixN, ic, :), &
                          sb_pool_mt(ix_polymeric_litter, ixN15, ic, :)), &
          calc_delta_N15(sb_pool_mt(ix_woody_litter, ixN, ic, :), &
                          sb_pool_mt(ix_woody_litter, ixN15, ic, :)), &
          calc_delta_N15(sb_pool_mt(ix_dom, ixN, ic, :), &
                          sb_pool_mt(ix_dom, ixN15, ic, :)), &
          calc_delta_N15(sb_pool_mt(ix_dom_assoc, ixN, ic, :), &
                          sb_pool_mt(ix_dom_assoc, ixN15, ic, :)), &
          calc_delta_N15(sb_pool_mt(ix_fungi, ixN, ic, :), &
                          sb_pool_mt(ix_fungi, ixN15, ic, :)), &
          calc_delta_N15(sb_pool_mt(ix_mycorrhiza, ixN, ic, :), &
                          sb_pool_mt(ix_mycorrhiza, ixN15, ic, :)), &
          calc_delta_N15(sb_pool_mt(ix_microbial, ixN, ic, :), &
                          sb_pool_mt(ix_microbial, ixN15, ic, :)), &
          calc_delta_N15(sb_pool_mt(ix_residue, ixN, ic, :), &
                          sb_pool_mt(ix_residue, ixN15, ic, :)), &
          calc_delta_N15(sb_pool_mt(ix_residue_assoc, ixN, ic, :), &
                          sb_pool_mt(ix_residue_assoc, ixN15, ic, :))
        lun=lun+1
        ! soil_diagnostics_microbial_lay

        hlp1_2d(:,:) = 0.0_wp
        DO is = 1,INT(num_sl_above_bedrock(ic))
          hlp1_2d(ic,is) = root_fraction_sl(ic,is) / soil_depth_sl(ic,is)
        ENDDO
        write(lun,'(401F16.6)') time_d, microbial_cue_eff_tmic_mavg(ic,:), microbial_nue_eff_tmic_mavg(ic,:), &
                               microbial_pue_eff_tmic_mavg(ic,:), &
                               veg_pool_mt(ix_fine_root, ixC, ic) * hlp1_2d(ic,:), &
                               enzyme_frac_poly(ic,:), enzyme_frac_residue(ic,:), enzyme_frac_poly_c(ic,:), &
                               enzyme_frac_poly_n(ic,:), enzyme_frac_poly_p(ic,:), enzyme_frac_poly_c_mavg(ic,:), &
                               enzyme_frac_poly_n_mavg(ic,:), enzyme_frac_poly_p_mavg(ic,:), &
                               dom_cn(ic,:), dom_cp(ic,:), dom_cn_mavg(ic,:), dom_cp_mavg(ic,:), &
                               enzyme_frac_AP(ic,:), &
                               limitation_status_diagnostics(ic,:), fact_n_status_mic_c_growth(ic,:), &
                               fact_p_status_mic_c_growth(ic,:)

        lun=lun+1
        ! soil_diagnostics_sorption_lay
        write(lun,'(281F16.6)') time_d, qmax_org(ic,:), bulk_dens_sl(ic,:), bulk_dens_corr_sl(ic,:), qmax_po4(ic,:), &
                               km_adsorpt_po4_sl(ic,:), volume_min_sl(ic,:)
        lun=lun+1
      END IF        ! l_land_plant_canopy_out .OR. l_soil_output
    END SELECT      ! land/soil


  ! Deallocate local allocatable variables
  DEALLOCATE(dC13soluable_litter)
  DEALLOCATE(dC13polymeric_litter)
  DEALLOCATE(dC13woody_litter)
  DEALLOCATE(dC13dom)
  DEALLOCATE(dC13dom_assoc)
  DEALLOCATE(dC13fungi)
  DEALLOCATE(dC13mycorrhiza)
  DEALLOCATE(dC13microbial)
  DEALLOCATE(dC13residue)
  DEALLOCATE(dC13residue_assoc)

  END SUBROUTINE write_output_pool



  !-----------------------------------------------------------------------------------------------------!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! section II Fluxes                                                                                 !!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !-----------------------------------------------------------------------------------------------------!


  !-----------------------------------------------------------------------------------------------------
  !> Fluxes: open files and write column headers
  !! 
  !! open output files, define file names for pools, write column header names
  !! 
  !! lun: start at 100 + 30
  !-----------------------------------------------------------------------------------------------------
  SUBROUTINE open_flux_output_files(tile, iblk, ics, ice, nc, output_interval_flux, spinup_id)

    USE mo_jsb_class,             ONLY: Get_model
    USE mo_jsb_tile_class,        ONLY: t_jsb_tile_abstract
    USE mo_jsb_model_class,       ONLY: t_jsb_model
    
    IMPLICIT NONE
    ! ---------------------------
    ! 0.1 InOut
    CLASS(t_jsb_tile_abstract), INTENT(inout)   :: tile           !< one tile with data structure for one lct
    INTEGER,INTENT(in)                          :: iblk, &        !< iBlock, start point of chunk, end point of chunk, number of points in the chunk
                                                   ics, &
                                                   ice, &
                                                   nc
    CHARACTER(len=*), INTENT(in)                :: output_interval_flux 
    CHARACTER(len=*), INTENT(in)                :: spinup_id
    ! ---------------------------
    ! 0.2 Local
    TYPE(t_jsb_model), POINTER       :: model
    INTEGER                          :: lun           ! logical unit number
    CHARACTER(100)                   :: fname         ! filename
    CHARACTER(len=*), PARAMETER :: routine = TRIM(modname)//':open_flux_output_files'
    ! ---------------------------
    ! 0.5 Get Memory
    model  => Get_model(tile%owner_model_id)


    !> 1.0 define open() statements for file output
    !! 
    !! 
    lun = lun_ascii_fluxes_start    !< max of 30 files for pool output (LUN 100 - 129)

    ! land
    SELECT CASE(model%config%qmodel_id)
      CASE(QLAND)  ! for "land & soil" see below
        ! mycfluxCNP
        fname = 'mycfluxCNP'//'_'//TRIM(spinup_id)//TRIM(output_interval_flux)//'.txt'
        OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
        lun=lun+1
        ! myc_diagnostics
        fname = 'myc_diagnostics'//'_'//TRIM(spinup_id)//TRIM(output_interval_flux)//'.txt'
        OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
        lun=lun+1
    END SELECT

    ! land/plant
    SELECT CASE(model%config%qmodel_id)
      CASE(QLAND,QPLANT) ! additional to CASE(QLAND)
        ! vegfluxN
        fname = 'vegfluxN'//'_'//TRIM(spinup_id)//TRIM(output_interval_flux)//'.txt'
        OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
        lun=lun+1
        ! vegfluxP
        fname = 'vegfluxP'//'_'//TRIM(spinup_id)//TRIM(output_interval_flux)//'.txt'
        OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
        lun=lun+1
    END SELECT

    ! land/plant/canopy
    SELECT CASE(model%config%qmodel_id)
      CASE(QLAND, QPLANT, QCANOPY)
        ! vegfluxC
        fname = 'vegfluxC'//'_'//TRIM(spinup_id)//TRIM(output_interval_flux)//'.txt'
        OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
        lun=lun+1
        ! vegfluxdC13DC14
        fname = 'vegfluxdC13DC14'//'_'//TRIM(spinup_id)//TRIM(output_interval_flux)//'.txt'
        OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
        lun=lun+1        
        ! vegfluxH2O
        fname = 'vegfluxH2O'//'_'//TRIM(spinup_id)//TRIM(output_interval_flux)//'.txt'
        OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
        lun=lun+1
        ! vegfluxEnergy
        fname = 'vegfluxEnergy'//'_'//TRIM(spinup_id)//TRIM(output_interval_flux)//'.txt'
        OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
        lun=lun+1
        ! atm_forcing
        fname = 'atm_forcing'//'_'//TRIM(spinup_id)//TRIM(output_interval_flux)//'.txt'
        OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
        lun=lun+1
    END SELECT

    ! land/soil
    SELECT CASE(model%config%qmodel_id)
      CASE(QLAND, QSOIL)
        ! soilfluxCdC13DC14
        fname = 'soilfluxCdC13DC14'//'_'//TRIM(spinup_id)//TRIM(output_interval_flux)//'.txt'
        OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
        lun=lun+1
        ! soilfluxN
        fname = 'soilfluxN'//'_'//TRIM(spinup_id)//TRIM(output_interval_flux)//'.txt'
        OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
        lun=lun+1
        ! soilfluxP
        fname = 'soilfluxP'//'_'//TRIM(spinup_id)//TRIM(output_interval_flux)//'.txt'
        OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
        lun=lun+1
        ! soilfluxdN
        fname = 'soilfluxdN'//'_'//TRIM(spinup_id)//TRIM(output_interval_flux)//'.txt'
        OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
        lun=lun+1
        ! sb_massBalance
        fname = 'sb_massBalance'//'_'//TRIM(spinup_id)//TRIM(output_interval_flux)//'.txt'
        OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
        lun=lun+1
        ! soilfluxC_lay
        fname = 'soilfluxC_lay'//'_'//TRIM(spinup_id)//TRIM(output_interval_flux)//'.txt'
        OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
        lun=lun+1
        ! soilfluxN_lay
        fname = 'soilfluxN_lay'//'_'//TRIM(spinup_id)//TRIM(output_interval_flux)//'.txt'
        OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
        lun=lun+1
        ! soilfluxP_lay
        fname = 'soilfluxP_lay'//'_'//TRIM(spinup_id)//TRIM(output_interval_flux)//'.txt'
        OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
        lun=lun+1
    END SELECT

    !> 1.1  test if lun for fluxes is too big
    !! 
    IF (lun > lun_ascii_fluxes_max) THEN
      WRITE(message_text,'(a)') 'lun of fluxes is too big, reaching values that are reserved for lun of soil forcing output'
      CALL finish(TRIM(routine),message_text)
    END IF

    !>2.0 write column headers into output files
    !> 
    CALL init_output_writing_flux(model%config%qmodel_id)

    !> 3.0 set all flux-output *_acc variables to zero, but do not produce output
    !> that is needed when, e.g., switching between diff. output periods (fluxnet diagnostics monthly -> timestep) 
    !>   to delete/set zero these *_acc variables that might contain data from previous output period & output file
    !>
    CALL write_output_flux(tile, model%options, &
                           time_d=0.0_wp, &
                           output_interval_flux=0.0_wp, &
                           l_flux_output_interval=.FALSE., & 
                           l_zero_acc_variables=.TRUE.)

  END SUBROUTINE open_flux_output_files


  !-----------------------------------------------------------------------------------------------------
  !> Fluxes: write column headers of output files
  !! 
  !! 
  !-----------------------------------------------------------------------------------------------------
  SUBROUTINE init_output_writing_flux(qmodel_id)

    USE mo_jsb_grid_class,        ONLY: t_jsb_vgrid
    USE mo_jsb_grid,              ONLY: Get_vgrid

    IMPLICIT NONE
    ! ---------------------------
    ! 0.1 InOut
    INTEGER  ,INTENT(in) :: qmodel_id
    ! ---------------------------
    ! 0.2 Local
    TYPE(t_jsb_vgrid), POINTER :: vgrid_soil_sb                ! Vertical grid
    INTEGER                    :: nsoil_sb                     ! number of soil layers as used/defined by the SB_ process
    INTEGER                    :: lun,isoil
    CHARACTER(2)               :: layerstr
    CHARACTER(450)             :: p1str,p2str,p3str,p4str,p5str,p6str,p7str,p8str,p9str,p10str, &
                                  p11str,p12str,p13str,p14str,p15str,p16str,p17str,p18str,p19str,p20str,p21str, &
                                  p22str, p23str, p24str, p25str, p26str, p27str, p28str
    ! ---------------------------
    ! 0.5 Get Memory
    vgrid_soil_sb => Get_vgrid('soil_layer_sb')
    nsoil_sb      =  vgrid_soil_sb%n_levels


    !>1.0 write headers
    !>
    lun = lun_ascii_fluxes_start    !< max of 30 files for pool output (LUN 100 - 129)
    
    ! land 
    SELECT CASE(qmodel_id)
      CASE(QLAND)
        ! mycfluxCNP [mol / m2 / interval]
        WRITE(lun,'(a)') 'Time mycExportC mycExportN mycExportP mycExportC13 mycExportC14 mycExportN15'
        lun=lun+1
        ! myc_diagnostics
        WRITE(lun,'(a)') 'Time fCm_resp fCm_SOM'
        lun=lun+1
    END SELECT


    ! land & plant
    SELECT CASE(qmodel_id)
      CASE(QLAND,QPLANT) ! additional to CASE(QLAND)
        ! vegfluxN [mol N / m2 / interval]
        WRITE(lun,'(7a)') &
          & 'Time Uptake_NH4 Uptake_NO3 RecyclingN ReserveUseN ', &
          & 'FixationN Fixation_dN15 Uptake_NH4_dN15 Uptake_NO3_dN15 GrowthN ', &
          & 'Growth_sapwood_dN15 ', &
          & 'GrowthN_leaf GrowthN_froot GrowthN_croot GrowthN_swood GrowthN_hwood ', &
          & 'GrowthN_labile GrowthN_reserve GrowthN_fruit GrowthN_sbed ', &
          & 'LitterfN_leaf LitterfN_froot LitterfN_croot LitterfN_swood LitterfN_hwood ', &
          & 'LitterfN_labile LitterfN_reserve LitterfN_fruit LitterfN_sbed RootN_Exudation'
        lun=lun+1
        ! vegfluxP [mol P / m2 / interval]
        WRITE(lun,'(5a)') &
          & 'Time Uptake_PO4 RecyclingP ReserveUseP GrowthP ', &
          & 'GrowthP_leaf GrowthP_froot GrowthP_croot GrowthP_swood GrowthP_hwood ', &
          & 'GrowthP_labile GrowthP_reserve GrowthP_fruit GrowthP_sbed ', &
          & 'LitterfP_leaf LitterfP_froot LitterfP_croot LitterfP_swood LitterfP_hwood ', &
          & 'LitterfP_labile LitterfP_reserve LitterfP_fruit LitterfP_sbed RootP_Exudation'
        lun=lun+1
    END SELECT


    ! land & plant & canopy
    SELECT CASE(qmodel_id)
      CASE(QLAND, QPLANT, QCANOPY)
        ! vegfluxC [mol C / m2 / interval]
        WRITE(lun,'(5a)') &
          & 'Time GPP GrowthResp MaintResp NTransformResp NFixationResp NPP ReserveUseC RootC_Exudation GrowthC ', &
          & 'GrowthC_leaf GrowthC_froot GrowthC_croot GrowthC_swood GrowthC_hwood ', &
          & 'GrowthC_labile GrowthC_reserve GrowthC_fruit GrowthC_sbed ', &
          & 'LitterfC_leaf LitterfC_froot LitterfC_croot LitterfC_swood LitterfC_hwood ', &
          & 'LitterfC_labile LitterfC_reserve LitterfC_fruit LitterfC_sbed exudationC_base'
        lun=lun+1 
        ! vegfluxdC13DC14
        WRITE(lun,'(2a)') 'Time GPP_dC13 GrowthResp_dC13 MaintResp_dC13 Growth_sapwood_dC13 ', &
                          'GPP_DC14 GrowthResp_DC14 MaintResp_DC14 Growth_sapwood_DC14'
        lun=lun+1
        ! vegfluxH2O [kg/m2/interval, ... , m/interval, m/interval ]
        WRITE(lun,'(a)') 'Time Precip PotEvaporation Evaporation Interception Transpiration SrfRunoff Drainage GwRunoff GC GA'
        lun=lun+1
        ! vegfluxEnergy [Wm-2] [Wm-2] [Wm-2] [Wm-2] [--] [--] [µmol m-2 s-1] [µmol m-2 s-1] [--] [--] [--]
        WRITE(lun,'(a)') 'Time Rnet Qh Qle Qg Albedo Albedo_noon PPFD aPPFD fAPPFD albedo_vis albedo_nir'
        lun=lun+1        
        ! atm_forcing [???]
        WRITE(lun,'(2a)') 'Time sum_Precip mean_AirTemperature mean_Wind mean_SWdown mean_LWdown mean_VPD', &
                          ' atm_CO2 atm_CO2_dC13 atm_CO2_DC14'
        lun=lun+1
    END SELECT

    
    ! land/soil
    SELECT CASE(qmodel_id)
      CASE(QLAND, QSOIL)
        !! all soil models (simple_1d and jsm)
        ! soilfluxCdC13DC14 [mol C /m2 / interval]
        WRITE(lun,'(6a)') 'Time Litterfall_C ', &
                          'HetResp LeachingDOM_C LateralLossDOM_C Litterfall_dC13  ', &
                          'HetResp_dC13 LeachingDOM_dC13 LateralLossDOM_dC13 ', &
                          'Litterfall_DC14 HetResp_DC14 LeachingDOM_DC14 LateralLossDOM_DC14 ', &
                          'flux_soluable_2_dom_C flux_residue_2_dom_C flux_poly_2_dom_C ', &
                          'flux_micturn_2_dom_C flux_micturn_2_res_C flux_dom_2_microbes_C'
        lun=lun+1
        ! soilfluxN [mol N /m2 /interval]
        WRITE(lun,'(8a)') 'Time Litterfall_N ', &
                          'NDeposition Nfix_asymb NetMinNH4 NetMinNO3 Nitrification Denitrification Emission_NOY ', &
                          'Emission_N2O Emission_N2 PlantUptakeNH4 PlantUptakeNO3 LeachingNH4 LeachingNO3 LeachingDOM_N ', &
                          'LateralLoss_NH4 LateralLoss_NO3 LateralLossDOM_N ', & 
                          'Fast_exchange_nh4 MycUptake_NH4 MycUptake_NO3 MycUptake_NOrg ', &
                          'flux_soluable_2_dom_nitrogen flux_residue_2_dom_nitrogen flux_poly_2_dom_nitrogen ', &
                          'flux_micturn_2_dom_nitrogen flux_micturn_2_res_nitrogen flux_dom_2_microbes_nitrogen ', &
                          'grazMinNH4'  
        lun=lun+1
        ! soilfluxP [mol P /m2 /interval]
        WRITE(lun,'(6a)') 'Time Litterfall_P ', &
                          'PDeposition Leaching_PO4 LeachingDOM_P LateralLoss_PO4 LateralLossDOM_P ', &
                          'Net_Mineralisation_PO4 PlantUptake_PO4 Weathering_PO4 ', &
                          'Occlusion_PO4 Slow_exchange_PO4 Fast_exchange_PO4 Biochem_mineralisation_PO4 ',&
                          'flux_soluable_2_dom_P flux_residue_2_dom_P flux_poly_2_dom_P ', &
                          'flux_micturn_2_dom_P flux_micturn_2_res_P flux_dom_2_microbes_P grazMinPO4'
        lun=lun+1
        ! soilfluxdN
        WRITE(lun,'(2a)') 'Time dN15_Deposition dN15_fix_asymb Emission_NOY_dN15 Emission_N2O_dN15 Emission_N2_dN15 ', &
                          'Leaching_NH4_dN15 Leaching_NO3_dN15 LateralLoss_NH4_dN15 LateralLoss_NO3_dN15 grazMinNH4_dN15' 
        lun=lun+1
        ! sb_massBalance
        WRITE(lun,'(2a)') 'Time sb_deltaPoolC sb_deltaPoolN sb_deltaPoolP sb_deltaPoolC13 sb_deltaPoolC14 sb_deltaPoolN15 ', &
                          'sb_totalFluxC sb_totalFluxN sb_totalFluxP sb_totalFluxC13 sb_totalFluxC14 sb_totalFluxN15'
        lun=lun+1
        ! soilfluxC_lay [mol C /m3 /interval]
        p1str=''
        p2str=''
        p3str=''
        p4str=''
        p5str=''
        p6str='' 
        p7str=''
        p8str=''
        p9str=''
        p10str=''
        p11str=''
        p12str=''
        p13str=''
        p14str=''
        p15str=''
        p16str=''
        DO isoil = 1, nsoil_sb
          IF(isoil<10) THEN 
            WRITE(layerstr,'(I1)') isoil
          ELSE
            WRITE(layerstr,'(I2)') isoil
          ENDIF
          WRITE(p1str,'(a)') TRIM(p1str)//' HetRespiration_L'//TRIM(layerstr)
          WRITE(p2str,'(a)') TRIM(p2str)//' HetRespirationC13_L'//TRIM(layerstr)
          WRITE(p3str,'(a)') TRIM(p3str)//' HetRespirationC14_L'//TRIM(layerstr)
          WRITE(p4str,'(a)') TRIM(p4str)//' Transport_DOC_L'//TRIM(layerstr)
          WRITE(p5str,'(a)') TRIM(p5str)//' Transport_Res_L'//TRIM(layerstr)
          WRITE(p6str,'(a)') TRIM(p6str)//' Transport_aRes_L'//TRIM(layerstr)
          WRITE(p7str,'(a)') TRIM(p7str)//' Root_Respiration_L'//TRIM(layerstr)
          WRITE(p8str,'(a)') TRIM(p8str)//' LateralLoss_DOC_L'//TRIM(layerstr) 
          WRITE(p9str,'(a)') TRIM(p9str)//' ae_carbon_L'//TRIM(layerstr)
          WRITE(p10str,'(a)') TRIM(p10str)//' flux_soluable_2_dom_C_L'//TRIM(layerstr)
          WRITE(p11str,'(a)') TRIM(p11str)//' flux_residue_2_dom_C_L'//TRIM(layerstr)
          WRITE(p12str,'(a)') TRIM(p12str)//' flux_poly_2_dom_C_L'//TRIM(layerstr)
          WRITE(p13str,'(a)') TRIM(p13str)//' flux_micturn_2_dom_C_L'//TRIM(layerstr)
          WRITE(p14str,'(a)') TRIM(p14str)//' flux_micturn_2_res_C_L'//TRIM(layerstr)
          WRITE(p15str,'(a)') TRIM(p15str)//' flux_dom_2_microbes_C_L'//TRIM(layerstr)
          WRITE(p16str,'(a)') TRIM(p16str)//' exudation_C_L'//TRIM(layerstr)
        END DO
        WRITE(lun,'(17a)') 'Time ', TRIM(p1str),TRIM(p2str),TRIM(p3str),TRIM(p4str),TRIM(p5str), & 
                                    TRIM(p6str),TRIM(p7str),TRIM(p8str),TRIM(p9str),TRIM(p10str), &
                                    TRIM(p11str),TRIM(p12str),TRIM(p13str),TRIM(p14str), &
                                    TRIM(p15str), TRIM(p16str)

        lun=lun+1
        ! soilfluxN_lay [mol N /m3 /interval]
        p1str=''
        p2str=''
        p3str=''
        p4str=''
        p5str=''
        p6str=''
        p7str=''
        p8str=''
        p9str=''
        p10str=''
        p11str=''
        p12str=''
        p13str=''
        p14str=''
        p15str=''
        p16str=''
        p17str=''
        p18str='' 
        p19str='' 
        p20str='' 
        p21str=''
        p22str=''
        p23str=''
        p24str=''
        p25str=''
        p26str=''
        p27str=''
        p28str=''
        DO isoil = 1, nsoil_sb
          IF(isoil<10) THEN 
            WRITE(layerstr,'(I1)') isoil
          ELSE
            WRITE(layerstr,'(I2)') isoil
          ENDIF
          WRITE(p1str,'(a)') TRIM(p1str)//' NetMineral_nh4_L'//TRIM(layerstr)
          WRITE(p2str,'(a)') TRIM(p2str)//' PlantUptake_nh4_L'//TRIM(layerstr)
          WRITE(p3str,'(a)') TRIM(p3str)//' MicUptake_nh4_L'//TRIM(layerstr)
          WRITE(p4str,'(a)') TRIM(p4str)//' MycUptake_nh4_L'//TRIM(layerstr)
          WRITE(p5str,'(a)') TRIM(p5str)//' Transport_nh4_L'//TRIM(layerstr)
          WRITE(p6str,'(a)') TRIM(p6str)//' Transport_assoc_nh4_L'//TRIM(layerstr)
          WRITE(p7str,'(a)') TRIM(p7str)//' AsymbNfixation_L'//TRIM(layerstr)
          WRITE(p8str,'(a)') TRIM(p8str)//' NetMineral_no3_L'//TRIM(layerstr)
          WRITE(p9str,'(a)') TRIM(p9str)//' PlantUptake_no3_L'//TRIM(layerstr)
          WRITE(p10str,'(a)') TRIM(p10str)//' MicUptake_no3_L'//TRIM(layerstr)
          WRITE(p11str,'(a)') TRIM(p11str)//' MycUptake_no3_L'//TRIM(layerstr)
          WRITE(p12str,'(a)') TRIM(p12str)//' Transport_no3_L'//TRIM(layerstr)
          WRITE(p13str,'(a)') TRIM(p13str)//' Transport_DON_L'//TRIM(layerstr)
          WRITE(p14str,'(a)') TRIM(p14str)//' MycUptake_norg_L'//TRIM(layerstr)
          WRITE(p15str,'(a)') TRIM(p15str)//' Adsorpt_nh4_L'//TRIM(layerstr)
          WRITE(p16str,'(a)') TRIM(p16str)//' Nitrification_no3_L'//TRIM(layerstr)
          WRITE(p17str,'(a)') TRIM(p17str)//' Denitrification_n2_L'//TRIM(layerstr)
          WRITE(p18str,'(a)') TRIM(p18str)//' LateralLoss_nh4_L'//TRIM(layerstr) 
          WRITE(p19str,'(a)') TRIM(p19str)//' LateralLoss_no3_L'//TRIM(layerstr) 
          WRITE(p20str,'(a)') TRIM(p20str)//' LateralLoss_DON_L'//TRIM(layerstr) 
          WRITE(p21str,'(a)') TRIM(p21str)//' ae_nitrogen_L'//TRIM(layerstr)
          WRITE(p22str,'(a)') TRIM(p22str)//' flux_soluable_2_dom_n_L'//TRIM(layerstr)
          WRITE(p23str,'(a)') TRIM(p23str)//' flux_residue_2_dom_n_L'//TRIM(layerstr)
          WRITE(p24str,'(a)') TRIM(p24str)//' flux_poly_2_dom_n_L'//TRIM(layerstr)
          WRITE(p25str,'(a)') TRIM(p25str)//' flux_micturn_2_dom_n_L'//TRIM(layerstr)
          WRITE(p26str,'(a)') TRIM(p26str)//' flux_micturn_2_res_n_L'//TRIM(layerstr)
          WRITE(p27str,'(a)') TRIM(p27str)//' flux_dom_2_microbes_n_L'//TRIM(layerstr)
          WRITE(p28str,'(a)') TRIM(p28str)//' grazing_min_nh4_L'//TRIM(layerstr)

        END DO
        WRITE(lun,'(29a)') 'Time ', TRIM(p1str),TRIM(p2str),TRIM(p3str),TRIM(p4str),TRIM(p5str), & 
                                    TRIM(p6str),TRIM(p7str),TRIM(p8str),TRIM(p9str),TRIM(p10str), &
                                    TRIM(p11str),TRIM(p12str),TRIM(p13str),TRIM(p14str),TRIM(p15str), &
                                    TRIM(p16str),TRIM(p17str), TRIM(p18str),TRIM(p19str),TRIM(p20str),TRIM(p21str), &
                                    TRIM(p22str),TRIM(p23str),TRIM(p24str),TRIM(p25str),TRIM(p26str),TRIM(p27str), &
                                    TRIM(p28str)
        lun=lun+1
        ! soilfluxP_lay  [mol P /m3 /interval]
        p1str=''
        p2str=''
        p3str=''
        p4str=''
        p6str=''
        p7str=''
        p8str=''
        p9str=''
        p10str=''
        p11str=''
        p12str=''
        p13str=''
        p14str=''
        p15str=''
        p16str=''
        p17str=''
        p18str='' 
        p19str='' 
        p20str='' 
        p21str=''
        p22str=''
        p23str=''
        p24str=''
        p25str=''
        p26str=''
        p27str=''
        DO isoil = 1, nsoil_sb
          IF(isoil<10) THEN 
            WRITE(layerstr,'(I1)') isoil
          ELSE
            WRITE(layerstr,'(I2)') isoil
          ENDIF
          WRITE(p1str,'(a)') TRIM(p1str)//' Weath_L'//TRIM(layerstr)
          WRITE(p2str,'(a)') TRIM(p2str)//' Ocl_L'//TRIM(layerstr)
          WRITE(p3str,'(a)') TRIM(p3str)//' Exchange_fast_L'//TRIM(layerstr)
          WRITE(p4str,'(a)') TRIM(p4str)//' Exchange_slow_L'//TRIM(layerstr)
          WRITE(p6str,'(a)') TRIM(p6str)//' Adsorpt_fast_L'//TRIM(layerstr)

          WRITE(p7str,'(a)')  TRIM(p7str)//' NetMin_L'//TRIM(layerstr)
          WRITE(p8str,'(a)')  TRIM(p8str)//' Biochem_min_L'//TRIM(layerstr)
          WRITE(p9str,'(a)')  TRIM(p9str)//' Mic_upt_L'//TRIM(layerstr)
          WRITE(p10str,'(a)') TRIM(p10str)//' Plant_upt_L'//TRIM(layerstr)
          WRITE(p11str,'(a)') TRIM(p11str)//' Myc_upt_L'//TRIM(layerstr)

          WRITE(p12str,'(a)') TRIM(p12str)//' Transport_solute_L'//TRIM(layerstr)
          WRITE(p13str,'(a)') TRIM(p13str)//' Transport_assoc_fast_L'//TRIM(layerstr)
          WRITE(p14str,'(a)') TRIM(p14str)//' Transport_assoc_slow_L'//TRIM(layerstr)
          WRITE(p15str,'(a)') TRIM(p15str)//' Transport_occluded_L'//TRIM(layerstr)
          WRITE(p16str,'(a)') TRIM(p16str)//' Transport_primary_L'//TRIM(layerstr)
          WRITE(p17str,'(a)') TRIM(p17str)//' Transport_DOP_L'//TRIM(layerstr)
          WRITE(p18str,'(a)') TRIM(p18str)//' LateralLoss_solute_L'//TRIM(layerstr) 
          WRITE(p19str,'(a)') TRIM(p19str)//' LateralLoss_DOP_L'//TRIM(layerstr) 
          WRITE(p20str,'(a)') TRIM(p20str)//' ae_phosphorus_L'//TRIM(layerstr)
          WRITE(p21str,'(a)') TRIM(p21str)//' flux_soluable_2_dom_P_L'//TRIM(layerstr)
          WRITE(p22str,'(a)') TRIM(p22str)//' flux_residue_2_dom_P_L'//TRIM(layerstr)
          WRITE(p23str,'(a)') TRIM(p23str)//' flux_poly_2_dom_P_L'//TRIM(layerstr)
          WRITE(p24str,'(a)') TRIM(p24str)//' flux_micturn_2_dom_P_L'//TRIM(layerstr)
          WRITE(p25str,'(a)') TRIM(p25str)//' flux_micturn_2_res_P_L'//TRIM(layerstr)
          WRITE(p26str,'(a)') TRIM(p26str)//' flux_dom_2_microbes_P_L'//TRIM(layerstr)
          WRITE(p27str,'(a)') TRIM(p27str)//' grazing_min_po4_L'//TRIM(layerstr)
        END DO
        WRITE(lun,'(27a)') 'Time ', TRIM(p1str),TRIM(p2str),TRIM(p3str),TRIM(p4str), &
                                    TRIM(p6str),TRIM(p7str),TRIM(p8str),TRIM(p9str),TRIM(p10str), &
                                    TRIM(p11str),TRIM(p12str),TRIM(p13str),TRIM(p14str),TRIM(p15str), &
                                    TRIM(p16str),TRIM(p17str),TRIM(p18str),TRIM(p19str),TRIM(p20str), &
                                    TRIM(p21str),TRIM(p22str),TRIM(p23str),TRIM(p24str),TRIM(p25str), &
                                    TRIM(p26str),TRIM(p27str)  
        lun=lun+1
    END SELECT
    
  END SUBROUTINE init_output_writing_flux

  ! ======================================================================================================= !
  !>Fluxes: write data to files in a certain interval and accumulate fluxes
  !>
  SUBROUTINE write_output_flux(tile, options, time_d, output_interval_flux, l_flux_output_interval, l_zero_acc_variables)

    USE mo_jsb_class,              ONLY: Get_model
    USE mo_jsb_model_class,        ONLY: t_jsb_model
    USE mo_jsb_tile_class,         ONLY: t_jsb_tile_abstract
    USE mo_jsb_task_class,         ONLY: t_jsb_task_options
    USE mo_jsb_process_class,      ONLY: SB_, A2L_, Q_ASSIMI_, VEG_, Q_RAD_, SPQ_
    USE mo_jsb_grid_class,         ONLY: t_jsb_vgrid
    USE mo_jsb_grid,               ONLY: Get_vgrid
    USE mo_jsb_math_constants,     ONLY: eps8
    USE mo_jsb_physical_constants, ONLY: molar_mass_C, molar_mass_N
    USE mo_q_rad_parameters,       ONLY: rad2ppfd
    USE mo_sb_constants,           ONLY: organic_cn
    USE mo_isotope_util,           ONLY: calc_delta_C13, calc_delta_C14, calc_delta_N15
    USE mo_spq_config_class,       ONLY: max_soil_layers
    USE mo_atmland_util,           ONLY: calc_spec_humidity_sat
    USE mo_atmland_constants,      ONLY: eps_vpd
    ! ----------------------------------------------------------------------------------------------------- !
    dsl4jsb_Use_config(SB_)
    dsl4jsb_Use_memory(A2L_)
    dsl4jsb_Use_memory(Q_ASSIMI_)
    dsl4jsb_Use_memory(VEG_)
    dsl4jsb_Use_memory(Q_RAD_)
    dsl4jsb_Use_memory(SPQ_)
    dsl4jsb_Use_memory(SB_)
    ! ----------------------------------------------------------------------------------------------------- !
    CLASS(t_jsb_tile_abstract), INTENT(inout) :: tile                     !< one tile with data structure for one lct
    TYPE(t_jsb_task_options),   INTENT(in)    :: options                  !< model options
    REAL(wp),                   INTENT(in)    :: time_d, &                !< time in days
                                                 output_interval_flux     !< output interval in days (1.0 == one day exactly)
    LOGICAL,                    INTENT(in)    :: l_flux_output_interval   !< logical: output at current timestep TRUE/FALSE
    LOGICAL,                    INTENT(in)    :: l_zero_acc_variables     !< if TRUE, then run this routine but only set all *_acc variables to zero 
    ! ----------------------------------------------------------------------------------------------------- !
    TYPE(t_jsb_model),      POINTER   :: model                    !< the model
    TYPE(t_lnd_bgcm_store), POINTER   :: bgcm_store               !< the bgcm store of this tile
    TYPE(t_lctlib_element), POINTER   :: lctlib                   !< land-cover-type library - parameter across pft's
    TYPE(t_jsb_vgrid), POINTER        :: vgrid_soil_sb            !< Vertical grid
    INTEGER                           :: nsoil_sb                 !< number of soil layers as used/defined by the SB_ process
    LOGICAL                           :: l_flux_calc_acc_output   !< this is set to TRUE by default (below) because this routine is called only if it is TRUE
    ! local accumulation variables
      ! Note: the *_acc variables are initialized only the first time the subroutine is called. 
      ! On subsequent calls the old value of *_acc is retained.
      ! To improve code readability the SAVE attribute is added, even though this is redundant.
    REAL(wp), SAVE                        :: ndat_acc            = 0.0_wp
    REAL(wp), SAVE                        :: gpp_acc             = 0.0_wp
    REAL(wp), SAVE                        :: growth_resp_acc     = 0.0_wp
    REAL(wp), SAVE                        :: maint_resp_acc      = 0.0_wp   
    REAL(wp), SAVE                        :: n_transform_resp_acc = 0.0_wp   
    REAL(wp), SAVE                        :: n_fixation_resp_acc = 0.0_wp 
    REAL(wp), SAVE                        :: exudation_c_acc     = 0.0_wp  
    REAL(wp), SAVE                        :: npp_acc             = 0.0_wp   
    REAL(wp), SAVE                        :: res_use_c_acc       = 0.0_wp   
    REAL(wp), SAVE                        :: gpp_C13_acc         = 0.0_wp
    REAL(wp), SAVE                        :: growth_resp_C13_acc = 0.0_wp
    REAL(wp), SAVE                        :: maint_resp_C13_acc  = 0.0_wp
    REAL(wp), SAVE                        :: gpp_C14_acc         = 0.0_wp
    REAL(wp), SAVE                        :: growth_resp_C14_acc = 0.0_wp
    REAL(wp), SAVE                        :: maint_resp_C14_acc  = 0.0_wp
    REAL(wp), SAVE                        :: uptake_nh4_acc      = 0.0_wp
    REAL(wp), SAVE                        :: plant_uptake_nh4_acc = 0.0_wp   
    REAL(wp), SAVE                        :: uptake_nh4_n15_acc  = 0.0_wp   
    REAL(wp), SAVE                        :: uptake_no3_acc      = 0.0_wp 
    REAL(wp), SAVE                        :: plant_uptake_no3_acc = 0.0_wp  
    REAL(wp), SAVE                        :: uptake_no3_n15_acc  = 0.0_wp   
    REAL(wp), SAVE                        :: recycling_n_acc     = 0.0_wp  
    REAL(wp), SAVE                        :: res_use_n_acc       = 0.0_wp  
    REAL(wp), SAVE                        :: n_fixation_acc      = 0.0_wp   
    REAL(wp), SAVE                        :: n_fixation_n15_acc  = 0.0_wp   
    REAL(wp), SAVE                        :: plant_uptake_po4_acc = 0.0_wp
    REAL(wp), SAVE                        :: uptake_po4_acc      = 0.0_wp
    REAL(wp), SAVE                        :: recycling_p_acc     = 0.0_wp  
    REAL(wp), SAVE                        :: res_use_p_acc       = 0.0_wp   
    REAL(wp), SAVE                        :: precip_acc          = 0.0_wp 
    REAL(wp), SAVE                        :: evaporation_acc     = 0.0_wp 
    REAL(wp), SAVE                        :: transpiration_acc   = 0.0_wp 
    REAL(wp), SAVE                        :: interception_acc    = 0.0_wp 
    REAL(wp), SAVE                        :: evapopot_acc        = 0.0_wp 
    REAL(wp), SAVE                        :: srf_runoff_acc      = 0.0_wp 
    REAL(wp), SAVE                        :: drainage_acc        = 0.0_wp 
    REAL(wp), SAVE                        :: gw_runoff_acc       = 0.0_wp
    REAL(wp), SAVE                        :: gc_acc              = 0.0_wp
    REAL(wp), SAVE                        :: ga_acc              = 0.0_wp
    REAL(wp), SAVE                        :: n_deposition_acc    = 0.0_wp
    REAL(wp), SAVE                        :: n_deposition_n15_acc = 0.0_wp
    REAL(wp), SAVE                        :: ppfd_acc            = 0.0_wp
    REAL(wp), SAVE                        :: rnet_acc            = 0.0_wp
    REAL(wp), SAVE                        :: qh_acc              = 0.0_wp
    REAL(wp), SAVE                        :: qle_acc             = 0.0_wp
    REAL(wp), SAVE                        :: qg_acc              = 0.0_wp
    REAL(wp), SAVE                        :: albedo_acc          = 0.0_wp
    REAL(wp), SAVE                        :: appfd_acc           = 0.0_wp
    !kschufft
    REAL(wp), SAVE                        :: exudationC_base_acc = 0.0_wp
    REAL(wp), SAVE                        :: exudation_n_acc     = 0.0_wp
    REAL(wp), SAVE                        :: exudation_p_acc     = 0.0_wp
    ! veg_growth veg_litterfall fluxes CNP
    REAL(wp), SAVE                        :: growth_c_acc        = 0.0_wp   ! sum of all tissues, but seed_bed
    REAL(wp), SAVE                        :: growth_n_acc        = 0.0_wp   ! sum of all tissues, but seed_bed
    REAL(wp), SAVE                        :: growth_p_acc        = 0.0_wp   ! sum of all tissues, but seed_bed
    REAL(wp), SAVE    :: veg_growth_leaf_c_acc                  = 0.0_wp
    REAL(wp), SAVE    :: veg_growth_fine_root_c_acc             = 0.0_wp
    REAL(wp), SAVE    :: veg_growth_coarse_root_c_acc           = 0.0_wp
    REAL(wp), SAVE    :: veg_growth_sap_wood_c_acc              = 0.0_wp
    REAL(wp), SAVE    :: veg_growth_heart_wood_c_acc            = 0.0_wp
    REAL(wp), SAVE    :: veg_growth_labile_c_acc                = 0.0_wp
    REAL(wp), SAVE    :: veg_growth_reserve_c_acc               = 0.0_wp
    REAL(wp), SAVE    :: veg_growth_fruit_c_acc                 = 0.0_wp
    REAL(wp), SAVE    :: veg_growth_seed_bed_c_acc              = 0.0_wp
    REAL(wp), SAVE    :: veg_growth_leaf_n_acc                  = 0.0_wp
    REAL(wp), SAVE    :: veg_growth_fine_root_n_acc             = 0.0_wp
    REAL(wp), SAVE    :: veg_growth_coarse_root_n_acc           = 0.0_wp
    REAL(wp), SAVE    :: veg_growth_sap_wood_n_acc              = 0.0_wp
    REAL(wp), SAVE    :: veg_growth_heart_wood_n_acc            = 0.0_wp
    REAL(wp), SAVE    :: veg_growth_labile_n_acc                = 0.0_wp
    REAL(wp), SAVE    :: veg_growth_reserve_n_acc               = 0.0_wp
    REAL(wp), SAVE    :: veg_growth_fruit_n_acc                 = 0.0_wp
    REAL(wp), SAVE    :: veg_growth_seed_bed_n_acc              = 0.0_wp
    REAL(wp), SAVE    :: veg_growth_leaf_p_acc                  = 0.0_wp
    REAL(wp), SAVE    :: veg_growth_fine_root_p_acc             = 0.0_wp
    REAL(wp), SAVE    :: veg_growth_coarse_root_p_acc           = 0.0_wp
    REAL(wp), SAVE    :: veg_growth_sap_wood_p_acc              = 0.0_wp
    REAL(wp), SAVE    :: veg_growth_heart_wood_p_acc            = 0.0_wp
    REAL(wp), SAVE    :: veg_growth_labile_p_acc                = 0.0_wp
    REAL(wp), SAVE    :: veg_growth_reserve_p_acc               = 0.0_wp
    REAL(wp), SAVE    :: veg_growth_fruit_p_acc                 = 0.0_wp
    REAL(wp), SAVE    :: veg_growth_seed_bed_p_acc              = 0.0_wp
    REAL(wp), SAVE    :: veg_litterfall_leaf_c_acc              = 0.0_wp
    REAL(wp), SAVE    :: veg_litterfall_fine_root_c_acc         = 0.0_wp
    REAL(wp), SAVE    :: veg_litterfall_coarse_root_c_acc       = 0.0_wp
    REAL(wp), SAVE    :: veg_litterfall_sap_wood_c_acc          = 0.0_wp
    REAL(wp), SAVE    :: veg_litterfall_heart_wood_c_acc        = 0.0_wp
    REAL(wp), SAVE    :: veg_litterfall_labile_c_acc            = 0.0_wp
    REAL(wp), SAVE    :: veg_litterfall_reserve_c_acc           = 0.0_wp
    REAL(wp), SAVE    :: veg_litterfall_fruit_c_acc             = 0.0_wp
    REAL(wp), SAVE    :: veg_litterfall_seed_bed_c_acc          = 0.0_wp
    REAL(wp), SAVE    :: veg_litterfall_leaf_n_acc              = 0.0_wp
    REAL(wp), SAVE    :: veg_litterfall_fine_root_n_acc         = 0.0_wp
    REAL(wp), SAVE    :: veg_litterfall_coarse_root_n_acc       = 0.0_wp
    REAL(wp), SAVE    :: veg_litterfall_sap_wood_n_acc          = 0.0_wp
    REAL(wp), SAVE    :: veg_litterfall_heart_wood_n_acc        = 0.0_wp
    REAL(wp), SAVE    :: veg_litterfall_labile_n_acc            = 0.0_wp
    REAL(wp), SAVE    :: veg_litterfall_reserve_n_acc           = 0.0_wp
    REAL(wp), SAVE    :: veg_litterfall_fruit_n_acc             = 0.0_wp
    REAL(wp), SAVE    :: veg_litterfall_seed_bed_n_acc          = 0.0_wp
    REAL(wp), SAVE    :: veg_litterfall_leaf_p_acc              = 0.0_wp
    REAL(wp), SAVE    :: veg_litterfall_fine_root_p_acc         = 0.0_wp
    REAL(wp), SAVE    :: veg_litterfall_coarse_root_p_acc       = 0.0_wp
    REAL(wp), SAVE    :: veg_litterfall_sap_wood_p_acc          = 0.0_wp
    REAL(wp), SAVE    :: veg_litterfall_heart_wood_p_acc        = 0.0_wp
    REAL(wp), SAVE    :: veg_litterfall_labile_p_acc            = 0.0_wp
    REAL(wp), SAVE    :: veg_litterfall_reserve_p_acc           = 0.0_wp
    REAL(wp), SAVE    :: veg_litterfall_fruit_p_acc             = 0.0_wp
    REAL(wp), SAVE    :: veg_litterfall_seed_bed_p_acc          = 0.0_wp
    REAL(wp), SAVE                        :: growth_swood_c13_acc = 0.0_wp
    REAL(wp), SAVE                        :: growth_swood_c14_acc = 0.0_wp
    REAL(wp), SAVE                        :: growth_swood_n15_acc = 0.0_wp
    ! soil - local accumulation variables (partly also used for land output)
    REAL(wp), SAVE                        :: litterfallC_acc     = 0.0_wp
    REAL(wp), SAVE                        :: litterfallC13_acc   = 0.0_wp
    REAL(wp), SAVE                        :: litterfallC14_acc   = 0.0_wp
    REAL(wp), SAVE                        :: litterfallN_acc     = 0.0_wp
    REAL(wp), SAVE                        :: litterfallP_acc     = 0.0_wp
    REAL(wp), SAVE                        :: het_respC_acc       = 0.0_wp
    REAL(wp), SAVE                        :: het_respC13_acc     = 0.0_wp
    REAL(wp), SAVE                        :: het_respC14_acc     = 0.0_wp
    REAL(wp), SAVE                        :: leachingCorg_acc    = 0.0_wp
    REAL(wp), SAVE                        :: leachingC13org_acc  = 0.0_wp
    REAL(wp), SAVE                        :: leachingC14org_acc  = 0.0_wp
    REAL(wp), SAVE                        :: leachingNorg_acc    = 0.0_wp
    REAL(wp), SAVE                        :: leachingPorg_acc    = 0.0_wp
    REAL(wp), SAVE                        :: lateral_loss_Corg_acc    = 0.0_wp 
    REAL(wp), SAVE                        :: lateral_loss_C13org_acc  = 0.0_wp 
    REAL(wp), SAVE                        :: lateral_loss_C14org_acc  = 0.0_wp 
    REAL(wp), SAVE                        :: lateral_loss_Norg_acc    = 0.0_wp 
    REAL(wp), SAVE                        :: lateral_loss_Porg_acc    = 0.0_wp 
    REAL(wp), SAVE                        :: net_min_nh4_acc     = 0.0_wp
    REAL(wp), SAVE                        :: net_min_no3_acc     = 0.0_wp
    REAL(wp), SAVE                        :: net_min_po4_acc     = 0.0_wp
    REAL(wp), SAVE                        :: nitrification_acc   = 0.0_wp
    REAL(wp), SAVE                        :: denitrification_acc = 0.0_wp
    REAL(wp), SAVE                        :: fast_exchange_nh4_acc  = 0.0_wp
    REAL(wp), SAVE                        :: emission_noy_n15_acc = 0.0_wp
    REAL(wp), SAVE                        :: emission_noy_acc    = 0.0_wp
    REAL(wp), SAVE                        :: emission_n2o_acc    = 0.0_wp
    REAL(wp), SAVE                        :: emission_n2o_n15_acc = 0.0_wp
    REAL(wp), SAVE                        :: emission_n2_acc     = 0.0_wp
    REAL(wp), SAVE                        :: emission_n2_n15_acc = 0.0_wp
    REAL(wp), SAVE                        :: leaching_nh4_acc    = 0.0_wp
    REAL(wp), SAVE                        :: leaching_nh4_n15_acc = 0.0_wp
    REAL(wp), SAVE                        :: leaching_no3_acc    = 0.0_wp
    REAL(wp), SAVE                        :: leaching_no3_n15_acc = 0.0_wp
    REAL(wp), SAVE                        :: p_deposition_acc = 0.0_wp
    REAL(wp), SAVE                        :: leaching_po4_acc    = 0.0_wp
    REAL(wp), SAVE                        :: lateral_loss_nh4_acc  = 0.0_wp 
    REAL(wp), SAVE                        :: lateral_loss_nh4_n15_acc = 0.0_wp
    REAL(wp), SAVE                        :: lateral_loss_no3_acc    = 0.0_wp
    REAL(wp), SAVE                        :: lateral_loss_no3_n15_acc = 0.0_wp 
    REAL(wp), SAVE                        :: lateral_loss_po4_acc    = 0.0_wp  
    REAL(wp), SAVE                        :: weathering_po4_acc    = 0.0_wp
    REAL(wp), SAVE                        :: occlusion_po4_acc    = 0.0_wp
    REAL(wp), SAVE                        :: slow_exchange_po4_acc    = 0.0_wp
    REAL(wp), SAVE                        :: fast_exchange_po4_acc  = 0.0_wp
    REAL(wp), SAVE                        :: biochem_mineralisation_po4_acc  = 0.0_wp
    REAL(wp), SAVE                        :: asymb_n_fixation_acc = 0.0_wp
    REAL(wp), SAVE                        :: asymb_n_fixation_n15_acc = 0.0_wp
    REAL(wp), SAVE                        :: sb_delta_pools_carbon_acc     = 0.0_wp
    REAL(wp), SAVE                        :: sb_delta_pools_nitrogen_acc   = 0.0_wp
    REAL(wp), SAVE                        :: sb_delta_pools_phosphorus_acc = 0.0_wp
    REAL(wp), SAVE                        :: sb_delta_pools_carbon13_acc   = 0.0_wp
    REAL(wp), SAVE                        :: sb_delta_pools_carbon14_acc   = 0.0_wp
    REAL(wp), SAVE                        :: sb_delta_pools_nitrogen15_acc = 0.0_wp
    REAL(wp), SAVE                        :: sb_total_flux_carbon_acc      = 0.0_wp
    REAL(wp), SAVE                        :: sb_total_flux_nitrogen_acc    = 0.0_wp
    REAL(wp), SAVE                        :: sb_total_flux_phosphorus_acc  = 0.0_wp
    REAL(wp), SAVE                        :: sb_total_flux_carbon13_acc    = 0.0_wp
    REAL(wp), SAVE                        :: sb_total_flux_carbon14_acc    = 0.0_wp
    REAL(wp), SAVE                        :: sb_total_flux_nitrogen15_acc  = 0.0_wp
    REAL(wp), SAVE                        :: myc_export_c_acc              = 0.0_wp
    REAL(wp), SAVE                        :: myc_export_n_acc              = 0.0_wp
    REAL(wp), SAVE                        :: myc_export_p_acc              = 0.0_wp
    REAL(wp), SAVE                        :: myc_export_c13_acc            = 0.0_wp
    REAL(wp), SAVE                        :: myc_export_c14_acc            = 0.0_wp
    REAL(wp), SAVE                        :: myc_export_n15_acc            = 0.0_wp
    REAL(wp), SAVE                        :: myc_uptake_nh4_acc            = 0.0_wp
    REAL(wp), SAVE                        :: myc_uptake_no3_acc            = 0.0_wp
    REAL(wp), SAVE                        :: myc_uptake_norg_acc           = 0.0_wp
    REAL(wp), SAVE                        :: myc_respiration_c_acc         = 0.0_wp
    REAL(wp), SAVE                        :: f_cm_som_acc                  = 0.0_wp
    !kschufft
    REAL(wp), SAVE                        :: flux_soluable_2_dom_nitrogen_acc  = 0.0_wp
    REAL(wp), SAVE                        :: flux_residue_2_dom_nitrogen_acc  = 0.0_wp
    REAL(wp), SAVE                        :: flux_poly_2_dom_nitrogen_acc  = 0.0_wp
    REAL(wp), SAVE                        :: flux_micturn_2_dom_nitrogen_acc = 0.0_wp
    REAL(wp), SAVE                        :: flux_micturn_2_res_nitrogen_acc  = 0.0_wp
    REAL(wp), SAVE                        :: flux_dom_2_microbes_nitrogen_acc = 0.0_wp
    REAL(wp), SAVE                        :: graz_min_nh4_acc                 = 0.0_wp
    REAL(wp), SAVE                        :: graz_min_nh4_n15_acc             = 0.0_wp
    REAL(wp), SAVE                        :: graz_min_po4_acc                 = 0.0_wp

    REAL(wp), SAVE                        :: flux_soluable_2_dom_C_acc  = 0.0_wp
    REAL(wp), SAVE                        :: flux_residue_2_dom_C_acc  = 0.0_wp
    REAL(wp), SAVE                        :: flux_poly_2_dom_C_acc  = 0.0_wp
    REAL(wp), SAVE                        :: flux_micturn_2_dom_C_acc = 0.0_wp
    REAL(wp), SAVE                        :: flux_micturn_2_res_C_acc  = 0.0_wp
    REAL(wp), SAVE                        :: flux_dom_2_microbes_C_acc = 0.0_wp
    
    REAL(wp), SAVE                        :: flux_soluable_2_dom_P_acc  = 0.0_wp
    REAL(wp), SAVE                        :: flux_residue_2_dom_P_acc  = 0.0_wp
    REAL(wp), SAVE                        :: flux_poly_2_dom_P_acc  = 0.0_wp
    REAL(wp), SAVE                        :: flux_micturn_2_dom_P_acc = 0.0_wp
    REAL(wp), SAVE                        :: flux_micturn_2_res_P_acc  = 0.0_wp
    REAL(wp), SAVE                        :: flux_dom_2_microbes_P_acc = 0.0_wp
    ! atmospheric forcing   (note: 'precip_acc' is already calculated for vegfluxH2O output)
    REAL(wp), SAVE                        :: t_air_acc                     = 0.0_wp
    REAL(wp), SAVE                        :: wind_air_acc                  = 0.0_wp
    REAL(wp), SAVE                        :: swvis_srf_down_acc            = 0.0_wp
    REAL(wp), SAVE                        :: swnir_srf_down_acc            = 0.0_wp
    REAL(wp), SAVE                        :: lw_srf_down_acc               = 0.0_wp
    REAL(wp), SAVE                        :: vpd_acc                       = 0.0_wp
    REAL(wp), SAVE                        :: co2_mixing_ratio_acc          = 0.0_wp
    REAL(wp), SAVE                        :: co2_mixing_ratio_C13_acc      = 0.0_wp
    REAL(wp), SAVE                        :: co2_mixing_ratio_C14_acc      = 0.0_wp
    ! variables with accumulated information per soil layer
    ! hardcoded Dimension, using max soil layer number of 20 (which actually is the max soil layer number)
    REAL(wp), DIMENSION(max_soil_layers), SAVE :: &
                                             ! C
                                             het_respiration_lay_acc = 0.0_wp       , &
                                             het_respiration_c13_lay_acc = 0.0_wp   , &
                                             het_respiration_c14_lay_acc = 0.0_wp   , &
                                             transport_DOC_lay_acc = 0.0_wp         , &
                                             transport_Res_lay_acc = 0.0_wp         , & !kschufft
                                             transport_aRes_lay_acc = 0.0_wp        , & !kschufft
                                             root_respiration_lay_acc = 0.0_wp      , &
                                             exudation_c_lay_acc      = 0.0_wp      , &
                                             lateral_loss_Corg_lay_acc = 0.0_wp     , & 
                                             ! N
                                             net_mineral_nh4_lay_acc = 0.0_wp       , &
                                             plant_uptake_nh4_lay_acc = 0.0_wp      , &
                                             microbial_upt_nh4_lay_acc = 0.0_wp     , &
                                             microbial_upt_no3_lay_acc = 0.0_wp     , &
                                             myc_uptake_nh4_lay_acc = 0.0_wp        , &
                                             asymb_n_fixation_lay_acc = 0.0_wp      , &
                                             net_mineral_no3_lay_acc = 0.0_wp       , &
                                             plant_uptake_no3_lay_acc = 0.0_wp      , &
                                             myc_uptake_no3_lay_acc = 0.0_wp        , &
                                             myc_uptake_norg_lay_acc = 0.0_wp       , &
                                             transport_solute_nh4_acc = 0.0_wp      , &
                                             transport_nh4_assoc_acc = 0.0_wp       , &
                                             transport_solute_no3_acc = 0.0_wp      , &
                                             transport_DON_lay_acc = 0.0_wp         , &
                                             f_adsorpt_nh4_lay_acc = 0.0_wp         , &  
                                             nitrification_lay_acc = 0.0_wp         , &  
                                             denitrification_lay_acc = 0.0_wp       , &  
                                             lateral_loss_nh4_lay_acc = 0.0_wp      , & 
                                             lateral_loss_no3_lay_acc = 0.0_wp      , & 
                                             lateral_loss_Norg_lay_acc = 0.0_wp     , &
                                             graz_min_nh4_lay_acc      = 0.0_wp     ,& !kschufft
                                             graz_min_po4_lay_acc      = 0.0_wp     ,&
                                             ! P
                                             f_exchange_po4_acc = 0.0_wp        , &  
                                             f_adsorpt_po4_acc = 0.0_wp         , & 
                                             net_min_po4_lay_acc = 0.0_wp       , &
                                             ocl_po4_acc = 0.0_wp               , &
                                             weath_po4_acc = 0.0_wp             , &
                                             biochem_min_po4_acc = 0.0_wp       , &
                                             s_exchange_po4_acc = 0.0_wp        , &
                                             plant_upt_po4_acc = 0.0_wp         , &
                                             microbial_upt_po4_acc = 0.0_wp     , &
                                             myc_upt_po4_acc = 0.0_wp           , &
                                             transport_solute_po4_acc = 0.0_wp      , &
                                             transport_assoc_fast_po4_acc = 0.0_wp  , &
                                             transport_assoc_slow_po4_acc = 0.0_wp  , &
                                             transport_occluded_po4_acc = 0.0_wp    , &
                                             transport_primary_po4_acc = 0.0_wp     , &
                                             transport_DOP_lay_acc = 0.0_wp         , &
                                             lateral_loss_po4_lay_acc = 0.0_wp      , & 
                                             lateral_loss_Porg_lay_acc = 0.0_wp     , &
                                                                                          !kschufft
                                             ae_carbon_acc = 0.0_wp                 ,&
                                             ae_nitrogen_acc = 0.0_wp               ,&
                                             ae_phosphorus_acc = 0.0_wp             ,&
                                             flux_soluable_2_dom_nitrogen_lay_acc = 0.0_wp, &
                                             flux_residue_2_dom_nitrogen_lay_acc = 0.0_wp, &
                                             flux_poly_2_dom_nitrogen_lay_acc = 0.0_wp, &
                                             flux_micturn_2_dom_nitrogen_lay_acc = 0.0_wp, &
                                             flux_micturn_2_res_nitrogen_lay_acc = 0.0_wp, &
                                             flux_dom_2_microbes_nitrogen_lay_acc = 0.0_wp, &
                                             flux_soluable_2_dom_C_lay_acc = 0.0_wp, &
                                             flux_residue_2_dom_C_lay_acc = 0.0_wp, &
                                             flux_poly_2_dom_C_lay_acc = 0.0_wp, &
                                             flux_micturn_2_dom_C_lay_acc = 0.0_wp, &
                                             flux_micturn_2_res_C_lay_acc = 0.0_wp, &
                                             flux_dom_2_microbes_C_lay_acc = 0.0_wp, &
                                             flux_soluable_2_dom_P_lay_acc = 0.0_wp, &
                                             flux_residue_2_dom_P_lay_acc = 0.0_wp, &
                                             flux_poly_2_dom_P_lay_acc = 0.0_wp, &
                                             flux_micturn_2_dom_P_lay_acc = 0.0_wp, &
                                             flux_micturn_2_res_P_lay_acc = 0.0_wp, &
                                             flux_dom_2_microbes_P_lay_acc = 0.0_wp
    REAL(wp), ALLOCATABLE, DIMENSION(:,:) :: hlp1_2d
    INTEGER                               :: lun                        !< locical unit number
    REAL(wp)                              :: hlp1
    REAL(wp)                              :: dC13gpp, dC13rg, dC13rm, dC13wgr
    REAL(wp)                              :: dC13litterfall, dC13het_resp, dC13leaching, dC13lateral_loss 
    REAL(wp)                              :: dC13atm
    REAL(wp)                              :: mean_sw_down
    REAL(wp)                              :: dtime_one
    REAL(wp)                              :: dtime                !< timestep lengt
    REAL(wp)                              :: f_veg_flux_output
    INTEGER                               :: iblk, ics, ice, nc, ic, is
    CHARACTER(len=*), PARAMETER :: routine = TRIM(modname)//':write_output_flux'
    ! ----------------------------------------------------------------------------------------------------- !
    dsl4jsb_Def_mt2L2D :: veg_pool_mt
    dsl4jsb_Def_mt2L2D :: veg_litterfall_mt
    dsl4jsb_Def_mt2L2D :: veg_growth_mt
    dsl4jsb_Def_mt1L2D :: veg_exudation_mt
    dsl4jsb_Def_mt1L2D :: veg_reserve_use_mt
    dsl4jsb_Def_mt2L3D :: sb_transport_mt
    dsl4jsb_Def_mt1L3D :: sb_mycorrhiza_export_mt
    dsl4jsb_Def_mt1L3D :: sb_delta_pools_mt
    ! ----------------------------------------------------------------------------------------------------- !
    dsl4jsb_Def_config(SB_)
    dsl4jsb_Def_memory(A2L_)
    dsl4jsb_Def_memory(Q_ASSIMI_)
    dsl4jsb_Def_memory(VEG_)
    dsl4jsb_Def_memory(Q_RAD_)
    dsl4jsb_Def_memory(SPQ_)
    dsl4jsb_Def_memory(SB_)
    ! ----------------------------------------------------------------------------------------------------- !
    ! A2L_
    dsl4jsb_Real2D_onChunk      :: swpar_srf_down
    dsl4jsb_Real2D_onChunk      :: nhx_deposition
    dsl4jsb_Real2D_onChunk      :: noy_deposition
    dsl4jsb_Real2D_onChunk      :: nhx_n15_deposition
    dsl4jsb_Real2D_onChunk      :: noy_n15_deposition
    dsl4jsb_Real2D_onChunk      :: p_deposition
    dsl4jsb_Real2D_onChunk      :: rain 
    dsl4jsb_Real2D_onChunk      :: snow
    dsl4jsb_Real2D_onChunk      :: t_air
    dsl4jsb_Real2D_onChunk      :: wind_air
    dsl4jsb_Real2D_onChunk      :: swvis_srf_down
    dsl4jsb_Real2D_onChunk      :: swnir_srf_down
    dsl4jsb_Real2D_onChunk      :: lw_srf_down
    dsl4jsb_Real2D_onChunk      :: co2_mixing_ratio
    dsl4jsb_Real2D_onChunk      :: co2_mixing_ratio_C13
    dsl4jsb_Real2D_onChunk      :: co2_mixing_ratio_C14
    dsl4jsb_Real2D_onChunk      :: press_srf
    dsl4jsb_Real2D_onChunk      :: q_air
    ! Q_ASSIMI_
    dsl4jsb_Real2D_onChunk      :: gross_assimilation
    dsl4jsb_Real2D_onChunk      :: gross_assimilation_C13
    dsl4jsb_Real2D_onChunk      :: gross_assimilation_C14
    dsl4jsb_Real2D_onChunk      :: canopy_cond
    dsl4jsb_Real2D_onChunk      :: aerodyn_cond
    ! SPQ_ 2D
    dsl4jsb_Real2D_onChunk      :: drainage
    dsl4jsb_Real2D_onChunk      :: gw_runoff
    dsl4jsb_Real2D_onChunk      :: srf_runoff 
    dsl4jsb_Real2D_onChunk      :: evaporation 
    dsl4jsb_Real2D_onChunk      :: transpiration
    dsl4jsb_Real2D_onChunk      :: sensible_heat_flx
    dsl4jsb_Real2D_onChunk      :: latent_heat_flx
    dsl4jsb_Real2D_onChunk      :: ground_heat_flx
    dsl4jsb_Real2D_onChunk      :: num_sl_above_bedrock
    ! SPQ_ 3D
    dsl4jsb_Real3D_onChunk      :: soil_depth_sl
    ! Q_RAD_
    dsl4jsb_Real2D_onChunk      :: albedo
    dsl4jsb_Real2D_onChunk      :: albedo_noon
    dsl4jsb_Real2D_onChunk      :: rad_srf_net
    dsl4jsb_Real2D_onChunk      :: appfd
    dsl4jsb_Real2D_onChunk      :: alb_vis
    dsl4jsb_Real2D_onChunk      :: alb_nir
    ! VEG_ 2D
    dsl4jsb_Real2D_onChunk      :: growth_respiration
    dsl4jsb_Real2D_onChunk      :: maint_respiration
    dsl4jsb_Real2D_onChunk      :: fmaint_rate_root
    dsl4jsb_Real2D_onChunk      :: n_transform_respiration
    dsl4jsb_Real2D_onChunk      :: n_fixation_respiration
    dsl4jsb_Real2D_onChunk      :: npp
    dsl4jsb_Real2D_onChunk      :: growth_respiration_c13
    dsl4jsb_Real2D_onChunk      :: maint_respiration_c13
    dsl4jsb_Real2D_onChunk      :: growth_respiration_c14
    dsl4jsb_Real2D_onChunk      :: maint_respiration_c14
    dsl4jsb_Real2D_onChunk      :: uptake_nh4
    dsl4jsb_Real2D_onChunk      :: uptake_nh4_n15
    dsl4jsb_Real2D_onChunk      :: uptake_no3
    dsl4jsb_Real2D_onChunk      :: uptake_no3_n15
    dsl4jsb_Real2D_onChunk      :: recycling_leaf_n
    dsl4jsb_Real2D_onChunk      :: recycling_fine_root_n
    dsl4jsb_Real2D_onChunk      :: recycling_heart_wood_n
    dsl4jsb_Real2D_onChunk      :: n_fixation
    dsl4jsb_Real2D_onChunk      :: n_fixation_n15
    dsl4jsb_Real2D_onChunk      :: uptake_po4
    dsl4jsb_Real2D_onChunk      :: recycling_leaf_p
    dsl4jsb_Real2D_onChunk      :: recycling_fine_root_p
    dsl4jsb_Real2D_onChunk      :: recycling_heart_wood_p
    dsl4jsb_Real2D_onChunk      :: interception 
    !kschufft
    dsl4jsb_Real2D_onCHunk      :: exudationC_base
    dsl4jsb_Real2D_onChunk      :: evapopot
    ! VEG_ 3D
    dsl4jsb_Real3D_onChunk      :: root_fraction_sl
    ! SB_ 2D
    dsl4jsb_Real2D_onChunk      :: total_flux_carbon
    dsl4jsb_Real2D_onChunk      :: total_flux_nitrogen
    dsl4jsb_Real2D_onChunk      :: total_flux_phosphorus
    dsl4jsb_Real2D_onChunk      :: total_flux_carbon13
    dsl4jsb_Real2D_onChunk      :: total_flux_carbon14
    dsl4jsb_Real2D_onChunk      :: total_flux_nitrogen15
    dsl4jsb_Real2D_onChunk      :: emission_noy
    dsl4jsb_Real2D_onChunk      :: emission_noy_n15
    dsl4jsb_Real2D_onChunk      :: emission_n2o
    dsl4jsb_Real2D_onChunk      :: emission_n2o_n15
    dsl4jsb_Real2D_onChunk      :: emission_n2
    dsl4jsb_Real2D_onChunk      :: emission_n2_n15
    dsl4jsb_Real2D_onChunk      :: leaching_dom_carbon
    dsl4jsb_Real2D_onChunk      :: leaching_dom_nitrogen
    dsl4jsb_Real2D_onChunk      :: leaching_dom_phosphorus
    dsl4jsb_Real2D_onChunk      :: leaching_dom_carbon13
    dsl4jsb_Real2D_onChunk      :: leaching_dom_carbon14
    dsl4jsb_Real2D_onChunk      :: leaching_dom_nitrogen15
    dsl4jsb_Real2D_onChunk      :: leaching_nh4_solute
    dsl4jsb_Real2D_onChunk      :: leaching_nh4_n15_solute
    dsl4jsb_Real2D_onChunk      :: leaching_no3_solute
    dsl4jsb_Real2D_onChunk      :: leaching_no3_n15_solute
    dsl4jsb_Real2D_onChunk      :: leaching_po4_solute
    dsl4jsb_Real3D_onChunk      :: transport_nh4_solute 
    dsl4jsb_Real3D_onChunk      :: transport_nh4_assoc 
    dsl4jsb_Real3D_onChunk      :: transport_no3_solute
    ! SB_ 3D
    dsl4jsb_Real3D_onChunk      :: het_respiration
    dsl4jsb_Real3D_onChunk      :: het_respiration_c13
    dsl4jsb_Real3D_onChunk      :: het_respiration_c14
    dsl4jsb_Real3D_onChunk      :: net_mineralisation_nh4
    dsl4jsb_Real3D_onChunk      :: net_mineralisation_no3
    dsl4jsb_Real3D_onChunk      :: net_mineralisation_po4
    dsl4jsb_Real3D_onChunk      :: nitrification_no3
    dsl4jsb_Real3D_onChunk      :: nitrification_noy
    dsl4jsb_Real3D_onChunk      :: nitrification_n2o
    dsl4jsb_Real3D_onChunk      :: denitrification_n2
    dsl4jsb_Real3D_onChunk      :: denitrification_noy
    dsl4jsb_Real3D_onChunk      :: denitrification_n2o
    dsl4jsb_Real3D_onChunk      :: asymb_n_fixation 
    dsl4jsb_Real3D_onChunk      :: asymb_n_fixation_n15 
    dsl4jsb_Real3D_onChunk      :: weathering_po4
    dsl4jsb_Real3D_onChunk      :: occlusion_po4
    dsl4jsb_Real3D_onChunk      :: slow_exchange_po4
    dsl4jsb_Real3D_onChunk      :: fast_exchange_po4
    dsl4jsb_Real3D_onChunk      :: fast_adsorpt_po4
    dsl4jsb_Real3D_onChunk      :: fast_adsorpt_nh4
    dsl4jsb_Real3D_onChunk      :: fast_exchange_nh4
    dsl4jsb_Real3D_onChunk      :: biochem_mineralisation_po4
    dsl4jsb_Real3D_onChunk      :: plant_uptake_nh4_sl
    dsl4jsb_Real3D_onChunk      :: plant_uptake_no3_sl
    dsl4jsb_Real3D_onChunk      :: plant_uptake_po4_sl
    dsl4jsb_Real3D_onChunk      :: mycorrhiza_uptake_nh4_sl
    dsl4jsb_Real3D_onChunk      :: mycorrhiza_uptake_no3_sl
    dsl4jsb_Real3D_onChunk      :: mycorrhiza_uptake_norg_sl
    dsl4jsb_Real3D_onChunk      :: mycorrhiza_uptake_po4_sl
    dsl4jsb_Real3D_onChunk      :: microbial_uptake_nh4_sl
    dsl4jsb_Real3D_onChunk      :: microbial_uptake_no3_sl
    dsl4jsb_Real3D_onChunk      :: microbial_uptake_po4_sl
    dsl4jsb_Real3D_onChunk      :: transport_po4_solute
    dsl4jsb_Real3D_onChunk      :: transport_po4_assoc_fast
    dsl4jsb_Real3D_onChunk      :: transport_po4_assoc_slow
    dsl4jsb_Real3D_onChunk      :: transport_po4_occluded
    dsl4jsb_Real3D_onChunk      :: transport_po4_primary
    dsl4jsb_Real3D_onChunk      :: myc_respiration
    dsl4jsb_Real3D_onChunk      :: lateral_loss_dom_carbon_sl
    dsl4jsb_Real3D_onChunk      :: lateral_loss_dom_nitrogen_sl
    dsl4jsb_Real3D_onChunk      :: lateral_loss_dom_phosphorus_sl
    dsl4jsb_Real3D_onChunk      :: lateral_loss_dom_carbon13_sl
    dsl4jsb_Real3D_onChunk      :: lateral_loss_dom_carbon14_sl
    dsl4jsb_Real3D_onChunk      :: lateral_loss_dom_nitrogen15_sl
    dsl4jsb_Real3D_onChunk      :: lateral_loss_nh4_solute_sl
    dsl4jsb_Real3D_onChunk      :: lateral_loss_nh4_n15_solute_sl
    dsl4jsb_Real3D_onChunk      :: lateral_loss_no3_solute_sl
    dsl4jsb_Real3D_onChunk      :: lateral_loss_no3_n15_solute_sl
    dsl4jsb_Real3D_onChunk      :: lateral_loss_po4_solute_sl
        !kschufft
    dsl4jsb_Real3D_onChunk      :: ae_carbon
    dsl4jsb_Real3D_onChunk      :: ae_nitrogen
    dsl4jsb_Real3D_onChunk      :: ae_phosphorus
    dsl4jsb_Real3D_onChunk      :: residue_depolymerisation_rate
    dsl4jsb_Real3D_onChunk      :: flux_soluable_2_dom_nitrogen
    dsl4jsb_Real3D_onChunk      :: flux_residue_2_dom_nitrogen
    dsl4jsb_Real3D_onChunk      :: flux_poly_2_dom_nitrogen
    dsl4jsb_Real3D_onChunk      :: flux_micturn_2_dom_nitrogen
    dsl4jsb_Real3D_onChunk      :: flux_micturn_2_res_nitrogen 
    dsl4jsb_Real3D_onChunk      :: flux_dom_2_microbes_nitrogen
    dsl4jsb_Real3D_onChunk      :: flux_soluable_2_dom_C
    dsl4jsb_Real3D_onChunk      :: flux_residue_2_dom_C
    dsl4jsb_Real3D_onChunk      :: flux_poly_2_dom_C
    dsl4jsb_Real3D_onChunk      :: flux_micturn_2_dom_C
    dsl4jsb_Real3D_onChunk      :: flux_micturn_2_res_C 
    dsl4jsb_Real3D_onChunk      :: flux_dom_2_microbes_C
    dsl4jsb_Real3D_onChunk      :: flux_soluable_2_dom_P
    dsl4jsb_Real3D_onChunk      :: flux_residue_2_dom_P
    dsl4jsb_Real3D_onChunk      :: flux_poly_2_dom_P
    dsl4jsb_Real3D_onChunk      :: flux_micturn_2_dom_P
    dsl4jsb_Real3D_onChunk      :: flux_micturn_2_res_P
    dsl4jsb_Real3D_onChunk      :: flux_dom_2_microbes_P
    dsl4jsb_Real3D_onChunk      :: grazing_mineralisation_nh4
    dsl4jsb_Real3D_onChunk      :: grazing_mineralisation_nh4_n15
    dsl4jsb_Real3D_onChunk      :: grazing_mineralisation_po4
    ! ----------------------------------------------------------------------------------------------------- !
    iblk     = options%iblk
    ics      = options%ics
    ice      = options%ice
    nc       = options%nc
    dtime    = options%dtime
    ! ----------------------------------------------------------------------------------------------------- !
    IF (debug_on() .AND. iblk == 1) CALL message(TRIM(routine), 'Starting on tile '//TRIM(tile%name)//' ...')
    ! ----------------------------------------------------------------------------------------------------- !
    model         => Get_model(tile%owner_model_id)
    lctlib        => model%lctlib(tile%lcts(1)%lib_id)
    vgrid_soil_sb => Get_vgrid('soil_layer_sb')
    nsoil_sb      =  vgrid_soil_sb%n_levels
    ! ----------------------------------------------------------------------------------------------------- !
    dsl4jsb_Get_config(SB_)
    dsl4jsb_Get_memory(A2L_) 
    dsl4jsb_Get_memory(Q_ASSIMI_)
    dsl4jsb_Get_memory(VEG_)
    dsl4jsb_Get_memory(Q_RAD_)
    dsl4jsb_Get_memory(SPQ_)
    dsl4jsb_Get_memory(SB_)
    ! ----------------------------------------------------------------------------------------------------- !
    ALLOCATE(hlp1_2d(nc, nsoil_sb))
    ! ----------------------------------------------------------------------------------------------------- !
    bgcm_store => tile%bgcm_store
    dsl4jsb_Get_mt2L2D(VEG_BGCM_POOL_ID, veg_pool_mt)
    dsl4jsb_Get_mt2L2D(VEG_BGCM_LITTERFALL_ID, veg_litterfall_mt)
    dsl4jsb_Get_mt2L2D(VEG_BGCM_GROWTH_ID, veg_growth_mt)
    dsl4jsb_Get_mt1L2D(VEG_BGCM_EXUDATION_ID, veg_exudation_mt)
    dsl4jsb_Get_mt1L2D(VEG_BGCM_RESERVE_USE_ID, veg_reserve_use_mt)
    dsl4jsb_Get_mt2L3D(SB_BGCM_TRANSPORT_ID, sb_transport_mt)
    dsl4jsb_Get_mt1L3D(SB_BGCM_MYCO_EXPORT_ID, sb_mycorrhiza_export_mt)
    dsl4jsb_Get_mt1L3D(SB_BGCM_DELTA_POOLS_ID, sb_delta_pools_mt)
    ! ----------------------------------------------------------------------------------------------------- !
    dsl4jsb_Get_var2D_onChunk(A2L_, swpar_srf_down)
    dsl4jsb_Get_var2D_onChunk(A2L_, nhx_deposition)
    dsl4jsb_Get_var2D_onChunk(A2L_, noy_deposition)
    dsl4jsb_Get_var2D_onChunk(A2L_, nhx_n15_deposition)
    dsl4jsb_Get_var2D_onChunk(A2L_, noy_n15_deposition)
    dsl4jsb_Get_var2D_onChunk(A2L_, p_deposition)
    dsl4jsb_Get_var2D_onChunk(A2L_, rain)
    dsl4jsb_Get_var2D_onChunk(A2L_, snow)
    dsl4jsb_Get_var2D_onChunk(A2L_, t_air)
    dsl4jsb_Get_var2D_onChunk(A2L_, wind_air)
    dsl4jsb_Get_var2D_onChunk(A2L_, swvis_srf_down)
    dsl4jsb_Get_var2D_onChunk(A2L_, swnir_srf_down)
    dsl4jsb_Get_var2D_onChunk(A2L_, lw_srf_down)
    dsl4jsb_Get_var2D_onChunk(A2L_, co2_mixing_ratio)
    dsl4jsb_Get_var2D_onChunk(A2L_, co2_mixing_ratio_C13)
    dsl4jsb_Get_var2D_onChunk(A2L_, co2_mixing_ratio_C14)
    dsl4jsb_Get_var2D_onChunk(A2L_, press_srf)
    dsl4jsb_Get_var2D_onChunk(A2L_, q_air)
    ! ---------------------------
    dsl4jsb_Get_var2D_onChunk(Q_ASSIMI_, gross_assimilation)
    dsl4jsb_Get_var2D_onChunk(Q_ASSIMI_, gross_assimilation_C13)
    dsl4jsb_Get_var2D_onChunk(Q_ASSIMI_, gross_assimilation_C14)
    dsl4jsb_Get_var2D_onChunk(Q_ASSIMI_, canopy_cond)
    dsl4jsb_Get_var2D_onChunk(Q_ASSIMI_, aerodyn_cond)
    ! ---------------------------
    ! SPQ_ 2D
    dsl4jsb_Get_var2D_onChunk(SPQ_, evaporation)
    dsl4jsb_Get_var2D_onChunk(SPQ_, interception)
    dsl4jsb_Get_var2D_onChunk(SPQ_, evapopot)
    dsl4jsb_Get_var2D_onChunk(SPQ_, srf_runoff)
    dsl4jsb_Get_var2D_onChunk(SPQ_, drainage)
    dsl4jsb_Get_var2D_onChunk(SPQ_, gw_runoff)
    dsl4jsb_Get_var2D_onChunk(SPQ_, transpiration)
    dsl4jsb_Get_var2D_onChunk(SPQ_, sensible_heat_flx)
    dsl4jsb_Get_var2D_onChunk(SPQ_, latent_heat_flx)
    dsl4jsb_Get_var2D_onChunk(SPQ_, ground_heat_flx)
    dsl4jsb_Get_var2D_onChunk(SPQ_, num_sl_above_bedrock)
    ! SPQ_ 3D
    dsl4jsb_Get_var3D_onChunk(SPQ_, soil_depth_sl)
    ! ---------------------------
    dsl4jsb_Get_var2D_onChunk(Q_RAD_, albedo)
    dsl4jsb_Get_var2D_onChunk(Q_RAD_, albedo_noon)
    dsl4jsb_Get_var2D_onChunk(Q_RAD_, rad_srf_net)
    dsl4jsb_Get_var2D_onChunk(Q_RAD_, appfd)
    dsl4jsb_Get_var2D_onChunk(Q_RAD_, alb_vis)
    dsl4jsb_Get_var2D_onChunk(Q_RAD_, alb_nir)
    ! ---------------------------
    ! VEG_ 2D
    dsl4jsb_Get_var2D_onChunk(VEG_, growth_respiration)
    dsl4jsb_Get_var2D_onChunk(VEG_, maint_respiration)
    dsl4jsb_Get_var2D_onChunk(VEG_, fmaint_rate_root)
    dsl4jsb_Get_var2D_onChunk(VEG_, n_transform_respiration)
    dsl4jsb_Get_var2D_onChunk(VEG_, n_fixation_respiration)
    dsl4jsb_Get_var2D_onChunk(VEG_, npp)
    dsl4jsb_Get_var2D_onChunk(VEG_, growth_respiration_c13)
    dsl4jsb_Get_var2D_onChunk(VEG_, maint_respiration_c13)
    dsl4jsb_Get_var2D_onChunk(VEG_, growth_respiration_c14)
    dsl4jsb_Get_var2D_onChunk(VEG_, maint_respiration_c14)
    dsl4jsb_Get_var2D_onChunk(VEG_, uptake_nh4)
    dsl4jsb_Get_var2D_onChunk(VEG_, uptake_nh4_n15)
    dsl4jsb_Get_var2D_onChunk(VEG_, uptake_no3)
    dsl4jsb_Get_var2D_onChunk(VEG_, uptake_no3_n15)
    dsl4jsb_Get_var2D_onChunk(VEG_, recycling_leaf_n)
    dsl4jsb_Get_var2D_onChunk(VEG_, recycling_fine_root_n)
    dsl4jsb_Get_var2D_onChunk(VEG_, recycling_heart_wood_n)
    dsl4jsb_Get_var2D_onChunk(VEG_, n_fixation)
    dsl4jsb_Get_var2D_onChunk(VEG_, n_fixation_n15)
    dsl4jsb_Get_var2D_onChunk(VEG_, uptake_po4)
    dsl4jsb_Get_var2D_onChunk(VEG_, recycling_leaf_p)
    dsl4jsb_Get_var2D_onChunk(VEG_, recycling_fine_root_p)
    dsl4jsb_Get_var2D_onChunk(VEG_, recycling_heart_wood_p)
     !kschufft
    dsl4jsb_Get_var2D_onChunk(VEG_, exudationC_base) 
    ! VEG_ 3D
    dsl4jsb_Get_var3D_onChunk(VEG_, root_fraction_sl)
    ! ---------------------------
    ! SB_ 2D
    dsl4jsb_Get_var2D_onChunk(SB_,  total_flux_carbon)    
    dsl4jsb_Get_var2D_onChunk(SB_,  total_flux_nitrogen)  
    dsl4jsb_Get_var2D_onChunk(SB_,  total_flux_phosphorus)
    dsl4jsb_Get_var2D_onChunk(SB_,  total_flux_carbon13)  
    dsl4jsb_Get_var2D_onChunk(SB_,  total_flux_carbon14)  
    dsl4jsb_Get_var2D_onChunk(SB_,  total_flux_nitrogen15)
    dsl4jsb_Get_var2D_onChunk(SB_,  emission_noy)
    dsl4jsb_Get_var2D_onChunk(SB_,  emission_noy_n15)
    dsl4jsb_Get_var2D_onChunk(SB_,  emission_n2o)
    dsl4jsb_Get_var2D_onChunk(SB_,  emission_n2o_n15)
    dsl4jsb_Get_var2D_onChunk(SB_,  emission_n2)
    dsl4jsb_Get_var2D_onChunk(SB_,  emission_n2_n15)
    dsl4jsb_Get_var2D_onChunk(SB_,  leaching_dom_carbon)
    dsl4jsb_Get_var2D_onChunk(SB_,  leaching_dom_nitrogen)
    dsl4jsb_Get_var2D_onChunk(SB_,  leaching_dom_phosphorus)
    dsl4jsb_Get_var2D_onChunk(SB_,  leaching_dom_carbon13)
    dsl4jsb_Get_var2D_onChunk(SB_,  leaching_dom_carbon14)
    dsl4jsb_Get_var2D_onChunk(SB_,  leaching_dom_nitrogen15)
    dsl4jsb_Get_var2D_onChunk(SB_,  leaching_nh4_solute)
    dsl4jsb_Get_var2D_onChunk(SB_,  leaching_nh4_n15_solute)
    dsl4jsb_Get_var2D_onChunk(SB_,  leaching_no3_solute)
    dsl4jsb_Get_var2D_onChunk(SB_,  leaching_no3_n15_solute)
    dsl4jsb_Get_var2D_onChunk(SB_,  leaching_po4_solute)
    ! SB_ 3D
    dsl4jsb_Get_var3D_onChunk(SB_,  transport_nh4_solute)
    dsl4jsb_Get_var3D_onChunk(SB_,  transport_nh4_assoc)
    dsl4jsb_Get_var3D_onChunk(SB_,  transport_no3_solute)
    dsl4jsb_Get_var3D_onChunk(SB_,  lateral_loss_dom_carbon_sl)
    dsl4jsb_Get_var3D_onChunk(SB_,  lateral_loss_dom_nitrogen_sl)
    dsl4jsb_Get_var3D_onChunk(SB_,  lateral_loss_dom_phosphorus_sl) 
    dsl4jsb_Get_var3D_onChunk(SB_,  lateral_loss_dom_carbon13_sl) 
    dsl4jsb_Get_var3D_onChunk(SB_,  lateral_loss_dom_carbon14_sl) 
    dsl4jsb_Get_var3D_onChunk(SB_,  lateral_loss_dom_nitrogen15_sl) 
    dsl4jsb_Get_var3D_onChunk(SB_,  lateral_loss_nh4_solute_sl) 
    dsl4jsb_Get_var3D_onChunk(SB_,  lateral_loss_nh4_n15_solute_sl) 
    dsl4jsb_Get_var3D_onChunk(SB_,  lateral_loss_no3_solute_sl) 
    dsl4jsb_Get_var3D_onChunk(SB_,  lateral_loss_no3_n15_solute_sl) 
    dsl4jsb_Get_var3D_onChunk(SB_,  lateral_loss_po4_solute_sl) 
    dsl4jsb_Get_var3D_onChunk(SB_,  het_respiration)
    dsl4jsb_Get_var3D_onChunk(SB_,  het_respiration_c13)
    dsl4jsb_Get_var3D_onChunk(SB_,  het_respiration_c14)
    dsl4jsb_Get_var3D_onChunk(SB_,  net_mineralisation_nh4)
    dsl4jsb_Get_var3D_onChunk(SB_,  net_mineralisation_no3)
    dsl4jsb_Get_var3D_onChunk(SB_,  net_mineralisation_po4)
    dsl4jsb_Get_var3D_onChunk(SB_,  nitrification_no3)
    dsl4jsb_Get_var3D_onChunk(SB_,  nitrification_noy)
    dsl4jsb_Get_var3D_onChunk(SB_,  nitrification_n2o)
    dsl4jsb_Get_var3D_onChunk(SB_,  denitrification_n2)
    dsl4jsb_Get_var3D_onChunk(SB_,  denitrification_noy)
    dsl4jsb_Get_var3D_onChunk(SB_,  denitrification_n2o)
    dsl4jsb_Get_var3D_onChunk(SB_,  asymb_n_fixation)
    dsl4jsb_Get_var3D_onChunk(SB_,  asymb_n_fixation_n15)
    dsl4jsb_Get_var3D_onChunk(SB_,  slow_exchange_po4)
    dsl4jsb_Get_var3D_onChunk(SB_,  fast_exchange_po4)
    dsl4jsb_Get_var3D_onChunk(SB_,  fast_adsorpt_po4)
    dsl4jsb_Get_var3D_onChunk(SB_,  fast_adsorpt_nh4)
    dsl4jsb_Get_var3D_onChunk(SB_,  fast_exchange_nh4)
    dsl4jsb_Get_var3D_onChunk(SB_,  weathering_po4)
    dsl4jsb_Get_var3D_onChunk(SB_,  occlusion_po4)
    dsl4jsb_Get_var3D_onChunk(SB_,  biochem_mineralisation_po4)
    dsl4jsb_Get_var3D_onChunk(SB_,  plant_uptake_nh4_sl)
    dsl4jsb_Get_var3D_onChunk(SB_,  plant_uptake_no3_sl)
    dsl4jsb_Get_var3D_onChunk(SB_,  plant_uptake_po4_sl)
    dsl4jsb_Get_var3D_onChunk(SB_,  mycorrhiza_uptake_nh4_sl)
    dsl4jsb_Get_var3D_onChunk(SB_,  mycorrhiza_uptake_no3_sl)
    dsl4jsb_Get_var3D_onChunk(SB_,  mycorrhiza_uptake_norg_sl)
    dsl4jsb_Get_var3D_onChunk(SB_,  mycorrhiza_uptake_po4_sl)
    dsl4jsb_Get_var3D_onChunk(SB_,  microbial_uptake_nh4_sl)
    dsl4jsb_Get_var3D_onChunk(SB_,  microbial_uptake_no3_sl)
    dsl4jsb_Get_var3D_onChunk(SB_,  microbial_uptake_po4_sl)
    dsl4jsb_Get_var3D_onChunk(SB_,  transport_po4_solute)
    dsl4jsb_Get_var3D_onChunk(SB_,  transport_po4_assoc_fast)
    dsl4jsb_Get_var3D_onChunk(SB_,  transport_po4_assoc_slow)
    dsl4jsb_Get_var3D_onChunk(SB_,  transport_po4_occluded)
    dsl4jsb_Get_var3D_onChunk(SB_,  transport_po4_primary)
    dsl4jsb_Get_var3D_onChunk(SB_,  myc_respiration)
        !kschufft
    dsl4jsb_Get_var3D_onChunk(SB_,  ae_carbon)
    dsl4jsb_Get_var3D_onChunk(SB_,  ae_nitrogen)
    dsl4jsb_Get_var3D_onChunk(SB_,  ae_phosphorus)
    dsl4jsb_Get_var3D_onChunk(SB_,  flux_soluable_2_dom_nitrogen)
    dsl4jsb_Get_var3D_onChunk(SB_,  flux_poly_2_dom_nitrogen)
    dsl4jsb_Get_var3D_onChunk(SB_,  flux_residue_2_dom_nitrogen)
    dsl4jsb_Get_var3D_onChunk(SB_,  flux_micturn_2_dom_nitrogen)
    dsl4jsb_Get_var3D_onChunk(SB_,  flux_micturn_2_res_nitrogen)
    dsl4jsb_Get_var3D_onChunk(SB_,  flux_dom_2_microbes_nitrogen)
    dsl4jsb_Get_var3D_onChunk(SB_,  grazing_mineralisation_nh4)
    dsl4jsb_Get_var3D_onChunk(SB_,  grazing_mineralisation_nh4_n15)
    dsl4jsb_Get_var3D_onChunk(SB_,  grazing_mineralisation_po4)
    dsl4jsb_Get_var3D_onChunk(SB_,  flux_soluable_2_dom_C)
    dsl4jsb_Get_var3D_onChunk(SB_,  flux_poly_2_dom_C)
    dsl4jsb_Get_var3D_onChunk(SB_,  flux_residue_2_dom_C)
    dsl4jsb_Get_var3D_onChunk(SB_,  flux_micturn_2_dom_C)
    dsl4jsb_Get_var3D_onChunk(SB_,  flux_micturn_2_res_C)
    dsl4jsb_Get_var3D_onChunk(SB_,  flux_dom_2_microbes_C)
    dsl4jsb_Get_var3D_onChunk(SB_,  flux_soluable_2_dom_P)
    dsl4jsb_Get_var3D_onChunk(SB_,  flux_poly_2_dom_P)
    dsl4jsb_Get_var3D_onChunk(SB_,  flux_residue_2_dom_P)
    dsl4jsb_Get_var3D_onChunk(SB_,  flux_micturn_2_dom_P)
    dsl4jsb_Get_var3D_onChunk(SB_,  flux_micturn_2_res_P)
    dsl4jsb_Get_var3D_onChunk(SB_,  flux_dom_2_microbes_P)
    ! ----------------------------------------------------------------------------------------------------- !

    !> 0.9 the point in ics:ice of which data will be written to file; per default = 1
    !>
    !> is actually defined and declared globally in his module above, but ifort complains anyway
    !>
    ic = 1

    !>1.0 modify 'dtime_one' factor accordingly to output interval
    !> 
    ! accumulate fluxes across the timesteps of one interval: \n
    !  for timestep-interval leave it to seconds (which is the basic timestep for fluxes), i.e. multiply by 1 \n
    !  for any interval larger timestep multiply by dtime_one
    IF(output_interval_flux < 1.0_wp) THEN
      dtime_one = 1.0_wp                 ! interval = one timestep
    ELSE
      dtime_one = dtime / 1000000.0_wp   ! interval > one timestep
    END IF
    ! veg_flux output factor [veg_flux * f_veg_flux_output]
    f_veg_flux_output = dtime_one * 1000000._wp / dtime

    !>2.0 output
    !> 
    ! land/plant/canopy
    ! land/soil  (simple_1d/jsm)
    ! 
    ! set accumulation to TRUE always, because this routine is called only if it is TRUE
    ! unless this routine is called only to "set to zero all the *_acc variables"
    l_flux_calc_acc_output = .TRUE.
    IF (l_zero_acc_variables) THEN
      l_flux_calc_acc_output = .FALSE.
    ENDIF
    ! set starting-lun for write
    lun = lun_ascii_fluxes_start    !< max of 30 files for pool output (LUN 100 - 129)
    !
    SELECT CASE(model%config%qmodel_id)
    CASE(QLAND, QPLANT, QCANOPY)
      !! accumulate fluxes across the timesteps of one interval
      !! 
      IF(l_flux_calc_acc_output) THEN
        ! timestep counter for averaging
        ndat_acc             = ndat_acc              + 1.0_wp
        ! Vegetation
        gpp_acc              = gpp_acc               + gross_assimilation(ic)        * dtime_one
        growth_resp_acc      = growth_resp_acc       + growth_respiration(ic)        * dtime_one
        maint_resp_acc       = maint_resp_acc        + maint_respiration(ic)         * dtime_one
        n_transform_resp_acc = n_transform_resp_acc  + n_transform_respiration(ic)   * dtime_one
        n_fixation_resp_acc  = n_fixation_resp_acc   + n_fixation_respiration(ic)    * dtime_one
        exudation_c_acc      = exudation_c_acc       + veg_exudation_mt(ixC, ic)      * dtime_one *1000000._wp/dtime   
        exudationC_base_acc  = exudationC_base_acc   + exudationC_base(ic)            * dtime_one *1000000._wp/dtime   
        npp_acc              = npp_acc               + npp(ic)                       * dtime_one
        res_use_c_acc        = res_use_c_acc         + veg_reserve_use_mt(ixC, ic)    * dtime_one *1000000._wp/dtime
        growth_c_acc         = growth_c_acc          + (veg_growth_mt(ix_leaf, ixC, ic)        + & 
                                                        veg_growth_mt(ix_fine_root, ixC, ic)   + &
                                                        veg_growth_mt(ix_coarse_root, ixC, ic) + & 
                                                        veg_growth_mt(ix_sap_wood, ixC, ic)    + & 
                                                        veg_growth_mt(ix_heart_wood, ixC, ic)  + & 
                                                        veg_growth_mt(ix_labile, ixC, ic)      + & 
                                                        veg_growth_mt(ix_reserve, ixC, ic)     + & 
                                                        veg_growth_mt(ix_fruit, ixC, ic)) * dtime_one *1000000._wp/dtime
        growth_swood_c13_acc = growth_swood_c13_acc  + veg_growth_mt(ix_sap_wood, ixC13, ic) * dtime_one *1000000._wp/dtime
        growth_swood_c14_acc = growth_swood_c14_acc  + veg_growth_mt(ix_sap_wood, ixC14, ic) * dtime_one *1000000._wp/dtime
        
        veg_growth_leaf_c_acc            = veg_growth_leaf_c_acc            &
                                           & + veg_growth_mt(ix_leaf, ixC, ic) * f_veg_flux_output
        veg_growth_fine_root_c_acc       = veg_growth_fine_root_c_acc       &
                                           & + veg_growth_mt(ix_fine_root, ixC, ic) * f_veg_flux_output
        veg_growth_coarse_root_c_acc     = veg_growth_coarse_root_c_acc     &
                                           & + veg_growth_mt(ix_coarse_root, ixC, ic) * f_veg_flux_output
        veg_growth_sap_wood_c_acc        = veg_growth_sap_wood_c_acc        &
                                           & + veg_growth_mt(ix_sap_wood, ixC, ic) * f_veg_flux_output
        veg_growth_heart_wood_c_acc      = veg_growth_heart_wood_c_acc      &
                                           & + veg_growth_mt(ix_heart_wood, ixC, ic) * f_veg_flux_output
        veg_growth_labile_c_acc          = veg_growth_labile_c_acc          &
                                           & + veg_growth_mt(ix_labile, ixC, ic) * f_veg_flux_output
        veg_growth_reserve_c_acc         = veg_growth_reserve_c_acc         &
                                           & + veg_growth_mt(ix_reserve, ixC, ic) * f_veg_flux_output
        veg_growth_fruit_c_acc           = veg_growth_fruit_c_acc           &
                                           & + veg_growth_mt(ix_fruit, ixC, ic) * f_veg_flux_output
        veg_growth_seed_bed_c_acc        = veg_growth_seed_bed_c_acc        &
                                           & + veg_growth_mt(ix_seed_bed, ixC, ic) * f_veg_flux_output
                                           !kschufft
        exudation_n_acc                  = exudation_n_acc + veg_exudation_mt(ixN, ic) * dtime_one *1000000._wp/dtime

        veg_growth_leaf_n_acc            = veg_growth_leaf_n_acc            &
                                           & + veg_growth_mt(ix_leaf, ixN, ic) * f_veg_flux_output
        veg_growth_fine_root_n_acc       = veg_growth_fine_root_n_acc       &
                                           & + veg_growth_mt(ix_fine_root, ixN, ic) * f_veg_flux_output
        veg_growth_coarse_root_n_acc     = veg_growth_coarse_root_n_acc     &
                                           & + veg_growth_mt(ix_coarse_root, ixN, ic) * f_veg_flux_output
        veg_growth_sap_wood_n_acc        = veg_growth_sap_wood_n_acc        &
                                           & + veg_growth_mt(ix_sap_wood, ixN, ic) * f_veg_flux_output
        veg_growth_heart_wood_n_acc      = veg_growth_heart_wood_n_acc      &
                                           & + veg_growth_mt(ix_heart_wood, ixN, ic) * f_veg_flux_output
        veg_growth_labile_n_acc          = veg_growth_labile_n_acc          &
                                           & + veg_growth_mt(ix_labile, ixN, ic) * f_veg_flux_output
        veg_growth_reserve_n_acc         = veg_growth_reserve_n_acc         &
                                           & + veg_growth_mt(ix_reserve, ixN, ic) * f_veg_flux_output
        veg_growth_fruit_n_acc           = veg_growth_fruit_n_acc           &
                                           & + veg_growth_mt(ix_fruit, ixN, ic) * f_veg_flux_output
        veg_growth_seed_bed_n_acc        = veg_growth_seed_bed_n_acc        &
                                           & + veg_growth_mt(ix_seed_bed, ixN, ic) * f_veg_flux_output
        exudation_p_acc                  = exudation_p_acc + veg_exudation_mt(ixP, ic) * dtime_one *1000000._wp/dtime

        veg_growth_leaf_p_acc            = veg_growth_leaf_p_acc            &
                                           & + veg_growth_mt(ix_leaf, ixP, ic) * f_veg_flux_output
        veg_growth_fine_root_p_acc       = veg_growth_fine_root_p_acc       &
                                           & + veg_growth_mt(ix_fine_root, ixP, ic) * f_veg_flux_output
        veg_growth_coarse_root_p_acc     = veg_growth_coarse_root_p_acc     &
                                           & + veg_growth_mt(ix_coarse_root, ixP, ic) * f_veg_flux_output
        veg_growth_sap_wood_p_acc        = veg_growth_sap_wood_p_acc        &
                                           & + veg_growth_mt(ix_sap_wood, ixP, ic) * f_veg_flux_output
        veg_growth_heart_wood_p_acc      = veg_growth_heart_wood_p_acc      &
                                           & + veg_growth_mt(ix_heart_wood, ixP, ic) * f_veg_flux_output
        veg_growth_labile_p_acc          = veg_growth_labile_p_acc          &
                                           & + veg_growth_mt(ix_labile, ixP, ic) * f_veg_flux_output
        veg_growth_reserve_p_acc         = veg_growth_reserve_p_acc         &
                                           & + veg_growth_mt(ix_reserve, ixP, ic) * f_veg_flux_output
        veg_growth_fruit_p_acc           = veg_growth_fruit_p_acc           &
                                           & + veg_growth_mt(ix_fruit, ixP, ic) * f_veg_flux_output
        veg_growth_seed_bed_p_acc        = veg_growth_seed_bed_p_acc        &
                                           & + veg_growth_mt(ix_seed_bed, ixP, ic) * f_veg_flux_output
        veg_litterfall_leaf_c_acc        = veg_litterfall_leaf_c_acc        &
                                           & + veg_litterfall_mt(ix_leaf, ixC, ic) * f_veg_flux_output
        veg_litterfall_fine_root_c_acc   = veg_litterfall_fine_root_c_acc   &
                                           & + veg_litterfall_mt(ix_fine_root, ixC, ic) * f_veg_flux_output
        veg_litterfall_coarse_root_c_acc = veg_litterfall_coarse_root_c_acc &
                                           & + veg_litterfall_mt(ix_coarse_root, ixC, ic) * f_veg_flux_output
        veg_litterfall_sap_wood_c_acc    = veg_litterfall_sap_wood_c_acc    &
                                           & + veg_litterfall_mt(ix_sap_wood, ixC, ic) * f_veg_flux_output
        veg_litterfall_heart_wood_c_acc  = veg_litterfall_heart_wood_c_acc  &
                                           & + veg_litterfall_mt(ix_heart_wood, ixC, ic) * f_veg_flux_output
        veg_litterfall_labile_c_acc      = veg_litterfall_labile_c_acc      &
                                           & + veg_litterfall_mt(ix_labile, ixC, ic) * f_veg_flux_output
        veg_litterfall_reserve_c_acc     = veg_litterfall_reserve_c_acc     &
                                           & + veg_litterfall_mt(ix_reserve, ixC, ic) * f_veg_flux_output
        veg_litterfall_fruit_c_acc       = veg_litterfall_fruit_c_acc       &
                                           & + veg_litterfall_mt(ix_fruit, ixC, ic) * f_veg_flux_output
        veg_litterfall_seed_bed_c_acc    = veg_litterfall_seed_bed_c_acc    &
                                           & + veg_litterfall_mt(ix_seed_bed, ixC, ic) * f_veg_flux_output
        veg_litterfall_leaf_n_acc        = veg_litterfall_leaf_n_acc        &
                                           & + veg_litterfall_mt(ix_leaf, ixN, ic) * f_veg_flux_output
        veg_litterfall_fine_root_n_acc   = veg_litterfall_fine_root_n_acc   &
                                           & + veg_litterfall_mt(ix_fine_root, ixN, ic) * f_veg_flux_output
        veg_litterfall_coarse_root_n_acc = veg_litterfall_coarse_root_n_acc &
                                           & + veg_litterfall_mt(ix_coarse_root, ixN, ic) * f_veg_flux_output
        veg_litterfall_sap_wood_n_acc    = veg_litterfall_sap_wood_n_acc    &
                                           & + veg_litterfall_mt(ix_sap_wood, ixN, ic) * f_veg_flux_output
        veg_litterfall_heart_wood_n_acc  = veg_litterfall_heart_wood_n_acc  &
                                           & + veg_litterfall_mt(ix_heart_wood, ixN, ic) * f_veg_flux_output
        veg_litterfall_labile_n_acc      = veg_litterfall_labile_n_acc      &
                                           & + veg_litterfall_mt(ix_labile, ixN, ic) * f_veg_flux_output
        veg_litterfall_reserve_n_acc     = veg_litterfall_reserve_n_acc     &
                                           & + veg_litterfall_mt(ix_reserve, ixN, ic) * f_veg_flux_output
        veg_litterfall_fruit_n_acc       = veg_litterfall_fruit_n_acc       &
                                           & + veg_litterfall_mt(ix_fruit, ixN, ic) * f_veg_flux_output
        veg_litterfall_seed_bed_n_acc    = veg_litterfall_seed_bed_n_acc    &
                                           & + veg_litterfall_mt(ix_seed_bed, ixN, ic) * f_veg_flux_output
        veg_litterfall_leaf_p_acc        = veg_litterfall_leaf_p_acc        &
                                           & + veg_litterfall_mt(ix_leaf, ixP, ic) * f_veg_flux_output
        veg_litterfall_fine_root_p_acc   = veg_litterfall_fine_root_p_acc   &
                                           & + veg_litterfall_mt(ix_fine_root, ixP, ic) * f_veg_flux_output
        veg_litterfall_coarse_root_p_acc = veg_litterfall_coarse_root_p_acc &
                                           & + veg_litterfall_mt(ix_coarse_root, ixP, ic) * f_veg_flux_output
        veg_litterfall_sap_wood_p_acc    = veg_litterfall_sap_wood_p_acc    &
                                           & + veg_litterfall_mt(ix_sap_wood, ixP, ic) * f_veg_flux_output
        veg_litterfall_heart_wood_p_acc  = veg_litterfall_heart_wood_p_acc  &
                                           & + veg_litterfall_mt(ix_heart_wood, ixP, ic) * f_veg_flux_output
        veg_litterfall_labile_p_acc      = veg_litterfall_labile_p_acc      &
                                           & + veg_litterfall_mt(ix_labile, ixP, ic) * f_veg_flux_output
        veg_litterfall_reserve_p_acc     = veg_litterfall_reserve_p_acc     &
                                           & + veg_litterfall_mt(ix_reserve, ixP, ic) * f_veg_flux_output
        veg_litterfall_fruit_p_acc       = veg_litterfall_fruit_p_acc       &
                                           & + veg_litterfall_mt(ix_fruit, ixP, ic) * f_veg_flux_output
        veg_litterfall_seed_bed_p_acc    = veg_litterfall_seed_bed_p_acc    &
                                           & + veg_litterfall_mt(ix_seed_bed, ixP, ic) * f_veg_flux_output
        
        gpp_C13_acc          = gpp_C13_acc           + gross_assimilation_C13(ic)    * dtime_one
        growth_resp_C13_acc  = growth_resp_C13_acc   + growth_respiration_c13(ic)    * dtime_one
        maint_resp_C13_acc   = maint_resp_C13_acc    + maint_respiration_c13(ic)     * dtime_one
        gpp_C14_acc          = gpp_C14_acc           + gross_assimilation_C14(ic)    * dtime_one
        growth_resp_C14_acc  = growth_resp_C14_acc   + growth_respiration_c14(ic)    * dtime_one
        maint_resp_C14_acc   = maint_resp_C14_acc    + maint_respiration_c14(ic)     * dtime_one
        uptake_nh4_acc       = uptake_nh4_acc        + uptake_nh4(ic)                * dtime_one
        uptake_nh4_n15_acc   = uptake_nh4_n15_acc    + uptake_nh4_n15(ic)            * dtime_one
        uptake_no3_acc       = uptake_no3_acc        + uptake_no3(ic)                * dtime_one
        uptake_no3_n15_acc   = uptake_no3_n15_acc    + uptake_no3_n15(ic)            * dtime_one
        recycling_n_acc      = recycling_n_acc       + ( recycling_leaf_n(ic) + recycling_fine_root_n(ic) + &
                                                                                  recycling_heart_wood_n(ic)) * &
                                                                                         dtime_one *1000000._wp/dtime
        res_use_n_acc        = res_use_n_acc         + veg_reserve_use_mt(ixN, ic)  * dtime_one *1000000._wp/dtime
        n_fixation_acc       = n_fixation_acc        + n_fixation(ic)                * dtime_one 
        n_fixation_n15_acc   = n_fixation_n15_acc    + n_fixation_n15(ic)            * dtime_one
        growth_n_acc         = growth_n_acc          + (veg_growth_mt(ix_leaf, ixN, ic)        + & 
                                                        veg_growth_mt(ix_fine_root, ixN, ic)   + &
                                                        veg_growth_mt(ix_coarse_root, ixN, ic) + & 
                                                        veg_growth_mt(ix_sap_wood, ixN, ic)    + & 
                                                        veg_growth_mt(ix_heart_wood, ixN, ic)  + & 
                                                        veg_growth_mt(ix_labile, ixN, ic)      + & 
                                                        veg_growth_mt(ix_reserve, ixN, ic)     + & 
                                                        veg_growth_mt(ix_fruit, ixN, ic)) * dtime_one *1000000._wp/dtime
        growth_swood_n15_acc = growth_swood_n15_acc  + veg_growth_mt(ix_sap_wood, ixN15, ic) * dtime_one *1000000._wp/dtime
        uptake_po4_acc       = uptake_po4_acc        + uptake_po4(ic)                * dtime_one
        recycling_p_acc      = recycling_p_acc       + (recycling_leaf_p(ic) + recycling_fine_root_p(ic) + &
                                                        recycling_heart_wood_p(ic))  * dtime_one *1000000._wp/dtime
        growth_p_acc         = growth_p_acc          + (veg_growth_mt(ix_leaf, ixP, ic) + & 
                                                        veg_growth_mt(ix_fine_root, ixP, ic)   + &
                                                        veg_growth_mt(ix_coarse_root, ixP, ic) + & 
                                                        veg_growth_mt(ix_sap_wood, ixP, ic)    + & 
                                                        veg_growth_mt(ix_heart_wood, ixP, ic)  + & 
                                                        veg_growth_mt(ix_labile, ixP, ic)      + & 
                                                        veg_growth_mt(ix_reserve, ixP, ic)     + & 
                                                        veg_growth_mt(ix_fruit, ixP, ic)) * dtime_one *1000000._wp/dtime     
        res_use_p_acc        = res_use_p_acc         + veg_reserve_use_mt(ixP, ic)  * dtime_one *1000000._wp/dtime
        IF(output_interval_flux < 1.0_wp) THEN  ! if interval == timestep (output_interval_flux is smaller 1.0 only for timestep interval)
           precip_acc       = precip_acc        + (rain(ic) + snow(ic))  
           interception_acc = interception_acc  + interception(ic)       
           evapopot_acc     = evapopot_acc      + evapopot(ic)       
           evaporation_acc  = evaporation_acc   + evaporation(ic)       
           transpiration_acc= transpiration_acc + transpiration(ic)      
           srf_runoff_acc   = srf_runoff_acc    + srf_runoff(ic)              
           drainage_acc     = drainage_acc      + drainage(ic)            
           gw_runoff_acc    = gw_runoff_acc     + gw_runoff(ic)
        ELSE
           precip_acc       = precip_acc        + (rain(ic) + snow(ic))          * dtime
           interception_acc = interception_acc  + interception(ic)                 * dtime 
           evapopot_acc     = evapopot_acc      + evapopot(ic)                     * dtime 
           evaporation_acc  = evaporation_acc   + evaporation(ic)                  * dtime
           transpiration_acc= transpiration_acc + transpiration(ic)                * dtime
           srf_runoff_acc   = srf_runoff_acc    + srf_runoff(ic)                   * dtime
           drainage_acc     = drainage_acc      + drainage(ic)                     * dtime
           gw_runoff_acc    = gw_runoff_acc     + gw_runoff(ic)                    * dtime
        ENDIF 
        gc_acc           = gc_acc            + canopy_cond(ic)                                                      
        ga_acc           = ga_acc            + aerodyn_cond(ic) 
        albedo_acc       = albedo_acc        + albedo(ic)
        ppfd_acc         = ppfd_acc          + swpar_srf_down(ic) * rad2ppfd  * dtime_one               
        rnet_acc         = rnet_acc          + rad_srf_net(ic)              * dtime_one               
        qh_acc           = qh_acc            - sensible_heat_flx(ic)          * dtime_one               
        qle_acc          = qle_acc           - latent_heat_flx(ic)            * dtime_one               
        qg_acc           = qg_acc            - ground_heat_flx(ic)            * dtime_one               
        appfd_acc        = appfd_acc         + appfd(ic)                      * dtime_one               
        ! Mycorrhiza
        myc_export_c_acc      = myc_export_c_acc      + SUM(sb_mycorrhiza_export_mt(ixC, ic, :) * &
                                                          soil_depth_sl(ic,:)) * dtime_one *1000000._wp/dtime
        myc_export_n_acc      = myc_export_n_acc      + SUM(sb_mycorrhiza_export_mt(ixN, ic, :) * &
                                                          soil_depth_sl(ic,:)) * dtime_one *1000000._wp/dtime
        myc_export_p_acc      = myc_export_p_acc      + SUM(sb_mycorrhiza_export_mt(ixP, ic, :) * &
                                                          soil_depth_sl(ic,:)) * dtime_one *1000000._wp/dtime
        myc_export_c13_acc    = myc_export_c13_acc    + SUM(sb_mycorrhiza_export_mt(ixC13, ic, :) * &
                                                          soil_depth_sl(ic,:)) * dtime_one *1000000._wp/dtime
        myc_export_c14_acc    = myc_export_c14_acc    + SUM(sb_mycorrhiza_export_mt(ixC14, ic, :) * &
                                                          soil_depth_sl(ic,:)) * dtime_one *1000000._wp/dtime
        myc_export_n15_acc    = myc_export_n15_acc    + SUM(sb_mycorrhiza_export_mt(ixN15, ic, :) * &
                                                          soil_depth_sl(ic,:)) * dtime_one *1000000._wp/dtime
        myc_respiration_c_acc = myc_respiration_c_acc + SUM(myc_respiration(ic,:) * soil_depth_sl(ic,:)) * &
                                molar_mass_C * dtime_one * 1000000._wp / dtime
        f_cm_som_acc          = f_cm_som_acc + organic_cn * molar_mass_C / molar_mass_N * &
                                SUM(mycorrhiza_uptake_norg_sl(ic,:) * soil_depth_sl(ic,:)) * &
                                dtime_one * 1000000._wp / dtime
        ! atmospheric forcing (A2L_)
        t_air_acc                = t_air_acc                + t_air(ic)
        wind_air_acc             = wind_air_acc             + wind_air(ic)
        swvis_srf_down_acc       = swvis_srf_down_acc       + swvis_srf_down(ic)
        swnir_srf_down_acc       = swnir_srf_down_acc       + swnir_srf_down(ic)
        lw_srf_down_acc          = lw_srf_down_acc          + lw_srf_down(ic)
        vpd_acc                  = vpd_acc + (( calc_spec_humidity_sat(t_air(ic),press_srf(ic)) - q_air(ic) ) / &
                                                eps_vpd * press_srf(ic) / 1000.0_wp)
        co2_mixing_ratio_acc     = co2_mixing_ratio_acc     + co2_mixing_ratio(ic)
        co2_mixing_ratio_C13_acc = co2_mixing_ratio_C13_acc + co2_mixing_ratio_C13(ic)
        co2_mixing_ratio_C14_acc = co2_mixing_ratio_C14_acc + co2_mixing_ratio_C14(ic)


        !> 1.2 write fluxes and set to zero when output interval is reached
        !>
        IF(l_flux_output_interval) THEN

          IF(ndat_acc > 1.0_wp) THEN
            gc_acc     = gc_acc / ndat_acc
            ga_acc     = ga_acc / ndat_acc
            albedo_acc = albedo_acc / ndat_acc
          ENDIF
    
          ! land
          SELECT CASE(model%config%qmodel_id)  ! cases: land & land/plant & canopy
          CASE(QLAND)
            ! mycfluxCNP
            write(lun,'(7F24.12)') time_d, & 
                                   myc_export_c_acc, myc_export_n_acc, myc_export_p_acc, &
                                   myc_export_c13_acc, myc_export_c14_acc, myc_export_n15_acc
            lun=lun+1
            ! myc_diagnostics
            write(lun,'(3F24.12)') time_d, myc_respiration_c_acc, f_cm_som_acc
            lun=lun+1
          END SELECT

          ! land & plant
          SELECT CASE(model%config%qmodel_id)
          CASE(QLAND,QPLANT)
            ! vegfluxN
            write(lun,'(31F24.8)') &
              & time_d, uptake_nh4_acc, uptake_no3_acc, recycling_n_acc, & 
              & res_use_n_acc, n_fixation_acc, & 
              & calc_delta_N15(n_fixation_acc, n_fixation_n15_acc), &
              & calc_delta_N15(uptake_nh4_acc, uptake_nh4_n15_acc), &
              & calc_delta_N15(uptake_no3_acc, uptake_no3_n15_acc), &
              & growth_n_acc, &
              & calc_delta_N15(veg_growth_sap_wood_n_acc, growth_swood_n15_acc), &
              & veg_growth_leaf_n_acc, veg_growth_fine_root_n_acc, &
              & veg_growth_coarse_root_n_acc, veg_growth_sap_wood_n_acc, &
              & veg_growth_heart_wood_n_acc, veg_growth_labile_n_acc, &
              & veg_growth_reserve_n_acc, veg_growth_fruit_n_acc, &
              & veg_growth_seed_bed_n_acc, &
              & veg_litterfall_leaf_n_acc, veg_litterfall_fine_root_n_acc, &
              & veg_litterfall_coarse_root_n_acc, veg_litterfall_sap_wood_n_acc, &
              & veg_litterfall_heart_wood_n_acc, veg_litterfall_labile_n_acc, &
              & veg_litterfall_reserve_n_acc, veg_litterfall_fruit_n_acc, &
              & veg_litterfall_seed_bed_n_acc, exudation_n_acc
            lun=lun+1
            ! vegfluxP
            write(lun,'(26F24.8)') &
              & time_d, uptake_po4_acc, recycling_p_acc, res_use_p_acc, growth_p_acc, &
              & veg_growth_leaf_p_acc, veg_growth_fine_root_p_acc, &
              & veg_growth_coarse_root_p_acc, veg_growth_sap_wood_p_acc, &
              & veg_growth_heart_wood_p_acc, veg_growth_labile_p_acc, &
              & veg_growth_reserve_p_acc, veg_growth_fruit_p_acc, &
              & veg_growth_seed_bed_p_acc, &
              & veg_litterfall_leaf_p_acc, veg_litterfall_fine_root_p_acc, &
              & veg_litterfall_coarse_root_p_acc, veg_litterfall_sap_wood_p_acc, &
              & veg_litterfall_heart_wood_p_acc, veg_litterfall_labile_p_acc, &
              & veg_litterfall_reserve_p_acc, veg_litterfall_fruit_p_acc, &
              & veg_litterfall_seed_bed_p_acc, exudation_p_acc
            lun=lun+1
          END SELECT


          ! land & plant & canopy
          SELECT CASE(model%config%qmodel_id)
          CASE(QLAND,QPLANT,QCANOPY)
            ! vegfluxC
            write(lun,'(31F24.8)') &
              & time_d, gpp_acc, growth_resp_acc, maint_resp_acc , &
              & n_transform_resp_acc,n_fixation_resp_acc         , & 
              & npp_acc, res_use_c_acc, exudation_c_acc, growth_c_acc, &
              & veg_growth_leaf_c_acc, veg_growth_fine_root_c_acc, &
              & veg_growth_coarse_root_c_acc, veg_growth_sap_wood_c_acc, &
              & veg_growth_heart_wood_c_acc, veg_growth_labile_c_acc, &
              & veg_growth_reserve_c_acc, veg_growth_fruit_c_acc, &
              & veg_growth_seed_bed_c_acc, &
              & veg_litterfall_leaf_c_acc, veg_litterfall_fine_root_c_acc, &
              & veg_litterfall_coarse_root_c_acc, veg_litterfall_sap_wood_c_acc, &
              & veg_litterfall_heart_wood_c_acc, veg_litterfall_labile_c_acc, &
              & veg_litterfall_reserve_c_acc, veg_litterfall_fruit_c_acc, &
              & veg_litterfall_seed_bed_c_acc, exudationC_base_acc
            lun=lun+1
            ! vegfluxdC13DC14
            dC13gpp = calc_delta_C13(gpp_acc,gpp_C13_acc)
            dC13rg  = calc_delta_C13(growth_resp_acc,growth_resp_C13_acc)
            dC13rm  = calc_delta_C13(maint_resp_acc,maint_resp_C13_acc) 
            dC13wgr = calc_delta_C13(veg_growth_sap_wood_c_acc, growth_swood_c13_acc)
            write(lun,'(9F18.6)') time_d, dC13gpp, dC13rg, dC13rm, dC13wgr                   , & 
                                  calc_delta_C14(gpp_acc,gpp_C14_acc,dC13gpp)                , & 
                                  calc_delta_C14(growth_resp_acc,growth_resp_C14_acc,dC13rg) , & 
                                  calc_delta_C14(maint_resp_acc,maint_resp_C14_acc,dC13rm)   , &
                                  calc_delta_C14(veg_growth_sap_wood_c_acc, growth_swood_c14_acc, growth_swood_c13_acc)
            lun=lun+1
            ! vegfluxH2O
            write(lun,'(12F18.8)') time_d, precip_acc, evapopot_acc, evaporation_acc, interception_acc, & 
                                   transpiration_acc, srf_runoff_acc, drainage_acc, gw_runoff_acc, gc_acc, ga_acc
            lun=lun+1
            IF (ppfd_acc > eps8) THEN
               hlp1 = appfd_acc / ppfd_acc
            ELSE
               hlp1 = 0.0_wp
            ENDIF
            ! vegfluxEnergy
            write(lun,'(12F18.8)') time_d, rnet_acc, qh_acc, qle_acc, -qg_acc, albedo_acc, & 
                                   albedo_noon, ppfd_acc, appfd_acc, hlp1, alb_vis(ic), alb_nir(ic)
            lun=lun+1
            ! atm_forcing
            IF(ndat_acc > 1.0_wp) THEN
              t_air_acc                = t_air_acc                / ndat_acc
              wind_air_acc             = wind_air_acc             / ndat_acc
              swvis_srf_down_acc       = swvis_srf_down_acc       / ndat_acc
              swnir_srf_down_acc       = swnir_srf_down_acc       / ndat_acc
              lw_srf_down_acc          = lw_srf_down_acc          / ndat_acc
              vpd_acc                  = vpd_acc                  / ndat_acc
              co2_mixing_ratio_acc     = co2_mixing_ratio_acc     / ndat_acc
              co2_mixing_ratio_C13_acc = co2_mixing_ratio_C13_acc / ndat_acc
              co2_mixing_ratio_C14_acc = co2_mixing_ratio_C14_acc / ndat_acc
            ENDIF
            mean_sw_down = swvis_srf_down_acc + swnir_srf_down_acc
            dC13atm = calc_delta_C13(co2_mixing_ratio_acc, co2_mixing_ratio_C13_acc) 
            write(lun,'(10F20.8)') time_d, precip_acc, t_air_acc, wind_air_acc, mean_sw_down, lw_srf_down_acc, &
                                   vpd_acc, co2_mixing_ratio_acc, dC13atm, &
                                   calc_delta_C14(co2_mixing_ratio_acc, co2_mixing_ratio_C14_acc, dC13atm)
            lun=lun+1
          END SELECT  ! select land/plant & canopy
        END IF ! l_flux_output_interval
      END IF   ! l_flux_calc_acc_output
      
      !! set all acc variables zero
      !! either if:
      !!   a) flux-output variables were written out, and thus need to be zero'ed
      !!   b) this routine is called only to zero the *_acc (flux-output variables) because, e.g., 
      !!        the output interval is changed and new output files are created (to delete potential prev. accumulated values)
      IF (l_flux_output_interval .OR. l_zero_acc_variables) THEN
        ndat_acc                   = 0.0_wp
        gpp_acc                    = 0.0_wp
        growth_resp_acc            = 0.0_wp
        maint_resp_acc             = 0.0_wp
        n_transform_resp_acc       = 0.0_wp
        n_fixation_resp_acc        = 0.0_wp
        exudation_c_acc            = 0.0_wp
         !kschufft
        exudationC_base_acc        = 0.0_wp
        exudation_n_acc            = 0.0_wp
        exudation_p_acc            = 0.0_wp
        npp_acc                    = 0.0_wp
        res_use_c_acc              = 0.0_wp
        growth_c_acc               = 0.0_wp
        growth_swood_c13_acc       = 0.0_wp
        growth_swood_c14_acc       = 0.0_wp
        veg_growth_leaf_c_acc              = 0.0_wp
        veg_growth_fine_root_c_acc         = 0.0_wp
        veg_growth_coarse_root_c_acc       = 0.0_wp
        veg_growth_sap_wood_c_acc          = 0.0_wp
        veg_growth_heart_wood_c_acc        = 0.0_wp
        veg_growth_labile_c_acc            = 0.0_wp
        veg_growth_reserve_c_acc           = 0.0_wp
        veg_growth_fruit_c_acc             = 0.0_wp
        veg_growth_seed_bed_c_acc          = 0.0_wp
        veg_growth_leaf_n_acc              = 0.0_wp
        veg_growth_fine_root_n_acc         = 0.0_wp
        veg_growth_coarse_root_n_acc       = 0.0_wp
        veg_growth_sap_wood_n_acc          = 0.0_wp
        veg_growth_heart_wood_n_acc        = 0.0_wp
        veg_growth_labile_n_acc            = 0.0_wp
        veg_growth_reserve_n_acc           = 0.0_wp
        veg_growth_fruit_n_acc             = 0.0_wp
        veg_growth_seed_bed_n_acc          = 0.0_wp
        veg_growth_leaf_p_acc              = 0.0_wp
        veg_growth_fine_root_p_acc         = 0.0_wp
        veg_growth_coarse_root_p_acc       = 0.0_wp
        veg_growth_sap_wood_p_acc          = 0.0_wp
        veg_growth_heart_wood_p_acc        = 0.0_wp
        veg_growth_labile_p_acc            = 0.0_wp
        veg_growth_reserve_p_acc           = 0.0_wp
        veg_growth_fruit_p_acc             = 0.0_wp
        veg_growth_seed_bed_p_acc          = 0.0_wp
        veg_litterfall_leaf_c_acc          = 0.0_wp
        veg_litterfall_fine_root_c_acc     = 0.0_wp
        veg_litterfall_coarse_root_c_acc   = 0.0_wp
        veg_litterfall_sap_wood_c_acc      = 0.0_wp
        veg_litterfall_heart_wood_c_acc    = 0.0_wp
        veg_litterfall_labile_c_acc        = 0.0_wp
        veg_litterfall_reserve_c_acc       = 0.0_wp
        veg_litterfall_fruit_c_acc         = 0.0_wp
        veg_litterfall_seed_bed_c_acc      = 0.0_wp
        veg_litterfall_leaf_n_acc          = 0.0_wp
        veg_litterfall_fine_root_n_acc     = 0.0_wp
        veg_litterfall_coarse_root_n_acc   = 0.0_wp
        veg_litterfall_sap_wood_n_acc      = 0.0_wp
        veg_litterfall_heart_wood_n_acc    = 0.0_wp
        veg_litterfall_labile_n_acc        = 0.0_wp
        veg_litterfall_reserve_n_acc       = 0.0_wp
        veg_litterfall_fruit_n_acc         = 0.0_wp
        veg_litterfall_seed_bed_n_acc      = 0.0_wp
        veg_litterfall_leaf_p_acc          = 0.0_wp
        veg_litterfall_fine_root_p_acc     = 0.0_wp
        veg_litterfall_coarse_root_p_acc   = 0.0_wp
        veg_litterfall_sap_wood_p_acc      = 0.0_wp
        veg_litterfall_heart_wood_p_acc    = 0.0_wp
        veg_litterfall_labile_p_acc        = 0.0_wp
        veg_litterfall_reserve_p_acc       = 0.0_wp
        veg_litterfall_fruit_p_acc         = 0.0_wp
        veg_litterfall_seed_bed_p_acc      = 0.0_wp             
        gpp_C13_acc                = 0.0_wp
        growth_resp_C13_acc        = 0.0_wp
        maint_resp_C13_acc         = 0.0_wp
        gpp_C14_acc                = 0.0_wp
        growth_resp_C14_acc        = 0.0_wp
        maint_resp_C14_acc         = 0.0_wp
        recycling_n_acc            = 0.0_wp
        res_use_n_acc              = 0.0_wp
        growth_n_acc               = 0.0_wp
        growth_swood_n15_acc       = 0.0_wp
        n_fixation_acc             = 0.0_wp
        n_fixation_n15_acc         = 0.0_wp
        uptake_po4_acc             = 0.0_wp
        recycling_p_acc            = 0.0_wp
        res_use_p_acc              = 0.0_wp
        growth_p_acc               = 0.0_wp
        precip_acc                 = 0.0_wp
        evaporation_acc            = 0.0_wp
        interception_acc           = 0.0_wp
        evapopot_acc               = 0.0_wp
        transpiration_acc          = 0.0_wp
        drainage_acc               = 0.0_wp
        gw_runoff_acc              = 0.0_wp
        srf_runoff_acc             = 0.0_wp
        gc_acc                     = 0.0_wp
        ga_acc                     = 0.0_wp
        ppfd_acc                   = 0.0_wp
        rnet_acc                   = 0.0_wp
        qh_acc                     = 0.0_wp
        qle_acc                    = 0.0_wp
        qg_acc                     = 0.0_wp
        albedo_acc                 = 0.0_wp
        appfd_acc                  = 0.0_wp
        uptake_no3_acc             = 0.0_wp
        uptake_no3_n15_acc         = 0.0_wp
        uptake_nh4_acc             = 0.0_wp
        uptake_nh4_n15_acc         = 0.0_wp
        myc_export_c_acc           = 0.0_wp
        myc_export_n_acc           = 0.0_wp
        myc_export_p_acc           = 0.0_wp
        myc_export_c13_acc         = 0.0_wp
        myc_export_c14_acc         = 0.0_wp
        myc_export_n15_acc         = 0.0_wp
        myc_respiration_c_acc      = 0.0_wp
        f_cm_som_acc               = 0.0_wp
        ! atmospheric forcing (precip_acc is already considered above, see vegfluxH2O)
        t_air_acc                  = 0.0_wp
        wind_air_acc               = 0.0_wp
        swvis_srf_down_acc         = 0.0_wp
        lw_srf_down_acc            = 0.0_wp
        vpd_acc                    = 0.0_wp
        co2_mixing_ratio_acc       = 0.0_wp
        co2_mixing_ratio_C13_acc   = 0.0_wp
        co2_mixing_ratio_C14_acc   = 0.0_wp
      ENDIF     ! IF (l_flux_output_interval .OR. l_zero_acc_variables)
    END SELECT  ! land/plant/canopy

    ! land/soil
    SELECT CASE(model%config%qmodel_id)
    CASE(QLAND, QSOIL)
      IF (l_flux_calc_acc_output) THEN
        !! all soil models (simple_1d and jsm)
        ! accumulate variables
        ! Carbon
        litterfallC_acc = litterfallC_acc + (veg_litterfall_mt(ix_leaf, ixC, ic) + & 
                                veg_litterfall_mt(ix_fine_root, ixC, ic) + veg_litterfall_mt(ix_coarse_root, ixC, ic) + &
                                veg_litterfall_mt(ix_sap_wood, ixC, ic) + veg_litterfall_mt(ix_heart_wood, ixC, ic) + &
                                veg_litterfall_mt(ix_labile, ixC, ic) + veg_litterfall_mt(ix_reserve, ixC, ic) + &
                                veg_litterfall_mt(ix_fruit, ixC, ic) + veg_litterfall_mt(ix_seed_bed, ixC, ic)) * &
                                1000000.0_wp * dtime_one/dtime
        litterfallC13_acc = litterfallC13_acc + (veg_litterfall_mt(ix_leaf, ixC13, ic) + & 
                                veg_litterfall_mt(ix_fine_root, ixC13, ic) + veg_litterfall_mt(ix_coarse_root, ixC13, ic) + &
                                veg_litterfall_mt(ix_sap_wood, ixC13, ic) + veg_litterfall_mt(ix_heart_wood, ixC13, ic) + &
                                veg_litterfall_mt(ix_labile, ixC13, ic) + veg_litterfall_mt(ix_reserve, ixC13, ic) + &
                                veg_litterfall_mt(ix_fruit, ixC13, ic) + veg_litterfall_mt(ix_seed_bed, ixC13, ic)) * &
                                1000000.0_wp * dtime_one/dtime
        litterfallC14_acc = litterfallC14_acc + (veg_litterfall_mt(ix_leaf, ixC14, ic) + & 
                                veg_litterfall_mt(ix_fine_root, ixC14, ic) + veg_litterfall_mt(ix_coarse_root, ixC14, ic) + &
                                veg_litterfall_mt(ix_sap_wood, ixC14, ic) + veg_litterfall_mt(ix_heart_wood, ixC14, ic) + &
                                veg_litterfall_mt(ix_labile, ixC14, ic) + veg_litterfall_mt(ix_reserve, ixC14, ic) + &
                                veg_litterfall_mt(ix_fruit, ixC14, ic) + veg_litterfall_mt(ix_seed_bed, ixC14, ic)) * &
                                1000000.0_wp * dtime_one/dtime
        litterfallN_acc = litterfallN_acc + (veg_litterfall_mt(ix_leaf, ixN, ic) + & 
                                veg_litterfall_mt(ix_fine_root, ixN, ic) + veg_litterfall_mt(ix_coarse_root, ixN, ic) + &
                                veg_litterfall_mt(ix_sap_wood, ixN, ic) + veg_litterfall_mt(ix_heart_wood, ixN, ic) + &
                                veg_litterfall_mt(ix_labile, ixN, ic) + veg_litterfall_mt(ix_reserve, ixN, ic) + &
                                veg_litterfall_mt(ix_fruit, ixN, ic) + veg_litterfall_mt(ix_seed_bed, ixN, ic)) * &
                                1000000.0_wp * dtime_one/dtime
        litterfallP_acc = litterfallP_acc + (veg_litterfall_mt(ix_leaf, ixP, ic) + & 
                                veg_litterfall_mt(ix_fine_root, ixP, ic) + veg_litterfall_mt(ix_coarse_root, ixP, ic) + &
                                veg_litterfall_mt(ix_sap_wood, ixP, ic) + veg_litterfall_mt(ix_heart_wood, ixP, ic) + &
                                veg_litterfall_mt(ix_labile, ixP, ic) + veg_litterfall_mt(ix_reserve, ixP, ic) + &
                                veg_litterfall_mt(ix_fruit, ixP, ic) + veg_litterfall_mt(ix_seed_bed, ixP, ic)) * &
                                1000000.0_wp * dtime_one/dtime

        het_respC_acc      = het_respC_acc      + SUM(het_respiration(ic,:) * soil_depth_sl(ic,:)) * dtime_one
        het_respC13_acc    = het_respC13_acc    + SUM(het_respiration_c13(ic,:) * soil_depth_sl(ic,:)) * dtime_one
        het_respC14_acc    = het_respC14_acc    + SUM(het_respiration_c14(ic,:) * soil_depth_sl(ic,:)) * dtime_one
        leachingCorg_acc   = leachingCorg_acc   + leaching_dom_carbon(ic)   * dtime_one
        leachingC13org_acc = leachingC13org_acc + leaching_dom_carbon13(ic) * dtime_one
        leachingC14org_acc = leachingC14org_acc + leaching_dom_carbon14(ic) * dtime_one

        lateral_loss_Corg_acc   = lateral_loss_Corg_acc   + SUM(lateral_loss_dom_carbon_sl(ic,:) &
          &                       * soil_depth_sl(ic,:)) * dtime_one 
        lateral_loss_C13org_acc = lateral_loss_C13org_acc + SUM(lateral_loss_dom_carbon13_sl(ic,:) &
          &                       * soil_depth_sl(ic,:)) * dtime_one
        lateral_loss_C14org_acc = lateral_loss_C14org_acc + SUM(lateral_loss_dom_carbon14_sl(ic,:) &
          &                       * soil_depth_sl(ic,:)) * dtime_one 

        ! Nitrogen
        n_deposition_acc     = n_deposition_acc  + (nhx_deposition(ic) + noy_deposition(ic)) * dtime_one      ! unit: mumol/m2/sec * dtime_one
        n_deposition_n15_acc = n_deposition_n15_acc  + (nhx_n15_deposition(ic) + noy_n15_deposition(ic)) * dtime_one      ! unit: mumol/m2/sec * dtime_one
        net_min_nh4_acc      = net_min_nh4_acc + SUM(net_mineralisation_nh4(ic,:) * soil_depth_sl(ic,:)) * dtime_one
        net_min_no3_acc      = net_min_no3_acc  + SUM(net_mineralisation_no3(ic,:) &
          &                    * soil_depth_sl(ic,:)) * dtime_one                     
        plant_uptake_nh4_acc = plant_uptake_nh4_acc + &
                             SUM(plant_uptake_nh4_sl(ic,:) * soil_depth_sl(ic,:)) * dtime_one 
        plant_uptake_no3_acc = plant_uptake_no3_acc + &
                             SUM(plant_uptake_no3_sl(ic,:) * soil_depth_sl(ic,:)) * dtime_one
        nitrification_acc    = nitrification_acc + & 
                              SUM((nitrification_no3(ic,:) + nitrification_noy(ic,:) + nitrification_n2o(ic,:)) * &
                              soil_depth_sl(ic,:)) * dtime_one
        denitrification_acc  = denitrification_acc + & 
                              SUM((denitrification_n2(ic,:) + denitrification_noy(ic,:) + denitrification_n2o(ic,:))*&
                              soil_depth_sl(ic,:)) * dtime_one 
        emission_noy_acc = emission_noy_acc + emission_noy(ic) * dtime_one
        emission_noy_n15_acc = emission_noy_n15_acc + emission_noy_n15(ic) * dtime_one
        emission_n2o_acc = emission_n2o_acc + emission_n2o(ic) * dtime_one
        emission_n2o_n15_acc = emission_n2o_n15_acc + emission_n2o_n15(ic) * dtime_one
        emission_n2_acc  = emission_n2_acc  + emission_n2(ic) * dtime_one
        emission_n2_n15_acc  = emission_n2_n15_acc  + emission_n2_n15(ic) * dtime_one
        leaching_nh4_acc = leaching_nh4_acc + leaching_nh4_solute(ic) * dtime_one
        leaching_nh4_n15_acc = leaching_nh4_n15_acc + leaching_nh4_n15_solute(ic) * dtime_one
        leaching_no3_acc = leaching_no3_acc + leaching_no3_solute(ic) * dtime_one
        leaching_no3_n15_acc = leaching_no3_n15_acc + leaching_no3_n15_solute(ic) * dtime_one
        leachingNorg_acc = leachingNorg_acc + leaching_dom_nitrogen(ic) * dtime_one

        lateral_loss_nh4_acc     = lateral_loss_nh4_acc + SUM(lateral_loss_nh4_solute_sl(ic,:) &
          &                        * soil_depth_sl(ic,:)) * dtime_one 
        lateral_loss_nh4_n15_acc = lateral_loss_nh4_n15_acc + SUM(lateral_loss_nh4_n15_solute_sl(ic,:) &
          &                        * soil_depth_sl(ic,:)) * dtime_one 
        lateral_loss_no3_acc     = lateral_loss_no3_acc + SUM(lateral_loss_no3_solute_sl(ic,:) &
          &                        * soil_depth_sl(ic,:)) * dtime_one 
        lateral_loss_no3_n15_acc = lateral_loss_no3_n15_acc + SUM(lateral_loss_no3_n15_solute_sl(ic,:) &
          &                        * soil_depth_sl(ic,:)) * dtime_one 
        lateral_loss_Norg_acc    = lateral_loss_Norg_acc + SUM(lateral_loss_dom_nitrogen_sl(ic,:) &
          &                        * soil_depth_sl(ic,:)) * dtime_one 

        asymb_n_fixation_acc =  asymb_n_fixation_acc + SUM(asymb_n_fixation(ic,:)*soil_depth_sl(ic,:)) * dtime_one
        asymb_n_fixation_n15_acc =  asymb_n_fixation_n15_acc + SUM(asymb_n_fixation_n15(ic,:)*soil_depth_sl(ic,:)) * &
                                    dtime_one
        fast_exchange_nh4_acc  = fast_exchange_nh4_acc + &
                                   SUM(fast_exchange_nh4(ic,:) * soil_depth_sl(ic,:)) * dtime_one
        myc_uptake_nh4_acc  = myc_uptake_nh4_acc  + SUM(mycorrhiza_uptake_nh4_sl(ic,:)  * soil_depth_sl(ic,:)) * dtime_one
        myc_uptake_no3_acc  = myc_uptake_no3_acc  + SUM(mycorrhiza_uptake_no3_sl(ic,:)  * soil_depth_sl(ic,:)) * dtime_one
        myc_uptake_norg_acc = myc_uptake_norg_acc + SUM(mycorrhiza_uptake_norg_sl(ic,:) * soil_depth_sl(ic,:)) * dtime_one
        
                !kschufft
        flux_soluable_2_dom_nitrogen_acc = flux_soluable_2_dom_nitrogen_acc + &
                                          SUM(flux_soluable_2_dom_nitrogen(ic,:)*soil_depth_sl(ic,:)) * &
                                          dtime_one/dtime *1000000.0_wp
        flux_residue_2_dom_nitrogen_acc = flux_residue_2_dom_nitrogen_acc + &
                                          SUM(flux_residue_2_dom_nitrogen(ic,:)*soil_depth_sl(ic,:)) * &
                                          dtime_one/dtime *1000000.0_wp
        flux_poly_2_dom_nitrogen_acc = flux_poly_2_dom_nitrogen_acc + &
                                          SUM(flux_poly_2_dom_nitrogen(ic,:)*soil_depth_sl(ic,:)) * &
                                          dtime_one/dtime *1000000.0_wp
        flux_micturn_2_dom_nitrogen_acc = flux_micturn_2_dom_nitrogen_acc + &
                                          SUM(flux_micturn_2_dom_nitrogen(ic,:)*soil_depth_sl(ic,:)) * &
                                          dtime_one/dtime *1000000.0_wp
        flux_micturn_2_res_nitrogen_acc = flux_micturn_2_res_nitrogen_acc + &
                                          SUM(flux_micturn_2_res_nitrogen(ic,:)*soil_depth_sl(ic,:)) * &
                                          dtime_one/dtime *1000000.0_wp
        flux_dom_2_microbes_nitrogen_acc = flux_dom_2_microbes_nitrogen_acc + &
                                           SUM(flux_dom_2_microbes_nitrogen(ic,:)*soil_depth_sl(ic,:)) * &
                                          dtime_one/dtime *1000000.0_wp

        graz_min_nh4_acc      = graz_min_nh4_acc + SUM(grazing_mineralisation_nh4(ic,:) * soil_depth_sl(ic,:)) * dtime_one
        graz_min_nh4_n15_acc      = graz_min_nh4_n15_acc + SUM(grazing_mineralisation_nh4_n15(ic,:) * &
                                       soil_depth_sl(ic,:)) * dtime_one

        graz_min_po4_acc      = graz_min_po4_acc + SUM(grazing_mineralisation_po4(ic,:) * soil_depth_sl(ic,:)) * dtime_one

                                                       !kschufft
        flux_soluable_2_dom_C_acc = flux_soluable_2_dom_C_acc + &
                                          SUM(flux_soluable_2_dom_C(ic,:)*soil_depth_sl(ic,:)) * &
                                          dtime_one/dtime *1000000.0_wp
        flux_residue_2_dom_C_acc = flux_residue_2_dom_C_acc + &
                                          SUM(flux_residue_2_dom_C(ic,:)*soil_depth_sl(ic,:)) * &
                                          dtime_one/dtime *1000000.0_wp
        flux_poly_2_dom_C_acc = flux_poly_2_dom_C_acc + &
                                          SUM(flux_poly_2_dom_C(ic,:)*soil_depth_sl(ic,:)) * &
                                          dtime_one/dtime *1000000.0_wp
        flux_micturn_2_dom_C_acc = flux_micturn_2_dom_C_acc + &
                                          SUM(flux_micturn_2_dom_C(ic,:)*soil_depth_sl(ic,:)) * &
                                          dtime_one/dtime *1000000.0_wp
        flux_micturn_2_res_C_acc = flux_micturn_2_res_C_acc + &
                                          SUM(flux_micturn_2_res_C(ic,:)*soil_depth_sl(ic,:)) * &
                                          dtime_one/dtime *1000000.0_wp
        flux_dom_2_microbes_C_acc = flux_dom_2_microbes_C_acc + &
                                          SUM(flux_dom_2_microbes_C(ic,:)*soil_depth_sl(ic,:)) * &
                                          dtime_one/dtime *1000000.0_wp


        flux_soluable_2_dom_P_acc = flux_soluable_2_dom_P_acc + &
                                          SUM(flux_soluable_2_dom_P(ic,:)*soil_depth_sl(ic,:)) * &
                                          dtime_one/dtime *1000000.0_wp
        flux_residue_2_dom_P_acc = flux_residue_2_dom_P_acc + &
                                          SUM(flux_residue_2_dom_P(ic,:)*soil_depth_sl(ic,:)) * &
                                          dtime_one/dtime *1000000.0_wp
        flux_poly_2_dom_P_acc = flux_poly_2_dom_P_acc + &
                                          SUM(flux_poly_2_dom_P(ic,:)*soil_depth_sl(ic,:)) * &
                                          dtime_one/dtime *1000000.0_wp
        flux_micturn_2_dom_P_acc = flux_micturn_2_dom_P_acc + &
                                          SUM(flux_micturn_2_dom_P(ic,:)*soil_depth_sl(ic,:)) * &
                                          dtime_one/dtime *1000000.0_wp
        flux_micturn_2_res_P_acc = flux_micturn_2_res_P_acc + &
                                          SUM(flux_micturn_2_res_P(ic,:)*soil_depth_sl(ic,:)) * &
                                          dtime_one/dtime *1000000.0_wp
        flux_dom_2_microbes_P_acc = flux_dom_2_microbes_P_acc + &
                                          SUM(flux_dom_2_microbes_P(ic,:)*soil_depth_sl(ic,:)) * &
                                          dtime_one/dtime *1000000.0_wp
        
        ! Phosphorus
        p_deposition_acc       = p_deposition_acc + p_deposition(ic) * dtime_one
        leaching_po4_acc       = leaching_po4_acc + leaching_po4_solute(ic) * dtime_one           
        leachingPorg_acc       = leachingPorg_acc + leaching_dom_phosphorus(ic) * dtime_one

        lateral_loss_po4_acc   = lateral_loss_po4_acc + SUM(lateral_loss_po4_solute_sl(ic,:) &
          &                      * soil_depth_sl(ic,:)) * dtime_one 
        lateral_loss_Porg_acc  = lateral_loss_Porg_acc + SUM(lateral_loss_dom_phosphorus_sl(ic,:) &
          &                      * soil_depth_sl(ic,:)) * dtime_one 

        net_min_po4_acc        = net_min_po4_acc + &
                                  SUM(net_mineralisation_po4(ic,:) * soil_depth_sl(ic,:)) * dtime_one                
        plant_uptake_po4_acc   = plant_uptake_po4_acc + &
                                SUM(plant_uptake_po4_sl(ic,:) * soil_depth_sl(ic,:)) * dtime_one
        weathering_po4_acc     = weathering_po4_acc + &
                                  SUM(weathering_po4(ic,:) * soil_depth_sl(ic,:)) * dtime_one
        occlusion_po4_acc      = occlusion_po4_acc + &
                                  SUM(occlusion_po4(ic,:) * soil_depth_sl(ic,:)) * dtime_one
        slow_exchange_po4_acc  = slow_exchange_po4_acc + &
                                   SUM(slow_exchange_po4(ic,:) * soil_depth_sl(ic,:)) * dtime_one
        fast_exchange_po4_acc  = fast_exchange_po4_acc + &
                                   SUM(fast_exchange_po4(ic,:) * soil_depth_sl(ic,:)) * dtime_one
        biochem_mineralisation_po4_acc = biochem_mineralisation_po4_acc + &
                                        SUM(biochem_mineralisation_po4(ic,:) * soil_depth_sl(ic,:)) * dtime_one
        ! mass balance checks in SB_
        sb_delta_pools_carbon_acc     = sb_delta_pools_carbon_acc     + &
                                          SUM(sb_delta_pools_mt(ixC, ic, :)*soil_depth_sl(ic,:))
        sb_delta_pools_nitrogen_acc   = sb_delta_pools_nitrogen_acc   + &
                                          SUM(sb_delta_pools_mt(ixN, ic, :)*soil_depth_sl(ic,:))
        sb_delta_pools_phosphorus_acc = sb_delta_pools_phosphorus_acc + &
                                          SUM(sb_delta_pools_mt(ixP, ic, :)*soil_depth_sl(ic,:))
        sb_delta_pools_carbon13_acc   = sb_delta_pools_carbon13_acc   + &
                                          SUM(sb_delta_pools_mt(ixC13, ic, :)*soil_depth_sl(ic,:))
        sb_delta_pools_carbon14_acc   = sb_delta_pools_carbon14_acc   + &
                                          SUM(sb_delta_pools_mt(ixC14, ic, :)*soil_depth_sl(ic,:))
        sb_delta_pools_nitrogen15_acc = sb_delta_pools_nitrogen15_acc + &
                                          SUM(sb_delta_pools_mt(ixN15, ic, :)*soil_depth_sl(ic,:))
        sb_total_flux_carbon_acc      = sb_total_flux_carbon_acc      + total_flux_carbon(ic)
        sb_total_flux_nitrogen_acc    = sb_total_flux_nitrogen_acc    + total_flux_nitrogen(ic)
        sb_total_flux_phosphorus_acc  = sb_total_flux_phosphorus_acc  + total_flux_phosphorus(ic)
        sb_total_flux_carbon13_acc    = sb_total_flux_carbon13_acc    + total_flux_carbon13(ic)
        sb_total_flux_carbon14_acc    = sb_total_flux_carbon14_acc    + total_flux_carbon14(ic)
        sb_total_flux_nitrogen15_acc  = sb_total_flux_nitrogen15_acc  + total_flux_nitrogen15(ic)

        ! accumulate layer-variables
        ! C 
        het_respiration_lay_acc(1:nsoil_sb)     = het_respiration_lay_acc(1:nsoil_sb) + &
                                                  het_respiration(ic,:) * dtime_one
        het_respiration_c13_lay_acc(1:nsoil_sb) = het_respiration_c13_lay_acc(1:nsoil_sb) + &
                                                  het_respiration_c13(ic,:) * dtime_one
        het_respiration_c14_lay_acc(1:nsoil_sb) = het_respiration_c14_lay_acc(1:nsoil_sb) + &
                                                  het_respiration_c14(ic,:) * dtime_one
        transport_DOC_lay_acc(1:nsoil_sb)       = transport_DOC_lay_acc(1:nsoil_sb) + &
                                                  sb_transport_mt(ix_dom, ixC, ic, :) * &
                                                  1000000.0_wp * dtime_one/dtime
        transport_Res_lay_acc(1:nsoil_sb)       = transport_Res_lay_acc(1:nsoil_sb) + &
                                                  sb_transport_mt(ix_residue, ixC, ic, :) * &
                                                  1000000.0_wp * dtime_one/dtime
        transport_aRes_lay_acc(1:nsoil_sb)       = transport_aRes_lay_acc(1:nsoil_sb) + &
                                                  sb_transport_mt(ix_residue_assoc, ixC, ic, :) * &
                                                  1000000.0_wp * dtime_one/dtime
        hlp1_2d(:,:) = 0.0_wp
        DO is = 1,INT(num_sl_above_bedrock(ic))
          hlp1_2d(ic,is) = root_fraction_sl(ic,is) / soil_depth_sl(ic,is)
        ENDDO
        root_respiration_lay_acc(1:nsoil_sb)    = root_respiration_lay_acc(1:nsoil_sb) + &
                                                  fmaint_rate_root(ic) * &
                                                  (veg_pool_mt(ix_fine_root, ixN, ic) &
                                                  + veg_pool_mt(ix_coarse_root, ixN, ic)) * &
                                                  hlp1_2d(ic,:) * dtime_one
                                                  ! root_fraction_sl(ic,:) / soil_depth_sl(ic,:) * dtime_one

        exudation_c_lay_acc(1:nsoil_sb)        = exudation_c_lay_acc(1:nsoil_sb) + &
                                                 veg_exudation_mt(ixC,ic) * &
                                                 hlp1_2d(ic,:) * dtime_one *1000000._wp/dtime
        lateral_loss_Corg_lay_acc(1:nsoil_sb)   = lateral_loss_Corg_lay_acc(1:nsoil_sb) + &
                                                  lateral_loss_dom_carbon_sl(ic,:) * &
                                                  dtime_one

        ae_carbon_acc(1:nsoil_sb)               = ae_carbon_acc(1:nsoil_sb) + &
                                                  ae_carbon(ic,:)*dtime_one * 1000000.0_wp / dtime 
        ! N
        net_mineral_nh4_lay_acc(1:nsoil_sb)     = net_mineral_nh4_lay_acc(1:nsoil_sb) + &
                                                  net_mineralisation_nh4(ic,:) * dtime_one
        plant_uptake_nh4_lay_acc(1:nsoil_sb)    = plant_uptake_nh4_lay_acc(1:nsoil_sb) + &
                                                  plant_uptake_nh4_sl(ic,:) * dtime_one
        microbial_upt_nh4_lay_acc(1:nsoil_sb)   = microbial_upt_nh4_lay_acc(1:nsoil_sb) + &
                                                  microbial_uptake_nh4_sl(ic,:) * dtime_one
        microbial_upt_no3_lay_acc(1:nsoil_sb)   = microbial_upt_no3_lay_acc(1:nsoil_sb) + &
                                                  microbial_uptake_no3_sl(ic,:) * dtime_one
        myc_uptake_nh4_lay_acc(1:nsoil_sb)      = myc_uptake_nh4_lay_acc(1:nsoil_sb) + &
                                                  mycorrhiza_uptake_nh4_sl(ic,:) * dtime_one
        transport_solute_nh4_acc(1:nsoil_sb)    = transport_solute_nh4_acc(1:nsoil_sb) + &
                                                  transport_nh4_solute(ic,:) * dtime_one
        transport_nh4_assoc_acc(1:nsoil_sb)     = transport_nh4_assoc_acc(1:nsoil_sb) + &
                                                  transport_nh4_assoc(ic,:) * dtime_one
        asymb_n_fixation_lay_acc(1:nsoil_sb)    = asymb_n_fixation_lay_acc(1:nsoil_sb) + &
                                                  asymb_n_fixation(ic,:) * dtime_one
        net_mineral_no3_lay_acc(1:nsoil_sb)     = net_mineral_no3_lay_acc(1:nsoil_sb) + &
                                                  net_mineralisation_no3(ic,:) * dtime_one
        plant_uptake_no3_lay_acc(1:nsoil_sb)    = plant_uptake_no3_lay_acc(1:nsoil_sb) + &
                                                  plant_uptake_no3_sl(ic,:) * dtime_one
        myc_uptake_no3_lay_acc(1:nsoil_sb)      = myc_uptake_no3_lay_acc(1:nsoil_sb) + &
                                                  mycorrhiza_uptake_no3_sl(ic,:) * dtime_one
        transport_solute_no3_acc(1:nsoil_sb)    = transport_solute_no3_acc(1:nsoil_sb) + &
                                                  transport_no3_solute(ic,:) * dtime_one
        transport_DON_lay_acc(1:nsoil_sb)       = transport_DON_lay_acc(1:nsoil_sb) + &
                                                  sb_transport_mt(ix_dom, ixN, ic, :) * &
                                                  1000000.0_wp * dtime_one/dtime
        myc_uptake_norg_lay_acc(1:nsoil_sb)     = myc_uptake_norg_lay_acc(1:nsoil_sb) + &
                                                  mycorrhiza_uptake_norg_sl(ic,:) * dtime_one
        f_adsorpt_nh4_lay_acc(1:nsoil_sb)       = f_adsorpt_nh4_lay_acc(1:nsoil_sb) + &
                                                  fast_adsorpt_nh4(ic,:) * dtime_one
        nitrification_lay_acc(1:nsoil_sb)       = nitrification_lay_acc(1:nsoil_sb) + &
                                                  (nitrification_no3(ic,:) + nitrification_noy(ic,:) + &
                                                   nitrification_n2o(ic,:)) * dtime_one
        denitrification_lay_acc(1:nsoil_sb)     = denitrification_lay_acc(1:nsoil_sb) + &
                                                  (denitrification_n2(ic,:) + denitrification_noy(ic,:) + &
                                                   denitrification_n2o(ic,:)) * dtime_one
        lateral_loss_nh4_lay_acc(1:nsoil_sb)    = lateral_loss_nh4_lay_acc(1:nsoil_sb) + &
                                                  lateral_loss_nh4_solute_sl(ic,:) * dtime_one
        lateral_loss_no3_lay_acc(1:nsoil_sb)    = lateral_loss_no3_lay_acc(1:nsoil_sb) + &
                                                  lateral_loss_no3_solute_sl(ic,:) * dtime_one
        lateral_loss_Norg_lay_acc(1:nsoil_sb)   = lateral_loss_Norg_lay_acc(1:nsoil_sb) + &
                                                  lateral_loss_dom_nitrogen_sl(ic,:) * dtime_one
        ae_nitrogen_acc(1:nsoil_sb)             = ae_nitrogen_acc(1:nsoil_sb) + &
                                                  ae_nitrogen(ic,:)*dtime_one * 1000000._wp / dtime 

        flux_soluable_2_dom_nitrogen_lay_acc(1:nsoil_sb) = flux_soluable_2_dom_nitrogen_lay_acc(1:nsoil_sb) + &
                                                  flux_soluable_2_dom_nitrogen(ic,:) * &
                                                  dtime_one *1000000._wp /dtime

        flux_poly_2_dom_nitrogen_lay_acc(1:nsoil_sb) = flux_poly_2_dom_nitrogen_lay_acc(1:nsoil_sb) + &
                                              flux_poly_2_dom_nitrogen(ic,:) * &
                                              dtime_one *1000000._wp /dtime

        flux_residue_2_dom_nitrogen_lay_acc(1:nsoil_sb) = flux_residue_2_dom_nitrogen_lay_acc(1:nsoil_sb) + &
                                                 flux_residue_2_dom_nitrogen(ic,:) * &
                                                 dtime_one *1000000._wp /dtime

        flux_micturn_2_dom_nitrogen_lay_acc(1:nsoil_sb) = flux_micturn_2_dom_nitrogen_lay_acc(1:nsoil_sb) + &
                                                 flux_micturn_2_dom_nitrogen(ic,:) * &
                                                 dtime_one *1000000._wp /dtime

        flux_micturn_2_res_nitrogen_lay_acc(1:nsoil_sb) = flux_micturn_2_res_nitrogen_lay_acc(1:nsoil_sb) + &
                                                 flux_micturn_2_res_nitrogen(ic,:) * &
                                                 dtime_one *1000000._wp /dtime
        flux_dom_2_microbes_nitrogen_lay_acc(1:nsoil_sb) = flux_dom_2_microbes_nitrogen_lay_acc(1:nsoil_sb) + &
                                                  flux_dom_2_microbes_nitrogen(ic,:) * &
                                                  dtime_one *1000000._wp /dtime

        flux_soluable_2_dom_C_lay_acc(1:nsoil_sb) = flux_soluable_2_dom_C_lay_acc(1:nsoil_sb) + &
                                                  flux_soluable_2_dom_C(ic,:) * &
                                                  dtime_one *1000000._wp /dtime

        flux_poly_2_dom_C_lay_acc(1:nsoil_sb) = flux_poly_2_dom_C_lay_acc(1:nsoil_sb) + &
                                              flux_poly_2_dom_C(ic,:) * &
                                              dtime_one *1000000._wp /dtime

        flux_residue_2_dom_C_lay_acc(1:nsoil_sb) = flux_residue_2_dom_C_lay_acc(1:nsoil_sb) + &
                                                 flux_residue_2_dom_C(ic,:) * &
                                                 dtime_one *1000000._wp /dtime

        flux_micturn_2_dom_C_lay_acc(1:nsoil_sb) = flux_micturn_2_dom_C_lay_acc(1:nsoil_sb) + &
                                                 flux_micturn_2_dom_C(ic,:) * &
                                                 dtime_one *1000000._wp /dtime

        flux_micturn_2_res_C_lay_acc(1:nsoil_sb) = flux_micturn_2_res_C_lay_acc(1:nsoil_sb) + &
                                                 flux_micturn_2_res_C(ic,:) * &
                                                 dtime_one *1000000._wp /dtime
        flux_dom_2_microbes_C_lay_acc(1:nsoil_sb) = flux_dom_2_microbes_C_lay_acc(1:nsoil_sb) + &
                                                  flux_dom_2_microbes_C(ic,:) * &
                                                  dtime_one *1000000._wp /dtime


        flux_soluable_2_dom_P_lay_acc(1:nsoil_sb) = flux_soluable_2_dom_P_lay_acc(1:nsoil_sb) + &
                                                  flux_soluable_2_dom_P(ic,:) * &
                                                  dtime_one *1000000._wp /dtime

        flux_poly_2_dom_P_lay_acc(1:nsoil_sb) = flux_poly_2_dom_P_lay_acc(1:nsoil_sb) + &
                                              flux_poly_2_dom_P(ic,:) * &
                                              dtime_one *1000000._wp /dtime

        flux_residue_2_dom_P_lay_acc(1:nsoil_sb) = flux_residue_2_dom_P_lay_acc(1:nsoil_sb) + &
                                                 flux_residue_2_dom_P(ic,:) * &
                                                 dtime_one *1000000._wp /dtime

        flux_micturn_2_dom_P_lay_acc(1:nsoil_sb) = flux_micturn_2_dom_P_lay_acc(1:nsoil_sb) + &
                                                 flux_micturn_2_dom_P(ic,:) * &
                                                 dtime_one *1000000._wp /dtime

        flux_micturn_2_res_P_lay_acc(1:nsoil_sb) = flux_micturn_2_res_P_lay_acc(1:nsoil_sb) + &
                                                 flux_micturn_2_res_P(ic,:) * &
                                                 dtime_one *1000000._wp /dtime
        flux_dom_2_microbes_P_lay_acc(1:nsoil_sb) = flux_dom_2_microbes_P_lay_acc(1:nsoil_sb) + &
                                                  flux_dom_2_microbes_P(ic,:) * &
                                                  dtime_one *1000000._wp /dtime
        graz_min_nh4_lay_acc(1:nsoil_sb)     = graz_min_nh4_lay_acc(1:nsoil_sb) + &
                                               grazing_mineralisation_nh4(ic,:) * dtime_one     
        graz_min_po4_lay_acc(1:nsoil_sb)     = graz_min_po4_lay_acc(1:nsoil_sb) + &
                                               grazing_mineralisation_po4(ic,:) * dtime_one    
                                                  
        ! P
        f_exchange_po4_acc(1:nsoil_sb)       = f_exchange_po4_acc(1:nsoil_sb) + &
                                               fast_exchange_po4(ic,:) * dtime_one
        f_adsorpt_po4_acc(1:nsoil_sb)        = f_adsorpt_po4_acc(1:nsoil_sb) + &
                                               fast_adsorpt_po4(ic,:) * dtime_one
        net_min_po4_lay_acc(1:nsoil_sb)      = net_min_po4_lay_acc(1:nsoil_sb) + &
                                               net_mineralisation_po4(ic,:) * dtime_one
        weath_po4_acc(1:nsoil_sb)            = weath_po4_acc(1:nsoil_sb) + &
                                               weathering_po4(ic,:) * dtime_one
        ocl_po4_acc(1:nsoil_sb)              = ocl_po4_acc(1:nsoil_sb) + &
                                               occlusion_po4(ic,:) * dtime_one
        s_exchange_po4_acc(1:nsoil_sb)       = s_exchange_po4_acc(1:nsoil_sb) + &
                                               slow_exchange_po4(ic,:) * dtime_one
        biochem_min_po4_acc(1:nsoil_sb)      = biochem_min_po4_acc(1:nsoil_sb) + &
                                               biochem_mineralisation_po4(ic,:) * dtime_one
        plant_upt_po4_acc(1:nsoil_sb)        = plant_upt_po4_acc(1:nsoil_sb) + &
                                               plant_uptake_po4_sl(ic,:) * dtime_one
        microbial_upt_po4_acc(1:nsoil_sb)    = microbial_upt_po4_acc(1:nsoil_sb) + &
                                               microbial_uptake_po4_sl(ic,:) * dtime_one
        myc_upt_po4_acc(1:nsoil_sb)          = myc_upt_po4_acc(1:nsoil_sb) + &
                                               mycorrhiza_uptake_po4_sl(ic,:) * dtime_one
        transport_solute_po4_acc(1:nsoil_sb)     = transport_solute_po4_acc(1:nsoil_sb) + &
                                                   transport_po4_solute(ic,:) * dtime_one
        transport_assoc_fast_po4_acc(1:nsoil_sb) = transport_assoc_fast_po4_acc(1:nsoil_sb) + &
                                                   transport_po4_assoc_fast(ic,:) * dtime_one
        transport_assoc_slow_po4_acc(1:nsoil_sb) = transport_assoc_slow_po4_acc(1:nsoil_sb) + &
                                                   transport_po4_assoc_slow(ic,:) * dtime_one
        transport_occluded_po4_acc(1:nsoil_sb)   = transport_occluded_po4_acc(1:nsoil_sb) + &
                                                   transport_po4_occluded(ic,:) * dtime_one
        transport_primary_po4_acc(1:nsoil_sb)    = transport_primary_po4_acc(1:nsoil_sb) + &
                                                   transport_po4_primary(ic,:) * dtime_one
        transport_DOP_lay_acc(1:nsoil_sb)        = transport_DOP_lay_acc(1:nsoil_sb) + &
                                                   sb_transport_mt(ix_dom, ixP, ic, :) * &
                                                   1000000.0_wp * dtime_one/dtime
        lateral_loss_po4_lay_acc(1:nsoil_sb)     = lateral_loss_po4_lay_acc(1:nsoil_sb) + &
                                                   lateral_loss_po4_solute_sl(ic,:) * dtime_one
        lateral_loss_Porg_lay_acc(1:nsoil_sb)    = lateral_loss_Porg_lay_acc(1:nsoil_sb) + &
                                                   lateral_loss_dom_phosphorus_sl(ic,:) * dtime_one

        ae_phosphorus_acc(1:nsoil_sb)            = ae_phosphorus_acc(1:nsoil_sb) + &
                                                   ae_phosphorus(ic,:)*dtime_one * 1000000._wp / dtime 

        ! write output & zero acc variables
        IF(l_flux_output_interval) THEN
          ! soilfluxCdC13DC14
          dC13litterfall    = calc_delta_C13(litterfallC_acc,litterfallC13_acc)
          dC13het_resp      = calc_delta_C13(het_respC_acc,het_respC13_acc)
          dC13leaching      = calc_delta_C13(leachingCorg_acc,leachingC13org_acc)
          dC13lateral_loss  = calc_delta_C13(lateral_loss_Corg_acc, lateral_loss_C13org_acc) 
          write(lun,'(20F24.10)') time_d, litterfallC_acc, het_respC_acc, leachingCorg_acc, lateral_loss_Corg_acc, &
             dC13litterfall, dC13het_resp, dC13leaching, dC13lateral_loss, & 
             calc_delta_C14(litterfallC_acc,litterfallC14_acc,dC13litterfall), &
             calc_delta_C14(het_respC_acc,het_respC14_acc,dC13het_resp), &
             calc_delta_C14(leachingCorg_acc,leachingC14org_acc,dC13leaching), &
             calc_delta_C14(lateral_loss_Corg_acc,lateral_loss_C14org_acc,dC13lateral_loss), &
             flux_soluable_2_dom_C_acc, flux_residue_2_dom_C_acc, flux_poly_2_dom_C_acc, &
             flux_micturn_2_dom_C_acc, flux_micturn_2_res_C_acc, &
             flux_dom_2_microbes_C_acc  
          lun = lun + 1
          ! soilfluxN
          write(lun,'(31F20.12)') time_d,litterfallN_acc,n_deposition_acc,asymb_n_fixation_acc,net_min_nh4_acc, net_min_no3_acc, &
                                nitrification_acc,denitrification_acc,emission_noy_acc,emission_n2o_acc,emission_n2_acc,& 
                                plant_uptake_nh4_acc,plant_uptake_no3_acc,leaching_nh4_acc,leaching_no3_acc,leachingNorg_acc, &
                                lateral_loss_nh4_acc,lateral_loss_no3_acc,lateral_loss_Norg_acc, & 
                                fast_exchange_nh4_acc, myc_uptake_nh4_acc, myc_uptake_no3_acc, myc_uptake_norg_acc, &
                                flux_soluable_2_dom_nitrogen_acc, flux_residue_2_dom_nitrogen_acc, flux_poly_2_dom_nitrogen_acc, &
                                flux_micturn_2_dom_nitrogen_acc, flux_micturn_2_res_nitrogen_acc, &
                                flux_dom_2_microbes_nitrogen_acc, graz_min_nh4_acc
          lun=lun+1
          ! soilfluxP
          write(lun,'(29F20.12)') time_d, litterfallP_acc, p_deposition_acc, leaching_po4_acc, leachingPorg_acc, &
                                 lateral_loss_po4_acc, lateral_loss_Porg_acc, &
                                 net_min_po4_acc, plant_uptake_po4_acc, weathering_po4_acc, occlusion_po4_acc, &
                                 slow_exchange_po4_acc, fast_exchange_po4_acc, biochem_mineralisation_po4_acc, &
                                 flux_soluable_2_dom_P_acc, flux_residue_2_dom_P_acc, flux_poly_2_dom_P_acc, &
                                 flux_micturn_2_dom_P_acc, flux_micturn_2_res_P_acc, &
                                 flux_dom_2_microbes_P_acc, graz_min_po4_acc  
          lun = lun + 1 
          ! soilfluxdN
          write(lun,'(21F18.8)') time_d, & 
               calc_delta_N15(n_deposition_acc,n_deposition_n15_acc), & 
               calc_delta_N15(asymb_n_fixation_acc,asymb_n_fixation_n15_acc), & 
               calc_delta_N15(emission_noy_acc,emission_noy_n15_acc), &
               calc_delta_N15(emission_n2o_acc,emission_n2o_n15_acc), &
               calc_delta_N15(emission_n2_acc,emission_n2_n15_acc), &
               calc_delta_N15(leaching_nh4_acc,leaching_nh4_n15_acc), & 
               calc_delta_N15(leaching_no3_acc,leaching_no3_n15_acc), &
               calc_delta_N15(lateral_loss_nh4_acc,lateral_loss_nh4_n15_acc), & 
               calc_delta_N15(lateral_loss_no3_acc,lateral_loss_no3_n15_acc), &
               calc_delta_N15(graz_min_nh4_acc, graz_min_nh4_n15_acc) 
          lun=lun+1
          ! sb_massBalance
          write(lun,'(13F24.12)') time_d, &
                                  sb_delta_pools_carbon_acc, sb_delta_pools_nitrogen_acc, sb_delta_pools_phosphorus_acc, &
                                  sb_delta_pools_carbon13_acc, sb_delta_pools_carbon14_acc, sb_delta_pools_nitrogen15_acc, &
                                  sb_total_flux_carbon_acc, sb_total_flux_nitrogen_acc, sb_total_flux_phosphorus_acc, &
                                  sb_total_flux_carbon13_acc, sb_total_flux_carbon14_acc, sb_total_flux_nitrogen15_acc
          lun=lun+1

          ! soilfluxC_lay
          write(lun,'(321F20.10)') time_d, &
            het_respiration_lay_acc(1:nsoil_sb), & 
            calc_delta_C13(het_respiration_lay_acc(1:nsoil_sb),het_respiration_c13_lay_acc(1:nsoil_sb)), & 
            calc_delta_C14(het_respiration_lay_acc(1:nsoil_sb),het_respiration_c14_lay_acc(1:nsoil_sb), &
              &            calc_delta_C13(het_respiration_lay_acc(1:nsoil_sb),het_respiration_c13_lay_acc(1:nsoil_sb))), &
            transport_DOC_lay_acc(1:nsoil_sb), &
            transport_Res_lay_acc(1:nsoil_sb), &
            transport_aRes_lay_acc(1:nsoil_sb), &
            root_respiration_lay_acc(1:nsoil_sb), &
            lateral_loss_Corg_lay_acc(1:nsoil_sb), &
            ae_carbon_acc(1:nsoil_sb), flux_soluable_2_dom_C_lay_acc(1:nsoil_sb), &
            flux_residue_2_dom_C_lay_acc(1:nsoil_sb), flux_poly_2_dom_C_lay_acc(1:nsoil_sb), &
            flux_micturn_2_dom_C_lay_acc(1:nsoil_sb), flux_micturn_2_res_C_lay_acc(1:nsoil_sb), &
            flux_dom_2_microbes_C_lay_acc(1:nsoil_sb), exudation_c_lay_acc(1:nsoil_sb)
          lun = lun + 1
          ! soilfluxN_lay
          write(lun,'(561F20.10)') time_d, &   
            net_mineral_nh4_lay_acc(1:nsoil_sb),   plant_uptake_nh4_lay_acc(1:nsoil_sb), &
            microbial_upt_nh4_lay_acc(1:nsoil_sb), myc_uptake_nh4_lay_acc(1:nsoil_sb), &
            transport_solute_nh4_acc(1:nsoil_sb),  transport_nh4_assoc_acc(1:nsoil_sb), &
            asymb_n_fixation_lay_acc(1:nsoil_sb), &
            net_mineral_no3_lay_acc(1:nsoil_sb),   plant_uptake_no3_lay_acc(1:nsoil_sb), &
            microbial_upt_no3_lay_acc(1:nsoil_sb), myc_uptake_no3_lay_acc(1:nsoil_sb), &
            transport_solute_no3_acc(1:nsoil_sb),  transport_DON_lay_acc(1:nsoil_sb), &
            myc_uptake_norg_lay_acc(1:nsoil_sb), f_adsorpt_nh4_lay_acc(1:nsoil_sb), &
            nitrification_lay_acc(1:nsoil_sb), denitrification_lay_acc(1:nsoil_sb), &
            lateral_loss_nh4_lay_acc(1:nsoil_sb), &
            lateral_loss_no3_lay_acc(1:nsoil_sb), &
            lateral_loss_Norg_lay_acc(1:nsoil_sb),&
            ae_nitrogen_acc(1:nsoil_sb), flux_soluable_2_dom_nitrogen_lay_acc(1:nsoil_sb), &
            flux_residue_2_dom_nitrogen_lay_acc(1:nsoil_sb), flux_poly_2_dom_nitrogen_lay_acc(1:nsoil_sb), &
            flux_micturn_2_dom_nitrogen_lay_acc(1:nsoil_sb), flux_micturn_2_res_nitrogen_lay_acc(1:nsoil_sb), &
            flux_dom_2_microbes_nitrogen_lay_acc(1:nsoil_sb), graz_min_nh4_lay_acc(1:nsoil_sb)
          lun = lun + 1
          ! soilfluxP_lay
          write(lun,'(541F20.10)') time_d, &
            weath_po4_acc(1:nsoil_sb), ocl_po4_acc(1:nsoil_sb), &
            f_exchange_po4_acc(1:nsoil_sb), s_exchange_po4_acc(1:nsoil_sb), &
            f_adsorpt_po4_acc(1:nsoil_sb), &
            net_min_po4_lay_acc(1:nsoil_sb), biochem_min_po4_acc(1:nsoil_sb), &
            microbial_upt_po4_acc(1:nsoil_sb), plant_upt_po4_acc(1:nsoil_sb), &
            myc_upt_po4_acc(1:nsoil_sb), &
            transport_solute_po4_acc(1:nsoil_sb), transport_assoc_fast_po4_acc(1:nsoil_sb), &
            transport_assoc_slow_po4_acc(1:nsoil_sb), transport_occluded_po4_acc(1:nsoil_sb), &
            transport_primary_po4_acc(1:nsoil_sb), transport_DOP_lay_acc(1:nsoil_sb), &
            lateral_loss_po4_lay_acc(1:nsoil_sb), &
            lateral_loss_Porg_lay_acc(1:nsoil_sb), &
            ae_phosphorus_acc(1:nsoil_sb), flux_soluable_2_dom_P_lay_acc(1:nsoil_sb), &
            flux_residue_2_dom_P_lay_acc(1:nsoil_sb), flux_poly_2_dom_P_lay_acc(1:nsoil_sb), &
            flux_micturn_2_dom_P_lay_acc(1:nsoil_sb), flux_micturn_2_res_P_lay_acc(1:nsoil_sb), &
            flux_dom_2_microbes_P_lay_acc(1:nsoil_sb), graz_min_po4_lay_acc(1:nsoil_sb)
          lun = lun + 1

          !! set acc variables to zero (see below)
        END IF      ! l_flux_output_interval
      END IF        ! l_flux_calc_acc_output
      
      !! set all acc variables zero
      !! either if:
      !!   a) flux-output variables were written out, and thus need to be zero'ed
      !!   b) this routine is called only to zero the *_acc (flux-output variables) because, e.g., 
      !!        the output interval is changed and new output files are created (to delete potential prev. accumulated values)
      IF (l_flux_output_interval .OR. l_zero_acc_variables) THEN
        !! land & soil model
        litterfallC_acc                   = 0.0_wp
        het_respC_acc                     = 0.0_wp
        leachingCorg_acc                  = 0.0_wp
        lateral_loss_Corg_acc             = 0.0_wp
        litterfallC13_acc                 = 0.0_wp
        het_respC13_acc                   = 0.0_wp
        leachingC13org_acc                = 0.0_wp
        lateral_loss_C13org_acc           = 0.0_wp 
        litterfallC14_acc                 = 0.0_wp
        het_respC14_acc                   = 0.0_wp
        leachingC14org_acc                = 0.0_wp
        lateral_loss_C14org_acc           = 0.0_wp
        nitrification_acc                 = 0.0_wp
        denitrification_acc               = 0.0_wp
        litterfallN_acc                   = 0.0_wp
        net_min_nh4_acc                   = 0.0_wp
        net_min_no3_acc                   = 0.0_wp
        plant_uptake_nh4_acc              = 0.0_wp
        plant_uptake_no3_acc              = 0.0_wp
        leachingNorg_acc                  = 0.0_wp
        lateral_loss_Norg_acc             = 0.0_wp 
        myc_uptake_nh4_acc                = 0.0_wp
        myc_uptake_no3_acc                = 0.0_wp
        myc_uptake_norg_acc               = 0.0_wp
                !kschufft
        flux_soluable_2_dom_nitrogen_acc  = 0.0_wp
        flux_residue_2_dom_nitrogen_acc   = 0.0_wp
        flux_poly_2_dom_nitrogen_acc      = 0.0_wp
        flux_micturn_2_dom_nitrogen_acc   = 0.0_wp
        flux_micturn_2_res_nitrogen_acc   = 0.0_wp
        flux_dom_2_microbes_nitrogen_acc  = 0.0_wp
        flux_soluable_2_dom_C_acc  = 0.0_wp
        flux_residue_2_dom_C_acc   = 0.0_wp
        flux_poly_2_dom_C_acc      = 0.0_wp
        flux_micturn_2_dom_C_acc   = 0.0_wp
        flux_micturn_2_res_C_acc   = 0.0_wp
        flux_dom_2_microbes_C_acc  = 0.0_wp
        flux_soluable_2_dom_P_acc  = 0.0_wp
        flux_residue_2_dom_P_acc   = 0.0_wp
        flux_poly_2_dom_P_acc      = 0.0_wp
        flux_micturn_2_dom_P_acc   = 0.0_wp
        flux_micturn_2_res_P_acc   = 0.0_wp
        flux_dom_2_microbes_P_acc  = 0.0_wp
        graz_min_nh4_acc                  = 0.0_wp
        graz_min_nh4_n15_acc              = 0.0_wp
        graz_min_po4_acc                  = 0.0_wp
        fast_exchange_nh4_acc             = 0.0_wp
        emission_noy_acc                  = 0.0_wp
        emission_noy_n15_acc              = 0.0_wp
        emission_n2o_acc                  = 0.0_wp
        emission_n2o_n15_acc              = 0.0_wp
        emission_n2_acc                   = 0.0_wp
        emission_n2_n15_acc               = 0.0_wp
        leaching_nh4_acc                  = 0.0_wp
        leaching_nh4_n15_acc              = 0.0_wp
        leaching_no3_acc                  = 0.0_wp
        leaching_no3_n15_acc              = 0.0_wp
        lateral_loss_nh4_acc              = 0.0_wp 
        lateral_loss_nh4_n15_acc          = 0.0_wp 
        lateral_loss_no3_acc              = 0.0_wp 
        lateral_loss_no3_n15_acc          = 0.0_wp 
        asymb_n_fixation_acc              = 0.0_wp
        asymb_n_fixation_n15_acc          = 0.0_wp
        litterfallP_acc                   = 0.0_wp
        leachingPorg_acc                  = 0.0_wp
        lateral_loss_Porg_acc             = 0.0_wp 
        p_deposition_acc                  = 0.0_wp
        n_deposition_acc                  = 0.0_wp
        n_deposition_n15_acc              = 0.0_wp
        leaching_po4_acc                  = 0.0_wp
        lateral_loss_po4_acc              = 0.0_wp 
        net_min_po4_acc                   = 0.0_wp
        plant_uptake_po4_acc              = 0.0_wp
        weathering_po4_acc                = 0.0_wp
        occlusion_po4_acc                 = 0.0_wp
        slow_exchange_po4_acc             = 0.0_wp
        fast_exchange_po4_acc             = 0.0_wp
        biochem_mineralisation_po4_acc    = 0.0_wp
        sb_delta_pools_carbon_acc         = 0.0_wp
        sb_delta_pools_nitrogen_acc       = 0.0_wp
        sb_delta_pools_phosphorus_acc     = 0.0_wp
        sb_delta_pools_carbon13_acc       = 0.0_wp
        sb_delta_pools_carbon14_acc       = 0.0_wp
        sb_delta_pools_nitrogen15_acc     = 0.0_wp
        sb_total_flux_carbon_acc          = 0.0_wp
        sb_total_flux_nitrogen_acc        = 0.0_wp
        sb_total_flux_phosphorus_acc      = 0.0_wp
        sb_total_flux_carbon13_acc        = 0.0_wp
        sb_total_flux_carbon14_acc        = 0.0_wp
        sb_total_flux_nitrogen15_acc      = 0.0_wp

        !! simple_1d & jsm
        ! C
        het_respiration_lay_acc(1:nsoil_sb)         = 0.0_wp
        het_respiration_c13_lay_acc(1:nsoil_sb)     = 0.0_wp
        het_respiration_c14_lay_acc(1:nsoil_sb)     = 0.0_wp
        transport_DOC_lay_acc(1:nsoil_sb)           = 0.0_wp
        transport_Res_lay_acc(1:nsoil_sb)           = 0.0_wp
        transport_aRes_lay_acc(1:nsoil_sb)           = 0.0_wp
        root_respiration_lay_acc(1:nsoil_sb)        = 0.0_wp
        exudation_c_lay_acc(1:nsoil_sb)             = 0.0_wp
        lateral_loss_Corg_lay_acc(1:nsoil_sb)       = 0.0_wp 
        ae_carbon_acc(1:nsoil_sb)                   = 0.0_wp        
        ! N
        net_mineral_nh4_lay_acc(1:nsoil_sb)         = 0.0_wp
        plant_uptake_nh4_lay_acc(1:nsoil_sb)        = 0.0_wp
        microbial_upt_nh4_lay_acc(1:nsoil_sb)       = 0.0_wp
        microbial_upt_no3_lay_acc(1:nsoil_sb)       = 0.0_wp
        myc_uptake_nh4_lay_acc(1:nsoil_sb)          = 0.0_wp
        asymb_n_fixation_lay_acc(1:nsoil_sb)        = 0.0_wp
        net_mineral_no3_lay_acc(1:nsoil_sb)         = 0.0_wp
        plant_uptake_no3_lay_acc(1:nsoil_sb)        = 0.0_wp
        myc_uptake_no3_lay_acc(1:nsoil_sb)          = 0.0_wp
        transport_solute_nh4_acc(1:nsoil_sb)        = 0.0_wp
        transport_nh4_assoc_acc(1:nsoil_sb)         = 0.0_wp
        transport_solute_no3_acc(1:nsoil_sb)        = 0.0_wp
        transport_DON_lay_acc(1:nsoil_sb)           = 0.0_wp
        myc_uptake_norg_lay_acc(1:nsoil_sb)         = 0.0_wp
        f_adsorpt_nh4_lay_acc(1:nsoil_sb)           = 0.0_wp
        nitrification_lay_acc(1:nsoil_sb)           = 0.0_wp
        denitrification_lay_acc(1:nsoil_sb)         = 0.0_wp
        lateral_loss_nh4_lay_acc(1:nsoil_sb)        = 0.0_wp 
        lateral_loss_no3_lay_acc(1:nsoil_sb)        = 0.0_wp 
        lateral_loss_Norg_lay_acc(1:nsoil_sb)       = 0.0_wp 
        ae_nitrogen_acc(1:nsoil_sb)                 = 0.0_wp
        flux_soluable_2_dom_nitrogen_lay_acc(1:nsoil_sb) = 0.0_wp
        flux_residue_2_dom_nitrogen_lay_acc(1:nsoil_sb) = 0.0_wp
        flux_poly_2_dom_nitrogen_lay_acc(1:nsoil_sb) = 0.0_wp
        flux_micturn_2_dom_nitrogen_lay_acc(1:nsoil_sb) = 0.0_wp
        flux_micturn_2_res_nitrogen_lay_acc(1:nsoil_sb) = 0.0_wp
        flux_dom_2_microbes_nitrogen_lay_acc(1:nsoil_sb) = 0.0_wp
        flux_soluable_2_dom_C_lay_acc(1:nsoil_sb) = 0.0_wp
        flux_residue_2_dom_C_lay_acc(1:nsoil_sb) = 0.0_wp
        flux_poly_2_dom_C_lay_acc(1:nsoil_sb) = 0.0_wp
        flux_micturn_2_dom_C_lay_acc(1:nsoil_sb) = 0.0_wp
        flux_micturn_2_res_C_lay_acc(1:nsoil_sb) = 0.0_wp
        flux_dom_2_microbes_C_lay_acc(1:nsoil_sb) = 0.0_wp
        flux_soluable_2_dom_P_lay_acc(1:nsoil_sb) = 0.0_wp
        flux_residue_2_dom_P_lay_acc(1:nsoil_sb) = 0.0_wp
        flux_poly_2_dom_P_lay_acc(1:nsoil_sb) = 0.0_wp
        flux_micturn_2_dom_P_lay_acc(1:nsoil_sb) = 0.0_wp
        flux_micturn_2_res_P_lay_acc(1:nsoil_sb) = 0.0_wp
        flux_dom_2_microbes_P_lay_acc(1:nsoil_sb) = 0.0_wp
        net_min_po4_lay_acc(1:nsoil_sb)             = 0.0_wp
        graz_min_nh4_lay_acc(1:nsoil_sb)            = 0.0_wp
        graz_min_po4_lay_acc(1:nsoil_sb)            = 0.0_wp
        ! P
        f_exchange_po4_acc(1:nsoil_sb)              = 0.0_wp
        f_adsorpt_po4_acc(1:nsoil_sb)               = 0.0_wp
        weath_po4_acc(1:nsoil_sb)                   = 0.0_wp
        ocl_po4_acc(1:nsoil_sb)                     = 0.0_wp
        s_exchange_po4_acc(1:nsoil_sb)              = 0.0_wp
        net_min_po4_lay_acc(1:nsoil_sb)             = 0.0_wp
        biochem_min_po4_acc(1:nsoil_sb)             = 0.0_wp
        microbial_upt_po4_acc(1:nsoil_sb)           = 0.0_wp
        plant_upt_po4_acc(1:nsoil_sb)               = 0.0_wp 
        myc_upt_po4_acc(1:nsoil_sb)                 = 0.0_wp  
        transport_solute_po4_acc(1:nsoil_sb)        = 0.0_wp
        transport_assoc_fast_po4_acc(1:nsoil_sb)    = 0.0_wp
        transport_assoc_slow_po4_acc(1:nsoil_sb)    = 0.0_wp
        transport_occluded_po4_acc(1:nsoil_sb)      = 0.0_wp
        transport_primary_po4_acc(1:nsoil_sb)       = 0.0_wp
        transport_DOP_lay_acc(1:nsoil_sb)           = 0.0_wp      
        lateral_loss_po4_lay_acc(1:nsoil_sb)        = 0.0_wp 
        lateral_loss_Porg_lay_acc(1:nsoil_sb)       = 0.0_wp 
        ae_phosphorus_acc(1:nsoil_sb)               = 0.0_wp
      ENDIF           ! IF (l_flux_output_interval .OR. l_zero_acc_variables) THEN
    END SELECT        ! land/soil

  END SUBROUTINE write_output_flux


  !-----------------------------------------------------------------------------------------------------!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !! section III Soil forcing                                                                          !!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !-----------------------------------------------------------------------------------------------------!


  !-----------------------------------------------------------------------------------------------------
  !> Soil forcing file: open files and write column headers
  !! 
  !! open output files, define file names for pools, write column header names
  !! 
  !! lun: start at 100 + 60
  !-----------------------------------------------------------------------------------------------------
  SUBROUTINE open_output_files_soil_forcing(tile, iblk, ics, ice, nc)

    USE mo_jsb_class,             ONLY: Get_model
    USE mo_jsb_tile_class,        ONLY: t_jsb_tile_abstract
    USE mo_jsb_model_class,       ONLY: t_jsb_model
    USE mo_jsb_process_class,     ONLY: VEG_, SB_
    
    ! Use of process configurations (t_PROC_config)
    dsl4jsb_Use_config(VEG_)
    dsl4jsb_Use_config(SB_)

    IMPLICIT NONE
    ! ---------------------------
    ! 0.1 InOut
    CLASS(t_jsb_tile_abstract), INTENT(in) :: tile         !< one tile with data structure for one lct
    INTEGER,                    INTENT(in) :: iblk, &      !< iBlock, start point of chunk, end point of chunk, number of points in the chunk
                                              ics, &
                                              ice, &
                                              nc
    ! ---------------------------
    ! 0.2 Local
    TYPE(t_jsb_model),POINTER             :: model
    INTEGER        :: lun           ! logical unit number
    CHARACTER(100) :: fname         ! filename
    CHARACTER(len=*), PARAMETER :: routine = TRIM(modname)//':open_output_files_soil_forcing'
    ! ---------------------------
    ! 0.3 Declare Memory
    ! Declare process configuration and memory Pointers
    dsl4jsb_Def_config(VEG_)
    dsl4jsb_Def_config(SB_)
    ! ---------------------------
    ! 0.5 Get Memory
    model  => Get_model(tile%owner_model_id)
    ! Get process config
    dsl4jsb_Get_config(VEG_)
    dsl4jsb_Get_config(SB_)




    !! 
    !! output interval will be always timestep !
    !! 


    !> 1.0 define open() statements for file output
    !! 
    ! set initial lun to not conflict with the above pool & flux output
    lun = lun_ascii_soil_forc_start
    fname = 'soilphysics'//'_timestep.txt'
    OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
    lun=lun+1
    fname = 'veglitterfall'//'_timestep.txt'
    OPEN (UNIT=lun, FILE=fname, ACTION="write", STATUS="unknown")
    lun=lun+1

    !> 1.1  test if lun for soil forcing is too big
    !! 
    IF (lun > lun_ascii_soil_forc_max) THEN
      WRITE(message_text,'(a)') 'lun of soil forcing output files is too large'
      CALL finish(TRIM(routine),message_text)
    END IF

    !> 2.0 write column headers into output files
    !! 
    CALL init_output_files_soil_forcing()

  END SUBROUTINE open_output_files_soil_forcing


  !-----------------------------------------------------------------------------------------------------
  !> Soil forcing files: write column headers of output files
  !! 
  !! complete litterfall output (used as input for the stand alone soil model) \n
  !! and some soilphysics variables
  !-----------------------------------------------------------------------------------------------------
  SUBROUTINE init_output_files_soil_forcing()

    USE mo_jsb_grid_class,        ONLY: t_jsb_vgrid
    USE mo_jsb_grid,              ONLY: Get_vgrid

    IMPLICIT NONE
    ! ---------------------------
    ! 0.1 InOut
    !..
    ! ---------------------------
    ! 0.2 Local
    TYPE(t_jsb_vgrid), POINTER    :: vgrid_soil_sb                ! Vertical grid
    INTEGER                       :: nsoil_sb                     ! number of soil layers as used/defined by the SB_ process
    INTEGER                       :: lun, isoil
    CHARACTER(2)                  :: layerstr
    CHARACTER(450)                :: p1str,p2str,p3str,p4str,p5str,p6str,p7str,p8str,p9str,p10str
    ! ---------------------------
    ! 0.5 Get Memory
    vgrid_soil_sb => Get_vgrid('soil_layer_sb')
    nsoil_sb      =  vgrid_soil_sb%n_levels



    !> 1.0 write headers
    !!
    ! set initial lun to not conflict with the above pool & flux output
    lun = lun_ascii_soil_forc_start
    
    ! soilphysics
    p1str=''
    p2str=''
    p3str=''
    p4str=''
    DO isoil = 1,nsoil_sb
      IF(isoil<10) THEN 
        WRITE(layerstr,'(I1)') isoil
      ELSE
        WRITE(layerstr,'(I2)') isoil
      END IF
      WRITE(p1str,'(a)') TRIM(p1str)//' TemperatureSoil_L'//TRIM(layerstr)
      WRITE(p2str,'(a)') TRIM(p2str)//' WaterContentSoil_L'//TRIM(layerstr)
      WRITE(p3str,'(a)') TRIM(p3str)//' PercolationSoil_L'//TRIM(layerstr)
      WRITE(p4str,'(a)') TRIM(p4str)//' RootFractionSoil_L'//TRIM(layerstr)
    END DO
    WRITE(lun,'(8a)') 'Time ', &
                      TRIM(p1str), TRIM(p2str), TRIM(p3str), TRIM(p4str), &
                      ' VegPoolFineRootCarbon', &
                      ' nhx_deposition noy_deposition nhx_dn15_deposition noy_dn15_deposition p_deposition', &
                      ' demandUptakeN demandUptakeP'
    lun=lun+1
    ! veglitterfall
    WRITE(lun,'(10a)')  'Time ', &
                        'leafC leafN leafP leafC13 leafC14 leafN15 ' , &
                        'fine_rootC fine_rootN fine_rootP fine_rootC13 fine_rootC14 fine_rootN15 ' , &
                        'coarse_rootC coarse_rootN coarse_rootP coarse_rootC13 coarse_rootC14 coarse_rootN15 ' , &
                        'sap_woodC sap_woodN sap_woodP sap_woodC13 sap_woodC14 sap_woodN15 ' , &
                        'heart_woodC heart_woodN heart_woodP heart_woodC13 heart_woodC14 heart_woodN15 ' , &
                        'labileC labileN labileP labileC13 labileC14 labileN15 ' , &
                        'reserveC reserveN reserveP reserveC13 reserveC14 reserveN15 ' , &
                        'fruitC fruitN fruitP fruitC13 fruitC14 fruitN15 ' , &
                        'seed_bedC seed_bedN seed_bedP seed_bedC13 seed_bedC14 seed_bedN15'
    lun=lun+1

  END SUBROUTINE init_output_files_soil_forcing

  ! ======================================================================================================= !
  !>Subroutine to generate the forcing file for the stand alone soil model from 
  !>  litterfall data and SB rate modifiers 
  !>
  SUBROUTINE write_soil_forcing(tile, options, time_d)

    USE mo_jsb_class,             ONLY: Get_model
    USE mo_jsb_tile_class,        ONLY: t_jsb_tile_abstract
    USE mo_jsb_task_class,        ONLY: t_jsb_task_options
    USE mo_jsb_model_class,       ONLY: t_jsb_model
    USE mo_jsb_process_class,     ONLY: A2L_, SPQ_, VEG_
    USE mo_isotope_util,          ONLY: calc_delta_C13,calc_delta_C14,calc_delta_N15
    ! ----------------------------------------------------------------------------------------------------- !
    dsl4jsb_Use_memory(A2L_)
    dsl4jsb_Use_memory(VEG_)
    dsl4jsb_Use_memory(SPQ_)
    ! ----------------------------------------------------------------------------------------------------- !
    CLASS(t_jsb_tile_abstract), INTENT(inout)     :: tile         !< one tile with data structure for one lct
    TYPE(t_jsb_task_options),   INTENT(in)        :: options      !< model options
    REAL(wp),                   INTENT(in)        :: time_d       !< time in days
    ! ----------------------------------------------------------------------------------------------------- !
    TYPE(t_jsb_model),      POINTER :: model          !< the model
    TYPE(t_lnd_bgcm_store), POINTER :: bgcm_store     !< the bgcm store of this tile
    TYPE(t_lctlib_element), POINTER :: lctlib         !< land-cover-type library - parameter across pft's
    REAL(wp)                        :: dC13_leaf,dC13_fine_root,dC13_coarse_root,&
      &                                dC13_sap_wood,dC13_heart_wood,dC13_labile, & 
                                       dC13_reserve,dC13_fruit,dC13_seed_bed
    REAL(wp)                        :: DC14_leaf,DC14_fine_root,DC14_coarse_root,&
      &                                DC14_sap_wood,DC14_heart_wood,DC14_labile, & 
                                       DC14_reserve,DC14_fruit,DC14_seed_bed
    REAL(wp)                        :: dN15_leaf,dN15_fine_root,dN15_coarse_root,& 
      &                                dN15_sap_wood,dN15_heart_wood,dN15_labile, & 
                                       dN15_reserve,dN15_fruit,dN15_seed_bed
    INTEGER                         :: lun          ! logical unit number
    INTEGER                         :: iblk, ics, ice, nc
    CHARACTER(len=*), PARAMETER :: routine = TRIM(modname)//':write_soil_forcing'
    ! ----------------------------------------------------------------------------------------------------- !
    dsl4jsb_Def_mt2L2D :: veg_pool_mt
    dsl4jsb_Def_mt2L2D :: veg_litterfall_mt
    ! ----------------------------------------------------------------------------------------------------- !
    dsl4jsb_Def_memory(A2L_)
    dsl4jsb_Def_memory(VEG_)
    dsl4jsb_Def_memory(SPQ_)
    ! ----------------------------------------------------------------------------------------------------- !
    ! A2L_
    dsl4jsb_Real2D_onChunk      :: nhx_deposition 
    dsl4jsb_Real2D_onChunk      :: noy_deposition 
    dsl4jsb_Real2D_onChunk      :: nhx_n15_deposition 
    dsl4jsb_Real2D_onChunk      :: noy_n15_deposition 
    dsl4jsb_Real2D_onChunk      :: p_deposition 
    ! VEG_ 2D
    dsl4jsb_Real2D_onChunk      :: demand_uptake_n_tuptake_mavg
    dsl4jsb_Real2D_onChunk      :: demand_uptake_p_tuptake_mavg
    ! VEG_ 3D
    dsl4jsb_Real3D_onChunk      :: root_fraction_sl
    ! SPQ_
    dsl4jsb_Real3D_onChunk      :: t_soil_sl
    dsl4jsb_Real3D_onChunk      :: w_soil_sl
    dsl4jsb_Real3D_onChunk      :: percolation_sl
    ! ----------------------------------------------------------------------------------------------------- !
    iblk      = options%iblk
    ics       = options%ics
    ice       = options%ice
    nc        = options%nc
    ! ----------------------------------------------------------------------------------------------------- !
    IF (debug_on() .AND. iblk == 1) CALL message(TRIM(routine), 'Starting on tile '//TRIM(tile%name)//' ...')
    ! ----------------------------------------------------------------------------------------------------- !
    model  => Get_model(tile%owner_model_id)
    lctlib => model%lctlib(tile%lcts(1)%lib_id) 
    ! ----------------------------------------------------------------------------------------------------- !
    dsl4jsb_Get_memory(A2L_) 
    dsl4jsb_Get_memory(VEG_)
    dsl4jsb_Get_memory(SPQ_)                         
    ! ----------------------------------------------------------------------------------------------------- !
    bgcm_store => tile%bgcm_store
    dsl4jsb_Get_mt2L2D(VEG_BGCM_POOL_ID, veg_pool_mt)
    dsl4jsb_Get_mt2L2D(VEG_BGCM_LITTERFALL_ID, veg_litterfall_mt)
    ! ----------------------------------------------------------------------------------------------------- !
    dsl4jsb_Get_var2D_onChunk(A2L_,  nhx_deposition)
    dsl4jsb_Get_var2D_onChunk(A2L_,  noy_deposition)
    dsl4jsb_Get_var2D_onChunk(A2L_,  nhx_n15_deposition)
    dsl4jsb_Get_var2D_onChunk(A2L_,  noy_n15_deposition)
    dsl4jsb_Get_var2D_onChunk(A2L_,  p_deposition)
    ! ---------------------------
    dsl4jsb_Get_var2D_onChunk(VEG_,  demand_uptake_n_tuptake_mavg)
    dsl4jsb_Get_var2D_onChunk(VEG_,  demand_uptake_p_tuptake_mavg)
    dsl4jsb_Get_var3D_onChunk(VEG_,  root_fraction_sl)
    ! ---------------------------
    dsl4jsb_Get_var3D_onChunk(SPQ_, t_soil_sl)
    dsl4jsb_Get_var3D_onChunk(SPQ_, w_soil_sl)
    dsl4jsb_Get_var3D_onChunk(SPQ_, percolation_sl)
    ! ----------------------------------------------------------------------------------------------------- !

    !>1.0 write output
    !>
    ! set initial lun to not conflict with the above pool & flux output
    lun = lun_ascii_soil_forc_start
    ! soilphysics
    write(lun,'(90F18.8)') time_d                                                         , &
                            ! variables with one value per soil layer (20 sl max)
                            t_soil_sl(ic,:)                                             , &
                            w_soil_sl(ic,:)                                             , &
                            percolation_sl(ic,:)                                        , &
                            root_fraction_sl(ic,:)                                      , &
                            ! variables with one value only
                            veg_pool_mt(ix_fine_root, ixC, ic)                            , &
                            nhx_deposition(ic)                                          , &
                            noy_deposition(ic)                                          , &
                            calc_delta_N15(nhx_deposition(ic),nhx_n15_deposition(ic)) , &
                            calc_delta_N15(noy_deposition(ic),noy_n15_deposition(ic)) , &
                            p_deposition(ic)                                            , &
                            demand_uptake_n_tuptake_mavg(ic)                            , &
                            demand_uptake_p_tuptake_mavg(ic) 
                            
    lun=lun+1
    !> convert 13C and 14C in delta notation
    !! 
    dC13_leaf        = calc_delta_C13(veg_litterfall_mt(ix_leaf, ixC, ic),veg_litterfall_mt(ix_leaf, ixC13, ic))
    dC13_fine_root   = calc_delta_C13(veg_litterfall_mt(ix_fine_root, ixC, ic),veg_litterfall_mt(ix_fine_root, ixC13, ic))
    dC13_coarse_root = calc_delta_C13(veg_litterfall_mt(ix_coarse_root, ixC, ic),veg_litterfall_mt(ix_coarse_root, ixC13, ic))
    dC13_sap_wood    = calc_delta_C13(veg_litterfall_mt(ix_sap_wood, ixC, ic),veg_litterfall_mt(ix_sap_wood, ixC13, ic))
    dC13_heart_wood  = calc_delta_C13(veg_litterfall_mt(ix_heart_wood, ixC, ic),veg_litterfall_mt(ix_heart_wood, ixC13, ic))
    dC13_labile      = calc_delta_C13(veg_litterfall_mt(ix_labile, ixC, ic),veg_litterfall_mt(ix_labile, ixC13, ic))
    dC13_reserve     = calc_delta_C13(veg_litterfall_mt(ix_reserve, ixC, ic),veg_litterfall_mt(ix_reserve, ixC13, ic))
    dC13_fruit       = calc_delta_C13(veg_litterfall_mt(ix_fruit, ixC, ic),veg_litterfall_mt(ix_fruit, ixC13, ic))
    dC13_seed_bed    = calc_delta_C13(veg_litterfall_mt(ix_seed_bed, ixC, ic),veg_litterfall_mt(ix_seed_bed, ixC13, ic))
    DC14_leaf        = calc_delta_C14(veg_litterfall_mt(ix_leaf, ixC, ic), &
                                      veg_litterfall_mt(ix_leaf, ixC14, ic),dC13_leaf)
    DC14_fine_root   = calc_delta_C14(veg_litterfall_mt(ix_fine_root, ixC, ic), &
                                      veg_litterfall_mt(ix_fine_root, ixC14, ic),dC13_fine_root)
    DC14_coarse_root = calc_delta_C14(veg_litterfall_mt(ix_coarse_root, ixC, ic), &
                                      veg_litterfall_mt(ix_coarse_root, ixC14, ic),dC13_coarse_root)
    DC14_sap_wood    = calc_delta_C14(veg_litterfall_mt(ix_sap_wood, ixC, ic), &
                                      veg_litterfall_mt(ix_sap_wood, ixC14, ic),dC13_sap_wood)
    DC14_heart_wood  = calc_delta_C14(veg_litterfall_mt(ix_heart_wood, ixC, ic), &
                                      veg_litterfall_mt(ix_heart_wood, ixC14, ic),dC13_heart_wood)
    DC14_labile      = calc_delta_C14(veg_litterfall_mt(ix_labile, ixC, ic), &
                                      veg_litterfall_mt(ix_labile, ixC14, ic),dC13_labile)
    DC14_reserve     = calc_delta_C14(veg_litterfall_mt(ix_reserve, ixC, ic), &
                                      veg_litterfall_mt(ix_reserve, ixC14, ic),dC13_reserve)
    DC14_fruit       = calc_delta_C14(veg_litterfall_mt(ix_fruit, ixC, ic), &
                                      veg_litterfall_mt(ix_fruit, ixC14, ic),dC13_fruit)
    DC14_seed_bed    = calc_delta_C14(veg_litterfall_mt(ix_seed_bed, ixC, ic), &
                                      veg_litterfall_mt(ix_seed_bed, ixC14, ic),dC13_seed_bed)
    dN15_leaf        = calc_delta_N15(veg_litterfall_mt(ix_leaf, ixN, ic),veg_litterfall_mt(ix_leaf, ixN15, ic))
    dN15_fine_root   = calc_delta_N15(veg_litterfall_mt(ix_fine_root, ixN, ic),veg_litterfall_mt(ix_fine_root, ixN15, ic))
    dN15_coarse_root = calc_delta_N15(veg_litterfall_mt(ix_coarse_root, ixN, ic),veg_litterfall_mt(ix_coarse_root, ixN15, ic))
    dN15_sap_wood    = calc_delta_N15(veg_litterfall_mt(ix_sap_wood, ixN, ic),veg_litterfall_mt(ix_sap_wood, ixN15, ic))
    dN15_heart_wood  = calc_delta_N15(veg_litterfall_mt(ix_heart_wood, ixN, ic),veg_litterfall_mt(ix_heart_wood, ixN15, ic))
    dN15_labile      = calc_delta_N15(veg_litterfall_mt(ix_labile, ixN, ic),veg_litterfall_mt(ix_labile, ixN15, ic))
    dN15_reserve     = calc_delta_N15(veg_litterfall_mt(ix_reserve, ixN, ic),veg_litterfall_mt(ix_reserve, ixN15, ic))
    dN15_fruit       = calc_delta_N15(veg_litterfall_mt(ix_fruit, ixN, ic),veg_litterfall_mt(ix_fruit, ixN15, ic))
    dN15_seed_bed    = calc_delta_N15(veg_litterfall_mt(ix_seed_bed, ixN, ic),veg_litterfall_mt(ix_seed_bed, ixN15, ic))
    ! veglitterfall
    write(lun,'(60F18.8)') time_d, &   ! 6 variables for (currently) 9 elements + time_d = 55 columns
      ! leaf
      veg_litterfall_mt(ix_leaf, ixC, ic)*1000._wp, &
      veg_litterfall_mt(ix_leaf, ixN, ic)*1000000._wp, &
      veg_litterfall_mt(ix_leaf, ixP, ic)*1000000._wp    , &
      dC13_leaf, DC14_leaf, dN15_leaf, & 
      ! fine_root
      veg_litterfall_mt(ix_fine_root, ixC, ic)*1000._wp, &
      veg_litterfall_mt(ix_fine_root, ixN, ic)*1000000._wp, &
      veg_litterfall_mt(ix_fine_root, ixP, ic)*1000000._wp   , &
      dC13_fine_root, DC14_fine_root, dN15_fine_root, & 
      ! coarse_root
      veg_litterfall_mt(ix_coarse_root, ixC, ic)*1000._wp, &
      veg_litterfall_mt(ix_coarse_root, ixN, ic)*1000000._wp, &
      veg_litterfall_mt(ix_coarse_root, ixP, ic)*1000000._wp   , &
      dC13_coarse_root, DC14_coarse_root, dN15_coarse_root, & 
      ! sap_wood
      veg_litterfall_mt(ix_sap_wood, ixC, ic)*1000._wp, &
      veg_litterfall_mt(ix_sap_wood, ixN, ic)*1000000._wp, &
      veg_litterfall_mt(ix_sap_wood, ixP, ic)*1000000._wp    , &
      dC13_sap_wood, DC14_sap_wood, dN15_sap_wood, & 
      ! heart_wood
      veg_litterfall_mt(ix_heart_wood, ixC, ic)*1000._wp, &
      veg_litterfall_mt(ix_heart_wood, ixN, ic)*1000000._wp, &
      veg_litterfall_mt(ix_heart_wood, ixP, ic)*1000000._wp    , &
      dC13_heart_wood, DC14_heart_wood, dN15_heart_wood, & 
      ! labile
      veg_litterfall_mt(ix_labile, ixC, ic)*1000._wp, &
      veg_litterfall_mt(ix_labile, ixN, ic)*1000000._wp, &
      veg_litterfall_mt(ix_labile, ixP, ic)*1000000._wp    , &
      dC13_labile, DC14_labile, dN15_labile, & 
      ! reserve
      veg_litterfall_mt(ix_reserve, ixC, ic)*1000._wp, &
      veg_litterfall_mt(ix_reserve, ixN, ic)*1000000._wp, &
      veg_litterfall_mt(ix_reserve, ixP, ic)*1000000._wp   , &
      dC13_reserve, DC14_reserve, dN15_reserve, & 
      ! fruit
      veg_litterfall_mt(ix_fruit, ixC, ic)*1000._wp, &
      veg_litterfall_mt(ix_fruit, ixN, ic)*1000000._wp, &
      veg_litterfall_mt(ix_fruit, ixP, ic)*1000000._wp   , &
      dC13_fruit, DC14_fruit, dN15_fruit, & 
      ! seed_bed
      veg_litterfall_mt(ix_seed_bed, ixC, ic)*1000._wp, &
      veg_litterfall_mt(ix_seed_bed, ixN, ic)*1000000._wp, &
      veg_litterfall_mt(ix_seed_bed, ixP, ic)*1000000._wp  , &
      dC13_seed_bed, DC14_seed_bed, dN15_seed_bed 
    lun=lun+1

  END SUBROUTINE write_soil_forcing 



  !-----------------------------------------------------------------------------------------------------!
  !!                                                                                                   !!
  !! Close output files                                                                                !!
  !!                                                                                                   !!
  !-----------------------------------------------------------------------------------------------------!


  !-----------------------------------------------------------------------------------------------------
  !> Pools and Fluxes: close files
  !! 
  !! at this point of the simulation all data has been written
  !-----------------------------------------------------------------------------------------------------
  SUBROUTINE close_output_files

    IMPLICIT NONE
    ! 0.1 InOut
    ! 
    ! 0.2 Local
    INTEGER :: lun, lun_start, lun_end
    LOGICAL :: opened

    ! 1.0 pools
    lun_start = lun_ascii_pools_start
    lun_end   = lun_ascii_pools_max
    DO lun = lun_start,lun_end
       INQUIRE(lun,opened=opened)
       IF(opened) CLOSE(lun) 
    ENDDO

    ! 2.0 fluxes
    lun_start = lun_ascii_fluxes_start
    lun_end   = lun_ascii_fluxes_max
    DO lun = lun_start,lun_end
       INQUIRE(lun,opened=opened)
       IF(opened) CLOSE(lun) 
    ENDDO

  END SUBROUTINE close_output_files


  !-----------------------------------------------------------------------------------------------------
  !> Soil forcing files: close files
  !! 
  !! at this point of the simulation all data has been written
  !-----------------------------------------------------------------------------------------------------
  SUBROUTINE close_output_files_soil_forcing

    IMPLICIT NONE
    ! 0.1 InOut
    ! 
    ! 0.2 Local
    INTEGER :: lun, lun_start, lun_end
    LOGICAL :: opened

    ! 1.0 close soil_forcing output files
    lun_start = lun_ascii_soil_forc_start
    lun_end   = lun_ascii_soil_forc_max
    DO lun = lun_start,lun_end
       INQUIRE(lun,opened=opened)
       IF(opened) CLOSE(lun) 
    ENDDO

  END SUBROUTINE close_output_files_soil_forcing

END MODULE mo_qs_output_txt
