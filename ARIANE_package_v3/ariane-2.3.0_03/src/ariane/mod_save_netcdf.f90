!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! - Ariane - (May - 2007)
!! 
!! bruno.blanke@univ-brest.fr and nicolas.grima@univ-brest.fr
!! 
!! This software is a computer program whose purpose is 
!! the computation of 3D streamlines in a given velocity field 
!! (as the output of an Ocean General Circulation Model) and 
!! subsequent water masses analyses.
!! 
!! This software is governed by the CeCILL license under French law and
!! abiding by the rules of distribution of free software.  You can  use, 
!! modify and/ or redistribute the software under the terms of the CeCILL
!! license as circulated by CEA, CNRS and INRIA at the following URL
!! "http://www.cecill.info". 
!! 
!! As a counterpart to the access to the source code and  rights to copy,
!! modify and redistribute granted by the license, users are provided only
!! with a limited warranty  and the software's author,  the holder of the
!! economic rights,  and the successive licensors  have only  limited
!! liability. 
!! 
!! In this respect, the user's attention is drawn to the risks associated
!! with loading,  using,  modifying and/or developing or reproducing the
!! software by the user in light of its specific status of free software,
!! that may mean  that it is complicated to manipulate,  and  that  also
!! therefore means  that it is reserved for developers  and  experienced
!! professionals having in-depth computer knowledge. Users are therefore
!! encouraged to load and test the software's suitability as regards their
!! requirements in conditions enabling the security of their systems and/or 
!! data to be ensured and,  more generally, to use and operate it in the 
!! same conditions as regards security. 
!! 
!! The fact that you are presently reading this means that you have had
!! knowledge of the CeCILL license and that you accept its terms.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!=========================================================================
!!****h* ariane/mod_save_netcdf
!! NAME
!!   mod_save_netcdf
!!
!! FUNCTION
!!   .
!!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
!!
!! AUTHOR
!!   * Origin  : Nicolas Grima
!! 
!! CREATION DATE
!!   * November 2005
!!
!! HISTORY
!!   Date (dd/mm/yyyy/) - Modification(s)
!!
!! ARGUMENTS
!!   * ncid (input): the netcdf file ID
!!
!! TODO
!!   
!!
!! USED BY
!!   
!!
!! SOURCE
!!=======================================================================
MODULE mod_save_netcdf

  USE mod_precision
  USE mod_which_type
  USE mod_cst
  USE mod_namelist
  USE mod_netcdf_output
  USE mod_posin
  USE mod_init_particules
  USE mod_quant
  USE mod_reducmem
  USE mod_fx
  USE mod_fy
  USE mod_fz

  IMPLICIT NONE

  !---------------------------------------!
  !- NetCDF file and data identificators -!
  !---------------------------------------!

  INTEGER(kind = iprec) ::    ncid_data_pos
  INTEGER(kind = iprec) ::  ncid_data_stats

  INTEGER(kind = iprec) ::        init_x_id
  INTEGER(kind = iprec) ::        init_y_id
  INTEGER(kind = iprec) ::        init_z_id
  INTEGER(kind = iprec) ::        init_t_id
  INTEGER(kind = iprec) ::      init_age_id
  INTEGER(kind = iprec) ::   init_transp_id
  INTEGER(kind = iprec) ::     init_temp_id
  INTEGER(kind = iprec) ::     init_salt_id
  INTEGER(kind = iprec) ::     init_dens_id
  INTEGER(kind = iprec) ::      init_lon_id
  INTEGER(kind = iprec) ::      init_lat_id
  INTEGER(kind = iprec) ::    init_depth_id

  INTEGER(kind = iprec) ::       final_x_id
  INTEGER(kind = iprec) ::       final_y_id
  INTEGER(kind = iprec) ::       final_z_id
  INTEGER(kind = iprec) ::       final_t_id
  INTEGER(kind = iprec) ::     final_age_id
  INTEGER(kind = iprec) ::  final_transp_id
  INTEGER(kind = iprec) :: final_section_id
  INTEGER(kind = iprec) ::    final_temp_id
  INTEGER(kind = iprec) ::    final_salt_id
  INTEGER(kind = iprec) ::    final_dens_id
  INTEGER(kind = iprec) ::     final_lon_id
  INTEGER(kind = iprec) ::     final_lat_id
  INTEGER(kind = iprec) ::   final_depth_id

  !!- Quantitative -!!
  INTEGER(kind = iprec) ::        xy_mer_id
  INTEGER(kind = iprec) ::      xy_zonal_id
  INTEGER(kind = iprec) ::       xz_vert_id
  INTEGER(kind = iprec) ::      xz_zonal_id
  INTEGER(kind = iprec) ::        yz_mer_id
  INTEGER(kind = iprec) ::       yz_vert_id
  INTEGER(kind = iprec) ::         xy_uh_id
  INTEGER(kind = iprec) ::         xy_vh_id
  INTEGER(kind = iprec) ::        xy_zuh_id
  INTEGER(kind = iprec) ::       xy_z2uh_id
  INTEGER(kind = iprec) ::        xy_zvh_id
  INTEGER(kind = iprec) ::       xy_z2vh_id
  INTEGER(kind = iprec) ::        xy_tuh_id
  INTEGER(kind = iprec) ::       xy_t2uh_id
  INTEGER(kind = iprec) ::        xy_tvh_id
  INTEGER(kind = iprec) ::       xy_t2vh_id
  INTEGER(kind = iprec) ::        xy_suh_id
  INTEGER(kind = iprec) ::       xy_s2uh_id
  INTEGER(kind = iprec) ::        xy_svh_id
  INTEGER(kind = iprec) ::       xy_s2vh_id
  INTEGER(kind = iprec) ::        xy_ruh_id
  INTEGER(kind = iprec) ::       xy_r2uh_id
  INTEGER(kind = iprec) ::        xy_rvh_id
  INTEGER(kind = iprec) ::       xy_r2vh_id
  INTEGER(kind = iprec) ::       tmask_id

  !!- Qualitative -!!
  INTEGER(kind = iprec) ::      traj_lon_id
  INTEGER(kind = iprec) ::      traj_lat_id
  INTEGER(kind = iprec) ::    traj_depth_id
  INTEGER(kind = iprec) ::     traj_time_id
  INTEGER(kind = iprec) ::     traj_temp_id
  INTEGER(kind = iprec) ::     traj_salt_id
  INTEGER(kind = iprec) ::     traj_dens_id
  INTEGER(kind = iprec) ::       traj_iU_id
  INTEGER(kind = iprec) ::       traj_jV_id
  INTEGER(kind = iprec) ::       traj_kW_id


  !------------------------------------------------------------------!
  !- This is a generic interface to add attributes to a variable    -!
  !- in a Necdf File.                                               -!
  !------------------------------------------------------------------!
  INTERFACE sub_save_netcdf_trajectories_generic
    MODULE PROCEDURE                 &
         sub_save_netcdf_trajectories, &
         sub_save_netcdf_trajectories_svm ! svm = small virtual memory
  END INTERFACE sub_save_netcdf_trajectories_generic
  !!***  

CONTAINS
  !=========================================================================
  !!****f* mod_save_netcdf/sub_save_netcdf_grid()
  !! NAME
  !!   sub_save_netcdf_grid()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DATE
  !!   * December 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * ncid (input): the netcdf file ID
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_save_netcdf_grid()

  END SUBROUTINE sub_save_netcdf_grid
  !!***  
  !=========================================================================
  !!****f* mod_save_netcdf/sub_save_netcdf_init()
  !! NAME
  !!   sub_save_netcdf_init()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DATE
  !!   * November 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * ncid (input): the netcdf file ID
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_save_netcdf_data_init()

    !--------------------------------------------------!
    !- Define position variables and their attributes -!
    !--------------------------------------------------!
    CALL sub_save_netcdf_data_init_pos()

    !------------------------------------------------!
    !- Save namelist parameters in this netcdf file -!
    !------------------------------------------------!
    CALL sub_save_namelist_in_netcdf(ncid_data_pos)

    !---------------------------------------------!
    !- Close define mode (to enter in data mode) -!
    !---------------------------------------------!
    CALL sub_netcdf_end_def (ncid_data_pos)

  END SUBROUTINE sub_save_netcdf_data_init
  !!***  
  !=========================================================================
  !!****f* mod_save_netcdf/sub_save_netcdf_init_pos()
  !! NAME
  !!   sub_save_netcdf_init()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DATE
  !!   * November 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * ncid (input): the netcdf file ID
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_save_netcdf_data_init_pos()

    INTEGER(kind = iprec) :: dim_ntraj_id
    INTEGER(kind = iprec) :: dim_nb_output_id

    INTEGER(kind = iprec), DIMENSION(:), ALLOCATABLE :: dims_out

    CHARACTER(len = 64)   :: c_name
    CHARACTER(len = 2)    :: c_itype
    CHARACTER(len = 2)    :: c_rstype, c_rltype
    CHARACTER(len = 4)    :: c_unit
    INTEGER(kind = iprec) :: itype = 0
    REAL(kind = rshort)   :: real_short_type = 0._rshort
    REAL(kind = rlong)    :: real_long_type  = 0._rlong

    WRITE(lun_standard,*)''
    WRITE(lun_standard,*)'================================================='
    WRITE(lun_standard,*)'= Create and Define NetCDF ouput POSITIONS file ='
    WRITE(lun_standard,*)'================================================='

    !----------------------!
    !- Create NetCDF File -!
    !----------------------!
    IF (TRIM(mode) == 'qualitative') THEN
      c_name = 'ariane_trajectories_qualitative.nc'
    ELSE
      c_name = 'ariane_positions_quantitative.nc'
    ENDIF

    CALL sub_netcdf_create( &
         TRIM(c_name)     , &
         ncid_data_pos    , &
         large_file = output_netcdf_large_file)
    WRITE(lun_standard,*)'--- Successful creation of ',TRIM(c_name),' ---', ncid_data_pos

    !---------------------------------------!
    !- Write dimensions in the NetCDF file -!
    !---------------------------------------!
    IF (TRIM(mode) == 'qualitative') THEN

      ALLOCATE(dims_out(2))
      CALL sub_memory(SIZE(dims_out),'i','dims_out','sub_save_netcdf_data_init_pos')


      !! NG 29 april 2010 change nb_output-1 by 0_iprec
      !! NG to introduce an unlimited dimenstion 
      !! NG to have the possibiity to write a record > 2GB
      !! NG 29 april 2010
      CALL sub_netcdf_dimensions(                     &
           ncid      = ncid_data_pos                , &
           dims_name = (/ 'ntraj    ', 'nb_output'/), &
           dims      = (/       ntraj, 0_iprec      /), &
           dims_id   = dims_out(:)                    &
           )

      dim_ntraj_id     = dims_out(1)
      dim_nb_output_id = dims_out(2)

      CALL sub_memory(-SIZE(dims_out),'i','dims_out','sub_save_netcdf_data_init_pos')
      DEALLOCATE(dims_out)

      WRITE(lun_standard,*)'  - Dimensions in qualitative are:'
      WRITE(lun_standard,*)'    - ntraj     =', ntraj
      WRITE(lun_standard,*)'    - nb_output =', nb_output + 1
      WRITE(lun_standard,*)''

    ELSE ! Quantitative !

      ALLOCATE(dims_out(1))
      CALL sub_memory(SIZE(dims_out),'i','dims_out','sub_save_netcdf_data_init_pos')

      CALL sub_netcdf_dimensions(   &
           ncid = ncid_data_pos   , &
           dims_name = (/'ntraj'/), &
           dims      = (/ ntraj /), &
           dims_id   = dims_out(:)  &
           )
      dim_ntraj_id   = dims_out(1)

      CALL sub_memory(-SIZE(dims_out),'i','dims_out','sub_save_netcdf_data_init_pos')
      DEALLOCATE(dims_out)

      WRITE(lun_standard,*)'  - Dimensions in quantitative are:'
      WRITE(lun_standard,*)'    - ntraj =', ntraj
      WRITE(lun_standard,*)''

    ENDIF

    !------------------!
    !- INTEGER TYPE ? -!
    !------------------!
    CALL sub_which_type(itype, c_itype)

    !---------------!
    !- REAL TYPE ? -!
    !---------------!
    CALL sub_which_type(real_short_type, c_rstype)
    CALL sub_which_type(real_long_type, c_rltype)

    !---------------------!
    !- Initial Positions -!
    !---------------------!
    !- init_x -!
    !----------!
    CALL sub_netcdf_var_and_att_def(                   &
         ncid              = ncid_data_pos           , &
         var_name          = 'init_x'                , &
         var_type          = c_rltype                , &
         var_dimids        = (/dim_ntraj_id/)        , &
         var_id            = init_x_id               , &
         att_title         = 'What is init_x ?'      , &
         att_longname      = 'Initial position in i' , &
         att_units         = 'No dimension'          , &
         att_missing_value = mask_value                &
         )
    WRITE(lun_standard,*)'  - Initial Positions in x are defined', init_x_id

    !----------!
    !- init_y -!
    !----------!
    CALL sub_netcdf_var_and_att_def(                   &
         ncid              = ncid_data_pos           , &
         var_name          = 'init_y'                , &
         var_type          = c_rltype                , &
         var_dimids        = (/dim_ntraj_id/)        , &
         var_id            = init_y_id               , &
         att_title         = 'What is init_y ?'      , &
         att_longname      = 'Initial position in j' , &
         att_units         = 'No dimension'          , &
         att_missing_value = mask_value                &
         )
    WRITE(lun_standard,*)'  - Initial Positions in y are defined', init_y_id


    !----------!
    !- init_z -!
    !----------!
    CALL sub_netcdf_var_and_att_def(                   &
         ncid              = ncid_data_pos           , &
         var_name          = 'init_z'                , &
         var_type          = c_rltype                , &
         var_dimids        = (/dim_ntraj_id/)        , &
         var_id            = init_z_id               , &
         att_title         = 'What is init_z ?'      , &
         att_longname      = 'Initial position in k' , &
         att_units         = 'No dimension'          , &
         att_missing_value = mask_value                &
         )
    WRITE(lun_standard,*)'  - Initial Positions in z are defined', init_z_id

    !----------!
    !- init_t -!
    !----------!
    CALL sub_netcdf_var_and_att_def(                          &
         ncid              = ncid_data_pos                  , &
         var_name          = 'init_t'                       , &
         var_type          = c_rltype                       , &
         var_dimids        = (/dim_ntraj_id/)               , &
         var_id            = init_t_id                      , &
         att_title         = 'What is init_t ?'             , &
         att_longname      = 'Initial position in l (time)' , &
         att_units         = 'See global attributes...'     , &
         att_missing_value = mask_value                       &
         )
    WRITE(lun_standard,*)'  - Initial Positions in t are defined', init_t_id

    !------------!
    !- init_age -!
    !------------!
    CALL sub_netcdf_var_and_att_def(                          &
         ncid              = ncid_data_pos                  , &
         var_name          = 'init_age'                     , &
         var_type          = c_rltype                       , &
         var_dimids        = (/dim_ntraj_id/)               , &
         var_id            = init_age_id                    , &
         att_title         = 'What is init_age ?'           , &
         att_longname      = 'Initial age (time)'           , &
         att_units         = 'seconds'                      , &
         att_missing_value = mask_value                       &
         )
    WRITE(lun_standard,*)'  - Initial Ages are defined', init_age_id

    !---------------------!
    !- initial transport -!
    !---------------------!
    c_unit='m3/s'

    CALL sub_netcdf_var_and_att_def(                          &
         ncid              = ncid_data_pos                  , &
         var_name          = 'init_transp'                  , &
         var_type          = c_rltype                       , &
         var_dimids        = (/dim_ntraj_id/)               , &
         var_id            = init_transp_id                 , &
         att_title         = 'What is init_transp ?'        , &
         att_longname      = 'Initial transport'            , &
         att_units         = c_unit                         , &
         att_missing_value = mask_value                       &
         )
    WRITE(lun_standard,*)'  - Initial transports are defined', init_transp_id

    IF ((key_alltracers).AND.(TRIM(mode) =='quantitative')) THEN
      !-----------------------!
      !- initial temperature -!
      !-----------------------!
      CALL sub_netcdf_var_and_att_def(                          &
           ncid              = ncid_data_pos                  , &
           var_name          = 'init_temp'                    , &
           var_type          = c_rstype                       , &
           var_dimids        = (/dim_ntraj_id/)               , &
           var_id            = init_temp_id                   , &
           att_title         = 'What is init_temp ?'          , &
           att_longname      = 'Initial temperature'          , &
           att_units         = 'degres'                       , &
           att_missing_value = mask_value                       &
           )
      WRITE(lun_standard,*)'  - Initial temperatures are defined',init_temp_id

      !--------------------!
      !- initial salinity -!
      !--------------------!
      CALL sub_netcdf_var_and_att_def(                          &
           ncid              = ncid_data_pos                  , &
           var_name          = 'init_salt'                    , &
           var_type          = c_rstype                       , &
           var_dimids        = (/dim_ntraj_id/)               , &
           var_id            = init_salt_id                   , &
           att_title         = 'What is init_salt ?'          , &
           att_longname      = 'Initial salinity'             , &
           att_units         = 'psu'                          , &
           att_missing_value = mask_value                       &
           )
      WRITE(lun_standard,*)'  - Initial salinities are defined', init_salt_id

      !-------------------!
      !- initial density -!
      !-------------------!
      CALL sub_netcdf_var_and_att_def(                          &
           ncid              = ncid_data_pos                  , &
           var_name          = 'init_dens'                    , &
           var_type          = c_rstype                       , &
           var_dimids        = (/dim_ntraj_id/)               , &
           var_id            = init_dens_id                   , &
           att_title         = 'What is init_dens ?'          , &
           att_longname      = 'Initial density'              , &
           att_units         = '...'                          , &
           att_missing_value = mask_value                       &
           )
      WRITE(lun_standard,*)'  - Initial densities are defined', init_dens_id

    ENDIF

    !-------------------!
    !- Final Positions -!
    !-------------------!
    !- final_x -!
    !-----------!
    CALL sub_netcdf_var_and_att_def(                        &
         ncid              = ncid_data_pos                , &
         var_name          = 'final_x'                    , &
         var_type          = c_rltype                     , &
         var_dimids        = (/dim_ntraj_id/)             , &
         var_id            = final_x_id                   , &
         att_title         = 'What is final_x ?'          , &
         att_longname      = 'Final position in x (or i)' , &
         att_units         = 'No dimension'               , &
         att_missing_value = mask_value                     &
         )
    WRITE(lun_standard,*)'  - Final Positions in x are defined', final_x_id

    !-----------!
    !- final_y -!
    !-----------!
    CALL sub_netcdf_var_and_att_def(                        &
         ncid              = ncid_data_pos                , &
         var_name          = 'final_y'                    , &
         var_type          = c_rltype                     , &
         var_dimids        = (/dim_ntraj_id/)             , &
         var_id            = final_y_id                   , &
         att_title         = 'What is final_y ?'          , &
         att_longname      = 'Final position in y (or j)' , &
         att_units         = 'No dimension'               , &
         att_missing_value = mask_value                     &
         )
    WRITE(lun_standard,*)'  - Final Positions in y are defined', final_y_id

    !-----------!
    !- final_z -!
    !-----------!
    CALL sub_netcdf_var_and_att_def(                        &
         ncid              = ncid_data_pos                , &
         var_name          = 'final_z'                    , &
         var_type          = c_rltype                     , &
         var_dimids        = (/dim_ntraj_id/)             , &
         var_id            = final_z_id                   , &
         att_title         = 'What is final_z ?'          , &
         att_longname      = 'Final position in z (or k)' , &
         att_units         = 'No dimension'               , &
         att_missing_value = mask_value                     &
         )
    WRITE(lun_standard,*)'  - Final Positions in z are defined', final_z_id

    !-----------!
    !- final_t -!
    !-----------!
    CALL sub_netcdf_var_and_att_def(                        &
         ncid              = ncid_data_pos                , &
         var_name          = 'final_t'                    , &
         var_type          = c_rltype                     , &
         var_dimids        = (/dim_ntraj_id/)             , &
         var_id            = final_t_id                   , &
         att_title         = 'What is final_t ?'          , &
         att_longname      = 'Final position in t (time)' , &
         att_units         = 'See global attributes...'   , &
         att_missing_value = mask_value                     &
         )
    WRITE(lun_standard,*)'  - Final Positions in t are defined', final_t_id

    !-------------!
    !- final_age -!
    !-------------!
    CALL sub_netcdf_var_and_att_def(                        &
         ncid              = ncid_data_pos                , &
         var_name          = 'final_age'                  , &
         var_type          = c_rltype                     , &
         var_dimids        = (/dim_ntraj_id/)             , &
         var_id            = final_age_id                 , &
         att_title         = 'What is fial_age ?'         , &
         att_longname      = 'Final Age.'                 , &
         att_units         = 'seconds'                    , &
         att_missing_value = mask_value                     &
         )
    WRITE(lun_standard,*)'  - Final Ages are defined'

    !-------------------!
    !- final transport -!
    !-------------------!
    c_unit='m3/s'

    CALL sub_netcdf_var_and_att_def(                        &
         ncid              = ncid_data_pos                , &
         var_name          = 'final_transp'               , &
         var_type          = c_rltype                     , &
         var_dimids        = (/dim_ntraj_id/)             , &
         var_id            = final_transp_id              , &
         att_title         = 'What is final_transp ?'     , &
         att_longname      = 'Final transport'            , &
         att_units         = c_unit                       , &
         att_missing_value = mask_value                     &
         )
    WRITE(lun_standard,*)'  - Final transports are defined', final_transp_id

    !-----------------!
    !- final section -!
    !-----------------!
    IF (TRIM(mode) == 'quantitative') THEN
      CALL sub_netcdf_var_and_att_def(                        &
           ncid              = ncid_data_pos                , &
           var_name          = 'final_section'              , &
           var_type          = c_itype                      , &
           var_dimids        = (/dim_ntraj_id/)             , &
           var_id            = final_section_id             , &
           att_title         = 'What is final_section ?'    , &
           att_longname      = 'Final section'              , &
           att_units         = 'No dimension'               , &
           att_missing_value = -1._rprec                     &
           )
      WRITE(lun_standard,*)'  - Final sections are defined', final_section_id
    ENDIF

    IF ((key_alltracers).AND.(TRIM(mode) == 'quantitative')) THEN
      !---------------------!
      !- final temperature -!
      !---------------------!
      CALL sub_netcdf_var_and_att_def(                        &
           ncid              = ncid_data_pos                , &
           var_name          = 'final_temp'                 , &
           var_type          = c_rstype                     , &
           var_dimids        = (/dim_ntraj_id/)             , &
           var_id            = final_temp_id                , &
           att_title         = 'What is final_temp ?'       , &
           att_longname      = 'Final temperature'          , &
           att_units         = 'degres'                     , &
           att_missing_value = mask_value                     &
           )
      WRITE(lun_standard,*)'  - Final temperatures are defined', final_temp_id

      !------------------!
      !- final salinity -!
      !------------------!
      CALL sub_netcdf_var_and_att_def(                        &
           ncid              = ncid_data_pos                , &
           var_name          = 'final_salt'                 , &
           var_type          = c_rstype                     , &
           var_dimids        = (/dim_ntraj_id/)             , &
           var_id            = final_salt_id                , &
           att_title         = 'What is final_salt ?'       , &
           att_longname      = 'Final salinity'             , &
           att_units         = 'psu'                        , &
           att_missing_value = mask_value                     &
           )
      WRITE(lun_standard,*)'  - Final salinities are defined', final_salt_id

      !-----------------!
      !- final density -!
      !-----------------!
      CALL sub_netcdf_var_and_att_def(                        &
           ncid              = ncid_data_pos                , &
           var_name          = 'final_dens'                 , &
           var_type          = c_rstype                     , &
           var_dimids        = (/dim_ntraj_id/)             , &
           var_id            = final_dens_id                , &
           att_title         = 'What is final_dens ?'       , &
           att_longname      = 'Final density'              , &
           att_units         = '...'                        , &
           att_missing_value = mask_value                     &
           )
      WRITE(lun_standard,*)'  - Final densities are defined', final_dens_id

    ENDIF


    IF (TRIM(mode) == 'qualitative') THEN

      !----------------!
      !- Trajectories -!
      !----------------!
      !------------!
      !- traj_lon -!
      !------------!
      CALL sub_netcdf_var_and_att_def(                     &
           ncid              = ncid_data_pos             , &
           var_name          = 'traj_lon'                , &
           var_type          = c_rltype                  , &
           var_dimids        = (/dim_ntraj_id            , &
           &                     dim_nb_output_id/)      , &
           var_id            = traj_lon_id               , &
           att_title         = 'What is traj_lon ?'      , &
           att_longname      = 'Trajectory: x positions' , &
           att_units         = 'No dimension'            , &
           att_missing_value = mask_value                  &
           )
      WRITE(lun_standard,*)'  - Positions in x for trajectories are defined', &
           traj_lon_id

      !------------!
      !- traj_lat -!
      !------------!
      CALL sub_netcdf_var_and_att_def(                     &
           ncid              = ncid_data_pos             , &
           var_name          = 'traj_lat'                , &
           var_type          = c_rltype                  , &
           var_dimids        = (/dim_ntraj_id            , &
           &                     dim_nb_output_id/)      , &
           var_id            = traj_lat_id               , &
           att_title         = 'What is traj_lat ?'      , &
           att_longname      = 'Trajectory: y positions' , &
           att_units         = 'No dimension'            , &
           att_missing_value = mask_value                  &
           )
      WRITE(lun_standard,*)'  - Positions in y for trajectories are defined', &
           traj_lat_id

      !--------------!
      !- traj_depth -!
      !--------------!
      CALL sub_netcdf_var_and_att_def(                     &
           ncid              = ncid_data_pos             , &
           var_name          = 'traj_depth'              , &
           var_type          = c_rltype                  , &
           var_dimids        = (/dim_ntraj_id            , &
           &                     dim_nb_output_id/)      , &
           var_id            = traj_depth_id             , &
           att_title         = 'What is traj_depth ?'    , &
           att_longname      = 'Trajectory: z positions' , &
           att_units         = 'No dimension'            , &
           att_missing_value = mask_value                  &
           )
      WRITE(lun_standard,*)'  - Positions in z for trajectories are defined', &
           traj_depth_id

      !-------------!
      !- traj_time -!
      !-------------!
      CALL sub_netcdf_var_and_att_def(                        &
           ncid              = ncid_data_pos                , &
           var_name          = 'traj_time'                  , &
           var_type          = c_rltype                     , &
           var_dimids        = (/dim_ntraj_id               , &
           &                     dim_nb_output_id/)         , &
           var_id            = traj_time_id                 , &
           att_title         = 'What is traj_time ?'        , &
           att_longname      = 'Trajectory: time positions' , &
           att_units         = 'See global attributes'      , &
           att_missing_value = mask_value                     &
           )
      WRITE(lun_standard,*)'  - Positions in time for trajectories are defined', &
           traj_time_id

      IF (key_iU_jV_kW) THEN

        !-----------!
        !- traj_iU -!
        !-----------!
        CALL sub_netcdf_var_and_att_def(                     &
             ncid              = ncid_data_pos             , &
             var_name          = 'traj_iU'                 , &
             var_type          = c_rltype                  , &
             var_dimids        = (/dim_ntraj_id            , &
             &                     dim_nb_output_id/)      , &
             var_id            = traj_iU_id                , &
             att_title         = 'ind i on grid U'         , &
             att_longname      = 'Trajectory: i on grid U' , &
             att_units         = 'No dimension'            , &
             att_missing_value = mask_value                  &
             )
        WRITE(lun_standard,*)'  - i on grid U for trajectories are defined', &
             traj_iU_id

        !-----------!
        !- traj_jV -!
        !-----------!
        CALL sub_netcdf_var_and_att_def(                     &
             ncid              = ncid_data_pos             , &
             var_name          = 'traj_jV'                 , &
             var_type          = c_rltype                  , &
             var_dimids        = (/dim_ntraj_id            , &
             &                     dim_nb_output_id/)      , &
             var_id            = traj_jV_id                , &
             att_title         = 'ind j on grid V'         , &
             att_longname      = 'Trajectory: j on grid V' , &
             att_units         = 'No dimension'            , &
             att_missing_value = mask_value                  &
             )
        WRITE(lun_standard,*)'  - j on grid V for trajectories are defined', &
             traj_jV_id

        !-----------!
        !- traj_kW -!
        !-----------!
        CALL sub_netcdf_var_and_att_def(                     &
             ncid              = ncid_data_pos             , &
             var_name          = 'traj_kW'                 , &
             var_type          = c_rltype                  , &
             var_dimids        = (/dim_ntraj_id            , &
             &                     dim_nb_output_id/)      , &
             var_id            = traj_kW_id                , &
             att_title         = 'ind k on grid W'         , &
             att_longname      = 'Trajectory: k on grid W' , &
             att_units         = 'No dimension'            , &
             att_missing_value = mask_value                  &
             )
        WRITE(lun_standard,*)'  - k on grid W for trajectories are defined', &
             traj_kW_id

      ENDIF

      IF (key_alltracers) THEN
        !-------------!
        !- traj_temp -!
        !-------------!
        CALL sub_netcdf_var_and_att_def(                      &
             ncid              = ncid_data_pos              , &
             var_name          = 'traj_temp'                , &
             var_type          = c_rstype                   , &
             var_dimids        = (/dim_ntraj_id             , &
             &                     dim_nb_output_id/)       , &
             var_id            = traj_temp_id               , &
             att_title         = 'What is traj_temp ?'      , &
             att_longname      = 'Trajectory: temperatures' , &
             att_units         = 'degres'                   , &
             att_missing_value = mask_value                   &
             )
        WRITE(lun_standard,*)'  - Temperatures for trajectories are defined', &
             traj_temp_id

        !-------------!
        !- traj_salt -!
        !-------------!
        CALL sub_netcdf_var_and_att_def(                    &
             ncid              = ncid_data_pos            , &
             var_name          = 'traj_salt'              , &
             var_type          = c_rstype                 , &
             var_dimids        = (/dim_ntraj_id           , &
             &                     dim_nb_output_id/)     , &
             var_id            = traj_salt_id             , &
             att_title         = 'What is traj_salt ?'    , &
             att_longname      = 'Trajectory: salinities' , &
             att_units         = 'psu'                    , &
             att_missing_value = mask_value                 &
             )
        WRITE(lun_standard,*)'  - Salinities for trajectories are defined', &
             traj_salt_id

        !-------------!
        !- traj_dens -!
        !-------------!
        CALL sub_netcdf_var_and_att_def(                   &
             ncid              = ncid_data_pos           , &
             var_name          = 'traj_dens'             , &
             var_type          = c_rstype                , &
             var_dimids        = (/dim_ntraj_id          , &
             &                     dim_nb_output_id/)    , &
             var_id            = traj_dens_id            , &
             att_title         = 'What is traj_dens ?'   , &
             att_longname      = 'Trajectory: densities' , &
             att_units         = '...'                   , &
             att_missing_value = mask_value                &
             )
        WRITE(lun_standard,*)'  - Densities for trajectories are defined', &
             traj_dens_id

      ENDIF

    ELSE ! Quantitative - We save the lon, lat depth of the init and final
      ! positions

      !---------------------!
      !- Initial Positions -!
      !---------------------!
      !- init_lon -!
      !------------!
      CALL sub_netcdf_var_and_att_def(                           &
           ncid              = ncid_data_pos                   , &
           var_name          = 'init_lon'                      , &
           var_type          = c_rltype                        , &
           var_dimids        = (/dim_ntraj_id/)                , &
           var_id            = init_lon_id                     , &
           att_title         = 'What is init_lon ?'            , &
           att_longname      = 'Initial position in longitude' , &
           att_units         = 'No dimension'                  , &
           att_missing_value = mask_value                        &
           )
      WRITE(lun_standard,*)'  - Initial Positions in lon are defined'

      !------------!
      !- init_lat -!
      !------------!
      CALL sub_netcdf_var_and_att_def(                          &
           ncid              = ncid_data_pos                  , &
           var_name          = 'init_lat'                     , &
           var_type          = c_rltype                       , &
           var_dimids        = (/dim_ntraj_id/)               , &
           var_id            = init_lat_id                    , &
           att_title         = 'What is init_lat ?'           , &
           att_longname      = 'Initial position in latitude' , &
           att_units         = 'No dimension'                 , &
           att_missing_value = mask_value                       &
           )
      WRITE(lun_standard,*)'  - Initial Positions in latitude are defined'


      !--------------!
      !- init_depth -!
      !--------------!
      CALL sub_netcdf_var_and_att_def(                       &
           ncid              = ncid_data_pos               , &
           var_name          = 'init_depth'                , &
           var_type          = c_rltype                    , &
           var_dimids        = (/dim_ntraj_id/)            , &
           var_id            = init_depth_id               , &
           att_title         = 'What is init_depth?'       , &
           att_longname      = 'Initial position in depth' , &
           att_units         = 'No dimension'              , &
           att_missing_value = mask_value                    &
           )
      WRITE(lun_standard,*)'  - Initial Positions in depth are defined'

      !-------------------!
      !- Final Positions -!
      !-------------------!
      !- final_lon -!
      !-------------!
      CALL sub_netcdf_var_and_att_def(                         &
           ncid              = ncid_data_pos                 , &
           var_name          = 'final_lon'                   , &
           var_type          = c_rltype                      , &
           var_dimids        = (/dim_ntraj_id/)              , &
           var_id            = final_lon_id                  , &
           att_title         = 'What is final_lon ?'         , &
           att_longname      = 'Final position in longitude' , &
           att_units         = 'No dimension'                , &
           att_missing_value = mask_value                      &
           )
      WRITE(lun_standard,*)'  - Final Positions in lon are defined'

      !-------------!
      !- final_lat -!
      !-------------!
      CALL sub_netcdf_var_and_att_def(                        &
           ncid              = ncid_data_pos                , &
           var_name          = 'final_lat'                  , &
           var_type          = c_rltype                     , &
           var_dimids        = (/dim_ntraj_id/)             , &
           var_id            = final_lat_id                 , &
           att_title         = 'What is final_lat ?'        , &
           att_longname      = 'Final position in latitude' , &
           att_units         = 'No dimension'               , &
           att_missing_value = mask_value                     &
           )
      WRITE(lun_standard,*)'  - Final Positions in latitude are defined'


      !---------------!
      !- final_depth -!
      !---------------!
      CALL sub_netcdf_var_and_att_def(                     &
           ncid              = ncid_data_pos             , &
           var_name          = 'final_depth'             , &
           var_type          = c_rltype                  , &
           var_dimids        = (/dim_ntraj_id/)          , &
           var_id            = final_depth_id            , &
           att_title         = 'What is final_depth?'    , &
           att_longname      = 'Final position in depth' , &
           att_units         = 'No dimension'            , &
           att_missing_value = mask_value                  &
           )
      WRITE(lun_standard,*)'  - Final Positions in depth are defined'

    ENDIF

  END SUBROUTINE sub_save_netcdf_data_init_pos
  !!***  
  !=========================================================================
  !!****f* mod_save_netcdf/sub_save_netcdf_init_stats()
  !! NAME
  !!   sub_save_netcdf_init_stats()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DATE
  !!   * November 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * ncid (input): the netcdf file ID
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_save_netcdf_data_init_stats()

    INTEGER(kind = iprec) :: dimx_id
    INTEGER(kind = iprec) :: dimy_id
    INTEGER(kind = iprec) :: dimz_id
    INTEGER(kind = iprec) :: dim_nb_sect_id

    INTEGER(kind = iprec), DIMENSION(:), ALLOCATABLE :: dims_out

    CHARACTER(len = 64)   :: c_name
    CHARACTER(len = 2)    :: c_rtype
    INTEGER(kind = iprec) :: nb_sect
    REAL(kind = rshort)    :: rtype = 0._rshort

    WRITE(lun_standard,*)''
    WRITE(lun_standard,*)'=================================================='
    WRITE(lun_standard,*)'= Create and Define NetCDF ouput STATISTICS file ='
    WRITE(lun_standard,*)'=================================================='

    !----------------------!
    !- Create NetCDF File -!
    !----------------------!
    c_name = 'ariane_statistics_quantitative.nc'

    CALL sub_netcdf_create(       &
         TRIM(c_name)           , &
         ncid_data_stats        , &
         large_file = output_netcdf_large_file )
    WRITE(lun_standard,*)'--- Successful creation of ',TRIM(c_name),' ---', ncid_data_pos

    !---------------------------------------!
    !- Write dimensions in the NetCDF file -!
    !---------------------------------------!
    IF (key_eco) THEN
      nb_sect = 1
    ELSE
      IF (icrit1 == 1) THEN
        nb_sect = nsect + 1 + 1
      ELSE
        nb_sect = nsect + 1  ! start from 0 to nsect2
      ENDIF
    ENDIF

    ALLOCATE(dims_out(4))
    CALL sub_memory(SIZE(dims_out),'i','dims_out','sub_save_netcdf_data_init_stats')

    CALL sub_netcdf_dimensions(                           &
         ncid = ncid_data_stats                         , &
         dims_name = (/ 'imt_reg', 'jmt_reg', 'kmt_reg', 'nb_sect'/), &
         dims      = (/   imt_reg,   jmt_reg,   kmt_reg,   nb_sect/), &
         dims_id   = dims_out(:)  &
         )

    dimx_id        = dims_out(1)
    dimy_id        = dims_out(2)
    dimz_id        = dims_out(3)
    dim_nb_sect_id = dims_out(4)

    CALL sub_memory(-SIZE(dims_out),'i','dims_out','sub_save_netcdf_data_init_stats')
    DEALLOCATE(dims_out)

    WRITE(lun_standard,*)'  - Dimensions in quantitative are:'
    WRITE(lun_standard,*)'    - imt_reg     =', imt_reg
    WRITE(lun_standard,*)'    - jmt_reg     =', jmt_reg
    WRITE(lun_standard,*)'    - kmt_reg     =', kmt_reg
    WRITE(lun_standard,*)'    - nb_sect =', nb_sect
    WRITE(lun_standard,*)''

    !---------------!
    !- REAL TYPE ? -!
    !---------------!
    CALL sub_which_type(rtype, c_rtype)

    !------------!
    !- xy_zonal -!
    !------------!
    CALL sub_netcdf_var_and_att_def(                &
         ncid              = ncid_data_stats      , &
         var_name          = 'xy_zonal'           , &
         var_type          = c_rtype              , &
         var_dimids        = (/dimx_id            , &
         &                     dimy_id            , &
         &                     dim_nb_sect_id/)   , &
         var_id            = xy_zonal_id          , &
         att_title         = 'What is xy_zonal ?' , &
         att_longname      = 'Stats: xy zonal'    , &
         att_units         = '...'                , &
         att_missing_value = mask_value             &
         )
    WRITE(lun_standard,*)'  - XY zonal dimensions are defined'

    !----------!
    !- xy_mer -!
    !----------!
    CALL sub_netcdf_var_and_att_def(                   &
         ncid              = ncid_data_stats         , &
         var_name          = 'xy_mer'                , &
         var_type          = c_rtype                 , &
         var_dimids        = (/dimx_id               , &
         &                     dimy_id               , &
         &                     dim_nb_sect_id/)      , &
         var_id            = xy_mer_id               , &
         att_title         = 'What is xy_mer ?'      , &
         att_longname      = 'Stats: xy meridional'  , &
         att_units         = '...'                   , &
         att_missing_value = mask_value                &
         )
    WRITE(lun_standard,*)'  - XY meridional dimensions are defined'

    !------------!
    !- xz_zonal -!
    !------------!
    CALL sub_netcdf_var_and_att_def(                &
         ncid              = ncid_data_stats      , &
         var_name          = 'xz_zonal'           , &
         var_type          = c_rtype              , &
         var_dimids        = (/dimx_id            , &
         &                     dimz_id            , &
         &                     dim_nb_sect_id/)   , &
         var_id            = xz_zonal_id          , &
         att_title         = 'What is xz_zonal ?' , &
         att_longname      = 'Stats: xz zonal'    , &
         att_units         = '...'                , &
         att_missing_value = mask_value             &
         )
    WRITE(lun_standard,*)'  - XZ zonal dimensions are defined'

    !-----------!
    !- xz_vert -!
    !-----------!
    CALL sub_netcdf_var_and_att_def(                 &
         ncid              = ncid_data_stats       , &
         var_name          = 'xz_vert'             , &
         var_type          = c_rtype               , &
         var_dimids        = (/dimx_id             , &
         &                     dimz_id             , &
         &                     dim_nb_sect_id/)    , &
         var_id            = xz_vert_id            , &
         att_title         = 'What is xz_vert ?'   , &
         att_longname      = 'Stats: xz vertical'  , &
         att_units         = '...'                 , &
         att_missing_value = mask_value              &
         )
    WRITE(lun_standard,*)'  - XZ vertical dimensions are defined'

    !----------!
    !- yz_mer -!
    !----------!
    CALL sub_netcdf_var_and_att_def(                   &
         ncid              = ncid_data_stats         , &
         var_name          = 'yz_mer'                , &
         var_type          = c_rtype                 , &
         var_dimids        = (/dimy_id               , &
         &                     dimz_id               , &
         &                     dim_nb_sect_id/)      , &
         var_id            = yz_mer_id               , &
         att_title         = 'What is yz_mer ?'      , &
         att_longname      = 'Stats: yz meridional'  , &
         att_units         = '...'                   , &
         att_missing_value = mask_value                &
         )
    WRITE(lun_standard,*)'  - YZ meridional dimensions are defined'

    !-----------!
    !- yz_vert -!
    !-----------!
    CALL sub_netcdf_var_and_att_def(                 &
         ncid              = ncid_data_stats       , &
         var_name          = 'yz_vert'             , &
         var_type          = c_rtype               , &
         var_dimids        = (/dimy_id             , &
         &                     dimz_id             , &
         &                     dim_nb_sect_id/)    , &
         var_id            = yz_vert_id            , &
         att_title         = 'What is yz_vert ?'   , &
         att_longname      = 'Stats: yz vertical'  , &
         att_units         = '...'                 , &
         att_missing_value = mask_value              &
         )
    WRITE(lun_standard,*)'  - YZ vertical dimensions are defined'

    !---------!
    !- xy_uh -!
    !---------!
    CALL sub_netcdf_var_and_att_def(                 &
         ncid              = ncid_data_stats       , &
         var_name          = 'xy_uh'               , &
         var_type          = c_rtype               , &
         var_dimids        = (/dimx_id             , &
         &                     dimy_id             , &
         &                     dim_nb_sect_id/)    , &
         var_id            = xy_uh_id              , &
         att_title         = 'What is xy_uh ?'     , &
         att_longname      = 'Stats: xy uh'        , &
         att_units         = '...'                 , &
         att_missing_value = mask_value              &
         )
    WRITE(lun_standard,*)'  - XY uh dimensions are defined'

    !---------!
    !- xy_vh -!
    !---------!
    CALL sub_netcdf_var_and_att_def(                 &
         ncid              = ncid_data_stats       , &
         var_name          = 'xy_vh'               , &
         var_type          = c_rtype               , &
         var_dimids        = (/dimx_id             , &
         &                     dimy_id             , &
         &                     dim_nb_sect_id/)    , &
         var_id            = xy_vh_id              , &
         att_title         = 'What is xy_vh ?'     , &
         att_longname      = 'Stats: xy vh'        , &
         att_units         = '...'                 , &
         att_missing_value = mask_value              &
         )
    WRITE(lun_standard,*)'  - XY vh dimensions are defined'

    !----------!
    !- xy_zuh -!
    !----------!
    CALL sub_netcdf_var_and_att_def(                 &
         ncid              = ncid_data_stats       , &
         var_name          = 'xy_zuh'              , &
         var_type          = c_rtype               , &
         var_dimids        = (/dimx_id             , &
         &                     dimy_id             , &
         &                     dim_nb_sect_id/)    , &
         var_id            = xy_zuh_id             , &
         att_title         = 'What is xy_zuh ?'    , &
         att_longname      = 'Stats: xy zuh'       , &
         att_units         = '...'                 , &
         att_missing_value = mask_value              &
         )
    WRITE(lun_standard,*)'  - XY zuh dimensions are defined'

    !-----------!
    !- xy_z2uh -!
    !-----------!
    CALL sub_netcdf_var_and_att_def(                 &
         ncid              = ncid_data_stats       , &
         var_name          = 'xy_z2uh'             , &
         var_type          = c_rtype               , &
         var_dimids        = (/dimx_id             , &
         &                     dimy_id             , &
         &                     dim_nb_sect_id/)    , &
         var_id            = xy_z2uh_id            , &
         att_title         = 'What is xy_z2uh ?'   , &
         att_longname      = 'Stats: xy z2uh'      , &
         att_units         = '...'                 , &
         att_missing_value = mask_value              &
         )
    WRITE(lun_standard,*)'  - XY z2uh dimensions are defined'

    !----------!
    !- xy_zvh -!
    !----------!
    CALL sub_netcdf_var_and_att_def(                 &
         ncid              = ncid_data_stats       , &
         var_name          = 'xy_zvh'              , &
         var_type          = c_rtype               , &
         var_dimids        = (/dimx_id             , &
         &                     dimy_id             , &
         &                     dim_nb_sect_id/)    , &
         var_id            = xy_zvh_id             , &
         att_title         = 'What is xy_zvh ?'    , &
         att_longname      = 'Stats: xy zvh'       , &
         att_units         = '...'                 , &
         att_missing_value = mask_value              &
         )
    WRITE(lun_standard,*)'  - XY zvh dimensions are defined'

    !-----------!
    !- xy_z2vh -!
    !-----------!
    CALL sub_netcdf_var_and_att_def(                 &
         ncid              = ncid_data_stats       , &
         var_name          = 'xy_z2vh'             , &
         var_type          = c_rtype               , &
         var_dimids        = (/dimx_id             , &
         &                     dimy_id             , &
         &                     dim_nb_sect_id/)    , &
         var_id            = xy_z2vh_id            , &
         att_title         = 'What is xy_z2vh ?'   , &
         att_longname      = 'Stats: xy z2vh'      , &
         att_units         = '...'                 , &
         att_missing_value = mask_value              &
         )
    WRITE(lun_standard,*)'  - XY z2vh dimensions are defined'

    IF (key_alltracers) THEN

      !----------!
      !- xy_tuh -!
      !----------!
      CALL sub_netcdf_var_and_att_def(                 &
           ncid              = ncid_data_stats       , &
           var_name          = 'xy_tuh'              , &
           var_type          = c_rtype               , &
           var_dimids        = (/dimx_id             , &
           &                     dimy_id             , &
           &                     dim_nb_sect_id/)    , &
           var_id            = xy_tuh_id             , &
           att_title         = 'What is xy_tuh ?'    , &
           att_longname      = 'Stats: xy tuh'       , &
           att_units         = '...'                 , &
           att_missing_value = mask_value              &
           )
      WRITE(lun_standard,*)'  - XY tuh dimensions are defined'

      !-----------!
      !- xy_t2uh -!
      !-----------!
      CALL sub_netcdf_var_and_att_def(                 &
           ncid              = ncid_data_stats       , &
           var_name          = 'xy_t2uh'             , &
           var_type          = c_rtype               , &
           var_dimids        = (/dimx_id             , &
           &                     dimy_id             , &
           &                     dim_nb_sect_id/)    , &
           var_id            = xy_t2uh_id            , &
           att_title         = 'What is xy_t2uh ?'   , &
           att_longname      = 'Stats: xy t2uh'      , &
           att_units         = '...'                 , &
           att_missing_value = mask_value              &
           )
      WRITE(lun_standard,*)'  - XY t2uh dimensions are defined'

      !----------!
      !- xy_tvh -!
      !----------!
      CALL sub_netcdf_var_and_att_def(                 &
           ncid              = ncid_data_stats       , &
           var_name          = 'xy_tvh'              , &
           var_type          = c_rtype               , &
           var_dimids        = (/dimx_id             , &
           &                     dimy_id             , &
           &                     dim_nb_sect_id/)    , &
           var_id            = xy_tvh_id             , &
           att_title         = 'What is xy_tvh ?'    , &
           att_longname      = 'Stats: xy tvh'       , &
           att_units         = '...'                 , &
           att_missing_value = mask_value              &
           )
      WRITE(lun_standard,*)'  - XY tvh dimensions are defined'

      !-----------!
      !- xy_t2vh -!
      !-----------!
      CALL sub_netcdf_var_and_att_def(                 &
           ncid              = ncid_data_stats       , &
           var_name          = 'xy_t2vh'             , &
           var_type          = c_rtype               , &
           var_dimids        = (/dimx_id             , &
           &                     dimy_id             , &
           &                     dim_nb_sect_id/)    , &
           var_id            = xy_t2vh_id            , &
           att_title         = 'What is xy_t2vh ?'   , &
           att_longname      = 'Stats: xy t2vh'      , &
           att_units         = '...'                 , &
           att_missing_value = mask_value              &
           )
      WRITE(lun_standard,*)'  - XY t2vh dimensions are defined'

      !----------!
      !- xy_suh -!
      !----------!
      CALL sub_netcdf_var_and_att_def(                 &
           ncid              = ncid_data_stats       , &
           var_name          = 'xy_suh'              , &
           var_type          = c_rtype               , &
           var_dimids        = (/dimx_id             , &
           &                     dimy_id             , &
           &                     dim_nb_sect_id/)    , &
           var_id            = xy_suh_id             , &
           att_title         = 'What is xy_suh ?'    , &
           att_longname      = 'Stats: xy suh'       , &
           att_units         = '...'                 , &
           att_missing_value = mask_value              &
           )
      WRITE(lun_standard,*)'  - XY suh dimensions are defined'

      !-----------!
      !- xy_s2uh -!
      !-----------!
      CALL sub_netcdf_var_and_att_def(                 &
           ncid              = ncid_data_stats       , &
           var_name          = 'xy_s2uh'             , &
           var_type          = c_rtype               , &
           var_dimids        = (/dimx_id             , &
           &                     dimy_id             , &
           &                     dim_nb_sect_id/)    , &
           var_id            = xy_s2uh_id            , &
           att_title         = 'What is xy_s2uh ?'   , &
           att_longname      = 'Stats: xy s2uh'      , &
           att_units         = '...'                 , &
           att_missing_value = mask_value              &
           )
      WRITE(lun_standard,*)'  - XY s2uh dimensions are defined'

      !----------!
      !- xy_svh -!
      !----------!
      CALL sub_netcdf_var_and_att_def(                 &
           ncid              = ncid_data_stats       , &
           var_name          = 'xy_svh'              , &
           var_type          = c_rtype               , &
           var_dimids        = (/dimx_id             , &
           &                     dimy_id             , &
           &                     dim_nb_sect_id/)    , &
           var_id            = xy_svh_id             , &
           att_title         = 'What is xy_svh ?'    , &
           att_longname      = 'Stats: xy svh'       , &
           att_units         = '...'                 , &
           att_missing_value = mask_value              &
           )
      WRITE(lun_standard,*)'  - XY svh dimensions are defined'

      !-----------!
      !- xy_s2vh -!
      !-----------!
      CALL sub_netcdf_var_and_att_def(                 &
           ncid              = ncid_data_stats       , &
           var_name          = 'xy_s2vh'             , &
           var_type          = c_rtype               , &
           var_dimids        = (/dimx_id             , &
           &                     dimy_id             , &
           &                     dim_nb_sect_id/)    , &
           var_id            = xy_s2vh_id            , &
           att_title         = 'What is xy_s2vh ?'   , &
           att_longname      = 'Stats: xy s2vh'      , &
           att_units         = '...'                 , &
           att_missing_value = mask_value              &
           )
      WRITE(lun_standard,*)'  - XY s2vh dimensions are defined'

      !----------!
      !- xy_ruh -!
      !----------!
      CALL sub_netcdf_var_and_att_def(                 &
           ncid              = ncid_data_stats       , &
           var_name          = 'xy_ruh'              , &
           var_type          = c_rtype               , &
           var_dimids        = (/dimx_id             , &
           &                     dimy_id             , &
           &                     dim_nb_sect_id/)    , &
           var_id            = xy_ruh_id             , &
           att_title         = 'What is xy_ruh ?'    , &
           att_longname      = 'Stats: xy ruh'       , &
           att_units         = '...'                 , &
           att_missing_value = mask_value              &
           )
      WRITE(lun_standard,*)'  - XY ruh dimensions are defined'

      !-----------!
      !- xy_r2uh -!
      !-----------!
      CALL sub_netcdf_var_and_att_def(                 &
           ncid              = ncid_data_stats       , &
           var_name          = 'xy_r2uh'             , &
           var_type          = c_rtype               , &
           var_dimids        = (/dimx_id             , &
           &                     dimy_id             , &
           &                     dim_nb_sect_id/)    , &
           var_id            = xy_r2uh_id            , &
           att_title         = 'What is xy_r2uh ?'   , &
           att_longname      = 'Stats: xy r2uh'      , &
           att_units         = '...'                 , &
           att_missing_value = mask_value              &
           )
      WRITE(lun_standard,*)'  - XY r2uh dimensions are defined'

      !----------!
      !- xy_rvh -!
      !----------!
      CALL sub_netcdf_var_and_att_def(                 &
           ncid              = ncid_data_stats       , &
           var_name          = 'xy_rvh'              , &
           var_type          = c_rtype               , &
           var_dimids        = (/dimx_id             , &
           &                     dimy_id             , &
           &                     dim_nb_sect_id/)    , &
           var_id            = xy_rvh_id             , &
           att_title         = 'What is xy_rvh ?'    , &
           att_longname      = 'Stats: xy srvh'       , &
           att_units         = '...'                 , &
           att_missing_value = mask_value              &
           )
      WRITE(lun_standard,*)'  - XY rvh dimensions are defined'

      !-----------!
      !- xy_r2vh -!
      !-----------!
      CALL sub_netcdf_var_and_att_def(                 &
           ncid              = ncid_data_stats       , &
           var_name          = 'xy_r2vh'             , &
           var_type          = c_rtype               , &
           var_dimids        = (/dimx_id             , &
           &                     dimy_id             , &
           &                     dim_nb_sect_id/)    , &
           var_id            = xy_r2vh_id            , &
           att_title         = 'What is xy_r2vh ?'   , &
           att_longname      = 'Stats: xy r2vh'      , &
           att_units         = '...'                 , &
           att_missing_value = mask_value              &
           )
      WRITE(lun_standard,*)'  - XY r2vh dimensions are defined'

    ENDIF

    !---------!
    !- Tmask -!
    !---------!
    CALL sub_netcdf_var_and_att_def(                 &
         ncid              = ncid_data_stats       , &
         var_name          = 'tmask'               , &
         var_type          = c_rtype               , &
         var_dimids        = (/dimx_id             , &
         &                     dimy_id             , &
         &                     dimz_id/)           , &
         var_id            = tmask_id              , &
         att_title         = 'What is tmask?'      , &
         att_longname      = '3D T grid mask'      , &
         att_units         = 'no unit'             , &
         att_missing_value = mask_value              &
         )
    WRITE(lun_standard,*)'  - Tmask dimensions are defined'

    !------------------------------------------------!
    !- Save namelist parameters in this netcdf file -!
    !------------------------------------------------!
    CALL sub_save_namelist_in_netcdf(ncid_data_stats)

    !---------------------------------------------!
    !- Close define mode (to enter in data mode) -!
    !---------------------------------------------!
    CALL sub_netcdf_end_def (ncid_data_stats)

  END SUBROUTINE sub_save_netcdf_data_init_stats

  !!***  
  !!=======================================================================
  !!=======================================================================
  SUBROUTINE sub_save_netcdf_init_pos()

    REAL(kind = rprec), DIMENSION(:), ALLOCATABLE :: xlon
    REAL(kind = rprec), DIMENSION(:), ALLOCATABLE :: ylat
    !! REAL(kind = rprec), DIMENSION(:), ALLOCATABLE :: zdepth

    INTEGER(kind = iprec) :: n

    WRITE(lun_standard,*)''
    WRITE(lun_standard,*)'--- Initial positions are saving: ', ncid_data_pos

    !----------!
    !- Init_x -!
    !----------!
    IF (ALLOCATED(tfi)) THEN
      CALL sub_netcdf_generic_write_var( &
           ncid   = ncid_data_pos      , &
           varid  = init_x_id          , &
           values = tfi(1:ntraj)         & 
           )
      WRITE(lun_standard,*)'  - Initial X is done'
    ENDIF

    !----------!
    !- Init_y -!
    !----------!
    IF (ALLOCATED(tfj)) THEN
      CALL sub_netcdf_generic_write_var( &
           ncid   = ncid_data_pos      , &
           varid  = init_y_id          , &
           values = tfj(1:ntraj)         &
           )
      WRITE(lun_standard,*)'  - Initial Y is done'
    ENDIF

    !----------!
    !- Init_z -!
    !----------!
    IF (ALLOCATED(tfk)) THEN
      CALL sub_netcdf_generic_write_var( &
           ncid   = ncid_data_pos      , &
           varid  = init_z_id          , &
           values = tfk(1:ntraj)         &
           )
      WRITE(lun_standard,*)'  - Initial Z is done'
    ENDIF

    !----------!
    !- Init_t -!
    !----------!
    IF (ALLOCATED(tfl)) THEN
      CALL sub_netcdf_generic_write_var( &
           ncid   = ncid_data_pos      , &
           varid  = init_t_id          , &
           values = tfl(1:ntraj)         &
           )
      WRITE(lun_standard,*)'  - Initial Time is done'
    ENDIF

    !------------!
    !- Init_age -!
    !------------!
    IF (ALLOCATED(tage)) THEN
      CALL sub_netcdf_generic_write_var( &
           ncid   = ncid_data_pos      , &
           varid  = init_age_id        , &
           values = tage(1:ntraj)        &
           )
      WRITE(lun_standard,*)'  - Initial Age is done'
    ENDIF

    !---------------------!
    !- initial transport -!
    !---------------------!
    IF (ALLOCATED(ttr)) THEN
      CALL sub_netcdf_generic_write_var( &
           ncid   = ncid_data_pos      , &
           varid  = init_transp_id     , &
           values = ttr(1:ntraj)         &
           )
      WRITE(lun_standard,*)'  - Initial Transport is done'
    ENDIF

    IF ((key_alltracers).AND.(TRIM(mode) == 'quantitative')) THEN
      !-----------------------!
      !- initial temperature -!
      !-----------------------!
      IF (ALLOCATED(init_temp)) THEN
        CALL sub_netcdf_generic_write_var( &
             ncid   = ncid_data_pos      , &
             varid  = init_temp_id       , &
             values = init_temp(1:ntraj)   &
             )
        WRITE(lun_standard,*)'  -Initial  Temperature is done'
      ENDIF

      !--------------------!
      !- initial salinity -!
      !--------------------!
      IF (ALLOCATED(init_salt)) THEN
        CALL sub_netcdf_generic_write_var( &
             ncid   = ncid_data_pos      , &
             varid  = init_salt_id       , &
             values = init_salt(1:ntraj)   &
             )
        WRITE(lun_standard,*)'  - Initial Salinity is done'
      ENDIF

      !-------------------!
      !- initial density -!
      !-------------------!
      IF (ALLOCATED(init_dens)) THEN
        CALL sub_netcdf_generic_write_var( &
             ncid   = ncid_data_pos      , &
             varid  = init_dens_id       , &
             values = init_dens(1:ntraj)   &
             )
        WRITE(lun_standard,*)'  - Initial Density is done'
      ENDIF

    ENDIF

    IF (TRIM(mode) == 'quantitative') THEN

      ALLOCATE(xlon(ntraj))
      CALL sub_memory(SIZE(xlon),'r','xlon','sub_save_netcdf_init_pos')
      ALLOCATE(ylat(ntraj))
      CALL sub_memory(SIZE(ylat),'r','ylat','sub_save_netcdf_init_pos')
      !! NG: 15/09/2008 ALLOCATE(zdepth(ntraj))

      IF (key_roms.OR.key_mars.OR.key_symphonie) THEN

        DO n=1,ntraj
          xlon(n)=fx(tfi(n),tfj(n))
          ylat(n)=fy(tfi(n),tfj(n))
          !! NG: 15/09/2008     zdepth(n)= fz(gi=tfi(n), gj=tfj(n), gk=-tfk(n), il=1)
        ENDDO

      ELSE

        DO n=1,ntraj
          xlon(n)=fx(tfi(n),tfj(n))
          ylat(n)=fy(tfi(n),tfj(n))
          !! NG: 15/09/2008    zdepth(n)=fz(tfi(n),tfj(n),-tfk(n))
        ENDDO

      ENDIF

      !------------!
      !- Init_lon -!
      !------------!
      CALL sub_netcdf_generic_write_var( &
           ncid   = ncid_data_pos      , &
           varid  = init_lon_id        , &
           values = xlon(1:ntraj)        & 
           )
      WRITE(lun_standard,*)'  - Initial Lon is done'

      !------------!
      !- Init_lat -!
      !------------!
      CALL sub_netcdf_generic_write_var( &
           ncid   = ncid_data_pos      , &
           varid  = init_lat_id        , &
           values = ylat(1:ntraj)        &
           )
      WRITE(lun_standard,*)'  - Initial Lat is done'

      !--------------!
      !- Init_depth -!
      !--------------!
      !! NG: 15/09/2008 CALL sub_netcdf_generic_write_var( &
      !! NG: 15/09/2008      ncid   = ncid_data_pos      , &
      !! NG: 15/09/2008      varid  = init_depth_id      , &
      !! NG: 15/09/2008      values = zdepth(1:ntraj)      &
      !! NG: 15/09/2008      )
      !! NG: 15/09/2008 WRITE(lun_standard,*)'  - Initial Depth is done'

      WRITE(lun_standard,*)'  - Depth will be saved at the end'

      CALL sub_memory(-SIZE(xlon),'r','xlon','sub_save_netcdf_init_pos')
      DEALLOCATE(xlon)
      CALL sub_memory(-SIZE(ylat),'r','ylat','sub_save_netcdf_init_pos')
      DEALLOCATE(ylat)
      !! NG: 15/09/2008 DEALLOCATE(zdepth)

    ENDIF


  END SUBROUTINE sub_save_netcdf_init_pos

  !!***  
  !!=======================================================================
  !!=======================================================================
  !! NG: 15_09_2008
  SUBROUTINE sub_save_netcdf_init_depth(zdepth)

    REAL(kind = rprec), DIMENSION(:), INTENT(IN) :: zdepth


    WRITE(lun_standard,*)''
    WRITE(lun_standard,*)'--- Initial Depths are saving: ', ncid_data_pos
    !--------------!
    !- Init_depth -!
    !--------------!
    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_data_pos      , &
         varid  = init_depth_id      , &
         values = zdepth(1:ntraj)      &
         )
    WRITE(lun_standard,*)'  - Initial Depth is done'


  END SUBROUTINE sub_save_netcdf_init_depth

  !!***  
  !!=======================================================================
  !!=======================================================================
  SUBROUTINE sub_save_netcdf_final_pos(    &
       final_x, final_y, final_z, final_t, &
       final_age, final_section, zdepth  , &
       final_temp, final_salt, final_dens  )

    REAL(kind = rprec)   , DIMENSION(:), INTENT(IN) :: final_x
    REAL(kind = rprec)   , DIMENSION(:), INTENT(IN) :: final_y
    REAL(kind = rprec)   , DIMENSION(:), INTENT(IN) :: final_z
    REAL(kind = rprec)   , DIMENSION(:), INTENT(IN) :: final_t
    REAL(kind = rprec)   , DIMENSION(:), INTENT(IN) :: final_age
    INTEGER(kind = iprec), DIMENSION(:), INTENT(IN) :: final_section

    REAL(kind = rprec), OPTIONAL, DIMENSION(:),INTENT(IN) :: zdepth

    REAL(kind = rprec), OPTIONAL, DIMENSION(:), INTENT(IN) :: final_temp
    REAL(kind = rprec), OPTIONAL, DIMENSION(:), INTENT(IN) :: final_salt
    REAL(kind = rprec), OPTIONAL, DIMENSION(:), INTENT(IN) :: final_dens

    REAL(kind = rprec), DIMENSION(:), ALLOCATABLE :: xlon
    REAL(kind = rprec), DIMENSION(:), ALLOCATABLE :: ylat


    INTEGER(kind = iprec) :: n

    WRITE(lun_standard,*)''
    WRITE(lun_standard,*)'--- Final positions are saving: ', ncid_data_pos

    !-----------!
    !- Final_x -!
    !-----------!
    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_data_pos      , &
         varid  = final_x_id         , &
         values = final_x(1:ntraj)     & 
         )
    WRITE(lun_standard,*)'  - Final X is done'

    !-----------!
    !- Final_y -!
    !-----------!
    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_data_pos      , &
         varid  = final_y_id         , &
         values = final_y(1:ntraj)     &
         )
    WRITE(lun_standard,*)'  - Final Y is done'

    !-----------!
    !- Final_z -!
    !-----------!
    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_data_pos      , &
         varid  = final_z_id         , &
         values = final_z(1:ntraj)     &
         )
    WRITE(lun_standard,*)'  - Final Z is done'

    !-----------!
    !- Final_t -!
    !-----------!
    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_data_pos      , &
         varid  = final_t_id         , &
         values = final_t(1:ntraj)    &
         )
    WRITE(lun_standard,*)'  - Final Time is done'

    !-------------!
    !- Final_age -!
    !-------------!
    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_data_pos      , &
         varid  = final_age_id       , &
         values = final_age(1:ntraj)   &
         )
    WRITE(lun_standard,*)'  - Final Age is done'


    !-------------------!
    !- final transport -!
    !-------------------!
    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_data_pos      , &
         varid  = final_transp_id    , &
         values = ttr(1:ntraj)         &
         )
    WRITE(lun_standard,*)'  - Final Transport is done'

    !------------------!
    !- final sections -!
    !------------------!
    IF (TRIM(mode)=='quantitative') THEN

      CALL sub_netcdf_generic_write_var(    &
           ncid   = ncid_data_pos         , &
           varid  = final_section_id      , &
           values = final_section(1:ntraj)  &
           )
      WRITE(lun_standard,*)'  - Final Section is done'

      ALLOCATE(xlon(ntraj))
      CALL sub_memory(SIZE(xlon),'r','xlon','sub_save_netcdf_final_pos')
      ALLOCATE(ylat(ntraj))
      CALL sub_memory(SIZE(ylat),'r','ylat','sub_save_netcdf_final_pos')
      !! NG: 15/09/2008 ALLOCATE(zdepth(ntraj))

      IF (key_roms.OR.key_mars.OR.key_symphonie) THEN

        DO n=1,ntraj
          xlon(n)=fx(final_x(n),final_y(n))
          ylat(n)=fy(final_x(n),final_y(n))
          !! NG: 15/09/2008 zdepth(n)= fz(gi=final_x(n), gj=final_y(n), gk=-final_z(n), il=1)
        ENDDO

      ELSE

        DO n=1,ntraj
          xlon(n)=fx(final_x(n),final_y(n))
          ylat(n)=fy(final_x(n),final_y(n))
          !! NG: 15/09/2008 zdepth(n)=fz(final_x(n),final_y(n),-final_z(n))
        ENDDO

      ENDIF

      !-------------!
      !- Final_lon -!
      !-------------!
      CALL sub_netcdf_generic_write_var( &
           ncid   = ncid_data_pos      , &
           varid  = final_lon_id       , &
           values = xlon(1:ntraj)        & 
           )
      WRITE(lun_standard,*)'  - Final Lon is done'

      !-------------!
      !- Final_lat -!
      !-------------!
      CALL sub_netcdf_generic_write_var( &
           ncid   = ncid_data_pos      , &
           varid  = final_lat_id       , &
           values = ylat(1:ntraj)        &
           )
      WRITE(lun_standard,*)'  - Final Lat is done'

      !---------------!
      !- Final_depth -!
      !---------------!
      CALL sub_netcdf_generic_write_var( &
           ncid   = ncid_data_pos      , &
           varid  = final_depth_id     , &
           values = zdepth(1:ntraj)      &
           )
      WRITE(lun_standard,*)'  - Final Depth is done'

      CALL sub_memory(-SIZE(xlon),'r','xlon','sub_save_netcdf_final_pos')
      DEALLOCATE(xlon)
      CALL sub_memory(-SIZE(ylat),'r','ylat','sub_save_netcdf_final_pos')
      DEALLOCATE(ylat)
      !! NG: 15/09/2008 DEALLOCATE(zdepth)

    ENDIF

    IF ((key_alltracers).AND.(TRIM(mode)=='quantitative')) THEN
      !---------------------!
      !- final temperature -!
      !---------------------!
      IF (PRESENT(final_temp)) THEN
        CALL sub_netcdf_generic_write_var( &
             ncid   = ncid_data_pos      , &
             varid  = final_temp_id      , &
             values = final_temp(1:ntraj)  &
             )
        WRITE(lun_standard,*)'  - Final Temperature is done'
      ENDIF

      !------------------!
      !- final salinity -!
      !------------------!
      IF (PRESENT(final_salt)) THEN
        CALL sub_netcdf_generic_write_var( &
             ncid   = ncid_data_pos      , &
             varid  = final_salt_id      , &
             values = final_salt(1:ntraj)  &
             )
        WRITE(lun_standard,*)'  - Final Salinity is done'
      ENDIF

      !-----------------!
      !- final density -!
      !-----------------!
      IF (PRESENT(final_dens)) THEN
        CALL sub_netcdf_generic_write_var( &
             ncid   = ncid_data_pos      , &
             varid  = final_dens_id      , &
             values = final_dens(1:ntraj)  &
             )
        WRITE(lun_standard,*)'  - Final Density is done'
      ENDIF

    ENDIF

  END SUBROUTINE sub_save_netcdf_final_pos

  !!***  
  !!=======================================================================
  !!=======================================================================
  SUBROUTINE sub_save_netcdf_trajectories(        &
       traj_lon, traj_lat, traj_depth, traj_time, &
       traj_temp, traj_salt, traj_dens,           &
       traj_iU, traj_jV, traj_kW          )

    REAL(kind = rprec), DIMENSION(:,:), INTENT(IN) :: traj_lon
    REAL(kind = rprec), DIMENSION(:,:), INTENT(IN) :: traj_lat
    REAL(kind = rprec), DIMENSION(:,:), INTENT(IN) :: traj_depth
    REAL(kind = rprec), DIMENSION(:,:), INTENT(IN) :: traj_time

    REAL(kind = rprec), OPTIONAL, DIMENSION(:,:), INTENT(IN) :: traj_temp
    REAL(kind = rprec), OPTIONAL, DIMENSION(:,:), INTENT(IN) :: traj_salt
    REAL(kind = rprec), OPTIONAL, DIMENSION(:,:), INTENT(IN) :: traj_dens

    REAL(kind = rprec), OPTIONAL, DIMENSION(:,:), INTENT(IN) :: traj_iU
    REAL(kind = rprec), OPTIONAL, DIMENSION(:,:), INTENT(IN) :: traj_jV
    REAL(kind = rprec), OPTIONAL, DIMENSION(:,:), INTENT(IN) :: traj_kW

    WRITE(lun_standard,*)''
    WRITE(lun_standard,*)'--- Trajectory positions are saving: ', ncid_data_pos

    !------------!
    !- Traj_lon -!
    !------------!
    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_data_pos      , &
         varid  = traj_lon_id        , &
         values = traj_lon(:,:)        & 
         )
    WRITE(lun_standard,*)'  - X is done'

    !------------!
    !- Traj_lat -!
    !------------!
    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_data_pos      , &
         varid  = traj_lat_id        , &
         values = traj_lat(:,:)        &
         )
    WRITE(lun_standard,*)'  - Y is done'

    !--------------!
    !- Traj_depth -!
    !--------------!
    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_data_pos      , &
         varid  = traj_depth_id      , &
         values = traj_depth(:,:)      &
         )
    WRITE(lun_standard,*)'  - Z is done'

    !-------------!
    !- Traj_time -!
    !-------------!
    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_data_pos      , &
         varid  = traj_time_id       , &
         values = traj_time(:,:)       &
         )
    WRITE(lun_standard,*)'  - Time is done'


    IF (key_alltracers) THEN
      !--------------------!
      !- traj temperature -!
      !--------------------!
      IF (PRESENT(traj_temp)) THEN
        CALL sub_netcdf_generic_write_var( &
             ncid   = ncid_data_pos      , &
             varid  = traj_temp_id       , &
             values = traj_temp(:,:)       &
             )
        WRITE(lun_standard,*)'  - Temperatures is done'
      ENDIF

      !-----------------!
      !- traj salinity -!
      !-----------------!
      IF (PRESENT(traj_salt)) THEN
        CALL sub_netcdf_generic_write_var( &
             ncid   = ncid_data_pos      , &
             varid  = traj_salt_id       , &
             values = traj_salt(:,:)       &
             )
        WRITE(lun_standard,*)'  - Salinities is done'
      ENDIF

      !----------------!
      !- traj density -!
      !----------------!
      IF (PRESENT(traj_dens)) THEN
        CALL sub_netcdf_generic_write_var( &
             ncid   = ncid_data_pos      , &
             varid  = traj_dens_id       , &
             values = traj_dens(:,:)       &
             )
        WRITE(lun_standard,*)'  - Densities is done'
      ENDIF

    ENDIF

    IF (key_iU_jV_kW) THEN

      !-----------!
      !- Traj_iU -!
      !-----------!
      CALL sub_netcdf_generic_write_var( &
           ncid   = ncid_data_pos      , &
           varid  = traj_iU_id         , &
           values = traj_iU(:,:)         & 
           )
      WRITE(lun_standard,*)'  - i on grid U is done'

      !-----------!
      !- Traj_jV -!
      !-----------!
      CALL sub_netcdf_generic_write_var( &
           ncid   = ncid_data_pos      , &
           varid  = traj_jV_id         , &
           values = traj_jV(:,:)         & 
           )
      WRITE(lun_standard,*)'  - j on grid V is done'

      !-----------!
      !- Traj_kW -!
      !-----------!
      CALL sub_netcdf_generic_write_var( &
           ncid   = ncid_data_pos      , &
           varid  = traj_kW_id         , &
           values = traj_kW(:,:)         & 
           )
      WRITE(lun_standard,*)'  - k on grid W is done'

    ENDIF

  END SUBROUTINE sub_save_netcdf_trajectories
  !!***  
  !!=======================================================================
  !!=======================================================================
  SUBROUTINE sub_save_netcdf_trajectories_svm(    &
       time_indice                              , &
       traj_lon, traj_lat, traj_depth, traj_time, &
       traj_temp, traj_salt, traj_dens,           &
       traj_iU, traj_jV, traj_kW          )

    INTEGER(kind = iprec) , INTENT(IN) :: time_indice

    REAL(kind = rprec), DIMENSION(:), INTENT(IN) :: traj_lon
    REAL(kind = rprec), DIMENSION(:), INTENT(IN) :: traj_lat
    REAL(kind = rprec), DIMENSION(:), INTENT(IN) :: traj_depth
    REAL(kind = rprec), DIMENSION(:), INTENT(IN) :: traj_time

    REAL(kind = rprec), OPTIONAL, DIMENSION(:), INTENT(IN) :: traj_temp
    REAL(kind = rprec), OPTIONAL, DIMENSION(:), INTENT(IN) :: traj_salt
    REAL(kind = rprec), OPTIONAL, DIMENSION(:), INTENT(IN) :: traj_dens

    REAL(kind = rprec), OPTIONAL, DIMENSION(:), INTENT(IN) :: traj_iU
    REAL(kind = rprec), OPTIONAL, DIMENSION(:), INTENT(IN) :: traj_jV
    REAL(kind = rprec), OPTIONAL, DIMENSION(:), INTENT(IN) :: traj_kW

    WRITE(lun_standard,*)''
    WRITE(lun_standard,*)'--- Save trajectory positions at indice : ', &
         time_indice, ' (',ncid_data_pos,')'

    !------------!
    !- Traj_lon -!
    !------------!
    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_data_pos      , &
         varid  = traj_lon_id        , &
         values = traj_lon(:)        , & 
         start  = (/ 1, time_indice /) &
         )
    WRITE(lun_standard,*)'  - X is done'

    !------------!
    !- Traj_lat -!
    !------------!
    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_data_pos      , &
         varid  = traj_lat_id        , &
         values = traj_lat(:)        , & 
         start  = (/ 1, time_indice /)&
         )
    WRITE(lun_standard,*)'  - Y is done'

    !--------------!
    !- Traj_depth -!
    !--------------!
    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_data_pos      , &
         varid  = traj_depth_id      , &
         values = traj_depth(:)      , & 
         start  = (/ 1, time_indice /)&
         )
    WRITE(lun_standard,*)'  - Z is done'

    !-------------!
    !- Traj_time -!
    !-------------!
    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_data_pos      , &
         varid  = traj_time_id       , &
         values = traj_time(:)       , & 
         start  = (/ 1, time_indice /)&
         )
    WRITE(lun_standard,*)'  - Time is done'


    IF (key_alltracers) THEN
      !--------------------!
      !- traj temperature -!
      !--------------------!
      IF (PRESENT(traj_temp)) THEN
        CALL sub_netcdf_generic_write_var( &
             ncid   = ncid_data_pos      , &
             varid  = traj_temp_id       , &
             values = traj_temp(:)       , & 
             start  = (/ 1, time_indice /)&
             )
        WRITE(lun_standard,*)'  - Temperatures is done'
      ENDIF

      !-----------------!
      !- traj salinity -!
      !-----------------!
      IF (PRESENT(traj_salt)) THEN
        CALL sub_netcdf_generic_write_var( &
             ncid   = ncid_data_pos      , &
             varid  = traj_salt_id       , &
             values = traj_salt(:)       , & 
             start  = (/ 1, time_indice /) &
             )
        WRITE(lun_standard,*)'  - Salinities is done'
      ENDIF

      !----------------!
      !- traj density -!
      !----------------!
      IF (PRESENT(traj_dens)) THEN
        CALL sub_netcdf_generic_write_var( &
             ncid   = ncid_data_pos      , &
             varid  = traj_dens_id       , &
             values = traj_dens(:)       , & 
             start  = (/ 1, time_indice /) &
             )
        WRITE(lun_standard,*)'  - Densities is done'
      ENDIF

      IF (key_iU_jV_kW) THEN

        !-----------!
        !- Traj_iU -!
        !-----------!
        CALL sub_netcdf_generic_write_var( &
             ncid   = ncid_data_pos      , &
             varid  = traj_iU_id         , &
             values = traj_iU(:)         , & 
             start  = (/ 1, time_indice /) & 
             )
        WRITE(lun_standard,*)'  - i on grid U is done'

        !-----------!
        !- Traj_jV -!
        !-----------!
        CALL sub_netcdf_generic_write_var( &
             ncid   = ncid_data_pos      , &
             varid  = traj_jV_id         , &
             values = traj_jV(:)         , & 
             start  = (/ 1, time_indice /) & 
             )
        WRITE(lun_standard,*)'  - j on grid V is done'

        !-----------!
        !- Traj_kW -!
        !-----------!
        CALL sub_netcdf_generic_write_var( &
             ncid   = ncid_data_pos      , &
             varid  = traj_kW_id         , &
             values = traj_kW(:)         , & 
             start  = (/ 1, time_indice /) & 
             )
        WRITE(lun_standard,*)'  - k on grid W is done'

      ENDIF

    ENDIF

  END SUBROUTINE sub_save_netcdf_trajectories_svm
  !!***  
  !!=======================================================================
  !!=======================================================================
  SUBROUTINE sub_save_netcdf_stats()

    INTEGER(kind = iprec) :: beg, nb_sect
    INTEGER(kind = iprec) :: imin_reg
    INTEGER(kind = iprec) :: imax_reg

    IF (key_eco) THEN
      beg     = 1
      nb_sect = 1
    ELSE
      beg = 0
      IF (icrit1 == 1) THEN
        nb_sect = nsect + 1 
      ELSE
        nb_sect = nsect 
      ENDIF
    ENDIF

    WRITE(lun_standard,*)''
    WRITE(lun_standard,*)'--- Statistics are saving: ', ncid_data_stats

    IF ((key_periodic).AND.(dims_reg(1,2) < dims_reg(1,1))) THEN

      uxy(:,:,:)  = CSHIFT(uxy(:,:,:),shift=dims_reg(1,2),dim=1)
      vxy(:,:,:)  = CSHIFT(vxy(:,:,:),shift=dims_reg(1,2),dim=1)
      uxz(:,:,:)  = CSHIFT(uxz(:,:,:),shift=dims_reg(1,2),dim=1)
      wxz(:,:,:)  = CSHIFT(wxz(:,:,:),shift=dims_reg(1,2),dim=1)
      uh(:,:,:)   = CSHIFT(uh(:,:,:),shift=dims_reg(1,2),dim=1)
      vh(:,:,:)   = CSHIFT(vh(:,:,:),shift=dims_reg(1,2),dim=1)
      zuh(:,:,:)  = CSHIFT(zuh(:,:,:),shift=dims_reg(1,2),dim=1)
      zvh(:,:,:)  = CSHIFT(zvh(:,:,:),shift=dims_reg(1,2),dim=1)
      z2uh(:,:,:) = CSHIFT(z2uh(:,:,:),shift=dims_reg(1,2),dim=1)
      z2vh(:,:,:) = CSHIFT(z2vh(:,:,:),shift=dims_reg(1,2),dim=1)

      IF (key_alltracers) THEN
        tuh(:,:,:)  = CSHIFT(tuh(:,:,:),shift=dims_reg(1,2),dim=1)
        tvh(:,:,:)  = CSHIFT(tvh(:,:,:),shift=dims_reg(1,2),dim=1)
        t2uh(:,:,:) = CSHIFT(t2uh(:,:,:),shift=dims_reg(1,2),dim=1)
        t2vh(:,:,:) = CSHIFT(t2vh(:,:,:),shift=dims_reg(1,2),dim=1)
        suh(:,:,:)  = CSHIFT(suh(:,:,:),shift=dims_reg(1,2),dim=1)
        svh(:,:,:)  = CSHIFT(svh(:,:,:),shift=dims_reg(1,2),dim=1)
        s2uh(:,:,:) = CSHIFT(s2uh(:,:,:),shift=dims_reg(1,2),dim=1)
        s2vh(:,:,:) = CSHIFT(s2vh(:,:,:),shift=dims_reg(1,2),dim=1)
        ruh(:,:,:)  = CSHIFT(ruh(:,:,:),shift=dims_reg(1,2),dim=1)
        rvh(:,:,:)  = CSHIFT(rvh(:,:,:),shift=dims_reg(1,2),dim=1)
        r2uh(:,:,:) = CSHIFT(r2uh(:,:,:),shift=dims_reg(1,2),dim=1)
        r2vh(:,:,:) = CSHIFT(r2vh(:,:,:),shift=dims_reg(1,2),dim=1)
      ENDIF

      imin_reg = dims_reg(1,1) - dims_reg(1,2)
      imax_reg = imt

    ELSE
      imin_reg = dims_reg(1,1)
      imax_reg = dims_reg(1,2)

    ENDIF

    !------------!
    !- xy_zonal -!
    !------------!
    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_data_stats    , &
         varid  = xy_zonal_id        , &
         values = uxy(imin_reg:imax_reg, &
         dims_reg(2,1):dims_reg(2,2), &
         beg:nb_sect) &
         )
    WRITE(lun_standard,*)'  - XY zonal is done'

    !----------!
    !- xy_mer -!
    !----------!
    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_data_stats    , &
         varid  = xy_mer_id          , &
         values = vxy(imin_reg:imax_reg, &
         dims_reg(2,1):dims_reg(2,2), &
         beg:nb_sect) &
         )
    WRITE(lun_standard,*)'  - XY meridional is done'

    !------------!
    !- xz_zonal -!
    !------------!
    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_data_stats    , &
         varid  = xz_zonal_id        , &
         values = uxz(imin_reg:imax_reg, &
         dims_reg(3,1):dims_reg(3,2), &
         beg:nb_sect) &
         )
    WRITE(lun_standard,*)'  - XZ zonal is done'

    !-----------!
    !- xz_vert -!
    !-----------!
    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_data_stats    , &
         varid  = xz_vert_id         , &
         values = wxz(imin_reg:imax_reg, &
         dims_reg(3,1):dims_reg(3,2), &
         beg:nb_sect)  &
         )
    WRITE(lun_standard,*)'  - XZ vertical is done'

    !----------!
    !- yz_mer -!
    !----------!
    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_data_stats    , &
         varid  = yz_mer_id          , &
         values = vyz(dims_reg(2,1):dims_reg(2,2), &
         dims_reg(3,1):dims_reg(3,2), &
         beg:nb_sect)  &
         )
    WRITE(lun_standard,*)'  - YZ meridional is done'

    !-----------!
    !- yz_vert -!
    !-----------!
    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_data_stats    , &
         varid  = yz_vert_id         , &
         values = wyz(dims_reg(2,1):dims_reg(2,2), &
         dims_reg(3,1):dims_reg(3,2), &
         beg:nb_sect)  &
         )
    WRITE(lun_standard,*)'  - YZ vertical is done'

    !---------!
    !- xy_uh -!
    !---------!
    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_data_stats    , &
         varid  = xy_uh_id           , &
         values = uh(imin_reg:imax_reg, &
         dims_reg(2,1):dims_reg(2,2), &
         beg:nb_sect)   &
         )
    WRITE(lun_standard,*)'  - XY uh is done'

    !---------!
    !- xy_vh -!
    !---------!
    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_data_stats    , &
         varid  = xy_vh_id           , &
         values = vh(imin_reg:imax_reg, &
         dims_reg(2,1):dims_reg(2,2), &
         beg:nb_sect)   &
         )
    WRITE(lun_standard,*)'  - XY vh is done'

    !----------!
    !- xy_zuh -!
    !----------!
    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_data_stats    , &
         varid  = xy_zuh_id          , &
         values = zuh(imin_reg:imax_reg, &
         dims_reg(2,1):dims_reg(2,2), &
         beg:nb_sect)  &
         )
    WRITE(lun_standard,*)'  - XY zuh is done'

    !-----------!
    !- xy_z2uh -!
    !-----------!
    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_data_stats    , &
         varid  = xy_z2uh_id         , &
         values = z2uh(imin_reg:imax_reg, &
         dims_reg(2,1):dims_reg(2,2), &
         beg:nb_sect) &
         )
    WRITE(lun_standard,*)'  - XY z2uh is done'

    !----------!
    !- xy_zvh -!mask_value
    !----------!
    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_data_stats    , &
         varid  = xy_zvh_id          , &
         values = zvh(imin_reg:imax_reg, &
         dims_reg(2,1):dims_reg(2,2), &
         beg:nb_sect)  &
         )
    WRITE(lun_standard,*)'  - XY zvh is done'

    !-----------!
    !- xy_z2vh -!
    !-----------!
    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_data_stats    , &
         varid  = xy_z2vh_id         , &
         values = z2vh(imin_reg:imax_reg, &
         dims_reg(2,1):dims_reg(2,2), &
         beg:nb_sect) &
         )
    WRITE(lun_standard,*)'  - XY z2vh is done'

    IF (key_alltracers) THEN

      !----------!
      !- xy_tuh -!
      !----------!
      CALL sub_netcdf_generic_write_var( &
           ncid   = ncid_data_stats    , &
           varid  = xy_tuh_id          , &
           values = tuh(imin_reg:imax_reg, &
           dims_reg(2,1):dims_reg(2,2), &
           beg:nb_sect)  &
           )
      WRITE(lun_standard,*)'  - XY tuh is done'

      !-----------!
      !- xy_t2uh -!
      !-----------!
      CALL sub_netcdf_generic_write_var( &
           ncid   = ncid_data_stats    , &
           varid  = xy_t2uh_id         , &
           values = t2uh(imin_reg:imax_reg, &
           dims_reg(2,1):dims_reg(2,2), &
           beg:nb_sect) &
           )
      WRITE(lun_standard,*)'  - XY t2uh is done'

      !----------!
      !- xy_tvh -!
      !----------!
      CALL sub_netcdf_generic_write_var( &
           ncid   = ncid_data_stats    , &
           varid  = xy_tvh_id          , &
           values = tvh(imin_reg:imax_reg, &
           dims_reg(2,1):dims_reg(2,2), &
           beg:nb_sect)  &
           )
      WRITE(lun_standard,*)'  - XY tvh is done'

      !-----------!
      !- xy_t2vh -!
      !-----------!
      CALL sub_netcdf_generic_write_var( &
           ncid   = ncid_data_stats    , &
           varid  = xy_t2vh_id         , &
           values = t2vh(imin_reg:imax_reg, &
           dims_reg(2,1):dims_reg(2,2), &
           beg:nb_sect) &
           )
      WRITE(lun_standard,*)'  - XY t2vh is done'

      !----------!
      !- xy_suh -!
      !----------!
      CALL sub_netcdf_generic_write_var( &
           ncid   = ncid_data_stats    , &
           varid  = xy_suh_id          , &
           values = suh(imin_reg:imax_reg, &
           dims_reg(2,1):dims_reg(2,2), &
           beg:nb_sect)  &
           )
      WRITE(lun_standard,*)'  - XY suh is done'

      !-----------!
      !- xy_s2uh -!
      !-----------!
      CALL sub_netcdf_generic_write_var( &
           ncid   = ncid_data_stats    , &
           varid  = xy_s2uh_id         , &
           values = s2uh(imin_reg:imax_reg, &
           dims_reg(2,1):dims_reg(2,2), &
           beg:nb_sect) &
           )
      WRITE(lun_standard,*)'  - XY s2uh is done'

      !----------!
      !- xy_svh -!
      !----------!
      CALL sub_netcdf_generic_write_var( &
           ncid   = ncid_data_stats    , &
           varid  = xy_svh_id          , &
           values = svh(imin_reg:imax_reg, &
           dims_reg(2,1):dims_reg(2,2), &
           beg:nb_sect)  &
           )
      WRITE(lun_standard,*)'  - XY svh is done'

      !-----------!
      !- xy_s2vh -!
      !-----------!
      CALL sub_netcdf_generic_write_var( &
           ncid   = ncid_data_stats    , &
           varid  = xy_s2vh_id         , &
           values = s2vh(imin_reg:imax_reg, &
           dims_reg(2,1):dims_reg(2,2), &
           beg:nb_sect) &
           )
      WRITE(lun_standard,*)'  - XY s2vh is done'

      !----------!
      !- xy_ruh -!
      !----------!
      CALL sub_netcdf_generic_write_var( &
           ncid   = ncid_data_stats    , &
           varid  = xy_ruh_id          , &
           values = ruh(imin_reg:imax_reg, &
           dims_reg(2,1):dims_reg(2,2), &
           beg:nb_sect)  &
           )
      WRITE(lun_standard,*)'  - XY ruh is done'

      !-----------!
      !- xy_r2uh -!
      !-----------!
      CALL sub_netcdf_generic_write_var( &
           ncid   = ncid_data_stats    , &
           varid  = xy_r2uh_id         , &
           values = r2uh(imin_reg:imax_reg, &
           dims_reg(2,1):dims_reg(2,2), &
           beg:nb_sect) &
           )
      WRITE(lun_standard,*)'  - XY r2uh is done'

      !----------!
      !- xy_rvh -!
      !----------!
      CALL sub_netcdf_generic_write_var( &
           ncid   = ncid_data_stats    , &
           varid  = xy_rvh_id          , &
           values = rvh(imin_reg:imax_reg, &
           dims_reg(2,1):dims_reg(2,2), &
           beg:nb_sect)  &
           )
      WRITE(lun_standard,*)'  - XY rvh is done'

      !-----------!
      !- xy_r2vh -!
      !-----------!
      CALL sub_netcdf_generic_write_var( &
           ncid   = ncid_data_stats    , &
           varid  = xy_r2vh_id         , &
           values = r2vh(imin_reg:imax_reg, &
           dims_reg(2,1):dims_reg(2,2), &
           beg:nb_sect) &
           )
      WRITE(lun_standard,*)'  - XY r2vh is done'

    END IF

    !---------!
    !- Tmask -!
    !---------!
    IF (key_mars) THEN
      CALL sub_netcdf_generic_write_var         ( &
           ncid   = ncid_data_stats             , &
           varid  = tmask_id                    , &
           values = tmask(:,:,1:dims_reg(3,3),1))

    ELSE
      CALL sub_netcdf_generic_write_var( &
           ncid   = ncid_data_stats    , &
           varid  = tmask_id           , &
           values = tmask(:,:,:,1)     )
    ENDIF
    WRITE(lun_standard,*)'  - Tmask is done'

  END SUBROUTINE sub_save_netcdf_stats

  !!***  
  !!========================================================================
  !!****f* mod_save_netcdf/sub_save_netcdf_data()
  !! NAME
  !!   sub_save_netcdf_data()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DATE
  !!   * November 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   *
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_save_netcdf_data_close(ncid)

    INTEGER(kind = iprec), INTENT(in):: ncid

    CALL sub_netcdf_close(ncid)

    WRITE(lun_standard,*)'  - unit number', ncid

  END SUBROUTINE sub_save_netcdf_data_close
  !!***
  !=========================================================================
  !!****f* mod_save_netcdf/sub_namelist_in_netcdf()
  !! NAME
  !!   sub_namelist_in_netcdf()
  !!
  !! FUNCTION
  !!   * Write namelist parameters in the netcdf file in the global 
  !!     attributes part.
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DAT
  !!   * November 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   No arguments.
  !!
  !! TODO
  !!
  !! USED BY
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_save_namelist_in_netcdf(ncid)

    INTEGER(kind = iprec), INTENT(in) :: ncid

!!!!!!!!!!!!
    !! ARIANE !!
!!!!!!!!!!!!
    CALL sub_netcdf_generic_put_att( &
         ncid      = ncid          , &
         att_name  = 'key_roms'    , &
         att_value = key_roms        &
         )

    IF (key_roms) THEN
      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'roms_global_attribute'  , &
           att_value = roms_global_attribute      &
           )
    ENDIF

    CALL sub_netcdf_generic_put_att( &
         ncid      = ncid          , &
         att_name  = 'key_mars'    , &
         att_value = key_mars        &
         )

    CALL sub_netcdf_generic_put_att ( &
         ncid      = ncid           , &
         att_name  = 'key_symphonie', &
         att_value = key_symphonie    &
         )

    CALL sub_netcdf_generic_put_att ( &
         ncid      = ncid           , &
         att_name  = 'key_B2C_grid' , &
         att_value =  key_B2C_grid    &
         )

    CALL sub_netcdf_generic_put_att(    &
         ncid      = ncid             , &
         att_name  = 'key_sequential' , &
         att_value =  key_sequential    &
         )

    CALL sub_netcdf_generic_put_att(   &
         ncid      = ncid             , &
         att_name  = 'key_alltracers' , &
         att_value = key_alltracers     &
         )

    CALL sub_netcdf_generic_put_att(       &
         ncid      = ncid                , &
         att_name  = 'key_ascii_outputs' , &
         att_value = key_ascii_outputs     &
         )

    CALL sub_netcdf_generic_put_att(       &
         ncid      = ncid                , &
         att_name  = 'key_iU_jV_kW'      , &
         att_value = key_iU_jV_kW          &
         )

    CALL sub_netcdf_generic_put_att(  &
         ncid      = ncid            , &
         att_name  = 'key_read_age'  , &
         att_value = key_read_age      &
         )

    CALL sub_netcdf_generic_put_att( &
         ncid     = ncid           , &
         att_name  = 'mode'        , &
         att_value = TRIM(mode)      &
         )

    CALL sub_netcdf_generic_put_att( &
         ncid      = ncid          , &
         att_name  = 'forback'     , &
         att_value = TRIM(forback)   &
         )

    CALL sub_netcdf_generic_put_att( &
         ncid      = ncid          , &
         att_name  = 'bin'         , &
         att_value = TRIM(bin)       &
         )

    CALL sub_netcdf_generic_put_att(   &
         ncid      = ncid            , &
         att_name  = 'init_final'    , &
         att_value = TRIM(init_final)  &
         )

    CALL sub_netcdf_generic_put_att( &
         ncid      = ncid          , &
         att_name  = 'nmax'        , &
         att_value = nmax            &
         )

    CALL sub_netcdf_generic_put_att( &
         ncid      = ncid          , &
         att_name  = 'tunit'       , &
         att_value = tunit           &
         )

    CALL sub_netcdf_generic_put_att( &
         ncid      = ncid          , &
         att_name  = 'ntfic'       , &
         att_value = ntfic           &
         )

    CALL sub_netcdf_generic_put_att( &
         ncid      = ncid          , &
         att_name  = 'tcyc'        , &
         att_value = tcyc            &
         )

    CALL sub_netcdf_generic_put_att(         &
         ncid      = ncid                  , &
         att_name  = 'key_approximatesigma', &
         att_value = key_approximatesigma    &
         )

    CALL sub_netcdf_generic_put_att(      &
         ncid      = ncid               , &
         att_name  = 'key_computesigma' , &
         att_value = key_computesigma     &
         )

    CALL sub_netcdf_generic_put_att(    &
         ncid      = ncid             , &
         att_name  = 'zsigma'         , &
         att_value = zsigma             &
         )

    CALL sub_netcdf_generic_put_att(    &
         ncid      = ncid             , &
         att_name  = 'memory_log'     , &
         att_value = memory_log         &
         )

    CALL sub_netcdf_generic_put_att(    &
         ncid      = ncid             , &
         att_name  = 'output_netcdf_large_file', &
         att_value = output_netcdf_large_file    &
         )


!!!!!!!!!!!!!!!!
    !! SEQUENTIAL !!
!!!!!!!!!!!!!!!!
    IF (key_sequential) THEN

      CALL sub_netcdf_generic_put_att(         &
           ncid      = ncid                  , &
           att_name  = 'key_interp_temporal' , &
           att_value = key_interp_temporal     &
           )

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'maxcycles'   , &
           att_value = maxcycles       &
           )

    ENDIF


!!!!!!!!!!!!!!!!!
    !! QUALITATIVE !!
!!!!!!!!!!!!!!!!!
    IF (TRIM(mode) == 'qualitative') THEN

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'delta_t'     , &
           att_value = delta_t         &
           )

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'frequency'   , &
           att_value = frequency       &
           )

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'nb_output'   , &
           att_value = nb_output       &
           )

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'mask'        , &
           att_value = mask            &
           )

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid         , &
           att_name  = 'key_region' , &
           att_value = key_region     &
           )

      !! Region limits are stored
      IF (key_region) THEN

        !! imt
        CALL sub_netcdf_generic_put_att(  &
             ncid      = ncid             , &
             att_name  = 'imt_reg_start'  , &
             att_value =  dims_reg(1,1)     &
             )
        CALL sub_netcdf_generic_put_att(  &
             ncid      = ncid             , &
             att_name  = 'imt_reg_end'    , &
             att_value =  dims_reg(1,2)     &
             )
        CALL sub_netcdf_generic_put_att(  &
             ncid      = ncid             , &
             att_name  = 'imt_reg'        , &
             att_value =  dims_reg(1,3)     &
             )

        !! jmt
        CALL sub_netcdf_generic_put_att(  &
             ncid      = ncid             , &
             att_name  = 'jmt_reg_start'  , &
             att_value =  dims_reg(2,1)     &
             )
        CALL sub_netcdf_generic_put_att(  &
             ncid      = ncid             , &
             att_name  = 'jmt_reg_end'    , &
             att_value =  dims_reg(2,2)     &
             )
        CALL sub_netcdf_generic_put_att(  &
             ncid      = ncid             , &
             att_name  = 'jmt_reg'        , &
             att_value =  dims_reg(2,3)     &
             )

        !! kmt
        CALL sub_netcdf_generic_put_att(  &
             ncid      = ncid             , &
             att_name  = 'kmt_reg_start'  , &
             att_value =  dims_reg(3,1)     &
             )
        CALL sub_netcdf_generic_put_att(  &
             ncid      = ncid             , &
             att_name  = 'kmt_reg_end'    , &
             att_value =  dims_reg(3,2)     &
             )
        CALL sub_netcdf_generic_put_att(  &
             ncid      = ncid             , &
             att_name  = 'kmt_reg'        , &
             att_value =  dims_reg(3,3)     &
             )

      ENDIF
!!!!!!!!!!!!!!!!!!!!
    ELSE !! QUANTITATIVE !!
!!!!!!!!!!!!!!!!!!!!
      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'key_2dquant' , &
           att_value = key_2dquant     &
           )

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'key_eco'     , &
           att_value = key_eco         &
           )

      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid           , &
           att_name  = 'key_reducmem' , &
           att_value = key_reducmem     &
           )

      !! Region limits are stored
      IF (key_reducmem) THEN

        !! imt !!
        CALL sub_netcdf_generic_put_att(  &
             ncid      = ncid             , &
             att_name  = 'imt_reg_start'  , &
             att_value =  dims_reg(1,1)     &
             )
        CALL sub_netcdf_generic_put_att(  &
             ncid      = ncid             , &
             att_name  = 'imt_reg_end'    , &
             att_value =  dims_reg(1,2)     &
             )
        CALL sub_netcdf_generic_put_att(  &
             ncid      = ncid             , &
             att_name  = 'imt_reg'        , &
             att_value =  dims_reg(1,3)     &
             )

        !! jmt !!
        CALL sub_netcdf_generic_put_att(  &
             ncid      = ncid             , &
             att_name  = 'jmt_reg_start'  , &
             att_value =  dims_reg(2,1)     &
             )
        CALL sub_netcdf_generic_put_att(  &
             ncid      = ncid             , &
             att_name  = 'jmt_reg_end'    , &
             att_value =  dims_reg(2,2)     &
             )
        CALL sub_netcdf_generic_put_att(  &
             ncid      = ncid             , &
             att_name  = 'jmt_reg'        , &
             att_value =  dims_reg(2,3)     &
             )

        !! kmt !!
        CALL sub_netcdf_generic_put_att(  &
             ncid      = ncid             , &
             att_name  = 'kmt_reg_start'  , &
             att_value =  dims_reg(3,1)     &
             )
        CALL sub_netcdf_generic_put_att(  &
             ncid      = ncid             , &
             att_name  = 'kmt_reg_end'    , &
             att_value =  dims_reg(3,2)     &
             )
        CALL sub_netcdf_generic_put_att(  &
             ncid      = ncid             , &
             att_name  = 'kmt_reg'        , &
             att_value =  dims_reg(3,3)     &
             )

      ENDIF


      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'key_unitm3'  , &
           att_value = key_unitm3      &
           )

      CALL sub_netcdf_generic_put_att(         &
           ncid      = ncid                  , &
           att_name  = 'key_nointerpolstats' , &
           att_value = key_nointerpolstats     &
           )

      CALL sub_netcdf_generic_put_att(   &
           ncid      = ncid            , &
           att_name  = 'max_transport' , &
           att_value = max_transport     &
           )

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'lmin'        , &
           att_value = lmin            &
           )

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'lmax'        , &
           att_value = lmax            &
           )

    ENDIF



!!!!!!!!!!
    !! ROMS !!
!!!!!!!!!!
    IF (key_roms) THEN

!!!!!!!!!!!!!!!!
      !! Dimensions !!
!!!!!!!!!!!!!!!!
      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'xi_rho'      , &
           att_value = xi_rho          &
           )

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'eta_rho'     , &
           att_value = eta_rho         &
           )

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 's_w'         , &
           att_value = s_w             &
           )

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'time'        , &
           att_value = time            & 
           )

!!!!!!!!!!
      !! ZETA !!
!!!!!!!!!!
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'c_dir_ze'     , &
           att_value = TRIM(c_dir_ze)  &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'c_prefix_ze'     , &
           att_value = TRIM(c_prefix_ze)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'ind0_ze'     , &
           att_value =  ind0_ze  &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'indn_ze'     , &
           att_value =  indn_ze  &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'maxsize_ze'     , &
           att_value = maxsize_ze   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'c_suffix_ze'     , &
           att_value =  TRIM(c_suffix_ze)  &
           )

      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_var_ze'     , &
           att_value = TRIM(nc_var_ze)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_att_mask_ze'     , &
           att_value = TRIM(nc_att_mask_ze)   &
           )

!!!!!!!!!!!!!!!!!!!!!!!!!!!
      !! ROMS GLOBAL ATTRIBUTS !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'dir_glbatt'     , &
           att_value = TRIM(dir_glbatt)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'fn_glbatt'     , &
           att_value = TRIM(fn_glbatt)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_glbatt_hc'     , &
           att_value = TRIM(nc_glbatt_hc)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_glbatt_sc_w'     , &
           att_value = TRIM(nc_glbatt_sc_w)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_glbatt_Cs_w'     , &
           att_value = TRIM(nc_glbatt_Cs_w)   &
           )

!!!!!!!!!!!!!
      !! GRDROMS !!
!!!!!!!!!!!!!
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'dir_grd_roms'     , &
           att_value =  TRIM(dir_grd_roms)  &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'fn_grd_roms'     , &
           att_value = TRIM(fn_grd_roms)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_var_lon_rho_roms'     , &
           att_value = TRIM(nc_var_lon_rho_roms)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_var_lon_u_roms'     , &
           att_value = TRIM(nc_var_lon_u_roms)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_var_lat_rho_roms'     , &
           att_value = nc_var_lat_rho_roms    &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_var_lat_v_roms'     , &
           att_value = TRIM(nc_var_lat_v_roms)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_var_pm_roms'     , &
           att_value = TRIM(nc_var_pm_roms)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_var_pn_roms'     , &
           att_value =  TRIM(nc_var_pn_roms)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_var_h_roms'     , &
           att_value =  TRIM(nc_var_h_roms)  &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_var_mask_rho_roms'     , &
           att_value = TRIM(nc_var_mask_rho_roms)   &
           )

!!!!!!!!!!
      !! MARS !!
!!!!!!!!!!
    ELSEIF (key_mars) THEN

!!!!!!!!!!!!!!!!
      !! Dimensions !!
!!!!!!!!!!!!!!!!
      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'x_t'         , &
           att_value = x_t             &
           )

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'y_t'         , &
           att_value = y_t             &
           )

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'sigma_t'     , &
           att_value = sigma_t         &
           )

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'time'        , &
           att_value = time            & 
           )

!!!!!!!!!!
      !! ZETA !!
!!!!!!!!!!
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'c_dir_ze'     , &
           att_value = TRIM(c_dir_ze)  &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'c_prefix_ze'     , &
           att_value = TRIM(c_prefix_ze)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'ind0_ze'     , &
           att_value =  ind0_ze  &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'indn_ze'     , &
           att_value =  indn_ze  &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'maxsize_ze'     , &
           att_value = maxsize_ze   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'c_suffix_ze'     , &
           att_value =  TRIM(c_suffix_ze)  &
           )

      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_var_ze'     , &
           att_value = TRIM(nc_var_ze)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_att_mask_ze'     , &
           att_value = TRIM(nc_att_mask_ze)   &
           )

!!!!!!!!!!!!!
      !! GRDMARS !!
!!!!!!!!!!!!!
      CALL sub_netcdf_generic_put_att     ( &
           ncid      = ncid               , &
           att_name  = 'dir_grd_mars'     , &
           att_value =  TRIM(dir_grd_mars)  &
           )
      CALL sub_netcdf_generic_put_att   ( &
           ncid      = ncid             , &
           att_name  = 'fn_grd_mars'    , &
           att_value = TRIM(fn_grd_mars)  &
           )
      CALL sub_netcdf_generic_put_att         ( &
           ncid      = ncid                   , &
           att_name  = 'nc_var_lon_t_mars'    , &
           att_value = TRIM(nc_var_lon_t_mars)  &
           )
      CALL sub_netcdf_generic_put_att         ( &
           ncid      = ncid                   , &
           att_name  = 'nc_var_lon_u_mars'    , &
           att_value = TRIM(nc_var_lon_u_mars)  &
           )
      CALL sub_netcdf_generic_put_att         ( &
           ncid      = ncid                   , &
           att_name  = 'nc_var_lat_t_mars'    , &
           att_value = nc_var_lat_t_mars        &
           )
      CALL sub_netcdf_generic_put_att         ( &
           ncid      = ncid                   , &
           att_name  = 'nc_var_lat_v_mars'    , &
           att_value = TRIM(nc_var_lat_v_mars)  &
           )
      CALL sub_netcdf_generic_put_att         ( &
           ncid      = ncid                   , &
           att_name  = 'nc_var_hc'            , &
           att_value = TRIM(nc_glbatt_hc      ) &
           )
      CALL sub_netcdf_generic_put_att         ( &
           ncid      = ncid                   , &
           att_name  = 'nc_var_sc_w'          , &
           att_value = TRIM(nc_glbatt_sc_w    ) &
           )
      CALL sub_netcdf_generic_put_att         ( &
           ncid      = ncid                   , &
           att_name  = 'nc_var_Cs_w'          , &
           att_value = TRIM(nc_glbatt_Cs_w    ) &
           )
      CALL sub_netcdf_generic_put_att           ( &
           ncid      = ncid                     , &
           att_name  = 'nc_var_bathy_t_mars'    , &
           att_value =  TRIM(nc_var_bathy_t_mars) &
           )
      CALL sub_netcdf_generic_put_att          ( &
           ncid      = ncid                    , &
           att_name  = 'nc_var_mask_t_mars'    , &
           att_value = TRIM(nc_var_mask_t_mars)  &
           )


!!!!!!!!!!!!!!!
      !! SYMPHONIE !!
!!!!!!!!!!!!!!!
    ELSEIF (key_symphonie) THEN

!!!!!!!!!!!!!!!!
      !! Dimensions !!
!!!!!!!!!!!!!!!!
      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'x_dim'       , &
           att_value = x_dim           &
           )
      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'y_dim'       , &
           att_value = y_dim           &
           )
      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'z_dim'       , &
           att_value = z_dim           &
           )
      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'time'        , &
           att_value = time            & 
           )

!!!!!!!!!
      !! SSE !!
!!!!!!!!!
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'c_dir_sse'     , &
           att_value = TRIM(c_dir_sse)  &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'c_prefix_sse'     , &
           att_value = TRIM(c_prefix_sse)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'ind0_sse'     , &
           att_value =  ind0_sse  &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'indn_sse'     , &
           att_value =  indn_sse  &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'maxsize_sse'     , &
           att_value = maxsize_sse   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'c_suffix_sse'     , &
           att_value =  TRIM(c_suffix_sse)  &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_var_sse'     , &
           att_value = TRIM(nc_var_sse)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_att_mask_sse'     , &
           att_value = TRIM(nc_att_mask_sse)   &
           )

!!!!!!!!!!!!!!!!!!
      !! GRDSYMPHONIE !!
!!!!!!!!!!!!!!!!!!
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'dir_grd_symp'     , &
           att_value =  TRIM(dir_grd_symp)  &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'fn_grd_symp'     , &
           att_value = TRIM(fn_grd_symp)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_var_lon_t_symp'     , &
           att_value = TRIM(nc_var_lon_t_symp)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_var_lon_u_symp'     , &
           att_value = TRIM(nc_var_lon_u_symp)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_var_lat_t_symp'     , &
           att_value =  TRIM(nc_var_lat_t_symp)  &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_var_lat_v_symp'     , &
           att_value = TRIM(nc_var_lat_v_symp)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_var_depth_t_symp'     , &
           att_value = TRIM(nc_var_depth_t_symp)   &
           )

!!!!!!!!!
      !! B2C !!
!!!!!!!!!
    ELSEIF (key_B2C_grid) THEN

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'B2C_grid_Z_or_Sigma', &
           att_value = B2C_grid_Z_or_Sigma    &
           ) 

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'nb_dim_lon'  , &
           att_value = nb_dim_lon      &
           ) 

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'nb_dim_lat'  , &
           att_value = nb_dim_lat      &
           ) 

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'nb_dim_depth', &
           att_value =  nb_dim_depth   &
           ) 

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'nb_dim_time' , &
           att_value =  nb_dim_time    &
           ) 

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'key_add_bottom', &
           att_value =  key_add_bottom   &
           ) 

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'key_partialsteps', &
           att_value =  key_partialsteps   &
           ) 

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'key_B2C_save_data', &
           att_value =  key_B2C_save_data   &
           ) 

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'key_read_w'  , &
           att_value =  key_read_w     &
           ) 

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'periodic_lon', &
           att_value =  periodic_lon   &
           ) 

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'periodic_lat', &
           att_value =  periodic_lat   &
           ) 

!!!!!!!!!!!!!!
      !! B2CGRIDZ !!
!!!!!!!!!!!!!!
      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'dir_B2C_grid', &
           att_value = dir_B2C_grid    &
           ) 

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'file_name_B2C_grid', &
           att_value =  file_name_B2C_grid   &
           ) 

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'nc_var_xx_tt', &
           att_value =  nc_var_xx_tt   &
           ) 

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'nc_var_xx_uu', &
           att_value =  nc_var_xx_uu   &
           ) 

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'nc_var_yy_tt', &
           att_value =  nc_var_yy_tt   &
           ) 

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'nc_var_yy_vv', &
           att_value =  nc_var_yy_vv   &
           ) 

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'nc_var_zz_ww', &
           att_value =  nc_var_zz_ww   &
           ) 

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'nc_var_e1f', &
           att_value =  nc_var_e1f   &
           ) 

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'nc_var_e2f', &
           att_value =  nc_var_e2f   &
           ) 

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'nc_var_e3f', &
           att_value =  nc_var_e3f   &
           ) 

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'nc_var_e2u', &
           att_value =  nc_var_e2u   &
           ) 

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'nc_var_e1v', &
           att_value =  nc_var_e1v   &
           ) 

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'nc_var_e1t', &
           att_value =  nc_var_e1t   &
           ) 

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'nc_var_e2t', &
           att_value =  nc_var_e2t   &
           ) 

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'nc_var_e3t', &
           att_value =  nc_var_e3t   &
           ) 

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'nc_var_tmask', &
           att_value =  nc_var_tmask   &
           ) 

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'nc_mask_val', &
           att_value =  nc_mask_val   &
           ) 

!!!!!!!!!!
    ELSE !! OPA  !!
!!!!!!!!!!

!!!!!!!!!!!!!!!!
      !! Dimensions !!
!!!!!!!!!!!!!!!!
      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'imt'         , &
           att_value = imt             &
           )

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'jmt'         , &
           att_value = jmt             &
           )

      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'kmt'         , &
           att_value = kmt             &
           )
      CALL sub_netcdf_generic_put_att( &
           ncid      = ncid          , &
           att_name  = 'lmt'         , &
           att_value = lmt             &
           )

!!!!!!!!!!!!!!
      !! OPAPARAM !!
!!!!!!!!!!!!!!
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid           , &
           att_name  = 'key_computew' , &
           att_value = key_computew     &
           )
      CALL sub_netcdf_generic_put_att(   &
           ncid      = ncid            , &
           att_name  = 'w_surf_option' , &
           att_value = w_surf_option     &
           )
      IF (TRIM(w_surf_option) == 'E-P-R') THEN
        CALL sub_netcdf_generic_put_att( &
             ncid      = ncid             , &
             att_name  = 'epr_coef'       , &
             att_value = epr_coef           &
             )
      ENDIF
      CALL sub_netcdf_generic_put_att(      &
           ncid      = ncid               , &
           att_name  = 'key_partialsteps' , &
           att_value = key_partialsteps     &
           )
      CALL sub_netcdf_generic_put_att( &
           ncid     = ncid           , &
           att_name  = 'key_jfold'   , &
           att_value = key_jfold       &
           )
      CALL sub_netcdf_generic_put_att( &
           ncid     = ncid           , &
           att_name  = 'pivot'       , &
           att_value = pivot           &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid           , &
           att_name  = 'key_periodic' , &
           att_value = key_periodic     &
           )


!!!!!!!!!!
      !! MESH !!
!!!!!!!!!!
      CALL sub_netcdf_generic_put_att(   &
           ncid      = ncid            , &
           att_name  = 'dir_mesh'      , &
           att_value =  TRIM(dir_mesh)   &
           )
      CALL sub_netcdf_generic_put_att(   &
           ncid      = ncid            , &
           att_name  = 'fn_mesh'       , &
           att_value = TRIM(fn_mesh)     &
           )
      CALL sub_netcdf_generic_put_att(      &
           ncid      = ncid               , &
           att_name  = 'nc_var_xx_tt'     , &
           att_value = TRIM(nc_var_xx_tt)   &
           )
      CALL sub_netcdf_generic_put_att(      &
           ncid      = ncid               , &
           att_name  = 'nc_var_xx_uu'     , &
           att_value = TRIM(nc_var_xx_uu)   &
           )
      CALL sub_netcdf_generic_put_att(      &
           ncid      = ncid               , &
           att_name  = 'nc_var_yy_tt'     , &
           att_value = TRIM(nc_var_yy_tt)   &
           )
      CALL sub_netcdf_generic_put_att(      &
           ncid      = ncid               , &
           att_name  = 'nc_var_yy_vv'     , &
           att_value = TRIM(nc_var_yy_vv)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_var_zz_ww'     , &
           att_value = TRIM(nc_var_zz_ww)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_var_e2u'     , &
           att_value = TRIM(nc_var_e2u)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_var_e1v'     , &
           att_value = TRIM(nc_var_e1v)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_var_e1t'     , &
           att_value =  TRIM(nc_var_e1t)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_var_e2t'     , &
           att_value =  TRIM(nc_var_e2t)  &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_var_e3t'     , &
           att_value = TRIM(nc_var_e3t)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_var_tmask'     , &
           att_value = TRIM(nc_var_tmask)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_mask_val'     , &
           att_value = nc_mask_val   &
           )

    ENDIF !! ROMS / SYMPHONIE / OPA !!

!!!!!!!!!!!!!!!!!!!
    !! ZONAL CURRENT !!
!!!!!!!!!!!!!!!!!!!
    CALL sub_netcdf_generic_put_att(  &
         ncid      = ncid            , &
         att_name  = 'c_dir_zo'      , &
         att_value = TRIM(c_dir_zo)    &
         )
    CALL sub_netcdf_generic_put_att(   &
         ncid      = ncid             , &
         att_name  = 'c_prefix_zo'    , &
         att_value = TRIM(c_prefix_zo)  &
         )
    CALL sub_netcdf_generic_put_att(  &
         ncid      = ncid            , &
         att_name  = 'ind0_zo'       , &
         att_value =  ind0_zo          &
         )
    CALL sub_netcdf_generic_put_att(  &
         ncid      = ncid            , &
         att_name  = 'indn_zo'       , &
         att_value = indn_zo           &
         )
    CALL sub_netcdf_generic_put_att(  &
         ncid      = ncid            , &
         att_name  = 'maxsize_zo'    , &
         att_value =  maxsize_zo       &
         )
    CALL sub_netcdf_generic_put_att(   &
         ncid      = ncid             , &
         att_name  = 'c_suffix_zo'    , &
         att_value = TRIM(c_suffix_zo)  &
         )
    CALL sub_netcdf_generic_put_att(  &
         ncid      = ncid            , &
         att_name  = 'nc_var_zo'     , &
         att_value =  nc_var_zo        &
         )
    CALL sub_netcdf_generic_put_att(    &
         ncid      = ncid              , &
         att_name  = 'nc_var_eivu'     , &
         att_value =  TRIM(nc_var_eivu)  &
         )
    CALL sub_netcdf_generic_put_att(       &
         ncid      = ncid                 , &
         att_name  = 'nc_att_mask_zo'     , &
         att_value = TRIM( nc_att_mask_zo)  &
         )

!!!!!!!!!!!!!!!!!!!!!!!!
    !! MERIDIONAL CURRENT !!
!!!!!!!!!!!!!!!!!!!!!!!!
    CALL sub_netcdf_generic_put_att(  &
         ncid      = ncid            , &
         att_name  = 'c_dir_me'      , &
         att_value = TRIM(c_dir_me)    &
         )
    CALL sub_netcdf_generic_put_att(   &
         ncid      = ncid             , &
         att_name  = 'c_prefix_me'    , &
         att_value = TRIM(c_prefix_me)  &
         )
    CALL sub_netcdf_generic_put_att( &
         ncid      = ncid           , &
         att_name  = 'ind0_me'      , &
         att_value =  ind0_me         &
         )
    CALL sub_netcdf_generic_put_att( &
         ncid      = ncid           , &
         att_name  = 'indn_me'      , &
         att_value = indn_me          &
         )
    CALL sub_netcdf_generic_put_att(  &
         ncid      = ncid            , &
         att_name  = 'maxsize_me'    , &
         att_value = maxsize_me        &
         )
    CALL sub_netcdf_generic_put_att(    &
         ncid      = ncid              , &
         att_name  = 'c_suffix_me'     , &
         att_value =  TRIM(c_suffix_me)  &
         )
    CALL sub_netcdf_generic_put_att(  &
         ncid      = ncid            , &
         att_name  = 'nc_var_me'     , &
         att_value = TRIM(nc_var_me)   &
         )
    CALL sub_netcdf_generic_put_att(    &
         ncid      = ncid              , &
         att_name  = 'nc_var_eivv'     , &
         att_value =  TRIM(nc_var_eivv)  &
         )
    CALL sub_netcdf_generic_put_att(      &
         ncid      = ncid                 , &
         att_name  = 'nc_att_mask_me'     , &
         att_value =  TRIM(nc_att_mask_me)  &
         )

!!!!!!!!!!!!!!!!!!!!!!
    !! VERTICAL CURRENT !!
!!!!!!!!!!!!!!!!!!!!!!
    IF (.NOT.(key_computew)) THEN
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'c_dir_ve'      , &
           att_value =  TRIM(c_dir_ve)   &
           )
      CALL sub_netcdf_generic_put_att(    &
           ncid      = ncid              , &
           att_name  = 'c_prefix_ve'     , &
           att_value =  TRIM(c_prefix_ve)  &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'ind0_ve'       , &
           att_value =  ind0_ve          &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'indn_ve'       , &
           att_value =  indn_ve          &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'maxsize_ve'    , &
           att_value = maxsize_ve        &
           )
      CALL sub_netcdf_generic_put_att(   &
           ncid      = ncid             , &
           att_name  = 'c_suffix_ve'    , &
           att_value = TRIM(c_suffix_ve)  &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_var_ve'     , &
           att_value = TRIM(nc_var_ve)   &
           )

      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_var_eivw'     , &
           att_value = TRIM(nc_var_eivw)   &
           )

      CALL sub_netcdf_generic_put_att(      &
           ncid      = ncid                , &
           att_name  = 'nc_att_mask_ve'    , &
           att_value = TRIM(nc_att_mask_ve)  &
           )
    ENDIF

    IF (TRIM(w_surf_option) == 'E-P-R') THEN
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'c_dir_ep'      , &
           att_value =  TRIM(c_dir_ep)   &
           )
      CALL sub_netcdf_generic_put_att(    &
           ncid      = ncid              , &
           att_name  = 'c_prefix_ep'     , &
           att_value =  TRIM(c_prefix_ep)  &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'ind0_ep'       , &
           att_value =  ind0_ep          &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'indn_ep'       , &
           att_value =  indn_ep          &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'maxsize_ep'    , &
           att_value = maxsize_ep        &
           )
      CALL sub_netcdf_generic_put_att(   &
           ncid      = ncid             , &
           att_name  = 'c_suffix_ep'    , &
           att_value = TRIM(c_suffix_ep)  &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_var_ep'     , &
           att_value = TRIM(nc_var_ep)   &
           )

      CALL sub_netcdf_generic_put_att(      &
           ncid      = ncid                , &
           att_name  = 'nc_att_mask_ep'    , &
           att_value = TRIM(nc_att_mask_ep)  &
           )
    ENDIF

!!!!!!!!!!!!!
    !! TRACERS !!
!!!!!!!!!!!!!
    IF (key_alltracers) THEN

!!!!!!!!!!!!!!!!!
      !! TEMPERATURE !!
!!!!!!!!!!!!!!!!!
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'c_dir_te'     , &
           att_value = TRIM(c_dir_te)   &
           )

      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'c_prefix_te'     , &
           att_value = TRIM(c_prefix_te)   &
           )

      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'ind0_te'     , &
           att_value = ind0_te   &
           )

      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'indn_te'     , &
           att_value = indn_te   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'maxsize_te'     , &
           att_value = maxsize_te   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'c_suffix_te'     , &
           att_value = TRIM(c_suffix_te)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_var_te'     , &
           att_value = TRIM(nc_var_te)    &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_att_mask_te'     , &
           att_value = TRIM(nc_att_mask_te)   &
           )

!!!!!!!!!!!!!!
      !! SALINITY !!
!!!!!!!!!!!!!!
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'c_dir_sa'     , &
           att_value = TRIM(c_dir_sa)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'c_prefix_sa'     , &
           att_value = TRIM(c_prefix_sa)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'ind0_sa'     , &
           att_value =  ind0_sa  &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'indn_sa'     , &
           att_value =  indn_sa  &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'maxsize_sa'     , &
           att_value =  maxsize_sa  &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'c_suffix_sa'     , &
           att_value = TRIM(c_suffix_sa)   &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_var_sa'     , &
           att_value =  TRIM(nc_var_sa)  &
           )
      CALL sub_netcdf_generic_put_att(  &
           ncid      = ncid            , &
           att_name  = 'nc_att_mask_sa'     , &
           att_value = TRIM(nc_att_mask_sa)   &
           )

!!!!!!!!!!!!!
      !! DENSITY !!
!!!!!!!!!!!!!
      IF (.NOT.(key_sigma)) THEN
        CALL sub_netcdf_generic_put_att(  &
             ncid      = ncid            , &
             att_name  = 'c_dir_de'     , &
             att_value = TRIM(c_dir_de)  &
             )
        CALL sub_netcdf_generic_put_att(  &
             ncid      = ncid            , &
             att_name  = 'c_prefix_de'     , &
             att_value = TRIM(c_prefix_de)   &
             )
        CALL sub_netcdf_generic_put_att(  &
             ncid      = ncid            , &
             att_name  = 'ind0_de'     , &
             att_value =  ind0_de  &
             )
        CALL sub_netcdf_generic_put_att(  &
             ncid      = ncid            , &
             att_name  = 'indn_de'     , &
             att_value =  indn_de  &
             )
        CALL sub_netcdf_generic_put_att(  &
             ncid      = ncid            , &
             att_name  = 'maxsize_de'     , &
             att_value = maxsize_de   &
             )
        CALL sub_netcdf_generic_put_att(  &
             ncid      = ncid            , &
             att_name  = 'c_suffix_de'     , &
             att_value =  TRIM(c_suffix_de)  &
             )

        CALL sub_netcdf_generic_put_att(  &
             ncid      = ncid            , &
             att_name  = 'nc_var_de'     , &
             att_value = TRIM(nc_var_de)   &
             )
        CALL sub_netcdf_generic_put_att(  &
             ncid      = ncid            , &
             att_name  = 'nc_att_mask_de'     , &
             att_value = TRIM(nc_att_mask_de)   &
             )
      ENDIF
    ENDIF

    !!NG     CALL sub_netcdf_generic_put_att(  &
    !!NG         ncid     = ncid            , &
    !!NG         att_name  =      , &
    !!NG         att_value =   &
    !!NG         )

  END SUBROUTINE sub_save_namelist_in_netcdf
  !!***

END MODULE mod_save_netcdf
