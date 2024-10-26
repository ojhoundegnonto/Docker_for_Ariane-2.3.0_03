!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! - Ariane - (May - 2007)
!! m
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
!!****h* ariane/mod_B2C_grid_save
!! NAME
!!   mod_B2C_grid_save
!!
!! FUNCTION
!!   .
!!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
!!
!! AUTHOR
!!   * Origin  : Nicolas Grima
!! 
!! CREATION DATE
!!   * November 2008
!!
!! HISTORY
!!   Date (dd/mm/yyyy/) - Modification(s)
!!
!! ARGUMENTS
!!
!! TODO
!!   
!!
!! USED BY
!!   
!!
!! SOURCE
!!=======================================================================
MODULE mod_B2C_grid_save

  USE mod_precision
  USE mod_cst
  USE mod_which_type
  USE mod_namelist
  USE mod_input_grid
  USE mod_netcdf_output

  IMPLICIT NONE

  INTEGER(kind = iprec) :: ncid_B2C_U
  INTEGER(kind = iprec) :: uc_id
  INTEGER(kind = iprec) :: ind_time_u


  INTEGER(kind = iprec) :: ncid_B2C_V
  INTEGER(kind = iprec) :: vc_id
  INTEGER(kind = iprec) :: ind_time_V

  INTEGER(kind = iprec) :: ncid_B2C_W
  INTEGER(kind = iprec) :: wc_id
  INTEGER(kind = iprec) :: ind_time_W

  !!***  
CONTAINS
!!$  !=========================================================================
!!$  !!****f* mod_save_netcdf/sub_B2C_grid_save()
!!$  !! NAME
!!$  !!   sub_B2C_grid_save()
!!$  !!
!!$  !! FUNCTION
!!$  !!   .
!!$  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
!!$  !!
!!$  !! AUTHOR
!!$  !!   * Origin  : Nicolas Grima
!!$  !! 
!!$  !! CREATION DATE
!!$  !!   * November 2008
!!$  !!
!!$  !! HISTORY
!!$  !!   Date (dd/mm/yyyy/) - Modification(s)
!!$  !!
!!$  !! ARGUMENTS
!!$  !!
!!$  !! TODO
!!$  !!   
!!$  !!
!!$  !! USED BY
!!$  !!   
!!$  !!
!!$  !! SOURCE
!!$  !!=======================================================================
!!$  SUBROUTINE sub_B2C_grid_save(uu,vv)
!!$
!!$    REAL(kind = rprec), DIMENSION(:,:,:,:), intent(in) :: uu, vv
!!$
!!$    !!CALL sub_B2C_grid_save_U(uu)
!!$
!!$    !!CALL sub_B2C_grid_save_V(vv)
!!$
!!$  END SUBROUTINE sub_B2C_grid_save
  !*****
  !=========================================================================
  !!****f* mod_save_netcdf/sub_B2C_grid_save_U()
  !! NAME
  !!   sub_B2C_grid_save_U()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DATE
  !!   * November 2008
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_B2C_grid_save_U(uu,umask)

    REAL(kind = rprec), DIMENSION(:,:,:,:), INTENT(in) :: uu
    INTEGER(kind = iprec), DIMENSION(:,:,:,:), INTENT(in) :: umask


    REAL(kind = rprec), DIMENSION(:,:,:,:), ALLOCATABLE :: tmp_array

    INTEGER(kind = iprec) :: ncid_B2C
    INTEGER(kind = iprec) :: uc_id

    INTEGER(kind = iprec) :: dim_lon_id
    INTEGER(kind = iprec) :: dim_lat_id
    INTEGER(kind = iprec) :: dim_depth_id
    INTEGER(kind = iprec) :: dim_time_id

    INTEGER(kind = iprec), DIMENSION(:), ALLOCATABLE :: dims_out

    CHARACTER(len = 64)   :: c_name
    CHARACTER(len = 2)    :: c_itype
    CHARACTER(len = 2)    :: c_rstype, c_rltype
    INTEGER(kind = iprec) :: itype = 0
    REAL(kind = rshort)   :: real_short_type = 0._rshort
    REAL(kind = rlong)    :: real_long_type  = 0._rlong

    INTEGER(kind = iprec) :: loop_time
    INTEGER(kind = iprec) :: nb_i,nb_j,nb_k,nb_l

    WRITE(lun_standard,*)''
    WRITE(lun_standard,*)'=== Create and Define NetCDF ouput B2C-grid file Ugrid ==='

    nb_i=SIZE(uu,dim=1)
    nb_j=SIZE(uu,dim=2)
    nb_k=SIZE(uu,dim=3)
    nb_l=SIZE(uu,dim=4)

    ALLOCATE(tmp_array(nb_i,nb_j,nb_k,1))
    CALL sub_memory(size(tmp_array),'r','tmp_array','sub_B2C_grid_save_U')

    !----------------------!
    !- Create NetCDF File -!
    !----------------------!
    c_name = 'B2C_grid_data_Ugrid.nc'

    CALL sub_netcdf_create( &
         TRIM(c_name)     , &
         ncid_B2C           &
         )
    WRITE(lun_standard,*)'--- Successful creation of ',TRIM(c_name),' ---', ncid_B2C

    !---------------------------------------!
    !- Write dimensions in the NetCDF file -!
    !---------------------------------------!

    ALLOCATE(dims_out(4))
    CALL sub_memory(size(dims_out),'i','dims_out','sub_B2C_grid_save_U')

    CALL sub_netcdf_dimensions(                                       &
         ncid      = ncid_B2C                                       , &
         dims_name = (/ 'nb_lon  ','nb_lat  ','nb_depth','nb_time '/), &
         dims      = (/nb_i,nb_j,nb_k,nb_l/)                        , &
         dims_id   = dims_out(:)                                      &
         )

    dim_lon_id   = dims_out(1)
    dim_lat_id   = dims_out(2)
    dim_depth_id = dims_out(3)
    dim_time_id  = dims_out(4)

    CALL sub_memory(-size(dims_out),'i','dims_out','sub_B2C_grid_save_U')
    DEALLOCATE(dims_out)

    WRITE(lun_standard,*)'  - Dimensions are:'
    WRITE(lun_standard,*)'    - nb_lon   =', nb_i
    WRITE(lun_standard,*)'    - nb_lat   =', nb_j
    WRITE(lun_standard,*)'    - nb_depth =', nb_k
    WRITE(lun_standard,*)'    - nb_time  =', nb_l

    !------------------!
    !- INTEGER TYPE ? -!
    !------------------!
    CALL sub_which_type(itype, c_itype)

    !---------------!
    !- REAL TYPE ? -!
    !---------------!
    CALL sub_which_type(real_short_type, c_rstype)
    CALL sub_which_type(real_long_type, c_rltype)

    !----------!
    !- uCgrid -!
    !----------!
    CALL sub_netcdf_var_and_att_def(                                             &
         ncid              = ncid_B2C                                          , &
         var_name          = 'uCgrid'                                          , &
         var_type          = c_rltype                                          , &
         var_dimids        = (/dim_lon_id,dim_lat_id,dim_depth_id,dim_time_id/), &
         var_id            = uc_id                                             , &
         att_title         = 'Zonal current on C-grid interpolated from B-grid', &
         att_longname      = 'Zonal current on C-grid'                         , &
         att_units         = 'm/s'                                             , &
         att_missing_value = mask_value                                          &
         )
    WRITE(lun_standard,*)'  - U on C-grid are defined', uc_id

    !---------------------------------------------!
    !- Close define mode (to enter in data mode) -!
    !---------------------------------------------!
    CALL sub_netcdf_end_def (ncid_B2C)

    !----------!
    !- uCgrid -!
    !----------!
    DO loop_time=1,lmt

       tmp_array(:,:,:,1)=uu(:,:,:,loop_time)

       !! WHERE (umask(:,:,:,1)==0) &
       !!     tmp_array(:,:,:,1)=mask_value

       CALL sub_netcdf_generic_write_var( &
            ncid   = ncid_B2C           , &
            varid  = uc_id              , &
            values = tmp_array(:,:,:,1:1), &
            start  = (/1,1,1,loop_time/))
    ENDDO
    WRITE(lun_standard,*)'  - U on C-grid is done'

    !---------------------!
    !- Close NetCDF File -!
    !---------------------!
    CALL sub_netcdf_close(ncid_B2C)
    WRITE(lun_standard,*)'--- ',TRIM(c_name),' is closed ---'
    WRITE(lun_standard,*)'========================================================='
    WRITE(lun_standard,*)''

    CALL sub_memory(-size(tmp_array),'r','tmp_array','sub_B2C_grid_save_U')
    DEALLOCATE(tmp_array)

  END SUBROUTINE sub_B2C_grid_save_U
  !!***  
  !=========================================================================
  !!****f* mod_save_netcdf/sub_B2C_grid_save_V()
  !! NAME
  !!   sub_B2C_grid_save_V()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DATE
  !!   * November 2008
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_B2C_grid_save_V(vv, vmask)

    REAL(kind = rprec), DIMENSION(:,:,:,:), INTENT(in) :: vv
    INTEGER(kind = iprec), DIMENSION(:,:,:,:), INTENT(in) :: vmask


    REAL(kind = rprec), DIMENSION(:,:,:,:), ALLOCATABLE :: tmp_array

    INTEGER(kind = iprec) :: ncid_B2C
    INTEGER(kind = iprec) :: vc_id

    INTEGER(kind = iprec) :: dim_lon_id
    INTEGER(kind = iprec) :: dim_lat_id
    INTEGER(kind = iprec) :: dim_depth_id
    INTEGER(kind = iprec) :: dim_time_id

    INTEGER(kind = iprec), DIMENSION(:), ALLOCATABLE :: dims_out

    CHARACTER(len = 64)   :: c_name
    CHARACTER(len = 2)    :: c_itype
    CHARACTER(len = 2)    :: c_rstype, c_rltype
    INTEGER(kind = iprec) :: itype = 0
    REAL(kind = rshort)   :: real_short_type = 0._rshort
    REAL(kind = rlong)    :: real_long_type  = 0._rlong

    INTEGER(kind = iprec) :: loop_time
    INTEGER(kind = iprec) :: nb_i,nb_j,nb_k,nb_l

    WRITE(lun_standard,*)''
    WRITE(lun_standard,*)'=== Create and Define NetCDF ouput B2C-grid file Vgrid ==='

    nb_i=SIZE(vv,dim=1)
    nb_j=SIZE(vv,dim=2)
    nb_k=SIZE(vv,dim=3)
    nb_l=SIZE(vv,dim=4)

    ALLOCATE(tmp_array(nb_i,nb_j,nb_k,1))
    CALL sub_memory(size(tmp_array),'r','tmp_array','sub_B2C_grid_save_V')

    !----------------------!
    !- Create NetCDF File -!
    !----------------------!
    c_name = 'B2C_grid_data_Vgrid.nc'

    CALL sub_netcdf_create( &
         TRIM(c_name)     , &
         ncid_B2C           &
         )
    WRITE(lun_standard,*)'--- Successful creation of ',TRIM(c_name),' ---', ncid_B2C

    !---------------------------------------!
    !- Write dimensions in the NetCDF file -!
    !---------------------------------------!

    ALLOCATE(dims_out(4))
    CALL sub_memory(size(dims_out),'i','dims_out','sub_B2C_grid_save_V')

    CALL sub_netcdf_dimensions(                                       &
         ncid      = ncid_B2C                                       , &
         dims_name = (/ 'nb_lon  ','nb_lat  ','nb_depth','nb_time '/), &
         dims      = (/nb_i,nb_j,nb_k,nb_l/)                        , &
         dims_id   = dims_out(:)                                      &
         )

    dim_lon_id   = dims_out(1)
    dim_lat_id   = dims_out(2)
    dim_depth_id = dims_out(3)
    dim_time_id  = dims_out(4)

    CALL sub_memory(-size(dims_out),'i','dims_out','sub_B2C_grid_save_V')
    DEALLOCATE(dims_out)

    WRITE(lun_standard,*)'  - Dimensions are:'
    WRITE(lun_standard,*)'    - nb_lon   =', nb_i
    WRITE(lun_standard,*)'    - nb_lat   =', nb_j
    WRITE(lun_standard,*)'    - nb_depth =', nb_k
    WRITE(lun_standard,*)'    - nb_time  =', nb_l

    !------------------!
    !- INTEGER TYPE ? -!
    !------------------!
    CALL sub_which_type(itype, c_itype)

    !---------------!
    !- REAL TYPE ? -!
    !---------------!
    CALL sub_which_type(real_short_type, c_rstype)
    CALL sub_which_type(real_long_type, c_rltype)

    !----------!
    !- uCgrid -!
    !----------!
    CALL sub_netcdf_var_and_att_def(                                             &
         ncid              = ncid_B2C                                          , &
         var_name          = 'vCgrid'                                          , &
         var_type          = c_rltype                                          , &
         var_dimids        = (/dim_lon_id,dim_lat_id,dim_depth_id,dim_time_id/), &
         var_id            = vc_id                                             , &
         att_title         = 'Meridional current on C-grid interpolated from B-grid', &
         att_longname      = 'Meridional current on C-grid'                         , &
         att_units         = 'm/s'                                             , &
         att_missing_value = mask_value                                          &
         )
    WRITE(lun_standard,*)'  - V on C-grid are defined', vc_id

    !------------------------------------------------!
    !- Save namelist parameters in this netcdf file -!
    !------------------------------------------------!
    !!    CALL sub_save_namelist_in_netcdf(ncid_B2C)

    !---------------------------------------------!
    !- Close define mode (to enter in data mode) -!
    !---------------------------------------------!
    CALL sub_netcdf_end_def (ncid_B2C)

    !----------!
    !- vCgrid -!
    !----------!
    DO loop_time=1,lmt

       tmp_array(:,:,:,1)=vv(:,:,:,loop_time)

       !!WHERE (vmask(:,:,:,1)==0) &
       !!   tmp_array(:,:,:,1)=mask_value

       CALL sub_netcdf_generic_write_var(           &
            ncid   = ncid_B2C                     , &
            varid  = vc_id                        , &
            values = tmp_array(:,:,:,1:1), &
            start  = (/1,1,1,loop_time/))
    ENDDO
    WRITE(lun_standard,*)'  - V on C-grid is done'

    !---------------------!
    !- Close NetCDF File -!
    !---------------------!
    CALL sub_netcdf_close(ncid_B2C)
    WRITE(lun_standard,*)'--- ',TRIM(c_name),' is closed ---'
    WRITE(lun_standard,*)'========================================================='
    WRITE(lun_standard,*)''

    CALL sub_memory(-size(tmp_array),'r','tmp_array','sub_B2C_grid_save_V')
    DEALLOCATE(tmp_array)

  END SUBROUTINE sub_B2C_grid_save_V
  !!***  
  !=========================================================================
  !!****f* mod_save_netcdf/sub_B2C_grid_save_W()
  !! NAME
  !!   sub_B2C_grid_save_W()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima
  !! 
  !! CREATION DATE
  !!   * November 2008
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_B2C_grid_save_W(ww)

    REAL(kind = rprec), DIMENSION(:,:,:,:), INTENT(in) :: ww

    REAL(kind = rprec), DIMENSION(:,:,:,:), ALLOCATABLE :: tmp_array

    INTEGER(kind = iprec) :: ncid_B2C
    INTEGER(kind = iprec) :: wc_id

    INTEGER(kind = iprec) :: dim_lon_id
    INTEGER(kind = iprec) :: dim_lat_id
    INTEGER(kind = iprec) :: dim_depth_id
    INTEGER(kind = iprec) :: dim_time_id

    INTEGER(kind = iprec), DIMENSION(:), ALLOCATABLE :: dims_out

    CHARACTER(len = 64)   :: c_name
    CHARACTER(len = 2)    :: c_itype
    CHARACTER(len = 2)    :: c_rstype, c_rltype
    INTEGER(kind = iprec) :: itype = 0
    REAL(kind = rshort)   :: real_short_type = 0._rshort
    REAL(kind = rlong)    :: real_long_type  = 0._rlong

    INTEGER(kind = iprec) :: loop_time, loop_k
    INTEGER(kind = iprec) :: nb_i,nb_j,nb_k,nb_l

    WRITE(lun_standard,*)''
    WRITE(lun_standard,*)'=== Create and Define NetCDF ouput B2C-grid file Wgrid ==='

    nb_i=SIZE(ww,dim=1)
    nb_j=SIZE(ww,dim=2)
    nb_k=SIZE(ww,dim=3)
    nb_l=SIZE(ww,dim=4)


    ALLOCATE(tmp_array(nb_i,nb_j,nb_k,1))
    CALL sub_memory(size(tmp_array),'r','tmp_array','sub_B2C_grid_save_W')

    !----------------------!
    !- Create NetCDF File -!
    !----------------------!
    c_name = 'B2C_grid_data_Wgrid.nc'

    CALL sub_netcdf_create( &
         TRIM(c_name)     , &
         ncid_B2C           &
         )
    WRITE(lun_standard,*)'--- Successful creation of ',TRIM(c_name),' ---', ncid_B2C

    !---------------------------------------!
    !- Write dimensions in the NetCDF file -!
    !---------------------------------------!
    ALLOCATE(dims_out(4))
    CALL sub_memory(size(dims_out),'i','dims_out','sub_B2C_grid_save_W')

    CALL sub_netcdf_dimensions(                                       &
         ncid      = ncid_B2C                                       , &
         dims_name = (/ 'nb_lon  ','nb_lat  ','nb_depth','nb_time '/), &
         dims      = (/nb_i,nb_j,nb_k,nb_l/)                            , &
         dims_id   = dims_out(:)                                      &
         )

    dim_lon_id   = dims_out(1)
    dim_lat_id   = dims_out(2)
    dim_depth_id = dims_out(3)
    dim_time_id  = dims_out(4)

    CALL sub_memory(-size(dims_out),'i','dims_out','sub_B2C_grid_save_W')
    DEALLOCATE(dims_out)

    WRITE(lun_standard,*)'  - Dimensions are:'
    WRITE(lun_standard,*)'    - nb_lon   =', nb_i
    WRITE(lun_standard,*)'    - nb_lat   =', nb_j
    WRITE(lun_standard,*)'    - nb_depth =', nb_k
    WRITE(lun_standard,*)'    - nb_time  =', nb_l

    !------------------!
    !- INTEGER TYPE ? -!
    !------------------!
    CALL sub_which_type(itype, c_itype)

    !---------------!
    !- REAL TYPE ? -!
    !---------------!
    CALL sub_which_type(real_short_type, c_rstype)
    CALL sub_which_type(real_long_type, c_rltype)

    !----------!
    !- uCgrid -!
    !----------!
    CALL sub_netcdf_var_and_att_def(                                             &
         ncid              = ncid_B2C                                          , &
         var_name          = 'wCgrid'                                          , &
         var_type          = c_rltype                                          , &
         var_dimids        = (/dim_lon_id,dim_lat_id,dim_depth_id,dim_time_id/), &
         var_id            = wc_id                                             , &
         att_title         = 'Vertical current on C-grid'                      , &
         att_longname      = 'Vertical current on C-grid'                      , &
         att_units         = 'm/s'                                             , &
         att_missing_value = mask_value                                          &
         )
    WRITE(lun_standard,*)'  - W on C-grid are defined', wc_id

    !------------------------------------------------!
    !- Save namelist parameters in this netcdf file -!
    !------------------------------------------------!
    !!    CALL sub_save_namelist_in_netcdf(ncid_B2C)

    !---------------------------------------------!
    !- Close define mode (to enter in data mode) -!
    !---------------------------------------------!
    CALL sub_netcdf_end_def (ncid_B2C)

    !----------!
    !- vCgrid -!
    !----------!
    DO loop_time=1,lmt

       DO loop_k = 1, nb_k
          tmp_array(:,:,loop_k,1)=ww(:,:,loop_k,loop_time) / &
               (e1t(:,:,1,1) * e2t(:,:,1,1))
       ENDDO

       WHERE (tmask(:,:,:,1)==0) &
            tmp_array(:,:,:,1)=mask_value

       CALL sub_netcdf_generic_write_var(           &
            ncid   = ncid_B2C                     , &
            varid  = wc_id                        , &
            values = tmp_array(:,:,:,1:1), &
            start  = (/1,1,1,loop_time/))
    ENDDO
    WRITE(lun_standard,*)'  - W on C-grid is done'

    !---------------------!
    !- Close NetCDF File -!
    !---------------------!
    CALL sub_netcdf_close(ncid_B2C)
    WRITE(lun_standard,*)'--- ',TRIM(c_name),' is closed ---'
    WRITE(lun_standard,*)'========================================================='
    WRITE(lun_standard,*)''

    CALL sub_memory(-size(tmp_array),'r','tmp_array','sub_B2C_grid_save_W')
    DEALLOCATE(tmp_array)

  END SUBROUTINE sub_B2C_grid_save_W
  !*****
  !=========================================================================
  !!****f* mod_save_netcdf/sub_B2C_grid_save_U_seq()
  !! NAME
  !!   sub_B2C_grid_save_U_seq()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas.Grima@univ-brest.fr
  !! 
  !! CREATION DATE
  !!   * May 2009
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_B2C_grid_save_U_seq(&
       uu                          , &
       new_file                    , &
       ind_file                    , &
       ndigits                     , &
       dimt                        , &
       close_file                    &
       )

    REAL(kind = rprec), DIMENSION(:,:,:,:), INTENT(in) :: uu
    LOGICAL                               , INTENT(in) :: new_file
    INTEGER(kind = iprec)                 , INTENT(in) :: ind_file
    INTEGER(kind = iprec)                 , INTENT(in) :: ndigits
    INTEGER(kind = iprec)                 , INTENT(in) :: dimt
    LOGICAL                               , INTENT(in) :: close_file


    CHARACTER(len=256) :: c_filename

    INTEGER(kind = iprec), DIMENSION(:), ALLOCATABLE :: dims_out
    INTEGER(kind = iprec) :: nb_i, nb_j, nb_k
    INTEGER(kind = iprec) :: dim_lon_id
    INTEGER(kind = iprec) :: dim_lat_id
    INTEGER(kind = iprec) :: dim_depth_id
    INTEGER(kind = iprec) :: dim_time_id

    INTEGER(kind = iprec) :: itype = 0
    REAL(kind = rshort)   :: real_short_type = 0._rshort
    REAL(kind = rlong)    :: real_long_type  = 0._rlong

    CHARACTER(len = 2)    :: c_itype
    CHARACTER(len = 2)    :: c_rstype, c_rltype

    IF (new_file) THEN

       !- Build file name -!
       IF (ind_file < 0) THEN
          CALL sub_build_filename(ind_file, ndigits, 'B2C_grid_data_', 'Ugrid.nc', c_filename)
       ELSE
          CALL sub_build_filename(ind_file, ndigits, 'B2C_grid_data_', '_Ugrid.nc', c_filename)
       ENDIF

       !- Create Netcdf file -!
       !! c_name=TRIM(cdir)//TRIM(c_filename) !! need to add a new parameter in namelist item B2C

       CALL sub_netcdf_create(TRIM(c_filename), ncid_B2C_U)
       WRITE(lun_standard,*)' '
       WRITE(lun_standard,*)'  --- Successful creation of ',TRIM(c_filename),' ---', ncid_B2C_U

       !---------------------------------------!
       !- Write dimensions in the NetCDF file -!
       !---------------------------------------!

       ALLOCATE(dims_out(4))
       CALL sub_memory(size(dims_out),'i','dims_out','sub_B2C_grid_save_U_seq')

       nb_i=SIZE(uu,dim=1)
       nb_j=SIZE(uu,dim=2)
       nb_k=SIZE(uu,dim=3)

       CALL sub_netcdf_dimensions(                                       &
            ncid      = ncid_B2C_U                                       , &
            dims_name = (/ 'nb_lon  ','nb_lat  ','nb_depth','nb_time '/), &
            dims      = (/nb_i,nb_j,nb_k,NF90_UNLIMITED/)              , &
            dims_id   = dims_out(:)                                      &
            )

       dim_lon_id   = dims_out(1)
       dim_lat_id   = dims_out(2)
       dim_depth_id = dims_out(3)
       dim_time_id  = dims_out(4)


       CALL sub_memory(-size(dims_out),'i','dims_out','sub_B2C_grid_save_U_seq')
       DEALLOCATE(dims_out)

       !------------------!
       !- INTEGER TYPE ? -!
       !------------------!
       CALL sub_which_type(itype, c_itype)

       !---------------!
       !- REAL TYPE ? -!
       !---------------!
       CALL sub_which_type(real_short_type, c_rstype)
       CALL sub_which_type(real_long_type, c_rltype)

       !----------!
       !- uCgrid -!
       !----------!
       CALL sub_netcdf_var_and_att_def(                                             &
            ncid              = ncid_B2C_U                                        , &
            var_name          = 'uCgrid'                                          , &
            var_type          = c_rltype                                          , &
            var_dimids        = (/dim_lon_id,dim_lat_id,dim_depth_id,dim_time_id/), &
            var_id            = uc_id                                             , &
            att_title         = 'Zonal current on C-grid interpolated from B-grid', &
            att_longname      = 'Zonal current on C-grid'                         , &
            att_units         = 'm/s'                                             , &
            att_missing_value = mask_value                                          &
            )
       WRITE(lun_standard,*)'    - U on C-grid are defined', uc_id

       !---------------------------------------------!
       !- Close define mode (to enter in data mode) -!
       !---------------------------------------------!
       CALL sub_netcdf_end_def(ncid_B2C_U)

       IF (TRIM(forback) == 'forward' ) THEN
          ind_time_U=1
       ELSE
          ind_time_U=dimt
       ENDIF

    ENDIF

    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_B2C_U         , &
         varid  = uc_id              , &
         values = uu(:,:,:,:)        , &
         start  = (/1,1,1,ind_time_U/))

    IF (TRIM(forback) == 'forward' ) THEN
       ind_time_U=ind_time_U+1
    ELSE
       ind_time_U=ind_time_U-1
    ENDIF

    IF (close_file) THEN
       CALL sub_close_netcdf_file(ncid_B2C_U)
    ENDIF

  END SUBROUTINE sub_B2C_grid_save_U_seq
  !!***  
  !=========================================================================
  !!****f* mod_save_netcdf/sub_B2C_grid_save_V_seq()
  !! NAME
  !!   sub_B2C_grid_save_V_seq()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas.Grima@univ-brest.fr
  !! 
  !! CREATION DATE
  !!   * May 2009
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_B2C_grid_save_V_seq(&
       vv                          , &
       new_file                    , &
       ind_file                    , &
       ndigits                     , &
       dimt                        , &
       close_file                    &
       )

    REAL(kind = rprec), DIMENSION(:,:,:,:), INTENT(in) :: vv
    LOGICAL                               , INTENT(in) :: new_file
    INTEGER(kind = iprec)                 , INTENT(in) :: ind_file
    INTEGER(kind = iprec)                 , INTENT(in) :: ndigits
    INTEGER(kind = iprec)                 , INTENT(in) :: dimt
    LOGICAL                               , INTENT(in) :: close_file


    CHARACTER(len=256) :: c_filename

    INTEGER(kind = iprec), DIMENSION(:), ALLOCATABLE :: dims_out
    INTEGER(kind = iprec) :: nb_i, nb_j, nb_k
    INTEGER(kind = iprec) :: dim_lon_id
    INTEGER(kind = iprec) :: dim_lat_id
    INTEGER(kind = iprec) :: dim_depth_id
    INTEGER(kind = iprec) :: dim_time_id

    INTEGER(kind = iprec) :: itype = 0
    REAL(kind = rshort)   :: real_short_type = 0._rshort
    REAL(kind = rlong)    :: real_long_type  = 0._rlong

    CHARACTER(len = 2)    :: c_itype
    CHARACTER(len = 2)    :: c_rstype, c_rltype

    IF (new_file) THEN

       !- Build file name -!
       IF (ind_file < 0) THEN
          CALL sub_build_filename(ind_file, ndigits, 'B2C_grid_data_', 'Vgrid.nc', c_filename)
       ELSE
          CALL sub_build_filename(ind_file, ndigits, 'B2C_grid_data_', '_Vgrid.nc', c_filename)
       ENDIF

       !- Create Netcdf file -!
       !! c_name=TRIM(cdir)//TRIM(c_filename) !! need to add a new parameter in namelist item B2C

       CALL sub_netcdf_create(TRIM(c_filename), ncid_B2C_V)
       WRITE(lun_standard,*)' '
       WRITE(lun_standard,*)'  --- Successful creation of ',TRIM(c_filename),' ---', ncid_B2C_V

       !---------------------------------------!
       !- Write dimensions in the NetCDF file -!
       !---------------------------------------!

       ALLOCATE(dims_out(4))
       CALL sub_memory(size(dims_out),'i','dims_out','sub_B2C_grid_save_V_seq')

       nb_i=SIZE(vv,dim=1)
       nb_j=SIZE(vv,dim=2)
       nb_k=SIZE(vv,dim=3)

       CALL sub_netcdf_dimensions(                                       &
            ncid      = ncid_B2C_V                                       , &
            dims_name = (/ 'nb_lon  ','nb_lat  ','nb_depth','nb_time '/), &
            dims      = (/nb_i,nb_j,nb_k,NF90_UNLIMITED/)              , &
            dims_id   = dims_out(:)                                      &
            )

       dim_lon_id   = dims_out(1)
       dim_lat_id   = dims_out(2)
       dim_depth_id = dims_out(3)
       dim_time_id  = dims_out(4)

       CALL sub_memory(-size(dims_out),'i','dims_out','sub_B2C_grid_save_V_seq')
       DEALLOCATE(dims_out)

       !------------------!
       !- INTEGER TYPE ? -!
       !------------------!
       CALL sub_which_type(itype, c_itype)

       !---------------!
       !- REAL TYPE ? -!
       !---------------!
       CALL sub_which_type(real_short_type, c_rstype)
       CALL sub_which_type(real_long_type, c_rltype)

       !----------!
       !- vCgrid -!
       !----------!
       CALL sub_netcdf_var_and_att_def(                                             &
            ncid              = ncid_B2C_V                                        , &
            var_name          = 'uCgrid'                                          , &
            var_type          = c_rltype                                          , &
            var_dimids        = (/dim_lon_id,dim_lat_id,dim_depth_id,dim_time_id/), &
            var_id            = vc_id                                             , &
            att_title         = 'Meridional current on C-grid interpolated from B-grid', &
            att_longname      = 'Meridional current on C-grid'                         , &
            att_units         = 'm/s'                                             , &
            att_missing_value = mask_value                                          &
            )
       WRITE(lun_standard,*)'    - V on C-grid are defined', vc_id

       !---------------------------------------------!
       !- Close define mode (to enter in data mode) -!
       !---------------------------------------------!
       CALL sub_netcdf_end_def(ncid_B2C_V)

       IF (TRIM(forback) == 'forward' ) THEN
          ind_time_V=1
       ELSE
          ind_time_V=dimt
       ENDIF

    ENDIF

    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_B2C_V         , &
         varid  = vc_id              , &
         values = vv(:,:,:,:)        , &
         start  = (/1,1,1,ind_time_V/))

    IF (TRIM(forback) == 'forward' ) THEN
       ind_time_V=ind_time_V+1
    ELSE
       ind_time_V=ind_time_V-1
    ENDIF

    IF (close_file) THEN
       CALL sub_close_netcdf_file(ncid_B2C_V)
    ENDIF

  END SUBROUTINE sub_B2C_grid_save_V_seq
  !!***  
  !=========================================================================
  !!****f* mod_save_netcdf/sub_B2C_grid_save_W_seq()
  !! NAME
  !!   sub_B2C_grid_save_W_seq()
  !!
  !! FUNCTION
  !!   .
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas.Grima@univ-brest.fr
  !! 
  !! CREATION DATE
  !!   * May 2009
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_B2C_grid_save_W_seq(&
       ww                          , &
       new_file                    , &
       ind_file                    , &
       ndigits                     , &
       dimt                        , &
       close_file                    &
       )

    REAL(kind = rprec), DIMENSION(:,:,:,:), INTENT(in) :: ww
    LOGICAL                               , INTENT(in) :: new_file
    INTEGER(kind = iprec)                 , INTENT(in) :: ind_file
    INTEGER(kind = iprec)                 , INTENT(in) :: ndigits
    INTEGER(kind = iprec)                 , INTENT(in) :: dimt
    LOGICAL                               , INTENT(in) :: close_file


    CHARACTER(len=256) :: c_filename

    INTEGER(kind = iprec), DIMENSION(:), ALLOCATABLE :: dims_out
    INTEGER(kind = iprec) :: nb_i, nb_j, nb_k
    INTEGER(kind = iprec) :: dim_lon_id
    INTEGER(kind = iprec) :: dim_lat_id
    INTEGER(kind = iprec) :: dim_depth_id
    INTEGER(kind = iprec) :: dim_time_id

    INTEGER(kind = iprec) :: itype = 0
    REAL(kind = rshort)   :: real_short_type = 0._rshort
    REAL(kind = rlong)    :: real_long_type  = 0._rlong

    CHARACTER(len = 2)    :: c_itype
    CHARACTER(len = 2)    :: c_rstype, c_rltype

    IF (new_file) THEN

       !- Build file name -!
       IF (ind_file < 0) THEN
          CALL sub_build_filename(ind_file, ndigits, 'B2C_grid_data_', 'Wgrid.nc', c_filename)
       ELSE
          CALL sub_build_filename(ind_file, ndigits, 'B2C_grid_data_', '_Wgrid.nc', c_filename)
       ENDIF

       !- Create Netcdf file -!
       !! c_name=TRIM(cdir)//TRIM(c_filename) !! need to add a new parameter in namelist item B2C

       CALL sub_netcdf_create(TRIM(c_filename), ncid_B2C_W)
       WRITE(lun_standard,*)' '
       WRITE(lun_standard,*)'  --- Successful creation of ',TRIM(c_filename),' ---', ncid_B2C_W

       !---------------------------------------!
       !- Write dimensions in the NetCDF file -!
       !---------------------------------------!

       ALLOCATE(dims_out(4))
       CALL sub_memory(size(dims_out),'i','dims_out','sub_B2C_grid_save_W_seq')

       nb_i=SIZE(ww,dim=1)
       nb_j=SIZE(ww,dim=2)
       nb_k=SIZE(ww,dim=3)

       CALL sub_netcdf_dimensions(                                       &
            ncid      = ncid_B2C_W                                       , &
            dims_name = (/ 'nb_lon  ','nb_lat  ','nb_depth','nb_time '/), &
            dims      = (/nb_i,nb_j,nb_k,NF90_UNLIMITED/)              , &
            dims_id   = dims_out(:)                                      &
            )

       dim_lon_id   = dims_out(1)
       dim_lat_id   = dims_out(2)
       dim_depth_id = dims_out(3)
       dim_time_id  = dims_out(4)

       CALL sub_memory(-size(dims_out),'i','dims_out','sub_B2C_grid_save_W_seq')
       DEALLOCATE(dims_out)

       !------------------!
       !- INTEGER TYPE ? -!
       !------------------!
       CALL sub_which_type(itype, c_itype)

       !---------------!
       !- REAL TYPE ? -!
       !---------------!
       CALL sub_which_type(real_short_type, c_rstype)
       CALL sub_which_type(real_long_type, c_rltype)

       !----------!
       !- wCgrid -!
       !----------!
       CALL sub_netcdf_var_and_att_def(                                             &
            ncid              = ncid_B2C_W                                        , &
            var_name          = 'uCgrid'                                          , &
            var_type          = c_rltype                                          , &
            var_dimids        = (/dim_lon_id,dim_lat_id,dim_depth_id,dim_time_id/), &
            var_id            = vc_id                                             , &
            att_title         = 'Vertical current on C-grid interpolated from B-grid', &
            att_longname      = 'Vertical current on C-grid'                         , &
            att_units         = 'm/s'                                             , &
            att_missing_value = mask_value                                          &
            )
       WRITE(lun_standard,*)'    - W on C-grid are defined', wc_id

       !---------------------------------------------!
       !- Close define mode (to enter in data mode) -!
       !---------------------------------------------!
       CALL sub_netcdf_end_def(ncid_B2C_W)

       IF (TRIM(forback) == 'forward' ) THEN
          ind_time_W=1
       ELSE
          ind_time_W=dimt
       ENDIF

    ENDIF

    CALL sub_netcdf_generic_write_var( &
         ncid   = ncid_B2C_W         , &
         varid  = wc_id              , &
         values = ww(:,:,:,:)        , &
         start  = (/1,1,1,ind_time_W/))

    IF (TRIM(forback) == 'forward' ) THEN
       ind_time_W=ind_time_W+1
    ELSE
       ind_time_W=ind_time_W-1
    ENDIF

    IF (close_file) THEN
       CALL sub_close_netcdf_file(ncid_B2C_W)
    ENDIF

  END SUBROUTINE sub_B2C_grid_save_W_seq
  !!***  
END MODULE mod_B2C_grid_save
