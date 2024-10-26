!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! - Ariane - (March - 2010)
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
MODULE mod_input_data_main

  !------------------!
  ! USE ASSOCIAITION !
  !------------------!
  USE mod_precision
  USE mod_lun
  USE mod_namelist
  USE mod_netcdf
  USE mod_memory

  !-------------!
  ! DECLARATION !
  !-------------!
  IMPLICIT NONE

  REAL(kind=rprec), DIMENSION(:,:,:,:), ALLOCATABLE :: &
       tmp_array

  LOGICAL :: id_comments = .FALSE.

CONTAINS
!=========================================================================
!!****f* mod_input_data/sub_input_data_main()
!! NAME
!!   sub_input_data_main()
!!
!! FUNCTION
!!   These subroutine reads 3d data time series in different netcdf 
!!   files.
  !!   We only assume that netcdf file_name are build on this form:
  !!                  file_name = prefix_XXXX_suffix
  !!   Where XXXX, is an integer from "ind0" to "indn" on "ndigits" (here 4).
  !!   
  !!   The number of time step in each netcdf file could be different.
  !!
  !!   If time series is not completed, program stop.
  !!
  !!   WARNING: Today, we assume that the first data time step correspond
  !!            to the first record in the netcdf file. (start_time = 1)
  !!   
  !!   * Begin loop on netcdf files for a variable (current or tracer):
  !!       - build file name,
  !!       - open netcdf file,
  !!       - read dimensions,
  !!       - verify if time dimension is reached,
  !!       - read data,
  !!       - If needed, read EIV current components,
  !!       - if needed, masked values are set to zero,
  !!       - close netcdf file
  !!   * END loop.
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima (April-May 2005)
  !! 
  !! CREATION DATE
  !!   * April-May 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   Except "var", all these arguments are red from a namelist file.
  !!   * cdir   : netcdf file data directory
  !!   * prefix : prefix file name
  !!   * ind0   : first indice
  !!   * indn   : last indice
  !!   * ndigits: number of digits of indices
  !!   * suffix : suffix file name
  !!   * ncvar  : netcdf variable name
  !!   * nceiv  : netcdf EIV variable name for currents (or NONE)
  !!   * ncmask : netcdf mask name                      (or NONE)
  !!
  !!   * var    : 4d array where netcdf values will be affected
  !!
  !! TODO
  !!   * add the possibilty to start time step not at the first netcdf
  !!     file record.
  !!
  !! USES
  !!   * sub_build_filename      (mod_netcdf.f90)
  !!   * sub_open_netcdf_file    (mod_netcdf.f90)
  !!   * sub_select_var_dims     (mod_netcdf.f90)
  !!   * sub_read_netcdf_var4d   (mod_netcdf.f90)
  !!   * sub_read_netcdf_att_val (mod_netcdf.f90)
  !!   * sub_close_netcdf_file   (mod_netcdf.f90)
  !!
  !! USED BY
  !!   * sub_input_data (mod_input_data.f90)
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_input_data_main(cdir,prefix,ind0,indn,ndigits,suffix, &
       ncvar,nceiv,ncmask,var,mask, key_ijt)

    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    CHARACTER(len = *)    , INTENT(in) :: cdir, prefix, suffix,  ncvar, nceiv, ncmask
    INTEGER(kind  = iprec), INTENT(in) :: ind0, indn, ndigits
    REAL(kind     = rprec), DIMENSION(:,:,:,:), INTENT(out) :: var
    INTEGER(kind  = iprec), DIMENSION(:,:,:,:), INTENT(out), OPTIONAL :: mask
    LOGICAL, INTENT(in), OPTIONAL :: key_ijt


    !- local variables -!
    CHARACTER(len = 4), PARAMETER :: c_none='NONE' ! 
    INTEGER(kind = iprec) :: ii           ! Used for do loop
    INTEGER(kind = iprec) :: start_time ! First time step, dim. 4 of var
    INTEGER(kind = iprec) :: end_time   ! Last time step , dim. 4 of var
    INTEGER(kind = iprec) :: ncid         ! netcdf file ID
    INTEGER(kind = iprec) :: varid        ! netcdf variable ID
    INTEGER(kind = iprec) :: indmin       ! minimum indice
    INTEGER(kind = iprec) :: indmax       ! maximum indice
    INTEGER(kind = iprec) :: dimx         ! dimension in x (i)
    INTEGER(kind = iprec) :: dimy         ! dimension in y (j)
    INTEGER(kind = iprec) :: dimz         ! dimension in z (k)
    INTEGER(kind = iprec) :: dimt         ! dimension in t (l)
    INTEGER(kind = iprec) :: tmp_var_id   ! temporaly variable ID
    REAL(kind    = rprec) :: att_mask_val ! attribute mask value
    CHARACTER(len = 128)  :: c_filename   ! file name
    REAL(kind = rprec) :: sfval
    REAL(kind = rprec) :: aoval

    !!NG: 10/07/2009 ! tmp_array: temporaly array
    !!NG: 10/07/2009 REAL(kind    = rprec), DIMENSION(:,:,:,:), ALLOCATABLE :: tmp_array

    ! l_esc: a logical to exit do loop if time series is completed before 
    !        the end of the netcdf file.
    LOGICAL                :: l_esc = .FALSE. ! a logical to exit do loop

    ! dimsorder: array where netcdf variable dimensions order are stored.
    !            This, because sometime dimensions order could be different 
    !            than (x, y, z, t). It could be (x, y, t, nothing) or 
    !            (z, t, nothing, nothing).
    !            More information: see sub_select_var_dims and 
    !            sub_read_netcdf_var4d in mod_netcdf.f90 file.
    INTEGER(kind = iprec), DIMENSION(4) :: dimsorder 

    !-------------!
    ! Code begins !
    !-------------!
    !- Initialization -!
    start_time = 1 ! first time step corresponds to the first netcdf record !
    end_time   = 0 ! end_time is intialized.
    l_esc      = .FALSE. ! l_esc is intialized.
    IF (PRESENT(mask)) THEN
      mask(:,:,:,:)=1_iprec
    ENDIF

    !- If there is only one netcdf file, ind0 must be < 0 (by default -1). -!
    !- and DoLoop is executed only once.                                   -!
    !- Else Doloop is executed indn - ind0 + 1 times.                      -!
    !- We test it here.                                                    -!
    IF (ind0 < 0) THEN
      indmin = -1
      indmax = -1
    ELSE
      indmin = ind0
      indmax = indn
    ENDIF

    !---------------------------!
    !- main DoLoop starts here -!
    !---------------------------!
    DO ii = indmin, indmax

      !- Build file name -!
      CALL sub_build_filename(ii, ndigits, prefix, suffix, c_filename)

      !- Open Netcdf file -!
      CALL sub_open_netcdf_file(cdir, c_filename, ncid)

      !- Read variable dimensions -!
      IF(PRESENT(key_ijt)) THEN
        CALL sub_select_var_dims(ncid, ncvar, varid, dimsorder, &
             dimx, dimy, dimz, dimt, key_ijt)
      ELSE
        CALL sub_select_var_dims(ncid, ncvar, varid, dimsorder, &
             dimx, dimy, dimz, dimt)
      ENDIF

      !- From the precedent call we increment end_time -!
      end_time = end_time + dimt

      !- We test here, if end_time is greater than the last time dimension value -!
      IF ( end_time >= lmt ) THEN !! NG: 10/07/2009
        end_time = lmt
        l_esc = .TRUE.
      ENDIF

      !-----------------------------------------------------------------!
      !- Read netcdf 3d variable and store it in our 4d array variable -!
      !- Fourth dimension is time -!------------------------------------!
      !----------------------------!
      !!NG: bug 15/04/2009 CALL sub_read_netcdf_var4d(ncid, varid, var(:,:,1:dimz,start_time:end_time), &
      !!NG: bug 15/04/2009     dimsorder, dims_reg)
      CALL sub_read_netcdf_var4d(ncid, varid,   &
           var(:,:,:,start_time:end_time), &
           dimsorder, dims_reg)

      !- Test if mask value must be read -!
      IF ( ncmask /= c_none) THEN

        !- Read mask value in netcdf file -!
        CALL sub_read_netcdf_att_val( &
             ncid                   , &
             varid                  , &
             ncmask                 , &
             att_mask_val           , &
             0._rprec               )

        !-----------------------------------!
        !- Scale_factor is red or set to 1 -!
        !-----------------------------------!
        CALL sub_read_netcdf_att_val( &
             ncid                   , &
             varid                  , &
             'scale_factor'         , &
             sfval                  , &
             1._rprec               )

        att_mask_val = att_mask_val * sfval

        !---------------------------------!
        !- Add_offset is red or set to 0 -!
        !---------------------------------!
        CALL sub_read_netcdf_att_val( &
             ncid                   , &
             varid                  , &
             'add_offset'           , &
             aoval                  , &
             0._rprec               )

        att_mask_val = att_mask_val + aoval

        !- Mask 4d array values with 0 -!
        IF (PRESENT(mask)) THEN
          WHERE(var(:,:,:, start_time) == att_mask_val) &
               mask(:,:,:,1)                   = 0_iprec
        ENDIF

        WHERE(var(:,:,:, start_time:end_time) == att_mask_val) &
             var(:,:,:, start_time:end_time) = 0._rprec

      ENDIF

      !-------------------------------------------------!
      !- Test if we have to read EIV array values and  -!
      !- if yes,  add it to current                    -!
      !-------------------------------------------------!
      IF ( nceiv /= c_none ) THEN

        !- Read variable dimensions -!
        CALL sub_select_var_dims(ncid, nceiv, tmp_var_id, dimsorder, &
             dimx, dimy, dimz, dimt)

        !- Dynamic allocation of tmp_array -!
        !!NG: 10/07/2009 modifications
        IF (.NOT.ALLOCATED(tmp_array)) THEN
          ALLOCATE(tmp_array(1:dimx, 1:dimy, 1:dimz, 1:dimt))
          CALL sub_memory(SIZE(tmp_array),'r','tmp_array','sub_input_data_main')
        ELSEIF ( &
             (SIZE(tmp_array, dim=1) /= dimx).OR. &
             (SIZE(tmp_array, dim=2) /= dimy).OR. &
             (SIZE(tmp_array, dim=3) /= dimz).OR. &
             (SIZE(tmp_array, dim=4) /= dimt)) THEN
          CALL sub_memory(-SIZE(tmp_array),'r','tmp_array', &
               'sub_input_data_main')
          DEALLOCATE(tmp_array)
          ALLOCATE(tmp_array(1:dimx, 1:dimy, 1:dimz, 1:dimt))
          CALL sub_memory(SIZE(tmp_array),'r','tmp_array','sub_input_data_main')
        ENDIF
        !!NG: 10/07/2009 

        !- Read EIV values in tmp_array -!
        CALL sub_read_netcdf_var4d(ncid, tmp_var_id, tmp_array(:,:,:,:), &
             dimsorder, dims_reg)

        !-----------------------------------!
        !- Test if mask value must be read -!
        !-----------------------------------!
        IF ( ncmask /= c_none) THEN
          !- Read mask value in netcdf file -!

          CALL sub_read_netcdf_att_val( &
               ncid                   , &
               varid                  , &
               ncmask                 , &
               att_mask_val           , &
               0._rprec               )

          !- Mask 4d array values with 0 -!

          WHERE(tmp_array(:,:,:,:) == att_mask_val) &
               tmp_array(:,:,:,:) = 0._rprec
        ENDIF

        !- Add EIV component to current -! 
        var(:,:,:,start_time:end_time) = var(:,:,:,start_time:end_time) + &
             tmp_array(:,:,:,:)

        !- Deallocate memory of tmp_array -!
        !!NG: 10/07/2009 CALL sub_memory(-size(tmp_array),'r','tmp_array','sub_input_data_main')
        !!NG: 10/07/2009 DEALLOCATE(tmp_array)


      ENDIF

      !- Comments -!
      WRITE(lun_standard,*)' - ', TRIM(ncvar),': max ', MAXVAL(var), ' min ', MINVAL(var)

      !- Close netcdf file -!
      CALL sub_close_netcdf_file(ncid)

      !- Exit DoLoop if l_esc == .TRUE.
      IF (l_esc) EXIT

      !----------------------------------------------------------------!
      !- Start_time pointer is switched to stored correctly in the 4d -!
      !- array val the next time series                               -!
      !----------------------------------------------------------------!
      start_time = end_time + 1

    ENDDO

    !- We stop the program if the time series in not completed -!
    IF ( end_time < lmt ) STOP 'mod_input_data: mod_input_data_main: end_time < lmt!'

  END SUBROUTINE sub_input_data_main
  !!***
  !=========================================================================
  !!****f* mod_input_data_seq/sub_input_data_seq_main()
  !! NAME
  !!   sub_input_data_seq_main()
  !!
  !! FUNCTION
  !!   These subroutine reads 3d data time series in different netcdf 
  !!   files.
  !!   We only assume that netcdf file_name are build on this form:
  !!                  file_name = prefix_XXXX_suffix
  !!   Where XXXX, is an integer from "ind0" to "indn" on "ndigits" (here 4).
  !!   
  !!   The number of time step in each netcdf file could be different.
  !!
  !!   If time series is not completed, program stop.
  !!
  !!   WARNING: Today, we assume that the first data time step correspond
  !!            to the first record in the netcdf file. (start_time = 1)
  !!   
  !!   * Begin loop on netcdf files for a variable (current or tracer):
  !!       - build file name,
  !!       - open netcdf file,
  !!       - read dimensions,
  !!       - verify if time dimension is reached,
  !!       - read data,
  !!       - If needed, read EIV current components,
  !!       - if needed, masked values are set to zero,
  !!       - close netcdf file
  !!   * END loop.
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima (April-May 2005)
  !! 
  !! CREATION DATE
  !!   * April-May 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   Except "var", all these arguments are red from a namelist file.
  !!   * cdir   : netcdf file data directory
  !!   * prefix : prefix file name
  !!   * ind0   : first indice
  !!   * indn   : last indice
  !!   * ndigits: number of digits of indices
  !!   * suffix : suffix file name
  !!   * ncvar  : netcdf variable name
  !!   * nceiv  : netcdf EIV variable name for currents (or NONE)
  !!   * ncmask : netcdf mask name                      (or NONE)
  !!
  !!   * var    : 4d array where netcdf values will be affected
  !!
  !! TODO
  !!   * add the possibilty to start time step not at the first netcdf
  !!     file record.
  !!
  !! USES
  !!   * sub_build_filename      (mod_netcdf.f90)
  !!   * sub_open_netcdf_file    (mod_netcdf.f90)
  !!   * sub_select_var_dims     (mod_netcdf.f90)
  !!   * sub_read_netcdf_var4d   (mod_netcdf.f90)
  !!   * sub_read_netcdf_att_val (mod_netcdf.f90)
  !!   * sub_close_netcdf_file   (mod_netcdf.f90)
  !!
  !! USED BY
  !!   * sub_input_data_seq (mod_input_data_seq.f90)
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_input_data_seq_main(      &
       ncid, varid                       , &
       new_file, ind_file                , &
       ind_time, ind_time_size           , &
       dimsorder                         , &
       cdir, prefix, ndigits, suffix     , &
       ncvar, nceiv, ncmask, var, mask, key_ijt)

    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind = iprec) , INTENT(inout) :: ncid
    INTEGER(kind = iprec) , INTENT(inout) :: varid
    LOGICAL               , INTENT(inout) :: new_file
    INTEGER(kind = iprec) , INTENT(inout) :: ind_file
    INTEGER(kind = iprec) , INTENT(inout) :: ind_time
    INTEGER(kind = iprec) , INTENT(out)   :: ind_time_size
    INTEGER(kind  = iprec), DIMENSION(:,:,:,:), INTENT(out), OPTIONAL :: mask
    LOGICAL, INTENT(in), OPTIONAL :: key_ijt

    ! dimsorder: array where netcdf variable dimensions order are stored.
    !            This, because sometime dimensions order could be different 
    !            than (x, y, z, t). It could be (x, y, t, nothing) or 
    !            (z, t, nothing, nothing).
    !            More information: see sub_select_var_dims and 
    !            sub_read_netcdf_var4d in mod_netcdf.f90 file.
    INTEGER(kind = iprec), DIMENSION(4), INTENT(out) :: dimsorder 

    INTEGER(kind = iprec), DIMENSION(4) :: tmp_dimsorder 

    CHARACTER(len = *)    , INTENT(in)    :: cdir, prefix, suffix, ncvar, nceiv, ncmask
    INTEGER(kind  = iprec), INTENT(in)    :: ndigits
    REAL(kind     = rprec), DIMENSION(:,:,:,:), INTENT(out) :: var

    REAL(kind = rprec) :: sfval
    REAL(kind = rprec) :: aoval

    !- local variables -!
    CHARACTER(len = 4), PARAMETER :: c_none='NONE' ! 
    INTEGER(kind = iprec) :: dimx         ! dimension in x (i)
    INTEGER(kind = iprec) :: dimy         ! dimension in y (j)
    INTEGER(kind = iprec) :: dimz         ! dimension in z (k)
    INTEGER(kind = iprec) :: dimt         ! dimension in t (l)
    INTEGER(kind = iprec) :: tmp_var_id   ! temporaly variable ID
    REAL(kind    = rprec) :: att_mask_val ! attribute mask value
    CHARACTER(len = 128)  :: c_filename   ! file name

    !!NG:10/07/2009 ! tmp_array: temporaly array
    !!NG:10/07/2009 REAL(kind    = rprec), DIMENSION(:,:,:,:), ALLOCATABLE :: tmp_array

    !-------------!
    ! Code begins !
    !-------------!
    IF (PRESENT(mask)) THEN
       mask(:,:,:,:)=1_iprec
    ENDIF

    IF (new_file) THEN
       !- Build file name -!
       CALL sub_build_filename(ind_file, ndigits, prefix, suffix, c_filename)

       !- Open Netcdf file -!
       CALL sub_open_netcdf_file(cdir, c_filename, ncid)
       new_file = .FALSE.

       !- Read variable dimensions -!
       IF(PRESENT(key_ijt)) THEN
          CALL sub_select_var_dims(ncid, ncvar, varid, dimsorder, &
               dimx, dimy, dimz, dimt, key_ijt)
       ELSE
          CALL sub_select_var_dims(ncid, ncvar, varid, dimsorder, &
               dimx, dimy, dimz, dimt)
       ENDIF

       ind_time_size = dimt
       IF (TRIM(forback) == 'backward'.AND.(ind_time<1)) ind_time = dimt

    ENDIF

    !-----------------------------------------------------------------!
    !- Read netcdf 3d variable and store it in our 4d array variable -!
    !- Fourth dimension is time -!------------------------------------!
    !----------------------------!
    !! NG: 19/01/2009 write(lun_standard,*)'dimsorder ',dimsorder
    !! NG: 19/01/2009 write(lun_standard,*)'dims_reg ',dims_reg
    CALL sub_read_netcdf_var4d(ncid, varid, var(:,:,:,:), &
         dimsorder, dims_reg(:,:), ind_time)

    !- Test if mask value must be read -!
    IF ( ncmask /= c_none) THEN

       !- Read mask value in netcdf file -!
       CALL sub_read_netcdf_att_val( &
            ncid                   , &
            varid                  , &
            ncmask                 , &
            att_mask_val           , &
            0._rprec               )

       !-----------------------------------!
       !- Scale_factor is red or set to 1 -!
       !-----------------------------------!
       CALL sub_read_netcdf_att_val( &
            ncid                   , &
            varid                  , &
            'scale_factor'         , &
            sfval                  , &
            1._rprec               )

       att_mask_val = att_mask_val * sfval

       !---------------------------------!
       !- Add_offset is red or set to 0 -!
       !---------------------------------!
       CALL sub_read_netcdf_att_val( &
            ncid                   , &
            varid                  , &
            'add_offset'           , &
            aoval                  , &
            0._rprec               )

       att_mask_val = att_mask_val + aoval

       !- Mask 4d array values with 0 -!
       IF (PRESENT(mask)) THEN
          WHERE(var(:,:,:,1) == att_mask_val) &
               mask(:,:,:,1) = 0_iprec
       ENDIF

       !- Mask 4d array values with 0 -!
       WHERE(var(:,:,:,:) == att_mask_val) &
            var(:,:,:,:) = 0._rprec
    ENDIF

!!!! WARNING COMMENT THIS PART !!!!
!!$    ! Karen Guihou !
!!$    !- Mask 4d array values with 0 -!
!!$    write(*,*)' ----> Try to solve problem - Karen Guihou <----'
!!$    WHERE(isnan(var(:,:,:,:))) &
!!$         var(:,:,:,:) = 0._rprec
!!$    ! Karen Guihou !
!!!! WARNING COMMENT THIS PART !!!!

    !-------------------------------------------------!
    !- Test if we have to read EIV array values and  -!
    !- if yes,  add it to current                    -!
    !-------------------------------------------------!
    IF ( nceiv /= c_none ) THEN

       !- Read variable dimensions -!
       CALL sub_select_var_dims(ncid, nceiv, tmp_var_id, tmp_dimsorder, &
            dimx, dimy, dimz, dimt)

       !!NG: 10/07/2009 !- Dynamic allocation of tmp_array -!
       !!NG: 10/07/2009 ALLOCATE(tmp_array(1:dimx, 1:dimy, 1:dimz, 1:it_ind))
       !!NG: 10/07/2009 CALL sub_memory(size(tmp_array),'r','tmp_array','sub_input_data_seq_main')

       !- Dynamic allocation of tmp_array -!
       !!NG: 10/07/2009 modifications
       IF (.NOT.ALLOCATED(tmp_array)) THEN
          ALLOCATE(tmp_array(1:dimx, 1:dimy, 1:dimz, 1:it_ind))
          CALL sub_memory(SIZE(tmp_array),'r','tmp_array','sub_input_data_seq_main')
       ELSEIF ( &
            (SIZE(tmp_array, dim=1) /= dimx).OR. &
            (SIZE(tmp_array, dim=2) /= dimy).OR. &
            (SIZE(tmp_array, dim=3) /= dimz).OR. &
            (SIZE(tmp_array, dim=4) /= it_ind)) THEN
          CALL sub_memory(-SIZE(tmp_array),'r','tmp_array', &
               'sub_input_data_main')
          DEALLOCATE(tmp_array)
          ALLOCATE(tmp_array(1:dimx, 1:dimy, 1:dimz, 1:it_ind))
          CALL sub_memory(SIZE(tmp_array),'r','tmp_array','sub_input_data_seq_main')
       ENDIF
       !!NG: 10/07/2009 

       !- Read EIV values in tmp_array -!
       CALL sub_read_netcdf_var4d(ncid, tmp_var_id, tmp_array(:,:,:,:), &
            tmp_dimsorder, dims_reg, ind_time)

       !-----------------------------------!
       !- Test if mask value must be read -!
       !-----------------------------------!
       IF ( ncmask /= c_none) THEN
          !- Read mask value in netcdf file -!

          CALL sub_read_netcdf_att_val( &
               ncid                   , &
               tmp_var_id             , &
               ncmask                 , &
               att_mask_val           , &
               0._rprec               )

          !- Mask 4d array values with 0 -!

          WHERE(tmp_array(:,:,:,:) == att_mask_val) &
               tmp_array(:,:,:,:) = 0._rprec
       ENDIF

       !- Add EIV component to current -! 
       var(:,:,:,:) = var(:,:,:,:) + tmp_array(:,:,:,:)

       !!NG: 10/07/2009 !- Deallocate memory of tmp_array -!
       !!NG: 10/07/2009 CALL sub_memory(-size(tmp_array),'r','tmp_array','sub_input_data_seq_main')
       !!NG: 10/07/2009 DEALLOCATE(tmp_array)

    ENDIF

    !- Comments -!
    IF (id_comments) THEN
       WRITE(lun_standard,*)' - ', TRIM(ncvar),': max ', MAXVAL(var), ' min ', MINVAL(var)
    ENDIF

    IF (TRIM(forback) == 'forward' ) THEN
       ind_time = ind_time + 1

       IF (ind_time > ind_time_size) THEN
          !- Close netcdf file -!
          CALL sub_close_netcdf_file(ncid)
          new_file = .TRUE.
          ind_file = ind_file + 1
          ind_time = 1
       ENDIF

    ELSE !! Backward

       ind_time = ind_time - 1

       IF (ind_time < 1) THEN
          !- Close netcdf file -!
          CALL sub_close_netcdf_file(ncid)
          new_file = .TRUE.
          ind_file = ind_file - 1
       ENDIF

    ENDIF

  END SUBROUTINE sub_input_data_seq_main
  !!***

END MODULE mod_input_data_main
