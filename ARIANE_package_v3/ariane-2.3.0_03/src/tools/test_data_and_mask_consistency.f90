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
PROGRAM test_data_and_mask_consistency

  USE mod_precision
  USE mod_configure
  USE mod_namelist
  USE mod_netcdf
  USE mod_input_grid
  USE mod_input_data_main

  IMPLICIT NONE

  REAL(kind = rprec), DIMENSION(:,:,:,:), ALLOCATABLE :: &
       uu      , & ! Zonal Transport
       vv          ! Meridional transport

  LOGICAL, DIMENSION(2) :: new_file

  LOGICAL :: problem = .FALSE.
  !
  INTEGER (kind = iprec), DIMENSION(2) :: &
       ncids        , & ! Netcdf file IDs
       varids       , & ! Netcdf file variable IDs
       ind_file     , & ! File indices
       ind_time     , & ! Time indices
       ind_time_size    ! Time size

  INTEGER (kind = iprec), DIMENSION(4,2) :: &
       dimsorders

  INTEGER(kind = iprec) :: ii, jj, kk

  !-----------!
  ! MAIN PART !
  !-----------!
  ! Print version !
  WRITE(*,*)''
  WRITE(*,*)'================================================='
  WRITE(*,*)'= -o0) DATA & MASK CONSISTENCY  v'//TRIM(VERSION)//'  (0o- ='
  WRITE(*,*)'================================================='

  !----------------------!
  !- READ NAMELIST FILE -!
  !----------------------!
  CALL sub_read_namelist()

  !-------------------!
  !- READ INPUT MESH -!
  !-------------------!
  key_reducmem=.FALSE.
  CALL sub_reducmem_read_reg_lim()
  CALL sub_input_grid()

  !--------------------!
  ! Dynamic allocation !
  !--------------------!
  IF (key_roms) THEN
     WRITE(*,*)''
     WRITE(*,*)'This program is not yet avalaible for ROMS data.'
     WRITE(*,*)'Please contact grima@univ-brest.fr if you need it'
     WRITE(*,*)'We stop...'
     STOP
  ELSEIF (key_symphonie) THEN
     WRITE(*,*)''
     WRITE(*,*)'This program is not yet avalaible for Symphonie data.'
     WRITE(*,*)'Please contact grima@univ-brest.fr if you need it'
     WRITE(*,*)'We stop'
     STOP
  ELSE !! OPA

     ALLOCATE(uu(imt,jmt,kmt,1))
     ALLOCATE(vv(imt,jmt,kmt,1))

     ncids(:)         = 0
     varids(:)        = 0
     ind_file(1) = ind0_zo
     ind_file(2) = ind0_me
     ind_time(:)      = 1
     new_file(:)      = .TRUE.
     ind_time_size(:) = 0
     dimsorders(:,:) = 0

     !-- Read Zonal Current --!
     CALL sub_input_data_seq_main(                         &
          ncids(1), varids(1)                           , &
          new_file(1), ind_file(1)                       , &
          ind_time(1), ind_time_size(1)                 , &
          dimsorders(:,1)                               , &
          c_dir_zo, c_prefix_zo, maxsize_zo, c_suffix_zo , &
          nc_var_zo, nc_var_eivu, nc_att_mask_zo,uu(:,:,:,1:1))

     !-- Read Meridional Current --!
     CALL sub_input_data_seq_main(          &
          ncids(2), varids(2)                   , &
          new_file(2),ind_file(2)                       , &
          ind_time(2), ind_time_size(2)                , &
          dimsorders(:,2)                               , &
          c_dir_me,c_prefix_me, maxsize_me, c_suffix_me  , &
          nc_var_me,nc_var_eivv,nc_att_mask_me,vv(:,:,:,1:1))

  ENDIF

  exter: DO kk = 1, kmt
     DO jj = 2, jmt
        DO ii = 2, imt
           IF (tmask(ii,jj,kk,1).LE.0.5) THEN
              IF (                  &
                   uu(ii  ,jj  ,kk,1) + &
                   uu(ii-1,jj  ,kk,1) + &
                   vv(ii  ,jj  ,kk,1) + &
                   vv(ii  ,jj-1,kk,1) /= 0._rprec) THEN
                 WRITE(lun_standard,*) ''
                 WRITE(lun_standard,*) ' i, j, k = ', ii, jj, kk
                 WRITE(lun_standard,*) ' tmask = ', tmask(ii,jj,kk,1)
                 WRITE(lun_standard,*) ' u(i), u(i-1) =', uu(ii,jj,kk,1), uu(ii-1,jj,kk,1)
                 WRITE(lun_standard,*) ' v(j), v(j-1) =', vv(ii,jj,kk,1), vv(ii,jj-1,kk,1)
                 problem = .TRUE.
                 if (problem) exit exter
              END IF
           END IF
        END DO
     END DO
  END DO exter

  IF (problem) THEN
     WRITE(lun_standard,*) ''
     WRITE(lun_standard,*) '- WARNING - WARNING - WARNING - WARNING - WARNING - WARNING -'
     WRITE(lun_standard,*) '     !! You CAN''T use this data with this mask !!!'
     WRITE(lun_standard,*) '     There''s some inconsistency between data and mask'
     WRITE(lun_standard,*) '- WARNING - WARNING - WARNING - WARNING - WARNING - WARNING -'
  ELSE
     WRITE(lun_standard,*) ''
     WRITE(lun_standard,*) 'At time step one the consistency between &
          & data and mask is correct.'
     WRITE(lun_standard,*) 'You can run Ariane without coast crash ;-)'
  END IF

  DEALLOCATE(uu)
  DEALLOCATE(vv)

END PROGRAM test_data_and_mask_consistency
