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
PROGRAM mkseg0

  USE mod_precision
  USE mod_configure
  USE mod_namelist
  USE mod_netcdf
  USE mod_input_grid

  IMPLICIT NONE

  CHARACTER(len = 1), DIMENSION(:,:), ALLOCATABLE ::  val
  REAL(kind = rprec), DIMENSION(:,:), ALLOCATABLE ::  h
  INTEGER(kind = iprec) :: i, j
  REAL(kind = rprec) :: h0,h1,h2

  !-----------!
  ! MAIN PART !
  !-----------!
  ! Print mkseg0 version !
  WRITE(*,*)''
  WRITE(*,*)'====================================================='
  WRITE(*,*)'=              -o0)  MKSEG0 v'//TRIM(VERSION)//'  (0o-            ='
  WRITE(*,*)'====================================================='

  !----------------------!
  !- READ NAMELIST FILE -!
  !----------------------!
  CALL sub_read_namelist()

  !-------------------!
  !- READ INPUT MESH -!
  !-------------------!---------------------------------------------------!
  !-- Allocate and read coordinates and scale factors 
  !-- xx_tt, xx_uu, xx_vv, xx_ff
  !-- yy_tt, yy_uu, yy_vv, yy_ff
  !-- zz_tt, zz_ww
  !-- e2u
  !-- e1v
  !-- e1t, e2t, e3t
  !-- tmask
  !-----------------------------------------------------------------------!
  key_reducmem=.FALSE.

  CALL sub_reducmem_read_reg_lim()

  IF (key_roms) THEN
    CALL sub_input_grid()
  ELSEIF (key_mars) THEN
    CALL sub_input_grid()
  ELSEIF (key_symphonie) THEN
    CALL sub_input_grid()
  ELSE !! OPA
    CALL sub_input_tmask_surf_opa()
  ENDIF

  !--------------------!
  ! Dynamic allocation !
  !--------------------!
  ALLOCATE(val(imt,jmt))
  ALLOCATE(h(imt,jmt))

  val(:,:)='~'
  h0=50.
  h1=100.
  h2=250.

  IF (key_roms) THEN
    h(:,:) = h_roms(:,:,1,1)
  ELSEIF (key_mars) THEN
    h(:,:) = h_mars(:,:,1,1)
  ELSEIF (key_symphonie) THEN
    h(:,:) = h_symp(:,:,1,1)
  ELSE !! OPA
    h(:,:)=h1
  ENDIF

  DO j=1,jmt
    DO i=1,imt
      IF (tmask(i,j,1,1).LE.0.5) val(i,j)='#'
      IF (tmask(i,j,1,1).GT.0.5) THEN
        IF (h(i,j).GT.h2) val(i,j)='+'
        IF ((h(i,j).LE.h2).AND.(h(i,j).GT.h1)) val(i,j)='o'
        IF ((h(i,j).LE.h1).AND.(h(i,j).GT.h0)) val(i,j)='-'
        IF (h(i,j).LE.h0) val(i,j)=':'
      ENDIF
    END DO
  END DO

  !! TONGA
  !val(154,86)="X"
  !val(145,65)="X"
  !val(128,91)="M"

  !
  OPEN(30,file='segrid',form='FORMATTED')

  DO j=jmt,1,-1
    WRITE(30,'(5000a1)')(val(i,j),i=1,imt)
  END DO

  CLOSE(30)

  DEALLOCATE(val)
  DEALLOCATE(h)

  CALL sub_coord_dealloc()
  CALL sub_scalef_dealloc()
  IF (key_roms) CALL sub_h_roms_dealloc()

  STOP

END PROGRAM mkseg0
