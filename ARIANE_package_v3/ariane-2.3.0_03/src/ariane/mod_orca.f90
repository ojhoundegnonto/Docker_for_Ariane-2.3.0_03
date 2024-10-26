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
MODULE mod_orca

  USE mod_precision
  USE mod_namelist ! imt, jmt, key_jfold, pivot

  IMPLICIT NONE

  INTERFACE sub_orca_north_pole
    MODULE PROCEDURE                     &
         sub_orca_north_pole_int_scalar, &
         sub_orca_north_pole_real_scalar
  END INTERFACE sub_orca_north_pole

  INTERFACE sub_orca_east_west_periodic
    MODULE PROCEDURE                          &
         sub_orca_e_w_perio_int_scalar, &
         sub_orca_e_w_perio_real_scalar
  END INTERFACE sub_orca_east_west_periodic

CONTAINS

  !!==============================================================
  !---------------------------------------------
  ! folding conditions in the vicinity of j=jmt
  !---------------------------------------------
  ! BBL __________________________________________________________________________
  ! BBL BBL: Regle de repliement meridien telle qu'elle est appliquee dans ORCA2
  ! BBL  NG: C est également valable pour ORCA 4, 2, 025 (T-point pivot)
  ! NG   NG: en revanche elle est différente pour ORCA05 (F-point pivot)
  ! NG
  ! NG   T-point pivot: (ORCA 4, 2, 0.25)
  ! NG   -------------
  ! NG
  ! NG     IF (j0(n) >= jmt) THEN
  ! NG       
  ! NG        i0(n)=     imt + 2 - i0(n)
  ! NG
  ! NG        j0(n)= 2 * jmt - 2 - j0(n)
  ! NG
  ! NG      ENDIF
  ! NG
  ! NG   F-point pivot: (ORCA 0.5)
  ! NG   -------------
  ! NG
  ! NG     IF (j0(n) >= jmt) THEN
  ! NG       
  ! NG        i0(n)=     imt + 1 - i0(n)
  ! NG
  ! NG        j0(n)= 2 * jmt - 1 - j0(n)
  ! NG
  ! NG      ENDIF
  ! NG
  SUBROUTINE sub_orca_north_pole_int_scalar(i_ind, j_ind, j_to_test, cycl_cond)

    INTEGER(kind = iprec), INTENT(inout) :: i_ind, j_ind
    INTEGER(kind = iprec), OPTIONAL, INTENT(in) :: j_to_test
    INTEGER(kind = iprec), OPTIONAL, INTENT(out):: cycl_cond

    INTEGER(kind = iprec) :: factor
    INTEGER(kind = iprec) :: j_2_test

    IF (PRESENT(j_to_test)) THEN
      j_2_test = j_to_test
    ELSE
      j_2_test = j_ind
    ENDIF

    IF ((key_jfold).AND.(j_2_test >= jmt )) THEN

      IF (TRIM(pivot) == 'T') THEN

        factor = 2

      ELSEIF (TRIM(pivot) == 'F') THEN

        factor = 1

      ENDIF

      i_ind =     imt + factor - i_ind

      j_ind = 2 * jmt - factor - j_ind

      IF(PRESENT(cycl_cond)) cycl_cond = 1

    ELSE

      IF(PRESENT(cycl_cond)) cycl_cond = 0

    ENDIF

  END SUBROUTINE sub_orca_north_pole_int_scalar

  !!====================================================================
  SUBROUTINE sub_orca_north_pole_real_scalar(x_ind, y_ind, j_to_test)

    INTEGER(kind = iprec), INTENT(in) :: j_to_test
    REAL(kind = rprec)   , INTENT(inout) :: x_ind, y_ind

    INTEGER(kind = iprec) :: factor

    IF ((key_jfold).AND.(j_to_test >= jmt)) THEN

      IF (TRIM(pivot) == 'T') THEN

        factor = 2

      ELSEIF (TRIM(pivot) == 'F') THEN

        factor = 1

      ENDIF

      x_ind = REAL(     imt + (factor - 1), kind = rprec) - x_ind

      y_ind = REAL( 2 * jmt - (factor + 1), kind = rprec) - y_ind


    ENDIF

  END SUBROUTINE sub_orca_north_pole_real_scalar

  !!==============================================================
  !!==============================================================
  !------------------------------
  ! beware of cyclic conditions !
  !------------------------------ 
  ! BBL __________________________________________________________________________
  ! BBL BBL: Regle de periodicite zonale telle qu'elle est appliquee dans ORCA2
  ! BBL  NG: Cette règle est également valable pour ORCA 4, 2, 05 et 025

  SUBROUTINE sub_orca_e_w_perio_int_scalar(i_ind, &
       i_to_test_west, i_to_test_east, cycl_cond)

    INTEGER(kind = iprec)          , INTENT(inout) :: i_ind
    INTEGER(kind = iprec), OPTIONAL, INTENT(in)    :: i_to_test_west
    INTEGER(kind = iprec), OPTIONAL, INTENT(in)    :: i_to_test_east
    INTEGER(kind = iprec), OPTIONAL, INTENT(out)   :: cycl_cond

    INTEGER(kind = iprec) :: i_2_test_west, i_2_test_east

    IF (PRESENT(i_to_test_west)) THEN
      i_2_test_west = i_to_test_west
    ELSE
      i_2_test_west = i_ind
    ENDIF

    IF (PRESENT(i_to_test_east)) THEN
      i_2_test_east = i_to_test_east
    ELSE
      i_2_test_east = i_ind
    ENDIF


    IF (key_periodic) THEN 

      IF (i_2_test_west <= 1) THEN

        i_ind = i_ind + (imt - 2)

        IF(PRESENT(cycl_cond)) cycl_cond = 1

      ELSEIF (i_2_test_east > imt) THEN

        i_ind = i_ind - (imt - 2)

        IF(PRESENT(cycl_cond)) cycl_cond = 2

      ELSE

        IF(PRESENT(cycl_cond)) cycl_cond = 0

      ENDIF

    ENDIF

  END SUBROUTINE sub_orca_e_w_perio_int_scalar

  !==============================================================
  SUBROUTINE sub_orca_e_w_perio_real_scalar(x_ind, i_to_test_west, i_to_test_east)

    REAL(kind = rprec)   , INTENT(inout) :: x_ind
    INTEGER(kind = iprec), INTENT(in)    :: i_to_test_west
    INTEGER(kind = iprec), INTENT(in)    :: i_to_test_east

    IF (key_periodic) THEN 

      IF (i_to_test_west <= 1) THEN

        x_ind = x_ind + REAL((imt - 2), kind = rprec)

      ELSEIF (i_to_test_east > imt) THEN

        x_ind = x_ind -  REAL((imt - 2), kind = rprec)

      ENDIF

    ENDIF

  END SUBROUTINE sub_orca_e_w_perio_real_scalar

END MODULE mod_orca
