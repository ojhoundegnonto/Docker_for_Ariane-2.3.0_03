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
!!****h* ariane/mod_fy
!! NAME
!!   mod_fy (mod_fy.f90 - Fortran90 module)
!!
!! USAGE
!!   Include 'USE mod_fy' in the header of your Fortran 90 source 
!!   code.
!!   Then you'll have access to the subroutine:
!!      - fy
!!
!! AUTHOR
!!   * Origin  : Bruno Blanke  (1992)
!!   * F77toF90: Nicolas Grima (April-May 2005)
!! 
!! CREATION DATE
!!   * April-May 2005
!!
!! HISTORY
!!   Date (dd/mm/yyyy/) - Modification(s)
!!
!! RESULT
!!   
!!
!! EXAMPLES
!!   * USE mod_fy
!!
!! NOTES
!!   ROBODoc header style.
!!
!! TODO
!!   
!!
!! PORTABILITY
!!         Machine-OS    - Fortran90/95 compiler
!!   * i686-pc-linux-gnu -         ifort
!!   * i686-pc-linux-gnu -          g95
!!
!! SEE ALSO
!!   * mod_fx
!!   * mod_fz
!!
!! USES
!!   * USE mod_precision
!!   * USE mod_namelist
!!   * USE mod_input_grid
!!
!! USED BY
!!   * posini
!!   * trajec
!!
!! SOURCE
!!========================================================================= 
MODULE mod_fy

  USE mod_precision
  USE mod_namelist
  USE mod_input_grid
  USE mod_reducmem

  IMPLICIT NONE

CONTAINS
  !!***
  !=========================================================================
  !!****f* mod_fy/fy()
  !! NAME
  !!   fy()
  !!
  !! FUNCTION
  !!   fy computes the ...
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
  !!   * gi: i position.
  !!   * gj: j position.
  !!
  !! TODO
  !!    * add comments
  !!
  !! USED BY
  !!   * posini
  !!   * trajec
  !!
  !! SOURCE
  !!=======================================================================
  FUNCTION fy(gi,gj)

    !-------------!
    ! Declaration !
    !-------------!

    !- arguments -!
    REAL(kind=rprec), INTENT(in)  :: gi, gj
    REAL(kind=rprec)              :: fy

    !- local variables -!
    INTEGER(kind=iprec) :: i1, i2, i1s, i2s
    INTEGER(kind=iprec) :: j1, j2, j1s, j2s
    INTEGER(kind=iprec) :: k1s, k2s
    REAL(kind=rprec)    :: a, b

    !-------------!
    ! Code begins !
    !-------------!
    i1 = INT(gi + 0.5_rprec, kind = iprec)
    i2 = i1 + 1_iprec
    a  = (gi + 0.5_rprec) - REAL(i1, kind = rprec)

    IF (key_periodic) THEN
      IF (i2.GE.imt) THEN
        i1 = 1_iprec
        i2 = 2_iprec
      ENDIF
    ELSE
      IF (i2.GT.imt) i2 = i1
    ENDIF

    j1 = INT(gj, kind = iprec)
    j2 = j1 + 1_iprec

 IF ((.NOT.key_jfold).OR.((2+int(gj)) < jmt)) THEN

    b  = gj - AINT(gj)
    IF (j2.GT.jmt) j2=j1

    !-------------------------------------------!
    !           Here fy is computed             !
    !-------------------------------------------!
    CALL sub_reducmem_shift_or_not_ind(i1,j1,1,i1s,j1s,k1s)
    CALL sub_reducmem_shift_or_not_ind(i2,j2,1,i2s,j2s,k2s)

    fy=  (1._rprec-a) * (1._rprec-b) * yy_vv(i1s,j1s,1,1) + &
         (1._rprec-a) *           b  * yy_vv(i1s,j2s,1,1) + &
         a            * (1._rprec-b) * yy_vv(i2s,j1s,1,1) + &
         a             *          b  * yy_vv(i2s,j2s,1,1)
    !-------------------------------------------!

 ELSE

 ! easy patch for j-folding condition
 ! in the vicinity of the singularity, we use longitude of the nearest T

    i1   = INT(gi)
    i2   = i1

    CALL sub_reducmem_shift_or_not_ind(i1,j1,1,i1s,j1s,k1s)
    CALL sub_reducmem_shift_or_not_ind(i2,j2,1,i2s,j2s,k2s)

    fy = yy_tt(i1s+1, j1s+1, 1, 1)

  ENDIF

    RETURN

  END FUNCTION fy
  !!***
END MODULE mod_fy
