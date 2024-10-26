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
!!****h* ariane/mod_fx
!! NAME
!!   mod_fx (mod_fx.f90 - Fortran90 module)
!!
!! USAGE
!!   Include 'USE mod_fx' in the header of your Fortran 90 source 
!!   code.
!!   Then you'll have access to the subroutine:
!!      - fx
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
!!   * Use mod_fx
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
!!   * mod_fy
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
MODULE  mod_fx

  USE mod_precision
  USE mod_namelist
  USE mod_input_grid
  USE mod_reducmem

  IMPLICIT NONE

CONTAINS
  !!***
  !========================================================================
  !!****f* mod_fx/fx()
  !! NAME
  !!   fx()
  !!
  !! FUNCTION
  !!   fx computes the ...
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
  !! ARGUMENTS
  !!   * gi: i position.
  !!   * gj: j position.
  !!
  !! TODO
  !!   * add comments
  !!
  !! USED BY
  !!   * posini
  !!   * trajec
  !!   * mod_quant
  !!
  !! SOURCE
  !!=======================================================================
  FUNCTION fx(gi,gj)

    !-------------!
    ! Declaration !
    !-------------!

    !- arguments -!
    REAL(kind=rprec), INTENT(in)  :: gi, gj
    REAL(kind=rprec)              :: fx

    !- local variables -!
    REAL(kind=rprec)              :: xx_uu_SW
    REAL(kind=rprec)              :: xx_uu_NW
    REAL(kind=rprec)              :: xx_uu_SE
    REAL(kind=rprec)              :: xx_uu_NE

    INTEGER(kind=iprec) :: i1, i2, i1s, i2s
    INTEGER(kind=iprec) :: j1, j2, j1s, j2s
    INTEGER(kind=iprec) :: k1s, k2s
    !!NG: 29/06/2009 INTEGER(kind=iprec) :: iper
    REAL(kind=rprec)    :: a, b

    !-------------!
    ! Code begins !
    !-------------!

!!!!!!!!!!!!!!!!!!!!!!!
    !! Longitude Indices !!
!!!!!!!!!!!!!!!!!!!!!!!
    i1   = INT(gi)       ! i1 = integer part of gi
    i2   = i1 + 1
    a    = gi - AINT(gi)
    !!NG: 29/06/2009 iper = 0             ! initialize iper (key_periodic)

    !- test if the grid is periodic -!
    IF (key_periodic) THEN
      !!NG: 29/06/2009 IF (i1 == 1) iper = 1
      IF (i2 >= imt) THEN
        i1   = 1
        i2   = 2
        !!NG: 29/06/2009 iper = 1
      ENDIF
    ELSE
      IF (i2 > imt) i2 = i1
    ENDIF

!!!!!!!!!!!!!!!!!!!!!!!
    !! Latitude Indices !!
!!!!!!!!!!!!!!!!!!!!!!!
    j1 = INT(gj+0.5_rprec)
    j2 = j1 + 1

    IF ((.NOT.key_jfold).OR.((2+int(gj)) < jmt)) THEN

      b  = (gj + 0.5_rprec) - j1
      IF (j2 > jmt) j2 = j1

      !--------------------------------------------------------------!
      !                      Here fx is computed                     !
      !--------------------------------------------------------------!
      CALL sub_reducmem_shift_or_not_ind(i1,j1,1,i1s,j1s,k1s)
      CALL sub_reducmem_shift_or_not_ind(i2,j2,1,i2s,j2s,k2s)

      !---------------!
      !- Periodicity -!
      !---------------!
      ! independant of where the longitude is cutted and 
      ! longitude conventions: 0/360 or -180/180 or 80/420

      !NG: 22 april 2011 Test !!
      IF ( (.NOT.(key_roms))     .AND. &
           (.NOT.(key_symphonie))      &
           ) THEN

        IF (xx_uu(i1s, j1s, 1, 1) > xx_uu(i2s, j1s, 1, 1)) THEN
          xx_uu_SW=xx_uu(i1s, j1s, 1, 1) - 360._rprec
        ELSE
          xx_uu_SW = xx_uu(i1s, j1s, 1, 1)
        ENDIF
        IF (xx_uu(i1s, j2s, 1, 1) > xx_uu(i2s, j2s, 1, 1)) THEN
          xx_uu_NW=xx_uu(i1s, j2s, 1, 1) - 360._rprec
        ELSE
          xx_uu_NW = xx_uu(i1s, j2s, 1, 1)
        ENDIF

      ELSE
        xx_uu_SW = xx_uu(i1s, j1s, 1, 1)
        xx_uu_NW = xx_uu(i1s, j2s, 1, 1)
      ENDIF

      xx_uu_SE=xx_uu(i2s, j1s, 1, 1)
      xx_uu_NE=xx_uu(i2s, j2s, 1, 1)

      !--------------!
      !- Compute fx -!
      !--------------!
      !! NG: write(*,*)' '
      !! NG: write(*,*)'xx_uu(i1s, j1s, 1, 1), xx_uu_SW: ', xx_uu(i1s, j1s, 1, 1), xx_uu_SW
      !! NG: write(*,*)'xx_uu(i1s, j2s, 1, 1), xx_uu_NW: ', xx_uu(i1s, j2s, 1, 1), xx_uu_NW
      !! NG: write(*,*)'xx_uu(i2s, j1s, 1, 1), xx_uu_SE: ', xx_uu(i2s, j1s, 1, 1), xx_uu_SE
      !! NG: write(*,*)'xx_uu(i2s, j2s, 1, 1), xx_uu_NE: ', xx_uu(i2s, j2s, 1, 1), xx_uu_NE

      fx = (1._rprec-a) * (1._rprec-b) * xx_uu_SW + &
           (1._rprec-a) *           b  * xx_uu_NW + &
           a            * (1._rprec-b) * xx_uu_SE + &
           a            *           b  * xx_uu_NE

      !! NG:  write(*,*)'fx = ', fx

      ! add BB 25/06/09 blindage drakkar
      !!NG: 21 feb 2011 IF (fx > 180._rprec) THEN
      !!NG: 21 feb 2011    fx = fx - 180._rprec
      !!NG: 21 feb 2011    !! NG: write(*,*)'fx - 180. = ', fx
      !!NG: 21 feb 2011 END IF
      ! end BB 25/06/09 blindage drakkar
      !--------------------------------------------------------------!

    ELSE

      ! easy patch for j-folding condition
      ! in the vicinity of the singularity, we use longitude of the nearest T

      j1   = INT(gj)       
      j2   = j1

      CALL sub_reducmem_shift_or_not_ind(i1,j1,1,i1s,j1s,k1s)
      CALL sub_reducmem_shift_or_not_ind(i2,j2,1,i2s,j2s,k2s)

      fx = xx_tt(i1s+1, j1s+1, 1, 1)

    ENDIF


    RETURN

  END FUNCTION fx
  !!***
END MODULE mod_fx
