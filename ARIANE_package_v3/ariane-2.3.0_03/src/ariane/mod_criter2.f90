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
!!****h* ariane/mod_criter2
!! NAME
!!   mod_criter2 (mod_criter2.f90 - Fortran90 module)
!!
!! USAGE
!!   Include 'USE mod_criter2' in the header of your Fortran 90 source 
!!   code.
!!   Then you'll have access to the subroutine:
!!      - criter2
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
!!   
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
!!   * mod_criter0
!!   * mod_criter1
!!
!! USES
!!   * USE mod_precision
!!
!! USED BY
!!   * posini
!!   * trajec
!!
!! SOURCE
!!=========================================================================  
MODULE mod_criter2

  !--------------------!
  !- Add here modules -!
  !--------------------!
  USE mod_precision
  USE mod_memory
  USE mod_sigma
  USE mod_zinter
  USE mod_input_data

  IMPLICIT NONE

CONTAINS
  !!***
  !=========================================================================
  !!****f* mod_criter2/criter2()
  !! NAME
  !!   criter2()
  !!
  !! FUNCTION
  !!    TRANSPARENT "hydrological" criterion
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
  !!   No arguments.
  !!
  !! TODO
  !!   * users can modify this function to their own criterion.
  !!
  !! USED BY
  !!   * posini
  !!   * trajec
  !!
  !! SOURCE
  !!=======================================================================
  FUNCTION criter2(fi,fj,fk,fl,hi,hj,hk,hl,i,j,k,l)

    !-----------------------------------------------------!
    ! to be used for TRANSPARENT "hydrological" criterion !
    !-----------------------------------------------------!

    !-------------!
    ! Declaration !
    !-------------!
    INTEGER(kind=iprec), OPTIONAL, INTENT(in) :: i,j,k,l
    REAL(kind=rprec)   , OPTIONAL, INTENT(in) :: fi, fj, fk, fl, hi, hj, hk, hl
    LOGICAL criter2

    !-----------!
    ! Criterion !
    !----------------------------------------!
    !- ADD AT THE END OF EACH LINE "!!ctr0" -!
    !----------------------------------------!
    criter2=.FALSE.    !!ctr2
    !
    !------------!
    !- Examples -!
    !------------!
    !     t=zinter(tt,hi,hj,hk,hl)
    !     s=zinter(ss,hi,hj,hk,hl)
    !     r1=sigma(2000._rprec,s,t)
    !     criter2=(r1.le.36.701_rprec)
    !-----------------------------!

    RETURN
  END FUNCTION criter2
  !!***
END MODULE mod_criter2
