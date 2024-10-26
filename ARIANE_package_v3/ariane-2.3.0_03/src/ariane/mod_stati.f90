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
!!****h* ariane/mod_stati
!! NAME
!!   mod_stati (mod_stati.f90 - Fortran90 module)
!!
!! USAGE
!!   Include 'USE mod_stati' in the header of your Fortran 90 source 
!!   code.
!!   Then you'll have access to the subroutines:
!!      - sub_stati_alloc
!!      - sub_stati_init
!!      - sub_stati_dealloc
!!
!! FUNCTION
!!   
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
!!   
!!
!! USES
!!   * USE mod_precision
!!
!! USED BY
!!   * mod_quant.f90
!!   * posini.f90
!!   * trajec.f90
!!
!! SOURCE
!!=========================================================================
MODULE mod_stati

  !------------------!
  ! USE ASSOCIAITION !
  !------------------!
  USE mod_precision
  USE mod_init_particules

  !-------------!
  ! DECLARATION !
  !-------------!
  IMPLICIT NONE
  INTEGER(kind=iprec), DIMENSION(:), ALLOCATABLE :: isn

CONTAINS
  !!***
  !=========================================================================
  !!****f* mod_stati/sub_stati_alloc()
  !! NAME
  !!   sub_stati_alloc()
  !!
  !! FUNCTION
  !!   Dynamic allocation of the array isn    INTEGER(kind=iprec), INTENT(in) :: dim1
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
  !!   * Input:
  !!      - dim1: dimension 
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * trajec
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_stati_alloc()

    !-------------!
    ! Declaration !
    !-------------!

    !-------------!
    ! Code begins !
    !-------------!
    !- Dynamic allocation -!
    ALLOCATE(isn(-1:maxsect+1))
    CALL sub_memory(size(isn),'i','isn','sub_stati_alloc')

  END SUBROUTINE sub_stati_alloc
  !!***
  !=========================================================================
  !!****f* mod_stati/sub_stati_init()
  !! NAME
  !!   sub_stati_init()
  !!
  !! FUNCTION
  !!   "isn" intializations.
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
  !!   * INPUT:
  !!      - val: initialization value.
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * trajec
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_stati_init(val)

    !-------------!
    ! Declaration !
    !-------------!
    !- argument -!
    INTEGER(kind=iprec), INTENT(in) :: val

    !-------------!
    ! Code begins !
    !-------------!
    !- Initialization -!
    isn(:)=val

  END SUBROUTINE sub_stati_init
  !!***
  !=========================================================================
  !!****f* mod_stati/sub_stati_dealloc()
  !! NAME
  !!   sub_stati_dealloc()
  !!
  !! FUNCTION
  !!   DEallocate dynamic memory.
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
  !!   * No argument
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * trajec
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_stati_dealloc()

    !-------------!
    ! Code begins !
    !-------------!
    !- Deallocate isn array -!
    IF(ALLOCATED(isn)) THEN
       CALL sub_memory(-size(isn),'i','isn','sub_stati_alloc')
       DEALLOCATE(isn)
    END IF

  END SUBROUTINE sub_stati_dealloc
  !!***
END MODULE mod_stati
