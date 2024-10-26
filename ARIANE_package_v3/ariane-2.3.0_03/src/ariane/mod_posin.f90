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
!!****h* ariane/mod_posin
!! NAME
!!   mod_posin (mod_posin.f90 - Fortran90 module)
!!
!! USAGE
!!   Include 'USE mod_posin' in the header of your Fortran 90 source 
!!   code.
!!   Then you'll have access to the subroutine:
!!      - sub_posini_alloc
!!      - sub_posini_dealloc
!!
!! FUNCTION
!!   Declare, allocate and deallocate variables.
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
!!   * posini.f90
!!   * trajec.f90
!!
!! SOURCE
!!=========================================================================
MODULE mod_posin

  !------------------!
  ! USE ASSOCIAITION !
  !------------------!
  USE mod_precision
  USE mod_memory
  USE mod_namelist

  !-------------!
  ! DECLARATION !
  !-------------!
  IMPLICIT NONE

  REAL(kind=rprec), DIMENSION(:), ALLOCATABLE :: & 
       tfi, & ! x initial positions
       tfj, & ! y initial positions
       tfk, & ! z initial positions
       tfl, & ! time initial positions
       ttr, & ! transport
       tage   ! age

  REAL(kind=rprec), DIMENSION(:), ALLOCATABLE :: & 
       init_temp , &
       init_salt , &
       init_dens

CONTAINS
  !!***
  !=========================================================================
  !!****f* mod_posin/sub_posin_alloc()
  !! NAME
  !!   sub_posin_alloc()
  !!
  !! FUNCTION
  !!   Dynamic allocation
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
  SUBROUTINE sub_posin_alloc(dim1)

    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind=iprec), INTENT(in) :: dim1

    !-------------!
    ! Code begins !
    !-------------!
    !- Dynamic allocation -!
    ALLOCATE(tfi(dim1))
    CALL sub_memory(size(tfi),'r','tfi','sub_posin_alloc')
    ALLOCATE(tfj(dim1))
    CALL sub_memory(size(tfj),'r','tfj','sub_posin_alloc')
    ALLOCATE(tfk(dim1))
    CALL sub_memory(size(tfk),'r','tfk','sub_posin_alloc')
    ALLOCATE(tfl(dim1))
    CALL sub_memory(size(tfl),'r','tfl','sub_posin_alloc')

    tfl(:)=-1._rprec
    ALLOCATE(ttr(dim1))
    CALL sub_memory(size(ttr),'r','ttr','sub_posin_alloc')
    ALLOCATE(tage(dim1))
    CALL sub_memory(size(tage),'r','tage','sub_posin_alloc')

    IF(key_alltracers) THEN
      ALLOCATE(init_temp(dim1))
      CALL sub_memory(size(init_temp),'r','init_temp','sub_posin_alloc')
      ALLOCATE(init_salt(dim1))
      CALL sub_memory(size(init_salt),'r','init_salt','sub_posin_alloc')
      ALLOCATE(init_dens(dim1))
      CALL sub_memory(size(init_dens),'r','init_dens','sub_posin_alloc')
    ENDIF

  END SUBROUTINE sub_posin_alloc
  !!***
  !========================================

  !  subroutine sub_posin_init()   !
  !                                !  --> see posini.f90
  !  end subroutine sub_posin_init !

  !=========================================================================
  !!****f* mod_posin/sub_posin_dealloc()
  !! NAME
  !!   sub_posin_dealloc()
  !!
  !! FUNCTION
  !!   Deallocate memory.
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
  !!   No arguments.
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * trajec
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_posin_dealloc()

    !-------------!
    ! Code begins !
    !-------------!
    !- Deallocate memory -!
    CALL sub_memory(-size(tfi),'r','tfi','sub_posin_dealloc')
    DEALLOCATE(tfi)
    CALL sub_memory(-size(tfj),'r','tfj','sub_posin_dealloc')
    DEALLOCATE(tfj)
    CALL sub_memory(-size(tfk),'r','tfk','sub_posin_dealloc')
    DEALLOCATE(tfk)
    CALL sub_memory(-size(tfl),'r','tfl','sub_posin_dealloc')
    DEALLOCATE(tfl)
    CALL sub_memory(-size(ttr),'r','ttr','sub_posin_dealloc')
    DEALLOCATE(ttr)
    CALL sub_memory(-size(tage),'r','tage','sub_posin_dealloc')
    DEALLOCATE(tage)

    CALL sub_posin_dealloc_init_tracers()

  END SUBROUTINE sub_posin_dealloc

  !!***
  !!=======================================================================
  SUBROUTINE sub_posin_dealloc_init_tracers()

    !-------------!
    ! Code begins !
    !-------------!
    IF (ALLOCATED(init_temp)) THEN
       CALL sub_memory(-size(init_temp),'r','init_temp','sub_posin_dealloc_init_tracers')
       DEALLOCATE(init_temp)
    ENDIF
    IF (ALLOCATED(init_salt))THEN
       CALL sub_memory(-size(init_salt),'r','init_salt','sub_posin_dealloc_init_tracers')
        DEALLOCATE(init_salt)
    ENDIF
    IF (ALLOCATED(init_dens))THEN
       CALL sub_memory(-size(init_dens),'r','init_dens','sub_posin_dealloc_init_tracers')
        DEALLOCATE(init_dens)
    ENDIF

  END SUBROUTINE sub_posin_dealloc_init_tracers
  !!***
END MODULE mod_posin
