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
!!****h* ariane/mod_flags
!! NAME
!!   mod_flags (mod_flags.f90 - Fortran90 module)
!!
!! USAGE
!!   Include 'USE mod_flags' in the header of your Fortran 90 source 
!!   code.
!!   Then you'll have access to the subroutines:
!!      - sub_flags_alloc
!!      - sub_flags_init
!!      - sub_flags_dealloc
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
!!   USE mod_flags
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
!!   * mod_precision
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
MODULE mod_flags

  USE mod_precision
  USE mod_memory

  IMPLICIT NONE

  !--------------------------------------------------------------------!
  ! mtfin: informs about the sections used in quantitative experiments !
  !--------------------------------------------------------------------!
  INTEGER(kind=iprec), DIMENSION(:,:,:), ALLOCATABLE :: mtfin

CONTAINS
  !!***
  !========================================================================
  !!****f* mod_/sub_flags_alloc()
  !! NAME
  !!   sub_flags_alloc()
  !!
  !! FUNCTION
  !!   mtfin dynamic allocation 
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
  !!   * dim1: size of the first  dimension.
  !!   * dim2: size of the second dimension.
  !!   * dim3: size of the third  dimension.
  !!   WARNING : third dimension starts from 0, first and second 
  !!             dimesnions start from 1.
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * trajec
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_flags_alloc(dim1, dim2, dim3)

    !-------------!
    ! Declaration !
    !-------------!
    INTEGER(kind=iprec), INTENT(in) :: dim1, dim2, dim3

    !--------------------------!
    ! mtfin Dynamic Allocation !
    !--------------------------!
    ALLOCATE(mtfin(dim1, dim2, 0:dim3))
    CALL sub_memory(size(mtfin),'i','mtfin','sub_flags_alloc')

  END SUBROUTINE sub_flags_alloc
  !!***
  !=========================================================================
  !!****f* mod_/sub_flags_init()
  !! NAME
  !!   sub_flags_init()
  !!
  !! FUNCTION
  !!   Initialize "mtfin" array with the argument "val".
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
  !!   * val : value which initialize "mtfin"
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * trajec
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_flags_init(val)

    !-------------!
    ! Declaration !
    !-------------!
    INTEGER(kind=iprec), INTENT(in) :: val

    !------------------!
    ! Initialize mtfin !
    !------------------!
    mtfin(:,:,:)=val

  END SUBROUTINE sub_flags_init
  !!***
  !=========================================================================
  !!****f* mod_/sub_flags_dealloc()
  !! NAME
  !!   sub_flags_dealloc()
  !!
  !! FUNCTION
  !!   Deallocate of the dynamic array allocation.
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
  SUBROUTINE sub_flags_dealloc()

    !------------------!
    ! Deallocate mtfin !
    !------------------!
    CALL sub_memory(-size(mtfin),'i','mtfin','sub_flags_alloc')
    DEALLOCATE(mtfin)

  END SUBROUTINE sub_flags_dealloc
  !!***
END MODULE mod_flags
