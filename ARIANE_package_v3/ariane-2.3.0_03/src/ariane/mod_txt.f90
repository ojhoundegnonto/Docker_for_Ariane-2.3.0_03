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
!!****h* ariane/mod_txt
!! NAME
!!   mod_txt (mod_txt.f90 - Fortran90 module)
!!
!! USAGE
!!   Include 'USE mod_txt' in the header of your Fortran 90 source 
!!   code.
!!   Then you'll have access to the subroutine:
!!      - sub_txt_alloc
!!      - sub_txt_init
!!      - sub_txt_dealloc
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
!!   * USE mod_namelist
!!
!! USED BY
!!   * mod_param.f90
!!   * mod_quant.f90
!!   * posini
!!   * trajec
!!
!! SOURCE
!!=========================================================================
MODULE mod_txt

  !------------------!
  ! USE ASSOCIAITION !
  !------------------!
  USE mod_precision
  USE mod_namelist
  USE mod_init_particules

  !-------------!
  ! DECLARATION !
  !-------------!
  IMPLICIT NONE
  !---------------------------------------------------------------
  CHARACTER(len=1) , DIMENSION(:), ALLOCATABLE :: &
       chpstat    !
  CHARACTER(len=15), DIMENSION(:), ALLOCATABLE :: &
       fntname, & !
       secname    !
  CHARACTER(len=1)  :: &
       idiru, & !
       idirv, & !
       idirw    !
  CHARACTER(len=15) :: sname !
  CHARACTER(len=20) :: pipo  !
  CHARACTER(len=30) :: fname !
  !---------------------------------------------------------------

CONTAINS
  !!***
  !=========================================================================
  !!****f* mod_txt/sub_txt_alloc()
  !! NAME
  !!   sub_txt_alloc()
  !!
  !! FUNCTION
  !!   Array memory dynamic allocation.
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
  !!   * INPUTS:
  !!      - dim1: dimension 1
  !!      - dim2: dimesnion 2
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * trajec
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_txt_alloc()

    !-------------!
    ! Declaration !
    !-------------!

    !-------------!
    ! Code begins !
    !-------------!
    !- Dynamic allocation -!
    ALLOCATE(fntname(maxsect))
    ALLOCATE(secname(0:maxsect+1))
    ALLOCATE(chpstat(nstat))
  END SUBROUTINE sub_txt_alloc
  !!***
  !=========================================================================
  !!****f* mod_txt/sub_txt_init()
  !! NAME
  !!   sub_txt_init()
  !!
  !! FUNCTION
  !!   Array intializations.
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
  SUBROUTINE sub_txt_init()

    !-------------!
    ! Code begins !
    !-------------!
    !- Initialization -!
    chpstat(1)='x'
    chpstat(2)='y'
    chpstat(3)='z'
    chpstat(4)='a'
    IF (key_alltracers) THEN
      chpstat(5)='t'
      chpstat(6)='s'
      chpstat(7)='r'
    ENDIF

  END SUBROUTINE sub_txt_init
  !!***
  !=========================================================================
  !!****f* mod_txt/sub_txt_dealloc()
  !! NAME
  !!   sub_txt_dealloc()
  !!
  !! FUNCTION
  !!   DEallocate array dynamic memory.
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
  SUBROUTINE sub_txt_dealloc()

    !-------------!
    ! Code begins !
    !-------------!
    !- Deallocate isn array -!
    DEALLOCATE(fntname)
    DEALLOCATE(secname)
    DEALLOCATE(chpstat)

  END SUBROUTINE sub_txt_dealloc
  !!***
END MODULE mod_txt
