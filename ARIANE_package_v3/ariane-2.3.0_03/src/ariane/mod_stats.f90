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
!!****h* ariane/mod_stats
!! NAME
!!   mod_stats (mod_stats.f90 - Fortran90 module)
!!
!! USAGE
!!   Include 'USE mod_stats' in the header of your Fortran 90 source 
!!   code.
!!   Then you'll have access to the subroutines:
!!      - sub_stats_alloc
!!      - sub_stats_init
!!      - sub_stats_dealloc
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
!!   * USE  mod_precision
!!
!! USED BY
!!   * posini
!!   * trajec
!!
!! SOURCE
!!=========================================================================
MODULE mod_stats

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
  !---------------------------------------------------
  REAL(kind=rprec), DIMENSION(:,:), ALLOCATABLE :: & !
       s0x          , & ! 
       s0x2         , & !
       s0min        , & !
       s0max        , & !
       s1x          , & !
       s1x2         , & !
       s1min        , & ! 
       s1max            !
  !---------------------------------------------------
  REAL(kind=rprec),   DIMENSION(:), ALLOCATABLE :: & !
       sectrans     , & !
       sn               !
  !---------------------------------------------------

CONTAINS
  !!***
  !=========================================================================
  !!****f* mod_stats/sub_stats_alloc()
  !! NAME
  !!   sub_stats_alloc()
  !!
  !! FUNCTION
  !!   Array memory dynamic allocation
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
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * trajec
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_stats_alloc()

    !-------------!
    ! Declaration !
    !-------------!

    !-------------!
    ! Code begins !
    !-------------!
    ALLOCATE(  s0x(nstat, -1:maxsect+1))
    CALL sub_memory(size(s0x),'r','s0x','sub_stats_alloc')
    ALLOCATE( s0x2(nstat, -1:maxsect+1))
    CALL sub_memory(size(s0x2),'r','s0x2','sub_stats_alloc')
    ALLOCATE(s0min(nstat, -1:maxsect+1))
    CALL sub_memory(size(s0min),'r','s0min','sub_stats_alloc')
    ALLOCATE(s0max(nstat, -1:maxsect+1))
    CALL sub_memory(size(s0max),'r','s0max','sub_stats_alloc')
    ALLOCATE(  s1x(nstat, -1:maxsect+1))
    CALL sub_memory(size(s1x),'r','s1x','sub_stats_alloc')
    ALLOCATE( s1x2(nstat, -1:maxsect+1))
    CALL sub_memory(size(s1x2),'r','s1x2','sub_stats_alloc')
    ALLOCATE(s1min(nstat, -1:maxsect+1))
    CALL sub_memory(size(s1min),'r','s1min','sub_stats_alloc')
    ALLOCATE(s1max(nstat, -1:maxsect+1))
    CALL sub_memory(size(s1max),'r','s1max','sub_stats_alloc')
    ALLOCATE(sectrans(0:maxsect+1))
    CALL sub_memory(size(sectrans),'r','sectrans','sub_stats_alloc')
    ALLOCATE(sn(-1:maxsect+1))
    CALL sub_memory(size(sn),'r','sn','sub_stats_alloc')

  END SUBROUTINE sub_stats_alloc
  !!***
  !=========================================================================
  !!****f* mod_stats/sub_stats_init()
  !! NAME
  !!   sub_stats_init()
  !!
  !! FUNCTION
  !!   Array initializations
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
  !!   * No argument.
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * trajec
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_stats_init()

    !-------------!
    ! Code begins !
    !-------------!
    !- initializations -!
    s0x(:,:)  =  0._rprec
    s0x2(:,:) =  0._rprec
    s0min(:,:)=  1.e35_rprec
    s0max(:,:)= -1.e35_rprec
    s1x(:,:)  =  0._rprec
    s1x2(:,:) =  0._rprec
    s1min(:,:)=  1.e35_rprec
    s1max(:,:)= -1.e35_rprec

    sectrans(:)=0._rprec

    sn(:)=0._rprec

  END SUBROUTINE sub_stats_init
  !!***
  !=========================================================================
  !!****f* mod_stats/sub_stats_dealloc()
  !! NAME
  !!   sub_stats_dealloc()
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
  !!   * No argument.
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * trajec
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_stats_dealloc()

    !-------------!
    ! Code begins !
    !-------------!

    CALL sub_memory(-size(s0x),'r','s0x','sub_stats_dealloc')
    DEALLOCATE(  s0x)
    CALL sub_memory(-size(s0x2),'r','s0x2','sub_stats_dealloc')
    DEALLOCATE( s0x2)
    CALL sub_memory(-size(s0min),'r','s0min','sub_stats_dealloc')
    DEALLOCATE(s0min)
    CALL sub_memory(-size(s0max),'r','s0max','sub_stats_dealloc')
    DEALLOCATE(s0max)
    CALL sub_memory(-size(s1x),'r','s1x','sub_stats_dealloc')
    DEALLOCATE(  s1x)
    CALL sub_memory(-size(s1x2),'r','s1x2','sub_stats_dealloc')
    DEALLOCATE( s1x2)
    CALL sub_memory(-size(s1min),'r','s1min','sub_stats_dealloc')
    DEALLOCATE(s1min)
    CALL sub_memory(-size(s1max),'r','s1max','sub_stats_dealloc')
    DEALLOCATE(s1max)

    CALL sub_memory(-size(sectrans),'r','sectrans','sub_stats_dealloc')
    DEALLOCATE(sectrans)

    CALL sub_memory(-size(sn),'r','sn','sub_stats_dealloc')
    DEALLOCATE(sn)

  END SUBROUTINE sub_stats_dealloc
  !!***
END MODULE mod_stats
