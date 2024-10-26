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
!!****h* ariane/mod_memory
!! NAME
!!   mod_memory (mod_memory.f90 - Fortran90 module)
!!
!! USAGE
!!   Include 'USE mod_memory' in the header of your Fortran 90 source 
!!   code.
!!
!! FUNCTION
!!   Allocate Logical Unit Numbers (MEMORY).
!!   You have to add or change MEMORY in this module.
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
!!   * mod_param.f90
!!   * mod_reducmem.f90
!!   * mod_quant.f90
!!   * trajec.f90
!!
!! SOURCE
!!=========================================================================
MODULE mod_memory

  !------------------!
  ! USE ASSOCIAITION !
  !------------------!
  USE mod_precision
  USE mod_lun
  USE mod_namelist

  !-------------!
  ! DECLARATION !
  !-------------!
  IMPLICIT NONE

  !!NG: 23/07/2009 change iprec by ilong to represent value
  !!NG: 23/07/2009 upper than 2GB.
  INTEGER(kind=ilong), PRIVATE :: &
       memory_r, & ! memory for allocatated real values
       memory_i, & ! memory for allocatated integer vales
       memory  , & ! memory_r + memory_i
       memory_max

CONTAINS

  !!***
  !=========================================================================
  !!****f* mod_memory/sub_open_memory_file()
  SUBROUTINE sub_open_memory_file()

    INTEGER(kind = iprec) :: ios

    OPEN (                           &
         UNIT = lun_memory         , & 
         FILE = 'ariane_memory.log', &
         FORM = "formatted"        , &
         ACCESS = "sequential"     , &
         ACTION = "write"          , &
         POSITION = "rewind"       , &
         IOSTAT = ios)

    IF (IOS /= 0 ) THEN
      WRITE(lun_error,*)':-(:-(:-(:-(:-(:-(:-(:-(:-(:-(:-(:-(:-(:-(:-(:-(:-('
      WRITE(lun_error,*)'mod_memory: problem to open ariane memory log file'
      WRITE(lun_error,*)':-(:-(:-(:-(:-(:-(:-(:-(:-(:-(:-(:-(:-(:-(:-(:-(:-('
      STOP
    ENDIF

    memory_r = 0
    memory_i = 0
    memory   = 0
    memory_max = 0

  END SUBROUTINE sub_open_memory_file

  !!***
  !=========================================================================
  !!****f* mod_memory/sub_memory()
  SUBROUTINE sub_memory( &
       mem_size        , &
       mem_type        , &
       mem_var         , &
       mem_sub           )

    !! CALL sub_memory(size(),'','','')

    INTEGER(kind = iprec) :: mem_size
    CHARACTER(LEN = 1)    :: mem_type
    CHARACTER(LEN = *)    :: mem_var
    CHARACTER(LEN = *)    :: mem_sub
    LOGICAL               :: is_opened

    IF (TRIM(mem_type) == 'r') THEN
      mem_size = mem_size * rprec
      memory_r = memory_r + mem_size
    ELSEIF (TRIM(mem_type) == 'i') THEN
      mem_size = mem_size * iprec
      memory_i = memory_i + mem_size
    ELSE
      STOP
    ENDIF

    memory = memory_r + memory_i

    IF (memory > memory_max )  memory_max=memory

    INQUIRE (UNIT = lun_memory, OPENED=is_opened)

    IF (memory_log.AND.is_opened) THEN
      WRITE(UNIT = lun_memory, FMT = 1001 ) &
           memory        , &
           memory_r      , &
           memory_i      , &
           mem_size      , &
           TRIM(mem_type), &
           TRIM(mem_var) , &
           TRIM(mem_sub)
    ENDIF

1001 FORMAT(I12,1x,I12,1x,I12,1x,I12,1x,A,1x,A,1x,A)

    !!NG : to read data on matlab
    !!NG: [memory, mem_r, mem_i, mem, tabname, subname] = ...
    !!NG: textread('ariane_memory.log','%d %d %d %d %s %s',-1);
    !!NG: plot((memory/1024)/1024);
    !!NG: hold on
    !!NG: plot ((mem_r/1024)/1024,'r');
    !!NG: plot ((mem_i/1024)/1024,'g');
    !!NG: title('Ariane memory log in MegaBytes (MB)');
    !!NG: ylabel('Memory in MB');
    !!NG: xlabel('Numbre of allocate and deallocate');

  END SUBROUTINE sub_memory
  !!***
  !=========================================================================
  !!****f* mod_memory/sub_close_memory_file()
  SUBROUTINE sub_close_memory_file()

    close(UNIT = lun_memory)

    WRITE(lun_standard,*)''
    WRITE(lun_standard,*)' Ariane max allocatable memory is (in MB): ', &
         (memory_max / 1024._rprec)/1024_rprec

  END SUBROUTINE sub_close_memory_file
  !!***
END MODULE mod_memory
!!***
