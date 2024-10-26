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
MODULE mod_print_criters

  USE mod_precision
  USE mod_configure
  USE mod_lun

  IMPLICIT NONE

 
CONTAINS
  !!***
  !========================================================================
  SUBROUTINE sub_print_criters()

    CALL sub_print_criter0()
    CALL sub_print_criter1()
    CALL sub_print_criter2()

  END SUBROUTINE sub_print_criters

  !!***
  !========================================================================
  SUBROUTINE sub_print_criter0()

    CHARACTER(len=256) :: shell_command

    shell_command = 'cp '//TRIM(PREFIX)//'/bin/mod_criter0.f90 ./.'

    WRITE(lun_standard,*)''
    WRITE(lun_standard,*)'======================='
    WRITE(lun_standard,*)'= criter0 source code ='
    WRITE(lun_standard,*)'======================='
    CALL SYSTEM(TRIM(shell_command))
    CALL SYSTEM ('grep -i ctr0 mod_criter0.f90')

  END SUBROUTINE sub_print_criter0

  !!***
  !========================================================================
  SUBROUTINE sub_print_criter1()

    CHARACTER(len=256) :: shell_command

    shell_command = 'cp '//TRIM(PREFIX)//'/bin/mod_criter1.f90 ./.'

    WRITE(lun_standard,*)''
    WRITE(lun_standard,*)'======================='
    WRITE(lun_standard,*)'= criter1 source code ='
    WRITE(lun_standard,*)'======================='
    CALL SYSTEM(TRIM(shell_command))
    CALL SYSTEM ('grep -i ctr1 mod_criter1.f90')

  END SUBROUTINE sub_print_criter1

  !!***
  !========================================================================
  SUBROUTINE sub_print_criter2()

    CHARACTER(len=256) :: shell_command

    shell_command = 'cp '//TRIM(PREFIX)//'/bin/mod_criter2.f90 ./.'

    WRITE(lun_standard,*)''
    WRITE(lun_standard,*)'======================='
    WRITE(lun_standard,*)'= criter2 source code ='
    WRITE(lun_standard,*)'======================='
    CALL SYSTEM(TRIM(shell_command))
    CALL SYSTEM ('grep -i ctr2 mod_criter2.f90')
  END SUBROUTINE sub_print_criter2


END MODULE mod_print_criters
