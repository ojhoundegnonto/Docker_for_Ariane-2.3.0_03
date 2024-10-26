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
!!****h* ariane/mod_lun
!! NAME
!!   mod_lun (mod_lun.f90 - Fortran90 module)
!!
!! USAGE
!!   Include 'USE mod_lun' in the header of your Fortran 90 source 
!!   code.
!!
!! FUNCTION
!!   Allocate Logical Unit Numbers (LUN).
!!   You have to add or change LUN in this module.
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
MODULE mod_lun

  !------------------!
  ! USE ASSOCIAITION !
  !------------------!
  USE mod_precision

  !-------------!
  ! DECLARATION !
  !-------------!
  IMPLICIT NONE

  INTEGER(kind=iprec), PARAMETER :: &
       lun_error       =  0 , & ! error messages
       lun_nml         =  4 , & ! namelist
       lun_standard    =  6 , & ! Fortran standard output
       lun_subset      = 11 , & ! subset.txt
       lun_coast_crash = 13 , & ! coast crash
       lun_sections    = 24 , & ! sections.txt
       lun_init        = 54 , & ! init.sav
       lun_final       = 52 , & ! final.sav
       lun_reg         = 65 , & ! region_limits
       lun_output      = 58 , & ! output
       lun_initial     = 53 , & ! initial.bin
       lun_traj        = 55 , & ! traj.txt
       lun_stats       = 57 , & ! stats.txt
       lun_init_pos    = 50 , & ! init_pos.txt
       lun_fin_pos     = 59 , & ! final_pos.txt
       lun_memory      = 60 , & ! ariane_memory.log
       lun_trans       = 90 , & ! transparent sections
       lun_dummy       = 99     ! a dummy L. U. N.

END MODULE mod_lun
!!***
