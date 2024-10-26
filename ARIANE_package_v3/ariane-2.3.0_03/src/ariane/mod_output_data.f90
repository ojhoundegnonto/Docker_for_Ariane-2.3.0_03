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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! UNDER CONSTRUCTION                     !!
!! TO introduce NETCDF format for outputs !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
MODULE mod_output_data

  USE mod_precision
  USE mod_lun
  USE mod_namelist

  IMPLICIT NONE

CONTAINS

  !!================================================================================
  SUBROUTINE sub_output_data_open_files()

    WRITE(lun_standard,*)''
    WRITE(lun_standard,*)'================================'
    WRITE(lun_standard,*)'= Opening Ascii Output file(s) ='
    WRITE(lun_standard,*)'================================'

    IF (key_ascii_outputs) THEN

      WRITE(lun_standard,*)'  - ASCII files -'

      !=================================================================
      ! we open output files
      !=================================================================

      OPEN(UNIT=lun_init,file='init.sav'  ,form='UNFORMATTED', ACTION='WRITE')
      WRITE(lun_standard,*)'    - init.sav'

      OPEN(UNIT=lun_final,file='final.sav',form='UNFORMATTED', ACTION='WRITE')
      WRITE(lun_standard,*)'    - final.sav'

      OPEN(UNIT=lun_output,file='output', form='FORMATTED'  , ACTION='WRITE')
      WRITE(lun_standard,*)'    - output'

      !- QUANTITATIVE -!
      IF (TRIM(mode) == 'quantitative') THEN

        OPEN(UNIT=lun_init_pos, file='init_pos.txt' , form='FORMATTED')
        WRITE(lun_standard,*)'    - init_pos.txt'

        OPEN(UNIT=lun_fin_pos , file='final_pos.txt', form='FORMATTED')
        WRITE(lun_standard,*)'    - final_pos.txt'

      ENDIF

    ENDIF

    IF (TRIM(mode) == 'quantitative') THEN
      OPEN(UNIT=lun_stats   , file='stats.txt'    , form='FORMATTED')
      WRITE(lun_standard,*)'    - stats.txt'
    ENDIF

  END SUBROUTINE sub_output_data_open_files

  !!================================================================================
  SUBROUTINE sub_output_data_coast_crash()
  END SUBROUTINE sub_output_data_coast_crash

END MODULE mod_output_data
