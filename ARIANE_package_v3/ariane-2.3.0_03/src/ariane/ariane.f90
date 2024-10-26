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
PROGRAM prg_ariane

  USE mod_precision
  USE mod_configure
  USE mod_memory
  USE mod_namelist
  USE mod_print_criters

  !--------------!
  ! DECLARATIONS !
  !--------------!
  IMPLICIT NONE 

  !-----------!
  ! MAIN PART !
  !-----------!
  ! Print Ariane version !
  WRITE(lun_standard,*)''
  WRITE(lun_standard,*)' If Ariane results are used in your publication(s),'
  WRITE(lun_standard,*)'please feel free to reference Ariane and to send us '
  WRITE(lun_standard,*)'       your publication references (thanks).'
  WRITE(lun_standard,*)''
  WRITE(lun_standard,*)'        http://www.univ-brest.fr/lpo/ariane'
  WRITE(lun_standard,*)''
  WRITE(lun_standard,*)'====================================================='
  WRITE(lun_standard,*)'=             -o0)  ARIANE v'//TRIM(VERSION)//'  (0o-             ='
  WRITE(lun_standard,*)'====================================================='
  WRITE(lun_standard,*)''
  WRITE(lun_standard,*)'       (Ariane is governed by the CeCILL license)'
  WRITE(lun_standard,*)'               (http://www.cecill.info)'

  WRITE(lun_standard,*)''
  WRITE(lun_standard,*)'====================='
  WRITE(lun_standard,*)'= Machine precision ='
  WRITE(lun_standard,*)'====================='
  WRITE(lun_standard,*)'  -            iprec:', iprec
  WRITE(lun_standard,*)'  -            rprec:', rprec
  WRITE(lun_standard,*)'  -            qprec:', qprec
  WRITE(lun_standard,*)'  -     range(iprec):', RANGE(0_iprec)
  WRITE(lun_standard,*)'  -     range(rprec):', RANGE(0._rprec)
  WRITE(lun_standard,*)'  - Precision(rprec):', PRECISION(1.0_rprec)
  WRITE(lun_standard,*)'  -   Spacing(rprec):', SPACING(1.0_rprec)
  WRITE(lun_standard,*)'  -   EPSILON(rprec):', EPSILON(1.0_rprec)

  IF (qprec > 8 ) THEN
    WRITE(lun_standard,*)''
    WRITE(lun_standard,*)'-- QUADRUPLE PRECISION IS ACTIVATED --'
    WRITE(lun_standard,*)'        quad value = ', qprec
  ENDIF

  !------------------------------!
  !- Open Ariane memory log file-!
  !------------------------------!
  CALL sub_open_memory_file()

  !======================!
  !- READ NAMELIST FILE -!
  !======================!
  CALL sub_read_namelist(.TRUE.)

  IF (TRIM(mode)== 'quantitative') THEN
    CALL sub_print_criters()
  ENDIF

  IF (key_sequential) THEN
    CALL trajec_seq()
  ELSE
    CALL trajec()
  ENDIF

  !------------------------------!
  !- Close Ariane memory log file-!
  !------------------------------!
  CALL sub_close_memory_file()

  ! END !
  WRITE(lun_standard,*)''
  WRITE(lun_standard,*)'====================================================='
  WRITE(lun_standard,*)'=             -o0)  ARIANE Finish  (0o-             ='
  WRITE(lun_standard,*)'====================================================='
  WRITE(lun_standard,*)''
  WRITE(lun_standard,*)' If Ariane results are used in your publication(s),'
  WRITE(lun_standard,*)'please feel free to reference Ariane and to send us '
  WRITE(lun_standard,*)'       your publication references (thanks).'
  WRITE(lun_standard,*)''
  WRITE(lun_standard,*)'        http://www.univ-brest.fr/lpo/ariane'

END PROGRAM prg_ariane
