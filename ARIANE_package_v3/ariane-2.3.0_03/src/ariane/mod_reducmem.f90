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
!!****h* ariane/mod_reducmem
!! NAME
!!   mod_reducmem (mod_reducmem.f90 - Fortran90 module)
!!
!! USAGE
!!   Include 'USE mod_reducmem' in the header of your Fortran 90 source 
!!   code.
!!   Then you'll have access to the subroutine:
!!      - sub_reducmem_shift_or_not_ind
!!
!! FUNCTION
!!   
!! 
!! AUTHOR
!!   * Origin  : Nicolas Grima (October 2005)
!! 
!! CREATION DATE
!!   * October 2005
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
!!
!! SEE ALSO
!!   
!!
!! USES
!!   * USE mod_precision
!!
!! USED BY
!!   * trajec
!!
!! SOURCE
!!=========================================================================
MODULE mod_reducmem

  !------------------!
  ! USE ASSOCIAITION !
  !------------------!
  USE mod_precision
  USE mod_namelist
  USE mod_lun

  !-------------!
  ! DECLARATION !
  !-------------!
  IMPLICIT NONE
  INTEGER(kind = iprec) ::  imt_reg_start, imt_reg_end, imt_reg
  INTEGER(kind = iprec) ::  jmt_reg_start, jmt_reg_end, jmt_reg
  INTEGER(kind = iprec) ::  kmt_reg_start, kmt_reg_end, kmt_reg


  INTEGER(kind = iprec), DIMENSION(4,3) :: dims_reg

  !====================================!
  !=           dims_reg(4,3)          =!
  !=                                  =!
  !=       start    end    size       =!
  !=     +-------+-------+------+     =!
  != imt |       |       |      |     =!
  !=     +-------+-------+------+     =!
  != jmt |       |       |      |     =!
  !=     +-------+-------+------+     =!
  != kmt |       |       |      |     =!
  !=     +-------+-------+------+     =!
  != lmt |       |       |      |     =!
  !=     +-------+-------+------+     =!
  !=                                  =!
  !====================================!

CONTAINS
  !!***
  !=========================================================================
  !!****f* mod_reducmem/sub_read_reg_lim()
  !! NAME
  !!   sub_read_reg_lim()
  !!
  !! FUNCTION
  !!   
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima (October 2005)
  !! 
  !! CREATION DATE
  !!   * October 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * Outputs:
  !!
  !! TODO
  !!
  !! USED BY
  !!   * trajec
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_reducmem_read_reg_lim()

    IF ( ((TRIM(mode) == 'quantitative') .AND. key_reducmem) .OR. &
         ((TRIM(mode) == 'qualitative')  .AND. key_region)  ) THEN

      OPEN(UNIT=lun_reg, file='region_limits', form='FORMATTED')

      READ(lun_reg,*)imt_reg_start, imt_reg_end, imt_reg

      READ(lun_reg,*)jmt_reg_start, jmt_reg_end, jmt_reg

      READ(lun_reg,*)kmt_reg_start, kmt_reg_end, kmt_reg

      CLOSE(lun_reg)

      WRITE(lun_standard,*)''
      WRITE(lun_standard,*)'================='
      WRITE(lun_standard,*)'= Region Limits ='
      WRITE(lun_standard,*)'================='
      WRITE(lun_standard,*)'(should be the same than in the region_limits file)'
      WRITE(lun_standard,*)'  - in x : ', imt_reg_start, imt_reg_end, imt_reg
      WRITE(lun_standard,*)'  - in y : ', jmt_reg_start, jmt_reg_end, jmt_reg
      WRITE(lun_standard,*)'  - in z : ', kmt_reg_start, kmt_reg_end, kmt_reg


      IF ( (imt_reg_end > imt_reg_start).AND.((imt_reg_start + imt_reg - 1) > imt) ) THEN
        WRITE(lun_error, *) ''
        WRITE(lun_error, *) '---ERROR---'
        WRITE(lun_error, *) 'mod_reducmem: region limits in x are wrong!'
        WRITE(lun_error, *) 'Please contact Ariane team to correct this.'
        WRITE(lun_error, *) 'http://www.univ-brest.fr/lpo/ariane'
        STOP
      ENDIF

      IF ( (jmt_reg_end > jmt_reg_start).AND.((jmt_reg_start + jmt_reg - 1) > jmt) ) THEN
        WRITE(lun_error, *) ''
        WRITE(lun_error, *) '---ERROR---'
        WRITE(lun_error, *) 'mod_reducmem: region limits in y are wrong!'
        WRITE(lun_error, *) 'Please contact Ariane team to correct this.'
        WRITE(lun_error, *) 'http://www.univ-brest.fr/lpo/ariane'
        STOP
      ENDIF

      IF ( (kmt_reg_end > kmt_reg_start).AND.((kmt_reg_start + kmt_reg - 1) > kmt) ) THEN
        WRITE(lun_error, *) ''
        WRITE(lun_error, *) '---ERROR---'
        WRITE(lun_error, *) 'mod_reducmem: region limits in k are wrong!'
        WRITE(lun_error, *) 'Please contact Ariane team to correct this.'
        WRITE(lun_error, *) 'http://www.univ-brest.fr/lpo/ariane'
        STOP
      ENDIF

      IF ( (imt_reg_end > imt_reg_start).AND.((imt_reg_end-imt_reg_start+1) /= imt_reg) ) THEN
        WRITE(lun_error, *) ''
        WRITE(lun_error, *) '---ERROR---'
        WRITE(lun_error, *) 'mod_reducmem: region limits in x are wrong!'
        WRITE(lun_error, *) 'Please verify region_limits file.'
        STOP
      ENDIF

      IF ( (jmt_reg_end > jmt_reg_start).AND.((jmt_reg_end-jmt_reg_start+1) /= jmt_reg) ) THEN
        WRITE(lun_error, *) ''
        WRITE(lun_error, *) '---ERROR---'
        WRITE(lun_error, *) 'mod_reducmem: region limits in y are wrong!'
        WRITE(lun_error, *) 'Please verify region_limits file.'
        STOP
      ENDIF

      IF ( (kmt_reg_end > kmt_reg_start).AND.((kmt_reg_end-kmt_reg_start + 1) /= kmt_reg) ) THEN
        WRITE(lun_error, *) ''
        WRITE(lun_error, *) '---ERROR---'
        WRITE(lun_error, *) 'mod_reducmem: region limits in k are wrong!'
        WRITE(lun_error, *) 'Please verify region_limits file.'
        STOP
      ENDIF


    ELSE

      imt_reg_start = 1
      imt_reg_end   = imt
      imt_reg       = imt_reg_end - imt_reg_start + 1

      jmt_reg_start = 1
      jmt_reg_end   = jmt
      jmt_reg       = jmt_reg_end - jmt_reg_start + 1

      kmt_reg_start = 1
      IF (key_add_bottom) THEN
        kmt_reg_end   = kmt - 1
      ELSE
        kmt_reg_end   = kmt
      ENDIF
      kmt_reg       = kmt_reg_end - kmt_reg_start + 1

    ENDIF

    dims_reg(1,1) = imt_reg_start
    dims_reg(1,2) = imt_reg_end
    dims_reg(1,3) = imt_reg

    dims_reg(2,1) = jmt_reg_start
    dims_reg(2,2) = jmt_reg_end
    dims_reg(2,3) = jmt_reg

    dims_reg(3,1) = kmt_reg_start
    dims_reg(3,2) = kmt_reg_end
    dims_reg(3,3) = kmt_reg

    dims_reg(4,1) = 1
    dims_reg(4,2) = lmt
    dims_reg(4,3) = lmt

  END SUBROUTINE sub_reducmem_read_reg_lim
  !!***

  !=========================================================================
  !!****f* mod_reducmem/sub_reducmem_shift_or_not_ind()
  !! NAME
  !!   sub_reducmem_shift_or_not_ind()
  !!
  !! FUNCTION
  !!   
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima (October 2005)
  !! 
  !! CREATION DATE
  !!   * October 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * Outputs:
  !!
  !! TODO
  !!
  !! USED BY
  !!   * trajec
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_reducmem_shift_or_not_ind(iin, jin, kin, iout, jout, kout)

    INTEGER(kind = iprec), INTENT(IN)  :: iin , jin , kin
    INTEGER(kind = iprec), INTENT(OUT) :: iout, jout, kout

    IF (key_reducmem.OR.key_region) THEN

      iout = MOD(iin + imt - imt_reg_start + 1, imt)

      IF ( iout == 0 ) THEN
        iout = imt
      ENDIF

      !!NG      IF ( (iout > imt_reg).OR.(iout < 1)) THEN
      !!NG        WRITE(lun_error,*)
      !!NG        WRITE(lun_error,*)' reducmem ERROR: iin =',  iin, 'iout =', iout
      !!NG        WRITE(lun_error,*)'                 imt =', imt
      !!NG        WRITE(lun_error,*)'       imt_reg_start =', imt_reg_start
      !!NG        WRITE(lun_error,*)'                 jin =',  jin
      !!NG        WRITE(lun_error,*)'                 kin =',  kin
      !!NG        STOP
      !!NG      ENDIF
      jout = MAX(jin - jmt_reg_start + 1, 1)
      !!NG      IF ( (jout > jmt_reg).OR.(jout < 1) ) THEN
      !!NG        WRITE(lun_error,*)' reducmem ERROR: jin =',  jin, 'jout =', jout
      !!NG        STOP
      !!NG      ENDIF
      kout = MAX(kin - kmt_reg_start + 1, 1)
      !!NG      IF ( (kout > kmt_reg).OR.(kout < 1) ) THEN
      !!NG        WRITE(lun_error,*)' reducmem ERROR: kin =',  kin, 'kout =', kout
      !!NG        STOP
      !!NG      ENDIF

    ELSE

      iout = iin
      jout = jin
      kout = kin

    ENDIF

  END SUBROUTINE sub_reducmem_shift_or_not_ind
  !!***
END MODULE mod_reducmem
