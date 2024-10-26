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
MODULE mod_region_ind

  USE mod_precision

  IMPLICIT NONE


CONTAINS
  !----------------
  !----------------
  SUBROUTINE sub_comput_lim(ispr, i_shift, imt, jmt, reg1, reg2, length)
    INTEGER(kind= iprec), INTENT(IN) :: i_shift, imt, jmt
    INTEGER(kind= iprec), INTENT(OUT):: reg1, reg2, length
    INTEGER(kind= iprec), DIMENSION(:,:), INTENT(IN) :: ispr


    INTEGER(kind= iprec), DIMENSION(:,:), ALLOCATABLE :: mat
    INTEGER(kind= iprec), DIMENSION(:)  , ALLOCATABLE :: rep
    INTEGER(kind=iprec) :: i,j, ind, ind_lim, cdim, imv1, imv2


    IF (.NOT.ALLOCATED(mat)) ALLOCATE(mat(imt,jmt))

    IF (I_SHIFT == 0) STOP

    IF (I_SHIFT > 0) THEN
      mat(:,:)=RESHAPE( source=(/ ((i,i=1,imt),j=1,jmt) /), &
           shape= (/ imt, jmt /) )
      IF (.NOT.ALLOCATED(rep)) Allocate(rep(imt))
      ind_lim=imt
      cdim=1
    ELSE
      mat(:,:)=RESHAPE( source=(/ ((j,i=1,imt),j=jmt,1,-1) /), &
           shape= (/ imt, jmt /) )
      IF (.NOT.ALLOCATED(rep)) Allocate(rep(jmt))
      ind_lim=jmt
      cdim=2
    ENDIF

    ! WRITE(*,*) SHAPE(rep)

    DO ind = 0, ind_lim-1
      rep(ind+1)= MINVAL( CSHIFT(ispr(:,:),i_shift*ind,cdim) * mat(:,:), &
           mask=CSHIFT(ispr(:,:),i_shift*ind,cdim)>0)

    ENDDO

    !!write(*,*)
    !!write(*,*) rep
    !!write(*,*)

    reg2 = MAXLOC(rep(:), dim=1) - 1

    IF (reg2 == 0) reg2 = ind_lim 

    reg1 = MOD( MAXVAL(rep(:)) + reg2, ind_lim)

    length = mod(ind_lim - reg1 + reg2 + 1, ind_lim)

    IF ( length == 0) length = ind_lim 

    !! We add 1 point in each direction
    IF (reg1 == 1 ) THEN
      imv1 = 0
    ELSE
      imv1 = 1
      reg1 = reg1 - imv1
      
    ENDIF

    IF (reg2 > ind_lim ) reg2 = ind_lim

    IF (reg2 == ind_lim ) THEN
      imv2 = 0
    ELSE
      imv2 = 1
      reg2 = reg2 + imv2
    ENDIF

    length = length + imv1 + imv2

    IF (ALLOCATED(rep)) DEAllocate(rep)

  END SUBROUTINE sub_comput_lim

END MODULE mod_region_ind
