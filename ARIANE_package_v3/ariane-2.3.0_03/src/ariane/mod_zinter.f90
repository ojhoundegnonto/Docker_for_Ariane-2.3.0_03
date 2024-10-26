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
!!****h* ariane/mod_zinter
!! NAME
!!   mod_zinter (mod_zinter.f90 - Fortran90 module)
!!
!! USAGE
!!   Include 'USE mod_zinter' in the header of your Fortran 90 source 
!!   code.
!!   Then you'll have access to the function:
!!      - zinter
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
!!
!! SEE ALSO
!!   
!!
!! USES
!!   * USE mod_precision
!!   * USE mod_namelist
!!   * USE mod_input_grid
!!
!! USED BY
!!   * mod_quali.f90
!!   * mod_quant.f90
!!   * posini.f90
!!   * trajec.f90
!!
!! SOURCE
!!=========================================================================
MODULE mod_zinter

  !-----------------!
  ! USE ASSOCIATION !
  !-----------------!
  USE mod_precision
  USE mod_namelist
  USE mod_input_grid
  USE mod_reducmem

  !-------------!
  ! DECLARATION !
  !-------------!
  IMPLICIT NONE

CONTAINS
  !!***
  !=========================================================================
  !!****f* mod_zinter/zinter()
  !! NAME
  !!   zinter()
  !!
  !! FUNCTION
  !!   Compute vertical interpolation.
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
  !!      - tab:
  !!      - hi :
  !!      - hj :
  !!      - hk :
  !!      - hl :
  !!
  !!   * OUTPUT:
  !!      - zinter:
  !!
  !! TODO
  !!   * Should be migrate to Fortran 90 !!!!
  !!   * more comments !
  !!
  !! USED BY
  !!   * (mod_criter0.f90) ?
  !!   * (mod_criter1.f90) ?
  !!   * (mod_criter2.f90) ?
  !!   * mod_quant.f90
  !!   * posini.f90
  !!   * trajec.f90
  !!
  !! SOURCE
  !!=======================================================================
  FUNCTION zinter(tab,hi,hj,hk,hl)

    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    !!NG:    REAL(kind=rprec), DIMENSION(dims_reg(1,3),dims_reg(2,3),dims_reg(3,3),it_ind), INTENT(in) :: tab
    REAL(kind=rprec), DIMENSION(:,:,:,:), INTENT(in) :: tab
    REAL(kind=rprec), INTENT(in) :: hi,hj,hk,hl
    REAL(kind=rprec) :: zinter

    !- local variables -! 
    INTEGER(kind=iprec) :: i , j , k
    INTEGER(kind=iprec) :: is, js, ks
    INTEGER(kind=iprec) :: l1 !
    INTEGER(kind=iprec) :: l2 !

    REAL(kind=rprec) :: h       !
    REAL(kind=rprec) :: x       !
    REAL(kind=rprec) :: y       !
    REAL(kind=rprec) :: z       !
    REAL(kind=rprec) :: coef    !
    REAL(kind=rprec) :: trilin1 !
    REAL(kind=rprec) :: trilin2 !
    REAL(kind=rprec) :: t       !

    !-------------!
    ! Code begins !
    !-------------!
    i=INT(hi+.5_rprec)
    j=INT(hj+.5_rprec)
    k=INT(-hk-.5_rprec)

    h=hl
100 IF (h.LT.0.5_rprec) THEN
      h=h+lmt
      GOTO 100
    ENDIF

    t=1._rprec -( h - REAL(INT(h), kind = rprec))

    !! NG: modification 03_03_2008
    !! NG: to fix a bug when this routine is called by criterx
    !! NG: with hl different of 1._rprec.
    IF (key_sequential.AND.(.NOT.key_interp_temporal)) THEN
      l1=1
    ELSE
      l1=MOD(INT(h),lmt)
    ENDIF

    l2=l1+1

    IF (l1.EQ.0) l1=lmt
    IF (l2.EQ.-1) l2=lmt-1
    IF (l2.EQ.0) l2=lmt

    x=-hi+.5_rprec+REAL(i, kind = rprec)
    y=-hj+.5_rprec+REAL(j, kind = rprec)
    z=hk+1.5_rprec+REAL(k, kind = rprec)

    IF (key_periodic) THEN
      IF (i.GE.(imt-1)) i=i-(imt-2)
    ELSE
      IF (i.LT.1) THEN
        i=1
        x=1._rprec
      ENDIF

      IF (i.GT.(imt-1)) THEN
        i=imt-1
        x=0._rprec
      ENDIF
    ENDIF

    IF (j.LT.1) THEN
      j=1
      y=1._rprec
    ENDIF

    IF (j.GT.(jmt-1)) THEN
      j=jmt-1
      y=0._rprec
    ENDIF

    IF (k.LT.1) THEN
      k=1
      z=1._rprec
    ENDIF

    IF (k.GT.(kmt-1)) THEN
      k=kmt-1
      z=0._rprec
    ENDIF

    CALL sub_reducmem_shift_or_not_ind(i,j,k,is,js,ks)

    coef=x            *  y           * z            * tmask(is  ,js  ,ks  ,1) + &
         x            *  y           * (1._rprec-z) * tmask(is  ,js  ,ks+1,1) + &
         x            * (1._rprec-y) * z            * tmask(is  ,js+1,ks  ,1) + &
         x            * (1._rprec-y) * (1._rprec-z) * tmask(is  ,js+1,ks+1,1) + &
         (1._rprec-x) *  y           * z            * tmask(is+1,js  ,ks  ,1) + &
         (1._rprec-x) *  y           * (1._rprec-z) * tmask(is+1,js  ,ks+1,1) + &
         (1._rprec-x) * (1._rprec-y) * z            * tmask(is+1,js+1,ks  ,1) + &
         (1._rprec-x) * (1._rprec-y) * (1._rprec-z) * tmask(is+1,js+1,ks+1,1)

    trilin1= &
         x            *  y           *  z          * tab(is  ,js  ,ks  ,l1)*tmask(is  ,js  ,ks  ,1) + &            
         x            *  y           *(1._rprec-z) * tab(is  ,js  ,ks+1,l1)*tmask(is  ,js  ,ks+1,1) + &
         x            * (1._rprec-y) *  z          * tab(is  ,js+1,ks  ,l1)*tmask(is  ,js+1,ks  ,1) + &
         x            * (1._rprec-y) *(1._rprec-z) * tab(is  ,js+1,ks+1,l1)*tmask(is  ,js+1,ks+1,1) + &
         (1._rprec-x) *  y           *  z          * tab(is+1,js  ,ks  ,l1)*tmask(is+1,js  ,ks  ,1) + &
         (1._rprec-x) *  y           *(1._rprec-z) * tab(is+1,js  ,ks+1,l1)*tmask(is+1,js  ,ks+1,1) + &
         (1._rprec-x) * (1._rprec-y) *  z          * tab(is+1,js+1, ks ,l1)*tmask(is+1,js+1,ks  ,1) + &
         (1._rprec-x) * (1._rprec-y) *(1._rprec-z) * tab(is+1,js+1,ks+1,l1)*tmask(is+1,js+1,ks+1,1)


    IF (key_sequential.AND.(.NOT.key_interp_temporal)) THEN

      zinter = trilin1 / coef

    ELSE

      trilin2= &
           x            *  y           *  z           * tab(is  ,js  ,ks  ,l2)*tmask(is  ,js  ,ks  ,1) + &
           x            *  y           * (1._rprec-z) * tab(is  ,js  ,ks+1,l2)*tmask(is  ,js  ,ks+1,1) + &
           x            * (1._rprec-y) *  z           * tab(is  ,js+1,ks  ,l2)*tmask(is  ,js+1,ks  ,1) + &
           x            * (1._rprec-y) * (1._rprec-z) * tab(is  ,js+1,ks+1,l2)*tmask(is  ,js+1,ks+1,1) + &
           (1._rprec-x) *  y           *  z           * tab(is+1,js  ,ks  ,l2)*tmask(is+1,js  ,ks  ,1) + &
           (1._rprec-x) *  y           * (1._rprec-z) * tab(is+1,js  ,ks+1,l2)*tmask(is+1,js  ,ks+1,1) + &
           (1._rprec-x) * (1._rprec-y) *  z           * tab(is+1,js+1,ks  ,l2)*tmask(is+1,js+1,ks  ,1) + &
           (1._rprec-x) * (1._rprec-y) * (1._rprec-z) * tab(is+1,js+1,ks+1,l2)*tmask(is+1,js+1,ks+1,1)

      zinter=(t*trilin1+(1._rprec-t)*trilin2)/coef

    ENDIF

    RETURN

  END FUNCTION zinter
  !!***
END MODULE mod_zinter
