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
PROGRAM mkseg
  !
  ! automate the indexing of the segments used by ARIANE in quantitative
  !   diagnostics
  !
  ! --- copyright: Bruno Blanke
  !
  ! first version: February 1999
  ! this version: December 2003
  !
  ! #include "parameter.h"

  USE mod_precision
  USE mod_configure
  USE mod_namelist
  USE mod_netcdf
  USE mod_input_grid
  USE mod_region_ind

  IMPLICIT NONE

  !- Change this value if it is to short - NG January 2006 -!
  INTEGER(kind = iprec), PARAMETER :: maxsegm = 512
  !
  INTEGER(kind = iprec) :: i, j, ii, jj, imin, imax, i0, j0, ihot, itemp, iter, jmax
  INTEGER(kind = iprec) :: nsegm, nspr, maxsect0, maxtrsp, n, iflag
  INTEGER(kind = iprec), DIMENSION(:)    , ALLOCATABLE :: or, it1, it2, jt1, jt2, nsec
  INTEGER(kind = iprec), DIMENSION(:,:)  , ALLOCATABLE :: val, ispr
  INTEGER(kind = iprec), DIMENSION(:)    , ALLOCATABLE :: ibeg,iend,idel,jbeg,jend,jdel
  INTEGER(kind = iprec), DIMENSION(:,:,:), ALLOCATABLE :: sval
  INTEGER(kind=iprec) :: reg_west, reg_east, reg_south, reg_north, ilength, jlength
  INTEGER(kind=iprec) :: abs_reg_west, abs_reg_east, abs_reg_south, abs_reg_north
  !
  CHARACTER(len = 1), DIMENSION(:,:), ALLOCATABLE ::  val0, val1, flag
  !
  LOGICAL :: l_esc
  !
  !-----------!
  ! MAIN PART !
  !-----------!
  ! Print mkseg version !
  WRITE(lun_standard,*)''
  WRITE(lun_standard,*)'====================================================='
  WRITE(lun_standard,*)'=              -o0)  MKSEG v'//TRIM(VERSION)//'  (0o-             ='
  WRITE(lun_standard,*)'====================================================='

  !----------------------!
  !- READ NAMELIST FILE -!
  !----------------------!
  CALL sub_read_namelist()

  !----------------------------------------!
  ! Dynamic allocation and initialisations !
  !----------------------------------------!
  ALLOCATE(val0(imt,jmt))
  !!  val0(:,:)='~'

  ALLOCATE(val1(imt,jmt))
  !!  val1(:,:)='^'

  ALLOCATE(or(maxsegm))
  or(:)=0
  ALLOCATE(it1(maxsegm))
  !!  it1(:)=0
  ALLOCATE(it2(maxsegm))
  !!  it2(:)=0
  ALLOCATE(jt1(maxsegm))
  !!  jt1(:)=0
  ALLOCATE(jt2(maxsegm))
  !!  jt2(:)=0
  ALLOCATE(nsec(maxsegm))
  !!  nsec(:)=0
  ALLOCATE(val(imt,jmt))
  !!  val(:,:)=0
  ALLOCATE(ispr(imt,jmt))
  !!  ispr(:,:)=0
  ALLOCATE(flag(imt,jmt))
  !!  flag(:,:)='+'
  ALLOCATE(sval(imt,jmt,2))
  !!  sval(:,:,:)=0
  ALLOCATE(ibeg(4))
  ALLOCATE(iend(4))
  ALLOCATE(idel(4))
  ALLOCATE(jbeg(4))
  ALLOCATE(jend(4))
  ALLOCATE(jdel(4))
  !-------------------!
  !- READ INPUT MESH -!
  !-------------------!---------------------------------------------------!
  !-- Allocate and read coordinates and scale factors 
  !-- xx_tt, xx_uu, xx_vv, xx_ff
  !-- yy_tt, yy_uu, yy_vv, yy_ff
  !-- zz_tt, zz_ww
  !-- e2u
  !-- e1v
  !-- e1t, e2t, e3t
  !-- tmask
  !-----------------------------------------------------------------------!
  key_reducmem=.FALSE.
  CALL sub_reducmem_read_reg_lim()

  IF (key_roms) THEN
    CALL sub_input_grid()
  ELSEIF (key_mars) THEN
    CALL sub_input_grid()
  ELSEIF (key_symphonie) THEN
    CALL sub_input_grid()
  ELSE !! OPA
    CALL sub_input_tmask_surf_opa()
  ENDIF

  !-----------------------------------------------------------------------!
  DO j=1,jmt
    DO i=1,imt
      IF (tmask(i,j,1,1).LE.0.5) val0(i,j)='#'
      IF (tmask(i,j,1,1).GT.0.5) val0(i,j)='-'
    END DO
  END DO


  !-----------------------------------------------------------------------!
  OPEN(30,file='segrid',form='FORMATTED')
  !
  DO j=jmt,1,-1
    READ(30,'(5000a1)')(val1(i,j),i=1,imt)
  END DO
  !
  IF (key_periodic) THEN
    DO j=1,jmt
      IF (val1(1,j).NE.val1(imt-1,j)) THEN
        WRITE(lun_error,*)' '
        WRITE(lun_error,*)'ouch ! periodicity is not respected...',imt-1,j
        val1(imt-1,j)='*'
        GOTO 9999
      ENDIF
      IF (val1(2,j).NE.val1(imt,j)) THEN
        WRITE(lun_error,*)' '
        WRITE(lun_error,*)'ouch ! periodicity is not respected...',2,j
        val1(2,j)='*'
        GOTO 9999
      ENDIF
    END DO
  ENDIF

  IF (key_jfold) THEN
    IF (TRIM(pivot) == 'T' ) THEN !!NG 8 march 2013
      DO i=2,imt
        IF (val1(i,jmt-2).NE.val1(imt+2-i,jmt)) THEN
          WRITE(lun_error,*)' '
          WRITE(lun_error,*)'1 ouch ! j folding is not respected...',i,jmt-2
          write(*,*)val1(i,jmt-2),val1(imt+2-i,jmt)
          val1(i,jmt-2)='*'
          val1(imt+2-i,jmt)='*'
          GOTO 9999
        ENDIF
      END DO
      DO i=2,imt/2
        IF (val1(i,jmt-1).NE.val1(imt+2-i,jmt-1)) THEN
          WRITE(lun_error,*)' '
          WRITE(lun_error,*)'2 ouch ! j folding is not respected...',i,jmt-1
          val1(i,jmt-1)='*'
          GOTO 9999
        ENDIF
      END DO
    ELSE !!NG 8 march 2013 F pivot
      DO i=1,imt
        IF (val1(i,jmt-1).NE.val1(imt+1-i,jmt)) THEN
          WRITE(lun_error,*)' '
          WRITE(lun_error,*)'ouch ! j folding is not respected...',i,jmt-1
          val1(i,jmt-1)='*'
          GOTO 9999
        ENDIF
      END DO
    ENDIF

  ENDIF

  !-----------------------------------------------------------------------!
  !
  imin=1
  imax=imt
  jmax=jmt
  IF (key_periodic) THEN
    imin=2
    imax=imt-1
  ENDIF
  IF (key_jfold) THEN
    jmax=jmt-1
  ENDIF
  !
  i0=0
  j0=0
  !
  ihot=0
  !
  DO j=1,jmax
    DO i=imin,imax
      val(i,j)=999
      IF ((val0(i,j).EQ.'#').AND.(val1(i,j).NE.'#')) THEN
        WRITE(lun_error,*)' '
        WRITE(lun_error,*)'ouch ! a land gridcell has been deleted... ',i,j
        val1(i,j)='*'
        GOTO 9999
      ENDIF
      IF ((val0(i,j).NE.'#').AND.(val1(i,j).EQ.'#')) THEN
        WRITE(lun_error,*)' '
        WRITE(lun_error,*)'ouch ! a land gridcell has been added... ',i,j
        val1(i,j)='*'
        GOTO 9999
      ENDIF
      IF (val1(i,j).EQ.'@') THEN
        ihot=ihot+1
        val(i,j)=0
        i0=i
        j0=j
        WRITE(lun_standard,*) 'hot point (@): ', i, j
      ENDIF
      IF (val1(i,j).EQ.'#') val(i,j)=-99
      IF ((val1(i,j).EQ.'-').OR.(val1(i,j).EQ.':').OR. &
           (val1(i,j).EQ.'o').OR.(val1(i,j).EQ.'+')) val(i,j)=0
      IF (val1(i,j).EQ.'1') val(i,j)=1
      IF (val1(i,j).EQ.'2') val(i,j)=2
      IF (val1(i,j).EQ.'3') val(i,j)=3
      IF (val1(i,j).EQ.'4') val(i,j)=4
      IF (val1(i,j).EQ.'5') val(i,j)=5
      IF (val1(i,j).EQ.'6') val(i,j)=6
      IF (val1(i,j).EQ.'7') val(i,j)=7
      IF (val1(i,j).EQ.'8') val(i,j)=8
      IF (val1(i,j).EQ.'9') val(i,j)=9
      IF (val1(i,j).EQ.'a') val(i,j)=-1
      IF (val1(i,j).EQ.'b') val(i,j)=-2
      IF (val1(i,j).EQ.'c') val(i,j)=-3
      IF (val1(i,j).EQ.'d') val(i,j)=-4
      IF (val1(i,j).EQ.'e') val(i,j)=-5
      IF (val1(i,j).EQ.'f') val(i,j)=-6
      IF (val1(i,j).EQ.'g') val(i,j)=-7
      IF (val1(i,j).EQ.'h') val(i,j)=-8
      IF (val1(i,j).EQ.'i') val(i,j)=-9
      IF (val(i,j).EQ.999) THEN
        WRITE(lun_error,*)' '
        WRITE(lun_error,*)'ouch ! unknown marker...',i,j
        val1(i,j)='*'
        GOTO 9999
      ENDIF
    END DO
  END DO
  !
  IF (ihot.EQ.0) THEN
    WRITE(lun_error,*)' '
    WRITE(lun_error,*)'no hot point specified (symbol @)...'
    WRITE(lun_error,*)'segments will NOT be tested or oriented'
  ENDIF
  IF (ihot.GT.1) THEN
    WRITE(lun_error,*)' '
    WRITE(lun_error,*)'only one hot point should be specified (symbol @)...'
    WRITE(lun_error,*)'segments will NOT be tested or oriented'
  ENDIF
  !!NG: 8 march 2013  IF ((ihot.EQ.1).AND.(key_jfold)) THEN
  !!NG: 8 march 2013      IF (TRIM(pivot) /= 'T') THEN
  !!NG: 8 march 2013         WRITE(lun_error,*)' '
  !!NG: 8 march 2013         WRITE(lun_error,*)'j_folding only implemented for T pivot...'
  !!NG: 8 march 2013         WRITE(lun_error,*)'segments will NOT be tested or oriented'
  !!NG: 8 march 2013         ihot=0
  !!NG: 8 march 2013         i0=0
  !!NG: 8 march 2013         j0=0
  !!NG: 8 march 2013      ENDIF
  !!NG: 8 march 2013   ENDIF
  !       
  nsegm=1
  !
  DO j=1,jmax
    ii=0
    DO i=imin,imax-1
      IF ((val0(i,j).NE.'#').AND.(i.GE.ii)) THEN
        IF ((ABS(val(i,j)).GT.0).AND.(val(i+1,j).EQ.val(i,j))) THEN
          nsec(nsegm)=val(i,j)
          it1(nsegm)=i
          sval(i,j,1)=nsegm
          jt1(nsegm)=j
          jt2(nsegm)=j
          DO ii=i+1,imax
            IF (val(ii,j).EQ.val(i,j)) THEN
              it2(nsegm)=ii
              sval(ii,j,1)=nsegm
            ELSE
              GOTO 111
            ENDIF
          END DO
111       nsegm=nsegm+1
        ENDIF
      ENDIF
    END DO
  END DO
  !
  DO i=imin,imax
    jj=0
    DO j=1,jmax-1
      IF ((val0(i,j).NE.'#').AND.(j.GE.jj)) THEN
        IF ((ABS(val(i,j)).GT.0).AND.(val(i,j+1).EQ.val(i,j))) THEN
          nsec(nsegm)=val(i,j)
          jt1(nsegm)=j
          sval(i,j,2)=nsegm
          it1(nsegm)=i
          it2(nsegm)=i
          DO jj=j+1,jmax
            IF (val(i,jj).EQ.val(i,j)) THEN
              jt2(nsegm)=jj
              sval(i,jj,2)=nsegm
            ELSE
              GOTO 222
            ENDIF
          END DO
222       nsegm=nsegm+1
        ENDIF
      ENDIF
    END DO
  END DO
  !
  DO j=1,jmax
    DO i=imin,imax
      IF ((val(i,j).GT.-90).AND.(ABS(val(i,j)).GT.0).AND. &
           (sval(i,j,1).EQ.0).AND.(sval(i,j,2).EQ.0)) THEN
        WRITE(lun_error,*)' '
        WRITE(lun_error,*)'ouch ! unformed segment...',i,j
        val1(i,j)='*'
        GOTO 9999
      ENDIF
    END DO
  END DO
  !
  nsegm=nsegm-1
  !
  IF (ihot.NE.1) GOTO 2222
  !
  ispr(i0,j0)=1
  !
  ibeg(1)=imin
  iend(1)=imax
  idel(1)=1
  jbeg(1)=1
  jend(1)=jmax
  jdel(1)=1
  !
  ibeg(2)=imax
  iend(2)=imin
  idel(2)=-1
  jbeg(2)=1
  jend(2)=jmax
  jdel(2)=1
  !
  ibeg(3)=imax
  iend(3)=imin
  idel(3)=-1
  jbeg(3)=jmax
  jend(3)=1
  jdel(3)=-1
  !
  ibeg(4)=imin
  iend(4)=imax
  idel(4)=1
  jbeg(4)=jmax
  jend(4)=1
  jdel(4)=-1
  !
  iter=0

777 nspr=0
  !
  iter=iter+1
  IF (iter.GT.4) iter=1
  !
  DO j=jbeg(iter),jend(iter),jdel(iter)
    DO i=ibeg(iter),iend(iter),idel(iter)
      IF (ispr(i,j).EQ.1) THEN
        IF (i.GT.1) THEN
          IF ((ispr(i-1,j).EQ.0).AND. &
               (val(i-1,j).GE.-90).AND.(val(i-1,j).LE.0)) THEN
            ispr(i-1,j)=1
            nspr=nspr+1
          ELSE
            IF (val(i-1,j).GT.0) THEN
              IF (sval(i-1,j,2).EQ.0) THEN
                IF (val(i-1,j).EQ.1) THEN
                  WRITE(lun_error,*)' '
                  WRITE(lun_error,*)'ouch ! bad attack (i-1,j)...',i-1,j
                  val1(i-1,j)='*'
                  GOTO 9999
                ENDIF
              ELSE
                IF (or(sval(i-1,j,2)).EQ.0) THEN
                  or(sval(i-1,j,2))=1
                ELSE
                  IF (or(sval(i-1,j,2)).NE.1) THEN
                    WRITE(lun_error,*)' '
                    WRITE(lun_error,*)'ouch ! double orientation (i-1,j)...',i-1,j
                    val1(i-1,j)='*'
                    GOTO 9999
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        IF (i.LT.imt) THEN
          IF ((ispr(i+1,j).EQ.0).AND. &
               (val(i+1,j).GE.-90).AND.(val(i+1,j).LE.0)) THEN
            ispr(i+1,j)=1
            nspr=nspr+1
          ELSE
            IF (val(i+1,j).GT.0) THEN
              IF (sval(i+1,j,2).EQ.0) THEN
                IF (val(i+1,j).EQ.1) THEN
                  WRITE(lun_error,*)' '
                  WRITE(lun_error,*)'ouch ! bad attack (i+1,j)...',i+1,j
                  val1(i+1,j)='*'
                  GOTO 9999
                ENDIF
              ELSE
                IF (or(sval(i+1,j,2)).EQ.0) THEN
                  or(sval(i+1,j,2))=-1
                ELSE
                  IF (or(sval(i+1,j,2)).NE.-1) THEN
                    WRITE(lun_error,*)' '
                    WRITE(lun_error,*)'ouch ! double orientation (i+1,j)...',i+1,j
                    val1(i+1,j)='*'
                    GOTO 9999
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        IF (j.GT.1) THEN
          IF ((ispr(i,j-1).EQ.0).AND. &
               (val(i,j-1).GE.-90).AND.(val(i,j-1).LE.0)) THEN
            ispr(i,j-1)=1
            nspr=nspr+1
          ELSE
            IF (val(i,j-1).GT.0) THEN
              IF (sval(i,j-1,1).EQ.0) THEN
                IF (val(i,j-1).EQ.1) THEN
                  WRITE(lun_error,*)' '
                  WRITE(lun_error,*)'ouch ! bad attack (i,j-1)...',i,j-1
                  val1(i,j-1)='*'
                  GOTO 9999
                ENDIF
              ELSE
                IF (or(sval(i,j-1,1)).EQ.0) THEN
                  or(sval(i,j-1,1))=1
                ELSE
                  IF (or(sval(i,j-1,1)).NE.1) THEN
                    WRITE(lun_error,*)' '
                    WRITE(lun_error,*)'ouch ! double orientation (i,j-1)...',i,j-1
                    val1(i,j-1)='*'
                    GOTO 9999
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        IF (j.LT.jmax) THEN
          IF ((ispr(i,j+1).EQ.0).AND. &
               (val(i,j+1).GE.-90).AND.(val(i,j+1).LE.0)) THEN
            ispr(i,j+1)=1
            nspr=nspr+1
          ELSE
            IF (val(i,j+1).GT.0) THEN
              IF (sval(i,j+1,1).EQ.0) THEN
                IF (val(i,j+1).EQ.1) THEN
                  WRITE(lun_error,*)' '
                  WRITE(lun_error,*)'ouch ! bad attack (i,j+1)...',i,j+1
                  val1(i,j+1)='*'
                  GOTO 9999
                ENDIF
              ELSE
                IF (or(sval(i,j+1,1)).EQ.0) THEN
                  or(sval(i,j+1,1))=-1
                ELSE
                  IF (or(sval(i,j+1,1)).NE.-1) THEN
                    WRITE(lun_error,*)' '
                    WRITE(lun_error,*)'ouch ! double orientation (i,j+1)...',i,j+1
                    val1(i,j+1)='*'
                    GOTO 9999
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        IF (key_periodic) THEN
          DO jj=1,jmax
            IF (ispr(imt,jj).EQ.1) ispr(2,jj)=1
            IF (sval(imt,jj,1).NE.0) sval(2,jj,1)=sval(imt,jj,1)
            IF (sval(imt,jj,2).NE.0) sval(2,jj,2)=sval(imt,jj,2)
            IF (ispr(1,jj).EQ.1) ispr(imt-1,jj)=1
            IF (sval(1,jj,1).NE.0) sval(imt-1,jj,1)=sval(1,jj,1)
            IF (sval(1,jj,2).NE.0) sval(imt-1,jj,2)=sval(1,jj,2)
          END DO
        END IF
        IF (key_jfold) THEN
          IF (TRIM(pivot) == 'T') THEN !!NG 8 march 2013
            DO ii=2,imt
              IF (ispr(ii,jmt-2).EQ.1) ispr(imt+2-ii,jmt)=1
              IF (sval(ii,jmt-2,1).NE.0) sval(imt+2-ii,jmt,1)=sval(ii,jmt-2,1)
              IF (sval(ii,jmt-2,2).NE.0) sval(imt+2-ii,jmt,2)=sval(ii,jmt-2,2)
            END DO
            DO ii=2,imt !!NG 8 march 2013 imt/2
              IF (ispr(ii,jmt-1).EQ.1) ispr(imt+2-ii,jmt-1)=1
              IF (sval(ii,jmt-1,1).NE.0) sval(imt+2-ii,jmt-1,1)=sval(ii,jmt-1,1)
              IF (sval(ii,jmt-1,2).NE.0) sval(imt+2-ii,jmt-1,2)=sval(ii,jmt-1,2)
            END DO
          ELSE !!NG 8 march 2013 F pivot
            DO ii=1,imt
              IF (ispr(ii,jmt-1).EQ.1) ispr(imt+1-ii,jmt)=1
              IF (sval(ii,jmt-1,1).NE.0) sval(imt+1-ii,jmt,1)=sval(ii,jmt-1,1)
              IF (sval(ii,jmt-1,2).NE.0) sval(imt+1-ii,jmt,2)=sval(ii,jmt-1,2)
            END DO
          ENDIF
        END IF
      ENDIF
    END DO
  END DO
  !
  DO i=1,imt
    IF ((val0(i,1).NE.'#').AND.(ispr(i,1).EQ.1)) THEN
      WRITE(lun_error,*)' '
      WRITE(lun_error,*)'ouch ! southern limit reached...',i,1
      val1(i,1)='*'
      GOTO 9999
    ENDIF
  END DO
  IF (.NOT.key_periodic) THEN
    DO j=1,jmax
      IF ((val0(1,j).NE.'#').AND.(ispr(1,j).EQ.1)) THEN
        WRITE(lun_error,*)' '
        WRITE(lun_error,*)'ouch ! western limit reached...',1,j
        val1(1,j)='*'
        GOTO 9999
      ENDIF
      IF ((val0(imt,j).NE.'#').AND.(ispr(imt,j).EQ.1)) THEN
        WRITE(lun_error,*)' '
        WRITE(lun_error,*)'ouch ! eastern limit reached...',imt,j
        val1(imt,j)='*'
        GOTO 9999
      ENDIF
    END DO
  ENDIF
  IF (.NOT.key_jfold) THEN
    DO i=1,imt
      IF ((val0(i,jmt).NE.'#').AND.(ispr(i,jmt).EQ.1)) THEN
        WRITE(lun_error,*)' '
        WRITE(lun_error,*)'ouch ! northern limit reached...',i,jmt
        val1(i,jmt)='*'
        GOTO 9999
      ENDIF
    END DO
  ENDIF
  IF (nspr.GT.0) GOTO 777
  !
2222 WRITE(lun_standard,*)' '
  WRITE(lun_standard,*)'Total number of segments: ',nsegm
  !
  maxsect0=-1
  maxtrsp=-1
  !
  DO n=1,nsegm
    IF (nsec(n).GT.maxsect0) maxsect0=nsec(n)
    IF (-nsec(n).GT.maxtrsp) maxtrsp=-nsec(n)
  END DO
  !
  DO i=1,maxsect0
    iflag=0
    DO n=1,nsegm
      IF (nsec(n).EQ.i) iflag=1
    END DO
    IF (iflag.EQ.0) WRITE(lun_error,*)'WARNING: no opaque section #',i
  END DO
  !
  DO i=1,maxtrsp
    iflag=0
    DO n=1,nsegm
      IF (nsec(n).EQ.-i) iflag=1
    END DO
    IF (iflag.EQ.0) WRITE(lun_error,*)'WARNING: no transparent section #',-i
  END DO
  !       
  DO n=1,nsegm
    IF (or(n).EQ.-1) THEN
      IF (it1(n).EQ.it2(n)) THEN
        it1(n)=-it1(n)
        it2(n)=-it2(n)
      ELSE
        jt1(n)=-jt1(n)
        jt2(n)=-jt2(n)
      ENDIF
    ENDIF
    IF (key_periodic) THEN
      IF (jt1(n).EQ.jt2(n)) THEN
        IF (it1(n).EQ.2) it1(n)=1
        IF (it2(n).EQ.(imt-1)) it2(n)=imt
      ENDIF
    ENDIF
  END DO
  !
  !!NG:
  DO
    l_esc=.TRUE.
    DO n=2,nsegm
      IF ( nsec(n-1) > nsec(n) ) THEN
        l_esc=.FALSE.
        itemp = nsec(n-1); nsec(n-1) = nsec(n); nsec(n) = itemp
        itemp = it1(n-1) ;  it1(n-1) =  it1(n);  it1(n) = itemp
        itemp = it2(n-1) ;  it2(n-1) =  it2(n);  it2(n) = itemp
        itemp = jt1(n-1) ;  jt1(n-1) =  jt1(n);  jt1(n) = itemp
        itemp = jt2(n-1) ;  jt2(n-1) =  jt2(n);  jt2(n) = itemp
      ENDIF
    END DO
    IF (l_esc) EXIT
  ENDDO

  WRITE(lun_standard,*)''
  WRITE(lun_standard,*)'*******************************************************'
  WRITE(lun_standard,*)'* Please verify that the content of sections.txt file *'
  WRITE(lun_standard,*)'*                     is correct                      *'
  WRITE(lun_standard,*)'*           (You can rename the section name)         *'
  WRITE(lun_standard,*)'*      (In some cases you will have to add a lid)     *'
  WRITE(lun_standard,*)'*******************************************************'
  WRITE(lun_standard,*)''

  DO n=1,nsegm
    WRITE(lun_standard,7171)nsec(n),it1(n),it2(n),jt1(n),jt2(n),1,kmt, &
         '"',nsec(n),'"'
  END DO

  OPEN (lun_sections, FILE='sections.txt',form='FORMATTED', STATUS='REPLACE')
  DO n=1,nsegm
    WRITE(lun_sections,7171)nsec(n),it1(n),it2(n),jt1(n),jt2(n),1,kmt, &
         '"',nsec(n),'"'
  END DO
  CLOSE(lun_sections)

  WRITE(lun_standard,*)''
  WRITE(lun_standard,*)'*******************************************************'
  WRITE(lun_standard,*)'*   Please verify that the region_limits are correct  *'
  WRITE(lun_standard,*)'*  You could edit the segrid_region_limits to do this *'
  WRITE(lun_standard,*)'*******************************************************'
  WRITE(lun_standard,*)''

7171 FORMAT(i2,6(1x,i5),1x,a1,i1,'section',a1)
  !!NG:
  !
  !!NG:DO jj=jmt,1,-1
  !!NG:   WRITE(38,'(500i1)')(ispr(ii,jj),ii=1,imt)
  !!NG:END DO

  !
  ! Extract the covering of the ispr region
  where(val(:,:) <= -10 ) val(:,:) =0
  where((val(:,:) > -10).AND.(val(:,:) < 10).AND.(val(:,:) /= 0) ) val(:,:) = 1
  ! Add the covering to the ispr region
  ispr(:,:) = ispr(:,:) + val(:,:)

  !NG COMPUTES THE REGION LIMITS using the effective region + the covering + 1
  CALL sub_comput_lim(ispr, 1, imt, jmt, reg_west, reg_east, ilength)
  CALL sub_comput_lim(ispr(:,jmt:1:-1), -1, imt, jmt, reg_south, reg_north, jlength)

  OPEN(UNIT=lun_reg, FILE='region_limits',form='FORMATTED', STATUS='REPLACE')
  WRITE(lun_reg,*) reg_west ,  reg_east, ilength
  WRITE(lun_reg,*) reg_south, reg_north, jlength 
  WRITE(lun_reg,*) 1        , kmt      , kmt
  CLOSE(lun_reg)
  !NG: END

  abs_reg_west  = ABS(reg_west)
  abs_reg_east  = ABS(reg_east)
  abs_reg_south = ABS(reg_south)
  abs_reg_north = ABS(reg_north)

  OPEN(32,file='segrid_region_limits',form='FORMATTED')

  IF (abs_reg_west < abs_reg_east) THEN
    IF (abs_reg_west /= 1) THEN
      val1(1:abs_reg_west-1,:) ='.'
    END IF
    IF (abs_reg_east /= imt) THEN
      val1(abs_reg_east+1:,:)  ='.'
    END IF
  ELSE
    !! write(*,*)'-dbg- abs_reg_west > abs_reg_east'
    IF ((abs_reg_west /= imt).AND.(abs_reg_east /= 1)) THEN
      val1(abs_reg_east+1:abs_reg_west-1,:) ='.'
    ELSEIF ((abs_reg_west == imt).AND.(abs_reg_east == 1)) THEN
      val1(abs_reg_east:abs_reg_west,:) ='.'
    ELSEIF (abs_reg_west == imt) THEN
      val1(abs_reg_east+1:abs_reg_west,:) ='.'
    ELSEIF (abs_reg_east == 1) THEN
      val1(abs_reg_east:abs_reg_west-1,:) ='.'
    ENDIF
  ENDIF

  IF (abs_reg_north /= jmt ) THEN
    val1(:,abs_reg_north+1:) ='.'
  ENDIF

  val1(:,1:abs_reg_south-1)='.'

  DO jj=jmt,1,-1
    WRITE(32,'(5000a1)')(val1(ii,jj),ii=1,imt)
  END DO

  CLOSE(32)


  GOTO 1234

9999 REWIND(30)

  DO jj=jmt,1,-1
    WRITE(30,'(5000a1)')(val1(ii,jj),ii=1,imt)
  END DO

1234 CONTINUE 

  CALL sub_coord_dealloc()
  CALL sub_scalef_dealloc()
  IF (key_roms) CALL sub_h_roms_dealloc()

  STOP
  !
END PROGRAM mkseg
