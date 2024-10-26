!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! - Ariane - (November 2008)
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
MODULE mod_B2C_grid_interpolation

  !------------------!
  ! USE ASSOCIAITION !
  !------------------!
  USE mod_precision
  USE mod_cst
  USE mod_lun
  USE mod_memory

  !-------------!
  ! DECLARATION !
  !-------------!
  IMPLICIT NONE

CONTAINS

  !!=========================================================================
  SUBROUTINE sub_B2C_grid_interpolation( &
       current_cpnt                    , &
       scale_factor_Bgrid              , &
       scale_factor_Cgrid              , &
       scale_factor_Fgrid              , &
       scale_factor_Tgrid              , &
       direction                       , &
       key_partialsteps)

    REAL(kind = rprec), DIMENSION(:,:,:,:), INTENT(inout) :: &
         current_cpnt

    REAL(kind = rprec), DIMENSION(:,:,:,:), INTENT(in)    :: &
         scale_factor_Bgrid                                , &
         scale_factor_Cgrid                                , &
         scale_factor_Fgrid                                , &
         scale_factor_Tgrid

    LOGICAL , INTENT(in)    :: key_partialsteps

    !! INTEGER(kind=iprec), DIMENSION(:,:,:,:),INTENT(in) :: Cgridmask

    REAL(kind = rprec), DIMENSION(:,:,:,:), ALLOCATABLE :: &
         tmp_array

    INTEGER(kind = iprec),DIMENSION(:,:,:,:), ALLOCATABLE :: &
         dfactor

    INTEGER(kind = iprec) :: nbi, nbj, nbk, nbt
    INTEGER(kind = iprec) :: loop_time, loop_depth, loop_j, loop_i

    CHARACTER(len=*) :: direction

    nbi=SIZE(current_cpnt,1)
    nbj=SIZE(current_cpnt,2)
    nbk=SIZE(current_cpnt,3)
    nbt=SIZE(current_cpnt,4)

    ALLOCATE(tmp_array(nbi,nbj,nbk,nbt))
    CALL sub_memory(size(tmp_array),'r','tmp_array','sub_B2C_grid_interpolation')

    ALLOCATE(dfactor(nbi,nbj,nbk,1))
    CALL sub_memory(size(dfactor),'i','dfactor','sub_B2C_grid_interpolation')

    dfactor(:,:,:,:) = iTwo

    tmp_array(:,:,:,:)=current_cpnt(:,:,:,:)

    IF (key_partialsteps) THEN

       IF ((direction == 'u').OR.(direction == 'U')) THEN

          !! WHERE(Cgridmask(:,2:nbj,:,1)/=0 .AND. Cgridmask(:,1:nbj-1,:,1)/=0) &
          !!      dfactor(:,2:nbj,:,1) = 2

          DO loop_time = 1, nbt
             DO loop_depth = 1, nbk


                DO loop_j = nbj, 2, -1
                   DO loop_i = nbi-1, 1, -1

                      IF ((scale_factor_Tgrid(loop_i,loop_j,loop_depth,1) /=0._rprec).AND. &
                           (scale_factor_Tgrid(loop_i+1,loop_j,loop_depth,1) /=0._rprec)) THEN

                         current_cpnt(loop_i,loop_j,loop_depth,loop_time)      = &
                              (tmp_array(loop_i,loop_j-1,loop_depth,loop_time) * &
                              scale_factor_Bgrid(loop_i,loop_j-1,1,1)          * &
                              scale_factor_Fgrid(loop_i,loop_j-1,loop_depth,1)          + &
                              tmp_array(loop_i,loop_j,loop_depth,loop_time)    * &
                              scale_factor_Bgrid(loop_i,loop_j,1,1)            * &
                              scale_factor_Fgrid(loop_i,loop_j,loop_depth,1))           / &
                              (dfactor(loop_i,loop_j,loop_depth,1)             * & 
                              scale_factor_Cgrid(loop_i,loop_j,1,1)            * &
                              MIN(scale_factor_Tgrid(loop_i,loop_j,loop_depth,1)        , &
                              scale_factor_Tgrid(loop_i+1,loop_j,loop_depth,1)))

                      ENDIF

                   ENDDO

                   loop_i = nbi
                   IF (scale_factor_Tgrid(loop_i,loop_j,loop_depth,1) /=0._rprec) THEN
                      current_cpnt(loop_i,loop_j,loop_depth,loop_time)      = &
                           (tmp_array(loop_i,loop_j-1,loop_depth,loop_time) * &
                           scale_factor_Bgrid(loop_i,loop_j-1,1,1)          * &
                           scale_factor_Fgrid(loop_i,loop_j-1,loop_depth,1)          + &
                           tmp_array(loop_i,loop_j,loop_depth,loop_time)    * &
                           scale_factor_Bgrid(loop_i,loop_j,1,1)            * &
                           scale_factor_Fgrid(loop_i,loop_j,loop_depth,1))           / &
                           (dfactor(loop_i,loop_j,loop_depth,1)             * & 
                           scale_factor_Cgrid(loop_i,loop_j,1,1)            * &
                           scale_factor_Tgrid(loop_i,loop_j,loop_depth,1))
                   ENDIF

                ENDDO

                loop_j = 1
                DO loop_i = nbi-1, 1, -1

                   IF ((scale_factor_Tgrid(loop_i,loop_j,loop_depth,1) /=0._rprec).AND. &
                        (scale_factor_Tgrid(loop_i+1,loop_j,loop_depth,1) /=0._rprec)) THEN

                      current_cpnt(loop_i,loop_j,loop_depth,loop_time)      = &
                           (tmp_array(loop_i,loop_j,loop_depth,loop_time)   * &
                           scale_factor_Bgrid(loop_i,loop_j,1,1)            * &
                           scale_factor_Fgrid(loop_i,loop_j,loop_depth,1))           / &
                           (dfactor(loop_i,loop_j,loop_depth,1)             * & 
                           scale_factor_Cgrid(loop_i,loop_j,1,1)            * &
                           MIN(scale_factor_Tgrid(loop_i,loop_j,loop_depth,1)        , &
                           scale_factor_Tgrid(loop_i+1,loop_j,loop_depth,1)))

                   ENDIF

                ENDDO

                loop_j = 1
                loop_i = nbi
                IF (scale_factor_Tgrid(loop_i,loop_j,loop_depth,1) /=0._rprec) THEN
                   current_cpnt(loop_i,loop_j,loop_depth,loop_time)      = &
                        (tmp_array(loop_i,loop_j,loop_depth,loop_time)   * &
                        scale_factor_Bgrid(loop_i,loop_j,1,1)            * &
                        scale_factor_Fgrid(loop_i,loop_j,loop_depth,1))           / &
                        (dfactor(loop_i,loop_j,loop_depth,1)             * & 
                        scale_factor_Cgrid(loop_i,loop_j,1,1)            * &
                        scale_factor_Tgrid(loop_i,loop_j,loop_depth,1))
                ENDIF

             ENDDO
          ENDDO

          !- Comments -!
          WRITE(lun_standard,*)' - Zonal Component: max ', &
               MAXVAL(current_cpnt), ' min ', MINVAL(current_cpnt)

       ELSEIF ((direction == 'v').OR.(direction == 'V')) THEN

          !! WHERE(Cgridmask(2:nbi,:,:,1)/=0 .AND. Cgridmask(1:nbi-1,:,:,1)/=0) &
          !!      dfactor(2:nbi,:,:,1) = 2

          DO loop_time = 1, nbt
             DO loop_depth = 1, nbk

                DO loop_j = nbj-1, 1, -1
                   DO loop_i =  nbi, 2, -1

                      IF ((scale_factor_Tgrid(loop_i,loop_j,loop_depth,1) /=0._rprec).AND. &
                           (scale_factor_Tgrid(loop_i,loop_j+1,loop_depth,1) /=0._rprec)) THEN

                         current_cpnt(loop_i,loop_j,loop_depth,loop_time)     = &
                              (tmp_array(loop_i-1,loop_j,loop_depth,loop_time)* &
                              scale_factor_Bgrid(loop_i-1,loop_j,1,1)         * &
                              scale_factor_Fgrid(loop_i-1,loop_j,loop_depth,1)         + &
                              tmp_array(loop_i,loop_j,loop_depth,loop_time)   * &
                              scale_factor_Bgrid(loop_i,loop_j,1,1)           * &
                              scale_factor_Fgrid(loop_i,loop_j,loop_depth,1))          / &
                              (dfactor(loop_i,loop_j,loop_depth,1)            * &
                              scale_factor_Cgrid(loop_i,loop_j,1,1)           * &
                              MIN(scale_factor_Tgrid(loop_i,loop_j,loop_depth,1)       , &
                              scale_factor_Tgrid(loop_i,loop_j+1,loop_depth,1)))

                      ENDIF

                   ENDDO
                ENDDO

                loop_j = nbj
                DO loop_i =  nbi, 2, -1

                   IF (scale_factor_Tgrid(loop_i,loop_j,loop_depth,1) /=0._rprec) THEN

                      current_cpnt(loop_i,loop_j,loop_depth,loop_time)     = &
                           (tmp_array(loop_i-1,loop_j,loop_depth,loop_time)* &
                           scale_factor_Bgrid(loop_i-1,loop_j,1,1)         * &
                           scale_factor_Fgrid(loop_i-1,loop_j,loop_depth,1)         + &
                           tmp_array(loop_i,loop_j,loop_depth,loop_time)   * &
                           scale_factor_Bgrid(loop_i,loop_j,1,1)           * &
                           scale_factor_Fgrid(loop_i,loop_j,loop_depth,1))          / &
                           (dfactor(loop_i,loop_j,loop_depth,1)            * &
                           scale_factor_Cgrid(loop_i,loop_j,1,1)           * &
                           scale_factor_Tgrid(loop_i,loop_j,loop_depth,1))

                   ENDIF

                ENDDO

                loop_i=1
                DO loop_j = nbj-1, 1, -1

                   IF ((scale_factor_Tgrid(loop_i,loop_j,loop_depth,1) /=0._rprec).AND. &
                        (scale_factor_Tgrid(loop_i,loop_j+1,loop_depth,1) /=0._rprec)) THEN
                      current_cpnt(loop_i,loop_j,loop_depth,loop_time)     = &
                           (tmp_array(loop_i,loop_j,loop_depth,loop_time)  * &
                           scale_factor_Bgrid(loop_i,loop_j,1,1)           * &
                           scale_factor_Fgrid(loop_i,loop_j,loop_depth,1))          / &
                           (scale_factor_Cgrid(loop_i,loop_j,1,1)          * &
                           scale_factor_Cgrid(loop_i,loop_j,1,1)           * &
                           MIN(scale_factor_Tgrid(loop_i,loop_j,loop_depth,1)       , &
                           scale_factor_Tgrid(loop_i,loop_j+1,loop_depth,1)))
                   ENDIF
                ENDDO

                loop_i=1
                loop_j=nbj
                IF (scale_factor_Tgrid(loop_i,loop_j,loop_depth,1) /=0._rprec) THEN

                   current_cpnt(loop_i,loop_j,loop_depth,loop_time)     = &
                        (tmp_array(loop_i,loop_j,loop_depth,loop_time)   * &
                        scale_factor_Bgrid(loop_i,loop_j,1,1)           * &
                        scale_factor_Fgrid(loop_i,loop_j,loop_depth,1))          / &
                        (dfactor(loop_i,loop_j,loop_depth,1)            * &
                        scale_factor_Cgrid(loop_i,loop_j,1,1)           * &
                        scale_factor_Tgrid(loop_i,loop_j,loop_depth,1))

                ENDIF


             ENDDO
          ENDDO

          !- Comments -!
          WRITE(lun_standard,*)' - Meridional Component: max ', &
               MAXVAL(current_cpnt), ' min ', MINVAL(current_cpnt)

       ELSEIF((direction == 't').OR.(direction == 'T')) THEN
          WRITE(lun_standard,*)' mod_B2C_grid_interpolation: T points on B and C grid are at the same place !!!'
       ELSE
          STOP
       ENDIF

    ELSE !!! NO PARTIAL STEPS means scale_factor_Tgrid and scale_factor_Fgrid = 1D !!!!

       IF ((direction == 'u').OR.(direction == 'U')) THEN

          !! WHERE(Cgridmask(:,2:nbj,:,1)/=0 .AND. Cgridmask(:,1:nbj-1,:,1)/=0) &
          !!      dfactor(:,2:nbj,:,1) = 2

          DO loop_time = 1, nbt
             DO loop_depth = 1, nbk


                DO loop_j = nbj, 2, -1
                   DO loop_i = nbi-1, 1, -1

                      IF ((scale_factor_Tgrid(1,1,loop_depth,1) /=0._rprec).AND. &
                           (scale_factor_Tgrid(1,1,loop_depth,1) /=0._rprec)) THEN

                         current_cpnt(loop_i,loop_j,loop_depth,loop_time)      = &
                              (tmp_array(loop_i,loop_j-1,loop_depth,loop_time) * &
                              scale_factor_Bgrid(loop_i,loop_j-1,1,1)          * &
                              scale_factor_Fgrid(1,1,loop_depth,1)          + &
                              tmp_array(loop_i,loop_j,loop_depth,loop_time)    * &
                              scale_factor_Bgrid(loop_i,loop_j,1,1)            * &
                              scale_factor_Fgrid(1,1,loop_depth,1))           / &
                              (dfactor(loop_i,loop_j,loop_depth,1)             * & 
                              scale_factor_Cgrid(loop_i,loop_j,1,1)            * &
                              scale_factor_Tgrid(1,1,loop_depth,1))

                      ENDIF

                   ENDDO

                   loop_i = nbi
                   IF (scale_factor_Tgrid(1,1,loop_depth,1) /=0._rprec) THEN
                      current_cpnt(loop_i,loop_j,loop_depth,loop_time)      = &
                           (tmp_array(loop_i,loop_j-1,loop_depth,loop_time) * &
                           scale_factor_Bgrid(loop_i,loop_j-1,1,1)          * &
                           scale_factor_Fgrid(1,1,loop_depth,1)          + &
                           tmp_array(loop_i,loop_j,loop_depth,loop_time)    * &
                           scale_factor_Bgrid(loop_i,loop_j,1,1)            * &
                           scale_factor_Fgrid(1,1,loop_depth,1))           / &
                           (dfactor(loop_i,loop_j,loop_depth,1)             * & 
                           scale_factor_Cgrid(loop_i,loop_j,1,1)            * &
                           scale_factor_Tgrid(1,1,loop_depth,1))
                   ENDIF

                ENDDO

                loop_j = 1
                DO loop_i = nbi-1, 1, -1

                   IF ((scale_factor_Tgrid(1,1,loop_depth,1) /=0._rprec).AND. &
                        (scale_factor_Tgrid(1,1,loop_depth,1) /=0._rprec)) THEN

                      current_cpnt(loop_i,loop_j,loop_depth,loop_time)      = &
                           (tmp_array(loop_i,loop_j,loop_depth,loop_time)   * &
                           scale_factor_Bgrid(loop_i,loop_j,1,1)            * &
                           scale_factor_Fgrid(1,1,loop_depth,1))           / &
                           (dfactor(loop_i,loop_j,loop_depth,1)             * & 
                           scale_factor_Cgrid(loop_i,loop_j,1,1)            * &
                           scale_factor_Tgrid(1,1,loop_depth,1))

                   ENDIF

                ENDDO

                loop_j = 1
                loop_i = nbi
                IF (scale_factor_Tgrid(1,1,loop_depth,1) /=0._rprec) THEN
                   current_cpnt(loop_i,loop_j,loop_depth,loop_time)      = &
                        (tmp_array(loop_i,loop_j,loop_depth,loop_time)   * &
                        scale_factor_Bgrid(loop_i,loop_j,1,1)            * &
                        scale_factor_Fgrid(1,1,loop_depth,1))           / &
                        (dfactor(loop_i,loop_j,loop_depth,1)             * & 
                        scale_factor_Cgrid(loop_i,loop_j,1,1)            * &
                        scale_factor_Tgrid(1,1,loop_depth,1))
                ENDIF

             ENDDO
          ENDDO

          !- Comments -!
          WRITE(lun_standard,*)' - Zonal Component: max ', &
               MAXVAL(current_cpnt), ' min ', MINVAL(current_cpnt)

       ELSEIF ((direction == 'v').OR.(direction == 'V')) THEN

          !! WHERE(Cgridmask(2:nbi,:,:,1)/=0 .AND. Cgridmask(1:nbi-1,:,:,1)/=0) &
          !!      dfactor(2:nbi,:,:,1) = 2

          DO loop_time = 1, nbt
             DO loop_depth = 1, nbk

                DO loop_j = nbj-1, 1, -1
                   DO loop_i =  nbi, 2, -1

                      IF ((scale_factor_Tgrid(1,1,loop_depth,1) /=0._rprec).AND. &
                           (scale_factor_Tgrid(1,1,loop_depth,1) /=0._rprec)) THEN

                         current_cpnt(loop_i,loop_j,loop_depth,loop_time)     = &
                              (tmp_array(loop_i-1,loop_j,loop_depth,loop_time)* &
                              scale_factor_Bgrid(loop_i-1,loop_j,1,1)         * &
                              scale_factor_Fgrid(1,1,loop_depth,1)         + &
                              tmp_array(loop_i,loop_j,loop_depth,loop_time)   * &
                              scale_factor_Bgrid(loop_i,loop_j,1,1)           * &
                              scale_factor_Fgrid(1,1,loop_depth,1))          / &
                              (dfactor(loop_i,loop_j,loop_depth,1)            * &
                              scale_factor_Cgrid(loop_i,loop_j,1,1)           * &
                              scale_factor_Tgrid(1,1,loop_depth,1))

                      ENDIF

                   ENDDO
                ENDDO

                loop_j = nbj
                DO loop_i =  nbi, 2, -1

                   IF (scale_factor_Tgrid(1,1,loop_depth,1) /=0._rprec) THEN

                      current_cpnt(loop_i,loop_j,loop_depth,loop_time)     = &
                           (tmp_array(loop_i-1,loop_j,loop_depth,loop_time)* &
                           scale_factor_Bgrid(loop_i-1,loop_j,1,1)         * &
                           scale_factor_Fgrid(1,1,loop_depth,1)         + &
                           tmp_array(loop_i,loop_j,loop_depth,loop_time)   * &
                           scale_factor_Bgrid(loop_i,loop_j,1,1)           * &
                           scale_factor_Fgrid(1,1,loop_depth,1))          / &
                           (dfactor(loop_i,loop_j,loop_depth,1)            * &
                           scale_factor_Cgrid(loop_i,loop_j,1,1)           * &
                           scale_factor_Tgrid(1,1,loop_depth,1))

                   ENDIF

                ENDDO

                loop_i=1
                DO loop_j = nbj-1, 1, -1

                   IF ((scale_factor_Tgrid(1,1,loop_depth,1) /=0._rprec).AND. &
                        (scale_factor_Tgrid(1,1,loop_depth,1) /=0._rprec)) THEN
                      current_cpnt(loop_i,loop_j,loop_depth,loop_time)     = &
                           (tmp_array(loop_i,loop_j,loop_depth,loop_time)  * &
                           scale_factor_Bgrid(loop_i,loop_j,1,1)           * &
                           scale_factor_Fgrid(1,1,loop_depth,1))          / &
                           (scale_factor_Cgrid(loop_i,loop_j,1,1)          * &
                           scale_factor_Cgrid(loop_i,loop_j,1,1)           * &
                           scale_factor_Tgrid(1,1,loop_depth,1))
                   ENDIF
                ENDDO

                loop_i=1
                loop_j=nbj
                IF (scale_factor_Tgrid(1,1,loop_depth,1) /=0._rprec) THEN

                   current_cpnt(loop_i,loop_j,loop_depth,loop_time)     = &
                        (tmp_array(loop_i,loop_j,loop_depth,loop_time)   * &
                        scale_factor_Bgrid(loop_i,loop_j,1,1)           * &
                        scale_factor_Fgrid(1,1,loop_depth,1))          / &
                        (dfactor(loop_i,loop_j,loop_depth,1)            * &
                        scale_factor_Cgrid(loop_i,loop_j,1,1)           * &
                        scale_factor_Tgrid(1,1,loop_depth,1))

                ENDIF


             ENDDO
          ENDDO

          !- Comments -!
          WRITE(lun_standard,*)' - Meridional Component: max ', &
               MAXVAL(current_cpnt), ' min ', MINVAL(current_cpnt)

       ELSE
          STOP
       ENDIF

    ENDIF

    WRITE(lun_standard,*)' - Interpolation form B-grid to C_grid is done -'


    CALL sub_memory(-size(tmp_array),'r','tmp_array','sub_B2C_grid_interpolation')
    DEALLOCATE(tmp_array)

    CALL sub_memory(-size(dfactor),'i','dfactor','sub_B2C_grid_interpolation')
    DEALLOCATE(dfactor)


  END SUBROUTINE sub_B2C_grid_interpolation


END MODULE mod_B2C_grid_interpolation
