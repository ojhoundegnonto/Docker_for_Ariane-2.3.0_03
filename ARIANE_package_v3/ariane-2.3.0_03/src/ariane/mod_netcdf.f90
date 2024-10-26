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
!!****h* ariane/mod_netcdf
!! NAME
!!   mod_netcdf (mod_netcdf.f90 - Fortran90 module)
!!
!! USAGE
!!   Include 'USE mod_netcdf' in the header of your Fortran 90 source 
!!   code.
!!   Then you'll have access to the subroutine:
!!      - sub_build_filename
!!      - sub_open_netcdf_file
!!      - sub_close_netcdf_file
!!      - sub_read_netcdf_varid_ndims
!!      - sub_read_netcdf_var_dims
!!      - sub_select_var_dims
!!      - sub_read_netcdf_var4d
!!      - sub_read_netcdf_att_info
!!      - 
!!
!! FUNCTION
!!   Netcdf subroutine to Ariane application.
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
!!   * USE mod_namelist
!!   * USE netcdf
!!
!! USED BY
!!   * mod_input_data.f90
!!   * mod_input_grid.f90
!!
!! SOURCE
!!=========================================================================
MODULE mod_netcdf

  !------------------!
  ! USE ASSOCIAITION !
  !------------------!
  USE mod_precision
  USE mod_cst
  USE mod_memory
  USE mod_namelist
  USE netcdf
  USE mod_reducmem

  !!NG: 19 06 2009
  REAL(kind = rprec), DIMENSION(:)      , ALLOCATABLE :: dumtab1d
  REAL(kind = rprec), DIMENSION(:,:)    , ALLOCATABLE :: dumtab2d
  REAL(kind = rprec), DIMENSION(:,:,:)  , ALLOCATABLE :: dumtab3d
  REAL(kind = rprec), DIMENSION(:,:,:,:), ALLOCATABLE :: dumtab4d


  !!***
CONTAINS
  !=========================================================================
  !!****f* mod_netcdf/sub_build_filename()
  !! NAME
  !!   sub_build_filename()
  !!
  !! FUNCTION
  !!   Build file name with namelist parameters.
  !!   We assume that netcdf file_name are build on this form:
  !!                  netcdf file_name = prefix_XXXX_suffix
  !!   Where XXXX, is an integer from "ind0" to "indn" on "ndigits" (here 4).
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
  !!   * Input:
  !!      - ind     : an integer epresenting the netcdf file number
  !!      - maxsize : number of digits on which "ind is represented
  !!      - c_prefix: first part of the netcdf file name
  !!      - c_suffix: last part of the netcdf file name
  !!   * Output:
  !!      - c_filename: the netcdf file_name
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * mod_input_data.f90
  !!   * mod_input_grid.f90
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_build_filename(ind, maxsize, c_prefix, c_suffix, c_filename)

    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind=iprec), INTENT(in)  :: ind, maxsize
    CHARACTER(len=*)   , INTENT(in)  :: c_prefix, c_suffix
    CHARACTER(len=*)   , INTENT(out) :: c_filename

    !- local variables -!
    CHARACTER(len=4), PARAMETER :: c_none='NONE'
    CHARACTER(len=14)           :: cmft

    !-------------!
    ! Code begins !
    !-------------!
    !- Test if the netcdf filename has a number -!
    !- ind == -1 means that there's a single netcdf file -!
    !- Each case is represented :
    !-         - netcdf file name = prefix
    !-         - netcdf file name = suffix
    !-         - netcdf file name = prefix suffix
    !- ind /= -1 means that the netcdf file has a number -!
    !- Each case is represented :
    !-         - netcdf file name = prefix ind suffix
    !-         - netcdf file name = ind suffix
    !-         - netcdf file name = prefix ind
    IF (ind < 0) THEN
      IF  ((c_prefix /= c_none ).AND.(c_suffix == c_none)) THEN
        c_filename = TRIM(c_prefix)
      ELSEIF ((c_prefix == c_none ).AND.(c_suffix /= c_none)) THEN
        c_filename = TRIM(c_suffix)
      ELSEIF((c_prefix /= c_none ).AND.(c_suffix /= c_none))  THEN
        c_filename = TRIM(c_prefix) // TRIM(c_suffix)
      ELSE
        WRITE(lun_error,*)'mod_netcdf: build_filename: error', ind, c_prefix, c_suffix
        STOP
      ENDIF
    ELSE
      IF ((c_prefix /= c_none ).AND.(c_suffix /= c_none)) THEN
        WRITE(cmft      , 1101) maxsize, maxsize
        WRITE(c_filename, FMT=cmft) TRIM(c_prefix), ind, TRIM(c_suffix)
      ELSEIF ((c_prefix == c_none ).AND.(c_suffix /= c_none)) THEN
        WRITE(cmft      , 1102) maxsize, maxsize
        WRITE(c_filename,  FMT=cmft) ind, TRIM(c_suffix)
      ELSEIF ((c_prefix /= c_none ).AND.(c_suffix == c_none)) THEN
        WRITE(cmft      , 1103) maxsize, maxsize
        WRITE(c_filename,  FMT=cmft) TRIM(c_prefix), ind
      ELSE
        WRITE(lun_error,*)'mod_netcdf: build_filename: error', ind,  c_prefix, c_suffix
        STOP
      ENDIF
    ENDIF

    !- Formats -!
    !- For example: I5.5 means that integer will represented on 5 digits -!
    !-              zero value are added to complete digits              -!
1101 FORMAT('(A, I', I2, '.', I2, ', A)')
1102 FORMAT('(I'   , I2, '.', I2, ', A)')
1103 FORMAT('(A, I', I2, '.', I2, ')')

  END SUBROUTINE sub_build_filename
  !!***
  !=========================================================================
  !!****f* mod_netcdf/sub_open_netcdf_file()
  !! NAME
  !!   sub_open_netcdf_file()
  !!
  !! FUNCTION
  !!   Open a netcdf file.
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
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
  !!       - c_dir     : the directory where the netcdf file is.
  !!       - c_filename: the netcdf file name.
  !!   * OUTPUT:
  !!       - ncid      : return the netcdf file ID.
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * mod_input_data.f90
  !!   * mod_input_grid.f90
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_open_netcdf_file(c_dir, c_filename, ncid)

    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    CHARACTER(len = *)    , INTENT(in)  :: c_dir, c_filename
    INTEGER(kind  = iprec), INTENT(out) :: ncid

    !- local variables -!
    INTEGER(kind = ishort) :: is_err

    !-------------!
    ! Code begins !
    !-------------!
    !- Netcdf call -!
    is_err = nf90_open( &
         path = TRIM(c_dir)//'/'//TRIM(c_filename), &
         mode = nf90_nowrite                 , &
         ncid = ncid                           &
         )

    !- Test if there's a problem -!
    IF (is_err /= nf90_noerr) THEN
      WRITE(lun_error,*) 'mod_netcdf: sub_open_netcdf_file: problem to open netcdf file!'
      WRITE(lun_error,*) ' path = ', TRIM(c_dir)//'/'//TRIM(c_filename)
      STOP
    ELSE
      WRITE(lun_standard,*)''
      WRITE(lun_standard,*)'-> Netcdf File - Successful Opening -', &
           TRIM(c_dir)//'/'//TRIM(c_filename), ' - ncid = ', ncid
    ENDIF

  END SUBROUTINE sub_open_netcdf_file
  !!***
  !========================================================================
  !=========================================================================
  !!****f* mod_netcdf/sub_close_netcdf_file()
  !! NAME
  !!   sub_close_netcdf_file()
  !!
  !! FUNCTION
  !!   Close a netcdf file.
  !!   !! Netcdf version 3.6.0 or newer is required for F90 calls !!
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
  !!   * ncid (input): the netcdf file ID
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * mod_input_data.f90
  !!   * mod_input_grid.f90
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_close_netcdf_file(ncid)

    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind=iprec), INTENT(in) :: ncid

    !- local variables -!
    INTEGER(kind=ishort) :: is_err

    !-------------!
    ! Code begins !
    !-------------!
    !- Netcdf call -!
    is_err = nf90_close(ncid = ncid)

    !- Test if there's a problem -!
    IF (is_err /= nf90_noerr) THEN
      WRITE(lun_error,*) 'mod_netcdf: sub_close_netcdf_file: problem to close netcdf file!'
      WRITE(lun_error,*) ' ncid = ', ncid
      STOP 
    ELSE
      WRITE(lun_standard,*)'-> Netcdf File - Successful Closing - ncid =', ncid
      WRITE(lun_standard,*)''
    ENDIF

  END SUBROUTINE sub_close_netcdf_file
  !!***
  !========================================================================
  !=========================================================================
  !!****f* mod_netcdf/sub_read_netcdf_varid_ndims()
  !! NAME
  !!   sub_read_netcdf_varid_ndims()
  !!
  !! FUNCTION
  !!   Inquire netcdf variable ID and dims 
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
  !!   * Inputs:
  !!      - ncid  : netcdf file ID
  !!      - nc_var: netcdf avriable name
  !!   * Outputs:
  !!      - varid : netcdf variable ID
  !!      - ndims : netcdf variable dimension number
  !!
  !! TODO
  !!   
  !! USES
  !!   * nf90_inq_varid         (Netcdf lib 3.6.x or newer)
  !!   * nf90_inquire_variable  (Netcdf lib 3.6.x or newer)
  !!
  !! USED BY
  !!   * mod_netcdf (Private)
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_read_netcdf_varid_ndims(ncid, nc_var, varid, ndims)

    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind=iprec), INTENT(in)  :: ncid
    CHARACTER(len= *)  , INTENT(in)  :: nc_var
    INTEGER(kind=iprec), INTENT(out) :: varid
    INTEGER(kind=iprec), OPTIONAL, INTENT(out) :: ndims

    !- local variables -!
    INTEGER(kind=ishort) :: is_err

    !-------------!
    ! Code begins !
    !-------------!

    !-------------!
    !- Get varid -!
    !-------------!
    is_err     = nf90_inq_varid( &
         ncid  = ncid          , &
         name  = TRIM(nc_var)  , &
         varid = varid           &
         )
    !- Test problem or error -!
    IF (is_err /= nf90_noerr) THEN
      WRITE(lun_error,*)'mod_netcdf: sub_read_netcdf_varid_ndims: netcdf variable name problem!'
      WRITE(lun_error,*)' ncid  = ', ncid
      WRITE(lun_error,*)' name  = ', TRIM(nc_var)
      WRITE(lun_error,*)' varid = ', varid
      STOP
    ENDIF

    !------------------------------------!
    !- Get variable number of dimension -!
    !------------------------------------!
    IF (PRESENT(ndims)) THEN

      is_err = nf90_inquire_variable( &
           ncid = ncid              , &
           varid = varid            , &
           ndims = ndims              &
           )

      !-------------------------!
      !- Test problem or error -!
      !-------------------------!
      IF (is_err /= nf90_noerr) &
           STOP 'mod_netcdf: sub_read_netcdf_varid_ndims: netcdf variable ndims problem!'

    ENDIF

  END SUBROUTINE sub_read_netcdf_varid_ndims
  !!***
  !========================================================================
  !=========================================================================
  !!****f* mod_netcdf/sub_read_netcdf_var_dims()
  !! NAME
  !!   sub_read_netcdf_var_dims()
  !!
  !! FUNCTION
  !!   Read netcdf variable dimension lengths from netcdf dimension ID.
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
  !!   * Inputs:
  !!      - ncid : netcdf file ID
  !!      - varid: netcdf variable ID
  !!      - ndims: netcdf variable dimension number
  !!   * Output:
  !!      - dims : netcdf variable dimensions
  !!
  !! TODO
  !!   
  !! USES
  !!  * nf90_inquire_variable
  !!  * nf90_inquire_dimension
  !!
  !! USED BY
  !!   * mod_netcdf (Private)
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_read_netcdf_var_dims(ncid, varid, ndims, dims)

    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind=iprec), INTENT(in) :: ncid, varid, ndims
    INTEGER(kind=iprec), DIMENSION(:), INTENT(out):: dims

    !- local variables -!
    INTEGER(kind=ishort) :: is_err ! error code returns by netcdf function
    INTEGER(kind=ishort) :: ii     ! dummy variable for a DoLoop

    ! dimids: netcdf dimension ID. It necessary to have this information
    !         before to inquire each dimension length of a netcdf variable.
    INTEGER(kind=ishort), DIMENSION(:), ALLOCATABLE :: dimids

    !-------------!
    ! Code begins !
    !-------------!

    !----------------------!
    !- Dynamic allocation -!
    !----------------------!
    ALLOCATE(dimids(ndims))
    !!NG : 19 06 2009 CALL sub_memory(size(dimids),'i','dimids','sub_read_netcdf_var_dims')

    !------------------------------!
    !- Get variable dimension IDs -!
    !------------------------------!
    is_err = nf90_inquire_variable( &
         ncid = ncid              , &
         varid = varid            , &
         dimids = dimids            &
         )
    !-------------------------!
    !- Test problem or error -!
    !-------------------------!
    IF (is_err /= nf90_noerr) THEN
      WRITE(lun_error,*) 'mod_netcdf: sub_read_netcdf_var_dims: problem to inquire variable!'
      WRITE(lun_error,*) ' ncid   = ', ncid
      WRITE(lun_error,*) ' varid  = ', varid
      WRITE(lun_error,*) ' dimids = ', dimids
      STOP 
    ENDIF

    !-----------------------------------------------------!
    !- For each dimension, get variable dimension length -!
    !-----------------------------------------------------!
    DO ii = 1, ndims

      !------------------------!
      !- Get dimension length -!
      !------------------------!
      is_err = nf90_inquire_dimension( &
           ncid = ncid               , &
           dimid = dimids(ii)        , &
           len = dims(ii)              &
           )
      !-------------------------!
      !- Test problem or error -!
      !-------------------------!
      IF (is_err /= nf90_noerr) THEN
        WRITE(lun_error,*) 'mod_netcdf: sub_read_netcdf_var_dims: problem to inquire dimension!'
        WRITE(lun_error,*) ' ncid   = ', ncid
        WRITE(lun_error,*) ' ii     = ', ii
        WRITE(lun_error,*) ' dimid  = ', dimids(ii)
        WRITE(lun_error,*) ' len = '   , dims(ii)
        STOP 
      ENDIF

    ENDDO

    !----------------------!
    !- Deallocation -!
    !----------------------!
    !!NG : 19 06 2009 CALL sub_memory(-size(dimids),'i','dimids','sub_read_netcdf_var_dims')
    DEALLOCATE(dimids)


  END SUBROUTINE sub_read_netcdf_var_dims
  !!***
  !========================================================================
  !!****f* mod_netcdf/sub_select_var_dims()
  !! NAME
  !!   sub_select_var_dims()
  !!
  !! FUNCTION
  !!   Get netcdf variable dimension lengths, calls:
  !!        - sub_read_netcdf_varid_ndims
  !!        - sub_read_netcdf_var_dims
  !!   to do this.
  !!   And after compare these dimesion length with imt, jmt, kmt and lmt
  !!   and the order how there used (or stored) for a netcdf variable.
  !!
  !!   Dimension order is very important to reorder netcdf 1d, 2d, 3d or 4d 
  !!   arrays in the 4d Ariane arrays which have (imt, jmt, kmt, lmt)
  !!   dimension.
  !!
  !! EXAMPLES
  !!   * A netcdf variable has (imt, jmt, kmt, lmt) dimensions.
  !!     In this case:
  !!          - dimx = imt
  !!          - dimy = jmt
  !!          - dimz = kmt
  !!          - dimt = lmt
  !!     And:   - dimsorder = /1,2,3,4/
  !!
  !!   * A netcdf variable has (kmt,lmt) dimensions.
  !!     In this case:
  !!          - dimx = 1
  !!          - dimy = 1
  !!          - dimz = kmt
  !!          - dimt = lmt
  !!     And:   - dimsorder = /0,0,1,2/
  !!
  !!   * A netcdf variable has (imt, jmt, lmt) dimensions.
  !!     In this case:
  !!          - dimx = imt
  !!          - dimy = jmt
  !!          - dimz = 1
  !!          - dimt = lmt
  !!     And:   - dimsorder = /1,2,0,3/
  !!
  !!   * A netcdf variable has (imt, jmt, 1, lmt) dimensions.
  !!     In this case:
  !!          - dimx = imt
  !!          - dimy = jmt
  !!          - dimz = 1
  !!          - dimt = lmt
  !!     And:   - dimsorder = /1,2,4,3/
  !!
  !!   * A netcdf variable has (1, 1, kmt, 1) dimensions.
  !!     In this case:
  !!          - dimx = 1
  !!          - dimy = 1
  !!          - dimz = kmt
  !!          - dimt = 1
  !!     And:   - dimsorder = /2,3,1,4/
  !!
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
  !!   * Inputs:
  !!      - ncid       : netcdf file ID
  !!      - nc_var_name: netcdf variable name
  !!
  !!   * Outputs:
  !!      - varid    : netcdf variable ID
  !!      - dimsorder: dimensions order (see below Examples)
  !!      - dimx     : x dimension
  !!      - dimy     : y dimension
  !!      - dimz     : z dimension
  !!      - dimt     : time dimension
  !!
  !! TODO
  !!   
  !! USES
  !!  * sub_read_netcdf_varid_ndims
  !!  * sub_read_netcdf_var_dims
  !!
  !! USED BY
  !!   * mod_input_data.f90
  !!   * mod_input_grid.f90
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_select_var_dims( &
       ncid, nc_var_name, varid, dimsorder, &
       dimx, dimy, dimz, dimt, key_ijt)

    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind = iprec), INTENT(in)  :: ncid
    CHARACTER(len = *)   , INTENT(in)  :: nc_var_name
    INTEGER(kind = iprec), DIMENSION(4), INTENT(out) :: dimsorder
    INTEGER(kind = iprec), INTENT(out) :: varid, dimx, dimy, dimz, dimt
    LOGICAL              , OPTIONAL    :: key_ijt !! NG 05_10_2009

    !- local variables -!
    INTEGER(kind = iprec) :: ii    ! dummy variable for a DoLoop
    INTEGER(kind = iprec) :: ndims ! number of dimensions
    INTEGER(kind = iprec) :: icnt

    ! dims: variable dimension lengths are stored in this array
    INTEGER(kind = iprec), DIMENSION(:), ALLOCATABLE :: dims

    INTEGER(kind = iprec), DIMENSION(4) :: sumdims, dumdims

    !-------------!
    ! Code begins !
    !-------------!

    !-------------------!
    !- Initializations -!
    !-------------------!
    dimsorder(:) = 0
    dimx = 1
    dimy = 1
    dimz = 1
    dimt = 1

    icnt = 1 ! counter

    !- Dimension sum -!
    sumdims(1) = 1  ! 1+0+0+0
    sumdims(2) = 3  ! 1+2+0+0
    sumdims(3) = 6  ! 1+2+3+0
    sumdims(4) = 10 ! 1+2+3+4

    WRITE(lun_standard,*)  'TRIM(nc_var_name) = ', TRIM(nc_var_name)
    !-------------------------------------------------!
    !- Read the netcdf variable number of dimensions -!
    !-------------------------------------------------!
    CALL sub_read_netcdf_varid_ndims(ncid, TRIM(nc_var_name), varid, ndims)

    !---------------------------!
    !- allocate dims array and -!
    !- set dimsorder array.-!---!
    !-----------------------!
    IF ( (ndims > 0).AND.(ndims < 5)) THEN

      !----------------------!
      !- Dynamic allocation -!
      !----------------------!
      ALLOCATE(dims(ndims))
      !!NG : 19 06 2009 CALL sub_memory(size(dims),'i','dims','sub_select_var_dims')

      !------------------------------------------!
      !- Read netcdf variable dimension lengths -!
      !------------------------------------------!
      CALL sub_read_netcdf_var_dims(ncid, varid, ndims, dims)

      !----------------------------------------------------------------!
      !- DoLoop: Read below FUNCTION and EXAMPLES to understand these -!
      !-         lines. -!---------------------------------------------!
      !------------------!
      DO ii = 1, ndims

        !!NG        !-------!
        !!NG        !- IMT -!
        !!NG        !-------!
        !!NG        IF     ( (dims(ii) == imt).OR.(key_roms.AND.(dims(ii) == imt-1)) ) THEN
        !!NG          dimx = dims_reg(1,3)
        !!NG          dimsorder(1) = ii
        !!NG          !-------!
        !!NG          !- JMT -!
        !!NG          !-------!
        !!NG        ELSEIF ( (dims(ii) == jmt).OR.(key_roms.AND.(dims(ii) == jmt-1)) ) THEN
        !!NG          dimy = dims_reg(2,3)
        !!NG          dimsorder(2) = ii
        !!NG
        !!NG          !-------!
        !!NG          !- KMT -!
        !!NG          !-------!
        !!NG        ELSEIF ( (dims(ii) == kmt).OR.(key_roms.AND.(dims(ii) == kmt-1)) &
        !!NG             .OR.(key_symphonie.AND.(dims(ii) == kmt-1))) THEN
        !!NG          dimz = dims_reg(3,3)
        !!NG          dimsorder(3) = ii

        !-------!
        !- IMT -!
        !-------!
        IF  (                                                        &
             (dims(ii) == imt.AND.dimsorder(1) == 0)             .OR.&
             (key_roms.AND.dims(ii) == imt-1.AND.dimsorder(1) == 0) &
             ) THEN
          dimx = dims_reg(1,3)
          dimsorder(1) = ii

          !-------!
          !- JMT -!
          !-------!
        ELSEIF (                                                     &
             (dims(ii) == jmt.AND.dimsorder(2) == 0)            .OR.&
             (key_roms.AND.dims(ii) == jmt-1.AND.dimsorder(2) == 0) &
             ) THEN
          dimy = dims_reg(2,3)
          dimsorder(2) = ii

          !-------!
          !- KMT -!
          !-------!
        ELSEIF (PRESENT(key_ijt)) THEN

          !- We assume that the last dimension is time -!
          IF (ii == ndims) THEN
            !-------!
            !- LMT -!
            !-------!
            !!NG-idea: IF ( (dims(ii) < lmt).AND.(ind0_xx >= 0) ) THEN
            IF (dims(ii) < lmt) THEN
              dimt = dims(ii)
            ELSE
              dimt = lmt
            ENDIF

            dimsorder(4) = ii

            !----------------------------------------------------!
            !- Sometimes netcdf arrays were stored in 4D arrays -!
            !- with some dimensions set at 1 -!------------------!
            !- example: OPA 8.2/ORCA 2 -!-----!
            !---------------------------!
          ELSEIF (dims(ii) == 1 ) THEN
            dumdims(icnt) = ii
            icnt = icnt + 1
          ELSE
            WRITE(lun_error,*)'mod_netcdf: dimension problems in input files !'
            STOP
          ENDIF

        ELSE 

          IF((dims(ii) == kmt.AND.dimsorder(3) == 0)                      .OR.&
               (key_add_bottom.AND.dims(ii) == kmt-1.AND.dimsorder(3) == 0).OR.&
               (key_roms.AND.dims(ii) == kmt-1.AND.dimsorder(3) == 0)      .OR.&
               (key_symphonie.AND.dims(ii) == kmt-1.AND.dimsorder(3) == 0)     &
               ) THEN

            IF (key_add_bottom) THEN
              dimz = dims_reg(3,3)-1
            ELSE
              dimz = dims_reg(3,3)
            ENDIF
            dimsorder(3) = ii

          ELSE !!NG-bug: There was a bug here in version v1.2.1 !!

            !- We assume that the last dimension is time -!
            IF (ii == ndims) THEN
              !-------!
              !- LMT -!
              !-------!
              !!NG-idea: IF ( (dims(ii) < lmt).AND.(ind0_xx >= 0) ) THEN
              IF (dims(ii) < lmt) THEN
                dimt = dims(ii)
              ELSE
                dimt = lmt
              ENDIF

              dimsorder(4) = ii

              !----------------------------------------------------!
              !- Sometimes netcdf arrays were stored in 4D arrays -!
              !- with some dimensions set at 1 -!------------------!
              !- example: OPA 8.2/ORCA 2 -!-----!
              !---------------------------!
            ELSEIF (dims(ii) == 1 ) THEN
              dumdims(icnt) = ii
              icnt = icnt + 1
            ELSE
              WRITE(lun_error,*)'mod_netcdf: dimension problems in input files !'
              STOP
            ENDIF

          ENDIF

        ENDIF

      ENDDO

      !---------------------!
      !- Deallocate memory -!
      !---------------------!
      !!NG : 19 06 2009 CALL sub_memory(-size(dims),'i','dims','sub_select_var_dims')
      DEALLOCATE(dims)

    ELSE

      !-----------------------!
      !- IF ERROR or PROBLEM -!
      !-----------------------!
      STOP 'mod_netcdf: sub_select_var_dims: problem on ndims!'

    ENDIF

    !----------------------------------------------------!
    !- Sometimes netcdf arrays were stored in 4D arrays -!
    !- with some dimensions set at 1 -!------------------!
    !- example: OPA 8.2/ORCA 2 -!-----!
    !---------------------------!
    icnt = 1
    IF (  SUM(dimsorder) /= sumdims(ndims) ) THEN
      DO ii = 1, ndims
        IF (dimsorder(ii) == 0 ) THEN
          dimsorder(ii) = dumdims(icnt)
          icnt = icnt + 1
        ENDIF
      ENDDO
    ENDIF

  END SUBROUTINE sub_select_var_dims
  !!***
  !========================================================================
  !!****f* mod_netcdf/sub_read_netcdf_var()
  !! NAME
  !!   sub_read_netcdf_var()
  !!
  !! FUNCTION
  !!   Read netcdf data
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima (March 2007)
  !! 
  !! CREATION DATE
  !!   * September 2009
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * Inputs:
  !!      - ncid      : netcdf file ID
  !!      - varid     : netcdf varaible ID
  !!      - var       : a variable
  !!
  !!   * Output:
  !!      - var: a variable
  !!
  !! TODO
  !!   
  !! USES
  !!   * nf90_get_var (netcdf lib)
  !!
  !! USED BY
  !!   * mod_input_particules.f90
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_read_netcdf_var( ncid, varid, var)

    INTEGER(kind = iprec), INTENT(IN)  :: ncid
    INTEGER(kind = iprec), INTENT(IN)  :: varid
    REAL(kind = rprec)   , INTENT(OUT) :: var

    INTEGER(kind = iprec) :: is_err

    is_err = nf90_get_var( &
         ncid   = ncid   , &
         varid  = varid  , &
         values = var      &
         )

    !-------------------------!
    !- Test problem or error -!
    !-------------------------!
    IF (is_err /= nf90_noerr) THEN
      WRITE(lun_error,*) 'mod_netcdf: sub_read_netcdf_var: &
           &problem to read 1 value netcdf data !', is_err
      WRITE(lun_error,*) ' ncid   = ', ncid
      WRITE(lun_error,*) ' varid  = ', varid
      STOP 
    ENDIF

  END SUBROUTINE sub_read_netcdf_var

  !!***
  !========================================================================
  !!****f* mod_netcdf/sub_read_netcdf_var1d()
  !! NAME
  !!   sub_read_netcdf_var1d()
  !!
  !! FUNCTION
  !!   Read 1d netcdf data array and store it in a 1d array.
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima (March 2007)
  !! 
  !! CREATION DATE
  !!   * March 2007
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * Inputs:
  !!      - ncid      : netcdf file ID
  !!      - varid     : netcdf varaible ID
  !!      - var       : 1D array
  !!
  !!   * Output:
  !!      - var: a 1d dummy array
  !!
  !! TODO
  !!   
  !! USES
  !!   * nf90_get_var (netcdf lib)
  !!
  !! USED BY
  !!   * mod_input_particules.f90
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_read_netcdf_var1d(ncid, varid, var)

    INTEGER(kind = iprec), INTENT(IN) :: ncid
    INTEGER(kind = iprec), INTENT(IN) :: varid
    REAL(kind = rprec), DIMENSION(:), INTENT(OUT) :: var

    INTEGER(kind = iprec) :: is_err

    is_err = nf90_get_var( &
         ncid   = ncid   , &
         varid  = varid  , &
         values = var(:)   &
         )

    !-------------------------!
    !- Test problem or error -!
    !-------------------------!
    IF (is_err /= nf90_noerr) THEN
      WRITE(lun_error,*) 'mod_netcdf: sub_read_netcdf_var1d: &
           &problem to read 1D netcdf data !', is_err
      WRITE(lun_error,*) ' ncid   = ', ncid
      WRITE(lun_error,*) ' varid  = ', varid
      STOP 
    ENDIF

  END SUBROUTINE sub_read_netcdf_var1d

  !!***
  !========================================================================
  !!****f* mod_netcdf/sub_read_netcdf_var4d()
  !! NAME
  !!   sub_read_netcdf_var4d()
  !!
  !! FUNCTION
  !!   Read 1d, 2d, 3d or 4d netcdf data array and 
  !!   store it in a 4d array (Reshape: intrinsinc F90 function).
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
  !!   * Inputs:
  !!      - ncid      : netcdf file ID
  !!      - varid     : netcdf varaible ID
  !!      - dimsorder : variable dimension order
  !!      - dims_reg  : the region dimensions (see mod_reducmem.f90)
  !!
  !!   * Output:
  !!      - var: a 4d dummy array
  !!
  !! TODO
  !!   
  !! USES
  !!   * nf90_get_var (netcdf lib)
  !!   * Reshape      (F90 intrinsinc function)
  !!
  !! USED BY
  !!   * mod_input_data.f90
  !!   * mod_input_grid.f90
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_read_netcdf_var4d( ncid, varid, var, dimsorder, dims_reg, &
       ind_time)

    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind=iprec), INTENT(in)  :: ncid, varid
    REAL(kind=rprec)   , DIMENSION(:,:,:,:), INTENT(out) :: var
    INTEGER(kind=iprec), DIMENSION(4), INTENT(in) :: dimsorder
    INTEGER(kind=iprec), DIMENSION(4,3), INTENT(in) :: dims_reg
    INTEGER(kind=iprec), OPTIONAL :: ind_time


    !- local variables -!
    REAL(kind = rprec)                                  :: sfval
    REAL(kind = rprec)                                  :: aoval
    !!NG: 19 06 2009 REAL(kind = rprec), DIMENSION(:)      , ALLOCATABLE :: dumtab1d
    !!NG: 19 06 2009 REAL(kind = rprec), DIMENSION(:,:)    , ALLOCATABLE :: dumtab2d
    !!NG: 19 06 2009 REAL(kind = rprec), DIMENSION(:,:,:)  , ALLOCATABLE :: dumtab3d
    !!NG: 19 06 2009 REAL(kind = rprec), DIMENSION(:,:,:,:), ALLOCATABLE :: dumtab4d

    INTEGER(kind=iprec), DIMENSION(4)  :: nb_dims, st_dims
    INTEGER(kind=iprec), DIMENSION(4)  :: refsize, refstart, order
    INTEGER(kind=ishort) :: ii, is_err, nof, inc
    INTEGER(kind=ishort) :: limit, lim1, lim2, lim3
    INTEGER(kind=ishort) :: indmin11, indmax11, start11, count11
    INTEGER(kind=ishort) :: indmin21, indmax21, start21, count21
    INTEGER(kind=ishort) :: indmin31, indmax31, start31, count31
    INTEGER(kind=ishort) :: indmin41, indmax41, start41, count41
    INTEGER(kind=ishort) :: indmin12, indmax12, start12, count12
    INTEGER(kind=ishort) :: indmin22, indmax22, start22, count22
    INTEGER(kind=ishort) :: indmin32, indmax32, start32, count32
    INTEGER(kind=ishort) :: indmin42, indmax42, start42, count42

    !-------------!
    ! Code begins !
    !-------------!

    !-------------------!
    !- Initializations -!
    !-------------------!

    !------------------------------------------------!
    !- Get the reference size of the 4d array where -!
    !- netcdf data will be stored. -!----------------!
    !-------------------------------!
    refsize(1) = SIZE(var,DIM=1)
    refsize(2) = SIZE(var,DIM=2)
    refsize(3) = SIZE(var,DIM=3)
    refsize(4) = SIZE(var,DIM=4)

    !--------------------------------------!
    !- Get the first indice of the region -!
    !--------------------------------------!
    refstart(1) = dims_reg(1,1)
    refstart(2) = dims_reg(2,1)
    refstart(3) = dims_reg(3,1)
    IF (PRESENT(ind_time)) THEN
      refstart(4) = ind_time
    ELSE
      refstart(4) = dims_reg(4,1)
    ENDIF

    !-----------------------------------------------!
    !- The number of dimension of the netcdf array -!
    !- "nof" should be equal to 1, 2, 3, or 4 -!----!
    !------------------------------------------!
    nof = COUNT(dimsorder > 0)

    !- Initializations -!
    order(:) = dimsorder(:)
    nb_dims(:)=0
    st_dims(:)=0

    !------------------------------------------------------------------!
    !-In this DoLoop "nb_dims" and "order" arrays will be set.
    !- These information stored in these arrays is very important to
    !- reshape netcdf data arrays to the 4d Ariane arrays.
    !- examples:
    !-    - If dimsorder=/1,2,3,4/ then
    !-         nb_dims  =/imt_reg,jmt_reg,kmt_reg,lmt_reg/
    !-         or nb_dims  =/imt,jmt,kmt,lmt/
    !-         order    =/1,2,3,4/
    !-
    !-    - If dimsorder=/0,0,1,2/ then
    !-         nb_dims  =/kmt_reg,lmt_reg,0,0/
    !-         or nb_dims  =/kmt,lmt,0,0/
    !-         order    =/3,4,1,2/
    !-
    !-    - If dimsorder=/1,2,0,3/ then
    !-         nb_dims  =/imt_reg,jmt_reg,lmt_reg,0/
    !-         or nb_dims  =/imt,jmt,lmt,0/
    !-         order    =/1,2,4,3/
    !------------------------------------------------------------------!
    inc = nof + 1
    DO ii =1, 4
      IF ( dimsorder(ii) /= 0) THEN
        nb_dims(dimsorder(ii)) = refsize(ii)
        IF ( nb_dims(dimsorder(ii)) == 1 ) THEN
          st_dims(dimsorder(ii)) = 1
        ELSE
          st_dims(dimsorder(ii)) = refstart(ii)
        ENDIF
      ELSE
        order(ii) = inc
        inc = inc + 1
      ENDIF
    ENDDO

    IF (PRESENT(ind_time)) THEN
      st_dims(dimsorder(4)) = refstart(4)
    ENDIF

    !! NG: DEBUG
    !! write(*,*)'DEBUG: refsize: ',refsize
    !! write(*,*)'DEBUG: refstart: ',refstart
    !! write(*,*)'DEBUG: nb_dims: ',nb_dims
    !! NG:


    !---------------------------------------------------------------------!
    !- Get 1d, 2d, 3d or 4d netcdf data array and store it in a 4d array -!
    !---------------------------------------------------------------------!
    SELECT CASE(nof)


    CASE(1)
      !=============!
      !== 1D case ==!
      !=============!
      IF (.NOT.ALLOCATED(dumtab1d)) THEN
        ALLOCATE(dumtab1d(nb_dims(1)))
        CALL sub_memory(SIZE(dumtab1d),'r','dumtab1d','sub_read_netcdf_var4d')
      ELSEIF (SIZE(dumtab1d, dim=1) /= nb_dims(1)) THEN
        CALL sub_memory(-SIZE(dumtab1d),'r','dumtab1d','sub_read_netcdf_var4d')
        DEALLOCATE(dumtab1d)
        ALLOCATE(dumtab1d(nb_dims(1)))
        CALL sub_memory(SIZE(dumtab1d),'r','dumtab1d','sub_read_netcdf_var4d')
      ENDIF
      !-------------------------------------------------------!
      ! Test if the region is in two parts in i (longitude). -!-------------!
      ! In this case we read the netcdf data in two times (performance ?). -!
      !---------------------------------------------------------------------!
      IF ( (dimsorder(1) == 1) .AND. (dims_reg(1,1) > dims_reg(1,2)) ) THEN

        !------------------------------------------------------------!
        !---------------------------- 1D ----------------------------!
        !------------------------------------------------------------!
        !-                                                          -!
        !-       first                                    second    -!
        !-       part                                      part     -!
        !-     ========|---------------------------------|+++++     -!
        !-                                                          -!
        !-          data are stored =>  |+++++========|             -!
        !-                                                          -!
        !------------------------------------------------------------!

        limit = dims_reg(1,3) - dims_reg(1,2)

        !--------------!
        !- First part -!
        !--------------!
        is_err = nf90_get_var(             &
             ncid   = ncid               , &
             varid  = varid              , &
             values = dumtab1d(1:limit)  , &
             start  = (/ dims_reg(1,1) /), &
             count  = (/ limit /)          &
             )

        !-------------------------!
        !- Test problem or error -!
        !-------------------------!
        IF (is_err /= nf90_noerr) THEN
          WRITE(lun_error,*) 'mod_netcdf: sub_read_netcdf_var4d: problem to read 1D netcdf data!'
          WRITE(lun_error,*) ' NetCDF error  = ', is_err
          WRITE(lun_error,*) ' dims_reg(1,1) = ', dims_reg(1,1)
          WRITE(lun_error,*) ' limit         = ', limit
          STOP 
        ENDIF

        !---------------!
        !- second part -!
        !---------------!
        is_err = nf90_get_var(                      &
             ncid   = ncid                        , &
             varid  = varid                       , &
             values = dumtab1d(limit+1:nb_dims(1)), &
             start  = (/ 1 /)                     , &
             count  = (/ dims_reg(1,2)+1 /)         &
             )

        !-------------------------!
        !- Test problem or error -!
        !-------------------------!
        IF (is_err /= nf90_noerr) THEN
          WRITE(lun_error,*) 'mod_netcdf: sub_read_netcdf_var4d: problem to read 1D netcdf data!'
          WRITE(lun_error,*) ' NetCDF error    = ', is_err
          WRITE(lun_error,*) ' dims_reg(1,2)+1 = ', dims_reg(1,2)+1
          STOP 
        ENDIF

      ELSE

        !------------------------------------------------------------!
        !---------------------------- 1D ----------------------------!
        !------------------------------------------------------------!
        !-                                                          -!
        !-     ------------------|=============|---------------     -!
        !-                                                          -!
        !-          data are stored =>  |=============|             -!
        !-                                                          -!
        !------------------------------------------------------------!
        is_err = nf90_get_var(          &
             ncid   = ncid            , &
             varid  = varid           , &
             values = dumtab1d(:)     , &
             start  = (/ st_dims(1) /), &
             count  = (/ nb_dims(1) /)  &
             )

        !-------------------------!
        !- Test problem or error -!
        !-------------------------!
        IF (is_err /= nf90_noerr) THEN
          WRITE(lun_error,*) 'mod_netcdf: sub_read_netcdf_var4d: problem to read 1D netcdf data !'
          WRITE(lun_error,*) ' NetCDF error = ', is_err
          WRITE(lun_error,*) ' st_dims(1)   = ', st_dims(1)
          WRITE(lun_error,*) ' nb_dims(1)   = ', nb_dims(1)
          STOP 
        ENDIF

      ENDIF

      var(:,:,:,:) = RESHAPE(source = dumtab1d, shape = refsize, order = order)
      !!NG: 19 06 2009 CALL sub_memory(-size(dumtab1d),'r','dumtab1d','sub_select_var_dims')
      !!NG: 19 06 2009 DEALLOCATE(dumtab1d)

    CASE(2) 
      !=============!
      !== 2D case ==!
      !=============!
      IF (.NOT.ALLOCATED(dumtab2d)) THEN
        ALLOCATE(dumtab2d(nb_dims(1), nb_dims(2)))
        CALL sub_memory(SIZE(dumtab2d),'r','dumtab2d','sub_read_netcdf_var4d')
      ELSEIF ( &
           (SIZE(dumtab2d, dim=1) /= nb_dims(1)).OR. &
           (SIZE(dumtab2d, dim=2) /= nb_dims(2)) ) THEN
        CALL sub_memory(-SIZE(dumtab2d),'r','dumtab2d','sub_read_netcdf_var4d')
        DEALLOCATE(dumtab2d)
        ALLOCATE(dumtab2d(nb_dims(1), nb_dims(2)))
        CALL sub_memory(SIZE(dumtab2d),'r','dumtab2d','sub_read_netcdf_var4d')
      ENDIF
      ! Test if the region is in two parts in i (longitude).
      ! In this case we read the netcdf data in two times (performance ?).
      !
      IF ( (dimsorder(1) /= 0).AND.(dims_reg(1,1) > dims_reg(1,2)) ) THEN

        !------------------------------------------------------------!
        !---------------------------- 2D ----------------------------!
        !------------------------------------------------------------!
        !-                                                          -!
        !-     +----------------------------------------------+     -!
        !-     |                                              |     -!
        !-     |                                              |     -!
        !-     |                                              |     -!
        !-     |========                             +++++++++|     -!
        !-     |       |                             |        |     -!
        !-     |       |                             |        |     -!
        !-     |       |                             |        |     -!
        !-     |       |                             |        |     -!
        !-     |========                             +++++++++|     -!
        !-     |                                              |     -!
        !-     +----------------------------------------------+     -!
        !-                                                          -!
        !-                                  +++++++++========       -!
        !-      data are stored =>          |       |       |       -!
        !-                                  |       |       |       -!
        !-                                  |       |       |       -!
        !-                                  |       |       |       -!
        !-                                  +++++++++========       -!
        !-                                                          -!
        !------------------------------------------------------------!

        IF (nb_dims(1) /= 1 ) THEN
          lim1 = dims_reg(1,3) - dims_reg(1,2)
          lim2 = lim1 + 1
          lim3 = dims_reg(1,2)
        ELSE
          lim1 = 1
          lim2 = lim1
          lim3 = lim1
        ENDIF

        !-------------------!
        !- 2 cases :       -!
        !-     ->  i, j    -!
        !-     ->  j, i    -!
        !-------------------!
        IF (dimsorder(1) == 1) THEN
          indmin11 = 1         ; indmax11 = lim1 
          start11  = st_dims(1); count11  = lim1
          indmin21 = 1         ; indmax21 = nb_dims(2)
          start21  = st_dims(2); count21  = nb_dims(2)

          indmin12 = lim2      ; indmax12 = nb_dims(1)
          start12  = 1         ; count12  = lim3
          indmin22 = 1         ; indmax22 = nb_dims(2)
          start22  = st_dims(2); count22  = nb_dims(2)
        ELSE ! we assume that dimsorder(1) == 2
          indmin11 = 1         ; indmax11 = nb_dims(1)
          start11  = st_dims(1); count11  = nb_dims(1)
          indmin21 = 1         ; indmax21 = lim1
          start21  = st_dims(2); count21  = lim1

          indmin12 = 1         ; indmax12 = nb_dims(1)
          start12  = st_dims(1); count12  = nb_dims(1)
          indmin22 = lim2      ; indmax22 = nb_dims(2)
          start22  = 1         ; count22  = lim3
        ENDIF

        !--------------!
        !- First part -!
        !--------------!
        is_err = nf90_get_var(                &
             ncid   = ncid                  , &
             varid  = varid                 , &
             values = dumtab2d(indmin11:indmax11,indmin21:indmax21), &
             start  = (/start11, start21 /) , &
             count  = (/count11, count21  /)  &
             )

        !-------------------------!
        !- Test problem or error -!
        !-------------------------!
        IF (is_err /= nf90_noerr) THEN
          WRITE(lun_error,*) 'mod_netcdf: sub_read_netcdf_var4d: problem to read 2D netcdf data !'
          WRITE(lun_error,*) ' NetCDF error      = ', is_err
          WRITE(lun_error,*) ' start11, start21  = ', start11, start21
          WRITE(lun_error,*) ' count11, count21  = ', count11, count21
          STOP 
        ENDIF


        !---------------!
        !- second part -!
        !---------------!
        is_err = nf90_get_var(                      &
             ncid   = ncid                        , &
             varid  = varid                       , &
             values = dumtab2d(indmin12:indmax12,indmin22:indmax22), &
             start  = (/start12, start22 /)                     , &
             count  = (/count12, count22 /)         &
             )

        !-------------------------!
        !- Test problem or error -!
        !-------------------------!
        IF (is_err /= nf90_noerr) THEN
          WRITE(lun_error,*) 'mod_netcdf: sub_read_netcdf_var4d: problem to read 2D netcdf data !'
          WRITE(lun_error,*) ' NetCDF error      = ', is_err
          WRITE(lun_error,*) ' start12, start22  = ', start12, start22
          WRITE(lun_error,*) ' count12, count22  = ', count12, count22
          STOP 
        ENDIF

      ELSE

        !------------------------------------------------------------!
        !---------------------------- 2D ----------------------------!
        !------------------------------------------------------------!
        !-                                                          -!
        !-     +----------------------------------------------+     -!
        !-     |                                              |     -!
        !-     |                                              |     -!
        !-     |                                              |     -!
        !-     |            =================                 |     -!
        !-     |            |               |                 |     -!
        !-     |            |               |                 |     -!
        !-     |            |               |                 |     -!
        !-     |            =================                 |     -!
        !-     |                                              |     -!
        !-     |                                              |     -!
        !-     +----------------------------------------------+     -!
        !-                                                          -!
        !-                                  =================       -!
        !-      data are stored =>          |               |       -!
        !-                                  |               |       -!
        !-                                  |               |       -!
        !-                                  =================       -!
        !-                                                          -!
        !------------------------------------------------------------!
        is_err = nf90_get_var(                      &
             ncid   = ncid                        , &
             varid  = varid                       , &
             values = dumtab2d(:,:)               , &
             start  = (/ st_dims(1), st_dims(2) /), &
             count  = (/ nb_dims(1), nb_dims(2) /)  &
             )

        !-------------------------!
        !- Test problem or error -!
        !-------------------------!
        IF (is_err /= nf90_noerr) THEN
          WRITE(lun_error,*) 'mod_netcdf: sub_read_netcdf_var4d: problem to read 2D netcdf data !'
          WRITE(lun_error,*) ' NetCDF error            = ', is_err
          WRITE(lun_error,*) ' st_dims(1), st_dims(2)  = ', st_dims(1), st_dims(2)
          WRITE(lun_error,*) ' nb_dims(1), nb_dims(2)  = ', nb_dims(1), nb_dims(2)
          STOP 
        ENDIF

      ENDIF
      var(:,:,:,:) = RESHAPE(source = dumtab2d, shape = refsize, order = order)
      !!NG: 19 06 2009 CALL sub_memory(-size(dumtab2d),'r','dumtab2d','sub_read_netcdf_var4d')
      !!NG: 19 06 2009 DEALLOCATE(dumtab2d)


    CASE(3)
      !=============!
      !== 3D case ==!
      !=============!
      IF (.NOT.ALLOCATED(dumtab3d)) THEN
        ALLOCATE(dumtab3d(nb_dims(1), nb_dims(2), nb_dims(3)))
        CALL sub_memory(SIZE(dumtab3d),'r','dumtab3d','sub_read_netcdf_var4d')
      ELSEIF ( &
           (SIZE(dumtab3d, dim=1) /= nb_dims(1)).OR. &
           (SIZE(dumtab3d, dim=2) /= nb_dims(2)).OR. &
           (SIZE(dumtab3d, dim=3) /= nb_dims(3))) THEN
        CALL sub_memory(-SIZE(dumtab3d),'r','dumtab3d','sub_read_netcdf_var4d')
        DEALLOCATE(dumtab3d)
        ALLOCATE(dumtab3d(nb_dims(1), nb_dims(2), nb_dims(3)))
        CALL sub_memory(SIZE(dumtab3d),'r','dumtab3d','sub_read_netcdf_var4d')
      ENDIF
      ! Test if the region is in two parts in i (longitude).
      ! In this case we read the netcdf data in two times (performance ?).
      !
      IF ( (dimsorder(1) /= 0).AND.(dims_reg(1,1) > dims_reg(1,2)) ) THEN


        !------------------------------------------------------------!
        !---------------------------- 3D ----------------------------!
        !------------------------------------------------------------!
        !-        +----------------------------------------------+  -!
        !-       /                                              /|  -!
        !-      /                                              / |  -!
        !-     /                                              /  |  -!
        !-    /                                              /   +  -!
        !-   +----------------------------------------------+   /|  -!
        !-   |                                              |  / |  -!
        !-   |                                              | /  |  -!
        !-   |                                              |/   |  -!
        !-   |========                             ++++++++++    +  -!
        !-   |       |                             |        |   /|  -!
        !-   |       |                             |        |  / +  -!
        !-   |       |                             |        | / /   -!
        !-   |       |                             |        |/ /    -!
        !-   |========                             ++++++++++ /     -!
        !-   |                                              |/      -!
        !-   +----------------------------------------------+       -!
        !-                                                          -!
        !-                                       +++++++++=======+  -!
        !-                                      /       /       /|  -!
        !-          data are stored =>         /       /       / |  -!
        !-                                    /       /       /  |  -!
        !-                                   /       /       /   +  -!
        !-                                  +++++++++========    /  -!
        !-                                  |       |       |   /   -!
        !-                                  |       |       |  /    -!
        !-                                  |       |       | /     -!
        !-                                  |       |       |/      -!
        !-                                  +++++++++========       -!
        !-                                                          -!
        !------------------------------------------------------------!

        IF (nb_dims(1) /= 1 ) THEN
          lim1 = dims_reg(1,3) - dims_reg(1,2)
          lim2 = lim1 + 1
          lim3 = dims_reg(1,2)
        ELSE
          lim1 = 1
          lim2 = lim1
          lim3 = lim1
        ENDIF

        !------------------------------!
        !- 3 cases :                  -!
        !-     ->  i, j, k or i, k, j -!
        !-     ->  j, i, k or k, i, j -!
        !-     ->  j, k, i or k, j, i -!
        !------------------------------!
        IF (dimsorder(1) == 1) THEN
          indmin11 = 1         ; indmax11 = lim1  
          start11  = st_dims(1); count11  = lim1
          indmin21 = 1         ; indmax21 = nb_dims(2)
          start21  = st_dims(2); count21  = nb_dims(2)
          indmin31 = 1         ; indmax31 = nb_dims(3)
          start31  = st_dims(3); count31  = nb_dims(3)

          indmin12 = lim2      ; indmax12 = nb_dims(1)
          start12  = 1         ; count12  = lim3
          indmin22 = 1         ; indmax22 = nb_dims(2)
          start22  = st_dims(2); count22  = nb_dims(2)
          indmin32 = 1         ; indmax32 = nb_dims(3)
          start32  = st_dims(3); count32  = nb_dims(3)
        ELSEIF (dimsorder(1) == 2) THEN
          indmin11 = 1         ; indmax11 = nb_dims(1)
          start11  = st_dims(1); count11  = nb_dims(1)
          indmin21 = 1         ; indmax21 = lim1
          start21  = st_dims(2); count21  = lim1
          indmin31 = 1         ; indmax31 = nb_dims(3)
          start31  = st_dims(3); count31  = nb_dims(3)

          indmin12 = 1         ; indmax12 = nb_dims(1)
          start12  = st_dims(1); count12  = nb_dims(1)
          indmin22 = lim2      ; indmax22 = nb_dims(2)
          start22  = 1         ; count22  = lim3
          indmin32 = 1         ; indmax32 = nb_dims(3)
          start32  = st_dims(3); count32  = nb_dims(3)
        ELSE ! we assume that dimsorder(1) == 3
          indmin11 = 1         ; indmax11 = nb_dims(1)
          start11  = st_dims(1); count11  = nb_dims(1)
          indmin21 = 1         ; indmax21 = nb_dims(2)
          start21  = st_dims(2); count21  = nb_dims(2)
          indmin31 = 1         ; indmax31 = lim1
          start31  = st_dims(3); count31  = lim1

          indmin12 = 1         ; indmax12 = nb_dims(1)
          start12  = st_dims(1); count12  = nb_dims(1)
          indmin22 = 1         ; indmax22 = nb_dims(2)
          start22  = st_dims(2); count22  = nb_dims(2)
          indmin32 = lim2      ; indmax32 = nb_dims(3)
          start32  = 1         ; count32  = lim3
        ENDIF

        !--------------!
        !- First part -!
        !--------------!
        is_err = nf90_get_var(                        &
             ncid   = ncid                          , &
             varid  = varid                         , &
             values = dumtab3d(indmin11:indmax11    , &
             indmin21:indmax21    , &
             indmin31:indmax31)   , &
             start  = (/start11, start21, start31 /), &
             count  = (/count11, count21, count31 /)  &
             )

        !-------------------------!
        !- Test problem or error -!
        !-------------------------!
        IF (is_err /= nf90_noerr) THEN
          WRITE(lun_error,*) 'mod_netcdf: sub_read_netcdf_var4d: problem to read 3D netcdf data !'
          WRITE(lun_error,*) ' NetCDF error               = ', is_err
          WRITE(lun_error,*) ' start11, start21, start31  = ', start11, start21, start31
          WRITE(lun_error,*) ' count11, count21, count31  = ', count11, count21, count31
          STOP 
        ENDIF

        !---------------!
        !- second part -!
        !---------------!
        is_err = nf90_get_var(                        &
             ncid   = ncid                          , &
             varid  = varid                         , &
             values = dumtab3d(indmin12:indmax12    , &
             indmin22:indmax22    , &
             indmin32:indmax32)   , &
             start  = (/start12, start22, start32 /), &
             count  = (/count12, count22, count32 /)  &
             )

        !-------------------------!
        !- Test problem or error -!
        !-------------------------!
        IF (is_err /= nf90_noerr) THEN
          WRITE(lun_error,*) 'mod_netcdf: sub_read_netcdf_var4d: problem to read 3D netcdf data !'
          WRITE(lun_error,*) ' NetCDF error               = ', is_err
          WRITE(lun_error,*) ' start12, start22, start32  = ', start12, start22, start32
          WRITE(lun_error,*) ' count12, count22, start32  = ', count12, count22, start32
          STOP 
        ENDIF

      ELSE

        !------------------------------------------------------------!
        !---------------------------- 3D ----------------------------!
        !------------------------------------------------------------!
        !-        +----------------------------------------------+  -!
        !-       /                                              /|  -!
        !-      /                                              / |  -!
        !-     /                                              /  |  -!
        !-    /                                              /   |  -!
        !-   +----------------------------------------------+    |  -!
        !-   |                                              |    |  -!
        !-   |                                              |    |  -!
        !-   |                                              |    |  -!
        !-   |       *==============*                       |    |  -!
        !-   |       |              |                       |    |  -!
        !-   |       |              |                       |    +  -!
        !-   |       |              |                       |   /   -!
        !-   |       |              |                       |  /    -!
        !-   |       *==============*                       | /     -!
        !-   |                                              |/      -!
        !-   +----------------------------------------------+       -!
        !-                                                          -!
        !-                                       *===============*  -!
        !-                                      /               /|  -!
        !-          data are stored =>         /               / |  -!
        !-                                    /               /  |  -!
        !-                                   /               /   |  -!
        !-                                  *===============*    /  -!
        !-                                  |               |   /   -!
        !-                                  |               |  /    -!
        !-                                  |               | /     -!
        !-                                  |               |/      -!
        !-                                  *===============*       -!
        !-                                                          -!
        !------------------------------------------------------------!


        is_err = nf90_get_var(                                  &
             ncid   = ncid                                    , &
             varid  = varid                                   , &
             values = dumtab3d(:,:,:)                         , &
             start  = (/ st_dims(1), st_dims(2), st_dims(3) /), &
             count  = (/ nb_dims(1), nb_dims(2), nb_dims(3) /)  &
             )

        !-------------------------!
        !- Test problem or error -!
        !-------------------------!
        IF (is_err /= nf90_noerr) THEN
          WRITE(lun_error,*) 'mod_netcdf: sub_read_netcdf_var4d: problem to read 3D netcdf data !'
          WRITE(lun_error,*) ' NetCDF error                        = ', is_err
          WRITE(lun_error,*) ' st_dims(1), st_dims(2), st_dims(3)  = ', st_dims(1), st_dims(2), st_dims(3)
          WRITE(lun_error,*) ' nb_dims(1), nb_dims(2), nb_dims(3)  = ', nb_dims(1), nb_dims(2), nb_dims(3)
          STOP 
        ENDIF

      ENDIF

      var(:,:,:,:) = RESHAPE(source = dumtab3d, shape = refsize, order = order)
      !!NG: 19 06 2009 CALL sub_memory(-size(dumtab3d),'r','dumtab3d','sub_read_netcdf_var4d')
      !!NG: 19 06 2009 DEALLOCATE(dumtab3d)

    CASE(4)
      !=============!
      !== 4D case ==!
      !=============!

      IF (.NOT.ALLOCATED(dumtab4d)) THEN
        ALLOCATE(dumtab4d(nb_dims(1), nb_dims(2), nb_dims(3), nb_dims(4)))
        CALL sub_memory(SIZE(dumtab4d),'r','dumtab4d','sub_read_netcdf_var4d')
      ELSEIF ( &
           (SIZE(dumtab4d, dim=1) /= nb_dims(1)).OR. &
           (SIZE(dumtab4d, dim=2) /= nb_dims(2)).OR. &
           (SIZE(dumtab4d, dim=3) /= nb_dims(3)).OR. &
           (SIZE(dumtab4d, dim=4) /= nb_dims(4))) THEN
        CALL sub_memory(-SIZE(dumtab4d),'r','dumtab4d','sub_read_netcdf_var4d')
        DEALLOCATE(dumtab4d)
        ALLOCATE(dumtab4d(nb_dims(1), nb_dims(2),nb_dims(3),nb_dims(4)))
        CALL sub_memory(SIZE(dumtab4d),'r','dumtab4d','sub_read_netcdf_var4d')
      ENDIF
      ! Test if the region is in two parts in i (longitude).
      ! In this case we read the netcdf data in two times (performance ?).
      !
      IF ( (dimsorder(1) /= 0).AND.(dims_reg(1,1) > dims_reg(1,2)) ) THEN

        !------------------------------------------------------------!
        !------------------------ 3D  + TIME ------------------------!
        !------------------------------------------------------------!
        !-        +----------------------------------------------+  -!
        !-       /                                              /|  -!
        !-      /                                              / |  -!
        !-     /                                              /  |  -!
        !-    /                                              /   +  -!
        !-   +----------------------------------------------+   /|  -!
        !-   |                                              |  / |  -!
        !-   |                                              | /  |  -!
        !-   |                                              |/   |  -!
        !-   |========                             ++++++++++    +  -!
        !-   |       |                             |        |   /|  -!
        !-   |       |                             |        |  / +  -!
        !-   |       |                             |        | / /   -!
        !-   |       |                             |        |/ /    -!
        !-   |========                             ++++++++++ /     -!
        !-   |                                              |/      -!
        !-   +----------------------------------------------+       -!
        !-                                                          -!
        !-                                       +++++++++=======+  -!
        !-                                      /       /       /|  -!
        !-          data are stored =>         /       /       / |  -!
        !-                                    /       /       /  |  -!
        !-                                   /       /       /   +  -!
        !-                                  +++++++++========    /  -!
        !-                                  |       |       |   /   -!
        !-                                  |       |       |  /    -!
        !-                                  |       |       | /     -!
        !-                                  |       |       |/      -!
        !-                                  +++++++++========       -!
        !-                                                          -!
        !------------------------------------------------------------!


        IF (nb_dims(1) /= 1 ) THEN
          lim1 = dims_reg(1,3) - dims_reg(1,2)
          lim2 = lim1 + 1
          lim3 = dims_reg(1,2)
        ELSE
          lim1 = 1
          lim2 = lim1
          lim3 = lim1
        ENDIF

        !--------------------------------!
        !- 4 cases :                    -!
        !-     ->  i = first  dimension -!
        !-     ->  i = second dimension -!
        !-     ->  i = third  dimension -!
        !-     ->  i = fourth dimension -!
        !--------------------------------!
        IF (dimsorder(1) == 1) THEN
          indmin11 = 1         ; indmax11 = lim1
          start11  = st_dims(1); count11  = lim1
          indmin21 = 1         ; indmax21 = nb_dims(2)
          start21  = st_dims(2); count21  = nb_dims(2)
          indmin31 = 1         ; indmax31 = nb_dims(3)
          start31  = st_dims(3); count31  = nb_dims(3)
          indmin41 = 1         ; indmax41 = nb_dims(4)
          start41  = st_dims(4); count41  = nb_dims(4)

          indmin12 = lim2      ; indmax12 = nb_dims(1)
          start12  = 1         ; count12  = lim3
          indmin22 = 1         ; indmax22 = nb_dims(2)
          start22  = st_dims(2); count22  = nb_dims(2)
          indmin32 = 1         ; indmax32 = nb_dims(3)
          start32  = st_dims(3); count32  = nb_dims(3)
          indmin42 = 1         ; indmax42 = nb_dims(4)
          start42  = st_dims(4); count42  = nb_dims(4)
        ELSEIF (dimsorder(1) == 2) THEN
          indmin11 = 1         ; indmax11 = nb_dims(1)
          start11  = st_dims(1); count11  = nb_dims(1)
          indmin21 = 1         ; indmax21 = lim1
          start21  = st_dims(2); count21  = lim1
          indmin31 = 1         ; indmax31 = nb_dims(3)
          start31  = st_dims(3); count31  = nb_dims(3)
          indmin41 = 1         ; indmax41 = nb_dims(4)
          start41  = st_dims(4); count41  = nb_dims(4)

          indmin12 = 1         ; indmax12 = nb_dims(1)
          start12  = st_dims(1); count12  = nb_dims(1)
          indmin22 = lim2      ; indmax22 = nb_dims(2)
          start22  = 1         ; count22  = lim3
          indmin32 = 1         ; indmax32 = nb_dims(3)
          start32  = st_dims(3); count32  = nb_dims(3)
          indmin42 = 1         ; indmax42 = nb_dims(4)
          start42  = st_dims(4); count42  = nb_dims(4)
        ELSEIF (dimsorder(1) == 3) THEN
          indmin11 = 1         ; indmax11 = nb_dims(1)
          start11  = st_dims(1); count11  = nb_dims(1)
          indmin21 = 1         ; indmax21 = nb_dims(2)
          start21  = st_dims(2); count21  = nb_dims(2)
          indmin31 = 1         ; indmax31 = lim1
          start31  = st_dims(3); count31  = lim1
          indmin41 = 1         ; indmax41 = nb_dims(4)
          start41  = st_dims(4); count41  = nb_dims(4)

          indmin12 = 1         ; indmax12 = nb_dims(1)
          start12  = st_dims(1); count12  = nb_dims(1)
          indmin22 = 1         ; indmax22 = nb_dims(2)
          start22  = st_dims(2); count22  = nb_dims(2)
          indmin32 = lim2      ; indmax32 = nb_dims(3)
          start32  = 1         ; count32  = lim3
          indmin42 = 1         ; indmax42 = nb_dims(4)
          start42  = st_dims(4); count42  = nb_dims(4)
        ELSE ! we assume that dimsorder(1) == 4
          indmin11 = 1         ; indmax11 = nb_dims(1)
          start11  = st_dims(1); count11  = nb_dims(1)
          indmin21 = 1         ; indmax21 = nb_dims(2)
          start21  = st_dims(2); count21  = nb_dims(2)
          indmin31 = 1         ; indmax31 = nb_dims(3)
          start31  = st_dims(3); count31  = nb_dims(3)
          indmin41 = 1         ; indmax41 = lim1
          start41  = st_dims(4); count41  = lim1

          indmin12 = 1         ; indmax12 = nb_dims(1)
          start12  = st_dims(1); count12  = nb_dims(1)
          indmin22 = 1         ; indmax22 = nb_dims(2)
          start22  = st_dims(2); count22  = nb_dims(2)
          indmin32 = 1         ; indmax32 = nb_dims(3)
          start32  = st_dims(3); count32  = nb_dims(3)
          indmin42 = lim2      ; indmax42 = nb_dims(4)
          start42  = 1         ; count42  = lim3
        ENDIF

        !--------------!
        !- First part -!
        !--------------!
        is_err = nf90_get_var(                        &
             ncid   = ncid                          , &
             varid  = varid                         , &
             values = dumtab4d(indmin11:indmax11    , &
             indmin21:indmax21    , &
             indmin31:indmax31    , &
             indmin41:indmax41)   , &
             start  = (/start11, start21, start31, start41 /), &
             count  = (/count11, count21, count31, count41 /)  &
             )

        !-------------------------!
        !- Test problem or error -!
        !-------------------------!
        IF (is_err /= nf90_noerr) THEN
          WRITE(lun_error,*) 'mod_netcdf: sub_read_netcdf_var4d: problem to read 4D netcdf data !'
          WRITE(lun_error,*) ' NetCDF error                        = ', is_err
          WRITE(lun_error,*) ' start11, start21, start31, start41  = ', start11, start21, start31, start41
          WRITE(lun_error,*) ' count11, count21, count31, count41  = ', count11, count21, count31, count41
          STOP 
        ENDIF

        !---------------!
        !- second part -!
        !---------------!
        is_err = nf90_get_var(                        &
             ncid   = ncid                          , &
             varid  = varid                         , &
             values = dumtab4d(indmin12:indmax12    , &
             indmin22:indmax22    , &
             indmin32:indmax32    , &
             indmin42:indmax42)   , &
             start  = (/start12, start22, start32, start42 /), &
             count  = (/count12, count22, count32, count42 /)  &
             )

        !-------------------------!
        !- Test problem or error -!
        !-------------------------!
        IF (is_err /= nf90_noerr) THEN
          WRITE(lun_error,*) 'mod_netcdf: sub_read_netcdf_var4d: problem to read 4D netcdf data !'
          WRITE(lun_error,*) ' NetCDF error                        = ', is_err
          WRITE(lun_error,*) ' start12, start22, start32, start42  = ', start12, start22, start32, start42
          WRITE(lun_error,*) ' count12, count22, start32, count42  = ', count12, count22, start32, count42
          STOP 
        ENDIF

      ELSE

        !------------------------------------------------------------!
        !------------------------ 3D + TIME -------------------------!
        !------------------------------------------------------------!
        !-        +----------------------------------------------+  -!
        !-       /                                              /|  -!
        !-      /                                              / |  -!
        !-     /                                              /  |  -!
        !-    /                                              /   |  -!
        !-   +----------------------------------------------+    |  -!
        !-   |                                              |    |  -!
        !-   |                                              |    |  -!
        !-   |                                              |    |  -!
        !-   |       *==============*                       |    |  -!
        !-   |       |              |                       |    |  -!
        !-   |       |              |                       |    +  -!
        !-   |       |              |                       |   /   -!
        !-   |       |              |                       |  /    -!
        !-   |       *==============*                       | /     -!
        !-   |                                              |/      -!
        !-   +----------------------------------------------+       -!
        !-                                                          -!
        !-                                       *===============*  -!
        !-                                      /               /|  -!
        !-          data are stored =>         /               / |  -!
        !-                                    /               /  |  -!
        !-                                   /               /   |  -!
        !-                                  *===============*    /  -!
        !-                                  |               |   /   -!
        !-                                  |               |  /    -!
        !-                                  |               | /     -!
        !-                                  |               |/      -!
        !-                                  *===============*       -!
        !-                                                          -!
        !------------------------------------------------------------!

        is_err = nf90_get_var(                                              &
             ncid   = ncid                                                , &
             varid  = varid                                               , &
             values = dumtab4d(:,:,:,:)                                   , &
             start  = (/ st_dims(1), st_dims(2), st_dims(3), st_dims(4) /), &
             count  = (/ nb_dims(1), nb_dims(2), nb_dims(3), nb_dims(4) /)  &
             )

        !-------------------------!
        !- Test problem or error -!
        !-------------------------!
        IF (is_err /= nf90_noerr) THEN
          WRITE(lun_error,*) 'mod_netcdf: sub_read_netcdf_var4d: problem to read 4D netcdf data !'
          WRITE(lun_error,*) ' NetCDF error                                    = ', is_err
          WRITE(lun_error,*) ' st_dims(1), st_dims(2), st_dims(3), st_dims(4)  = ', &
               st_dims(1), st_dims(2), st_dims(3), st_dims(4)
          WRITE(lun_error,*) ' nb_dims(1), nb_dims(2), nb_dims(3), nb_dims(4)  = ', &
               nb_dims(1), nb_dims(2), nb_dims(3), nb_dims(4)
          STOP 
        ENDIF



      ENDIF

      !! Bug if shape(var) = shape(dumtab4d), i.e. refsize(:) = nb_dims(:)
      !! var(:,:,:,:) = RESHAPE(source = dumtab4d, shape = refsize, order = order)
      !! NG: 19/01/2009 write(lun_standard,*)'size var ',shape(var)
      !! NG: 19/01/2009 write(lun_standard,*)'size  dumtab4d',shape(dumtab4d)
      var(:,:,:,:) = dumtab4d(:,:,:,:)

      !!NG: 19 06 2009 CALL sub_memory(-size(dumtab4d),'r','dumtab4d','sub_read_netcdf_var4d')
      !!NG: 19 06 2009 DEALLOCATE(dumtab4d)

    CASE DEFAULT
      STOP
    END SELECT

    !---------------------------------------------------------!
    !- To reduce place some data are saved in integer 16bits -!
    !-            for example DRAKKAR - OPA 1/4 of degres    -!
    !-    Here data are first scaled and an offset is added  -!
    !---------------------------------------------------------!

    !-----------------------------------!
    !- Scale_factor is red or set to 1 -!
    !-----------------------------------!
    CALL sub_read_netcdf_att_val( &
         ncid                   , &
         varid                  , &
         'scale_factor'         , &
         sfval                  , &
         rOne                   )

    var(:,:,:,:) = var(:,:,:,:) * sfval

    !---------------------------------!
    !- Add_offset is red or set to 0 -!
    !---------------------------------!
    CALL sub_read_netcdf_att_val( &
         ncid                   , &
         varid                  , &
         'add_offset'           , &
         aoval                  , &
         rZero                  )

    var(:,:,:,:) = var(:,:,:,:) + aoval

  END SUBROUTINE sub_read_netcdf_var4d
  !!***

  SUBROUTINE sub_read_netcdf_4D_surf(ncid, varid, dumtab4D, dimx, dimy)

    INTEGER(kind = iprec), INTENT(in) :: ncid
    INTEGER(kind = iprec), INTENT(in) :: varid
    REAL(kind = rprec)   , DIMENSION(:,:,:,:), INTENT(out):: dumtab4D
    INTEGER(kind = iprec), INTENT(in) :: dimx
    INTEGER(kind = iprec), INTENT(in) :: dimy

    INTEGER(kind = iprec) :: is_err

    is_err = nf90_get_var          ( &
         ncid   = ncid             , &
         varid  = varid            , &
         values = dumtab4d(:,:,:,:), &
         start  = [1,1,1,1]        , &
         count  = [dimx,dimy,1,1]  )

    !-------------------------!
    !- Test problem or error -!
    !-------------------------!
    IF (is_err /= nf90_noerr) THEN
      WRITE(lun_error,*) 'mod_netcdf: sub_read_netcdf_4D_surf !'
      WRITE(lun_error,*) 'NetCDF error = ', is_err
      STOP 
    ENDIF

  END SUBROUTINE sub_read_netcdf_4D_surf

  !========================================================================
  !!****f* mod_netcdf/sub_read_netcdf_att_val()
  !! NAME
  !!   sub_read_netcdf_att_info()
  !!
  !! FUNCTION
  !!   Read  attribute information of a netcdf variable
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
  !!   * Inputs:
  !!      - ncid   : netcdf file ID
  !!      - varid  : netcdf variable ID
  !!      - attname: attribute name of the netcdf variable (ID)
  !!
  !!   * Output:
  !!      - attval: attribute value
  !!
  !! TODO
  !!   
  !! USES
  !!   * nf90_get_att (netcdf lib)
  !!
  !! USED BY
  !!   * mod_input_data.f90
  !!   * mod_input_grid.f90
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_read_netcdf_att_val(ncid, varid, attname, attval, valdef)

    !-------------!
    ! Declaration !
    !-------------!
    !- arguments -!
    INTEGER(kind=iprec), INTENT(in)  :: ncid
    INTEGER(kind=iprec), INTENT(in)  :: varid
    CHARACTER(len=*)   , INTENT(in)  :: attname
    REAL(kind=rprec)   , INTENT(out) :: attval
    REAL(kind=rprec)   , INTENT(in)  :: valdef

    !- local variables -!
    INTEGER(kind=ishort) :: is_err ! error code returns by netcdf function

    !-------------!
    ! Code begins !
    !-------------!
    !- Netcdf function which get an attribute value for a specified variable.
    is_err = nf90_get_att(  &
         ncid   = ncid    , &
         varid  = varid   , &
         name   = attname , &
         values = attval    &
         )

    !-----------------------------------------------------------!
    !- If the attribute doesn't exist set to the default value -!
    !-----------------------------------------------------------!
    IF (is_err /= NF90_NOERR) THEN
      attval = valdef
    ENDIF


  END SUBROUTINE sub_read_netcdf_att_val
  !!***

  !========================================================================
  !!****f* mod_netcdf/sub_read_netcdf_global_att()
  !! NAME
  !!   sub_read_netcdf_global_att()
  !!
  !! FUNCTION
  !!   Read ROMS Global Attributes hc sc_w and Cs_w
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima (November 2005)
  !! 
  !! CREATION DATE
  !!   * November 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * Input:
  !!
  !!   * Output:
  !!
  !! TODO
  !!   
  !! USES
  !!   * 
  !!
  !! USED BY
  !!   * mod_input_data.f90
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_read_netcdf_global_att( &
       c_dir, c_fn                   , &
       c_hc, c_sc_w, c_Cs_w          , &
       hc, sc_w, Cs_w)

    CHARACTER(len = *)              , INTENT(IN)  :: c_dir, c_fn
    CHARACTER(len = *)              , INTENT(IN)  :: c_hc, c_sc_w, c_Cs_w
    REAL(kind = rprec)              , INTENT(OUT) :: hc
    REAL(kind = rprec), DIMENSION(:), INTENT(OUT) :: sc_w, Cs_w


    INTEGER(kind = iprec) :: is_err       ! netcdf error code
    INTEGER(kind = iprec) :: ncid         ! netcdf file ID
    CALL sub_open_netcdf_file(c_dir, c_fn, ncid)

    !- HC -!
    is_err = nf90_get_att (    &
         ncid   = ncid       , &
         varid  = NF90_GLOBAL, &
         name   = c_hc       , &
         values = hc      )

    !-------------------------!
    !- Test problem or error -!
    !-------------------------!
    IF (is_err /= nf90_noerr) THEN
      WRITE(lun_error,*) 'mod_netcdf: sub_read_netcdf_global_att: &
           & problem to read ',TRIM(c_hc) 
      STOP 
    ENDIF

    !- SC_W -!
    is_err = nf90_get_att (                 &
         ncid   = ncid                    , &
         varid  = NF90_GLOBAL             , &
         name   = c_sc_w                  , &
         values = sc_w(dims_reg(3,3):1:-1)  )

    !-------------------------!
    !- Test problem or error -!
    !-------------------------!
    IF (is_err /= nf90_noerr) THEN
      WRITE(lun_error,*) 'mod_netcdf: sub_read_netcdf_global_att: &
           & problem to read ',TRIM(c_sc_w) 
      STOP 
    ENDIF

    !- CS_W -!
    is_err = nf90_get_att (                 &
         ncid   = ncid                    , &
         varid  = NF90_GLOBAL             , &
         name   = c_Cs_w                  , &
         values = cs_w(dims_reg(3,3):1:-1)  )

    !-------------------------!
    !- Test problem or error -!
    !-------------------------!
    IF (is_err /= nf90_noerr) THEN
      WRITE(lun_error,*) 'mod_netcdf: sub_read_netcdf_global_att: &
           & problem to read ',TRIM(c_Cs_w) 
      STOP 
    ENDIF


    !- Close netcdf file -!
    CALL sub_close_netcdf_file(ncid)

  END SUBROUTINE sub_read_netcdf_global_att
  !!***

  !========================================================================
  !!****f* mod_netcdf/sub_read_netcdf_global_att()
  !! NAME
  !!   sub_read_netcdf_global_att()
  !!
  !! FUNCTION
  !!   Read ROMS Global Attributes hc sc_w and Cs_w
  !!
  !! AUTHOR
  !!   * Origin  : Nicolas Grima (November 2005)
  !! 
  !! CREATION DATE
  !!   * November 2005
  !!
  !! HISTORY
  !!   Date (dd/mm/yyyy/) - Modification(s)
  !!
  !! ARGUMENTS
  !!   * Input:
  !!
  !!   * Output:
  !!
  !! TODO
  !!   
  !! USES
  !!   * 
  !!
  !! USED BY
  !!   * mod_input_data.f90
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_netcdf_dealloc_mem()

    IF (ALLOCATED(dumtab1d)) THEN
      CALL sub_memory(-SIZE(dumtab1d),'r','dumtab1d', &
           'sub_netcdf_dealloc_mem')
      DEALLOCATE(dumtab1d)
    END IF
    IF (ALLOCATED(dumtab2d)) THEN
      CALL sub_memory(-SIZE(dumtab2d),'r','dumtab2d', &
           'sub_netcdf_dealloc_mem')
      DEALLOCATE(dumtab2d)
    END IF
    IF (ALLOCATED(dumtab3d)) THEN
      CALL sub_memory(-SIZE(dumtab3d),'r','dumtab3d', &
           'sub_netcdf_dealloc_mem')
      DEALLOCATE(dumtab3d)
    END IF
    IF (ALLOCATED(dumtab4d)) THEN
      CALL sub_memory(-SIZE(dumtab4d),'r','dumtab4d', &
           'sub_netcdf_dealloc_mem')
      DEALLOCATE(dumtab4d)
    END IF

  END SUBROUTINE sub_netcdf_dealloc_mem


END MODULE mod_netcdf
