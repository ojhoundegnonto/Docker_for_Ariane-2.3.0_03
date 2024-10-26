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
!!****h* ariane/mod_precision
!! NAME
!!   mod_precision (mod_precision.f90 - Fortran90 module)
!!
!! USAGE
!!   Include 'USE mod_precision' in the header of your Fortran 90 source 
!!   code.
!!
!! FUNCTION
!!   Specify Integer and Real precision. You could change all the variable
!!   of all the code from here ans only here.
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
!!   * No another USE
!!
!! USED BY
!!   * All the routines of the Ariane application
!!
!! SOURCE
!!=========================================================================
MODULE mod_precision

  !-------------!
  ! Declaration !
  !-------------!
  IMPLICIT NONE

  INTEGER(kind=4), PARAMETER :: &
       ishort=4               , & ! Here we decided that single precision is 4
       ilong =8                   ! and double precision is 8 
  !                               ! (IEEE norm, except some cray machines)

  INTEGER(kind=ishort), PARAMETER :: &
       iprec=ishort               ! We decided that single precsion for 
  !                               ! Integer variables is right.

  !- We use here the F90 intinsinc function KIND to determine the real precision -!
  INTEGER(kind=ishort), PARAMETER :: &
       rshort=KIND(1.0)            , & !  Real single precision
       rlong =KIND(1.0d0)              !  Real double precision

  !- Quad precision is defined here if it is required and available.
  !- Quad precision improve the stability of the results for the same
  !- compiler on different platforms (Intel versus AMD for example).
  !- Quad precision affects only the ttt variable in trajec.f90
  !- and trajec_seq.f90 files.
#if defined quad
  INTEGER(kind=ishort), PARAMETER :: qprec = selected_real_kind(p=33)
#else
  INTEGER(kind=ishort), PARAMETER :: qprec = rlong
#endif


  !- By default compilation will be made in double-precision for real variables. -!
  INTEGER(kind=ishort), PARAMETER :: &
       rprec=rlong ! If needed, you can change rlong by rshort (not tested).

END MODULE mod_precision
!!***
