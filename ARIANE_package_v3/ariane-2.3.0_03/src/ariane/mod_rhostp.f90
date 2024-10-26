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
!!****h* ariane/mod_rhostp
!! NAME
!!   mod_rhostp (mod_rhostp.f90 - Fortran90 module)
!!
!! USAGE
!!   Include 'USE mod_rhostp' in the header of your Fortran 90 source 
!!   code.
!!   Then you'll have access to the function:
!!      - rhostp
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
!!   * i686-pc-linux-gnu -          g95
!!
!! SEE ALSO
!!   
!!
!! USES
!!   * USE mod_precision
!!
!! USED BY
!!   
!!
!! SOURCE
!!=========================================================================
MODULE mod_rhostp

  !------------------!
  ! USE ASSOCIAITION !
  !------------------!
  USE mod_precision

  !-------------!
  ! Declaration !
  !-------------!
  IMPLICIT NONE

CONTAINS
  !!***
  !=========================================================================
  !!****f* mod_rhostp/rhostp()
  !! NAME
  !!   rhostp()
  !!
  !! FUNCTION
  !!
  !!     computes potential density sigma_s,theta,z (relative to depth z)
  !!
  !!  New scheme using Jackett and McDougall s revised equation which
  !!  computes rho correctly when the input is potential temperature.
  !!  The UNESCO equation used above is strictly correct only when the
  !!  input is in-situ temperature, whereas the prognostic variable in spem
  !!  is potential temperature referrenced to the surface.  For more
  !!  details see the comments in rhocal.F.
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
  !!      - rho0: density
  !!      - s   : salinity
  !!      - t   : temperature
  !!      - z   : depth
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * mod_input_data.f90
  !!   * mod_sigma.f90
  !!
  !! SOURCE
  !!=======================================================================
  FUNCTION rhostp(rho0,s,t,z)

    USE mod_precision

    !-------------!
    ! DECLARATION !
    !-------------!
    IMPLICIT NONE
    !- arguments -!
    REAL(kind=rprec), INTENT(in) :: rho0,s,t,z

    !- local variables -!
    REAL(kind=rprec) :: rhostp
    REAL(kind=rprec) :: bulk

    bulk = 19092.56_rprec           + &
         t*(209.8925_rprec          - &
         t*(3.041638_rprec          - &
         t*(-1.852732e-3_rprec      - &
         t*(1.361629e-5_rprec))))   + &
         s*(104.4077_rprec          - &
         t*(6.500517_rprec          - &
         t*(.1553190_rprec          - &
         t*(-2.326469e-4_rprec))))  + &
         SQRT(s*s*s)                * &
         (-5.587545_rprec           + &
         t*(0.7390729_rprec         - &
         t*(1.909078e-2_rprec)))    - &
         z*(4.721788e-1_rprec       + &
         t*(1.028859e-2_rprec       + &
         t*(-2.512549e-4_rprec      - &
         t*(5.939910e-7_rprec))))   - &
         z*s*(-1.571896e-2_rprec    - &
         t*(2.598241e-4_rprec       + &
         t*(-7.267926e-6_rprec)))   - &
         z*SQRT(s*s*s)              * &
         2.042967e-3_rprec          + &
         z*z*(1.045941e-5_rprec     - &
         t*(5.782165e-10_rprec      - &
         t*(1.296821e-7_rprec)))    + &
         z*z*s *(-2.595994e-7_rprec + &
         t*(-1.248266e-9_rprec      + &
         t*(-3.508914e-9_rprec)))

    rhostp =   ( 999.842594_rprec         &
         + t * ( 6.793952e-2_rprec        &
         + t * (-9.095290e-3_rprec        &
         + t * ( 1.001685e-4_rprec        &
         + t * (-1.120083e-6_rprec        &
         + t * ( 6.536332e-9_rprec)))))   &
         + s * ( 0.824493_rprec           &
         + t * (-4.08990e-3_rprec         &
         + t * ( 7.64380e-5_rprec         &
         + t * (-8.24670e-7_rprec         &
         + t * ( 5.38750e-9_rprec)))))    &
         + SQRT(s*s*s)*(-5.72466e-3_rprec &
         + t*( 1.02270e-4_rprec           &
         + t*(-1.65460e-6_rprec)))        &
         + 4.8314e-4_rprec*s*s)           &
         / (1.0_rprec + 0.1_rprec*z/bulk) - rho0

    RETURN

  END FUNCTION rhostp
  !!***
END MODULE mod_rhostp
