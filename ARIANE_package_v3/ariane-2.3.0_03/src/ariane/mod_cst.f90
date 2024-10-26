!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! - ARIANE - (January - 2010)
!! 
!! contact : nicolas.grima@univ-brest.fr
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
module mod_cst

  !===================================================================!
  !- All constants of the program have to be declared in this module -!
  !===================================================================!

  USE mod_precision

  !======================================!
  !- DECLARATIONS of GLOBAL VARIABLE(S) -!
  !======================================!
  IMPLICIT none

  REAL(kind = rprec)   , PARAMETER :: rZero   =  0.0_rprec
  REAL(kind = rprec)   , PARAMETER :: rHalf   =  0.5_rprec
  REAL(kind = rprec)   , PARAMETER :: rQuarter=  0.25_rprec
  REAL(kind = rprec)   , PARAMETER :: rOne    =  1.0_rprec
  REAL(kind = rprec)   , PARAMETER :: rTwo    =  2.0_rprec
  REAL(kind = rprec)   , PARAMETER :: rThree  =  3.0_rprec
  REAL(kind = rprec)   , PARAMETER :: rFour   =  4.0_rprec
  REAL(kind = rprec)   , PARAMETER :: rFive   =  5.0_rprec
  REAL(kind = rprec)   , PARAMETER :: rTen    = 10.0_rprec
  REAL(kind = rprec)   , PARAMETER :: rOneHundred =  100.0_rprec
  REAL(kind = rprec)   , PARAMETER :: rcircle = 360_rprec
  REAL(kind = rprec)   , PARAMETER :: rOneThousan = 1000.0_rprec
  REAL(kind = rprec)   , PARAMETER :: rMax    = 1.e20_rprec
  

  INTEGER(kind = iprec), PARAMETER :: iZero   =  0_iprec
  INTEGER(kind = iprec), PARAMETER :: iOne    =  1_iprec
  INTEGER(kind = iprec), PARAMETER :: iTwo    =  2_iprec
  INTEGER(kind = iprec), PARAMETER :: iThree  =  3_iprec
  INTEGER(kind = iprec), PARAMETER :: iFour   =  4_iprec
  INTEGER(kind = iprec), PARAMETER :: iFive   =  5_iprec
  INTEGER(kind = iprec), PARAMETER :: iSix    =  6_iprec
  INTEGER(kind = iprec), PARAMETER :: iSeven  =  7_iprec
  INTEGER(kind = iprec), PARAMETER :: iEight  =  8_iprec
  INTEGER(kind = iprec), PARAMETER :: iNine   =  9_iprec
  INTEGER(kind = iprec), PARAMETER :: iTen    = 10_iprec
  INTEGER(kind = iprec), PARAMETER :: iTwenty = 20_iprec
  INTEGER(kind = iprec), PARAMETER :: iHigh   = 9999999_iprec
  INTEGER(kind = iprec), PARAMETER :: iMega   = 1000000_iprec
  INTEGER(kind = iprec), PARAMETER :: iMaxSubDomEddies = iMega

  INTEGER(kind = iprec), PARAMETER :: nb_max_tracers = 50_iprec
  INTEGER(kind = iprec), PARAMETER :: pcat = iFour ! simon's cat www.simonscat.com

  REAL(kind = rprec), PARAMETER :: mask_value = 1.e20_rprec
  REAL(kind = rprec), PARAMETER :: missing_value = 1.e20_rprec

  INTEGER(kind=iprec), PARAMETER :: i_defp =  iOne
  INTEGER(kind=iprec), PARAMETER :: i_defn = -iOne
  INTEGER(kind=iprec), PARAMETER :: i_high = 9999999_iprec
  REAL(kind=rprec)   , PARAMETER :: r_def  = rZero
  CHARACTER(len = 4) , PARAMETER :: c_def  = 'NONE'
  LOGICAL            , PARAMETER :: l_def  = .FALSE.

  REAL(kind=rprec)   , PARAMETER :: pi = ACOS(-1._rprec)

  REAL(kind=rprec)   , PARAMETER :: earth_radius = 6378.137 ! at equator

end module mod_cst
