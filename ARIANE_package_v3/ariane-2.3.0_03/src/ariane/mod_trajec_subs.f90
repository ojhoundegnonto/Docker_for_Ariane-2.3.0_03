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
MODULE mod_trajec_subs

  USE mod_precision
  USE mod_lun

CONTAINS
  !=========================================================================
  !!****f* ariane/sub_dont_reachside()
  !! NAME
  !!   sub_dont_reachside()
  !!
  !! FUNCTION
  !!   case 1: we do not reach the side of the cell (for a given direction)
  !!   diagnostic of the final position
  !!   - linear formulation, if Uexit = Ulocal
  !!   - exponential formulation, in other cases
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
  !!   *
  !!
  !! TODO
  !!   
  !!
  !! USED BY
  !!   * 
  !!
  !! SOURCE
  !!=======================================================================
  SUBROUTINE sub_dont_reachside(hh, delta, gg, vel_comp, ttt, imin, imax, np)
    INTEGER(kind=iprec), INTENT(in)  :: imin, imax
    REAL(kind=rprec)   , INTENT(in)  :: delta, gg, vel_comp
    REAL(kind=qprec)   , INTENT(in)  :: ttt
    REAL(kind=rprec)   , INTENT(out) :: hh
    INTEGER(kind = iprec) , OPTIONAL, INTENT(in) :: np

    !!NG: 31 may 2010 IF (delta /= 0._rprec) THEN
    IF (ABS(delta*ttt) >= 1.e-07_rprec) THEN
      ! BBL __________________________________________________________________________
      ! BBL BBL BBL: On utilise la formule *complexe* dans le cas general
      ! BBL

      hh = gg + vel_comp * ( EXP(delta*ttt) - 1._rprec ) / delta

      !!NG: To understand why quad precision is required.
      !!NG      IF (PRESENT(np)) THEN
      !!NG        IF (np <= 4 ) WRITE(12,*)np, hh, EXP(delta*ttt), delta, ttt
      !!NG      ENDIF
    ELSE
      ! BBL __________________________________________________________________________
      ! BBL BBL BBL: On utilise une relation *lineaire* dans le cas ou la composante
      ! BBL           zonale du transport est indentique sur les facettes d'entree et
      ! BBL           de sortie
      ! BBL

      !!NG: 31 may 2010 hh = gg + vel_comp * ttt
      hh = gg + vel_comp * ttt * ( 1._rprec + 0.5_rprec * delta * ttt )

      !! write(*,*)'mod_trajec_subs: sub_dont_reachside: delta=0', imin, hh, imax

    ENDIF

    IF ((hh < MIN(imin,imax)).OR.(hh > MAX(imin,imax))) THEN
      ! BBL __________________________________________________________________________
      ! BBL BBL BBL: La position zonale nouvellement calculee se doit d'etre dans la
      ! BBL           meme maille TEMPERATURE que precedemment
      ! BBL          Si ce n'est pas le cas, il s'agit a priori d'une erreur d'arrondi
      ! BBL            machine, qu'il faut corriger manuellement
      ! BBL          NOTE: ces erreurs ne se produisent a priori jamais quand le code
      ! BBL           est compile en DOUBLE PRECISION
      ! BBL
      WRITE(lun_error,*)'bad hh value...(now corrected):',imin,hh,imax
      !!       WRITE(lun_error,*)'n tx ty tz ttt: ',n,tx,ty,tz,ttt
      hh=ANINT(hh)
      !!NG may 2017
      IF (PRESENT(np)) THEN
        WRITE(lun_error,*)'New hh value:',np,hh,'-',gg,vel_comp,ttt,delta
      ELSE
        WRITE(lun_error,*)'New hh value:',hh,'-',gg,vel_comp,ttt,delta
      ENDIF
    ENDIF

  END SUBROUTINE sub_dont_reachside

END MODULE mod_trajec_subs
