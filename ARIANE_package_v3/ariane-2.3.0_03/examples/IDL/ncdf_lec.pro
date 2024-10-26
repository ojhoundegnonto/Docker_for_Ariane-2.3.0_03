;------------------------------------------------------------
;------------------------------------------------------------
;------------------------------------------------------------
;+
; NAME:ncdflec
;
; PURPOSE:donne des infos sur un fichier netcdf et permet de recupere
; les variables qui y sont ecrites 
;
; CATEGORY:lecture de fichiers netcdf
;
; CALLING SEQUENCE: res=ncdflec('nom_de _fichier')
;
; INPUTS:nom_de _fichier:nom d'un fichier net cdf situe ds e repertoire
; stipule par iodir
;
; KEYWORD PARAMETERS:
;
;	ATT: 'global' ou au nom d'une variable. permet de voir tous les
; attributs rattaches a une variable.
;
;	/DIM:donne la liste des dimensions
;
;	VAR:
;       1) /var: donne la liste des variables
;       2) var='nom de variable': ds ce cas la fonction retourne la variable
;
;       IODIR: string contenant le repertoire ou aller chercher le
;       fichier a lire.
;
;       _EXTRA: permet de passer les mots cles definits par IDL pour
;       les fonction NETCDF (en particulier OFFSET et COUNT ds ncdf_varget)
;
; OUTPUTS:-1 sauf si var='nom de variable' auquel cas la fonction retourne la variable
;
; REMARQUE:les noms des variables du programme sont similaires a ceux employes
; ds le manuel IDL 'scientific data formats'
;
; MODIFICATION HISTORY: Sebastien Masson (smasson@lodyc.jussieu.fr)
;                       4/1/98
;-
;------------------------------------------------------------
function ncdf_lec,nom,ATT=att,DIM=dim,VAR=var, IODIR = iodir, _extra = ex
   res = -1
;------------------------------------------------------------
   if NOT keyword_set(IODIR) then iodir = ''
   if not(keyword_set(att) or keyword_set(dim) or keyword_set(var)) then BEGIN
      att = 1
      dim = 1
      var = 1
;      commande='ncdump -c '+iodir+nom
;      spawn,commande
;      goto,fini
   endif
;------------------------------------------------------------
; ouverture du fichier nom
;------------------------------------------------------------
   cdfid=ncdf_open(iodir+nom)
;------------------------------------------------------------
; que contient le fichier??
;------------------------------------------------------------
   contient=ncdf_inquire(cdfid)
   print,'dans le fichier, ',iodir+nom,', il y a:'
   if keyword_set(dim) then begin 
      print,'nombre de dimensions: ',strtrim(contient.ndims,1)
      print,'numero de la dimension dont la valeur est infini: ',strtrim(contient.recdim,1)
   endif
   if keyword_set(var) then $
    print,'nombre de variables  :',strtrim(contient.nvars,1)
   if keyword_set(att) then begin
      if strlowcase(att) ne 'global' then goto,nonglobal
      print,'nombre de attributs globaux :',strtrim(contient.ngatts,1)
   endif
;------------------------------------------------------------
; attributs globaux
;------------------------------------------------------------
   if keyword_set(att) then begin
      print, '----------------------------'
      print,'ATTRIBUTS GLOBAUX'
      for attiq=0,contient.ngatts-1 do begin
         name=ncdf_attname(cdfid,attiq,/global) ;nom de l''atribut
         ncdf_attget,cdfid,name,value,/global ;valeur de l''atribut
         print,name,': ',string(value)
      endfor
   endif
nonglobal:
;------------------------------------------------------------
; affichage des differentes dimensions
;------------------------------------------------------------
   if keyword_set(dim) then begin 
      print, '----------------------------'
      print,'DIMENSIONS'
   endif
   nomdim   =strarr(contient.ndims)
   tailledim=lonarr(contient.ndims)
   for dimiq=0,contient.ndims-1 do begin
      ncdf_diminq,cdfid,dimiq,name,value ; nom et valeur de la dimension
      nomdim[dimiq]=name
      tailledim[dimiq]=value
      if keyword_set(dim) then begin 
         print,'dimension numero ',strtrim(dimiq,1),', nom: ',nomdim[dimiq] $
          ,', valeur: ' ,strtrim(tailledim[dimiq],1)
      endif
   endfor
;------------------------------------------------------------
; affichage des differentes variables
;------------------------------------------------------------
;
   if keyword_set(att) or keyword_set(var) then begin 
; vature de var ? string ou 1
   help, var, output = nature
   if (strpos(nature, 'STRING'))[0] NE -1 then nature = 'string' ELSE nature = '1'
; si on doit juste lire la variable
   if nature EQ 'string' then begin
      ncdf_varget, cdfid, var, res, _extra = ex
      GOTO, sortie
   ENDIF 
; si c''est pour avoir des renseignements
   if not keyword_set(att) then att='rien'
      print, '----------------------------'
      for varid=0,contient.nvars-1 do begin
         varcontient=ncdf_varinq(cdfid,varid) ; que contient la variable
         if strlowcase(att) eq strlowcase(varcontient.name) or keyword_set(var) $
          then begin
            print,'variable numero: ',strtrim(varid,1),', nom:',varcontient.name $
             ,', type:' ,varcontient.datatype,', dimensions:',nomdim(varcontient.dim)
            if strlowcase(att) eq strlowcase(varcontient.name) then begin
               for attiq=0,varcontient.natts-1 do begin
                  name=ncdf_attname(cdfid,varid,attiq)
                  ncdf_attget,cdfid,varid,name,value
                  print,'     ',strtrim(attiq),' ',name,': ',strtrim(string(value),1)
               endfor
               goto, sortie
            endif
         endif
      endfor
   endif
;------------------------------------------------------------
sortie:
   ncdf_close,cdfid
;------------------------------------------------------------
;------------------------------------------------------------
fini:

   return, res
end
