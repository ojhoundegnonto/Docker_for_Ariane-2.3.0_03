; calcul de la fonction de courant associee aux ecoulements de masse...
;
; construction du masque pour psi
;
; on cherche d'abord un point psi du domaine couvert par psi
;
; calcul du champ de divergence
print,'computing divergence...'
div=dblarr(imt,jmt,pmt+1)
for j=1,jmt-1 do begin
 for i=1,imt-1 do begin
  div(i,j,*)=uxy(i,j,*)-uxy(i-1,j,*)+vxy(i,j,*)-vxy(i,j-1,*)
 endfor
endfor
divmax=max(abs(div))

if iperio eq 1 then begin
 div(0,*,*)=div(imt-2,*,*)
endif

print,'looking for one active psi point...'
; reperage d'un point psi "actif"
for pp=pmt,0,-1 do begin
 for j=0,jmt-2 do begin
  for i=0,imt-2 do begin
   if abs(div(i,j,pp)) le divmax and abs(div(i+1,j,pp)) le divmax and abs(div(i,j+1,pp)) le divmax and abs(div(i+1,j+1,pp)) le divmax then begin
    if uxy(i,j,pp) ne 0 and vxy(i,j,pp) ne 0 and uxy(i,j+1,pp) ne 0 and vxy(i+1,j,pp) ne 0 then begin
     ip0=i
     jp0=j
     p0=pp
     i=imt-2
     j=jmt-2
     pp=0
    endif
   endif
  endfor
 endfor
endfor

;certains points posent probleme (comme l'amerique centrale)
print,'computing diagnonal land connections...'
ipb=intarr(imt,jmt)
for j=0,jmt-2 do begin
 for i=0,imt-2 do begin
  if (mt(i,j) eq mt(i+1,j+1)) and (mt(i,j+1) eq mt(i+1,j)) then begin
   if mt(i,j)+mt(i+1,j) eq 1 then begin
    ipb(i,j)=1
   endif
  endif
 endfor
endfor
if iperio eq 1 then begin
 ipb(0,*)=ipb(0,*) > ipb(imt-2,*)
 ipb(1,*)=ipb(1,*) > ipb(imt-1,*)
 ipb(imt-2,*)=ipb(0,*)
 ipb(imt-1,*)=ipb(1,*)
endif
if jperio eq 1 then begin
 for i=0,imt/2-1 do begin
  ipb(i,jmt-4)=ipb(i,jmt-4) > ipb(imt-i-1,jmt-1)
  ipb(i,jmt-3)=ipb(i,jmt-3) > ipb(imt-i-1,jmt-2)
  ipb(i,jmt-2)=ipb(i,jmt-2) > ipb(imt-i-1,jmt-3)
  ipb(i,jmt-1)=ipb(i,jmt-1) > ipb(imt-i-1,jmt-4)
  ipb(imt-i-1,jmt-1)=ipb(i,jmt-4)
  ipb(imt-i-1,jmt-2)=ipb(i,jmt-3)
  ipb(imt-i-1,jmt-3)=ipb(i,jmt-2)
  ipb(imt-i-1,jmt-4)=ipb(i,jmt-1)
 endfor
endif

;une fois trouve un point psi actif, construction du masque de psi
mp=intarr(imt,jmt)
mp(ip0,jp0)=1

print,'computing psi mask...'
mpold=0
totmp=1
while totmp gt mpold do begin
 print,mpold,totmp
 mpold=totmp
 for j=1,jmt-2 do begin
  for i=1,imt-2 do begin
   if ipb(i,j) eq 0 and mp(i,j) eq 1 then begin
    if mp(i+1,j) eq 0 and (mt2(i+1,j) eq 1 or mt2(i+1,j+1) eq 1) then begin
     mp(i+1,j)=1
    endif
    if mp(i-1,j) eq 0 and (mt2(i,j) eq 1 or mt2(i,j+1) eq 1) then begin
     mp(i-1,j)=1
    endif
    if mp(i,j+1) eq 0 and (mt2(i,j+1) eq 1 or mt2(i+1,j+1) eq 1) then begin
     mp(i,j+1)=1
    endif
    if mp(i,j-1) eq 0 and (mt2(i,j) eq 1 or mt2(i+1,j) eq 1) then begin
     mp(i,j-1)=1
    endif
   endif
  endfor
 endfor

 for j=jmt-2,1,-1 do begin
  for i=1,imt-2 do begin
   if ipb(i,j) eq 0 and mp(i,j) eq 1 then begin
    if mp(i+1,j) eq 0 and (mt2(i+1,j) eq 1 or mt2(i+1,j+1) eq 1) then begin
     mp(i+1,j)=1
    endif
    if mp(i-1,j) eq 0 and (mt2(i,j) eq 1 or mt2(i,j+1) eq 1) then begin
     mp(i-1,j)=1
    endif
    if mp(i,j+1) eq 0 and (mt2(i,j+1) eq 1 or mt2(i+1,j+1) eq 1) then begin
     mp(i,j+1)=1
    endif
    if mp(i,j-1) eq 0 and (mt2(i,j) eq 1 or mt2(i+1,j) eq 1) then begin
     mp(i,j-1)=1
    endif
   endif
  endfor
 endfor
 for j=1,jmt-2 do begin
  for i=imt-2,1,-1 do begin
   if ipb(i,j) eq 0 and mp(i,j) eq 1 then begin
    if mp(i+1,j) eq 0 and (mt2(i+1,j) eq 1 or mt2(i+1,j+1) eq 1) then begin
     mp(i+1,j)=1
    endif
    if mp(i-1,j) eq 0 and (mt2(i,j) eq 1 or mt2(i,j+1) eq 1) then begin
     mp(i-1,j)=1
    endif
    if mp(i,j+1) eq 0 and (mt2(i,j+1) eq 1 or mt2(i+1,j+1) eq 1) then begin
     mp(i,j+1)=1
    endif
    if mp(i,j-1) eq 0 and (mt2(i,j) eq 1 or mt2(i+1,j) eq 1) then begin
     mp(i,j-1)=1
    endif
   endif
  endfor
 endfor
 for j=jmt-2,1,-1 do begin
  for i=imt-2,1,-1 do begin
   if ipb(i,j) eq 0 and mp(i,j) eq 1 then begin
    if mp(i+1,j) eq 0 and (mt2(i+1,j) eq 1 or mt2(i+1,j+1) eq 1) then begin
     mp(i+1,j)=1
    endif
    if mp(i-1,j) eq 0 and (mt2(i,j) eq 1 or mt2(i,j+1) eq 1) then begin
     mp(i-1,j)=1
    endif
    if mp(i,j+1) eq 0 and (mt2(i,j+1) eq 1 or mt2(i+1,j+1) eq 1) then begin
     mp(i,j+1)=1
    endif
    if mp(i,j-1) eq 0 and (mt2(i,j) eq 1 or mt2(i+1,j) eq 1) then begin
     mp(i,j-1)=1
    endif
   endif
  endfor
 endfor
 if iperio eq 1 then begin
  mp(0,*)=mp(0,*) > mp(imt-2,*)
  mp(1,*)=mp(1,*) > mp(imt-1,*)
  mp(imt-2,*)=mp(0,*)
  mp(imt-1,*)=mp(1,*)
 endif
 if jperio eq 1 then begin
  for i=0,imt/2-1 do begin
   mp(i,jmt-4)=mp(i,jmt-4) > mp(imt-i-1,jmt-1)
   mp(i,jmt-3)=mp(i,jmt-3) > mp(imt-i-1,jmt-2)
   mp(i,jmt-2)=mp(i,jmt-2) > mp(imt-i-1,jmt-3)
   mp(i,jmt-1)=mp(i,jmt-1) > mp(imt-i-1,jmt-4)
   mp(imt-i-1,jmt-1)=mp(i,jmt-4)
   mp(imt-i-1,jmt-2)=mp(i,jmt-3)
   mp(imt-i-1,jmt-3)=mp(i,jmt-2)
   mp(imt-i-1,jmt-4)=mp(i,jmt-1)
  endfor
 endif
 totmp=round(total(mp))
endwhile

;maintenant que l'on conait le masque pour psi, on peut commencer l'integration
psi=dblarr(imt,jmt,pmt+1)-1.e12
ipsi=intarr(imt,jmt)

print,'setting reference value for psi...'
iref=intarr(imt,jmt)
if n_elements(ii) eq 0 then begin
 print,'interactive selection for psi reference has not been activated...'
 stop
endif

iref(ii,jj)=1
irefold=0
totiref=1
while totiref ne irefold do begin
 print,irefold,totiref
 irefold=totiref
 for j=1,jmt-2 do begin
  for i=1,imt-2 do begin
   if iref(i,j) eq 1 then begin
    if iref(i+1,j) eq 0 and mt(i+1,j+1)+mt(i+1,j) eq 0 then begin
     iref(i+1,j)=1
    endif
    if iref(i-1,j) eq 0 and mt(i,j+1)+mt(i,j) eq 0 then begin
     iref(i-1,j)=1
    endif
    if iref(i,j+1) eq 0 and mt(i+1,j+1)+mt(i,j+1) eq 0 then begin
     iref(i,j+1)=1
    endif
    if iref(i,j-1) eq 0 and mt(i+1,j)+mt(i,j) eq 0 then begin
     iref(i,j-1)=1
    endif
   endif
  endfor
 endfor
 for j=jmt-2,1,-1 do begin
  for i=1,imt-2 do begin
   if iref(i,j) eq 1 then begin
    if iref(i+1,j) eq 0 and mt(i+1,j+1)+mt(i+1,j) eq 0 then begin
     iref(i+1,j)=1
    endif
    if iref(i-1,j) eq 0 and mt(i,j+1)+mt(i,j) eq 0 then begin
     iref(i-1,j)=1
    endif
    if iref(i,j+1) eq 0 and mt(i+1,j+1)+mt(i,j+1) eq 0 then begin
     iref(i,j+1)=1
    endif
    if iref(i,j-1) eq 0 and mt(i+1,j)+mt(i,j) eq 0 then begin
     iref(i,j-1)=1
    endif
   endif
  endfor
 endfor
 for j=1,jmt-2 do begin
  for i=imt-2,1,-1 do begin
   if iref(i,j) eq 1 then begin
    if iref(i+1,j) eq 0 and mt(i+1,j+1)+mt(i+1,j) eq 0 then begin
     iref(i+1,j)=1
    endif
    if iref(i-1,j) eq 0 and mt(i,j+1)+mt(i,j) eq 0 then begin
     iref(i-1,j)=1
    endif
    if iref(i,j+1) eq 0 and mt(i+1,j+1)+mt(i,j+1) eq 0 then begin
     iref(i,j+1)=1
    endif
    if iref(i,j-1) eq 0 and mt(i+1,j)+mt(i,j) eq 0 then begin
     iref(i,j-1)=1
    endif
   endif
  endfor
 endfor
 for j=jmt-2,1,-1 do begin
  for i=imt-2,1,-1 do begin
   if iref(i,j) eq 1 then begin
    if iref(i+1,j) eq 0 and mt(i+1,j+1)+mt(i+1,j) eq 0 then begin
     iref(i+1,j)=1
    endif
    if iref(i-1,j) eq 0 and mt(i,j+1)+mt(i,j) eq 0 then begin
     iref(i-1,j)=1
    endif
    if iref(i,j+1) eq 0 and mt(i+1,j+1)+mt(i,j+1) eq 0 then begin
     iref(i,j+1)=1
    endif
    if iref(i,j-1) eq 0 and mt(i+1,j)+mt(i,j) eq 0 then begin
     iref(i,j-1)=1
    endif
   endif
  endfor
 endfor
 if iperio eq 1 then begin
  iref(0,*)=iref(0,*) > iref(imt-2,*)
  iref(1,*)=iref(1,*) > iref(imt-1,*)
  iref(imt-2,*)=iref(0,*)
  iref(imt-1,*)=iref(1,*)
 endif
 if jperio eq 1 then begin
  for i=1,imt/2-1 do begin
   iref(i,jmt-4)=iref(i,jmt-4) > iref(imt-i-1,jmt-1)
   iref(i,jmt-3)=iref(i,jmt-3) > iref(imt-i-1,jmt-2)
   iref(i,jmt-2)=iref(i,jmt-2) > iref(imt-i-1,jmt-3)
   iref(i,jmt-1)=iref(i,jmt-1) > iref(imt-i-1,jmt-4)
   iref(imt-i-1,jmt-1)=iref(i,jmt-4)
   iref(imt-i-1,jmt-2)=iref(i,jmt-3)
   iref(imt-i-1,jmt-3)=iref(i,jmt-2)
   iref(imt-i-1,jmt-4)=iref(i,jmt-1)
  endfor
 endif
 totiref=round(total(iref))
endwhile

ip1=-1
jp1=-1
for j=0,jmt-1 do begin
 for i=0,imt-1 do begin
  if iref(i,j) eq 1 and mp(i,j) eq 1 then begin
   ip1=i
   jp1=j
   i=imt-1
   j=jmt-1
  endif
 endfor
endfor

if ip1 lt 0 then begin
 print,'reference value for psi does not match active domain bounds...'
 stop
endif

psi(ip1,jp1,*)=0.
ipsi(ip1,jp1)=1
totipsi=1

print,'computing psi...'
while totipsi lt mpold do begin
 print,totipsi,mpold
 for j=1,jmt-2 do begin
  for i=1,imt-2 do begin
   if ipsi(i,j) eq 1 then begin
    if ipsi(i+1,j) eq 0 and mp(i+1,j) eq 1 then begin
     psi(i+1,j,*)=psi(i,j,*)+vxy(i+1,j,*)
     ipsi(i+1,j)=1
    endif
    if ipsi(i-1,j) eq 0 and mp(i-1,j) eq 1 then begin
     psi(i-1,j,*)=psi(i,j,*)-vxy(i,j,*)
     ipsi(i-1,j)=1
    endif
    if ipsi(i,j+1) eq 0 and mp(i,j+1) eq 1 then begin
     psi(i,j+1,*)=psi(i,j,*)-uxy(i,j+1,*)
     ipsi(i,j+1)=1
    endif
    if ipsi(i,j-1) eq 0 and mp(i,j-1) eq 1 then begin
     psi(i,j-1,*)=psi(i,j,*)+uxy(i,j,*)
     ipsi(i,j-1)=1
    endif
   endif
  endfor
 endfor
 for j=jmt-2,1,-1 do begin
  for i=1,imt-2 do begin
   if ipsi(i,j) eq 1 then begin
    if ipsi(i+1,j) eq 0 and mp(i+1,j) eq 1 then begin
     psi(i+1,j,*)=psi(i,j,*)+vxy(i+1,j,*)
     ipsi(i+1,j)=1
    endif
    if ipsi(i-1,j) eq 0 and mp(i-1,j) eq 1 then begin
     psi(i-1,j,*)=psi(i,j,*)-vxy(i,j,*)
     ipsi(i-1,j)=1
    endif
    if ipsi(i,j+1) eq 0 and mp(i,j+1) eq 1 then begin
     psi(i,j+1,*)=psi(i,j,*)-uxy(i,j+1,*)
     ipsi(i,j+1)=1
    endif
    if ipsi(i,j-1) eq 0 and mp(i,j-1) eq 1 then begin
     psi(i,j-1,*)=psi(i,j,*)+uxy(i,j,*)
     ipsi(i,j-1)=1
    endif
   endif
  endfor
 endfor
 for j=1,jmt-2 do begin
  for i=imt-2,1,-1 do begin
   if ipsi(i,j) eq 1 then begin
    if ipsi(i+1,j) eq 0 and mp(i+1,j) eq 1 then begin
     psi(i+1,j,*)=psi(i,j,*)+vxy(i+1,j,*)
     ipsi(i+1,j)=1
    endif
    if ipsi(i-1,j) eq 0 and mp(i-1,j) eq 1 then begin
     psi(i-1,j,*)=psi(i,j,*)-vxy(i,j,*)
     ipsi(i-1,j)=1
    endif
    if ipsi(i,j+1) eq 0 and mp(i,j+1) eq 1 then begin
     psi(i,j+1,*)=psi(i,j,*)-uxy(i,j+1,*)
     ipsi(i,j+1)=1
    endif
    if ipsi(i,j-1) eq 0 and mp(i,j-1) eq 1 then begin
     psi(i,j-1,*)=psi(i,j,*)+uxy(i,j,*)
     ipsi(i,j-1)=1
    endif
   endif
  endfor
 endfor
 for j=jmt-2,1,-1 do begin
  for i=imt-2,1,-1 do begin
   if ipsi(i,j) eq 1 then begin
    if ipsi(i+1,j) eq 0 and mp(i+1,j) eq 1 then begin
     psi(i+1,j,*)=psi(i,j,*)+vxy(i+1,j,*)
     ipsi(i+1,j)=1
    endif
    if ipsi(i-1,j) eq 0 and mp(i-1,j) eq 1 then begin
     psi(i-1,j,*)=psi(i,j,*)-vxy(i,j,*)
     ipsi(i-1,j)=1
    endif
    if ipsi(i,j+1) eq 0 and mp(i,j+1) eq 1 then begin
     psi(i,j+1,*)=psi(i,j,*)-uxy(i,j+1,*)
     ipsi(i,j+1)=1
    endif
    if ipsi(i,j-1) eq 0 and mp(i,j-1) eq 1 then begin
     psi(i,j-1,*)=psi(i,j,*)+uxy(i,j,*)
     ipsi(i,j-1)=1
    endif
   endif
  endfor
 endfor
 if iperio eq 1 then begin
  for j=0,jmt-1 do begin
   if ipsi(0,j) eq 0 and ipsi(imt-2,j) eq 1 then begin
    ipsi(0,j)=1
    psi(0,j,*)=psi(imt-2,j,*)
   endif
   if ipsi(0,j) eq 1 and ipsi(imt-2,j) eq 0 then begin
    ipsi(imt-2,j)=1
    psi(imt-2,j,*)=psi(0,j,*)
   endif
   if ipsi(1,j) eq 0 and ipsi(imt-1,j) eq 1 then begin
    ipsi(1,j)=1
    psi(1,j,*)=psi(imt-1,j,*)
   endif
   if ipsi(1,j) eq 1 and ipsi(imt-1,j) eq 0 then begin
    ipsi(imt-1,j)=1
    psi(imt-1,j,*)=psi(1,j,*)
   endif
  endfor
 endif
 if jperio eq 1 then begin
  for i=0,imt/2-1 do begin
   if ipsi(i,jmt-4) eq 0 and ipsi(imt-i-1,jmt-1) eq 1 then begin
    ipsi(i,jmt-4)=1
    psi(i,jmt-4,*)=psi(imt-i-1,jmt-1,*)
   endif
   if ipsi(i,jmt-4) eq 1 and ipsi(imt-i-1,jmt-1) eq 0 then begin
    ipsi(imt-i-1,jmt-1)=1
    psi(imt-i-1,jmt-1,*)=psi(i,jmt-4,*)
   endif
   if ipsi(i,jmt-3) eq 0 and ipsi(imt-i-1,jmt-2) eq 1 then begin
    ipsi(i,jmt-3)=1
    psi(i,jmt-3,*)=psi(imt-i-1,jmt-2,*)
   endif
   if ipsi(i,jmt-3) eq 1 and ipsi(imt-i-1,jmt-2) eq 0 then begin
    ipsi(imt-i-1,jmt-2)=1
    psi(imt-i-1,jmt-2,*)=psi(i,jmt-3,*)
   endif
   if ipsi(i,jmt-2) eq 0 and ipsi(imt-i-1,jmt-3) eq 1 then begin
    ipsi(i,jmt-2)=1
    psi(i,jmt-2,*)=psi(imt-i-1,jmt-3,*)
   endif
   if ipsi(i,jmt-2) eq 1 and ipsi(imt-i-1,jmt-3) eq 0 then begin
    ipsi(imt-i-1,jmt-3)=1
    psi(imt-i-1,jmt-3,*)=psi(i,jmt-2,*)
   endif
   if ipsi(i,jmt-1) eq 0 and ipsi(imt-i-1,jmt-4) eq 1 then begin
    ipsi(i,jmt-1)=1
    psi(i,jmt-1,*)=psi(imt-i-1,jmt-4,*)
   endif
   if ipsi(i,jmt-1) eq 1 and ipsi(imt-i-1,jmt-4) eq 0 then begin
    ipsi(imt-i-1,jmt-4)=1
    psi(imt-i-1,jmt-4,*)=psi(i,jmt-1,*)
   endif
  endfor
 endif
 totipsi=round(total(ipsi))
endwhile

psi=psi/1.e6

pmin=dblarr(pmt+1) & pmax=pmin
for pp=0,pmt do begin
 pmax(pp)=max(psi(*,*,pp))
endfor
psi(where(psi le -1.e5))=1.e6
for pp=0,pmt do begin
 pmin(pp)=min(psi(*,*,pp))
endfor

print,' '
print,'#(p)','minimum','maximum','section',format='(a12,a12,a12,a18)'
for pp=0,pmt-1 do begin
 print,pp,pmin(pp),pmax(pp),segname(pind(pp)),format='(i12,f12.2,f12.2,a18)'
endfor
print,pmt,pmin(pmt),pmax(pmt),'Total',format='(i12,f12.2,f12.2,a18)'

end
