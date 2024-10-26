window,9
contour,mt,xt,yt,/nodata,title='CHOOSE REFERENCE (zero) FOR PSI...',$
 subtitle='LEFT click: position    RIGHT click: final choice',$
 xrange=range_x,yrange=range_y

for j=1,jmt-1 do begin 
 for i=1,imt-2 do begin
  if mt(i,j)+mt(i+1,j) eq 1 then begin
   if abs(xp(i,j-1)-xp(i,j))le 180. then begin
    plots,[xp(i,j-1),xp(i,j)],[yp(i,j-1),yp(i,j)],noclip=0,thick=3
   endif
  endif
 endfor
endfor
if iperio eq 0 then begin
 for j=1,jmt-1 do begin 
  if mt(0,j)+mt(1,j) eq 1 then begin
   plots,[xp(0,j-1),xp(0,j)],[yp(0,j-1),yp(0,j)],noclip=0,thick=3
  endif
 endfor
endif
for j=0,jmt-2 do begin
 for i=1,imt-1 do begin
  if mt(i,j)+mt(i,j+1) eq 1 then begin
   if abs(xp(i-1,j)-xp(i,j))le 180. then begin
    plots,[xp(i-1,j),xp(i,j)],[yp(i-1,j),yp(i,j)],noclip=0,thick=3
   endif
  endif
 endfor
endfor

plots,!x.crange,[0,0],linestyle=3,noclip=0
for lon=-360,360,180 do begin
 plots,[lon,lon],!y.crange,noclip=0,linestyle=3
endfor
for j=0,jmt-1 do begin 
 for i=0,imt-1 do begin 
  if mtseg(i,j) eq 1 then begin
   plots,[xt(i,j),xt(i,j)],[yt(i,j),yt(i,j)],noclip=0,psym=4,symsize=.5
  endif
 endfor
endfor

pb=1
while pb eq 1 do begin
 cursor,xc,yc,/down
 print,xc,yc,format='(2f8.1)'
 if !mouse.button eq 4 then begin
  dist=(xt-xc)^2+(yt-yc)^2
  toto=min(dist,ij)
  jj=ij/imt
  ii=ij-jj*imt
;print,xt(ii,jj),yt(ii,jj),ii,jj
; print,xc,yc,ii,jj,format='(2f8.1,16x,3(i3,1x))'
  if mt(ii,jj) eq 0 then begin
   pb=0
  endif else begin
   print,'point OCEAN...'
  endelse
 endif
endwhile

wdelete,9

end
