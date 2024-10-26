FUNCTION eos,s,tpot

ZR1= ((((6.536332E-9*tpot-1.120083E-6)*tpot+1.001685E-4)*tpot $
  -9.095290E-3)*tpot+6.793952E-2)*tpot+999.842594
ZR2= (((5.3875E-9*tpot-8.2467E-7)*tpot+7.6438E-5)*tpot $
  -4.0899E-3)*tpot+8.24493E-1
ZR3= (-1.6546E-6*tpot+1.0227E-4)*tpot-5.72466E-3
ZR4= 4.8314E-4

res=(ZR4*s + ZR3*sqrt(abs(s)) + ZR2)*s + ZR1-1000.

return,res

END
