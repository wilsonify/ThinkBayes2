      SUBROUTINE STEB_RAT(XM1,EB1,XM2,EB2,RATM,RATE,REsmall)
C BERG, APR 21, 2002. Ratio of two numbers with error bars in a
C                     conservative Gaussian approximation.
      include 'implicit.sta'
      RATM=XM1/XM2
      RE1=ABS(EB1/XM2)
      RE2=ABS(XM1)/(ABS(XM2)-EB2)-ABS(RATM)
      RATE=SQRT(RE1**2+RE2**2)
      RE2=ABS(RATM)-ABS(XM1)/(ABS(XM2)+EB2)
      REsmall=SQRT(RE1**2+RE2**2)
      RETURN
      END
