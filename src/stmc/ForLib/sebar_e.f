      SUBROUTINE SEBAR_E(N,PC,EBUP,EBDO)
C
C ERROR BAR FOR THE GAUSSIAN ERROR BAR.
C =================================
C INPUT:   NUMBER OF GAUSSIAN DATA, CONFIDENCE INTERVAL.
C OUTPUT:  UPPER AND LOWER ERROR BAR LIMITS (EXACT).
C
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      COMMON /CHI2PAR/ NF
      NF=N-1
      IF(N>17 000) CALL SEBAR_E_AS(N,PC,EBUP,EBDO)
      IF(N>17 000) RETURN
      IF(PC<=ZERO) Q=HALF*(ONE-0.954499736D00)
      IF(PC>ZERO) Q=HALF*(ONE-PC)
      P=ONE-Q
      EBUP=ONE/SQRT(chi2pdf_xq(Q))
      EBDO=ONE/SQRT(chi2pdf_xq(P))
      RETURN
      END
