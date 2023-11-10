      FUNCTION FCT_LN(K)
C Calculates the Log of the factorial k!.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      XKP1=(K+1)*ONE
      FCT_LN=GAMMA_LN(XKP1)
      RETURN
      END
