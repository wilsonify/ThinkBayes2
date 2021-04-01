      FUNCTION BETA(A,B)
C INCOMPLETE BETA FUNCTION
      include '../../ForLib/implicit.sta'
      BETA=EXP(GAMMA_LN(A)+GAMMA_LN(B)-GAMMA_LN(A+B))
      RETURN
      END
