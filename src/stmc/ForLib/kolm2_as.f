      SUBROUTINE KOLM2_AS(N,Fxct,DEL,Q)
C Copyright, Bernd Berg, Apr 19, 2001.
C Asymptotic two-sided Kolmogorov test in the form of
C M.A. Stephens, J. Royal Stat. Soc. B 32 (1970) 115.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      DIMENSION Fxct(N)
C
      DEL=ZERO
      DO I=1,N
        Femp=(ONE*(I-1))/N
        DEL=MAX(DEL,Fxct(I)-Femp)
        Femp=(ONE*I)/N
        DEL=MAX(DEL,Femp-Fxct(I))
      END DO
C
      SQN=SQRT(N*ONE)
      A=-TWO*(SQN*DEL+(12*DEL)/100+(11*DEL)/(100*SQN))**2
      SIGN_TWO=TWO
      Q=ZERO
      CUT=ZERO
      DO J=1,100
        ADD=SIGN_TWO*EXP(A*J**2)
        Q=Q+ADD
        IF(ABS(ADD).LT.CUT) RETURN
        SIGN_TWO=-SIGN_TWO
        CUT=ABS(ADD)/1000
      END DO
      Q=ONE
C
      RETURN
      END
