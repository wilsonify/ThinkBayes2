      SUBROUTINE KOLM1_AS(N,Fxct,EPS1,EPS2,Q1as,Q2as)
C Copyright, Bernd Berg, Apr 3, 2001.
C Asymptotic one-sided Kolmogorov tests, implementing Smirnov's
C equation, see van der Waerden, Mathematical Statistics, Springer 1969.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      DIMENSION Fxct(N)
C
      DO J=1,2
        EPS=ZERO
        IF(J.EQ.1) THEN
          DO I=1,N
            Femp=(ONE*(I-1))/N
            EPS=MAX(EPS,Fxct(I)-Femp)
          END DO
          EPS1=EPS
          Q1as=EXP(-(TWO*N*EPS**2))
        ELSE
          DO I=1,N
            Femp=(ONE*I)/N
            EPS=MAX(EPS,Femp-Fxct(I))
          END DO
          EPS2=EPS
          Q2as=EXP(-(TWO*N*EPS**2))
        END IF
      END DO
C
      RETURN
      END
