      SUBROUTINE KOLM1(N,Fxct,DEL1,DEL2,Q1,Q2)
C Copyright, Bernd Berg, Mar 2, 2001.
C Exact one-sided Kolmogorov tests, implementing equation of
C Birnbaum and Tingey, Ann. Math. Stat. 22 (1951) 592. See also: 
C van der Waerden, Mathematical Statistics, Springer 1969.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      DIMENSION Fxct(N)
C
      DO J=1,2
        DEL=ZERO
        IF(J==1) THEN
          DO I=1,N
            Femp=(ONE*(I-1))/N
            DEL=MAX(DEL,Fxct(I)-Femp)
          END DO
          DEL1=DEL
        ELSE
          DO I=1,N
            Femp=(ONE*I)/N
            DEL=MAX(DEL,Femp-Fxct(I))
          END DO
          DEL2=DEL
        END IF
C
        DELLN=LOG(DEL)
        XN=ONE*(N+1)
        YNLN=GAMMA_LN(XN)
        Q=ZERO
        DO K=0,N
          XKP1=ONE*(K+1)
          XNP1MK=ONE*(N+1-K)
          BLN=YNLN-GAMMA_LN(XKP1)-GAMMA_LN(XNP1MK)
          DELPLUS=DEL+(ONE*K)/(ONE*N)
          X=ONE-DELPLUS
          IF(X<=ZERO) GO TO 1
          QLN=BLN+DELLN+(K-1)*LOG(DELPLUS)+(N-K)*LOG(X)
          Q=Q+EXP(QLN)
        END DO
1       CONTINUE
        IF(J==1) THEN
          Q1=Q
        ELSE
          Q2=Q
        END IF
      END DO
C
      RETURN
      END
