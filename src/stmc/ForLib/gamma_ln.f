      FUNCTION GAMMA_LN(X)
C BERG, JUNE 23, 1999.
C LN OF GAMMA FUNCTION ALA LANCZOS, SIAM Num. Anal. B1 (1964) 1.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      IF(X.LE.ZERO) THEN
        PRINT*,"GAMMA_LN: Argument X =",X
        STOP 'GAMMA_LN: illegal argument.'
      END IF
      IF(X.GT.ONE) THEN ! Full accuracy of Lanczos formula.
        Y=X
      ELSE ! Use Gamma(z+1)=z*Gamma(z).
        Y=ONE+X
      ENDIF
      SER=((ONE+C1_L/Y)+C2_L/(Y+ONE))+C3_L/(Y+TWO)
      SER=((SER+C4_L/(Y+THREE))+C5_L/(Y+FOUR))+C6_L/(Y+FIVE)
      GAMMA_LN=(Y-HALF)*LOG(Y+FNINE_HALF)-(Y+FNINE_HALF)+LOG(STP_L*SER)
      IF(X.GT.ONE) RETURN
      GAMMA_LN=GAMMA_LN-LOG(X)
      RETURN
      END
