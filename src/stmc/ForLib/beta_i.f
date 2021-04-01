      FUNCTION BETA_I(X,A,B)
C INCOMPLETE BETA FUNCTION. Copyright, Bernd Berg, Apr 2 2000.
      include 'implicit.sta'
      include 'constants.par'
      PARAMETER (ITER_MAX=200,EPS=1.D-10)
 
      IF(X.LT.ZERO .OR. X.GT.ONE) STOP 'BAD ARGUMENT X IN BETA_I'
      IF(X.EQ.ZERO .OR. X.EQ.ONE) THEN
        BT=ZERO
      ELSE
        BT=EXP(GAMMA_LN(A+B)-GAMMA_LN(A)-GAMMA_LN(B)
     &    +A*LOG(X)+B*LOG(ONE-X))
      ENDIF
 
      IF(X.LT.(A+ONE)/(A+B+TWO)) THEN
        XX=X
        AA=A
        BB=B
      ELSE
        XX=ONE-X
        AA=B
        BB=A
      END IF

      APB=AA+BB
      AP1=AA+ONE
      AM1=AA-ONE
 
      BCFM=ONE
      BM=ONE
      BCF=ONE
      BZ=ONE-APB*XX/AP1
      DO ITER=1,ITER_MAX
        XITER=ITER*ONE
        TWO_ITER=XITER+XITER
        C1=XITER*(BB-XITER)*XX/((AM1+TWO_ITER)*(AA+TWO_ITER))
        C2=-(AA+XITER)*(APB+XITER)*XX/((AA+TWO_ITER)*(AP1+TWO_ITER))
        BCFP=BCF+C1*BCFM
        BP=BZ+C1*BM
        BPP=BP+C2*BZ
        BCFOLD=BCF
        BCFM=BCFP/BPP
        BM=BP/BPP
        BCF=(BCFP+C2*BCF)/BPP
        BZ=ONE
        IF(ABS(BCF-BCFOLD).LT.EPS*ABS(BCF)) GO TO 1
      END DO
      STOP 'BETA_I: A or B too big, or ITER_MAX too small'
C
1     IF(X.LT.(A+ONE)/(A+B+TWO)) THEN
        BETA_I=BT*BCF/A
      ELSE
        BETA_I=ONE-BT*BCF/B
      ENDIF
      RETURN
      END
