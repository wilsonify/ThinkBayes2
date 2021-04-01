      SUBROUTINE KOLM2_AS2(N1,N2,DAT1,DAT2,DEL,Q)
C Copyright, Bernd Berg, Apr 27, 2001.
C Asymptotic two-sided Kolmogorov test for two data sets.
C See kolm2_as.f for further comments.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      DIMENSION DAT1(N1),DAT2(N2)
C
C
      Femp1=ZERO
      Femp2=ZERO
      DEL=ZERO
      I1=1
      I2=1
1     CONTINUE
      IF(DAT1(I1).LT.DAT2(I2)) THEN
        Femp1=(ONE*I1)/N1
        I1=I1+1
      ELSE IF(DAT1(I1).EQ.DAT2(I2)) THEN
        Femp1=(ONE*I1)/N1
        Femp2=(ONE*I2)/N2
        I1=I1+1
        I2=I2+1  
      ELSE
        Femp2=(ONE*I2)/N2
        I2=I2+1  
      END IF
      DEL=MAX(DEL,ABS(Femp1-Femp2))
      IF(I1.LE.N1.AND.I2.LE.N2) GO TO 1
C
C
C Effective number of data N1*N2/(N1+N2).
      SQN=SQRT((N1*N2*ONE)/((N1+N2)*ONE)) 
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
      
     
