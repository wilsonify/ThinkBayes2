      SUBROUTINE KOLM2_DEL2(N1,N2,DAT1,DAT2,DEL)
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      DIMENSION DAT1(N1),DAT2(N2)
      IF(N1.GT.N2) STOP "KOLM2: N2.GE.N1 required."
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
      RETURN
      END
