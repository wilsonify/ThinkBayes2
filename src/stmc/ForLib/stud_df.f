      FUNCTION STUD_DF(T)  
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      COMMON /PARSTUD/ NF
C
      A=HALF*NF
      B=HALF
      X=(NF*ONE)/((NF*ONE)+T**2)
      IF(T==ZERO) STUD_DF=    HALF
      IF(T<ZERO) STUD_DF=    HALF*BETA_I(X,A,B)
      IF(T>ZERO) STUD_DF=ONE-HALF*BETA_I(X,A,B)
C
      RETURN
      END   
