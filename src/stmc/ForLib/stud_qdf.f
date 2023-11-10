      FUNCTION STUD_QDF(T)  
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      COMMON /PARSTUD/ NF
C
      A=HALF*NF
      B=HALF
      X=(NF*ONE)/((NF*ONE)+T**2)
      IF(T==ZERO) STUD_QDF=    HALF
      IF(T<ZERO) STUD_QDF=    HALF*BETA_I(X,A,B)
      IF(T>ZERO) STUD_QDF=ONE-HALF*BETA_I(X,A,B)
      IF(STUD_QDF>HALF) STUD_QDF=ONE-STUD_QDF
C
      RETURN
      END   
