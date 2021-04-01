      PROGRAM PDS
C
C PROBABILITY DENSITIES AND DISTRIBUTION FUNCTIONS:
C YU UNIFORM, YG GAUSS AND YC CAUCHY.
C  
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER (IUGNU=8)
C
C PROBABILITY DENSITIES:
C
      OPEN(UNIT=IUGNU,STATUS='UNKNOWN',FILE='fig1.d',FORM='FORMATTED')
      DO I=-30,30
        X=(I*ONE)/TEN
        YU=ZERO
        IF(X.GE.-ONE .AND. X.LE.+ONE) YU=HALF
        YG=EXP(-HALF*X**2)/SQRT(TPI) 
        YC=ONE/(PI+PI*X**2)
        IF(X.EQ.-ONE) WRITE(IUGNU,'(4G16.7)') X,ZERO,YG,YC
        WRITE(IUGNU,'(4G16.7)') X,YU,YG,YC
        IF(X.EQ.+ONE) WRITE(IUGNU,'(4G16.7)') X,ZERO,YG,YC
      END DO  
      CLOSE (IUGNU)
C
C CUMULATIVE DISTRIBUTION FUNCTIONS:
C
      OPEN(UNIT=IUGNU,STATUS='UNKNOWN',FILE='fig2.d',FORM='FORMATTED')
      DO I=-30,30
        X=(I*ONE)/TEN
        YU=ZERO
        IF(X.GT.-ONE) YU=HALF*(X+ONE)
        IF(X.GT.+ONE) YU=ONE
        YG=GAU_DF(X)
        YC=CAU_DF(X)
        WRITE(IUGNU,'(4G16.7)') X,YU,YG,YC
      END DO  
      CLOSE (IUGNU)
C
      STOP
      END

      include '../../ForLib/gau_df.f'
      include '../../ForLib/error_f.f'
      include '../../ForLib/gamma_p.f'
      include '../../ForLib/gamma_ln.f'
      include '../../ForLib/cau_df.f'
