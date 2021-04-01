      PROGRAM F_TEST
C
C VARIANCE RATIO TEST (F-TEST): COMPARISON OF TWO ERROR BARS.
C ===========================================================
C
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      CHARACTER*16 CHIN
C
      PRINT*,'VARIANCE RATIO TEST (F-TEST):'
      PRINT*,'INPUT:   TWO ERROR BARS (NOT VARIANCES!) AND NUMBER OF'
      PRINT*,'         INDEPENDENT DATA THEY ARE BASED ON.' 
      PRINT*,'OUTPUT:  LIKELIHOOD THAT THEIR RATIO IS DUE TO CHANCE.' 
      PRINT*,'          '
1     CONTINUE
      PRINT*,'ENTER ERROR BAR OF 1. DATA POINT:'
      READ(5,*) EB1
      PRINT*,'ENTER NUMBER OF INDEPENDENT MEASUREMENTS FOR 
     & 1. DATA POINT:'
      READ(5,*) NDAT1
      PRINT*,'ENTER ERROR BAR OF 2. DATA POINT:'
      READ(5,*) EB2
      PRINT*,'ENTER NUMBER OF INDEPENDENT MEASUREMENTS FOR 
     & 2. DATA POINT:'
      READ(5,*) NDAT2
C
      CALL FTEST(EB1,NDAT1,EB2,NDAT2,Q)
C
      PRINT 101,Q,TEN**2*Q
101   FORMAT(/,1X,'F-TEST: Q =',1F12.8,
     &            ',    OR (EQUIVALENTLY):',1F6.2,'%',/)
C
      PRINT*,'DO YOU WANT ANOTHER RUN? (Y). '
      READ(5,100) CHIN
100   FORMAT(1A16)
      IF(CHIN.EQ.'Y' .OR. CHIN.EQ.'y') GO TO 1
C
      STOP
      END

      INCLUDE '../../ForLib/beta_i.f'
      INCLUDE '../../ForLib/Ftest.f'
      INCLUDE '../../ForLib/F_df.f'
      INCLUDE '../../ForLib/gamma_ln.f'
