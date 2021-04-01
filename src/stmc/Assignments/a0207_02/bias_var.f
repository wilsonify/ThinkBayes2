      PROGRAM BIAS_VAR
C
C Copyright, Bernd Berg, Oct 31, 2000.
C VARIANCE EXAMPLE FOR DOUBLE JACKKNIFE BIAS CORRECTED
C ESTIMATORS (Berg, Comp. Phys. 69 (92) 7-14).
C
      include '../../ForLib/implicit.sta'
      PARAMETER(IUO=6,IUD=10,ISEED1=1,ISEED2=2, N=16,N1=N-1)
      DIMENSION DATA(N),DAT2(N),DATJ(N),DATJ2(N)
      DIMENSION DATJJ(N1,N),DATJJ2(N1,N),DATJ2MM(N)
C
      CALL RMASET(-IUO,IUD,ISEED1,ISEED2,'ranmar.d')
      DO I=1,N
        CALL RANMAR(XR)
        DATA(I)=XR
        DAT2(I)=DATA(I)**2
      END DO
C
      WRITE(IUO,*) '  '
      WRITE(IUO,*) 'NUMERICAL ESTIMATES FOR THE MEAN SQUARED:'
      WRITE(IUO,*) '  '
      DATM=STMEAN(N,DATA)
      DATM2=DATM**2
      WRITE(IUO,'(" BIASED ESTIMATE FROM ALL DATA:",7X,1F10.6)') DATM2
C
      CALL STEB0(N,DAT2,BADM2,VAR,BADM2E)
      WRITE(IUO,'(" BAD, BIASED ESTIMATOR (FALSE):        ",1F9.6,
     &            "  +/-",1F9.6)') BADM2,BADM2E
C
      DA2M=STMEAN(N,DAT2)
      DATMM2=DATM2-(DA2M-DATM2)/(N-1)
      WRITE(IUO,'(" UNBIASED ESTIMATOR:      ",12X,1F10.6)') DATMM2
C
      CALL DATJACK(N,DATA,DATJ)
C USER DEFINED FUNCTION F(X)=X**2:
      DO I=1,N
        DATJ2(I)=DATJ(I)**2
      END DO
      CALL STEBJ0(N,DATJ2,DATJ2M,VAR,ERROR0)
      WRITE(IUO,'(" STANDARD, BIASED JACKKNIFE ESTIMATOR: ",1F9.6,
     &            "  +/-",1F9.6)') DATJ2M,ERROR0
C
      CALL DATJACK2(N,DATA,DATJJ)
C USER DEFINED FUNCTION F(X)=X**2:
      DO I2=1,N
      DO I1=1,N1
        DATJJ2(I1,I2)=DATJJ(I1,I2)**2
      END DO
      END DO
      CALL STEBJJ1(N,DATJJ2,DATJ2,DATJ2MM,DATMM2,VAR,ERROR)
      WRITE(IUO,*) 'SECOND LEVEL'
      WRITE(IUO,'(" BIAS-CORRECTED JACKKNIFE ESTIMATOR:   ",1F9.6,
     &            "  +/-",1F9.6)') DATMM2,ERROR
C
      STOP
      END

      INCLUDE '../../ForLib/bias.f'
      INCLUDE '../../ForLib/datjack.f'
      INCLUDE '../../ForLib/datjack2.f'
      INCLUDE '../../ForLib/ranmar.f'
      INCLUDE '../../ForLib/rmaset.f'
      INCLUDE '../../ForLib/steb0.f'
      INCLUDE '../../ForLib/stebj0.f'
      INCLUDE '../../ForLib/stebjj1.f'
      INCLUDE '../../ForLib/stmean.f'
