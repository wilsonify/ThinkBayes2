      PROGRAM BIAS_BIG
C
C Copyright, Bernd Berg, November 1, 2000.
C ILLUSTRATION OF THE DOUBLE JACKKNIFE BIAS CORRECTED ESTIMATOR.
C Here the bias is big through the ABS in the EXP function.
C (Related paper: Berg, Comp. Phys. Commun. 69 (92) 7-14.)
C
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER(IUO=6,IUD=10,ISEED=1,ISEED2=0, NTRY=2000,NPR=4)
C     PARAMETER(IUO=6,IUD=10,ISEED=1,ISEED2=0, NTRY=0002,NPR=4)
      PARAMETER(NDAT=320,N=32,N1=N-1,A0=2.0D00)
C STUP=STUPID:
      DIMENSION STUP2M(NTRY),DATM2(NTRY),DATJ2M(NTRY),DATMM2(NTRY)
      DIMENSION DAT0(NDAT),DATA(N),STUP2(N),DAT2(N),DATJ(N),DATJ2(N)
      DIMENSION DATJJ(N1,N),DATJJ2(N1,N),DATJ2MM(N)
C
      WRITE(IUO,*) '   '
      WRITE(IUO,*) 'EXACT:'
      EXACT=ONE/THREE
      WRITE(IUO,*)  EXACT
      CALL RMASET(IUO,IUD,ISEED1,ISEED2,'ranmar.d')
C
      DO ITRY=1,NTRY
        IF(ITRY<=NPR) WRITE(IUO,*) '   '
        IF(ITRY<=NPR) WRITE(IUO,*) 'ITRY =',ITRY
C
        DO I=1,NDAT
          CALL RMAGAU(DAT0(I),YG)
        END DO
        CALL BINING(NDAT,DAT0,N,DATA)
        DO I=1,NDAT
          DAT0(I)=DAT0(I)**4
        END DO
        CALL BINING(NDAT,DAT0,N,DAT2)
C
        DATM=STMEAN(N,DATA)
        DA2M=STMEAN(N,DAT2)
        DATM2(ITRY)=EXP(A0*ABS(DATM))/DA2M
        IF(ITRY<=NPR) WRITE(IUO,'(" BIASED ESTIMATE FROM ALL DATA:",7X
     &  1F9.6)') DATM2(ITRY)
C
        CALL DATJACK(N,DATA,DATJ)
        CALL DATJACK(N,DAT2,DATJ2)
C USER DEFINED FUNCTION:
        DO I=1,N
          STUP2(I)=EXP(A0*ABS(DATA(I)))/DAT2(I)
          DATJ2(I)=EXP(A0*ABS(DATJ(I)))/DATJ2(I)
        END DO
        CALL STEB0 (N,STUP2,STUP2M(ITRY),VAR,ERROR0)
        CALL STEBJ0(N,DATJ2,DATJ2M(ITRY),VAR,ERROR0)
        IF(ITRY<=NPR) THEN
          WRITE(IUO,'(" BAD, BIASED ESTIMATOR:",15X,
     &    1F9.6,"  +/-",1F9.6)') STUP2M(ITRY),ERROR0
          WRITE(IUO,'(" STANDARD, BIASED JACKKNIFE ESTIMATOR:",
     &    1F9.6,"  +/-",1F9.6)') DATJ2M(ITRY),ERROR0
        END IF
C
        CALL DATJACK2(N,DATA,DATJJ)
        CALL DATJACK2(N,DAT2,DATJJ2)
C USER DEFINED FUNCTION:
        DO I2=1,N
        DO I1=1,N1
          DATJJ2(I1,I2)=EXP(A0*ABS(DATJJ(I1,I2)))/DATJJ2(I1,I2)
        END DO
        END DO
        CALL STEBJJ1(N,DATJJ2,DATJ2,DATJ2MM,DATMM2(ITRY),VAR,ERROR)
        IF(ITRY<=NPR) THEN
          WRITE(IUO,*) 'SECOND LEVEL'
          WRITE(IUO,'(" BIAS-CORRECTED JACKKNIFE ESTIMATOR:  "
     &    ,1F9.6,"  +/-",1F9.6)') DATMM2(ITRY),ERROR
        END IF
      END DO
C
      WRITE(IUO,*) '   '
      WRITE(IUO,*) 'Average over NTRY =',NTRY,'  calculations.'
      WRITE(IUO,*) 'Comparision of results:'
      CALL STEB0(NTRY,DATM2,XM2,V,EM2)
      WRITE(IUO,'(" BIASED ESTIMATE FROM ALL DATA:",7X
     & 1F9.6,"  +/-",1F9.6)') XM2,EM2
      CALL STEB0(NTRY,STUP2M,XS2M,V,EST2M)
      WRITE(IUO,'(" BAD, BIASED ESTIMATOR:",15X,
     & 1F9.6,"  +/-",1F9.6)') XS2M,EST2M
      CALL STEB0(NTRY,DATJ2M,XJ2M,V,EJ2M)
      WRITE(IUO,'(" STANDARD, BIASED JACKKNIFE ESTIMATOR:",
     & 1F9.6,"  +/-",1F9.6)') XJ2M,EJ2M
      CALL STEB0(NTRY,DATMM2,XMM2,V,EMM2)
      WRITE(IUO,*) 'SECOND LEVEL'
      WRITE(IUO,'(" BIAS-CORRECTED JACKKNIFE ESTIMATOR:  "
     &,1F9.6,"  +/-",1F9.6)') XMM2,EMM2
C
      STOP
      END


      INCLUDE '../../ForLib/bias.f'
      INCLUDE '../../ForLib/bining.f'
      INCLUDE '../../ForLib/datjack.f'
      INCLUDE '../../ForLib/datjack2.f'
      INCLUDE '../../ForLib/ranmar.f'
      INCLUDE '../../ForLib/rmagau.f'
      INCLUDE '../../ForLib/rmaset.f'
      INCLUDE '../../ForLib/steb0.f'
      INCLUDE '../../ForLib/stebj0.f'
      INCLUDE '../../ForLib/stebjj1.f'
      INCLUDE '../../ForLib/stmean.f'
