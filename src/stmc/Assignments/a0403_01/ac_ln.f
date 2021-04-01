      PROGRAM AC_LN
C
C Copyright, Bernd Berg, Sep 26, 2002.
C - AUTOCORRELATION FUNCTIN AND INTEGRATED AUTOCORRELATION TIME: 
C   TESTED FOR GAUSSIAN DATA GENERATED BY METROPOLIS MC.
C
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER(IUO=6,IUD0=10,IUD1=11,ISEED1=1,ISEED2=0)
      PARAMETER(K=17,NDAT=2**K,NBINS=32,LMEAN=.FALSE.,A=THREE,NT=09)
      CHARACTER CBIN*2
      DIMENSION DATA(NDAT),ACORJ(0:NBINS,0:NT),ACE(0:NT)
      DIMENSION ACLNJ(0:NBINS,0:NT),ACLNE(0:NT),WORK(NBINS)
      LTEST=.true.
      LTEST=.false.
      IF(LTEST) STOP "AC_LN: LTEST."
C
      WRITE(IUO,*) ' '
      WRITE(IUO,*) 'NDAT  =  ',NDAT
      WRITE(IUO,*) 'NBINS =  ',NBINS
      WRITE(IUO,*) 'RANGE A = ',A
C
C Generation of Gaussian Metropolis data:
C
      CALL RMASET(IUO,IUD0,ISEED1,ISEED2,'ranmar.d')
      CALL GAU_METRO(A,NDAT,DATA,XAM)
      WRITE(IUO,*) 'METROPOLIS FOR GAUSSIAN RANDOM NUMBERS: '
      WRITE(IUO,*) 'ACCEPTANCE RATE: ',XAM
      CALL STEB0(NDAT,DATA,DM,DV,DE)
      WRITE(IUO,*) "DM,DV,DE:",DM,DV,DE
C
      OPEN(IUD0,FILE='acor.d',FORM='formatted',STATUS='unknown')
      OPEN(IUD1,FILE='acln.d',FORM='formatted',STATUS='unknown')
      WRITE(IUO,*) ' '
      DO IT=0,NT
        ACORJ(0,IT)=AUTCORF(IT,NDAT,DATA,LMEAN)
        CALL AUTCORJ(IT,NDAT,NBINS,DATA,WORK,ACORJ(1,IT),LMEAN)
        DO IBIN=1,NBINS
          IF(ACORJ(IBIN,IT).LE.ZERO) STOP "AC: ACORJ.LE.ZERO."
          ACLNJ(IBIN,IT)=LOG(ACORJ(IBIN,IT))
        END DO
        CALL STEBJ0(NBINS,ACORJ(1,IT),ACM,ACV,ACE(IT))
        BIAS=ACORJ(0,IT)-ACM
        WRITE(IUD0,'(1I6,4F14.8)') IT,ACORJ(0,IT),ACM,ACE(IT),BIAS
        CALL STEBJ0(NBINS,ACLNJ(1,IT),ACLNM,ACLNV,ACLNE(IT))
        ACLNJ(0,IT)=LOG(ACORJ(0,IT))
        BIAS=ACLNJ(0,IT)-ACLNM
        WRITE(IUD1,'(1I6,4F14.8)') IT,ACLNJ(0,IT),ACLNM,ACLNE(IT),BIAS
      END DO
      CLOSE(IUD0)
      CLOSE(IUD1)

      DO IBIN=0,NBINS
        WRITE(CBIN,'(I2.2)') IBIN
        OPEN(IUD0,FILE="datj"//Cbin//".d",FORM='FORMATTED',
     &           STATUS='UNKNOWN')
        IF(IBIN.EQ.0) WRITE(IUD0,'(2I10)') NBINS,(NT+1)
        DO IT=0,NT
          WRITE(IUD0,'(F10.0,2F14.8)') (IT*ONE),ACLNJ(IBIN,IT),ACLNE(IT)
        END DO
        CLOSE(IUD0)
      END DO

      STOP
      END

      INCLUDE '../../ForLib/ac_int.f'
      INCLUDE '../../ForLib/ac_intj.f'
      INCLUDE '../../ForLib/autcorf.f'
      INCLUDE '../../ForLib/autcorj.f'
      INCLUDE '../../ForLib/bias.f'
      INCLUDE '../../ForLib/bining.f'
      INCLUDE '../../ForLib/datjack.f'
      INCLUDE '../../ForLib/gau_metro.f'
      INCLUDE '../../ForLib/ranmar.f'
      INCLUDE '../../ForLib/rmaset.f'
      INCLUDE '../../ForLib/steb0.f'
      INCLUDE '../../ForLib/stebj0.f'
      INCLUDE '../../ForLib/stmean.f'