      PROGRAM JACK_EB
C
C Copyright, Bernd Berg, Oct 31, 2000.
C ILLUSTRATION THAT JACKKNIfE ERROR BARS:
C (1) JACKKNIfE ERROR BARS AND STANDARD ERROR BARS
C     AGREE FOR UNBIASED QUANTITIES.
C (2) JACKKNIFE ERROR BAR AND BIAS FOR THE MEAN
c     EXPECTATION VALUE SQUARED.
c
      include '../../ForLib/implicit.sta'
      PARAMETER(IUO=6,IUD=10,ISEED1=1,ISEED2=0, NDAT=320)
      DIMENSION DATA(NDAT),DATJ(NDAT)
C
      CALL RMASET(IUO,IUD,ISEED1,ISEED2,'ranmar.d')
      DO I=1,NDAT
        CALL RANMAR(DATA(I))
      END DO
      CALL DATJACK(NDAT,DATA,DATJ)
      CALL STEB0 (NDAT,DATA,XM,VAR,XE)
      WRITE(IUO,'(1X,"STEB0:  ",1G16.7," +/-",1G16.7)') XM,XE
      CALL STEBJ0(NDAT,DATJ,XM,VAR,XE)
      WRITE(IUO,'(1X,"STEBJ0: XM =  ",1G16.7," +/-",1G16.7)') XM,XE
      XMEAN=STMEAN(NDAT,DATA)
      CALL STEBJ1(NDAT,DATJ,XMEAN,XJM,XMM,VAR,XE)
      WRITE(IUO,'(1X,"STEBJ1: XMM = ",1G16.7," +/-",1G16.7)') XMM,XE
      WRITE(IUO,*) 'NO BIAS!'
C
      WRITE(IUO,*)
C     WRITE(IUO,*) 'Now f(\hat{x})=\hat{x}^2:' ! DOS
      WRITE(IUO,*) 'Now f(\\hat{x})=\\hat{x}^2:' ! LINUX
      XMEAN2=XMEAN**2
      DO I=1,NDAT
        DATJ(I)=DATJ(I)**2
      END DO
      CALL STEBJ0(NDAT,DATJ,XM,VAR,XE)
      WRITE(IUO,'(1X,"STEBJ0: XM =  ",1G16.7," +/-",1G16.7)') XM,XE
      CALL STEBJ1(NDAT,DATJ,XMEAN2,XJM,XMM2,VAR,XM2E)
      WRITE(IUO,'(1X,"STEBJ1: XMEAN2=",1G16.7," +/-",1G16.7)') XMEAN2,XE
      WRITE(IUO,'(1X,"STEBJ1: XMM2 = ",1G16.7," +/-",1G16.7)') XMM2,XM2E
      BIASM=XMEAN2-XMM2
      WRITE(IUO,'(1X,"EST. BIAS =   ",1G16.7)') BIASM
C
      STOP
      END                         

      INCLUDE '../../ForLib/bias.f'
      INCLUDE '../../ForLib/datjack.f'
      INCLUDE '../../ForLib/ranmar.f'
      INCLUDE '../../ForLib/rmaset.f'
      INCLUDE '../../ForLib/steb0.f'
      INCLUDE '../../ForLib/stebj0.f'
      INCLUDE '../../ForLib/stebj1.f'
      INCLUDE '../../ForLib/stmean.f'
