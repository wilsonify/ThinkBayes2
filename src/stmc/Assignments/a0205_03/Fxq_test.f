      PROGRAM Fxq_test
C
C Copyright Bernd Berg, Oct 30, 2000.
C F-Ratio: test of the correctness of the q-tile function F_XQ.
C
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER (IUO=6,IUD=10,N=28,NDVD=7)
      COMMON /PARFT/ NF1,NF2
      DO I=1,N
        F=(I*ONE)/(NDVD*ONE)
        NF1=16
        NF2=16
        FDF1=F_DF(F)
        FQ1=F_XQ(FDF1)
        NF1=32
        NF2=32
        FDF2=F_DF(F)
        FQ2=F_XQ(FDF2)
        NF1=32
        NF2=16
        FDF3=F_DF(F)
        FQ3=F_XQ(FDF3)
        WRITE(IUO,'(7F10.5)') F,FDF1,FDF2,FDF3,FQ1,FQ2,FQ3
      END DO
      CLOSE(IUD)
      STOP
      END

      INCLUDE '../../ForLib/beta_i.f'
      INCLUDE '../../ForLib/gamma_ln.f'
      INCLUDE '../../ForLib/F_df.f'
      INCLUDE '../../ForLib/F_xq.f'
      INCLUDE '../../ForLib/fi1.f'
