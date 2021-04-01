      PROGRAM FIG_F_DF
C
C Copyright, Bernd Berg, Oct 30, 2000.
C F-Ratio distribution function and peaked distribution function.
C
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER (IUO=6,IUD=19,NGNU=280,NDVD=70)
      COMMON /PARFT/ NF1,NF2
      OPEN(IUD,FILE='F_df.d',form='formatted',status='unknown')
      DO IGNU=0,NGNU
        F=(IGNU*ONE)/(NDVD*ONE)
        NF1=16
        NF2=16
        FDF1=F_DF(F)
        FQDF1=F_QDF(F)
        NF1=32
        NF2=32
        FDF2=F_DF(F)
        FQDF2=F_QDF(F)
        NF1=32
        NF2=16
        FDF3=F_DF(F)
        FQDF3=F_QDF(F)
        WRITE(IUD,'(7F10.5)') F,FDF1,FDF2,FDF3,FQDF1,FQDF2,FQDF3
      END DO
      CLOSE(IUD)
      STOP
      END

      INCLUDE '../../ForLib/beta_i.f'
      INCLUDE '../../ForLib/gamma_ln.f'
      INCLUDE '../../ForLib/F_df.f'
      INCLUDE '../../ForLib/F_qdf.f'
