      PROGRAM FIG_F_PD
C
C Copyright Bernd Berg, Oct 30, 2000.
C F-Ratio probability densities.
C
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER (IUO=6,IUD=10,NGNU=280,NDVD=70)
      COMMON /PARFT/ NF1,NF2
      Fsum=zero
      OPEN(IUD,file='F_pd.d',form='formatted',status='unknown')
      DO IGNU=0,NGNU
        F=(IGNU*ONE)/(NDVD*ONE)
        NF1=16
        NF2=16
        F1=F_PD(F)
        NF1=32
        NF2=32
        F2=F_PD(F)
        NF1=32
        NF2=16
        F3=F_PD(F)
        WRITE(IUD,'(4F12.6)') F,F1,F2,F3
        Fsum=Fsum+F2/(NDVD*ONE)
      END DO
      CLOSE(IUD)
      WRITE(IUO,*) 'Fsum =',Fsum
C
      STOP
      END

      INCLUDE '../../ForLib/beta_i.f'
      INCLUDE '../../ForLib/gamma_ln.f'
      INCLUDE '../../ForLib/F_pd.f'

