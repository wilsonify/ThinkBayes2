      PROGRAM HIST_GAU
C
C HISTOGRAM for the normal distribution in [-3,+3].
C  
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
C GNUPLOT filenames must be chose to match character*5!      
      PARAMETER(IUO=6,IUD=10,ISEED1=1,ISEED2=0)
      PARAMETER(NHIST=30,NDAT=10 000,NDH=NDAT/2)
      DIMENSION HIST(NHIST),DATA(NDAT)
C
      XMIN=-THREE
      XMAX=+THREE
      CALL RMASET(IUO,IUD,ISEED1,ISEED2,'ranmar.d')
C
      ntest=0
      DO I=1,NDH
        CALL RMAGAU(XG,YG)
        I1=2*I-1
        I2=2*I
        DATA(I1)=XG
        DATA(I2)=YG
      END DO
      WRITE(IUO,*) DATA(9999),DATA(10000)
      CALL HIST_GNU(-IUD,NHIST,NDAT,HIST,DATA,XMIN,XMAX)
C
      STOP
      END

      include '../../ForLib/rmaset.f'
      include '../../ForLib/ranmar.f'
      include '../../ForLib/rmagau.f'
      include '../../ForLib/hist_gnu.f'
