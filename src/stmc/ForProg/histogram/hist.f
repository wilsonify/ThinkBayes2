      PROGRAM HISTOGRAM
C
C Copyright, Bernd Berg, Sep 21, 2000.
C ILLUSTRATION OF SAMPLING FOR A HISTOGRAM.
C  
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
C GNUPLOT filenames must be chose to match character*5!      
      PARAMETER(IUO=6,IUD=10,ISEED1=1,ISEED2=0)
      PARAMETER(NHIST=10,NDAT=10 000,N1=100,N2=NDAT)
      DIMENSION HIST(NHIST),DATA(NDAT)
C
      XMIN=ZERO
      XMAX=ONE
      CALL RMASET(IUO,IUD,ISEED1,ISEED2,'ranmar.d')
C
      DO I=1,N1
        CALL RANMAR(DATA(I))
      END DO
      CALL HIST_GNU(-IUD,NHIST,N1,HIST,DATA,XMIN,XMAX)
C
      DO I=1,N2
        CALL RANMAR(DATA(I))
      END DO
      CALL HIST_GNU(-IUD,NHIST,N2,HIST,DATA,XMIN,XMAX)
C
      STOP
      END

      include '../../ForLib/rmaset.f'
      include '../../ForLib/ranmar.f'
      include '../../ForLib/hist_gnu.f'
