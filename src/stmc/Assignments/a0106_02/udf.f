      PROGRAM UDF
C
C EMPIRICAL DISTRIBUTION FUNCTION AND Q-DISTRIBUTION.
C  
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER(IUO=6,IUD=10,ISEED1=1,ISEED2=0,N=100)
      DIMENSION DATA(N)
C
      CALL RMASET(-1,-1,ISEED1,ISEED2,'ranmar.d')
      DO I=1,N
      CALL RANMAR(DATA(I))
      DATA(I)=DATA(I)
      END DO
      CALL HEAPSORT(N,DATA)
      CALL DF_GNU(IUD,N,DATA)
C
      STOP
      END

      include '../../ForLib/rmaset.f'
      include '../../ForLib/ranmar.f'
      include '../../ForLib/hist_gnu.f'
      include '../../ForLib/heapsort.f'
      include '../../ForLib/df_gnu.f'

