      PROGRAM EQDF
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
      END DO
      CALL HEAPSORT(N,DATA)
      CALL QDF_GNU(IUD,N,DATA)
C
      DO I=1,N
      CALL RMAGAU(DATA(I),YG)
      END DO
      CALL HEAPSORT(N,DATA)
      CALL QDF_GNU(IUD,N,DATA)
C
      DO I=1,N
      CALL RANMAR(DATA(I))
      DATA(I)=TWO*(DATA(I)-HALF)
      END DO
      CALL HEAPSORT(N,DATA)
      CALL QDF_GNU(IUD,N,DATA)
C
      STOP
      END

      include '../../ForLib/rmaset.f'
      include '../../ForLib/ranmar.f'
      include '../../ForLib/rmagau.f'
      include '../../ForLib/heapsort.f'
      include '../../ForLib/qdf_gnu.f'

