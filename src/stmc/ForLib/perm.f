      SUBROUTINE PERM(II,N,IP)
C
C PURPOSE: GENERATES PERMUTATIONS OF NUMBERS 1,...,N.
C II=1,...,N! PICKS OUT A SPECIFIC PERMUTATION.
C IP(N):      CONTAINS THE RESULT (PERMUTATION OF 1,...,N).         
C
      PARAMETER(NMAX=12)
      DIMENSION IWORK(NMAX),IP(N)
      IF(N>NMAX) STOP 'PERM: 12! IS LARGEST VALUE REPRESENTABLE.'
C
      NM1=N-1
      IP(1)=1
      DO I=2,N
      IP(I)=0
      END DO
C
      NF=1
      DO I=2,N
      NF=NF*I
      END DO
      IF(II>NF) STOP 'PERM: II MUST BE .LE. N!.'
C
      JJ=NF-II+1
      DO I=N,2,-1
      KK=JJ/I
      IWORK(I)=JJ-I*KK
      JJ=KK
      END DO
C
      DO I=2,N
      J=I-IWORK(I)
      IP(I)=IP(J)
      IP(J)=I
      END DO
C
      RETURN
      END
