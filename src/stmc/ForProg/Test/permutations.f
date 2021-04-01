      PROGRAM PERMUTATIONS
C
C Copyright, Bernd Berg, Oct 30, 2000.
C ILLUSTRATES GENERATION OF PERMUTATIONS BY PERM.F.  
C
      include '../../ForLib/implicit.sta'
      PARAMETER(IUO=6,NDAT=20,N=12)
      DIMENSION IPER(N)
C
      NF=1
      DO I=1,N
        NF=NF*I
      END DO
      CALL RMASET(-1,-1,1,0,'ramarin.d')
      CALL RANMAR(X)
      II=1+INT((NF*X))
      WRITE(IUO,*)
      WRITE(IUO,'(1X,"INPUT A NUMBER IN THEN RANGE 1,...,12!.")')
      WRITE(IUO,'(1X,"RESULT:")')
      WRITE(IUO,'(1X,"RANDOM PERMUTATION",I10," OF 1,...,12:")') II
      CALL PERM(II,N,IPER)
      WRITE(IUO,'(/,4X,12I3)') IPER
C
      WRITE(IUO,*)
      WRITE(IUO,*) 'ALL PERMUTATION OF 1,2,3:'
      N3=3
      DO II=1,NDAT
        CALL PERM(II,N3,IPER)
        WRITE(IUO,'(1I10,": ",12I3)') II,(IPER(J),J=1,N3)
      END DO
C
      STOP
      END
      include '../../ForLib/rmaset.f'
      include '../../ForLib/ranmar.f'
      include '../../ForLib/perm.f'
