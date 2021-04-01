      PROGRAM E1D
C     =================
C Copyright, Bernd Berg, Aug 10, 2002.
C 1d Ising Model on finite lattice.
C
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      include 'lat.par'
      PARAMETER(IUO=6,IUD=10,NBETA=500,DBETA=TWO/NBETA)
      OPEN(IUD,FILE="E1d.d",form='formatted',status='unknown')
      DO IBETA=1,NBETA
        BETA=DBETA*IBETA
        THB=TANH(BETA)
        Eo=-(ML-1)*ONE/(ML*ONE)*THB         ! Open BC.
        Ep=-(THB+THB**(ML-1))/(ONE+THB**ML) ! Periodic BC.
        WRITE(IUD,'(3F14.6)') BETA,Eo,Ep
      END DO
      CLOSE(IUD)
      STOP
      END

