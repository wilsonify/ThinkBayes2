      PROGRAM KOL2
C Copyright, Bernd Berg, Apr 20 2001.
C Comparision of the asymptotic 2-sided Kolmogorov test with
c results from the exact one sided test.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER(N=05,NITER=04,  IUO=6,IUD=10,ISEED1=1,ISEED2=0)
      DIMENSION DATA(N),Fxct(N)
C 
      CALL RMASET(IUO,-IUD,ISEED1,ISEED2,"no_file")
      WRITE(IUO,*) " "
      WRITE(IUO,'(" KOLM2_AS with  N =",2I6)') N
      DO ITER=1,NITER
        DO I=1,N
          CALL RANMAR(DATA(I))
        END DO 
        CALL HEAPSORT(N,DATA)
        DO I=1,N
          Fxct(I)=DATA(I)
        END DO 
        CALL KOLM1(N,Fxct,DEL1,DEL2,Q1,Q2)
        Q3=MIN(Q1,Q2)
        CALL KOLM2_AS(N,Fxct,DEL,Q)
        WRITE(IUO,'(" ITER,DEL,Q3,2Q3,Q_as =",I4,4F10.5)')
     &   ITER,DEL,Q3,(2*Q3),Q
      END DO 
C
      stop
      end

      include '../../ForLib/gamma_ln.f'
      include '../../ForLib/cau_df.f'
      include '../../ForLib/heapsort.f'
      include '../../ForLib/rmaset.f'
      include '../../ForLib/ranmar.f'
      include '../../ForLib/rmacau.f'
      include '../../ForLib/kolm1.f'
      include '../../ForLib/kolm2_as.f'
