      PROGRAM KOL1
C Bernd Berg, Mar 2 2001.
C Statistical investigation of the 1-sided Kolmogorov test.
C Examples: Uniform, Gaussian and Cauchy random numbers.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER(IUO=6,IUD=10,ISEED1=1,ISEED2=0)
C ICASE=1: Uniform,  ICASE=2: Gaussian,  ICASE=3: Cauchy.
      PARAMETER(N=5,NRPT=10000,ICASE=3)
      DIMENSION DATA(N),Fxct(N)
      DIMENSION Q1(NRPT),Q2(NRPT),Q3(NRPT)
      IF(ICASE.LT.1 .OR. ICASE.GT.3) STOP "KOL1: ICASE false."
C 
      OPEN(IUD,FILE="kol1.d",form="formatted",status="unknown")
      CALL RMASET(IUO,-IUD,ISEED1,ISEED2,"no_file")
      DO IRPT=1,NRPT
        DO I=1,N
          IF(ICASE.EQ.1) CALL RANMAR(DATA(I))
          IF(ICASE.EQ.2) CALL RMAGAU(DATA(I),XR2)
          IF(ICASE.EQ.3) CALL RMACAU(DATA(I))
        END DO 
        IF(N.GT.1) CALL HEAPSORT(N,DATA)
        DO I=1,N
          IF(ICASE.EQ.1) Fxct(I)=DATA(I)
          IF(ICASE.EQ.2) Fxct(I)=GAU_DF(DATA(I))
          IF(ICASE.EQ.3) Fxct(I)=CAU_DF(DATA(I))
        END DO 
        CALL KOLM1(N,Fxct,EPS1,EPS2,Q1(IRPT),Q2(IRPT))
        Q3(IRPT)=MIN(Q1(IRPT),Q2(IRPT))
      END DO 
C
      CALL HEAPSORT(NRPT,Q1)
      CALL HEAPSORT(NRPT,Q2)
      CALL HEAPSORT(NRPT,Q3)
      DO IRPT=1,NRPT
        F=(ONE*IRPT)/(NRPT+1)
        IF(F.GT.HALF) F=ONE-F
        WRITE(IUD,'(4F11.5)') Q1(IRPT),Q2(IRPT),Q3(IRPT),F
      END DO
      CLOSE(IUD)
C
      stop
      end


      include '../../ForLib/error_f.f'
      include '../../ForLib/gamma_ln.f'
      include '../../ForLib/gamma_p.f'
      include '../../ForLib/gau_df.f'
      include '../../ForLib/cau_df.f'
      include '../../ForLib/heapsort.f'
      include '../../ForLib/rmaset.f'
      include '../../ForLib/ranmar.f'
      include '../../ForLib/rmagau.f'
      include '../../ForLib/rmacau.f'
      include '../../ForLib/kolm1.f'

