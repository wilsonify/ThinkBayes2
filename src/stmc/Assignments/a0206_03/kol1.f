      PROGRAM KOL1
C Bernd Berg, Mar 17 2001.
C Examples for the 1-sided Kolmogorov test.
C LCAU=.FALSE.: uniform random numbers and
C LCAU=.TRUE.:  Cauchy random numbers.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER(IUO=6,IUD=10,ISEED1=1,ISEED2=0)
      DIMENSION DATA(100),Fxct(100)
      LCAU=.TRUE.
      LCAU=.FALSE.
C 
      IF(.NOT.LCAU) WRITE(IUO,*) "Uniform random numbers:"
      IF(LCAU)      WRITE(IUO,*) "Cauchy random numbers:"
      N=8
1     CONTINUE
        CALL RMASET(IUO,-IUD,ISEED1,ISEED2,"no_file")
        DO I=1,N
          IF(.NOT.LCAU) CALL RANMAR(DATA(I))
          IF(LCAU)      CALL RMACAU(DATA(I))
        END DO 
        CALL HEAPSORT(N,DATA)
        DO I=1,N
          IF(.NOT.LCAU) Fxct(I)=DATA(I)
          IF(LCAU)      Fxct(I)=CAU_DF(DATA(I))
        END DO 
        CALL KOLM1(N,Fxct,DEL1,DEL2,Q1,Q2)
        WRITE(IUO,'(" N,Q1,Q2:",I6,2F10.2)') N,Q1,Q2
      IF(N==8) THEN
        N=100
      GO TO 1
      END IF
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

