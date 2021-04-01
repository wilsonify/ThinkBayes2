      PROGRAM CHI2_TEST2
C Bernd Berg, Jan 18 2003.
C CHI2 Test for events from one dice versus another.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER(IUO=6,IUD=10,ISEED1=1,ISEED2=0)
      PARAMETER(NHIST=6,NDA1=1000,NDA2=1000,NRPT=10 000,NHM1=NHIST-1)
      DIMENSION HIST1(NHIST),HIST2(NHIST),Q(NRPT)
      COMMON /CHI2PAR/ NF
      NF=NHIST-1
      FCT=ONE*NHIST
      RATIO=(ONE*NDA1)/(ONE*NDA2)
c     WRITE(IUO,'(" X:")') 
c     WRITE(IUO,'(6F10.0)') X
C 
      CALL RMASET(IUO,-IUD,ISEED1,ISEED2,"no_file")
      DO IRPT=1,NRPT
        DO IHIST=1,NHIST
          HIST1(IHIST)=ZERO
          HIST2(IHIST)=ZERO
        END DO
        DO IDAT=1,NDA1
          IDICE=1+INT(FCT*RMAFUN())
          HIST1(IDICE)=HIST1(IDICE)+ONE
        END DO 
        DO IDAT=1,NDA2
          IDICE=1+INT(FCT*RMAFUN())
          HIST2(IDICE)=HIST2(IDICE)+ONE
        END DO 
c       WRITE(IUO,'(" IRPT =",I10)') IRPT
c       WRITE(IUO,'(6F10.0)') HIST
        CHI2=ZERO
        DO IHIST=1,NHIST
          SUM=HIST1(IHIST)+RATIO**2*HIST2(IHIST)
          IF(SUM.LT.HALF) STOP "CHI2_TEST: Singular, no event."
          CHI2=CHI2+(HIST1(IHIST)-RATIO*HIST2(IHIST))**2/SUM 
        END DO 
        Q(IRPT)=1-CHI2_DF(CHI2)
c       PRINT*,"CHI2,Q:",CHI2,Q(IRPT)
      END DO 
C
      CALL HEAPSORT(NRPT,Q)
      OPEN(IUD,FILE="chi2_2.d",form="formatted",status="unknown")
      DO IRPT=1,NRPT
        F=(ONE*IRPT)/(NRPT+1)
        IF(F.GT.HALF) F=ONE-F
        WRITE(IUD,'(4F11.5)') Q(IRPT),F
      END DO
      CLOSE(IUD)
C
      stop
      end


      include '../../ForLib/chi2_df.f'
      include '../../ForLib/gamma_ln.f'
      include '../../ForLib/gamma_p.f'
      include '../../ForLib/heapsort.f'
      include '../../ForLib/rmaset.f'
      include '../../ForLib/rmafun.f'

