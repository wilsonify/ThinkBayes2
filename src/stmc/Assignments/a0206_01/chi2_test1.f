      PROGRAM CHI2_TEST1
C Bernd Berg, Jan 18 2003.
C CHI2 Test for events from a dice versus the expectation.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER(IUO=6,IUD=10,ISEED1=1,ISEED2=0)
      PARAMETER(NHIST=6,NDAT=1000,NRPT=10 000,NHM1=NHIST-1)
      DIMENSION HIST(NHIST),X(NHIST),Q(NRPT),QFALSE(NRPT)
      COMMON /CHI2PAR/ NF
      FCT=ONE*NHIST
      DO IHIST=1,NHIST
        X(IHIST)=(ONE*NDAT)/(ONE*NHIST)
      END DO
c     WRITE(IUO,'(" X:")') 
c     WRITE(IUO,'(6F10.0)') X
C 
      CALL RMASET(IUO,-IUD,ISEED1,ISEED2,"no_file")
      DO IRPT=1,NRPT
        DO IHIST=1,NHIST
          HIST(IHIST)=ZERO
        END DO
        DO IDAT=1,NDAT
          IDICE=1+INT(FCT*RMAFUN())
          HIST(IDICE)=HIST(IDICE)+ONE
        END DO 
c       WRITE(IUO,'(" IRPT =",I10)') IRPT
c       WRITE(IUO,'(6F10.0)') HIST
        CHI2=ZERO
        DO IHIST=1,NHIST
          CHI2=CHI2+(HIST(IHIST)-X(IHIST))**2/X(IHIST)
        END DO 
        NF=NHIST-1
        Q(IRPT)=ONE-CHI2_DF(CHI2)
        NF=NHIST
        QFALSE(IRPT)=ONE-CHI2_DF(CHI2)
c       PRINT*,"IRPT,CHI2,Q:",IRPT,CHI2,Q(IRPT)
      END DO 
C
      CALL HEAPSORT(NRPT,Q)
      CALL HEAPSORT(NRPT,QFALSE)
      OPEN(IUD,FILE="chi2_1.d",form="formatted",status="unknown")
      DO IRPT=1,NRPT
        F=(ONE*IRPT)/(NRPT+1)
        IF(F.GT.HALF) F=ONE-F
        WRITE(IUD,'(4F11.5)') Q(IRPT),QFALSE(IRPT),F
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

