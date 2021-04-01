      PROGRAM K2
C Bernd Berg, Mar 2 2001.
C Kolmogorov DEL-distribution for two empirical uniform distributions.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      CHARACTER C1*2,C2*2 
      PARAMETER(IUO=6,IUD=10,ISEED1=1,ISEED2=0)
      PARAMETER(LUNI=.TRUE.,LCAU=.FALSE.,LGAU=.FALSE.)
C     PARAMETER(LUNI=.FALSE.,LCAU=.TRUE.,LGAU=.FALSE.)
C     PARAMETER(LUNI=.FALSE.,LCAU=.FALSE.,LGAU=.TRUE.)
c     PARAMETER(N1=01,N2=02,NRPT=1 000 000)
c     PARAMETER(N1=03,N2=05,NRPT=1 000 000)
c     PARAMETER(N1=06,N2=08,NRPT=1 000 000)
      PARAMETER(N1=12,N2=16,NRPT=1 000 000)
c     PARAMETER(N1=1,N2=2,NRPT=100)
      DIMENSION DAT1(N1),DAT2(N2),DEL(0:NRPT)
      ITEST=0
      IF(LUNI) WRITE(IUO,*) "Uniform random numbers."
      IF(LUNI) ITEST=ITEST+1
      IF(LCAU) WRITE(IUO,*) "Cauchy random numbers."
      IF(LCAU) ITEST=ITEST+1
      IF(LGAU) WRITE(IUO,*) "Gaussian random numbers."
      IF(LGAU) ITEST=ITEST+1
      IF(ITEST.NE.1) STOP "Random number options false or incompatible."
      ltest=.true.
      ltest=.false.
      IF(ltest) STOP 'ltest.'
C
      WRITE(C1,'(I2.2)') N1
      WRITE(C2,'(I2.2)') N2
      OPEN(IUD,FILE="K2_"//C1//"_"//C2//".d",
     &     form="formatted",status="unknown")
      CALL RMASET(-IUO,-IUD,ISEED1,ISEED2,"no_file")
c
      DO IRPT=1,NRPT
        DO I=1,N1
          IF(LUNI) CALL RANMAR(DAT1(I))
          IF(LCAU) CALL RMACAU(DAT1(I))
          IF(LGAU) CALL RMAGAU(DAT1(I),DAT_unused)
        END DO 
        DO I=1,N2
          IF(LUNI) CALL RANMAR(DAT2(I))
          IF(LCAU) CALL RMACAU(DAT2(I))
          IF(LGAU) CALL RMAGAU(DAT2(I),DAT_unused)
        END DO 
        CALL HEAPSORT(N1,DAT1)
        CALL HEAPSORT(N2,DAT2)
        CALL KOLM2_DEL2(N1,N2,DAT1,DAT2,DEL(IRPT))
      END DO 
C
      CALL HEAPSORT(NRPT,DEL(1))
      IRPT0=0
      DEL(0)=ZERO
      Q0=ONE
      WRITE(IUD,'(I10,2F12.6)') IRPT0,DEL(IRPT0),Q0
      DO IRPT=2,NRPT
        IF(DEL(IRPT).GT.DEL(IRPT-1)) THEN
          F=(ONE*IRPT)/(NRPT+1)
          Q=ONE-F
          WRITE(IUD,'(I10,2F12.6)') IRPT,DEL(IRPT-1),Q0
          WRITE(IUD,'(I10,2F12.6)') IRPT,DEL(IRPT-1),Q
          Q0=Q
        END IF
      END DO
      WRITE(IUD,'(I10,2F12.6)') NRPT,DEL(NRPT),Q0
      WRITE(IUD,'(I10,2F12.6)') NRPT,DEL(NRPT),Q
      CLOSE(IUD)
C
      stop
      end

      include '../../ForLib/heapsort.f'
      include '../../ForLib/rmaset.f'
      include '../../ForLib/ranmar.f'
      include '../../ForLib/rmacau.f'
      include '../../ForLib/rmagau.f'
      include '../../ForLib/kolm2_del2.f'
