      PROGRAM BIN_HIS
C
C Copyright, Bernd Berg, April 4, 2000.
C HISTOGRAM OF BINNED UNIFORM RANDOM NUMBERS.
C  
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
C GNUPLOT filenames must be chose to match character*5!      
      PARAMETER(IUO=6,IUD=10,ISEED1=1,ISEED2=0)
      PARAMETER(NHIST=11,NDAT=10 000,NBINS=500,NGNU=100)
      DIMENSION HIST(NHIST),DATA(NDAT),DATB(NBINS)
C
C Mean = 0.5;  standard deviation = 1/sqrt(24*NBIN)
      NBIN=NDAT/NBINS
      sdv=ONE/sqrt((ONE*12*NBIN))
      WRITE(IUO,*) 'sdv,1/(2*sdv**2):',sdv,1/(two*sdv**2)
      XMIN=HALF-FOUR*sdv            ! Plot range: +/- FOUR
      XMAX=HALF+FOUR*sdv            ! standard deviations.
      CALL RMASET(IUO,IUD,ISEED1,ISEED2,'ranmar.d')
C
      DO I=1,NDAT
        CALL RANMAR(DATA(I))
      END DO
      CALL BINING(NDAT,DATA,NBINS,DATB)
      CALL HIST_GNU(-IUD,NHIST,NBINS,HIST,DATB,XMIN,XMAX)
C
      OPEN(IUD,FILE='gaus.d',form='formatted',status='unknown')
      DO I=-NGNU,NGNU
        x=HALF+(FOUR*sdv*I)/NGNU
        f=exp(-(x-HALF)**2/(TWO*sdv**2))/(sqrt(tpi)*sdv)
        ftest=sqrt((120*ONE)/pi)*exp(-(120*ONE)*(x-HALF)**2)
        write(iud,'(3F14.5)') x,f,ftest
      END DO
C
      STOP
      END

      include '../../ForLib/bining.f'
      include '../../ForLib/hist_gnu.f'
      include '../../ForLib/rmaset.f'
      include '../../ForLib/ranmar.f'
