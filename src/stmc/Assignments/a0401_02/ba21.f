      PROGRAM BA21 ! Variant of BAUTO
C
C - INTEGRATED AUTOCORRELATION TIME BY BINNING:
C   TESTED FOR GAUSSIAN DATA GENERATED BY METROPOLIS MC.
C
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER(IUO=6,IUD=10,ISEED1=1,ISEED2=0, K=21,NDAT=2**K)
      PARAMETER(KBMAX=K-14, A=THREE)
      DIMENSION DATA(NDAT),DATB(NDAT),DBE(0:K)
      COMMON /CHI2PAR/ NF         
C
      WRITE(IUO,*) ' '
      WRITE(IUO,*) 'NDAT,NBIN = ',NDAT,NBIN
C
      CALL RMASET(IUO,IUD,ISEED1,ISEED2,'ranmar.d')
      WRITE(IUO,*) 'Metropolis Range A = ',A
      CALL GAU_METRO(A,NDAT,DATA,XAM)
      WRITE(IUO,*) 'METROPOLIS FOR GAUSSIAN RANDOM NUMBERS: '
      WRITE(IUO,*) 'ACCEPTANCE RATE: ',XAM
C
      WRITE(IUO,*) '  '
      WRITE(IUO,'(" BININGS",9X,"MEAN",6X,"VARIANCE",5X,"ERROR BAR",
     &              9X,"RATIO")')
      OPEN(UNIT=IUD,FILE='b_aint21.d',form='formatted',status='unknown') 
      DO KB=0,KBMAX
        NBIN=2**KB
        NBINS=NDAT/NBIN
        CALL BINING(NDAT,DATA,NBINS,DATB)
        CALL STEB0(NBINS,DATB, DBMEAN,DBV,DBE(KB))
        RATIO=(DBE(KB)/DBE(0))**2
        WRITE(IUO,'(1I7,4F14.6)') KB,DBMEAN,DBV,DBE(KB),RATIO
        NF=NBINS-1
C 97.5% and 0.25\% confidence limits:
        RAT_U=RATIO/chi2pdf_xq(P025)
        RAT_D=RATIO/chi2pdf_xq(P975)
C Bounds corresponding to the one sigma level:
        RAT_U=RATIO+HALF*(RAT_U-RATIO)
        RAT_D=RATIO+HALF*(RAT_D-RATIO)
        XBIN=ONE*(NBIN-1)
        WRITE(IUD,'(I4,F8.2,I10,3F10.5)')
     &    KB,XBIN,NBINS,RATIO,RAT_U,RAT_D
      END DO
      CLOSE(IUD)
C
      STOP
      END

      INCLUDE '../../ForLib/bining.f'
      INCLUDE '../../ForLib/gau_metro.f'
      INCLUDE '../../ForLib/ranmar.f'
      INCLUDE '../../ForLib/rmaset.f'
      INCLUDE '../../ForLib/steb0.f'

      include '../../ForLib/chi2pdf_xq.f'
      include '../../ForLib/chi2_xq.f'
      include '../../ForLib/chi2_df.f'
      include '../../ForLib/fi1.f'
      include '../../ForLib/gamma_ln.f'
      include '../../ForLib/gamma_p.f'

