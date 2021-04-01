      PROGRAM UNCOR_GAU
C
C Copyright, Bernd Berg, Feb 11, 2001.
C - CORRELATION FUNCTION AND INTEGRATED AUTOCORRELATION TIME: 
C   TESTED FOR UNCORRELATED GAUSSIAN RANDOM NUMBERS. 
C
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER(IUO=6,IUD=10,ISEED1=1,ISEED2=0)
      PARAMETER(K=17,NDAT=2**K,NBINS=32,LMEAN=.FALSE.,A=THREE,NT=32)
      DIMENSION DATA(NDAT),ACORJ(NBINS,0:NT),ACOR(0:NT)
      DIMENSION ACINTJ(NBINS,0:NT),ACINT(0:NT),WORK(NBINS)
C
      LTEST=.true.
      LTEST=.false.
C
      WRITE(IUO,*) ' '
      WRITE(IUO,*) 'NDAT  =  ',NDAT
      WRITE(IUO,*) 'NBINS =  ',NBINS
      WRITE(IUO,*) 'RANGE A = ',A
C
C Generation of Gaussian random numbers:
C
      CALL RMASET(IUO,IUD,ISEED1,ISEED2,'ranmar.d')
      IDAT=0
      DO IDATH=1,(NDAT/2)
        CALL RMAGAU(DATA(2*IDATH-1),DATA(2*IDATH))
      END DO
      CALL STEB0(NDAT,DATA,DM,DV,DE)
      WRITE(IUO,*) "DM,DV,DE:",DM,DV,DE
C
      OPEN(IUD,FILE='uacor.d',FORM='formatted',STATUS='unknown')
      WRITE(IUO,*) ' '
      DO IT=0,NT
        ACOR(IT)=AUTCORF(IT,NDAT,DATA,LMEAN)
        CALL AUTCORJ(IT,NDAT,NBINS,DATA,WORK,ACORJ(1,IT),LMEAN)
        CALL STEBJ0(NBINS,ACORJ(1,IT),ACM,ACV,ACE)
        WRITE(IUO,'(" IT,ACOR:",1I6,2F15.9," +/-",1F15.9)') 
     &                IT,ACOR(IT),ACM,ACE
        ABIAS=100*ABS(ACOR(IT)-ACM)/ABS(ACOR(IT)) ! Bias in %.
        WRITE(IUD,'(1I6,3F14.8,1F14.4)') IT,ACOR(IT),ACM,ACE,ABIAS
        ACM_s=HALF+50*ACM
        ACE_s=50*ACE 
      END DO
      CLOSE(IUD)
      IF(LTEST) STOP "UNCOR_GAU: LTEST."
C
      OPEN(IUD,FILE='uat_int.d',FORM='formatted',STATUS='unknown')
      CALL AC_INT(NT,ACOR,ACINT)
      CALL AC_INTJ(NT,NBINS,ACORJ,ACINTJ)
      WRITE(IUO,*) "  "
      DO IT=0,NT
        CALL STEBJ0(NBINS,ACINTJ(1,IT),ACM,ACV,ACE)
        WRITE(IUO,'(" IT,ACINT:",1I6,2F15.9," +/-",1F15.9)') 
     &              IT,ACINT(IT),ACM,ACE
        ABIAS=100*ABS(ACINT(IT)-ACM)/ACINT(IT) ! Bias in %.
        WRITE(IUD,'(1I6,3F14.8,1F14.4)') IT,ACINT(IT),ACM,ACE,ABIAS
      END DO
      CLOSE(IUD)
C
      STOP
      END


      INCLUDE '../../ForLib/ac_int.f'
      INCLUDE '../../ForLib/ac_intj.f'
      INCLUDE '../../ForLib/autcorf.f'
      INCLUDE '../../ForLib/autcorj.f'
      INCLUDE '../../ForLib/bias.f'
      INCLUDE '../../ForLib/bining.f'
      INCLUDE '../../ForLib/datjack.f'
      INCLUDE '../../ForLib/ranmar.f'
      INCLUDE '../../ForLib/rmagau.f'
      INCLUDE '../../ForLib/rmaset.f'
      INCLUDE '../../ForLib/steb0.f'
      INCLUDE '../../ForLib/stebj0.f'
      INCLUDE '../../ForLib/stmean.f'
