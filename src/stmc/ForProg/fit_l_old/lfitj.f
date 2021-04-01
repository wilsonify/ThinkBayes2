      PROGRAM LFITJ
C
C Copyright, Bernd Berg, September 27, 2002.
C Jackknife version of the LINEAR FIT: Y=A*X+B.
C
C INPUT:   GAUSSIAN DATA          Y (I), I=1,...,N  
C          AND THEIR ERROR BARS   YE(I), I=1,...,N.
C
C OUTPUT:  CONSTANTS  AM(1),AM(2) WITH Jackknife ERRORS BARS AE(1),AE(2),
C          CORRESPONDING TO THE FIT Y=AM(1)+AM(2)*X.
C
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER(IUO=06,IUD=10,NMAX=100,NBINS=32)
      CHARACTER CBIN*2
      DIMENSION X(NMAX),Y(NMAX),YE(NMAX),AJ(0:NBINS,2),AM(2),AE(2)
      DIMENSION SGA(2),COVJ(0:NBINS,2,2),COVM(2,2)
      LTEST=.TRUE.  
      LTEST=.FALSE.  
      IF(LTEST) STOP "LFITJ: LTEST."

      WRITE(IUO,*) '  ' 
      WRITE(IUO,*) 'LINEAR REGRESSION JACKKNIFED:' 
      DO IBIN=0,NBINS 
        WRITE(CBIN,'(I2.2)') IBIN
        OPEN(IUD,FILE="datj"//CBIN//".d",FORM='FORMATTED',STATUS='OLD')
        IF(IBIN.EQ.0) THEN
          READ(IUD,*) NBINS_IN,NDAT
          WRITE(IUO,'(" NBINS,NBINS_IN,NDAT:",3I9)') NBINS,NBINS_IN,NDAT
          IF(NBINS.NE.NBINS_IN) STOP "NBINS must equal NBINS_IN."
        END IF
        DO IDAT=1,NDAT
          READ(IUD,*) X(IDAT),Y(IDAT),YE(IDAT)
        END DO
        CLOSE(IUD)
C Write mean value data on file for use with gnuplot:
        IF(IBIN.EQ.0) THEN
          OPEN(IUD,FILE="data.d",STATUS='UNKNOWN')
          DO IDAT=1,NDAT
            WRITE(IUD,'(3G16.7)') X(IDAT),Y(IDAT),YE(IDAT)
          END DO
          CLOSE(IUD)
        END IF
        CALL SUBL(IUO,NDAT,X,Y,YE)
        CALL FIT_L(NDAT,X,Y,YE,AM,SGA,CHI2,Q,COVJ(IBIN,1,2))
        AJ(IBIN,1)=AM(1)
        AJ(IBIN,2)=AM(2)
        COVJ(IBIN,1,1)=SGA(1)**2
        COVJ(IBIN,2,2)=SGA(2)**2
      END DO

      CALL STEBJ0(NBINS,AJ(1,1),AM(1),AV,AE(1))
      CALL STEBJ0(NBINS,AJ(1,2),AM(2),AV,AE(2))
      WRITE(IUO,'(/," A1M =",F14.8," +/=",F14.8)') AM(1),AE(1)
      WRITE(IUO,'(  " A2M =",F14.8," +/=",F14.8)') AM(2),AE(2)
      CALL STEBJ0(NBINS,COVJ(1,1,1),COVM(1,1),CV,CE)
      CALL STEBJ0(NBINS,COVJ(1,1,2),COVM(1,2),CV,CE)
      COVM(2,1)=COVM(1,2)
      CALL STEBJ0(NBINS,COVJ(1,2,2),COVM(2,2),CV,CE)
C
C OUTPUT TO PLOT RESULTS WITH GNUPLOT:
C ====================================
C
C SET CONFIDENCE ELLIPSE TO STANDARD:
      PROB=-ONE
      CALL FIT_LGNU(IUD,NDAT,X,Y,YE,AM,AE,COVM,PROB)
      CALL SUBPLOT(IUO,IUD,NDAT,X,Y,YE,AM)

      CALL AUTOJ_TAU(IUO,NBINS,AJ(0,2)) ! Application routine.

      STOP
      END

      include '../../ForLib/error_f.f'
      include '../../ForLib/gamma_p.f'
      include '../../ForLib/gamma_ln.f'
      include '../../ForLib/ellipse.f'
      include '../../ForLib/eigen_2x2.f'
c     include 'fit_l.f'
      include '../../ForLib/fit_l.f'
      include '../../ForLib/fit_lgnu.f'
      include '../../ForLib/gaudif.f'
      include '../../ForLib/stebj0.f'
      include 'subl_exp.f'
      include '../../ForLib/autoj_tau.f'
