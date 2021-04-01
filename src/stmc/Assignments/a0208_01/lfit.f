      PROGRAM LFIT
C
C Copyright, Bernd Berg, September 27, 2002.
C LINEAR FIT: FIT OF INPUT DATA BY STRAIGHT LINE Y=A*X+B.
C
C INPUT:   GAUSSIAN DATA          Y (I), I=1,...,N  
C          AND THEIR ERROR BARS   YE(I), I=1,...,N.
C
C OUTPUT:  CONSTANTS  A(1), A(2) WITH ERRORS BARS SGA(1), SGA(2).
C          CORRESPONDING TO THE FIT Y=A(1)+A(2)*X. IF N.GE.3:
C          ALSO  CHI2  AND  THE GOODNESS OF FIT Q ARE RETURNED.
C
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER(IUO=06,IUD=10,NMAX=100)
      DIMENSION X(NMAX),Y(NMAX),YE(NMAX), A(2),SGA(2),COV(2,2)
C
      WRITE(IUO,*) '  '
      WRITE(IUO,*) 'LINEAR REGRESSION: '
C
      OPEN(IUD,FORM='FORMATTED',STATUS='OLD')
      READ(IUD,*)   N
      WRITE(IUO,*) N, '   DATA FROM FOR008.DAT USED'
      DO I=1,N
        READ(IUD,*) X(I),Y(I),YE(I)
      END DO
      CLOSE(IUD)
C Write original data on file for use with gnuplot:
      OPEN(UNIT=IUD,FILE='data.d',STATUS='UNKNOWN')
      DO I=1,N
        WRITE(IUD,'(3G16.7)') X(I),Y(I),YE(I)
      END DO
      CLOSE(IUD)
      CALL SUBL(IUO,N,X,Y,YE)
      CALL FIT_L(N,X,Y,YE,A,SGA,CHI2,Q,COV(1,2))
      WRITE(IUO,*) 'RESULTS: '
      WRITE(IUO,*) 'A(1) =',A(1),'    +/-  ',SGA(1)
      WRITE(IUO,*) 'A(2) =',A(2),'    +/-  ',SGA(2)
      WRITE(IUO,*) '      '
      COV(1,1)=SGA(1)**2
      COV(2,1)=COV(1,2)
      COV(2,2)=SGA(2)**2
      WRITE(IUO,*) 'COVARIANCE MATRIX:  '
      WRITE(IUO,*) COV(1,1),COV(1,2)
      WRITE(IUO,*) COV(2,1),COV(2,2)
      IF(N.GT.2) THEN
        WRITE(IUO,*) '      '
        WRITE(IUO,*) 'STATISTICAL ANALYSIS: '
        WRITE(IUO,*) 'CHI2 =',CHI2
        WRITE(IUO,*) '   Q =',Q
        WRITE(IUO,*) 'PLOTS: fit.plt; lfit.plt and ellipse.plt'
        WRITE(IUO,*) '      '
      END IF
C
C OUTPUT TO PLOT RESULTS WITH GNUPLOT:
C ====================================
C
C SET CONFIDENCE ELLIPSE TO STANDARD:
      PROB=-ONE
      CALL FIT_LGNU(IUD,N,X,Y,YE,A,SGA,COV,PROB)
      CALL SUBPLOT(IUO,IUD,N,X,Y,YE,A)
C
      STOP
      END

      include '../../ForLib/error_f.f'
      include '../../ForLib/gamma_p.f'
      include '../../ForLib/gamma_ln.f'
      include '../../ForLib/gaudif.f'
      include '../../ForLib/ellipse.f'
      include '../../ForLib/eigen_2x2.f'
      include '../../ForLib/fit_l.f'
      include '../../ForLib/fit_lgnu.f'
      include '../../ForProg/fit_l/subl_linear.f'
c     include '../../ForProg/fit_l/subl_1ox.f'
c     include '../../ForProg/fit_l/subl_power.f'
