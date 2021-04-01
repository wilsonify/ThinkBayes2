
C Copyright, Bernd Berg, September 27, 2002.

      SUBROUTINE LGNU(IUD,N,X,Y,EY,A,SGA,COV,PROB)
C PROB:  CONFIDENCE LEVEL FOR ELLIPSE.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
C ===> YOU CAN SET XMIN,XMAX DIFFERENT FROM DEFAULT:
      PARAMETER(NFIG=200,LMIMA=.FALSE.,XMIN=-3.0D0,XMAX=+6.0D00)
      DIMENSION X(N),Y(N),EY(N), A(2),SGA(2),COV(2,2)
      DIMENSION COVDD(2),SGB(2),EIGVS(2,2)
C
C 1. REGRESSION LINE
C
      OPEN(UNIT=IUD,FILE='lfit.d1',STATUS='UNKNOWN')
      DO I=1,N
        WRITE(IUD,200) X(I),Y(I),EY(I)
      END DO
200   FORMAT(1X,4G16.7)
      CLOSE(IUD)
C
C 2. CONFIDENCE ELLIPSE:
C
      CALL EIGEN_2X2(COV,COVDD,EIGVS)
      SGB(1)=SQRT(COVDD(1))
      SGB(2)=SQRT(COVDD(2))
      CALL ELLIPSE(A,SGA,SGB,EIGVS,PROB,IUD)
C 
C 3. INCLUDE CONFIDENCE LIMITS FOR REGRESSION LINE:
C
                DELX=(X(N)-X(1))/NFIG
      IF(LMIMA) DELX=(XMAX-XMIN)/NFIG
C
      OPEN(UNIT=IUD,FILE='lfit.d2',STATUS='UNKNOWN')
      DO I=0,NFIG
        XX=X(1)+I*DELX
        IF(LMIMA) XX=XMIN+I*DELX
        VARB1=ABS(EIGVS(1,1)+XX*EIGVS(2,1))**2*COVDD(1)
        VARB2=ABS(EIGVS(1,2)+XX*EIGVS(2,2))**2*COVDD(2)
        YM=A(1)+A(2)*XX-SQRT(VARB1+VARB2)
        YY=A(1)+A(2)*XX
        YP=A(1)+A(2)*XX+SQRT(VARB1+VARB2)
        WRITE(IUD,200) XX,YM,YY,YP
      END DO
      CLOSE(IUD)
C
      RETURN
      END


      SUBROUTINE SUBL(IUO,N,X,Y,EY)
C SUBL - linear package. Straight line fit y=a1+a2*x.
      include '../../ForLib/implicit.sta'
      DATA NCALL/0/
      SAVE NCALL
      NCALL=NCALL+1
      IF(NCALL.EQ.1) WRITE(IUO,'(1X,
     & "SUBL: STRAIGHT LINE FIT   Y=A(1)+A(2)*X.",/,"  ")')
      RETURN
      END
    
 
      SUBROUTINE SUBPLOT(IUO,IUD,N,X,Y,EY,A)
C subl.linear package
C Straight line fit y=a1+a2*x.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER(NPLOT=200)
      DIMENSION X(N),Y(N),EY(N),A(2)

      WRITE(IUO,*) "SUBPLOT: Data for fit.plt, lfit.plt and gaudif.plt."

      OPEN(UNIT=IUD,FILE='plot.d',STATUS='UNKNOWN')
      DELX=(X(N)-X(1))/NPLOT
      DO I=0,NPLOT
        XX=X(1)+I*DELX
        YY=A(1)+A(2)*XX
        WRITE(IUD,'(2G16.7)') XX,YY
      END DO
      CLOSE(IUD)

      OPEN(UNIT=IUD,FILE='gaudif.d',STATUS='UNKNOWN')
      QM=ZERO
      DO I=1,N
        YY=A(1)+A(2)*X(I)
        CALL GAUDIF(Y(I),EY(I),YY,ZERO,Q)
        WRITE(IUD,'(5G16.7)') X(I),Y(I),EY(I),YY,Q
        QM=QM+Q
      END DO
      CLOSE(IUD)
      WRITE(IUO,*) "Average of Gaussian difference tests =",(QM/N)
      WRITE(IUO,*) " "

      RETURN
      END
