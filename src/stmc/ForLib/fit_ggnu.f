      SUBROUTINE FIT_GGNU(IUO,IUD,NDAT,NFIT,X,Y,SIGY,A)
C Copyright, Bernd Berg, September 27, 2002.
C Prepares data files data.d and gfit.d for gnuplot with gfit.plt.
      include 'implicit.sta'
      include 'constants.par'
      PARAMETER(NPLOT=200,MMAX=20)
      DIMENSION X(NDAT),Y(NDAT),SIGY(NDAT),A(NFIT),DYDA(MMAX)
      IF(NFIT.GT.MMAX) STOP "FIT_GGNU: MMAX too small."
C
      WRITE(IUO,*) "   "
      WRITE(IUO,*) "FIT_GGNU: Write data for gfit.plt and gaudif.plt"
      OPEN(IUD,file='data.d',form='formatted',status='unknown')
      DO I=1,NDAT
	CALL SUBG(X(I),A,YY,DYDA,NFIT)
        WRITE(IUD,'(5G14.6)') X(I),Y(I),SIGY(I),YY,(Y(I)-YY)
      END DO
      CLOSE(IUD)
C
      OPEN(IUD,file='gfit.d',form='formatted',status='unknown')
      DELX=(X(NDAT)-X(1))/NPLOT
      DO I=0,NPLOT
        XX=X(1)+I*DELX
	CALL SUBG(XX,A,YY,DYDA,NFIT)
        WRITE(IUD,'(2G16.7)') XX,YY
      END DO
      CLOSE(IUD)
C
      OPEN(IUD,file='gaudif.d',form='formatted',status='unknown')
      QM=ZERO
      DO I=1,NDAT
	CALL SUBG(X(I),A,YY,DYDA,NFIT)
        CALL GAUDIF(Y(I),SIGY(I),YY,ZERO,Q)
        WRITE(IUD,'(5G14.6)') X(I),Y(I),SIGY(I),YY,Q
        QM=QM+Q
      END DO
      CLOSE(IUD)
      WRITE(IUO,*) "Average of the Gaussian difference tests:",QM/NDAT
      RETURN
      END
