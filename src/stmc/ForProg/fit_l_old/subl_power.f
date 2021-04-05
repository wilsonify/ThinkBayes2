
C Copyright, Bernd Berg, November 1, 2001.
     
      SUBROUTINE SUBL(IUO,N,X,Y,EY)
C Fit y=c1*exp(-c2*ln(x)) -> ynew=ln(y)=ln(c1)-c2*xnew
c     with xnew=ln(x). Then: a1=ln(c1), a2=-c2. 
      include '../../ForLib/implicit.sta'
      DIMENSION X(N),Y(N),EY(N)
      WRITE(IUO,*) 'SUBL: FIT Y=C1*EXP(-C2*LOG(X)) ->',
     &                  ' Ynew=LOG(Y)=LOG(C1)-C2*LOG(X)'
      WRITE(IUO,*) '           =A1+A2*Xnew with A1=LOG(C1), A2=-C2.'
      WRITE(IUO,*) '  '
      DO I=1,N
        YUP=LOG(Y(I)+EY(I))
        Y(I)=( YUP + LOG(Y(I)-EY(I)) )/2
        EY(I)=YUP-Y(I)
        X(I)=LOG(X(I))
      END DO
      RETURN
      END
     
      SUBROUTINE SUBPLOT(IUO,IUD,N,X,Y,EY,A)
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER(NPLOT=200)
      DIMENSION X(N),Y(N),EY(N),A(2)
      WRITE(IUO,*) 'fit.g gives plot y=c1*exp(-c2*ln(x)).'
      C1=EXP(A(1))
      C2=-A(2)
      WRITE(IUO,'(1X,"C1,C2 =",2G16.7)') C1,C2       
      DO I=1,N
        X(I)=EXP(X(I))
        Y(I)=EXP(Y(I))
      END DO
      OPEN(UNIT=IUD,FILE='plot.d',STATUS='UNKNOWN')
      DELX=(X(N)-X(1))/NPLOT
      DO I=0,NPLOT
        XX=X(1)+I*DELX
        YY=C1*XX**(-C2)
      WRITE(IUD,'(2G16.7)') XX,YY
      END DO
      CLOSE(IUD)
C
      OPEN(UNIT=IUD,FILE='data.d',STATUS='OLD')
      DO I=1,N
      READ(IUD,*) X(I),Y(I),EY(I)
      END DO
      REWIND IUD
      CHI2=0.D00
      DO I=1,N
        YY=C1*X(I)**(-C2)
        TCHI2=(Y(I)-YY)**2/EY(I)**2
        CHI2=CHI2+TCHI2
      WRITE(IUD,'(5G14.6)') X(I),Y(I),EY(I),YY,TCHI2
      END DO
      CLOSE(IUD)
      CHI2pdf=CHI2/(N-2)
      WRITE(IUO,*) 'CHI2 and CHI2 per d.g.f. =',CHI2,CHI2/(N-2)
      Q=0
      IF(N>2.AND.CHI2pdf<TEN) Q=ONE-GAMMA_P(HALF*(N-2),HALF*CHI2)
      WRITE(IUO,*) 'Goodness of fit =         ',Q
      WRITE(IUO,*) '  '
      RETURN
      END
