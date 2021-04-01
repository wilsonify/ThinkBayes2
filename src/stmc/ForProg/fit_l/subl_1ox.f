    
C Copyright, Bernd Berg, November 1, 2000.
 
      SUBROUTINE SUBL(IUO,N,X,Y,EY)
C subl_1ox package
C Fit y=c1+c2/x -> ynew=c1*x+c2=a1+a2*x 
      include '../../ForLib/implicit.sta'
      DIMENSION X(N),Y(N),EY(N)
      WRITE(IUO,*) 'SUBL: FIT Y=C1+C2/X -> Ynew=Y*X=C1*X+C2'
      WRITE(IUO,*) '           =A1+A2*X with A1=C2, A2=C1.'
      WRITE(IUO,*) '  '
      DO I=1,N
        Y(I)=Y(I)*X(I)
        EY(I)=EY(I)*X(I)
      END DO
      RETURN
      END
     
      SUBROUTINE SUBPLOT(IUO,IUD,N,X,Y,EY,A)
C subl_1ox package
C Fit y=c1+c2/x -> ynew=c1*x+c2=a1+a2*x 
      include '../../ForLib/implicit.sta'
      PARAMETER(NPLOT=200)
      DIMENSION X(N),Y(N),EY(N),A(2)
      WRITE(IUO,*) 'fit.plt gives plot y=c1+c2/x.'
      WRITE(IUO,*) '  '
      C1=A(2)
      C2=A(1)
      DO I=1,N
        Y(I)=Y(I)*X(I)
        EY(I)=EY(I)*X(I)
      END DO
      OPEN(UNIT=IUD,FILE='plot.d',STATUS='UNKNOWN')
      DELX=(X(N)-X(1))/NPLOT
      DO I=0,NPLOT
        XX=X(1)+I*DELX
        YY=C1+C2/XX
        WRITE(IUD,'(2G16.7)') XX,YY
      END DO
      CLOSE(IUD)
      RETURN
      END
