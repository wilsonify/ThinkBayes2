      FUNCTION FI1(F,Y,X1,X2) 
C Copyright Bernd Berg, Sep 17, 2000.
C INVERSE OF THE FUNCTION F.
C RESULT:     FI1=X SUCH THAT Y=F(X). 
C PRECISSION: EPS=1/10**8 can be changed, see below.
C METHOD:     BISECTING INTERVAL, INITIAL RANGE [X1,X2] (X1<X2).
C ASSUMPTION: F(X) MONOTON (IN- OR DECREASING) IN THE INTERVAL [X1,X2].
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      EXTERNAL F
      EPS=ONE/TEN**8
C MAXIMUM NUMBER OF ITERATIONS:
      ITERMAX=1000
      ITER=0
C
      XX1=X1
      XX2=X2
1     Y1=F(XX1)
      Y2=F(XX2)
C
      IF(Y1>Y2) THEN
      XX1=X2
      XX2=X1
      GO TO 1
      END IF
C
      IF(Y<=Y1 .OR. Y>=Y2) GO TO 2
C
3     CONTINUE
      ITER=ITER+1
      IF(ITER>ITERMAX) GO TO 2
        XX=HALF*(XX1+XX2)
        FF=F(XX)
        IF(FF<=Y) XX1=XX
        IF(FF>Y) XX2=XX
      IF(ABS(XX2-XX1)>EPS ) GO TO 3
C
      FI1=HALF*(XX2+XX1) 
C
      RETURN
2     PRINT*,'FI1: NO CONVERGENCE OR Y OUT OF RANGE !'
      PRINT*,'ITERMAX,EPS:',ITERMAX,EPS
      PRINT*,'Y1,Y,Y2:    ',Y1,Y,Y2
      STOP 'FI1'
      END
