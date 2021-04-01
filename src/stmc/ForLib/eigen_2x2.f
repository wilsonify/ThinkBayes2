      SUBROUTINE EIGEN_2X2(AMAT,EVAL,EVCT)
C Real eigenvalues EVAL and eigenvectors EVCT of the AMAT 2x2 matrix.
C EVCT returns the eigenvectors stored column-wise. 
C AMAT is unchanged on exit.
      include 'implicit.sta'
      include 'constants.par'
      DIMENSION AMAT(2,2),EVAL(2),EVCT(2,2)
      PHALF=-HALF*(AMAT(1,1)+AMAT(2,2))
      Q=AMAT(1,1)*AMAT(2,2)-AMAT(1,2)*AMAT(2,1)
      ARG=PHALF**2-Q
      IF(ARG.LT.0) THEN
        PRINT*,"EIGEN_2X2 cannot be used. Eigenvalues are complex!" 
        STOP   "EIGEN_2X2 cannot be used. Eigenvalues are complex!" 
      END IF
      EVAL(1)=-PHALF+SQRT(ARG) ! lambda_1/2=-p/2 +/- sqrt((p/2)**2-q).
      EVAL(2)=-PHALF-SQRT(ARG)
C Eigenvectors:
      EPS=EM10
      DO I=1,2
        DENO1=EVAL(I)-AMAT(1,1)
        ABS1=ABS(DENO1)
        DENO2=EVAL(I)-AMAT(2,2)
        ABS2=ABS(DENO2)
        IF(ABS1.GT.ABS2) THEN
          IF(ABS1.EQ.EPS) THEN
            GO TO 1
          ELSE
            FACTOR=AMAT(1,2)/DENO1
            EVCT(2,I)=ONE/SQRT(ONE+FACTOR**2) 
            EVCT(1,I)=FACTOR*EVCT(2,I)
          END IF
        ELSE
          IF(ABS2.EQ.EPS) THEN
            GO TO 1
          ELSE
            FACTOR=AMAT(2,1)/DENO2
            EVCT(1,I)=ONE/SQRT(ONE+FACTOR**2) 
            EVCT(2,I)=FACTOR*EVCT(1,I)
          END IF
        END IF
      END DO
      RETURN
1     CONTINUE 
        EVCT(1,1)=ONE
        EVCT(2,1)=ZERO 
        EVCT(1,2)=ZERO
        EVCT(2,1)=ONE
      RETURN
      END
