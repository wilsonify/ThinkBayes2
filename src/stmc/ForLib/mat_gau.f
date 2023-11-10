      Subroutine MAT_GAU(AMAT,N,BMAT,M)
C Invert a square matrix
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER(NMAX=100)
      Dimension AMAT(N,N),BMAT(N,M),BIN(NMAX),IK(NMAX),JK(NMAX)
      IF(N>NMAX) STOP "MAT_GAU: Enlarge NMAX (N.GT.NMAX)!"
      LTEST=.TRUE.
      LTEST=.FALSE.
      
C - Find largest element.
      DO K=1,N

        AMAX=ZERO
1       DO I=K,N
          DO J=K,N
            IF(Abs(AMAT(I,J))>ABS(AMAX)) THEN
              AMAX=AMAT(I,J)
              IK(K)=I
              JK(K)=J
            END IF
          END DO ! DO J
        END DO ! DO I
        IF(AMAX==ZERO) STOP "MAT_GAU: Singular Matrix."
        DET=ONE
C - Interchange rows and columns to put AMAX in AMAT(k,k).
        I=IK(K)
        IF(I<K) GOTO 1
	IF(I>K) THEN
          DO J=1,N
            AA=AMAT(K,J)
	    AMAT(K,J)=AMAT(I,J)
            AMAT(I,J)=-AA
          END DO ! do j
	ENDIF ! IF I.GT.k.

        J=JK(K)
        IF(J<K) GOTO 2
	IF(J>K) THEN
          DO I=1,N
            AA=AMAT(I,K)
	    AMAT(I,K)=AMAT(I,J)
            AMAT(I,J)=-AA
          END DO ! DO I.
        ENDIF ! IF J.GT.K.

C - Accumulate elements of the inverse matrix.
        DO I=1,N
          IF(I/=K) AMAT(I,K)=-AMAT(I,K)/AMAX
        END DO ! DO I.
        DO I=1,N
          DO J=1,N
	    IF((I/=K).AND.(J/=K))
     1        AMAT(I,J)=AMAT(I,J)+AMAT(I,K)*AMAT(K,J)
          END DO ! DO J.
        END DO ! DO I.
	DO J=1,N
          IF(J/=K) AMAT(K,J)=AMAT(K,J)/AMAX
        END DO ! DO J.
        AMAT(K,K)=ONE/AMAX
        DET=DET*AMAX ! Determinant (presently not returned on output).
      END DO ! DO K.
2     CONTINUE  
      IF(LTEST) STOP "MAT_GAU - LTEST."

C - Restore ordering of the matrix.
      DO IL=1,N
        K=N+1-IL
        J=IK(K)
        IF(J>K) THEN
          DO I=1,N
            AA=AMAT(I,K)
            AMAT(I,K)=-AMAT(I,J)
            AMAT(I,J)=AA
          END DO ! DO I.
	ENDIF ! IF J.GT.K.
        I=JK(K)
        IF(I>K) THEN
          DO J=1,N
            AA=AMAT(K,J)
	    AMAT(K,J)=-AMAT(I,J)
            AMAT(I,J)=AA
          END DO ! DO J.
	ENDIF ! IF I.GT.K.
      END DO ! DO IL.

C - Calculate the solution vector.
      DO K=1,M
        DO I=1,N
          BIN(I)=BMAT(I,K)
        END DO ! DO I.
        DO I=1,N
          BMAT(I,K)=ZERO
          DO J=1,N
            BMAT(I,K)=BMAT(I,K)+AMAT(I,J)*BIN(J)
          END DO ! DO J.
        END DO ! DO I.
      END DO ! DO K.

      RETURN
      END
