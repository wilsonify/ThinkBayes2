      SUBROUTINE HEAP_PER(N,X,IPER)
C COPYRIGHT, BERND BERG, October 4, 2000.  AS HEAPSORT, IN 
C ADDITION THE ARRAY  IPER  KEEPS TRACK OF THE PERMUTATION.
      include 'implicit.sta'
      DIMENSION X(N),IPER(N)
C
      DO I=1,N
        IPER(I)=I ! Initialize IPER.
      END DO
C
C BUILDING THE HEAP:
C ==================
      NHALF=N/2
      DO M=NHALF,1,-1
        TEMP=X(M) 
        ITMP=IPER(M)
        I1=M
        I2=M+M
1       CONTINUE
C         INDEX THE LARGER OF X(I2) AND X(I2+1) AS I2:
          IF(I2.LT.N .AND. X(I2+1).GT.X(I2)) I2=I2+1
          IF(X(I2).GT.TEMP) THEN 
            X(I1)=X(I2)
            IPER(I1)=IPER(I2)
            I1=I2
            I2=I1+I1
          ELSE  
            I2=N+1
          ENDIF
        IF(I2.LE.N) GO TO 1
        X(I1)=TEMP ! FINAL POSITION FOR TEMP=X(M).
        IPER(I1)=ITMP
      END DO
C
C SORTING THE HEAP:
C =================
      DO I=N,3,-1 ! I IS THE NUMBER OF STILL COMPETING ELEMENTS.
        TEMP=X(I)
        ITMP=IPER(I)
        X(I)=X(1) ! STORE TOP OF THE HEAP.
        IPER(I)=IPER(1)
        I1=1
        I2=2
2       CONTINUE
C         INDEX THE LARGER OF X(I2) AND X(I2+1) AS I2:
          IF((I2+1).LT.I .AND. X(I2+1).GT.X(I2)) I2=I2+1
          IF(X(I2).GT.TEMP) THEN  
            X(I1)=X(I2)
            IPER(I1)=IPER(I2)
            I1=I2
            I2=I1+I1
          ELSE 
            I2=I
          ENDIF 
        IF(I2.LT.I) GO TO 2
        X(I1)=TEMP ! FINAL POSITION FOR TEMP=X(I).
        IPER(I1)=ITMP
      END DO 
      TEMP=X(2)
      ITMP=IPER(2)
      X(2)=X(1)
      IPER(2)=IPER(1)
      X(1)=TEMP
      IPER(1)=ITMP
      RETURN
      END
