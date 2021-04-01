      SUBROUTINE HEAPSORT(N,X)
C COPYRIGHT, BERND BERG, JUNE 16, 1999.
      include 'implicit.sta'
      DIMENSION X(N)
C
C BUILDING THE HEAP:
C ==================
      NHALF=N/2
      DO M=NHALF,1,-1
        TEMP=X(M) 
        I1=M
        I2=M+M
1       CONTINUE
C         INDEX THE LARGER OF X(I2) AND X(I2+1) AS I2:
          IF(I2.LT.N .AND. X(I2+1).GT.X(I2)) I2=I2+1
          IF(X(I2).GT.TEMP) THEN 
            X(I1)=X(I2)
            I1=I2
            I2=I1+I1
          ELSE  
            I2=N+1
          ENDIF
        IF(I2.LE.N) GO TO 1
        X(I1)=TEMP ! FINAL POSITION FOR TEMP=X(M).
      END DO
C
C SORTING THE HEAP:
C =================
      DO I=N,3,-1 ! I IS THE NUMBER OF STILL COMPETING ELEMENTS.
        TEMP=X(I)
        X(I)=X(1) ! STORE TOP OF THE HEAP.
        I1=1
        I2=2
2       CONTINUE
C         INDEX THE LARGER OF X(I2) AND X(I2+1) AS I2:
          IF((I2+1).LT.I .AND. X(I2+1).GT.X(I2)) I2=I2+1
          IF(X(I2).GT.TEMP) THEN  
            X(I1)=X(I2)
            I1=I2
            I2=I1+I1
          ELSE 
            I2=I
          ENDIF 
        IF(I2.LT.I) GO TO 2
        X(I1)=TEMP ! FINAL POSITION FOR TEMP=X(I).
      END DO 
      TEMP=X(2)
      X(2)=X(1)
      X(1)=TEMP
      RETURN
      END
