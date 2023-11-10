      SUBROUTINE HEAPISORT(N,IX)
C COPYRIGHT, BERND BERG, FEB 14, 2003.
C
      include '../../ForLib/implicit.sta'
      DIMENSION IX(N)
C
C BUILDING THE HEAP:
C ==================
      NHALF=N/2
      DO M=NHALF,1,-1
        ITEMP=IX(M) 
        I1=M
        I2=M+M
1       CONTINUE
C         INDEX THE LARGER OF IX(I2) AND IX(I2+1) AS I2:
          IF(I2<N .AND. IX(I2+1)>IX(I2)) I2=I2+1
          IF(IX(I2)>ITEMP) THEN
            IX(I1)=IX(I2)
            I1=I2
            I2=I1+I1
          ELSE  
            I2=N+1
          ENDIF
        IF(I2<=N) GO TO 1
        IX(I1)=ITEMP ! FINAL POSITION FOR ITEMP=IX(M).
      END DO
C
C SORTING THE HEAP:
C =================
      DO I=N,3,-1 ! I IS THE NUMBER OF STILL COMPETING ELEMENTS.
        ITEMP=IX(I)
        IX(I)=IX(1) ! STORE TOP OF THE HEAP.
        I1=1
        I2=2
2       CONTINUE
C         INDEX THE LARGER OF IX(I2) AND IX(I2+1) AS I2:
          IF((I2+1)<I .AND. IX(I2+1)>IX(I2)) I2=I2+1
          IF(IX(I2)>ITEMP) THEN
            IX(I1)=IX(I2)
            I1=I2
            I2=I1+I1
          ELSE 
            I2=I
          ENDIF 
        IF(I2<I) GO TO 2
        IX(I1)=ITEMP ! FINAL POSITION FOR ITEMP=IX(I).
      END DO 
      ITEMP=IX(2)
      IX(2)=IX(1)
      IX(1)=ITEMP
      RETURN
      END
