      SUBROUTINE HEAP_IPER(N,IX,IPER)
C COPYRIGHT, BERND BERG, FEB 11, 2003.  
C VERSION OF HEAP_PER, WHICH SORTS AN INTEGER ARRAY IX.
      include '../../ForLib/implicit.sta'
      DIMENSION IX(N),IPER(N)
C
      DO I=1,N
        IPER(I)=I ! Initialize IPER.
      END DO
C
C BUILDING THE HEAP:
C ==================
      NHALF=N/2
      DO M=NHALF,1,-1
        ITEMP=IX(M) 
        ITMP=IPER(M)
        I1=M
        I2=M+M
1       CONTINUE
C         INDEX THE LARGER OF IX(I2) AND IX(I2+1) AS I2:
          IF(I2<N .AND. IX(I2+1)>IX(I2)) I2=I2+1
          IF(IX(I2)>ITEMP) THEN
            IX(I1)=IX(I2)
            IPER(I1)=IPER(I2)
            I1=I2
            I2=I1+I1
          ELSE  
            I2=N+1
          ENDIF
        IF(I2<=N) GO TO 1
        IX(I1)=ITEMP ! FINAL POSITION FOR ITEMP=IX(M).
        IPER(I1)=ITMP
      END DO
C
C SORTING THE HEAP:
C =================
      DO I=N,3,-1 ! I IS THE NUMBER OF STILL COMPETING ELEMENTS.
        ITEMP=IX(I)
        ITMP=IPER(I)
        IX(I)=IX(1) ! STORE TOP OF THE HEAP.
        IPER(I)=IPER(1)
        I1=1
        I2=2
2       CONTINUE
C         INDEX THE LARGER OF IX(I2) AND IX(I2+1) AS I2:
          IF((I2+1)<I .AND. IX(I2+1)>IX(I2)) I2=I2+1
          IF(IX(I2)>ITEMP) THEN
            IX(I1)=IX(I2)
            IPER(I1)=IPER(I2)
            I1=I2
            I2=I1+I1
          ELSE 
            I2=I
          ENDIF 
        IF(I2<I) GO TO 2
        IX(I1)=ITEMP ! FINAL POSITION FOR ITEMP=IX(I).
        IPER(I1)=ITMP
      END DO 
      ITEMP=IX(2)
      ITMP=IPER(2)
      IX(2)=IX(1)
      IPER(2)=IPER(1)
      IX(1)=ITEMP
      IPER(1)=ITMP
      RETURN
      END
