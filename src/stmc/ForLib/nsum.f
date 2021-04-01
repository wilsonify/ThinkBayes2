      INTEGER FUNCTION NSUM(N1,N2,NA)
C SUM of an integer array. Copyright Bernd Berg, Dec 15, 2001.  
      DIMENSION NA(N1:N2) 
      NSUM=0
      DO I=N1,N2
        NSUM=NSUM+NA(I)
      END DO
      RETURN
      END 
