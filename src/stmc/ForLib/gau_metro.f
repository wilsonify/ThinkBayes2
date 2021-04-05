      SUBROUTINE GAU_METRO(A,NDAT,DATA,ACPT)
C
C Copyright, Bernd Berg, Nov 16 2000.
C METROPOLIS GENERATION OF GAUSSIAN RANDOM NUMBERS.
C    
C    INPUT:  (UNCHANGED ON EXIT)
C    ======
C    A:            STEPSIZE PARAMETER; DEL \IN [-A,+A].
C    NDAT:         ARRAY DIMENSION (NUMBER OF DATA).
C    RANMAR has to be initialized by RMASET on entry.
C                                                                     
C    OUTPUT:
C    ======= 
C    DATA(NDAT):   MARKOV SEQUENCE OF GAUSSIAN RANDOM NUMBERS.
C    ACPT:         ACCEPTANCE RATE.
C
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      DIMENSION DATA(NDAT)
      DATA ICALL/0/
      SAVE X,ICALL
      IF(ICALL==0) X=ZERO
      ICALL=1
C
      TWOA=TWO*A
      ACPT=ZERO
      DO I=1,NDAT
        X2=X**2
        CALL RANMAR(XR)
        XP=X+TWOA*(XR-HALF)
        XP2=XP**2
        IF(XP2<=X2) THEN
          X=XP
          ACPT=ACPT+ONE
        ELSE
          CALL RANMAR(XR)
          IF(EXP(-HALF*(XP2-X2))>=XR) THEN
            X=XP
            ACPT=ACPT+ONE
          END IF
        END IF
        DATA(I)=X
      END DO
      ACPT=ACPT/NDAT
C
      RETURN
      END
