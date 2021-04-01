      SUBROUTINE 
     & PT_REC2(BA,BASUM,NACPT_PT,IP_B,IPI_B,MY_ID,NPM1,NCALL,IUD)
C Copyright, Bernd Berg, Jan 10 2002.
      include 'implicit.sta'
      include 'constants.par'
      DIMENSION BA(0:NPM1),BASUM(0:NPM1)
      DIMENSION NACPT_PT(0:NPM1),IP_B(0:NPM1),IPI_B(0:NPM1)
      DATA WEIGHT/ZERO/, ICALL/0/, IADD/0/
      SAVE WEIGHT,ICALL,IADD
      N_PROC=NPM1+1
      IF(ICALL.EQ.0) THEN
        DO I=0,NPM1
          BASUM(I)=ZERO
        END DO
      END IF
      ICALL=ICALL+1
      NACPT_MAX=0
      DO I=0,NPM1
        IPI_B(IP_B(I))=I
        NACPT_PT(I)=2*NACPT_PT(I)
        NACPT_MAX=MAX(NACPT_MAX,NACPT_PT(I))
      END DO
      NACPT_MIN=NACPT_MAX
      DO I=1,NPM1
        NACPT_MIN=MIN(NACPT_MIN,NACPT_PT(IPI_B(I)))
      END DO
      IF(NACPT_MIN.GT.0) THEN
        IADD=IADD+1
        SIG2=ZERO
        DO I=1,NPM1
          SIG2=SIG2+(ONE/NACPT_PT(IPI_B(I)))**2
        END DO
        W=ONE/SQRT(SIG2)
        WEIGHT=WEIGHT+W
        DO I=0,NPM1
          BASUM(I)=BASUM(I)+W*BA(I) 
        END DO
      END IF
      IF(NACPT_MAX.EQ.0) CALL STOP_MPI(IUD,MY_ID,'PT_REC2: NACPT_MAX=0')
      DO I=0,NPM1
        IF(NACPT_PT(I).EQ.0) NACPT_PT(I)=1
      END DO
      XLA_D=ZERO
      DO I=1,NPM1
        XLA_D=XLA_D+NACPT_PT(IPI_B(I))*(BA(I)-BA(I-1))
      END DO
      XLA=(BA(NPM1)-BA(0) )/XLA_D
      BIM1_NEW=BA(0)
      DO I=1,NPM1
        BI_NEW=BIM1_NEW+(XLA*NACPT_PT(IPI_B(I)))*(BA(I)-BA(I-1))
        BA(I-1)=BIM1_NEW
        BIM1_NEW=BI_NEW
      END DO
      IF(IADD.EQ.0) CALL STOP_MPI(IUD,MY_ID,'PT_REC2: IADD=0.')
      IF(ICALL.EQ.NCALL) THEN
        DO I=0,NPM1
          BA(I)=BASUM(I)/WEIGHT
        END DO
      END IF
      RETURN
      END
