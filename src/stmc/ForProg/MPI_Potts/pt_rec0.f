      SUBROUTINE PT_REC0(BA,NACPT_PT,IP_B,IPI_B,MY_ID,NPM1,IUD)
      include 'implicit.sta'
      include 'constants.par'
      DIMENSION BA(0:NPM1),NACPT_PT(0:NPM1),IP_B(0:NPM1),IPI_B(0:NPM1)
      NACPT_MAX=0
      DO I=0,NPM1
        IPI_B(IP_B(I))=I
        NACPT_PT(I)=2*NACPT_PT(I)
        NACPT_MAX=MAX(NACPT_MAX,NACPT_PT(I))
      END DO
      IF(NACPT_MAX.EQ.0) CALL STOP_MPI(IUD,MY_ID,'PT_REC0: NACPT_MAX=0')
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
      RETURN
      END
