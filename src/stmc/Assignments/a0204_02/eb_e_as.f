      PROGRAM EB_E_AS
C
C Copyright, Bernd Berg, Oct 30, 2000.
C ERROR BAR OF THE VARIANCE: ASYMPTOTIC LARGE N BEHAVIOUR.
C
      include '../../ForLib/implicit.sta'
      PARAMETER (NMAX=14,PC=0.95D00,IUO=6)
C
      NDAT=1
      WRITE(IUO,99)
99    FORMAT(/,1X,'    NDAT    LOWER VARIANCE    ASYMPTOTIC ',
     &'  UPPER VARIANCE   ASYMPTOTIC',/)
      DO N=1,NMAX
        NDAT=2*NDAT
        CALL SEBAR_E(NDAT,PC,EBUP1,EBDO1)
        CALL SEBAR_E_AS(NDAT,PC,EBUP2,EBDO2)
        WRITE(IUO,100) NDAT,EBDO1**2,EBDO2**2,EBUP1**2,EBUP2**2
      END DO
100   FORMAT(1X,1I8,4F15.4)
C
      STOP
      END

      include '../../ForLib/chi2pdf_xq.f'
      include '../../ForLib/chi2_xq.f'
      include '../../ForLib/chi2_df.f'
      include '../../ForLib/error_f.f'
      include '../../ForLib/fi1.f'
      include '../../ForLib/gamma_ln.f'
      include '../../ForLib/gamma_p.f'
      include '../../ForLib/gau_df.f'
      include '../../ForLib/gau_xq.f'
      include '../../ForLib/sebar_e.f'
      include '../../ForLib/sebar_e_as.f'
