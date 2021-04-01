      PROGRAM F_ebar
C
C Copyright, Bernd Berg, Oct 30, 2000.
C Confidence intervals for F ratio estimates
C
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER (KDAT=10,PC1=0.70D00,PC2=0.95D00,IUO=6)
      COMMON /PARFT/ NF1,NF2 
C
      Q1=HALF*(ONE-PC1)
      P1=ONE-Q1
      Q2=HALF*(ONE-PC2)
      P2=ONE-Q2
      NDA1=2**KDAT
      NF1=NDA1-1
C
      WRITE(IUO,*) "   "
      WRITE(IUO,*) "NDA1 = ",NDA1,":"
      WRITE(IUO,*) "   "
      WRITE(IUO,*) "   SOME CONFIDENCE INTERVALS FOR F RATIO ESTIMATES."	
      WRITE(IUO,*) "   "
      WRITE(IUO,*) "                   q         q       1-q       1-q"
      WRITE(IUO,99)
99    FORMAT(/,'       NDA2      .025      .150      .850      .975',/) 
      DO K2=-4,4
        NDA2=2**(KDAT+K2)
        NF2=NDA2-1
        FDO2=ONE/F_XQ(P2)
        FDO1=ONE/F_XQ(P1)
        FUP1=ONE/F_XQ(Q1)
        FUP2=ONE/F_XQ(Q2)
        WRITE(IUO,'(1I11,4F10.3)') NDA2,FDO2,FDO1,FUP1,FUP2
      END DO
C
      STOP
      END

      include '../../ForLib/beta_i.f'
      include '../../ForLib/chi2pdf_xq.f'
      include '../../ForLib/chi2_xq.f'
      include '../../ForLib/chi2_df.f'
      include '../../ForLib/error_f.f'
      include '../../ForLib/fi1.f'
      include '../../ForLib/F_xq.f'
      include '../../ForLib/F_df.f'
      include '../../ForLib/gamma_ln.f'
      include '../../ForLib/gamma_p.f'
      include '../../ForLib/gau_df.f'
      include '../../ForLib/gau_xq.f'
      include '../../ForLib/sebar_e.f'
      include '../../ForLib/sebar_e_as.f'
