      PROGRAM VAR_E
C
C Copyright Bernd Berg, Oct 30, 2000.
C ERROR BAR OF THE VARIANCE: ASYMPTOTIC LARGE N BEHAVIOUR.
C
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER(IUO=6,KMAX=14)
      COMMON /CHI2PAR/ NF
C
      WRITE(IUO,*) '  '
      WRITE(IUO,*) '  CONFIDENCE INTERVALS FOR VARIANCE ESTIMATE ',
     &                                            'FROM NDAT DATA'
      WRITE(IUO,*) '  '
      WRITE(IUO,*) '  NDAT=2**K'
      WRITE(IUO,99)
C 99    FORMAT(2X,' NDAT  K  \ q    .025       .150       .500', ! DOS
99    FORMAT(2X,' NDAT  K  \\ q    .025       .150       .500', ! UNIX
     &                      '       .850       .975',/)
      DO K=1,KMAX
        NDAT=2**K
        NF=NDAT-1
        s2_025=ONE/chi2pdf_xq(P975)
        s2_150=ONE/chi2pdf_xq(P85)
        s2_med=ONE/chi2pdf_xq(HALF)
        s2_850=ONE/chi2pdf_xq(P15)
        s2_975=ONE/chi2pdf_xq(P025)
        WRITE(IUO,100) NDAT,K,s2_025,s2_150,s2_med,s2_850,s2_975
      END DO
100   FORMAT(2X,1I5,1I3,2X,5F11.3)
C
      STOP
      END

      include '../../ForLib/chi2pdf_xq.f'
      include '../../ForLib/chi2_xq.f'
      include '../../ForLib/chi2_df.f'
      include '../../ForLib/fi1.f'
      include '../../ForLib/gamma_ln.f'
      include '../../ForLib/gamma_p.f'
