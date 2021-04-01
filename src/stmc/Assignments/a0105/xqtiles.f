      PROGRAM XQTILES
C
C EXACT Q-TILES FOR GAUSSIAN AND CAUCHY.
C  
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER (IUO=8)
C
      WRITE(IUO,*) '        '
      WRITE(IUO,*) 'CAUCHY: '
C
      XQ1=CAU_XQ(P025)
      XQ2=CAU_XQ(P975)
      DX2=XQ2-XQ1
      WRITE(IUO,*) 'P = 0.95:'
      WRITE(IUO,100) XQ1,XQ2,DX2
C
      XQ1=CAU_XQ(P15)
      XQ2=CAU_XQ(P85)
      DX1=XQ2-XQ1
      WRITE(IUO,*) 'P = 0.70:'
      WRITE(IUO,100) XQ1,XQ2,DX1
      RATIO=DX2/DX1
      WRITE(IUO,*) 'RATIO =',RATIO
C
      WRITE(IUO,*) '       '
      WRITE(IUO,*) 'GAUSS: '
C
      XQ1=GAU_XQ(P025)
      XQ2=GAU_XQ(P975)
      DX2=XQ2-XQ1
      WRITE(IUO,*) 'P = 0.95:'
      WRITE(IUO,100) XQ1,XQ2,DX2
C
      XQ1=GAU_XQ(P15)
      XQ2=GAU_XQ(P85)
      DX1=XQ2-XQ1
      WRITE(IUO,*) 'P = 0.70:'
      WRITE(IUO,100) XQ1,XQ2,DX1
      RATIO=DX2/DX1
      WRITE(IUO,*) 'RATIO =',RATIO
C
100   FORMAT(1X,'XQ1,XQ2,(XQ2-XQ1):',3G14.5)
      STOP
      END

      INCLUDE '../../ForLib/cau_xq.f'
      INCLUDE '../../ForLib/gau_xq.f'
      INCLUDE '../../ForLib/fi1.f'
      INCLUDE '../../ForLib/gau_df.f'
      INCLUDE '../../ForLib/error_f.f'
      INCLUDE '../../ForLib/gamma_ln.f'
      INCLUDE '../../ForLib/gamma_p.f'
