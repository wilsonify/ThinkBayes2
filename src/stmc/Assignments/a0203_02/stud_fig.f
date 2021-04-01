      PROGRAM STUD_FIG
C
C PROBABILITY DENSITIES AND Q-DISTRIBUTION FUNCTION
c FOR CHI2 PER DEGREE OF FREEDOM (PDF) DISTRIBUTION.
C  
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      CHARACTER CF*1
      PARAMETER (IUG=8,NGNU=60,NFMAX=08,NFILES=2)
      COMMON /PARSTUD/ NF
      DIMENSION F(-NGNU:NGNU,NFMAX)
C
C PROBABILITY DENSITIES:
C
      MF=NFMAX/NFILES
      DO I=1,NFILES
      WRITE(CF,'(I1)') I
      DO IF=1,MF
      NF=IF+(I-1)*MF
      DO IGNU=-NGNU,NGNU
      T=THREE*(IGNU*ONE)/(NGNU*ONE)
      F(IGNU,NF)=STUD_PD(T)
      IF(I.EQ.NFILES.AND.IF.EQ.MF) F(IGNU,NF)=GAU_PD(T)
      END DO  
      END DO  
C
      OPEN(UNIT=IUG,STATUS='UNKNOWN',
     &     FILE='fpd'//CF//'.d',FORM='FORMATTED')
      IF1=1+(I-1)*MF
      IF2=IF1-1+MF
      DO IGNU=-NGNU,NGNU
      T=THREE*(IGNU*ONE)/(NGNU*ONE)
      WRITE(IUG,'(5G12.4)') T,(F(IGNU,IF),IF=IF1,IF2)
      END DO  
      CLOSE (IUG)
      END DO  
C
C Q DISTRIBUTION:
C
      MF=NFMAX/NFILES
      DO I=1,NFILES
      WRITE(CF,'(I1)') I
      DO IF=1,MF
      NF=IF+(I-1)*MF
      DO IGNU=-NGNU,NGNU
      T=THREE*(IGNU*ONE)/(NGNU*ONE)
      F(IGNU,NF)=STUD_QDF(T)
      IF(I.EQ.NFILES.AND.IF.EQ.MF) F(IGNU,NF)=GAU_QDF(T)
      END DO  
      END DO  
C
      OPEN(UNIT=IUG,STATUS='UNKNOWN',
     &     FILE='fqdf'//CF//'.d',FORM='FORMATTED')
      DO IGNU=-NGNU,NGNU
      T=THREE*(IGNU*ONE)/(NGNU*ONE)
      IF1=1+(I-1)*MF
      IF2=IF1-1+MF
      WRITE(IUG,'(5G12.4)') T,(F(IGNU,IF),IF=IF1,IF2)
      END DO  
      CLOSE (IUG)
      END DO  
C
      STOP
      END

      include '../../ForLib/beta.f'
      include '../../ForLib/beta_i.f'
      include '../../ForLib/error_f.f'
      include '../../ForLib/gau_pd.f'
      include '../../ForLib/gau_qdf.f'
      include '../../ForLib/gamma_ln.f'
      include '../../ForLib/gamma_p.f'
      include '../../ForLib/stud_qdf.f'
      include '../../ForLib/stud_pd.f'
