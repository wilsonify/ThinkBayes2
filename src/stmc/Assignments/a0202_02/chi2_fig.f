      PROGRAM CHI2_FIG
C Copyright, Bernd Berg, October 25, 2000.
C PROBABILITY DENSITIES AND Q-DISTRIBUTION FUNCTION
c FOR CHI2 PER DEGREE OF FREEDOM (PDF) DISTRIBUTION.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      CHARACTER CF*1
      PARAMETER (IUG=8,NGNU=60,NFMAX=20,NFILES=5)
      COMMON /CHI2PAR/ NF
      DIMENSION F(NGNU,NFMAX)
C
C PROBABILITY DENSITIES:
C
      MF=NFMAX/NFILES
      DO I=1,NFILES
        WRITE(CF,'(I1)') I
        DO IF=1,MF
          NF=IF+(I-1)*MF
          DO IGNU=1,NGNU
            CHI2=(TWO*IGNU)/(NGNU*ONE)
            F(IGNU,NF)=CHI2PDF_PD(CHI2)
          END DO  
        END DO  
C
        OPEN(UNIT=IUG,STATUS='UNKNOWN',
     &  FILE='fpd'//CF//'.d',FORM='FORMATTED')
        DO IGNU=1,NGNU
          CHI2=(TWO*IGNU)/NGNU
          IF1=1+(I-1)*MF
          IF2=IF1-1+MF
          WRITE(IUG,'(5G12.4)') CHI2,(F(IGNU,IF),IF=IF1,IF2)
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
          DO IGNU=1,NGNU
            CHI2=(TWO*IGNU)/(NGNU*ONE)
            F(IGNU,NF)=CHI2PDF_QDF(CHI2)
          END DO  
        END DO  
C
        OPEN(UNIT=IUG,STATUS='UNKNOWN',
     &  FILE='fqdf'//CF//'.d',FORM='FORMATTED')
        DO IGNU=1,NGNU
          CHI2=(TWO*IGNU)/(NGNU*ONE)
          IF1=1+(I-1)*MF
          IF2=IF1-1+MF
          WRITE(IUG,'(5G12.4)') CHI2,(F(IGNU,IF),IF=IF1,IF2)
        END DO  
      CLOSE (IUG)
      END DO  
C
      STOP
      END

      include '../../ForLib/gamma_ln.f'
      include '../../ForLib/gamma_p.f'
      include '../../ForLib/chi2pdf_qdf.f'
      include '../../ForLib/chi2pdf_pd.f'
