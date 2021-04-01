      PROGRAM PGAU_MET
C Metropolis Generation of Gaussian  Random numbers.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER (IUO=6,IUD=10,NDAT=2000,NGNU=500,ISEED1=1,ISEED2=0)
c     PARAMETER (IUO=6,IUD=10,NDAT=20000,NGNU=500,ISEED1=1,ISEED2=0)
      DIMENSION DATA(NDAT)
      A=THREE
      CALL RMASET(IUO,IUD,ISEED1,ISEED2,"no_file")
C Metropolsi data:
      WRITE(IUO,*) " "
      WRITE(IUO,*) "Metropolis Generation of Gaussian random numbers"
      WRITE(IUO,'(" NDAT,A:              ",I10,1F12.2)') NDAT,A
      CALL GAU_METRO(A,NDAT,DATA,ACPT)
      WRITE(IUO,'(" Acceptance rate found",1F22.6)') ACPT
      CALL HEAPSORT(NDAT,DATA)
      CALL DF_GNU(IUD,NDAT,DATA)
C Exact results:
      OPEN(IUD,FILE="dfexact.d",form="formatted",status="unknown")
      DO IGNU=-NGNU,NGNU
        X=(THREE*IGNU)/(ONE*NGNU)
        F=GAU_DF(X)
        Fq=GAU_QDF(X)
        WRITE(IUD,'(3F18.6)') X,F,Fq
      END DO 
      CLOSE(IUD)
      STOP
      END

      include '../../ForLib/df_gnu.f'
      include '../../ForLib/error_f.f'
      include '../../ForLib/gamma_p.f'
      include '../../ForLib/gamma_ln.f'
      include '../../ForLib/gau_df.f'
      include '../../ForLib/gau_qdf.f'

      include '../../ForLib/gau_metro.f'

      include '../../ForLib/heapsort.f'
      include '../../ForLib/rmaset.f'
      include '../../ForLib/ranmar.f'
