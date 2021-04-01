      PROGRAM CHI2_TABS
C
C CHI2 DISTRIBUTION: SOME TABLES.
C Copyright, Bernd Berg, October 25, 2000.
C
      include '../../ForLib/implicit.sta' 
      include '../../ForLib/constants.par' 
      PARAMETER (NMAX=8,NQ=5,IUO=6)
      DIMENSION F(NMAX),Q(NQ),XQ(NQ)
      COMMON /CHI2PAR/ NF
      DATA Q /0.025D00, 0.15D00, 0.50D00, 0.85D00, 0.975D00/
C
      WRITE(IUO,*) ' '
      WRITE(IUO,*) ' CHI2 distribution function'
      WRITE(IUO,*) ' '
      WRITE(IUO,101) (N,N=1,NMAX)
101   FORMAT(1X,'   CHI2 \\ N ',3X,10(1X,I4,2X)) ! UNIX
C 101   FORMAT(1X,'   CHI2 \ N ',3X,10(1X,I4,2X)) ! DOS
      WRITE(IUO,*) ' '
      DO I=1,NMAX
C CHI2:
        CHI2=I*ONE
C LOOP OVER DEGREES OF FREEDOM:
        DO N=1,NMAX
          NF=N
          F(N)=CHI2_DF(CHI2)
        END DO
      WRITE(IUO,100) CHI2,F
      END DO
100   FORMAT(3X,1F5.2,7X,10(2X,F5.3))
      WRITE(IUO,*) ' '
C
      WRITE(IUO,*) ' '
      WRITE(IUO,*)
     & ' Fractiles for the CHI2 distribution'
      WRITE(IUO,*) ' '
      WRITE(IUO,103) (Q(I),I=1,NQ)
103   FORMAT(3X,'  N \\ q',2X,5(2X,F8.3)) ! UNIX
C 103   FORMAT(3X,'  N \ q',2X,5(2X,F8.3)) ! DOS
      WRITE(IUO,*) ' '
C LOOP OVER DEGREES OF FREEDOM:
      DO N=1,NMAX
        NF=N
C LOOP OVER Q-TILES:
        DO I=1,NQ
          XQ(I)=CHI2_XQ(Q(I))
        END DO
      WRITE(IUO,200) N,XQ
      END DO
200   FORMAT(1X,1I5,6X,5(2X,F8.3))
      WRITE(IUO,*) ' '
C
      WRITE(IUO,*) ' '
      WRITE(IUO,*) 
     & ' CHI2 per degree of freedom distribution function '
      WRITE(IUO,*) ' '
      WRITE(IUO,102) (N,N=1,NMAX)
102   FORMAT(1X,'(CHI2/N) \\ N',3X,10(1X,I4,2X)) ! UNIX
C 102   FORMAT(1X,'(CHI2/N) \ N',3X,10(1X,I4,2X)) ! DOS
      WRITE(IUO,*) ' '
      DO I=1,NMAX
C CHI2 PER DEGREE OF FREEDOM:
        X=(I*ONE)/FOUR
C LOOP OVER DEGREES OF FREEDOM:
        DO N=1,NMAX
          NF=N
          F(N)=CHI2PDF_DF(X)
        END DO
        WRITE(IUO,100) X,F
      END DO
      WRITE(IUO,*) ' '
C
      WRITE(IUO,*) ' '
      WRITE(IUO,*) 
     & ' Fractiles for the CHI2 per degree of freedom distribution'
      WRITE(IUO,*) ' '
      WRITE(IUO,103) (Q(I),I=1,NQ)
      WRITE(IUO,*) ' '
C LOOP OVER DEGREES OF FREEDOM:
      DO N=1,NMAX
        NF=N
C LOOP OVER Q-TILES:
        DO I=1,NQ
          XQ(I)=CHI2PDF_XQ(Q(I))
        END DO
        WRITE(IUO,200) N,XQ
      END DO
      WRITE(IUO,*) ' '
C
      STOP
      END

      INCLUDE '../../ForLib/chi2_df.f'
      INCLUDE '../../ForLib/chi2_xq.f'
      INCLUDE '../../ForLib/chi2pdf_df.f'
      INCLUDE '../../ForLib/chi2pdf_xq.f'
      INCLUDE '../../ForLib/fi1.f'
      INCLUDE '../../ForLib/gamma_p.f'
      INCLUDE '../../ForLib/gamma_ln.f'
