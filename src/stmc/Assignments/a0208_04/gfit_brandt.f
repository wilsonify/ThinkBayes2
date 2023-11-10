      PROGRAM GFIT
C
C Copyright, Bernd Berg, September 27, 2002.
C LEVENBERG-MARQUARDT FIT PROGRAM.
C
C  NFIT:    NUMBER OF FIT PARAMETERS.
c  MDAT:    MAXIMUM NUMBER OF DATA.
c  NDAT:    ACTUAL NUMBER OF DATA (READ FROM DATA FILE).
C  A:       ARRAY OF FIT PARAMETERS (INITIAL GUESS READ FROM DATA FILE).
C  X,Y,YE:  DATA WITH THEIR STATISTICAL ERRORS.
C  LPRI:    PRINT CONTROL PARAMETER.
C
      include '../../ForLib/implicit.sta'
      PARAMETER(NFIT=2,LPRI=.TRUE., IUO=6,IUD=10,MDAT=1000)
      DIMENSION X(MDAT),Y(MDAT),YE(MDAT),A(NFIT),AE(NFIT)
      DIMENSION COVA(NFIT,NFIT),AMAT(NFIT,NFIT)
      EXTERNAL SUBG
C
C READ DATA:
C ==========
      READ(IUD,*) NDAT,(A(I),I=1,NFIT)
      WRITE(IUO,'(/," NUMBER OF DATA:",I12)') NDAT
      IF(NDAT>MDAT) STOP "GFIT: MDAT too small."
      WRITE(IUO,'(/," INITIAL GUESS FOR THE FIT PARAMETERS:")')
      DO I=1,NFIT
        WRITE(IUO,'(" I,A(I):",I6,G18.7)') I,A(I)
      END DO
      DO I=1,NDAT
        READ(IUD,*)           X(I),Y(I),YE(I)
C       WRITE(IUO,'(3G16.7)') X(I),Y(I),YE(I)
      END DO
      CLOSE(IUD)
C
C PERFORM FIT:
C ============
      CALL FIT_G(NDAT,X,Y,YE,NFIT,A,COVA,AMAT,CHI2,Q,SUBG,LPRI)
C
      WRITE(IUO,'(/," RESULTS FOR THE FIT PARAMETERS:")')
      DO I=1,NFIT
        AE(I)=SQRT(COVA(I,I))
        WRITE(IUO,'(I6,".  A(I) =",G16.7,"  +/-",1G16.7)') I,A(I),AE(I)
      END DO
      WRITE(IUO,'("  CHI2    OF THE FIT:",G16.7)') CHI2
      WRITE(IUO,'("  PROB. Q OF THE FIT:",G16.7)') Q
      IF(NDAT>NFIT) WRITE(IUO,'("  CHI2 p.d.f.:",7X,G16.7)')
     &                                CHI2/(NDAT-NFIT)
      WRITE(IUO,*) '     '
C
C PREPARE GNUPLOT PLOT:
C =====================
      CALL FIT_GGNU(IUO,IUD,NDAT,NFIT,X,Y,YE,A)
C
      STOP
      END

      include '../../ForLib/fit_g.f'
      include '../../ForLib/error_f.f'
      include '../../ForLib/gamma_ln.f' 
      include '../../ForLib/gamma_p.f' 
      include '../../ForLib/gaudif.f' 
      include '../../ForLib/mat_gau.f'
      include '../../ForLib/fit_ggnu.f'
      include '../../ForProg/fit_g/subg_linear.f'
c     include '../../ForProg/fit_g/subg_1ox.f'
c     include '../../ForProg/fit_g/subg_power.f'
c     include '../../ForProg/fit_g/subg_power2f.f'
