      PROGRAM GFITJ
C
C Copyright, Bernd Berg, September 27, 2002.
C Jackknife Version of the LEVENBERG-MARQUARDT FIT PROGRAM.
C
C  NFIT:    NUMBER OF FIT PARAMETERS.
c  MDAT:    MAXIMUM NUMBER OF DATA.
c  NDAT:    ACTUAL NUMBER OF DATA (READ FROM DATA FILE).
C  A:       ARRAY OF FIT PARAMETERS (INITIAL GUESS READ FROM DATA FILE).
C  X,Y,YE:  DATA WITH THEIR STATISTICAL ERRORS.
C  LPRI:    PRINT CONTROL PARAMETER.
C
      include '../../ForLib/implicit.sta'
      PARAMETER(NFIT=2,IUO=6,IUD=10,MDAT=100,NBINS=32)
      CHARACTER CBIN*2
      DIMENSION X(MDAT),Y(MDAT),YE(MDAT),AM(NFIT),AE(NFIT)
      DIMENSION AJ(0:NBINS,NFIT),COVA(NFIT,NFIT),AMAT(NFIT,NFIT)
      EXTERNAL SUBG
      LTEST=.TRUE.
      LTEST=.FALSE.
      IF(LTEST) STOP "GFITJ: LTEST."

      LPRI=.TRUE.
      DO IBIN=0,NBINS
        IF(IBIN>=1) LPRI=.FALSE.
        WRITE(CBIN,'(I2.2)') IBIN
C
C READ DATA:
C ==========
        OPEN(IUD,FILE="datj"//CBIN//".d",FORM='FORMATTED',STATUS='OLD')
        IF(IBIN==0) THEN
          READ(IUD,*) NBINS_IN,NDAT,(AM(I),I=1,NFIT)
          IF(NBINS_IN/=NBINS) STOP "GFIT: NBINS_IN.NE.NBINS."
          WRITE(IUO,'(/," NUMBER OF DATA:",I12)') NDAT
          IF(NDAT>MDAT) STOP "GFIT: MDAT too small."
          WRITE(IUO,'(/," INITIAL GUESS FOR THE FIT PARAMETERS:")')
        END IF
        DO I=1,NFIT
          IF(IBIN==0) WRITE(IUO,'(" I,A(I):",I6,G18.7)') I,AM(I)
        END DO
        DO I=1,NDAT
          READ(IUD,*)           X(I),Y(I),YE(I)
C         WRITE(IUO,'(3G16.7)') X(I),Y(I),YE(I)
        END DO
        CLOSE(IUD)
C
C PERFORM FIT:
C ============
        CALL FIT_G(NDAT,X,Y,YE,NFIT,AM,COVA,AMAT,CHI2,Q,SUBG,LPRI)
C
        IF(IBIN==0)
     &    WRITE(IUO,'(/," RESULTS FOR THE FIT PARAMETERS:")')
        WRITE(IUO,'(" IBIN =",I6,":")') IBIN
        DO I=1,NFIT
          WRITE(IUO,'(I6,".  AM(I) =",G16.7)') I,AM(I)
          AJ(IBIN,I)=AM(I)
        END DO
c       WRITE(IUO,'("  CHI2    OF THE FIT:",G16.7)') CHI2
c       WRITE(IUO,'("  PROB. Q OF THE FIT:",G16.7)') Q
c       IF(NDAT.GT.NFIT) WRITE(IUO,'("  CHI2 p.d.f.:",7X,G16.7,/)') 
c    &                                  CHI2/(NDAT-NFIT)
      END DO
 
      WRITE(IUO,'(/," FIT PARAMETERS WITH JACKKNIFE ERRORS:")')
      DO I=1,NFIT
        CALL STEBJ0(NBINS,AJ(1,I),AM(I),AV,AE(I))
        WRITE(IUO,'(I6,".  AM(I) =",G16.7," +/-",G16.7)') I,AM(I),AE(I)
      END DO
C
C PREPARE GNUPLOT PLOT:
C =====================
      CALL FIT_GGNU(IUO,IUD,NDAT,NFIT,X,Y,YE,AM)
C
      STOP
      END

      include '../../ForLib/error_f.f'
      include '../../ForLib/fit_g.f'
      include '../../ForLib/gamma_p.f' 
      include '../../ForLib/gamma_ln.f' 
      include '../../ForLib/gaudif.f'
      include '../../ForLib/mat_gau.f'
      include '../../ForLib/fit_ggnu.f'
      include '../../ForLib/stebj0.f'
      include 'subg_exp.f'
