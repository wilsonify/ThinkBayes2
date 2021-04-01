      PROGRAM GFITJ2
C
C Copyright, Bernd Berg, October 4, 2002.
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
      include '../../ForLib/constants.par'
      PARAMETER(NFIT=4,IUO=6,IUD=10,MDAT=100,NBINS=32)
      CHARACTER CBIN*2
      DIMENSION X(MDAT),Y(MDAT),YE(MDAT),AM(NFIT),AE(NFIT)
      DIMENSION AJ(0:NBINS,NFIT),COVA(NFIT,NFIT),AMAT(NFIT,NFIT)
      DIMENSION TAU_INTJ(NBINS)
      EXTERNAL SUBG
      LTEST=.TRUE.
      LTEST=.FALSE.
      IF(LTEST) STOP "GFITJ: LTEST."

      LPRI=.TRUE.
      DO IBIN=0,NBINS
        IF(IBIN.GE.1) LPRI=.FALSE.
        WRITE(CBIN,'(I2.2)') IBIN
C
C READ DATA:
C ==========
        OPEN(IUD,FILE="datj"//CBIN//".d",FORM='FORMATTED',STATUS='OLD')
        IF(IBIN.EQ.0) THEN
          READ(IUD,*) NBINS_IN,NDAT,(AM(I),I=1,NFIT)
          IF(NBINS_IN.NE.NBINS) STOP "GFIT: NBINS_IN.NE.NBINS."
          WRITE(IUO,'(/," NUMBER OF DATA:",I12)') NDAT
          IF(NDAT.GT.MDAT) STOP "GFIT: MDAT too small."
          WRITE(IUO,'(/," INITIAL GUESS FOR THE FIT PARAMETERS:")')
        END IF
        DO I=1,NFIT
          IF(IBIN.EQ.0) WRITE(IUO,'(" I,A(I):",I6,G18.7)') I,AM(I)
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
        NTMAX=100
        TAU_INTJ(IBIN)=F4TAU_INT(AM,NTMAX)
C
c       IF(IBIN.EQ.0)
c    &    WRITE(IUO,'(/," RESULTS FOR THE FIT PARAMETERS:")')
c       WRITE(IUO,'(" IBIN =",I6,":")') IBIN
        DO I=1,NFIT
c         WRITE(IUO,'(I6,".  AM(I) =",G16.7)') I,AM(I)
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

      CALL STEBJ0(NBINS,TAU_INTJ,TAU_INT,TAUV,TAUE)
      WRITE(IUO,'(/," tau_int estimate =",F10.4" +/-",F10.4)') 
     &                TAU_INT,TAUE
C
C PREPARE GNUPLOT PLOT:
C =====================
      CALL FIT_GGNU(IUO,IUD,NDAT,NFIT,X,Y,YE,AM)
C
      STOP
      END


      FUNCTION F4TAU_INT(A,NTMAX)
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      DIMENSION A(4)
      FACT=TWO/(A(1)+A(3))
      F4TAU_INT=ONE
      DO IT=1,NTMAX
        F4TAU_INT=F4TAU_INT+FACT*(A(1)*EXP(A(2)*IT)+A(3)*EXP(A(4)*IT))
      END DO
      RETURN
      END

      include '../../ForLib/error_f.f'
      include '../../ForLib/fit_g.f'
      include '../../ForLib/gamma_p.f' 
      include '../../ForLib/gamma_ln.f' 
      include '../../ForLib/gaudif.f'
      include '../../ForLib/mat_gau.f'
      include '../../ForLib/fit_ggnu.f'
      include '../../ForLib/stebj0.f'
      include '../../ForLib/autoj_tau.f'
      include '../../ForProg/fit_g/subg_exp2.f'
