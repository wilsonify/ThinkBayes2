      PROGRAM LRAN ! BB June 27 2003.
C
C TEST of MAT_GAU with a random matrix.
C
C  NFIT:    NUMBER OF FIT PARAMETERS.
C  A:       ARRAY OF FIT PARAMETERS (INITIAL GUESS READ FROM DATA FILE).
C  X,Y,EY:  DATA WITH THEIR STATISTICAL ERRORS.
C
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER(NFIT=4,IUO=6,IUD=10,ISD1=1,ISD2=0)
      DIMENSION AIN(NFIT,NFIT),BIN(NFIT), AMAT(NFIT,NFIT),BMAT(NFIT)
C
      CALL RMASET(IUO,IUD,ISD1,ISD2,'no_file')
C
      WRITE(IUO,'(/," BMAT IN:")')
      DO J=1,NFIT
        BMAT(J)=RMAFUN()
        BIN(J)=BMAT(J)
        WRITE(IUO,'(" J,BMAT(J):",I4,G17.6)') J,BMAT(J)
        AMAT(J,J)=AMAT(J,J)*(ONE+XLA) ! Marquard's definition.
      END DO
C
      WRITE(IUO,'(/," AMAT IN:")')
      DO J=1,NFIT
        DO K=1,NFIT
          AMAT(J,K)=RMAFUN()
          AIN(J,K)=AMAT(J,K)
          WRITE(IUO,'(" J,K,AMAT(J,K):",2I4,G17.6)') J,K,AMAT(J,K)
	END DO   
      END DO
C
      CALL MAT_GAU(AMAT,NFIT,BMAT,1)
C
      WRITE(IUO,'(/," BMAT OUT:")')
      DO J=1,NFIT
        WRITE(IUO,'(" J,BMAT(J):",I4,G17.6)') J,BMAT(J)
      END DO
C
      WRITE(IUO,'(/," AMAT OUT:")')
      DO J=1,NFIT
        DO K=1,NFIT
          WRITE(IUO,'(" J,K,AMAT(J,K):",2I4,G17.6)') J,K,AMAT(J,K)
	END DO   
      END DO
C
      WRITE(IUO,'(/," Test of the solution vector:")')
      DO J=1,NFIT
        BB=ZERO
        DO K=1,NFIT
          BB=BB+AIN(J,K)*BMAT(K)
	END DO   
        WRITE(IUO,'(" J,BB,BIN(J):",I4,2G17.6)') J,BB,BIN(J)
      END DO
C
      WRITE(IUO,'(/," Test of inverse matrix:")')
      DO J=1,NFIT
      DO K=1,NFIT
        CC=ZERO
        DO I=1,NFIT
          CC=CC+AIN(J,I)*AMAT(I,K)
	END DO   
        WRITE(IUO,'(" J,K,CC:",2I4,G17.6)') J,K,CC
      END DO
      END DO
C
      STOP "MAT_GAU called."
      END
C

      include '../../ForLib/mat_gau.f'
      include '../../ForLib/rmafun.f'
      include '../../ForLib/rmaset.f'
      

