      SUBROUTINE BINING(NDAT,DATA,NBINS,DATB)
C Copyright, Bernd Berg, Jan 10 1992.
C
C    PURPOSE:                                                         
C    CALCULATION OF  NBINS  BINNED DATA FROM  NDAT   ORIGINAL DATA.
C                                                                     
C    INPUT:  (UNCHANGED ON EXIT)
C                                                                     
C    NDAT:    NUMBER OF ORIGINAL DATA.
C    DATA:    ARRAY VECTOR CONTAINING THE ORIGINAL DATA.
C    NBINS:   NUMBER OF BINS REQUESTED.
C                                                                     
C    OUTPUT:
C     
C    DATB:    ARRAY CONTAINING THE BINNED DATA.
C
      include '../../ForLib/implicit.sta'
      DIMENSION DATA(NDAT),DATB(NBINS)
      DATA IWARN /0/
C
      NBIN=NDAT/NBINS
      NTEST=NBIN*NBINS
      IF(IWARN.EQ.0 .AND. NTEST.NE.NDAT) THEN
      PRINT*,'WARNING FROM SUBROUTINE BINING:'
      PRINT*,'NDAT =      ',NDAT
      PRINT*,'NBIN*NBINS =',NTEST
      PRINT*,'NBIN,NBINS: ',NBIN,NBINS
      IWARN=IWARN+1
      PRINT*,'NO WARNINGS IN FUTURE CALLS (TO CHANGE INSPECT BINING)!'
                                         END IF
C
      DO I=1,NBINS
      DATB(I)=0.0D00
      DO J=1,NBIN
      JJ=J+(I-1)*NBIN
      DATB(I)=DATB(I)+DATA(JJ)
      END DO 
      DATB(I)=DATB(I)/NBIN
      END DO    
C
      RETURN
      END
