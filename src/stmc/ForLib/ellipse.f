      SUBROUTINE ELLIPSE(XM,SGXM,SGYM,EIGVS,PROB,IUD)
C PURPOSE: PREPARE CONFIDENCE ELLIPSE FOR GNUPLOT.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER (NGNU=100)
      DIMENSION XM(2),SGXM(2),SGYM(2),EIGVS(2,2)
C
C 1. ELLIPSE:
C PROB<0: STANDARD COVARIANCE ELLIPSE WITH 39% CONFIDENCE.
      OPEN(UNIT=IUD,FILE='ellipse.d1',STATUS='UNKNOWN')
      IF(PROB<ZERO)           THEN
      R=ONE
                                 ELSE
      R=SQRT(-TWO*LOG(ONE-PROB))
                                 END IF
      DELPHY=TPI/NGNU
      DO I=0,NGNU
      PHY=I*DELPHY
      Y1=+R*SGYM(1)*SIN(PHY)
      Y2=+R*SGYM(2)*COS(PHY)
      X1=XM(1)+EIGVS(1,1)*Y1+EIGVS(1,2)*Y2
      X2=XM(2)+EIGVS(2,1)*Y1+EIGVS(2,2)*Y2
      WRITE(IUD,*)  X1,X2
      END DO
      CLOSE(IUD)
C
      OPEN(UNIT=IUD,FILE='ellipse.d2',STATUS='UNKNOWN')
C 2. STANDARD DEVIATIONS IN ORIGNAL DIRECTIONS:
      X1=XM(1)+SGXM(1)
      WRITE(IUD,*)  X1,XM(2)
      X1=XM(1)-SGXM(1)
      WRITE(IUD,*)  X1,XM(2)
      X1=XM(1)
      WRITE(IUD,*)  X1,XM(2)
      X2=XM(2)+SGXM(2)
      WRITE(IUD,*)  XM(1),X2
      X2=XM(2)-SGXM(2)
      WRITE(IUD,*)  XM(1),X2
      WRITE(IUD,*)  XM(1),XM(2)
C
C 3. STANDARD DEVIATIONS IN THE INDEPENDENT DIRECTIONS:
      WRITE(IUD,*) 'SET TEXTURE DASHES'
      DO I=1,2
      X1=XM(1)+SGYM(I)*EIGVS(1,I)
      X2=XM(2)+SGYM(I)*EIGVS(2,I)
      WRITE(IUD,*)  X1,X2
      X1=XM(1)-SGYM(I)*EIGVS(1,I)
      X2=XM(2)-SGYM(I)*EIGVS(2,I)
      WRITE(IUD,*)  X1,X2
      WRITE(IUD,*)  XM(1),XM(2)
      END DO
      CLOSE(IUD)
C
      RETURN
      END
