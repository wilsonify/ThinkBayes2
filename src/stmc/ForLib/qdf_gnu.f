      SUBROUTINE QDF_GNU(IUG,N,X)
C Copyright, Bernd Berg, June 15, 2002.
C GNUPLOT FOR THE PEAKED DISTRIBUTION FUNCTION.
C THE ARRAY X HAS TO BE SORTED ON INPUT. 
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      CHARACTER CI*2
      DIMENSION X(N)
      DATA ICNT /0/
      SAVE ICNT,YKEY
C
      ICNT=ICNT+1
      IF(ICNT>99) STOP 'DF_GNU: INCT=99 Exhaused!'
      WRITE(CI,'(I2.2)') ICNT
C
C GNUPLOT SCRIPT:
      XKEY=X(1)+(X(N)-X(1))/EIGHT
      OPEN(IUG,FILE='qdf'//CI//'.plt',STATUS='UNKNOWN',FORM='FORMATTED')
      WRITE(IUG,*) 'set noyzeroaxis'
      WRITE(IUG,*) 'set yrange [0:0.525]'
C     WRITE(IUG,'(" set key",1G16.6,",0.50")') XKEY
      WRITE(IUG,*) 'plot "qdf'//CI//'.d" using 1:3 with line 1'
      WRITE(IUG,*) 'pause -1'
      CLOSE(IUG)
C
      IF(ICNT==1) YKEY=XKEY
      YKEY=MIN(XKEY,YKEY)
      IF(ICNT>1) THEN ! Create qdf.plt to plot all histograms
      OPEN(IUG,FILE='qdf.plt',STATUS='UNKNOWN',FORM='FORMATTED')
      WRITE(IUG,*) 'set noyzeroaxis'
      WRITE(IUG,*) 'set yrange [0:0.525]'
c     WRITE(IUG,'(" set key",1G16.6,",0.50")') YKEY
      WRITE(IUG,*) 'plot "qdf01.d" using 1:3 with line 1,\\'
      DO I=2,(ICNT-1)
      WRITE(IUG,101) I,I
101   FORMAT('      "qdf',I2.2,'.d" using 1:3 with line ',I2,',\\')
      END DO
      WRITE(IUG,103) ICNT,ICNT
103   FORMAT('      "qdf',I2.2,'.d" using 1:3 with line ',I2)
      WRITE(IUG,*) 'pause -1'
      CLOSE(IUG)
      ENDIF
C
      OPEN(IUG,FILE='qdf'//CI//'.d',STATUS='UNKNOWN',FORM='FORMATTED')
      WRITE(IUG,'(3G16.7)') X(1),ZERO,ZERO
      DO I=1,(N-1)
      F=(I*ONE)/(N*ONE)
      Fq=MIN(F,ONE-F)
      WRITE(IUG,'(3G16.7)') X(I),  F,Fq
      WRITE(IUG,'(3G16.7)') X(I+1),F,Fq
      END DO
      WRITE(IUG,'(3G16.7)') X(N),ONE,ZERO
      CLOSE(IUG)
C
      RETURN
      END
