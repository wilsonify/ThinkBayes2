      SUBROUTINE DF_GNU(IUG,N,X)
C Copyright, Bernd Berg, Nov 7 2000.
C GNUPLOT FOR DISTRIBUTION FUNCTION.
C THE ARRAY X HAS TO BE SORTED ON INPUT (CALL HEASORT). 
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      CHARACTER CI*2
      DIMENSION X(N)
      DATA ICNT /0/
      SAVE ICNT,YKEY
C
      ICNT=ICNT+1
      IF(ICNT.GT.99) STOP 'DF_GNU: INCT=99 Exhausted!'
      WRITE(CI,'(I2.2)') ICNT
C
C GNUPLOT SCRIPT:
      XKEY=X(1)+(X(N)-X(1))/EIGHT
      OPEN(IUG,FILE='df'//CI//'.plt',STATUS='UNKNOWN',FORM='FORMATTED')
      WRITE(IUG,*) 'set noyzeroaxis'
      WRITE(IUG,'(" set key",1G16.6,",0.95")') XKEY
      WRITE(IUG,*) 'plot "df'//CI//'.d" using 1:2 with line 1,\\'
      WRITE(IUG,*) '     "df'//CI//'.d" using 1:3 with line 1'
      WRITE(IUG,*) 'pause -1'
      CLOSE(IUG)
C
      IF(ICNT.EQ.1) YKEY=XKEY
      YKEY=MIN(XKEY,YKEY)
      IF(ICNT.GT.1) THEN ! Create df.plt to plot all histograms
      OPEN(IUG,FILE='df.plt',STATUS='UNKNOWN',FORM='FORMATTED')
      WRITE(IUG,*) 'set noyzeroaxis'
      WRITE(IUG,'(" set key",1G16.6,",0.95")') YKEY
      WRITE(IUG,*) 'plot "df01.d" using 1:2 with line 1,\\'
      WRITE(IUG,*) '     "df01.d" using 1:3 with line 1,\\'
      DO I=2,(ICNT-1)
        WRITE(IUG,100) I,I
        WRITE(IUG,101) I,I
      END DO
100   FORMAT('      "df',I2.2,'.d" using 1:2 with line ',I2,',\\')
101   FORMAT('      "df',I2.2,'.d" using 1:3 with line ',I2,',\\')
      WRITE(IUG,102) ICNT,ICNT
102   FORMAT('      "df',I2.2,'.d" using 1:2 with line ',I2,',\\')
      WRITE(IUG,103) ICNT,ICNT
103   FORMAT('      "df',I2.2,'.d" using 1:3 with line ',I2)
      WRITE(IUG,*) 'pause -1'
      CLOSE(IUG)
      ENDIF
C
      OPEN(IUG,FILE='df'//CI//'.d',STATUS='UNKNOWN',FORM='FORMATTED')
      WRITE(IUG,'(3G16.7)') X(1),ZERO,ZERO
      DO I=1,(N-1)
      F=(ONE*I)/(ONE*N)
      Fq=MIN(F,ONE-F)
      WRITE(IUG,'(3G16.7)') X(I),  F,Fq
      WRITE(IUG,'(3G16.7)') X(I+1),F,Fq
      END DO
      WRITE(IUG,'(3G16.7)') X(N),ONE,ZERO
      CLOSE(IUG)
C
      RETURN
      END
