      SUBROUTINE HIST_GNU(IUG0,NHIST,NDAT,HIST,DATA,XMIN,XMAX)
C
C Copyright, Bernd Berg, Sep 21, 2000.
C GNUPLOT FOR HISTOGRAM. FOR IUG0<0 the HISTOGRAM will
C                        normalized to   SUM H(I)*DEL = 1.
C
C  IUG=IABS(IUG0) WRITE UNIT; FOR IUG=8 only: Distinct output format 
C                            suitable for a single histogram.                    
C  HIST:       HISTOGRAM ARRAY OF DIMENSION NHIST.
C  DATA:       DATA ARRAY OF DIMENSION NDAT.
C  XMIN,XMAX:  HISTOGRAMING RANGE. IF XMIN.GE.XMAX: MINIMUM AND
C              MAXIMUM OF THE DATA ARE CHOSEN.
C  CHIST:      NAME OF GNUPLOT FILE. E.G. hist1, hist2, ...
C              MUST BE CHOSEN TO MATCH CHARACTER*5!
C
      include 'implicit.sta'
      include 'constants.par'
      CHARACTER CI*2
      DIMENSION HIST(NHIST),DATA(NDAT)
      DATA ICNT /0/
      SAVE ICNT
      N_OMIT=0
C
      ICNT=ICNT+1
      IF(ICNT.GT.99) STOP 'HIST_GNU: INCT=99 Exhausted!'
      WRITE(CI,'(I2.2)') ICNT
C
C GNUPLOT SCRIPT:
      IUG=IABS(IUG0)
      WRITE(6,*) "         "
      WRITE(6,'(" HIST_GNU: Open files ",A7," and "A5,".")') 
     & 'h'//CI//'.plt','h'//CI//'.d'
      OPEN(IUG,FILE='h'//CI//'.plt',STATUS='UNKNOWN',FORM='FORMATTED')
      WRITE(IUG,*) 'plot "h'//CI//'.d" using 1:2 with line 1'
      WRITE(IUG,*) 'pause -1'
      CLOSE(IUG)
C
      IF(ICNT.GT.1) THEN ! Create his.plt to plot all histograms
        WRITE(6,*) "HIST_GNU: Open file his.plt to plot all histograms."
        OPEN(IUG,FILE='his.plt',STATUS='UNKNOWN',FORM='FORMATTED')
C       WRITE(IUG,*) 'plot "h01.d" using 1:2 with line 1,\'  ! DOS
        WRITE(IUG,*) 'plot "h01.d" using 1:2 with line 1,\\' ! UNIX
        DO I=2,(ICNT-1)
          WRITE(IUG,100) I,I
C         DOS:
C100      FORMAT('      "h',I2.2,'.d" using 1:2 with line ',I2,',\')
C         UNIX:
100       FORMAT('      "h',I2.2,'.d" using 1:2 with line ',I2,',\\')
        END DO
        WRITE(IUG,101) ICNT,ICNT
101     FORMAT('      "h',I2.2,'.d" using 1:2 with line ',I2)
        WRITE(IUG,*) 'pause -1'
        CLOSE(IUG)
      ENDIF
C
      DO I=1,NHIST
        HIST(I)=ZERO
      END DO
C
      IF(XMAX.LE.XMIN) THEN
        XMIN=DATA(1)
        XMAX=DATA(1)
        DO IDAT=1,NDAT
          XMIN=MIN(XMIN,DATA(IDAT))
          XMAX=MAX(XMAX,DATA(IDAT))
        END DO
        XMIN=XMIN-((XMAX-XMIN)/10000)/NHIST
        XMAX=XMAX+((XMAX-XMIN)/10000)/NHIST
      ENDIF
      WRITE(6,'(" HIST_GNU: XMIN,XMAX =",2G14.6)') XMIN,XMAX
C
C Map DATA linearly such that XMIN -> 1 and XMAX -> NHIST+1.
      FACTOR=(NHIST*ONE)/(XMAX-XMIN)
      XDAT=ZERO
      DO IDAT=1,NDAT
        X=HALF+FACTOR*(DATA(IDAT)-XMIN)
        I=NINT(X)
        IF(I.LE.0. OR. I.GT.NHIST) THEN 
          N_OMIT=N_OMIT+1
        ELSE
          HIST(I)=HIST(I)+ONE
          XDAT=XDAT+ONE
        ENDIF
      END DO
C      
      DEL=(XMAX-XMIN)/(NHIST*ONE)
      OPEN(IUG,FILE='h'//CI//'.d',STATUS='UNKNOWN',FORM='FORMATTED')
      X=XMIN
      IF(IUG0.LT.0) FACTOR=ONE/XDAT/DEL
      IF(IUG0.LT.0) HIST(1)=FACTOR*HIST(1)
      WRITE(IUG,'(2G16.7)') X,ZERO
      WRITE(IUG,'(2G16.7)') X,HIST(1)
      DO I=1,(NHIST-1)
        IF(IUG0.LT.0) HIST(I+1)=FACTOR*HIST(I+1)
        X=X+DEL
        WRITE(IUG,'(2G16.7)') X,HIST(I)
        IF(IUG.EQ.8) WRITE(IUG,'(2G16.7)') X,ZERO
        WRITE(IUG,'(2G16.7)') X,HIST(I+1)
      END DO
      X=X+DEL
      WRITE(IUG,'(2G16.7)') X,HIST(NHIST)
      WRITE(IUG,'(2G16.7)') X,ZERO
      CLOSE(IUG)
C
      IF(N_OMIT.GT.0) 
     & WRITE(6,'(" HIST_GNU Warning:",I10," Data omitted")') N_OMIT
      WRITE(6,*) "         "
      RETURN
      END
