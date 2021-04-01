      PROGRAM STUD_TABS
C
C  Copyright, Bernd Berg, Feb 22 1992.
c  STUDENT DISTRIBUTION: CONFIDENCE LEVELS FOR SMALL GAUSSIAN SAMPLES.
C
C  NSI:   CONFIDENCE LEVEL INVESTIGATED UP TO NSI*E-BAR.
C  MDAT:  MAXIMUM NUMBER OF (GAUSSIAN) INPUT DATA.
C
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      PARAMETER(NSI=5,MDAT=64)
      DIMENSION P(NSI),S(NSI)
      COMMON /PARSTUD/ NF
C
C  CONFIDENCE LEVEL:
C
      DO I=1,NSI
        S(I)=I*ONE
      END DO
      WRITE(10,*) ' '
      WRITE(10,99)
99    FORMAT(3X,'Table 1)')
      WRITE(10,*) ' '
      WRITE(10,100) S
100   FORMAT(3X,'N  \\ S ',5G11.5) 
      WRITE(10,*) ' '
C
      DO I=2,MDAT
        NF=I-1
        DO J=1,NSI
          P(J)=1.D00-2.D00*STUD_DF(-S(J))
        END DO
        WRITE(10,101) I,P
      END DO
101   FORMAT(1X,1I5,5X,5G11.5)
      WRITE(10,*) ' '
C
      DO J=1,NSI
        P(J)=1.D00-2.D00*GAU_DF(-S(J))
      END DO
      WRITE(10,102) P
102   FORMAT(1X,'INFINITY: ',5G11.5)
      WRITE(10,*) ' '
C
C ----------------------------------------------------------
C
      WRITE(11,98)
98    FORMAT(3X,'Table 2)')
C
      WRITE(11,*) ' '
      WRITE(11,103) P
103   FORMAT(3X,'N  \\ P  ',5G11.6) 
      WRITE(11,*) ' '
C
      DO I=2,MDAT
        NF=I-1
        IF(I.EQ.2) NJ=4
        IF(I.GT.2) NJ=NSI
        DO J=1,NJ
	  XJ=J*ONE
          P(J)=1.D00-GAU_DF(-XJ)
          S(J)=STUD_XQ(P(J))
        END DO
        WRITE(11,101) I,(S(J),J=1,NJ)
      END DO
C
      DO J=1,NSI
        S(J)=J*ONE
      END DO
      WRITE(11,*) ' '
      WRITE(11,102) S
      WRITE(11,*) ' '
C
      STOP
      END

      INCLUDE '../../ForLib/beta_i.f'
      INCLUDE '../../ForLib/error_f.f'
      INCLUDE '../../ForLib/fi1.f'
      INCLUDE '../../ForLib/gau_df.f'
      INCLUDE '../../ForLib/gamma_p.f'
      INCLUDE '../../ForLib/gamma_ln.f'
      INCLUDE '../../ForLib/stud_df.f'
      INCLUDE '../../ForLib/stud_xq.f'
