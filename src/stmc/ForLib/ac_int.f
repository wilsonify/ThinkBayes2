      SUBROUTINE AC_INT(NT,ACOR,ACINT)
C Copyright, Bernd Berg, Feb 11 2001.
C Input:  Array of autocorrelations ACOR.
C Output: Array of integrated autocorrelation times ACINT.
      include 'implicit.sta'
      include 'constants.par'
      DIMENSION ACOR(0:NT),ACINT(0:NT)
      ACINT(0)=ONE
      DO IT=1,NT
        ACINT(IT)=ACINT(IT-1)+TWO*ACOR(IT)/ACOR(0)
      END DO
      RETURN
      END
