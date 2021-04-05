      FUNCTION BINO_CLN(N,K)
C Copyright, Bernd Berg, May 13 2002.
C Log of the binomial probability density. Fast calculation see below.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
C For fast calculation activate the next three lines and call the
C subroutine FCT_LN_INIT before running the present routine.
c     PARAMETER(NMAX=500)
c     COMMON /BINO/ FCT_LN(0:NMAX),IBINO
c     IF(IBINO.NE.1) STOP "BINO_COEF: COMMON BINO not initialized."
      IF(N<=0 .OR. K<0 .OR. K>N) THEN
        PRINT*,'BINO_COEF: N or K false. N,K =',N,K
        STOP 'BINO_COEF: N or K false.'
      END IF
      BINO_CLN=FCT_LN(N)-FCT_LN(K)-FCT_LN(N-K)
      RETURN
      END
