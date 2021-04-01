      FUNCTION BINO_PD(N,K,P)
C Copyright, Bernd Berg, May 5 2001.
C Binomial probability density. Fast calculation see below.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
C For fast calculation activate the next three lines and call the
C subroutine FCT_LN_INIT before running the present routine.
c     PARAMETER(NMAX=500)
c     COMMON /BINO/ FCT_LN(0:NMAX),IBINO
c     IF(IBINO.NE.1) STOP "BINO_PD: COMMON BINO not initialized."

      IF(P.LT.ZERO .OR. P.GT.ONE) THEN 
        PRINT*,'BINO_PD: P false. P =',P
        STOP 'BINO_PD: P false.'
      END IF

      IF(N.LT.0 .OR. K.LT.0 .OR. K.GT.N) THEN
        PRINT*,'BINO_PD: N or K false. N,K =',N,K
        STOP 'BINO_PD: N or K false.'
      END IF

      IF(N.EQ.0) THEN
        BINO_PD=ONE ! For situations with binomial density factor 1.
        RETURN
      END IF

      IF(P.EQ.ZERO) THEN 
        IF(K.GT.0) BINO_PD=ZERO
        IF(K.EQ.0) BINO_PD=ONE
        RETURN
      END IF

      IF(P.EQ.ONE) THEN 
        IF(K.LT.N) BINO_PD=ZERO
        IF(K.EQ.N) BINO_PD=ONE
        RETURN
      END IF

      BINO_PD=EXP( FCT_LN(N)-FCT_LN(K)-FCT_LN(N-K)
     &      +K*LOG(P)+(N-K)*LOG(ONE-P) )

      RETURN
      END
