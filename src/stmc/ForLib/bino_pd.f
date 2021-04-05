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

      IF(P<ZERO .OR. P>ONE) THEN
        PRINT*,'BINO_PD: P false. P =',P
        STOP 'BINO_PD: P false.'
      END IF

      IF(N<0 .OR. K<0 .OR. K>N) THEN
        PRINT*,'BINO_PD: N or K false. N,K =',N,K
        STOP 'BINO_PD: N or K false.'
      END IF

      IF(N==0) THEN
        BINO_PD=ONE ! For situations with binomial density factor 1.
        RETURN
      END IF

      IF(P==ZERO) THEN
        IF(K>0) BINO_PD=ZERO
        IF(K==0) BINO_PD=ONE
        RETURN
      END IF

      IF(P==ONE) THEN
        IF(K<N) BINO_PD=ZERO
        IF(K==N) BINO_PD=ONE
        RETURN
      END IF

      BINO_PD=EXP( FCT_LN(N)-FCT_LN(K)-FCT_LN(N-K)
     &      +K*LOG(P)+(N-K)*LOG(ONE-P) )

      RETURN
      END
