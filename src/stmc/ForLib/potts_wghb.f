        SUBROUTINE POTTS_WGHB(WHB_TAB,BETA,H0,ND,NQM1)
C Copyright, Bernd Berg, Nov 22 2000.
C Table for the Potts model (with magnetic field) heat bath update.
        include '../../ForLib/implicit.sta'
        include '../../ForLib/constants.par'
        DIMENSION WHB_TAB(0:2*ND,0:NQM1)
        DO IQ=0,NQM1
          H=ZERO
          IF(IQ.EQ.0) H=H0
          DO IACT=0,2*ND
            WHB_TAB(IACT,IQ)=EXP(TWO*(BETA*IACT+H))
          END DO
        END DO
        RETURN
        END
