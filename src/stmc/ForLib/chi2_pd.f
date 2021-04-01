      FUNCTION CHI2_PD(CHI2)
C Copyright, Bernd Berg, July 1 1999.
C CHI2 probability density.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      COMMON /CHI2PAR/ NF
      XNF=ONE*NF
      A=HALF*XNF
      ALN=LOG(A)
      XCHI2=CHI2/XNF
      CLN=LOG(XCHI2)
      YLN=ALN-a*XCHI2+(A-ONE)*(ALN+CLN)-GAMMA_LN(A)
      CHI2_PD=EXP(YLN)/XNF
      RETURN
      END
