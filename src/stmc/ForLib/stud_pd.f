      FUNCTION STUD_PD(T)  
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      COMMON /PARSTUD/ NF
C
      F=NF*ONE
      FHALF=HALF*F
      SF=SQRT(F)
      STUD_PD=(ONE+(T/SF)**2)**(-(F+ONE)/TWO)/SF/BETA(HALF,FHALF)
C
      RETURN
      END   
