      FUNCTION RMAFUN()          
C Berg, DEC 10, 2001. FUNCTION VERSION OF RANMAR.
C PSEUDO RANDOM NUMBER GENERATOR. PROPOSED BY 
C MARSAGLIA, ZAMAN AND TSANG, Stat. Prob. 8 (1990) 35.
      include 'implicit.sta'
      include 'constants.par'
      COMMON/RASET1/U(97),C,CD,CM,I,J
C
      UNI=U(I)-U(J)
      IF(UNI.LT.ZERO) UNI=UNI+ONE
      U(I)=UNI           
      I=I-1              
      IF(I.EQ.0) I=97    
      J=J-1              
      IF(J.EQ.0) J=97   
      C=C-CD            
      IF(C.LT.ZERO) C=C+CM
      UNI=UNI-C
      IF(UNI.LT.ZERO) UNI=UNI+ONE
      RMAFUN=UNI
C
      RETURN
      END
