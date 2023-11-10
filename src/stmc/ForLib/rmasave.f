      SUBROUTINE  RMASAVE(IUNIT,file)
C Copyright, Bernd Berg, Sep 21, 2000. 
      include '../../ForLib/implicit.sta'
      character*(*) file
      COMMON/RASET1/U(97),C,CD,CM,I,J
      OPEN(UNIT=IUNIT,FILE=file,
     &  STATUS='UNKNOWN',FORM='UNFORMATTED')
      WRITE(IUNIT) U,C,CD,CM,I,J
      CLOSE(IUNIT)
      RETURN
      END
