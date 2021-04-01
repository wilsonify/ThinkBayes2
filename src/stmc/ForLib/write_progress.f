      subroutine write_progress(iud,ctxt,irpt,iact,acpt)
C Copyright, Bernd Berg, Dec 13, 2000.
C Write subroutine to monitor progress.
      include '../../ForLib/implicit.sta'
      character*(*) ctxt
      open(iud,file="progress.d",form="formatted",status="unknown")
      rewind iud
      write(iud,'(A24,2I12,F10.3)') ctxt,irpt,iact,acpt
      close(iud)
      return
      end 
