      SUBROUTINE STOP_MPI(IUD,MY_ID,ctest)
      character*(*) ctest
      character cmy*4
      write(cmy,'(i4.4)') MY_ID
      open(iud,file="error_stop"//cmy//".d",form='formatted',
     &         status='unknown')
      write(iud,'(/," STOP_MPI: MY_ID =",I10)') MY_ID
      write(iud,'(" Error ",a)') ctest
      close(iud)
      CALL MPI_FINALIZE(IERR)
      STOP
      END
