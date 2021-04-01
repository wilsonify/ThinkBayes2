      SUBROUTINE WRITE_MPI_I(NSEND,NRECV,NSR,IUO,itag)
C Copyright, Bernd Berg, Oct 31 2001.
C Purpose: Prints integer results (up to 10 integers) in a desired order.
      INCLUDE 'implicit.sta'
      INCLUDE 'mpif.h'
      DIMENSION ISTATUS(MPI_STATUS_SIZE)
      DIMENSION NSEND(NSR),NRECV(NSR)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD,MY_ID,IERR)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,N_PROC,IERR)
      IF(MY_ID.EQ.0) WRITE(IUO,*) "  "
      DO I=0,(N_PROC-1)
        IF(MY_ID.EQ.I) CALL MPI_SEND(NSEND,NSR,MPI_INTEGER,0,itag,
     &                               MPI_COMM_WORLD,IERR)
        IF(MY_ID.EQ.0) THEN 
           CALL MPI_RECV(NRECV,NSR,MPI_INTEGER,I,itag,
     &                               MPI_COMM_WORLD,ISTATUS,IERR)
           WRITE(IUO,'(10I7)') I,NRECV
        END IF
      END DO
      RETURN
      END
