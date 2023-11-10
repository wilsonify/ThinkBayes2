      SUBROUTINE P_PT_MPI(N_PROC,itag)
C Copyright, Bernd Berg, December 20, 2001.
C Parallel tempering for the Potts models heat bath code.
C MPI implementation of the beta exchange.
      include '../../ForLib/implicit.sta'
      INCLUDE 'mpif.h'
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'potts.par'
      include 'mc_pt_mpi.par'
      include '../../ForLib/potts_hb.com'
      include '../../ForLib/p_pt_mpi.com'
      DIMENSION ISTATUS(MPI_STATUS_SIZE)
      DATA INDEX/-1/
      SAVE INDEX
C
      itag=mod(itag+1,32768)
      INDEX=MOD(INDEX+1,3)
      MY_IND1=MOD(MY_B,3)
      MY_IND2=MOD(MY_B+2,3) ! MY_B-1+3
      MY_IND3=MOD(MY_B+1,3) ! MY_B-2+3
C
      IF(MY_IND1==INDEX) THEN ! Processes MY_IND1.
        NDEST3L=NEIGH(1)
        NSEND3L=-2
        IF(NEIGH(2)<N_PROC) THEN
          NSEND(1)=NEIGH(1)
          NSEND(2)=iact
          NSEND(3)=IACPT_PT
          CALL MPI_SEND(NSEND,3,MPI_INTEGER,NEIGH(2),itag,
     &                  MPI_COMM_WORLD,IERR)
          CALL MPI_RECV(NRECV,2,MPI_INTEGER,NEIGH(2),itag,
     &                  MPI_COMM_WORLD,ISTATUS,IERR)
          IF(NRECV(1)/=-2) THEN
            NSEND3L=NEIGH(2)
            NEIGH(1)=NEIGH(2)
            NEIGH(2)=NRECV(1)
            IACPT_PT=NRECV(2)
            MY_B=MY_B+1
            call potts_wghb(whb_tab,BA(MY_B),H0,nd,nqm1) ! beta=BA(MY_B)
          END IF
        END IF
        IF(NDEST3L>=0) CALL MPI_SEND(NSEND3L,1,MPI_INTEGER,NDEST3L,
     &                                     itag,MPI_COMM_WORLD,IERR)
      END IF
C
      IF(MY_IND2==INDEX) THEN ! Processes MY_IND2.
        NDEST3R=NEIGH(2)
        NSEND3R=-2
        IF(NEIGH(1)>=0) THEN
          CALL MPI_RECV(NRECV,3,MPI_INTEGER,NEIGH(1),itag,
     &                  MPI_COMM_WORLD,ISTATUS,IERR)
          NDEST1=NEIGH(1)
          IF(LPT_EX_IA(BA(MY_B),BA(MY_B-1),iact,NRECV(2))) THEN
            NSEND(1)=NEIGH(2)
            NSEND(2)=IACPT_PT+1
            IACPT_PT=NRECV(3)
            NSEND3R=NEIGH(1)
            NEIGH(2)=NEIGH(1)
            NEIGH(1)=NRECV(1)
            MY_B=MY_B-1
            call potts_wghb(whb_tab,BA(MY_B),H0,nd,nqm1) ! beta=BA(MY_B)
          ELSE
            NSEND(1)=-2
          END IF
          CALL MPI_SEND(NSEND,2,MPI_INTEGER,NDEST1,itag,
     &                  MPI_COMM_WORLD,IERR)
        END IF
        IF(NDEST3R<N_PROC) CALL MPI_SEND(NSEND3R,1,MPI_INTEGER,
     &                              NDEST3R,itag,MPI_COMM_WORLD,IERR)
      END IF
C
      IF(MY_IND3==INDEX) THEN ! Processes MY_IND3.
        IF(NEIGH(1)>=0) THEN
          CALL MPI_RECV(NRECV3R,1,MPI_INTEGER,NEIGH(1),
     &                  itag,MPI_COMM_WORLD,ISTATUS,IERR)
           IF(NRECV3R/=-2) NEIGH(1)=NRECV3R
        END IF 
        IF(NEIGH(2)<N_PROC) THEN
          CALL MPI_RECV(NRECV3L,1,MPI_INTEGER,NEIGH(2),
     &                  itag,MPI_COMM_WORLD,ISTATUS,IERR)
          IF(NRECV3L/=-2) NEIGH(2)=NRECV3L
        END IF 
      END IF
C
      RETURN
      END
