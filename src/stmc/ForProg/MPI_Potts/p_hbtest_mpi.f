      program p_hbtest_mpi
C Copyright, Bernd Berg, Jan 9 2002.
C MPI test for the Potts model Heat Bath (HB) MC algorithm.
      include '../../ForLib/implicit.sta'
      character cd*1,cq*2,cl*3,cmy_id*2,cnp*2
      include 'mpif.h'
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'mc_pt_mpi.par'
      include 'potts.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts_hb.com'
      include '../../ForLib/p_pt_mpi.com'
      include 'lat.dat'
C
      CALL MPI_INIT(IERR)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD,MY_ID,IERR)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,N_PROC,IERR)
      IF(MY_ID.EQ.0) WRITE(IUO,'(/," MPI: N_PROC =",I5)') N_PROC
      MY_B=MY_ID
      NEIGH(1)=MY_ID-1
      NEIGH(2)=MY_ID+1
      NSEND(1)=MY_B
      NSEND(2)=NEIGH(1) 
      NSEND(3)=NEIGH(2) 
C
      call p_inithb_mpi(cd,cq,cl,cmy_id,cnp,.false.) ! Initialize Potts HB MC.
      NSEND(4)=iact
C
      do iequi=1,nequi           ! Sweeps for reaching equilibrium.
        call potts_mchb
      end do
      NSEND(5)=iact
C
C Print some results in a specified order:
      IF(MY_ID.EQ.0) WRITE(IUO,'(30X,"     iact             ")')
      IF(MY_ID.EQ.0) WRITE(IUO,
     & '(/,"    MY_ID  MY_B      NEIGH    (start) (equilibrium) ")')
      itag=0 
      CALL WRITE_MPI_I(NSEND,NRECV,5,IUO,itag)
      CALL MPI_FINALIZE(IERR)
C
      STOP
      END

      include 'p_inithb_mpi.f'
      include 'potts_mchb.f'

      include '../../ForLib/ipointer.f'
      include '../../ForLib/isfun.f'
      include '../../ForLib/ixcor.f'
      include '../../ForLib/lat_init.f'
      include '../../ForLib/nsfun.f'

      include '../../ForLib/potts_act.f'
      include '../../ForLib/potts_act_tab.f'
      include '../../ForLib/potts_ran.f'
      include '../../ForLib/potts_wghb.f'

      include '../../ForLib/ranmar.f'
      include '../../ForLib/rmaset.f'
      include '../../ForLib/razero.f'

      include '../../ForLib/write_mpi_i.f'
