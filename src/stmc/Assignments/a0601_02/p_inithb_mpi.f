      subroutine p_inithb_mpi(cd,cq,cl,cmy_id,cnp,lpri)
C Copyright, Bernd Berg, Nov 26 2001.
C Initialization for MPI (parallel tempering) Potts Model heat bath MC.
      include '../../ForLib/implicit.sta'
      character cd*1,cq*2,cl*3,cmy_id*2,cnp*2
      INCLUDE 'mpif.h'
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'potts.par'
      include 'mc_pt_mpi.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts_hb.com'
      include '../../ForLib/p_pt_mpi.com'
      dimension ix(nd)
C
C Initialize:
      CALL MPI_COMM_RANK(MPI_COMM_WORLD,MY_ID,IERR)
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD,N_PROC,IERR)
      MY_B=MY_ID
      NEIGH(1)=MY_ID-1
      NEIGH(2)=MY_ID+1
      IACPT_PT=0                    ! PT exchange acceptance rate.
      NSEND(1)=MY_B
      NSEND(2)=NEIGH(1)
      NSEND(3)=NEIGH(2)
      IF(MPM1.LT.N_PROC-1) STOP "p_inithb_mpi: MPM1 - N_PROC mismatch."
      IF(N_PROC.eq.1) THEN
        BA(0)=half*(beta_min+beta_max)
      ELSE
        del_beta=(beta_max-beta_min)/(N_PROC-1)
        DO I=0,N_PROC-1 
          BA(I)=beta_min+I*del_beta
        END DO
      END IF
      iseed2=isd2_0+MY_B
      call rmaset(-iuo,iud1,iseed1,iseed2,'nexiste.pa') ! Random numbs.
      call ranmar(xr)                             ! First random number.
      ia_min=mlink
      ia_max=0
      call lat_init(ns,nd,ipf,ipb,nla,ix,nlink)               ! lattice.
      call potts_act_tab(idel,nqm1)                ! Potts action table.
      call potts_ran(ista,ns,nq)        ! Potts initial states (random).
      call potts_act(ista,ipf,idel,ns,nqm1,nd,iact)      ! Potts action.
      call potts_wghb(whb_tab,BA(MY_B),H0,nd,nqm1)    ! Weights.
      call razero(ha,0,nlink)             ! Initialize action histogram.
      do iq=0,nqm1
        nstate(iq)=0
        do is=0,ms ! Initialize magnetization arrays.
          hm(is,iq)=zero
        end do
      end do 
      do is=0,ms ! Initialize magnetization arrays.
        nstate(ista(is))=nstate(ista(is))+1
      end do 
C
      write(cd,'(I1.1)') nd
      write(cq,'(I2.2)') nq
      write(cl,'(I3.3)') nla(1)
      write(cmy_id,'(I2.2)') MY_ID
      write(cnp,'(I2.2)') N_PROC
      if(lpri .and .IUD.gt.0 .and. MY_ID.eq.0) then
        OPEN(IUD,file="pt"//cd//"d"//cq//"q"//cl//".t"//cnp,
     &       form="formatted",status="unknown")
        WRITE(IUD,'(/," MPI: N_PROC =",I5)') N_PROC
        WRITE(IUD,'(" mc_mpi_pt.par: Max. # processes:",I6)') (MPM1+1)
        WRITE(IUD,'(" lat.par: nd,nq,nla =",1I2,1I3,3X,8I4)') nd,nq,nla
        WRITE(IUD,'(" NPT_REC,nequi,nequ2,nequ1:",4I8)') 
     &                NPT_REC,nequi,nequ2,nequ1
        WRITE(IUD,'(" nrpt,nmea2,nmea1:",9X,4I8)') nrpt,nmea2,nmea1
        WRITE(IUD,'(" Product equi pt:",1I20)') 
     &                     NPT_REC*(nequi+nequ2*nequ1+nequ1+nequ3)
        WRITE(IUD,'(" Product meas:   ",1I20)') (nrpt*nmea2*nmea1)
        WRITE(IUD,'(/," Beta Exchanges (Recursion):")')
        WRITE(IUD,'(" BA:",3X,8F8.5)') (BA(I),I=0,N_PROC-1)
        CLOSE(IUD)
      end if
      return
      end
