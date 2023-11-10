      program p_ptmpi1
C Copyright, Bernd Berg, Dec 18 2001.
C Parallel tempering MPI implementation with recursion pt_rec1.f.
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
      NPM1=N_PROC-1
      call p_inithb_mpi(cd,cq,cl,cmy_id,cnp,.true.) ! Initialize Potts HB MC.
      IF(MY_ID==0) THEN
        OPEN(IUD,file="pt"//cd//"d"//cq//"q"//cl//".t"//cnp,
     &         form="formatted",status="old",access="append")
        WRITE(IUD,*) "p_ptmpi1.f with recursion pt_rec1.f."
        CLOSE(IUD)
      END IF 
C
      do iequi=1,nequi
        call potts_mchb
      end do
C
      itag=0
      DO IPT_REC=1,NPT_REC ! PT recursion to distribute the beta values.
C
        IACPT_PT=0
        do iequ2=1,nequ2
          do iequ1=1,nequ1
            call potts_mchb
          end do
          CALL P_PT_MPI(N_PROC,itag)
        end do
C
        CALL MPI_ALLGATHER(MY_B,1,MPI_INTEGER,
     &           IP_B(0),1,MPI_INTEGER,MPI_COMM_WORLD,IERROR)
        CALL MPI_ALLGATHER(IACPT_PT,1,MPI_INTEGER,
     &                NACPT_PT(0),1,MPI_INTEGER,MPI_COMM_WORLD,IERROR)
        CALL 
     &    PT_REC1(BA,BASUM,NACPT_PT,IP_B,IPI_B,MY_ID,NPM1,NPT_REC,IUD)
        CALL POTTS_WGHB(WHB_TAB,BA(MY_B),H0,nd,nqm1)
        IF(MY_ID==0) THEN
          OPEN(IUD,file="pt"//cd//"d"//cq//"q"//cl//".t"//cnp,
     &         form="formatted",status="old",access="append")
          WRITE(IUD,'(I4,".",5X,7I8)') IPT_REC,
     &                        (NACPT_PT(IPI_B(I))/2,I=1,NPM1)
          IF(IPT_REC==NPT_REC) WRITE(IUD,'(" BA:",3X,8F8.5)')
     &                                      (BA(I),I=0,NPM1)
          CLOSE(IUD)
        END IF
          do iequ3=1,nequ3 ! Equilibriate after beta recursion.
            call potts_mchb
          end do
      END DO
C
        IF(MY_ID==0) THEN
          OPEN(IUD,file="pt"//cd//"d"//cq//"q"//cl//".t"//cnp,
     &         form="formatted",status="old",access="append")
          WRITE(IUD,*) "Start of the production run ..."
        CLOSE(IUD)
        END IF
C 
C Production:
C
      OPEN(IUD1,file=cnp//"p"//cd//"d"//cq//"q"//cl//".d"//cmy_id,
     &          form="unformatted",status="unknown")
        WRITE(IUD1) nd,ml,nla,nq,NPT_REC,nequi,n_mpi,nequ1,
     &              nrpt,nmea2,nmea1
        CLOSE(IUD1)
      do irpt=1,nrpt
        do I=0,NPM1
          do ilink=0,nlink
            hab(ilink,I)=zero
          end do
        end do
        do imea2=1,nmea2
          call razero(ha,0,nlink)
          do imea1=1,nmea1
            call potts_mchb
          end do
          MY_BA(imea2)=MY_B
          do ilink=0,nlink
            hab(ilink,MY_B)=hab(ilink,MY_B)+ha(ilink)
          end do
          CALL P_PT_MPI(N_PROC,itag)
        end do
        OPEN(IUD1,file=cnp//"p"//cd//"d"//cq//"q"//cl//".d"//cmy_id,
     &            form="unformatted",status="old",access="append")
        WRITE(IUD1) hab,MY_BA,irpt
        CLOSE(IUD1) 
      end do
C
        IF(MY_ID==0) THEN
          OPEN(IUD1,file="pt"//cd//"d"//cq//"q"//cl//".t"//cnp,
     &         form="formatted",status="old",access="append")
          WRITE(IUD1,'(" all done. Results on",a2,"* files.")') cnp
          CLOSE(IUD1)
        END IF
      CALL MPI_FINALIZE(IERR)
      STOP
      END


      include 'p_pt_mpi.f'
      include 'p_inithb_mpi.f'
      include 'potts_mchb.f'
      include '../../ForLib/lpt_ex_ia.f'
      include '../../ForLib/pt_rec1.f'
      include '../../ForLib/stop_mpi.f'
      include '../../ForLib/write_mpi_i.f'

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
      include '../../ForLib/rmafun.f'
      include '../../ForLib/rmaset.f'
      include '../../ForLib/razero.f'

