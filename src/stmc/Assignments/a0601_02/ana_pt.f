      program ana_ptmpi ! Analysis program for Parallel Tempering data.
C Copyright, Bernd Berg, Jan 9 2002.
C Single processor program!
      include '../../ForLib/implicit.sta'
      character cd*1,cq*2,cl*3,cmy_id*2,cnp*2
      include '../../ForLib/constants.par'
      PARAMETER(N_PROC=08,NPM1=N_PROC-1)
      include 'lat.par'
      include 'mc_pt_mpi.par'
      include 'potts.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts_hb.com'
c     include 'p_pt_mpi.com'
      include '../../ForLib/p_pt_mpi.com'
      include 'lat.dat'
      DIMENSION HA_BETA(0:mlink,0:MPM1),HA_MAX(0:MPM1),HA_W(0:MPM1)
      DIMENSION ACPT_B(0:MPM1),ACPT_PT(0:MPM1)
      DIMENSION MY_B_TS(nrpt*nmea2,0:MPM1),MY_ID_TS(nrpt*nmea2,0:MPM1)
      DIMENSION NTUN(0:MPM1),NBOUNCE(0:MPM1),NACPT(0:MPM1)
C
      WRITE(IUO,'(/," MPI Analysis: N_PROC =",I5)') N_PROC
      WRITE(IUO,'(" mc_mpi_pt.par: Max. # processes:",I6)') (MPM1+1)
      write(iuo,'(" nd,nq,ml,nla:",2I4,I6,3X,6I6)') nd,nq,ml,nla
      write(iuo,'(" NPT_REC,nequi,nequ2,nequ1:",4I6)') 
     &              NPT_REC,nequi,nequ2,nequ1
      write(iuo,'(" nrpt,nmea2,nmea1:",9X,3I6)') nrpt,nmea2,nmea1
C
      write(cd,'(I1.1)') nd
      write(cq,'(I2.2)') nq
      write(cl,'(I3.3)') nla(1)
      write(cnp,'(I2.2)') N_PROC
      DO MY_B=0,NPM1
        HA_MAX(MY_B)=zero
        HA_W(MY_B)=zero
        ACPT_B(MY_B)=zero
        ACPT_PT(MY_B)=zero
        NTUN(MY_B)=0
        NBOUNCE(MY_B)=0
        NACPT(MY_B)=0
        do ilink=0,mlink
          HA_BETA(ilink,MY_B)=zero
        end do
      END DO
C
      DO MY_ID=0,NPM1
        write(cmy_id,'(I2.2)') MY_ID
        open(iud1,file=cnp//"p"//cd//"d"//cq//"q"//cl//".d"//cmy_id,
     &            form="unformatted",status="unknown")
        read(iud1) nd_in,ml_in,nla,nq_in,NPT_REC_in,nequi_in,nequ2_in,
     &             nequ1_in,nrpt_in,nmea2_in,nmea1_in
        nlink=nd_in
        do id=1,nd_in
          nlink=nlink*nla(id)
        end do
C
        itime=0
        ltun=.false.
        do irpt=1,nrpt
          read(iud1) hab,MY_BA,irpt_in
          do imea2=1,nmea2
            itime=itime+1
            MY_B2=MY_BA(imea2)
            MY_B_TS(itime,MY_ID)=MY_B2
            MY_ID_TS(itime,MY_B2)=MY_ID
            call tun_cnt(0,NPM1,MY_B2,MY_B2,NTUN(MY_ID),ltun)
          end do
          DO MY_B=0,NPM1
          do ilink=0,nlink
            HA_BETA(ilink,MY_B)=HA_BETA(ilink,MY_B)+hab(ilink,MY_B)
            HA_W(MY_B)=HA_W(MY_B)+hab(ilink,MY_B)
          end do
          END DO
        end do
        close(iud1)
      END DO
      WRITE(IUO,'(/," All DATA read.")')
      WRITE(IUO,'(/," HA_W:",4G16.7)') (HA_W(MY_B),MY_B=0,NPM1)
      WRITE(IUO,'(/," NTUN:",8I5)') (NTUN(MY_ID),MY_ID=0,NPM1)
      NTUN_ALL=NSUM(0,NPM1,NTUN)
      WRITE(IUO,'(" NTUN_ALL =",I10)') NTUN_ALL
C
      ntime=nrpt*nmea2
      write(iuo,'(/," ntime =",I10)') ntime
C
C Acceptance rates for beta-exchange:
      ASUM_B=zero 
      DO MY_B=0,NPM1
        do itime=2,ntime
          if(MY_ID_TS(itime,MY_B)/=MY_ID_TS(itime-1,MY_B))
     &      ACPT_B(MY_B)=ACPT_B(MY_B)+three
        end do
        ACPT_B(MY_B)=ACPT_B(MY_B)/(2*ntime)
        ASUM_B=ASUM_B+ACPT_B(MY_B)
      END DO 
      WRITE(IUO,*) "  "
      WRITE(IUO,'(" ACPT_B: ",4G16.7)') (ACPT_B(MY_B),MY_B=0,NPM1)
C
      ASUM_PT=zero
      DO MY_B=0,NPM1,2
        do itime=2,ntime
          if(MY_ID_TS(itime,MY_B)/=MY_ID_TS(itime-1,MY_B)) then
            if(MY_B+1<=NPM1.and.
     &      MY_ID_TS(itime,MY_B+1)==MY_ID_TS(itime-1,MY_B)) then
              ACPT_PT(MY_B+1)=ACPT_PT(MY_B+1)+three
            else
              ACPT_PT(MY_B)=ACPT_PT(MY_B)+three
            end if
          end if
        end do
      END DO 
      DO MY_B=0,NPM1
        ACPT_PT(MY_B)=ACPT_PT(MY_B)/ntime
        ASUM_PT=ASUM_PT+ACPT_PT(MY_B)
      END DO 
      WRITE(IUO,*) "  "
      WRITE(IUO,'(" ACPT_PT:",4G16.7)') (ACPT_PT(MY_B),MY_B=0,NPM1)
      WRITE(IUO,'(/," ASUM_B,ASUM_PT:",2G16.7)') ASUM_B,ASUM_PT
C
      open(iud1,file="b_ts.d",form="formatted",status="unknown")
      do itime=1,ntime
        write(iud1,'(I6,2X,8I4)') itime,
     &       (MY_B_TS(itime,MY_ID),MY_ID=0,NPM1)
      end do
      close(iud1)
C
      write(iuo,'(/," Bounce analysis:")')
      ntm1=ntime-1
      NPM2=NPM1-1
      DO MY_ID=0,NPM1
        DO itime=1,ntm1
          IF(MY_B_TS(itime,MY_ID)/=MY_B_TS(itime+1,MY_ID)) THEN
            NACPT(MY_B_TS(itime,MY_ID))=NACPT(MY_B_TS(itime,MY_ID))+1
            IF(MY_B_TS(1,MY_ID)==MY_B_TS(itime+1,MY_ID)) THEN
              NBOUNCE(MY_B_TS(itime,MY_ID))=
     &        NBOUNCE(MY_B_TS(itime,MY_ID))+1
            END IF ! MY_B(1,MY_I) is assgned next.
            MY_B_TS(1,MY_ID)=MY_B_TS(itime,MY_ID) ! itime-1 in IF.
          END IF
        END DO
      END DO
      write(iuo,'(" Accepted:",8I5)') (NACPT(MY_B),MY_B=0,NPM1) 
      write(iuo,'(" Bounced: ",8I5)') (NBOUNCE(MY_B),MY_B=0,NPM1) 
      DO MY_B=0,NPM1
        ACPT_PT(MY_B)=(ONE*NBOUNCE(MY_B))/(ONE*NACPT(MY_B))
      END DO
      write(iuo,'(" Probab.: ",8F5.2)') (ACPT_PT(MY_B),MY_B=0,NPM1) 
C
      open(iud1,file="ID_ts.d",form="formatted",status="unknown")
      do itime=1,ntime
        write(iud1,'(I6,2X,8I4)') itime,
     &       (MY_ID_TS(itime,MY_B),MY_B=0,NPM1)
      end do
      close(iud1)
C
      DO MY_B=0,MPM1
        if(lnomax) then
          HA_MAX(MY_B)=one
        else
          do ilink=0,nlink
            HA_MAX(MY_B)=MAX(HA_MAX(MY_B),HA_BETA(ilink,MY_B))
          end do
        end if
      END DO
      DO MY_B=0,MPM1
      do ilink=0,nlink
        HA_BETA(ilink,MY_B)=HA_BETA(ilink,MY_B)/HA_MAX(MY_B)
      end do
      END DO
      open(iud1,file="histo_1.d",form="formatted",status="unknown")
      do ilink=0,nlink
        act=(one*ilink)/(one*nlink)
        write(iud1,'(5G14.6)') act,(HA_BETA(ilink,MY_B),MY_B=0,3)
      end do
      close(iud1)
      IF(NPM1==7) THEN
        open(iud1,file="histo_2.d",form="formatted",status="unknown")
        do ilink=0,nlink
          act=(one*ilink)/(one*nlink)
          write(iud1,'(5G14.6)') act,(HA_BETA(ilink,MY_B),MY_B=4,7)
        end do
        close(iud1)
      END IF
      WRITE(IUO,*) "All done."
C
      STOP
      END

      include '../../ForLib/tun_cnt.f'
      include '../../ForLib/nsum.f'
