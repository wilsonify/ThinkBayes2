      program dat_transfer
c Fix screw up.
      include '../../ForLib/implicit.sta'
      character cd*1,cq*2,cl*3,cdat*12,cfix*12,cbck*14
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include 'muca.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      include '../../ForLib/potts_muca.com'
      include 'lat.dat'
      data mu_swp/0/, ntun/0/,ntun_old/0/,ltun0/.false./, irec1/1/
      data iequi/0/,iequ1/1/, irpt/1/,irp1/1/,imea1/1/,imeas/0/,ntu_m/0/
      ltest=.true.
      ltest=.false.
      if(ltest) stop "ltest."
      if(nb_swp.gt.nmeas) write(iuo,'(/," nb_swp =",I12,/,1X,
     & "WARNING MEASURMENT BACKUPS require nb_swp.ge.nmeas!",/)') nb_swp
C
      write(cd,'(I1.1)') nd
      write(cq,'(I2.2)') nq
      write(cl,'(I3.3)') nla(1)
      cdat="mu"//cd//"d"//cq//"q"//cl//".d"
      cfix="fi"//cd//"d"//cq//"q"//cl//".d"
      cbck="mu"//cd//"d"//cq//"q"//cl//".bck"
C
C Initialize Potts Metropolis MC:
      inquire(file=cbck,exist=lexist)
      lpr=.not.lexist
      call potts_init(lpr)
      if(lexist) call read_bck(cbck,irec1,mu_swp,na_swp,
     &           ntun,ntun_old,ltun0, iequ1,irp1,imea1,ntu_m)
c
      if(.not.lexist) STOP "ERROR."
      open(iud1,file=cdat,form="unformatted",status="old")
      read(iud1) 
      read(iud1) 
      open(iud2,file=cfix,form="unformatted",status="unknown")
      write(iuo,'(/," Data file ",A12," created.")') cfix
      write(iud2) nd,ml,nla,nq,namin,namax,irec,nrec_max,nmucasw,
     &            mu_swp,ntun,nequi,nrpt,nmeas
      write(iud2) wrat,ndel_muca
      do irpt=1,nrpt
        read(iud1)  ha,acpt,irpt_in,ntu_m
        write(iud2) ha,acpt,irpt,ntu_m
      end do
      close(iud1)
      close(iud2)
C
      stop
      end


      SUBROUTINE read_bck(cbck,irec1,mu_swp,na_swp,ntun,ntun_old,ltun0,
     &                         iequ1, irp1,imea1,ntu_m)
      include '../../ForLib/implicit.sta'
      character*(*) cbck
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include 'muca.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      include '../../ForLib/potts_muca.com'
      call rmaset(iuo,iud1,iseed1,iseed2,cbck)
      open(iud1,file=cbck,form="unformatted",status="old")
      rewind iud1
      read(iud1)
      read(iud1) wrat, ha,acpt,  hmuca,hup,hdn, gw,
     & ista,nlink,iact,iamin,iamax, ia_min,ia_max, ndel_muca,iastep,
     & irec1,mu_swp,na_swp,ntun,ltun0, iequi,irp1,imeas,ntu_m
      close(iud1)
      ntun_old=ntun
      iequ1=iequi+1
      imea1=imeas+1
      return
      end


      SUBROUTINE write_bck(cbck,irec,mu_swp,na_swp,ntun,ltun0,
     &                     iequi, irpt,imeas,ntu_m)
      include '../../ForLib/implicit.sta'
      character*(*) cbck
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include 'muca.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      include '../../ForLib/potts_muca.com'
C
C      print'(1X,"W_BCK: mu_wsp,iequ1,irp1,imea1 =",4I10)',
C     &                  mu_swp,(iequi+1),irpt,(imeas+1)
      call rmasave(iud1,cbck)
      open(iud1,file=cbck,form="unformatted",status="old",
     &          access="append")
      write(iud1) wrat, ha,acpt,  hmuca,hup,hdn, gw,
     & ista,nlink,iact,iamin,iamax, ia_min,ia_max, ndel_muca,iastep,
     & irec,mu_swp,na_swp,ntun,ltun0, iequi,irpt,imeas,ntu_m
      close(iud1)
C
      open(iud1,file="progress.d",form="formatted",status="unknown")
      write(iud1,'(/," nb_swp =",I10,6X,"Last backup with:")') nb_swp
      write(iud1,'(" irec1, mu_swp,   ntun  =",3I12)') irec,mu_swp,ntun
      write(iud1,'(" iequ1,irp1,imea1,ntu_m =",4I12,/)')
     &           (iequi+1),irpt,(imeas+1),ntu_m
      close(iud1)
      return
      end

      include 'p_mu_init.f'
      include 'p_mu_rec.f'

      include 'potts_init.f'
      include 'potts_met.f'

      include '../../ForLib/ipointer.f'
      include '../../ForLib/isfun.f'
      include '../../ForLib/ixcor.f'
      include '../../ForLib/lat_init.f'
      include '../../ForLib/nsfun.f'

      include '../../ForLib/potts_act.f'
      include '../../ForLib/potts_act_tab.f'
      include '../../ForLib/potts_order.f'
      include '../../ForLib/potts_ran.f'
      include '../../ForLib/potts_wght.f'
      include '../../ForLib/razero.f'

      include '../../ForLib/ranmar.f'
      include '../../ForLib/rmaset.f'
      include '../../ForLib/rmasave.f'
      include '../../ForLib/tun_cnt.f'
