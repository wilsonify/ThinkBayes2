C JUL 03 2001
      program prod_mu
C Copyright, Bernd Berg, May 26 2001.
C Potts muca with Metropolis updating.
      include '../../ForLib/implicit.sta'
      character cd*1,cq*2,cl*3,cdat*14,cbak*14
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
      if(nb_swp>nmeas) write(iuo,'(/," nb_swp =",I12,/,1X,
     & "WARNING MEASURMENT BACKUPS require nb_swp.ge.nmeas!",/)') nb_swp
      if(nb_swp>nmeas) stop "BACKUPS require nb_swp.ge.nmeas."
C
      write(cd,'(I1.1)') nd
      write(cq,'(I2.2)') nq
      write(cl,'(I3.3)') nla(1)
      cdat="mu"//cd//"d"//cq//"q"//cl//".dat"
      cbak="mu"//cd//"d"//cq//"q"//cl//".bak"
C
C Initialize Potts Metropolis MC:
      inquire(file=cbak,exist=lexist)
      lpr=.not.lexist
      call potts_init(lpr)
      if(lexist) call read_bak(cbak,irec,irec1,mu_swp,na_swp,
     &           ntun,ntun_old,ltun0, iequ1,irp1,imea1,ntu_m)
      if(.not.lexist) call potts_act(ista,ipf,idel,ns,nqm1,nd,iact)
      if(ntun>=maxtun) go to 3
c
      write(iuo,*) '   '
      write(iuo,'(" MUCA Recursion:",)')
      if(.not.lexist) call p_mu_init
      write(iuo,'(" namin,namax,maxtun:     ",3I10)') namin,namax,maxtun
      write(iuo,'(" nrec_max,nmucasw,iastep:",3I10)')
     &              nrec_max,nmucasw,iastep
      do irec=irec1,nrec_max
        if(.not.lexist) acpt=zero
        if(.not.lexist) na_swp=0
1       continue
          if(mu_swp>0.and.mod(mu_swp,nb_swp)==0) call write_bak
     &      (cbak,irec,mu_swp,na_swp,ntun,ltun0,iequi,irpt,imeas,ntu_m)
          mu_swp=mu_swp+1
          na_swp=na_swp+1
          call potts_met
          call tun_cnt(namin,namax,iamin,iamax,ntun,ltun0)
          if(ntun>ntun_old) then
            write(iuo,'(" ntun,irec,mu_swp,acpt:",I6,I9,I12,F9.3)')
     &      ntun,irec,mu_swp,((nmucasw*irec*one)/(mu_swp*one))
            ntun_old=ntun
          if(ntun>=maxtun) go to 2
          end if
        if((acpt/ns)<(nmucasw*one)) go to 1 !
C The next MUCA recursion is done, when acpt was  big enough:
        call p_mu_rec
        acpt=acpt/(ns*na_swp)
      end do
      write(iuo,*) "Muca recursion   n o t   finished, ia_max =",ia_max
      STOP         "Muca recursion   n o t   finished."
2     write(iuo,*) "Muca recursions done."
3     continue
      if(irp1==1) then
        write(iuo,'(/," Data file ",A14," created.")') cdat
        open(iud2,file=cdat,form="unformatted",status="unknown")
        write(iud2) nd,ml,nla,nq,namin,namax,irec,nrec_max,nmucasw,
     &              mu_swp,ntun,nequi,nrpt,nmeas
        write(iud2) wrat,ndel_muca
        close(iud2)
      end if
C
      if(nequi>=iequ1) then
      write(iuo,*) "  "
      write(iuo,'(I12," sweeps for reaching equilibrium.")') nequi
        if(iequ1==1) acpt=zero
        do iequi=iequ1,nequi
          call potts_met
          if(mod(iequi,nb_swp)==0) call write_bak(cbak,irec,
     &      mu_swp,na_swp,ntun,ltun0, iequi,irpt,imeas,ntu_m)
        end do
        write(iuo,'(" ia_min,ia_max, acpt rate:",2I9,F9.3)')
     &                ia_min,ia_max,(acpt/(nequi*ns))
      end if
C
      write(iuo,'(/,I9," times",I12," sweeps with measurements.")')
     & nrpt,nmeas
      if(nrpt>0.and.nmeas>0) then
        do irpt=irp1,nrpt
          call razero(ha,0,nlink)
          if(imea1==1) acpt=zero
          do imeas=imea1,nmeas
            call potts_met
            call tun_cnt(namin,namax,iamin,iamax,ntu_m,ltun0)
            if(mod(imeas,nb_swp)==0) call write_bak(cbak,irec,
     &        mu_swp,na_swp,ntun,ltun0, nequi,irpt,imeas,ntu_m)
          end do
          acpt=acpt/(nmeas*ns)
          write(iuo,'(" irpt,ntu_m,acpt:",2I10,F9.3)') irpt,ntu_m,acpt
          open(iud2,file=cdat,form="unformatted",
     &              status="old",access="append")
          if(imea1<=nmeas) write(iud2) ha,acpt,irpt,ntu_m
          close(iud2)
          imea1=1
        end do
      end if
C
      stop
      end


      SUBROUTINE read_bak(cbak,irec,irec1,mu_swp,na_swp,ntun,ntun_old,
     &                         ltun0,iequ1, irp1,imea1,ntu_m)
      include '../../ForLib/implicit.sta'
      character*(*) cbak
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include 'muca.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      include '../../ForLib/potts_muca.com'
      write(iuo,'(/," Interruption ...",/)') 
      call rmaset(iuo,iud1,iseed1,iseed2,cbak)
      open(iud1,file=cbak,form="unformatted",status="old")
      rewind iud1
      read(iud1)
      read(iud1) wrat, ha,acpt,  hmuca,hup,hdn, gw,
     & ista,nlink,iact,iamin,iamax, ia_min,ia_max, ndel_muca,iastep,
     & irec,mu_swp,na_swp,ntun,ltun0, iequi,irp1,imeas,ntu_m
      close(iud1)
      irec1=irec
      ntun_old=ntun
      iequ1=iequi+1
      imea1=imeas+1
      return
      end


      SUBROUTINE write_bak(cbak,irec,mu_swp,na_swp,ntun,ltun0,
     &                     iequi, irpt,imeas,ntu_m)
      include '../../ForLib/implicit.sta'
      character*(*) cbak
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include 'muca.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      include '../../ForLib/potts_muca.com'
      call rmasave(iud1,cbak)
      open(iud1,file=cbak,form="unformatted",status="old",
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
