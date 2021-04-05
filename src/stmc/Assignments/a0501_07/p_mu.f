      program p_mu
C Copyright, Bernd Berg, May 24 2001.
C Potts muca with Metropolis updating.
      include '../../ForLib/implicit.sta'
      character cd*1,cq*2,cl*3
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include 'muca.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      include '../../ForLib/potts_muca.com'
      include 'lat.dat'
      data ltun0/.false./
      ltest=.true.
      ltest=.false.
      if(ltest) stop "ltest."
      write(cd,'(I1.1)') nd
      write(cq,'(I2.2)') nq
      write(cl,'(I3.3)') nla(1)
C
C Initialize Potts Metropolis MC:
      lpr=.true.
      call potts_init(lpr)
      call potts_act(ista,ipf,idel,ns,nqm1,nd,iact)
c
      write(iuo,*) '   '
      write(iuo,'(" MUCA Recursion:")')
      call p_mu_init
      write(iuo,'(" namin,namax,maxtun:     ",3I10)') namin,namax,maxtun
      write(iuo,'(" nrec_max,nmucasw,iastep:",3I10)')
     &              nrec_max,nmucasw,iastep
      mu_swp=0
      ntun=0
      ntun_old=0
      do irec=1,nrec_max
        acpt=zero
        nswp=0
1       continue
          mu_swp=mu_swp+1
          nswp=nswp+1
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
        acpt=acpt/(ns*nswp)
        call write_progress(iud2,"mu_swp,ntun,acpt:",mu_swp,ntun,acpt)
      end do
      write(iuo,*) "Muca recursion   n o t   finished, ia_max =",ia_max
      STOP         "Muca recursion   n o t   finished."
C
2     continue
      write(iuo,*) "Muca recursions done."
      write(iuo,*) "  "
      write(iuo,'(I12," sweeps for reaching equilibrium.")') nequi
      if(nequi/=0) then
        acpt=zero
        do iequi=1,nequi
          call potts_met
        end do
        write(iuo,'(" ia_min,ia_max, acpt rate:",2I9,F9.3)')
     &                ia_min,ia_max,(acpt/(nequi*ns))
      end if
C
      write(iuo,*) "  "
      write(iuo,'(I9," times",I12," sweeps with measurements.")')
     & nrpt,nmeas
      if(nrpt>0.and.nmeas>0) then
        open(iud1,file="mu"//cd//"d"//cq//"q"//cl//".dat",
     &  form="unformatted",status="unknown")
        write(iud1) nd,ml,nla,nq,namin,namax,irec,nrec_max,nmucasw,
     &  mu_swp,ntun,nequi,nrpt,nmeas
        write(iud1) wrat,ndel_muca
        ntu_m=0
        do irpt=1,nrpt
          call razero(ha,0,nlink)
          acpt=zero
          do imeas=1,nmeas
            call potts_met
            call tun_cnt(namin,namax,iamin,iamax,ntu_m,ltun0)
          end do
          acpt=acpt/(nmeas*ns)
          call write_progress(iud2,"irpt,ntu_m,acpt:",irpt,ntu_m,acpt)
          write(iuo,'(" irpt,ntu_m,acpt:",2I10,F9.3)') irpt,ntu_m,acpt
          write(iud1) ha,acpt,irpt,ntu_m
        end do
      end if
C
      stop
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
      include '../../ForLib/write_progress.f'
