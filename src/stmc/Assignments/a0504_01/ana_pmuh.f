      program ana_pmuh
C Copyright Bernd Berg, May 30 2001.
C Analysis program for multicanonical data.
      include '../../ForLib/implicit.sta'
      character cd*1,cq*2,cl*3,ch*2
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include 'muca.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      include '../../ForLib/potts_muca.com'
      dimension b(0:mlink),hasum(0:mlink),ph(0:mlink),phe(0:mlink),
     & nla_in(nd),acta(0:mlink), ha_ar(nrpt,0:mlink),dha(nrpt)
      real*4 a1(3),a2(3),a3(3)
C      equivalence (b(0),wrat(-n2d,0)) ! Optional
      include 'lat.dat'
C
      do i=1,3
       a1(i)=0
       a2(i)=0
       a3(i)=0
      end do
      if(nd.ne.2 .or. (nq.ne.2 .and. nq.ne.10)) then
       print*,"This program works only for 2d with nq=2 or nq=10!"
       stop "ana_pmuh: nd or nq false!"
      end if
      ltest=.true.
      ltest=.false.
      write(cd,'(I1.1)') nd
      write(cq,'(I2.2)') nq
      write(cl,'(I3.3)') nla(1)
C
C Read in of data to calculate sum (needed for jackknife):
C
      call razero(hasum,0,mlink)
      open(iud1,file="mu"//cd//"d"//cq//"q"//cl//".d",
     &     form="unformatted",status="old")
      read(iud1) nd_in,ml_in,nla_in,nq_in,namin_in,namax_in,irec,
     & nrec_max_in,nmucasw_in,mu_sweep,ntun,nequi_in,nrpt_in,nmeas_in
      print*, "ntun=",ntun
      if(nd.ne.nd_in) stop "nd.ne.nd_in"
      if(ml.ne.ml_in) stop "ml.ne.ml_in"
      do id=1,nd
        if(nla(id).ne.nla_in(id)) stop "nla(id).ne.nla_in(id)"
      end do
      if(nq.ne.nq_in) stop "nq.ne.nq_in"
      if(namin.ne.namin_in) stop "namin.ne.namin_in"
      if(namax.ne.namax_in) stop "namax.ne.namax_in"
      if(nrpt.ne.nrpt_in) stop "nrpt.ne.nrpt_in"
      if(nmeas.ne.nmeas_in) stop "nmeas.ne.nmeas_in"
      read(iud1) wrat,ndel_muca
      call wrat_to_b(n2d,mlink,namin,namax,wrat,ndel_muca,b)
      ns=nsfun(nla,nd)
      nlink=nd*ns
      do irpt=1,nrpt
        read(iud1) ha,acpt,irpt_in,ntun
        do ilink=0,nlink
          hasum(ilink)=hasum(ilink)+ha(ilink)
        end do
      end do
      write(iuo,'(" 1. read done, hasum calculated. ntun=",I6)') ntun
      write(iuo,'("    namin,namax:",2I6)') namin,namax
C
C Reweight histograms:
C
      write(iuo,'(" Reweight histograms:")')
      open(13,file="beta.d", form="formatted", status="unknown")
      do ibeta=208,216
        beta0=0.7075D00+0.000025D00*ibeta
        print*, "----------- beta=", beta0, "-------------"
        rewind iud1
        read(iud1)
        read(iud1)
        call razero(phe,0,nlink)
        do irpt=0,nrpt ! irpt=0 calculates results using all data.
          if(irpt.eq.0) then
            do ilink=0,mlink
              ha(ilink)=zero
            end do
          else
            read(iud1) ha,acpt,irpt_in,ntun
          end if
	  iopt=1
          CALL POTTS_ZLN(nlink,namin,beta0,b,ha,hasum,iopt,Zln,Aln,A2ln)

          if(irpt.eq.0) then
            do ilink=0,nlink
              ph(ilink)=10**5*ha(ilink)
              acta(ilink)=(one*ilink)/nlink
            end do
          else
            do ilink=0,nlink
              phe(ilink)=phe(ilink)+
     &                          (ph(ilink)-10**5*ha(ilink))**2
            end do
          end if
          do ilink=0,nlink
           ha_ar(irpt,ilink)=10**5*ha(ilink)
          end do
        end do
        do ilink=0,mlink
          phe(ilink)=sqrt(((nrpt-1)*phe(ilink))/nrpt)
        end do
      do irpt=1,nrpt
       do ilink=0,mlink
        ha(ilink)=ha_ar(irpt,ilink)
       end do
       write(ch,'(I2.2)') irpt
       dha(irpt)=diff(namin, namax, acta, ha, phe, a1,a2,a3,irpt,dl)
      end do
      call stebj0(nrpt,dha,dhm,dhv,dhe)
c      write(13,'(3F10.5)') beta0,dhm,dhe
      write(13,'(34F10.5)') beta0,dha,dhe
      irpt=0
       dh=diff(namin, namax, acta, ph, phe, a1,a2,a3,irpt,dl)
C
      open(iud2,file="hb"//cd//"d"//cq//"q"//cl//".d",
     & form="formatted",status="unknown")
      do ilink=140,nlink
        if(hasum(ilink).gt.half) then
C           Just to get the small values out of the figure:
c            if(ph(ilink).lt.(one/ten)) ph(ilink)=-one
          actm=(one*ilink)/nlink
          write(iud2,'(F10.5,6F10.1)')
     &    actm,ph(ilink),phe(ilink)
        end if
      end do
      close(iud2)
      end do
      close (iud1)
C
      write(iuo,*) "All done."
C
      stop
      end

      include '../../ForLib/potts_zln.f'
      include '../../ForLib/wrat_to_b.f'
      include '../../ForLib/addln.f'
      include '../../ForLib/nsfun.f'
      include '../../ForLib/razero.f'
      include '../../ForLib/stebj0.f'
