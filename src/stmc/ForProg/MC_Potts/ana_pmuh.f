      program ana_pmuh
C Copyright Bernd Berg, May 30 2001.
C Analysis program for multicanonical data.
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
      dimension b(0:mlink),hasum(0:mlink),ph(0:mlink,3),phe(0:mlink,3),
     & nla_in(nd)
C      equivalence (b(0),wrat(-n2d,0)) ! Optional
      include 'lat.dat'
C
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
      open(iud1,file="mu"//cd//"d"//cq//"q"//cl//".dat",
     &     form="unformatted",status="old")
      read(iud1) nd_in,ml_in,nla_in,nq_in,namin_in,namax_in,irec,
     & nrec_max_in,nmucasw_in,mu_sweep,ntun,nequi_in,nrpt_in,nmeas_in
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
C Error bar for hasum histogram:
C
      call razero(phe(0,1),0,nlink)
      ha_max=zero
      rewind iud1
      read(iud1)
      read(iud1)
      do irpt=1,nrpt
        read(iud1) ha,acpt,irpt_in,ntun
        do ilink=0,nlink
        ha_max=max(ha_max,hasum(ilink))
        if(hasum(ilink).gt.half) phe(ilink,1)=phe(ilink,1)+
     &                           (hasum(ilink)-nrpt*ha(ilink))**2
        end do
      end do
C
C Write MUCA histogram to file h_d_q_l.d:
C
      open(iud2,file="hmu"//cd//"d"//cq//"q"//cl//".d",form="formatted",
     & status="unknown")
      do ilink=0,mlink
        if(hasum(ilink).gt.half) then
          actm=(one*ilink)/nlink
          em=two*nd*(one/nq-actm)
          hn=7*10**3*hasum(ilink)/ha_max
          hne=7*10**3*sqrt((phe(ilink,1)/(nrpt*(nrpt-1))))/ha_max
	  if(nq.eq.02) write(iud2,'(3F16.6)') em,hn,hne
	  if(nq.eq.10) write(iud2,'(4F16.6)') actm,em,(hn/four),(hne/four)
        end if
      end do
      close(iud2)
C
C Reweight histograms:
C
      write(iuo,'(" Reweight histograms:")')
      do ibeta=0,2
        rewind iud1
        read(iud1)
        read(iud1)
        if(nq.eq.02) beta0=(two*ibeta)/ten
        if(nq.eq.10) beta0=0.7075D00+0.0025D00*ibeta
        print*,"beta0 =",beta0
        call razero(phe(0,1+ibeta),0,nlink)
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
              ph(ilink,1+ibeta)=10**5*ha(ilink)
            end do
          else
            do ilink=0,nlink
              phe(ilink,1+ibeta)=phe(ilink,1+ibeta)+
     &                          (ph(ilink,1+ibeta)-10**5*ha(ilink))**2
            end do
          end if
        end do
        do ilink=0,mlink
          phe(ilink,1+ibeta)=sqrt(((nrpt-1)*phe(ilink,1+ibeta))/nrpt)
        end do
      end do
      close (iud1)
C
      open(iud2,file="hb"//cd//"d"//cq//"q"//cl//".d",
     & form="formatted",status="unknown")
      do ilink=140,nlink
        if(hasum(ilink).gt.half) then
          do ibeta=1,3
C           Just to get the small values out of the figure:
            if(ph(ilink,ibeta).lt.(one/ten)) ph(ilink,ibeta)=-one
          end do
          actm=(one*ilink)/nlink
          em=two*nd*(one/nq-actm)
          if(nq.eq.02) write(iud2,'(F10.5,6F10.1)')
     &    em,(ph(ilink,i),phe(ilink,i),i=1,3)
          if(nq.eq.10) write(iud2,'(2F10.5,6F10.1)')
     &    actm,em,(ph(ilink,i),phe(ilink,i),i=1,3)
        end if
      end do
      close(iud2)
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
