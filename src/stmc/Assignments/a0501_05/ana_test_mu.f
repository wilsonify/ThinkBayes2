      program ana_tmu
C Copyright Bernd Berg, May 14, 2001.
C Analysis program for multicanonical data. Test version of ana_pmu.f 
C which uses addln_cut to avoid underflow messages.
      include '../../ForLib/implicit.sta'
      character cd*1,cq*2,cl*3
      parameter(iud3=13,iud4=14,iud5=15,iud6=16,iud7=17)
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include 'muca.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      include '../../ForLib/potts_muca.com'
      dimension b(0:mlink),hasum(0:mlink),actj(0:nrpt),a2ctj(0:nrpt)
      dimension Cj(0:nrpt),nla_in(nd)
      dimension Zlnj(0:nrpt),Zlnj_dif(0:nrpt),Sj(0:nrpt)
C      equivalence (b(0),wrat(-n2d,0)) ! Optional, wrat in potts.com.
      include 'lat.dat'
C
      lfig=.true.
      ltest=.true.
      ltest=.false.
      if(ltest) stop "ana_pmu.f - ltest."
      write(cd,'(I1.1)') nd
      write(cq,'(I2.2)') nq
      write(cl,'(I3.3)') nla(1)
      write(iuo,'(" nd,nq,nla:",2I3,4I6)') nd,nq,nla
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
      if(nmucasw.ne.nmucasw_in) stop "nmucasw.ne.nmucasw_in"
      if(nequi.ne.nequi_in) stop "nequi.ne.nequi_in"
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
      write(iuo,'(" Read done, hasum calculated.")')
      write(iuo,'(" ntun,namin,namax:     ",3I9)') ntun,namin,namax
      write(iuo,'(" irec,nmucasw,mu_sweep:",3I9)') irec,nmucasw,mu_sweep
      acpt=(irec*nmucasw*one)/(mu_sweep*one)
      write(iuo,'(" acpt =                ",1F9.3)') acpt
      write(iuo,'(" nequi,nrpt,nmeas:     ",3I9)') nequi,nrpt,nmeas
C
C Jackknife data analysis:
C
      write(iuo,'(" Jackknife analysis:")')
      open(iud2,file="e"//cd//"d"//cq//"q"//cl//".d",form="formatted",
     & status="unknown")
      open(iud3,file="a"//cd//"d"//cq//"q"//cl//".d",form="formatted",
     & status="unknown")
      open(iud4,file="C"//cd//"d"//cq//"q"//cl//".d",form="formatted",
     & status="unknown")
      open(iud5,file="Z"//cd//"d"//cq//"q"//cl//".d",form="formatted",
     & status="unknown")
      open(iud6,file="F"//cd//"d"//cq//"q"//cl//".d",form="formatted",
     & status="unknown")
      open(iud7,file="S"//cd//"d"//cq//"q"//cl//".d",form="formatted",
     & status="unknown")
      do ibeta=0,40
        beta0=ibeta/(4*ten)
        rewind iud1
        read(iud1)
        read(iud1)
C
        call razero(ha,0,nlink)
        iopt=0
        do irpt=0,nrpt ! irpt=0 calculates results using all data.
          if(irpt.ge.1) read(iud1) ha,irpt_in,ntun
          if(ibeta.eq.0) CALL POTTS_Z0LN(nq,ns,nlink,namin,ndel_muca,
     &                                   b,ha,hasum,Zlnj_dif(irpt))
          CALL POTTS_ZLN(nlink,namin,beta0,b,ha,hasum,iopt,Zln,Aln,A2ln)
          actj(irpt)=exp(Aln-Zln)/nlink
          a2ctj(irpt)=exp(A2ln-Zln)/nlink**2
C Specific heat from the fluctuation dissipation theorem:
          Cj(irpt)=ns*beta0**2*
     &             ((nd*2)**2*a2ctj(irpt)-(nd*2*actj(irpt))**2)
          ej=-nd*two*(actj(irpt)-one/nq)  ! Energy.
          Zlnj(irpt)=Zln+Zlnj_dif(irpt)
C Entropy S=(e-F)*beta, but beta_potts=2*beta:
          Sj(irpt)=ej*beta0+Zlnj(irpt)/ns 
          if(nq.eq.10.and.lfig) Sj(irpt)=(ej*beta0+Zlnj(irpt)/ns)/three
        end do
C
C Action and energy:
        call stebj0(nrpt,actj(1),actm,actv,acte)
        em=-nd*two*(actm-one/nq)
        ee=nd*two*acte
        emm=-nd*(two*actj(0)-one/nq)
        write(iud2,'(4F12.6)') beta0,emm,em,ee
        write(iud3,'(4F12.6)') beta0,actj(0),actm,acte
C Expectation of action squared:
        call stebj0(nrpt,a2ctj(1),a2ctm,a2ctv,a2cte)
C Specific heat:
        call stebj0(nrpt,Cj(1),Cm,Cv,Ce)
        write(iud4,'(4F12.6)') beta0,Cj(0),Cm,Ce
C Partition function ln:
        call stebj0(nrpt,Zlnj(1),Zlnm,Zlnv,Zlne)
        write(iud5,'(4F12.6)') beta0,Zlnj(0),Zlnm,Zlne
        if(ibeta.ne.0) then
          F=-Zlnj(0)/(ns*beta0)
          Fm=-Zlnm  /(ns*beta0)
          Fe=-Zlne  /(ns*beta0)
          if(beta0.lt.half) then
             write(iud6,'(4F12.6)') beta0,F,Fm,Fe
          else
             Fas=(log(one*nq)-2*beta0*(nlink-namin))/(ns*beta0)
             write(iud6,'(5F12.6)') beta0,F,Fm,Fe,Fas
          end if
        end if
        call stebj0(nrpt,Sj(1),Sm,Sv,Se)
        write(iud7,'(4F12.6)') beta0,Sj(0),Sm,Se
      end do
      close (iud1)
      close (iud2)
      close (iud3)
      close (iud4)
      close (iud5)
      close (iud6)
      close (iud7)
      write(iuo,*) "All done."
C
      stop
      end

c     include '../../ForLib/potts_zln.f'
c     include '../../ForLib/potts_z0ln.f'
      include '../../ForLib/wrat_to_b.f'
      include '../../ForLib/addln_cut.f'
      include '../../ForLib/addln2_cut.f'
c     include '../../ForLib/addln_cut.f'
c     include '../../ForLib/addln.f'
      include '../../ForLib/nsfun.f'
      include '../../ForLib/razero.f'
      include '../../ForLib/stebj0.f'
      SUBROUTINE POTTS_Z0LN(nq,ns,nlink,namin,ndel_muca,b,ha,hasum,
     &                      Zln_dif)
C Copyright Bernd Berg, May 10 2000.
C Potts model, normalization of the partition function Z at beta=0.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      dimension b(0:nlink),ha(0:nlink),hasum(0:nlink)
      dimension ndel_muca(0:nlink) ! Eliminate in the final version!
C
      beta=zero
      Z0ln=ns*LOG(ONE*nq)
      if(namin.le.0) stop "POTTS_ZLN: namin false."
      ilink_min=nlink
      iact_next=namin+ndel_muca(namin) ! For test purposes only.
      do ilink=0,nlink
        ha_ilink=hasum(ilink)-ha(ilink) ! Jackknife histogram.
        if(ha_ilink.gt.half) then
          iact=ilink-namin
          if(ilink_min.gt.ilink) then
            ilink_min=ilink
            a=zero
            Zln=log(ha_ilink)+two*(beta-b(ilink))*iact
            Zln1_max=Zln
          else
            if(ilink.eq.namin) a=b(namin)*iact_old
            if(ilink.gt.namin) then
              a=a+(b(ilink)-b(ilink_old))*iact_old
              if(ilink.ne.iact_next) stop"POTTS_ZLN: iact_next problem."
              iact_next=ilink+ndel_muca(ilink)
            end if
            Zln1=log(ha_ilink)+two*((beta-b(ilink))*iact+a)
            Zln1_max=max(Zln1_max,Zln1)
            Zln=addln_cut(Zln,Zln1)
            iact_old=iact
            ilink_old=ilink
          end if
        end if
      end do
      Zln_dif=Z0ln-Zln
C
      return
      end
      SUBROUTINE POTTS_ZLN(nlink,namin,beta0,b,ha,hasum,iopt,
     &                     Zln,Aln,A2ln)
C Copyright Bernd Berg, May 12 2000.
C Potts model ln of the partition function Z and related variables.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      dimension b(0:nlink),ha(0:nlink),hasum(0:nlink)
C
      if(namin.le.0) stop "POTTS_ZLN: namin false."
      ilink_min=nlink
      do ilink=0,nlink
        ha(ilink)=hasum(ilink)-ha(ilink) ! Jackknife histogram.
        if(ha(ilink).gt.half) then
          iact=ilink-namin
          if(ilink_min.gt.ilink) then
            ilink_min=ilink
            a=zero
            Zln=log(ha(ilink))+two*(beta0-b(ilink))*iact
            Zln1_max=Zln
            Aln=Zln+log(ilink*one)
            A2ln=Zln+two*log(ilink*one)
          else
            if(ilink.eq.namin) a=b(namin)*iact_old
            if(ilink.gt.namin) then
              a=a+(b(ilink)-b(ilink_old))*iact_old
            end if
            Zln1=log(ha(ilink))+two*((beta0-b(ilink))*iact+a)
            Zln1_max=max(Zln1_max,Zln1)
            Aln1=Zln1+log(ilink*one)
            A2ln1=Zln1+two*log(ilink*one)
            Zln=addln_cut(Zln,Zln1)
            Aln=addln_cut(Aln,Aln1)
            A2ln=addln_cut(A2ln,A2ln1)
            iact_old=iact
            ilink_old=ilink
          end if
        end if
      end do
C
      if(iopt.ne.1) return
C iact probability density at beta0:
C
      a=zero
      hsum=zero
      do ilink=0,nlink
        if(ha(ilink).gt.half) then
          if(ilink.eq.namin) a=b(namin)*iact_old
          if(ilink.gt.namin) a=a+(b(ilink)-b(ilink_old))*iact_old
          iact=ilink-namin
          Zln1=log(ha(ilink))+two*((beta0-b(ilink))*iact+a)-Zln1_max
          if(Zln1.gt.0.01d00) stop "Zln1." 
          ha(ilink)=exp(Zln1)
          hsum=hsum+ha(ilink)
          iact_old=iact
          ilink_old=ilink
        else
          ha(ilink)=zero
        end if
      end do
      do ilink=0,nlink
        ha(ilink)=ha(ilink)/hsum
      end do
C
      return
      end
