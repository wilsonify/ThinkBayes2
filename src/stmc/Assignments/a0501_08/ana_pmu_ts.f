      program ana_pmu_ts
C Copyright Bernd Berg, Jul 10 2001.  
C Analysis program for multicanonical data, including times series.
      include '../../ForLib/implicit.sta'
      character cd*1,cq*2,cl*3
      parameter(iud3=13,iud4=14,iud5=15,iud6=16,iud7=17,iud8=18)
      include '../../ForLib/constants.par'
      include 'lat.par'
      include 'mc.par'
      include 'potts.par'
      include 'muca.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      include '../../ForLib/potts_muca.com'
      dimension b(0:mlink),a(0:mlink),hasum(0:mlink)
      dimension actj(0:nrpt),a2ctj(0:nrpt),Cj(0:nrpt),nla_in(nd)
      dimension Zlnj(0:nrpt),Zlnj_dif(0:nrpt),Sj(0:nrpt)
      dimension Alnj(0:nrpt),A2lnj(0:nrpt)
      dimension Alna(nrpt),A2lna(nrpt),Zlna(nrpt),tsa(nmeas,0:nq)
      dimension XM2lna(nrpt),XM2lnj(0:nrpt),xm2j(0:nrpt),nstate(0:nqm1)
C      equivalence (b(0),wrat(-n2d,0)) ! Optional, wrat in potts.com.
      include 'lat.dat'
C
      lfig=.true. ! For the reproduction of the free energy figure.
      lfig=.false.
      write(cd,'(I1.1)') nd
      write(cq,'(I2.2)') nq
      write(cl,'(I3.3)') nla(1)
      write(iuo,'(" nd,nq,nla:",2I3,4I6)') nd,nq,nla
C
C Read in of data to calculate sum (needed for jackknife):
C
      call razero(hasum,0,mlink)
      open(iud1,file="mu"//cd//"d"//cq//"q"//cl//"lts.D",
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
      act_min=nlink*one
      do irpt=1,nrpt
        read(iud1) ha,tsa,acpt,irpt_in,ntun
        do ilink=0,nlink
          hasum(ilink)=hasum(ilink)+ha(ilink)
        end do
        do imeas=1,nmeas
          act_min=min(act_min,tsa(imeas,nq))
        end do
      end do
      write(iuo,'(" Read done, hasum calculated.")')
      write(iuo,'(" ntun,namin,namax:     ",3I9)') ntun,namin,namax
      write(iuo,'(" irec,nmucasw,mu_sweep:",3I9)') irec,nmucasw,mu_sweep
      acpt=(irec*nmucasw*one)/(mu_sweep*one)
      write(iuo,'(" acpt =                ",1F9.3)') acpt
      write(iuo,'(" nequi,nrpt,nmeas:     ",3I9)') nequi,nrpt,nmeas
C
      do ilink=0,nlink
        if(hasum(ilink).gt.half) go to 1
      end do
1     write(iuo,'(" ilink,act_min =",I9,F12.0)') ilink,act_min
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
          if(irpt.ge.1) read(iud1) ha,tsa,irpt_in,ntun
          if(ibeta.eq.0) CALL POTTS_Z0LN(nq,ns,nlink,namin,ndel_muca,
     &                                   b,ha,hasum,Zlnj_dif(irpt))
          CALL POTTS_ZLN(nlink,namin,beta0,b,ha,hasum,iopt,Zln,Aln,A2ln)
          actj(irpt)=exp(Aln-Zln)/nlink
          a2ctj(irpt)=(exp(half*(A2ln-Zln))/nlink)**2
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
      close (iud2)
      close (iud3)
      close (iud4)
      close (iud5)
      close (iud6)
      close (iud7)
      write(iuo,*) "Histograms done. Times series starts."
C
C Time series analysis: ------------------------------------
C
      open(iud2,file="e"//cd//"d"//cq//"q"//cl//"lts.d",
     &          form="formatted",status="unknown")
      open(iud3,file="a"//cd//"d"//cq//"q"//cl//"lts.d",
     &          form="formatted",status="unknown")
      open(iud4,file="C"//cd//"d"//cq//"q"//cl//"lts.d",
     &          form="formatted",status="unknown")
      open(iud5,file="Z"//cd//"d"//cq//"q"//cl//"lts.d",
     &          form="formatted",status="unknown")
      open(iud6,file="F"//cd//"d"//cq//"q"//cl//"lts.d",
     &          form="formatted",status="unknown")
      open(iud7,file="S"//cd//"d"//cq//"q"//cl//"lts.d",
     &          form="formatted",status="unknown")
      open(iud8,file="M"//cd//"d"//cq//"q"//cl//"lts.d",
     &          form="formatted",status="unknown")
      iact_min=nint(act_min)
      do ibeta=0,60
        beta0=ibeta/(6*ten) ! 2d Ising.
        if(nq.eq.10.and.ibeta.ge.1) 
     &    beta0=4*one/ten+ibeta/ten**2 ! 2d 10-state Potts.
        print*,"beta0 =",beta0
        call p_mu_a(iact_min,nlink,namin,ndel_muca,beta0,b,a,hasum)
C
        if(ibeta.eq.0) then
          rewind iud1
          read(iud1)
          read(iud1)
          do irpt=1,nrpt ! Calculation using all data.
            read(iud1) ha,tsa,irpt_in,ntun
            CALL P_TS_Z0LN(nq,ns,nlink,nmeas,b,a,tsa(1,nq),Zlna(irpt))
          end do
          CALL P_TS_Z0LNJ(ns,nq,nrpt,Zlna,Zlnj,Zlnj_dif)
        end if
C
        rewind iud1
        read(iud1)
        read(iud1)
        do irpt=1,nrpt 
          read(iud1) ha,tsa,irpt_in,ntun
          CALL P_TS_ZLN(nq,nlink,nmeas,beta0,b,a,tsa,Zlna(irpt),
     &         Alna(irpt),A2lna(irpt),XM2lna(irpt),nstate)
        end do
        CALL P_TS_ZLNJ(nq,nrpt,Zlna,Alna,A2lna,XM2lna,
     &                 Zlnj,Alnj,A2lnj,XM2lnj)
C
        do irpt=0,nrpt
C Action, action squared, energy, magnetization squared:
          actj(irpt)=exp(Alnj(irpt)-Zlnj(irpt))/nlink
          a2ctj(irpt)=(exp(half*(A2lnj(irpt)-Zlnj(irpt)))/nlink)**2
          ej=-nd*two*(actj(irpt)-one/nq)  ! Energy.
          xm2j(irpt)=( exp(half*(XM2lnj(irpt)-Zlnj(irpt)))/
     &               (ns) )**2
C Specific heat from the fluctuation dissipation theorem:
          Cj(irpt)=ns*beta0**2*
     &           ((nd*2)**2*a2ctj(irpt)-(nd*2*actj(irpt))**2)
          Zln=Zlnj(irpt)
          Zlnj(irpt)=Zlnj(irpt)+Zlnj_dif(irpt)
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
C Magnetization:
        call stebj0(nrpt,xm2j(1),xm2m,xv,xm2e)
        write(iud8,'(4F12.6)') beta0,xm2j(0),xm2m,xm2e
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
      close (iud8)
      write(iuo,*) "All done."
C
      stop
      end


      include '../../ForLib/potts_zln.f'
      include '../../ForLib/potts_z0ln.f'
      include '../../ForLib/p_mu_a.f'
      include '../../ForLib/p_ts_z0ln.f'
      include '../../ForLib/p_ts_z0lnj.f'
      include '../../ForLib/p_ts_zln.f'
      include '../../ForLib/p_ts_zlnj.f'
      include '../../ForLib/wrat_to_b.f'
      include '../../ForLib/addln.f'
      include '../../ForLib/addln2.f'
      include '../../ForLib/nsfun.f'
      include '../../ForLib/razero.f'
      include '../../ForLib/stebj0.f'
