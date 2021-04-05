      program ana_tsOn
C Copyright, Bernd Berg, Feb 11 2002. O(n) model, time series analysis.
C Calculates the average energy (action), prepares histogram plots.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      character cd*1,cn*2,cl*3
      include 'lat.par'
      include 'mc.par'
      include 'on.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/on.com'
      include 'lat.dat'
      dimension act(nrpt)
      ltest=.true.
      ltest=.false.
      if(ltest) stop "ana_tsOn: ltest."
C
      write(cd,'(I1.1)') nd ! Prepare to read the data.
      write(cn,'(I2.2)') n
      write(cl,'(I3.3)') nla(1)
      open(iud1,file="o"//cn//"_"//cd//"d"//cl//".d",
     &          form="unformatted",status="old")
      read(iud1) beta_in,nd_in,nla,nlink,nequi_in,nrpt_in,nmeas_in
      write(iuo,'(" beta,nlink,nd,nla:",F15.10,I10,I3,2X,4I5)')
     &              beta,nlink,nd_in,nla
      write(iuo,'(" nequi,nrpt,nmeas: ",I15,2I10)')
     &              nequi_in,nrpt_in,nmeas_in
      if(nequi/=nequi_in) stop "nequi.ne.nequi_in."
      if(nrpt/=nrpt_in) stop "nrpt.ne.nrpt_in."
      if(nmeas/=nmeas_in) stop "nmeas.ne.nmeas_in."
      do irpt=1,nrpt
        read(iud1) tsa,acpt,i_in
        act(irpt)=stmean(nmeas,tsa)
      end do
      close(iud1)
C
      call steb0(nrpt,act,actm,actv,acte)
      write(iuo,'(" actm =",F15.10," +/-",F15.10)') actm,acte
      write(iuo,'(" em =  ",F15.10," +/-",F15.10)') (-2*actm),(2*acte)
C
      write(iuo,*) "   "
      write(iuo,*) "Comparison with Berg-Luescher NPB 190 (1981) p.419:"
      E_BL=4*(one-actm)
      write(iuo,'(" E_BL =",F15.10," +/-",F15.10)') E_BL,4*acte
      E_BL= 2.295d0
      E_BLe=0.0005d0
      write(iuo,'(" E_BL =",F15.10," +/-",F15.10)') E_BL,E_BLe
C E_BL=2*(2+Em) ->
      em=(E_BL-4)/2
      eme=E_BLe/2
      write(iuo,'(" em =  ",F15.10," +/-",F15.10)') em,eme
      stop
      end

      include '../../ForLib/stmean.f'
      include '../../ForLib/steb0.f'
