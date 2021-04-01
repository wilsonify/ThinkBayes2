      SUBROUTINE P_TS_ZLN(nq,nlink,nmeas,beta0,b,a,tsa,Zln,Aln,A2ln,
     &                    XM2ln,nstate) ! TEST ONLY!
C Copyright Bernd Berg, Jul 10 2002. Potts model: ln of the partition 
C function Z and related variables from time series fragments.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      dimension b(0:nlink),a(0:nlink),tsa(nmeas,0:nq)
      dimension nstate(0:nq-1)
C
      nqm1=nq-1
      do imeas=1,nmeas
        iact=nint(tsa(imeas,nq))
        do iq=0,nqm1
          nstate(iq)=nint(tsa(imeas,iq))
c         if(iq.eq.0) nstate(iq)=nlink/2 ! Test purposes.
c         if(iq.ne.0) nstate(iq)=0
        end do
        if(imeas.eq.1) then
          Zln=two*((beta0-b(iact))*iact+a(iact))
          Aln=Zln+log(iact*one)
          A2ln=Zln+two*log(iact*one)
          XM2ln=Zln+two*log(nstate(0)*one)
          do iq=1,nqm1
            XM2ln1=Zln+two*log(nstate(iq)*one)
            XM2ln=addln(XM2ln,XM2ln1)
          end do
        else
          Zln1=two*((beta0-b(iact))*iact+a(iact))
          Aln1=Zln1+log(iact*one)
          A2ln1=Zln1+two*log(iact*one)
          Zln=addln(Zln,Zln1)
          Aln=addln(Aln,Aln1)
          A2ln=addln(A2ln,A2ln1)
          do iq=0,nqm1
            XM2ln1=Zln1+two*log(nstate(iq)*one)
            XM2ln=addln(XM2ln,XM2ln1)
          end do
        end if
      end do
C
      return
      end
