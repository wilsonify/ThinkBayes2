      subroutine p_e_mc(iuo)
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
C Copyright, Bernd Berg, Jan 13 2002.
      include 'lat.par'
      include 'potts.par'
      include 'p_eds.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      include '../../ForLib/p_eds.com'
      dimension nqa(n2d)
      data Pcsum/mcase*zero/
      iamin=iact
      iamax=iact
C
      do i=1,ns ! One sweep.
        xr=Pmove*rmafun()
        Prob=Pcase(1)
        icase=1
        if(xr<=Prob) go to 1
        do icase=2,mcam1
          Prob=Prob+Pcase(icase)
          if(xr<=Prob) go to 1
        end do
        icase=mcase
1       continue
        is1=1+int((rmafun()*NAcase(icase)))
        is=Index(is1,icase)
        iq_in=ista(is)
        xr=rmafun()*qmove(icase)
        ifct=1
        jconf=1
        do id=1,nd
          iqf=ista(ipf(id,is))
          iqb=ista(ipb(id,is))
          nqa(id)=iqf
          nqa(id+nd)=iqb
          jconf=jconf+ifct*(iqf+nq*iqb)
          ifct=nq2*ifct
        end do
        iconf=jconf+iq_in*nconf0
        if(icase/=IAcase(iconf)) then
          print'(" is,iq_in,nqa:",2I4,6X,6I3)',icase,is,iq_in,nqa
          print*,"icase,IAcase(iconf),iconf:",icase,IAcase(iconf),iconf
          stop "p_e_mc at 0: icase-iconf mismatch!"
        end if
C q-state update:
        Prob=zero
        do iq=0,nqm1
          if(iq/=iq_in) then
            ia=0
            do id=1,n2d
              ia=ia+idel(nqa(id),iq)
            end do
            Prob=Prob+Peds1(ia,icase)
            if(xr<=Prob) go to 2
          end if
        end do
        write(iuo,*) "Error: iq,xr,Prob =",iq,xr,Prob
        stop "p_e_mc.f: error with the choice of the new iq."
2       continue
        ista(is)=iq
C Table updates:
        jconf=jconf+iq*nconf0
        jcase=IAcase(jconf)
        if(icase/=jcase) then
          iact=iact+ia-ia_array(icase) ! Action change.
          if(iact<0.or.iact>nlink) stop "p_e_mc: iact false!"
          iamin=min(iact,iamin)
          iamax=max(iact,iamax)
          call p_etabs_update(icase,jcase,is,is1)
        end if
        if(jcase/=IAcase(jconf)) then
          print*,"jcase,IAcase(jconf),jconf:",jcase,IAcase(jconf),jconf
          stop "p_e_mc at 0: jcase-jconf mismatch!"
        end if
C
        do id0=1,nd ! Forward neighbours.
          is0=ipf(id0,is)
          icase=IScase(is0)
          iq0=nqa(id0)
          ifct=1
          jconf=1+iq0*nconf0
          do id=1,nd
            jconf=jconf+ifct*(ista(ipf(id,is0))+nq*ista(ipb(id,is0)))
            ifct=nq2*ifct
          end do
          jcase=IAcase(jconf)
          if(icase/=jcase) then
            is1=Index(is0,mcap1)
            if(Index(is1,icase)/=is0) stop "p_e_mc.f: Index mismatch."
            call p_etabs_update(icase,jcase,is0,is1)
          end if
        end do
        if(jcase/=IAcase(jconf)) then
          print*,"jcase,IAcase(jconf),jconf:",jcase,IAcase(jconf),jconf
          stop "p_e_mc at 1: jcase-jconf mismatch!"
        end if
C
        do id0=1,nd ! Backward neighbours.
          is0=ipb(id0,is)
          icase=IScase(is0)
          iq0=nqa(id0+nd)
          ifct=1
          jconf=1+iq0*nconf0
          do id=1,nd
            jconf=jconf+ifct*(ista(ipf(id,is0))+nq*ista(ipb(id,is0)))
            ifct=nq2*ifct
          end do
          jcase=IAcase(jconf)
          if(icase/=jcase) then
            is1=Index(is0,mcap1)
            if(Index(is1,icase)/=is0) stop "p_e_mc: Index mismatch."
	    call p_etabs_update(icase,jcase,is0,is1)
          end if
        end do	    
	if(jcase/=IAcase(jconf)) then
	  print*,"jcase,IAcase(jconf),jconf:",jcase,IAcase(jconf),jconf
	  stop "p_e_mc at 2: jcase-jconf mismatch!"
        end if
	ha(iact)=ha(iact)+one/(one-Pstay)
      end do
C
      ia_min=min(ia_min,iamin)
      ia_max=min(ia_max,iamax)
      Pmove=zero
      do icase=1,mcase
        Pmove=Pmove+Pcase(icase) ! Re-calculation to avoid rounding.
        Pcsum(icase)=Pcsum(icase)+Pcase(icase) ! For Pcase_sort call.
      end do
      Pstay=One-Pmove
      return
      end


      subroutine p_etabs_update(icase,jcase,is0,is1)
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
C Copyright, Bernd Berg, Jan 13 2002. Update EDS tables.
      include 'lat.par'
      include 'potts.par'
      include 'p_eds.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      include '../../ForLib/p_eds.com'
      is2=Index(NAcase(icase),icase)
      Index(is1,icase)=is2
      Index(is2,mcap1)=is1
      NAcase(icase)=NAcase(icase)-1
      Pstay=Pstay+Pcase(icase)                ! Sensitive to rounding.
      Pcase(icase)=NAcase(icase)*qmove(icase) ! Avoids rounding.
      Pstay=Pstay-Pcase(icase)
      NAp1=NAcase(jcase)+1
      NAcase(jcase)=NAp1
      Pstay=Pstay+Pcase(jcase)
      Pcase(jcase)=NAcase(jcase)*qmove(jcase)
      Pstay=Pstay-Pcase(jcase)
      Pmove=One-Pstay
      Index(Nap1,jcase)=is0
      Index(is0,mcap1)=Nap1
      IScase(is0)=jcase
      return
      end


      subroutine Pcase_sort
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
C Copyright, Bernd Berg, Jan 13 2002.
      include 'lat.par'
      include 'potts.par'
      include 'p_eds.par'
      include '../../ForLib/lat.com'
      include '../../ForLib/potts.com'
      include '../../ForLib/p_eds.com'
      dimension icase_work(mcase)
c
      call heap_per(mcase,Pcsum,Icase_per)
c
      np1=mcase+1
      do icase=1,mcase
        Pcsum(icase)=Pcase(Icase_per(np1-icase))
        Icase_work(icase)=NAcase(Icase_per(np1-icase))
      end do
      do icase=1,mcase
        Pcase(icase)=Pcsum(icase)
        NAcase(icase)=Icase_work(icase)
      end do
c
      do icase=1,mcase
        Pcsum(icase)=qmove(Icase_per(np1-icase))
        Icase_work(icase)=ia_array(Icase_per(np1-icase))
      end do
      do icase=1,mcase
        qmove(icase)=Pcsum(icase)
        ia_array(icase)=Icase_work(icase)
      end do
c
      do id=0,n2d
        do icase=1,mcase
          Pcsum(icase)=Peds1(id,Icase_per(np1-icase))
        end do
        do icase=1,mcase
          Peds1(id,icase)=Pcsum(icase)
        end do
      end do
c
      do is=1,ns
        do icase=1,mcase
          Icase_work(icase)=Index(is,Icase_per(np1-icase))
        end do
        do icase=1,mcase
          Index(is,icase)=Icase_work(icase)
        end do
      end do
c
      do icase=1,mcase
        do is1=1,NAcase(icase)
          is=Index(is1,icase)
          Index(is,np1)=is1
        end do
      end do
c
      do icase=1,mcase
        Icase_work(Icase_per(np1-icase))=icase
      end do
      do iconf=1,nconf
        IAcase(iconf)=Icase_work(IAcase(iconf))
      end do
      do is=1,ns
        IScase(is)=Icase_work(IScase(is))
      end do
c
      do icase=1,mcase
        Pcsum(icase)=zero
      end do
      return
      end
