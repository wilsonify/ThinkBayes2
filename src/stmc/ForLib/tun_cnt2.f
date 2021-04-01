      subroutine tun_cnt2(namin,namax,iamin,iamax,ntun,ltun0)
C Copyright, Bernd Berg, Jul 15, 2002. Double tunneling count. 
C None are missed, if this routine is called after
C every sweep and (namax-namin) is sufficiently large.
      logical ltun0
      if(ltun0) then
        if(iamin.le.namin) then
          ntun=ntun+1
          ltun0=.false.
        end if
      else
        if(iamax.ge.namax) then 
          ntun=ntun+1
          ltun0=.true.
        end if
      end if
      return
      end
