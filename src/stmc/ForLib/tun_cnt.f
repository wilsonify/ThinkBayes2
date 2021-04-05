 
      subroutine tun_cnt(namin,namax,iamin,iamax,ntun,ltun0)
C Copyright, Bernd Berg, May 27, 2001.
C Tunneling count. None are missed, if this routine is called after
C every sweep and (namax-namin) is sufficiently large.
      logical ltun0
      if(ltun0) then
        if(iamin<=namin) then
          ntun=ntun+1
          ltun0=.false.
        end if
      else
        if(iamax>=namax) ltun0=.true.
      end if
      return
      end
